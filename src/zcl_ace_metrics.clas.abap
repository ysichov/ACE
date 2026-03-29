CLASS zcl_ace_metrics DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    "--- token-level detail for debugging ---
    TYPES:
      BEGIN OF ts_token_detail,
        token    TYPE string,
        kind     TYPE string,   " OPERATOR(kw) / OPERATOR(sym) / OPERATOR(sub) / OPERAND
        stmt_idx TYPE i,
        tok_idx  TYPE i,
        row      TYPE i,
      END OF ts_token_detail.
    TYPES:
      tt_token_details TYPE STANDARD TABLE OF ts_token_detail WITH EMPTY KEY.

    "--- result per code unit (method / form / module / program-level) ---
    TYPES:
      BEGIN OF ts_unit_result,
        program         TYPE program,
        include         TYPE program,
        unit_type       TYPE string,   " METHOD / FORM / MODULE / FUNCTION / PROGRAM
        unit_name       TYPE string,
        " McCabe cyclomatic complexity
        cyclomatic      TYPE i,
        " Halstead raw counts
        n1              TYPE i,        " total operators
        n2              TYPE i,        " total operands
        big_n1          TYPE i,        " distinct operators
        big_n2          TYPE i,        " distinct operands
        " Halstead derived
        vocabulary      TYPE i,        " η = η1 + η2
        prog_length     TYPE i,        " N = N1 + N2
        volume          TYPE f,        " V = N * log2(η)
        difficulty      TYPE f,        " D = (η1/2) * (N2/η2)
        effort          TYPE f,        " E = D * V
        " Lines of code
        loc             TYPE i,        " total lines in unit
        lloc            TYPE i,        " logical LOC (statements)
        cloc            TYPE i,        " comment lines
        " Token-level debug detail
        token_detail    TYPE tt_token_details,
      END OF ts_unit_result.
    TYPES:
      tt_unit_results TYPE STANDARD TABLE OF ts_unit_result WITH EMPTY KEY.

    "--- aggregate result for whole program ---
    TYPES:
      BEGIN OF ts_result,
        program              TYPE program,
        units                TYPE tt_unit_results,
        total_cyclomatic     TYPE i,
        total_volume         TYPE f,
        total_effort         TYPE f,
        total_loc            TYPE i,
        total_lloc           TYPE i,
        total_cloc           TYPE i,
        avg_cyclomatic       TYPE f,
      END OF ts_result.

    CLASS-METHODS calculate
      IMPORTING
        is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
        i_program     TYPE program
      RETURNING
        VALUE(rs_result) TYPE ts_result.

  PRIVATE SECTION.

    CLASS-METHODS is_branch_keyword
      IMPORTING i_kw      TYPE string
      RETURNING VALUE(rv) TYPE abap_bool.

    CLASS-METHODS is_sub_keyword
      IMPORTING i_kw      TYPE string
      RETURNING VALUE(rv) TYPE abap_bool.

    CLASS-METHODS log2
      IMPORTING i_val      TYPE f
      RETURNING VALUE(rv)  TYPE f.

ENDCLASS.



CLASS ZCL_ACE_METRICS IMPLEMENTATION.


  METHOD calculate.

    rs_result-program = i_program.

    LOOP AT is_parse_data-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE program = i_program.

      DATA(lo_scan) = <prog>-scan.
      CHECK lo_scan IS BOUND.
      CHECK lo_scan->statements IS NOT INITIAL.

      " ---- build ABAP keyword set (= operator vocabulary) ----
      " Only take first tokens of non-comment, non-COMPUTE statements.
      " COMPUTE statements (type 'C') start with a variable name, not a keyword —
      " including them would incorrectly classify variable names as operators.
      DATA lt_ops TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      CLEAR lt_ops.
      LOOP AT lo_scan->statements INTO DATA(ls_s_op).
        CHECK ls_s_op-type <> 'P'.   " skip comments
        CHECK ls_s_op-type <> 'C'.   " skip COMPUTE: first token is a variable, not a keyword
        READ TABLE lo_scan->tokens INDEX ls_s_op-from INTO DATA(ls_t_op).
        CHECK sy-subrc = 0.
        INSERT ls_t_op-str INTO TABLE lt_ops.
      ENDLOOP.

      " For COMPUTE statements the implicit keyword is 'COMPUTE' — add it explicitly
      " so that it counts as an operator when we later classify tokens.
      DATA(lv_compute_kw) = CONV string( 'COMPUTE' ).
      INSERT lv_compute_kw INTO TABLE lt_ops.

      " ---- collect unit boundaries ----
      " Sources:
      "   1. tt_calls_line  → METHOD / FORM / MODULE / FUNCTION blocks
      "   2. t_events       → START-OF-SELECTION / INITIALIZATION / AT ... event blocks
      " index = def_ind means no implementation (interface/abstract) → skip
      TYPES: BEGIN OF ts_boundary,
               stmt_from TYPE i,
               stmt_to   TYPE i,
               unit_type TYPE string,
               unit_name TYPE string,
               class     TYPE string,
             END OF ts_boundary.
      DATA lt_boundaries TYPE SORTED TABLE OF ts_boundary
        WITH UNIQUE KEY stmt_from.
      CLEAR lt_boundaries.

      " --- source 1: calls_line ---
      LOOP AT is_parse_data-tt_calls_line INTO DATA(ls_cl)
        WHERE include  = <prog>-include
          AND index    > 0
          AND ( eventtype = 'METHOD'   OR eventtype = 'FORM'
             OR eventtype = 'MODULE'   OR eventtype = 'FUNCTION' ).

        CHECK ls_cl-index <> ls_cl-def_ind.

        READ TABLE lt_boundaries WITH KEY stmt_from = ls_cl-index
          TRANSPORTING NO FIELDS.
        CHECK sy-subrc <> 0.

        DATA(lv_end_kw) = SWITCH string( ls_cl-eventtype
          WHEN 'METHOD'   THEN 'ENDMETHOD'
          WHEN 'FORM'     THEN 'ENDFORM'
          WHEN 'MODULE'   THEN 'ENDMODULE'
          WHEN 'FUNCTION' THEN 'ENDFUNCTION'
          ELSE                 '' ).

        DATA lv_stmt_to TYPE i.
        lv_stmt_to = 0.
        LOOP AT <prog>-t_keywords INTO DATA(ls_kw)
          WHERE index > ls_cl-index
            AND name  = lv_end_kw.
          lv_stmt_to = ls_kw-index.
          EXIT.
        ENDLOOP.
        CHECK lv_stmt_to > 0.

        INSERT VALUE ts_boundary(
          stmt_from = ls_cl-index
          stmt_to   = lv_stmt_to
          unit_type = ls_cl-eventtype
          unit_name = ls_cl-eventname
          class     = ls_cl-class
        ) INTO TABLE lt_boundaries.

      ENDLOOP.

      " --- source 2: t_events (START-OF-SELECTION, INITIALIZATION, AT ...) ---
      LOOP AT is_parse_data-t_events INTO DATA(ls_ev)
        WHERE include     = <prog>-include
          AND stmnt_from  > 0
          AND stmnt_to    > 0.

        READ TABLE lt_boundaries WITH KEY stmt_from = ls_ev-stmnt_from
          TRANSPORTING NO FIELDS.
        CHECK sy-subrc <> 0.

        INSERT VALUE ts_boundary(
          stmt_from = ls_ev-stmnt_from
          stmt_to   = ls_ev-stmnt_to
          unit_type = 'EVENT'
          unit_name = ls_ev-name
          class     = ''
        ) INTO TABLE lt_boundaries.

      ENDLOOP.

      CHECK lt_boundaries IS NOT INITIAL.

      " ---- metric calculation per unit ----
      DATA lv_first_row TYPE i.
      DATA lv_last_row  TYPE i.
      DATA lv_si        TYPE i.
      DATA lv_ti        TYPE i.
      DATA lt_dist_ops  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA lt_dist_opd  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA ls_stmt_f    LIKE LINE OF lo_scan->statements.
      DATA ls_stmt_t    LIKE LINE OF lo_scan->statements.
      DATA ls_tok_f     LIKE LINE OF lo_scan->tokens.
      DATA ls_tok_t     LIKE LINE OF lo_scan->tokens.
      DATA ls_stmt      LIKE LINE OF lo_scan->statements.
      DATA ls_kw_tok    LIKE LINE OF lo_scan->tokens.
      DATA ls_tok       LIKE LINE OF lo_scan->tokens.

      LOOP AT lt_boundaries INTO DATA(ls_b).

        DATA ls_unit TYPE ts_unit_result.
        CLEAR ls_unit.
        ls_unit-program   = <prog>-program.
        ls_unit-include   = <prog>-include.
        ls_unit-unit_type = ls_b-unit_type.
        ls_unit-unit_name = COND #(
          WHEN ls_b-unit_type =  'METHOD'
          THEN |{ ls_b-class }=>{ ls_b-unit_name }|
          ELSE ls_b-unit_name ).
        ls_unit-cyclomatic = 1.

        CLEAR: lt_dist_ops, lt_dist_opd.
        lv_first_row = 0.
        lv_last_row  = 0.

        CLEAR: ls_stmt_f, ls_stmt_t, ls_tok_f, ls_tok_t.
        READ TABLE lo_scan->statements INDEX ls_b-stmt_from INTO ls_stmt_f.
        IF sy-subrc = 0.
          READ TABLE lo_scan->tokens INDEX ls_stmt_f-from INTO ls_tok_f.
          IF sy-subrc = 0. lv_first_row = ls_tok_f-row. ENDIF.
        ENDIF.
        READ TABLE lo_scan->statements INDEX ls_b-stmt_to INTO ls_stmt_t.
        IF sy-subrc = 0.
          READ TABLE lo_scan->tokens INDEX ls_stmt_t-to INTO ls_tok_t.
          IF sy-subrc = 0. lv_last_row = ls_tok_t-row. ENDIF.
        ENDIF.
        IF lv_last_row >= lv_first_row AND lv_first_row > 0.
          ls_unit-loc = lv_last_row - lv_first_row + 1.
        ENDIF.

        lv_si = ls_b-stmt_from.
        WHILE lv_si <= ls_b-stmt_to.
          CLEAR ls_stmt.
          READ TABLE lo_scan->statements INDEX lv_si INTO ls_stmt.
          IF sy-subrc <> 0. EXIT. ENDIF.

          IF ls_stmt-type = 'P'.
            ADD 1 TO ls_unit-cloc.
          ELSE.
            ADD 1 TO ls_unit-lloc.
            CLEAR ls_kw_tok.
            READ TABLE lo_scan->tokens INDEX ls_stmt-from INTO ls_kw_tok.
            IF sy-subrc = 0 AND is_branch_keyword( ls_kw_tok-str ) = abap_true.
              ADD 1 TO ls_unit-cyclomatic.
            ENDIF.

            " For COMPUTE statements (type 'C') inject the virtual COMPUTE keyword
            " as an operator occurrence before processing the actual tokens.
            IF ls_stmt-type = 'C'.
              ADD 1 TO ls_unit-n1.
              INSERT lv_compute_kw INTO TABLE lt_dist_ops.
              APPEND VALUE ts_token_detail(
                token    = 'COMPUTE'
                kind     = 'OPERATOR(kw)'
                stmt_idx = lv_si
                tok_idx  = 0
                row      = ls_kw_tok-row
              ) TO ls_unit-token_detail.
            ENDIF.

            lv_ti = ls_stmt-from.
            WHILE lv_ti <= ls_stmt-to.
              CLEAR ls_tok.
              READ TABLE lo_scan->tokens INDEX lv_ti INTO ls_tok.
              IF sy-subrc <> 0. EXIT. ENDIF.
              IF ls_tok-str IS NOT INITIAL.
                DATA(lv_kind) = CONV string( '' ).
                READ TABLE lt_ops WITH TABLE KEY table_line = ls_tok-str
                  TRANSPORTING NO FIELDS.
                IF sy-subrc = 0.
                  " Primary keyword (first token of statement)
                  ADD 1 TO ls_unit-n1.
                  INSERT ls_tok-str INTO TABLE lt_dist_ops.
                  lv_kind = 'OPERATOR(kw)'.
                ELSE.
                  CASE ls_tok-str.
                    WHEN '+' OR '-' OR '*' OR '/' OR '**' OR '&&'
                      OR '=' OR '<>' OR '<' OR '>' OR '<=' OR '>='
                      OR '(' OR ')' OR ',' OR ':' OR '.' OR '->' OR '=>'.
                      " Symbolic operator
                      ADD 1 TO ls_unit-n1.
                      INSERT ls_tok-str INTO TABLE lt_dist_ops.
                      lv_kind = 'OPERATOR(sym)'.
                    WHEN OTHERS.
                      IF is_sub_keyword( ls_tok-str ) = abap_true.
                        " Sub-keyword: structural keyword within a statement
                        " (e.g. FROM, WHERE, INTO, SINGLE, EXPORTING, ...)
                        ADD 1 TO ls_unit-n1.
                        INSERT ls_tok-str INTO TABLE lt_dist_ops.
                        lv_kind = 'OPERATOR(sub)'.
                      ELSE.
                        ADD 1 TO ls_unit-n2.
                        INSERT ls_tok-str INTO TABLE lt_dist_opd.
                        lv_kind = 'OPERAND'.
                      ENDIF.
                  ENDCASE.
                ENDIF.
                APPEND VALUE ts_token_detail(
                  token    = ls_tok-str
                  kind     = lv_kind
                  stmt_idx = lv_si
                  tok_idx  = lv_ti
                  row      = ls_tok-row
                ) TO ls_unit-token_detail.
              ENDIF.
              ADD 1 TO lv_ti.
            ENDWHILE.
          ENDIF.
          ADD 1 TO lv_si.
        ENDWHILE.

        ls_unit-big_n1      = lines( lt_dist_ops ).
        ls_unit-big_n2      = lines( lt_dist_opd ).
        ls_unit-vocabulary  = ls_unit-big_n1 + ls_unit-big_n2.
        ls_unit-prog_length = ls_unit-n1 + ls_unit-n2.

        IF ls_unit-vocabulary > 0 AND ls_unit-prog_length > 0.
          DATA(lv_voc_f) = CONV f( ls_unit-vocabulary ).
          DATA(lv_len_f) = CONV f( ls_unit-prog_length ).
          ls_unit-volume = lv_len_f * log2( lv_voc_f ).
          IF ls_unit-big_n2 > 0.
            ls_unit-difficulty =
              ( CONV f( ls_unit-big_n1 ) / 2 )
              * ( CONV f( ls_unit-n2 ) / CONV f( ls_unit-big_n2 ) ).
          ENDIF.
          ls_unit-effort = ls_unit-difficulty * ls_unit-volume.
        ENDIF.

        APPEND ls_unit TO rs_result-units.

      ENDLOOP.

    ENDLOOP.

    " ---- aggregate totals ----
    DATA lv_cnt TYPE i.
    LOOP AT rs_result-units INTO DATA(ls_u).
      ADD ls_u-cyclomatic TO rs_result-total_cyclomatic.
      rs_result-total_volume = rs_result-total_volume + ls_u-volume.
      rs_result-total_effort = rs_result-total_effort + ls_u-effort.
      ADD ls_u-loc  TO rs_result-total_loc.
      ADD ls_u-lloc TO rs_result-total_lloc.
      ADD ls_u-cloc TO rs_result-total_cloc.
      ADD 1 TO lv_cnt.
    ENDLOOP.
    IF lv_cnt > 0.
      rs_result-avg_cyclomatic =
        CONV f( rs_result-total_cyclomatic ) / CONV f( lv_cnt ).
    ENDIF.

  ENDMETHOD.


  METHOD is_branch_keyword.
    CASE i_kw.
      WHEN 'IF' OR 'ELSEIF' OR 'WHEN' OR 'CATCH'
        OR 'LOOP' OR 'WHILE' OR 'DO'
        OR 'CHECK' OR 'AT' OR 'ON'.
        rv = abap_true.
      WHEN OTHERS.
        rv = abap_false.
    ENDCASE.
  ENDMETHOD.


  METHOD is_sub_keyword.
    " Sub-keywords: structural keywords that appear inside statements
    " but are never the first token — so they don't get into lt_ops automatically.
    " Grouped by statement family for readability.
    CASE i_kw.

      " --- SELECT / OPEN CURSOR ---
      WHEN 'SINGLE' OR 'DISTINCT'
        OR 'FROM' OR 'AS' OR 'JOIN' OR 'INNER' OR 'LEFT' OR 'OUTER' OR 'CROSS'
        OR 'ON' OR 'UP' OR 'TO' OR 'ROWS'
        OR 'WHERE' OR 'HAVING' OR 'GROUP' OR 'ORDER' OR 'BY'
        OR 'ASCENDING' OR 'DESCENDING'
        OR 'INTO' OR 'APPENDING' OR 'CORRESPONDING' OR 'FIELDS'
        OR 'FOR' OR 'ALL' OR 'ENTRIES' OR 'IN'
        OR 'UNION' OR 'INTERSECT' OR 'EXCEPT'.

      " --- CALL FUNCTION / CALL METHOD / CALL BADI ---
      WHEN 'EXPORTING' OR 'IMPORTING' OR 'CHANGING' OR 'TABLES'
        OR 'EXCEPTIONS' OR 'DESTINATION' OR 'STARTING'
        OR 'IN' OR 'BACKGROUND' OR 'TASK' OR 'UNIT'.

      " --- READ TABLE / LOOP AT / INSERT / MODIFY / DELETE ---
      WHEN 'WITH' OR 'KEY' OR 'BINARY' OR 'SEARCH'
        OR 'TRANSPORTING' OR 'FIELDS' OR 'REFERENCE'
        OR 'ASSIGNING' OR 'CASTING' OR 'RESULT'
        OR 'COMPARING' OR 'NO' OR 'FIELDS'.

      " --- MOVE / ASSIGN / CONVERT / WRITE ---
      WHEN 'CORRESPONDING' OR 'BASE' OR 'MAPPING' OR 'EXCEPT'
        OR 'DECIMALS' OR 'CURRENCY' OR 'UNIT' OR 'TIMEZONE'
        OR 'RESPECTING' OR 'BLANKS' OR 'REPLACEMENT' OR 'CHARACTER' OR 'MODE'.

      " --- RAISE / TRY / CATCH / MESSAGE ---
      WHEN 'RESUMABLE' OR 'SHORTDUMP' OR 'TYPE' OR 'LIKE'
        OR 'LEVEL' OR 'NUMBER' OR 'WITH'
        OR 'INTO' OR 'BEFORE' OR 'UNWIND'.

      " --- CREATE OBJECT / DATA / FIELD-SYMBOL ---
      WHEN 'OBJECT' OR 'AREA' OR 'HANDLE' OR 'SECTION'
        OR 'OPTIONAL' OR 'PREFERRED' OR 'PARAMETER'
        OR 'VALUE' OR 'DEFAULT' OR 'INITIAL'.

      " --- APPEND / COLLECT / SORT ---
      WHEN 'SORTED' OR 'BY' OR 'STABLE' OR 'ADJACENT'
        OR 'DUPLICATES' OR 'LINES' OR 'RANGE' OR 'STEP'.

      " --- OPEN/CLOSE DATASET ---
      WHEN 'DATASET' OR 'POSITION' OR 'AT' OR 'END'
        OR 'FILTER' OR 'ENCODING' OR 'CODE' OR 'PAGE'.

      WHEN OTHERS.
        rv = abap_false.
        RETURN.
    ENDCASE.
    rv = abap_true.
  ENDMETHOD.


  METHOD log2.
    IF i_val <= 0. RETURN. ENDIF.
    rv = log( i_val ) / log( CONV f( 2 ) ).
  ENDMETHOD.
ENDCLASS.
