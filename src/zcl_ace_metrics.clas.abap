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
        time_t          TYPE f,        " T = E / 18     (Stroud number: mental discriminations/sec)
        bugs            TYPE f,        " B = V / 3000   (expected delivered bugs, Halstead)
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
        total_time_t         TYPE f,        " T = E / 18   summed across all units
        total_bugs           TYPE f,        " B = V / 3000 summed across all units
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

    TYPES:
  BEGIN OF ts_known_operand,
    name TYPE string,
  END OF ts_known_operand.
TYPES:
  tt_known_operands TYPE HASHED TABLE OF ts_known_operand
    WITH UNIQUE KEY name.

CLASS-METHODS is_branch_keyword
  IMPORTING i_kw      TYPE string
  RETURNING VALUE(rv) TYPE abap_bool.

CLASS-METHODS build_operand_set
  IMPORTING
    is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
    i_include     TYPE program
    i_unit_type   TYPE string
    i_unit_name   TYPE string
    i_class       TYPE string
  RETURNING
    VALUE(rt_ops) TYPE tt_known_operands.

CLASS-METHODS classify_token
  IMPORTING
    i_token        TYPE string
    i_is_first     TYPE abap_bool
    it_operands    TYPE tt_known_operands
  RETURNING
    VALUE(rv_kind) TYPE string.

CLASS-METHODS is_numeric_literal
  IMPORTING i_token   TYPE string
  RETURNING VALUE(rv) TYPE abap_bool.

CLASS-METHODS is_string_literal
  IMPORTING i_token   TYPE string
  RETURNING VALUE(rv) TYPE abap_bool.

CLASS-METHODS is_symbolic_operator
  IMPORTING i_token   TYPE string
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

      LOOP AT is_parse_data-tt_calls_line INTO DATA(ls_cl)
        WHERE include  = <prog>-include
          AND index    > 0
          AND ( eventtype = 'METHOD'   OR eventtype = 'FORM'
             OR eventtype = 'MODULE'   OR eventtype = 'FUNCTION' ).

        CHECK ls_cl-index <> ls_cl-def_ind.
        READ TABLE lt_boundaries WITH KEY stmt_from = ls_cl-index TRANSPORTING NO FIELDS.
        CHECK sy-subrc <> 0.

        DATA(lv_end_kw) = SWITCH string( ls_cl-eventtype
          WHEN 'METHOD'   THEN 'ENDMETHOD'
          WHEN 'FORM'     THEN 'ENDFORM'
          WHEN 'MODULE'   THEN 'ENDMODULE'
          WHEN 'FUNCTION' THEN 'ENDFUNCTION'
          ELSE '' ).

        DATA lv_stmt_to TYPE i VALUE 0.
        LOOP AT <prog>-t_keywords INTO DATA(ls_kw)
          WHERE index > ls_cl-index AND name = lv_end_kw.
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

      LOOP AT is_parse_data-t_events INTO DATA(ls_ev)
        WHERE include    = <prog>-include
          AND stmnt_from > 0
          AND stmnt_to   > 0.

        READ TABLE lt_boundaries WITH KEY stmt_from = ls_ev-stmnt_from TRANSPORTING NO FIELDS.
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
          WHEN ls_b-unit_type = 'METHOD' AND ls_b-class IS NOT INITIAL
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

        DATA(lt_operands) = build_operand_set(
          is_parse_data = is_parse_data
          i_include     = <prog>-include
          i_unit_type   = ls_b-unit_type
          i_unit_name   = ls_b-unit_name
          i_class       = ls_b-class ).

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
              INSERT CONV string( 'COMPUTE' ) INTO TABLE lt_dist_ops.
              APPEND VALUE ts_token_detail(
                token    = 'COMPUTE'
                kind     = 'OPERATOR'
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

              DATA lv_is_first TYPE boolean.
              IF ls_tok-str IS NOT INITIAL.
                IF  lv_ti = ls_stmt-from AND ls_stmt-type <> 'C'.
                  lv_is_first = abap_true.
                else.
                  clear lv_is_first.
                ENDIF.

                DATA(lv_kind) = classify_token(
                  i_token     = ls_tok-str
                  i_is_first  = lv_is_first
                  it_operands = lt_operands ).

                IF lv_kind = 'OPERATOR'.
                  ADD 1 TO ls_unit-n1.
                  INSERT ls_tok-str INTO TABLE lt_dist_ops.
                ELSE.
                  ADD 1 TO ls_unit-n2.
                  INSERT ls_tok-str INTO TABLE lt_dist_opd.
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
          ls_unit-time_t = ls_unit-effort / 18.
          ls_unit-bugs   = ls_unit-volume  / 3000.
        ENDIF.

        APPEND ls_unit TO rs_result-units.

      ENDLOOP.

    ENDLOOP.

    DATA lv_cnt TYPE i.
    LOOP AT rs_result-units INTO DATA(ls_u).
      ADD ls_u-cyclomatic TO rs_result-total_cyclomatic.
      rs_result-total_volume = rs_result-total_volume + ls_u-volume.
      rs_result-total_effort = rs_result-total_effort + ls_u-effort.
      rs_result-total_time_t = rs_result-total_time_t + ls_u-time_t.
      rs_result-total_bugs   = rs_result-total_bugs   + ls_u-bugs.
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


  METHOD log2.
    IF i_val <= 0. RETURN. ENDIF.
    rv = log( i_val ) / log( CONV f( 2 ) ).
  ENDMETHOD.


  METHOD build_operand_set.
    " Operands = all known named identifiers visible in this unit:
    "   1. Variables/field-symbols from t_vars (unit scope + class scope)
    "   2. Parameters from t_params
    "   3. All unit names (eventname) in this include — e.g. 'test' in METHOD test.
    "   4. Class/interface names from tt_class_defs

    LOOP AT is_parse_data-t_vars INTO DATA(ls_v)
      WHERE include = i_include
        AND ( eventname = i_unit_name OR eventname IS INITIAL OR eventtype = 'CLASS' ).
      IF ls_v-name IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( ls_v-name ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

    LOOP AT is_parse_data-t_params INTO DATA(ls_p)
      WHERE include = i_include AND event = i_unit_name.
      IF ls_p-param IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( ls_p-param ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

    " All unit names in this include are operands at their declaration/call sites
    LOOP AT is_parse_data-tt_calls_line INTO DATA(ls_cl)
      WHERE include = i_include.
      IF ls_cl-eventname IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( ls_cl-eventname ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

    " Class and interface names
    LOOP AT is_parse_data-tt_class_defs INTO DATA(ls_cd).
      IF ls_cd-class IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( ls_cd-class ) ) INTO TABLE rt_ops.
      ENDIF.
      IF ls_cd-super IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( ls_cd-super ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD classify_token.
    " OPERAND if:
    "   a) string literal  'text' or `text`
    "   b) numeric literal  42 / 3.14
    "   c) known named identifier (var / param / unit name / class name)
    " OPERATOR: everything else — first token of statement, symbolic operators,
    "           any ABAP keyword / sub-keyword not in the known-operand set.

    " First token of any non-comment statement is always a keyword
    IF i_is_first = abap_true.
      rv_kind = 'OPERATOR'.
      RETURN.
    ENDIF.

    " Symbolic operators
    IF is_symbolic_operator( i_token ) = abap_true.
      rv_kind = 'OPERATOR'.
      RETURN.
    ENDIF.

    " String literals
    IF is_string_literal( i_token ) = abap_true.
      rv_kind = 'OPERAND'.
      RETURN.
    ENDIF.

    " Numeric literals
    IF is_numeric_literal( i_token ) = abap_true.
      rv_kind = 'OPERAND'.
      RETURN.
    ENDIF.

    " Known named identifier?
    READ TABLE it_operands WITH TABLE KEY name = to_upper( i_token )
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      rv_kind = 'OPERAND'.
      RETURN.
    ENDIF.

    " Catch-all: unknown word → ABAP keyword or sub-keyword → OPERATOR
    rv_kind = 'OPERATOR'.

  ENDMETHOD.


  METHOD is_numeric_literal.
    IF i_token IS INITIAL. rv = abap_false. RETURN. ENDIF.
    DATA(lv_c) = i_token(1).
    " Starts with digit
    IF lv_c CA '0123456789'.
      rv = abap_true. RETURN.
    ENDIF.
    " Signed number: -7 or +3
    DATA(lv_len) = strlen( i_token ).
    IF lv_len > 1 AND ( lv_c = '-' OR lv_c = '+' ).
      DATA(lv_c2) = i_token+1(1).
      IF lv_c2 CA '0123456789'.
        rv = abap_true. RETURN.
      ENDIF.
    ENDIF.
    rv = abap_false.
  ENDMETHOD.


  METHOD is_string_literal.
    DATA(lv_len) = strlen( i_token ).
    IF lv_len >= 2.
      DATA(lv_first) = i_token(1).
      DATA(lv_last_idx) = lv_len - 1.
      DATA(lv_last) = i_token+lv_last_idx(1).
      IF ( lv_first = '''' AND lv_last = '''' )
      OR ( lv_first = '`'  AND lv_last = '`'  ).
        rv = abap_true.
        RETURN.
      ENDIF.
    ENDIF.
    rv = abap_false.
  ENDMETHOD.


  METHOD is_symbolic_operator.
    CASE i_token.
      WHEN '+' OR '-' OR '*' OR '/' OR '**' OR '&&'
        OR '=' OR '<>' OR '<' OR '>' OR '<=' OR '>='
        OR '(' OR ')' OR ',' OR ':' OR '.' OR '->' OR '=>' OR '|'.
        rv = abap_true.
      WHEN OTHERS.
        rv = abap_false.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
