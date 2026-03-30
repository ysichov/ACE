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

        TYPES: tt_abap_statements TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.


    "--- result per code unit (method / form / module / program-level) ---
    TYPES:
      BEGIN OF ts_unit_result,
        program      TYPE program,
        include      TYPE program,
        unit_type    TYPE string,   " METHOD / FORM / MODULE / FUNCTION / PROGRAM
        unit_name    TYPE string,
        " McCabe cyclomatic complexity
        cyclomatic   TYPE i,
        " Halstead raw counts
        n1           TYPE i,        " total operators
        n2           TYPE i,        " total operands
        big_n1       TYPE i,        " distinct operators
        big_n2       TYPE i,        " distinct operands
        " Halstead derived
        vocabulary   TYPE i,        " η = η1 + η2
        prog_length  TYPE i,        " N = N1 + N2
        volume       TYPE f,        " V = N * log2(η)
        difficulty   TYPE f,        " D = (η1/2) * (N2/η2)
        effort       TYPE f,        " E = D * V
        time_t       TYPE f,        " T = E / 18     (Stroud number: mental discriminations/sec)
        bugs         TYPE f,        " B = V / 3000   (expected delivered bugs, Halstead)
        " Maintainability Index
        mi           TYPE f,        " MI = 171 - 5.2*ln(V) - 0.23*G - 16.2*ln(LOC)
        " Lines of code
        loc          TYPE i,        " total lines in unit
        lloc         TYPE i,        " logical LOC (statements)
        cloc         TYPE i,        " comment lines
        " Token-level debug detail
        token_detail TYPE tt_token_details,
      END OF ts_unit_result.
    TYPES:
      tt_unit_results TYPE STANDARD TABLE OF ts_unit_result WITH EMPTY KEY.

    "--- aggregate result for whole program ---
    TYPES:
      BEGIN OF ts_result,
        program          TYPE program,
        units            TYPE tt_unit_results,
        total_cyclomatic TYPE i,
        total_volume     TYPE f,
        total_effort     TYPE f,
        total_time_t     TYPE f,        " T = E / 18   summed across all units
        total_bugs       TYPE f,        " B = V / 3000 summed across all units
        total_loc        TYPE i,
        total_lloc       TYPE i,
        total_cloc       TYPE i,
        avg_cyclomatic   TYPE f,
      END OF ts_result.

    CLASS-METHODS calculate
      IMPORTING
        is_parse_data    TYPE zif_ace_parse_data=>ts_parse_data
        i_program        TYPE program
      RETURNING
        VALUE(rs_result) TYPE ts_result.
private section.

  types:
    BEGIN OF ts_known_operand,
    name TYPE string,
  END OF ts_known_operand .
  types:
    tt_known_operands TYPE HASHED TABLE OF ts_known_operand
    WITH UNIQUE KEY name .

  class-data MT_STATEMENTS type TT_ABAP_STATEMENTS .

  class-methods IS_BRANCH_KEYWORD
    importing
      !I_KW type STRING
    returning
      value(RV) type ABAP_BOOL .
  class-methods BUILD_OPERAND_SET
    importing
      !IS_PARSE_DATA type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
      !I_INCLUDE type PROGRAM
      !I_UNIT_TYPE type STRING
      !I_UNIT_NAME type STRING
      !I_CLASS type STRING
    returning
      value(RT_OPS) type TT_KNOWN_OPERANDS .
  class-methods CLASSIFY_TOKEN
    importing
      !I_TOKEN type STRING
      !I_IS_FIRST type ABAP_BOOL
      !IT_OPERANDS type TT_KNOWN_OPERANDS
    returning
      value(RV_KIND) type STRING .
  class-methods LOG2
    importing
      !I_VAL type F
    returning
      value(RV) type F .
  class-methods FILL_STATEMENTS .
ENDCLASS.



CLASS ZCL_ACE_METRICS IMPLEMENTATION.


  METHOD calculate.

    IF mt_statements IS INITIAL.
      fill_statements( ).
    ENDIF.

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

      " Build operand set ONCE per include, not per unit
      DATA(lt_operands) = build_operand_set(
        is_parse_data = is_parse_data
        i_include     = <prog>-include
        i_unit_type   = ''
        i_unit_name   = ''
        i_class       = '' ).

      DATA lv_first_row TYPE i.
      DATA lv_last_row  TYPE i.
      DATA lt_dist_ops  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA lt_dist_opd  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA ls_stmt_f    LIKE LINE OF lo_scan->statements.
      DATA ls_stmt_t    LIKE LINE OF lo_scan->statements.
      DATA ls_tok_f     LIKE LINE OF lo_scan->tokens.
      DATA ls_tok_t     LIKE LINE OF lo_scan->tokens.
      DATA ls_kw_tok    LIKE LINE OF lo_scan->tokens.

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

        LOOP AT lo_scan->statements ASSIGNING FIELD-SYMBOL(<stmt>)
          FROM ls_b-stmt_from TO ls_b-stmt_to.

          IF <stmt>-type = 'P'.
            ADD 1 TO ls_unit-cloc.
          ELSE.
            ADD 1 TO ls_unit-lloc.
            CLEAR ls_kw_tok.
            READ TABLE lo_scan->tokens INDEX <stmt>-from INTO ls_kw_tok.
            IF sy-subrc = 0 AND is_branch_keyword( ls_kw_tok-str ) = abap_true.
              ADD 1 TO ls_unit-cyclomatic.
            ENDIF.

            IF <stmt>-type = 'C'.
              ADD 1 TO ls_unit-n1.
              INSERT CONV string( 'COMPUTE' ) INTO TABLE lt_dist_ops.
              APPEND VALUE ts_token_detail(
                token    = 'COMPUTE'
                kind     = 'OPERATOR'
                stmt_idx = sy-tabix
                tok_idx  = 0
                row      = ls_kw_tok-row
              ) TO ls_unit-token_detail.
            ENDIF.

            DATA(lv_stmt_tabix) = sy-tabix.
            LOOP AT lo_scan->tokens ASSIGNING FIELD-SYMBOL(<tok>)
              FROM <stmt>-from TO <stmt>-to.

              IF <tok>-str IS NOT INITIAL.
                DATA lv_is_first TYPE boolean.
                IF sy-tabix = <stmt>-from AND <stmt>-type <> 'C'.
                  lv_is_first = abap_true.
                ELSE.
                  CLEAR lv_is_first.
                ENDIF.

                DATA(lv_tok_up)      = to_upper( <tok>-str ).
                DATA(lv_inline_name) = ``.

                IF lv_tok_up CP 'DATA(*)' AND strlen( lv_tok_up ) > 5.
                  DATA(lv_tmp) = <tok>-str+5.
                  DATA(lv_tmp_len) = strlen( lv_tmp ) - 1.
                  IF lv_tmp_len > 0.
                    lv_inline_name = lv_tmp(lv_tmp_len).
                  ENDIF.
                ELSEIF lv_tok_up CP 'FIELD-SYMBOL(<*)' AND strlen( lv_tok_up ) > 14.
                  DATA(lv_fs_tmp) = <tok>-str+14.
                  DATA(lv_fs_len) = strlen( lv_fs_tmp ) - 2.
                  IF lv_fs_len > 0.
                    lv_inline_name = lv_fs_tmp(lv_fs_len).
                  ENDIF.
                ENDIF.

                IF lv_inline_name IS NOT INITIAL.
                  DATA(lv_kw_part) = COND string(
                    WHEN lv_tok_up CP 'DATA(*)'          THEN 'DATA'
                    WHEN lv_tok_up CP 'FIELD-SYMBOL(<*)' THEN 'FIELD-SYMBOL'
                    ELSE 'DATA' ).
                  ADD 1 TO ls_unit-n1.
                  INSERT lv_kw_part INTO TABLE lt_dist_ops.
                  APPEND VALUE ts_token_detail(
                    token    = lv_kw_part
                    kind     = 'OPERATOR'
                    stmt_idx = lv_stmt_tabix
                    tok_idx  = sy-tabix
                    row      = <tok>-row
                  ) TO ls_unit-token_detail.
                  ADD 1 TO ls_unit-n2.
                  INSERT to_upper( lv_inline_name ) INTO TABLE lt_dist_opd.
                  APPEND VALUE ts_token_detail(
                    token    = lv_inline_name
                    kind     = 'OPERAND'
                    stmt_idx = lv_stmt_tabix
                    tok_idx  = sy-tabix
                    row      = <tok>-row
                  ) TO ls_unit-token_detail.
                ELSE.
                  DATA(lv_kind) = classify_token(
                    i_token     = <tok>-str
                    i_is_first  = lv_is_first
                    it_operands = lt_operands ).

                  IF lv_kind = 'OPERATOR'.
                    ADD 1 TO ls_unit-n1.
                    INSERT <tok>-str INTO TABLE lt_dist_ops.
                  ELSE.
                    ADD 1 TO ls_unit-n2.
                    INSERT <tok>-str INTO TABLE lt_dist_opd.
                  ENDIF.

                  APPEND VALUE ts_token_detail(
                    token    = <tok>-str
                    kind     = lv_kind
                    stmt_idx = lv_stmt_tabix
                    tok_idx  = sy-tabix
                    row      = <tok>-row
                  ) TO ls_unit-token_detail.
                ENDIF.

              ENDIF.
            ENDLOOP. " tokens
          ENDIF.
        ENDLOOP. " statements

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

        " Maintainability Index
        " MI = 171 - 5.2 * ln(V) - 0.23 * G - 16.2 * ln(LOC)
        IF ls_unit-volume > 0 AND ls_unit-loc > 0.
          DATA(lv_ln_vol) = log( ls_unit-volume ).
          DATA(lv_ln_loc) = log( CONV f( ls_unit-loc ) ).
          DATA(lv_cc_f)   = CONV f( ls_unit-cyclomatic ).
          ls_unit-mi = 171
            - ( CONV f( '5.2'  ) * lv_ln_vol )
            - ( CONV f( '0.23' ) * lv_cc_f   )
            - ( CONV f( '16.2' ) * lv_ln_loc ).
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
    " Operands = all known named identifiers:
    "   1. Variable/field-symbol names from t_vars (this include)
    "   2. Parameter names from t_params (this include)
    "   3. Unit names (eventname) from tt_calls_line (this include)
    "   4. Class/interface names from tt_class_defs
    " No scope filtering by class/method — a token found in code is checked
    " against these sets by name only.

    " Operands = all known named identifiers:
    "   1. Variable/field-symbol names from t_vars (this include)
    "   2. Parameter names from t_params (this include)
    "   3. Unit names (eventname) from tt_calls_line (this include)
    "   4. Class/interface names from tt_class_defs
    " No scope filtering by class/method — a token found in code is checked
    " against these sets by name only.

    FIELD-SYMBOLS: <ls_v>  LIKE LINE OF is_parse_data-t_vars,
                  <ls_p>  LIKE LINE OF is_parse_data-t_params,
                  <ls_cl> LIKE LINE OF is_parse_data-tt_calls_line,
                  <ls_cd> LIKE LINE OF is_parse_data-tt_class_defs.

    LOOP AT is_parse_data-t_vars ASSIGNING <ls_v>
      WHERE include = i_include.
      IF <ls_v>-name IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_v>-name ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

    LOOP AT is_parse_data-t_params ASSIGNING <ls_p>
      WHERE include = i_include.
      IF <ls_p>-param IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_p>-param ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

    LOOP AT is_parse_data-tt_calls_line ASSIGNING <ls_cl>
      WHERE include = i_include.
      IF <ls_cl>-eventname IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_cl>-eventname ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

    LOOP AT is_parse_data-tt_class_defs ASSIGNING <ls_cd>.
      IF <ls_cd>-class IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_cd>-class ) ) INTO TABLE rt_ops.
      ENDIF.
      IF <ls_cd>-super IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_cd>-super ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD classify_token.


    IF line_exists( mt_statements[ table_line = i_token ] ).
      rv_kind = 'OPERATOR'.
    else.
      rv_kind = 'OPERAND'.
    ENDIF.

  ENDMETHOD.


  METHOD fill_statements.
    mt_statements = VALUE tt_abap_statements(
      ( `ABS` )
      ( `ABSTRACT` )
      ( `ADD` )
      ( `ADD-CORRESPONDING` )
      ( `ALIASES` )
      ( `ALL` )
      ( `AND` )
      ( `ANY` )
      ( `APPEND` )
      ( `APPENDING` )
      ( `AS` )
      ( `ASCENDING` )
      ( `ASSERT` )
      ( `ASSIGN` )
      ( `ASSIGNING` )
      ( `AT` )
      ( `AUTHORITY-CHECK` )
      ( `AVG` )
      ( `BEGIN` )
      ( `BETWEEN` )
      ( `BINARY` )
      ( `BREAK-POINT` )
      ( `BUFFER` )
      ( `BY` )
      ( `BYPASSING` )
      ( `CALL` )
      ( `CASE` )
      ( `CAST` )
      ( `CATCH` )
      ( `CEIL` )
      ( `CHANGING` )
      ( `CHECK` )
      ( `CLASS` )
      ( `CLASS-DATA` )
      ( `CLASS-EVENTS` )
      ( `CLASS-METHODS` )
      ( `CLEANUP` )
      ( `CLEAR` )
      ( `CLIENT` )
      ( `CLOSE` )
      ( `COALESCE` )
      ( `COLLECT` )
      ( `COMMIT` )
      ( `COMMUNICATION` )
      ( `COMPARING` )
      ( `COMPUTE` )
      ( `CONCAT` )
      ( `CONCATENATE` )
      ( `CONDENSE` )
      ( `COND` )
      ( `CONNECTION` )
      ( `CONSTANTS` )
      ( `CONTINUE` )
      ( `CONTROLS` )
      ( `CONV` )
      ( `CORRESPONDING` )
      ( `COUNT` )
      ( `CREATE` )
      ( `CURRENT` )
      ( `CURSOR` )
      ( `DATA` )
      ( `DATASET` )
      ( `DEFAULT` )
      ( `DEFINE` )
      ( `DEFINITION` )
      ( `DELETE` )
      ( `DESCENDING` )
      ( `DESCRIBE` )
      ( `DIALOG` )
      ( `DISTINCT` )
      ( `DISTANCE` )
      ( `DIVIDE` )
      ( `DIVIDE-CORRESPONDING` )
      ( `DO` )
      ( `DYNPRO` )
      ( `ELSE` )
      ( `ELSEIF` )
      ( `ENCODING` )
      ( `END` )
      ( `END-OF-PAGE` )
      ( `END-OF-SELECTION` )
      ( `ENDCASE` )
      ( `ENDCLASS` )
      ( `ENDDO` )
      ( `ENDFORM` )
      ( `ENDFUNCTION` )
      ( `ENDIF` )
      ( `ENDINTERFACE` )
      ( `ENDLOOP` )
      ( `ENDMETHOD` )
      ( `ENDMODULE` )
      ( `ENDON` )
      ( `ENDPROVIDE` )
      ( `ENDSELECT` )
      ( `ENDTRY` )
      ( `ENDWHILE` )
      ( `ENTRIES` )
      ( `ESCAPE` )
      ( `EVENT` )
      ( `EVENTS` )
      ( `EXACT` )
      ( `EXCEPT` )
      ( `EXCEPTIONS` )
      ( `EXISTS` )
      ( `EXIT` )
      ( `EXPORT` )
      ( `EXPORTING` )
      ( `EXTRACT` )
      ( `FETCH` )
      ( `FIELD-GROUPS` )
      ( `FIELD-SYMBOLS` )
      ( `FIELDS` )
      ( `FILTER` )
      ( `FINAL` )
      ( `FIND` )
      ( `FIRST` )
      ( `FLOOR` )
      ( `FOR` )
      ( `FORM` )
      ( `FORMAT` )
      ( `FREE` )
      ( `FROM` )
      ( `FULL` )
      ( `FUNCTION` )
      ( `FUNCTION-POOL` )
      ( `GENERATE` )
      ( `GET` )
      ( `GROUP` )
      ( `HANDLER` )
      ( `HASHED` )
      ( `HAVING` )
      ( `HEADER` )
      ( `IF` )
      ( `IMPLEMENTATION` )
      ( `IMPORT` )
      ( `IMPORTING` )
      ( `IN` )
      ( `INCLUDE` )
      ( `INDEX` )
      ( `INFOTYPES` )
      ( `INHERITING` )
      ( `INITIAL` )
      ( `INITIALIZATION` )
      ( `INNER` )
      ( `INPUT` )
      ( `INSERT` )
      ( `INSTANCE` )
      ( `INSTR` )
      ( `INTERFACE` )
      ( `INTERFACES` )
      ( `INTERSECT` )
      ( `INTO` )
      ( `IS` )
      ( `JOIN` )
      ( `KEY` )
      ( `LAST` )
      ( `LEAVE` )
      ( `LEFT` )
      ( `LENGTH` )
      ( `LET` )
      ( `LIKE` )
      ( `LINE` )
      ( `LINE-SELECTION` )
      ( `LINES` )
      ( `LIST-PROCESSING` )
      ( `LOAD-OF-PROGRAM` )
      ( `LOCAL` )
      ( `LOG-POINT` )
      ( `LOOP` )
      ( `LOWER` )
      ( `LPAD` )
      ( `MATCH` )
      ( `MAX` )
      ( `MESSAGE` )
      ( `METHOD` )
      ( `METHODS` )
      ( `MIN` )
      ( `MODE` )
      ( `MODIFY` )
      ( `MODULE` )
      ( `MOVE` )
      ( `MOVE-CORRESPONDING` )
      ( `MULTIPLY` )
      ( `MULTIPLY-CORRESPONDING` )
      ( `NEW` )
      ( `NEW-LINE` )
      ( `NEW-PAGE` )
      ( `NEXT` )
      ( `NON-UNIQUE` )
      ( `NOT` )
      ( `NULL` )
      ( `OBJECT` )
      ( `OF` )
      ( `OFFSET` )
      ( `ON` )
      ( `OPEN` )
      ( `OPTIONAL` )
      ( `OR` )
      ( `ORDER` )
      ( `OTHERS` )
      ( `OUTER` )
      ( `OUTPUT` )
      ( `OVERLAY` )
      ( `PACK` )
      ( `PACKAGE` )
      ( `PARAMETER` )
      ( `PARAMETERS` )
      ( `PERFORM` )
      ( `PF-STATUS` )
      ( `POOL` )
      ( `PRIVATE` )
      ( `PROCESS` )
      ( `PROGRAM` )
      ( `PROTECTED` )
      ( `PROVIDE` )
      ( `PUBLIC` )
      ( `RAISE` )
      ( `RAISING` )
      ( `RANGES` )
      ( `READ` )
      ( `RECEIVE` )
      ( `REDUCE` )
      ( `REF` )
      ( `REFERENCE` )
      ( `REFRESH` )
      ( `REPLACE` )
      ( `REPORT` )
      ( `RESERVE` )
      ( `RESUME` )
      ( `RETRY` )
      ( `RETURN` )
      ( `RETURNING` )
      ( `RIGHT` )
      ( `ROLLBACK` )
      ( `ROUND` )
      ( `ROWS` )
      ( `RPAD` )
      ( `RUN` )
      ( `SCREEN` )
      ( `SCROLL` )
      ( `SEARCH` )
      ( `SECONDS` )
      ( `SECTION` )
      ( `SELECT` )
      ( `SELECT-OPTIONS` )
      ( `SELECTION-SCREEN` )
      ( `SET` )
      ( `SHIFT` )
      ( `SINGLE` )
      ( `SIZE` )
      ( `SKIP` )
      ( `SOME` )
      ( `SORT` )
      ( `SORTED` )
      ( `SPECIFIED` )
      ( `SPLIT` )
      ( `STABLE` )
      ( `STANDARD` )
      ( `START-OF-SELECTION` )
      ( `STATICS` )
      ( `STOP` )
      ( `STRUCTURE` )
      ( `SUBMIT` )
      ( `SUBSTR` )
      ( `SUBSTRING` )
      ( `SUBTRACT` )
      ( `SUBTRACT-CORRESPONDING` )
      ( `SUBROUTINE` )
      ( `SUM` )
      ( `SUPPRESS` )
      ( `SWITCH` )
      ( `TABLE` )
      ( `TABLES` )
      ( `THEN` )
      ( `TIME` )
      ( `TITLEBAR` )
      ( `TO` )
      ( `TO_LOWER` )
      ( `TO_UPPER` )
      ( `TOP-OF-PAGE` )
      ( `TRANSACTION` )
      ( `TRANSFER` )
      ( `TRANSLATE` )
      ( `TRANSPORTING` )
      ( `TRY` )
      ( `TYPE` )
      ( `TYPE-POOL` )
      ( `TYPE-POOLS` )
      ( `TYPES` )
      ( `UNASSIGN` )
      ( `UNION` )
      ( `UNIQUE` )
      ( `UNPACK` )
      ( `UNTIL` )
      ( `UP` )
      ( `UPDATE` )
      ( `UPPER` )
      ( `ULINE` )
      ( `USER-COMMAND` )
      ( `USING` )
      ( `VALUE` )
      ( `WAIT` )
      ( `WHEN` )
      ( `WHERE` )
      ( `WHILE` )
      ( `WITH` )
      ( `WINDOW` )
      ( `WORK` )
      ( `WRITE` )
    ).

  ENDMETHOD.
ENDCLASS.
