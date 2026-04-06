CLASS zcl_ace_parse_calcs DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

protected section.
  PRIVATE SECTION.
    DATA mv_eventtype TYPE string.
    DATA mv_eventname TYPE string.
    DATA mv_class     TYPE string.
    DATA mv_in_impl   TYPE abap_bool.

    CLASS-METHODS is_varname
      IMPORTING i_str         TYPE string
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    METHODS append_calc
      IMPORTING i_name      TYPE string
                i_program   TYPE program
                i_include   TYPE program
                i_class     TYPE string
                i_eventtype TYPE string
                i_eventname TYPE string
                i_line      TYPE i
      CHANGING  cs_source   TYPE zif_ace_parse_data=>ts_parse_data.

    METHODS append_comp
      IMPORTING i_name      TYPE string
                i_program   TYPE program
                i_include   TYPE program
                i_class     TYPE string
                i_eventtype TYPE string
                i_eventname TYPE string
                i_line      TYPE i
      CHANGING  cs_source   TYPE zif_ace_parse_data=>ts_parse_data.

ENDCLASS.



CLASS ZCL_ACE_PARSE_CALCS IMPLEMENTATION.


  METHOD zif_ace_stmt_handler~handle.
    CHECK i_stmt_idx > 0.

    DATA(lv_is_call_method) = abap_false.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    DATA(lv_stmt_kw) = SWITCH string( ls_stmt-type
      WHEN 'A' THEN '+CALL_METHOD'
      ELSE '' ).
    IF lv_stmt_kw = '+CALL_METHOD'. lv_is_call_method = abap_true. ENDIF.

    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
    CHECK sy-subrc = 0.

    DATA(lv_line) = ls_kw-row.

    " ── Ищем первый '=' ──────────────────────────────────────────
    DATA lv_eq_idx  TYPE i VALUE 0.
    DATA lv_tok_pos TYPE i.
    lv_tok_pos = ls_stmt-from.
    WHILE lv_tok_pos <= ls_stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_pos INTO DATA(ls_tok).
      IF sy-subrc <> 0. EXIT. ENDIF.
      IF ls_tok-str = '='. lv_eq_idx = lv_tok_pos. EXIT. ENDIF.
      lv_tok_pos += 1.
    ENDWHILE.
    CHECK lv_eq_idx > 0 OR lv_is_call_method = abap_true.

    IF lv_is_call_method = abap_false.
    " ── LHS → t_calculated ───────────────────────────────────────
    lv_tok_pos = ls_stmt-from.
    WHILE lv_tok_pos < lv_eq_idx.
      READ TABLE io_scan->tokens INDEX lv_tok_pos INTO ls_tok.
      IF sy-subrc <> 0. EXIT. ENDIF.
      DATA(lv_lhs) = ls_tok-str.
      REPLACE ALL OCCURRENCES OF 'DATA('         IN lv_lhs WITH ''.
      REPLACE ALL OCCURRENCES OF 'FIELD-SYMBOL(' IN lv_lhs WITH ''.
      REPLACE ALL OCCURRENCES OF 'FINAL('        IN lv_lhs WITH ''.
      REPLACE ALL OCCURRENCES OF ')'             IN lv_lhs WITH ''.
      CONDENSE lv_lhs NO-GAPS.
      IF lv_lhs CS '-'.
        DATA lv_dummy TYPE string.
        SPLIT lv_lhs AT '-' INTO lv_lhs lv_dummy.
      ENDIF.
      IF is_varname( lv_lhs ) = abap_true.
        append_calc( EXPORTING i_name      = lv_lhs
                               i_program   = i_program
                               i_include   = i_include
                               i_class     = i_class
                               i_eventtype = i_evtype
                               i_eventname = i_ev_name
                               i_line      = lv_line
                     CHANGING  cs_source   = cs_source ).
      ENDIF.
      lv_tok_pos += 1.
    ENDWHILE.

    " ── RHS → t_composed (только переменные вне вызовов) ─────────
    DATA lv_prev_arrow  TYPE abap_bool.
    DATA lv_skip_next   TYPE abap_bool.
    DATA lv_call_depth  TYPE i VALUE 0.
    lv_tok_pos = lv_eq_idx + 1.
    WHILE lv_tok_pos <= ls_stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_pos INTO ls_tok.
      IF sy-subrc <> 0. EXIT. ENDIF.
      DATA(lv_rhs) = ls_tok-str.

      IF ( lv_rhs CS '->' OR lv_rhs CS '=>' ) AND lv_rhs CS '('.
        lv_call_depth += 1.
        lv_prev_arrow = abap_false.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      IF lv_rhs = '->' OR lv_rhs = '=>'.
        lv_prev_arrow = abap_true.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      IF lv_prev_arrow = abap_true.
        lv_prev_arrow = abap_false.
        IF lv_rhs CS '('.
          READ TABLE io_scan->tokens INDEX lv_tok_pos - 2 INTO DATA(ls_prev_obj).
          IF sy-subrc = 0.
            DELETE cs_source-t_composed WHERE program = i_program
              AND include = i_include AND class = i_class
              AND eventtype = i_evtype AND eventname = i_ev_name
              AND line = lv_line AND name = ls_prev_obj-str.
          ENDIF.
          lv_call_depth += 1.
        ENDIF.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      IF lv_rhs CS '(' AND NOT lv_rhs = '('.
        IF NOT ( lv_rhs CP 'DATA(*' OR lv_rhs CP 'FIELD-SYMBOL(*' OR
                 lv_rhs CP 'FINAL(*' OR lv_rhs CP 'VALUE(*' OR
                 lv_rhs CP 'CONV(*'  OR lv_rhs CP 'CAST(*'  OR
                 lv_rhs CP 'COND(*'  OR lv_rhs CP 'SWITCH(*' OR
                 lv_rhs CP 'REF(*' ).
          lv_call_depth += 1.
          lv_tok_pos += 1. CONTINUE.
        ENDIF.
      ENDIF.

      DATA(lv_close_count) = strlen( lv_rhs )
        - strlen( replace( val = lv_rhs sub = ')' with = '' occ = 0 ) ).
      IF lv_close_count > 0.
        IF lv_call_depth >= lv_close_count.
          lv_call_depth -= lv_close_count.
        ELSE.
          lv_call_depth = 0.
        ENDIF.
      ENDIF.

      IF lv_call_depth > 0.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      IF lv_rhs = 'NEW' OR lv_rhs = 'CAST' OR lv_rhs = 'REF'.
        lv_skip_next = abap_true.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.
      IF lv_skip_next = abap_true.
        lv_skip_next = abap_false.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      DATA(lv_comp) = lv_rhs.
      REPLACE ALL OCCURRENCES OF ')' IN lv_comp WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN lv_comp WITH ''.
      CONDENSE lv_comp NO-GAPS.
      IF lv_comp CS '-'.
        SPLIT lv_comp AT '-' INTO lv_comp lv_dummy.
      ENDIF.
      IF is_varname( lv_comp ) = abap_true.
        append_comp( EXPORTING i_name      = lv_comp
                               i_program   = i_program
                               i_include   = i_include
                               i_class     = i_class
                               i_eventtype = i_evtype
                               i_eventname = i_ev_name
                               i_line      = lv_line
                     CHANGING  cs_source   = cs_source ).
      ENDIF.
      lv_tok_pos += 1.
    ENDWHILE.

    ENDIF. " lv_is_call_method = abap_false

    " ── TT_CALLS BINDINGS → t_composed / t_calculated ────────────
    LOOP AT cs_source-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE include = i_include.
      READ TABLE <prog>-t_keywords WITH KEY index = i_stmt_idx
        ASSIGNING FIELD-SYMBOL(<kword>) BINARY SEARCH.
      IF sy-subrc <> 0.
       " MESSAGE |CALCS: stmt={ i_stmt_idx } line={ lv_line } — keyword NOT FOUND in t_keywords| TYPE 'I'.
        EXIT.
      ENDIF.

      DATA(lv_calls_cnt) = lines( <kword>-tt_calls ).
      "MESSAGE |CALCS: stmt={ i_stmt_idx } line={ lv_line } kw={ <kword>-name } calls={ lv_calls_cnt }| TYPE 'I'.

      LOOP AT <kword>-tt_calls INTO DATA(ls_call).
        DATA(lv_bind_cnt) = lines( ls_call-bindings ).
        DATA(lv_has_e_bind) = abap_false.
        LOOP AT ls_call-bindings INTO DATA(ls_bind).
          DATA(lv_outer) = ls_bind-outer.
          CHECK lv_outer IS NOT INITIAL.
          REPLACE ALL OCCURRENCES OF ')' IN lv_outer WITH ''.
          CONDENSE lv_outer NO-GAPS.
          IF lv_outer CS '-'.
            SPLIT lv_outer AT '-' INTO lv_outer lv_dummy.
          ENDIF.
          CHECK is_varname( lv_outer ) = abap_true.
          CASE ls_bind-dir.
            WHEN 'I'.
              append_comp( EXPORTING i_name      = lv_outer
                                     i_program   = i_program
                                     i_include   = i_include
                                     i_class     = i_class
                                     i_eventtype = i_evtype
                                     i_eventname = i_ev_name
                                     i_line      = lv_line
                           CHANGING  cs_source   = cs_source ).
            WHEN 'E' OR 'C'.
              lv_has_e_bind = abap_true.
              append_comp( EXPORTING i_name      = lv_outer
                                     i_program   = i_program
                                     i_include   = i_include
                                     i_class     = i_class
                                     i_eventtype = i_evtype
                                     i_eventname = i_ev_name
                                     i_line      = lv_line
                           CHANGING  cs_source   = cs_source ).
              append_calc( EXPORTING i_name      = lv_outer
                                     i_program   = i_program
                                     i_include   = i_include
                                     i_class     = i_class
                                     i_eventtype = i_evtype
                                     i_eventname = i_ev_name
                                     i_line      = lv_line
                           CHANGING  cs_source   = cs_source ).
          ENDCASE.
        ENDLOOP.
        " Нет binding dir='E' — вызов встроен в выражение (rv = A * meth(...)).
        " Добавляем RETURNING-параметр метода в t_calculated чтобы
        " propagate_vars_backward мог по нему найти входные параметры.
        IF lv_has_e_bind = abap_false.
          DATA(lv_ret_cls) = COND string(
            WHEN ls_call-class IS NOT INITIAL THEN ls_call-class
            ELSE i_class ).
          LOOP AT cs_source-t_params INTO DATA(ls_ret_p)
            WHERE class = lv_ret_cls
              AND event = 'METHOD'
              AND name  = ls_call-name
              AND type  = 'R'.
            append_calc( EXPORTING i_name      = ls_ret_p-param
                                   i_program   = i_program
                                   i_include   = i_include
                                   i_class     = i_class
                                   i_eventtype = i_evtype
                                   i_eventname = i_ev_name
                                   i_line      = lv_line
                         CHANGING  cs_source   = cs_source ).
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_varname.
    CHECK i_str IS NOT INITIAL.
    CHECK NOT ( i_str CO '0123456789+-*/%&|<>=!()[]{}.,;:' ).
    CHECK i_str+0(1) <> ''''.
    CHECK i_str+0(1) <> '`'.
    CHECK i_str+0(1) <> '|'.
    CHECK i_str+0(1) <> '#'.
    CASE to_upper( i_str ).
      WHEN 'ABAP_TRUE' OR 'ABAP_FALSE' OR 'ABAP_ON' OR 'ABAP_OFF'
        OR 'SPACE' OR 'TRUE' OR 'FALSE'
        OR 'IF' OR 'ELSE' OR 'ENDIF' OR 'AND' OR 'OR' OR 'NOT'
        OR 'WHEN' OR 'THEN' OR 'COND' OR 'SWITCH' OR 'VALUE' OR 'CONV'
        OR 'LINES' OR 'LINE_INDEX' OR 'LINE_EXISTS'
        OR 'STRLEN' OR 'XSTRLEN' OR 'NUMOFCHAR'
        OR 'TO' OR 'FROM' OR 'IN' OR 'OF' OR 'BY' OR 'UP'
        OR 'EQ' OR 'NE' OR 'LT' OR 'LE' OR 'GT' OR 'GE'
        OR 'CO' OR 'CN' OR 'CA' OR 'NA' OR 'CS' OR 'NS' OR 'CP' OR 'NP'
        OR 'BIT-AND' OR 'BIT-OR' OR 'BIT-XOR' OR 'BIT-NOT'
        OR 'INITIAL' OR 'BOUND' OR 'SUPPLIED' OR 'REQUESTED'
        OR 'IS' OR 'BETWEEN' OR 'NEW' OR 'CAST' OR 'REF'
        OR 'DATA' OR 'FIELD-SYMBOL' OR 'FINAL' OR 'TABLE'
        OR 'EXACT' OR 'BASE' OR 'CORRESPONDING'
        OR 'XSDBOOL' OR 'BOOLC' OR 'BOOLX'
        OR 'ME' OR 'SUPER' OR 'RESULT'.
        rv_yes = abap_false. RETURN.
    ENDCASE.
    rv_yes = abap_true.
  ENDMETHOD.


  METHOD append_calc.
    " Дедупликация делается в GET_CODE_FLOW через SORT + DELETE ADJACENT DUPLICATES
    APPEND VALUE zcl_ace=>ts_var(
      program = i_program include = i_include
      class = i_class eventtype = i_eventtype eventname = i_eventname
      line = i_line name = i_name )
      TO cs_source-t_calculated.

  ENDMETHOD.


  METHOD append_comp.
    " Дедупликация делается в GET_CODE_FLOW через SORT + DELETE ADJACENT DUPLICATES
    APPEND VALUE zcl_ace=>ts_var(
      program = i_program include = i_include
      class = i_class eventtype = i_eventtype eventname = i_eventname
      line = i_line name = i_name )
      TO cs_source-t_composed.

  ENDMETHOD.
ENDCLASS.
