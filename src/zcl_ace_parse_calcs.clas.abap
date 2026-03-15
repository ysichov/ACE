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
      IMPORTING i_name    TYPE string
                i_program TYPE program
                i_include TYPE program
                i_line    TYPE i
      CHANGING  cs_source TYPE zcl_ace_window=>ts_source.

    METHODS append_comp
      IMPORTING i_name    TYPE string
                i_program TYPE program
                i_include TYPE program
                i_line    TYPE i
      CHANGING  cs_source TYPE zcl_ace_window=>ts_source.

ENDCLASS.



CLASS ZCL_ACE_PARSE_CALCS IMPLEMENTATION.


  METHOD zif_ace_stmt_handler~handle.
    CHECK i_stmt_idx > 0.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
    CHECK sy-subrc = 0.



    " ── Контекст ─────────────────────────────────────────────────
*    CASE ls_kw-str.
*      WHEN 'CLASS'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_n).
*        IF sy-subrc = 0. mv_class = ls_n-str. ENDIF.
*        LOOP AT io_scan->tokens FROM ls_stmt-from TO ls_stmt-to INTO DATA(ls_ct).
*          IF ls_ct-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. EXIT. ENDIF.
*        ENDLOOP.
*        RETURN.
*      WHEN 'ENDCLASS'.
*        CLEAR: mv_class, mv_in_impl, mv_eventtype, mv_eventname. RETURN.
*      WHEN 'METHOD'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_n.
*        IF sy-subrc = 0. mv_eventtype = 'METHOD'. mv_eventname = ls_n-str. ENDIF.
*        RETURN.
*      WHEN 'ENDMETHOD'. CLEAR: mv_eventtype, mv_eventname. RETURN.
*      WHEN 'FORM'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_n.
*        IF sy-subrc = 0. mv_eventtype = 'FORM'. mv_eventname = ls_n-str. ENDIF.
*        RETURN.
*      WHEN 'ENDFORM'. CLEAR: mv_eventtype, mv_eventname. RETURN.
*      WHEN 'FUNCTION'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_n.
*        IF sy-subrc = 0.
*          mv_eventtype = 'FUNCTION'. mv_eventname = ls_n-str.
*          REPLACE ALL OCCURRENCES OF '''' IN mv_eventname WITH ''.
*        ENDIF.
*        RETURN.
*      WHEN 'ENDFUNCTION'. CLEAR: mv_eventtype, mv_eventname. RETURN.
*      WHEN 'MODULE'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_n.
*        IF sy-subrc = 0. mv_eventtype = 'MODULE'. mv_eventname = ls_n-str. ENDIF.
*        RETURN.
*      WHEN 'ENDMODULE'. CLEAR: mv_eventtype, mv_eventname. RETURN.
*    ENDCASE.

    " ── Только COMPUTE (тип C и D) ───────────────────────────────
    "CHECK ls_stmt-type = 'C' OR ls_stmt-type = 'D'.

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
    CHECK lv_eq_idx > 0.

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
        append_calc( EXPORTING i_name    = lv_lhs
                               i_program = i_program
                               i_include = i_include
                               i_line    = lv_line
                     CHANGING  cs_source = cs_source ).
      ENDIF.
      lv_tok_pos += 1.
    ENDWHILE.

    " ── RHS → t_composed ─────────────────────────────────────────
    DATA lv_prev_arrow TYPE abap_bool.
    DATA lv_skip_next  TYPE abap_bool.
    lv_tok_pos = lv_eq_idx + 1.
    WHILE lv_tok_pos <= ls_stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_pos INTO ls_tok.
      IF sy-subrc <> 0. EXIT. ENDIF.
      DATA(lv_rhs) = ls_tok-str.

      " Токен содержит '->' или '=>': obj->attr / obj->meth( / cls=>meth(
      IF lv_rhs CS '->' OR lv_rhs CS '=>'.
        DATA lv_al TYPE string.
        DATA lv_ar TYPE string.
        IF lv_rhs CS '->'.
          SPLIT lv_rhs AT '->' INTO lv_al lv_ar.
          " Атрибут obj->attr (нет скобки) — obj является переменной
          IF NOT lv_ar CS '(' AND is_varname( lv_al ) = abap_true.
            append_comp( EXPORTING i_name    = lv_al
                                   i_program = i_program
                                   i_include = i_include
                                   i_line    = lv_line
                         CHANGING  cs_source = cs_source ).
          ENDIF.
          " obj->meth( — вызов, obj не нужен
        ENDIF.
        " CLS=>anything — класс, пропускаем
        lv_prev_arrow = abap_false.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      " Стрелка отдельным токеном
      IF lv_rhs = '->' OR lv_rhs = '=>'.
        lv_prev_arrow = abap_true.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      " Токен сразу после отдельной стрелки
      IF lv_prev_arrow = abap_true.
        lv_prev_arrow = abap_false.
        IF lv_rhs CS '('.
          " obj -> meth( — вызов: убираем obj, который был добавлен как обычный токен
          READ TABLE io_scan->tokens INDEX lv_tok_pos - 2 INTO DATA(ls_prev_obj).
          IF sy-subrc = 0.
            DELETE cs_source-t_composed WHERE program = i_program
              AND include = i_include AND line = lv_line AND name = ls_prev_obj-str.
          ENDIF.
        ENDIF.
        " obj -> attr — obj оставляем (добавлен ранее), атрибут не добавляем
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      " NEW/CAST/REF — следующий токен это имя типа, не переменная
      IF lv_rhs = 'NEW' OR lv_rhs = 'CAST' OR lv_rhs = 'REF'.
        lv_skip_next = abap_true.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.
      IF lv_skip_next = abap_true.
        lv_skip_next = abap_false.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      " Обычный токен — кандидат в переменную
      DATA(lv_comp) = lv_rhs.
      REPLACE ALL OCCURRENCES OF ')' IN lv_comp WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN lv_comp WITH ''.
      CONDENSE lv_comp NO-GAPS.
      IF lv_comp CS '-'.
        SPLIT lv_comp AT '-' INTO lv_comp lv_dummy.
      ENDIF.
      IF is_varname( lv_comp ) = abap_true.
        append_comp( EXPORTING i_name    = lv_comp
                               i_program = i_program
                               i_include = i_include
                               i_line    = lv_line
                     CHANGING  cs_source = cs_source ).
      ENDIF.
      lv_tok_pos += 1.
    ENDWHILE.
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
      program = i_program include = i_include line = i_line name = i_name )
      TO cs_source-t_calculated.

  ENDMETHOD.


  METHOD append_comp.
    " Дедупликация делается в GET_CODE_FLOW через SORT + DELETE ADJACENT DUPLICATES
    APPEND VALUE zcl_ace=>ts_var(
      program = i_program include = i_include line = i_line name = i_name )
      TO cs_source-t_composed.

  ENDMETHOD.
ENDCLASS.
