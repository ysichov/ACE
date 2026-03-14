CLASS zcl_ace_parse_calls DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

  PRIVATE SECTION.

    DATA mv_class_name TYPE string.
    DATA mv_event_type TYPE string.
    DATA mv_event_name TYPE string.
    DATA mv_in_impl    TYPE abap_bool.

    " Superclass cache — valid for mv_class_name
    DATA mv_super_cls  TYPE string.
    DATA mv_super      TYPE string.

    CLASS-METHODS resolve_var_type
      IMPORTING
        is_source      TYPE zcl_ace_window=>ts_source
        i_program      TYPE program
        i_evtype       TYPE string
        i_evname       TYPE string
        i_varname      TYPE string
      RETURNING
        VALUE(rv_type) TYPE string.

    METHODS get_super
      IMPORTING
        is_source       TYPE zcl_ace_window=>ts_source
      RETURNING
        VALUE(rv_super) TYPE string.

    METHODS parse_stmt_calls
      IMPORTING
        io_scan    TYPE REF TO cl_ci_scan
        i_stmt_idx TYPE i
        i_program  TYPE program
        i_include  TYPE program
      CHANGING
        cs_source  TYPE zcl_ace_window=>ts_source.

ENDCLASS.


CLASS zcl_ace_parse_calls IMPLEMENTATION.

  METHOD zif_ace_stmt_handler~handle.

    CHECK i_stmt_idx > 0.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
    CHECK sy-subrc = 0.

    CASE ls_kw-str.

      WHEN 'CLASS' OR 'INTERFACE'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_name).
        IF sy-subrc = 0.
          IF mv_class_name <> ls_name-str.
            CLEAR: mv_super_cls, mv_super.
          ENDIF.
          mv_class_name = ls_name-str.
          mv_in_impl    = abap_false.
          IF ls_kw-str = 'CLASS'.
            LOOP AT io_scan->tokens FROM ls_stmt-from TO ls_stmt-to INTO DATA(ls_t).
              IF ls_t-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. RETURN. ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
        RETURN.

      WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
        CLEAR: mv_class_name, mv_in_impl, mv_event_type, mv_event_name.
        CLEAR: mv_super_cls, mv_super.
        RETURN.

      WHEN 'METHOD'.
        mv_event_type = 'METHOD'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
        IF sy-subrc = 0. mv_event_name = ls_name-str. ENDIF.
        RETURN.

      WHEN 'ENDMETHOD'.
        CLEAR: mv_event_type, mv_event_name.
        RETURN.

      WHEN 'FORM'.
        mv_event_type = 'FORM'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
        IF sy-subrc = 0. mv_event_name = ls_name-str. ENDIF.
        RETURN.

      WHEN 'ENDFORM'.
        CLEAR: mv_event_type, mv_event_name.
        RETURN.

      WHEN 'FUNCTION'.
        mv_event_type = 'FUNCTION'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
        IF sy-subrc = 0.
          mv_event_name = ls_name-str.
          REPLACE ALL OCCURRENCES OF '''' IN mv_event_name WITH ''.
        ENDIF.
        RETURN.

      WHEN 'ENDFUNCTION'.
        CLEAR: mv_event_type, mv_event_name.
        RETURN.

      WHEN 'MODULE'.
        mv_event_type = 'MODULE'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
        IF sy-subrc = 0. mv_event_name = ls_name-str. ENDIF.
        RETURN.

      WHEN 'ENDMODULE'.
        CLEAR: mv_event_type, mv_event_name.
        RETURN.

      WHEN 'START-OF-SELECTION' OR 'END-OF-SELECTION'
        OR 'INITIALIZATION' OR 'TOP-OF-PAGE' OR 'END-OF-PAGE'
        OR 'AT' OR 'GET'.
        mv_event_type = 'EVENT'.
        mv_event_name = ls_kw-str.
        RETURN.

    ENDCASE.

    CHECK mv_event_type IS NOT INITIAL.

    parse_stmt_calls(
      EXPORTING io_scan    = io_scan
                i_stmt_idx = i_stmt_idx
                i_program  = i_program
                i_include  = i_include
      CHANGING  cs_source  = cs_source ).

  ENDMETHOD.


  METHOD get_super.
    CHECK mv_class_name IS NOT INITIAL.
    IF mv_super_cls = mv_class_name.
      rv_super = mv_super.
      RETURN.
    ENDIF.
    READ TABLE is_source-tt_class_defs WITH KEY class = mv_class_name
      INTO DATA(ls_cd).
    IF sy-subrc = 0 AND ls_cd-super IS NOT INITIAL.
      rv_super = ls_cd-super.
    ELSE.
      SELECT SINGLE refclsname FROM seometarel INTO @rv_super
        WHERE clsname = @mv_class_name AND reltype = '1'.
    ENDIF.
    mv_super_cls = mv_class_name.
    mv_super     = rv_super.
  ENDMETHOD.


  METHOD resolve_var_type.
    " t_vars is pre-sorted by (program, eventtype, eventname, name)
    " before this pass runs — so READ with BINARY SEARCH is O(log n).

    " 1. Local scope
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               eventtype = i_evtype
               eventname = i_evname
               name      = i_varname
      INTO DATA(ls_var)
      BINARY SEARCH.
    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
      RETURN.
    ENDIF.

    " 2. Class attributes (eventtype = '', eventname = '')
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               eventtype = ''
               eventname = ''
               name      = i_varname
      INTO ls_var
      BINARY SEARCH.
    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
      RETURN.
    ENDIF.

    " 3. Globals (class = '', eventtype = '', eventname = '')
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               class     = ''
               eventtype = ''
               eventname = ''
               name      = i_varname
      INTO ls_var
      BINARY SEARCH.
    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
    ENDIF.
  ENDMETHOD.


  METHOD parse_stmt_calls.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw_tok).
    CHECK sy-subrc = 0.

    DATA(lv_kw) = SWITCH string( ls_stmt-type
      WHEN 'C' THEN 'COMPUTE'
      WHEN 'D' THEN 'COMPUTE'
      WHEN 'A' THEN '+CALL_METHOD'
      ELSE          ls_kw_tok-str ).

    IF lv_kw = 'CALL'.
      READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok2).
      IF sy-subrc = 0. lv_kw = |CALL { ls_tok2-str }|. ENDIF.
    ENDIF.

    DATA(lv_super) = get_super( is_source = cs_source ).

    DATA lt_new_calls TYPE zcl_ace=>tt_calls.

    " ── helper: resolve RETURNING binding lhs ──────────────────────
    " Looks backwards from token i_from for pattern  lv_x = or DATA(lv_x) =
    " Returns the variable name that receives the result.
    DATA lv_lhs TYPE string.

    " ── helper: find preferred/single importing param for a method ──
    " Returns formal parameter name from t_params for class/method.
    " Used when call has a single positional argument.
    DATA lv_pref_param TYPE string.

    CASE lv_kw.

      WHEN 'PERFORM'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
        CHECK sy-subrc = 0.

        DATA ls_pf_call   TYPE zcl_ace=>ts_calls.
        DATA lv_pf_sec    TYPE string.
        DATA lv_pf_act_i  TYPE i.
        DATA ls_pf_bind   TYPE zcl_ace=>ts_param_binding.

        ls_pf_call-event = 'FORM'.
        ls_pf_call-name  = ls_tok-str.

        " Собираем фактические аргументы по порядку (USING / CHANGING / TABLES)
        DATA lt_pf_actuals TYPE string_table.
        DATA(lv_pf_i) = ls_stmt-from + 2.
        WHILE lv_pf_i <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_pf_i INTO DATA(ls_pf_t).
          IF sy-subrc <> 0. EXIT. ENDIF.
          CASE ls_pf_t-str.
            WHEN 'USING' OR 'CHANGING' OR 'TABLES'.
              lv_pf_sec = ls_pf_t-str.
            WHEN OTHERS.
              IF lv_pf_sec IS NOT INITIAL AND ls_pf_t-str IS NOT INITIAL.
                APPEND ls_pf_t-str TO lt_pf_actuals.
              ENDIF.
          ENDCASE.
          lv_pf_i += 1.
        ENDWHILE.

        " Сопоставляем с формальными параметрами из t_params по порядку
        DATA lt_pf_params TYPE TABLE OF zcl_ace=>ts_params WITH EMPTY KEY.
        lt_pf_params = VALUE #(
          FOR p IN cs_source-t_params
          WHERE ( event = 'FORM' AND name = ls_pf_call-name )
          ( p ) ).
        SORT lt_pf_params BY line.

        lv_pf_act_i = 1.
        LOOP AT lt_pf_params INTO DATA(ls_pf_p).
          READ TABLE lt_pf_actuals INDEX lv_pf_act_i INTO DATA(lv_pf_act).
          CLEAR ls_pf_bind.
          ls_pf_bind-outer = lv_pf_act.       " фактический (пусто если кончились)
          ls_pf_bind-inner = ls_pf_p-param.   " формальный
          APPEND ls_pf_bind TO ls_pf_call-bindings.
          lv_pf_act_i += 1.
        ENDLOOP.

        " Если формальных параметров нет — всё равно пишем outer-переменные
        IF lt_pf_params IS INITIAL.
          LOOP AT lt_pf_actuals INTO DATA(lv_pf_only).
            CLEAR ls_pf_bind.
            ls_pf_bind-outer = lv_pf_only.
            ls_pf_bind-inner = ''.
            APPEND ls_pf_bind TO ls_pf_call-bindings.
          ENDLOOP.
        ENDIF.

        APPEND ls_pf_call TO lt_new_calls.

      WHEN 'CALL FUNCTION'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0.
        DATA(lv_fname) = ls_tok-str.
        REPLACE ALL OCCURRENCES OF '''' IN lv_fname WITH ''.
        APPEND VALUE zcl_ace=>ts_calls(
          event = 'FUNCTION'
          name  = lv_fname
        ) TO lt_new_calls.

      WHEN 'CALL METHOD'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0 AND ls_tok-str IS NOT INITIAL.
        DATA(lv_call) = VALUE zcl_ace=>ts_calls( event = 'METHOD' ).
        DATA(lv_str)  = ls_tok-str.
        IF lv_str CS '->'.
          SPLIT lv_str AT '->' INTO lv_call-class lv_call-name.
        ELSEIF lv_str CS '=>'.
          SPLIT lv_str AT '=>' INTO lv_call-class lv_call-name.
        ELSE.
          lv_call-name = lv_str.
        ENDIF.
        REPLACE ALL OCCURRENCES OF '(' IN lv_call-name WITH ''.
        CONDENSE lv_call-name NO-GAPS.
        IF lv_call-class IS NOT INITIAL.
          IF lv_call-class = 'ME'.
            lv_call-class = mv_class_name.
          ELSEIF lv_call-class = 'SUPER'.
            lv_call-super = abap_true.
            lv_call-class = COND #( WHEN lv_super IS NOT INITIAL
                                    THEN lv_super ELSE mv_class_name ).
          ELSE.
            DATA(lv_resolved) = resolve_var_type(
              is_source = cs_source i_program = i_program
              i_evtype  = mv_event_type i_evname = mv_event_name
              i_varname = lv_call-class ).
            IF lv_resolved IS NOT INITIAL.
              lv_call-outer = lv_call-class.
              lv_call-inner = lv_call-name.
              lv_call-class = lv_resolved.
            ENDIF.
          ENDIF.
        ENDIF.

        " ── CALL METHOD bindings: линейный проход по токенам ──────────────────
        " Читаем токены слева направо: секция → формальный = фактический
        DATA(lv_section_cm) = ``.
        DATA(lv_tok_cm)     = ls_stmt-from + 3.
        WHILE lv_tok_cm <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_tok_cm INTO DATA(ls_t_cm).
          IF sy-subrc <> 0. EXIT. ENDIF.
          CASE ls_t_cm-str.
            WHEN 'EXPORTING' OR 'IMPORTING' OR 'CHANGING' OR 'RECEIVING'.
              lv_section_cm = ls_t_cm-str.
            WHEN '='.
              " skip — обрабатывается при чтении формального параметра
            WHEN OTHERS.
              IF lv_section_cm IS NOT INITIAL AND ls_t_cm-str IS NOT INITIAL.
                READ TABLE io_scan->tokens INDEX lv_tok_cm + 1 INTO DATA(ls_eq_cm).
                IF ls_eq_cm-str = '='.
                  READ TABLE io_scan->tokens INDEX lv_tok_cm + 2 INTO DATA(ls_var_cm).
                  IF sy-subrc = 0 AND ls_var_cm-str IS NOT INITIAL.
                    DATA(lv_cm_actual) = ls_var_cm-str.
                    REPLACE ALL OCCURRENCES OF ')' IN lv_cm_actual WITH ''.
                    CONDENSE lv_cm_actual NO-GAPS.
                    APPEND VALUE zcl_ace=>ts_param_binding(
                      inner = ls_t_cm-str   " формальный параметр
                      outer = lv_cm_actual  " фактический аргумент
                    ) TO lv_call-bindings.
                    lv_tok_cm += 2.  " перепрыгиваем '=' и actual
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.
          lv_tok_cm += 1.
        ENDWHILE.

        APPEND lv_call TO lt_new_calls.

      WHEN 'COMPUTE'.
        DATA lv_ci TYPE i.
        DATA ls_ct LIKE LINE OF io_scan->tokens.
        DATA ls_cn LIKE LINE OF io_scan->tokens.
        DATA lv_cn TYPE string.
        lv_ci = ls_stmt-from.
        WHILE lv_ci <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_ci INTO ls_ct.
          IF sy-subrc <> 0. EXIT. ENDIF.
          IF ls_ct-str = 'NEW'.
            READ TABLE io_scan->tokens INDEX lv_ci + 1 INTO ls_cn.
            IF sy-subrc = 0 AND ls_cn-str CS '('.
              lv_cn = ls_cn-str.
              REPLACE ALL OCCURRENCES OF '(' IN lv_cn WITH ''.
              CONDENSE lv_cn NO-GAPS.
              IF lv_cn IS NOT INITIAL.
                APPEND VALUE zcl_ace=>ts_calls(
                  event = 'METHOD' class = lv_cn name = 'CONSTRUCTOR'
                ) TO lt_new_calls.
              ENDIF.
              lv_ci += 1.
            ENDIF.
          ENDIF.
          lv_ci += 1.
        ENDWHILE.

      WHEN '+CALL_METHOD'.
        DATA lv_tstr       TYPE string.
        DATA lv_arrow      TYPE string.
        DATA lv_left_str   TYPE string.
        DATA lv_right_name TYPE string.
        DATA lv_right_part TYPE string.
        DATA lv_dummy      TYPE string.
        DATA lv_tok_idx    TYPE i.
        DATA ls_left       LIKE LINE OF io_scan->tokens.
        DATA ls_right      LIKE LINE OF io_scan->tokens.
        DATA lv_c          TYPE zcl_ace=>ts_calls.
        DATA lv_rtype      TYPE string.

        lv_tok_idx = ls_stmt-from.
        WHILE lv_tok_idx <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_tok_idx INTO DATA(ls_tok3).
          IF sy-subrc <> 0. EXIT. ENDIF.

          lv_tstr = ls_tok3-str.
          CLEAR: lv_arrow, lv_left_str, lv_right_name, lv_right_part, lv_dummy.

          IF lv_tstr CS '=>' AND lv_tstr CS '('.
            lv_arrow = '=>'.
            SPLIT lv_tstr AT '=>' INTO lv_left_str lv_right_part.
            SPLIT lv_right_part AT '(' INTO lv_right_name lv_dummy.
          ELSEIF lv_tstr CS '->' AND lv_tstr CS '('.
            lv_arrow = '->'.
            SPLIT lv_tstr AT '->' INTO lv_left_str lv_right_part.
            SPLIT lv_right_part AT '(' INTO lv_right_name lv_dummy.
          ELSEIF lv_tstr = '->' OR lv_tstr = '=>'.
            lv_arrow = lv_tstr.
            READ TABLE io_scan->tokens INDEX lv_tok_idx - 1 INTO ls_left.
            READ TABLE io_scan->tokens INDEX lv_tok_idx + 1 INTO ls_right.
            IF sy-subrc = 0 AND ls_right-str CS '(' AND NOT ls_right-str CO '()'.
              lv_left_str   = ls_left-str.
              lv_right_name = ls_right-str.
              REPLACE ALL OCCURRENCES OF '(' IN lv_right_name WITH ''.
              lv_tok_idx += 1.
            ENDIF.
          ELSEIF lv_tstr = 'NEW'.
            READ TABLE io_scan->tokens INDEX lv_tok_idx + 1 INTO ls_right.
            IF sy-subrc = 0 AND ls_right-str CS '('.
              lv_arrow      = '=>'.
              lv_left_str   = ls_right-str.
              REPLACE ALL OCCURRENCES OF '(' IN lv_left_str WITH ''.
              CONDENSE lv_left_str NO-GAPS.
              lv_right_name = 'CONSTRUCTOR'.
              lv_tok_idx += 1.
            ENDIF.
          ELSEIF lv_tstr CA '(' AND NOT lv_tstr CS '->' AND NOT lv_tstr CS '=>'.
            IF lv_tok_idx = ls_stmt-from.
              lv_arrow      = '->'.
              lv_left_str   = 'ME'.
              lv_right_name = lv_tstr.
              REPLACE ALL OCCURRENCES OF '(' IN lv_right_name WITH ''.
              CONDENSE lv_right_name NO-GAPS.
            ENDIF.
          ENDIF.

          IF lv_right_name IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF '(' IN lv_right_name WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN lv_right_name WITH ''.
            CONDENSE lv_right_name NO-GAPS.
          ENDIF.

          IF lv_arrow IS NOT INITIAL
            AND lv_left_str IS NOT INITIAL
            AND lv_right_name IS NOT INITIAL.

            CLEAR lv_c.
            lv_c-event = 'METHOD'.
            lv_c-name  = lv_right_name.

            IF lv_left_str = 'ME'.
              lv_c-class = mv_class_name.
            ELSEIF lv_left_str = 'SUPER'.
              lv_c-super = abap_true.
              lv_c-class = COND #( WHEN lv_super IS NOT INITIAL
                                   THEN lv_super ELSE mv_class_name ).
            ELSE.
              lv_rtype = resolve_var_type(
                is_source = cs_source i_program = i_program
                i_evtype  = mv_event_type i_evname = mv_event_name
                i_varname = lv_left_str ).
              IF lv_rtype IS NOT INITIAL.
                lv_c-class = lv_rtype.
                lv_c-outer = lv_left_str.
                lv_c-inner = lv_right_name.
              ELSEIF lv_arrow = '=>'.
                lv_c-class = lv_left_str.
              ELSE.
                lv_c-outer = lv_left_str.
                lv_c-inner = lv_right_name.
              ENDIF.
            ENDIF.

            " ── Bindings для +CALL_METHOD: линейный проход по токенам ───────────
            " Токены аргументов начинаются сразу после токена с '(' и идут до ')'.
            " Паттерны:
            "   named:      inner = formal,  outer = actual  (tok[i+1]='=')
            "   positional: outer = tok[i],  inner = PREFERRED/единственный IMPORTING
            "   lhs = ...:  outer = lhs,     inner = RETURNING param
            DATA lt_bindings   TYPE zcl_ace=>tt_param_bindings.
            DATA lv_single_arg TYPE string.
            DATA lv_positional TYPE abap_bool VALUE abap_true.
            DATA lv_scan_b     TYPE i.
            DATA ls_b          TYPE zcl_ace=>ts_param_binding.

            DATA(lv_call_class) = COND string(
              WHEN lv_c-class IS NOT INITIAL THEN lv_c-class ELSE mv_class_name ).

            " LHS: lv_x = obj->meth(…)  →  позиция lv_tok_idx-1 должна быть '='
            CLEAR lv_lhs.
            DATA(lv_look) = lv_tok_idx - 1.
            IF lv_look >= ls_stmt-from.
              READ TABLE io_scan->tokens INDEX lv_look INTO DATA(ls_lhs_eq).
              IF ls_lhs_eq-str = '='.
                READ TABLE io_scan->tokens INDEX lv_look - 1 INTO DATA(ls_lhs_var).
                IF sy-subrc = 0.
                  lv_lhs = ls_lhs_var-str.
                  REPLACE ALL OCCURRENCES OF 'DATA(' IN lv_lhs WITH ''.
                  REPLACE ALL OCCURRENCES OF ')'     IN lv_lhs WITH ''.
                  CONDENSE lv_lhs NO-GAPS.
                ENDIF.
              ENDIF.
            ENDIF.

            " Линейный проход по аргументам (от lv_tok_idx+1 до конца стейтмента)
            DATA lv_arg_s    TYPE string.
            DATA lv_b_actual TYPE string.
            DATA ls_eq_b     LIKE LINE OF io_scan->tokens.
            DATA ls_val_b    LIKE LINE OF io_scan->tokens.
            DATA ls_arg      LIKE LINE OF io_scan->tokens.
            lv_scan_b = lv_tok_idx + 1.
            WHILE lv_scan_b <= ls_stmt-to.
              READ TABLE io_scan->tokens INDEX lv_scan_b INTO ls_arg.
              IF sy-subrc <> 0. EXIT. ENDIF.
              lv_arg_s = ls_arg-str.

              IF lv_arg_s = ')' OR lv_arg_s CO ')'.
                EXIT.
              ENDIF.

              IF lv_arg_s = '(' OR lv_arg_s = ','.
                lv_scan_b += 1.
                CONTINUE.
              ENDIF.

              CLEAR ls_eq_b.
              READ TABLE io_scan->tokens INDEX lv_scan_b + 1 INTO ls_eq_b.
              IF ls_eq_b-str = '='.
                CLEAR ls_val_b.
                READ TABLE io_scan->tokens INDEX lv_scan_b + 2 INTO ls_val_b.
                IF sy-subrc = 0.
                  lv_positional = abap_false.
                  lv_b_actual = ls_val_b-str.
                  REPLACE ALL OCCURRENCES OF ')' IN lv_b_actual WITH ''.
                  CONDENSE lv_b_actual NO-GAPS.
                  CLEAR ls_b.
                  ls_b-inner = lv_arg_s.
                  ls_b-outer = lv_b_actual.
                  APPEND ls_b TO lt_bindings.
                  lv_scan_b += 3.
                  CONTINUE.
                ENDIF.
              ENDIF.

              IF lv_positional = abap_true AND lv_single_arg IS INITIAL
                AND lv_arg_s IS NOT INITIAL.
                lv_single_arg = lv_arg_s.
                REPLACE ALL OCCURRENCES OF ')' IN lv_single_arg WITH ''.
                CONDENSE lv_single_arg NO-GAPS.
              ENDIF.

              lv_scan_b += 1.
            ENDWHILE.

            " Позиционный единственный аргумент → ищем PREFERRED/единственный IMPORTING
            IF lv_positional = abap_true AND lv_single_arg IS NOT INITIAL.
              CLEAR lv_pref_param.
              LOOP AT cs_source-t_params INTO DATA(ls_pm)
                WHERE class = lv_call_class
                  AND event = 'METHOD'
                  AND name  = lv_c-name
                  AND type  = 'I'.
                IF ls_pm-preferred = 'X' OR lv_pref_param IS INITIAL.
                  lv_pref_param = ls_pm-param.
                ENDIF.
                IF ls_pm-preferred = 'X'. EXIT. ENDIF.
              ENDLOOP.
              IF lv_pref_param IS NOT INITIAL.
                CLEAR ls_b.
                ls_b-outer = lv_single_arg.
                ls_b-inner = lv_pref_param.
                APPEND ls_b TO lt_bindings.
              ENDIF.
            ENDIF.

            " LHS → RETURNING binding
            IF lv_lhs IS NOT INITIAL.
              DATA lv_ret_param TYPE string.
              LOOP AT cs_source-t_params INTO DATA(ls_ret)
                WHERE class = lv_call_class
                  AND event = 'METHOD'
                  AND name  = lv_c-name
                  AND type  = 'R'.
                lv_ret_param = ls_ret-param.
                EXIT.
              ENDLOOP.
              IF lv_ret_param IS NOT INITIAL.
                CLEAR ls_b.
                ls_b-outer = lv_lhs.
                ls_b-inner = lv_ret_param.
                APPEND ls_b TO lt_bindings.
              ENDIF.
            ENDIF.

            lv_c-bindings = lt_bindings.
            APPEND lv_c TO lt_new_calls.
            CLEAR: lv_single_arg, lv_positional, lv_lhs.
          ENDIF.

          lv_tok_idx += 1.
        ENDWHILE.

    ENDCASE.

    CHECK lt_new_calls IS NOT INITIAL.

    LOOP AT cs_source-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE include = i_include.
      READ TABLE <prog>-t_keywords INDEX i_stmt_idx
        ASSIGNING FIELD-SYMBOL(<kw>).
      IF sy-subrc = 0.
        LOOP AT lt_new_calls INTO DATA(ls_nc).
          READ TABLE <kw>-tt_calls WITH KEY event = ls_nc-event
                                            name  = ls_nc-name
                                            class = ls_nc-class
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_nc TO <kw>-tt_calls.
          ENDIF.
        ENDLOOP.
      ENDIF.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
