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

    CLASS-METHODS resolve_var_type
      IMPORTING
        is_source      TYPE zcl_ace_window=>ts_source
        i_program      TYPE program
        i_evtype       TYPE string
        i_evname       TYPE string
        i_varname      TYPE string
      RETURNING
        VALUE(rv_type) TYPE string.

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

      " Event blocks — не процедуры, но содержат исполняемый код
      WHEN 'START-OF-SELECTION' OR 'END-OF-SELECTION'
        OR 'INITIALIZATION' OR 'TOP-OF-PAGE' OR 'END-OF-PAGE'
        OR 'AT' OR 'GET'.
        mv_event_type = 'EVENT'.
        mv_event_name = ls_kw-str.
        RETURN.

    ENDCASE.

    " Parse calls в любом исполняемом контексте
    " (процедуры: METHOD/FORM/FUNCTION/MODULE, события: START-OF-SELECTION и др.)
    CHECK mv_event_type IS NOT INITIAL.

    parse_stmt_calls(
      EXPORTING io_scan    = io_scan
                i_stmt_idx = i_stmt_idx
                i_program  = i_program
                i_include  = i_include
      CHANGING  cs_source  = cs_source ).

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

    DATA lt_new_calls TYPE zcl_ace=>tt_calls.

    CASE lv_kw.

      WHEN 'PERFORM'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
        CHECK sy-subrc = 0.
        APPEND VALUE zcl_ace=>ts_calls(
          event = 'FORM'
          name  = ls_tok-str
        ) TO lt_new_calls.

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
        IF lv_call-class IS NOT INITIAL.
          IF lv_call-class = 'ME'.
            lv_call-class = mv_class_name.
          ELSEIF lv_call-class = 'SUPER'.
            lv_call-super = abap_true.
            lv_call-class = mv_class_name.
          ELSE.
            DATA(lv_resolved) = resolve_var_type(
              is_source = cs_source
              i_program = i_program
              i_evtype  = mv_event_type
              i_evname  = mv_event_name
              i_varname = lv_call-class ).
            IF lv_resolved IS NOT INITIAL.
              lv_call-outer = lv_call-class.
              lv_call-inner = lv_call-name.
              lv_call-class = lv_resolved.
            ENDIF.
          ENDIF.
        ENDIF.
        APPEND lv_call TO lt_new_calls.

      WHEN 'COMPUTE'.
        " Ищем NEW ClassName( ) — вызов конструктора
        DATA lv_ci  TYPE i.
        DATA ls_ct  LIKE LINE OF io_scan->tokens.
        DATA ls_cn  LIKE LINE OF io_scan->tokens.
        DATA lv_cn  TYPE string.
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
                  event = 'METHOD'
                  class = lv_cn
                  name  = 'CONSTRUCTOR'
                ) TO lt_new_calls.
              ENDIF.
              lv_ci += 1.
            ENDIF.
          ENDIF.
          lv_ci += 1.
        ENDWHILE.

      WHEN '+CALL_METHOD'.
        " Все переменные цикла объявлены заранее — CLEAR на каждой итерации
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
          " NEW ClassName( ) — вызов конструктора
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
          " show( ) — implicit me-> : токен заканчивается на ( без стрелки
          ELSEIF lv_tstr CA '(' AND NOT lv_tstr CS '->' AND NOT lv_tstr CS '=>'.
            " Первый токен стейтмента — это сам вызов
            IF lv_tok_idx = ls_stmt-from.
              lv_arrow      = '->'.
              lv_left_str   = 'ME'.
              lv_right_name = lv_tstr.
              REPLACE ALL OCCURRENCES OF '(' IN lv_right_name WITH ''.
              CONDENSE lv_right_name NO-GAPS.
            ENDIF.
          ENDIF.

          " Чистим имя метода от возможных артефактов (скобки, пробелы)
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
              lv_c-class = mv_class_name.
              lv_c-super = abap_true.
            ELSE.
              lv_rtype = resolve_var_type(
                is_source = cs_source
                i_program = i_program
                i_evtype  = mv_event_type
                i_evname  = mv_event_name
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

            APPEND lv_c TO lt_new_calls.
          ENDIF.

          lv_tok_idx += 1.
        ENDWHILE.

    ENDCASE.

    CHECK lt_new_calls IS NOT INITIAL.

    LOOP AT cs_source-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE include = i_include.
      LOOP AT <prog>-t_keywords ASSIGNING FIELD-SYMBOL(<kw>)
        WHERE index = i_stmt_idx.
        LOOP AT lt_new_calls INTO DATA(ls_nc).
          READ TABLE <kw>-tt_calls WITH KEY event = ls_nc-event
                                            name  = ls_nc-name
                                            class = ls_nc-class
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_nc TO <kw>-tt_calls.
          ENDIF.
        ENDLOOP.
        EXIT.
      ENDLOOP.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve_var_type.
    " 1. Local variables of the current procedure
    LOOP AT is_source-t_vars INTO DATA(ls_var)
      WHERE program   = i_program
        AND eventtype = i_evtype
        AND eventname = i_evname
        AND name      = i_varname.
      IF ls_var-type IS NOT INITIAL.
        rv_type = ls_var-type.
        RETURN.
      ENDIF.
    ENDLOOP.

    " 2. Class attributes (class-level, no event context)
    LOOP AT is_source-t_vars INTO ls_var
      WHERE program   = i_program
        AND eventtype = ''
        AND eventname = ''
        AND name      = i_varname.
      IF ls_var-type IS NOT INITIAL.
        rv_type = ls_var-type.
        RETURN.
      ENDIF.
    ENDLOOP.

    " 3. Global variables of the program
    LOOP AT is_source-t_vars INTO ls_var
      WHERE program   = i_program
        AND class     = ''
        AND eventtype = ''
        AND eventname = ''
        AND name      = i_varname.
      IF ls_var-type IS NOT INITIAL.
        rv_type = ls_var-type.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
