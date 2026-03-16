CLASS zcl_ace_parse_params DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

protected section.
  PRIVATE SECTION.

    DATA mv_class_name TYPE string.
    DATA mv_in_impl    TYPE abap_bool.

    METHODS parse_methods_stmt
      IMPORTING
        !io_scan    TYPE REF TO cl_ci_scan
        !i_stmt_idx TYPE i
        !i_program  TYPE program
        !i_include  TYPE program
        !i_kw       TYPE string
      CHANGING
        !cs_source  TYPE zcl_ace_window=>ts_source.

ENDCLASS.



CLASS ZCL_ACE_PARSE_PARAMS IMPLEMENTATION.


  METHOD zif_ace_stmt_handler~handle.

    CHECK i_stmt_idx > 0.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX stmt-from INTO DATA(kw_tok).
    CHECK sy-subrc = 0.

     mv_class_name = i_class.
    CASE kw_tok-str.

*      WHEN 'CLASS' OR 'INTERFACE'.
*        READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
*        IF sy-subrc = 0.
*          mv_class_name = name_tok-str.
*          mv_in_impl    = abap_false.
*          IF kw_tok-str = 'CLASS'.
*            LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(tok).
*              IF tok-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. RETURN. ENDIF.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.

*      WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
*        CLEAR: mv_class_name, mv_in_impl.

      WHEN 'METHODS' OR 'CLASS-METHODS'.
        "IF mv_in_impl = abap_false.
          parse_methods_stmt(
            EXPORTING io_scan    = io_scan
                      i_stmt_idx = i_stmt_idx
                      i_program  = i_program
                      i_include  = i_include
                      i_kw       = kw_tok-str
            CHANGING  cs_source  = cs_source ).
        "ENDIF.

      WHEN 'FORM'.
        parse_methods_stmt(
          EXPORTING io_scan    = io_scan
                    i_stmt_idx = i_stmt_idx
                    i_program  = i_program
                    i_include  = i_include
                    i_kw       = kw_tok-str
          CHANGING  cs_source  = cs_source ).

    ENDCASE.

  ENDMETHOD.


  METHOD parse_methods_stmt.

    DATA: lt_params LIKE cs_source-t_params.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.

    DATA(lv_ev_name)    = ``.
    DATA(lv_section)    = ``.
    DATA(lv_pname)      = ``.
    DATA(lv_ptype)      = ``.
    DATA(lv_ref)        = abap_false.
    DATA(lv_after_type) = abap_false.
    DATA(lv_is_form)    = xsdbool( i_kw = 'FORM' ).
    DATA(lv_last_row)   = 0.
    DATA(lv_preferred)  = ``.       " имя PREFERRED PARAMETER
    DATA(lv_skip_next)  = abap_false. " пропустить следующий токен после PREFERRED

    DATA(lv_tok_idx) = stmt-from + 1.

    " FORM: имя — второй токен
    IF lv_is_form = abap_true.
      READ TABLE io_scan->tokens INDEX lv_tok_idx INTO DATA(ftok).
      IF sy-subrc = 0.
        lv_ev_name  = ftok-str.
        lv_last_row = ftok-row.
        lv_tok_idx += 1.
      ENDIF.
    ENDIF.

    " --- локальный хелпер: добавить текущий параметр в обе таблицы ---
    DATA: ls_param TYPE zcl_ace_window=>ts_params.
    "ls_var2  TYPE zcl_ace_window=>ts_var2.

    WHILE lv_tok_idx <= stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_idx INTO DATA(tok).
      IF sy-subrc <> 0. EXIT. ENDIF.
      lv_last_row = tok-row.
      DATA(lv_str) = tok-str.

      " ---- Разделитель цепочки METHODS: meth1 ..., meth2 ...
      IF lv_str = ','.
        IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
          ls_param = VALUE #(
            program = i_program  include = i_include
            class   = mv_class_name
            event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
            name    = lv_ev_name
            type    = SWITCH #( lv_section
                        WHEN 'IMPORTING' OR 'USING' THEN 'I'
                        WHEN 'EXPORTING'             THEN 'E'
                        WHEN 'CHANGING'              THEN 'C'
                        WHEN 'RETURNING'             THEN 'R'
                        ELSE 'I' )
            param   = lv_pname   line = tok-row ).

          APPEND ls_param TO lt_params. "cs_source-t_params.

        ENDIF.
        lv_tok_idx += 1.
        READ TABLE io_scan->tokens INDEX lv_tok_idx INTO tok.
        IF sy-subrc = 0. lv_ev_name = tok-str. ENDIF.
        CLEAR: lv_section, lv_pname, lv_ptype, lv_ref, lv_after_type.
        lv_tok_idx += 1.
        CONTINUE.
      ENDIF.

      CASE lv_str.

        WHEN 'IMPORTING' OR 'EXPORTING' OR 'CHANGING' OR 'RETURNING'
          OR 'USING' OR 'TABLES'.
          IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
            ls_param = VALUE #(
              program = i_program  include = i_include
              class   = mv_class_name
              event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
              name    = lv_ev_name
              type    = SWITCH #( lv_section
                          WHEN 'IMPORTING' OR 'USING' THEN 'I'
                          WHEN 'EXPORTING'             THEN 'E'
                          WHEN 'CHANGING'              THEN 'C'
                          WHEN 'RETURNING'             THEN 'R'
                          ELSE 'I' )
              param   = lv_pname   line = tok-row ).
            APPEND ls_param TO lt_params. "cs_source-t_params.
          ENDIF.
          lv_section = lv_str.
          CLEAR: lv_pname, lv_ptype, lv_ref, lv_after_type.

        WHEN 'RAISING' OR 'EXCEPTIONS'.
          IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
            ls_param = VALUE #(
              program = i_program  include = i_include
              class   = mv_class_name
              event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
              name    = lv_ev_name
              type    = SWITCH #( lv_section
                          WHEN 'IMPORTING' OR 'USING' THEN 'I'
                          WHEN 'EXPORTING'             THEN 'E'
                          WHEN 'CHANGING'              THEN 'C'
                          WHEN 'RETURNING'             THEN 'R'
                          ELSE 'I' )
              param   = lv_pname   line = tok-row ).
            APPEND ls_param TO lt_params. "cs_source-t_params.
          ENDIF.
          CLEAR: lv_section, lv_pname, lv_ptype, lv_ref, lv_after_type.

        WHEN 'TYPE' OR 'LIKE'.
          lv_after_type = abap_true.
          lv_ref        = abap_false.

        WHEN 'REF'.
          lv_ref = abap_true.

        WHEN 'TO' OR 'OPTIONAL' OR 'DEFAULT'
          OR 'ABSTRACT' OR 'FINAL' OR 'REDEFINITION'.
          " пропускаем модификаторы

        WHEN 'VALUE'.
          " VALUE( param ) — токен VALUE сам по себе означает default-value keyword,
          " но VALUE(PARAM) как один токен обрабатывается в WHEN OTHERS ниже.
          " Здесь просто пропускаем одиночный VALUE.

        WHEN 'PREFERRED'.
          " Flush текущего параметра перед обработкой PREFERRED
          IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
            ls_param = VALUE #(
              program = i_program  include = i_include
              class   = mv_class_name
              event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
              name    = lv_ev_name
              type    = SWITCH #( lv_section
                          WHEN 'IMPORTING' OR 'USING' THEN 'I'
                          WHEN 'EXPORTING'             THEN 'E'
                          WHEN 'CHANGING'              THEN 'C'
                          WHEN 'RETURNING'             THEN 'R'
                          ELSE 'I' )
              param   = lv_pname   line = tok-row ).
            APPEND ls_param TO lt_params. "cs_source-t_params.
            CLEAR: lv_pname, lv_ptype, lv_ref, lv_after_type.
          ENDIF.
          lv_skip_next = abap_true.

        WHEN 'PARAMETER'.
          IF lv_skip_next = abap_true.
            READ TABLE io_scan->tokens INDEX lv_tok_idx + 1 INTO DATA(ls_pref_tok).
            IF sy-subrc = 0.
              lv_preferred = ls_pref_tok-str.
              IF lv_preferred+0(1) = '!'. lv_preferred = lv_preferred+1. ENDIF.
            ENDIF.
            lv_skip_next = abap_false.
          ENDIF.

        WHEN OTHERS.
          IF lv_str+0(1) = '!'.
            " !PARAM — новый параметр, сохраняем предыдущий
            IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
              ls_param = VALUE #(
                program = i_program  include = i_include
                class   = mv_class_name
                event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
                name    = lv_ev_name
                type    = SWITCH #( lv_section
                            WHEN 'IMPORTING' OR 'USING' THEN 'I'
                            WHEN 'EXPORTING'             THEN 'E'
                            WHEN 'CHANGING'              THEN 'C'
                            WHEN 'RETURNING'             THEN 'R'
                            ELSE 'I' )
                param   = lv_pname   line = tok-row ).
              APPEND ls_param TO lt_params. "cs_source-t_params.

            ENDIF.
            lv_pname = lv_str+1.
            CLEAR: lv_ptype, lv_ref, lv_after_type.

          ELSEIF lv_after_type = abap_true.
            " имя типа — сохраняем в lv_ptype
            IF lv_ref = abap_true.
              lv_ptype = |REF TO { lv_str }|.
            ELSE.
              lv_ptype = lv_str.
            ENDIF.
            lv_after_type = abap_false.
            lv_ref        = abap_false.

          ELSEIF lv_section IS NOT INITIAL AND lv_pname IS INITIAL.
            " параметр без ! — в т.ч. VALUE(param) для RETURNING
            lv_pname = lv_str.
            " Убираем VALUE( ... ) если есть
            IF lv_pname CP 'VALUE(*'.
              REPLACE FIRST OCCURRENCE OF 'VALUE(' IN lv_pname WITH ''.
              REPLACE ALL OCCURRENCES OF ')' IN lv_pname WITH ''.
              CONDENSE lv_pname NO-GAPS.
            ENDIF.
            CLEAR: lv_ptype, lv_ref, lv_after_type.

          ELSEIF lv_ev_name IS INITIAL AND lv_is_form = abap_false.
            " имя метода в цепочке METHODS
            lv_ev_name = lv_str.

          ENDIF.

      ENDCASE.

      lv_tok_idx += 1.
    ENDWHILE.

    " Последний параметр
    IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
      ls_param = VALUE #(
        program = i_program  include = i_include
        class   = mv_class_name
        event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
        name    = lv_ev_name
        type    = SWITCH #( lv_section
                    WHEN 'IMPORTING' OR 'USING' THEN 'I'
                    WHEN 'EXPORTING'             THEN 'E'
                    WHEN 'CHANGING'              THEN 'C'
                    WHEN 'RETURNING'             THEN 'R'
                    ELSE 'I' )
        param   = lv_pname   line = lv_last_row ).
      APPEND ls_param TO lt_params. "cs_source-t_params.
    ENDIF.

    " PREFERRED PARAMETER стоит в конце объявления — после того как все
    " параметры уже добавлены. Проставляем флаг постфактум.
    IF lv_preferred IS NOT INITIAL.
      READ TABLE lt_params WITH KEY name = lv_ev_name ASSIGNING FIELD-SYMBOL(<fp>).
      IF sy-subrc = 0.
        <fp>-preferred = 'X'.
      ENDIF.
*      DATA(lv_ev_type) = COND string( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' ).
*      LOOP AT cs_source-t_params ASSIGNING FIELD-SYMBOL(<fp>)
*        WHERE program = i_program
*          AND class   = mv_class_name
*          AND event   = lv_ev_type
*          AND name    = lv_ev_name
*          AND param   = lv_preferred.
      <fp>-preferred = 'X'.
*        EXIT.
*      ENDLOOP.
    ENDIF.

    LOOP AT lt_params into ls_param.
      APPEND ls_param TO cs_source-t_params.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
