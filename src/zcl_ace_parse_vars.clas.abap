CLASS zcl_ace_parse_vars DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

protected section.
  PRIVATE SECTION.
    DATA mv_class_name TYPE string.
    DATA mv_eventtype  TYPE string.
    DATA mv_eventname  TYPE string.
    DATA mv_section    TYPE string.
    DATA mv_in_impl    TYPE abap_bool.

    METHODS append_var
      IMPORTING i_name    TYPE string
                i_type    TYPE string
                i_icon    TYPE salv_de_tree_image
                i_line    TYPE i
                i_program TYPE program
                i_include TYPE program
      CHANGING  cs_source TYPE zif_ace_parse_data=>ts_parse_data.

    METHODS resolve_icon
      IMPORTING i_type         TYPE string
                i_ref          TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_icon) TYPE salv_de_tree_image.

ENDCLASS.



CLASS ZCL_ACE_PARSE_VARS IMPLEMENTATION.


  METHOD zif_ace_stmt_handler~handle.

    data: lv_kw(20).
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX stmt-from INTO DATA(kw_tok).
    CHECK sy-subrc = 0.
    lv_kw = kw_tok-str.

*  IF kw_tok-row = 7585.
*    BREAK-POINT.
*  ENDIF.
    " --- Context tracking ---
    mv_class_name = i_class.
    mv_eventname = i_ev_name.
    mv_eventtype = i_evtype.
    IF i_section IS SUPPLIED.
      mv_section = i_section.
    ENDIF.

    DATA(lv_line) = io_scan->tokens[ stmt-from ]-row.

    " ---------------------------------------------------------------
    " Обычные объявления: DATA / CLASS-DATA / PARAMETERS / SELECT-OPTIONS
    " ---------------------------------------------------------------
    DATA: lv_type       TYPE string,
          lv_ref        TYPE abap_bool,
          lv_after_type TYPE abap_bool,
          lv_for_next   TYPE abap_bool.

    CASE lv_kw.

      WHEN 'PARAMETERS'.
        READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO data(var_tok).
        CHECK sy-subrc = 0 AND var_tok-str IS NOT INITIAL.
        data(lv_name) = var_tok-str.
        LOOP AT io_scan->tokens FROM stmt-from + 2 TO stmt-to INTO data(dtok).
          IF dtok-str = 'CHECKBOX'.
            append_var( EXPORTING i_name    = lv_name
                                  i_type    = 'CHECKBOX'
                                  i_icon    = CONV #( icon_checked )
                                  i_line    = lv_line
                                  i_program = i_program
                                  i_include = i_include
                        CHANGING  cs_source = cs_source ).
            RETURN.
          ENDIF.
          IF dtok-str = 'TYPE' OR dtok-str = 'LIKE'.
            lv_after_type = abap_true. CONTINUE.
          ENDIF.
          IF lv_after_type = abap_true.
            IF dtok-str = 'REF'. lv_ref = abap_true. CONTINUE. ENDIF.
            IF dtok-str = 'TO'.  CONTINUE. ENDIF.
            lv_type = dtok-str. EXIT.
          ENDIF.
        ENDLOOP.
        CHECK lv_type IS NOT INITIAL.
        append_var( EXPORTING i_name    = lv_name
                              i_type    = lv_type
                              i_icon    = resolve_icon( i_type = lv_type i_ref = lv_ref )
                              i_line    = lv_line
                              i_program = i_program
                              i_include = i_include
                    CHANGING  cs_source = cs_source ).

      WHEN 'SELECT-OPTIONS'.
        READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO var_tok.
        CHECK sy-subrc = 0 AND var_tok-str IS NOT INITIAL.
        lv_name = var_tok-str.
        LOOP AT io_scan->tokens FROM stmt-from + 2 TO stmt-to INTO dtok.
          IF dtok-str = 'FOR'. lv_for_next = abap_true. CONTINUE. ENDIF.
          IF lv_for_next = abap_true. lv_type = dtok-str. EXIT. ENDIF.
        ENDLOOP.
        CHECK lv_type IS NOT INITIAL.
        append_var( EXPORTING i_name    = lv_name
                              i_type    = lv_type
                              i_icon    = CONV #( icon_select_all )
                              i_line    = lv_line
                              i_program = i_program
                              i_include = i_include
                    CHANGING  cs_source = cs_source ).

WHEN OTHERS.
        " Variable name — always token[2]
        READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO var_tok.
        CHECK sy-subrc = 0 AND var_tok-str IS NOT INITIAL.
        lv_name = var_tok-str.

        " Проверяем инлайн-декларацию: DATA( varname )
        "IF lv_name+0(1) = '(' OR lv_kw = 'DATA' AND lv_name CS '('.
        IF lv_kw+0(5) = 'DATA('.
          " Имя переменной внутри скобок
          DATA(lv_inline_name) = lv_kw.

          REPLACE ALL OCCURRENCES OF 'DATA(' IN lv_inline_name WITH ''.
          REPLACE ALL OCCURRENCES OF ')' IN lv_inline_name WITH ''.
          CONDENSE lv_inline_name NO-GAPS.
          IF lv_inline_name IS INITIAL.
            " имя в следующем токене
            READ TABLE io_scan->tokens INDEX stmt-from + 2 INTO DATA(var_tok2).
            IF sy-subrc = 0.
              lv_inline_name = var_tok2-str.
              REPLACE ALL OCCURRENCES OF ')' IN lv_inline_name WITH ''.
            ENDIF.
          ENDIF.
          CHECK lv_inline_name IS NOT INITIAL.

          " Ищем тип: NEW ClassName( или CAST ClassName(
          DATA lv_new_next TYPE abap_bool.
          DATA lv_cast_next TYPE abap_bool.
          LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(dtok_i).
            DATA(lv_up_i) = to_upper( dtok_i-str ).
            IF lv_new_next = abap_true OR lv_cast_next = abap_true.
              DATA(lv_cls_inline) = dtok_i-str.
              REPLACE ALL OCCURRENCES OF '(' IN lv_cls_inline WITH ''.
              IF lv_cls_inline IS NOT INITIAL AND lv_cls_inline <> '#'.
                append_var( EXPORTING i_name    = conv #( lv_inline_name )
                                      i_type    = to_upper( lv_cls_inline )
                                      i_icon    = resolve_icon( i_type = lv_cls_inline i_ref = abap_true )
                                      i_line    = lv_line
                                      i_program = i_program
                                      i_include = i_include
                            CHANGING  cs_source = cs_source ).
              ENDIF.
              EXIT.
            ENDIF.
            IF lv_up_i = 'NEW' OR lv_up_i = 'CAST'.
              IF lv_up_i = 'NEW'.  lv_new_next  = abap_true. ENDIF.
              IF lv_up_i = 'CAST'. lv_cast_next = abap_true. ENDIF.
            ENDIF.
          ENDLOOP.
          RETURN.
        ENDIF.

        " Обычный DATA varname TYPE ...
        LOOP AT io_scan->tokens FROM stmt-from + 2 TO stmt-to INTO dtok.
          IF dtok-str = 'TYPE' OR dtok-str = 'LIKE'.
            lv_after_type = abap_true. CONTINUE.
          ENDIF.
          IF lv_after_type = abap_true.
            IF dtok-str = 'REF'. lv_ref = abap_true. CONTINUE. ENDIF.
            IF dtok-str = 'TO'.  CONTINUE. ENDIF.
            lv_type = dtok-str. EXIT.
          ENDIF.
        ENDLOOP.
        CHECK lv_type IS NOT INITIAL.
        append_var( EXPORTING i_name    = lv_name
                              i_type    = lv_type
                              i_icon    = resolve_icon( i_type = lv_type i_ref = lv_ref )
                              i_line    = lv_line
                              i_program = i_program
                              i_include = i_include
                    CHANGING  cs_source = cs_source ).


    ENDCASE.

  ENDMETHOD.


  METHOD append_var.
    READ TABLE cs_source-t_vars WITH KEY program   = i_program
                                         include   = i_include
                                         class     = mv_class_name
                                         eventtype = mv_eventtype
                                         eventname = mv_eventname
                                         name      = i_name
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      INSERT VALUE zcl_ace=>ts_vars(
        program   = i_program
        include   = i_include
        class     = mv_class_name
        eventtype = mv_eventtype
        eventname = mv_eventname
        section   = mv_section
        line      = i_line
        name      = i_name
        type      = i_type
        icon      = i_icon ) INTO table cs_source-t_vars.
    ENDIF.
  ENDMETHOD.


  METHOD resolve_icon.
    IF i_ref = abap_true.
      rv_icon = CONV #( icon_oo_class ).
      RETURN.
    ENDIF.
    rv_icon = SWITCH #( to_upper( i_type )
      WHEN 'STRING'                THEN CONV #( icon_text_act )
      WHEN 'D'                     THEN CONV #( icon_date )
      WHEN 'T'                     THEN CONV #( icon_bw_time_sap )
      WHEN 'C'                     THEN CONV #( icon_wd_input_field )
      WHEN 'P'                     THEN CONV #( icon_increase_decimal )
      WHEN 'N' OR 'I' OR 'INT4'
           OR 'INT8' OR 'F'        THEN CONV #( icon_pm_order )
      WHEN 'BOOLEAN' OR 'ABAP_BOOL'
           OR 'FLAG' OR 'BOOLE_D'  THEN CONV #( icon_checked )
      ELSE                              CONV #( icon_element ) ).
  ENDMETHOD.
ENDCLASS.
