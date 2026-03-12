class ZCL_ACE_PARSER definition
  public
  create public .

public section.

  class-methods PARSE_TOKENS
    importing
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
    changing
      !CS_SOURCE type ZCL_ACE_WINDOW=>TS_SOURCE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_PARSER IMPLEMENTATION.


  method PARSE_TOKENS.

    DATA(lo_src)  = cl_ci_source_include=>create( p_name = i_include ).
    DATA(lo_scan) = NEW cl_ci_scan( p_include = lo_src ).

    " ---------------------------------------------------------------
    " Заполняем tt_progs для данного инклуда — один раз
    " ---------------------------------------------------------------
    DATA ls_prog TYPE zcl_ace_window=>ts_prog.
    ls_prog-program    = i_program.
    ls_prog-include    = i_include.
    ls_prog-source_tab = lo_src->lines.
    ls_prog-scan       = lo_scan.
    APPEND ls_prog TO cs_source-tt_progs.

    " ---------------------------------------------------------------
    " Заполняем t_keywords — по одной записи на каждый стейтмент
    " ---------------------------------------------------------------
    ASSIGN cs_source-tt_progs[ lines( cs_source-tt_progs ) ] TO FIELD-SYMBOL(<ls_prog>).
    LOOP AT lo_scan->statements INTO DATA(ls_kw_stmt).
      DATA(lv_kw_idx) = sy-tabix.
      READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from INTO DATA(ls_kw_tok2).
      CHECK sy-subrc = 0.

      " Воспроизводим логику CL_CI_SCAN=>KEYWORD():
      " TYPE='C' -> COMPUTE, TYPE='A' -> +CALL_METHOD, иначе первый токен
      DATA(lv_kw_name) = SWITCH string( ls_kw_stmt-type
        WHEN 'C' THEN 'COMPUTE'
        WHEN 'A' THEN '+CALL_METHOD'
        ELSE          ls_kw_tok2-str ).

      " CALL FUNCTION / CALL METHOD — различаем по второму токену
      IF lv_kw_name = 'CALL'.
        READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 1 INTO DATA(ls_tok2).
        IF sy-subrc = 0.
          lv_kw_name = |CALL { ls_tok2-str }|.
        ENDIF.
      ENDIF.

      " ---------------------------------------------------------------
      " Заполняем tt_calls — один вызов на стейтмент если это вызов
      " ---------------------------------------------------------------
      DATA ls_call TYPE zcl_ace=>ts_calls.
      CLEAR ls_call.

      CASE lv_kw_name.

        WHEN 'CALL FUNCTION'.
          " CALL FUNCTION 'NAME' -> токен[3]
          ls_call-event = 'FUNCTION'.
          READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 2 INTO DATA(ls_name_tok).
          IF sy-subrc = 0.
            ls_call-name = ls_name_tok-str.
            REPLACE ALL OCCURRENCES OF '''' IN ls_call-name WITH ''.
          ENDIF.

        WHEN 'CALL METHOD'.
          " CALL METHOD obj->meth / class=>meth
          ls_call-event = 'METHOD'.
          READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 2 INTO ls_name_tok.
          IF sy-subrc = 0.
            DATA(lv_call_str) = ls_name_tok-str.
            IF lv_call_str CS '->'.
              SPLIT lv_call_str AT '->' INTO ls_call-class ls_call-name.
            ELSEIF lv_call_str CS '=>'.
              SPLIT lv_call_str AT '=>' INTO ls_call-class ls_call-name.
            ELSE.
              ls_call-name = lv_call_str.
            ENDIF.
          ENDIF.

        WHEN '+CALL_METHOD'.
          " obj->meth( ). — первый токен содержит obj->meth(
          ls_call-event = 'METHOD'.
          DATA(lv_impl_str) = ls_kw_tok2-str.
          REPLACE ALL OCCURRENCES OF '(' IN lv_impl_str WITH ''.
          IF lv_impl_str CS '->'.
            SPLIT lv_impl_str AT '->' INTO ls_call-class ls_call-name.
          ELSEIF lv_impl_str CS '=>'.
            SPLIT lv_impl_str AT '=>' INTO ls_call-class ls_call-name.
          ELSE.
            ls_call-name = lv_impl_str.
          ENDIF.

        WHEN 'PERFORM'.
          " PERFORM form_name
          ls_call-event = 'FORM'.
          READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 1 INTO ls_name_tok.
          IF sy-subrc = 0.
            ls_call-name = ls_name_tok-str.
          ENDIF.

      ENDCASE.
      APPEND VALUE zcl_ace=>ts_kword(
        program  = i_program
        include  = i_include
        index    = lv_kw_idx
        line     = ls_kw_tok2-row
        name     = lv_kw_name
        from     = ls_kw_stmt-from
        to       = ls_kw_stmt-to
        tt_calls = COND #( WHEN ls_call-event IS NOT INITIAL
                           THEN VALUE #( ( ls_call ) ) )
      ) TO <ls_prog>-t_keywords.
    ENDLOOP.

    " ---------------------------------------------------------------
    " Dispatch map: keyword -> handlers  (multiple per keyword ok)
    " ---------------------------------------------------------------
    TYPES: BEGIN OF ts_dispatch,
             keyword TYPE string,
             handler TYPE REF TO zif_ace_stmt_handler,
           END OF ts_dispatch.
    DATA lt_dispatch TYPE SORTED TABLE OF ts_dispatch
                     WITH NON-UNIQUE KEY keyword.

    DATA(lo_events)     = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_events( ) ).
    DATA(lo_calls_line) = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_calls_line( ) ).
    "DATA(lo_params)     = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_params( ) ).
    DATA(lo_vars)       = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_vars( ) ).

    LOOP AT VALUE string_table(
           ( `CLASS` )        ( `INTERFACE` )
           ( `ENDCLASS` )     ( `ENDINTERFACE` )
           ( `METHODS` )      ( `CLASS-METHODS` )
           ( `FORM` )         ( `METHOD` )
           ( `MODULE` )       ( `FUNCTION` )
         ) INTO DATA(lv_kw).
      INSERT VALUE ts_dispatch( keyword = lv_kw handler = lo_calls_line )
        INTO TABLE lt_dispatch.
    ENDLOOP.

    " vars: CLASS/ENDCLASS + METHOD/FORM/... context + DATA/CLASS-DATA/PARAMETERS/SELECT-OPTIONS
    LOOP AT VALUE string_table(
           ( `CLASS` )          ( `ENDCLASS` )
           ( `METHOD` )         ( `ENDMETHOD` )
           ( `FORM` )           ( `ENDFORM` )
           ( `MODULE` )         ( `ENDMODULE` )
           ( `FUNCTION` )       ( `ENDFUNCTION` )
           ( `DATA` )           ( `CLASS-DATA` )
           ( `PARAMETERS` )     ( `SELECT-OPTIONS` )
         ) INTO lv_kw.
      INSERT VALUE ts_dispatch( keyword = lv_kw handler = lo_vars )
        INTO TABLE lt_dispatch.
    ENDLOOP.

    " params: второй dispatch-проход после основного (METHOD/FORM/FUNCTION)
    DATA lt_params_kws TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    INSERT `CLASS`        INTO TABLE lt_params_kws.
    INSERT `INTERFACE`    INTO TABLE lt_params_kws.
    INSERT `ENDCLASS`     INTO TABLE lt_params_kws.
    INSERT `ENDINTERFACE` INTO TABLE lt_params_kws.
    INSERT `METHODS`      INTO TABLE lt_params_kws.
    INSERT `CLASS-METHODS` INTO TABLE lt_params_kws.
    INSERT `FORM`         INTO TABLE lt_params_kws.

    " events: один раз по структурам, без цикла по стейтментам
    lo_events->handle(
      EXPORTING io_scan    = lo_scan
                i_stmt_idx = 0
                i_program  = i_program
                i_include  = i_include
      CHANGING  cs_source  = cs_source ).

    " ---------------------------------------------------------------
    " Цикл по ВСЕМ стейтментам напрямую.
    " get_procedure_iterator не используем — он пропускает METHOD
    " внутри классов и возвращает только верхнеуровневые процедуры.
    " ---------------------------------------------------------------
    DATA(lv_max) = lines( lo_scan->statements ).
    DO lv_max TIMES.
      DATA(lv_idx) = sy-index.

      READ TABLE lo_scan->statements INDEX lv_idx INTO DATA(ls_stmt).
      CHECK sy-subrc = 0.
      READ TABLE lo_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw_tok).
      CHECK sy-subrc = 0.

      LOOP AT lt_dispatch INTO DATA(ls_disp) WHERE keyword = ls_kw_tok-str.
        ls_disp-handler->handle(
          EXPORTING io_scan    = lo_scan
                    i_stmt_idx = lv_idx
                    i_program  = i_program
                    i_include  = i_include
          CHANGING  cs_source  = cs_source ).
      ENDLOOP.

      " lo_params обрабатывает CLASS/INTERFACE/ENDCLASS/ENDINTERFACE/METHODS/FORM
      READ TABLE lt_params_kws WITH TABLE KEY table_line = ls_kw_tok-str
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
*        lo_params->handle(
*          EXPORTING io_scan    = lo_scan
*                    i_stmt_idx = lv_idx
*                    i_program  = i_program
*                    i_include  = i_include
*          CHANGING  cs_source  = cs_source ).
      ENDIF.
    ENDDO.

  endmethod.
ENDCLASS.
