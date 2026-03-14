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


  METHOD parse_tokens.

    DATA(lo_src)  = cl_ci_source_include=>create( p_name = i_include ).
    DATA(lo_scan) = NEW cl_ci_scan( p_include = lo_src ).

    DATA ls_prog TYPE zcl_ace_window=>ts_prog.
    ls_prog-program    = i_program.
    ls_prog-include    = i_include.
    ls_prog-source_tab = lo_src->lines.
    ls_prog-scan       = lo_scan.
    APPEND ls_prog TO cs_source-tt_progs.

    ASSIGN cs_source-tt_progs[ lines( cs_source-tt_progs ) ] TO FIELD-SYMBOL(<ls_prog>).

    " Рекурсивно парсим sub-includes (CU/CO/CI/CM*) из levels ДО основного прохода.
    " CU: CLASS DEFINITION + PUBLIC SECTION + METHODS → meth_type по суффиксу инклуда.
    " CM*: METHOD ... ENDMETHOD → include корректный для каждого метода.
    DATA lt_seen TYPE HASHED TABLE OF program WITH UNIQUE KEY table_line.
    INSERT i_include INTO TABLE lt_seen.
    LOOP AT lo_scan->levels INTO DATA(ls_lev).
      DATA(lv_sub) = ls_lev-name.
      CHECK lv_sub IS NOT INITIAL.
      INSERT CONV program( lv_sub ) INTO TABLE lt_seen.
      CHECK sy-subrc = 0.
      READ TABLE cs_source-tt_progs WITH KEY include = CONV program( lv_sub )
        TRANSPORTING NO FIELDS.
      CHECK sy-subrc <> 0.
      zcl_ace_parser=>parse_tokens( EXPORTING i_program = i_program
                                              i_include = CONV program( lv_sub )
                                    CHANGING  cs_source = cs_source ).
    ENDLOOP.

    LOOP AT lo_scan->statements INTO DATA(ls_kw_stmt).
      DATA(lv_kw_idx) = sy-tabix.
      CHECK ls_kw_stmt-type <> '*' AND ls_kw_stmt-type <> 'P'.
      READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from INTO DATA(ls_kw_tok2).
      CHECK sy-subrc = 0.

      READ TABLE lo_scan->levels INDEX ls_kw_stmt-level INTO DATA(ls_kw_level).
      DATA(lv_stmt_include) = COND program(
        WHEN sy-subrc = 0 AND ls_kw_level-name IS NOT INITIAL
        THEN ls_kw_level-name
        ELSE i_include ).

      DATA(lv_kw_name) = SWITCH string( ls_kw_stmt-type
        WHEN 'C' THEN 'COMPUTE'
        WHEN 'D' THEN 'COMPUTE'
        WHEN 'A' THEN '+CALL_METHOD'
        ELSE          ls_kw_tok2-str ).

      IF lv_kw_name = 'CALL'.
        READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 1 INTO DATA(ls_tok2).
        IF sy-subrc = 0. lv_kw_name = |CALL { ls_tok2-str }|. ENDIF.
      ENDIF.

      APPEND VALUE zcl_ace=>ts_kword(
        program = i_program
        include = lv_stmt_include
        index   = lv_kw_idx
        line    = ls_kw_tok2-row
        v_line  = ls_kw_tok2-row
        name    = lv_kw_name
        from    = ls_kw_stmt-from
        to      = ls_kw_stmt-to
      ) TO <ls_prog>-t_keywords.
    ENDLOOP.

    DATA(lo_events)     = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_events( ) ).
    DATA(lo_calls_line) = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_calls_line( ) ).
    DATA(lo_params)     = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_params( ) ).
    DATA(lo_vars)       = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_vars( ) ).
    DATA(lo_calls)      = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_calls( ) ).
    DATA(lo_calcs)      = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_calcs( ) ).

    DATA lt_pass1     TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA lt_pass1_hdl TYPE TABLE OF REF TO zif_ace_stmt_handler WITH EMPTY KEY.

    LOOP AT VALUE string_table(
           ( `CLASS` ) ( `INTERFACE` ) ( `ENDCLASS` ) ( `ENDINTERFACE` )
           ( `PUBLIC` ) ( `PROTECTED` ) ( `PRIVATE` )
           ( `METHODS` ) ( `CLASS-METHODS` )
           ( `FORM` ) ( `METHOD` ) ( `MODULE` ) ( `FUNCTION` )
         ) INTO DATA(lv_kw).
      APPEND lv_kw TO lt_pass1.  APPEND lo_calls_line TO lt_pass1_hdl.
    ENDLOOP.

    LOOP AT VALUE string_table(
           ( `CLASS` ) ( `ENDCLASS` )
           ( `METHOD` ) ( `ENDMETHOD` ) ( `FORM` ) ( `ENDFORM` )
           ( `MODULE` ) ( `ENDMODULE` ) ( `FUNCTION` ) ( `ENDFUNCTION` )
           ( `DATA` ) ( `CLASS-DATA` ) ( `PARAMETERS` ) ( `SELECT-OPTIONS` )
         ) INTO lv_kw.
      APPEND lv_kw TO lt_pass1.  APPEND lo_vars TO lt_pass1_hdl.
    ENDLOOP.

    DATA lt_pass2 TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    INSERT `CLASS`              INTO TABLE lt_pass2.
    INSERT `INTERFACE`          INTO TABLE lt_pass2.
    INSERT `ENDCLASS`           INTO TABLE lt_pass2.
    INSERT `ENDINTERFACE`       INTO TABLE lt_pass2.
    INSERT `METHOD`             INTO TABLE lt_pass2.
    INSERT `ENDMETHOD`          INTO TABLE lt_pass2.
    INSERT `FORM`               INTO TABLE lt_pass2.
    INSERT `ENDFORM`            INTO TABLE lt_pass2.
    INSERT `FUNCTION`           INTO TABLE lt_pass2.
    INSERT `ENDFUNCTION`        INTO TABLE lt_pass2.
    INSERT `MODULE`             INTO TABLE lt_pass2.
    INSERT `ENDMODULE`          INTO TABLE lt_pass2.
    INSERT `PERFORM`            INTO TABLE lt_pass2.
    INSERT `CALL FUNCTION`      INTO TABLE lt_pass2.
    INSERT `CALL METHOD`        INTO TABLE lt_pass2.
    INSERT `+CALL_METHOD`       INTO TABLE lt_pass2.
    INSERT `COMPUTE`            INTO TABLE lt_pass2.
    INSERT `START-OF-SELECTION` INTO TABLE lt_pass2.
    INSERT `END-OF-SELECTION`   INTO TABLE lt_pass2.
    INSERT `INITIALIZATION`     INTO TABLE lt_pass2.
    INSERT `TOP-OF-PAGE`        INTO TABLE lt_pass2.
    INSERT `END-OF-PAGE`        INTO TABLE lt_pass2.
    INSERT `AT`                 INTO TABLE lt_pass2.
    INSERT `GET`                INTO TABLE lt_pass2.

    DATA lt_params_kws TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    INSERT `CLASS`         INTO TABLE lt_params_kws.
    INSERT `INTERFACE`     INTO TABLE lt_params_kws.
    INSERT `ENDCLASS`      INTO TABLE lt_params_kws.
    INSERT `ENDINTERFACE`  INTO TABLE lt_params_kws.
    INSERT `METHODS`       INTO TABLE lt_params_kws.
    INSERT `CLASS-METHODS` INTO TABLE lt_params_kws.
    INSERT `FORM`          INTO TABLE lt_params_kws.

    lo_events->handle(
      EXPORTING io_scan    = lo_scan
                i_stmt_idx = 0
                i_program  = i_program
                i_include  = i_include
      CHANGING  cs_source  = cs_source ).

    DATA(lv_max) = lines( lo_scan->statements ).

    " Pass 1 — только statements из текущего include (sub-includes уже обработаны)
    DO lv_max TIMES.
      DATA(lv_idx) = sy-index.

      READ TABLE lo_scan->statements INDEX lv_idx INTO DATA(ls_stmt).
      CHECK sy-subrc = 0.
      READ TABLE lo_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw_tok).
      CHECK sy-subrc = 0.

      READ TABLE lo_scan->levels INDEX ls_stmt-level INTO DATA(ls_level).
      DATA(lv_real_include) = COND program(
        WHEN sy-subrc = 0 AND ls_level-name IS NOT INITIAL
        THEN ls_level-name
        ELSE i_include ).

      IF lv_real_include <> i_include. CONTINUE. ENDIF.

      DATA(lv_eff_kw) = SWITCH string( ls_stmt-type
        WHEN 'C' THEN 'COMPUTE'
        WHEN 'D' THEN 'COMPUTE'
        WHEN 'A' THEN '+CALL_METHOD'
        ELSE          ls_kw_tok-str ).
      IF lv_eff_kw = 'CALL'.
        READ TABLE lo_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok_d).
        IF sy-subrc = 0. lv_eff_kw = |CALL { ls_tok_d-str }|. ENDIF.
      ENDIF.

      LOOP AT lt_pass1 INTO DATA(lv_p1_kw).
        DATA(lv_p1_idx) = sy-tabix.
        IF lv_p1_kw = lv_eff_kw.
          READ TABLE lt_pass1_hdl INDEX lv_p1_idx INTO DATA(lo_h).
          IF sy-subrc = 0.
            lo_h->handle(
              EXPORTING io_scan    = lo_scan
                        i_stmt_idx = lv_idx
                        i_program  = i_program
                        i_include  = i_include
              CHANGING  cs_source  = cs_source ).
          ENDIF.
        ENDIF.
      ENDLOOP.

      lo_calcs->handle(
        EXPORTING io_scan    = lo_scan
                  i_stmt_idx = lv_idx
                  i_program  = i_program
                  i_include  = i_include
        CHANGING  cs_source  = cs_source ).

      READ TABLE lt_params_kws WITH TABLE KEY table_line = ls_kw_tok-str
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lo_params->handle(
          EXPORTING io_scan    = lo_scan
                    i_stmt_idx = lv_idx
                    i_program  = i_program
                    i_include  = i_include
          CHANGING  cs_source  = cs_source ).
      ENDIF.
    ENDDO.

    SORT cs_source-t_vars BY program eventtype eventname name.

    " Pass 2 — только текущий include
    DO lv_max TIMES.
      lv_idx = sy-index.

      READ TABLE lo_scan->statements INDEX lv_idx INTO ls_stmt.
      CHECK sy-subrc = 0.
      READ TABLE lo_scan->tokens INDEX ls_stmt-from INTO ls_kw_tok.
      CHECK sy-subrc = 0.

      READ TABLE lo_scan->levels INDEX ls_stmt-level INTO ls_level.
      lv_real_include = COND program(
        WHEN sy-subrc = 0 AND ls_level-name IS NOT INITIAL
        THEN ls_level-name
        ELSE i_include ).

      IF lv_real_include <> i_include. CONTINUE. ENDIF.

      lv_eff_kw = SWITCH string( ls_stmt-type
        WHEN 'C' THEN 'COMPUTE'
        WHEN 'D' THEN 'COMPUTE'
        WHEN 'A' THEN '+CALL_METHOD'
        ELSE          ls_kw_tok-str ).
      IF lv_eff_kw = 'CALL'.
        READ TABLE lo_scan->tokens INDEX ls_stmt-from + 1 INTO ls_tok_d.
        IF sy-subrc = 0. lv_eff_kw = |CALL { ls_tok_d-str }|. ENDIF.
      ENDIF.

      READ TABLE lt_pass2 WITH TABLE KEY table_line = lv_eff_kw
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lo_calls->handle(
          EXPORTING io_scan    = lo_scan
                    i_stmt_idx = lv_idx
                    i_program  = i_program
                    i_include  = i_include
          CHANGING  cs_source  = cs_source ).
      ENDIF.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
