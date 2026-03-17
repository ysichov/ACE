class ZCL_ACE_PARSER definition
  public
  create public .

public section.

  methods PARSE_TOKENS
    importing
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_CLASS type STRING optional
    changing
      !CS_SOURCE type ZCL_ACE_WINDOW=>TS_SOURCE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_PARSER IMPLEMENTATION.


  METHOD parse_tokens.
    DATA: lv_class     TYPE string,
          lv_interface TYPE string,
          lv_eventtype TYPE string,
          lv_eventname TYPE string.

    READ TABLE cs_source-tt_progs WITH KEY include = i_include TRANSPORTING  NO FIELDS.
    CHECK sy-subrc <> 0.
    IF i_class IS NOT INITIAL.
      lv_class = i_class.
    ENDIF.

    DATA: lo_parser TYPE REF TO zcl_ace_parser.
    DATA(lo_src)  = cl_ci_source_include=>create( p_name = i_include ).
    DATA(lo_scan) = NEW cl_ci_scan( p_include = lo_src ).

    DATA ls_prog TYPE zcl_ace_window=>ts_prog.
    ls_prog-program    = i_program.
    ls_prog-include    = i_include.
    ls_prog-source_tab = lo_src->lines.
    ls_prog-scan       = lo_scan.
    APPEND ls_prog TO cs_source-tt_progs.

    DATA(lo_events)     = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_events( ) ).

    DATA(lo_calls_line) = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_calls_line( ) ).

    DATA(lo_params)     = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_params( ) ).
    DATA(lo_vars)       = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_vars( ) ).
    DATA(lo_calls)      = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_calls( ) ).
    DATA(lo_calcs)      = CAST zif_ace_stmt_handler( NEW zcl_ace_parse_calcs( ) ).

    DATA lt_pass1     TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA lt_pass1_hdl TYPE TABLE OF REF TO zif_ace_stmt_handler WITH EMPTY KEY.


    DATA lt_pass2 TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    INSERT `PERFORM`            INTO TABLE lt_pass2.
    INSERT `CALL FUNCTION`      INTO TABLE lt_pass2.
    INSERT `CALL METHOD`        INTO TABLE lt_pass2.
    INSERT `+CALL_METHOD`       INTO TABLE lt_pass2.
    INSERT `COMPUTE`            INTO TABLE lt_pass2.

    DATA lt_params_kws TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    INSERT `METHODS`       INTO TABLE lt_params_kws.
    INSERT `CLASS-METHODS` INTO TABLE lt_params_kws.
    INSERT `FORM`          INTO TABLE lt_params_kws.

    lo_events->handle(
      EXPORTING
        io_scan    = lo_scan
        i_stmt_idx = 0
        i_program  = i_program
        i_include  = i_include
      CHANGING
        cs_source  = cs_source ).


    ASSIGN cs_source-tt_progs[ lines( cs_source-tt_progs ) ] TO FIELD-SYMBOL(<ls_prog>).

    LOOP AT lo_scan->statements INTO DATA(ls_kw_stmt).
      IF ls_kw_stmt-level <> 1.
        READ TABLE lo_scan->levels INDEX ls_kw_stmt-level INTO DATA(ls_kw_level).

        NEW zcl_ace_parser( )->parse_tokens( EXPORTING i_program = i_program
                                                       i_include = ls_kw_level-name
                                                       i_class   = lv_class
                                             CHANGING  cs_source = cs_source ).
        CONTINUE.
      ENDIF.

      DATA(lv_kw_idx) = sy-tabix.
      CHECK ls_kw_stmt-type <> '*' AND ls_kw_stmt-type <> 'P'.
      READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from INTO DATA(ls_kw_tok).
      CHECK sy-subrc = 0.

      DATA(lv_kw_name) = SWITCH string( ls_kw_stmt-type
        WHEN 'C' THEN 'COMPUTE'
        WHEN 'D' THEN 'COMPUTE'
        WHEN 'A' THEN '+CALL_METHOD'
        ELSE          ls_kw_tok-str ).


      READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 1 INTO DATA(ls_tok2).
      IF ls_kw_tok-str = 'CLASS'.
        lv_class = ls_tok2-str.
        CLEAR lv_interface.
      ENDIF.
      IF  ls_kw_tok-str = 'INTERFACE'.
        CLEAR lv_class.
        lv_interface = ls_tok2-str.
      ENDIF.

      IF ls_kw_tok-str = 'METHOD'.
        lv_eventtype = 'METHOD'.
        lv_eventname = ls_tok2-str.
      ENDIF.
      IF ls_kw_tok-str = 'FORM'.
        lv_eventtype = 'FORM'.
        lv_eventname = ls_tok2-str.
      ENDIF.
      IF ls_kw_tok-str = 'MODULE'.
        lv_eventtype = 'MODULE'.
        lv_eventname = ls_tok2-str.
      ENDIF.

      IF lv_kw_name = 'CALL'.
        lv_kw_name = |CALL { ls_tok2-str }|.
      ENDIF.

      APPEND VALUE zcl_ace=>ts_kword(
        program = i_program
        include = i_include "lv_stmt_include
        index   = lv_kw_idx
        line    = ls_kw_tok-row
        v_line  = ls_kw_tok-row
        name    = lv_kw_name
        from    = ls_kw_stmt-from
        to      = ls_kw_stmt-to
      ) TO <ls_prog>-t_keywords.

      DATA(lv_eff_kw) = SWITCH string( ls_kw_stmt-type
        WHEN 'C' THEN 'COMPUTE'
        WHEN 'D' THEN 'COMPUTE'
        WHEN 'A' THEN '+CALL_METHOD'
        ELSE          ls_kw_tok-str ).

      IF lv_eff_kw = 'CALL'.
        READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 1 INTO DATA(ls_tok_d).
        IF sy-subrc = 0. lv_eff_kw = |CALL { ls_tok_d-str }|. ENDIF.
      ENDIF.

      IF lv_eff_kw = 'CLASS' OR lv_eff_kw =  'PUBLIC' OR lv_eff_kw =  'PROTECTED' OR lv_eff_kw = 'PRIVATE'
     OR lv_eff_kw = 'METHODS' OR lv_eff_kw = 'CLASS-METHODS'
     OR lv_eff_kw = 'FORM' OR lv_eff_kw = `METHOD` OR lv_eff_kw = `MODULE` OR lv_eff_kw = 'FUNCTION'.
        lo_calls_line->handle(
          EXPORTING
            io_scan     = lo_scan
            i_class     = lv_class
            i_interface = lv_interface
            i_stmt_idx  = lv_kw_idx
            i_program   = i_program
            i_include   = i_include
          CHANGING
            cs_source   = cs_source ).
      ENDIF.

      IF lv_eff_kw = 'DATA' OR lv_eff_kw =  'CLASS-DATA' OR lv_eff_kw = 'PARAMETERS' OR lv_eff_kw = 'SELECT-OPTIONS'.
        lo_vars->handle(
          EXPORTING
            io_scan    = lo_scan
            i_stmt_idx = lv_kw_idx
            i_program  = i_program
            i_include  = i_include
            i_class    = lv_class
            i_evtype   = lv_eventtype
            i_ev_name  = lv_eventname
          CHANGING
            cs_source  = cs_source ).
      ENDIF.

      IF lv_eff_kw = 'COMPUTE'.
        lo_calcs->handle(
          EXPORTING
            io_scan    = lo_scan
            i_stmt_idx = lv_kw_idx
            i_program  = i_program
            i_include  = i_include
          CHANGING
            cs_source  = cs_source ).
      ENDIF.

      READ TABLE lt_params_kws WITH TABLE KEY table_line = ls_kw_tok-str
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lo_params->handle(
          EXPORTING
            io_scan    = lo_scan
            i_class    = lv_class
            i_stmt_idx = lv_kw_idx
            i_program  = i_program
            i_include  = i_include
          CHANGING
            cs_source  = cs_source ).
      ENDIF.

      lv_eff_kw = SWITCH string( ls_kw_stmt-type
           WHEN 'C' THEN 'COMPUTE'
           WHEN 'D' THEN 'COMPUTE'
           WHEN 'A' THEN '+CALL_METHOD'
           ELSE          ls_kw_tok-str ).

      IF lv_eff_kw = 'CALL'.
        READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 1 INTO ls_tok_d.
        IF sy-subrc = 0. lv_eff_kw = |CALL { ls_tok_d-str }|. ENDIF.
      ENDIF.

      READ TABLE lt_pass2 WITH TABLE KEY table_line = lv_eff_kw
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lo_calls->handle(
          EXPORTING
            io_scan    = lo_scan
            i_class    = lv_class
            i_stmt_idx = lv_kw_idx
            i_program  = i_program
            i_include  = i_include
          CHANGING
            cs_source  = cs_source ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
