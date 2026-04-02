CLASS zcl_ace_parser DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  CLASS-METHODS parse
    IMPORTING
      !i_program  TYPE program
      !i_include  TYPE program
      !i_class    TYPE string OPTIONAL
      !i_run      TYPE i DEFAULT 1
    CHANGING
      !cs_source  TYPE zif_ace_parse_data=>ts_parse_data .

  CLASS-METHODS parse_tokens
    IMPORTING
      !i_program  TYPE program
      !i_include  TYPE program
      !i_class    TYPE string OPTIONAL
      !i_run      TYPE i DEFAULT 1
      !i_stmt_idx TYPE i DEFAULT 0
      !i_evtype   TYPE string OPTIONAL
      !i_ev_name  TYPE string OPTIONAL
    CHANGING
      !cs_source  TYPE zif_ace_parse_data=>ts_parse_data .

PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ACE_PARSER IMPLEMENTATION.


  METHOD parse.
    NEW zcl_ace_parser( )->parse_tokens(
      EXPORTING
        i_program = i_program
        i_include = i_include
        i_class   = i_class
        i_run     = i_run
      CHANGING
        cs_source = cs_source ).
  ENDMETHOD.


  METHOD parse_tokens.
    DATA: lv_class     TYPE string,
          lv_interface TYPE string,
          lv_eventtype TYPE string,
          lv_eventname TYPE string,
          lv_section   TYPE string.

    IF i_stmt_idx > 0.
      READ TABLE cs_source-tt_progs WITH KEY include = i_include ASSIGNING FIELD-SYMBOL(<prog2>).
      CHECK sy-subrc = 0.
      DATA lt_pass2 TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      INSERT `NEW`           INTO TABLE lt_pass2.
      INSERT `PERFORM`       INTO TABLE lt_pass2.
      INSERT `CALL FUNCTION` INTO TABLE lt_pass2.
      INSERT `CALL METHOD`   INTO TABLE lt_pass2.
      INSERT `+CALL_METHOD`  INTO TABLE lt_pass2.
      INSERT `COMPUTE`       INTO TABLE lt_pass2.
      INSERT `RAISE EVENT`   INTO TABLE lt_pass2.
      READ TABLE <prog2>-t_keywords WITH KEY index = i_stmt_idx INTO DATA(lv_key2).
      CHECK sy-subrc = 0.
      IF lv_key2-calls_parsed = abap_true. RETURN. ENDIF.
      DATA(lv_eff2) = lv_key2-name.
      IF lv_eff2 = 'CALL'.
        READ TABLE <prog2>-scan->statements INDEX i_stmt_idx INTO DATA(ls_s2).
        IF sy-subrc = 0.
          READ TABLE <prog2>-scan->tokens INDEX ls_s2-from + 1 INTO DATA(ls_t2).
          IF sy-subrc = 0. lv_eff2 = |CALL { ls_t2-str }|. ENDIF.
        ENDIF.
      ELSEIF lv_eff2 = 'RAISE'.
        READ TABLE <prog2>-scan->statements INDEX i_stmt_idx INTO ls_s2.
        IF sy-subrc = 0.
          READ TABLE <prog2>-scan->tokens INDEX ls_s2-from + 1 INTO ls_t2.
          IF sy-subrc = 0 AND ls_t2-str = 'EVENT'. lv_eff2 = 'RAISE EVENT'. ENDIF.
        ENDIF.
      ENDIF.
      DATA(lv_inc2) = CONV program( i_include ).
      DATA(lv_prg2) = CONV program( i_program ).

      " ── Сначала parse_calls — заполняет tt_calls с bindings ──────
      READ TABLE lt_pass2 WITH TABLE KEY table_line = lv_eff2 TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        IF lv_eff2 = 'RAISE EVENT'.
          DATA(lo_hdl2) = NEW zcl_ace_parse_handlers( ).
          lo_hdl2->zif_ace_stmt_handler~handle(
            EXPORTING io_scan = <prog2>-scan i_stmt_idx = i_stmt_idx
              i_program = lv_prg2 i_include = lv_inc2
              i_class = i_class i_evtype = i_evtype i_ev_name = i_ev_name
            CHANGING cs_source = cs_source ).
        ELSE.
          DATA(lo_calls2) = NEW zcl_ace_parse_calls( ).
          lo_calls2->zif_ace_stmt_handler~handle(
            EXPORTING io_scan = <prog2>-scan i_stmt_idx = i_stmt_idx
              i_program = lv_prg2 i_include = lv_inc2
              i_class = i_class i_evtype = i_evtype i_ev_name = i_ev_name
            CHANGING cs_source = cs_source ).
        ENDIF.
      ENDIF.

      " ── Затем parse_vars и parse_calcs — читают tt_calls-bindings ─
      IF lv_key2-name = 'DATA' OR lv_key2-name = 'CLASS-DATA' OR lv_key2-name = 'COMPUTE'.
        DATA(lo_vars2) = NEW zcl_ace_parse_vars( ).
        lo_vars2->zif_ace_stmt_handler~handle(
          EXPORTING io_scan = <prog2>-scan i_stmt_idx = i_stmt_idx
            i_program = lv_prg2 i_include = lv_inc2
            i_class = i_class i_evtype = i_evtype i_ev_name = i_ev_name
          CHANGING cs_source = cs_source ).
      ENDIF.
      IF lv_key2-name = 'COMPUTE' OR lv_key2-name = '+CALL_METHOD'.
        DATA(lo_calcs2) = NEW zcl_ace_parse_calcs( ).
        lo_calcs2->zif_ace_stmt_handler~handle(
          EXPORTING io_scan = <prog2>-scan i_stmt_idx = i_stmt_idx
            i_program = lv_prg2 i_include = lv_inc2
          CHANGING cs_source = cs_source ).
      ENDIF.

      READ TABLE <prog2>-t_keywords WITH KEY index = i_stmt_idx ASSIGNING FIELD-SYMBOL(<kw2>).
      IF sy-subrc = 0. <kw2>-calls_parsed = abap_true. ENDIF.
      RETURN.
    ENDIF.

    READ TABLE cs_source-tt_progs WITH KEY include = i_include TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.

    IF i_class IS NOT INITIAL. lv_class = i_class. ENDIF.
    IF lv_class IS INITIAL AND i_include IS NOT INITIAL.
      DATA(lv_inc_len2) = strlen( i_include ).
      IF lv_inc_len2 >= 2.
        DATA(lv_sfx) = substring( val = i_include off = lv_inc_len2 - 2 len = 2 ).
        IF lv_sfx = 'CU' OR lv_sfx = 'CO' OR lv_sfx = 'CI' OR lv_sfx = 'CP'.
          DATA(lv_cls_from_inc) = CONV string( i_include ).
          REPLACE REGEX '=+(CU|CO|CI|CP)$' IN lv_cls_from_inc WITH ''.
          IF lv_cls_from_inc IS NOT INITIAL. lv_class = lv_cls_from_inc. ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA(lo_src)  = cl_ci_source_include=>create( p_name = i_include ).
    DATA(lo_scan) = NEW cl_ci_scan( p_include = lo_src ).

    DATA ls_prog TYPE zif_ace_parse_data=>ts_prog.
    ls_prog-program    = i_program.
    ls_prog-include    = i_include.
    ls_prog-source_tab = lo_src->lines.
    ls_prog-scan       = lo_scan.
    ls_prog-v_source   = lo_src->lines.
    APPEND ls_prog TO cs_source-tt_progs.

    DATA(lo_events)     = NEW zcl_ace_parse_events( ).
    DATA(lo_calls_line) = NEW zcl_ace_parse_calls_line( ).
    DATA(lo_params)     = NEW zcl_ace_parse_params( ).
    DATA(lo_vars)       = NEW zcl_ace_parse_vars( ).

    DATA lt_params_kws TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    INSERT `METHODS`       INTO TABLE lt_params_kws.
    INSERT `CLASS-METHODS` INTO TABLE lt_params_kws.
    INSERT `FORM`          INTO TABLE lt_params_kws.

    lo_events->zif_ace_stmt_handler~handle(
      EXPORTING io_scan = lo_scan i_stmt_idx = 0
        i_program = i_program i_include = i_include
      CHANGING cs_source = cs_source ).

    ASSIGN cs_source-tt_progs[ lines( cs_source-tt_progs ) ] TO FIELD-SYMBOL(<ls_prog>).

    LOOP AT lo_scan->statements INTO DATA(ls_kw_stmt).

      IF ls_kw_stmt-level <> 1.
        READ TABLE lo_scan->levels INDEX ls_kw_stmt-level INTO DATA(ls_kw_level).
        NEW zcl_ace_parser( )->parse_tokens(
          EXPORTING i_program = i_program i_include = ls_kw_level-name i_class = lv_class
          CHANGING  cs_source = cs_source ).
        CONTINUE.
      ENDIF.

      DATA(lv_kw_idx) = sy-tabix.

      CHECK ls_kw_stmt-type <> 'P'.

      READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from     INTO DATA(ls_kw_tok).
      CHECK sy-subrc = 0.
      READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 1 INTO DATA(ls_tok2).
      READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 2 INTO DATA(ls_tok3).
      READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 3 INTO DATA(ls_tok4).

      DATA(lv_kw_name) = SWITCH string( ls_kw_stmt-type
        WHEN 'C' THEN 'COMPUTE'
        WHEN 'D' THEN 'COMPUTE'
        WHEN 'A' THEN '+CALL_METHOD'
        ELSE          ls_kw_tok-str ).

      IF ls_kw_tok-str = 'CLASS' AND ls_tok3-str = 'DEFINITION' AND ls_tok4-str = 'DEFERRED'.
        APPEND VALUE zif_ace_parse_data=>ts_kword(
          program = i_program include = i_include
          index   = lv_kw_idx line    = ls_kw_tok-row
          v_line  = ls_kw_tok-row     name    = lv_kw_name
          from    = ls_kw_stmt-from   to      = ls_kw_stmt-to
        ) TO <ls_prog>-t_keywords.
        CONTINUE.
      ENDIF.

      IF ls_kw_tok-str = 'CLASS' AND ls_tok2-str IS NOT INITIAL.
        IF ls_tok3-str = 'DEFINITION' OR ls_tok3-str = 'IMPLEMENTATION'.
          lv_class = ls_tok2-str. CLEAR lv_interface.
        ENDIF.
      ENDIF.
      IF ls_kw_tok-str = 'INTERFACE'.
        CLEAR: lv_class, lv_eventname, lv_eventtype.
        lv_interface = ls_tok2-str.
      ENDIF.
      IF ls_kw_tok-str = 'ENDINTERFACE'. CLEAR lv_interface. ENDIF.
      IF ls_kw_tok-str = 'METHOD'.    lv_eventtype = 'METHOD'. lv_eventname = ls_tok2-str. CLEAR lv_section. ENDIF.
      IF ls_kw_tok-str = 'FORM'.      lv_eventtype = 'FORM'.   lv_eventname = ls_tok2-str. CLEAR lv_section. ENDIF.
      IF ls_kw_tok-str = 'MODULE'.    lv_eventtype = 'MODULE'.  lv_eventname = ls_tok2-str. CLEAR lv_section. ENDIF.
      IF ls_kw_tok-str = 'ENDCLASS' OR ls_kw_tok-str = 'ENDINTERFACE'. CLEAR: lv_section, lv_eventname, lv_eventtype. ENDIF.
      IF ls_kw_tok-str = 'PUBLIC'    AND ls_tok2-str = 'SECTION'. lv_section = 'PUBLIC'.    ENDIF.
      IF ls_kw_tok-str = 'PROTECTED' AND ls_tok2-str = 'SECTION'. lv_section = 'PROTECTED'. ENDIF.
      IF ls_kw_tok-str = 'PRIVATE'   AND ls_tok2-str = 'SECTION'. lv_section = 'PRIVATE'.   ENDIF.

      DATA(lv_eff_kw) = SWITCH string( ls_kw_stmt-type
        WHEN 'C' THEN 'COMPUTE'
        WHEN 'D' THEN 'COMPUTE'
        WHEN 'A' THEN '+CALL_METHOD'
        ELSE          ls_kw_tok-str ).
      IF lv_eff_kw = 'CALL'.
        READ TABLE lo_scan->tokens INDEX ls_kw_stmt-from + 1 INTO DATA(ls_tok_d).
        IF sy-subrc = 0. lv_eff_kw = |CALL { ls_tok_d-str }|. ENDIF.
      ENDIF.

      APPEND VALUE zif_ace_parse_data=>ts_kword(
        program = i_program include = i_include
        index   = lv_kw_idx line    = ls_kw_tok-row
        v_line  = ls_kw_tok-row     name    = lv_kw_name
        from    = ls_kw_stmt-from   to      = ls_kw_stmt-to
      ) TO <ls_prog>-t_keywords.

      IF lv_eff_kw = 'CLASS'   OR lv_eff_kw = 'INTERFACE'
      OR lv_eff_kw = 'PUBLIC'  OR lv_eff_kw = 'PROTECTED' OR lv_eff_kw = 'PRIVATE'
      OR lv_eff_kw = 'METHODS' OR lv_eff_kw = 'CLASS-METHODS'
      OR lv_eff_kw = 'METHOD'  OR lv_eff_kw = 'FORM'
      OR lv_eff_kw = 'MODULE'  OR lv_eff_kw = 'FUNCTION'.
        lo_calls_line->zif_ace_stmt_handler~handle(
          EXPORTING io_scan = lo_scan i_class = lv_class i_interface = lv_interface
            i_stmt_idx = lv_kw_idx i_program = i_program i_include = i_include
          CHANGING cs_source = cs_source ).
      ENDIF.

      READ TABLE lt_params_kws WITH TABLE KEY table_line = ls_kw_tok-str TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lo_params->zif_ace_stmt_handler~handle(
          EXPORTING io_scan = lo_scan i_class = lv_class
            i_stmt_idx = lv_kw_idx i_program = i_program i_include = i_include
          CHANGING cs_source = cs_source ).
      ENDIF.

      DATA(lv_vars_class) = COND string(
        WHEN lv_class     IS NOT INITIAL THEN lv_class
        WHEN lv_interface IS NOT INITIAL THEN lv_interface
        ELSE '' ).

        IF lv_vars_class IS NOT INITIAL.
          IF lv_interface IS NOT INITIAL OR lv_section IS NOT INITIAL.
            lo_vars->zif_ace_stmt_handler~handle(
              EXPORTING io_scan = lo_scan i_stmt_idx = lv_kw_idx
                i_program = i_program i_include = i_include
                i_class = lv_vars_class i_evtype = lv_eventtype i_ev_name = lv_eventname
                i_section = lv_section
              CHANGING cs_source = cs_source ).
          ENDIF.
        ELSE.
          lo_vars->zif_ace_stmt_handler~handle(
            EXPORTING io_scan = lo_scan i_stmt_idx = lv_kw_idx
              i_program = i_program i_include = i_include
              i_class = lv_vars_class i_evtype = lv_eventtype i_ev_name = lv_eventname
              i_section = lv_section
            CHANGING cs_source = cs_source ).
        ENDIF.

    ENDLOOP.

    zcl_ace_parse_handlers=>collect(
      EXPORTING io_scan   = lo_scan
                i_program = i_program
                i_include = i_include
      CHANGING  cs_source = cs_source ).

    <ls_prog>-evtype     = lv_eventtype.
    <ls_prog>-evname     = lv_eventname.
    <ls_prog>-class      = lv_class.
    <ls_prog>-v_keywords = <ls_prog>-t_keywords.

  ENDMETHOD.
ENDCLASS.
