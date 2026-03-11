REPORT zace_source_parser2.


parameters: p_prog  TYPE progname.
* New concept: statement-based source parser
* Architecture:
*   - Sequentially iterates over PROCEDURE statements via CL_CI_SCAN
*   - Per statement a handler from the chain is called
*   - Handlers write into cs_source (ZCL_ACE_WINDOW=>TS_SOURCE)
*
* Handler 1: Zcl_ace_parse_EVENTS
*   -> reads scan->structures, fills cs_source-T_EVENTS
*
* Handler 2: Zcl_ace_parse_CALLS_LINE
*   -> reads FORM/METHOD/MODULE/FUNCTION statements
*   -> fills cs_source-TT_CALLS_LINE
*   -> for METHOD also links to METHODS signature (def_include, def_line)

*----------------------------------------------------------------------*
* INTERFACE  zif_ace_stmt_handler
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DEMO  (runs parser on this program itself)
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA ms_source TYPE zcl_ace_window=>ts_source.

  CALL METHOD zcl_ace_parser=>parse_tokens
    EXPORTING i_program = p_prog
              i_include = p_prog
    CHANGING  cs_source = ms_source.

  WRITE: / '=== EVENTS (ms_source-t_events) ==='.
  IF ms_source-t_events IS INITIAL.
    WRITE: / '  (none - REPORT type has events in structures)'.
  ENDIF.
  LOOP AT ms_source-t_events INTO DATA(ev).
    WRITE: / |  [{ ev-stmnt_type }] { ev-name } from={ ev-stmnt_from } line={ ev-line }|.
  ENDLOOP.

  ULINE.
  WRITE: / '=== CALLS LINE (ms_source-tt_calls_line) ==='.
  LOOP AT ms_source-tt_calls_line INTO DATA(cl).
    DATA lv_cls TYPE string.
    lv_cls = COND #( WHEN cl-class IS NOT INITIAL THEN |{ cl-class }~| ELSE `` ).
    WRITE: / |  [{ cl-eventtype }] { lv_cls }{ cl-eventname }|.
    WRITE: / |    inc={ cl-include } stmt#={ cl-index }|.
    IF cl-def_include IS NOT INITIAL.
      WRITE: / |    sig: { cl-def_include } line { cl-def_line }|.
    ENDIF.
  ENDLOOP.
  IF ms_source-tt_calls_line IS INITIAL.
    WRITE: / '  (none)'.
  ENDIF.
