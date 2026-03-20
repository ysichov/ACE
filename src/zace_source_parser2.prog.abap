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
  data(io_debugger) = new ZCL_ACE( i_prog = p_prog ).
*  data(lo) = NEW zcl_ace_parser( ).
*  lo->parse_tokens2(
*    EXPORTING i_program = p_prog
*              i_include = p_prog
*    CHANGING  cs_source = ms_source ).

        ZCL_ACE_PARSER=>parse(
          EXPORTING i_program = p_prog i_include = p_prog
          CHANGING  cs_source = ms_source ).


write ''.
