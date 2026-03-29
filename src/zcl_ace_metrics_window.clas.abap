CLASS zcl_ace_metrics_window DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS show
      IMPORTING
        is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
        i_program     TYPE program.

  PRIVATE SECTION.

    CLASS-METHODS format_f2
      IMPORTING i_val     TYPE f
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS cc_rating
      IMPORTING i_cc      TYPE i
      RETURNING VALUE(rv) TYPE string.

ENDCLASS.



CLASS ZCL_ACE_METRICS_WINDOW IMPLEMENTATION.


  METHOD show.

    DATA(ls_result) = zcl_ace_metrics=>calculate(
      is_parse_data = is_parse_data
      i_program     = i_program ).

    IF ls_result-units IS INITIAL.
      cl_demo_output=>display( |No code units found for program { i_program }| ).
      RETURN.
    ENDIF.

    " ---------------------------------------------------------------
    " Summary
    " ---------------------------------------------------------------
    cl_demo_output=>write_text( |=== Code Metrics: { i_program } ===| ).
    cl_demo_output=>write_text( |Units analysed    : { lines( ls_result-units ) }| ).
    cl_demo_output=>write_text( |Total Cyclomatic Complexity: { ls_result-total_cyclomatic }| ).
    cl_demo_output=>write_text( |Avg Cyclomatic Complexity per unit   : { format_f2( ls_result-avg_cyclomatic ) }| ).
    cl_demo_output=>write_text( |Total Halstead Vol: { format_f2( ls_result-total_volume ) }| ).
    cl_demo_output=>write_text( |Total Effort      : { format_f2( ls_result-total_effort ) }| ).
    cl_demo_output=>write_text(
      |LOC / LLOC / CLOC : { ls_result-total_loc } / { ls_result-total_lloc } / { ls_result-total_cloc }| ).
    cl_demo_output=>write_text( '' ).

    " ---------------------------------------------------------------
    " Per-unit detail table
    " ---------------------------------------------------------------
    cl_demo_output=>write_text( '--- Per-Unit Detail ---' ).

    TYPES: BEGIN OF ts_row,
             type       TYPE string,
             name       TYPE string,
             cc         TYPE i,
             risk       TYPE string,
             n1         TYPE i,
             n2         TYPE i,
             eta1       TYPE i,
             eta2       TYPE i,
             vocab      TYPE i,
             length     TYPE i,
             volume     TYPE string,
             difficulty TYPE string,
             effort     TYPE string,
             loc        TYPE i,
             lloc       TYPE i,
             cloc       TYPE i,
           END OF ts_row.
    DATA lt_rows TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.

    LOOP AT ls_result-units INTO DATA(ls_u).
      APPEND VALUE ts_row(
        type       = ls_u-unit_type
        name       = ls_u-unit_name
        cc         = ls_u-cyclomatic
        risk       = cc_rating( ls_u-cyclomatic )
        n1         = ls_u-n1
        n2         = ls_u-n2
        eta1       = ls_u-big_n1
        eta2       = ls_u-big_n2
        vocab      = ls_u-vocabulary
        length     = ls_u-prog_length
        volume     = format_f2( ls_u-volume )
        difficulty = format_f2( ls_u-difficulty )
        effort     = format_f2( ls_u-effort )
        loc        = ls_u-loc
        lloc       = ls_u-lloc
        cloc       = ls_u-cloc
      ) TO lt_rows.
    ENDLOOP.

    cl_demo_output=>write( lt_rows ).
    cl_demo_output=>write_text( '' ).

    " ---------------------------------------------------------------
    " Legend
    " ---------------------------------------------------------------
    cl_demo_output=>write_text( '--- McCabe CC Risk ---' ).
    cl_demo_output=>write_text( '  Cyclomatic Complexity  1-10  LOW      Simple, low risk' ).
    cl_demo_output=>write_text( '  Cyclomatic Complexity 11-20  MEDIUM   Moderate complexity' ).
    cl_demo_output=>write_text( '  Cyclomatic Complexity 21-50  HIGH     High risk, refactor recommended' ).
    cl_demo_output=>write_text( '  Cyclomatic Complexity 50+  CRITICAL Untestable, very high risk' ).
    cl_demo_output=>write_text( '' ).
    cl_demo_output=>write_text( '--- Halstead Symbols ---' ).
    cl_demo_output=>write_text( '  eta1/eta2 = distinct operators/operands' ).
    cl_demo_output=>write_text( '  N1/N2     = total operators/operands' ).
    cl_demo_output=>write_text( '  Vocab     = eta1+eta2   Length = N1+N2' ).
    cl_demo_output=>write_text( '  Volume    = Length * log2(Vocab)' ).
    cl_demo_output=>write_text( '  Diff      = (eta1/2) * (N2/eta2)' ).
    cl_demo_output=>write_text( '  Effort    = Diff * Volume  (mental work units)' ).

    cl_demo_output=>display( ).

  ENDMETHOD.


  METHOD format_f2.
    " Correctly format a TYPE F value to 2 decimal places.
    " The old approach (lv_str = i_val) produced scientific notation
    " like '1.84E+06', so format_f2 was returning just '1.84' instead
    " of the real value ~1,840,000.
    IF i_val = 0.
      rv = '0.00'.
      RETURN.
    ENDIF.

    " Use ABAP string template with DECIMALS modifier - this respects
    " the full magnitude of the float, not just its mantissa.
    DATA lv_dec TYPE decfloat34.
    lv_dec = i_val.
    rv = |{ lv_dec DECIMALS = 2 }|.
    CONDENSE rv NO-GAPS.
  ENDMETHOD.


  METHOD cc_rating.
    IF i_cc <= 10.
      rv = 'LOW'.
    ELSEIF i_cc <= 20.
      rv = 'MEDIUM'.
    ELSEIF i_cc <= 50.
      rv = 'HIGH'.
    ELSE.
      rv = 'CRITICAL'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
