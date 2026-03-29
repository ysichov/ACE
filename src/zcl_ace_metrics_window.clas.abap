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
  " Row type
  " ---------------------------------------------------------------
  TYPES: BEGIN OF ts_row,
           name        TYPE string,
           cc          TYPE i,
           risk        TYPE string,
           n1          TYPE i,
           n2          TYPE i,
           length      TYPE i,
           eta1        TYPE i,
           eta2        TYPE i,
           vocab       TYPE i,
           volume      TYPE string,
           difficulty  TYPE string,
           effort      TYPE string,
           loc         TYPE i,
           lloc        TYPE i,
           cloc        TYPE i,
           cloc_ratio  TYPE string,
         END OF ts_row.

  DATA ls_u       TYPE zcl_ace_metrics=>ts_unit_result.
  DATA lv_ratio   TYPE string.
  DATA lv_tot_cc   TYPE i.
  DATA lv_tot_loc  TYPE i.
  DATA lv_tot_lloc TYPE i.
  DATA lv_tot_cloc TYPE i.
  DATA lv_tot_vol  TYPE f.
  DATA lv_tot_eff  TYPE f.
  DATA lv_tot_n1   TYPE i.
  DATA lv_tot_n2   TYPE i.

  " ---------------------------------------------------------------
  " Accumulate grand totals
  " ---------------------------------------------------------------
  LOOP AT ls_result-units INTO ls_u.
    ADD ls_u-cyclomatic TO lv_tot_cc.
    ADD ls_u-loc        TO lv_tot_loc.
    ADD ls_u-lloc       TO lv_tot_lloc.
    ADD ls_u-cloc       TO lv_tot_cloc.
    ADD ls_u-n1         TO lv_tot_n1.
    ADD ls_u-n2         TO lv_tot_n2.
    lv_tot_vol = lv_tot_vol + ls_u-volume.
    lv_tot_eff = lv_tot_eff + ls_u-effort.
  ENDLOOP.

  IF lv_tot_loc > 0.
    lv_ratio = |{ CONV decfloat16( lv_tot_cloc * 100 / lv_tot_loc ) DECIMALS = 1 }%|.
  ELSE.
    lv_ratio = '-'.
  ENDIF.

  " ---------------------------------------------------------------
  " 1. Text summary (как раньше)
  " ---------------------------------------------------------------
  cl_demo_output=>write_text( |=== Code Metrics: { i_program } ===, Units analysed                    : { lines( ls_result-units ) }| ).
  "cl_demo_output=>write_text( |Units analysed                    : { lines( ls_result-units ) }| ).
  cl_demo_output=>write_text( |Total Cyclomatic Complexity: { lv_tot_cc },  Avg Cyclomatic Complexity per unit: { format_f2( ls_result-avg_cyclomatic ) }|  ).
  "cl_demo_output=>write_text( |Avg Cyclomatic Complexity per unit: { format_f2( ls_result-avg_cyclomatic ) }| ).
  cl_demo_output=>write_text( |Total Halstead Volume: { format_f2( lv_tot_vol ) }, Total Effort: { format_f2( lv_tot_eff ) }| ).
  "cl_demo_output=>write_text( |Total Effort                      : { format_f2( lv_tot_eff ) }| ).
  cl_demo_output=>write_text( |LOC / LLOC / CLOC/ CLOC Ratio     : { lv_tot_loc } / { lv_tot_lloc } / { lv_tot_cloc } / { CONV decfloat16( lv_tot_cloc * 100 / lv_tot_loc ) DECIMALS = 1 }%| ).
  "cl_demo_output=>write_text( '' ).

  " ---------------------------------------------------------------
  " 2. TOTAL — одна строка таблицей
  " ---------------------------------------------------------------
  DATA lt_total TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.
  APPEND VALUE ts_row(
    name        = |{ i_program } TOTAL|
    cc          = lv_tot_cc
    risk        = '' "cc_rating( lv_tot_cc )
    n1          = lv_tot_n1      n2   = lv_tot_n2
    loc         = lv_tot_loc     lloc = lv_tot_lloc   cloc = lv_tot_cloc
    cloc_ratio  = lv_ratio
    volume      = format_f2( lv_tot_vol )
    effort      = format_f2( lv_tot_eff )
  ) TO lt_total.

  cl_demo_output=>write_data( value = lt_total name = `Total` ).
  "cl_demo_output=>write_text( '' ).

  " ---------------------------------------------------------------
  " 3. EVENTS (не METHOD и не FORM)
  " ---------------------------------------------------------------
  DATA lt_events TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.
  LOOP AT ls_result-units INTO ls_u
    WHERE unit_type <> 'METHOD' AND unit_type <> 'FORM'.
    IF ls_u-loc > 0.
      lv_ratio = |{ CONV decfloat16( ls_u-cloc * 100 / ls_u-loc ) DECIMALS = 1 }%|.
    ELSE.
      lv_ratio = '-'.
    ENDIF.
    APPEND VALUE ts_row(
      name        = |{ ls_u-unit_name }|
      cc          = ls_u-cyclomatic
      risk        = cc_rating( ls_u-cyclomatic )
      n1          = ls_u-n1        n2   = ls_u-n2
      eta1        = ls_u-big_n1    eta2 = ls_u-big_n2
      vocab       = ls_u-vocabulary
      length      = ls_u-prog_length
      volume      = format_f2( ls_u-volume )
      difficulty  = format_f2( ls_u-difficulty )
      effort      = format_f2( ls_u-effort )
      loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
      cloc_ratio  = lv_ratio
    ) TO lt_events.
  ENDLOOP.

  IF lt_events IS NOT INITIAL.
    "cl_demo_output=>write_text( '--- Events ---' ).
    cl_demo_output=>write_data( value = lt_events name = `Events` ).
    cl_demo_output=>write_text( '' ).
  ENDIF.

  " ---------------------------------------------------------------
  " 4. FORMs
  " ---------------------------------------------------------------
  DATA lt_forms TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.
  LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'FORM'.
    IF ls_u-loc > 0.
      lv_ratio = |{ CONV decfloat16( ls_u-cloc * 100 / ls_u-loc ) DECIMALS = 1 }%|.
    ELSE.
      lv_ratio = '-'.
    ENDIF.
    APPEND VALUE ts_row(
      name        = ls_u-unit_name
      cc          = ls_u-cyclomatic
      risk        = cc_rating( ls_u-cyclomatic )
      n1          = ls_u-n1        n2   = ls_u-n2
      eta1        = ls_u-big_n1    eta2 = ls_u-big_n2
      vocab       = ls_u-vocabulary
      length      = ls_u-prog_length
      volume      = format_f2( ls_u-volume )
      difficulty  = format_f2( ls_u-difficulty )
      effort      = format_f2( ls_u-effort )
      loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
      cloc_ratio  = lv_ratio
    ) TO lt_forms.
  ENDLOOP.

  IF lt_forms IS NOT INITIAL.
    "cl_demo_output=>write_text( '--- FORMs ---' ).
    cl_demo_output=>write_data( value = lt_forms name = `Forms` ).
    cl_demo_output=>write_text( '' ).
  ENDIF.

  " ---------------------------------------------------------------
  " 5. METHODs grouped by class
  " ---------------------------------------------------------------
  DATA lt_classes TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'METHOD'.
    DATA(lv_class) = ls_u-unit_name.
    FIND FIRST OCCURRENCE OF '=>' IN lv_class MATCH OFFSET DATA(lv_off).
    IF sy-subrc = 0.
      lv_class = lv_class(lv_off).
    ENDIF.
    READ TABLE lt_classes WITH KEY table_line = lv_class TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND lv_class TO lt_classes.
    ENDIF.
  ENDLOOP.

  DATA lt_rows TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.

  LOOP AT lt_classes INTO DATA(lv_cls).
    CLEAR lt_rows.
    CLEAR: lv_tot_cc, lv_tot_loc, lv_tot_lloc, lv_tot_cloc,
           lv_tot_vol, lv_tot_eff, lv_tot_n1, lv_tot_n2.

    LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'METHOD'.
      DATA(lv_mname) = ls_u-unit_name.
      DATA(lv_mcls)  = ls_u-unit_name.
      FIND FIRST OCCURRENCE OF '=>' IN lv_mname MATCH OFFSET DATA(lv_moff).
      IF sy-subrc = 0.
        lv_mcls  = lv_mname(lv_moff).
        DATA(lv_moff2) = lv_moff + 2.
        lv_mname = lv_mname+lv_moff2.
      ENDIF.
      CHECK lv_mcls = lv_cls.

      IF ls_u-loc > 0.
        lv_ratio = |{ CONV decfloat16( ls_u-cloc * 100 / ls_u-loc ) DECIMALS = 1 }%|.
      ELSE.
        lv_ratio = '-'.
      ENDIF.

      APPEND VALUE ts_row(
        name        = lv_mname
        cc          = ls_u-cyclomatic
        risk        = cc_rating( ls_u-cyclomatic )
        n1          = ls_u-n1        n2   = ls_u-n2
        eta1        = ls_u-big_n1    eta2 = ls_u-big_n2
        vocab       = ls_u-vocabulary
        length      = ls_u-prog_length
        volume      = format_f2( ls_u-volume )
        difficulty  = format_f2( ls_u-difficulty )
        effort      = format_f2( ls_u-effort )
        loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
        cloc_ratio  = lv_ratio
      ) TO lt_rows.

      ADD ls_u-cyclomatic TO lv_tot_cc.
      ADD ls_u-loc        TO lv_tot_loc.
      ADD ls_u-lloc       TO lv_tot_lloc.
      ADD ls_u-cloc       TO lv_tot_cloc.
      ADD ls_u-n1         TO lv_tot_n1.
      ADD ls_u-n2         TO lv_tot_n2.
      lv_tot_vol = lv_tot_vol + ls_u-volume.
      lv_tot_eff = lv_tot_eff + ls_u-effort.
    ENDLOOP.

    CHECK lt_rows IS NOT INITIAL.

    IF lv_tot_loc > 0.
      lv_ratio = |{ CONV decfloat16( lv_tot_cloc * 100 / lv_tot_loc ) DECIMALS = 1 }%|.
    ELSE.
      lv_ratio = '-'.
    ENDIF.

    SORT lt_rows by cc DESCENDING.

    APPEND VALUE ts_row(
      name        = |CLASS TOTAL|
      cc          = lv_tot_cc
      risk        = '' "cc_rating( lv_tot_cc )
      n1          = lv_tot_n1      n2  = lv_tot_n2
      loc         = lv_tot_loc     lloc = lv_tot_lloc   cloc = lv_tot_cloc
      cloc_ratio  = lv_ratio
      volume      = format_f2( lv_tot_vol )
      effort      = format_f2( lv_tot_eff )
    ) TO lt_rows.

    "cl_demo_output=>write_text( |--- { lv_cls } ---| ).
    cl_demo_output=>write_data( value = lt_rows name = lv_cls ).
    cl_demo_output=>write_text( '' ).

  ENDLOOP.

  " ---------------------------------------------------------------
  " 6. All methods across all classes — sorted by CC DESC
  " ---------------------------------------------------------------
  DATA lt_all TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.

  LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'METHOD'.
    IF ls_u-loc > 0.
      lv_ratio = |{ CONV decfloat16( ls_u-cloc * 100 / ls_u-loc ) DECIMALS = 1 }%|.
    ELSE.
      lv_ratio = '-'.
    ENDIF.
    APPEND VALUE ts_row(
      name        = ls_u-unit_name
      cc          = ls_u-cyclomatic
      risk        = cc_rating( ls_u-cyclomatic )
      n1          = ls_u-n1        n2   = ls_u-n2
      eta1        = ls_u-big_n1    eta2 = ls_u-big_n2
      vocab       = ls_u-vocabulary
      length      = ls_u-prog_length
      volume      = format_f2( ls_u-volume )
      difficulty  = format_f2( ls_u-difficulty )
      effort      = format_f2( ls_u-effort )
      loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
      cloc_ratio  = lv_ratio
    ) TO lt_all.
  ENDLOOP.

  SORT lt_all BY cc DESCENDING.

  IF lt_all IS NOT INITIAL.
    cl_demo_output=>write_data( value = lt_all name = `All Methods (sorted by CC)` ).
    cl_demo_output=>write_text( '' ).
  ENDIF.

  " ---------------------------------------------------------------
  " Legend
  " ---------------------------------------------------------------
  cl_demo_output=>write_text( '--- McCabe CC Risk ---' ).
  cl_demo_output=>write_text( '  1-10   LOW      Simple, low risk' ).
  cl_demo_output=>write_text( '  11-20  MEDIUM   Moderate complexity' ).
  cl_demo_output=>write_text( '  21-50  HIGH     High risk, refactor recommended' ).
  cl_demo_output=>write_text( '  50+    CRITICAL Untestable, very high risk' ).
  cl_demo_output=>write_text( '' ).
  cl_demo_output=>write_text( '--- Halstead ---' ).
  cl_demo_output=>write_text( '  N1/N2 - total operators/operands, Length = N1 + N2' ).
  cl_demo_output=>write_text( '  eta1/eta2 - distinct operators/operands, Vocab = eta1 + eta2' ).
  cl_demo_output=>write_text( '  Volume=Length*log2(Vocab)  Diff = (eta1 / 2)*(N2 / eta2)  Effort = Diff * Volume' ).
  cl_demo_output=>write_text( '  CLOC_RATIO = CLOC/LOC %  (comment density)' ).

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
