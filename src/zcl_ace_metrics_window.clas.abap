CLASS zcl_ace_metrics_window DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS show
      IMPORTING
        is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
        i_program     TYPE program.

    CLASS-METHODS show_debug
      IMPORTING
        is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
        i_program     TYPE program.

    CLASS-METHODS build_html
      IMPORTING
        is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
        i_program     TYPE program
      RETURNING
        VALUE(rv)     TYPE w3htmltab.

  PRIVATE SECTION.

    TYPES: BEGIN OF ts_row,
             name       TYPE string,
             cc         TYPE i,
             risk       TYPE string,
             n1         TYPE i,
             n2         TYPE i,
             length     TYPE i,
             eta1       TYPE i,
             eta2       TYPE i,
             vocab      TYPE i,
             volume     TYPE string,
             difficulty TYPE string,
             effort     TYPE string,
             time_t     TYPE string,
             bugs       TYPE string,
             loc        TYPE i,
             lloc       TYPE i,
             cloc       TYPE i,
             cloc_ratio TYPE string,
             mi         TYPE string,
             mi_rating  TYPE string,
           END OF ts_row.
    TYPES tt_row TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.

    CLASS-METHODS format_f2
      IMPORTING i_val     TYPE f
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS format_time
      IMPORTING i_seconds TYPE f
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS cc_rating
      IMPORTING i_cc      TYPE i
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS mi_grade
      IMPORTING i_mi      TYPE f
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS html_hdr
      CHANGING ct_html TYPE w3htmltab.

    CLASS-METHODS html_row
      IMPORTING is_row  TYPE ts_row
      CHANGING  ct_html TYPE w3htmltab.

    CLASS-METHODS html_section
      IMPORTING i_name  TYPE string
                it_rows TYPE tt_row
      CHANGING  ct_html TYPE w3htmltab.

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

  DATA ls_u       TYPE zcl_ace_metrics=>ts_unit_result.
  DATA lv_ratio   TYPE string.
  DATA lv_tot_cc   TYPE i.
  DATA lv_tot_loc  TYPE i.
  DATA lv_tot_lloc TYPE i.
  DATA lv_tot_cloc TYPE i.
  DATA lv_tot_vol  TYPE f.
  DATA lv_tot_eff  TYPE f.
  DATA lv_tot_time_t TYPE f.
  DATA lv_tot_bugs   TYPE f.
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
    lv_tot_time_t = lv_tot_time_t + ls_u-time_t.
    lv_tot_bugs   = lv_tot_bugs   + ls_u-bugs.
  ENDLOOP.

  IF lv_tot_loc > 0.
    lv_ratio = |{ CONV decfloat16( lv_tot_cloc * 100 / lv_tot_loc ) DECIMALS = 1 }%|.
  ELSE.
    lv_ratio = '-'.
  ENDIF.

  " ---------------------------------------------------------------
  " 1. Text summary
  " ---------------------------------------------------------------
  cl_demo_output=>write_text( |=== Code Metrics: { i_program } ===, Units analysed                    : { lines( ls_result-units ) }| ).
  cl_demo_output=>write_text( |Total Cyclomatic Complexity: { lv_tot_cc },  Avg Cyclomatic Complexity per unit: { format_f2( ls_result-avg_cyclomatic ) }|  ).
  cl_demo_output=>write_text( |Total Halstead Volume: { format_f2( lv_tot_vol ) }, Total Effort: { format_f2( lv_tot_eff ) }| ).
  DATA(lv_sum_time_t) = lv_tot_eff / 18.
  cl_demo_output=>write_text( |Time: { format_time( lv_sum_time_t ) }, Expected Bugs: { format_f2( lv_tot_bugs ) }| ).

  cl_demo_output=>write_text( |LOC / LLOC / CLOC/ CLOC Ratio     : { lv_tot_loc } / { lv_tot_lloc } / { lv_tot_cloc } / { CONV decfloat16( lv_tot_cloc * 100 / lv_tot_loc ) DECIMALS = 1 }%| ).

  " ---------------------------------------------------------------
  " 2. TOTAL — одна строка таблицей
  " ---------------------------------------------------------------
  DATA lt_total TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.
  APPEND VALUE ts_row(
    name        = |{ i_program } TOTAL|
    cc          = lv_tot_cc
    risk        = ''
    n1          = lv_tot_n1        n2     = lv_tot_n2
    eta1        = ls_result-incl_big_n1
    eta2        = ls_result-incl_big_n2
    vocab       = ls_result-incl_vocabulary
    length      = ls_result-incl_prog_length
    loc         = lv_tot_loc       lloc   = lv_tot_lloc   cloc = lv_tot_cloc
    cloc_ratio  = lv_ratio
    volume      = format_f2( ls_result-incl_volume )
    difficulty  = format_f2( ls_result-incl_difficulty )
    effort      = format_f2( ls_result-incl_effort )
    time_t      = format_time( lv_tot_time_t )
    bugs        = format_f2( ls_result-incl_bugs )
  ) TO lt_total.

  cl_demo_output=>write_data( value = lt_total name = `Total` ).

  " ---------------------------------------------------------------
  " 3. EVENTS
  " ---------------------------------------------------------------
  DATA lt_events TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.
  LOOP AT ls_result-units INTO ls_u
    WHERE unit_type <> 'METHOD' AND unit_type <> 'FORM'.
    IF ls_u-loc > 0.
      lv_ratio = |{ CONV decfloat16( ls_u-cloc * 100 / ls_u-loc ) DECIMALS = 1 }%|.
    ELSE.
      lv_ratio = '-'.
    ENDIF.
    DATA(lv_mi_str)   = COND string( WHEN ls_u-mi <> 0 THEN format_f2( ls_u-mi ) ELSE '-' ).
    DATA(lv_mi_grade) = COND string(
      WHEN ls_u-mi = 0     THEN '-'
      WHEN ls_u-mi >= 85   THEN 'HIGH'
      WHEN ls_u-mi >= 65   THEN 'MEDIUM'
      ELSE                      'LOW' ).
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
      time_t      = format_time( ls_u-time_t )
      bugs        = format_f2( ls_u-bugs )
      loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
      cloc_ratio  = lv_ratio
      mi          = lv_mi_str
      mi_rating   = lv_mi_grade
    ) TO lt_events.
  ENDLOOP.

  IF lt_events IS NOT INITIAL.
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
    lv_mi_str   = COND string( WHEN ls_u-mi <> 0 THEN format_f2( ls_u-mi ) ELSE '-' ).
    lv_mi_grade = COND string(
      WHEN ls_u-mi = 0     THEN '-'
      WHEN ls_u-mi >= 85   THEN 'HIGH'
      WHEN ls_u-mi >= 65   THEN 'MEDIUM'
      ELSE                      'LOW' ).
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
      time_t      = format_time( ls_u-time_t )
      bugs        = format_f2( ls_u-bugs )
      loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
      cloc_ratio  = lv_ratio
      mi          = lv_mi_str
      mi_rating   = lv_mi_grade
    ) TO lt_forms.
  ENDLOOP.

  IF lt_forms IS NOT INITIAL.
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
           lv_tot_vol, lv_tot_eff, lv_tot_time_t, lv_tot_bugs,
           lv_tot_n1, lv_tot_n2.

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
      lv_mi_str   = COND string( WHEN ls_u-mi <> 0 THEN format_f2( ls_u-mi ) ELSE '-' ).
      lv_mi_grade = COND string(
        WHEN ls_u-mi = 0     THEN '-'
        WHEN ls_u-mi >= 85   THEN 'HIGH'
        WHEN ls_u-mi >= 65   THEN 'MEDIUM'
        ELSE                      'LOW' ).

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
        time_t      = format_time( ls_u-time_t )
        bugs        = format_f2( ls_u-bugs )
        loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
        cloc_ratio  = lv_ratio
        mi          = lv_mi_str
        mi_rating   = lv_mi_grade
      ) TO lt_rows.

      ADD ls_u-cyclomatic TO lv_tot_cc.
      ADD ls_u-loc        TO lv_tot_loc.
      ADD ls_u-lloc       TO lv_tot_lloc.
      ADD ls_u-cloc       TO lv_tot_cloc.
      ADD ls_u-n1         TO lv_tot_n1.
      ADD ls_u-n2         TO lv_tot_n2.
      lv_tot_vol    = lv_tot_vol    + ls_u-volume.
      lv_tot_eff    = lv_tot_eff    + ls_u-effort.
      lv_tot_time_t = lv_tot_time_t + ls_u-time_t.
      lv_tot_bugs   = lv_tot_bugs   + ls_u-bugs.
    ENDLOOP.

    CHECK lt_rows IS NOT INITIAL.

    IF lv_tot_loc > 0.
      lv_ratio = |{ CONV decfloat16( lv_tot_cloc * 100 / lv_tot_loc ) DECIMALS = 1 }%|.
    ELSE.
      lv_ratio = '-'.
    ENDIF.

    SORT lt_rows BY cc DESCENDING.

    READ TABLE ls_result-class_totals
      WITH KEY class_name = lv_cls
      INTO DATA(ls_ct).
    IF sy-subrc <> 0. CLEAR ls_ct. ENDIF.

    APPEND VALUE ts_row(
      name        = |CLASS TOTAL|
      cc          = lv_tot_cc
      risk        = ''
      n1          = lv_tot_n1
      n2          = lv_tot_n2
      eta1        = ls_ct-cls_big_n1
      eta2        = ls_ct-cls_big_n2
      vocab       = ls_ct-cls_vocabulary
      length      = ls_ct-cls_prog_length
      loc         = lv_tot_loc
      lloc        = lv_tot_lloc
      cloc        = lv_tot_cloc
      cloc_ratio  = lv_ratio
      volume      = format_f2( lv_tot_vol )
      difficulty  = format_f2( ls_ct-cls_difficulty )
      effort      = format_f2( lv_tot_eff )
      time_t      = format_time( lv_tot_time_t )
      bugs        = format_f2( lv_tot_bugs )
    ) TO lt_rows.

    cl_demo_output=>write_data( value = lt_rows name = lv_cls ).
    cl_demo_output=>write_text( '' ).

  ENDLOOP.

  " ---------------------------------------------------------------
  " 6. All methods sorted by CC DESC
  " ---------------------------------------------------------------
  DATA lt_all TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.

  LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'METHOD'.
    IF ls_u-loc > 0.
      lv_ratio = |{ CONV decfloat16( ls_u-cloc * 100 / ls_u-loc ) DECIMALS = 1 }%|.
    ELSE.
      lv_ratio = '-'.
    ENDIF.
    lv_mi_str   = COND string( WHEN ls_u-mi <> 0 THEN format_f2( ls_u-mi ) ELSE '-' ).
    lv_mi_grade = COND string(
      WHEN ls_u-mi = 0     THEN '-'
      WHEN ls_u-mi >= 85   THEN 'HIGH'
      WHEN ls_u-mi >= 65   THEN 'MEDIUM'
      ELSE                      'LOW' ).
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
      time_t      = format_time( ls_u-time_t )
      bugs        = format_f2( ls_u-bugs )
      loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
      cloc_ratio  = lv_ratio
      mi          = lv_mi_str
      mi_rating   = lv_mi_grade
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
  cl_demo_output=>write_text( '' ).
  cl_demo_output=>write_text( '  Time (T) = Effort / 18  (Stroud number: 18 mental discriminations/sec)' ).
  cl_demo_output=>write_text( '  Bugs (B) = Volume / 3000  (expected delivered defects, Halstead empirical formula)' ).
  cl_demo_output=>write_text( '  CLOC_RATIO = CLOC/LOC %  (comment density)' ).

  cl_demo_output=>write_text( '' ).
  cl_demo_output=>write_text( '--- Maintainability Index (MI) ---' ).
  cl_demo_output=>write_text( '  MI = 171 - 5.2*ln(V) - 0.23*G - 16.2*ln(LOC)' ).
  cl_demo_output=>write_text( '  >= 85  HIGH    Easy to maintain' ).
  cl_demo_output=>write_text( '  65-84  MEDIUM  Moderate maintainability' ).
  cl_demo_output=>write_text( '  < 65   LOW     Hard to maintain, refactor recommended' ).

  cl_demo_output=>display( ).

ENDMETHOD.


METHOD build_html.

  DATA(ls_result) = zcl_ace_metrics=>calculate(
    is_parse_data = is_parse_data
    i_program     = i_program ).

  IF ls_result-units IS INITIAL.
    APPEND |<p>No code units found for { i_program }</p>| TO rv.
    RETURN.
  ENDIF.

  DATA ls_u          TYPE zcl_ace_metrics=>ts_unit_result.
  DATA lv_ratio      TYPE string.
  DATA lv_tot_cc     TYPE i.
  DATA lv_tot_loc    TYPE i.
  DATA lv_tot_lloc   TYPE i.
  DATA lv_tot_cloc   TYPE i.
  DATA lv_tot_vol    TYPE f.
  DATA lv_tot_eff    TYPE f.
  DATA lv_tot_time_t TYPE f.
  DATA lv_tot_bugs   TYPE f.
  DATA lv_tot_n1     TYPE i.
  DATA lv_tot_n2     TYPE i.

  LOOP AT ls_result-units INTO ls_u.
    ADD ls_u-cyclomatic TO lv_tot_cc.
    ADD ls_u-loc        TO lv_tot_loc.
    ADD ls_u-lloc       TO lv_tot_lloc.
    ADD ls_u-cloc       TO lv_tot_cloc.
    ADD ls_u-n1         TO lv_tot_n1.
    ADD ls_u-n2         TO lv_tot_n2.
    lv_tot_vol    = lv_tot_vol    + ls_u-volume.
    lv_tot_eff    = lv_tot_eff    + ls_u-effort.
    lv_tot_time_t = lv_tot_time_t + ls_u-time_t.
    lv_tot_bugs   = lv_tot_bugs   + ls_u-bugs.
  ENDLOOP.

  IF lv_tot_loc > 0.
    lv_ratio = |{ CONV decfloat16( lv_tot_cloc * 100 / lv_tot_loc ) DECIMALS = 1 }%|.
  ELSE.
    lv_ratio = '-'.
  ENDIF.

  " --- HTML head + CSS ---
  APPEND '<!DOCTYPE html><html><head><meta charset="utf-8">' TO rv.
  APPEND '<style>' TO rv.
  APPEND 'body{font-family:Consolas,monospace;margin:16px;font-size:12px}' TO rv.
  APPEND 'h2{color:#2F5496;margin-bottom:4px}' TO rv.
  APPEND 'h3{color:#2F5496;margin-top:20px;margin-bottom:4px}' TO rv.
  APPEND 'table{border-collapse:collapse;width:100%;margin-bottom:12px}' TO rv.
  APPEND 'th{background:#BDD7EE;color:#1F3864;border:1px solid #9DC3E6;' TO rv.
  APPEND '   padding:4px 7px;text-align:left;font-weight:bold}' TO rv.
  APPEND 'td{border:1px solid #BDD7EE;padding:3px 7px;text-align:left}' TO rv.
  APPEND 'tr:nth-child(even) td{background:#EEF3FB}' TO rv.
  APPEND '.low{color:green}.med{color:darkorange}' TO rv.
  APPEND '.high{color:orangered;font-weight:bold}' TO rv.
  APPEND '.crit{color:red;font-weight:bold}' TO rv.
  APPEND '.mi-h{color:green}.mi-m{color:darkorange}' TO rv.
  APPEND '.mi-l{color:red;font-weight:bold}' TO rv.
  APPEND '.tot td{background:#D6E4F7;font-weight:bold}' TO rv.
  APPEND 'pre{background:#f5f5f5;padding:8px;font-size:11px;' TO rv.
  APPEND '    border:1px solid #ddd;white-space:pre-wrap;margin:4px 0}' TO rv.
  APPEND '</style></head><body>' TO rv.

  " --- Section 1: Summary ---
  APPEND |<h2>Code Metrics: { i_program }</h2>| TO rv.
  APPEND |<p>Units analysed: <b>{ lines( ls_result-units ) }</b></p>| TO rv.
  APPEND |<p>Total Cyclomatic Complexity: <b>{ lv_tot_cc }</b>| TO rv.
  APPEND |&nbsp;&nbsp;Avg CC / unit: | &&
         |<b>{ format_f2( ls_result-avg_cyclomatic ) }</b></p>| TO rv.
  APPEND |<p>Halstead Volume: <b>{ format_f2( ls_result-incl_volume ) }</b>| TO rv.
  DATA(lv_sum_time) = ls_result-incl_effort / 18.
  APPEND |&nbsp;&nbsp;Effort: <b>{ format_f2( ls_result-incl_effort ) }</b></p>| TO rv.
  APPEND |<p>Time: <b>{ format_time( lv_sum_time ) }</b>| &&
         |&nbsp;&nbsp;Expected Bugs: | &&
         |<b>{ format_f2( ls_result-incl_bugs ) }</b></p>| TO rv.
  APPEND |<p>LOC / LLOC / CLOC / CLOC%: | &&
         |<b>{ lv_tot_loc }</b> / <b>{ lv_tot_lloc }</b> / | TO rv.
  APPEND |<b>{ lv_tot_cloc }</b> / <b>{ lv_ratio }</b></p>| TO rv.

  " --- Section 2: Total ---
  DATA lt_total TYPE tt_row.
  APPEND VALUE ts_row(
    name        = |{ i_program } TOTAL|
    cc          = lv_tot_cc
    risk        = ''
    n1          = lv_tot_n1        n2     = lv_tot_n2
    eta1        = ls_result-incl_big_n1
    eta2        = ls_result-incl_big_n2
    vocab       = ls_result-incl_vocabulary
    length      = ls_result-incl_prog_length
    loc         = lv_tot_loc       lloc   = lv_tot_lloc   cloc = lv_tot_cloc
    cloc_ratio  = lv_ratio
    volume      = format_f2( ls_result-incl_volume )
    difficulty  = format_f2( ls_result-incl_difficulty )
    effort      = format_f2( ls_result-incl_effort )
    time_t      = format_time( lv_tot_time_t )
    bugs        = format_f2( ls_result-incl_bugs )
  ) TO lt_total.
  html_section( EXPORTING i_name = 'Total' it_rows = lt_total CHANGING ct_html = rv ).

  " --- Section 3: Events ---
  DATA lt_events TYPE tt_row.
  LOOP AT ls_result-units INTO ls_u
    WHERE unit_type <> 'METHOD' AND unit_type <> 'FORM'.
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
      time_t      = format_time( ls_u-time_t )
      bugs        = format_f2( ls_u-bugs )
      loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
      cloc_ratio  = lv_ratio
      mi          = COND #( WHEN ls_u-mi <> 0 THEN format_f2( ls_u-mi ) ELSE '-' )
      mi_rating   = mi_grade( ls_u-mi )
    ) TO lt_events.
  ENDLOOP.
  IF lt_events IS NOT INITIAL.
    html_section( EXPORTING i_name = 'Events' it_rows = lt_events CHANGING ct_html = rv ).
  ENDIF.

  " --- Section 4: Forms ---
  DATA lt_forms TYPE tt_row.
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
      time_t      = format_time( ls_u-time_t )
      bugs        = format_f2( ls_u-bugs )
      loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
      cloc_ratio  = lv_ratio
      mi          = COND #( WHEN ls_u-mi <> 0 THEN format_f2( ls_u-mi ) ELSE '-' )
      mi_rating   = mi_grade( ls_u-mi )
    ) TO lt_forms.
  ENDLOOP.
  IF lt_forms IS NOT INITIAL.
    html_section( EXPORTING i_name = 'Forms' it_rows = lt_forms CHANGING ct_html = rv ).
  ENDIF.

  " --- Section 5: Methods grouped by class ---
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

  LOOP AT lt_classes INTO DATA(lv_cls).
    DATA lt_rows TYPE tt_row.
    CLEAR lt_rows.
    CLEAR: lv_tot_cc, lv_tot_loc, lv_tot_lloc, lv_tot_cloc,
           lv_tot_vol, lv_tot_eff, lv_tot_time_t, lv_tot_bugs,
           lv_tot_n1, lv_tot_n2.

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
        time_t      = format_time( ls_u-time_t )
        bugs        = format_f2( ls_u-bugs )
        loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
        cloc_ratio  = lv_ratio
        mi          = COND #( WHEN ls_u-mi <> 0 THEN format_f2( ls_u-mi ) ELSE '-' )
        mi_rating   = mi_grade( ls_u-mi )
      ) TO lt_rows.

      ADD ls_u-cyclomatic TO lv_tot_cc.
      ADD ls_u-loc        TO lv_tot_loc.
      ADD ls_u-lloc       TO lv_tot_lloc.
      ADD ls_u-cloc       TO lv_tot_cloc.
      ADD ls_u-n1         TO lv_tot_n1.
      ADD ls_u-n2         TO lv_tot_n2.
      lv_tot_vol    = lv_tot_vol    + ls_u-volume.
      lv_tot_eff    = lv_tot_eff    + ls_u-effort.
      lv_tot_time_t = lv_tot_time_t + ls_u-time_t.
      lv_tot_bugs   = lv_tot_bugs   + ls_u-bugs.
    ENDLOOP.

    CHECK lt_rows IS NOT INITIAL.

    IF lv_tot_loc > 0.
      lv_ratio = |{ CONV decfloat16( lv_tot_cloc * 100 / lv_tot_loc ) DECIMALS = 1 }%|.
    ELSE.
      lv_ratio = '-'.
    ENDIF.

    SORT lt_rows BY cc DESCENDING.

    READ TABLE ls_result-class_totals
      WITH KEY class_name = lv_cls
      INTO DATA(ls_ct).
    IF sy-subrc <> 0. CLEAR ls_ct. ENDIF.

    APPEND VALUE ts_row(
      name        = 'CLASS TOTAL'
      cc          = lv_tot_cc
      risk        = ''
      n1          = lv_tot_n1        n2     = lv_tot_n2
      eta1        = ls_ct-cls_big_n1
      eta2        = ls_ct-cls_big_n2
      vocab       = ls_ct-cls_vocabulary
      length      = ls_ct-cls_prog_length
      loc         = lv_tot_loc        lloc   = lv_tot_lloc   cloc = lv_tot_cloc
      cloc_ratio  = lv_ratio
      volume      = format_f2( lv_tot_vol )
      difficulty  = format_f2( ls_ct-cls_difficulty )
      effort      = format_f2( lv_tot_eff )
      time_t      = format_time( lv_tot_time_t )
      bugs        = format_f2( lv_tot_bugs )
    ) TO lt_rows.

    html_section( EXPORTING i_name = lv_cls it_rows = lt_rows CHANGING ct_html = rv ).
  ENDLOOP.

  " --- Section 6: All methods sorted by CC DESC ---
  DATA lt_all TYPE tt_row.
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
      time_t      = format_time( ls_u-time_t )
      bugs        = format_f2( ls_u-bugs )
      loc         = ls_u-loc       lloc = ls_u-lloc    cloc = ls_u-cloc
      cloc_ratio  = lv_ratio
      mi          = COND #( WHEN ls_u-mi <> 0 THEN format_f2( ls_u-mi ) ELSE '-' )
      mi_rating   = mi_grade( ls_u-mi )
    ) TO lt_all.
  ENDLOOP.
  SORT lt_all BY cc DESCENDING.
  IF lt_all IS NOT INITIAL.
    html_section( EXPORTING
      i_name  = 'All Methods (sorted by CC)'
      it_rows = lt_all
      CHANGING ct_html = rv ).
  ENDIF.

  " --- Legend ---
  APPEND '<h3>McCabe CC Risk</h3><pre>' TO rv.
  APPEND '  1-10   LOW      Simple, low risk' TO rv.
  APPEND '  11-20  MEDIUM   Moderate complexity' TO rv.
  APPEND '  21-50  HIGH     High risk, refactor recommended' TO rv.
  APPEND '  50+    CRITICAL Untestable, very high risk' TO rv.
  APPEND '</pre>' TO rv.
  APPEND '<h3>Halstead</h3><pre>' TO rv.
  APPEND '  N1/N2  - total operators/operands   Length = N1+N2' TO rv.
  APPEND '  eta1/eta2 - distinct operators/operands   Vocab = eta1+eta2' TO rv.
  APPEND '  Volume = Length * log2(Vocab)' TO rv.
  APPEND '  Difficulty = (eta1/2) * (N2/eta2)   Effort = Diff * Volume' TO rv.
  APPEND '  Time (T) = Effort / 18  (Stroud: 18 discriminations/sec)' TO rv.
  APPEND '  Bugs (B) = Volume / 3000  (Halstead empirical formula)' TO rv.
  APPEND '  CLOC_RATIO = CLOC/LOC %  (comment density)' TO rv.
  APPEND '</pre>' TO rv.
  APPEND '<h3>Maintainability Index (MI)</h3><pre>' TO rv.
  APPEND '  MI = 171 - 5.2*ln(V) - 0.23*G - 16.2*ln(LOC)' TO rv.
  APPEND '  &gt;= 85  HIGH    Easy to maintain' TO rv.
  APPEND '  65-84  MEDIUM  Moderate maintainability' TO rv.
  APPEND '  &lt; 65   LOW     Hard to maintain, refactor recommended' TO rv.
  APPEND '</pre>' TO rv.
  APPEND '</body></html>' TO rv.

ENDMETHOD.


METHOD html_hdr.
  APPEND '<tr>' TO ct_html.
  APPEND '<th>Name</th><th>CC</th><th>Risk</th>' TO ct_html.
  APPEND '<th>N1</th><th>N2</th><th>Length</th>' TO ct_html.
  APPEND '<th>eta1</th><th>eta2</th><th>Vocab</th>' TO ct_html.
  APPEND '<th>Volume</th><th>Difficulty</th>' TO ct_html.
  APPEND '<th>Effort</th><th>Time</th><th>Bugs</th>' TO ct_html.
  APPEND '<th>LOC</th><th>LLOC</th><th>CLOC</th>' TO ct_html.
  APPEND '<th>CLOC%</th><th>MI</th><th>MI Rating</th></tr>' TO ct_html.
ENDMETHOD.


METHOD html_row.
  DATA lv_rc  TYPE string.
  DATA lv_mic TYPE string.
  CASE is_row-risk.
    WHEN 'LOW'.      lv_rc = 'low'.
    WHEN 'MEDIUM'.   lv_rc = 'med'.
    WHEN 'HIGH'.     lv_rc = 'high'.
    WHEN 'CRITICAL'. lv_rc = 'crit'.
  ENDCASE.
  CASE is_row-mi_rating.
    WHEN 'HIGH'.   lv_mic = 'mi-h'.
    WHEN 'MEDIUM'. lv_mic = 'mi-m'.
    WHEN 'LOW'.    lv_mic = 'mi-l'.
  ENDCASE.
  " Name + CC + Risk
  APPEND |<tr><td>{ is_row-name }</td>| &&
         |<td>{ is_row-cc }</td>| &&
         |<td class="{ lv_rc }">{ is_row-risk }</td>| TO ct_html.
  " Halstead counts
  APPEND |<td>{ is_row-n1 }</td><td>{ is_row-n2 }</td>| &&
         |<td>{ is_row-length }</td>| TO ct_html.
  APPEND |<td>{ is_row-eta1 }</td><td>{ is_row-eta2 }</td>| &&
         |<td>{ is_row-vocab }</td>| TO ct_html.
  " Halstead derived
  APPEND |<td>{ is_row-volume }</td>| &&
         |<td>{ is_row-difficulty }</td>| TO ct_html.
  APPEND |<td>{ is_row-effort }</td>| &&
         |<td>{ is_row-time_t }</td>| &&
         |<td>{ is_row-bugs }</td>| TO ct_html.
  " LOC group + MI
  APPEND |<td>{ is_row-loc }</td><td>{ is_row-lloc }</td>| &&
         |<td>{ is_row-cloc }</td>| TO ct_html.
  APPEND |<td>{ is_row-cloc_ratio }</td>| &&
         |<td>{ is_row-mi }</td>| &&
         |<td class="{ lv_mic }">{ is_row-mi_rating }</td></tr>| TO ct_html.
ENDMETHOD.


METHOD html_section.
  APPEND |<h3>{ i_name }</h3>| TO ct_html.
  APPEND '<table>' TO ct_html.
  html_hdr( CHANGING ct_html = ct_html ).
  LOOP AT it_rows INTO DATA(ls_row).
    " CLASS TOTAL / program TOTAL rows get a highlighted style
    IF ls_row-name CS 'TOTAL'.
      APPEND '<tr class="tot">' TO ct_html.
      APPEND |<td>{ ls_row-name }</td>| &&
             |<td>{ ls_row-cc }</td><td></td>| TO ct_html.
      APPEND |<td>{ ls_row-n1 }</td><td>{ ls_row-n2 }</td>| &&
             |<td>{ ls_row-length }</td>| TO ct_html.
      APPEND |<td>{ ls_row-eta1 }</td><td>{ ls_row-eta2 }</td>| &&
             |<td>{ ls_row-vocab }</td>| TO ct_html.
      APPEND |<td>{ ls_row-volume }</td>| &&
             |<td>{ ls_row-difficulty }</td>| TO ct_html.
      APPEND |<td>{ ls_row-effort }</td>| &&
             |<td>{ ls_row-time_t }</td>| &&
             |<td>{ ls_row-bugs }</td>| TO ct_html.
      APPEND |<td>{ ls_row-loc }</td><td>{ ls_row-lloc }</td>| &&
             |<td>{ ls_row-cloc }</td>| TO ct_html.
      APPEND |<td>{ ls_row-cloc_ratio }</td>| &&
             |<td>{ ls_row-mi }</td><td></td></tr>| TO ct_html.
    ELSE.
      html_row( EXPORTING is_row = ls_row CHANGING ct_html = ct_html ).
    ENDIF.
  ENDLOOP.
  APPEND '</table>' TO ct_html.
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


  METHOD format_time.
    " Convert seconds (TYPE F) to "Xh Ym" string, e.g. "2h 10m" or "45m" or "30s"
    DATA(lv_secs) = CONV i( i_seconds ).
    DATA(lv_h)    = lv_secs DIV 3600.
    DATA(lv_m)    = ( lv_secs MOD 3600 ) DIV 60.
    DATA(lv_s)    = lv_secs MOD 60.
    IF lv_h > 0.
      rv = |{ lv_h }h { lv_m }m|.
    ELSEIF lv_m > 0.
      rv = |{ lv_m }m { lv_s }s|.
    ELSE.
      rv = |{ lv_s }s|.
    ENDIF.
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


  METHOD mi_grade.
    rv = COND #(
      WHEN i_mi = 0   THEN '-'
      WHEN i_mi >= 85 THEN 'HIGH'
      WHEN i_mi >= 65 THEN 'MEDIUM'
      ELSE                 'LOW' ).
  ENDMETHOD.


  METHOD show_debug.
    " For each code unit shows:
    "   - header with unit name and summary counts
    "   - table of OPERATORS: token | occurrences | is_unique (first time seen)
    "   - table of OPERANDS:  token | occurrences | is_unique

    DATA(ls_result) = zcl_ace_metrics=>calculate(
      is_parse_data = is_parse_data
      i_program     = i_program ).

    IF ls_result-units IS INITIAL.
      cl_demo_output=>display( |No code units found for program { i_program }| ).
      RETURN.
    ENDIF.

    TYPES: BEGIN OF ts_tok_row,
             token      TYPE string,
             count      TYPE i,
             first_row  TYPE i,   " source row where first seen
           END OF ts_tok_row.
    TYPES tt_tok_rows TYPE STANDARD TABLE OF ts_tok_row WITH EMPTY KEY.

    cl_demo_output=>write_text( |=== Metrics Debug: { i_program } ===| ).

    LOOP AT ls_result-units INTO DATA(ls_u).

      cl_demo_output=>write_text(
        |--- { ls_u-unit_type }: { ls_u-unit_name } | &
        |  N1={ ls_u-n1 } η1={ ls_u-big_n1 } | &
        |  N2={ ls_u-n2 } η2={ ls_u-big_n2 } | &
        |  CC={ ls_u-cyclomatic }| ).

      " --- Build operator frequency table ---
      DATA lt_ops  TYPE tt_tok_rows.
      DATA lt_opds TYPE tt_tok_rows.
      CLEAR: lt_ops, lt_opds.

      LOOP AT ls_u-token_detail INTO DATA(ls_td).
        IF ls_td-kind = 'OPERATOR'.
          READ TABLE lt_ops WITH KEY token = ls_td-token ASSIGNING FIELD-SYMBOL(<op>).
          IF sy-subrc = 0.
            ADD 1 TO <op>-count.
          ELSE.
            APPEND VALUE ts_tok_row(
              token     = ls_td-token
              count     = 1
              first_row = ls_td-row
            ) TO lt_ops.
          ENDIF.
        ELSE.
          READ TABLE lt_opds WITH KEY token = ls_td-token ASSIGNING FIELD-SYMBOL(<opd>).
          IF sy-subrc = 0.
            ADD 1 TO <opd>-count.
          ELSE.
            APPEND VALUE ts_tok_row(
              token     = ls_td-token
              count     = 1
              first_row = ls_td-row
            ) TO lt_opds.
          ENDIF.
        ENDIF.
      ENDLOOP.

      SORT lt_ops  BY count DESCENDING token ASCENDING.
      SORT lt_opds BY count DESCENDING token ASCENDING.

      IF lt_ops IS NOT INITIAL.
        cl_demo_output=>write_data(
          value = lt_ops
          name  = |Operators (distinct={ lines( lt_ops ) }, total={ ls_u-n1 })| ).
      ENDIF.

      IF lt_opds IS NOT INITIAL.
        cl_demo_output=>write_data(
          value = lt_opds
          name  = |Operands (distinct={ lines( lt_opds ) }, total={ ls_u-n2 })| ).
      ENDIF.

      cl_demo_output=>write_text( '' ).

    ENDLOOP.

    cl_demo_output=>display( ).

  ENDMETHOD.
ENDCLASS.
