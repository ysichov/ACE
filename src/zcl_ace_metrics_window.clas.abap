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

    CLASS-METHODS build_html_package
      IMPORTING
        is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
        it_objects    TYPE zif_ace_parse_data=>tt_pkg_obj
        i_package     TYPE devclass
      RETURNING
        VALUE(rv)     TYPE w3htmltab.

  PRIVATE SECTION.

    TYPES: BEGIN OF ts_row,
             name       TYPE string,
             units      TYPE i,
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

    "! Running sums over a set of code units. SHOW and BUILD_HTML present the
    "! same numbers differently, so the arithmetic lives here once.
    TYPES: BEGIN OF ts_totals,
             units  TYPE i,
             cc     TYPE i,
             loc    TYPE i,
             lloc   TYPE i,
             cloc   TYPE i,
             n1     TYPE i,
             n2     TYPE i,
             vol    TYPE f,
             eff    TYPE f,
             time_t TYPE f,
             bugs   TYPE f,
           END OF ts_totals.

    "! I_PART of I_WHOLE as "12.3%", or '-' when I_WHOLE is zero.
    CLASS-METHODS pct
      IMPORTING i_part    TYPE i
                i_whole   TYPE i
      RETURNING VALUE(rv) TYPE string.

    "! Adds one unit's metrics into a running total.
    CLASS-METHODS add_unit
      IMPORTING is_unit TYPE zcl_ace_metrics=>ts_unit_result
      CHANGING  cs_tot  TYPE ts_totals.

    "! Sums every unit in the table.
    CLASS-METHODS sum_units
      IMPORTING it_units     TYPE zcl_ace_metrics=>tt_unit_results
      RETURNING VALUE(rs_tot) TYPE ts_totals.

    "! One metrics row for a single code unit. I_UNITS is what the row reports
    "! in its "units" column — SHOW leaves it at 0, BUILD_HTML counts 1 per unit.
    CLASS-METHODS unit_row
      IMPORTING is_unit   TYPE zcl_ace_metrics=>ts_unit_result
                i_name    TYPE string
                i_units   TYPE i DEFAULT 0
      RETURNING VALUE(rs) TYPE ts_row.

    "! A subtotal/total row built from accumulated sums rather than one unit.
    CLASS-METHODS totals_row
      IMPORTING is_tot    TYPE ts_totals
                i_name    TYPE string
      RETURNING VALUE(rs) TYPE ts_row.

    "! Splits a METHOD unit name "CLASS=>METH" into its two parts.
    "! A name without "=>" yields E_CLASS = the whole name and E_METHOD unchanged.
    CLASS-METHODS split_unit_name
      IMPORTING i_unit_name TYPE string
      EXPORTING e_class     TYPE string
                e_method    TYPE string.

    "! Distinct class names across all METHOD units, in order of first appearance.
    CLASS-METHODS class_names
      IMPORTING it_units     TYPE zcl_ace_metrics=>tt_unit_results
      RETURNING VALUE(rt)    TYPE string_table.

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

    "! An eta / Vocabulary cell. These are dictionary counts, defined for a
    "! single code unit or for a class - a container row that merely sums its
    "! units has none, and prints '-' rather than a misleading zero.
    CLASS-METHODS eta_cell
      IMPORTING i_val     TYPE i
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS html_hdr
      CHANGING ct_html TYPE w3htmltab.

    CLASS-METHODS html_row
      IMPORTING is_row  TYPE ts_row
      CHANGING  ct_html TYPE w3htmltab.

    CLASS-METHODS html_section
      IMPORTING i_name     TYPE string
                it_rows    TYPE tt_row
                i_numbered TYPE abap_bool OPTIONAL
      CHANGING  ct_html    TYPE w3htmltab.

    "! Page shell: doctype, CSS, opening <body>.
    CLASS-METHODS html_head
      CHANGING ct_html TYPE w3htmltab.

    "! Metric definitions and rating scales, plus the closing tags.
    CLASS-METHODS html_legend
      CHANGING ct_html TYPE w3htmltab.

    "! The whole report for ONE object - program, class pool or function group:
    "! summary paragraphs, Total table, Events, Forms, methods per class.
    "! E_HAS_UNITS stays false when the object holds no code unit at all, and
    "! nothing is appended then. ES_SUMMARY / ES_TOTALS hand the object's own
    "! numbers back so a package report can list and sum them.
    CLASS-METHODS html_object
      IMPORTING is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
                i_program     TYPE program
                i_title       TYPE string
                i_label       TYPE string
                i_anchor      TYPE string OPTIONAL
      EXPORTING e_has_units   TYPE abap_bool
                es_summary    TYPE ts_row
                es_totals     TYPE ts_totals
      CHANGING  ct_html       TYPE w3htmltab.

    "! Adds one set of sums into another - a package total over its objects.
    CLASS-METHODS add_totals
      IMPORTING is_add TYPE ts_totals
      CHANGING  cs_tot TYPE ts_totals.

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

  DATA ls_u     TYPE zcl_ace_metrics=>ts_unit_result.
  DATA ls_tot   TYPE ts_totals.

  " ---------------------------------------------------------------
  " Accumulate grand totals
  " ---------------------------------------------------------------
  ls_tot = sum_units( ls_result-units ).
  DATA(lv_ratio) = pct( i_part = ls_tot-cloc i_whole = ls_tot-loc ).

  " ---------------------------------------------------------------
  " 1. Text summary
  " ---------------------------------------------------------------
  cl_demo_output=>write_text( |=== Code Metrics: { i_program } ===, Units analysed                    : { lines( ls_result-units ) }| ).
  cl_demo_output=>write_text( |Total Cyclomatic Complexity: { ls_tot-cc },  Avg Cyclomatic Complexity per unit: { format_f2( ls_result-avg_cyclomatic ) }|  ).
  cl_demo_output=>write_text( |Total Halstead Volume: { format_f2( ls_tot-vol ) }, Total Effort: { format_f2( ls_tot-eff ) }| ).
  DATA(lv_sum_time_t) = ls_tot-eff / 18.
  cl_demo_output=>write_text( |Time: { format_time( lv_sum_time_t ) }, Expected Bugs: { format_f2( ls_tot-bugs ) }| ).

  cl_demo_output=>write_text( |LOC / LLOC / CLOC/ CLOC Ratio     : { ls_tot-loc } / { ls_tot-lloc } / { ls_tot-cloc } / { lv_ratio }| ).

  " ---------------------------------------------------------------
  " 2. TOTAL — a single summary row
  " ---------------------------------------------------------------
  DATA lt_total TYPE tt_row.
  " Additive sums only - see the note on class-scope Halstead in HTML_OBJECT.
  DATA(ls_total_row) = totals_row( is_tot = ls_tot
                                   i_name = |{ i_program } TOTAL| ).
  ls_total_row-units  = 0.
  ls_total_row-length = ls_tot-n1 + ls_tot-n2.
  APPEND ls_total_row TO lt_total.

  cl_demo_output=>write_data( value = lt_total name = `Total` ).

  " ---------------------------------------------------------------
  " 3. EVENTS
  " ---------------------------------------------------------------
  DATA lt_events TYPE tt_row.
  LOOP AT ls_result-units INTO ls_u
    WHERE unit_type <> 'METHOD' AND unit_type <> 'FORM'.
    APPEND unit_row( is_unit = ls_u i_name = ls_u-unit_name ) TO lt_events.
  ENDLOOP.

  IF lt_events IS NOT INITIAL.
    cl_demo_output=>write_data( value = lt_events name = `Events` ).
    cl_demo_output=>write_text( '' ).
  ENDIF.

  " ---------------------------------------------------------------
  " 4. FORMs
  " ---------------------------------------------------------------
  DATA lt_forms TYPE tt_row.
  LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'FORM'.
    APPEND unit_row( is_unit = ls_u i_name = ls_u-unit_name ) TO lt_forms.
  ENDLOOP.

  IF lt_forms IS NOT INITIAL.
    cl_demo_output=>write_data( value = lt_forms name = `Forms` ).
    cl_demo_output=>write_text( '' ).
  ENDIF.

  " ---------------------------------------------------------------
  " 5. METHODs grouped by class
  " ---------------------------------------------------------------
  DATA(lt_classes) = class_names( ls_result-units ).

  DATA lt_rows    TYPE tt_row.
  DATA ls_cls_tot TYPE ts_totals.

  LOOP AT lt_classes INTO DATA(lv_cls).
    CLEAR lt_rows.
    CLEAR ls_cls_tot.

    LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'METHOD'.
      split_unit_name( EXPORTING i_unit_name = ls_u-unit_name
                       IMPORTING e_class     = DATA(lv_mcls)
                                 e_method    = DATA(lv_mname) ).
      CHECK lv_mcls = lv_cls.

      APPEND unit_row( is_unit = ls_u i_name = lv_mname ) TO lt_rows.
      add_unit( EXPORTING is_unit = ls_u CHANGING cs_tot = ls_cls_tot ).
    ENDLOOP.

    CHECK lt_rows IS NOT INITIAL.

    SORT lt_rows BY cc DESCENDING.

    READ TABLE ls_result-class_totals
      WITH KEY class_name = lv_cls
      INTO DATA(ls_ct).
    IF sy-subrc <> 0. CLEAR ls_ct. ENDIF.

    DATA(ls_cls_row) = totals_row( is_tot = ls_cls_tot
                                   i_name = |CLASS TOTAL| ).
    ls_cls_row-units      = 0.
    ls_cls_row-eta1       = ls_ct-cls_big_n1.
    ls_cls_row-eta2       = ls_ct-cls_big_n2.
    ls_cls_row-vocab      = ls_ct-cls_vocabulary.
    ls_cls_row-length     = ls_ct-cls_prog_length.
    ls_cls_row-difficulty = format_f2( ls_ct-cls_difficulty ).
    APPEND ls_cls_row TO lt_rows.

    cl_demo_output=>write_data( value = lt_rows name = lv_cls ).
    cl_demo_output=>write_text( '' ).

  ENDLOOP.

  " ---------------------------------------------------------------
  " 6. All methods sorted by CC DESC
  " ---------------------------------------------------------------
  DATA lt_all TYPE tt_row.

  LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'METHOD'.
    APPEND unit_row( is_unit = ls_u i_name = ls_u-unit_name ) TO lt_all.
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

  html_head( CHANGING ct_html = rv ).

  html_object(
    EXPORTING
      is_parse_data = is_parse_data
      i_program     = i_program
      i_title       = |Code Metrics: { i_program }|
      i_label       = |{ i_program }|
    IMPORTING
      e_has_units   = DATA(lv_has_units)
    CHANGING
      ct_html       = rv ).

  IF lv_has_units = abap_false.
    APPEND |<p>No code units found for { i_program }</p>| TO rv.
    APPEND '</body></html>' TO rv.
    RETURN.
  ENDIF.

  html_legend( CHANGING ct_html = rv ).

ENDMETHOD.


METHOD build_html_package.

  html_head( CHANGING ct_html = rv ).

  DATA lt_overview TYPE tt_row.
  DATA lt_details  TYPE w3htmltab.
  DATA lt_empty    TYPE string_table.
  DATA ls_pkg_tot  TYPE ts_totals.
  DATA lv_idx      TYPE i.
  DATA lv_objects  TYPE i.

  " One calculation per object, in the order the package tree lists them:
  " programs first, then classes, interfaces, function groups.
  LOOP AT it_objects INTO DATA(ls_obj).
    lv_idx += 1.
    DATA(lv_anchor) = |o{ lv_idx }|.
    DATA(lv_oname)  = CONV string( ls_obj-obj_name ).

    html_object(
      EXPORTING
        is_parse_data = is_parse_data
        i_program     = CONV #( ls_obj-prog )
        i_title       = |{ ls_obj-obj_type } { lv_oname }|
        i_label       = lv_oname
        i_anchor      = lv_anchor
      IMPORTING
        e_has_units   = DATA(lv_has_units)
        es_summary    = DATA(ls_summary)
        es_totals     = DATA(ls_obj_tot)
      CHANGING
        ct_html       = lt_details ).

    IF lv_has_units = abap_false.
      APPEND |{ ls_obj-obj_type } { lv_oname }| TO lt_empty.
      CONTINUE.
    ENDIF.

    lv_objects += 1.
    ls_summary-name = |<a href="#{ lv_anchor }">{ lv_oname }</a>|.
    APPEND ls_summary TO lt_overview.
    add_totals( EXPORTING is_add = ls_obj_tot CHANGING cs_tot = ls_pkg_tot ).
  ENDLOOP.

  APPEND |<h2>Code Metrics: package { i_package }</h2>| TO rv.

  IF lt_overview IS INITIAL.
    APPEND |<p>No code units found in package { i_package }</p>| TO rv.
    APPEND '</body></html>' TO rv.
    RETURN.
  ENDIF.

  APPEND |<p>Objects with code: <b>{ lv_objects }</b>| TO rv.
  APPEND |&nbsp;&nbsp;Units analysed: <b>{ ls_pkg_tot-units }</b></p>| TO rv.
  APPEND |<p>Total Cyclomatic Complexity: <b>{ ls_pkg_tot-cc }</b>| TO rv.
  APPEND |&nbsp;&nbsp;LOC / LLOC / CLOC / CLOC%: | &&
         |<b>{ ls_pkg_tot-loc }</b> / <b>{ ls_pkg_tot-lloc }</b> / | TO rv.
  APPEND |<b>{ ls_pkg_tot-cloc }</b> / | &&
         |<b>{ pct( i_part = ls_pkg_tot-cloc i_whole = ls_pkg_tot-loc ) }</b></p>| TO rv.

  " eta / Vocab / Difficulty are dictionary-based and only defined inside one
  " object, so the package row sums what is summable and leaves those empty.
  DATA(ls_pkg_row) = totals_row( is_tot = ls_pkg_tot
                                 i_name = |PACKAGE { i_package } TOTAL| ).
  ls_pkg_row-length = ls_pkg_tot-n1 + ls_pkg_tot-n2.
  APPEND ls_pkg_row TO lt_overview.

  html_section( EXPORTING i_name  = |Objects ({ lv_objects })|
                          it_rows = lt_overview
                CHANGING  ct_html = rv ).

  APPEND LINES OF lt_details TO rv.

  IF lt_empty IS NOT INITIAL.
    APPEND |<h3>Objects without code units ({ lines( lt_empty ) })</h3><pre>| TO rv.
    LOOP AT lt_empty INTO DATA(lv_empty).
      APPEND |  { lv_empty }| TO rv.
    ENDLOOP.
    APPEND '</pre>' TO rv.
  ENDIF.

  html_legend( CHANGING ct_html = rv ).

ENDMETHOD.


METHOD html_object.

  CLEAR: e_has_units, es_summary, es_totals.

  DATA(ls_result) = zcl_ace_metrics=>calculate(
    is_parse_data = is_parse_data
    i_program     = i_program ).

  IF ls_result-units IS INITIAL.
    RETURN.
  ENDIF.
  e_has_units = abap_true.

  DATA ls_u   TYPE zcl_ace_metrics=>ts_unit_result.
  DATA ls_tot TYPE ts_totals.

  ls_tot = sum_units( ls_result-units ).
  DATA(lv_ratio) = pct( i_part = ls_tot-cloc i_whole = ls_tot-loc ).

  " --- Section 2: Total (built first so header can reference its values) ---
  " Compute per-group subtotals for Events, Forms, each Class
  TYPES: BEGIN OF lty_cls_sub,
           name TYPE string,
           tot  TYPE ts_totals,
         END OF lty_cls_sub.
  DATA lt_cls_sub TYPE TABLE OF lty_cls_sub WITH EMPTY KEY.
  DATA ls_ev_sub  TYPE ts_totals.
  DATA ls_fo_sub  TYPE ts_totals.

  LOOP AT ls_result-units INTO ls_u.
    DATA(lv_grp) = ls_u-unit_name.
    CASE ls_u-unit_type.
      WHEN 'METHOD'.
        FIND FIRST OCCURRENCE OF '=>' IN lv_grp MATCH OFFSET DATA(lv_goff).
        IF sy-subrc = 0. lv_grp = lv_grp(lv_goff). ENDIF.
        READ TABLE lt_cls_sub WITH KEY name = lv_grp ASSIGNING FIELD-SYMBOL(<cls_sub>).
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO lt_cls_sub ASSIGNING <cls_sub>.
          <cls_sub>-name = lv_grp.
        ENDIF.
        add_unit( EXPORTING is_unit = ls_u CHANGING cs_tot = <cls_sub>-tot ).
      WHEN 'FORM'.
        add_unit( EXPORTING is_unit = ls_u CHANGING cs_tot = ls_fo_sub ).
      WHEN OTHERS.
        add_unit( EXPORTING is_unit = ls_u CHANGING cs_tot = ls_ev_sub ).
    ENDCASE.
  ENDLOOP.

  DATA lt_total TYPE tt_row.
  DATA ls_sub   TYPE lty_cls_sub.

  IF ls_ev_sub-units > 0.
    APPEND totals_row( is_tot = ls_ev_sub i_name = 'Events' ) TO lt_total.
  ENDIF.

  IF ls_fo_sub-units > 0.
    APPEND totals_row( is_tot = ls_fo_sub i_name = 'Forms' ) TO lt_total.
  ENDIF.

  LOOP AT lt_cls_sub INTO ls_sub.
    READ TABLE ls_result-class_totals WITH KEY class_name = ls_sub-name INTO DATA(ls_ct2).
    IF sy-subrc <> 0. CLEAR ls_ct2. ENDIF.
    DATA(ls_sub_row) = totals_row( is_tot = ls_sub-tot i_name = ls_sub-name ).
    ls_sub_row-eta1       = ls_ct2-cls_big_n1.
    ls_sub_row-eta2       = ls_ct2-cls_big_n2.
    ls_sub_row-vocab      = ls_ct2-cls_vocabulary.
    ls_sub_row-length     = ls_ct2-cls_prog_length.
    ls_sub_row-difficulty = format_f2( ls_ct2-cls_difficulty ).
    APPEND ls_sub_row TO lt_total.
  ENDLOOP.

  " Grand total row. The object is a container, so it sums what is additive
  " across its units. eta1 / eta2 / Vocab / Difficulty are dictionary-based and
  " stay at class scope: merged per program they would make the same code look
  " denser as a monolith than as separate classes, which says nothing about the
  " code itself.
  DATA(ls_grand) = totals_row( is_tot = ls_tot
                               i_name = |{ i_label } TOTAL| ).
  ls_grand-units  = lines( ls_result-units ).
  ls_grand-length = ls_tot-n1 + ls_tot-n2.
  APPEND ls_grand TO lt_total.

  es_summary = ls_grand.
  es_totals  = ls_tot.

  " --- Section 1: Summary ---
  IF i_anchor IS INITIAL.
    APPEND |<h2>{ i_title }</h2>| TO ct_html.
  ELSE.
    APPEND |<h2 id="{ i_anchor }">{ i_title }</h2>| TO ct_html.
  ENDIF.
  APPEND |<p>Units analysed: <b>{ lines( ls_result-units ) }</b></p>| TO ct_html.
  APPEND |<p>Total Cyclomatic Complexity: <b>{ ls_tot-cc }</b>| TO ct_html.
  APPEND |&nbsp;&nbsp;Avg CC / unit: | &&
         |<b>{ format_f2( ls_result-avg_cyclomatic ) }</b></p>| TO ct_html.
  APPEND |<p>Halstead Volume: <b>{ ls_grand-volume }</b>| TO ct_html.
  APPEND |&nbsp;&nbsp;Effort: <b>{ ls_grand-effort }</b></p>| TO ct_html.
  APPEND |<p>Time: <b>{ ls_grand-time_t }</b>| &&
         |&nbsp;&nbsp;Expected Bugs: <b>{ ls_grand-bugs }</b></p>| TO ct_html.
  APPEND |<p>LOC / LLOC / CLOC / CLOC%: | &&
         |<b>{ ls_tot-loc }</b> / <b>{ ls_tot-lloc }</b> / | TO ct_html.
  APPEND |<b>{ ls_tot-cloc }</b> / <b>{ lv_ratio }</b></p>| TO ct_html.

  html_section( EXPORTING i_name = 'Total' it_rows = lt_total CHANGING ct_html = ct_html ).

  " --- Section 3: Events ---
  DATA lt_events TYPE tt_row.
  LOOP AT ls_result-units INTO ls_u
    WHERE unit_type <> 'METHOD' AND unit_type <> 'FORM'.
    APPEND unit_row( is_unit = ls_u i_name = ls_u-unit_name i_units = 1 ) TO lt_events.
  ENDLOOP.
  IF lt_events IS NOT INITIAL.
    IF ls_ev_sub-units > 1.
      APPEND totals_row( is_tot = ls_ev_sub i_name = 'TOTAL' ) TO lt_events.
    ENDIF.
    html_section( EXPORTING i_name = 'Events' it_rows = lt_events i_numbered = abap_true CHANGING ct_html = ct_html ).
  ENDIF.

  " --- Section 4: Forms ---
  DATA lt_forms TYPE tt_row.
  LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'FORM'.
    APPEND unit_row( is_unit = ls_u i_name = ls_u-unit_name i_units = 1 ) TO lt_forms.
  ENDLOOP.
  IF lt_forms IS NOT INITIAL.
    IF ls_fo_sub-units > 1.
      APPEND totals_row( is_tot = ls_fo_sub i_name = 'TOTAL' ) TO lt_forms.
    ENDIF.
    html_section( EXPORTING i_name = 'Forms' it_rows = lt_forms i_numbered = abap_true CHANGING ct_html = ct_html ).
  ENDIF.

  " --- Section 5: Methods grouped by class ---
  DATA(lt_classes) = class_names( ls_result-units ).

  DATA lt_rows    TYPE tt_row.
  DATA ls_cls_tot TYPE ts_totals.

  LOOP AT lt_classes INTO DATA(lv_cls).
    CLEAR lt_rows.
    CLEAR ls_cls_tot.

    LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'METHOD'.
      split_unit_name( EXPORTING i_unit_name = ls_u-unit_name
                       IMPORTING e_class     = DATA(lv_mcls)
                                 e_method    = DATA(lv_mname) ).
      CHECK lv_mcls = lv_cls.

      APPEND unit_row( is_unit = ls_u i_name = lv_mname i_units = 1 ) TO lt_rows.
      add_unit( EXPORTING is_unit = ls_u CHANGING cs_tot = ls_cls_tot ).
    ENDLOOP.

    CHECK lt_rows IS NOT INITIAL.

    SORT lt_rows BY cc DESCENDING.

    READ TABLE ls_result-class_totals
      WITH KEY class_name = lv_cls
      INTO DATA(ls_ct).
    IF sy-subrc <> 0. CLEAR ls_ct. ENDIF.

    DATA(ls_cls_row) = totals_row( is_tot = ls_cls_tot i_name = 'CLASS TOTAL' ).
    ls_cls_row-units      = lines( lt_rows ).
    ls_cls_row-eta1       = ls_ct-cls_big_n1.
    ls_cls_row-eta2       = ls_ct-cls_big_n2.
    ls_cls_row-vocab      = ls_ct-cls_vocabulary.
    ls_cls_row-length     = ls_ct-cls_prog_length.
    ls_cls_row-difficulty = format_f2( ls_ct-cls_difficulty ).
    APPEND ls_cls_row TO lt_rows.

    html_section( EXPORTING i_name = lv_cls it_rows = lt_rows i_numbered = abap_true CHANGING ct_html = ct_html ).
  ENDLOOP.

  " --- Section 6: All methods sorted by CC DESC ---
  DATA lt_all TYPE tt_row.
  LOOP AT ls_result-units INTO ls_u WHERE unit_type = 'METHOD'.
    APPEND unit_row( is_unit = ls_u i_name = ls_u-unit_name ) TO lt_all.
  ENDLOOP.
  SORT lt_all BY cc DESCENDING.
  IF lt_all IS NOT INITIAL.
    html_section( EXPORTING
      i_name     = 'All Methods (sorted by CC)'
      it_rows    = lt_all
      i_numbered = abap_true
      CHANGING ct_html = ct_html ).
  ENDIF.

ENDMETHOD.


METHOD html_head.

  APPEND '<!DOCTYPE html><html><head><meta charset="utf-8">' TO ct_html.
  APPEND '<style>' TO ct_html.
  APPEND 'body{font-family:Consolas,monospace;margin:16px;font-size:12px}' TO ct_html.
  APPEND 'h2{color:#2F5496;margin-bottom:4px}' TO ct_html.
  APPEND 'h3{color:#2F5496;margin-top:20px;margin-bottom:4px}' TO ct_html.
  APPEND 'table{border-collapse:collapse;width:100%;margin-bottom:12px}' TO ct_html.
  APPEND 'th{background:#BDD7EE;color:#1F3864;border:1px solid #9DC3E6;' TO ct_html.
  APPEND '   padding:4px 7px;text-align:left;font-weight:bold}' TO ct_html.
  APPEND 'td{border:1px solid #BDD7EE;padding:3px 7px;text-align:left}' TO ct_html.
  APPEND 'tr:nth-child(even) td{background:#EEF3FB}' TO ct_html.
  APPEND '.low{color:green}.med{color:darkorange}' TO ct_html.
  APPEND '.high{color:orangered;font-weight:bold}' TO ct_html.
  APPEND '.crit{color:red;font-weight:bold}' TO ct_html.
  APPEND '.mi-h{color:green}.mi-m{color:darkorange}' TO ct_html.
  APPEND '.mi-l{color:red;font-weight:bold}' TO ct_html.
  APPEND '.tot td{background:#D6E4F7;font-weight:bold}' TO ct_html.
  APPEND 'pre{background:#f5f5f5;padding:8px;font-size:11px;' TO ct_html.
  APPEND '    border:1px solid #ddd;white-space:pre-wrap;margin:4px 0}' TO ct_html.
  APPEND '</style></head><body>' TO ct_html.
ENDMETHOD.


METHOD html_legend.

  APPEND '<h3>LOC / LLOC / CLOC</h3><pre>' TO ct_html.
  APPEND '  LOC   - Lines of Code (total lines including blanks and comments)' TO ct_html.
  APPEND '  LLOC  - Logical Lines of Code (executable statements only)' TO ct_html.
  APPEND '  CLOC  - Comment Lines of Code (lines containing comments)' TO ct_html.
  APPEND '  CLOC% - Comment density = CLOC / LOC * 100' TO ct_html.
  APPEND '</pre>' TO ct_html.
  APPEND '<h3>McCabe CC Risk</h3><pre>' TO ct_html.
  APPEND '  1-10   LOW      Simple, low risk' TO ct_html.
  APPEND '  11-20  MEDIUM   Moderate complexity' TO ct_html.
  APPEND '  21-50  HIGH     High risk, refactor recommended' TO ct_html.
  APPEND '  50+    CRITICAL Untestable, very high risk' TO ct_html.
  APPEND '</pre>' TO ct_html.
  APPEND '<h3>Halstead</h3><pre>' TO ct_html.
  APPEND '  N1/N2  - total operators/operands   Length = N1+N2' TO ct_html.
  APPEND '  eta1/eta2 - distinct operators/operands   Vocab = eta1+eta2' TO ct_html.
  APPEND '  Volume = Length * log2(Vocab)' TO ct_html.
  APPEND '  Difficulty = (eta1/2) * (N2/eta2)   Effort = Diff * Volume' TO ct_html.
  APPEND '  Time (T) = Effort / 18  (Stroud: 18 discriminations/sec)' TO ct_html.
  APPEND '  Bugs (B) = Volume / 3000  (Halstead empirical formula)' TO ct_html.
  APPEND '  CLOC_RATIO = CLOC/LOC %  (comment density)' TO ct_html.
  APPEND '</pre>' TO ct_html.
  APPEND '<h3>Maintainability Index (MI)</h3><pre>' TO ct_html.
  APPEND '  MI = 171 - 5.2*ln(V) - 0.23*G - 16.2*ln(LOC)' TO ct_html.
  APPEND '  &gt;= 85  HIGH    Easy to maintain' TO ct_html.
  APPEND '  65-84  MEDIUM  Moderate maintainability' TO ct_html.
  APPEND '  &lt; 65   LOW     Hard to maintain, refactor recommended' TO ct_html.
  APPEND '</pre>' TO ct_html.
  APPEND '</body></html>' TO ct_html.
ENDMETHOD.


METHOD add_totals.
  cs_tot-units += is_add-units.
  cs_tot-cc    += is_add-cc.
  cs_tot-loc   += is_add-loc.
  cs_tot-lloc  += is_add-lloc.
  cs_tot-cloc  += is_add-cloc.
  cs_tot-n1    += is_add-n1.
  cs_tot-n2    += is_add-n2.
  cs_tot-vol    = cs_tot-vol    + is_add-vol.
  cs_tot-eff    = cs_tot-eff    + is_add-eff.
  cs_tot-time_t = cs_tot-time_t + is_add-time_t.
  cs_tot-bugs   = cs_tot-bugs   + is_add-bugs.
ENDMETHOD.


METHOD html_hdr.
  APPEND '<tr>' TO ct_html.
  APPEND '<th>Name</th><th>Units</th><th>CC</th><th>Risk</th>' TO ct_html.
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
  " Name + Units + CC + Risk
  APPEND |<tr><td>{ is_row-name }</td>| &&
         |<td>{ is_row-units }</td>| &&
         |<td>{ is_row-cc }</td>| &&
         |<td class="{ lv_rc }">{ is_row-risk }</td>| TO ct_html.
  " Halstead counts
  APPEND |<td>{ is_row-n1 }</td><td>{ is_row-n2 }</td>| &&
         |<td>{ is_row-length }</td>| TO ct_html.
  APPEND |<td>{ eta_cell( is_row-eta1 ) }</td>| &&
         |<td>{ eta_cell( is_row-eta2 ) }</td>| &&
         |<td>{ eta_cell( is_row-vocab ) }</td>| TO ct_html.
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
  IF i_numbered = abap_true.
    APPEND '<tr><th>№</th>' TO ct_html.
    APPEND '<th>Name</th><th>CC</th><th>Risk</th>' TO ct_html.
    APPEND '<th>N1</th><th>N2</th><th>Length</th>' TO ct_html.
    APPEND '<th>eta1</th><th>eta2</th><th>Vocab</th>' TO ct_html.
    APPEND '<th>Volume</th><th>Difficulty</th>' TO ct_html.
    APPEND '<th>Effort</th><th>Time</th><th>Bugs</th>' TO ct_html.
    APPEND '<th>LOC</th><th>LLOC</th><th>CLOC</th>' TO ct_html.
    APPEND '<th>CLOC%</th><th>MI</th><th>MI Rating</th></tr>' TO ct_html.
  ELSE.
    html_hdr( CHANGING ct_html = ct_html ).
  ENDIF.
  DATA lv_num TYPE i.
  LOOP AT it_rows INTO DATA(ls_row).
    lv_num += 1.
    IF ls_row-name CS 'TOTAL'.
      APPEND '<tr class="tot">' TO ct_html.
      IF i_numbered = abap_true.
        " numbered tables: (empty) | Name | CC | ...
        APPEND |<td></td><td>{ ls_row-name }</td>| &&
               |<td>{ ls_row-cc }</td><td></td>| TO ct_html.
      ELSE.
        " Total table: Name | Units | CC | ...
        APPEND |<td>{ ls_row-name }</td>| &&
               |<td>{ ls_row-units }</td>| &&
               |<td>{ ls_row-cc }</td><td></td>| TO ct_html.
      ENDIF.
      APPEND |<td>{ ls_row-n1 }</td><td>{ ls_row-n2 }</td>| &&
             |<td>{ ls_row-length }</td>| TO ct_html.
      APPEND |<td>{ eta_cell( ls_row-eta1 ) }</td>| &&
             |<td>{ eta_cell( ls_row-eta2 ) }</td>| &&
             |<td>{ eta_cell( ls_row-vocab ) }</td>| TO ct_html.
      APPEND |<td>{ ls_row-volume }</td>| &&
             |<td>{ ls_row-difficulty }</td>| TO ct_html.
      APPEND |<td>{ ls_row-effort }</td>| &&
             |<td>{ ls_row-time_t }</td>| &&
             |<td>{ ls_row-bugs }</td>| TO ct_html.
      APPEND |<td>{ ls_row-loc }</td><td>{ ls_row-lloc }</td>| &&
             |<td>{ ls_row-cloc }</td>| TO ct_html.
      APPEND |<td>{ ls_row-cloc_ratio }</td>| &&
             |<td>{ ls_row-mi }</td><td></td></tr>| TO ct_html.
    ELSEIF i_numbered = abap_true.
      DATA(lv_rc)  = COND string( WHEN ls_row-risk = 'LOW'      THEN 'low'
                                  WHEN ls_row-risk = 'MEDIUM'   THEN 'med'
                                  WHEN ls_row-risk = 'HIGH'     THEN 'high'
                                  WHEN ls_row-risk = 'CRITICAL' THEN 'crit' ).
      DATA(lv_mic) = COND string( WHEN ls_row-mi_rating = 'HIGH'   THEN 'mi-h'
                                  WHEN ls_row-mi_rating = 'MEDIUM' THEN 'mi-m'
                                  WHEN ls_row-mi_rating = 'LOW'    THEN 'mi-l' ).
      APPEND |<tr><td>{ lv_num }</td><td>{ ls_row-name }</td>| &&
             |<td>{ ls_row-cc }</td>| &&
             |<td class="{ lv_rc }">{ ls_row-risk }</td>| TO ct_html.
      APPEND |<td>{ ls_row-n1 }</td><td>{ ls_row-n2 }</td>| &&
             |<td>{ ls_row-length }</td>| TO ct_html.
      APPEND |<td>{ ls_row-eta1 }</td><td>{ ls_row-eta2 }</td>| &&
             |<td>{ ls_row-vocab }</td>| TO ct_html.
      APPEND |<td>{ ls_row-volume }</td><td>{ ls_row-difficulty }</td>| TO ct_html.
      APPEND |<td>{ ls_row-effort }</td><td>{ ls_row-time_t }</td>| &&
             |<td>{ ls_row-bugs }</td>| TO ct_html.
      APPEND |<td>{ ls_row-loc }</td><td>{ ls_row-lloc }</td>| &&
             |<td>{ ls_row-cloc }</td>| TO ct_html.
      APPEND |<td>{ ls_row-cloc_ratio }</td>| &&
             |<td>{ ls_row-mi }</td>| &&
             |<td class="{ lv_mic }">{ ls_row-mi_rating }</td></tr>| TO ct_html.
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


  METHOD eta_cell.
    rv = COND #( WHEN i_val > 0 THEN |{ i_val }| ELSE '-' ).
  ENDMETHOD.


  METHOD pct.
    rv = COND string(
      WHEN i_whole > 0
      THEN |{ CONV decfloat16( i_part * 100 / i_whole ) DECIMALS = 1 }%|
      ELSE '-' ).
  ENDMETHOD.


  METHOD add_unit.
    cs_tot-units  += 1.
    cs_tot-cc     += is_unit-cyclomatic.
    cs_tot-loc    += is_unit-loc.
    cs_tot-lloc   += is_unit-lloc.
    cs_tot-cloc   += is_unit-cloc.
    cs_tot-n1     += is_unit-n1.
    cs_tot-n2     += is_unit-n2.
    cs_tot-vol     = cs_tot-vol    + is_unit-volume.
    cs_tot-eff     = cs_tot-eff    + is_unit-effort.
    cs_tot-time_t  = cs_tot-time_t + is_unit-time_t.
    cs_tot-bugs    = cs_tot-bugs   + is_unit-bugs.
  ENDMETHOD.


  METHOD sum_units.
    LOOP AT it_units INTO DATA(ls_u).
      add_unit( EXPORTING is_unit = ls_u CHANGING cs_tot = rs_tot ).
    ENDLOOP.
  ENDMETHOD.


  METHOD unit_row.
    rs = VALUE ts_row(
      name       = i_name
      units      = i_units
      cc         = is_unit-cyclomatic
      risk       = cc_rating( is_unit-cyclomatic )
      n1         = is_unit-n1       n2   = is_unit-n2
      eta1       = is_unit-big_n1   eta2 = is_unit-big_n2
      vocab      = is_unit-vocabulary
      length     = is_unit-prog_length
      volume     = format_f2( is_unit-volume )
      difficulty = format_f2( is_unit-difficulty )
      effort     = format_f2( is_unit-effort )
      time_t     = format_time( is_unit-time_t )
      bugs       = format_f2( is_unit-bugs )
      loc        = is_unit-loc      lloc = is_unit-lloc   cloc = is_unit-cloc
      cloc_ratio = pct( i_part = is_unit-cloc i_whole = is_unit-loc )
      mi         = COND #( WHEN is_unit-mi <> 0 THEN format_f2( is_unit-mi ) ELSE '-' )
      mi_rating  = mi_grade( is_unit-mi ) ).
  ENDMETHOD.


  METHOD split_unit_name.
    e_class  = i_unit_name.
    e_method = i_unit_name.
    FIND FIRST OCCURRENCE OF '=>' IN i_unit_name MATCH OFFSET DATA(lv_off).
    CHECK sy-subrc = 0.
    e_class = i_unit_name(lv_off).
    DATA(lv_after) = lv_off + 2.
    e_method = i_unit_name+lv_after.
  ENDMETHOD.


  METHOD class_names.
    LOOP AT it_units INTO DATA(ls_u) WHERE unit_type = 'METHOD'.
      split_unit_name( EXPORTING i_unit_name = ls_u-unit_name
                       IMPORTING e_class     = DATA(lv_class) ).
      READ TABLE rt WITH KEY table_line = lv_class TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND lv_class TO rt.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD totals_row.
    rs = VALUE ts_row(
      name       = i_name
      units      = is_tot-units
      cc         = is_tot-cc
      n1         = is_tot-n1        n2   = is_tot-n2
      loc        = is_tot-loc       lloc = is_tot-lloc    cloc = is_tot-cloc
      cloc_ratio = pct( i_part = is_tot-cloc i_whole = is_tot-loc )
      volume     = format_f2( is_tot-vol )
      effort     = format_f2( is_tot-eff )
      time_t     = format_time( is_tot-time_t )
      bugs       = format_f2( is_tot-bugs ) ).
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
