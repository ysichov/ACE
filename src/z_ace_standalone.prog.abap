  REPORT z_ace. " ACE - Abap Code Explorer
  " & Multi-windows program for ABAP code analysis
  " &----------------------------------------------------------------------
  " & version: beta 0.5
  " & Git https://github.com/ysichov/ACE


  " & Written by Yurii Sychov
  " & e-mail:   ysichov@gmail.com
  " & blog:     https://ysychov.wordpress.com/blog/
  " & LinkedIn: https://www.linkedin.com/in/ysychov/
  " &----------------------------------------------------------------------

  " & External resources
  " & https://github.com/WegnerDan/abapMermaid
  " & https://github.com/oisee/vibing-steampunk

  " Main input fields for selecting the source object
INTERFACE zif_ace_stmt_handler DEFERRED.
INTERFACE zif_ace_parse_data DEFERRED.
CLASS zcl_ace_window DEFINITION DEFERRED.
CLASS zcl_ace_tree_builder DEFINITION DEFERRED.
CLASS zcl_ace_text_viewer DEFINITION DEFERRED.
CLASS zcl_ace_table_viewer DEFINITION DEFERRED.
CLASS zcl_ace_stmts DEFINITION DEFERRED.
CLASS zcl_ace_source_parser DEFINITION DEFERRED.
CLASS zcl_ace_sel_opt DEFINITION DEFERRED.
CLASS zcl_ace_rtti_tree DEFINITION DEFERRED.
CLASS zcl_ace_rtti DEFINITION DEFERRED.
CLASS zcl_ace_popup DEFINITION DEFERRED.
CLASS zcl_ace_parse_vars DEFINITION DEFERRED.
CLASS zcl_ace_parse_params DEFINITION DEFERRED.
CLASS zcl_ace_parse_handlers DEFINITION DEFERRED.
CLASS zcl_ace_parse_events DEFINITION DEFERRED.
CLASS zcl_ace_parse_calls_line DEFINITION DEFERRED.
CLASS zcl_ace_parse_calls DEFINITION DEFERRED.
CLASS zcl_ace_parse_calcs DEFINITION DEFERRED.
CLASS zcl_ace_parser DEFINITION DEFERRED.
CLASS zcl_ace_metrics_window DEFINITION DEFERRED.
CLASS zcl_ace_metrics DEFINITION DEFERRED.
CLASS zcl_ace_mermaid DEFINITION DEFERRED.
CLASS zcl_ace_keywords DEFINITION DEFERRED.
CLASS zcl_ace_html_viewer DEFINITION DEFERRED.
CLASS zcl_ace_exprs DEFINITION DEFERRED.
CLASS zcl_ace_combi_node DEFINITION DEFERRED.
CLASS zcl_ace_combi DEFINITION DEFERRED.
CLASS zcl_ace_alv_common DEFINITION DEFERRED.
CLASS zcl_ace DEFINITION DEFERRED.
INTERFACE zif_ace_parse_data.

  " --- calls (used in ts_kword) ---
  TYPES:
    BEGIN OF ts_param_binding,
      outer TYPE string,
      inner TYPE string,
      dir   TYPE char1,   " 'I'=importing/using  'E'=exporting/returning  'C'=changing
    END OF ts_param_binding .
  TYPES:
    tt_param_bindings TYPE STANDARD TABLE OF ts_param_binding WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_calls,
      class    TYPE string,
      event    TYPE string,
      type     TYPE string,
      name     TYPE string,
      outer    TYPE string,
      inner    TYPE string,
      super    TYPE boolean,
      bindings TYPE tt_param_bindings,
    END OF ts_calls .
  TYPES:
    tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer .

  " --- keyword entry ---
  TYPES:
    BEGIN OF ts_kword,
      program      TYPE string,
      include      TYPE string,
      index        TYPE i,
      line         TYPE i,
      v_line       TYPE i,
      v_from_row   TYPE i,
      v_to_row     TYPE i,
      sub          TYPE boolean,
      name         TYPE string,
      from         TYPE i,
      to           TYPE i,
      tt_calls     TYPE tt_calls,
      calls_parsed TYPE abap_bool,
    END OF ts_kword .
  TYPES:
    tt_kword TYPE STANDARD TABLE OF ts_kword WITH NON-UNIQUE DEFAULT KEY .

  " --- calls_line ---
  TYPES:
    BEGIN OF ts_calls_line,
      program     TYPE program,
      include     TYPE program,
      class       TYPE string,
      eventtype   TYPE string,
      meth_type   TYPE i,
      eventname   TYPE string,
      redefined   TYPE boolean,
      index       TYPE i,      " stmt index of METHOD/FORM/... in implementation scan
      def_ind     TYPE i,      " stmt index from METHODS declaration (definition only)
                               " if index = def_ind → no implementation, skip metrics
      def_include TYPE program,
      def_line    TYPE i,
      is_intf     TYPE boolean,
      end_idx     TYPE i,
      run2_done   TYPE abap_bool,
    END OF ts_calls_line .
  TYPES:
    tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY .

  " --- vars ---
  TYPES:
    BEGIN OF ts_vars,
      program   TYPE program,
      include   TYPE program,
      class     TYPE string,
      eventtype TYPE string,
      eventname TYPE string,
      section   TYPE string,
      name      TYPE string,
      line      TYPE i,
      type      TYPE string,
      icon      TYPE salv_de_tree_image,
    END OF ts_vars .

  " --- params ---
  TYPES:
    BEGIN OF ts_params,
      program   TYPE program,
      include   TYPE program,
      class     TYPE string,
      event     TYPE string,
      name      TYPE string,
      param     TYPE string,
      type      TYPE char1,
      preferred TYPE char1,
      line      TYPE i,
    END OF ts_params .

  " --- enhancement blocks ---
  TYPES:
    BEGIN OF ts_enh_block,
      ev_type     TYPE string,
      ev_name     TYPE string,
      position    TYPE string,
      enh_name    TYPE string,
      enh_include TYPE program,
      enh_id      TYPE i,
      from_line   TYPE i,
      to_line     TYPE i,
    END OF ts_enh_block .
  TYPES:
    tt_enh_blocks TYPE STANDARD TABLE OF ts_enh_block WITH EMPTY KEY .

  " --- parsed program/include entry ---
  TYPES:
    BEGIN OF ts_prog,
      stack         TYPE i,
      program       TYPE program,
      include       TYPE program,
      source_tab    TYPE sci_include,
      v_source      TYPE sci_include,
      v_keywords    TYPE tt_kword,
      scan          TYPE REF TO cl_ci_scan,
      t_keywords    TYPE tt_kword,
      selected      TYPE boolean,
      enh_collected TYPE boolean,
      tt_enh_blocks TYPE tt_enh_blocks,
      evtype        TYPE string,
      evname        TYPE string,
      class         TYPE string,
    END OF ts_prog .
  TYPES:
    tt_progs TYPE STANDARD TABLE OF ts_prog WITH EMPTY KEY .

  " --- events ---
  TYPES:
    BEGIN OF ts_event,
      program    TYPE program,
      include    TYPE program,
      type       TYPE string,
      stmnt_type TYPE string,
      stmnt_from TYPE i,
      stmnt_to   TYPE i,
      name       TYPE string,
      line       TYPE i,
    END OF ts_event .
  TYPES:
    tt_events TYPE STANDARD TABLE OF ts_event WITH EMPTY KEY .

  " --- calculated/composed variables ---
  TYPES:
    BEGIN OF ts_var,
      program   TYPE string,
      include   TYPE string,
      class     TYPE string,
      eventtype TYPE string,
      eventname TYPE string,
      line      TYPE i,
      name(100) TYPE c,
      type      TYPE string,
    END OF ts_var .
  TYPES:
    tt_calculated TYPE STANDARD TABLE OF ts_var WITH KEY program include class eventtype eventname line name .
  TYPES:
    tt_composed   TYPE STANDARD TABLE OF ts_var WITH KEY program include class eventtype eventname line name .

  " --- reference variables ---
  TYPES:
    BEGIN OF ts_refvar,
      program TYPE string,
      name    TYPE string,
      class   TYPE string,
    END OF ts_refvar .
  TYPES:
    tt_refvar TYPE STANDARD TABLE OF ts_refvar WITH EMPTY KEY .

  " --- internal table types used in code flow ---
  TYPES:
    BEGIN OF ts_int_tabs,
      eventtype TYPE string,
      eventname TYPE string,
      name      TYPE string,
      type      TYPE string,
    END OF ts_int_tabs .
  TYPES:
    tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY .

  " --- class metadata ---
  TYPES:
    BEGIN OF ts_meta,
      clsname    TYPE seoclsname,
      refclsname TYPE seoclsname,
      reltype    TYPE seoreltype,
      node       TYPE salv_de_node_key,
    END OF ts_meta .
  TYPES:
    tt_classes TYPE STANDARD TABLE OF ts_meta WITH EMPTY KEY .

  " --- class definition locations ---
  TYPES:
    BEGIN OF ts_class_def,
      class        TYPE string,
      super        TYPE string,
      is_intf      TYPE boolean,
      program      TYPE program,
      include      TYPE program,
      line         TYPE i,
      def_include  TYPE program,
      def_line     TYPE i,
      impl_include TYPE program,
      impl_line    TYPE i,
    END OF ts_class_def .
  TYPES:
    tt_class_defs TYPE STANDARD TABLE OF ts_class_def WITH NON-UNIQUE KEY class .

  " --- class sections (PUBLIC/PROTECTED/PRIVATE SECTION) ---
  TYPES:
    BEGIN OF ts_section,
      class   TYPE string,
      section TYPE string,
      include TYPE program,
      line    TYPE i,
    END OF ts_section .
  TYPES:
    tt_sections TYPE STANDARD TABLE OF ts_section WITH NON-UNIQUE KEY class .

  " --- SET HANDLER registration map ---
  TYPES:
    BEGIN OF ts_handler_map,
      src_class  TYPE string,
      event_name TYPE string,
      hdl_class  TYPE string,
      hdl_method TYPE string,
      include    TYPE program,
      line       TYPE i,
    END OF ts_handler_map .
  TYPES:
    tt_handler_map TYPE STANDARD TABLE OF ts_handler_map WITH EMPTY KEY .

  " --- derived table types ---
  TYPES:
    tt_vars   TYPE SORTED TABLE OF ts_vars
                WITH UNIQUE KEY program include class eventtype eventname name .
  TYPES:
    tt_params TYPE SORTED TABLE OF ts_params
                WITH UNIQUE KEY program include class event name param .

  " --- package object list (for package-level Class Map) ---
  TYPES:
    BEGIN OF ts_pkg_obj,
      obj_type TYPE trobjtype,
      obj_name TYPE sobj_name,
      prog     TYPE progname,
    END OF ts_pkg_obj .
  TYPES:
    tt_pkg_obj TYPE STANDARD TABLE OF ts_pkg_obj WITH DEFAULT KEY .

  " --- main aggregate ---
  TYPES:
    BEGIN OF ts_parse_data,
      tt_progs       TYPE tt_progs,
      t_events       TYPE tt_events,
      t_calculated   TYPE tt_calculated,
      t_composed     TYPE tt_composed,
      t_params       TYPE tt_params,
      tt_tabs        TYPE tt_tabs,
      tt_calls_line  TYPE tt_calls_line,
      t_vars         TYPE tt_vars,
      tt_refvar      TYPE tt_refvar,
      t_classes      TYPE tt_classes,
      tt_class_defs  TYPE tt_class_defs,
      tt_sections    TYPE tt_sections,
      tt_handler_map TYPE tt_handler_map,
      enh_collected  TYPE boolean,
    END OF ts_parse_data .

ENDINTERFACE.

INTERFACE zif_ace_stmt_handler.

  METHODS handle
    IMPORTING
      !io_scan     TYPE REF TO cl_ci_scan
      !i_stmt_idx  TYPE i
      !i_program   TYPE program
      !i_include   TYPE program
      !i_class     TYPE string OPTIONAL
      !i_interface TYPE string OPTIONAL
      !i_evtype    TYPE string OPTIONAL
      !i_ev_name   TYPE string OPTIONAL
      !i_section   TYPE string OPTIONAL
    CHANGING
      !cs_source   TYPE zif_ace_parse_data=>ts_parse_data .

ENDINTERFACE.

CLASS zcl_ace DEFINITION
  create public .

public section.

  types:
    BEGIN OF selection_display_s,
        ind         TYPE i,
        field_label TYPE lvc_fname,
        int_type(1),
        inherited   TYPE aqadh_type_of_icon,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string,
        high        TYPE string,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
        name        TYPE reptext,
        element     TYPE text60,
        domain      TYPE text60,
        datatype    TYPE string,
        length      TYPE i,
        color       TYPE lvc_t_scol,
        style       TYPE lvc_t_styl,
      END OF selection_display_s .
  types:
    BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string,
        high        TYPE string,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row .
  types:
    BEGIN OF sign_option_icon_s,
        sign          TYPE tvarv_sign,
        option        TYPE tvarv_opti,
        icon_name(64) TYPE c,
        icon          TYPE aqadh_type_of_icon,
      END OF sign_option_icon_s .
  types:
    BEGIN OF t_obj,
        name       TYPE string,
        alv_viewer TYPE REF TO zcl_ace_table_viewer,
      END OF t_obj .
  types:
    BEGIN OF t_popup,
        parent TYPE REF TO cl_gui_dialogbox_container,
        child  TYPE REF TO cl_gui_dialogbox_container,
      END OF t_popup .
  types:
    BEGIN OF t_classes_types,
        name TYPE string,
        full TYPE string,
        type TYPE char1,
        key  TYPE salv_de_node_key,
      END OF t_classes_types .
  types:
    BEGIN OF t_lang,
        spras(4),
        sptxt    TYPE sptxt,
      END OF t_lang .
  types:
    BEGIN OF t_stack,
        step       TYPE i,
        stacklevel TYPE tpda_stack_level,
        line       TYPE tpda_sc_line,
        program    TYPE tpda_program,
        eventtype  TYPE string,
        eventname  TYPE tpda_event,
        prg        TYPE program,
        include    TYPE tpda_include,
      END OF t_stack .
  types:
    BEGIN OF t_step_counter,
        step       TYPE i,
        stacklevel TYPE tpda_stack_level,
        line       TYPE tpda_sc_line,
        eventtype  TYPE string,
        eventname  TYPE string,
        class      TYPE string,
        first      TYPE boolean,
        last       TYPE boolean,
        program    TYPE tpda_program,
        include    TYPE tpda_include,
        time       LIKE sy-uzeit,
      END OF t_step_counter .
  types:
    " Aligned with zif_ace_parse_data=>ts_param_binding (dir added)
    BEGIN OF ts_param_binding,
        outer TYPE string,
        inner TYPE string,
        dir   TYPE char1,
      END OF ts_param_binding .
  types:
    tt_param_bindings TYPE STANDARD TABLE OF ts_param_binding WITH EMPTY KEY .
  types TS_CALLS type ZIF_ACE_PARSE_DATA=>TS_CALLS .
  types TT_CALLS type ZIF_ACE_PARSE_DATA=>TT_CALLS .
  types TS_KWORD type ZIF_ACE_PARSE_DATA=>TS_KWORD .
  types TT_KWORD type ZIF_ACE_PARSE_DATA=>TT_KWORD .
  types TS_CALLS_LINE type ZIF_ACE_PARSE_DATA=>TS_CALLS_LINE .
  types TT_CALLS_LINE type ZIF_ACE_PARSE_DATA=>TT_CALLS_LINE .
  types TS_VARS type ZIF_ACE_PARSE_DATA=>TS_VARS .
  types:
    BEGIN OF ts_var,
        program   TYPE string,
        include   TYPE string,
        class     TYPE string,
        eventtype TYPE string,
        eventname TYPE string,
        line      TYPE i,
        name(100) TYPE c,
        type      TYPE string,
      END OF ts_var .
  types:
    tt_calculated TYPE STANDARD TABLE OF ts_var WITH KEY program include class eventtype eventname line name .
  types:
    tt_composed   TYPE STANDARD TABLE OF ts_var WITH KEY program include class eventtype eventname line name .
  types:
    BEGIN OF ts_int_tabs,
        eventtype TYPE string,
        eventname TYPE string,
        name      TYPE string,
        type      TYPE string,
      END OF ts_int_tabs .
  types:
    tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY .
  types TS_PARAMS type ZIF_ACE_PARSE_DATA=>TS_PARAMS .
  types:
    BEGIN OF ts_parse_state,
        prev            TYPE string,
        change          TYPE string,
        kw              TYPE string,
        word            TYPE string,
        new             TYPE boolean,
        count           TYPE i,
        lv_default      TYPE boolean,
        ref             TYPE boolean,
        class           TYPE boolean,
        preferred       TYPE boolean,
        method_type     TYPE i,
        class_name      TYPE string,
        eventtype       TYPE string,
        eventname       TYPE string,
        token           TYPE ts_kword,
        call            TYPE ts_calls,
        call_line       TYPE ts_calls_line,
        variable        TYPE ts_vars,
        tab             TYPE ts_int_tabs,
        tabs            TYPE tt_tabs,
        composed        TYPE ts_var,
        composed_vars   TYPE tt_composed,
        calculated      TYPE ts_var,
        calculated_vars TYPE tt_calculated,
        param           TYPE ts_params,
      END OF ts_parse_state .
  types:
    BEGIN OF ts_tree,
        kind(1),
        value    TYPE string,
        param    TYPE string,
        program  TYPE program,
        include  TYPE program,
        class    type string,
        ev_type  TYPE string,
        ev_name  TYPE string,
        enh_id   TYPE i,
        var_name TYPE string,
      END OF ts_tree .
  types:
    BEGIN OF ts_call,
        include TYPE string,
        ev_name TYPE string,
        class   TYPE string,
      END OF ts_call .
  types:
    BEGIN OF t_sel_var,
        name      TYPE string,
        class     TYPE string,
        eventtype TYPE string,
        eventname TYPE string,
        i_sel     TYPE boolean,
        refval    TYPE REF TO data,
      END OF t_sel_var .
  types:
    BEGIN OF ts_if,
        if_ind      TYPE i,
        end_ind     TYPE i,
        before_else TYPE i,
      END OF ts_if .
  types:
    tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY .
  types:
    BEGIN OF ts_line,
        cond        TYPE string,
        program     TYPE string,
        include     TYPE string,
        line        TYPE i,
        ind         TYPE i,
        class       TYPE string,
        ev_name     TYPE string,
        ev_type     TYPE string,
        stack       TYPE i,
        code        TYPE string,
        arrow       TYPE string,
        subname     TYPE string,
        del         TYPE flag,
        els_before  TYPE i,
        els_after   TYPE i,
        active_root TYPE flag,
      END OF ts_line .
  types:
    tt_line TYPE TABLE OF ts_line WITH EMPTY KEY .

  class-data:
    m_option_icons   TYPE TABLE OF sign_option_icon_s .
  class-data:
    mt_lang          TYPE TABLE OF t_lang .
  class-data:
    mt_obj           TYPE TABLE OF t_obj .
  class-data:
    mt_popups        TYPE TABLE OF t_popup .
  class-data I_MERMAID_ACTIVE type BOOLEAN .
  class-data:
    mt_sel TYPE TABLE OF selection_display_s .
  data MV_PROG type PROG .
  data MV_PACKAGE type DEVCLASS .
  data MV_PKG_PARSED type ABAP_BOOL .
  data MV_CMAP_FOCUS type PROGNAME .
  data MT_PKG_OBJECTS type ZIF_ACE_PARSE_DATA=>TT_PKG_OBJ .
  data MV_SHOW_PROG type PROG .
  data MV_SHOW_PARSE_TIME type ABAP_BOOL .
  data:
    mt_compo          TYPE TABLE OF scompo .
  data MT_LOCALS type TPDA_SCR_LOCALS_IT .
  data MT_GLOBALS type TPDA_SCR_GLOBALS_IT .
  data MT_RET_EXP type TPDA_SCR_LOCALS_IT .
  data M_COUNTER type I .
  data:
    mt_steps          TYPE  TABLE OF zcl_ace=>t_step_counter WITH NON-UNIQUE KEY program include line eventtype eventname .
  data M_STEP type I .
  data M_I_FIND type BOOLEAN .
    "DATA m_stop_stack TYPE i .
    "DATA m_debug TYPE x .
  data M_REFRESH type BOOLEAN .
  data M_UPDATE type BOOLEAN .
  data I_STEP type BOOLEAN .
  data MS_STACK_PREV type ZCL_ACE=>T_STACK .
  data MS_STACK type ZCL_ACE=>T_STACK .
    "DATA i_history TYPE boolean .
  data M_HIST_STEP type I .
  data M_STEP_DELTA type I .
  data MV_RECURSE type I .
  data:
    mt_classes_types  TYPE TABLE OF zcl_ace=>t_classes_types .
  data MO_WINDOW type ref to ZCL_ACE_WINDOW .
  data MV_F7_STOP type BOOLEAN .
  data M_F6_LEVEL type I .
  data M_TARGET_STACK type I .
  data MO_TREE_LOCAL type ref to ZCL_ACE_RTTI_TREE .
  data:
    mt_selected_var   TYPE TABLE OF t_sel_var .
  data MV_STACK_CHANGED type BOOLEAN .
  data M_VARIABLE type ref to DATA .
  data:
    mt_new_string     TYPE TABLE OF  string .
  data M_QUICK type TPDA_SCR_QUICK_INFO .
  data:
    mr_statements     TYPE RANGE OF string .
  data MS_IF type TS_IF .
  data MT_IF type TT_IF .

    " open_int_table moved to ZCL_ACE_TABLE_VIEWER
  methods CONSTRUCTOR
    importing
      !I_PROG type PROG optional
      !I_PACKAGE type DEVCLASS optional
      !I_NEW_PARSER type ABAP_BOOL default ABAP_FALSE
      !I_SHOW_PARSE_TIME type ABAP_BOOL default ABAP_FALSE .
  methods SHOW .
  methods SHOW_PACKAGE .
  methods ENSURE_PACKAGE_PARSED .
  methods GET_CODE_FLOW
    importing
      !I_CALC_PATH type BOOLEAN optional
    returning
      value(RESULTS) type TT_LINE .
  methods GET_CODE_MIX
    importing
      !I_CALC_PATH type BOOLEAN optional .
  methods MARK_ACTIVE_ROOT
    importing
      !I_CALC_PATH type BOOLEAN optional
    changing
      !CT_RESULTS type TT_LINE .
  PROTECTED SECTION.
private section.

  types:
    tt_steps   TYPE STANDARD TABLE OF t_step_counter WITH EMPTY KEY .
  types:
    tt_sel_var TYPE STANDARD TABLE OF t_sel_var      WITH EMPTY KEY .

  data MV_DUMMY type I .
  constants:
    BEGIN OF c_kind,
        struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
        table  LIKE cl_abap_typedescr=>kind_table  VALUE cl_abap_typedescr=>kind_table,
        elem   LIKE cl_abap_typedescr=>kind_elem   VALUE cl_abap_typedescr=>kind_elem,
        class  LIKE cl_abap_typedescr=>kind_class  VALUE cl_abap_typedescr=>kind_class,
        intf   LIKE cl_abap_typedescr=>kind_intf   VALUE cl_abap_typedescr=>kind_intf,
        ref    LIKE cl_abap_typedescr=>kind_ref    VALUE cl_abap_typedescr=>kind_ref,
      END OF c_kind .

    " --- get_code_flow helpers ---
  methods PARSE_SEL_CALL .
  methods EXPAND_SELECTED_VARS_FORWARD
    importing
      !IT_STEPS type TT_STEPS
    changing
      !CT_SELECTED_VAR type TT_SEL_VAR .
  methods REMOVE_EMPTY_BLOCK_PAIRS
    changing
      !CT_STEPS type TT_STEPS .
  methods PROPAGATE_VARS_BACKWARD
    importing
      !IT_STEPS type TT_STEPS
    changing
      !CT_SELECTED_VAR type TT_SEL_VAR .
  methods BUILD_RESULT_LINES
    importing
      !IT_STEPS type TT_STEPS
      !IV_NO_FILTER type ABAP_BOOL
      !IT_SELECTED_VAR type TT_SEL_VAR
    changing
      !CT_RESULTS type TT_LINE .
  methods REMOVE_EMPTY_LOOP_PAIRS
    changing
      !CT_RESULTS type TT_LINE .
  methods ENRICH_RESULT_LINES
    changing
      !CT_RESULTS type TT_LINE .
  methods APPLY_CALC_PATH_FILTER
    changing
      !CT_RESULTS type TT_LINE .
ENDCLASS.
CLASS zcl_ace_alv_common DEFINITION
  create public .

public section.

  constants:
    c_white(4) TYPE x value '00000001' ##NO_TEXT.

  class-methods REFRESH
    importing
      !I_OBJ type ref to CL_GUI_ALV_GRID
      !I_LAYOUT type LVC_S_LAYO optional
      !I_SOFT type CHAR01 optional .
  class-methods TRANSLATE_FIELD
    importing
      !I_LANG type DDLANGUAGE optional
    changing
      !C_FLD type LVC_S_FCAT .
  class-methods GET_SELECTED
    importing
      !I_OBJ type ref to CL_GUI_ALV_GRID
    returning
      value(E_INDEX) type I .
protected section.
private section.
ENDCLASS.
"! Combinator factory — direct port of exported functions in abaplint combi.ts:
"!   str(), tok(), regex(), seq(), alt(), opt(), star(), plus(), per(), ver(), expr().
"! Returns ZCL_ACE_COMBI_NODE trees that can be walked via list_keywords( ).
CLASS zcl_ace_combi DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    " Declared via REF TO (not zcl_ace_combi_node=>tt_children) so the merged
    " standalone works — a DEFERRED class allows REF TO but not =>type access.
    TYPES tt_nodes TYPE STANDARD TABLE OF REF TO zcl_ace_combi_node WITH EMPTY KEY.

    "! str("WORD")  → Word        (single literal)
    "! str("END OF") → WordSequence (multi-word phrase, 1 entry in listKeywords)
    "! Replicates: indexOf(" ")>0 || indexOf("-")>0 → WordSequence else Word
    CLASS-METHODS str
      IMPORTING s             TYPE string
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! tok(TokenClassName) → Token (matches by token class name, no keyword)
    CLASS-METHODS tok
      IMPORTING token_name    TYPE string
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! regex(/.../) → Regex (no keyword)
    CLASS-METHODS regex
      IMPORTING pattern       TYPE string
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! seq( a, b, c, ... )
    CLASS-METHODS seq
      IMPORTING children      TYPE tt_nodes
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! alt( a, b, c, ... )  — also covers altPrio (same keywords)
    CLASS-METHODS alt
      IMPORTING children      TYPE tt_nodes
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! opt( a )  — also covers optPrio
    CLASS-METHODS opt
      IMPORTING child         TYPE REF TO zcl_ace_combi_node
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! star( a ) — also covers starPrio
    CLASS-METHODS star
      IMPORTING child         TYPE REF TO zcl_ace_combi_node
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! plus( a ) — also covers plusPrio
    CLASS-METHODS plus
      IMPORTING child         TYPE REF TO zcl_ace_combi_node
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! per( a, b, ... )
    CLASS-METHODS per
      IMPORTING children      TYPE tt_nodes
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! ver(version, a) / verNot — for keyword extraction we ignore the version
    "! filter (we want all keywords across all versions)
    CLASS-METHODS ver
      IMPORTING child         TYPE REF TO zcl_ace_combi_node
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! Reference to an Expression class — by name (e.g. 'COND', 'SOURCE', 'TARGET').
    "! In abaplint, mapInput(s) auto-instantiates the Expression. In ABAP we use
    "! a string name and resolve via dynamic call zcl_ace_exprs=>expr_<name>( ).
    CLASS-METHODS expr
      IMPORTING name          TYPE string
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

ENDCLASS.
"! Grammar node — direct port of abaplint combi.ts combinators.
"! Single class with discriminator (kind) instead of 11 separate combinator classes.
"! list_keywords( ) walks the tree and returns all str() literals — same algorithm
"! as Combi.listKeywords() in @abaplint/core.
CLASS zcl_ace_combi_node DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES tt_children TYPE STANDARD TABLE OF REF TO zcl_ace_combi_node WITH EMPTY KEY.

    " Discriminator values mirror combi.ts class names
    CONSTANTS:
      c_kind_word  TYPE c LENGTH 1 VALUE 'W',  " Word          → contributes to listKeywords
      c_kind_wseq  TYPE c LENGTH 1 VALUE 'Q',  " WordSequence  → contributes to listKeywords
      c_kind_token TYPE c LENGTH 1 VALUE 'T',  " Token  (tok)  → no keywords
      c_kind_regex TYPE c LENGTH 1 VALUE 'R',  " Regex         → no keywords
      c_kind_seq   TYPE c LENGTH 1 VALUE 'S',  " Sequence      → recurse
      c_kind_alt   TYPE c LENGTH 1 VALUE 'A',  " Alternative   → recurse
      c_kind_opt   TYPE c LENGTH 1 VALUE 'O',  " Optional      → recurse
      c_kind_star  TYPE c LENGTH 1 VALUE '*',  " Star          → recurse
      c_kind_plus  TYPE c LENGTH 1 VALUE '+',  " Plus          → recurse
      c_kind_per   TYPE c LENGTH 1 VALUE 'P',  " Permutation   → recurse
      c_kind_vers  TYPE c LENGTH 1 VALUE 'V',  " Vers / VersNot → recurse (single child)
      c_kind_expr  TYPE c LENGTH 1 VALUE 'E'.  " Expression reference → resolved at aggregation time

    DATA kind     TYPE c LENGTH 1 READ-ONLY.
    DATA value    TYPE string     READ-ONLY.   " word literal / token class name / regex / expression name
    DATA children TYPE tt_children READ-ONLY.

    " Factory methods — one per combinator type
    CLASS-METHODS new_word    IMPORTING s TYPE string                  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_wseq    IMPORTING s TYPE string                  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_token   IMPORTING token_name TYPE string         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_regex   IMPORTING pattern TYPE string            RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_seq     IMPORTING children TYPE tt_children      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_alt     IMPORTING children TYPE tt_children      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_opt     IMPORTING child TYPE REF TO zcl_ace_combi_node RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_star    IMPORTING child TYPE REF TO zcl_ace_combi_node RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_plus    IMPORTING child TYPE REF TO zcl_ace_combi_node RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_per     IMPORTING children TYPE tt_children      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_vers    IMPORTING child TYPE REF TO zcl_ace_combi_node RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_expr    IMPORTING name TYPE string               RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    "! Recursively collects all keyword literals from str() / WordSequence nodes.
    "! Mirrors Combi.listKeywords() in @abaplint/core (combi.ts).
    "! Expression nodes return their NAME prefixed with "expression/" — the aggregator
    "! resolves them in a separate pass to avoid infinite recursion.
    METHODS list_keywords RETURNING VALUE(result) TYPE string_table.

    METHODS constructor
      IMPORTING
        kind     TYPE c
        value    TYPE string     OPTIONAL
        children TYPE tt_children OPTIONAL.

ENDCLASS.
"! ABAP expression grammars - port of @abaplint/core 2_statements/expressions/*.ts
"! One CLASS-METHOD per expression, name = EXPR_<expression_name>.
"! Each method returns a runnable tree (zcl_ace_combi_node).
"! Discovered via RTTI by zcl_ace_keywords.
"!
"! Initial set covers the most-referenced expressions used by the seed statements
"! in zcl_ace_stmts. Extend incrementally.
CLASS zcl_ace_exprs DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    " Cond — boolean condition tree. Simplified: actual abaplint grammar is large;
    " for keyword extraction we list only its keywords.
    CLASS-METHODS expr_cond     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_compare  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_compare_operator RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_source   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_target   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_field    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_field_chain RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_field_symbol RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_data_definition RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_inline_data RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_loop_source RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_loop_target RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_for      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

ENDCLASS.
"! Aggregates all ABAP keywords from the grammar definitions in
"! ZCL_ACE_STMTS (statements) and ZCL_ACE_EXPRS (expressions),
"! mirroring abaplint's Combi.listKeywords() walk over every getMatcher().
"!
"! Discovery: RTTI scan for class-methods named STMT_* and EXPR_*.
"! Expression resolution: when a node's kind = E (Expression reference),
"! the corresponding EXPR_<name>( ) method is invoked once and its tree
"! walked too. A visited set prevents infinite recursion.
"!
"! Multi-word phrases ("END OF", "INNER JOIN") are split into individual
"! words too — needed for token-by-token classification in metrics.
"! The full phrases are also kept (under is_phrase = abap_true) for callers
"! that need them.
CLASS zcl_ace_keywords DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_keyword,
        word      TYPE string,        " uppercased
        is_phrase TYPE abap_bool,     " abap_true if originally a multi-word str()
      END OF ts_keyword,
      tt_keywords TYPE HASHED TABLE OF ts_keyword WITH UNIQUE KEY word.

    "! Returns the union of all keyword literals reachable from any
    "! statement matcher or expression. Lazily computed and cached.
    CLASS-METHODS get_all
      RETURNING VALUE(result) TYPE tt_keywords.

    "! True iff the (case-insensitive) word is in the keyword set.
    "! Drop-in replacement for the static-list check in ZCL_ACE_METRICS.
    CLASS-METHODS is_keyword
      IMPORTING token         TYPE string
      RETURNING VALUE(result) TYPE abap_bool.

    "! Forces reset of the cache. Useful after adding new STMT_/EXPR_ methods.
    CLASS-METHODS reset.

  PRIVATE SECTION.

    CLASS-DATA mt_cache         TYPE tt_keywords.
    CLASS-DATA mv_cached        TYPE abap_bool.
    " Names of expressions already walked during the current build,
    " to prevent infinite recursion through Expression references.
    CLASS-DATA mt_visited_exprs TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    CLASS-METHODS build.

    CLASS-METHODS collect_from_class
      IMPORTING class_name   TYPE string
                method_prefix TYPE string.

    CLASS-METHODS walk_node
      IMPORTING node TYPE REF TO zcl_ace_combi_node.

    CLASS-METHODS add_keyword
      IMPORTING raw TYPE string.

ENDCLASS.
CLASS zcl_ace_metrics DEFINITION
  create public .

public section.

  types:
    "--- token-level detail for debugging ---
    BEGIN OF ts_token_detail,
        token    TYPE string,
        kind     TYPE string,   " OPERATOR(kw) / OPERATOR(sym) / OPERATOR(sub) / OPERAND
        stmt_idx TYPE i,
        tok_idx  TYPE i,
        row      TYPE i,
      END OF ts_token_detail .
  types:
    tt_token_details TYPE STANDARD TABLE OF ts_token_detail WITH EMPTY KEY .

  "--- aggregate result per class (for includes containing OO code) ---
  TYPES:
    BEGIN OF ts_class_result,
      class_name       TYPE string,
      " McCabe / LOC totals (same semantics as ts_result fields)
      total_cyclomatic TYPE i,
      avg_cyclomatic   TYPE f,
      total_loc        TYPE i,
      total_lloc       TYPE i,
      total_cloc       TYPE i,
      " Halstead totals (summed N1/N2 across all methods of the class)
      total_n1         TYPE i,
      total_n2         TYPE i,
      total_volume     TYPE f,
      total_effort     TYPE f,
      total_time_t     TYPE f,
      total_bugs       TYPE f,
      " Class-scope Halstead (unique dictionaries merged across all methods)
      cls_big_n1       TYPE i,   " η1 — distinct operators, class scope
      cls_big_n2       TYPE i,   " η2 — distinct operands,  class scope
      cls_vocabulary   TYPE i,   " η  = η1 + η2
      cls_prog_length  TYPE i,   " N  = total_n1 + total_n2
      cls_volume       TYPE f,   " V  = N * log2(η)
      cls_difficulty   TYPE f,   " D  = (η1/2) * (N2/η2)
      cls_effort       TYPE f,   " E  = D * V
      cls_time_t       TYPE f,   " T  = E / 18
      cls_bugs         TYPE f,   " B  = V / 3000
    END OF ts_class_result.

 TYPES:
    tt_class_results TYPE STANDARD TABLE OF ts_class_result
      WITH EMPTY KEY.

  types:
    "--- result per code unit (method / form / module / program-level) ---
    BEGIN OF ts_unit_result,
        program      TYPE program,
        include      TYPE program,
        unit_type    TYPE string,   " METHOD / FORM / MODULE / FUNCTION / PROGRAM
        unit_name    TYPE string,
        " McCabe cyclomatic complexity
        cyclomatic   TYPE i,
        " Halstead raw counts
        n1           TYPE i,        " total operators
        n2           TYPE i,        " total operands
        big_n1       TYPE i,        " distinct operators
        big_n2       TYPE i,        " distinct operands
        " Halstead derived
        vocabulary   TYPE i,        " η = η1 + η2
        prog_length  TYPE i,        " N = N1 + N2
        volume       TYPE f,        " V = N * log2(η)
        difficulty   TYPE f,        " D = (η1/2) * (N2/η2)
        effort       TYPE f,        " E = D * V
        time_t       TYPE f,        " T = E / 18     (Stroud number: mental discriminations/sec)
        bugs         TYPE f,        " B = V / 3000   (expected delivered bugs, Halstead)
        " Maintainability Index
        mi           TYPE f,        " MI = 171 - 5.2*ln(V) - 0.23*G - 16.2*ln(LOC)
        " Lines of code
        loc          TYPE i,        " total lines in unit
        lloc         TYPE i,        " logical LOC (statements)
        cloc         TYPE i,        " comment lines
        " Token-level debug detail
        token_detail TYPE tt_token_details,
      END OF ts_unit_result .
  types:
    tt_unit_results TYPE STANDARD TABLE OF ts_unit_result WITH EMPTY KEY .

     "--- aggregate result for whole program ---
TYPES:
  BEGIN OF ts_result,
    program          TYPE program,
    units            TYPE tt_unit_results,
    total_cyclomatic TYPE i,
    total_volume     TYPE f,
    total_effort     TYPE f,
    total_time_t     TYPE f,        " T = E / 18   summed across all units
    total_bugs       TYPE f,        " B = V / 3000 summed across all units
    total_loc        TYPE i,
    total_lloc       TYPE i,
    total_cloc       TYPE i,
    avg_cyclomatic   TYPE f,
    " Include-level Halstead (unique dicts merged across ALL units of include)
    total_n1         TYPE i,        " sum of N1 across all units
    total_n2         TYPE i,        " sum of N2 across all units
    incl_big_n1      TYPE i,        " η1 — distinct operators, include scope
    incl_big_n2      TYPE i,        " η2 — distinct operands,  include scope
    incl_vocabulary  TYPE i,        " η  = η1 + η2
    incl_prog_length TYPE i,        " N  = total_n1 + total_n2
    incl_volume      TYPE f,        " V  = N * log2(η)
    incl_difficulty  TYPE f,        " D  = (η1/2) * (N2/η2)
    incl_effort      TYPE f,        " E  = D * V
    incl_time_t      TYPE f,        " T  = E / 18
    incl_bugs        TYPE f,        " B  = V / 3000
    class_totals     TYPE tt_class_results,
  END OF ts_result.

  class-methods CALCULATE
    importing
      !IS_PARSE_DATA type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
      !I_PROGRAM type PROGRAM
    returning
      value(RS_RESULT) type TS_RESULT .
private section.

  types:
    BEGIN OF ts_known_operand,
    name TYPE string,
  END OF ts_known_operand .
  types:
    tt_known_operands TYPE HASHED TABLE OF ts_known_operand
    WITH UNIQUE KEY name .

  class-methods IS_BRANCH_KEYWORD
    importing
      !I_KW type STRING
    returning
      value(RV) type ABAP_BOOL .
  class-methods BUILD_OPERAND_SET
    importing
      !IS_PARSE_DATA type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
      !I_INCLUDE type PROGRAM
      !I_UNIT_TYPE type STRING
      !I_UNIT_NAME type STRING
      !I_CLASS type STRING
    returning
      value(RT_OPS) type TT_KNOWN_OPERANDS .
  class-methods CLASSIFY_TOKEN
    importing
      !I_TOKEN type STRING
      !I_IS_FIRST type ABAP_BOOL
      !IT_OPERANDS type TT_KNOWN_OPERANDS
    returning
      value(RV_KIND) type STRING .
  class-methods LOG2
    importing
      !I_VAL type F
    returning
      value(RV) type F .
ENDCLASS.
CLASS zcl_ace_metrics_window DEFINITION
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
      IMPORTING i_name     TYPE string
                it_rows    TYPE tt_row
                i_numbered TYPE abap_bool OPTIONAL
      CHANGING  ct_html    TYPE w3htmltab.

ENDCLASS.
CLASS zcl_ace_parser DEFINITION
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
CLASS zcl_ace_parse_calcs DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

protected section.
  PRIVATE SECTION.
    DATA mv_eventtype TYPE string.
    DATA mv_eventname TYPE string.
    DATA mv_class     TYPE string.
    DATA mv_in_impl   TYPE abap_bool.

    CLASS-METHODS is_varname
      IMPORTING i_str         TYPE string
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    METHODS append_calc
      IMPORTING i_name      TYPE string
                i_program   TYPE program
                i_include   TYPE program
                i_class     TYPE string
                i_eventtype TYPE string
                i_eventname TYPE string
                i_line      TYPE i
      CHANGING  cs_source   TYPE zif_ace_parse_data=>ts_parse_data.

    METHODS append_comp
      IMPORTING i_name      TYPE string
                i_program   TYPE program
                i_include   TYPE program
                i_class     TYPE string
                i_eventtype TYPE string
                i_eventname TYPE string
                i_line      TYPE i
      CHANGING  cs_source   TYPE zif_ace_parse_data=>ts_parse_data.

ENDCLASS.
CLASS zcl_ace_parse_calls DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

protected section.
private section.

  data MV_CLASS_NAME type STRING .
  data MV_EVENT_TYPE type STRING .
  data MV_EVENT_NAME type STRING .
  data MV_IN_IMPL type ABAP_BOOL .
  data MV_SUPER_CLS type STRING .
  data MV_SUPER type STRING .

  " Lazily built lists (a CONSTANTS literal is limited to 255 chars,
  " so they are concatenated at runtime on first use):
  " mv_builtin_funcs — built-in functions and constructor expressions that
  " look like functional method calls (NAME( … )) but must not be recorded;
  " mv_skip_keywords — statement keywords that never contain method calls
  " (declarations, SQL, …), skipped by the generic fallback scan.
  data MV_BUILTIN_FUNCS type STRING .
  data MV_SKIP_KEYWORDS type STRING .

  methods RESOLVE_VAR_TYPE
    importing
      !IS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_EVTYPE type STRING
      !I_EVNAME type STRING
      !I_VARNAME type STRING
      !I_CLASS type STRING optional
    returning
      value(RV_TYPE) type STRING .
    " Resolves a reference chain like OBJ->MO_ATTR or CLS=>ATTR->SUB
    " to the class of the last segment.
  methods RESOLVE_CHAIN
    importing
      !IS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
      !I_PROGRAM type PROGRAM
      !I_EVTYPE type STRING
      !I_EVNAME type STRING
      !I_CHAIN type STRING
    returning
      value(RV_TYPE) type STRING .
  methods IS_BUILTIN
    importing
      !I_NAME type STRING
    returning
      value(RV_BUILTIN) type ABAP_BOOL .
  methods GET_SUPER
    importing
      !IS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
    returning
      value(RV_SUPER) type STRING .
  methods PARSE_STMT_CALLS
    importing
      !IO_SCAN type ref to CL_CI_SCAN
      !I_STMT_IDX type I
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
    changing
      !CS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA .
    " Линейный проход: распознаёт obj->meth( / cls=>meth( / NEW cls( и собирает BINDINGS
  methods COLLECT_METHOD_CALLS
    importing
      !IO_SCAN type ref to CL_CI_SCAN
      !I_STMT type SSTMNT
      !I_PROGRAM type PROGRAM
    changing
      !CS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
      !CT_CALLS type ZCL_ACE=>TT_CALLS .
ENDCLASS.
CLASS zcl_ace_parse_calls_line DEFINITION
  create public .

public section.
  interfaces ZIF_ACE_STMT_HANDLER .
protected section.
private section.

  types:
    BEGIN OF ts_meth_def,
      name        TYPE string,
      def_include TYPE program,
      def_line    TYPE i,
      meth_type   TYPE i,
    END OF ts_meth_def .

  data MV_CLASS_NAME type STRING .
  data MV_SUPER_NAME type STRING .
  data MV_IN_IMPL    type ABAP_BOOL .
  data MV_IS_INTF    type ABAP_BOOL .
  data MV_METH_TYPE  type I .
  data mt_meth_defs  TYPE STANDARD TABLE OF ts_meth_def WITH NON-UNIQUE KEY name .

  methods GET_METH_TYPE importing !I_INCLUDE TYPE program returning value(RV_TYPE) TYPE i .
  methods ON_CLASS_KW importing !IO_SCAN TYPE REF TO cl_ci_scan !I_STMT_IDX TYPE i !I_KW TYPE string .
  methods ON_METHODS_SIG
    importing !IO_SCAN TYPE REF TO cl_ci_scan !I_STMT_IDX TYPE i
              !I_PROGRAM TYPE program !I_INCLUDE TYPE program
    changing  !CS_SOURCE TYPE zif_ace_parse_data=>ts_parse_data .
  methods ON_BLOCK_START
    importing !IO_SCAN TYPE REF TO cl_ci_scan !I_STMT_IDX TYPE i
              !I_PROGRAM TYPE program !I_INCLUDE TYPE program !I_KW TYPE string
    changing  !CS_SOURCE TYPE zif_ace_parse_data=>ts_parse_data .
ENDCLASS.
CLASS zcl_ace_parse_events DEFINITION
  create public .

public section.

  interfaces ZIF_ACE_STMT_HANDLER .
protected section.
private section.
ENDCLASS.
CLASS zcl_ace_parse_handlers DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

    " Собрать карту хэндлеров из всего инклуда — вызывать при полном проходе
    CLASS-METHODS collect
      IMPORTING
        io_scan   TYPE REF TO cl_ci_scan
        i_program TYPE program
        i_include TYPE program
      CHANGING
        cs_source TYPE zif_ace_parse_data=>ts_parse_data.

    " Разрезолвить RAISE EVENT → список вызовов хэндлеров
    CLASS-METHODS resolve_raise_event
      IMPORTING
        io_scan    TYPE REF TO cl_ci_scan
        i_stmt_idx TYPE i
        i_program  TYPE program
        i_include  TYPE program
      CHANGING
        cs_source  TYPE zif_ace_parse_data=>ts_parse_data
        ct_calls   TYPE zcl_ace=>tt_calls.

  PRIVATE SECTION.
    CLASS-METHODS resolve_var_type
      IMPORTING
        is_source TYPE zif_ace_parse_data=>ts_parse_data
        i_program TYPE program
        i_evtype  TYPE string
        i_evname  TYPE string
        i_varname TYPE string
      RETURNING
        VALUE(rv_type) TYPE string.

ENDCLASS.
CLASS zcl_ace_parse_params DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

protected section.
  PRIVATE SECTION.

    DATA mv_class_name TYPE string.
    DATA mv_in_impl    TYPE abap_bool.

    METHODS parse_methods_stmt
      IMPORTING
        !io_scan    TYPE REF TO cl_ci_scan
        !i_stmt_idx TYPE i
        !i_program  TYPE program
        !i_include  TYPE program
        !i_kw       TYPE string
      CHANGING
        !cs_source  TYPE zif_ace_parse_data=>ts_parse_data.

ENDCLASS.
CLASS zcl_ace_parse_vars DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

protected section.
  PRIVATE SECTION.
    DATA mv_class_name TYPE string.
    DATA mv_eventtype  TYPE string.
    DATA mv_eventname  TYPE string.
    DATA mv_section    TYPE string.
    DATA mv_in_impl    TYPE abap_bool.

    METHODS append_var
      IMPORTING i_name    TYPE string
                i_type    TYPE string
                i_icon    TYPE salv_de_tree_image
                i_line    TYPE i
                i_program TYPE program
                i_include TYPE program
      CHANGING  cs_source TYPE zif_ace_parse_data=>ts_parse_data.

    METHODS resolve_icon
      IMPORTING i_type         TYPE string
                i_ref          TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_icon) TYPE salv_de_tree_image.

ENDCLASS.
CLASS zcl_ace_popup DEFINITION
  create public .

public section.

  class-data M_COUNTER type I .
  data M_ADDITIONAL_NAME type STRING .
  data MO_BOX type ref to CL_GUI_DIALOGBOX_CONTAINER .
  data MO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_IMP_EXP type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VARIABLES_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_TABLES_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_MERMAID type ref to ZCL_ACE_MERMAID .

  methods CONSTRUCTOR
    importing
      !I_ADDITIONAL_NAME type STRING optional .
  methods CREATE
    importing
      !I_WIDTH type I
      !I_HIGHT type I
      !I_NAME type TEXT100 optional
    returning
      value(RO_BOX) type ref to CL_GUI_DIALOGBOX_CONTAINER .
  methods ON_BOX_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
protected section.
private section.
ENDCLASS.
CLASS zcl_ace_html_viewer DEFINITION
  inheriting from ZCL_ACE_POPUP
  final
  create public .

public section.

  data MO_HTML type ref to CL_GUI_HTML_VIEWER .

  methods CONSTRUCTOR
    importing
      !IT_HTML type W3HTMLTAB
      !I_TITLE type TEXT100 default 'HTML'
      !I_WIDTH type I default 800
      !I_HEIGHT type I default 400 .
protected section.
private section.
ENDCLASS.
CLASS zcl_ace_mermaid DEFINITION
  inheriting from ZCL_ACE_POPUP
  create public .

public section.

  class-methods CHECK_MERMAID .

  data MO_VIEWER type ref to ZCL_ACE .
  data MO_MM_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_MM_TOOLBAR type ref to CL_GUI_CONTAINER .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_DIAGRAM type ref to OBJECT .
  data MV_TYPE type STRING .
  data MV_CALC_PATH type BOOLEAN .
  data MV_WITH_PARAMS type BOOLEAN .
  data MV_SHOW_EXT type BOOLEAN .
  data MV_ALL_METHODS type BOOLEAN .
  data MV_DIRECTION type UI_FUNC .

  methods CONSTRUCTOR
    importing
      !IO_DEBUGGER type ref to ZCL_ACE
      !I_TYPE type STRING .
  methods STEPS_FLOW
    importing
      !I_DIRECTION   type UI_FUNC    optional
      !I_WITH_PARAMS type BOOLEAN    optional
      !I_CALC_PATH   type BOOLEAN    optional .
  methods MAGIC_SEARCH
    importing
      !I_DIRECTION type UI_FUNC optional
      !I_CALC_PATH type BOOLEAN optional .
  methods CLASS_MAP
    importing
      !I_DIRECTION type UI_FUNC optional .
  methods ADD_TOOLBAR_BUTTONS .
  methods HND_TOOLBAR
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods OPEN_MERMAID
    importing
      !I_MM_STRING type STRING .
  methods REFRESH .

protected section.
private section.

  methods FORMAT_NODE_LABEL
    importing
      !I_CODE   type STRING
      !I_MAXLEN type i default 50
    returning
      value(RV_LABEL) type STRING .

  methods BUILD_NODES
    importing
      !I_DIRECTION type STRING
    changing
      !CT_LINES     type mo_viewer->tt_line
      !CV_MM_STRING type STRING .

  methods BUILD_EDGES
    importing
      !IT_LINES     type mo_viewer->tt_line
    changing
      !CV_MM_STRING type STRING .

ENDCLASS.
CLASS zcl_ace_rtti DEFINITION
  create public .

public section.

  class-methods CREATE_TABLE_BY_NAME
    importing
      !I_TNAME type TABNAME
    changing
      !C_TABLE type ref to DATA .
  class-methods CREATE_STRUC_HANDLE
    importing
      !I_TNAME type TABNAME
    exporting
      !E_T_COMP type ABAP_COMPONENT_TAB
      !E_HANDLE type ref to CL_ABAP_STRUCTDESCR .
protected section.
private section.
ENDCLASS.
CLASS zcl_ace_rtti_tree DEFINITION
  final
  create public .

public section.

  types:
    tt_table TYPE STANDARD TABLE OF ZCL_ACE=>ts_tree
            WITH NON-UNIQUE DEFAULT KEY .

  data MAIN_NODE_KEY type SALV_DE_NODE_KEY .
  data M_PRG_INFO type TPDA_SCR_PRG_INFO .
  data MO_VIEWER type ref to ZCL_ACE .
  data MO_TREE type ref to CL_SALV_TREE .
  data MT_LAZY_NODES type STANDARD TABLE OF SALV_DE_NODE_KEY WITH DEFAULT KEY.

  methods CONSTRUCTOR
    importing
      !I_HEADER type CLIKE default 'View'
      !I_TYPE type BOOLEAN optional
      !I_CONT type ref to CL_GUI_CONTAINER optional
      !I_DEBUGGER type ref to ZCL_ACE optional .
  methods CLEAR .
  methods ADD_BUTTONS
    importing
      !I_TYPE type BOOLEAN .
  methods ADD_NODE
    importing
      !I_NAME type STRING
      !I_REL type SALV_DE_NODE_KEY optional
      !I_ICON type SALV_DE_TREE_IMAGE optional
      !I_TREE type ZCL_ACE=>TS_TREE optional
    returning
      value(RV_NODE) type SALV_DE_NODE_KEY .
  methods DELETE_NODE
    importing
      !I_KEY type SALV_DE_NODE_KEY .
  methods DISPLAY
    importing
      !IO_DEBUGGER type ref to ZCL_ACE optional .
protected section.
PRIVATE SECTION.

  CONSTANTS:
    BEGIN OF c_kind,
      struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
      table  LIKE cl_abap_typedescr=>kind_table  VALUE cl_abap_typedescr=>kind_table,
      elem   LIKE cl_abap_typedescr=>kind_elem   VALUE cl_abap_typedescr=>kind_elem,
      class  LIKE cl_abap_typedescr=>kind_class  VALUE cl_abap_typedescr=>kind_class,
      intf   LIKE cl_abap_typedescr=>kind_intf   VALUE cl_abap_typedescr=>kind_intf,
      ref    LIKE cl_abap_typedescr=>kind_ref    VALUE cl_abap_typedescr=>kind_ref,
    END OF c_kind .
  DATA tree_table TYPE tt_table .

  METHODS hndl_double_click
    FOR EVENT double_click OF cl_salv_events_tree
    IMPORTING !node_key .
  METHODS hndl_expand_empty
    FOR EVENT expand_empty_folder OF cl_salv_events_tree
    IMPORTING !node_key .
  METHODS hndl_user_command
    FOR EVENT added_function OF cl_salv_events
    IMPORTING !e_salv_function .

  " --- hndl_double_click sub-handlers ---
  METHODS dblclk_event
    IMPORTING !i_program TYPE any
              !i_include TYPE any
              !i_value   TYPE any
              !i_ev_name TYPE any .
  METHODS dblclk_vars_lazy
    IMPORTING !i_param   TYPE string
              !i_program TYPE any
              !i_include TYPE any
              !i_ev_type TYPE any
              !i_ev_name TYPE any
              !i_kind    TYPE any
              !io_node   TYPE REF TO cl_salv_node .
  METHODS dblclk_module
    IMPORTING !i_include TYPE any
              !i_value   TYPE any .
  METHODS dblclk_form_plain
    IMPORTING !i_include TYPE any
              !i_value   TYPE any .
  METHODS dblclk_method_plain
    IMPORTING !i_include  TYPE any
              !i_ev_name  TYPE any
              !i_value    TYPE any .
  METHODS dblclk_form_enh
    IMPORTING !i_param  TYPE any
              !i_enh_id TYPE any .
  METHODS dblclk_method_enh
    IMPORTING !i_param   TYPE any
              !i_ev_name TYPE any
              !i_value   TYPE any
              !i_ev_type TYPE any .
  METHODS dblclk_var_leaf
    IMPORTING !i_include   TYPE any
              !i_value     TYPE any
              !i_var_name  TYPE any
              !i_class     TYPE string OPTIONAL
              !i_ev_type   TYPE string OPTIONAL
              !i_ev_name   TYPE string OPTIONAL
              !io_node     TYPE REF TO cl_salv_node .

  " --- hndl_expand_empty sub-handlers: one per param prefix ---
  METHODS load_package_object
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_prog     TYPE progname .
  METHODS expand_add_lazy
    IMPORTING !i_node_key TYPE salv_de_node_key .
  METHODS expand_vars_method
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_class    TYPE string
              !i_method   TYPE string
              !i_program  TYPE string .
  METHODS expand_intf_vars
    IMPORTING !i_node_key  TYPE salv_de_node_key
              !i_intf_name TYPE string .
  METHODS expand_lvars_form
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_form     TYPE string
              !i_program  TYPE string .
  METHODS expand_attr
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_class    TYPE string .
  METHODS expand_incls
    IMPORTING !i_node_key  TYPE salv_de_node_key
              !i_main_prog TYPE program .
  METHODS expand_gvars
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_program  TYPE string .
  METHODS expand_forms
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_prog     TYPE string .
  METHODS expand_mods
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_prog     TYPE string .
  METHODS expand_lclasses
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_prog     TYPE string .
  METHODS expand_lintfs
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_prog     TYPE string .
  METHODS expand_sect
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_class    TYPE string
              !i_section  TYPE string .
  METHODS expand_intf
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_intf     TYPE string .
  METHODS expand_class
    IMPORTING !i_node_key TYPE salv_de_node_key
              !i_class    TYPE string .
ENDCLASS.
CLASS zcl_ace_sel_opt DEFINITION
  create public .

public section.

  data MO_VIEWER type ref to ZCL_ACE_TABLE_VIEWER .
  data MO_SEL_ALV type ref to CL_GUI_ALV_GRID .
  data MT_FCAT type LVC_T_FCAT .
  data:
    mt_sel_tab TYPE TABLE OF ZCL_ACE=>selection_display_s .
  data MS_LAYOUT type LVC_S_LAYO .

  events SELECTION_DONE .

  methods CONSTRUCTOR
    importing
      !IO_VIEWER type ref to ZCL_ACE_TABLE_VIEWER
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
  CLASS-METHODS init_icons_table .
  methods RAISE_SELECTION_DONE .
  methods UPDATE_SEL_TAB .
  methods SET_VALUE
    importing
      !I_FIELD type ANY
      !I_LOW type ANY optional
      !I_HIGH type ANY optional
      !I_CLEAR type BOOLEAN default ABAP_TRUE .
  methods UPDATE_SEL_ROW
    changing
      !C_SEL_ROW type ZCL_ACE=>SELECTION_DISPLAY_S .
protected section.
private section.

  methods INIT_FCAT .
  methods HANDLE_SEL_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods ON_F4
    for event ONF4 of CL_GUI_ALV_GRID
    importing
      !ER_EVENT_DATA
      !ES_ROW_NO
      !E_FIELDNAME .
  methods ON_GRID_BUTTON_CLICK
    for event BUTTON_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_COL_ID
      !ES_ROW_NO .
  methods ON_DATA_CHANGED
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED .
  methods ON_DATA_CHANGED_FINISHED
    for event DATA_CHANGED_FINISHED of CL_GUI_ALV_GRID
    importing
      !E_MODIFIED .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_DOUBLECLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_ROW_NO
      !E_COLUMN .
  methods HANDLE_CONTEXT_MENU_REQUEST
    for event CONTEXT_MENU_REQUEST of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
ENDCLASS.
CLASS zcl_ace_source_parser DEFINITION
  create public .

public section.

  class-methods PARSE_CALL
    importing
      !I_PROGRAM   type PROGRAM
      !I_INCLUDE   type PROGRAM
      !I_INDEX     type I
      !I_STACK     type I
      !I_E_NAME    type STRING
      !I_E_TYPE    type STRING
      !I_CLASS     type STRING optional
      !I_STMT_IDX  type I optional
      !I_NO_STEPS  type ABAP_BOOL optional
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PARSE_CALL_FORM
    importing
      !I_CALL_NAME type STRING
      !I_PROGRAM   type PROGRAM
      !I_INCLUDE   type PROGRAM
      !I_STACK     type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PARSE_CLASS
    importing
      !KEY         type ZCL_ACE=>TS_KWORD
      !I_INCLUDE   type PROGRAM
      !I_CALL      type ZCL_ACE=>TS_CALLS
      !I_STACK     type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PARSE_SCREEN
    importing
      !KEY         type ZCL_ACE=>TS_KWORD
      !I_STACK     type I
      !I_CALL      type ZCL_ACE=>TS_CALLS
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods RESOLVE_CONTEXT
    importing
      !I_INCLUDE   type PROGRAM
      !I_EVTYPE    type STRING optional
      !I_EVNAME    type STRING optional
      !IO_DEBUGGER type ref to ZCL_ACE
    exporting
      !E_EVTYPE    type STRING
      !E_EVNAME    type STRING
      !E_CLASS     type STRING .
  class-methods CODE_EXECUTION_SCANNER
    importing
      !I_PROGRAM   type PROGRAM
      !I_INCLUDE   type PROGRAM
      !I_EVNAME    type STRING optional
      !I_EVTYPE    type STRING optional
      !I_CLASS     type STRING optional
      !I_STACK     type I optional
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods LINK_CALLS_TO_PARAMS
    importing
      !IO_DEBUGGER type ref to ZCL_ACE
    changing
      !CT_TOKENS   type ZCL_ACE=>TT_KWORD .
  class-methods PROCESS_SUPER_AND_INTERFACES
    importing
      !I_CLASS     type STRING
      !I_PROGRAM   type PROGRAM
      !I_STACK     type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods DETECT_METHOD_CALL
    importing
      !WORD            type STRING
      !I_PROGRAM       type PROGRAM
      !I_INCLUDE       type PROGRAM
      !I_CLASS         type STRING
      !IO_DEBUGGER     type ref to ZCL_ACE
      !L_TOKEN_ROW     type I
      !NEW             type BOOLEAN
    changing
      !CALL            type ZCL_ACE=>TS_CALLS
      !CALL_LINE       type ZCL_ACE=>TS_CALLS_LINE
      !CALCULATED      type ZCL_ACE=>TS_VAR
      !CALCULATED_VARS type ZCL_ACE=>TT_CALCULATED
      !CLASS_NAME      type STRING
      !TOKEN           type ZCL_ACE=>TS_KWORD .
  class-methods REGISTER_FIELD_SYMBOL
    importing
      !I_INCLUDE   type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE
    changing
      !CS_STATE    type ZCL_ACE=>TS_PARSE_STATE .
  class-methods COLLECT_EVENTS
    importing
      !IO_SCAN     type ref to CL_CI_SCAN
      !I_PROGRAM   type PROGRAM
      !I_INCLUDE   type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods COLLECT_ENHANCEMENTS
    importing
      !I_PROGRAM   type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods COLLECT_METHOD_ENHANCEMENTS
    importing
      !I_ENHNAME    type ENHNAME
      !I_ENHINCLUDE type PROGRAM
      !I_METHOD     type STRING
      !I_CLASS      type STRING
      !I_METH_POS   type STRING
      !I_ID         type I
      !IO_DEBUGGER  type ref to ZCL_ACE .
protected section.
private section.

  " Decides whether an object (function module / class) is customer code
  " for the "Only Z" filter: Z*/Y* prefixes plus objects in a customer
  " namespace (name starting with '/', e.g. /CLIN/CL_...).
  class-methods IS_CUSTOM_CODE
    importing
      !I_NAME type CLIKE
    returning
      value(RV_CUSTOM) type ABAP_BOOL .
ENDCLASS.
"! ABAP statement grammars - port of @abaplint/core 2_statements/statements/*.ts
"! One CLASS-METHOD per statement, name = STMT_<statement_name>.
"! Each method returns its runnable tree (zcl_ace_combi_node) - the same data
"! that abaplint's getMatcher() returns.
"! Discovered via RTTI by zcl_ace_keywords.
"!
"! Initial seed = the most common statements. Extend by porting more files from
"! abaplint/packages/core/src/abap/2_statements/statements/*.ts using the same
"! 1:1 mapping (str->str, tok->tok, seq->seq, alt->alt, opt->opt, star->star, ...).
CLASS zcl_ace_stmts DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    " Control flow
    CLASS-METHODS stmt_if           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_elseif       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_else         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endif        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_case         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_when         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_when_others  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endcase      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_do           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_enddo        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_while        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endwhile     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_loop         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endloop      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_continue     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_exit         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_check        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_try          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endtry       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_catch        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_cleanup      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_raise        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Data
    CLASS-METHODS stmt_data         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_data_begin   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_data_end     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_constant     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_field_symbol RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_types        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_clear        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_move         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_compute      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_assign       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_unassign     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Internal tables
    CLASS-METHODS stmt_append       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_insert_internal RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_modify_internal RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_delete_internal RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_read_table   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_sort         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_collect      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " OO
    CLASS-METHODS stmt_class_def    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_class_impl   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endclass     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_interface    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endinterface RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_method_def   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_method_impl  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endmethod    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_create_object RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_method  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Subroutines / function modules
    CLASS-METHODS stmt_form         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endform      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_perform      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_function     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endfunction  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_function RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_module       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endmodule    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " SQL
    CLASS-METHODS stmt_select       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endselect    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_insert_db    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_update_db    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_delete_db    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_modify_db    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_commit       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_rollback     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Reporting
    CLASS-METHODS stmt_report       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_write        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_message      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Misc
    CLASS-METHODS stmt_assert       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_return       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_include      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_export       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_import       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Selection screen / events
    CLASS-METHODS stmt_select_options RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_parameters     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_parameter      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_ranges         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_tables         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_selection_screen RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_selection   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_user_command RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_pf          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_line_sel    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_initialization RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_start_of_sel   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_end_of_sel     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_top_of_page    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_end_of_page    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_load_of_program RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_first       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_last        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at             RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endat          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " String operations
    CLASS-METHODS stmt_concatenate    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_split          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_replace        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_find           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_translate      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_condense       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_overlay        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_shift          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_search         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " System / control
    CLASS-METHODS stmt_describe       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_free           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_refresh        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_wait           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_stop           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_submit         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_leave          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_generate       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_authority_check RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_break_point    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_break          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_log_point      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_set            RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_get            RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_convert        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_pack           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_unpack         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " File I/O
    CLASS-METHODS stmt_open_dataset   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_close_dataset  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_read_dataset   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_transfer       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_delete_dataset RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " More OO
    CLASS-METHODS stmt_aliases        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_interfaces     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_class_data     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_class_events   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_events         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_raise_event    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_create_data    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_catch_sys_excs RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_case_type      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_when_type      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " More reporting
    CLASS-METHODS stmt_format         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_new_line       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_new_page       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_skip           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_uline          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_position       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_print_control  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_reserve        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_back           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_suppress       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_extract        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_field_groups   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Math / arithmetic
    CLASS-METHODS stmt_add            RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_subtract       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_multiply       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_divide         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_move_corres    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Macro / definition
    CLASS-METHODS stmt_define         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_end_of_def     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Misc more
    CLASS-METHODS stmt_provide        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endprovide     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_on_change      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endon          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_chain          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endchain       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_resume         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_retry          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_function_pool  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_program        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_type_pool      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_type_pools     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_infotypes      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_controls       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_statics        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_screen    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_transaction RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_dialog    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_set_pf_status  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_set_titlebar   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_window         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_loop_dynpro    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_process        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_field          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_receive        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_communication  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_set_handler    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_get_reference  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_badi      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

ENDCLASS.
CLASS zcl_ace_table_viewer DEFINITION
  inheriting from ZCL_ACE_POPUP
  create public .

public section.

  types:
    BEGIN OF t_elem,
               field TYPE fieldname,
               elem  TYPE ddobjname,
             END OF t_elem .

  data M_LANG type DDLANGUAGE .
  data M_TABNAME type TABNAME .
  data MO_ALV type ref to CL_GUI_ALV_GRID .
  data MO_SEL type ref to ZCL_ACE_SEL_OPT .
  data MR_TABLE type ref to DATA .
  data MO_SEL_PARENT type ref to CL_GUI_CONTAINER .
  data MO_ALV_PARENT type ref to CL_GUI_CONTAINER .
  data MT_ALV_CATALOG type LVC_T_FCAT .
  data:
    mt_fields          TYPE TABLE OF t_elem .
  data MO_SEL_WIDTH type I .
  data M_VISIBLE type C .
  data M_STD_TBAR type X .
  data M_SHOW_EMPTY type I .
  data MO_WINDOW type ref to ZCL_ACE_WINDOW .

  methods CONSTRUCTOR
    importing
      !I_TNAME type ANY optional
      !I_ADDITIONAL_NAME type STRING optional
      !IR_TAB type ref to DATA optional
      !IO_WINDOW type ref to ZCL_ACE_WINDOW .
  methods REFRESH_TABLE
    for event SELECTION_DONE of ZCL_ACE_SEL_OPT .
  CLASS-METHODS open_int_table
    IMPORTING
      !it_tab    TYPE ANY TABLE OPTIONAL
      !it_ref    TYPE REF TO data OPTIONAL
      !i_name    TYPE string
      !io_window TYPE REF TO zcl_ace_window .
protected section.
private section.

  methods CREATE_POPUP .
  methods CREATE_ALV .
  methods CREATE_SEL_ALV .
  methods SET_HEADER .
  methods CREATE_FIELD_CAT
    importing
      !I_TNAME type TABNAME
    returning
      value(ET_CATALOG) type LVC_T_FCAT .
  methods TRANSLATE_FIELD
    importing
      !I_LANG type DDLANGUAGE
    changing
      !C_FLD type LVC_S_FCAT .
  methods HANDLE_TAB_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods BEFORE_USER_COMMAND
    for event BEFORE_USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods ON_TABLE_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
ENDCLASS.
CLASS zcl_ace_text_viewer DEFINITION
  inheriting from ZCL_ACE_POPUP
  final
  create public .

public section.

  data MO_TEXT type ref to CL_GUI_TEXTEDIT .

  methods CONSTRUCTOR
    importing
      !IR_STR type ref to DATA .
protected section.
private section.
ENDCLASS.
CLASS zcl_ace_tree_builder DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_window TYPE REF TO zcl_ace_window
        io_tree   TYPE REF TO zcl_ace_rtti_tree.
    METHODS build.
    METHODS build_package
      IMPORTING
        i_package TYPE devclass.
    METHODS build_object_subtree
      IMPORTING
        i_root_key TYPE salv_de_node_key
        i_program  TYPE progname.
    METHODS get_package_objects
      IMPORTING
        i_package     TYPE devclass
      RETURNING
        VALUE(rt_obj) TYPE zif_ace_parse_data=>tt_pkg_obj.

protected section.
  PRIVATE SECTION.
    DATA mo_window TYPE REF TO zcl_ace_window.
    DATA mo_tree   TYPE REF TO zcl_ace_rtti_tree.

    METHODS get_include_prefix
      IMPORTING
        i_class          TYPE string
      RETURNING
        VALUE(rv_prefix) TYPE string.
    METHODS build_local_classes_node
      IMPORTING
        i_program           TYPE string
        i_excl_class        TYPE string
        i_refnode           TYPE salv_de_node_key
      RETURNING
        VALUE(r_locals_rel) TYPE salv_de_node_key.
    METHODS add_class
      IMPORTING
        i_class       TYPE string
        i_refnode     TYPE salv_de_node_key
        no_locals     TYPE boolean OPTIONAL
        i_tree        TYPE zcl_ace=>ts_tree OPTIONAL
        i_type        TYPE flag OPTIONAL
      RETURNING
        VALUE(r_node) TYPE salv_de_node_key.
    METHODS show_tree_includes
      IMPORTING
        i_root_key TYPE salv_de_node_key
        i_prog     TYPE prog.
    METHODS show_tree_enhancements
      IMPORTING
        i_root_key TYPE salv_de_node_key.
    METHODS show_tree_global_vars
      IMPORTING
        i_root_key TYPE salv_de_node_key.
    METHODS show_tree_events
      IMPORTING
        i_root_key TYPE salv_de_node_key
        i_prog     TYPE prog.
    METHODS show_tree_function_modules
      IMPORTING
        i_root_key TYPE salv_de_node_key.
    METHODS show_tree_local_classes
      IMPORTING
        i_root_key   TYPE salv_de_node_key
        i_prog       TYPE prog
        i_excl_class TYPE string.
    METHODS show_tree_class_hierarchy
      IMPORTING
        i_root_key TYPE salv_de_node_key
        i_cl_name  TYPE string.
    METHODS show_tree_subroutines
      IMPORTING
        i_root_key TYPE salv_de_node_key
        i_prog     TYPE prog.
    METHODS show_tree_modules
      IMPORTING
        i_root_key TYPE salv_de_node_key
        i_prog     TYPE prog.
ENDCLASS.
CLASS zcl_ace_window DEFINITION
  inheriting from ZCL_ACE_POPUP
  create public .

public section.

  types:
    BEGIN OF ts_table,
               ref      TYPE REF TO data,
               kind(1),
               value    TYPE string,
               typename TYPE abap_abstypename,
               fullname TYPE string,
             END OF ts_table .
  types:
    BEGIN OF ts_calls,
               class TYPE string,
               event TYPE string,
               type  TYPE string,
               name  TYPE string,
               outer TYPE string,
               inner TYPE string,
               super TYPE boolean,
             END OF ts_calls .
  types:
    tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer .

  " --- aliases for types moved to ZIF_ACE_PARSE_DATA ---
  TYPES ts_event       TYPE zif_ace_parse_data=>ts_event.
  TYPES tt_events      TYPE zif_ace_parse_data=>tt_events.
  TYPES ts_var         TYPE zif_ace_parse_data=>ts_var.
  TYPES tt_calculated  TYPE zif_ace_parse_data=>tt_calculated.
  TYPES tt_composed    TYPE zif_ace_parse_data=>tt_composed.
  TYPES ts_refvar      TYPE zif_ace_parse_data=>ts_refvar.
  TYPES tt_refvar      TYPE zif_ace_parse_data=>tt_refvar.
  TYPES tt_kword       TYPE zif_ace_parse_data=>tt_kword.
  TYPES tt_vars        TYPE zif_ace_parse_data=>tt_vars.
  TYPES tt_params      TYPE zif_ace_parse_data=>tt_params.
  TYPES ts_int_tabs    TYPE zif_ace_parse_data=>ts_int_tabs.
  TYPES tt_tabs        TYPE zif_ace_parse_data=>tt_tabs.
  TYPES ts_meta        TYPE zif_ace_parse_data=>ts_meta.
  TYPES tt_classes     TYPE zif_ace_parse_data=>tt_classes.
  TYPES ts_enh_block   TYPE zif_ace_parse_data=>ts_enh_block.
  TYPES tt_enh_blocks  TYPE zif_ace_parse_data=>tt_enh_blocks.
  TYPES ts_prog        TYPE zif_ace_parse_data=>ts_prog.
  TYPES tt_progs       TYPE zif_ace_parse_data=>tt_progs.
  TYPES ts_class_def   TYPE zif_ace_parse_data=>ts_class_def.
  TYPES tt_class_defs  TYPE zif_ace_parse_data=>tt_class_defs.
  TYPES ts_handler_map TYPE zif_ace_parse_data=>ts_handler_map.
  TYPES tt_handler_map TYPE zif_ace_parse_data=>tt_handler_map.

  " ts_source is now an alias for the canonical type in ZIF_ACE_PARSE_DATA
  TYPES ts_source TYPE zif_ace_parse_data=>ts_parse_data .
  types:
    BEGIN OF ts_locals,
               program    TYPE tpda_program,
               eventtype  TYPE tpda_event_type,
               eventname  TYPE tpda_event,
               loc_fill   TYPE boolean,
               locals_tab TYPE tpda_scr_locals_it,
               mt_fs      TYPE tpda_scr_locals_it,
             END OF ts_locals .
  types:
    BEGIN OF ts_globals,
               program     TYPE tpda_program,
               glob_fill   TYPE boolean,
               globals_tab TYPE tpda_scr_globals_it,
               mt_fs       TYPE tpda_scr_locals_it,
             END OF ts_globals .
  types:
    BEGIN OF ts_watch,
               program TYPE string,
               line    TYPE i,
             END OF ts_watch .
  types:
    tt_watch TYPE STANDARD  TABLE OF ts_watch WITH EMPTY KEY .
  types:
    BEGIN OF ts_bpoint,
               program TYPE string,
               include TYPE string,
               line    TYPE i,
               type    TYPE char1,
               del     TYPE char1,
             END OF ts_bpoint .
  types:
    tt_bpoints TYPE STANDARD TABLE OF ts_bpoint WITH EMPTY KEY .
  types:
    tt_table TYPE STANDARD TABLE OF ts_table
            WITH NON-UNIQUE DEFAULT KEY .

  data M_VERSION type X .
  data M_HISTORY type X .
  data M_VISUALIZATION type X .
  data M_VARHIST type X .
  data M_ZCODE type X .
  data M_DIRECTION type X .
  data M_PRG type TPDA_SCR_PRG_INFO .
  data M_DEBUG_BUTTON like SY-UCOMM .

  types:
    BEGIN OF ts_nav_entry,
      include TYPE program,
      line    TYPE i,
    END OF ts_nav_entry.
  types tt_nav_history TYPE STANDARD TABLE OF ts_nav_entry WITH EMPTY KEY.

  data MT_NAV_HISTORY type TT_NAV_HISTORY.
  data MV_NAV_IDX     type I value 0.
  data MV_NAV_SILENT  type BOOLEAN.
  data M_SHOW_STEP type BOOLEAN .
  data MT_BPOINTS type TT_BPOINTS .
  data MO_VIEWER type ref to ZCL_ACE .
  data MO_SPLITTER_CODE type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_VAR type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_STEPS type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_TOOLBAR_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_IMPORTING_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_LOCALS_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_EXPORTING_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_CODE_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_IMP_EXP_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_EDITOR_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_STEPS_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_STACK_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_HIST_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_CODE_VIEWER type ref to CL_GUI_ABAPEDIT .
  data:
    mt_stack               TYPE TABLE OF ZCL_ACE=>T_STACK .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_SALV_STACK type ref to CL_SALV_TABLE .
  data MO_SALV_STEPS type ref to CL_SALV_TABLE .
  data MO_SALV_HIST type ref to CL_SALV_TABLE .
  data MT_BREAKS type TPDA_BP_PERSISTENT_IT .
  data MT_WATCH type TT_WATCH .
  data MT_COVERAGE type TT_WATCH .
  data:
    mt_calls               TYPE TABLE OF ZCL_ACE=>TS_CALL .
  data M_HIST_DEPTH type I value 19 .
  data M_START_STACK type I .
  data MV_CALC_ONLY type BOOLEAN .
  data:
    mt_source              TYPE STANDARD  TABLE OF ts_source .
  data MS_SOURCES type TS_SOURCE .
  data:
    mt_params              TYPE STANDARD  TABLE OF ZCL_ACE=>ts_params .
  data:
    mt_locals_set          TYPE STANDARD TABLE OF ts_locals .
  data:
    mt_globals_set         TYPE STANDARD TABLE OF ts_globals .
  data MS_SEL_CALL type ZCL_ACE=>TS_CALLS_LINE .
  types:
    BEGIN OF ts_code_context,
               evtype TYPE string,
               evname TYPE string,
               class  TYPE string,
             END OF ts_code_context .
  data MS_CODE_CONTEXT type TS_CODE_CONTEXT .
  data MV_NEW_PARSER type ABAP_BOOL .
  data MV_SHOW_PARSE_TIME type ABAP_BOOL .

  methods CONSTRUCTOR
    importing
      !I_DEBUGGER        type ref to ZCL_ACE
      !I_ADDITIONAL_NAME type STRING optional .
  methods ADD_TOOLBAR_BUTTONS .
  methods HND_TOOLBAR
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods SHOW_PARSE_TIME
    importing
      !I_TS1 type TIMESTAMPL .
  methods SET_PROGRAM
    importing
      !I_INCLUDE type PROGRAM .
  methods SET_PROGRAM_LINE
    importing
      !I_LINE like SY-INDEX optional .
  methods PUSH_NAV_ENTRY
    importing
      !I_INCLUDE type PROGRAM
      !I_LINE    type I.
    " Window caption: CLASS->METHOD / FM: name / program - include
  methods SET_NAV_CAPTION
    importing
      !I_INCLUDE type PROGRAM
      !I_CLASS   type STRING optional
      !I_EVTYPE  type STRING optional
      !I_EVNAME  type STRING optional .
    " Open an object in the SAP editor when it is not part of the parsed set
  methods NAVIGATE_EXTERNAL
    importing
      !I_CALL type ZIF_ACE_PARSE_DATA=>TS_CALLS .
  methods SET_MIXPROG_LINE
    importing
      !I_LINE like SY-INDEX optional .
  methods CREATE_CODE_VIEWER .
  methods APPLY_DEPTH .
  methods SHOW_STACK .
  methods SHOW_COVERAGE .
  methods ON_STACK_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !COLUMN
      !ROW .
  methods ON_EDITOR_DOUBLE_CLICK
    for event DBLCLICK of CL_GUI_ABAPEDIT
    importing
      !SENDER .
  methods ON_EDITOR_BORDER_CLICK
    for event BORDER_CLICK of CL_GUI_ABAPEDIT
    importing
      !CNTRL_PRESSED_SET
      !LINE
      !SHIFT_PRESSED_SET .
protected section.
private section.
ENDCLASS.
CLASS ZCL_ACE_WINDOW IMPLEMENTATION.
  method ADD_TOOLBAR_BUTTONS.
      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.
      button  = VALUE #(
       ( function = 'RUN' icon = CONV #( icon_execute_object ) quickinfo = 'Run report' )

       ( COND #( WHEN ZCL_ACE=>I_MERMAID_ACTIVE = abap_true
        THEN VALUE #( function = 'CALLS' icon = CONV #( icon_workflow_process ) quickinfo = ' Calls Flow' text = 'Diagrams' ) ) )
       ( COND #( WHEN ZCL_ACE=>I_MERMAID_ACTIVE = abap_true
        THEN VALUE #( function = 'CMAP' icon = CONV #( icon_structure ) quickinfo = 'Static method call map' text = 'Class Map' ) ) )
       ( function = 'CODEMIX'     icon = CONV #( icon_wizard )              quickinfo = 'Full code flow sequence'          text = 'Code Flow' )
       ( function = 'TOGGLE_CALC' icon = CONV #( icon_biw_formula )        quickinfo = 'Toggle: show all steps / only calculated' text = 'Show All Steps' )
       ( function = 'HANDLERS'  icon = CONV #( icon_oo_event )            quickinfo = 'Event Handlers flow'              text = 'Handlers' )
       ( function = 'CODE'      icon = CONV #( icon_customer_warehouse )  quickinfo = 'Only Z'                           text = 'Only Z' )
       ( function = 'DEPTH_M'   icon = CONV #( icon_arrow_left )          quickinfo = 'Decrease depth'                   text = '' )
       ( function = 'DEPTH'     icon = CONV #( icon_next_hierarchy_level ) quickinfo = 'History depth level' text = |Depth { m_hist_depth }| )
       ( function = 'DEPTH_P'   icon = CONV #( icon_arrow_right )         quickinfo = 'Increase depth'                   text = '' )
       ( butn_type = 3  )
       "( function = 'METRICS'   icon = CONV #( icon_report )              quickinfo = 'Code Metrics (McCabe CC + Halstead)' text = 'Metrics' )
       "( function = 'MDEBUG'    icon = CONV #( icon_tools )               quickinfo = 'Metrics Debug: operators/operands per block' text = 'Mdebug' )
       ( function = 'MHTML'     icon = CONV #( icon_htm )                quickinfo = 'Code Metrics (McCabe CC + Halstead)'  text = 'Metrics' )
       ( butn_type = 3  )
       ( function = 'STEPS'       icon = CONV #( icon_next_step )    quickinfo = 'Steps table'                   text = 'Steps' )
       ( butn_type = 3  )
       ( function = 'WHOLE_CLASS' icon = CONV #( icon_select_all )   quickinfo = 'Get local class from Global'   text = 'Get whole Class' )
       ( function = 'INFO'        icon = CONV #( icon_bw_gis )       quickinfo = 'Documentation'                 text = '' )
                      ).
      mo_toolbar->add_button_group( button ).
      event-eventid = cl_gui_toolbar=>m_id_function_selected.
      event-appl_event = space.
      APPEND event TO events.
      mo_toolbar->set_registered_events( events = events ).
      SET HANDLER me->hnd_toolbar FOR mo_toolbar.
  endmethod.
  method CONSTRUCTOR.
      DATA:  text TYPE char100.
      text = i_debugger->mv_prog.
      super->constructor( ).
      mo_viewer = i_debugger.
      m_history = m_varhist = m_zcode = '01'.
      m_hist_depth = 19.
      mo_box = create( i_name = text i_width = 1300 i_hight = 350 ).
      SET HANDLER on_box_close FOR mo_box.
      CREATE OBJECT mo_splitter
        EXPORTING parent = mo_box rows = 3 columns = 1 EXCEPTIONS OTHERS = 1.
      mo_splitter->get_container( EXPORTING row = 2 column = 1 RECEIVING container = mo_code_container ).
      mo_splitter->get_container( EXPORTING row = 1 column = 1 RECEIVING container = mo_toolbar_container ).
      mo_splitter->get_container( EXPORTING row = 3 column = 1 RECEIVING container = mo_tables_container ).
      mo_splitter->set_row_height( id = 1 height = '4' ).
      mo_splitter->set_row_height( id = 2 height = '70' ).
      mo_splitter->set_row_sash( id = 1 type = 0 value = 0 ).
      CREATE OBJECT mo_splitter_code
        EXPORTING parent = mo_code_container rows = 1 columns = 2 EXCEPTIONS OTHERS = 1.
      mo_splitter_code->get_container( EXPORTING row = 1 column = 2 RECEIVING container = mo_editor_container ).
      mo_splitter_code->get_container( EXPORTING row = 1 column = 1 RECEIVING container = mo_locals_container ).
      mo_splitter_code->set_column_width( EXPORTING id = 1 width = '35' ).
      SET HANDLER on_box_close FOR mo_box.
      CREATE OBJECT mo_toolbar EXPORTING parent = mo_toolbar_container.
      add_toolbar_buttons( ).
      mo_toolbar->set_button_info( EXPORTING fcode = 'DEPTH' text = |Depth { m_hist_depth }| ).
      mo_toolbar->set_visible( 'X' ).
      create_code_viewer( ).
  endmethod.
  method CREATE_CODE_VIEWER.
      DATA: events TYPE cntl_simple_events,
            event  TYPE cntl_simple_event.
      CHECK mo_code_viewer IS INITIAL.
      CREATE OBJECT mo_code_viewer
        EXPORTING parent = mo_editor_container max_number_chars = 100.
      mo_code_viewer->init_completer( ).
      mo_code_viewer->upload_properties(
        EXCEPTIONS dp_error_create = 1 dp_error_general = 2 dp_error_send = 3 OTHERS = 4 ).
      event-eventid = cl_gui_textedit=>event_double_click.
      APPEND event TO events.
      mo_code_viewer->set_registered_events( events ).
      mo_code_viewer->register_event_border_click( ).
      mo_code_viewer->register_event_break_changed( ).
      SET HANDLER on_editor_double_click FOR mo_code_viewer.
      SET HANDLER on_editor_border_click FOR mo_code_viewer.
      mo_code_viewer->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
      mo_code_viewer->create_document( ).
      mo_code_viewer->set_readonly_mode( 1 ).
  endmethod.
  METHOD hnd_toolbar.
    CONSTANTS: c_mask TYPE x VALUE '01'.
    FIELD-SYMBOLS: <any> TYPE any.
    m_debug_button = fcode.
    READ TABLE mt_stack INDEX 1 INTO DATA(stack).
    CASE fcode.
      WHEN 'RUN'.
        DATA: lt_source TYPE STANDARD TABLE OF text255,
              lv_prog   TYPE progname VALUE 'Z_SMART_DEBUGGER_SCRIPT'.
        READ REPORT lv_prog INTO lt_source.
        DELETE lt_source INDEX 2.
        IF sy-subrc = 0.
          CALL FUNCTION 'CLPB_EXPORT'
            TABLES
              data_tab   = lt_source
            EXCEPTIONS
              clpb_error = 1
              OTHERS     = 2.
        ENDIF.
        lv_prog = mo_viewer->mv_prog.
        DATA(lv_count) = 0.
        SELECT COUNT(*) FROM reposrc WHERE progname = @lv_prog AND subc = '1' INTO @lv_count.
        IF lv_count = 1. SUBMIT (lv_prog) VIA SELECTION-SCREEN AND RETURN. ENDIF.

      WHEN 'DEPTH_M'.
        IF m_hist_depth > 0. m_hist_depth = m_hist_depth - 1. ENDIF.
        apply_depth( ).

      WHEN 'DEPTH_P'.
        IF m_hist_depth < 99. m_hist_depth = m_hist_depth + 1. ENDIF.
        apply_depth( ).

      WHEN 'DEPTH'.
        DATA: lv_answer TYPE c LENGTH 1, lv_value1 TYPE spop-varvalue1.
        CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
          EXPORTING
            textline1   = |Current depth: { m_hist_depth }. Enter new value (0-99):|
            titel       = 'Set History Depth'
            valuelength = '2'
          IMPORTING
            answer      = lv_answer
            value1      = lv_value1
          EXCEPTIONS
            OTHERS      = 1.
        IF sy-subrc <> 0 OR lv_answer <> 'J' OR lv_value1 IS INITIAL. RETURN. ENDIF.
        DATA(lv_new_depth) = CONV i( lv_value1 ).
        IF lv_new_depth < 0.
          lv_new_depth = 0.
        ELSEIF lv_new_depth > 99.
          lv_new_depth = 99.
        ENDIF.
        m_hist_depth = lv_new_depth.
        apply_depth( ).

      WHEN 'CALLS'.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
        apply_depth( ).
        IF mo_mermaid IS INITIAL OR mo_mermaid->mo_box IS INITIAL.
          mo_mermaid = NEW zcl_ace_mermaid( io_debugger = mo_viewer i_type = 'CALLS' ).
        ELSE.
          mo_mermaid->mv_type = 'CALLS'.
          mo_mermaid->refresh( ).
          mo_mermaid->mo_box->set_focus( mo_mermaid->mo_box ).
        ENDIF.

      WHEN 'CMAP'.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
        apply_depth( ).
        IF mo_mermaid IS INITIAL OR mo_mermaid->mo_box IS INITIAL.
          mo_mermaid = NEW zcl_ace_mermaid( io_debugger = mo_viewer i_type = 'CMAP' ).
        ELSE.
          mo_mermaid->mv_type = 'CMAP'.
          mo_mermaid->refresh( ).
          mo_mermaid->mo_box->set_focus( mo_mermaid->mo_box ).
        ENDIF.

      WHEN 'CODEMIX'.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
        apply_depth( ).
        mo_viewer->get_code_mix( i_calc_path = mv_calc_only ).
        mo_viewer->mo_window->show_stack( ).

      WHEN 'TOGGLE_CALC'.
        mv_calc_only = COND #( WHEN mv_calc_only = abap_true THEN abap_false ELSE abap_true ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'TOGGLE_CALC'
                    text  = COND #( WHEN mv_calc_only = abap_true
                                    THEN 'Only Calculated'
                                    ELSE 'Show All Steps' ) ).
        " Re-run current view with new filter if Code Flow is active
        IF mo_viewer->mo_window->m_prg-include = 'Code_Flow_Mix'.
          CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
          apply_depth( ).
          mo_viewer->get_code_mix( i_calc_path = mv_calc_only ).
          mo_viewer->mo_window->show_stack( ).
        ENDIF.

      WHEN 'HANDLERS'.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.

        LOOP AT mo_viewer->mo_window->ms_sources-tt_handler_map INTO DATA(ls_hm).
          CHECK ls_hm-hdl_method IS NOT INITIAL.
          DATA(lv_hdl_class) = ls_hm-hdl_class.
          IF lv_hdl_class IS INITIAL.
            LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(ls_cl_hdl)
              WHERE eventname = ls_hm-hdl_method AND eventtype = 'METHOD'.
              lv_hdl_class = ls_cl_hdl-class. EXIT.
            ENDLOOP.
          ENDIF.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
            INTO DATA(ls_call_hdl)
            WITH KEY class = lv_hdl_class eventtype = 'METHOD' eventname = ls_hm-hdl_method.
          CHECK sy-subrc = 0.
          ADD 1 TO mo_viewer->m_step.
          APPEND VALUE zcl_ace=>t_step_counter(
            step       = mo_viewer->m_step
            stacklevel = 1
            eventtype  = 'EVENT'
            eventname  = |EVENT:{ ls_hm-event_name }|
            program    = ls_call_hdl-program
            include    = ls_call_hdl-include
          ) TO mo_viewer->mt_steps.
          zcl_ace_source_parser=>parse_call(
            EXPORTING
              i_index     = ls_call_hdl-index
              i_e_name    = ls_call_hdl-eventname
              i_e_type    = ls_call_hdl-eventtype
              i_class     = ls_call_hdl-class
              i_program   = CONV #( ls_call_hdl-program )
              i_include   = CONV #( ls_call_hdl-include )
              i_stack     = 1
              io_debugger = mo_viewer ).
        ENDLOOP.

        IF mo_viewer->mt_steps IS INITIAL.
          MESSAGE 'No event handlers found. Run CodeMix first to parse the source.' TYPE 'I'.
          RETURN.
        ENDIF.
        mo_viewer->get_code_mix( ).
        mo_viewer->mo_window->show_stack( ).
        mo_viewer->mo_window->mo_box->set_caption(
          |Handlers: { lines( mo_viewer->mo_window->ms_sources-tt_handler_map ) } registered| ).

      WHEN 'CODE'.
        m_zcode = m_zcode BIT-XOR c_mask.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
        DATA(ls_ctx_code) = mo_viewer->mo_window->ms_code_context.
        zcl_ace_source_parser=>code_execution_scanner(
          i_program = mo_viewer->mo_window->m_prg-program
          i_include = mo_viewer->mo_window->m_prg-program io_debugger = mo_viewer
          i_evname = ls_ctx_code-evname i_evtype = ls_ctx_code-evtype i_class = ls_ctx_code-class ).
        IF m_zcode IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Z & Standard' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Only Z code' ).
        ENDIF.
        mo_viewer->mo_window->show_stack( ).
        IF mo_mermaid IS NOT INITIAL AND mo_mermaid->mo_box IS NOT INITIAL.
          mo_mermaid->refresh( ).
        ENDIF.
        IF mo_viewer->mo_window->m_prg-include = 'Code_Flow_Mix'.
          mo_viewer->get_code_mix( ).
          mo_viewer->mo_window->show_stack( ).
        ENDIF.

      WHEN 'METRICS'.
        zcl_ace_metrics_window=>show(
          is_parse_data = mo_viewer->mo_window->ms_sources
          i_program     = mo_viewer->mo_window->m_prg-program ).

      WHEN 'MDEBUG'.
        zcl_ace_metrics_window=>show_debug(
          is_parse_data = mo_viewer->mo_window->ms_sources
          i_program     = mo_viewer->mo_window->m_prg-program ).

      WHEN 'MHTML'.
        DATA(lv_mhtml_prg) = mo_viewer->mo_window->m_prg-program.
        DATA(lt_html) = zcl_ace_metrics_window=>build_html(
          is_parse_data = mo_viewer->mo_window->ms_sources
          i_program     = lv_mhtml_prg ).
        DATA(lo_html_popup) = NEW zcl_ace_html_viewer(
          it_html  = lt_html
          i_title  = CONV #( |Metrics: { lv_mhtml_prg }| )
          i_width  = 1200
          i_height = 600 ).
        IF lo_html_popup->mo_box IS NOT INITIAL.
          APPEND INITIAL LINE TO zcl_ace=>mt_popups
            ASSIGNING FIELD-SYMBOL(<mhtml_popup>).
          <mhtml_popup>-parent = mo_viewer->mo_window->mo_box.
          <mhtml_popup>-child  = lo_html_popup->mo_box.
           lo_html_popup->mo_box->set_focus( lo_html_popup->mo_box ).

        ENDIF.

      WHEN 'INFO'.
        DATA(l_url) = 'https://github.com/ysichov/ACE'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.

      WHEN 'STEPS'.
        zcl_ace_table_viewer=>open_int_table( i_name = 'Steps' it_tab = mo_viewer->mt_steps io_window = mo_viewer->mo_window ).

      WHEN 'WHOLE_CLASS'.
        DATA: lt_whole_class  TYPE sci_include,
              lv_class_name   TYPE string,
              lv_in_methods   TYPE boolean VALUE abap_false,
              lv_section_done TYPE boolean VALUE abap_false,
              lv_cu_first     TYPE boolean VALUE abap_true.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(ls_wc_prog).
        LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(ls_mac) WHERE program = ls_wc_prog-program.
          CHECK to_upper( CONV string( ls_mac-include ) ) CP '*CCMAC'.
          CHECK ls_mac-source_tab IS NOT INITIAL.
          APPEND LINES OF ls_mac-source_tab TO lt_whole_class.
          APPEND INITIAL LINE TO lt_whole_class.
        ENDLOOP.
        LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(ls_prog_wc) WHERE program = ls_wc_prog-program.
          DATA(lv_include_raw) = ls_prog_wc-include.
          DATA(lv_include)     = to_upper( CONV string( lv_include_raw ) ).
          DATA(lv_is_cp)     = xsdbool( lv_include CP '*CP' ).
          DATA(lv_is_cu)     = xsdbool( lv_include CP '*CU' ).
          DATA(lv_is_method) = xsdbool( lv_include CP '*CM*' ).
          IF lv_is_cp = abap_true OR lv_include CP '*====E' OR lv_include CS 'EIMP'
            OR lv_include CP '*CCMAC' OR lv_include CP '*CCIMP' OR lv_include CP '*CCAU'. CONTINUE.
          ENDIF.
          CHECK ls_prog_wc-source_tab IS NOT INITIAL.
          IF lv_is_method = abap_true AND lv_in_methods = abap_false.
            lv_in_methods = abap_true.
            APPEND INITIAL LINE TO lt_whole_class.
            APPEND |ENDCLASS.| TO lt_whole_class.
            APPEND INITIAL LINE TO lt_whole_class.
            APPEND |CLASS { lv_class_name } IMPLEMENTATION.| TO lt_whole_class.
            APPEND INITIAL LINE TO lt_whole_class.
            lv_section_done = abap_false.
          ENDIF.
          IF lv_section_done = abap_true. APPEND INITIAL LINE TO lt_whole_class. ENDIF.
          lv_section_done = abap_true.
          IF lv_is_cu = abap_true AND lv_cu_first = abap_true.
            lv_cu_first = abap_false.
            DATA(lv_eq_pos) = find( val = ls_prog_wc-include sub = '=' ).
            IF lv_eq_pos > 0. lv_class_name = substring( val = ls_prog_wc-include len = lv_eq_pos ). ENDIF.
          ENDIF.
          APPEND LINES OF ls_prog_wc-source_tab TO lt_whole_class.
        ENDLOOP.
        APPEND INITIAL LINE TO lt_whole_class.
        APPEND |ENDCLASS.| TO lt_whole_class.
        LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(ls_cc) WHERE program = ls_wc_prog-program.
          DATA(lv_cc_inc) = to_upper( CONV string( ls_cc-include ) ).
          CHECK lv_cc_inc CP '*CCIMP' OR lv_cc_inc CP '*CCAU'.
          CHECK ls_cc-source_tab IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_whole_class.
          APPEND LINES OF ls_cc-source_tab TO lt_whole_class.
        ENDLOOP.
        mo_code_viewer->set_text( table = lt_whole_class ).
        mo_box->set_caption( |Whole Class: { lv_class_name }| ).

    ENDCASE.
  ENDMETHOD.
  method ON_EDITOR_BORDER_CLICK.
      DATA: type TYPE char1, program TYPE program, include TYPE program, code_line TYPE i.
      IF cntrl_pressed_set IS INITIAL. type = 'S'. ELSE. type = 'E'. ENDIF.
      IF m_prg-include = 'Code_Flow_Mix'.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = 'Code_Flow_Mix' INTO DATA(prog_mix).
        READ TABLE prog_mix-t_keywords WITH KEY v_line = line INTO DATA(keyword).
      ELSE.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = m_prg-include INTO prog_mix.
        IF prog_mix-v_keywords IS NOT INITIAL.
          LOOP AT prog_mix-v_keywords INTO keyword WHERE v_line = line. EXIT. ENDLOOP.
        ELSE.
          LOOP AT prog_mix-t_keywords INTO keyword WHERE v_line = line. EXIT. ENDLOOP.
        ENDIF.
      ENDIF.
      CHECK keyword-include IS NOT INITIAL.
      program = m_prg-program. include = keyword-include. code_line = keyword-line.
      IF include IS INITIAL. program = m_prg-program. include = m_prg-include. code_line = line. ENDIF.
      LOOP AT mt_bpoints ASSIGNING FIELD-SYMBOL(<point>) WHERE line = code_line AND include = include.
        type = <point>-type.
        CALL FUNCTION 'RS_DELETE_BREAKPOINT'
          EXPORTING index = code_line mainprog = program program = include bp_type = type
          EXCEPTIONS not_executed = 1 OTHERS = 2.
        IF sy-subrc = 0. <point>-del = abap_true. ENDIF.
      ENDLOOP.
      IF sy-subrc <> 0.
        CALL FUNCTION 'RS_SET_BREAKPOINT'
          EXPORTING index = code_line program = include mainprogram = program bp_type = type
          EXCEPTIONS not_executed = 1 OTHERS = 2.
      ENDIF.
      DELETE mt_bpoints WHERE del IS NOT INITIAL.
      IF m_prg-include = 'Code_Flow_Mix'. set_mixprog_line( ). ELSE. set_program_line( ). ENDIF.
  endmethod.
  METHOD on_editor_double_click.
    sender->get_selection_pos(
      IMPORTING from_line = DATA(fr_line) from_pos = DATA(fr_pos)
                to_line   = DATA(to_line) to_pos   = DATA(to_pos) ).

    READ TABLE ms_sources-tt_progs WITH KEY include = m_prg-include INTO DATA(prog).
    IF sy-subrc <> 0.
      MESSAGE |DBG dblclick: include { m_prg-include } not in tt_progs| TYPE 'I'.
      RETURN.
    ENDIF.
    DATA(lr_kw) = REF #( prog-t_keywords ).
    IF prog-v_keywords IS NOT INITIAL. lr_kw = REF #( prog-v_keywords ). ENDIF.

    LOOP AT lr_kw->* INTO DATA(kw) WHERE v_line = fr_line. EXIT. ENDLOOP.
    IF sy-subrc <> 0.
      MESSAGE |DBG dblclick: no keyword for v_line { fr_line } in { m_prg-include }| TYPE 'I'.
      RETURN.
    ENDIF.
    MESSAGE |DBG dblclick: line { fr_line } kw={ kw-name } calls={ lines( kw-tt_calls ) }| TYPE 'S'.

    DATA lv_target_vline   TYPE i.
    DATA lv_target_include TYPE program.

    CASE kw-name.

      WHEN 'CLASS' OR 'INTERFACE'.
        " Ищем стейтмент по строке
        LOOP AT prog-scan->statements INTO DATA(ls_stmt).
          READ TABLE prog-scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw_tok).
          IF sy-subrc = 0 AND ls_kw_tok-row = kw-line. EXIT. ENDIF.
        ENDLOOP.
        CHECK sy-subrc = 0.
        READ TABLE prog-scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
        CHECK sy-subrc = 0.
        DATA(lv_cls_name) = ls_tok-str.
        READ TABLE prog-scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        DATA(lv_kw2) = COND string( WHEN sy-subrc = 0 THEN ls_tok-str ELSE '' ).
        READ TABLE prog-scan->tokens INDEX ls_stmt-from + 3 INTO DATA(ls_tok3).
        DATA(lv_kw3) = COND string( WHEN sy-subrc = 0 THEN ls_tok3-str ELSE '' ).
        READ TABLE ms_sources-tt_class_defs WITH KEY class = lv_cls_name INTO DATA(ls_cd).
        CHECK sy-subrc = 0.
        CASE lv_kw2.
          WHEN 'IMPLEMENTATION'.
            lv_target_include = ls_cd-def_include.
            lv_target_vline   = ls_cd-def_line.
          WHEN 'DEFINITION'.
            IF lv_kw3 = 'DEFERRED'.
              lv_target_include = ls_cd-def_include.
              lv_target_vline   = ls_cd-def_line.
            ELSE.
              lv_target_include = ls_cd-impl_include.
              lv_target_vline   = ls_cd-impl_line.
            ENDIF.
          WHEN OTHERS.
            RETURN.
        ENDCASE.
        " Конвертируем реальную строку → виртуальную в целевом инклуде
        IF lv_target_include IS NOT INITIAL AND lv_target_vline > 0.
          READ TABLE ms_sources-tt_progs WITH KEY include = lv_target_include INTO DATA(lv_tprog).
          IF sy-subrc = 0.
            DATA(lr_tkw) = REF #( lv_tprog-t_keywords ).
            IF lv_tprog-v_keywords IS NOT INITIAL. lr_tkw = REF #( lv_tprog-v_keywords ). ENDIF.
            LOOP AT lr_tkw->* INTO DATA(tkw) WHERE line = lv_target_vline. EXIT. ENDLOOP.
            IF sy-subrc = 0. lv_target_vline = tkw-v_line. ENDIF.
          ENDIF.
        ENDIF.

      WHEN 'METHOD'.
        " Читаем имя метода из токенов
        LOOP AT prog-scan->statements INTO ls_stmt.
          READ TABLE prog-scan->tokens INDEX ls_stmt-from INTO ls_kw_tok.
          IF sy-subrc = 0 AND ls_kw_tok-row = kw-line. EXIT. ENDIF.
        ENDLOOP.
        CHECK sy-subrc = 0.
        READ TABLE prog-scan->tokens INDEX ls_stmt-from + 1 INTO ls_tok.
        CHECK sy-subrc = 0.
        DATA(lv_meth_name) = ls_tok-str.
        " Ищем сигнатуру в def_include через tt_calls_line
        READ TABLE ms_sources-tt_calls_line
          WITH KEY include = kw-include eventtype = 'METHOD' eventname = lv_meth_name
          INTO DATA(ls_cl).
        IF sy-subrc = 0 AND ls_cl-def_include IS NOT INITIAL AND ls_cl-def_line > 0.
          " Переходим на METHODS name в def_include (CU/CO/CI инклуд)
          lv_target_include = ls_cl-def_include.
          lv_target_vline   = ls_cl-def_line.
          " Конвертируем реальную строку → виртуальную
          READ TABLE ms_sources-tt_progs WITH KEY include = lv_target_include INTO DATA(lv_defprog).
          IF sy-subrc = 0.
            DATA(lr_defkw) = REF #( lv_defprog-t_keywords ).
            IF lv_defprog-v_keywords IS NOT INITIAL. lr_defkw = REF #( lv_defprog-v_keywords ). ENDIF.
            LOOP AT lr_defkw->* INTO DATA(defkw) WHERE line = lv_target_vline. EXIT. ENDLOOP.
            IF sy-subrc = 0. lv_target_vline = defkw-v_line. ENDIF.
          ENDIF.
        ELSE.
          " Fallback — прыгаем на ENDMETHOD в том же инклуде
          LOOP AT lr_kw->* INTO DATA(kw2) WHERE name = 'ENDMETHOD' AND index > kw-index.
            lv_target_vline   = kw2-v_line.
            lv_target_include = m_prg-include.
            EXIT.
          ENDLOOP.
        ENDIF.

      WHEN 'METHODS' OR 'CLASS-METHODS'.
        " Читаем имя метода
        LOOP AT prog-scan->statements INTO ls_stmt.
          READ TABLE prog-scan->tokens INDEX ls_stmt-from INTO ls_kw_tok.
          IF sy-subrc = 0 AND ls_kw_tok-row = kw-line. EXIT. ENDIF.
        ENDLOOP.
        CHECK sy-subrc = 0.
        READ TABLE prog-scan->tokens INDEX ls_stmt-from + 1 INTO ls_tok.
        CHECK sy-subrc = 0.
        lv_meth_name = ls_tok-str.
        " Ищем тело метода в include через tt_calls_line
        READ TABLE ms_sources-tt_calls_line
          WITH KEY eventtype = 'METHOD' eventname = lv_meth_name
          INTO ls_cl.
        IF sy-subrc = 0 AND ls_cl-include IS NOT INITIAL.
          " Переходим на METHOD name в CM-инклуде
          lv_target_include = ls_cl-include.
          " Ищем v_line строки METHOD в CM-инклуде
          READ TABLE ms_sources-tt_progs WITH KEY include = lv_target_include INTO DATA(lv_implprog).
          IF sy-subrc = 0.
            DATA(lr_implkw) = REF #( lv_implprog-t_keywords ).
            IF lv_implprog-v_keywords IS NOT INITIAL. lr_implkw = REF #( lv_implprog-v_keywords ). ENDIF.
            LOOP AT lr_implkw->* INTO DATA(implkw) WHERE name = 'METHOD'.
              READ TABLE lv_implprog-scan->statements INDEX implkw-index INTO DATA(ls_impl_stmt).
              IF sy-subrc = 0.
                READ TABLE lv_implprog-scan->tokens INDEX ls_impl_stmt-from + 1 INTO DATA(ls_impl_tok).
                IF sy-subrc = 0 AND ls_impl_tok-str = lv_meth_name.
                  lv_target_vline = implkw-v_line.
                  IF lv_target_vline = 0. lv_target_vline = implkw-line. ENDIF.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

      WHEN 'INCLUDE'.
        " Читаем имя include-программы из токенов и переходим на строку 1
        LOOP AT prog-scan->statements INTO ls_stmt.
          READ TABLE prog-scan->tokens INDEX ls_stmt-from INTO ls_kw_tok.
          IF sy-subrc = 0 AND ls_kw_tok-row = kw-line. EXIT. ENDIF.
        ENDLOOP.
        IF sy-subrc = 0.
          READ TABLE prog-scan->tokens INDEX ls_stmt-from + 1 INTO ls_tok.
          IF sy-subrc = 0.
            lv_target_include = CONV #( ls_tok-str ).
            lv_target_vline   = 1.
          ENDIF.
        ENDIF.

      WHEN 'ENDMETHOD'.
        " → прыгаем на открывающий METHOD
        LOOP AT lr_kw->* INTO kw2 WHERE name = 'METHOD' AND index < kw-index.
          lv_target_vline   = kw2-v_line.
          lv_target_include = m_prg-include.
        ENDLOOP.

      WHEN 'PERFORM'.
        " Читаем имя формы из токенов, ищем в tt_calls_line
        LOOP AT prog-scan->statements INTO ls_stmt.
          READ TABLE prog-scan->tokens INDEX ls_stmt-from INTO ls_kw_tok.
          IF sy-subrc = 0 AND ls_kw_tok-row = kw-line. EXIT. ENDIF.
        ENDLOOP.
        IF sy-subrc = 0.
          READ TABLE prog-scan->tokens INDEX ls_stmt-from + 1 INTO ls_tok.
          IF sy-subrc = 0.
            DATA(lv_form_name) = ls_tok-str.
            READ TABLE ms_sources-tt_calls_line
              WITH KEY eventtype = 'FORM' eventname = lv_form_name
              INTO DATA(ls_form_cl).
            IF sy-subrc = 0.
              lv_target_include = ls_form_cl-include.
              " Ищем v_line строки FORM в инклуде
              READ TABLE ms_sources-tt_progs WITH KEY include = lv_target_include INTO DATA(ls_form_prog).
              IF sy-subrc = 0.
                DATA(lr_fkw) = REF #( ls_form_prog-t_keywords ).
                IF ls_form_prog-v_keywords IS NOT INITIAL. lr_fkw = REF #( ls_form_prog-v_keywords ). ENDIF.
                LOOP AT lr_fkw->* INTO DATA(fkw) WHERE name = 'FORM' AND index = ls_form_cl-index. EXIT. ENDLOOP.
                IF sy-subrc = 0.
                  lv_target_vline = fkw-v_line.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        " Double-click on a recorded call (obj->meth( ), meth( ), CALL METHOD,
        " CALL FUNCTION, chained/old/new syntax) → jump to its implementation.

        " Calls are parsed lazily — trigger the statement parse on demand,
        " same as the flow builder does via parse_tokens( i_stmt_idx ).
        IF kw-tt_calls IS INITIAL AND kw-calls_parsed = abap_false.
          DATA lv_ctx_class TYPE string.
          DATA lv_ctx_evty  TYPE string.
          DATA lv_ctx_evn   TYPE string.
          CLEAR: lv_ctx_class, lv_ctx_evty, lv_ctx_evn.
          " Containing unit (class/method) of the clicked include — needed
          " for variable type resolution inside the parser
          READ TABLE ms_sources-tt_calls_line
            WITH KEY include = kw-include INTO DATA(ls_ctx_cl).
          IF sy-subrc = 0.
            lv_ctx_class = ls_ctx_cl-class.
            lv_ctx_evty  = ls_ctx_cl-eventtype.
            lv_ctx_evn   = ls_ctx_cl-eventname.
          ENDIF.
          zcl_ace_parser=>parse_tokens(
            EXPORTING
              i_program  = CONV #( COND string( WHEN kw-program IS NOT INITIAL
                                                THEN kw-program ELSE m_prg-program ) )
              i_include  = CONV #( kw-include )
              i_stmt_idx = kw-index
              i_class    = lv_ctx_class
              i_evtype   = lv_ctx_evty
              i_ev_name  = lv_ctx_evn
            CHANGING
              cs_source  = ms_sources ).
          " Re-read: parse_tokens fills tt_calls in tt_progs-t_keywords
          READ TABLE ms_sources-tt_progs WITH KEY include = kw-include INTO DATA(ls_reprog).
          IF sy-subrc = 0.
            READ TABLE ls_reprog-t_keywords WITH KEY index = kw-index INTO DATA(ls_rekw).
            IF sy-subrc = 0. kw-tt_calls = ls_rekw-tt_calls. ENDIF.
          ENDIF.
        ENDIF.

        IF kw-tt_calls IS INITIAL.
          MESSAGE |DBG dblclick: kw={ kw-name } line { fr_line } — no calls after parse| TYPE 'I'.
          RETURN.
        ENDIF.

        " The double-clicked word (the editor selects it on double-click) —
        " used to pick the right call when the line contains several.
        DATA lv_word TYPE string.
        CLEAR lv_word.
        sender->get_selected_text_as_stream(
          IMPORTING selected_text = lv_word EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc = 0.
          lv_word = to_upper( condense( lv_word ) ).
        ELSE.
          CLEAR lv_word.
        ENDIF.

        DATA ls_call LIKE LINE OF kw-tt_calls.
        CLEAR ls_call.
        IF lv_word IS NOT INITIAL.
          LOOP AT kw-tt_calls INTO DATA(ls_c) WHERE name = lv_word.
            ls_call = ls_c. EXIT.
          ENDLOOP.
        ENDIF.
        IF ls_call-name IS INITIAL.
          READ TABLE kw-tt_calls INDEX 1 INTO ls_call.
        ENDIF.
        IF ls_call-name IS INITIAL.
          MESSAGE |DBG dblclick: word={ lv_word } — no matching call entry| TYPE 'I'.
          RETURN.
        ENDIF.
        MESSAGE |DBG dblclick: word={ lv_word } call={ ls_call-class }/{ ls_call-event }/{ ls_call-name }| TYPE 'S'.

        " Find the implementation of the called unit in the parsed data
        DATA ls_tgt_cl LIKE LINE OF ms_sources-tt_calls_line.
        CLEAR ls_tgt_cl.
        IF ls_call-class IS NOT INITIAL.
          READ TABLE ms_sources-tt_calls_line
            WITH KEY class = ls_call-class eventtype = ls_call-event
                     eventname = ls_call-name
            INTO ls_tgt_cl.
        ENDIF.
        IF ls_tgt_cl-include IS INITIAL.
          " No class recorded (or not found under it) — match by name only
          READ TABLE ms_sources-tt_calls_line
            WITH KEY eventtype = ls_call-event eventname = ls_call-name
            INTO ls_tgt_cl.
        ENDIF.
        IF ls_tgt_cl-include IS INITIAL.
          " Not in the parsed set (standard FM/class etc.) → open externally
          navigate_external( ls_call ).
          RETURN.
        ENDIF.

        lv_target_include = ls_tgt_cl-include.
        " Locate the METHOD/FORM/FUNCTION keyword of the implementation
        " (by its statement index) to get the target viewer line
        READ TABLE ms_sources-tt_progs WITH KEY include = lv_target_include INTO lv_tprog.
        IF sy-subrc = 0.
          lr_tkw = REF #( lv_tprog-t_keywords ).
          IF lv_tprog-v_keywords IS NOT INITIAL. lr_tkw = REF #( lv_tprog-v_keywords ). ENDIF.
          LOOP AT lr_tkw->* INTO tkw WHERE index = ls_tgt_cl-index. EXIT. ENDLOOP.
          IF sy-subrc = 0.
            lv_target_vline = COND #( WHEN tkw-v_line > 0 THEN tkw-v_line ELSE tkw-line ).
          ELSE.
            MESSAGE |DBG dblclick: stmt index { ls_tgt_cl-index } not found in keywords of { lv_target_include }| TYPE 'I'.
          ENDIF.
        ELSE.
          MESSAGE |DBG dblclick: target include { lv_target_include } not in tt_progs| TYPE 'I'.
        ENDIF.

    ENDCASE.

    IF lv_target_vline <= 0.
      MESSAGE |DBG dblclick: kw={ kw-name } — no target line resolved| TYPE 'S'.
      RETURN.
    ENDIF.

    " Record the ORIGIN in the navigation history — without it NAV_BACK has
    " nowhere to return to when the start position was never navigated to.
    push_nav_entry( i_include = m_prg-include i_line = fr_line ).

    IF lv_target_include <> m_prg-include AND lv_target_include IS NOT INITIAL.
      m_prg-include = lv_target_include.
      set_program( lv_target_include ).
    ENDIF.
    set_program_line( lv_target_vline ).
    set_nav_caption( lv_target_include ).

  ENDMETHOD.
  method ON_STACK_DOUBLE_CLICK.
      READ TABLE mo_viewer->mo_window->mt_stack INDEX row INTO DATA(stack).
      MOVE-CORRESPONDING stack TO mo_viewer->mo_window->m_prg.
      MOVE-CORRESPONDING stack TO mo_viewer->ms_stack.
      mo_viewer->mo_window->m_prg-program = stack-prg.
      mo_viewer->show( ).
      CASE stack-eventtype.
        WHEN 'FUNCTION'.
          mo_viewer->mo_window->mo_box->set_caption( |FM: { stack-eventname }| ).
        WHEN OTHERS.
          mo_viewer->mo_window->mo_box->set_caption( |{ stack-program } : { stack-eventname }| ).
      ENDCASE.
  endmethod.
  method SET_MIXPROG_LINE.
      TYPES: lntab TYPE STANDARD TABLE OF i.
      DATA: lines TYPE lntab, flag TYPE boolean, programs TYPE TABLE OF program.
      mo_code_viewer->remove_all_marker( 2 ).
      mo_code_viewer->remove_all_marker( 4 ).
      LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(prog) WHERE include <> 'Code_Flow_Mix'.
        COLLECT prog-program INTO programs.
      ENDLOOP.
      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = 'Code_Flow_Mix' INTO prog.
      flag = abap_true.
      DO 2 TIMES.
        LOOP AT programs INTO DATA(program).
          CALL METHOD cl_abap_debugger=>read_breakpoints
            EXPORTING main_program = program flag_other_session = flag
            IMPORTING breakpoints_complete = DATA(points)
            EXCEPTIONS c_call_error = 1 generate = 2 wrong_parameters = 3 OTHERS = 4.
          LOOP AT points INTO DATA(point).
            CLEAR lines.
            READ TABLE prog-t_keywords WITH KEY include = point-include line = point-line INTO DATA(keyword).
            IF sy-subrc = 0.
              APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
              <line> = keyword-v_line.
              APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
              MOVE-CORRESPONDING point TO <point>.
              IF flag IS INITIAL. <point>-type = 'S'. ELSE. <point>-type = 'E'. ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        IF flag IS NOT INITIAL.
          mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines ).
        ELSE.
          mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).
        ENDIF.
        CLEAR flag.
      ENDDO.
      IF i_line IS NOT INITIAL.
        CLEAR lines.
        APPEND INITIAL LINE TO lines ASSIGNING <line>.
        <line> = i_line.
        mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lines ).
        mo_code_viewer->select_lines( EXPORTING from_line = i_line to_line = i_line ).
      ENDIF.
      mo_code_viewer->clear_line_markers( 'S' ).
      mo_code_viewer->draw( ).
  endmethod.
  METHOD show_parse_time.
    DATA: lv_ts2 TYPE timestampl, lv_sec TYPE tzntstmpl, lv_str(20) TYPE c.
    GET TIME STAMP FIELD lv_ts2.
    CALL METHOD cl_abap_tstmp=>subtract
      EXPORTING tstmp1 = lv_ts2 tstmp2 = i_ts1 RECEIVING r_secs = lv_sec.
    WRITE lv_sec TO lv_str LEFT-JUSTIFIED DECIMALS 3.
    CONDENSE lv_str NO-GAPS.
    MESSAGE |parse_tokens: { lv_str } sec| TYPE 'I'.
  ENDMETHOD.
  METHOD set_program.
    IF i_include = 'VIRTUAL'.
      LOOP AT ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<sp_virt>).
        CLEAR <sp_virt>-selected.
      ENDLOOP.
      READ TABLE ms_sources-tt_progs WITH KEY include = 'VIRTUAL' ASSIGNING <sp_virt>.
      IF sy-subrc = 0.
        <sp_virt>-selected = abap_true.
        mo_code_viewer->set_text( table = <sp_virt>-source_tab ).
      ENDIF.
      RETURN.
    ENDIF.
    DATA lv_ts1 TYPE timestampl.
    IF mo_viewer->mv_show_parse_time = abap_true. GET TIME STAMP FIELD lv_ts1. ENDIF.
    ZCL_ACE_PARSER=>parse(
      EXPORTING i_program = i_include i_include = i_include
      CHANGING cs_source = mo_viewer->mo_window->ms_sources ).
    IF mo_viewer->mv_show_parse_time = abap_true. show_parse_time( lv_ts1 ). ENDIF.
    IF mo_viewer->m_step IS INITIAL.
      DATA(ls_ctx) = ms_code_context.
      zcl_ace_source_parser=>code_execution_scanner(
        i_program = i_include i_include = i_include io_debugger = mo_viewer
        i_evtype = ls_ctx-evtype i_evname = ls_ctx-evname i_class = ls_ctx-class ).
    ENDIF.
    LOOP AT ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<sp_prog>).
      CLEAR <sp_prog>-selected.
    ENDLOOP.
    READ TABLE ms_sources-tt_progs WITH KEY include = i_include ASSIGNING <sp_prog>.
    IF sy-subrc = 0.
      <sp_prog>-selected = abap_true.
      IF <sp_prog>-v_source IS NOT INITIAL.
        mo_code_viewer->set_text( table = <sp_prog>-v_source ).
      ELSE.
        mo_code_viewer->set_text( table = <sp_prog>-source_tab ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  method SET_PROGRAM_LINE.
      TYPES: lntab TYPE STANDARD TABLE OF i.
      DATA: lines TYPE lntab, line_num TYPE i.

      " Единственная точка записи истории навигации
      IF i_line IS NOT INITIAL AND m_prg-include IS NOT INITIAL AND mv_nav_silent IS INITIAL.
        push_nav_entry( i_include = m_prg-include i_line = i_line ).
      ENDIF.
      CLEAR mv_nav_silent.

      mo_code_viewer->remove_all_marker( 2 ).
      mo_code_viewer->remove_all_marker( 4 ).
      mo_code_viewer->remove_all_marker( 7 ).
      CLEAR mt_bpoints.
      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = m_prg-include INTO DATA(prog_cur).
      DATA(lr_kw) = REF #( prog_cur-t_keywords ).
      IF prog_cur-v_keywords IS NOT INITIAL. lr_kw = REF #( prog_cur-v_keywords ). ENDIF.
      DATA lt_includes TYPE STANDARD TABLE OF program WITH EMPTY KEY.
      DATA lv_inc      TYPE program.
      LOOP AT lr_kw->* INTO DATA(lv_kw_inc).
        lv_inc = lv_kw_inc-include.
        IF lv_inc IS NOT INITIAL.
          READ TABLE lt_includes WITH KEY table_line = lv_inc TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0. APPEND lv_inc TO lt_includes. ENDIF.
        ENDIF.
      ENDLOOP.
      IF lt_includes IS INITIAL. lv_inc = m_prg-include. APPEND lv_inc TO lt_includes. ENDIF.
      CALL METHOD cl_abap_debugger=>read_breakpoints
        EXPORTING main_program = mo_viewer->mo_window->m_prg-program
        IMPORTING breakpoints_complete = DATA(points)
        EXCEPTIONS c_call_error = 1 generate = 2 wrong_parameters = 3 OTHERS = 4.
      LOOP AT points INTO DATA(point).
        READ TABLE lt_includes WITH KEY table_line = point-include TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.
        LOOP AT lr_kw->* INTO DATA(bp_kw) WHERE include = point-include AND line = point-line. EXIT. ENDLOOP.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
          <line> = bp_kw-v_line.
          READ TABLE mt_bpoints TRANSPORTING NO FIELDS WITH KEY include = point-include line = point-line.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
            MOVE-CORRESPONDING point TO <point>. <point>-type = 'S'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).
      CLEAR lines.
      LOOP AT lt_includes INTO lv_inc.
        CALL METHOD cl_abap_debugger=>read_breakpoints
          EXPORTING main_program = lv_inc flag_other_session = abap_true
          IMPORTING breakpoints_complete = points
          EXCEPTIONS c_call_error = 1 generate = 2 wrong_parameters = 3 OTHERS = 4.
        LOOP AT points INTO point WHERE include = lv_inc.
          LOOP AT lr_kw->* INTO bp_kw WHERE include = point-include AND line = point-line. EXIT. ENDLOOP.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO lines ASSIGNING <line>. <line> = bp_kw-v_line.
            READ TABLE mt_bpoints TRANSPORTING NO FIELDS WITH KEY include = point-include line = point-line.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO mt_bpoints ASSIGNING <point>.
              MOVE-CORRESPONDING point TO <point>. <point>-type = 'E'.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines ).
      IF i_line IS NOT INITIAL.
        CLEAR lines.
        APPEND INITIAL LINE TO lines ASSIGNING <line>. <line> = i_line.
        mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lines ).
        mo_code_viewer->select_lines( EXPORTING from_line = i_line to_line = i_line ).
      ENDIF.
      mo_code_viewer->clear_line_markers( 'S' ).
      mo_code_viewer->draw( ).
  endmethod.
  METHOD show_coverage.
    DATA: split TYPE TABLE OF string.
    CLEAR: mt_watch, mt_coverage.
    LOOP AT mo_viewer->mt_steps INTO DATA(step).
      READ TABLE mt_stack WITH KEY include = step-include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO mt_stack ASSIGNING FIELD-SYMBOL(<stack>).
        MOVE-CORRESPONDING step TO <stack>.
        SPLIT <stack>-program AT '=' INTO TABLE split.
        <stack>-prg = <stack>-program.
        <stack>-program = split[ 1 ].
      ENDIF.
      IF step-include <> mo_viewer->mo_window->m_prg-include. CONTINUE. ENDIF.
    ENDLOOP.
    IF mt_stack IS INITIAL.
      SORT ms_sources-tt_progs BY stack.
      LOOP AT ms_sources-tt_progs INTO DATA(prog) WHERE program IS NOT INITIAL.
        CHECK prog-t_keywords IS NOT INITIAL.
        APPEND INITIAL LINE TO mt_stack ASSIGNING <stack>.
        MOVE-CORRESPONDING prog TO <stack>.
        SPLIT <stack>-program AT '=' INTO TABLE split.
        <stack>-prg = <stack>-program.
        <stack>-program = split[ 1 ].
        <stack>-stacklevel = prog-stack.
        DATA(pos) = strlen( <stack>-program ).
        pos = pos - 2.
        IF pos > 0.
          DATA(incl) = <stack>-include+pos(2).
          SELECT SINGLE funcname FROM tfdir
            WHERE pname_main = @<stack>-program AND include = @incl
            INTO @<stack>-eventname.
          IF sy-subrc = 0. <stack>-eventtype = 'FUNCTION'. CONTINUE. ENDIF.
        ENDIF.
        DATA: cl_key        TYPE seoclskey, meth_includes TYPE seop_methods_w_include.
        cl_key = <stack>-program.
        CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
          EXPORTING
            clskey                       = cl_key
          IMPORTING
            includes                     = meth_includes
          EXCEPTIONS
            _internal_class_not_existing = 1
            OTHERS                       = 2.
        IF sy-subrc = 0.
          READ TABLE meth_includes[] WITH KEY incname = <stack>-include INTO DATA(include).
          IF sy-subrc = 0.
            <stack>-eventtype = 'METHOD'. <stack>-eventname = include-cpdkey-cpdname.
          ENDIF.
        ENDIF.
        SPLIT <stack>-include AT '=' INTO TABLE split.
        CASE split[ lines( split ) ].
          WHEN 'CP'.    <stack>-eventtype = 'Class Pool'.
          WHEN 'CU'.    <stack>-eventtype = 'Public Section'.
          WHEN 'CI'.    <stack>-eventtype = 'Private Section'.
          WHEN 'CO'.    <stack>-eventtype = 'Protected Section'.
          WHEN 'IU'.    <stack>-eventtype = 'Interface Public Section'.
          WHEN 'CCAU'.  <stack>-eventtype = 'Unit Test Classes'.
          WHEN 'CCIMP'. <stack>-eventtype = 'Local helper classes'.
          WHEN 'CCDEF'. <stack>-eventtype = 'Local Definitions/Implementations'.
          WHEN 'CCMAC'. <stack>-eventtype = 'Macros'.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    SORT mt_coverage. DELETE ADJACENT DUPLICATES FROM mt_coverage.
  ENDMETHOD.
  METHOD apply_depth.
    CLEAR: mo_viewer->mt_steps, mo_viewer->m_step,
           mo_viewer->mo_window->mt_stack, mo_viewer->mo_window->mt_calls.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY selected = abap_true INTO DATA(source).
    IF sy-subrc <> 0 OR source-include = 'Code_Flow_Mix' OR source-include = 'VIRTUAL'.
      LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO source
        WHERE include <> 'Code_Flow_Mix' AND include <> 'VIRTUAL'. EXIT.
      ENDLOOP.
    ENDIF.
    CLEAR: mo_viewer->mt_steps, mo_viewer->m_step,
           mo_viewer->mo_window->mt_stack, mo_viewer->mo_window->mt_calls.
            DATA(ls_ctx) = mo_viewer->mo_window->ms_code_context.
        IF ls_ctx-evtype = 'EVENT'."IS NOT INITIAL.
          zcl_ace_source_parser=>code_execution_scanner(
            i_program = mo_viewer->mo_window->m_prg-program
            i_include = source-include io_debugger = mo_viewer
            i_evtype = ls_ctx-evtype
            i_evname = ls_ctx-evname ).

        ELSEIF ls_ctx-evtype IS NOT INITIAL.
          DATA(ls_sc) = mo_viewer->mo_window->ms_sel_call.
          zcl_ace_source_parser=>parse_call(
            i_index = ls_sc-index i_e_name = ls_ctx-evname i_e_type = ls_ctx-evtype
            i_class = ls_ctx-class i_program = CONV #( ls_sc-program )
            i_include = CONV #( ls_sc-include ) i_stack = 0 io_debugger = mo_viewer ).

        ELSE.
          zcl_ace_source_parser=>code_execution_scanner(
            i_program = mo_viewer->mo_window->m_prg-program
            i_include = source-include io_debugger = mo_viewer ).
        ENDIF.

*    DATA(ls_ctx) = mo_viewer->mo_window->ms_code_context.
*        IF ls_ctx-evtype IS NOT INITIAL.
*          DATA(ls_sc) = mo_viewer->mo_window->ms_sel_call.
*          zcl_ace_source_parser=>parse_call(
*            i_index = ls_sc-index i_e_name = ls_ctx-evname i_e_type = ls_ctx-evtype
*            i_class = ls_ctx-class i_program = CONV #( ls_sc-program )
*            i_include = CONV #( ls_sc-include ) i_stack = 0 io_debugger = mo_viewer ).
*
*        ELSE.
*          IF ls_ctx-evtype <> 'EVENT'.
*            zcl_ace_source_parser=>code_execution_scanner(
*              i_program = mo_viewer->mo_window->m_prg-program
*              i_include = mo_viewer->mo_window->m_prg-program io_debugger = mo_viewer
*              i_evtype = ls_ctx-evtype
*              i_evname = ls_ctx-evname ).
*          ELSE.
*            zcl_ace_source_parser=>code_execution_scanner(
*              i_program = mo_viewer->mo_window->m_prg-program
*              i_include = mo_viewer->mo_window->m_prg-program io_debugger = mo_viewer ).
*          ENDIF.
*        ENDIF.
    mo_viewer->mo_window->show_coverage( ).
    mo_viewer->mo_window->show_stack( ).
    IF mo_mermaid IS NOT INITIAL AND mo_mermaid->mo_box IS NOT INITIAL.
      mo_mermaid->refresh( ).
    ENDIF.
    mo_toolbar->set_button_info( EXPORTING fcode = 'DEPTH' text = |Depth { m_hist_depth }| ).
    IF mo_viewer->mo_window->m_prg-include = 'Code_Flow_Mix'.
      mo_viewer->get_code_mix( ).
      mo_viewer->mo_window->show_stack( ).
    ENDIF.
  ENDMETHOD.
  method SHOW_STACK.
      IF mo_salv_stack IS INITIAL.
        cl_salv_table=>factory(
          EXPORTING r_container = mo_tables_container
          IMPORTING r_salv_table = mo_salv_stack CHANGING t_table = mt_stack ).
        DATA: o_column TYPE REF TO cl_salv_column.
        DATA(o_columns) = mo_salv_stack->get_columns( ).
        o_column ?= o_columns->get_column( 'STEP' ).
        o_column->set_output_length( '3' ). o_column->set_short_text( 'STEP' ).
        o_column ?= o_columns->get_column( 'STACKLEVEL' ).
        o_column->set_output_length( '5' ).
        o_column ?= o_columns->get_column( 'PROGRAM' ).
        o_column->set_output_length( '20' ).
        o_column->set_long_text( 'Program/Class' ). o_column->set_medium_text( 'Program/Class' ).
        o_column ?= o_columns->get_column( 'EVENTTYPE' ).
        o_column->set_output_length( '10' ).
        o_column->set_long_text( 'Code TYPE' ). o_column->set_medium_text( 'Code TYPE' ).
        o_column ?= o_columns->get_column( 'INCLUDE' ).
        o_column->set_output_length( '40' ).
        o_column ?= o_columns->get_column( 'EVENTTYPE' ).
        o_column->set_output_length( '20' ).
        o_column ?= o_columns->get_column( 'EVENTNAME' ).
        o_column->set_output_length( '30' ).
        DATA(o_event) = mo_salv_stack->get_event( ).
        SET HANDLER on_stack_double_click FOR o_event.
        mo_salv_stack->display( ).
      ELSE.
        mo_salv_stack->refresh( ).
      ENDIF.
  endmethod.
  METHOD push_nav_entry.
    " Если мы не в конце истории — обрезаем "будущее"
    IF mv_nav_idx < lines( mt_nav_history ).
      DELETE mt_nav_history FROM mv_nav_idx + 1.
    ENDIF.
    " Не дублируем подряд одинаковые записи
    IF mt_nav_history IS NOT INITIAL.
      DATA(ls_last) = mt_nav_history[ lines( mt_nav_history ) ].
      IF ls_last-include = i_include AND ls_last-line = i_line.
        RETURN.
      ENDIF.
    ENDIF.
    APPEND VALUE ts_nav_entry( include = i_include line = i_line ) TO mt_nav_history.
    mv_nav_idx = lines( mt_nav_history ).
  ENDMETHOD.
  METHOD set_nav_caption.
    DATA lv_cap TYPE string.

    " Explicit unit info wins (passed on navigation to a known target)
    IF i_evname IS NOT INITIAL.
      CASE i_evtype.
        WHEN 'METHOD'.
          lv_cap = COND #( WHEN i_class IS NOT INITIAL
                           THEN |{ i_class }->{ i_evname }|
                           ELSE |{ i_evname }| ).
        WHEN 'FUNCTION'.
          lv_cap = |FM: { i_evname }|.
        WHEN 'FORM'.
          lv_cap = |FORM { i_evname }|.
        WHEN OTHERS.
          lv_cap = |{ i_evtype } { i_evname }|.
      ENDCASE.
    ELSE.
      " Derive from parsed data by include
      READ TABLE ms_sources-tt_calls_line WITH KEY include = i_include INTO DATA(ls_cl).
      IF sy-subrc = 0 AND ls_cl-eventname IS NOT INITIAL.
        CASE ls_cl-eventtype.
          WHEN 'METHOD'.
            lv_cap = |{ ls_cl-class }->{ ls_cl-eventname }|.
          WHEN 'FUNCTION'.
            lv_cap = |FM: { ls_cl-eventname }|.
          WHEN 'FORM'.
            lv_cap = |FORM { ls_cl-eventname }|.
          WHEN OTHERS.
            lv_cap = |{ ls_cl-eventtype } { ls_cl-eventname }|.
        ENDCASE.
      ENDIF.
    ENDIF.

    " Program - include suffix (only when they differ)
    READ TABLE ms_sources-tt_progs WITH KEY include = i_include INTO DATA(ls_pr).
    DATA(lv_prog) = COND string( WHEN sy-subrc = 0 AND ls_pr-program IS NOT INITIAL
                                 THEN ls_pr-program ELSE CONV #( m_prg-program ) ).
    DATA(lv_loc) = COND string(
      WHEN lv_prog IS NOT INITIAL AND lv_prog <> i_include
      THEN |{ lv_prog } - { i_include }|
      ELSE |{ i_include }| ).

    lv_cap = COND #( WHEN lv_cap IS INITIAL THEN lv_loc
                     ELSE |{ lv_cap }  [{ lv_loc }]| ).
    IF mo_box IS NOT INITIAL.
      mo_box->set_caption( lv_cap ).
    ENDIF.
  ENDMETHOD.
  METHOD navigate_external.
    " The call target is not part of the parsed sources (e.g. a standard
    " FM or class) — open it in the SAP workbench editor instead.
    DATA lv_obj_type TYPE seu_obj.
    DATA lv_obj_name TYPE string.

    CASE i_call-event.
      WHEN 'FUNCTION'.
        lv_obj_type = 'FUNC'.
        lv_obj_name = i_call-name.
      WHEN 'METHOD'.
        IF i_call-class IS INITIAL. RETURN. ENDIF.
        lv_obj_type = 'CLAS'.
        lv_obj_name = i_call-class.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    DATA lv_obj_name_c TYPE e071-obj_name.
    lv_obj_name_c = lv_obj_name.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = lv_obj_name_c
        object_type         = lv_obj_type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE |Cannot open { lv_obj_type } { lv_obj_name } (rc={ sy-subrc })| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS ZCL_ACE_TREE_BUILDER IMPLEMENTATION.
  METHOD constructor.
    mo_window = io_window.
    mo_tree   = io_tree.
  ENDMETHOD.
  METHOD build.
    DATA lt_splits_prg TYPE TABLE OF string.
    DATA lv_prog_str TYPE string.

    mo_tree->clear( ).
    SPLIT mo_window->m_prg-program AT '=' INTO TABLE lt_splits_prg.
    CHECK lt_splits_prg IS NOT INITIAL.

    cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).
    cl_gui_cfw=>dispatch( ).

    DATA(lv_root) = mo_tree->add_node(
      i_name = CONV #( mo_window->m_prg-program )
      i_icon = CONV #( icon_folder )
      i_tree = VALUE #( ) ).
    mo_tree->main_node_key = lv_root.

    DATA(lv_prog) = CONV prog( lt_splits_prg[ 1 ] ).
    lv_prog_str = lv_prog.

    show_tree_includes( i_root_key = lv_root i_prog = lv_prog ).
    show_tree_enhancements( i_root_key = lv_root ).
    show_tree_global_vars( i_root_key = lv_root ).
    show_tree_events( i_root_key = lv_root i_prog = lv_prog ).
    show_tree_function_modules( i_root_key = lv_root ).
    IF lines( lt_splits_prg ) = 1.
      show_tree_local_classes(
        i_root_key   = lv_root
        i_prog       = lv_prog
        i_excl_class = lv_prog_str ).
    ENDIF.
    SORT mo_window->ms_sources-tt_calls_line BY program class eventtype meth_type eventname.
    show_tree_class_hierarchy( i_root_key = lv_root i_cl_name = lv_prog_str ).
    show_tree_subroutines( i_root_key = lv_root i_prog = lv_prog ).
    show_tree_modules( i_root_key = lv_root i_prog = lv_prog ).

    mo_tree->display( ).
  ENDMETHOD.
  METHOD build_package.
    mo_tree->clear( ).
    cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).
    cl_gui_cfw=>dispatch( ).

    DATA(lv_root) = mo_tree->add_node(
      i_name = CONV #( i_package )
      i_icon = CONV #( icon_folder )
      i_tree = VALUE #( ) ).
    mo_tree->main_node_key = lv_root.

    DATA(lt_obj) = get_package_objects( i_package ).

    DATA lv_type_rel  TYPE salv_de_node_key.
    DATA lv_prev_type TYPE trobjtype.
    DATA lv_icon      TYPE salv_de_tree_image.

    LOOP AT lt_obj INTO DATA(ls_o).
      IF ls_o-obj_type <> lv_prev_type OR lv_type_rel IS INITIAL.
        lv_prev_type = ls_o-obj_type.
        lv_type_rel = mo_tree->add_node(
          i_name = SWITCH string( ls_o-obj_type
                     WHEN 'PROG' THEN 'Programs'
                     WHEN 'CLAS' THEN 'Classes'
                     WHEN 'INTF' THEN 'Interfaces'
                     WHEN 'FUGR' THEN 'Function Groups'
                     ELSE CONV string( ls_o-obj_type ) )
          i_icon = CONV #( icon_folder )
          i_rel  = lv_root
          i_tree = VALUE #( ) ).
      ENDIF.
      lv_icon = SWITCH salv_de_tree_image( ls_o-obj_type
                  WHEN 'PROG' THEN icon_project
                  WHEN 'CLAS' THEN icon_oo_class
                  WHEN 'INTF' THEN icon_oo_interface
                  ELSE icon_folder ).
      DATA(lv_node) = mo_tree->add_node(
        i_name = CONV #( ls_o-obj_name )
        i_icon = CONV #( lv_icon )
        i_rel  = lv_type_rel
        i_tree = VALUE #( kind = 'PKG' program = ls_o-prog param = |PKGOBJ:{ ls_o-prog }| ) ).
      APPEND lv_node TO mo_tree->mt_lazy_nodes.
    ENDLOOP.

    mo_tree->display( ).
  ENDMETHOD.
  METHOD build_object_subtree.
    DATA lt_splits_prg TYPE TABLE OF string.
    SPLIT i_program AT '=' INTO TABLE lt_splits_prg.
    CHECK lt_splits_prg IS NOT INITIAL.
    DATA(lv_prog)     = CONV prog( lt_splits_prg[ 1 ] ).
    DATA(lv_prog_str) = CONV string( lv_prog ).

    show_tree_includes( i_root_key = i_root_key i_prog = lv_prog ).
    show_tree_enhancements( i_root_key = i_root_key ).
    show_tree_global_vars( i_root_key = i_root_key ).
    show_tree_events( i_root_key = i_root_key i_prog = lv_prog ).
    show_tree_function_modules( i_root_key = i_root_key ).
    IF lines( lt_splits_prg ) = 1.
      show_tree_local_classes(
        i_root_key   = i_root_key
        i_prog       = lv_prog
        i_excl_class = lv_prog_str ).
    ENDIF.
    SORT mo_window->ms_sources-tt_calls_line BY program class eventtype meth_type eventname.
    show_tree_class_hierarchy( i_root_key = i_root_key i_cl_name = lv_prog_str ).
    show_tree_subroutines( i_root_key = i_root_key i_prog = lv_prog ).
    show_tree_modules( i_root_key = i_root_key i_prog = lv_prog ).
  ENDMETHOD.
  METHOD get_package_objects.
    DATA lt_pkg      TYPE STANDARD TABLE OF devclass.
    DATA lt_frontier TYPE STANDARD TABLE OF devclass.

    APPEND i_package TO lt_pkg.
    lt_frontier = lt_pkg.

    " Collect the package plus all sub-packages (recursive on tdevc-parentcl)
    WHILE lt_frontier IS NOT INITIAL.
      SELECT devclass FROM tdevc
        FOR ALL ENTRIES IN @lt_frontier
        WHERE parentcl = @lt_frontier-table_line
        INTO TABLE @DATA(lt_children).
      CLEAR lt_frontier.
      LOOP AT lt_children INTO DATA(ls_child).
        READ TABLE lt_pkg WITH KEY table_line = ls_child-devclass TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_child-devclass TO lt_pkg.
          APPEND ls_child-devclass TO lt_frontier.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

    SELECT object, obj_name FROM tadir
      FOR ALL ENTRIES IN @lt_pkg
      WHERE pgmid    = 'R3TR'
        AND devclass = @lt_pkg-table_line
        AND ( object = 'PROG' OR object = 'CLAS' OR object = 'INTF' OR object = 'FUGR' )
        AND delflag  = @space
      INTO TABLE @DATA(lt_tadir).
    TYPES: BEGIN OF lty_pkg_sort,
             sort_key TYPE i,
             obj_name TYPE sobj_name,
             obj      LIKE LINE OF lt_tadir,
           END OF lty_pkg_sort.
    DATA lt_sorted TYPE STANDARD TABLE OF lty_pkg_sort WITH DEFAULT KEY.

    LOOP AT lt_tadir INTO DATA(ls_sort_tadir).
      APPEND VALUE #(
        sort_key = SWITCH i( ls_sort_tadir-object
          WHEN 'PROG' THEN 1
          WHEN 'CLAS' THEN 2
          WHEN 'INTF' THEN 3
          WHEN 'FUGR' THEN 4
          ELSE 9 )
        obj_name = ls_sort_tadir-obj_name
        obj = ls_sort_tadir ) TO lt_sorted.
    ENDLOOP.
    SORT lt_sorted BY sort_key obj_name.

    LOOP AT lt_sorted INTO DATA(ls_sorted).
      DATA(ls_t) = ls_sorted-obj.
      DATA(ls_o)   = VALUE zif_ace_parse_data=>ts_pkg_obj( obj_type = ls_t-object obj_name = ls_t-obj_name ).
      DATA(lv_len) = strlen( ls_t-obj_name ).
      CASE ls_t-object.
        WHEN 'PROG'.
          ls_o-prog = ls_t-obj_name.
        WHEN 'CLAS'.
          ls_o-prog = ls_t-obj_name.
          IF lv_len < 30. ls_o-prog = ls_o-prog && repeat( val = `=` occ = 30 - lv_len ). ENDIF.
          ls_o-prog = ls_o-prog && 'CP'.
        WHEN 'INTF'.
          ls_o-prog = ls_t-obj_name.
          IF lv_len < 30. ls_o-prog = ls_o-prog && repeat( val = `=` occ = 30 - lv_len ). ENDIF.
          ls_o-prog = ls_o-prog && 'IP'.
        WHEN 'FUGR'.
          ls_o-prog = |SAPL{ ls_t-obj_name }|.
      ENDCASE.
      APPEND ls_o TO rt_obj.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_include_prefix.
    IF strlen( i_class ) >= 30.
      rv_prefix = i_class.
    ELSE.
      rv_prefix = i_class && repeat( val = `=` occ = 30 - strlen( i_class ) ) && `======`.
    ENDIF.
  ENDMETHOD.
  METHOD build_local_classes_node.
    DATA: test_rel  TYPE salv_de_node_key,
          intf_rel  TYPE salv_de_node_key,
          lv_prefix TYPE string,
          lv_ccau   TYPE string.
    lv_prefix = get_include_prefix( i_excl_class ).
    lv_ccau   = lv_prefix && 'CCAU'.

    LOOP AT mo_window->ms_sources-tt_class_defs INTO DATA(ls_cd)
      WHERE class <> i_excl_class
        AND program = i_program.

      IF ls_cd-def_include = lv_ccau.
        IF test_rel IS INITIAL.
          test_rel = mo_tree->add_node( i_name = 'Unit Test Classes' i_icon = CONV #( icon_folder )
            i_rel = i_refnode i_tree = VALUE #( ) ).
        ENDIF.
        add_class( i_class = ls_cd-class i_refnode = test_rel no_locals = abap_true i_type = 'T' ).

      ELSEIF ls_cd-is_intf = abap_true.
        IF intf_rel IS INITIAL.
          intf_rel = mo_tree->add_node( i_name = 'Interfaces' i_icon = CONV #( icon_oo_connection )
            i_rel = i_refnode i_tree = VALUE #( ) ).
        ENDIF.
        add_class( i_class = ls_cd-class i_refnode = intf_rel no_locals = abap_true i_type = 'I' ).

      ELSE.
        IF r_locals_rel IS INITIAL.
          r_locals_rel = mo_tree->add_node( i_name = 'Local Classes' i_icon = CONV #( icon_folder )
            i_rel = i_refnode i_tree = VALUE #( ) ).
        ENDIF.
        add_class( i_class = ls_cd-class i_refnode = r_locals_rel no_locals = abap_true ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD add_class.
    DATA: tree        TYPE zcl_ace=>ts_tree,
          splits_incl TYPE TABLE OF string,
          icon        TYPE salv_de_tree_image,
          class_rel   TYPE salv_de_node_key,
          include     TYPE string,
          prefix      TYPE string.

    IF i_type = 'I'.
      icon = icon_oo_connection.
    ELSEIF i_type = 'T'.
      icon = icon_test.
    ELSE.
      icon = icon_folder.
    ENDIF.

    LOOP AT mo_window->ms_sources-t_classes INTO DATA(ls_class) WHERE clsname = i_class AND reltype = '1'.
      IF class_rel IS INITIAL.
        class_rel = mo_tree->add_node( i_name = i_class i_icon = icon i_rel = i_refnode i_tree = i_tree ).
      ENDIF.
      add_class( i_class = CONV #( ls_class-refclsname ) i_refnode = class_rel no_locals = abap_true i_type = 'I' ).
    ENDLOOP.

    DATA lv_def_inc  TYPE program.
    DATA lv_def_line TYPE i.
    IF i_tree-kind = 'C'.
      lv_def_inc  = i_tree-include.
      lv_def_line = CONV i( i_tree-value ).
    ELSE.
      READ TABLE mo_window->ms_sources-tt_class_defs WITH KEY class = i_class INTO DATA(ls_cd).
      IF sy-subrc = 0.
        lv_def_inc  = ls_cd-def_include.
        lv_def_line = ls_cd-def_line.
      ENDIF.
    ENDIF.

    IF class_rel IS INITIAL.
      IF i_tree-kind = 'C'.
        class_rel = mo_tree->add_node( i_name = i_class i_icon = icon i_rel = i_refnode
                      i_tree = VALUE #( kind = 'C' include = lv_def_inc value = lv_def_line ) ).
      ELSE.
        class_rel = mo_tree->add_node( i_name = i_class i_icon = icon i_rel = i_refnode
                      i_tree = VALUE #( param = |CLASS:{ i_class }| include = lv_def_inc value = lv_def_line ) ).
      ENDIF.
    ENDIF.

    IF i_type = 'I'.
      DATA(lv_intf_var_cnt) = 0.
      LOOP AT mo_window->ms_sources-t_vars INTO DATA(lv_iv)
        WHERE class = i_class AND eventname IS INITIAL.
        lv_intf_var_cnt = lv_intf_var_cnt + 1.
      ENDLOOP.
      IF lv_intf_var_cnt > 0.
        DATA(lv_members_node) = mo_tree->add_node(
          i_name = |Members ({ lv_intf_var_cnt })|
          i_icon = CONV #( icon_header )
          i_rel  = class_rel
          i_tree = VALUE #( param = |INTF_VARS:{ i_class }| ) ).
        APPEND lv_members_node TO mo_tree->mt_lazy_nodes.
      ENDIF.
    ELSEIF i_type <> 'T'.
      DATA(lv_sec_labels) = VALUE string_table(
        ( `Public Section` ) ( `Protected Section` ) ( `Private Section` ) ).
      DATA(lv_sec_keys) = VALUE string_table(
        ( `PUBLIC` ) ( `PROTECTED` ) ( `PRIVATE` ) ).
      DATA(lv_si) = 0.
      LOOP AT lv_sec_keys INTO DATA(lv_sec_key).
        lv_si = lv_si + 1.
        READ TABLE mo_window->ms_sources-tt_sections WITH KEY class = i_class section = lv_sec_key INTO DATA(ls_sec).
        IF sy-subrc = 0.
          DATA(lv_sec_node) = mo_tree->add_node(
            i_name = lv_sec_labels[ lv_si ]
            i_icon = CONV #( icon_open_folder )
            i_rel  = class_rel
            i_tree = VALUE #( kind = 'M' include = ls_sec-include value = ls_sec-line
                              param = |SECT:{ i_class }:{ lv_sec_key }| ) ).
          READ TABLE mo_window->ms_sources-t_vars WITH KEY class = i_class section = lv_sec_key TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND lv_sec_node TO mo_tree->mt_lazy_nodes.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs) WHERE class = i_class AND eventtype = 'METHOD'.
      SPLIT subs-include AT '=' INTO TABLE splits_incl.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs-include INTO DATA(prog).
      READ TABLE prog-t_keywords WITH KEY index = subs-index INTO DATA(keyword).

      tree-kind = 'M'.
      IF subs-is_intf = abap_true AND subs-def_line > 0.
        tree-value = subs-def_line.
      ELSEIF keyword-v_line > 0.
        tree-value = keyword-v_line.
      ELSEIF keyword-line > 0.
        tree-value = keyword-line.
      ELSE.
        LOOP AT prog-t_keywords INTO DATA(lv_kw_fb) WHERE name = 'METHOD'.
          READ TABLE prog-scan->statements INDEX lv_kw_fb-index INTO DATA(ls_s_fb).
          IF sy-subrc = 0.
            READ TABLE prog-scan->tokens INDEX ls_s_fb-from + 1 INTO DATA(ls_t_fb).
            IF sy-subrc = 0 AND ls_t_fb-str = subs-eventname.
              tree-value = lv_kw_fb-line.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF tree-value IS INITIAL AND subs-def_line > 0.
        tree-value = subs-def_line.
      ENDIF.

      tree-include = subs-include.
      tree-program = subs-program.
      tree-ev_type = subs-eventtype.
      tree-ev_name = subs-eventname.
      CLEAR tree-param.

      IF i_type = 'I'.
        icon = icon_oo_inst_method.
      ELSEIF subs-redefined = abap_false.
        CASE subs-meth_type.
          WHEN 0 OR 1. icon = icon_led_green.
          WHEN 2.      icon = icon_led_yellow.
          WHEN 3.      icon = icon_led_red.
          WHEN OTHERS.
            IF subs-eventname = 'CONSTRUCTOR'.
              icon = icon_tools.
            ENDIF.
        ENDCASE.
      ELSE.
        icon = icon_oo_overwrite.
      ENDIF.

      DATA(event_node) = mo_tree->add_node( i_name = subs-eventname i_icon = icon i_rel = class_rel i_tree = tree ).
      APPEND event_node TO mo_tree->mt_lazy_nodes.

      LOOP AT mo_window->ms_sources-t_params INTO DATA(lv_p)
        WHERE class = subs-class AND event = 'METHOD' AND name = subs-eventname AND param IS NOT INITIAL.
        DATA(lv_p_icon) = COND salv_de_tree_image( WHEN lv_p-type = 'I' THEN CONV #( icon_parameter_import )
                                                   ELSE                      CONV #( icon_parameter_export ) ).
        mo_tree->add_node( i_name = lv_p-param i_icon = lv_p_icon i_rel = event_node
          i_tree = VALUE #( value = lv_p-line include = lv_p-include var_name = lv_p-param
                            ev_type = subs-eventtype ev_name = subs-eventname ) ).
      ENDLOOP.

      IF subs-include IS NOT INITIAL.
        READ TABLE mo_window->mt_calls WITH KEY include = subs-include ev_name = subs-eventname class = subs-class TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          zcl_ace_source_parser=>parse_call(
            i_index     = subs-index
            i_e_name    = subs-eventname
            i_e_type    = 'METHOD'
            i_class     = subs-class
            i_program   = subs-program
            i_include   = subs-include
            i_stack     = 0
            i_no_steps  = abap_true
            io_debugger = mo_window->mo_viewer ).
        ENDIF.
      ENDIF.
      DATA(lv_var_cnt) = 0.
      LOOP AT mo_window->ms_sources-t_vars INTO DATA(lv_v)
        WHERE program = subs-program AND class = subs-class
          AND eventtype = subs-eventtype AND eventname = subs-eventname.
        lv_var_cnt = lv_var_cnt + 1.
      ENDLOOP.
      IF lv_var_cnt > 0.
        DATA(lv_vars_node) = mo_tree->add_node(
          i_name = |Local vars ({ lv_var_cnt })| i_icon = CONV #( icon_folder ) i_rel = event_node
          i_tree = VALUE #( program = subs-program include = subs-include ev_type = 'VARS'
                            ev_name = subs-eventname param = |VARS:{ subs-class }:{ subs-eventname }| ) ).
        APPEND lv_vars_node TO mo_tree->mt_lazy_nodes.
      ENDIF.
    ENDLOOP.

    IF no_locals = abap_false.
      prefix = get_include_prefix( i_class ).
      include = prefix && 'CP'.
      build_local_classes_node( i_program = include i_excl_class = i_class i_refnode = class_rel ).
    ENDIF.

    r_node = class_rel.
  ENDMETHOD.
  METHOD show_tree_includes.
    " Scope includes to the current object's program (in package mode ms_sources
    " holds every parsed class, so an unfiltered list would show them all).
    DATA(lv_main_prog) = mo_window->m_prg-program.
    DATA lv_cnt TYPE i.
    LOOP AT mo_window->ms_sources-tt_progs TRANSPORTING NO FIELDS
      WHERE program = lv_main_prog AND include <> 'VIRTUAL' AND include <> lv_main_prog.
      lv_cnt = lv_cnt + 1.
    ENDLOOP.
    CHECK lv_cnt > 0.
    DATA(lv_node) = mo_tree->add_node(
      i_name = |Includes ({ lv_cnt })|
      i_icon = CONV #( icon_list )
      i_rel  = i_root_key
      i_tree = VALUE #( param = |INCLS:{ lv_main_prog }| program = lv_main_prog ) ).
    APPEND lv_node TO mo_tree->mt_lazy_nodes.
  ENDMETHOD.
  METHOD show_tree_enhancements.
    DATA lv_enh_rel TYPE salv_de_node_key.
    LOOP AT mo_window->ms_sources-tt_progs INTO DATA(prog_enh)
      WHERE program = mo_window->m_prg-program.
      IF prog_enh-enh_collected = abap_false.
        zcl_ace_source_parser=>collect_enhancements( i_program = prog_enh-include io_debugger = mo_window->mo_viewer ).
      ENDIF.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = prog_enh-include INTO prog_enh.
      LOOP AT prog_enh-tt_enh_blocks INTO DATA(enh_blk).
        IF lv_enh_rel IS INITIAL.
          lv_enh_rel = mo_tree->add_node( i_name = 'Enhancements' i_icon = CONV #( icon_folder )
            i_rel = i_root_key i_tree = VALUE #( ) ).
        ENDIF.
        mo_tree->add_node(
          i_name = |{ enh_blk-enh_name } { enh_blk-ev_type } { enh_blk-ev_name } ({ enh_blk-position })|
          i_icon = CONV #( icon_modify ) i_rel = lv_enh_rel
          i_tree = VALUE #( kind = 'M' value = enh_blk-position include = prog_enh-include
                            program = prog_enh-program ev_type = enh_blk-ev_type ev_name = enh_blk-ev_name
                            param = enh_blk-enh_include enh_id = enh_blk-enh_id ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD show_tree_global_vars.
    DATA lv_cnt TYPE i.
    LOOP AT mo_window->ms_sources-t_vars TRANSPORTING NO FIELDS
      WHERE program = mo_window->m_prg-program AND eventtype IS INITIAL AND class IS INITIAL.
      lv_cnt = lv_cnt + 1.
    ENDLOOP.
    CHECK lv_cnt > 0.
    DATA(lv_node) = mo_tree->add_node(
      i_name = |Global Vars ({ lv_cnt })| i_icon = CONV #( icon_header )
      i_rel  = i_root_key
      i_tree = VALUE #( param = 'GVARS:' program = mo_window->m_prg-program ) ).
    APPEND lv_node TO mo_tree->mt_lazy_nodes.
  ENDMETHOD.
  METHOD show_tree_events.
    DATA lv_events_rel TYPE salv_de_node_key.
    READ TABLE mo_window->mo_viewer->mt_steps INDEX 1 INTO DATA(first_step).
    IF first_step-line IS NOT INITIAL AND first_step-program = mo_window->m_prg-program.
      lv_events_rel = mo_tree->add_node( i_name = 'Events' i_icon = CONV #( icon_folder )
        i_rel = i_root_key i_tree = VALUE #( ) ).
      mo_tree->add_node( i_name = 'Code Flow start line' i_icon = CONV #( icon_oo_event )
        i_rel = lv_events_rel
        i_tree = VALUE #( kind = 'E' value = first_step-line include = first_step-include
                          program = mo_window->m_prg-program ev_type = 'EVENT'
                          ev_name = first_step-eventname ) ).
    ENDIF.
    LOOP AT mo_window->ms_sources-t_events INTO DATA(event) WHERE program = i_prog.
      IF lv_events_rel IS INITIAL.
        lv_events_rel = mo_tree->add_node( i_name = 'Events' i_icon = CONV #( icon_folder )
          i_rel = i_root_key i_tree = VALUE #( ) ).
      ENDIF.
      mo_tree->add_node( i_name = event-name i_icon = CONV #( icon_oo_event )
        i_rel = lv_events_rel
        i_tree = VALUE #( kind = 'E' include = event-include value = event-line
                          program = i_prog ev_type = 'EVENT' ev_name = event-name ) ).
    ENDLOOP.
  ENDMETHOD.
  METHOD show_tree_function_modules.
    DATA: fname              TYPE rs38l_fnam,
          exception_list     TYPE TABLE OF rsexc,
          export_parameter   TYPE TABLE OF rsexp,
          import_parameter   TYPE TABLE OF rsimp,
          changing_parameter TYPE TABLE OF rscha,
          tables_parameter   TYPE TABLE OF rstbl,
          incl_nr            TYPE includenr.
    DATA lv_fm_rel TYPE salv_de_node_key.
    LOOP AT mo_window->ms_sources-tt_progs INTO DATA(prog)
      WHERE program+0(4) = 'SAPL' AND program = mo_window->m_prg-program.
      DATA(len) = strlen( prog-include ) - 2.
      incl_nr = prog-include+len(2).
      SELECT SINGLE funcname FROM tfdir WHERE pname = @prog-program AND include = @incl_nr INTO @DATA(funcname).
      CHECK sy-subrc = 0.
      IF lv_fm_rel IS INITIAL.
        lv_fm_rel = mo_tree->add_node( i_name = 'Function Modules' i_icon = CONV #( icon_folder )
          i_rel = i_root_key i_tree = VALUE #( ) ).
      ENDIF.
      DATA(lv_func_rel) = mo_tree->add_node( i_name = CONV #( funcname ) i_icon = CONV #( icon_folder )
        i_rel = lv_fm_rel i_tree = VALUE #( ) ).
      fname = funcname.
      CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE' EXPORTING funcname = fname
        TABLES exception_list = exception_list export_parameter = export_parameter
               import_parameter = import_parameter changing_parameter = changing_parameter
               tables_parameter = tables_parameter EXCEPTIONS OTHERS = 4.
      IF sy-subrc = 0.
        LOOP AT import_parameter   INTO DATA(imp). mo_tree->add_node( i_name = CONV #( imp-parameter   ) i_icon = CONV #( icon_parameter_import   ) i_rel = lv_func_rel ). ENDLOOP.
        LOOP AT export_parameter   INTO DATA(exp). mo_tree->add_node( i_name = CONV #( exp-parameter   ) i_icon = CONV #( icon_parameter_export   ) i_rel = lv_func_rel ). ENDLOOP.
        LOOP AT mo_window->ms_sources-t_params INTO DATA(chg). mo_tree->add_node( i_name = CONV #( chg-param ) i_icon = CONV #( icon_parameter_changing ) i_rel = lv_func_rel ). ENDLOOP.
        LOOP AT tables_parameter   INTO DATA(tbl). mo_tree->add_node( i_name = CONV #( tbl-parameter   ) i_icon = CONV #( icon_parameter_table    ) i_rel = lv_func_rel ). ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD show_tree_local_classes.
    CONSTANTS c_lazy_threshold TYPE i VALUE 10.
    DATA lv_cls_cnt  TYPE i.
    DATA lv_intf_cnt TYPE i.
    LOOP AT mo_window->ms_sources-tt_class_defs INTO DATA(ls_cd)
      WHERE class <> i_excl_class AND program = i_prog.
      IF ls_cd-is_intf = abap_true.
        lv_intf_cnt = lv_intf_cnt + 1.
      ELSE.
        lv_cls_cnt = lv_cls_cnt + 1.
      ENDIF.
    ENDLOOP.
    CHECK lv_cls_cnt + lv_intf_cnt > 0.
    IF lv_cls_cnt + lv_intf_cnt <= c_lazy_threshold.
      build_local_classes_node( i_program = CONV string( i_prog ) i_excl_class = i_excl_class i_refnode = i_root_key ).
    ELSE.
      IF lv_cls_cnt > 0.
        DATA(lv_cls_node) = mo_tree->add_node( i_name = |Local Classes ({ lv_cls_cnt })| i_icon = CONV #( icon_folder )
          i_rel = i_root_key i_tree = VALUE #( param = |LCLASSES:{ i_prog }| program = i_prog ) ).
        APPEND lv_cls_node TO mo_tree->mt_lazy_nodes.
      ENDIF.
      IF lv_intf_cnt > 0.
        DATA(lv_int_node) = mo_tree->add_node( i_name = |Interfaces ({ lv_intf_cnt })| i_icon = CONV #( icon_oo_connection )
          i_rel = i_root_key i_tree = VALUE #( param = |LINTFS:{ i_prog }| program = i_prog ) ).
        APPEND lv_int_node TO mo_tree->mt_lazy_nodes.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD show_tree_class_hierarchy.
    READ TABLE mo_window->ms_sources-tt_class_defs WITH KEY class = i_cl_name TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.
    DATA(lv_cl) = i_cl_name.
    DO.
      READ TABLE mo_window->ms_sources-t_classes WITH KEY clsname = lv_cl reltype = '2' INTO DATA(ls_class).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      lv_cl = ls_class-refclsname.
    ENDDO.
    DO.
      READ TABLE mo_window->ms_sources-tt_class_defs WITH KEY class = lv_cl INTO DATA(ls_cd).
      add_class( i_class   = lv_cl
                 i_refnode = i_root_key
                 i_tree    = VALUE #( kind = 'C' include = ls_cd-def_include value = ls_cd-def_line ) ).
      READ TABLE mo_window->ms_sources-t_classes WITH KEY refclsname = lv_cl reltype = '2' INTO ls_class.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      lv_cl = ls_class-clsname.
    ENDDO.
  ENDMETHOD.
  METHOD show_tree_subroutines.
    CONSTANTS c_lazy_threshold TYPE i VALUE 10.
    DATA lv_icon      TYPE salv_de_tree_image.
    DATA splits_incl  TYPE TABLE OF string.
    DATA lv_form_cnt  TYPE i.
    LOOP AT mo_window->ms_sources-tt_calls_line TRANSPORTING NO FIELDS
      WHERE eventtype = 'FORM' AND program = i_prog.
      lv_form_cnt = lv_form_cnt + 1.
    ENDLOOP.
    CHECK lv_form_cnt > 0.
    IF lv_form_cnt > c_lazy_threshold.
      DATA(lv_lazy) = mo_tree->add_node( i_name = |Subroutines ({ lv_form_cnt })| i_icon = CONV #( icon_folder )
        i_rel = i_root_key i_tree = VALUE #( param = |FORMS:{ i_prog }| program = i_prog ) ).
      APPEND lv_lazy TO mo_tree->mt_lazy_nodes.
      RETURN.
    ENDIF.
    DATA lv_forms_rel TYPE salv_de_node_key.
    LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs) WHERE eventtype = 'FORM' AND program = i_prog.
      IF lv_forms_rel IS INITIAL.
        lv_forms_rel = mo_tree->add_node( i_name = 'Subroutines' i_icon = CONV #( icon_folder ) i_rel = i_root_key i_tree = VALUE #( ) ).
      ENDIF.
      SPLIT subs-include AT '=' INTO TABLE splits_incl.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs-include INTO DATA(prog).
      READ TABLE prog-t_keywords WITH KEY index = subs-index INTO DATA(keyword).
      DATA(lv_ev_node) = mo_tree->add_node( i_name = subs-eventname i_icon = CONV #( icon_biw_info_source_ina )
        i_rel = lv_forms_rel
        i_tree = VALUE #( kind = 'M' value = keyword-v_line include = subs-include
                          program = subs-program ev_type = subs-eventtype ev_name = subs-eventname ) ).
      LOOP AT mo_window->ms_sources-t_params INTO DATA(param)
        WHERE event = 'FORM' AND name = subs-eventname AND param IS NOT INITIAL.
        CASE param-type.
          WHEN 'I'. lv_icon = icon_parameter_import.
          WHEN 'E'. lv_icon = icon_parameter_export.
          WHEN OTHERS. lv_icon = icon_parameter_changing.
        ENDCASE.
        mo_tree->add_node( i_name = param-param i_icon = lv_icon i_rel = lv_ev_node
          i_tree = VALUE #( value = param-line include = param-include var_name = param-param
                            ev_type = subs-eventtype ev_name = subs-eventname ) ).
      ENDLOOP.
      DATA lv_var_cnt TYPE i.
      CLEAR lv_var_cnt.
      LOOP AT mo_window->ms_sources-t_vars TRANSPORTING NO FIELDS
        WHERE program = subs-program AND eventtype = 'FORM' AND eventname = subs-eventname.
        lv_var_cnt = lv_var_cnt + 1.
      ENDLOOP.
      IF lv_var_cnt > 0.
        DATA(lv_vars_node) = mo_tree->add_node( i_name = |Local vars ({ lv_var_cnt })| i_icon = CONV #( icon_header )
          i_rel = lv_ev_node
          i_tree = VALUE #( param = |LVARS:FORM:{ subs-eventname }| program = subs-program ) ).
        APPEND lv_vars_node TO mo_tree->mt_lazy_nodes.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD show_tree_modules.
    CONSTANTS c_lazy_threshold TYPE i VALUE 10.
    DATA splits_incl TYPE TABLE OF string.
    DATA lv_mod_cnt  TYPE i.
    LOOP AT mo_window->ms_sources-tt_calls_line TRANSPORTING NO FIELDS
      WHERE eventtype = 'MODULE' AND program = i_prog.
      lv_mod_cnt = lv_mod_cnt + 1.
    ENDLOOP.
    CHECK lv_mod_cnt > 0.
    IF lv_mod_cnt > c_lazy_threshold.
      DATA(lv_lazy) = mo_tree->add_node( i_name = |Modules ({ lv_mod_cnt })| i_icon = CONV #( icon_folder )
        i_rel = i_root_key i_tree = VALUE #( param = |MODS:{ i_prog }| program = i_prog ) ).
      APPEND lv_lazy TO mo_tree->mt_lazy_nodes.
      RETURN.
    ENDIF.
    DATA lv_mod_rel TYPE salv_de_node_key.
    LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs) WHERE eventtype = 'MODULE' AND program = i_prog.
      IF lv_mod_rel IS INITIAL.
        lv_mod_rel = mo_tree->add_node( i_name = 'Modules' i_icon = CONV #( icon_folder ) i_rel = i_root_key i_tree = VALUE #( ) ).
      ENDIF.
      SPLIT subs-include AT '=' INTO TABLE splits_incl.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs-include INTO DATA(prog).
      READ TABLE prog-t_keywords WITH KEY index = subs-index INTO DATA(keyword).
      mo_tree->add_node( i_name = subs-eventname i_icon = CONV #( icon_biw_info_source_ina )
        i_rel = lv_mod_rel
        i_tree = VALUE #( kind = 'M' value = keyword-v_line include = subs-include
                          program = subs-program ev_type = subs-eventtype ev_name = subs-eventname ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS ZCL_ACE_TEXT_VIEWER IMPLEMENTATION.
  method CONSTRUCTOR.

      super->constructor( ).
      mo_box = create( i_name = 'text' i_width = 700 i_hight = 200 ).
      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_box
          rows    = 1
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter->get_container(
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_variables_container ).

      SET HANDLER on_box_close FOR mo_box.

      CREATE OBJECT mo_text
        EXPORTING
          parent                 = mo_variables_container
        EXCEPTIONS
          error_cntl_create      = 1
          error_cntl_init        = 2
          error_cntl_link        = 3
          error_dp_create        = 4
          gui_type_not_supported = 5
          OTHERS                 = 6.
      IF sy-subrc <> 0.
        on_box_close( mo_box ).
      ENDIF.

      mo_text->set_readonly_mode( ).
      FIELD-SYMBOLS <str> TYPE string.
      ASSIGN ir_str->* TO <str>.
      DATA string TYPE TABLE OF char255.

      WHILE strlen( <str> ) > 255.
        APPEND <str>+0(255) TO string.
        SHIFT <str> LEFT BY 255 PLACES.
      ENDWHILE.

      APPEND <str> TO string.
      mo_text->set_text_as_r3table( string ).
      CALL METHOD cl_gui_cfw=>flush.
      mo_text->set_focus( mo_box ).
  endmethod.
ENDCLASS.

CLASS ZCL_ACE_TABLE_VIEWER IMPLEMENTATION.
  method BEFORE_USER_COMMAND.
      CASE e_ucomm.
        WHEN '&INFO'.
          DATA(l_url) = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.
          CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
      ENDCASE.
  endmethod.
  method CONSTRUCTOR.
      DATA: comp         TYPE abap_componentdescr,
            comp_notab   TYPE abap_component_tab,
            comp_tab2str TYPE abap_component_tab,
            comp_str     TYPE abap_component_tab,
            str          TYPE string,
            data         TYPE REF TO data.

      DATA: l_notab   TYPE REF TO data,
            l_tab2str TYPE REF TO data.

      DATA: handle_notab   TYPE REF TO cl_abap_structdescr,
            handle_tab2str TYPE REF TO cl_abap_structdescr,
            o_new_tab      TYPE REF TO cl_abap_tabledescr.

      FIELD-SYMBOLS: <notab>   TYPE STANDARD TABLE,
                     <tab2str> TYPE STANDARD TABLE,
                     <any_tab> TYPE ANY TABLE,
                     <temptab> TYPE ANY TABLE.

      super->constructor( i_additional_name = i_additional_name ).
      mo_window = io_window.
      m_lang = sy-langu.
      mo_sel_width = 0.
      m_tabname = i_tname.
      create_popup( ).

      IF ir_tab IS NOT BOUND.
        ZCL_ACE_RTTI=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table ).
      ELSE.
        FIELD-SYMBOLS:<any> TYPE any.
        ASSIGN ir_tab->* TO <any>.
        DATA o_tabl  TYPE REF TO cl_abap_tabledescr.
        DATA o_struc TYPE REF TO cl_abap_structdescr.
        o_tabl ?= cl_abap_typedescr=>describe_by_data( <any> ).
        TRY.
            o_struc ?= o_tabl->get_table_line_type( ).
            ASSIGN ir_tab->* TO <any_tab>.
            TRY.
                LOOP AT o_struc->components INTO DATA(component).

                  IF component-type_kind NE 'h'.
                    comp-name = component-name.
                    comp-type ?= o_struc->get_component_type( component-name ).
                    APPEND comp TO comp_notab.
                    APPEND comp TO comp_tab2str.
                  ELSE.
                    comp-name = component-name.
                    comp-type ?= cl_abap_typedescr=>describe_by_data(  str ).
                    APPEND comp TO comp_tab2str.
                    APPEND comp TO comp_str.

                    comp-name = component-name && '_REF'.
                    comp-type ?= cl_abap_typedescr=>describe_by_data(  data ).
                    APPEND comp TO comp_tab2str.
                  ENDIF.
                ENDLOOP.
              CATCH cx_sy_move_cast_error.
            ENDTRY.

            TRY.
                handle_notab  = cl_abap_structdescr=>create( comp_notab ).
                handle_tab2str  = cl_abap_structdescr=>create( comp_tab2str ).

                o_new_tab = cl_abap_tabledescr=>create(
                  p_line_type  = handle_notab
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  p_unique     = abap_false ).

                CREATE DATA l_notab TYPE HANDLE o_new_tab.

                o_new_tab = cl_abap_tabledescr=>create(
                  p_line_type  = handle_tab2str
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  p_unique     = abap_false ).

                CREATE DATA l_tab2str TYPE HANDLE o_new_tab.

                ASSIGN l_notab->* TO <notab>.
                MOVE-CORRESPONDING <any_tab> TO <notab>.
                ASSIGN l_tab2str->* TO <tab2str>.
                MOVE-CORRESPONDING <notab> TO <tab2str>.

                LOOP AT <any_tab> ASSIGNING FIELD-SYMBOL(<old_struc>).
                  READ TABLE <tab2str> ASSIGNING FIELD-SYMBOL(<new_struc>) INDEX sy-tabix.
                  LOOP AT comp_str INTO comp.
                    ASSIGN COMPONENT comp-name OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<field>).
                    ASSIGN COMPONENT comp-name OF STRUCTURE <old_struc> TO <temptab>.
                    <field> = | { icon_view_table } [{ lines( <temptab> ) }] |.
                    ASSIGN COMPONENT comp-name  OF STRUCTURE <old_struc> TO <field>.
                    ASSIGN COMPONENT |{ comp-name }_REF| OF STRUCTURE <new_struc> TO FIELD-SYMBOL(<ref>).
                    GET REFERENCE OF <field> INTO <ref>.
                  ENDLOOP.
                ENDLOOP.

                GET REFERENCE OF <tab2str> INTO mr_table.
              CATCH cx_root.
                mr_table = ir_tab.
            ENDTRY.
          CATCH cx_sy_move_cast_error.  "no structure
            comp-name = 'FIELD'.
            comp-type ?= cl_abap_typedescr=>describe_by_data( str ).
            APPEND comp TO comp_tab2str.

            handle_tab2str  = cl_abap_structdescr=>create( comp_tab2str ).
            o_new_tab = cl_abap_tabledescr=>create(
              p_line_type  = handle_tab2str
              p_table_kind = cl_abap_tabledescr=>tablekind_std
              p_unique     = abap_false ).

            CREATE DATA l_tab2str TYPE HANDLE o_new_tab.
            ASSIGN l_tab2str->* TO <tab2str>.
            ASSIGN ir_tab->* TO <any_tab>.

            LOOP AT <any_tab> ASSIGNING <old_struc>.
              APPEND INITIAL LINE TO <tab2str> ASSIGNING <new_struc>.
              ASSIGN COMPONENT 'FIELD' OF STRUCTURE <new_struc> TO <field>.
              <field> = <old_struc>.
            ENDLOOP.
            GET REFERENCE OF <tab2str> INTO mr_table.
        ENDTRY.
      ENDIF.

      create_alv( ).
      create_sel_alv( ).
      mo_alv->set_focus( mo_alv ).
  endmethod.
  method CREATE_ALV.
      DATA: layout TYPE lvc_s_layo,
            effect TYPE i,
            f4s    TYPE lvc_t_f4.

      FIELD-SYMBOLS: <f_tab>   TYPE table.

      mo_alv = NEW #( i_parent = mo_alv_parent ).
      mt_alv_catalog = create_field_cat( m_tabname ).

      IF mt_alv_catalog IS INITIAL.
        RETURN. "todo show tables without structure
      ENDIF.

      ASSIGN mr_table->* TO <f_tab>.
      set_header( ).
      layout-cwidth_opt = abap_true.
      layout-sel_mode = 'D'.

      SET HANDLER   before_user_command
                    handle_user_command
                    handle_tab_toolbar
                    FOR mo_alv.

      CALL METHOD mo_alv->set_table_for_first_display
        EXPORTING
          i_save          = abap_true
          i_default       = abap_true
          is_layout       = layout
        CHANGING
          it_fieldcatalog = mt_alv_catalog
          it_outtab       = <f_tab>.

      mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mt_alv_catalog ).
      LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<catalog>).
        CLEAR <catalog>-key.
        DATA(f4) = VALUE lvc_s_f4( register = abap_true chngeafter = abap_true fieldname = <catalog>-fieldname ).
        INSERT f4 INTO TABLE f4s.
      ENDLOOP.

      mo_alv->register_f4_for_fields( it_f4 = f4s ).
      mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).

      LOOP AT mt_alv_catalog ASSIGNING FIELD-SYMBOL(<cat>) WHERE scrtext_l IS INITIAL.
        ZCL_ACE_ALV_COMMON=>translate_field( CHANGING c_fld = <cat> ).
      ENDLOOP.

      mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).
      me->handle_user_command( EXPORTING e_ucomm = 'TECH' ).
      me->handle_user_command( EXPORTING e_ucomm = 'SHOW' ).
      mo_alv->set_toolbar_interactive( ).
  endmethod.
  method CREATE_FIELD_CAT.
      DATA: lr_field       TYPE REF TO data,
            lr_table_descr TYPE REF TO cl_abap_structdescr,
            lr_data_descr  TYPE REF TO cl_abap_datadescr,
            it_tabdescr    TYPE abap_compdescr_tab,
            l_texttab      TYPE tabname,
            lr_temp        TYPE REF TO data,
            l_name         TYPE string,
            l_dd04         TYPE dd04v.

      FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                     <struc> TYPE any,
                     <field> TYPE any.

      ASSIGN mr_table->* TO <tab>.
      CREATE DATA lr_temp LIKE LINE OF <tab>.
      ASSIGN lr_temp->* TO <struc>.

      TRY.
          lr_table_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_temp ).
        CATCH cx_root.
          RETURN.
      ENDTRY.

      it_tabdescr[] = lr_table_descr->components[].

      LOOP AT it_tabdescr INTO DATA(ls)
         WHERE type_kind NE 'h'
           AND type_kind NE 'l'.
        DATA(l_ind) = sy-tabix.

        ASSIGN COMPONENT ls-name OF STRUCTURE <struc> TO <field>.
        GET REFERENCE OF <field> INTO lr_field.
        lr_data_descr ?= cl_abap_typedescr=>describe_by_data_ref( lr_field ).
        l_name = lr_data_descr->absolute_name.
        REPLACE ALL OCCURRENCES OF '\TYPE=' IN l_name WITH ''.
        APPEND VALUE #( field = ls-name elem = l_name ) TO mt_fields.

        CLEAR l_dd04.
        CALL FUNCTION 'DDIF_DTEL_GET'
          EXPORTING
            name          = CONV ddobjname( l_name )
            langu         = m_lang
          IMPORTING
            dd04v_wa      = l_dd04
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.

        APPEND INITIAL LINE TO et_catalog ASSIGNING FIELD-SYMBOL(<catalog>).

        <catalog>-col_pos = l_ind.
        <catalog>-style = ZCL_ACE_ALV_COMMON=>c_white.
        <catalog>-fieldname = ls-name.
        <catalog>-f4availabl = abap_true.

        IF l_dd04 IS INITIAL.
          <catalog>-scrtext_s = <catalog>-scrtext_m = <catalog>-scrtext_l = <catalog>-reptext = <catalog>-fieldname = ls-name.
        ELSE.
          MOVE-CORRESPONDING l_dd04 TO <catalog>.
        ENDIF.
      ENDLOOP.
  endmethod.
  method CREATE_POPUP.
      mo_box = create( i_width = 800 i_hight = 150 ).

      "save new popup ref
      APPEND INITIAL LINE TO ZCL_ACE=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
      <popup>-parent = mo_window->mo_box.
      <popup>-child = mo_box.

      SET HANDLER on_box_close FOR mo_box.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_box
          rows    = 1
          columns = 2
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter->set_column_mode( mode = mo_splitter->mode_absolute ).
      mo_splitter->set_column_width( id = 1 width = mo_sel_width ).

      CALL METHOD:
       mo_splitter->get_container(  EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_sel_parent ),

        mo_splitter->get_container
         EXPORTING
          row       = 1
          column    = 2
         RECEIVING
          container = mo_alv_parent.

      SET HANDLER on_table_close FOR mo_box.
  endmethod.
  method CREATE_SEL_ALV.
      IF mo_sel IS INITIAL.
        mo_sel     = NEW #( io_viewer = me io_container = mo_sel_parent ).
        SET HANDLER refresh_table FOR mo_sel.
      ELSE.
        mo_sel->update_sel_tab( ).
      ENDIF.
  endmethod.
  method HANDLE_TAB_TOOLBAR.
      IF m_visible IS INITIAL.
        DATA(toolbar) = VALUE ttb_button(
         ( function = 'SEL_ON' icon = icon_arrow_left quickinfo = 'Show Select-Options'  butn_type = 0 )
         ( butn_type = 3 ) ).
      ENDIF.

      APPEND VALUE #( function = 'TECH' icon = icon_wd_caption quickinfo = 'Tech names'  butn_type = 0 ) TO toolbar.

      LOOP AT ZCL_ACE=>mt_lang INTO DATA(lang).
        IF sy-tabix > 10.
          EXIT.
        ENDIF.
        APPEND VALUE #( function = lang-spras icon = icon_foreign_trade quickinfo = lang-sptxt butn_type = 0 text = lang-sptxt ) TO toolbar.
      ENDLOOP.

      toolbar = VALUE ttb_button( BASE toolbar
       ( function = 'SHOW'  icon = icon_list  quickinfo = 'Show empty columns'   butn_type = 0  )
       ( function = 'TBAR' icon = COND #( WHEN m_std_tbar IS INITIAL THEN icon_column_right ELSE icon_column_left )
          quickinfo = COND #( WHEN m_std_tbar IS INITIAL THEN 'Show standard ALV function'  ELSE 'Hide standard ALV function') )
       ( butn_type = 3 ) ).

      IF m_std_tbar IS INITIAL.
        e_object->mt_toolbar =  toolbar.
      ELSE.
        e_object->mt_toolbar =  toolbar = VALUE ttb_button( BASE toolbar ( LINES OF e_object->mt_toolbar ) ).
      ENDIF.
  endmethod.
  method HANDLE_USER_COMMAND.
      DATA: it_fields  TYPE lvc_t_fcat,
            clause(45),
            sel_width  TYPE i.

      FIELD-SYMBOLS: <f_tab>  TYPE STANDARD  TABLE.
      ASSIGN mr_table->* TO <f_tab>.
      mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = it_fields[] ).
      IF e_ucomm = 'SEL_ON' AND m_visible IS INITIAL.
        create_sel_alv( ).
        m_visible = abap_true.
        IF mo_sel_width = 0.
          sel_width = 500.
        ELSE.
          sel_width = mo_sel_width.
        ENDIF.

        mo_splitter->set_column_width( EXPORTING id = 1 width =  sel_width ).
        mo_alv->set_toolbar_interactive( ).
        RETURN.
      ELSEIF e_ucomm = 'TBAR'.
        m_std_tbar = BIT-NOT  m_std_tbar.
      ELSE.
        IF e_ucomm = 'SHOW'.
          IF m_show_empty IS INITIAL.
            m_show_empty = 1.
          ELSE.
            CLEAR m_show_empty.
          ENDIF.
        ENDIF.

        LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<fields>) WHERE domname NE 'MANDT'.
          <fields>-col_pos = sy-tabix.
          CASE e_ucomm.

            WHEN 'SHOW'.
              IF m_show_empty = abap_false.
                <fields>-no_out = ' '.
              ELSE.
                clause = |{ <fields>-fieldname } IS NOT INITIAL|.
                LOOP AT <f_tab> ASSIGNING FIELD-SYMBOL(<f_line>)  WHERE (clause).
                  EXIT.
                ENDLOOP.
                IF sy-subrc NE 0.
                  <fields>-no_out = abap_true.
                ENDIF.
              ENDIF.

            WHEN 'TECH'. "technical field name
              <fields>-scrtext_l = <fields>-scrtext_m = <fields>-scrtext_s =  <fields>-reptext = <fields>-fieldname.

            WHEN OTHERS. "header names translation
              IF line_exists( ZCL_ACE=>mt_lang[ spras = e_ucomm ] ).
                translate_field( EXPORTING i_lang = CONV #( e_ucomm )  CHANGING c_fld = <fields> ).
                IF mo_sel IS BOUND.
                  READ TABLE mo_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) WITH KEY field_label = <fields>-fieldname.
                  IF sy-subrc = 0.
                    IF <fields>-scrtext_l IS NOT INITIAL.
                      <sel>-name = <fields>-scrtext_l.
                    ENDIF.
                    IF <sel>-name IS INITIAL.
                      IF <fields>-reptext IS NOT INITIAL.
                        <sel>-name = <fields>-reptext.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDIF.

      IF line_exists( ZCL_ACE=>mt_lang[ spras = e_ucomm ] ).
        m_lang = e_ucomm.
        set_header( ).
        mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang ).
      ENDIF.

      CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].

      ZCL_ACE_ALV_COMMON=>refresh( mo_alv ).
      IF mo_sel IS BOUND.
        IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
          mo_sel->update_sel_tab( ).
        ENDIF.
        ZCL_ACE_ALV_COMMON=>refresh( mo_sel->mo_sel_alv ).
        mo_sel->mo_sel_alv->refresh_table_display(  ).
      ENDIF.
  endmethod.
  method ON_TABLE_CLOSE.

      DATA:  tabix LIKE sy-tabix.
      CALL METHOD sender->free
        EXCEPTIONS
          cntl_error = 1
          OTHERS     = 2.

      "Free Memory
      LOOP AT ZCL_ACE=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
        IF <obj>-alv_viewer->mo_box = sender.
          tabix = sy-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF sy-subrc = 0.
        FREE <obj>-alv_viewer->mr_table.
        FREE <obj>-alv_viewer->mo_alv.

        FREE <obj>-alv_viewer.
        IF  tabix NE 0.
          DELETE ZCL_ACE=>mt_obj INDEX  tabix.
        ENDIF.
      ENDIF.
  endmethod.
  METHOD open_int_table.
    DATA r_tab TYPE REF TO data.
    IF it_ref IS BOUND. r_tab = it_ref. ELSE. GET REFERENCE OF it_tab INTO r_tab. ENDIF.
    APPEND INITIAL LINE TO zcl_ace=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW zcl_ace_table_viewer( i_additional_name = i_name ir_tab = r_tab io_window = io_window ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
  ENDMETHOD.
  method REFRESH_TABLE.
      DATA: row    TYPE ZCL_ACE=>t_sel_row,
            filter TYPE lvc_t_filt.

      CLEAR filter.
      set_header( ).

      LOOP AT mo_sel->mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
        LOOP AT <sel>-range INTO DATA(l_range).
          APPEND VALUE #( fieldname = <sel>-field_label
                                low = l_range-low
                               high = l_range-high
                               sign = l_range-sign
                             option = l_range-opti ) TO filter.
        ENDLOOP.
      ENDLOOP.

      IF mo_sel->mt_sel_tab IS NOT INITIAL.
        CALL METHOD mo_alv->set_filter_criteria
          EXPORTING
            it_filter = filter.
        ZCL_ACE_ALV_COMMON=>refresh( mo_sel->mo_sel_alv ).
        ZCL_ACE_ALV_COMMON=>refresh( mo_alv ).
        mo_sel->mo_viewer->handle_user_command( 'SHOW' ).

      ENDIF.

  endmethod.
  method SET_HEADER.
      DATA: text       TYPE as4text,
            header(80) TYPE c.

      SELECT SINGLE ddtext INTO  text
        FROM dd02t
       WHERE tabname = m_tabname
         AND ddlanguage = m_lang.

      header = |{ m_tabname } - {  text } { m_additional_name }|.
      mo_box->set_caption(  header ).
  endmethod.
  method TRANSLATE_FIELD.
      DATA: l_dd04 TYPE dd04v.

      READ TABLE mt_fields INTO DATA(l_field) WITH KEY field = c_fld-fieldname.
      CHECK l_field-elem IS NOT INITIAL.
      CLEAR l_dd04.

      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = CONV ddobjname( l_field-elem )
          langu         = i_lang
        IMPORTING
          dd04v_wa      = l_dd04
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.

      IF sy-subrc = 0.
        IF l_dd04-reptext IS NOT INITIAL.
          MOVE-CORRESPONDING l_dd04 TO c_fld.
        ENDIF.
      ENDIF.
  endmethod.
ENDCLASS.

CLASS zcl_ace_stmts IMPLEMENTATION.

  " ===== Control flow =====

  METHOD stmt_if.            " seq("IF", Cond)
    r = zcl_ace_combi=>seq( VALUE #( ( zcl_ace_combi=>str( `IF` ) )
                                     ( zcl_ace_combi=>expr( `COND` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_elseif.        " seq("ELSEIF", Cond)
    r = zcl_ace_combi=>seq( VALUE #( ( zcl_ace_combi=>str( `ELSEIF` ) )
                                     ( zcl_ace_combi=>expr( `COND` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_else.          " str("ELSE")
    r = zcl_ace_combi=>str( `ELSE` ).
  ENDMETHOD.

  METHOD stmt_endif.
    r = zcl_ace_combi=>str( `ENDIF` ).
  ENDMETHOD.

  METHOD stmt_case.          " seq("CASE", opt("TYPE OF"), Source)
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CASE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE OF` ) ) )
      ( zcl_ace_combi=>expr( `SOURCE` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_when.          " seq("WHEN", Source [OR Source]*)
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WHEN` ) )
      ( zcl_ace_combi=>expr( `SOURCE` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_when_others.
    r = zcl_ace_combi=>str( `WHEN OTHERS` ).
  ENDMETHOD.

  METHOD stmt_endcase.
    r = zcl_ace_combi=>str( `ENDCASE` ).
  ENDMETHOD.

  METHOD stmt_do.            " seq("DO", opt(Source "TIMES"), opt("VARYING" ...))
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DO` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIMES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VARYING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEXT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RANGE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_enddo.
    r = zcl_ace_combi=>str( `ENDDO` ).
  ENDMETHOD.

  METHOD stmt_while.         " seq("WHILE", Cond, opt("VARY" ...))
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WHILE` ) )
      ( zcl_ace_combi=>expr( `COND` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VARY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEXT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endwhile.
    r = zcl_ace_combi=>str( `ENDWHILE` ).
  ENDMETHOD.

  METHOD stmt_loop.
    " seq("LOOP", opt("AT" LoopSource), opt(LoopTarget), opt("WHERE" Cond),
    "      opt("USING KEY"), opt("FROM" Source), opt("TO" Source), opt("STEP" Source),
    "      opt("GROUP BY" ...))
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `LOOP` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `AT` ) )
          ( zcl_ace_combi=>expr( `LOOP_SOURCE` ) ) ) ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>expr( `LOOP_TARGET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `WHERE` ) )
          ( zcl_ace_combi=>expr( `COND` ) ) ) ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STEP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GROUP BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GROUP` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endloop.
    r = zcl_ace_combi=>str( `ENDLOOP` ).
  ENDMETHOD.

  METHOD stmt_continue.
    r = zcl_ace_combi=>str( `CONTINUE` ).
  ENDMETHOD.

  METHOD stmt_exit.
    r = zcl_ace_combi=>str( `EXIT` ).
  ENDMETHOD.

  METHOD stmt_check.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CHECK` ) )
      ( zcl_ace_combi=>expr( `COND` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_try.
    r = zcl_ace_combi=>str( `TRY` ).
  ENDMETHOD.

  METHOD stmt_endtry.
    r = zcl_ace_combi=>str( `ENDTRY` ).
  ENDMETHOD.

  METHOD stmt_catch.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CATCH` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEFORE UNWIND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_cleanup.
    r = zcl_ace_combi=>str( `CLEANUP` ).
  ENDMETHOD.

  METHOD stmt_raise.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RAISE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESUMABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEW` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EVENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHORTDUMP` ) ) ) ) ).
  ENDMETHOD.

  " ===== Data =====

  METHOD stmt_data.          " seq("DATA", DataDefinition, optPrio("%_PREDEFINED"))
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DATA` ) )
      ( zcl_ace_combi=>expr( `DATA_DEFINITION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_data_begin.    " "DATA: BEGIN OF [PUBLIC SECTION] name"
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DATA` ) )
      ( zcl_ace_combi=>str( `BEGIN OF` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMMON PART` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_data_end.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DATA` ) )
      ( zcl_ace_combi=>str( `END OF` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_constant.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONSTANTS` ) )
      ( zcl_ace_combi=>expr( `DATA_DEFINITION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_field_symbol.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FIELD-SYMBOLS` ) )
      ( zcl_ace_combi=>expr( `FIELD_SYMBOL` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STANDARD TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SORTED TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HASHED TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ANY TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX TABLE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_types.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `TYPES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REF TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STANDARD TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SORTED TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HASHED TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH UNIQUE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH NON-UNIQUE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH EMPTY KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH DEFAULT KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OCCURS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_clear.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLEAR` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH NULL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_move.
    " verNot(Cloud,"MOVE") seq("EXACT"? Source "TO" "?"? Target "%_LENGTH"?)
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MOVE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXACT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `?` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_compute.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `COMPUTE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXACT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_assign.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ASSIGN` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOCAL COPY OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL LINE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPONENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF STRUCTURE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCREMENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RANGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CASTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HANDLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DECIMALS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_unassign.
    r = zcl_ace_combi=>str( `UNASSIGN` ).
  ENDMETHOD.

  " ===== Internal tables =====

  METHOD stmt_append.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `APPEND` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STEP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SORTED BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASSIGNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CASTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_insert_internal.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INSERT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL LINE INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEFORE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AFTER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASSIGNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CASTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_modify_internal.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MODIFY` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TRANSPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_delete_internal.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DELETE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ADJACENT DUPLICATES FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPARING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_read_table.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `READ TABLE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH TABLE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPONENTS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BINARY SEARCH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TRANSPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASSIGNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CASTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ELSE UNASSIGN` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_sort.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SORT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASCENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DESCENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS TEXT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BYPASSING BUFFER` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_collect.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `COLLECT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASSIGNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE INTO` ) ) ) ) ).
  ENDMETHOD.

  " ===== OO =====

  METHOD stmt_class_def.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLASS` ) )
      ( zcl_ace_combi=>str( `DEFINITION` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PUBLIC` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INHERITING FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ABSTRACT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FINAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CREATE PUBLIC` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CREATE PROTECTED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CREATE PRIVATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED MEMORY ENABLED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR TESTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RISK LEVEL HARMLESS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RISK LEVEL DANGEROUS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RISK LEVEL CRITICAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DURATION SHORT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DURATION MEDIUM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DURATION LONG` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOAD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFERRED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOCAL FRIENDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GLOBAL FRIENDS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_class_impl.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLASS` ) )
      ( zcl_ace_combi=>str( `IMPLEMENTATION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endclass.
    r = zcl_ace_combi=>str( `ENDCLASS` ).
  ENDMETHOD.

  METHOD stmt_interface.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INTERFACE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PUBLIC` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOAD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFERRED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endinterface.
    r = zcl_ace_combi=>str( `ENDINTERFACE` ).
  ENDMETHOD.

  METHOD stmt_method_def.
    " Methods/Class-Methods declaration — many keywords, see method_def.ts
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>alt( VALUE #( ( zcl_ace_combi=>str( `METHODS` ) )
                                     ( zcl_ace_combi=>str( `CLASS-METHODS` ) ) ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FINAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ABSTRACT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFAULT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FAIL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RETURNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RAISING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR TESTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR EVENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REDEFINITION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AMDP OPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `READ-ONLY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CDS SESSION CLIENT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_method_impl.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `METHOD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY KERNEL MODULE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY DATABASE PROCEDURE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY DATABASE FUNCTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR HDB` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LANGUAGE SQLSCRIPT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `READ-ONLY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endmethod.
    r = zcl_ace_combi=>str( `ENDMETHOD` ).
  ENDMETHOD.

  METHOD stmt_create_object.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CREATE OBJECT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AREA HANDLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_method.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL METHOD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RECEIVING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTIONS` ) ) ) ) ).
  ENDMETHOD.

  " ===== Subroutines / function modules =====

  METHOD stmt_form.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FORM` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RAISING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endform.
    r = zcl_ace_combi=>str( `ENDFORM` ).
  ENDMETHOD.

  METHOD stmt_perform.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PERFORM` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON COMMIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON ROLLBACK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEVEL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IF FOUND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_function.
    r = zcl_ace_combi=>str( `FUNCTION` ).
  ENDMETHOD.

  METHOD stmt_endfunction.
    r = zcl_ace_combi=>str( `ENDFUNCTION` ).
  ENDMETHOD.

  METHOD stmt_call_function.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL FUNCTION` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN UPDATE TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BACKGROUND TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BACKGROUND UNIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS SEPARATE UNIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `KEEPING LOGICAL UNIT OF WORK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING NEW TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CALLING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON END OF TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DESTINATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PARAMETER-TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTION-TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OTHERS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_module.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MODULE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OUTPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT EXIT-COMMAND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endmodule.
    r = zcl_ace_combi=>str( `ENDMODULE` ).
  ENDMETHOD.

  " ===== SQL =====

  METHOD stmt_select.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SELECT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SINGLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR UPDATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DISTINCT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO CORRESPONDING FIELDS OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `APPENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `APPENDING TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GROUP BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HAVING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ORDER BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UP TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ROWS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PACKAGE SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BYPASSING BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INNER JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT OUTER JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RIGHT OUTER JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CROSS JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UNION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTERSECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASCENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DESCENDING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endselect.
    r = zcl_ace_combi=>str( `ENDSELECT` ).
  ENDMETHOD.

  METHOD stmt_insert_db.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INSERT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCEPTING DUPLICATE KEYS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_update_db.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `UPDATE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDICATORS SET STRUCTURE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_delete_db.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DELETE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_modify_db.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MODIFY` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_commit.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `COMMIT WORK` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND WAIT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_rollback.
    r = zcl_ace_combi=>str( `ROLLBACK WORK` ).
  ENDMETHOD.

  " ===== Reporting =====

  METHOD stmt_report.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `REPORT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO STANDARD PAGE HEADING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGE-ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFINING DATABASE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_write.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WRITE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING EDIT MASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING NO EDIT MASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-ZERO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-SIGN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT-JUSTIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RIGHT-JUSTIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CENTERED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UNDER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-GAP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COLOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTENSIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INVERSE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HOTSPOT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `QUICKINFO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS CHECKBOX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS SYMBOL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS ICON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS LINE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_message.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MESSAGE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NUMBER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DISPLAY LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RAISING` ) ) ) ) ).
  ENDMETHOD.

  " ===== Misc =====

  METHOD stmt_assert.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ASSERT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBKEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONDITION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_return.
    r = zcl_ace_combi=>str( `RETURN` ).
  ENDMETHOD.

  METHOD stmt_include.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INCLUDE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IF FOUND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STRUCTURE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_export.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `EXPORT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATABASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATA BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTERNAL TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPRESSION ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPRESSION OFF` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_import.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `IMPORT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATABASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATA BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTERNAL TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CONVERSION ERRORS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING STRUCTURE BOUNDARIES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT CHARACTER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCEPTING PADDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCEPTING TRUNCATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHAR-TO-HEX MODE` ) ) ) ) ).
  ENDMETHOD.

  " ===== Selection screen / events =====

  METHOD stmt_select_options.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SELECT-OPTIONS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFAULT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OPTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SIGN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCHCODE OBJECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODIF ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOWER CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OBLIGATORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-DISPLAY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO INTERVALS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-EXTENSION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VISIBLE LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE-REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USER-COMMAND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_parameters.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PARAMETERS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DECIMALS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFAULT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCHCODE OBJECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODIF ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOWER CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OBLIGATORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-DISPLAY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VISIBLE LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS CHECKBOX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RADIOBUTTON GROUP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS LISTBOX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE CHECK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE-REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USER-COMMAND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_parameter.
    r = zcl_ace_combi=>str( `PARAMETER` ).
  ENDMETHOD.

  METHOD stmt_ranges.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RANGES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_tables.
    r = zcl_ace_combi=>str( `TABLES` ).
  ENDMETHOD.

  METHOD stmt_selection_screen.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SELECTION-SCREEN` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF TABBED BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF TABBED BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH FRAME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TITLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO INTERVALS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS WINDOW` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS SUBSCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDE BLOCKS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDE PARAMETERS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDE SELECT-OPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SKIP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ULINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMMENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `POSITION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PUSHBUTTON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FUNCTION KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DYNAMIC SELECTIONS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_at_selection.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `AT SELECTION-SCREEN` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OUTPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RADIOBUTTON GROUP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE-REQUEST FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HELP-REQUEST FOR` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_at_user_command.
    r = zcl_ace_combi=>str( `AT USER-COMMAND` ).
  ENDMETHOD.

  METHOD stmt_at_pf.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `AT PF` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_at_line_sel.
    r = zcl_ace_combi=>str( `AT LINE-SELECTION` ).
  ENDMETHOD.

  METHOD stmt_initialization.
    r = zcl_ace_combi=>str( `INITIALIZATION` ).
  ENDMETHOD.

  METHOD stmt_start_of_sel.
    r = zcl_ace_combi=>str( `START-OF-SELECTION` ).
  ENDMETHOD.

  METHOD stmt_end_of_sel.
    r = zcl_ace_combi=>str( `END-OF-SELECTION` ).
  ENDMETHOD.

  METHOD stmt_top_of_page.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `TOP-OF-PAGE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DURING LINE-SELECTION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_end_of_page.
    r = zcl_ace_combi=>str( `END-OF-PAGE` ).
  ENDMETHOD.

  METHOD stmt_load_of_program.
    r = zcl_ace_combi=>str( `LOAD-OF-PROGRAM` ).
  ENDMETHOD.

  METHOD stmt_at_first.
    r = zcl_ace_combi=>str( `AT FIRST` ).
  ENDMETHOD.

  METHOD stmt_at_last.
    r = zcl_ace_combi=>str( `AT LAST` ).
  ENDMETHOD.

  METHOD stmt_at.
    " AT NEW <field> / AT END OF <field>
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `AT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEW` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endat.
    r = zcl_ace_combi=>str( `ENDAT` ).
  ENDMETHOD.

  " ===== String operations =====

  METHOD stmt_concatenate.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONCATENATE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SEPARATED BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESPECTING BLANKS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_split.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SPLIT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_replace.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `REPLACE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIRST OCCURRENCE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL OCCURRENCES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OCCURRENCES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBSTRING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REGEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PCRE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECTION OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECTION LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESPECTING CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESULTS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_find.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FIND` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIRST OCCURRENCE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL OCCURRENCES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBSTRING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REGEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PCRE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECTION OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECTION LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESPECTING CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCH COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCH OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCH LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCH LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESULTS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBMATCHES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_translate.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `TRANSLATE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO UPPER CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LOWER CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM CODE PAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO CODE PAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM NUMBER FORMAT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO NUMBER FORMAT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_condense.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONDENSE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-GAPS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_overlay.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `OVERLAY` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ONLY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_shift.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SHIFT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RIGHT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CIRCULAR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PLACES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DELETING LEADING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DELETING TRAILING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UP TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_search.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SEARCH` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ABBREVIATED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENDING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND MARK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  " ===== System / control =====

  METHOD stmt_describe.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DESCRIBE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DISTANCE BETWEEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OCCURS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `KIND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPONENTS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OUTPUT-LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DECIMALS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EDIT MASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HELP-ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PAGES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT MARGIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TOP LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TITLE LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HEAD LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIRST-LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DISTANCE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_free.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FREE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OBJECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_refresh.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `REFRESH` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONTROL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM SCREEN` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_wait.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WAIT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UP TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECONDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UNTIL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR ASYNCHRONOUS TASKS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR PUSH CHANNELS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR MESSAGING CHANNELS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_stop.
    r = zcl_ace_combi=>str( `STOP` ).
  ENDMETHOD.

  METHOD stmt_submit.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SUBMIT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VIA SELECTION-SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VIA JOB` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NUMBER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING LIST TO MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING SELECTION-SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING SELECTION-SET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING SELECTION-SETS OF PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH FREE SELECTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH SELECTION-TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND RETURN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO SAP-SPOOL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SPOOL PARAMETERS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ARCHIVE PARAMETERS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITHOUT SPOOL DYNPRO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_leave.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `LEAVE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO TRANSACTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND SKIP FIRST SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LIST-PROCESSING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND RETURN TO SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIST-PROCESSING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO CURRENT TRANSACTION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_generate.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `GENERATE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBROUTINE POOL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPORT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DYNPRO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NAME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WORD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TRACE-FILE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_authority_check.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `AUTHORITY-CHECK` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OBJECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR USER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DUMMY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_break_point.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `BREAK-POINT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_break.
    r = zcl_ace_combi=>str( `BREAK` ).
  ENDMETHOD.

  METHOD stmt_log_point.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `LOG-POINT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBKEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELDS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_set.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SET` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOCALE LANGUAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COUNTRY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODIFIER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BLANK LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COUNTRY-SPECIFIC CONVERSIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CURSOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXTENDED CHECK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HANDLER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACTIVATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HOLD DATA` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LANGUAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT SCROLL-BOUNDARY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MARGIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PARAMETER ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PF-STATUS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCLUDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMMEDIATELY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PROPERTY OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RUN TIME ANALYZER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RUN TIME CLOCK RESOLUTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TITLEBAR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UPDATE TASK LOCAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USER-COMMAND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_get.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `GET` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CURSOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PARAMETER ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PF-STATUS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCLUDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PROPERTY OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RUN TIME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD VALUE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BADI` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOCALE LANGUAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COUNTRY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODIFIER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STAMP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENDING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LATE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_convert.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONVERT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STAMP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INVERTED-DATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIME STAMP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIME ZONE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DAYLIGHT SAVING TIME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TEXT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO SORTABLE CODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_pack.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PACK` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_unpack.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `UNPACK` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) ) ) ).
  ENDMETHOD.

  " ===== File I/O =====

  METHOD stmt_open_dataset.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `OPEN DATASET` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR OUTPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR APPENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR UPDATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BINARY MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN TEXT MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN LEGACY BINARY MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN LEGACY TEXT MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BIG ENDIAN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LITTLE ENDIAN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NATIVE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENCODING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFAULT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UTF-8` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NON-UNICODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH SMART LINEFEED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH NATIVE LINEFEED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH UNIX LINEFEED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH WINDOWS LINEFEED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CODE PAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CONVERSION ERRORS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT CHARACTER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CONVERSION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FILTER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT POSITION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_close_dataset.
    r = zcl_ace_combi=>str( `CLOSE DATASET` ).
  ENDMETHOD.

  METHOD stmt_read_dataset.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `READ DATASET` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MAXIMUM LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACTUAL LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_transfer.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `TRANSFER` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO END OF LINE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_delete_dataset.
    r = zcl_ace_combi=>str( `DELETE DATASET` ).
  ENDMETHOD.

  " ===== More OO =====

  METHOD stmt_aliases.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ALIASES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_interfaces.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INTERFACES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ABSTRACT METHODS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FINAL METHODS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATA VALUES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL METHODS ABSTRACT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL METHODS FINAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PARTIALLY IMPLEMENTED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_class_data.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLASS-DATA` ) )
      ( zcl_ace_combi=>expr( `DATA_DEFINITION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_class_events.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLASS-EVENTS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_events.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `EVENTS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_raise_event.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RAISE EVENT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_create_data.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CREATE DATA` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HANDLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AREA HANDLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STANDARD TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SORTED TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HASHED TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ANY TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH UNIQUE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH NON-UNIQUE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH EMPTY KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REF TO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_catch_sys_excs.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CATCH SYSTEM-EXCEPTIONS` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_case_type.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CASE TYPE OF` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_when_type.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WHEN TYPE` ) ) ) ).
  ENDMETHOD.

  " ===== More reporting =====

  METHOD stmt_format.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FORMAT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COLOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTENSIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INVERSE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HOTSPOT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FRAMES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFF` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_new_line.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `NEW-LINE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-SCROLLING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SCROLLING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_new_page.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `NEW-PAGE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-TITLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH-TITLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-HEADING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH-HEADING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PRINT ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PRINT OFF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO DIALOG` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEW-SECTION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_skip.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SKIP` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LINE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_uline.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ULINE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_position.
    r = zcl_ace_combi=>str( `POSITION` ).
  ENDMETHOD.

  METHOD stmt_print_control.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PRINT-CONTROL` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `POSITION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FUNCTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANNEL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FONT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CPI` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LPI` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COLOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WIDTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HEIGHT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_reserve.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RESERVE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_back.
    r = zcl_ace_combi=>str( `BACK` ).
  ENDMETHOD.

  METHOD stmt_suppress.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SUPPRESS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DIALOG` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_extract.
    r = zcl_ace_combi=>str( `EXTRACT` ).
  ENDMETHOD.

  METHOD stmt_field_groups.
    r = zcl_ace_combi=>str( `FIELD-GROUPS` ).
  ENDMETHOD.

  " ===== Math / arithmetic =====

  METHOD stmt_add.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ADD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `THEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UNTIL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GIVING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCORDING TO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_subtract.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SUBTRACT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_multiply.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MULTIPLY` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_divide.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DIVIDE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_move_corres.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MOVE-CORRESPONDING` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `KEEPING TARGET LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPANDING NESTED TABLES` ) ) ) ) ).
  ENDMETHOD.

  " ===== Macro / definition =====

  METHOD stmt_define.
    r = zcl_ace_combi=>str( `DEFINE` ).
  ENDMETHOD.

  METHOD stmt_end_of_def.
    r = zcl_ace_combi=>str( `END-OF-DEFINITION` ).
  ENDMETHOD.

  " ===== Misc more =====

  METHOD stmt_provide.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PROVIDE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BETWEEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endprovide.
    r = zcl_ace_combi=>str( `ENDPROVIDE` ).
  ENDMETHOD.

  METHOD stmt_on_change.
    r = zcl_ace_combi=>str( `ON CHANGE OF` ).
  ENDMETHOD.

  METHOD stmt_endon.
    r = zcl_ace_combi=>str( `ENDON` ).
  ENDMETHOD.

  METHOD stmt_chain.
    r = zcl_ace_combi=>str( `CHAIN` ).
  ENDMETHOD.

  METHOD stmt_endchain.
    r = zcl_ace_combi=>str( `ENDCHAIN` ).
  ENDMETHOD.

  METHOD stmt_resume.
    r = zcl_ace_combi=>str( `RESUME` ).
  ENDMETHOD.

  METHOD stmt_retry.
    r = zcl_ace_combi=>str( `RETRY` ).
  ENDMETHOD.

  METHOD stmt_function_pool.
    r = zcl_ace_combi=>str( `FUNCTION-POOL` ).
  ENDMETHOD.

  METHOD stmt_program.
    r = zcl_ace_combi=>str( `PROGRAM` ).
  ENDMETHOD.

  METHOD stmt_type_pool.
    r = zcl_ace_combi=>str( `TYPE-POOL` ).
  ENDMETHOD.

  METHOD stmt_type_pools.
    r = zcl_ace_combi=>str( `TYPE-POOLS` ).
  ENDMETHOD.

  METHOD stmt_infotypes.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INFOTYPES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NAME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALID` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_controls.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONTROLS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLEVIEW` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABSTRIP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CUSTOM CONTROL` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_statics.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `STATICS` ) )
      ( zcl_ace_combi=>expr( `DATA_DEFINITION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_screen.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL SCREEN` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENDING AT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_transaction.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL TRANSACTION` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND SKIP FIRST SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH AUTHORITY-CHECK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITHOUT AUTHORITY-CHECK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UPDATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGES INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OPTIONS FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH PARAMETER` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_dialog.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL DIALOG` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_set_pf_status.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SET PF-STATUS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCLUDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMMEDIATELY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_set_titlebar.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SET TITLEBAR` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_window.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WINDOW` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENDING AT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_loop_dynpro.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `LOOP` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH CONTROL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CURSOR` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_process.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PROCESS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEFORE OUTPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AFTER INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON HELP-REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON VALUE-REQUEST` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_field.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FIELD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODULE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON CHAIN-INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON CHAIN-REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SELECT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_receive.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RECEIVE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESULTS FROM FUNCTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `KEEPING TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTIONS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_communication.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `COMMUNICATION` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALLOCATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCEPT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SEND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RECEIVE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEALLOCATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DESTINATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_set_handler.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SET HANDLER` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACTIVATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR ALL INSTANCES` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_get_reference.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `GET REFERENCE OF` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_badi.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL BADI` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RECEIVING` ) ) ) ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ZCL_ACE_SOURCE_PARSER IMPLEMENTATION.
  METHOD is_custom_code.
    DATA(lv_name) = CONV string( i_name ).
    IF lv_name IS INITIAL.
      RETURN.
    ENDIF.
    " Z*/Y* customer objects, or any object in a customer namespace (/NS/...)
    IF lv_name(1) = 'Z' OR lv_name(1) = 'Y' OR lv_name(1) = '/'.
      rv_custom = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD resolve_context.
    e_evtype = i_evtype.
    e_evname = i_evname.
    IF i_evtype IS NOT INITIAL AND i_evname IS NOT INITIAL.
      READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
        WITH KEY include = i_include eventtype = i_evtype eventname = i_evname
        INTO DATA(ls_cl).
      IF sy-subrc = 0. e_class = ls_cl-class. RETURN. ENDIF.
      FIND '=' IN i_include.
      IF sy-subrc = 0.
        DATA(lv_splits) = VALUE string_table( ).
        SPLIT i_include AT '=' INTO TABLE lv_splits.
        e_class = lv_splits[ 1 ].
      ENDIF.
      RETURN.
    ENDIF.
    FIND '=' IN i_include.
    IF sy-subrc = 0.
      DATA(lv_sp) = VALUE string_table( ).
      SPLIT i_include AT '=' INTO TABLE lv_sp.
      e_class = lv_sp[ 1 ].
      READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
        WITH KEY include = i_include eventtype = 'METHOD'
        INTO ls_cl.
      IF sy-subrc = 0. e_evtype = 'METHOD'. e_evname = ls_cl-eventname. ENDIF.
      RETURN.
    ENDIF.
    READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
      WITH KEY include = i_include INTO ls_cl.
    IF sy-subrc = 0.
      e_class  = ls_cl-class.
      e_evtype = ls_cl-eventtype.
      e_evname = ls_cl-eventname.
    ENDIF.
  ENDMETHOD.
  METHOD code_execution_scanner.

    DATA: max       TYPE i,
          call_line TYPE zcl_ace=>ts_calls_line,
          program   TYPE program,
          include   TYPE program,
          prefix    TYPE string,
          event     TYPE string,
          stack     TYPE i,
          statement TYPE i,
          prog      TYPE zif_ace_parse_data=>ts_prog.

    SORT io_debugger->mo_window->ms_sources-tt_calls_line.
    stack = i_stack + 1.
    CHECK stack <= io_debugger->mo_window->m_hist_depth.

    zcl_ace_parser=>parse(
      EXPORTING i_program = i_program i_include = i_include i_run = 1
      CHANGING  cs_source = io_debugger->mo_window->ms_sources ).

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs
      WITH KEY include = i_include ASSIGNING FIELD-SYMBOL(<prog>).
    IF sy-subrc <> 0. RETURN. ENDIF.

    DATA: structures LIKE <prog>-scan->structures.

    " If a specific event name is requested, find only that event's structure.
    " Otherwise fall through to the general logic (all events or program body).
    IF i_evname IS NOT INITIAL AND i_evtype = 'EVENT'.

      " Locate the event in t_events by name
      READ TABLE io_debugger->mo_window->ms_sources-t_events
        WITH KEY program = i_program name = i_evname
        INTO DATA(ls_sel_event).
      IF sy-subrc <> 0.
        " Try matching by stmnt_type / stmnt_from via structures
        LOOP AT <prog>-scan->structures INTO DATA(str_ev)
          WHERE type = 'E' AND ( stmnt_type = '1' OR stmnt_type = '2' OR stmnt_type = '3' ).
          READ TABLE io_debugger->mo_window->ms_sources-t_events
            WITH KEY program = i_program stmnt_type = str_ev-stmnt_type stmnt_from = str_ev-stmnt_from
            INTO ls_sel_event.
          IF sy-subrc = 0 AND ls_sel_event-name = i_evname. EXIT. ENDIF.
          CLEAR ls_sel_event.
        ENDLOOP.
      ENDIF.

      IF ls_sel_event-stmnt_from > 0.
        " Find the matching structure for this event
        LOOP AT <prog>-scan->structures INTO DATA(str_match)
          WHERE type = 'E'
            AND stmnt_type = ls_sel_event-stmnt_type
            AND stmnt_from = ls_sel_event-stmnt_from.
          APPEND str_match TO structures.
          EXIT.
        ENDLOOP.
      ENDIF.

    ELSE.

      LOOP AT <prog>-scan->structures INTO DATA(structure)
        WHERE type = 'E' AND ( stmnt_type = '1' OR stmnt_type = '2' OR stmnt_type = '3' ).
      ENDLOOP.

      IF sy-subrc = 0.
        structures = <prog>-scan->structures.
        DELETE structures WHERE type <> 'E'.
        LOOP AT structures ASSIGNING FIELD-SYMBOL(<structure>) WHERE stmnt_type = 'g'.
          CLEAR <structure>-stmnt_type.
        ENDLOOP.
        SORT structures BY stmnt_type ASCENDING.
      ELSE.
        CLEAR max.
        LOOP AT <prog>-scan->structures INTO DATA(str) WHERE type <> 'C' AND type <> 'R'.
          IF max < str-stmnt_to.
            max = str-stmnt_to.
            APPEND str TO structures.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

    LOOP AT structures INTO str.

      IF str-type = 'E'.
        READ TABLE io_debugger->mo_window->ms_sources-t_events
          WITH KEY program = i_program stmnt_type = str-stmnt_type stmnt_from = str-stmnt_from
          ASSIGNING FIELD-SYMBOL(<event>).
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs
          WITH KEY include = <event>-include INTO prog.
        READ TABLE prog-scan->statements INDEX <event>-stmnt_from INTO DATA(command).
        READ TABLE prog-scan->levels INDEX command-level INTO DATA(level).
        CLEAR event.
        LOOP AT prog-scan->tokens FROM command-from TO command-to INTO DATA(word).
          IF event IS INITIAL. event = word-str. ELSE. event = |{ event } { word-str }|. ENDIF.
        ENDLOOP.
        <event>-name = event. <event>-line = word-row.
        statement = <event>-stmnt_from + 1.
      ELSE.
        statement = str-stmnt_from.
        prog = <prog>.
      ENDIF.

      READ TABLE prog-t_keywords WITH KEY index = str-stmnt_from INTO DATA(key).
      IF key IS NOT INITIAL.
        zcl_ace_parser=>parse(
          EXPORTING i_program = CONV #( key-program ) i_include = CONV #( key-include ) i_run = 1
          CHANGING  cs_source = io_debugger->mo_window->ms_sources ).
      ENDIF.

      WHILE statement <= str-stmnt_to.
        READ TABLE prog-t_keywords WITH KEY index = statement INTO key.

        IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS'
        OR key-name = 'PARAMETERS' OR key-name = 'INCLUDE' OR key-name = 'REPORT'
        OR key-name = 'PUBLIC' OR key-name = 'PROTECTED' OR key-name = 'PRIVATE'
        OR key-name IS INITIAL OR sy-subrc <> 0 OR key-sub IS NOT INITIAL.
          ADD 1 TO statement. CONTINUE.
        ENDIF.
        ADD 1 TO io_debugger->m_step.

        READ TABLE io_debugger->mt_steps
          WITH KEY line = key-line program = i_program include = key-include
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
          <step>-step = io_debugger->m_step. <step>-line = key-line.
          IF i_evtype IS INITIAL. <step>-eventtype = 'EVENT'. <step>-eventname = event.
          ELSE. <step>-eventtype = i_evtype. <step>-eventname = i_evname. ENDIF.
          <step>-stacklevel = stack. <step>-program = i_program. <step>-include = key-include.
          IF <step>-eventtype = 'METHOD'. <step>-class = i_class. ENDIF.
        ENDIF.

        " Точечный парсинг calls/calcs/vars через parse_tokens( i_stmt_idx )
        IF key-calls_parsed = abap_false.
          zcl_ace_parser=>parse_tokens(
            EXPORTING
              i_program  = CONV #( key-program )
              i_include  = CONV #( key-include )
              i_stmt_idx = key-index
              i_class    = i_class
              i_evtype   = i_evtype
              i_ev_name  = i_evname
            CHANGING
              cs_source  = io_debugger->mo_window->ms_sources ).
          " Перечитываем key — calls_parsed = true, tt_calls заполнен
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = key-include INTO prog.
          READ TABLE prog-t_keywords WITH KEY index = statement INTO key.
        ENDIF.

        LOOP AT key-tt_calls INTO DATA(call).
          IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).

            IF call-event = 'FORM'.
              parse_call_form(
                i_call_name = call-name i_program = CONV #( call_line-program )
                i_include   = CONV #( call_line-include ) i_stack = stack io_debugger = io_debugger ).

            ELSEIF call-event = 'FUNCTION'.
              DATA func TYPE rs38l_fnam.
              func = call-name.
              IF io_debugger->mo_window->m_zcode IS INITIAL OR is_custom_code( func ).
                CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                  CHANGING funcname = func include = include
                  EXCEPTIONS function_not_exists = 1 include_not_exists = 2
                             group_not_exists = 3 no_selections = 4 no_function_include = 5 OTHERS = 6.
                code_execution_scanner( i_program = include i_include = include i_stack = stack
                  i_evtype = call-event i_evname = call-name io_debugger = io_debugger ).
              ENDIF.

            ELSEIF call-event = 'METHOD'.
              parse_class( i_include = i_include i_call = call i_stack = stack
                           io_debugger = io_debugger key = key ).

            ELSEIF call-event = 'SCREEN'.
              parse_screen( i_stack = stack i_call = call io_debugger = io_debugger key = key ).
            ENDIF.
          ENDIF.
        ENDLOOP.

        ADD 1 TO statement.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.
  method COLLECT_ENHANCEMENTS.

      DATA: form_name    TYPE string,
            position     TYPE string,
            enh_prog     TYPE program,
            tabix        TYPE i.

      TYPES: BEGIN OF ts_form_offset,
               form_name TYPE string,
               include   TYPE program,
               offset    TYPE i,
             END OF ts_form_offset.
      DATA lt_form_offsets TYPE STANDARD TABLE OF ts_form_offset.
      DATA lv_offset       TYPE i.

      DATA(lv_enh_prog) = i_program.
      SELECT SINGLE master FROM d010inc
        INTO @DATA(lv_master)
        WHERE include = @i_program.
      IF sy-subrc = 0 AND lv_master IS NOT INITIAL.
        lv_enh_prog = lv_master.
      ENDIF.

      DATA(lv_prog_enh_tabix) = 0.
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = i_program
        ASSIGNING FIELD-SYMBOL(<prog_enh>).
      IF sy-subrc = 0.
        lv_prog_enh_tabix = sy-tabix.
        IF <prog_enh>-enh_collected = abap_true.
          RETURN.
        ENDIF.
      ENDIF.

      SELECT programname, enhname, enhinclude, id, full_name, enhmode
        FROM d010enh
        INTO TABLE @DATA(lt_enh)
        WHERE programname = @lv_enh_prog
          AND version = 'A'.

      CHECK lt_enh IS NOT INITIAL.

      TYPES: BEGIN OF ts_enh_ext,
               programname  TYPE d010enh-programname,
               enhname      TYPE d010enh-enhname,
               enhinclude   TYPE d010enh-enhinclude,
               id           TYPE d010enh-id,
               full_name    TYPE d010enh-full_name,
               enhmode      TYPE d010enh-enhmode,
               enhtype      TYPE i,
               full_name_30 TYPE c LENGTH 30,
             END OF ts_enh_ext.
      DATA lt_enh_ext TYPE STANDARD TABLE OF ts_enh_ext.
      LOOP AT lt_enh INTO DATA(ls_enh_raw).
        DATA(ls_ext) = CORRESPONDING ts_enh_ext( ls_enh_raw ).
        ls_ext-enhtype = COND i(
          WHEN ls_ext-enhmode = 'D' AND ls_ext-full_name CS '%_BEGIN'    THEN 1
          WHEN ls_ext-enhmode = 'D' AND ls_ext-full_name CS '%_END'      THEN 2
          WHEN ls_ext-enhmode <> 'D' AND ls_ext-full_name CS '\SE:BEGIN' THEN 1
          WHEN ls_ext-enhmode <> 'D' AND ls_ext-full_name CS '\SE:END'   THEN 2
          ELSE 1 ).
        ls_ext-full_name_30 = ls_ext-full_name.
        APPEND ls_ext TO lt_enh_ext.
      ENDLOOP.
      SORT lt_enh_ext BY full_name_30 ASCENDING enhtype ASCENDING.

      LOOP AT lt_enh_ext INTO DATA(ls_enh).
        CLEAR: form_name, position.
        DATA(lv_full) = ls_enh-full_name.

        IF ls_enh-enhmode = 'D'.
          DATA(lv_class_name) = ``.
          DATA(lv_method_name) = ``.
          FIND FIRST OCCURRENCE OF REGEX '\\TY:([^\\]+)' IN lv_full SUBMATCHES lv_class_name.
          FIND FIRST OCCURRENCE OF REGEX '\\ME:([^\\]+)' IN lv_full SUBMATCHES lv_method_name.
          FIND FIRST OCCURRENCE OF REGEX '\\SE:([^\\]+)' IN lv_full SUBMATCHES position.
          CHECK lv_class_name IS NOT INITIAL AND lv_method_name IS NOT INITIAL AND position IS NOT INITIAL.
          DATA(lv_meth_pos) = SWITCH string( position WHEN '%_BEGIN' THEN 'BEGIN' WHEN '%_END' THEN 'END' ELSE `` ).
          CHECK lv_meth_pos IS NOT INITIAL.
          DATA(lv_has_overwrite) = abap_false.
          IF lv_meth_pos = 'BEGIN'.
            collect_method_enhancements(
              EXPORTING i_enhname = CONV #( ls_enh-enhname ) i_enhinclude = CONV #( ls_enh-enhinclude )
                        i_method = lv_method_name i_class = lv_class_name i_meth_pos = 'OVERWRITE'
                        i_id = CONV #( ls_enh-id ) io_debugger = io_debugger ).
            READ TABLE io_debugger->mo_window->ms_sources-tt_progs TRANSPORTING NO FIELDS
              WITH KEY program = lv_class_name.
            LOOP AT io_debugger->mo_window->ms_sources-tt_progs INTO DATA(ls_prog_ow).
              READ TABLE ls_prog_ow-tt_enh_blocks TRANSPORTING NO FIELDS
                WITH KEY ev_name = lv_method_name position = 'OVERWRITE'.
              IF sy-subrc = 0. lv_has_overwrite = abap_true. EXIT. ENDIF.
            ENDLOOP.
          ENDIF.
          IF lv_has_overwrite = abap_false.
            collect_method_enhancements(
              EXPORTING i_enhname = CONV #( ls_enh-enhname ) i_enhinclude = CONV #( ls_enh-enhinclude )
                        i_method = lv_method_name i_class = lv_class_name i_meth_pos = lv_meth_pos
                        i_id = CONV #( ls_enh-id ) io_debugger = io_debugger ).
          ENDIF.

        ELSE.
          FIND FIRST OCCURRENCE OF REGEX '\\FO:([^\\]+)' IN lv_full SUBMATCHES form_name.
          FIND FIRST OCCURRENCE OF REGEX '\\SE:([^\\]+)' IN lv_full SUBMATCHES position.
          CHECK form_name IS NOT INITIAL AND position IS NOT INITIAL.
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = ls_enh-enhinclude TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            ZCL_ACE_PARSER=>parse( EXPORTING i_program = CONV #( ls_enh-enhinclude )
              i_include = CONV #( ls_enh-enhinclude ) CHANGING cs_source = io_debugger->mo_window->ms_sources ).
          ENDIF.

          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
            WITH KEY eventtype = 'FORM' eventname = form_name INTO DATA(ls_call_line).
          CHECK sy-subrc = 0.

          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = ls_call_line-include ASSIGNING FIELD-SYMBOL(<prog>).
          CHECK sy-subrc = 0.

          DATA(lv_form_tabix) = 0.
          DATA ls_kw_form TYPE ZCL_ACE=>ts_kword.
          LOOP AT <prog>-t_keywords INTO ls_kw_form.
            IF ls_kw_form-name = 'FORM' AND ls_kw_form-index = ls_call_line-index.
              lv_form_tabix = sy-tabix. EXIT.
            ENDIF.
          ENDLOOP.
          CHECK lv_form_tabix > 0.
          READ TABLE <prog>-v_keywords WITH KEY include = ls_kw_form-include
            index = ls_kw_form-index INTO DATA(ls_vkw_form).
          IF sy-subrc = 0. ls_kw_form-v_line = ls_vkw_form-v_line. ENDIF.

          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = ls_enh-enhinclude INTO DATA(ls_enh_prog).
          CHECK sy-subrc = 0.

          DATA lt_enh_kw TYPE zcl_ace=>tt_kword.
          DATA lv_in_block TYPE boolean.
          CLEAR: lt_enh_kw, lv_in_block.
          LOOP AT ls_enh_prog-t_keywords INTO DATA(ls_kw).
            IF ls_kw-name = 'ENHANCEMENT'.
              DATA(lv_enh_id) = CONV i( ls_enh-id ).
              READ TABLE ls_enh_prog-scan->statements INDEX ls_kw-index INTO DATA(ls_stmt).
              READ TABLE ls_enh_prog-scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
              IF CONV i( ls_tok-str ) = lv_enh_id. lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw.
              ELSE. lv_in_block = abap_false. ENDIF.
              CONTINUE.
            ENDIF.
            IF ls_kw-name = 'ENDENHANCEMENT'.
              IF lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw. ENDIF.
              CLEAR lv_in_block. CONTINUE.
            ENDIF.
            IF lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw. ENDIF.
          ENDLOOP.
          CHECK lt_enh_kw IS NOT INITIAL.

          DATA ls_kw_end TYPE ZCL_ACE=>ts_kword.
          CLEAR ls_kw_end.
          IF position = 'BEGIN'.
            tabix = lv_form_tabix + 1.
          ELSE.
            tabix = lv_form_tabix + 1.
            LOOP AT <prog>-t_keywords INTO ls_kw_end FROM tabix.
              IF ls_kw_end-name = 'ENDFORM'. tabix = sy-tabix. EXIT. ENDIF.
              CLEAR ls_kw_end.
            ENDLOOP.
            READ TABLE <prog>-v_keywords WITH KEY include = ls_kw_end-include
              index = ls_kw_end-index INTO DATA(ls_vkw_end).
            IF sy-subrc = 0. ls_kw_end-v_line = ls_vkw_end-v_line. ENDIF.
          ENDIF.

          DATA(lv_vsrc_tabix) = COND i( WHEN position = 'BEGIN' THEN ls_kw_form-v_line + 1 ELSE ls_kw_end-v_line ).
          DATA(lv_vkw_tabix) = 0.
          IF position = 'BEGIN'.
            LOOP AT <prog>-v_keywords INTO DATA(ls_vkw_anchor)
              WHERE include = ls_kw_form-include AND index = ls_kw_form-index AND name = 'FORM'.
              lv_vkw_tabix = sy-tabix + 1. EXIT.
            ENDLOOP.
          ELSE.
            LOOP AT <prog>-v_keywords INTO ls_vkw_anchor
              WHERE include = ls_kw_end-include AND index = ls_kw_end-index AND name = 'ENDFORM'.
              lv_vkw_tabix = sy-tabix. EXIT.
            ENDLOOP.
          ENDIF.
          IF lv_vkw_tabix = 0. lv_vkw_tabix = lines( <prog>-v_keywords ) + 1. ENDIF.

          DATA(lv_enh_inserted) = 1.
          DATA(lv_tmp_line) = lt_enh_kw[ 1 ]-line.
          DATA(lv_tmp_last) = lt_enh_kw[ lines( lt_enh_kw ) ]-line.
          WHILE lv_tmp_line <= lv_tmp_last.
            ADD 1 TO lv_enh_inserted.
            READ TABLE lt_enh_kw WITH KEY line = lv_tmp_line INTO DATA(ls_ins_pre).
            IF sy-subrc = 0 AND ls_ins_pre-name = 'ENDENHANCEMENT'. ADD 1 TO lv_enh_inserted. ENDIF.
            ADD 1 TO lv_tmp_line.
          ENDWHILE.

          LOOP AT <prog>-v_keywords ASSIGNING FIELD-SYMBOL(<kw_v>) WHERE v_line >= lv_vsrc_tabix.
            ADD lv_enh_inserted TO <kw_v>-v_line.
            ADD lv_enh_inserted TO <kw_v>-v_from_row.
            ADD lv_enh_inserted TO <kw_v>-v_to_row.
          ENDLOOP.

          DATA(lv_vkw_vline) = lv_vsrc_tabix + 1.
          LOOP AT lt_enh_kw INTO DATA(ls_vkw_ins).
            ls_vkw_ins-v_line = lv_vkw_vline. ls_vkw_ins-v_from_row = lv_vkw_vline. ls_vkw_ins-v_to_row = lv_vkw_vline.
            INSERT ls_vkw_ins INTO <prog>-v_keywords INDEX lv_vkw_tabix.
            ADD 1 TO lv_vkw_tabix. ADD 1 TO lv_vkw_vline.
          ENDLOOP.

          DATA(lv_src_tabix) = lv_vsrc_tabix.
          DATA lv_sep TYPE string.
          IF position = 'BEGIN'.
            lv_sep = |"{ repeat( val = `"` occ = 40 ) } ENH { lv_enh_id } { form_name } BEGIN|.
          ELSE.
            lv_sep = |"{ repeat( val = `"` occ = 40 ) } ENH { lv_enh_id } { form_name } END|.
          ENDIF.
          INSERT CONV string( lv_sep ) INTO <prog>-v_source INDEX lv_src_tabix.
          ADD 1 TO lv_src_tabix. ADD 1 TO lv_offset.
          DATA(lv_first_line) = lt_enh_kw[ 1 ]-line.
          DATA(lv_last_line)  = lt_enh_kw[ lines( lt_enh_kw ) ]-line.
          DATA(lv_cur_line)   = lv_first_line.
          WHILE lv_cur_line <= lv_last_line.
            READ TABLE ls_enh_prog-source_tab INDEX lv_cur_line INTO DATA(lv_fe_src_line).
            IF sy-subrc = 0.
              READ TABLE lt_enh_kw WITH KEY line = lv_cur_line INTO DATA(ls_ins_chk).
              IF sy-subrc = 0 AND ls_ins_chk-name = 'ENHANCEMENT'.
                REPLACE REGEX '(ENHANCEMENT\s+\d+)(\s+)\.' IN lv_fe_src_line
                  WITH `$1$2` && ls_enh-enhname && `.`.
              ENDIF.
              INSERT lv_fe_src_line INTO <prog>-v_source INDEX lv_src_tabix.
              ADD 1 TO lv_src_tabix. ADD 1 TO lv_offset.
              IF sy-subrc = 0 AND ls_ins_chk-name = 'ENDENHANCEMENT'.
                DATA(lv_end_sep) = |"{ repeat( val = `"` occ = 40 ) } ENH { lv_enh_id } END|.
                INSERT CONV string( lv_end_sep ) INTO <prog>-v_source INDEX lv_src_tabix.
                ADD 1 TO lv_src_tabix. ADD 1 TO lv_offset.
              ENDIF.
            ENDIF.
            ADD 1 TO lv_cur_line.
          ENDWHILE.

          APPEND INITIAL LINE TO <prog>-tt_enh_blocks ASSIGNING FIELD-SYMBOL(<enh_blk>).
          <enh_blk>-ev_type    = ls_call_line-eventtype.
          <enh_blk>-ev_name    = form_name.
          <enh_blk>-position   = position.
          <enh_blk>-enh_name   = ls_enh-enhname.
          <enh_blk>-enh_include = ls_enh-enhinclude.
          <enh_blk>-enh_id     = lv_enh_id.
          <enh_blk>-from_line  = 0.
          <enh_blk>-to_line    = 0.
        ENDIF.
      ENDLOOP.

      IF lv_prog_enh_tabix > 0.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs
          INDEX lv_prog_enh_tabix ASSIGNING <prog_enh>.
        IF sy-subrc = 0. <prog_enh>-enh_collected = abap_true. ENDIF.
      ENDIF.

  endmethod.
  method COLLECT_EVENTS.
      LOOP AT io_scan->structures INTO DATA(struc) WHERE type = 'E'.
        APPEND INITIAL LINE TO io_debugger->mo_window->ms_sources-t_events
          ASSIGNING FIELD-SYMBOL(<event>).
        <event>-program = i_program.
        MOVE-CORRESPONDING struc TO <event>.
        <event>-include = i_include.
      ENDLOOP.
  endmethod.
  method COLLECT_METHOD_ENHANCEMENTS.

      DATA(lv_enhname_trimmed)  = condense( val = CONV string( i_enhname ) ).
      DATA(lv_enhinclude_str)   = condense( val = CONV string( i_enhinclude ) ).
      DATA(lv_eimp_include) = CONV program(
        substring( val = lv_enhinclude_str len = strlen( lv_enhinclude_str ) - 1 ) && 'EIMP' ).
      DATA(lv_impl_prefix) = COND string(
        WHEN i_meth_pos = 'BEGIN'     THEN 'IPR_'
        WHEN i_meth_pos = 'END'       THEN 'IPO_'
        WHEN i_meth_pos = 'OVERWRITE' THEN 'IOW_' ).
      DATA(lv_impl_method) = lv_impl_prefix && lv_enhname_trimmed && '~' && i_method.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = lv_eimp_include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ZCL_ACE_PARSER=>parse( EXPORTING i_program = lv_eimp_include i_include = lv_eimp_include
          CHANGING cs_source = io_debugger->mo_window->ms_sources ).
      ENDIF.
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = lv_eimp_include INTO DATA(ls_eimp_prog).
      CHECK sy-subrc = 0.

      DATA(lv_class_prog) = CONV program(
        i_class && repeat( val = '=' occ = 30 - strlen( i_class ) ) && 'CP' ).
      DATA(lv_cm_pattern) = lv_class_prog(28) && 'CM%'.
      SELECT include FROM d010inc
        WHERE master = @lv_class_prog AND include LIKE @lv_cm_pattern
        INTO TABLE @DATA(lt_cm_includes).
      LOOP AT lt_cm_includes INTO DATA(ls_cm).
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs
          WITH KEY include = ls_cm-include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ZCL_ACE_PARSER=>parse( EXPORTING i_program = ls_cm-include i_include = ls_cm-include
            CHANGING cs_source = io_debugger->mo_window->ms_sources ).
        ENDIF.
      ENDLOOP.

      DATA ls_call_line_m TYPE ZCL_ACE=>ts_calls_line.
      LOOP AT io_debugger->mo_window->ms_sources-tt_calls_line INTO ls_call_line_m
        WHERE eventtype = 'METHOD' AND eventname = i_method AND class = i_class.
        EXIT.
      ENDLOOP.
      CHECK sy-subrc = 0.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = ls_call_line_m-include ASSIGNING FIELD-SYMBOL(<prog_m>).
      CHECK sy-subrc = 0.

      DATA(lv_meth_tabix) = 0.
      DATA ls_kw_meth TYPE ZCL_ACE=>ts_kword.
      LOOP AT <prog_m>-t_keywords INTO ls_kw_meth.
        IF ls_kw_meth-name = 'METHOD' AND ls_kw_meth-index = ls_call_line_m-index.
          lv_meth_tabix = sy-tabix. EXIT.
        ENDIF.
      ENDLOOP.
      CHECK lv_meth_tabix > 0.

      DATA lt_enh_kw TYPE zcl_ace=>tt_kword.
      DATA lv_in_block TYPE boolean.
      LOOP AT ls_eimp_prog-t_keywords INTO DATA(ls_kw).
        IF ls_kw-name = 'METHOD'.
          READ TABLE ls_eimp_prog-scan->statements INDEX ls_kw-index INTO DATA(ls_stmt).
          READ TABLE ls_eimp_prog-scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
          IF ls_tok-str = lv_impl_method OR ls_tok-str CP |IOW_*~{ i_method }|.
            lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw.
          ELSE. lv_in_block = abap_false. ENDIF.
          CONTINUE.
        ENDIF.
        IF ls_kw-name = 'ENDMETHOD'.
          IF lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw. EXIT. ENDIF.
          CLEAR lv_in_block. CONTINUE.
        ENDIF.
        IF lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw. ENDIF.
      ENDLOOP.
      CHECK lt_enh_kw IS NOT INITIAL.

      DATA(lv_ins_tabix) = lv_meth_tabix + 1.
      DATA ls_kw_end TYPE ZCL_ACE=>ts_kword.
      IF i_meth_pos = 'END' OR i_meth_pos = 'OVERWRITE'.
        LOOP AT <prog_m>-t_keywords INTO ls_kw_end FROM lv_ins_tabix.
          IF ls_kw_end-name = 'ENDMETHOD'. lv_ins_tabix = sy-tabix + 1. EXIT. ENDIF.
        ENDLOOP.
      ENDIF.

      IF i_meth_pos = 'OVERWRITE'.
        READ TABLE <prog_m>-tt_enh_blocks TRANSPORTING NO FIELDS
          WITH KEY ev_type = 'METHOD' ev_name = i_method position = 'OVERWRITE' enh_name = i_enhname.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO <prog_m>-tt_enh_blocks ASSIGNING FIELD-SYMBOL(<enh_blk_ow>).
          <enh_blk_ow>-ev_type = 'METHOD'. <enh_blk_ow>-ev_name = i_method.
          <enh_blk_ow>-position = 'OVERWRITE'. <enh_blk_ow>-enh_name = i_enhname.
          <enh_blk_ow>-enh_include = CONV #( |{ i_enhinclude }IMP| ).
          <enh_blk_ow>-from_line = 0. <enh_blk_ow>-to_line = 0.
        ENDIF.
        RETURN.
      ENDIF.

      READ TABLE <prog_m>-tt_enh_blocks TRANSPORTING NO FIELDS
        WITH KEY ev_type = 'METHOD' ev_name = i_method position = i_meth_pos enh_name = i_enhname.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO <prog_m>-tt_enh_blocks ASSIGNING FIELD-SYMBOL(<enh_blk>).
        <enh_blk>-ev_type = 'METHOD'. <enh_blk>-ev_name = i_method.
        <enh_blk>-position = i_meth_pos. <enh_blk>-enh_name = i_enhname.
        <enh_blk>-enh_include = CONV #( |{ i_enhinclude }IMP| ).
        <enh_blk>-from_line = 0. <enh_blk>-to_line = 0.
      ENDIF.

  endmethod.
  method DETECT_METHOD_CALL.
      DATA: split TYPE string_table.
      IF call-event = 'METHOD' AND call-name IS NOT INITIAL.
        APPEND call TO token-tt_calls.
        CLEAR: call-event, call-type, call-name, call-outer, call-inner.
      ENDIF.
      call-name = word. call-event = 'METHOD'.
      REPLACE ALL OCCURRENCES OF '(' IN call-name WITH ''.
      REPLACE ALL OCCURRENCES OF 'ME->' IN call-name WITH ''.
      FIND FIRST OCCURRENCE OF '->' IN call-name.
      IF sy-subrc = 0.
        SPLIT call-name AT '->' INTO TABLE split.
        IF split[ 1 ] <> ')'.
          READ TABLE io_debugger->mo_window->ms_sources-t_vars
            WITH KEY program = i_program name = split[ 1 ] INTO DATA(vars).
          IF sy-subrc <> 0. call-class = split[ 1 ]. ELSE. call-class = vars-type. ENDIF.
        ENDIF.
        call-name = split[ 2 ].
        IF split[ 1 ] = 'SUPER'. call-class = class_name. call-super = abap_true. ENDIF.
      ENDIF.
      FIND FIRST OCCURRENCE OF '=>' IN call-name.
      IF sy-subrc = 0.
        SPLIT call-name AT '=>' INTO TABLE split.
        IF split[ 1 ] <> ')'. call-class = split[ 1 ]. ENDIF.
        call-name = split[ 2 ].
      ENDIF.
      IF call-class IS INITIAL.
        IF i_class IS NOT INITIAL. call_line-class = call-class = i_class. ENDIF.
        IF class_name IS NOT INITIAL. call_line-class = call-class = class_name. ENDIF.
      ENDIF.
      call-event = 'METHOD'.
      IF new = abap_true.
        call-class = call-name. call-name = 'CONSTRUCTOR'.
        call_line-class = call-class. call_line-eventname = call-name. call_line-eventtype = 'METHOD'.
        READ TABLE calculated_vars WITH KEY line = l_token_row program = i_include INTO DATA(calc).
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO io_debugger->mo_window->ms_sources-tt_refvar ASSIGNING FIELD-SYMBOL(<refvar>).
          <refvar>-name = calc-name. <refvar>-class = call-class.
        ENDIF.
      ENDIF.
      READ TABLE io_debugger->mo_window->ms_sources-tt_refvar WITH KEY name = call-class INTO DATA(refvar).
      IF sy-subrc = 0. call-class = refvar-class. ENDIF.
  endmethod.
  method LINK_CALLS_TO_PARAMS.
      FIELD-SYMBOLS: <s_token> TYPE ZCL_ACE=>ts_kword, <call> TYPE ZCL_ACE=>ts_calls.
      DATA: call TYPE ZCL_ACE=>ts_calls, param TYPE ZCL_ACE=>ts_params, index TYPE i.
      LOOP AT ct_tokens ASSIGNING <s_token> WHERE tt_calls IS NOT INITIAL.
        READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
        index = 0.
        LOOP AT io_debugger->mo_window->ms_sources-t_params INTO param
          WHERE event = call-event AND name = call-name.
          ADD 1 TO index.
          READ TABLE <s_token>-tt_calls INDEX index ASSIGNING <call>.
          IF sy-subrc = 0.
            <call>-inner = param-param.
            IF param-type = 'I'. <call>-type = '>'. ELSE. <call>-type = '<'. ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
  endmethod.
  METHOD parse_call.

    DATA: statement TYPE i,
          stack     TYPE i,
          include   TYPE progname,
          prefix    TYPE string,
          program   TYPE program.

    stack = i_stack + 1.
    CHECK stack <= io_debugger->mo_window->m_hist_depth.

    READ TABLE io_debugger->mt_steps
      WITH KEY program = i_include eventname = i_e_name eventtype = i_e_type class = i_class
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0. RETURN. ENDIF.

    READ TABLE io_debugger->mo_window->mt_calls
      WITH KEY include = i_include ev_name = i_e_name class = i_class
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      APPEND INITIAL LINE TO io_debugger->mo_window->mt_calls ASSIGNING FIELD-SYMBOL(<method_call>).
      <method_call>-include = i_include.
      <method_call>-ev_name = i_e_name.
      <method_call>-class   = i_class.
    ENDIF.

    DATA: cl_key        TYPE seoclskey,
          meth_includes TYPE seop_methods_w_include.
    cl_key = i_class.

    READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
      WITH KEY class = i_class TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      statement = i_index.
    ELSE.
      CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
        EXPORTING clskey = cl_key IMPORTING includes = meth_includes
        EXCEPTIONS _internal_class_not_existing = 1 OTHERS = 2.
      IF lines( meth_includes ) IS INITIAL. statement = i_index. ELSE. statement = 1. ENDIF.
    ENDIF.

    " Если передан конкретный индекс statement через i_stmt_idx — использовать его
    IF i_stmt_idx > 0.
      statement = i_stmt_idx.
    ENDIF.

    IF i_include IS NOT INITIAL.
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = i_include INTO DATA(prog).
      IF sy-subrc <> 0.
        zcl_ace_parser=>parse(
          EXPORTING i_program = i_program i_include = i_include
          CHANGING  cs_source = io_debugger->mo_window->ms_sources ).
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO prog.
      ENDIF.
    ELSE.
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = i_program INTO prog.
      IF sy-subrc <> 0.
        zcl_ace_parser=>parse(
          EXPORTING i_program = i_program i_include = i_program
          CHANGING  cs_source = io_debugger->mo_window->ms_sources ).
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO prog.
      ENDIF.
    ENDIF.

    DATA(max) = lines( prog-scan->statements ).
    DO.
      IF statement > max. EXIT. ENDIF.

      READ TABLE prog-t_keywords WITH KEY index = statement INTO DATA(key).
      IF sy-subrc <> 0. ADD 1 TO statement. CONTINUE. ENDIF.

      " Точечный парсинг calls/calcs/vars через parse_tokens( i_stmt_idx )
      IF key-calls_parsed = abap_false.
        zcl_ace_parser=>parse_tokens(
          EXPORTING
            i_program  = CONV #( key-program )
            i_include  = CONV #( key-include )
            i_stmt_idx = key-index
            i_class    = i_class
            i_evtype   = i_e_type
            i_ev_name  = i_e_name
          CHANGING
            cs_source  = io_debugger->mo_window->ms_sources ).
        " Перечитываем key — calls_parsed = true, tt_calls заполнен
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs
          WITH KEY include = key-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY index = statement INTO key.
      ENDIF.

      IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS' OR key-name IS INITIAL.
        ADD 1 TO statement. CONTINUE.
      ENDIF.

      READ TABLE io_debugger->mt_steps
        WITH KEY line = key-line program = i_program include = key-include
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0 AND i_no_steps IS INITIAL.
        ADD 1 TO io_debugger->m_step.
        APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
        <step>-step       = io_debugger->m_step.
        <step>-line       = key-line.
        <step>-eventname  = i_e_name.
        <step>-eventtype  = i_e_type.
        <step>-stacklevel = stack.
        <step>-program    = i_program.
        <step>-include    = key-include.
        <step>-class      = i_class.
      ENDIF.

      LOOP AT key-tt_calls INTO DATA(call).
        IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).
          IF call-event = 'FORM'.
            parse_call_form( i_call_name = call-name i_program = i_include
              i_include = i_include i_stack = stack io_debugger = io_debugger ).

          ELSEIF call-event = 'FUNCTION'.
            DATA func TYPE rs38l_fnam.
            func = call-name.
            REPLACE ALL OCCURRENCES OF '''' IN func WITH ''.
            IF io_debugger->mo_window->m_zcode IS INITIAL OR is_custom_code( func ).
              CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                CHANGING funcname = func include = include
                EXCEPTIONS function_not_exists = 1 include_not_exists = 2
                           group_not_exists = 3 no_selections = 4 no_function_include = 5 OTHERS = 6.
              code_execution_scanner( i_program = include i_include = include i_stack = stack
                i_evtype = call-event i_evname = call-name io_debugger = io_debugger ).
            ENDIF.

          ELSEIF call-event = 'METHOD'.
            DATA inlude TYPE program.
            IF i_include IS INITIAL. include = i_program. ELSE. include = i_include. ENDIF.
            IF call-class = 'ME' OR call-class IS INITIAL. call-class = i_class. ENDIF.
            parse_class( i_include = include i_call = call i_stack = stack
                         io_debugger = io_debugger key = key ).

          ELSEIF call-event = 'SCREEN'.
            parse_screen( i_stack = stack i_call = call io_debugger = io_debugger key = key ).
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF key-name = 'ENDFORM' OR key-name = 'ENDMETHOD' OR key-name = 'ENDMODULE'.
        RETURN.
      ENDIF.

      ADD 1 TO statement.
    ENDDO.

  ENDMETHOD.
  METHOD parse_call_form.

    DATA call_line TYPE zcl_ace=>ts_calls_line.
    READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
      WITH KEY eventname = i_call_name eventtype = 'FORM' INTO call_line.
    CHECK sy-subrc = 0.

    DATA(lv_inc) = CONV program( call_line-include ).
    IF lv_inc IS INITIAL. lv_inc = i_include. ENDIF.

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_inc TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      zcl_ace_parser=>parse( EXPORTING i_program = lv_inc i_include = lv_inc
        CHANGING cs_source = io_debugger->mo_window->ms_sources ).
    ENDIF.

    zcl_ace_source_parser=>collect_enhancements( i_program = lv_inc io_debugger = io_debugger ).

    DATA(lv_stack) = i_stack + 1.
    CHECK lv_stack <= io_debugger->mo_window->m_hist_depth.

    READ TABLE io_debugger->mt_steps
      WITH KEY program = lv_inc eventname = i_call_name eventtype = 'FORM' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0. RETURN. ENDIF.

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = lv_inc INTO DATA(prog).
    CHECK sy-subrc = 0.

    DATA(lv_use_vkw) = abap_false.
    IF prog-v_keywords IS NOT INITIAL. lv_use_vkw = abap_true. ENDIF.

    DATA(lv_tabix) = 0.
    IF lv_use_vkw = abap_true.
      LOOP AT prog-v_keywords INTO DATA(kw).
        IF kw-name = 'FORM' AND kw-index = call_line-index. lv_tabix = sy-tabix. EXIT. ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT prog-t_keywords INTO kw.
        IF kw-name = 'FORM' AND kw-index = call_line-index. lv_tabix = sy-tabix. EXIT. ENDIF.
      ENDLOOP.
    ENDIF.
    CHECK lv_tabix > 0.

    DATA(lv_has_pre) = abap_false.
    READ TABLE prog-tt_enh_blocks TRANSPORTING NO FIELDS WITH KEY ev_name = i_call_name position = 'BEGIN'.
    IF sy-subrc = 0. lv_has_pre = abap_true. ENDIF.
    DATA(lv_body_stack) = COND i( WHEN lv_has_pre = abap_true THEN lv_stack + 1 ELSE lv_stack ).

    DATA ls_cur_enh TYPE zif_ace_parse_data=>ts_enh_block.
    CLEAR ls_cur_enh.

    FIELD-SYMBOLS <kw_tab> TYPE zcl_ace=>tt_kword.
    IF lv_use_vkw = abap_true. ASSIGN prog-v_keywords TO <kw_tab>.
    ELSE. ASSIGN prog-t_keywords TO <kw_tab>. ENDIF.

    LOOP AT <kw_tab> INTO kw FROM lv_tabix.
      IF kw-name = 'ENDFORM'. EXIT. ENDIF.

      IF kw-name = 'ENHANCEMENT'.
        DATA(lv_enh_id_cur) = 0.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = kw-include INTO DATA(enh_prog).
        IF sy-subrc = 0.
          READ TABLE enh_prog-scan->statements INDEX kw-index INTO DATA(ls_enh_stmt).
          IF sy-subrc = 0.
            READ TABLE enh_prog-scan->tokens INDEX ls_enh_stmt-from + 1 INTO DATA(ls_enh_tok).
            lv_enh_id_cur = CONV i( ls_enh_tok-str ).
          ENDIF.
        ENDIF.
        READ TABLE prog-tt_enh_blocks INTO ls_cur_enh
          WITH KEY enh_include = kw-include ev_name = i_call_name enh_id = lv_enh_id_cur.
        IF sy-subrc <> 0.
          READ TABLE prog-tt_enh_blocks INTO ls_cur_enh WITH KEY enh_include = kw-include ev_name = i_call_name.
        ENDIF.
        CONTINUE.
      ENDIF.
      IF kw-name = 'ENDENHANCEMENT'. CLEAR ls_cur_enh. CONTINUE. ENDIF.
      IF kw-name = 'FORM' OR kw-name = 'DATA' OR kw-name = 'TYPES'
        OR kw-name = 'CONSTANTS' OR kw-name IS INITIAL. CONTINUE. ENDIF.

      " Точечный парсинг через parse_call с i_stmt_idx
      IF kw-calls_parsed = abap_false.
        parse_call(
          EXPORTING
            i_program   = CONV #( kw-program )
            i_include   = CONV #( kw-include )
            i_index     = 0
            i_stack     = lv_stack
            i_e_name    = i_call_name
            i_e_type    = 'FORM'
            i_stmt_idx  = kw-index
            io_debugger = io_debugger ).
        " Перечитываем kw с актуальным tt_calls
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_inc INTO prog.
        IF lv_use_vkw = abap_true. ASSIGN prog-v_keywords TO <kw_tab>.
        ELSE. ASSIGN prog-t_keywords TO <kw_tab>. ENDIF.
        READ TABLE <kw_tab> WITH KEY index = kw-index INTO kw.
      ENDIF.

      READ TABLE io_debugger->mt_steps
        WITH KEY line = kw-line program = i_program include = kw-include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ADD 1 TO io_debugger->m_step.
        APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
        <step>-step = io_debugger->m_step. <step>-line = kw-line.
        <step>-program = i_program. <step>-include = kw-include.
        IF ls_cur_enh IS NOT INITIAL.
          <step>-eventtype = 'ENHANCEMENT'.
          <step>-eventname = |{ ls_cur_enh-enh_name } { ls_cur_enh-enh_id }|.
          IF ls_cur_enh-position = 'BEGIN'. <step>-stacklevel = lv_stack.
          ELSE. <step>-stacklevel = lv_body_stack + 1. ENDIF.
        ELSE.
          <step>-eventtype = 'FORM'. <step>-eventname = i_call_name. <step>-stacklevel = lv_body_stack.
        ENDIF.
      ENDIF.

      LOOP AT kw-tt_calls INTO DATA(call).
        IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).
          IF call-event = 'FORM'.
            zcl_ace_source_parser=>parse_call_form( i_call_name = call-name i_program = lv_inc
              i_include = lv_inc i_stack = lv_stack io_debugger = io_debugger ).
          ELSEIF call-event = 'FUNCTION'.
            DATA func TYPE rs38l_fnam.
            func = call-name.
            REPLACE ALL OCCURRENCES OF '''' IN func WITH ''.
            IF io_debugger->mo_window->m_zcode IS INITIAL OR is_custom_code( func ).
              DATA lv_finc TYPE progname.
              CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                CHANGING funcname = func include = lv_finc EXCEPTIONS OTHERS = 6.
              IF sy-subrc = 0.
                zcl_ace_source_parser=>code_execution_scanner( i_program = lv_finc i_include = lv_finc
                  i_stack = lv_stack i_evtype = 'FUNCTION' i_evname = CONV #( func ) io_debugger = io_debugger ).
              ENDIF.
            ENDIF.
          ELSEIF call-event = 'METHOD'.
            zcl_ace_source_parser=>parse_class( i_include = lv_inc i_call = call
              i_stack = lv_stack io_debugger = io_debugger key = kw ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD parse_class.

    DATA: cl_key        TYPE seoclskey,
          meth_includes TYPE seop_methods_w_include,
          prefix        TYPE string,
          program       TYPE program,
          include       TYPE progname,
          stack         TYPE i,
          class_call    TYPE zcl_ace=>ts_calls.

    cl_key = i_call-class.
    stack = i_stack.

    DATA(lv_local_exists) = abap_false.
    READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
      WITH KEY class = i_call-class TRANSPORTING NO FIELDS.
    IF sy-subrc = 0. lv_local_exists = abap_true. ENDIF.

    IF lv_local_exists = abap_false.
      CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
        EXPORTING clskey = cl_key IMPORTING includes = meth_includes
        EXCEPTIONS _internal_class_not_existing = 1 OTHERS = 2.
    ENDIF.

    IF io_debugger->mo_window->m_zcode IS INITIAL
       OR is_custom_code( i_call-class )
       OR meth_includes IS INITIAL.

      IF lines( meth_includes ) > 0 AND lv_local_exists = abap_false.
        prefix = i_call-class && repeat( val = `=` occ = 30 - strlen( i_call-class ) ).
        include = program = prefix && 'CP'.
        ZCL_ACE_PARSER=>parse( EXPORTING i_program = program i_include = include
          i_class = i_call-class CHANGING cs_source = io_debugger->mo_window->ms_sources ).
      ELSE.
        program = i_include.
      ENDIF.

      IF i_call-super IS INITIAL.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
          WITH KEY class = cl_key eventtype = 'METHOD' eventname = i_call-name INTO DATA(call_line).
      ELSE.
        sy-subrc = 1.
      ENDIF.

      IF sy-subrc <> 0.
        WHILE call_line IS INITIAL.
          LOOP AT io_debugger->mo_window->ms_sources-t_classes INTO DATA(ls_class) WHERE clsname = cl_key.
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
              WITH KEY class = ls_class-refclsname eventtype = 'METHOD' eventname = i_call-name INTO call_line.
            IF sy-subrc = 0. EXIT. ENDIF.
          ENDLOOP.
          IF sy-subrc <> 0. EXIT. ENDIF.
          cl_key = ls_class-refclsname.
        ENDWHILE.
      ENDIF.

      IF call_line IS INITIAL.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
          WITH KEY class = cl_key eventtype = 'METHOD' eventname = i_call-name INTO call_line.
      ENDIF.

      IF sy-subrc = 0.
        IF call_line-include IS NOT INITIAL. include = call_line-include. ENDIF.
        IF i_call-name = 'CONSTRUCTOR'.
          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
            WITH KEY class = cl_key eventtype = 'METHOD' eventname = 'CLASS_CONSTRUCTOR' INTO DATA(call_super).
          IF sy-subrc = 0.
            zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_super-index
              i_e_name = 'CLASS_CONSTRUCTOR' i_e_type = call_line-eventtype
              i_program = CONV #( include ) i_include = CONV #( include )
              i_class = call_line-class i_stack = i_stack io_debugger = io_debugger ).
          ENDIF.
        ENDIF.
        zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
          i_e_name = call_line-eventname i_e_type = call_line-eventtype
          i_program = CONV #( include ) i_include = CONV #( include )
          i_class = call_line-class i_stack = i_stack io_debugger = io_debugger ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  method PARSE_SCREEN.

      DATA: stack    TYPE i,
            ftab     TYPE STANDARD TABLE OF d021s,
            scr_code TYPE STANDARD TABLE OF d022s,
            prog     TYPE progname,
            num(4)   TYPE n,
            fmnum    TYPE sychar04,
            code_str TYPE string,
            pbo      TYPE boolean,
            pai      TYPE boolean,
            split    TYPE TABLE OF string.

      stack = i_stack + 1.
      prog = key-program.
      fmnum = num = i_Call-name.

      CALL FUNCTION 'RS_IMPORT_DYNPRO'
        EXPORTING dyname = prog dynumb = fmnum
        TABLES ftab = ftab pltab = scr_code EXCEPTIONS OTHERS = 19.
      IF sy-subrc <> 0. RETURN. ENDIF.

      LOOP AT scr_code ASSIGNING FIELD-SYMBOL(<code>).
        CONDENSE <code>.
        FIND '"' IN <code> MATCH OFFSET DATA(pos).
        IF pos <> 0. <code> = <code>+0(pos). ENDIF.
      ENDLOOP.
      DELETE scr_code WHERE line+0(1) = '*' OR line+0(1) = '"' OR line IS INITIAL.

      LOOP AT scr_code INTO DATA(code).
        IF code_str IS INITIAL. code_str = code-line.
        ELSE. code_str = |{ code_str } { code-line }|. ENDIF.
      ENDLOOP.
      SPLIT code_str AT '.' INTO TABLE scr_code.

      LOOP AT scr_code INTO code.
        CONDENSE code-line.
        IF code-line = 'PROCESS BEFORE OUTPUT'.
          pbo = abap_true. CLEAR pai.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
          ADD 1 TO io_debugger->m_step.
          <step>-step = io_debugger->m_step. <step>-line = key-line.
          <step>-eventname = i_call-name. <step>-eventtype = i_call-event.
          <step>-stacklevel = stack. <step>-program = key-program. <step>-include = key-include.
          CONTINUE.
        ENDIF.
        IF code-line = 'PROCESS AFTER INPUT'. pai = abap_true. CLEAR pbo. CONTINUE. ENDIF.
        CHECK pbo IS NOT INITIAL.
        SPLIT code-line AT | | INTO TABLE split.
        CHECK split[ 1 ] = 'MODULE'.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
          WITH KEY program = key-program eventtype = 'MODULE' eventname = split[ 2 ] INTO DATA(call_line).
        IF sy-subrc = 0.
          ZCL_ACE_SOURCE_PARSER=>parse_call( EXPORTING i_index = call_line-index
            i_e_name = call_line-eventname i_e_type = call_line-eventtype
            i_program = CONV #( call_line-program ) i_include = CONV #( call_line-include )
            i_stack = stack io_debugger = io_debugger ).
        ENDIF.
      ENDLOOP.

      LOOP AT scr_code INTO code.
        CONDENSE code-line.
        IF code-line = 'PROCESS AFTER INPUT'.
          CLEAR pbo. pai = abap_true.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING <step>.
          ADD 1 TO io_debugger->m_step.
          <step>-step = io_debugger->m_step. <step>-line = key-line.
          <step>-eventname = i_call-name. <step>-eventtype = i_call-event.
          <step>-stacklevel = stack. <step>-program = key-program. <step>-include = key-include.
          CONTINUE.
        ENDIF.
        IF code-line = 'PROCESS BEFORE OUTPUT'. pbo = abap_true. CLEAR pai. CONTINUE. ENDIF.
        CHECK pai IS NOT INITIAL.
        SPLIT code-line AT | | INTO TABLE split.
        CHECK split[ 1 ] = 'MODULE'.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
          WITH KEY program = key-program eventtype = 'MODULE' eventname = split[ 2 ] INTO call_line.
        IF sy-subrc = 0.
          ZCL_ACE_SOURCE_PARSER=>parse_call( EXPORTING i_index = call_line-index
            i_e_name = call_line-eventname i_e_type = call_line-eventtype
            i_program = CONV #( call_line-program ) i_include = CONV #( call_line-include )
            i_stack = stack io_debugger = io_debugger ).
        ENDIF.
      ENDLOOP.

  endmethod.
  method PROCESS_SUPER_AND_INTERFACES.
      DATA: suffix TYPE string, lt_classes TYPE STANDARD TABLE OF zif_ace_parse_data=>ts_meta,
            prefix TYPE string, program TYPE program, include TYPE program.
      SELECT clsname, refCLSNAME, reltype FROM seometarel APPENDING TABLE @lt_classes WHERE clsname = @i_class.
      LOOP AT lt_classes INTO DATA(interface).
        prefix = interface-refclsname && repeat( val = `=` occ = 30 - strlen( interface-refclsname ) ).
        CASE interface-reltype.
          WHEN '0' OR '1'. suffix = 'IU'.
          WHEN '2'. suffix = 'CP'.
          WHEN OTHERS. RETURN.
        ENDCASE.
        include = program = prefix && suffix.
        ZCL_ACE_PARSER=>parse( EXPORTING i_program = program i_include = include
          i_class = CONV #( interface-refclsname ) CHANGING cs_source = io_debugger->mo_window->ms_sources ).
      ENDLOOP.
      APPEND LINES OF lt_classes TO io_debugger->mo_window->ms_sources-t_classes[].
  endmethod.
  method REGISTER_FIELD_SYMBOL.
      DATA: split TYPE TABLE OF string.
      SPLIT cs_state-change AT '-' INTO TABLE split.
      cs_state-change = split[ 1 ].
      IF cs_state-eventtype IS INITIAL.
        READ TABLE io_debugger->mo_window->mt_globals_set WITH KEY program = i_include ASSIGNING FIELD-SYMBOL(<globals_set>).
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO io_debugger->mo_window->mt_globals_set ASSIGNING <globals_set>.
          <globals_set>-program = i_include.
        ENDIF.
        READ TABLE <globals_set>-mt_fs WITH KEY name = cs_state-change TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO <globals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<gl_fs>).
          <gl_fs>-name = cs_state-change.
        ENDIF.
      ELSE.
        READ TABLE io_debugger->mo_window->mt_locals_set
          WITH KEY program = i_include eventtype = cs_state-eventtype eventname = cs_state-eventname
          ASSIGNING FIELD-SYMBOL(<locals_set>).
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO io_debugger->mo_window->mt_locals_set ASSIGNING <locals_set>.
          <locals_set>-program = i_include. <locals_set>-eventname = cs_state-eventname. <locals_set>-eventtype = cs_state-eventtype.
        ENDIF.
        READ TABLE <locals_set>-mt_fs WITH KEY name = cs_state-change TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
          <loc_fs>-name = cs_state-change.
        ENDIF.
      ENDIF.
  endmethod.
ENDCLASS.

CLASS ZCL_ACE_SEL_OPT IMPLEMENTATION.
  method CONSTRUCTOR.

      DATA: effect     TYPE i,
            handle_alv TYPE i.

      mo_viewer = io_viewer.
      mo_sel_alv = NEW #( i_parent = io_container ).
      update_sel_tab( ).

      ms_layout-s_dragdrop-col_ddid = handle_alv.
      init_fcat( ).
      ms_layout-cwidth_opt = abap_true.
      ms_layout-col_opt = abap_true.
      ms_layout-ctab_fname = 'COLOR'.
      ms_layout-stylefname = 'STYLE'.

      DATA(gt_f4) = VALUE  lvc_t_f4( register   = abap_true chngeafter = abap_true
                               ( fieldname  = 'LOW'  )
                               ( fieldname  = 'HIGH'  ) ).

      mo_sel_alv->register_f4_for_fields( it_f4 = gt_f4 ).
      mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
      mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

      SET HANDLER handle_user_command
                  handle_sel_toolbar
                  on_data_changed
                  on_data_changed_finished
                  on_grid_button_click
                  handle_context_menu_request
                  handle_doubleclick
                  on_f4 FOR mo_sel_alv.

      CALL METHOD mo_sel_alv->set_table_for_first_display
        EXPORTING
          i_save          = abap_true
          i_default       = abap_true
          is_layout       = ms_layout
        CHANGING
          it_outtab       = mt_sel_tab[]
          it_fieldcatalog = mt_fcat.

      mo_sel_alv->set_toolbar_interactive( ).
  endmethod.
  method HANDLE_CONTEXT_MENU_REQUEST.
      DATA: func  TYPE ui_func,
            funcs TYPE ui_functions.

      DATA(l_index) = ZCL_ACE_ALV_COMMON=>get_selected( mo_sel_alv ).

      IF l_index IS NOT INITIAL.
        READ TABLE mt_sel_tab INTO DATA(l_sel) INDEX l_index.
      ENDIF.

      e_object->get_functions( IMPORTING fcodes = DATA(fcodes) ).

      LOOP AT fcodes INTO DATA(fcode) WHERE fcode NE '&OPTIMIZE'.
        func = fcode-fcode.
        APPEND func TO funcs.
      ENDLOOP.

      e_object->hide_functions( funcs ).
      e_object->add_separator( ).

      IF l_sel-range[]  IS NOT INITIAL OR l_index IS INITIAL.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'SEL_CLEAR'
            text  = 'Clear Select-Options'.
      ENDIF.
  endmethod.
  method HANDLE_DOUBLECLICK.
      DATA: it_bdcdata TYPE TABLE OF  bdcdata.

      CHECK es_row_no-row_id IS NOT INITIAL.

      READ TABLE mt_sel_tab INDEX es_row_no-row_id INTO DATA(l_sel).
      APPEND VALUE #( program = 'SAPLSD_ENTRY' dynpro = '1000' dynbegin = abap_true ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WB_DISPLAY' ) TO it_bdcdata.

      IF e_column = 'ELEMENT'.
        SET PARAMETER ID 'DTYP' FIELD l_sel-element.
        APPEND VALUE #( fnam = 'RSRD1-DDTYPE' fval = abap_true ) TO it_bdcdata.
        CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
      ELSEIF e_column = 'DOMAIN'.
        SET PARAMETER ID 'DOM' FIELD l_sel-domain.
        APPEND VALUE #( fnam = 'RSRD1-DOMA' fval = abap_true ) TO it_bdcdata.
        CALL TRANSACTION 'SE11' USING it_bdcdata MODE 'E'.
      ELSE.
        CALL FUNCTION 'DOCU_CALL'
          EXPORTING
            id                = 'DE'
            langu             = mo_viewer->m_lang
            object            = l_sel-element
            typ               = 'E'
            displ             = abap_true
            displ_mode        = 3
            use_sec_langu     = abap_true
            display_shorttext = abap_true.
      ENDIF.
  endmethod.
  method HANDLE_SEL_TOOLBAR.
      e_object->mt_toolbar[] = VALUE #( butn_type = 0 disabled = ''
       ( function = 'SEL_OFF' icon = icon_arrow_right    quickinfo = 'Hide' )
       ( function = 'SEL_CLEAR' icon = icon_delete_row    quickinfo = 'Clear Select-Options' ) ).
  endmethod.
  method HANDLE_USER_COMMAND.
      DATA:  sel_width TYPE i.

      IF e_ucomm = 'SEL_OFF'.

        mo_viewer->m_visible = ''.

        sel_width = 0.
        CALL METHOD mo_viewer->mo_splitter->get_column_width
          EXPORTING
            id                = 1
          IMPORTING
            result            = mo_viewer->mo_sel_width
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3.

        CALL METHOD mo_viewer->mo_splitter->set_column_width
          EXPORTING
            id    = 1
            width = sel_width.
        mo_viewer->mo_alv->set_toolbar_interactive( ).
        RETURN.
      ENDIF.

      IF e_ucomm = 'SEL_CLEAR' OR e_ucomm = 'DELR'.
        mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).

        LOOP AT sel_rows INTO DATA(l_row).
          READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX l_row-index.
          IF e_ucomm = 'SEL_CLEAR'.
            CLEAR : <sel>-low, <sel>-high, <sel>-sign, <sel>-opti, <sel>-range.
          ENDIF.
          update_sel_row( CHANGING c_sel_row = <sel> ).
        ENDLOOP.
        RAISE EVENT selection_done.
      ENDIF.

      ZCL_ACE_ALV_COMMON=>refresh( mo_viewer->mo_alv ).
      RAISE EVENT selection_done.
  endmethod.
  method INIT_FCAT.
      mt_fcat = VALUE #(
       ( fieldname = 'IND'         coltext = '№'  outputlen = 3 style = '00000003' )
       ( fieldname = 'FIELD_LABEL' coltext = 'Label'  outputlen = 30  )
       ( fieldname = 'SIGN'        coltext = 'SIGN'   tech = abap_true )
       ( fieldname = 'OPTI'        coltext = 'Option' tech = abap_true )
       ( fieldname = 'OPTION_ICON' coltext = 'Option' icon = abap_true outputlen = 4 style = cl_gui_alv_grid=>mc_style_button )
       ( fieldname = 'LOW'         coltext = 'From data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4 col_opt = abap_true  )
       ( fieldname = 'HIGH'        coltext = 'To data' edit = abap_true lowercase = abap_true outputlen = 45 style = cl_gui_alv_grid=>mc_style_f4  col_opt = abap_true )
       ( fieldname = 'MORE_ICON'   coltext = 'Range' icon = abap_true  style = cl_gui_alv_grid=>mc_style_button  )
       ( fieldname = 'RANGE'   tech = abap_true  )
       ( fieldname = 'INHERITED'   coltext = 'Inh.' icon = abap_true outputlen = 4 seltext = 'Inherited' style = '00000003')
       ( fieldname = 'EMITTER'    coltext = 'Emit.' icon = abap_true outputlen = 4 seltext = 'Emitter' style = '00000003')
       ( fieldname = 'NAME' coltext = 'Field name'  outputlen = 60 style = '00000003')
       ( fieldname = 'ELEMENT' coltext = 'Data element'  outputlen = 15 style = '00000209' )
       ( fieldname = 'DOMAIN'  coltext = 'Domain'  outputlen = 15 style = '00000209' )
       ( fieldname = 'DATATYPE' coltext = 'Type'  outputlen = 5 style = '00000003')
       ( fieldname = 'LENGTH' coltext = 'Length'  outputlen = 5 style = '00000003')
       ( fieldname = 'TRANSMITTER'   tech = abap_true  )
       ( fieldname = 'RECEIVER'    tech = abap_true  )
       ( fieldname = 'COLOR'    tech = abap_true  ) ).
  endmethod.
  method ON_DATA_CHANGED.
      DATA: l_start TYPE i,
            time    TYPE sy-uzeit.

      FIELD-SYMBOLS: <field> TYPE any.

      LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_cells>).
        READ TABLE mt_sel_tab INDEX <ls_cells>-row_id ASSIGNING FIELD-SYMBOL(<tab>).
        ASSIGN COMPONENT <ls_cells>-fieldname OF STRUCTURE <tab> TO <field>.
        READ TABLE mo_viewer->mt_alv_catalog WITH KEY fieldname = <tab>-field_label INTO DATA(l_cat).

        IF <field> IS NOT INITIAL AND <ls_cells>-value IS INITIAL.
          READ TABLE <tab>-range INTO DATA(l_second) INDEX 2.
          IF sy-subrc = 0.
            IF ( <ls_cells>-fieldname = 'LOW' AND <tab>-high IS INITIAL ) OR  ( <ls_cells>-fieldname = 'HIGH' AND <tab>-low IS INITIAL  ).
              DELETE <tab>-range INDEX 1.
            ELSE.
              CLEAR l_second.
            ENDIF.
          ENDIF.
        ENDIF.

        IF l_cat-convexit = 'ALPHA' AND NOT  <ls_cells>-value CA '+*'.
          <ls_cells>-value = |{ <ls_cells>-value ALPHA = IN }|.
          l_start = 128 - l_cat-dd_outlen.
          <ls_cells>-value = <ls_cells>-value+l_start(l_cat-dd_outlen).
        ENDIF.

        IF <ls_cells>-value IS NOT INITIAL.
          IF <tab>-int_type = 'D'.
            DATA:  date TYPE sy-datum.
            CALL FUNCTION 'CONVERT_DATE_INPUT'
              EXPORTING
                input                     = <ls_cells>-value
                plausibility_check        = abap_true
              IMPORTING
                output                    = date
              EXCEPTIONS
                plausibility_check_failed = 1
                wrong_format_in_input     = 2
                OTHERS                    = 3.

            IF sy-subrc = 0.
              <ls_cells>-value = |{  date DATE = USER }|.
            ENDIF.
          ELSEIF <tab>-int_type = 'T'.
            CALL FUNCTION 'CONVERT_TIME_INPUT'
              EXPORTING
                input                     = <ls_cells>-value
              IMPORTING
                output                    = time
              EXCEPTIONS
                plausibility_check_failed = 1
                wrong_format_in_input     = 2
                OTHERS                    = 3.
            <ls_cells>-value =  time+0(2) && ':' &&  time+2(2) && ':' &&  time+4(2).
          ENDIF.
        ENDIF.
      ENDLOOP.
      CHECK sy-subrc = 0.

      IF l_second IS INITIAL.
        <field> = <ls_cells>-value.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = <ls_cells>-fieldname i_value = <ls_cells>-value ).
      ELSE.
        <tab>-low = l_second-low.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'LOW' i_value = l_second-low ).
        IF l_second-high CO '0 '.
          CLEAR l_second-high.
        ENDIF.
        <tab>-high = l_second-high.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'HIGH' i_value = l_second-high ).

        <tab>-opti = l_second-opti.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'OPTI' i_value = l_second-opti ).
        <tab>-sign = l_second-sign.
        er_data_changed->modify_cell( EXPORTING i_row_id = <ls_cells>-row_id i_fieldname = 'SIGN' i_value = l_second-sign ).
      ENDIF.

      update_sel_row( CHANGING c_sel_row = <tab> ).
      ZCL_ACE_ALV_COMMON=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout ).
      raise_selection_done( ).
  endmethod.
  method ON_DATA_CHANGED_FINISHED.
      CHECK e_modified IS NOT INITIAL.
      RAISE EVENT selection_done.
  endmethod.
  method ON_F4.
      DATA: return_tab TYPE STANDARD TABLE OF ddshretval,
            objects    TYPE TABLE OF objec,
            objec      TYPE objec,
            l_otype    TYPE otype,
            l_plvar    TYPE plvar,
            l_multiple TYPE boolean,
            l_clear    TYPE boolean.

      IF e_fieldname = 'LOW'.
        l_multiple = abap_true.
      ENDIF.

      READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
      DATA(l_fname) =  <sel>-field_label.

      ZCL_ACE=>mt_sel[] = mt_sel_tab[].
      IF <sel>-element = 'HROBJID'.
        READ TABLE mt_sel_tab INTO DATA(l_sel) WITH KEY field_label = 'OTYPE'.
        l_otype = l_sel-low.
        READ TABLE mt_sel_tab INTO l_sel WITH KEY field_label = 'PLVAR'.
        IF sy-subrc = 0 AND l_sel-low IS NOT INITIAL.
          l_plvar = l_sel-low.
        ELSE.
          CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
            IMPORTING
              act_plvar       = l_plvar
            EXCEPTIONS
              no_active_plvar = 1
              OTHERS          = 2.
        ENDIF.
      ELSEIF <sel>-element = 'PERSNO'.
        l_otype = 'P'.
      ENDIF.

      IF l_otype IS NOT INITIAL.
        CALL FUNCTION 'RH_OBJID_REQUEST'
          EXPORTING
            plvar            = l_plvar
            otype            = l_otype
            seark_begda      = sy-datum
            seark_endda      = sy-datum
            dynpro_repid     = sy-repid
            dynpro_dynnr     = sy-dynnr
            set_mode         = l_multiple
          IMPORTING
            sel_object       = objec
          TABLES
            sel_hrobject_tab = objects
          EXCEPTIONS
            OTHERS           = 6.
        IF sy-subrc = 0.
          l_clear = abap_true.
          LOOP AT objects INTO objec.
            IF e_fieldname = 'LOW'.
              set_value( EXPORTING i_field = <sel>-field_label i_low = objec-objid i_clear = l_clear ).
              CLEAR l_clear.
            ELSE.
              set_value( EXPORTING i_field = <sel>-field_label i_high = objec-objid i_clear = l_clear ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.

        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = mo_viewer->m_tabname
            fieldname         = l_fname
            callback_program  = sy-repid
            callback_form     = 'CALLBACK_F4_SEL'
            multiple_choice   = l_multiple
          TABLES
            return_tab        = return_tab
          EXCEPTIONS
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            OTHERS            = 5.

        IF sy-subrc = 0 AND lines( return_tab ) > 0.
          ASSIGN er_event_data->m_data->* TO FIELD-SYMBOL(<itab>).
          CLEAR <sel>-range.
          l_clear = abap_true.
          LOOP AT return_tab ASSIGNING FIELD-SYMBOL(<ret>) WHERE fieldname = l_fname.
            IF e_fieldname = 'LOW'.
              set_value( EXPORTING i_field = <sel>-field_label i_low = <ret>-fieldval i_clear = l_clear ).
              CLEAR l_clear.
            ELSE.
              set_value( EXPORTING i_field = <sel>-field_label i_high = <ret>-fieldval ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      er_event_data->m_event_handled = abap_true.
      raise_selection_done( ).
  endmethod.
  method ON_GRID_BUTTON_CLICK.
      DATA: l_tabfield TYPE rstabfield,
            opt        TYPE rsoptions VALUE 'XXXXXXXXXX',
            sign       TYPE raldb_sign,
            option     TYPE raldb_opti.

      READ TABLE mt_sel_tab INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
      CASE es_col_id.
        WHEN 'OPTION_ICON'.
          CALL FUNCTION 'SELECT_OPTION_OPTIONS'
            EXPORTING
              selctext     = 'nnnn'
              option_list  = opt
            IMPORTING
              sign         = sign
              option       = option
            EXCEPTIONS
              delete_line  = 1
              not_executed = 2
              illegal_sign = 3
              OTHERS       = 4.
          IF sy-subrc = 0.
            <tab>-sign =  sign.
            <tab>-opti =  option.
          ELSEIF sy-subrc = 1.
            CLEAR: <tab>-low, <tab>-high,<tab>-sign, <tab>-opti, <tab>-range.
          ENDIF.
        WHEN 'MORE_ICON'.
          l_tabfield-tablename = mo_viewer->m_tabname.
          l_tabfield-fieldname = <tab>-field_label.

          CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
            EXPORTING
              title             = 'title'
              text              = 'text'
              tab_and_field     = l_tabfield
            TABLES
              range             = <tab>-range
            EXCEPTIONS
              no_range_tab      = 1
              cancelled         = 2
              internal_error    = 3
              invalid_fieldname = 4
              OTHERS            = 5.
          IF sy-subrc = 0.
            READ TABLE <tab>-range INDEX 1 INTO DATA(l_range).
            MOVE-CORRESPONDING l_range TO <tab>.
            IF <tab>-opti NE 'BT'.
              CLEAR <tab>-high.
            ENDIF.
          ENDIF.
      ENDCASE.
      update_sel_row( CHANGING c_sel_row = <tab> ).
      RAISE EVENT selection_done.
  endmethod.
  method INIT_ICONS_TABLE.
    " Populate sign/option icon mapping table used in UPDATE_SEL_ROW
    ZCL_ACE=>m_option_icons = VALUE #(
     ( sign = space option = space  icon_name = icon_led_inactive )
     ( sign = 'I' option = 'EQ' icon_name = icon_equal_green )
     ( sign = 'I' option = 'NE' icon_name = icon_not_equal_green )
     ( sign = 'I' option = 'LT' icon_name = icon_less_green )
     ( sign = 'I' option = 'LE' icon_name = icon_less_equal_green )
     ( sign = 'I' option = 'GT' icon_name = icon_greater_green )
     ( sign = 'I' option = 'GE' icon_name = icon_greater_equal_green )
     ( sign = 'I' option = 'CP' icon_name = icon_pattern_include_green )
     ( sign = 'I' option = 'NP' icon_name = icon_pattern_exclude_green )
     ( sign = 'I' option = 'BT' icon_name = icon_interval_include_green )
     ( sign = 'I' option = 'NB' icon_name = icon_interval_exclude_green )
     ( sign = 'E' option = 'EQ' icon_name = icon_equal_red )
     ( sign = 'E' option = 'NE' icon_name = icon_not_equal_red )
     ( sign = 'E' option = 'LT' icon_name = icon_less_red )
     ( sign = 'E' option = 'LE' icon_name = icon_less_equal_red )
     ( sign = 'E' option = 'GT' icon_name = icon_greater_red )
     ( sign = 'E' option = 'GE' icon_name = icon_greater_equal_red )
     ( sign = 'E' option = 'CP' icon_name = icon_pattern_include_red )
     ( sign = 'E' option = 'NP' icon_name = icon_pattern_exclude_red )
     ( sign = 'E' option = 'BT' icon_name = icon_interval_include_red )
     ( sign = 'E' option = 'NB' icon_name = icon_interval_exclude_red ) ).
  endmethod.
  method RAISE_SELECTION_DONE.
      DATA: row TYPE ZCL_ACE=>t_sel_row.

      ZCL_ACE_ALV_COMMON=>refresh( mo_sel_alv ).
      RAISE EVENT selection_done.

  endmethod.
  method SET_VALUE.
      READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = i_field.
      CHECK sy-subrc = 0.
      IF i_low IS SUPPLIED.
        IF i_clear IS INITIAL.
          APPEND VALUE #( sign = 'I' opti = 'EQ' low = i_low high = i_high ) TO <to>-range.
        ELSE.
          CLEAR:  <to>-opti, <to>-sign,<to>-range.
          IF i_low IS SUPPLIED.
            <to>-low = i_low.
          ENDIF.
          IF i_high IS SUPPLIED.
            <to>-high = i_high.
          ENDIF.
          update_sel_row( CHANGING c_sel_row = <to> ).
        ENDIF.
      ELSE.
        CLEAR:  <to>-opti, <to>-sign.
        <to>-high = i_high.
        update_sel_row( CHANGING c_sel_row = <to> ).
      ENDIF.
  endmethod.
  method UPDATE_SEL_ROW.

      IF c_sel_row-high IS INITIAL AND c_sel_row-opti = 'BT'.
        CLEAR c_sel_row-opti.
      ENDIF.

      IF c_sel_row-low IS NOT INITIAL AND c_sel_row-opti IS INITIAL.
        c_sel_row-sign = 'I'.
        c_sel_row-opti = 'EQ'.
      ENDIF.

      IF c_sel_row-high IS NOT INITIAL AND c_sel_row-opti NE 'NB' .
        c_sel_row-opti = 'BT'.
      ENDIF.

      IF c_sel_row-sign IS INITIAL AND c_sel_row-opti IS INITIAL.
        CLEAR: c_sel_row-low, c_sel_row-low.
      ENDIF.

      IF c_sel_row-low CA  '*%+&' AND c_sel_row-opti <> 'NP'.
        c_sel_row-sign = 'I'.
        c_sel_row-opti = 'CP'.
      ENDIF.

      IF c_sel_row-opti IS NOT INITIAL AND c_sel_row-sign IS INITIAL.
        c_sel_row-sign = 'I'.
      ENDIF.

      TRY.
          c_sel_row-option_icon = ZCL_ACE=>m_option_icons[ sign = c_sel_row-sign option = c_sel_row-opti ]-icon_name.
        CATCH cx_sy_itab_line_not_found.                "#EC NO_HANDLER
      ENDTRY.

      IF c_sel_row-sign IS NOT INITIAL.
        READ TABLE c_sel_row-range ASSIGNING FIELD-SYMBOL(<range>) INDEX 1.
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO c_sel_row-range ASSIGNING <range>.
        ENDIF.
        MOVE-CORRESPONDING c_sel_row TO <range>.
        IF c_sel_row-opti NE 'BT' AND c_sel_row-opti NE 'NB' .
          CLEAR c_sel_row-high.
        ENDIF.
        IF c_sel_row-int_type = 'D' OR c_sel_row-int_type = 'T' .
          DO 2 TIMES.
            ASSIGN COMPONENT  COND string( WHEN sy-index = 1 THEN 'LOW' ELSE 'HIGH'  ) OF STRUCTURE <range> TO FIELD-SYMBOL(<field>).
            IF <field> IS INITIAL.
              CONTINUE.
            ENDIF.

            IF c_sel_row-int_type = 'D'.
              CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                EXPORTING
                  date_external           = <field>
                IMPORTING
                  date_internal           = <field>
                EXCEPTIONS
                  date_external_i_invalid = 1
                  OTHERS                  = 2.
            ELSE.
              REPLACE ALL OCCURRENCES OF ':' IN <field> WITH ''.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDIF.
      c_sel_row-more_icon = COND #( WHEN c_sel_row-range IS INITIAL THEN icon_enter_more    ELSE icon_display_more  ).
  endmethod.
  method UPDATE_SEL_TAB.
      IF mt_sel_tab[] IS NOT INITIAL.
        DATA(sel_tab_copy) = mt_sel_tab.
      ENDIF.
      CLEAR mt_sel_tab[].
      mo_viewer->mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mo_viewer->mt_alv_catalog ).
      LOOP AT mo_viewer->mt_alv_catalog INTO DATA(l_catalog) WHERE domname NE 'MANDT'.
        DATA(ind) = sy-tabix.
        APPEND INITIAL LINE TO mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel_tab>).
        READ TABLE sel_tab_copy INTO DATA(copy) WITH KEY field_label = l_catalog-fieldname.

        IF sy-subrc = 0.
          MOVE-CORRESPONDING copy TO <sel_tab>.
        ELSE.
          <sel_tab>-option_icon = icon_led_inactive.
          <sel_tab>-more_icon = icon_enter_more.
        ENDIF.

        <sel_tab>-ind =  ind.
        <sel_tab>-field_label = l_catalog-fieldname.
        <sel_tab>-int_type = l_catalog-inttype.
        <sel_tab>-element = l_catalog-rollname.
        <sel_tab>-domain =  l_catalog-domname.
        <sel_tab>-datatype = l_catalog-datatype.
        <sel_tab>-length = l_catalog-outputlen.
        ZCL_ACE_ALV_COMMON=>translate_field( EXPORTING i_lang = mo_viewer->m_lang CHANGING c_fld = l_catalog ).
        <sel_tab>-name = l_catalog-scrtext_l.
      ENDLOOP.
  endmethod.
ENDCLASS.

CLASS ZCL_ACE_RTTI_TREE IMPLEMENTATION.
  method ADD_BUTTONS.

      DATA(o_functions) = mo_tree->get_functions( ).
      o_functions->set_all( ).

      o_functions->set_group_layout( abap_false ).
      o_functions->set_group_aggregation( abap_false ).
      o_functions->set_group_print( abap_false ).

      CHECK mo_viewer IS NOT INITIAL AND i_type = 'L'.

      o_functions->add_function(
        name     = 'NAV_BACK'
        icon     = CONV #( icon_arrow_left )
        text     = ''
        tooltip  = 'Navigate Back'
        position = if_salv_c_function_position=>left_of_salv_functions ).

      o_functions->add_function(
        name     = 'NAV_FORWARD'
        icon     = CONV #( icon_arrow_right )
        text     = ''
        tooltip  = 'Navigate Forward'
        position = if_salv_c_function_position=>left_of_salv_functions ).

      o_functions->add_function(
        name     = 'REFRESH'
        icon     = CONV #( icon_refresh )
        text     = ''
        tooltip  = 'Refresh'
        position = if_salv_c_function_position=>left_of_salv_functions ).

  endmethod.
  method ADD_NODE.

      DATA(style) = COND salv_de_constant(
        WHEN i_tree-param IS NOT INITIAL THEN if_salv_c_tree_style=>intensified
        ELSE                                  if_salv_c_tree_style=>default ).

      rv_node =
            mo_tree->get_nodes( )->add_node(
              related_node   = i_rel
              collapsed_icon = i_icon
              expanded_icon  = i_icon
              relationship   = if_salv_c_node_relation=>last_child
              data_row       = i_tree
              row_style      = style
              text           = CONV #( i_name )
              folder         = abap_true
            )->get_key( ).

  endmethod.
  method CLEAR.

        mo_tree->get_nodes( )->delete_all( ).
      CLEAR mt_lazy_nodes.

  endmethod.
  method CONSTRUCTOR.

      super->constructor( ).
      mo_viewer = i_debugger.

      cl_salv_tree=>factory(
        EXPORTING
          r_container = i_cont
        IMPORTING
          r_salv_tree = mo_tree
        CHANGING
          t_table     = tree_table ).

      DATA(o_setting) =  mo_tree->get_tree_settings( ).
      o_setting->set_hierarchy_header( i_header ).
      o_setting->set_hierarchy_size( 30 ).
      o_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

      DATA(o_columns) = mo_tree->get_columns( ).
      o_columns->set_optimize( abap_true ).

      o_columns->get_column( 'VALUE' )->set_visible( abap_false ).
      o_columns->get_column( 'PARAM' )->set_visible( abap_false ).
      o_columns->get_column( 'INCLUDE' )->set_visible( abap_false ).
      o_columns->get_column( 'VAR_NAME' )->set_visible( abap_false ).

      add_buttons( i_type ).

      DATA(o_event) = mo_tree->get_event( ) .
      SET HANDLER hndl_double_click
                  hndl_expand_empty
                  hndl_user_command FOR o_event.

      mo_tree->display( ).

  endmethod.
  method DELETE_NODE.

      DATA(o_nodes) = mo_tree->get_nodes( ).
      DATA(l_node) =  o_nodes->get_node( i_key ).

      IF l_node IS NOT INITIAL.
        l_node->delete( ).
      ENDIF.

  endmethod.
  method DISPLAY.

      DATA(o_columns) = mo_tree->get_columns( ).
      o_columns->get_column( 'KIND'    )->set_visible( abap_false ).
      o_columns->get_column( 'PROGRAM' )->set_visible( abap_false ).
      o_columns->get_column( 'EV_TYPE' )->set_visible( abap_false ).
      o_columns->get_column( 'EV_NAME' )->set_visible( abap_false ).
      o_columns->get_column( 'ENH_ID'  )->set_visible( abap_false ).

      LOOP AT mt_lazy_nodes INTO DATA(lv_lazy_key).
        TRY.
            mo_tree->get_nodes( )->get_node( lv_lazy_key )->set_expander( abap_true ).
          CATCH cx_root.
        ENDTRY.
      ENDLOOP.

      TRY.
          mo_tree->get_nodes( )->get_node( main_node_key )->expand( ).
        CATCH cx_root.
      ENDTRY.

      TRY.
          DATA(o_nodes_obj) = mo_tree->get_nodes( ).
          DATA(lv_child_key) = o_nodes_obj->get_node( main_node_key )->get_first_child( )->get_key( ).
          DO.
            TRY.
                DATA(o_child) = o_nodes_obj->get_node( lv_child_key ).
                READ TABLE mt_lazy_nodes WITH KEY table_line = lv_child_key TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  o_child->expand( ).
                ENDIF.
                lv_child_key = o_child->get_next_sibling( )->get_key( ).
              CATCH cx_root.
                EXIT.
            ENDTRY.
          ENDDO.
        CATCH cx_root.
      ENDTRY.

      mo_tree->display( ).
      IF main_node_key IS NOT INITIAL.
        TRY.
            mo_tree->get_nodes( )->set_top_node( main_node_key ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.

  endmethod.
  method HNDL_DOUBLE_CLICK.
    " Dispatcher: routes double-click to a private handler based on node kind/type.
    " Refactored 2026-03: each branch extracted into its own private method.

    DATA(o_nodes) = mo_tree->get_nodes( ).
    DATA(o_node)  = o_nodes->get_node( node_key ).
    DATA r_row TYPE REF TO data.

    r_row = o_node->get_data_row( ).
    ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT 'KIND'     OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
    ASSIGN COMPONENT 'VALUE'    OF STRUCTURE <row> TO FIELD-SYMBOL(<value>).
    ASSIGN COMPONENT 'PARAM'    OF STRUCTURE <row> TO FIELD-SYMBOL(<param>).
    ASSIGN COMPONENT 'PROGRAM'  OF STRUCTURE <row> TO FIELD-SYMBOL(<program>).
    ASSIGN COMPONENT 'INCLUDE'  OF STRUCTURE <row> TO FIELD-SYMBOL(<include>).
    ASSIGN COMPONENT 'CLASS'    OF STRUCTURE <row> TO FIELD-SYMBOL(<class>).
    ASSIGN COMPONENT 'EV_TYPE'  OF STRUCTURE <row> TO FIELD-SYMBOL(<ev_type>).
    ASSIGN COMPONENT 'EV_NAME'  OF STRUCTURE <row> TO FIELD-SYMBOL(<ev_name>).
    ASSIGN COMPONENT 'ENH_ID'   OF STRUCTURE <row> TO FIELD-SYMBOL(<enh_id>).
    ASSIGN COMPONENT 'VAR_NAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<var_name>).

    " kind='C' = global class node — no navigation, no highlight
    CHECK <kind> <> 'C'.

    DATA(lv_param) = CONV string( <param> ).

    " --- Package root: reset Class Map focus back to the whole package ---
    IF mo_viewer->mv_package IS NOT INITIAL AND node_key = main_node_key.
      CLEAR mo_viewer->mv_cmap_focus.
      IF mo_viewer->mo_window->mo_mermaid IS NOT INITIAL
         AND mo_viewer->mo_window->mo_mermaid->mo_box IS NOT INITIAL.
        mo_viewer->mo_window->mo_mermaid->mv_type = 'CMAP'.
        mo_viewer->mo_window->mo_mermaid->refresh( ).
      ENDIF.
      RETURN.
    ENDIF.

    " --- Package object node (PKGOBJ:) — parse it, build subtree, focus Class Map on it ---
    IF lv_param IS NOT INITIAL AND strlen( lv_param ) > 7 AND lv_param+0(7) = 'PKGOBJ:'.
      DATA(lv_pkg_prog) = CONV progname( lv_param+7 ).
      load_package_object( i_node_key = node_key i_prog = lv_pkg_prog ).
      mo_viewer->mv_cmap_focus = lv_pkg_prog.
      READ TABLE mo_viewer->mt_pkg_objects INTO DATA(ls_pkg_focus_obj) WITH KEY prog = lv_pkg_prog.
      IF sy-subrc = 0 AND ls_pkg_focus_obj-obj_type = 'PROG'.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
        mo_viewer->mo_window->apply_depth( ).
      ENDIF.
      " If a diagram window is already open, live-rebuild the Class Map for this class
      IF mo_viewer->mo_window->mo_mermaid IS NOT INITIAL
         AND mo_viewer->mo_window->mo_mermaid->mo_box IS NOT INITIAL.
        mo_viewer->mo_window->mo_mermaid->mv_type = 'CMAP'.
        mo_viewer->mo_window->mo_mermaid->refresh( ).
      ENDIF.
      RETURN.
    ENDIF.

    " --- EVENT node (kind='E', ev_type='EVENT') ---
    IF <kind> = 'E' AND <ev_type> = 'EVENT' AND <ev_name> IS NOT INITIAL.
      dblclk_event(
        i_program = <program>
        i_include = <include>
        i_value   = <value>
        i_ev_name = <ev_name> ).
      RETURN.
    ENDIF.

    " --- VARS: lazy-load folder ---
    IF lv_param IS NOT INITIAL AND lv_param+0(5) = 'VARS:'.
      dblclk_vars_lazy(
        i_param   = lv_param
        i_program = <program>
        i_include = <include>
        i_ev_type = <ev_type>
        i_ev_name = <ev_name>
        i_kind    = <kind>
        io_node   = o_node ).
      RETURN.
    ENDIF.

    " --- ATTR: folder — no action (expansion handled by HNDL_EXPAND_EMPTY) ---
    IF lv_param IS NOT INITIAL AND lv_param+0(5) = 'ATTR:'.
      RETURN.
    ENDIF.

    " --- Structural markers (CLASS:/LCLASSES:): navigate without highlight ---
    IF lv_param IS NOT INITIAL AND ( lv_param+0(6) = 'CLASS:' OR lv_param+0(9) = 'LCLASSES:' ).
      IF <include> IS NOT INITIAL.
        mo_viewer->mo_window->set_program( CONV #( <include> ) ).
      ENDIF.
      mo_viewer->mo_window->set_program_line( CONV #( <value> ) ).
      RETURN.
    ENDIF.

    " --- Other lazy-load markers (GVARS/FORMS/MODS): no action ---
    IF lv_param IS NOT INITIAL AND (
        lv_param+0(6) = 'GVARS:' OR lv_param+0(6) = 'FORMS:' OR lv_param+0(5) = 'MODS:' ).
      RETURN.
    ENDIF.

    " --- Navigation nodes: resolve context ---
    READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
      WITH KEY program = <program> include = <include> eventname = <ev_name> eventtype = <ev_type>
      INTO mo_viewer->mo_window->ms_sel_call.

    IF <var_name> IS INITIAL AND ( <ev_type> = 'METHOD' OR <ev_type> = 'FORM' OR <ev_type> = 'MODULE' ).
      mo_viewer->mo_window->ms_code_context = VALUE #(
        evtype = <ev_type>
        evname = <ev_name>
        class  = mo_viewer->mo_window->ms_sel_call-class ).
    ENDIF.

    " --- MODULE (plain, no param) ---
    IF <kind> = 'M' AND <param> IS INITIAL AND <ev_type> = 'MODULE' AND <include> IS NOT INITIAL.
      dblclk_module( i_include = <include> i_value = <value> ).
      RETURN.
    ENDIF.

    " --- FORM plain (no param) ---
    IF <kind> = 'M' AND <param> IS INITIAL AND <ev_type> = 'FORM' AND <include> IS NOT INITIAL.
      dblclk_form_plain( i_include = <include> i_value = <value> ).
      RETURN.
    ENDIF.

    " --- METHOD plain (no param) ---
    IF <kind> = 'M' AND <param> IS INITIAL AND <ev_type> = 'METHOD' AND <include> IS NOT INITIAL.
      dblclk_method_plain(
        i_include = <include>
        i_ev_name = <ev_name>
        i_value   = <value> ).
      RETURN.
    ENDIF.

    " --- FORM enhancement (param IS NOT INITIAL) ---
    IF <kind> = 'M' AND <param> IS NOT INITIAL AND <ev_type> = 'FORM'.
      dblclk_form_enh(
        i_param  = <param>
        i_enh_id = <enh_id> ).
      RETURN.
    ENDIF.

    " --- METHOD enhancement (param IS NOT INITIAL) ---
    IF <kind> = 'M' AND <param> IS NOT INITIAL AND <ev_type> = 'METHOD'.
      dblclk_method_enh(
        i_param   = <param>
        i_ev_name = <ev_name>
        i_value   = <value>
        i_ev_type = <ev_type> ).
      RETURN.
    ENDIF.

    " --- Fallback: navigate + highlight var/param leaf ---
    dblclk_var_leaf(
      i_include  = <include>
      i_value    = <value>
      i_var_name = <var_name>
      i_class    = <class> "mo_viewer->mo_window->ms_sel_call-class
      i_ev_type  = <ev_type>
      i_ev_name  = <ev_name>
      io_node    = o_node ).

  endmethod.
  method DBLCLK_EVENT.
    DATA(lv_ev_program) = CONV program( i_program ).
    DATA(lv_ev_name)    = CONV string( i_ev_name ).
    mo_viewer->mo_window->ms_code_context = VALUE #(
      evtype = 'EVENT'
      evname = lv_ev_name ).
    CLEAR mo_viewer->mo_window->ms_sel_call.
    CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
    zcl_ace_source_parser=>code_execution_scanner(
      i_program   = lv_ev_program
      i_include   = lv_ev_program
      i_evtype    = 'EVENT'
      i_evname    = lv_ev_name
      io_debugger = mo_viewer ).
    mo_viewer->mo_window->show_coverage( ).
    mo_viewer->mo_window->show_stack( ).
    IF mo_viewer->mo_window->mo_mermaid IS NOT INITIAL
       AND mo_viewer->mo_window->mo_mermaid->mo_box IS NOT INITIAL.
      mo_viewer->mo_window->mo_mermaid->refresh( ).
    ENDIF.
    IF i_include IS NOT INITIAL.
      mo_viewer->mo_window->set_program( CONV #( i_include ) ).
    ENDIF.
    mo_viewer->mo_window->set_program_line( CONV #( i_value ) ).
  endmethod.
  method DBLCLK_VARS_LAZY.
    DATA ls_clear_row TYPE ZCL_ACE=>ts_tree.
    SPLIT i_param AT ':' INTO DATA(lv_pfx) DATA(lv_lazy_class) DATA(lv_lazy_meth).
    DATA(lv_lazy_prog) = CONV program( i_program ).
    ls_clear_row-kind    = i_kind.
    ls_clear_row-program = i_program.
    ls_clear_row-include = i_include.
    ls_clear_row-ev_type = i_ev_type.
    ls_clear_row-ev_name = i_ev_name.
    io_node->set_data_row( REF #( ls_clear_row ) ).
    DATA(lv_added) = 0.
    LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_ev)
      WHERE program = lv_lazy_prog AND class = lv_lazy_class AND eventtype = 'METHOD' AND eventname = lv_lazy_meth.
      add_node( i_name = lv_ev-name i_icon = lv_ev-icon i_rel = io_node->get_key( )
                i_tree = VALUE ZCL_ACE=>ts_tree( value = lv_ev-line include = lv_ev-include ) ).
      lv_added = lv_added + 1.
    ENDLOOP.
    IF lv_added > 0.
      TRY. io_node->expand( ). CATCH cx_root. ENDTRY.
    ENDIF.
  endmethod.
  method DBLCLK_MODULE.
    DATA(lv_mod_include) = CONV program( i_include ).
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_mod_include TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      ZCL_ACE_PARSER=>parse(
        EXPORTING i_program = lv_mod_include i_include = lv_mod_include
        CHANGING cs_source = mo_viewer->mo_window->ms_sources ).
    ENDIF.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_mod_include ASSIGNING FIELD-SYMBOL(<mod_prog>).
    IF sy-subrc = 0.
      LOOP AT mo_viewer->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<mp>). CLEAR <mp>-selected. ENDLOOP.
      <mod_prog>-selected = abap_true.
      mo_viewer->mo_window->m_prg-include = lv_mod_include.
      IF <mod_prog>-v_source IS NOT INITIAL.
        mo_viewer->mo_window->mo_code_viewer->set_text( table = <mod_prog>-v_source ).
        DATA(lv_mod_orig_line) = CONV i( i_value ).
        DATA(lv_mod_vline)     = lv_mod_orig_line.
        READ TABLE <mod_prog>-v_keywords
          WITH KEY include = lv_mod_include line = lv_mod_orig_line INTO DATA(ls_mod_vkw).
        IF sy-subrc = 0. lv_mod_vline = ls_mod_vkw-v_line. ENDIF.
        mo_viewer->mo_window->set_program_line( lv_mod_vline ).
      ELSE.
        mo_viewer->mo_window->mo_code_viewer->set_text( table = <mod_prog>-source_tab ).
        mo_viewer->mo_window->set_program_line( CONV #( i_value ) ).
      ENDIF.
    ENDIF.
  endmethod.
  method DBLCLK_FORM_PLAIN.
    DATA(lv_form_include) = CONV program( i_include ).
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_form_include TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      ZCL_ACE_PARSER=>parse(
        EXPORTING i_program = lv_form_include i_include = lv_form_include
        CHANGING cs_source = mo_viewer->mo_window->ms_sources ).
    ENDIF.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_form_include ASSIGNING FIELD-SYMBOL(<form_prog>).
    IF sy-subrc = 0.
      LOOP AT mo_viewer->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<fp>). CLEAR <fp>-selected. ENDLOOP.
      <form_prog>-selected = abap_true.
      mo_viewer->mo_window->m_prg-include = lv_form_include.
      IF <form_prog>-v_source IS NOT INITIAL.
        mo_viewer->mo_window->mo_code_viewer->set_text( table = <form_prog>-v_source ).
        DATA(lv_form_orig_line) = CONV i( i_value ).
        DATA(lv_form_vline)     = lv_form_orig_line.
        READ TABLE <form_prog>-v_keywords
          WITH KEY include = lv_form_include line = lv_form_orig_line INTO DATA(ls_form_vkw).
        IF sy-subrc = 0. lv_form_vline = ls_form_vkw-v_line. ENDIF.
        mo_viewer->mo_window->set_program_line( lv_form_vline ).
      ELSE.
        mo_viewer->mo_window->mo_code_viewer->set_text( table = <form_prog>-source_tab ).
        mo_viewer->mo_window->set_program_line( CONV #( i_value ) ).
      ENDIF.
    ENDIF.
  endmethod.
  method DBLCLK_METHOD_PLAIN.
    DATA(lv_cm_include) = CONV program( i_include ).
    DATA(lv_cm_method)  = CONV string( i_ev_name ).
    DATA(lv_cm_value)   = CONV i( i_value ).
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_cm_include TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      ZCL_ACE_PARSER=>parse(
        EXPORTING i_program = lv_cm_include i_include = lv_cm_include
        CHANGING cs_source = mo_viewer->mo_window->ms_sources ).
    ENDIF.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_cm_include INTO DATA(ls_cm_check).
    IF sy-subrc = 0 AND ls_cm_check-tt_enh_blocks IS INITIAL.
      ZCL_ACE_SOURCE_PARSER=>collect_enhancements( i_program = lv_cm_include io_debugger = mo_viewer ).
    ELSEIF sy-subrc <> 0.
      ZCL_ACE_SOURCE_PARSER=>collect_enhancements( i_program = lv_cm_include io_debugger = mo_viewer ).
    ENDIF.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_cm_include INTO DATA(ls_cm_prog2).
    IF sy-subrc = 0.
      READ TABLE ls_cm_prog2-tt_enh_blocks TRANSPORTING NO FIELDS
        WITH KEY ev_type = 'METHOD' ev_name = lv_cm_method.
      IF sy-subrc <> 0.
        mo_viewer->mo_window->set_program( lv_cm_include ).
        mo_viewer->mo_window->m_prg-include = lv_cm_include.
        mo_viewer->mo_window->set_program_line( lv_cm_value ).
        RETURN.
      ENDIF.
      DATA(lv_cm_meth_line) = 0.
      DATA(lv_cm_endm_line) = 0.
      LOOP AT ls_cm_prog2-t_keywords INTO DATA(ls_cm_kw) WHERE name = 'METHOD'.
        DATA(ls_cm_stmt) = ls_cm_prog2-scan->statements[ ls_cm_kw-index ].
        DATA(ls_cm_tok)  = ls_cm_prog2-scan->tokens[ ls_cm_stmt-from + 1 ].
        IF ls_cm_tok-str = lv_cm_method AND ls_cm_kw-line = lv_cm_value.
          lv_cm_meth_line = ls_cm_kw-line. EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_cm_meth_line = 0.
        LOOP AT ls_cm_prog2-t_keywords INTO ls_cm_kw WHERE name = 'METHOD'.
          ls_cm_stmt = ls_cm_prog2-scan->statements[ ls_cm_kw-index ].
          ls_cm_tok  = ls_cm_prog2-scan->tokens[ ls_cm_stmt-from + 1 ].
          IF ls_cm_tok-str = lv_cm_method. lv_cm_meth_line = ls_cm_kw-line. EXIT. ENDIF.
        ENDLOOP.
      ENDIF.
      IF lv_cm_meth_line > 0.
        LOOP AT ls_cm_prog2-t_keywords INTO DATA(ls_cm_kw2)
          WHERE name = 'ENDMETHOD' AND line > lv_cm_meth_line.
          lv_cm_endm_line = ls_cm_kw2-line. EXIT.
        ENDLOOP.
      ENDIF.
      IF lv_cm_meth_line > 0 AND lv_cm_endm_line > 0.
        DATA lt_cm_src  TYPE sci_include.
        DATA lt_virt_kw TYPE ZCL_ACE=>tt_kword.
        LOOP AT ls_cm_prog2-source_tab INTO DATA(lv_cm_line)
          FROM lv_cm_meth_line TO lv_cm_endm_line.
          APPEND lv_cm_line TO lt_cm_src.
          DATA(lv_virt_row)     = lines( lt_cm_src ).
          DATA(lv_cm_real_line) = lv_cm_meth_line + lv_virt_row - 1.
          LOOP AT ls_cm_prog2-t_keywords INTO DATA(ls_vkw_src) WHERE line = lv_cm_real_line.
            DATA(ls_vkw)   = ls_vkw_src.
            ls_vkw-include = lv_cm_include.
            ls_vkw-v_line  = lv_virt_row.
            APPEND ls_vkw TO lt_virt_kw.
          ENDLOOP.
        ENDLOOP.
        DATA: lt_cm_pre     TYPE sci_include,
              lt_cm_post    TYPE sci_include,
              lt_cm_pre_kw  TYPE ZCL_ACE=>tt_kword,
              lt_cm_post_kw TYPE ZCL_ACE=>tt_kword.
        LOOP AT ls_cm_prog2-tt_enh_blocks INTO DATA(ls_cm_eb)
          WHERE ev_type = 'METHOD' AND ev_name = lv_cm_method
            AND ( position = 'BEGIN' OR position = 'END' OR position = 'OVERWRITE' ).
          IF ls_cm_eb-position = 'OVERWRITE'.
            DATA(lv_enh_eimp_ow) = ls_cm_eb-enh_include.
            READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
              WITH KEY include = lv_enh_eimp_ow TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              ZCL_ACE_PARSER=>parse(
                EXPORTING i_program = CONV #( lv_enh_eimp_ow ) i_include = CONV #( lv_enh_eimp_ow )
                CHANGING cs_source = mo_viewer->mo_window->ms_sources ).
            ENDIF.
            READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
              WITH KEY include = lv_enh_eimp_ow INTO DATA(ls_enh_eimp_ow).
            IF sy-subrc = 0.
              DATA(lv_iow_name) = |IOW_| && ls_cm_eb-enh_name && '~' && lv_cm_method.
              DATA lv_in_iow    TYPE boolean.
              DATA lv_iow_first TYPE i.
              DATA lv_iow_last  TYPE i.
              CLEAR: lv_in_iow, lv_iow_first, lv_iow_last.
              LOOP AT ls_enh_eimp_ow-t_keywords INTO DATA(ls_ow_kw).
                IF ls_ow_kw-name = 'METHOD'.
                  DATA(ls_ow_stmt) = ls_enh_eimp_ow-scan->statements[ ls_ow_kw-index ].
                  DATA(ls_ow_tok)  = ls_enh_eimp_ow-scan->tokens[ ls_ow_stmt-from + 1 ].
                  IF ls_ow_tok-str = lv_iow_name OR ls_ow_tok-str CP |IOW_*~{ lv_cm_method }|.
                    lv_in_iow = abap_true. lv_iow_first = ls_ow_kw-line.
                    INSERT |* OVERWRITTEN BY ENHANCEMENT: | && ls_cm_eb-enh_name INTO lt_cm_src INDEX 1.
                    INSERT |* | && ls_enh_eimp_ow-source_tab[ ls_ow_kw-line ] INTO lt_cm_src INDEX 2.
                    LOOP AT lt_virt_kw ASSIGNING FIELD-SYMBOL(<vkw_ow>).
                      <vkw_ow>-v_line = <vkw_ow>-v_line + 2.
                    ENDLOOP.
                  ELSE.
                    lv_in_iow = abap_false.
                  ENDIF.
                ELSEIF ls_ow_kw-name = 'ENDMETHOD' AND lv_in_iow = abap_true.
                  lv_iow_last = ls_ow_kw-line. EXIT.
                ENDIF.
              ENDLOOP.
              IF lv_iow_first > 0 AND lv_iow_last > lv_iow_first.
                DATA lv_iow_idx TYPE i.
                lv_iow_idx = lv_iow_first.
                WHILE lv_iow_idx <= lv_iow_last.
                  DATA(lv_iow_src_line) = ls_enh_eimp_ow-source_tab[ lv_iow_idx ].
                  IF lv_iow_idx = lv_iow_first OR lv_iow_idx = lv_iow_last.
                    APPEND |* | && lv_iow_src_line TO lt_cm_src.
                  ELSE.
                    APPEND lv_iow_src_line TO lt_cm_src.
                  ENDIF.
                  LOOP AT ls_enh_eimp_ow-t_keywords INTO DATA(ls_iow_kw2) WHERE line = lv_iow_idx.
                    DATA(ls_iow_vkw)   = ls_iow_kw2.
                    ls_iow_vkw-include = lv_enh_eimp_ow.
                    ls_iow_vkw-v_line  = lines( lt_cm_src ).
                    APPEND ls_iow_vkw TO lt_virt_kw.
                  ENDLOOP.
                  lv_iow_idx = lv_iow_idx + 1.
                ENDWHILE.
              ENDIF.
            ENDIF.
            CONTINUE.
          ENDIF.
          DATA(lv_enh_eimp2) = ls_cm_eb-enh_include.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
            WITH KEY include = lv_enh_eimp2 TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            ZCL_ACE_PARSER=>parse(
              EXPORTING i_program = CONV #( lv_enh_eimp2 ) i_include = CONV #( lv_enh_eimp2 )
              CHANGING cs_source = mo_viewer->mo_window->ms_sources ).
          ENDIF.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
            WITH KEY include = lv_enh_eimp2 INTO DATA(ls_enh_eimp2).
          CHECK sy-subrc = 0.
          DATA(lv_impl_pfx2) = COND string(
            WHEN ls_cm_eb-position = 'BEGIN' THEN 'IPR_' ELSE 'IPO_' ).
          DATA(lv_impl_nm2) = lv_impl_pfx2 && ls_cm_eb-enh_name && '~' && lv_cm_method.
          DATA: lv_in_eb2 TYPE boolean, lt_eb2_lines TYPE sci_include,
                lt_eb2_kw TYPE ZCL_ACE=>tt_kword.
          DATA lv_eb2_first TYPE i.
          DATA lv_eb2_last  TYPE i.
          CLEAR: lv_in_eb2, lt_eb2_lines, lt_eb2_kw, lv_eb2_first, lv_eb2_last.
          LOOP AT ls_enh_eimp2-t_keywords INTO DATA(ls_ek2).
            IF ls_ek2-name = 'METHOD'.
              DATA(ls_ek2_stmt) = ls_enh_eimp2-scan->statements[ ls_ek2-index ].
              DATA(ls_ek2_tok)  = ls_enh_eimp2-scan->tokens[ ls_ek2_stmt-from + 1 ].
              IF ls_ek2_tok-str = lv_impl_nm2.
                lv_in_eb2 = abap_true. lv_eb2_first = ls_ek2-line.
              ELSE.
                lv_in_eb2 = abap_false.
              ENDIF.
              CONTINUE.
            ENDIF.
            IF ls_ek2-name = 'ENDMETHOD' AND lv_in_eb2 = abap_true.
              lv_eb2_last = ls_ek2-line. CLEAR lv_in_eb2.
            ENDIF.
          ENDLOOP.
          IF lv_eb2_first > 0 AND lv_eb2_last > lv_eb2_first.
            DATA lv_eb2_idx TYPE i.
            lv_eb2_idx = lv_eb2_first.
            WHILE lv_eb2_idx <= lv_eb2_last.
              DATA(lv_eb2_src) = ls_enh_eimp2-source_tab[ lv_eb2_idx ].
              IF lv_eb2_idx = lv_eb2_first OR lv_eb2_idx = lv_eb2_last.
                APPEND |* | && lv_eb2_src TO lt_eb2_lines.
              ELSE.
                APPEND lv_eb2_src TO lt_eb2_lines.
              ENDIF.
              LOOP AT ls_enh_eimp2-t_keywords INTO DATA(ls_ek2_kw) WHERE line = lv_eb2_idx.
                DATA(ls_eb2_vkw)   = ls_ek2_kw.
                ls_eb2_vkw-include = lv_enh_eimp2.
                ls_eb2_vkw-v_line  = lv_eb2_idx - lv_eb2_first + 1.
                APPEND ls_eb2_vkw TO lt_eb2_kw.
              ENDLOOP.
              lv_eb2_idx = lv_eb2_idx + 1.
            ENDWHILE.
          ENDIF.
          IF lt_eb2_lines IS NOT INITIAL.
            IF ls_cm_eb-position = 'BEGIN'.
              APPEND LINES OF lt_eb2_lines TO lt_cm_pre.
              APPEND LINES OF lt_eb2_kw    TO lt_cm_pre_kw.
            ELSE.
              APPEND LINES OF lt_eb2_lines TO lt_cm_post.
              APPEND LINES OF lt_eb2_kw    TO lt_cm_post_kw.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lt_cm_pre IS NOT INITIAL.
          DATA(lv_cm_ins) = 2.
          DATA(lv_pre_seq) = 0.
          LOOP AT lt_cm_pre INTO DATA(lv_p).
            INSERT lv_p INTO lt_cm_src INDEX lv_cm_ins.
            LOOP AT lt_virt_kw ASSIGNING FIELD-SYMBOL(<vkw_s>) WHERE v_line >= lv_cm_ins.
              <vkw_s>-v_line = <vkw_s>-v_line + 1.
            ENDLOOP.
            lv_pre_seq = lv_pre_seq + 1.
            LOOP AT lt_cm_pre_kw ASSIGNING FIELD-SYMBOL(<pkw>) WHERE v_line = lv_pre_seq.
              DATA(ls_pkw_ins)  = <pkw>.
              ls_pkw_ins-v_line = lv_cm_ins.
              APPEND ls_pkw_ins TO lt_virt_kw.
            ENDLOOP.
            lv_cm_ins = lv_cm_ins + 1.
          ENDLOOP.
        ENDIF.
        IF lt_cm_post IS NOT INITIAL.
          DATA(lv_cm_post_idx) = lines( lt_cm_src ).
          DATA(lv_post_seq) = 0.
          LOOP AT lt_cm_post INTO DATA(lv_pp).
            INSERT lv_pp INTO lt_cm_src INDEX lv_cm_post_idx.
            LOOP AT lt_virt_kw ASSIGNING FIELD-SYMBOL(<vkw_p>) WHERE v_line >= lv_cm_post_idx.
              <vkw_p>-v_line = <vkw_p>-v_line + 1.
            ENDLOOP.
            lv_post_seq = lv_post_seq + 1.
            LOOP AT lt_cm_post_kw ASSIGNING FIELD-SYMBOL(<pokw>) WHERE v_line = lv_post_seq.
              DATA(ls_pokw_ins)  = <pokw>.
              ls_pokw_ins-v_line = lv_cm_post_idx.
              APPEND ls_pokw_ins TO lt_virt_kw.
            ENDLOOP.
            lv_cm_post_idx = lv_cm_post_idx + 1.
          ENDLOOP.
        ENDIF.
        DELETE mo_viewer->mo_window->ms_sources-tt_progs WHERE include = 'VIRTUAL'.
        APPEND INITIAL LINE TO mo_viewer->mo_window->ms_sources-tt_progs
          ASSIGNING FIELD-SYMBOL(<virt_prog>).
        <virt_prog>-program    = mo_viewer->mo_window->m_prg-program.
        <virt_prog>-include    = 'VIRTUAL'.
        <virt_prog>-source_tab = lt_cm_src.
        <virt_prog>-t_keywords = lt_virt_kw.
        <virt_prog>-selected   = abap_true.
        LOOP AT mo_viewer->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog_cm>).
          IF <prog_cm>-include <> 'VIRTUAL'. CLEAR <prog_cm>-selected. ENDIF.
        ENDLOOP.
        mo_viewer->mo_window->m_prg-include = 'VIRTUAL'.
        mo_viewer->mo_window->set_program( 'VIRTUAL' ).
        mo_viewer->mo_window->set_program_line( 1 ).
        READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
          WITH KEY include = lv_cm_include eventname = lv_cm_method eventtype = 'METHOD'
          INTO DATA(ls_cm_cl).
        mo_viewer->mo_window->ms_code_context = VALUE #(
          evtype = 'METHOD'
          evname = lv_cm_method
          class  = ls_cm_cl-class ).
        RETURN.
      ENDIF.
    ENDIF.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
      WITH KEY include = lv_cm_include eventname = lv_cm_method eventtype = 'METHOD'
      INTO DATA(ls_sel_cl).
    mo_viewer->mo_window->ms_code_context = VALUE #(
      evtype = 'METHOD'
      evname = lv_cm_method
      class  = ls_sel_cl-class ).
    mo_viewer->mo_window->set_program( lv_cm_include ).
    mo_viewer->mo_window->set_program_line( lv_cm_value ).
  endmethod.
  method DBLCLK_FORM_ENH.
    DATA(lv_enh_include_f) = CONV program( i_param ).
    DATA(lv_form_enh_id)   = CONV i( i_enh_id ).
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_enh_include_f TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      ZCL_ACE_PARSER=>parse(
        EXPORTING i_program = CONV #( lv_enh_include_f ) i_include = CONV #( lv_enh_include_f )
        CHANGING cs_source = mo_viewer->mo_window->ms_sources ).
    ENDIF.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_enh_include_f INTO DATA(ls_form_enh_prog).
    IF sy-subrc = 0.
      DATA lt_form_virt_src  TYPE sci_include.
      DATA lt_form_virt_kw   TYPE ZCL_ACE=>tt_kword.
      DATA lv_in_enh_block   TYPE boolean.
      DATA lv_enh_start_line TYPE i.
      DATA lv_enh_end_line   TYPE i.
      CLEAR: lv_in_enh_block, lv_enh_start_line, lv_enh_end_line.
      LOOP AT ls_form_enh_prog-t_keywords INTO DATA(ls_fe_kw).
        IF ls_fe_kw-name = 'ENHANCEMENT'.
          READ TABLE ls_form_enh_prog-scan->statements INDEX ls_fe_kw-index INTO DATA(ls_fe_stmt).
          READ TABLE ls_form_enh_prog-scan->tokens INDEX ls_fe_stmt-from + 1 INTO DATA(ls_fe_tok).
          IF CONV i( ls_fe_tok-str ) = lv_form_enh_id.
            lv_in_enh_block = abap_true. lv_enh_start_line = ls_fe_kw-line.
          ELSE.
            lv_in_enh_block = abap_false.
          ENDIF.
          CONTINUE.
        ENDIF.
        IF ls_fe_kw-name = 'ENDENHANCEMENT' AND lv_in_enh_block = abap_true.
          lv_enh_end_line = ls_fe_kw-line. CLEAR lv_in_enh_block. CONTINUE.
        ENDIF.
      ENDLOOP.
      IF lv_enh_start_line > 0 AND lv_enh_end_line > 0.
        LOOP AT ls_form_enh_prog-source_tab INTO DATA(lv_fe_src_line)
          FROM lv_enh_start_line TO lv_enh_end_line.
          APPEND lv_fe_src_line TO lt_form_virt_src.
          DATA(lv_fe_vrow) = lines( lt_form_virt_src ).
          DATA(lv_fe_real) = lv_enh_start_line + lv_fe_vrow - 1.
          LOOP AT ls_form_enh_prog-t_keywords INTO DATA(ls_fe_vkw) WHERE line = lv_fe_real.
            DATA(ls_fe_out_kw)   = ls_fe_vkw.
            ls_fe_out_kw-include = lv_enh_include_f.
            ls_fe_out_kw-v_line  = lv_fe_vrow.
            APPEND ls_fe_out_kw TO lt_form_virt_kw.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
      DELETE mo_viewer->mo_window->ms_sources-tt_progs WHERE include = 'VIRTUAL'.
      APPEND INITIAL LINE TO mo_viewer->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<virt_f>).
      <virt_f>-program    = mo_viewer->mo_window->m_prg-program.
      <virt_f>-include    = 'VIRTUAL'.
      <virt_f>-source_tab = lt_form_virt_src.
      <virt_f>-t_keywords = lt_form_virt_kw.
      <virt_f>-selected   = abap_true.
      LOOP AT mo_viewer->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog_f>).
        IF <prog_f>-include <> 'VIRTUAL'. CLEAR <prog_f>-selected. ENDIF.
      ENDLOOP.
      mo_viewer->mo_window->m_prg-include = 'VIRTUAL'.
      mo_viewer->mo_window->set_program( 'VIRTUAL' ).
      mo_viewer->mo_window->set_program_line( 1 ).
    ENDIF.
  endmethod.
  method DBLCLK_METHOD_ENH.
    DATA(lv_eimp_include) = CONV program( i_param ).
    DATA(lv_eimp_method)  = CONV string( i_ev_name ).
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_eimp_include TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      ZCL_ACE_PARSER=>parse(
        EXPORTING i_program = CONV #( lv_eimp_include ) i_include = CONV #( lv_eimp_include )
        CHANGING cs_source = mo_viewer->mo_window->ms_sources ).
    ENDIF.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_eimp_include INTO DATA(ls_eimp_prog).
    IF sy-subrc = 0.
      DATA(lv_enh_pos)     = CONV string( i_value ).
      DATA(lv_impl_prefix) = COND string(
        WHEN lv_enh_pos = 'BEGIN'     AND i_ev_type = 'METHOD' THEN 'IPR_'
        WHEN lv_enh_pos = 'END'       AND i_ev_type = 'METHOD' THEN 'IPO_'
        WHEN lv_enh_pos = 'OVERWRITE' AND i_ev_type = 'METHOD' THEN 'IOW_'
        WHEN lv_enh_pos = 'BEGIN'     AND i_ev_type = 'FORM'   THEN 'IPF_'
        WHEN lv_enh_pos = 'END'       AND i_ev_type = 'FORM'   THEN 'IPF_'
        ELSE '' ).
      DATA(lv_cp_pattern) = COND string(
        WHEN lv_impl_prefix IS NOT INITIAL THEN lv_impl_prefix && |*~{ lv_eimp_method }|
        ELSE |*~| && lv_eimp_method ).
      DATA(lv_meth_line) = 0.
      DATA(lv_endm_line) = 0.
      LOOP AT ls_eimp_prog-t_keywords INTO DATA(ls_eimp_kw) WHERE name = 'METHOD'.
        DATA(ls_eimp_stmt) = ls_eimp_prog-scan->statements[ ls_eimp_kw-index ].
        DATA(ls_eimp_tok)  = ls_eimp_prog-scan->tokens[ ls_eimp_stmt-from + 1 ].
        IF ls_eimp_tok-str CP lv_cp_pattern. lv_meth_line = ls_eimp_kw-line. EXIT. ENDIF.
      ENDLOOP.
      IF lv_meth_line > 0.
        LOOP AT ls_eimp_prog-t_keywords INTO DATA(ls_eimp_kw2)
          WHERE name = 'ENDMETHOD' AND line > lv_meth_line.
          lv_endm_line = ls_eimp_kw2-line. EXIT.
        ENDLOOP.
        IF lv_endm_line > 0.
          DATA lt_meth_src TYPE sci_include.
          DATA lt_meth_kw  TYPE ZCL_ACE=>tt_kword.
          LOOP AT ls_eimp_prog-source_tab INTO DATA(lv_src_line)
            FROM lv_meth_line TO lv_endm_line.
            APPEND lv_src_line TO lt_meth_src.
            DATA(lv_meth_vrow) = lines( lt_meth_src ).
            DATA(lv_meth_real) = lv_meth_line + lv_meth_vrow - 1.
            LOOP AT ls_eimp_prog-t_keywords INTO DATA(ls_meth_kw) WHERE line = lv_meth_real.
              DATA(ls_meth_vkw)   = ls_meth_kw.
              ls_meth_vkw-include = lv_eimp_include.
              ls_meth_vkw-v_line  = lv_meth_vrow.
              APPEND ls_meth_vkw TO lt_meth_kw.
            ENDLOOP.
          ENDLOOP.
          DELETE mo_viewer->mo_window->ms_sources-tt_progs WHERE include = 'VIRTUAL'.
          APPEND INITIAL LINE TO mo_viewer->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<virt_e>).
          <virt_e>-program    = mo_viewer->mo_window->m_prg-program.
          <virt_e>-include    = 'VIRTUAL'.
          <virt_e>-source_tab = lt_meth_src.
          <virt_e>-t_keywords = lt_meth_kw.
          <virt_e>-selected   = abap_true.
          LOOP AT mo_viewer->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog_e>).
            IF <prog_e>-include <> 'VIRTUAL'. CLEAR <prog_e>-selected. ENDIF.
          ENDLOOP.
          mo_viewer->mo_window->m_prg-include = 'VIRTUAL'.
          mo_viewer->mo_window->set_program( 'VIRTUAL' ).
          mo_viewer->mo_window->set_program_line( 1 ).
          RETURN.
        ENDIF.
      ENDIF.
      mo_viewer->mo_window->set_program( CONV #( lv_eimp_include ) ).
      mo_viewer->mo_window->set_program_line( '1' ).
    ENDIF.
  endmethod.
  method DBLCLK_VAR_LEAF.
    IF i_include IS NOT INITIAL.
      mo_viewer->mo_window->set_program( CONV #( i_include ) ).
    ENDIF.
    mo_viewer->mo_window->set_program_line( CONV #( i_value ) ).
    DATA(lv_var_name) = CONV string( i_var_name ).
    IF lv_var_name IS NOT INITIAL.
      READ TABLE mo_viewer->mt_selected_var WITH KEY name = lv_var_name
        class = i_class eventtype = i_ev_type eventname = i_ev_name
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE mo_viewer->mt_selected_var WHERE name = lv_var_name
          AND class = i_class AND eventtype = i_ev_type AND eventname = i_ev_name.
        io_node->set_row_style( if_salv_c_tree_style=>default ).
      ELSE.
        io_node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
        APPEND INITIAL LINE TO mo_viewer->mt_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
        <sel>-name      = lv_var_name.
        <sel>-class     = i_class.
        <sel>-eventtype = i_ev_type.
        <sel>-eventname = i_ev_name.
        <sel>-i_sel     = abap_true.
      ENDIF.
    ENDIF.
  endmethod.
METHOD hndl_expand_empty.
    DATA(o_node) = mo_tree->get_nodes( )->get_node( node_key ).
    DATA r_row TYPE REF TO data.
    r_row = o_node->get_data_row( ).
    ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT 'PARAM'   OF STRUCTURE <row> TO FIELD-SYMBOL(<param>).
    ASSIGN COMPONENT 'PROGRAM' OF STRUCTURE <row> TO FIELD-SYMBOL(<program>).
    ASSIGN COMPONENT 'KIND'    OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
    ASSIGN COMPONENT 'EV_TYPE' OF STRUCTURE <row> TO FIELD-SYMBOL(<ev_type>).
    ASSIGN COMPONENT 'EV_NAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<ev_name>).
    ASSIGN COMPONENT 'INCLUDE' OF STRUCTURE <row> TO FIELD-SYMBOL(<include>).

    " kind='M', ev_type='METHOD', no param — lazy-load local vars for a method node
    IF <kind> = 'M' AND <ev_type> = 'METHOD' AND <param> IS INITIAL AND <ev_name> IS NOT INITIAL.
      DATA(lv_inc) = CONV program( <include> ).
      DATA(lv_mth) = CONV string( <ev_name> ).
      READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
        WITH KEY include = lv_inc eventtype = 'METHOD' eventname = lv_mth INTO DATA(ls_cl).
      CHECK sy-subrc = 0.
      zcl_ace_source_parser=>parse_call(
        i_index = ls_cl-index i_e_name = lv_mth i_e_type = 'METHOD'
        i_class = ls_cl-class i_program = ls_cl-program i_include = lv_inc
        i_stack = 0 i_no_steps = abap_true io_debugger = mo_viewer ).
      DATA lv_vcnt TYPE i.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars TRANSPORTING NO FIELDS
        WHERE program = ls_cl-program AND class = ls_cl-class
          AND eventtype = 'METHOD' AND eventname = lv_mth.
        lv_vcnt = lv_vcnt + 1.
      ENDLOOP.
      CHECK lv_vcnt > 0.
      DATA(lv_vnode) = add_node(
        i_name = |Local vars ({ lv_vcnt })| i_icon = CONV #( icon_folder ) i_rel = node_key
        i_tree = VALUE #( program = ls_cl-program include = lv_inc ev_type = 'VARS'
                          ev_name = lv_mth param = |VARS:{ ls_cl-class }:{ lv_mth }| ) ).
      expand_add_lazy( lv_vnode ).
      TRY. mo_tree->get_nodes( )->get_node( node_key )->expand( ). CATCH cx_root. ENDTRY.
      RETURN.
    ENDIF.

    CHECK <param> IS NOT INITIAL.
    DATA(lv_param) = CONV string( <param> ).
    DATA(lv_prog)  = CONV string( <program> ).
    DATA(lv_len)   = strlen( lv_param ).

    " Lazy-load a package object: parse it and build its subtree under this node
    IF lv_len > 7 AND substring( val = lv_param len = 7 ) = 'PKGOBJ:'.
      load_package_object( i_node_key = node_key
                           i_prog     = CONV #( substring( val = lv_param off = 7 ) ) ).
      RETURN.
    ENDIF.

    IF lv_len >= 5 AND substring( val = lv_param len = 5 ) = 'VARS:'.
      SPLIT lv_param AT ':' INTO DATA(lv_pfx) DATA(lv_class) DATA(lv_meth).
      expand_vars_method( i_node_key = node_key i_class = lv_class i_method = lv_meth i_program = lv_prog ).
      RETURN.
    ENDIF.

    IF lv_len > 10 AND substring( val = lv_param len = 10 ) = 'INTF_VARS:'.
      expand_intf_vars( i_node_key = node_key i_intf_name = substring( val = lv_param off = 10 ) ).
      RETURN.
    ENDIF.

    IF lv_len > 10 AND substring( val = lv_param len = 10 ) = 'LVARS:FORM'.
      SPLIT lv_param AT ':' INTO DATA(lv_lv_pfx) DATA(lv_lv_type) DATA(lv_lv_form).
      expand_lvars_form( i_node_key = node_key i_form = lv_lv_form i_program = lv_prog ).
      RETURN.
    ENDIF.

    IF lv_len >= 5 AND substring( val = lv_param len = 5 ) = 'ATTR:'.
      expand_attr( i_node_key = node_key i_class = substring( val = lv_param off = 5 ) ).
      RETURN.
    ENDIF.

    IF lv_len > 6 AND substring( val = lv_param len = 6 ) = 'INCLS:'.
      expand_incls( i_node_key = node_key i_main_prog = CONV #( substring( val = lv_param off = 6 ) ) ).
      RETURN.
    ENDIF.

    IF lv_param = 'GVARS:'.
      expand_gvars( i_node_key = node_key i_program = lv_prog ).
      RETURN.
    ENDIF.

    IF lv_len > 6 AND substring( val = lv_param len = 6 ) = 'FORMS:'.
      expand_forms( i_node_key = node_key i_prog = substring( val = lv_param off = 6 ) ).
      RETURN.
    ENDIF.

    IF lv_len > 5 AND substring( val = lv_param len = 5 ) = 'MODS:'.
      expand_mods( i_node_key = node_key i_prog = substring( val = lv_param off = 5 ) ).
      RETURN.
    ENDIF.

    IF lv_len > 9 AND substring( val = lv_param len = 9 ) = 'LCLASSES:'.
      expand_lclasses( i_node_key = node_key i_prog = substring( val = lv_param off = 9 ) ).
      RETURN.
    ENDIF.

    IF lv_len > 7 AND substring( val = lv_param len = 7 ) = 'LINTFS:'.
      expand_lintfs( i_node_key = node_key i_prog = substring( val = lv_param off = 7 ) ).
      RETURN.
    ENDIF.

    IF lv_len > 5 AND substring( val = lv_param len = 5 ) = 'SECT:'.
      DATA(lv_sect_rest) = substring( val = lv_param off = 5 ).
      SPLIT lv_sect_rest AT ':' INTO DATA(lv_sect_class) DATA(lv_sect_key).
      expand_sect( i_node_key = node_key i_class = lv_sect_class i_section = lv_sect_key ).
      RETURN.
    ENDIF.

    IF lv_len > 5 AND substring( val = lv_param len = 5 ) = 'INTF:'.
      expand_intf( i_node_key = node_key i_intf = substring( val = lv_param off = 5 ) ).
      RETURN.
    ENDIF.

    IF lv_len > 6 AND substring( val = lv_param len = 6 ) = 'CLASS:'.
      expand_class( i_node_key = node_key i_class = substring( val = lv_param off = 6 ) ).
      RETURN.
    ENDIF.
  ENDMETHOD.
  method HNDL_USER_COMMAND.

      CONSTANTS: c_mask TYPE x VALUE '01'.

      cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).

      CASE e_salv_function.

        WHEN 'NAV_BACK'.
          CHECK mo_viewer->mo_window->mv_nav_idx > 1.
          mo_viewer->mo_window->mv_nav_idx = mo_viewer->mo_window->mv_nav_idx - 1.
          DATA(ls_back) = mo_viewer->mo_window->mt_nav_history[ mo_viewer->mo_window->mv_nav_idx ].
          IF ls_back-include <> mo_viewer->mo_window->m_prg-include.
            mo_viewer->mo_window->m_prg-include = ls_back-include.
            mo_viewer->mo_window->mv_nav_silent = abap_true.
            mo_viewer->mo_window->set_program( ls_back-include ).
          ENDIF.
          mo_viewer->mo_window->mv_nav_silent = abap_true.
          mo_viewer->mo_window->set_program_line( ls_back-line ).
          mo_viewer->mo_window->set_nav_caption( ls_back-include ).

        WHEN 'NAV_FORWARD'.
          CHECK mo_viewer->mo_window->mv_nav_idx < lines( mo_viewer->mo_window->mt_nav_history ).
          mo_viewer->mo_window->mv_nav_idx = mo_viewer->mo_window->mv_nav_idx + 1.
          DATA(ls_fwd) = mo_viewer->mo_window->mt_nav_history[ mo_viewer->mo_window->mv_nav_idx ].
          IF ls_fwd-include <> mo_viewer->mo_window->m_prg-include.
            mo_viewer->mo_window->m_prg-include = ls_fwd-include.
            mo_viewer->mo_window->mv_nav_silent = abap_true.
            mo_viewer->mo_window->set_program( ls_fwd-include ).
          ENDIF.
          mo_viewer->mo_window->mv_nav_silent = abap_true.
          mo_viewer->mo_window->set_program_line( ls_fwd-line ).
          mo_viewer->mo_window->set_nav_caption( ls_fwd-include ).

        WHEN 'REFRESH'.
          mo_viewer->mo_tree_local->display( ).
          RETURN.

      ENDCASE.

  endmethod.
METHOD expand_add_lazy.
    APPEND i_node_key TO mt_lazy_nodes.
    TRY.
        mo_tree->get_nodes( )->get_node( i_node_key )->set_expander( abap_true ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.
METHOD load_package_object.
    " Parse a package object and build its subtree under the given node (once).
    mo_viewer->mo_window->m_prg-program = i_prog.
    mo_viewer->mo_window->m_prg-include = i_prog.
    mo_viewer->mo_window->set_program( CONV #( i_prog ) ).

    DATA(o_node) = mo_tree->get_nodes( )->get_node( i_node_key ).

    " Already loaded? just show the source, do not rebuild the subtree
    DATA lv_has_child TYPE abap_bool.
    TRY.
        IF o_node->get_first_child( ) IS BOUND. lv_has_child = abap_true. ENDIF.
      CATCH cx_root.
    ENDTRY.
    IF lv_has_child = abap_true. RETURN. ENDIF.

    NEW zcl_ace_tree_builder(
      io_window = mo_viewer->mo_window
      io_tree   = me )->build_object_subtree(
        i_root_key = i_node_key
        i_program  = i_prog ).

    LOOP AT mt_lazy_nodes INTO DATA(lv_lz).
      TRY.
          mo_tree->get_nodes( )->get_node( lv_lz )->set_expander( abap_true ).
        CATCH cx_root.
      ENDTRY.
    ENDLOOP.
    TRY.
        o_node->expand( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.
METHOD expand_attr.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
      WITH KEY class = i_class eventtype = 'METHOD' INTO DATA(ls_cl).
    CHECK sy-subrc = 0.
    LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_v)
      WHERE program = ls_cl-program AND class = i_class AND eventname IS INITIAL.
      add_node( i_name = lv_v-name i_icon = lv_v-icon i_rel = i_node_key
                i_tree = VALUE #( value = lv_v-line include = lv_v-include var_name = lv_v-name ) ).
    ENDLOOP.
  ENDMETHOD.
METHOD expand_class.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_class_defs WITH KEY class = i_class INTO DATA(ls_cd).
    DATA(lv_is_intf) = COND abap_bool( WHEN sy-subrc = 0 AND ls_cd-is_intf = abap_true THEN abap_true ).

    IF lv_is_intf = abap_true.
      DATA lv_cnt TYPE i.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars TRANSPORTING NO FIELDS
        WHERE class = i_class AND eventname IS INITIAL.
        lv_cnt = lv_cnt + 1.
      ENDLOOP.
      IF lv_cnt > 0.
        DATA(lv_memb) = add_node(
          i_name = |Members ({ lv_cnt })| i_icon = CONV #( icon_header ) i_rel = i_node_key
          i_tree = VALUE #( param = |INTF_VARS:{ i_class }| ) ).
        expand_add_lazy( lv_memb ).
      ENDIF.
    ELSE.
      " PUBLIC / PROTECTED / PRIVATE sections
      DATA(lv_sec_labels) = VALUE string_table(
        ( `Public Section` ) ( `Protected Section` ) ( `Private Section` ) ).
      DATA(lv_sec_keys) = VALUE string_table(
        ( `PUBLIC` ) ( `PROTECTED` ) ( `PRIVATE` ) ).
      DATA(lv_si) = 0.
      LOOP AT lv_sec_keys INTO DATA(lv_sec_key).
        lv_si = lv_si + 1.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_sections
          WITH KEY class = i_class section = lv_sec_key INTO DATA(ls_sec).
        IF sy-subrc = 0.
          add_node(
            i_name = lv_sec_labels[ lv_si ] i_icon = CONV #( icon_open_folder ) i_rel = i_node_key
            i_tree = VALUE #( kind = 'M' include = ls_sec-include value = ls_sec-line ) ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Methods
    LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_m)
      WHERE class = i_class AND eventtype = 'METHOD'.
      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = lv_m-include INTO DATA(lv_prog).
      READ TABLE lv_prog-t_keywords WITH KEY index = lv_m-index INTO DATA(lv_kw).
      DATA(lv_micon) = COND salv_de_tree_image(
        WHEN lv_is_intf = abap_true OR lv_m-is_intf = abap_true THEN CONV #( icon_oo_inst_method )
        WHEN lv_m-redefined = abap_false THEN
          COND #( WHEN lv_m-meth_type = 0 OR lv_m-meth_type = 1 THEN CONV #( icon_led_green )
                  WHEN lv_m-meth_type = 2                         THEN CONV #( icon_led_yellow )
                  WHEN lv_m-meth_type = 3                         THEN CONV #( icon_led_red )
                  WHEN lv_m-eventname = 'CONSTRUCTOR'             THEN CONV #( icon_tools )
                  ELSE                                                 CONV #( icon_led_green ) )
        ELSE CONV #( icon_oo_overwrite ) ).
      DATA(lv_mnode) = add_node(
        i_name = lv_m-eventname i_icon = lv_micon i_rel = i_node_key
        i_tree = VALUE #( kind = 'M' value = lv_kw-v_line include = lv_m-include
                          program = lv_m-program ev_type = lv_m-eventtype ev_name = lv_m-eventname ) ).
      LOOP AT mo_viewer->mo_window->ms_sources-t_params INTO DATA(lv_p)
        WHERE class = i_class AND event = 'METHOD' AND name = lv_m-eventname AND param IS NOT INITIAL.
        DATA(lv_picon) = COND salv_de_tree_image(
          WHEN lv_p-type = 'I' THEN CONV #( icon_parameter_import )
          ELSE                      CONV #( icon_parameter_export ) ).
        add_node( i_name = lv_p-param i_icon = lv_picon i_rel = lv_mnode
                  i_tree = VALUE #( value = lv_p-line include = lv_p-include var_name = lv_p-param ) ).
      ENDLOOP.
      IF lv_m-include IS NOT INITIAL.
        READ TABLE mo_viewer->mo_window->mt_calls
          WITH KEY include = lv_m-include ev_name = lv_m-eventname class = lv_m-class
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          zcl_ace_source_parser=>parse_call(
            i_index = lv_m-index i_e_name = lv_m-eventname i_e_type = 'METHOD'
            i_class = lv_m-class i_program = lv_m-program i_include = lv_m-include
            i_stack = 0 i_no_steps = abap_true io_debugger = mo_viewer ).
        ENDIF.
      ENDIF.
      DATA lv_vcnt TYPE i.
      CLEAR lv_vcnt.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars TRANSPORTING NO FIELDS
        WHERE program = lv_m-program AND class = i_class
          AND eventtype = 'METHOD' AND eventname = lv_m-eventname.
        lv_vcnt = lv_vcnt + 1.
      ENDLOOP.
      IF lv_vcnt > 0.
        DATA(lv_vnode) = add_node(
          i_name = |Local vars ({ lv_vcnt })| i_icon = CONV #( icon_folder ) i_rel = lv_mnode
          i_tree = VALUE #( program = lv_m-program include = lv_m-include ev_type = 'VARS'
                            ev_name = lv_m-eventname param = |VARS:{ i_class }:{ lv_m-eventname }| ) ).
        expand_add_lazy( lv_vnode ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
METHOD expand_forms.
    LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_fs)
      WHERE eventtype = 'FORM' AND program = i_prog.
      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = lv_fs-include INTO DATA(lv_fp).
      READ TABLE lv_fp-t_keywords WITH KEY index = lv_fs-index INTO DATA(lv_fkw).
      DATA(lv_fn) = add_node(
        i_name = lv_fs-eventname i_icon = CONV #( icon_biw_info_source_ina ) i_rel = i_node_key
        i_tree = VALUE #( kind = 'M' value = lv_fkw-v_line include = lv_fs-include
                          program = lv_fs-program ev_type = lv_fs-eventtype ev_name = lv_fs-eventname ) ).
      LOOP AT mo_viewer->mo_window->ms_sources-t_params INTO DATA(lv_fprm)
        WHERE event = 'FORM' AND name = lv_fs-eventname AND param IS NOT INITIAL.
        DATA(lv_ficon) = COND salv_de_tree_image(
          WHEN lv_fprm-type = 'I' THEN CONV #( icon_parameter_import )
          WHEN lv_fprm-type = 'E' THEN CONV #( icon_parameter_export )
          ELSE                         CONV #( icon_parameter_changing ) ).
        add_node( i_name = lv_fprm-param i_icon = lv_ficon i_rel = lv_fn
                  i_tree = VALUE #( param = lv_fprm-param ) ).
      ENDLOOP.
      DATA lv_fvar_cnt TYPE i.
      CLEAR lv_fvar_cnt.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars TRANSPORTING NO FIELDS
        WHERE program = lv_fs-program AND eventtype = 'FORM' AND eventname = lv_fs-eventname.
        lv_fvar_cnt = lv_fvar_cnt + 1.
      ENDLOOP.
      IF lv_fvar_cnt > 0.
        DATA(lv_fvn) = add_node(
          i_name = |Local vars ({ lv_fvar_cnt })| i_icon = CONV #( icon_header ) i_rel = lv_fn
          i_tree = VALUE #( param = |LVARS:FORM:{ lv_fs-eventname }| program = lv_fs-program ) ).
        expand_add_lazy( lv_fvn ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
METHOD expand_gvars.
    LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_v)
      WHERE program = i_program AND eventtype IS INITIAL AND class IS INITIAL.
      add_node( i_name = lv_v-name i_icon = lv_v-icon i_rel = i_node_key
                i_tree = VALUE #( value = lv_v-line include = lv_v-include var_name = lv_v-name ) ).
    ENDLOOP.
  ENDMETHOD.
METHOD expand_incls.
    LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(ls_p)
      WHERE program = i_main_prog AND include <> 'VIRTUAL' AND include <> i_main_prog.
      add_node( i_name = CONV #( ls_p-include ) i_icon = CONV #( icon_document )
                i_rel  = i_node_key
                i_tree = VALUE #( kind = 'M' include = ls_p-include program = ls_p-program value = 1 ) ).
    ENDLOOP.
  ENDMETHOD.
METHOD expand_intf.
    " Members node (vars)
    DATA lv_cnt TYPE i.
    LOOP AT mo_viewer->mo_window->ms_sources-t_vars TRANSPORTING NO FIELDS
      WHERE class = i_intf AND eventname IS INITIAL.
      lv_cnt = lv_cnt + 1.
    ENDLOOP.
    IF lv_cnt > 0.
      DATA(lv_memb) = add_node(
        i_name = |Members ({ lv_cnt })| i_icon = CONV #( icon_header ) i_rel = i_node_key
        i_tree = VALUE #( param = |INTF_VARS:{ i_intf }| ) ).
      expand_add_lazy( lv_memb ).
    ENDIF.
    " Methods
    LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_m)
      WHERE class = i_intf AND eventtype = 'METHOD'.
      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = lv_m-include INTO DATA(lv_prog).
      READ TABLE lv_prog-t_keywords WITH KEY index = lv_m-index INTO DATA(lv_kw).
      DATA(lv_mnode) = add_node(
        i_name = lv_m-eventname i_icon = CONV #( icon_oo_inst_method ) i_rel = i_node_key
        i_tree = VALUE #( kind = 'M' value = lv_kw-v_line include = lv_m-include
                          program = lv_m-program ev_type = lv_m-eventtype ev_name = lv_m-eventname ) ).
      LOOP AT mo_viewer->mo_window->ms_sources-t_params INTO DATA(lv_p)
        WHERE class = i_intf AND event = 'METHOD' AND name = lv_m-eventname AND param IS NOT INITIAL.
        DATA(lv_picon) = COND salv_de_tree_image(
          WHEN lv_p-type = 'I' THEN CONV #( icon_parameter_import )
          ELSE                      CONV #( icon_parameter_export ) ).
        add_node( i_name = lv_p-param i_icon = lv_picon i_rel = lv_mnode
                  i_tree = VALUE #( value = lv_p-line include = lv_p-include var_name = lv_p-param
                                    ev_type = lv_m-eventtype ev_name = lv_m-eventname ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
METHOD expand_intf_vars.
    LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_v)
      WHERE class = i_intf_name AND eventname IS INITIAL.
      add_node( i_name = lv_v-name i_icon = lv_v-icon i_rel = i_node_key
                i_tree = VALUE #( value = lv_v-line include = lv_v-include var_name = lv_v-name ) ).
    ENDLOOP.
  ENDMETHOD.
METHOD expand_lclasses.
    DATA lv_splits TYPE TABLE OF string.
    SPLIT i_prog AT '=' INTO TABLE lv_splits.
    DATA(lv_main) = lv_splits[ 1 ].
    LOOP AT mo_viewer->mo_window->ms_sources-tt_class_defs INTO DATA(lv_cd)
      WHERE is_intf = abap_false AND program = i_prog AND class <> lv_main.
      DATA(lv_inc)  = COND program( WHEN lv_cd-def_include IS NOT INITIAL THEN lv_cd-def_include ELSE lv_cd-include ).
      DATA(lv_node) = add_node(
        i_name = CONV #( lv_cd-class ) i_icon = CONV #( icon_folder ) i_rel = i_node_key
        i_tree = VALUE #( kind = 'M' value = lv_cd-def_line include = lv_inc
                          program = lv_cd-program param = |CLASS:{ lv_cd-class }| ) ).
      expand_add_lazy( lv_node ).
    ENDLOOP.
  ENDMETHOD.
METHOD expand_lintfs.
    DATA lv_splits TYPE TABLE OF string.
    SPLIT i_prog AT '=' INTO TABLE lv_splits.
    DATA(lv_main) = lv_splits[ 1 ].
    LOOP AT mo_viewer->mo_window->ms_sources-tt_class_defs INTO DATA(lv_cd)
      WHERE is_intf = abap_true AND program = i_prog AND class <> lv_main.
      DATA(lv_inc)  = COND program( WHEN lv_cd-def_include IS NOT INITIAL THEN lv_cd-def_include ELSE lv_cd-include ).
      DATA(lv_node) = add_node(
        i_name = CONV #( lv_cd-class ) i_icon = CONV #( icon_oo_connection ) i_rel = i_node_key
        i_tree = VALUE #( kind = 'M' value = lv_cd-def_line include = lv_inc
                          program = lv_cd-program param = |INTF:{ lv_cd-class }| ) ).
      expand_add_lazy( lv_node ).
    ENDLOOP.
  ENDMETHOD.
METHOD expand_lvars_form.
    LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_v)
      WHERE program = i_program AND eventtype = 'FORM' AND eventname = i_form.
      add_node( i_name = lv_v-name i_icon = lv_v-icon i_rel = i_node_key
                i_tree = VALUE #( value = lv_v-line include = lv_v-include var_name = lv_v-name ) ).
    ENDLOOP.
  ENDMETHOD.
METHOD expand_mods.
    LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_ms)
      WHERE eventtype = 'MODULE' AND program = i_prog.
      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = lv_ms-include INTO DATA(lv_mp).
      READ TABLE lv_mp-t_keywords WITH KEY index = lv_ms-index INTO DATA(lv_mkw).
      add_node(
        i_name = lv_ms-eventname i_icon = CONV #( icon_biw_info_source_ina ) i_rel = i_node_key
        i_tree = VALUE #( kind = 'M' value = lv_mkw-v_line include = lv_ms-include
                          program = lv_ms-program ev_type = lv_ms-eventtype ev_name = lv_ms-eventname ) ).
    ENDLOOP.
  ENDMETHOD.
METHOD expand_sect.
    LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_v)
      WHERE class = i_class AND section = i_section AND eventname IS INITIAL.
      add_node( i_name = lv_v-name i_icon = lv_v-icon i_rel = i_node_key
                i_tree = VALUE #( value = lv_v-line include = lv_v-include var_name = lv_v-name ) ).
    ENDLOOP.
  ENDMETHOD.
METHOD expand_vars_method.
    READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
      WITH KEY class = i_class eventtype = 'METHOD' eventname = i_method
      INTO DATA(ls_cl).
    IF sy-subrc = 0.
      zcl_ace_source_parser=>parse_call(
        i_index = ls_cl-index i_e_name = i_method i_e_type = 'METHOD'
        i_class = i_class i_program = ls_cl-program i_include = ls_cl-include
        i_stack = 0 i_no_steps = abap_true io_debugger = mo_viewer ).
    ENDIF.
    LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_v)
      WHERE program = i_program AND class = i_class
        AND eventtype = 'METHOD' AND eventname = i_method.
      add_node( i_name = lv_v-name i_icon = lv_v-icon i_rel = i_node_key
                i_tree = VALUE #( value = lv_v-line include = lv_v-include var_name = lv_v-name
                                  class = i_class ev_type = 'METHOD' ev_name = i_method ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS ZCL_ACE_RTTI IMPLEMENTATION.
  method CREATE_STRUC_HANDLE.

      cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = i_tname
                                           RECEIVING  p_descr_ref    = DATA(o_descr)
                                           EXCEPTIONS type_not_found = 1 ).
      IF sy-subrc = 0.
        e_handle ?= o_descr.
      ELSE.
        RETURN.
      ENDIF.
  endmethod.
  method CREATE_TABLE_BY_NAME.
      DATA: o_new_tab  TYPE REF TO cl_abap_tabledescr,
            o_new_type TYPE REF TO cl_abap_structdescr.

      create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_handle = o_new_type ).
      o_new_tab = cl_abap_tabledescr=>create(
        p_line_type  = o_new_type
        p_table_kind = cl_abap_tabledescr=>tablekind_std
        p_unique     = abap_false ).
      CREATE DATA c_table TYPE HANDLE o_new_tab.  "Create a New table type

  endmethod.
ENDCLASS.

CLASS ZCL_ACE_POPUP IMPLEMENTATION.
  method CONSTRUCTOR.

      m_additional_name = i_additional_name.
  endmethod.
  method CREATE.
      DATA: l_top  TYPE i,
            l_left TYPE i.

      ADD 1 TO m_counter.
      l_top  = l_left =  50 -  5 * ( m_counter DIV 5 ) - ( m_counter MOD 5 ) * 5.
      CREATE OBJECT ro_box
        EXPORTING
          width                       = i_width
          height                      = i_hight
          top                         = l_top
          left                        = l_left
          caption                     = i_name
          lifetime                    = 2
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          event_already_registered    = 6
          error_regist_event          = 7
          OTHERS                      = 8.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
  endmethod.
  method ON_BOX_CLOSE.

      LOOP AT ZCL_ACE=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>) WHERE parent = sender .
        <popup>-child->free( ).
        CLEAR <popup>-child.
      ENDLOOP.
      IF sy-subrc <> 0.
        DELETE  ZCL_ACE=>mt_popups WHERE child = sender.
      ENDIF.
      DELETE ZCL_ACE=>mt_popups WHERE child IS INITIAL.
      sender->free( ).
      CLEAR mo_box.

  endmethod.
ENDCLASS.

CLASS ZCL_ACE_PARSE_VARS IMPLEMENTATION.
  METHOD zif_ace_stmt_handler~handle.

    data: lv_kw(20).
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX stmt-from INTO DATA(kw_tok).
    CHECK sy-subrc = 0 and kw_tok-str <> 'TYPES'.
    lv_kw = kw_tok-str.

*  IF kw_tok-row = 7585.
*    BREAK-POINT.
*  ENDIF.
    " --- Context tracking ---
    mv_class_name = i_class.
    mv_eventname = i_ev_name.
    mv_eventtype = i_evtype.
    IF i_section IS SUPPLIED.
      mv_section = i_section.
    ENDIF.

    DATA(lv_line) = io_scan->tokens[ stmt-from ]-row.

    " ---------------------------------------------------------------
    " Обычные объявления: DATA / CLASS-DATA / PARAMETERS / SELECT-OPTIONS
    " ---------------------------------------------------------------
    DATA: lv_type       TYPE string,
          lv_ref        TYPE abap_bool,
          lv_after_type TYPE abap_bool,
          lv_for_next   TYPE abap_bool.

    CASE lv_kw.

      WHEN 'PARAMETERS'.
        READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO data(var_tok).
        CHECK sy-subrc = 0 AND var_tok-str IS NOT INITIAL.
        data(lv_name) = var_tok-str.
        LOOP AT io_scan->tokens FROM stmt-from + 2 TO stmt-to INTO data(dtok).
          IF dtok-str = 'CHECKBOX'.
            append_var( EXPORTING i_name    = lv_name
                                  i_type    = 'CHECKBOX'
                                  i_icon    = CONV #( icon_checked )
                                  i_line    = lv_line
                                  i_program = i_program
                                  i_include = i_include
                        CHANGING  cs_source = cs_source ).
            RETURN.
          ENDIF.
          IF dtok-str = 'TYPE' OR dtok-str = 'LIKE'.
            lv_after_type = abap_true. CONTINUE.
          ENDIF.
          IF lv_after_type = abap_true.
            IF dtok-str = 'REF'. lv_ref = abap_true. CONTINUE. ENDIF.
            IF dtok-str = 'TO'.  CONTINUE. ENDIF.
            lv_type = dtok-str. EXIT.
          ENDIF.
        ENDLOOP.
        CHECK lv_type IS NOT INITIAL.
        append_var( EXPORTING i_name    = lv_name
                              i_type    = lv_type
                              i_icon    = resolve_icon( i_type = lv_type i_ref = lv_ref )
                              i_line    = lv_line
                              i_program = i_program
                              i_include = i_include
                    CHANGING  cs_source = cs_source ).

      WHEN 'SELECT-OPTIONS'.
        READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO var_tok.
        CHECK sy-subrc = 0 AND var_tok-str IS NOT INITIAL.
        lv_name = var_tok-str.
        LOOP AT io_scan->tokens FROM stmt-from + 2 TO stmt-to INTO dtok.
          IF dtok-str = 'FOR'. lv_for_next = abap_true. CONTINUE. ENDIF.
          IF lv_for_next = abap_true. lv_type = dtok-str. EXIT. ENDIF.
        ENDLOOP.
        CHECK lv_type IS NOT INITIAL.
        append_var( EXPORTING i_name    = lv_name
                              i_type    = lv_type
                              i_icon    = CONV #( icon_select_all )
                              i_line    = lv_line
                              i_program = i_program
                              i_include = i_include
                    CHANGING  cs_source = cs_source ).

WHEN OTHERS.
        " Variable name — always token[2]
        READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO var_tok.
        CHECK sy-subrc = 0 AND var_tok-str IS NOT INITIAL.
        lv_name = var_tok-str.

        " Проверяем инлайн-декларацию: DATA( varname )
        "IF lv_name+0(1) = '(' OR lv_kw = 'DATA' AND lv_name CS '('.
        IF lv_kw+0(5) = 'DATA('.
          " Имя переменной внутри скобок
          DATA(lv_inline_name) = lv_kw.

          REPLACE ALL OCCURRENCES OF 'DATA(' IN lv_inline_name WITH ''.
          REPLACE ALL OCCURRENCES OF ')' IN lv_inline_name WITH ''.
          CONDENSE lv_inline_name NO-GAPS.
          IF lv_inline_name IS INITIAL.
            " имя в следующем токене
            READ TABLE io_scan->tokens INDEX stmt-from + 2 INTO DATA(var_tok2).
            IF sy-subrc = 0.
              lv_inline_name = var_tok2-str.
              REPLACE ALL OCCURRENCES OF ')' IN lv_inline_name WITH ''.
            ENDIF.
          ENDIF.
          CHECK lv_inline_name IS NOT INITIAL.

          " Ищем тип: NEW ClassName( или CAST ClassName(
          DATA lv_new_next TYPE abap_bool.
          DATA lv_cast_next TYPE abap_bool.
          DATA lv_inline_added TYPE abap_bool.
          LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(dtok_i).
            DATA(lv_up_i) = to_upper( dtok_i-str ).
            IF lv_new_next = abap_true OR lv_cast_next = abap_true.
              DATA(lv_cls_inline) = dtok_i-str.
              REPLACE ALL OCCURRENCES OF '(' IN lv_cls_inline WITH ''.
              IF lv_cls_inline IS NOT INITIAL AND lv_cls_inline <> '#'.
                append_var( EXPORTING i_name    = conv #( lv_inline_name )
                                      i_type    = to_upper( lv_cls_inline )
                                      i_icon    = resolve_icon( i_type = lv_cls_inline i_ref = abap_true )
                                      i_line    = lv_line
                                      i_program = i_program
                                      i_include = i_include
                            CHANGING  cs_source = cs_source ).
                lv_inline_added = abap_true.
              ENDIF.
              EXIT.
            ENDIF.
            IF lv_up_i = 'NEW' OR lv_up_i = 'CAST'.
              IF lv_up_i = 'NEW'.  lv_new_next  = abap_true. ENDIF.
              IF lv_up_i = 'CAST'. lv_cast_next = abap_true. ENDIF.
            ENDIF.
          ENDLOOP.
          " Простое присваивание DATA(lv_x) = expr — тип неизвестен, переменную всё равно регистрируем
          IF lv_inline_added = abap_false.
            append_var( EXPORTING i_name    = conv #( lv_inline_name )
                                  i_type    = ''
                                  i_icon    = resolve_icon( i_type = '' i_ref = abap_false )
                                  i_line    = lv_line
                                  i_program = i_program
                                  i_include = i_include
                        CHANGING  cs_source = cs_source ).
          ENDIF.
          RETURN.
        ENDIF.

        " Обычный DATA varname TYPE ...
        LOOP AT io_scan->tokens FROM stmt-from + 2 TO stmt-to INTO dtok.
          IF dtok-str = 'TYPE' OR dtok-str = 'LIKE'.
            lv_after_type = abap_true. CONTINUE.
          ENDIF.
          IF lv_after_type = abap_true.
            IF dtok-str = 'REF'. lv_ref = abap_true. CONTINUE. ENDIF.
            IF dtok-str = 'TO'.  CONTINUE. ENDIF.
            lv_type = dtok-str. EXIT.
          ENDIF.
        ENDLOOP.
        CHECK lv_type IS NOT INITIAL.
        append_var( EXPORTING i_name    = lv_name
                              i_type    = lv_type
                              i_icon    = resolve_icon( i_type = lv_type i_ref = lv_ref )
                              i_line    = lv_line
                              i_program = i_program
                              i_include = i_include
                    CHANGING  cs_source = cs_source ).
    ENDCASE.

  ENDMETHOD.
  METHOD append_var.
    READ TABLE cs_source-t_vars WITH KEY program   = i_program
                                         include   = i_include
                                         class     = mv_class_name
                                         eventtype = mv_eventtype
                                         eventname = mv_eventname
                                         name      = i_name
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      INSERT VALUE zcl_ace=>ts_vars(
        program   = i_program
        include   = i_include
        class     = mv_class_name
        eventtype = mv_eventtype
        eventname = mv_eventname
        section   = mv_section
        line      = i_line
        name      = i_name
        type      = i_type
        icon      = i_icon ) INTO table cs_source-t_vars.
    ENDIF.
  ENDMETHOD.
  METHOD resolve_icon.
    IF i_ref = abap_true.
      rv_icon = CONV #( icon_oo_class ).
      RETURN.
    ENDIF.
    rv_icon = SWITCH #( to_upper( i_type )
      WHEN 'STRING'                THEN CONV #( icon_text_act )
      WHEN 'D'                     THEN CONV #( icon_date )
      WHEN 'T'                     THEN CONV #( icon_bw_time_sap )
      WHEN 'C'                     THEN CONV #( icon_wd_input_field )
      WHEN 'P'                     THEN CONV #( icon_increase_decimal )
      WHEN 'N' OR 'I' OR 'INT4'
           OR 'INT8' OR 'F'        THEN CONV #( icon_pm_order )
      WHEN 'BOOLEAN' OR 'ABAP_BOOL'
           OR 'FLAG' OR 'BOOLE_D'  THEN CONV #( icon_checked )
      ELSE                              CONV #( icon_element ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ZCL_ACE_PARSE_PARAMS IMPLEMENTATION.
  METHOD zif_ace_stmt_handler~handle.

    CHECK i_stmt_idx > 0.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX stmt-from INTO DATA(kw_tok).
    CHECK sy-subrc = 0.

     mv_class_name = i_class.
    CASE kw_tok-str.

*      WHEN 'CLASS' OR 'INTERFACE'.
*        READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
*        IF sy-subrc = 0.
*          mv_class_name = name_tok-str.
*          mv_in_impl    = abap_false.
*          IF kw_tok-str = 'CLASS'.
*            LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(tok).
*              IF tok-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. RETURN. ENDIF.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.

*      WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
*        CLEAR: mv_class_name, mv_in_impl.

      WHEN 'METHODS' OR 'CLASS-METHODS'.
        "IF mv_in_impl = abap_false.
          parse_methods_stmt(
            EXPORTING io_scan    = io_scan
                      i_stmt_idx = i_stmt_idx
                      i_program  = i_program
                      i_include  = i_include
                      i_kw       = kw_tok-str
            CHANGING  cs_source  = cs_source ).
        "ENDIF.

      WHEN 'FORM'.
        parse_methods_stmt(
          EXPORTING io_scan    = io_scan
                    i_stmt_idx = i_stmt_idx
                    i_program  = i_program
                    i_include  = i_include
                    i_kw       = kw_tok-str
          CHANGING  cs_source  = cs_source ).

    ENDCASE.

  ENDMETHOD.
  METHOD parse_methods_stmt.

    DATA: lt_params LIKE cs_source-t_params.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.

    DATA(lv_ev_name)    = ``.
    DATA(lv_section)    = ``.
    DATA(lv_pname)      = ``.
    DATA(lv_ptype)      = ``.
    DATA(lv_ref)        = abap_false.
    DATA(lv_after_type) = abap_false.
    DATA(lv_is_form)    = xsdbool( i_kw = 'FORM' ).
    DATA(lv_last_row)   = 0.
    DATA(lv_preferred)  = ``.
    DATA(lv_skip_next)  = abap_false.

    DATA(lv_tok_idx) = stmt-from + 1.

    " FORM: method name is the second token
    IF lv_is_form = abap_true.
      READ TABLE io_scan->tokens INDEX lv_tok_idx INTO DATA(ftok).
      IF sy-subrc = 0.
        lv_ev_name  = ftok-str.
        lv_last_row = ftok-row.
        lv_tok_idx = lv_tok_idx + 1.
      ENDIF.
    ENDIF.

    DATA: ls_param TYPE zcl_ace=>ts_params.

    WHILE lv_tok_idx <= stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_idx INTO DATA(tok).
      IF sy-subrc <> 0. EXIT. ENDIF.
      lv_last_row = tok-row.
      DATA(lv_str) = tok-str.

      " --- Chain separator: METHODS meth1 ..., meth2 ...
      IF lv_str = ','.
        IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
          INSERT VALUE #(
            program = i_program  include = i_include
            class   = mv_class_name
            event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
            name    = lv_ev_name
            type    = SWITCH #( lv_section
                        WHEN 'IMPORTING' OR 'USING' THEN 'I'
                        WHEN 'EXPORTING'             THEN 'E'
                        WHEN 'CHANGING'              THEN 'C'
                        WHEN 'RETURNING'             THEN 'R'
                        ELSE 'I' )
            param   = lv_pname   line = tok-row )
            INTO TABLE lt_params.
        ENDIF.
        lv_tok_idx = lv_tok_idx + 1.
        READ TABLE io_scan->tokens INDEX lv_tok_idx INTO tok.
        IF sy-subrc = 0. lv_ev_name = tok-str. ENDIF.
        CLEAR: lv_section, lv_pname, lv_ptype, lv_ref, lv_after_type.
        lv_tok_idx = lv_tok_idx + 1.
        CONTINUE.
      ENDIF.

      CASE lv_str.

        WHEN 'IMPORTING' OR 'EXPORTING' OR 'CHANGING' OR 'RETURNING'
          OR 'USING' OR 'TABLES'.
          " Flush previous parameter before switching section
          IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
            INSERT VALUE #(
              program = i_program  include = i_include
              class   = mv_class_name
              event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
              name    = lv_ev_name
              type    = SWITCH #( lv_section
                          WHEN 'IMPORTING' OR 'USING' THEN 'I'
                          WHEN 'EXPORTING'             THEN 'E'
                          WHEN 'CHANGING'              THEN 'C'
                          WHEN 'RETURNING'             THEN 'R'
                          ELSE 'I' )
              param   = lv_pname   line = tok-row )
              INTO TABLE lt_params.
          ENDIF.
          lv_section = lv_str.
          CLEAR: lv_pname, lv_ptype, lv_ref, lv_after_type.

        WHEN 'RAISING' OR 'EXCEPTIONS'.
          " Flush previous parameter, then stop collecting params
          IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
            INSERT VALUE #(
              program = i_program  include = i_include
              class   = mv_class_name
              event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
              name    = lv_ev_name
              type    = SWITCH #( lv_section
                          WHEN 'IMPORTING' OR 'USING' THEN 'I'
                          WHEN 'EXPORTING'             THEN 'E'
                          WHEN 'CHANGING'              THEN 'C'
                          WHEN 'RETURNING'             THEN 'R'
                          ELSE 'I' )
              param   = lv_pname   line = tok-row )
              INTO TABLE lt_params.
          ENDIF.
          CLEAR: lv_section, lv_pname, lv_ptype, lv_ref, lv_after_type.

        WHEN 'TYPE' OR 'LIKE'.
          lv_after_type = abap_true.
          lv_ref        = abap_false.

        WHEN 'REF'.
          lv_ref = abap_true.

        WHEN 'TO' OR 'OPTIONAL' OR 'DEFAULT'
          OR 'ABSTRACT' OR 'FINAL' OR 'REDEFINITION'.
          " Skip method/parameter modifiers

        WHEN 'VALUE'.
          " Single VALUE keyword (default value marker) — skip

        WHEN 'PREFERRED'.
          " Flush current parameter before processing PREFERRED
          IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
            INSERT VALUE #(
              program = i_program  include = i_include
              class   = mv_class_name
              event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
              name    = lv_ev_name
              type    = SWITCH #( lv_section
                          WHEN 'IMPORTING' OR 'USING' THEN 'I'
                          WHEN 'EXPORTING'             THEN 'E'
                          WHEN 'CHANGING'              THEN 'C'
                          WHEN 'RETURNING'             THEN 'R'
                          ELSE 'I' )
              param   = lv_pname   line = tok-row )
              INTO TABLE lt_params.
            CLEAR: lv_pname, lv_ptype, lv_ref, lv_after_type.
          ENDIF.
          lv_skip_next = abap_true.

        WHEN 'PARAMETER'.
          IF lv_skip_next = abap_true.
            READ TABLE io_scan->tokens INDEX lv_tok_idx + 1 INTO DATA(ls_pref_tok).
            IF sy-subrc = 0.
              lv_preferred = ls_pref_tok-str.
              IF lv_preferred+0(1) = '!'. lv_preferred = lv_preferred+1. ENDIF.
            ENDIF.
            lv_skip_next = abap_false.
          ENDIF.

        WHEN OTHERS.
          IF lv_str+0(1) = '!'.
            " !PARAM — explicit parameter name, flush previous
            IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
              INSERT VALUE #(
                program = i_program  include = i_include
                class   = mv_class_name
                event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
                name    = lv_ev_name
                type    = SWITCH #( lv_section
                            WHEN 'IMPORTING' OR 'USING' THEN 'I'
                            WHEN 'EXPORTING'             THEN 'E'
                            WHEN 'CHANGING'              THEN 'C'
                            WHEN 'RETURNING'             THEN 'R'
                            ELSE 'I' )
                param   = lv_pname   line = tok-row )
                INTO TABLE lt_params.
            ENDIF.
            lv_pname = lv_str+1.
            CLEAR: lv_ptype, lv_ref, lv_after_type.

          ELSEIF lv_after_type = abap_true.
            " Type name token — store type, do not start new param
            IF lv_ref = abap_true.
              lv_ptype = |REF TO { lv_str }|.
            ELSE.
              lv_ptype = lv_str.
            ENDIF.
            lv_after_type = abap_false.
            lv_ref        = abap_false.

          ELSEIF lv_section IS NOT INITIAL AND lv_pname IS INITIAL.
            " First parameter in current section (no ! prefix)
            " Also handles VALUE(param) for RETURNING
            lv_pname = lv_str.
            IF lv_pname CP 'VALUE(*'.
              REPLACE FIRST OCCURRENCE OF 'VALUE(' IN lv_pname WITH ''.
              REPLACE ALL OCCURRENCES OF ')' IN lv_pname WITH ''.
              CONDENSE lv_pname NO-GAPS.
            ENDIF.
            CLEAR: lv_ptype, lv_ref, lv_after_type.

          ELSEIF lv_section IS NOT INITIAL AND lv_pname IS NOT INITIAL
             AND lv_after_type = abap_false.
            " Next param without ! in same section — flush previous, start new
            INSERT VALUE #(
              program = i_program  include = i_include
              class   = mv_class_name
              event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
              name    = lv_ev_name
              type    = SWITCH #( lv_section
                          WHEN 'IMPORTING' OR 'USING' THEN 'I'
                          WHEN 'EXPORTING'             THEN 'E'
                          WHEN 'CHANGING'              THEN 'C'
                          WHEN 'RETURNING'             THEN 'R'
                          ELSE 'I' )
              param   = lv_pname   line = tok-row )
              INTO TABLE lt_params.
            lv_pname = lv_str.
            IF lv_pname CP 'VALUE(*'.
              REPLACE FIRST OCCURRENCE OF 'VALUE(' IN lv_pname WITH ''.
              REPLACE ALL OCCURRENCES OF ')' IN lv_pname WITH ''.
              CONDENSE lv_pname NO-GAPS.
            ENDIF.
            CLEAR: lv_ptype, lv_ref, lv_after_type.

          ELSEIF lv_ev_name IS INITIAL AND lv_is_form = abap_false.
            " Method name in METHODS chain
            lv_ev_name = lv_str.

          ENDIF.

      ENDCASE.

      lv_tok_idx = lv_tok_idx + 1.
    ENDWHILE.

    " Flush last parameter
    IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
      INSERT VALUE #(
        program = i_program  include = i_include
        class   = mv_class_name
        event   = COND #( WHEN lv_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
        name    = lv_ev_name
        type    = SWITCH #( lv_section
                    WHEN 'IMPORTING' OR 'USING' THEN 'I'
                    WHEN 'EXPORTING'             THEN 'E'
                    WHEN 'CHANGING'              THEN 'C'
                    WHEN 'RETURNING'             THEN 'R'
                    ELSE 'I' )
        param   = lv_pname   line = lv_last_row )
        INTO TABLE lt_params.
    ENDIF.

    " Mark PREFERRED PARAMETER after all params are collected
    IF lv_preferred IS NOT INITIAL.
      READ TABLE lt_params WITH KEY name = lv_ev_name ASSIGNING FIELD-SYMBOL(<fp>).
      IF sy-subrc = 0.
        <fp>-preferred = 'X'.
      ENDIF.
    ENDIF.

    LOOP AT lt_params INTO ls_param.
      INSERT ls_param INTO TABLE cs_source-t_params.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS zcl_ace_parse_handlers IMPLEMENTATION.
  METHOD zif_ace_stmt_handler~handle.
    " Точечный вызов для RAISE EVENT — добавляем хэндлеры в t_keywords->tt_calls
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
    CHECK sy-subrc = 0 AND ls_kw-str = 'RAISE'.
    READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok2).
    CHECK sy-subrc = 0 AND ls_tok2-str = 'EVENT'.

    DATA lt_calls TYPE zcl_ace=>tt_calls.
    resolve_raise_event(
      EXPORTING io_scan    = io_scan
                i_stmt_idx = i_stmt_idx
                i_program  = i_program
                i_include  = i_include
      CHANGING  cs_source  = cs_source
                ct_calls   = lt_calls ).

    CHECK lt_calls IS NOT INITIAL.

    LOOP AT cs_source-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE include = i_include.
      READ TABLE <prog>-t_keywords WITH KEY index = i_stmt_idx
        ASSIGNING FIELD-SYMBOL(<kw>).
      IF sy-subrc = 0.
        LOOP AT lt_calls INTO DATA(ls_c).
          READ TABLE <kw>-tt_calls
            WITH KEY event = ls_c-event name = ls_c-name class = ls_c-class
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_c TO <kw>-tt_calls.
          ENDIF.
        ENDLOOP.
      ENDIF.
      EXIT.
    ENDLOOP.
  ENDMETHOD.
  METHOD collect.
    " ---------------------------------------------------------------
    " Два вида записей в tt_handler_map:
    "
    " 1. METHODS meth FOR EVENT ev_name OF class
    "    → статическая декларация хэндлера в определении класса
    "
    " 2. SET HANDLER obj->method FOR src_obj
    "    → динамическая регистрация, резолвим типы из t_vars
    " ---------------------------------------------------------------

    LOOP AT io_scan->statements INTO DATA(ls_stmt).
      READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
      CHECK sy-subrc = 0.

      CASE ls_kw-str.

        " ── METHODS meth FOR EVENT ev_name OF src_class ─────────────
        WHEN 'METHODS' OR 'CLASS-METHODS'.
          DATA(lv_i) = ls_stmt-from + 1.
          WHILE lv_i <= ls_stmt-to.
            READ TABLE io_scan->tokens INDEX lv_i INTO DATA(ls_t).
            IF sy-subrc <> 0. EXIT. ENDIF.
            IF ls_t-str = 'FOR'.
              READ TABLE io_scan->tokens INDEX lv_i + 1 INTO DATA(ls_t1).
              IF sy-subrc = 0 AND ls_t1-str = 'EVENT'.
                READ TABLE io_scan->tokens INDEX lv_i + 2 INTO DATA(ls_ev_tok).
                READ TABLE io_scan->tokens INDEX lv_i + 3 INTO DATA(ls_of_tok).
                READ TABLE io_scan->tokens INDEX lv_i + 4 INTO DATA(ls_src_tok).
                IF sy-subrc = 0 AND ls_of_tok-str = 'OF'.
                  " Имя метода — второй токен стейтмента
                  READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_meth_tok).
                  " hdl_class пустой — заполним позже из calls_line или SET HANDLER
                  READ TABLE cs_source-tt_handler_map
                    WITH KEY src_class  = ls_src_tok-str
                             event_name = ls_ev_tok-str
                             hdl_method = ls_meth_tok-str
                    TRANSPORTING NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND VALUE zif_ace_parse_data=>ts_handler_map(
                      src_class  = ls_src_tok-str
                      event_name = ls_ev_tok-str
                      hdl_class  = ``
                      hdl_method = ls_meth_tok-str
                      include    = i_include
                      line       = ls_kw-row
                    ) TO cs_source-tt_handler_map.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
            lv_i = lv_i + 1.
          ENDWHILE.

        " ── SET HANDLER obj->method FOR src_obj ─────────────────────
        WHEN 'SET'.
          READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok2).
          CHECK sy-subrc = 0 AND ls_tok2-str = 'HANDLER'.

          DATA(lv_j) = ls_stmt-from + 2.
          WHILE lv_j <= ls_stmt-to.
            READ TABLE io_scan->tokens INDEX lv_j INTO DATA(ls_sh).
            IF sy-subrc <> 0. EXIT. ENDIF.

            IF ls_sh-str CS '->'.
              DATA(lv_hdl_obj)  = ``.
              DATA(lv_hdl_meth) = ``.
              SPLIT ls_sh-str AT '->' INTO lv_hdl_obj lv_hdl_meth.
              CONDENSE: lv_hdl_obj, lv_hdl_meth.

              " Ищем FOR → src_obj
              DATA(lv_k) = lv_j + 1.
              DATA(lv_src_obj) = ``.
              WHILE lv_k <= ls_stmt-to.
                READ TABLE io_scan->tokens INDEX lv_k INTO DATA(ls_for).
                IF sy-subrc <> 0. EXIT. ENDIF.
                IF ls_for-str = 'FOR'.
                  READ TABLE io_scan->tokens INDEX lv_k + 1 INTO DATA(ls_src).
                  IF sy-subrc = 0 AND ls_src-str <> 'ALL'. lv_src_obj = ls_src-str. ENDIF.
                  EXIT.
                ENDIF.
                lv_k = lv_k + 1.
              ENDWHILE.

              " Резолвим тип объекта хэндлера
              DATA(lv_hdl_class) = ``.
              IF lv_hdl_obj = 'ME' OR lv_hdl_obj IS INITIAL.
                " Ищем класс по имени метода в calls_line
                LOOP AT cs_source-tt_calls_line INTO DATA(ls_cl_me)
                  WHERE eventname = lv_hdl_meth AND eventtype = 'METHOD'.
                  lv_hdl_class = ls_cl_me-class. EXIT.
                ENDLOOP.
              ELSE.
                lv_hdl_class = resolve_var_type(
                  is_source = cs_source i_program = i_program
                  i_evtype = `` i_evname = `` i_varname = lv_hdl_obj ).
              ENDIF.

              " Резолвим тип источника события
              DATA(lv_src_class) = ``.
              IF lv_src_obj IS NOT INITIAL AND lv_src_obj <> '*'.
                lv_src_class = resolve_var_type(
                  is_source = cs_source i_program = i_program
                  i_evtype = `` i_evname = `` i_varname = lv_src_obj ).
              ENDIF.

              " Обновляем запись из FOR EVENT или добавляем новую
              READ TABLE cs_source-tt_handler_map
                WITH KEY hdl_method = lv_hdl_meth
                ASSIGNING FIELD-SYMBOL(<hm>).
              IF sy-subrc = 0.
                IF lv_hdl_class IS NOT INITIAL AND <hm>-hdl_class IS INITIAL.
                  <hm>-hdl_class = lv_hdl_class.
                ENDIF.
                IF lv_src_class IS NOT INITIAL AND <hm>-src_class IS INITIAL.
                  <hm>-src_class = lv_src_class.
                ENDIF.
              ELSE.
                APPEND VALUE zif_ace_parse_data=>ts_handler_map(
                  src_class  = lv_src_class
                  event_name = ``
                  hdl_class  = lv_hdl_class
                  hdl_method = lv_hdl_meth
                  include    = i_include
                  line       = ls_kw-row
                ) TO cs_source-tt_handler_map.
              ENDIF.
            ENDIF.
            lv_j = lv_j + 1.
          ENDWHILE.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD resolve_raise_event.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    " RAISE EVENT ev_name → токен from+2
    READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO DATA(ls_ev).
    CHECK sy-subrc = 0.
    DATA(lv_ev_name) = ls_ev-str.

    LOOP AT cs_source-tt_handler_map INTO DATA(ls_hm)
      WHERE event_name = lv_ev_name.

      DATA(lv_class) = ls_hm-hdl_class.

      " Если класс не резолвился — ищем в calls_line по имени метода
      IF lv_class IS INITIAL.
        LOOP AT cs_source-tt_calls_line INTO DATA(ls_cl)
          WHERE eventname = ls_hm-hdl_method AND eventtype = 'METHOD'.
          lv_class = ls_cl-class. EXIT.
        ENDLOOP.
      ENDIF.

      APPEND VALUE zcl_ace=>ts_calls(
        event = 'METHOD'
        class = lv_class
        name  = ls_hm-hdl_method
        type  = 'H'
      ) TO ct_calls.
    ENDLOOP.
  ENDMETHOD.
  METHOD resolve_var_type.
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               eventtype = i_evtype
               eventname = i_evname
               name      = i_varname
      INTO DATA(ls_var).
    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type. RETURN.
    ENDIF.
    READ TABLE is_source-t_vars
      WITH KEY program = i_program name = i_varname
      INTO ls_var.
    IF sy-subrc = 0. rv_type = ls_var-type. ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ZCL_ACE_PARSE_EVENTS IMPLEMENTATION.
  method ZIF_ACE_STMT_HANDLER~HANDLE.

    CHECK i_stmt_idx = 0.

    LOOP AT io_scan->structures INTO DATA(struc) WHERE type = 'E'.
      DATA lv_name TYPE string.
      DATA lv_line TYPE i.
      CLEAR: lv_name, lv_line.

      READ TABLE io_scan->statements
        INDEX struc-stmnt_from
        INTO DATA(stmt).
      IF sy-subrc = 0.
        DATA lv_from TYPE i.
        DATA lv_to   TYPE i.
        lv_from = stmt-from.
        lv_to   = stmt-to.
        lv_line = io_scan->tokens[ lv_from ]-row.
        LOOP AT io_scan->tokens FROM lv_from TO lv_to INTO DATA(tok).
          IF lv_name IS INITIAL.
            lv_name = tok-str.
          ELSE.
            lv_name = |{ lv_name } { tok-str }|.
          ENDIF.
        ENDLOOP.
      ENDIF.

      APPEND VALUE zif_ace_parse_data=>ts_event(
        program    = i_program
        include    = i_include
        type       = struc-type
        stmnt_type = struc-stmnt_type
        stmnt_from = struc-stmnt_from
        stmnt_to   = struc-stmnt_to
        name       = lv_name
        line       = lv_line
      ) TO cs_source-t_events.
    ENDLOOP.

  endmethod.
ENDCLASS.

CLASS ZCL_ACE_PARSE_CALLS_LINE IMPLEMENTATION.
  METHOD get_meth_type.
    DATA(lv_s)   = condense( val = CONV string( i_include ) ).
    DATA(lv_len) = strlen( lv_s ).
    IF lv_len >= 2.
      DATA(lv_off)    = lv_len - 2.
      DATA(lv_suffix) = lv_s+lv_off(2).
      CASE lv_suffix.
        WHEN 'CU'. rv_type = 1.
        WHEN 'CO'. rv_type = 2.
        WHEN 'CI'. rv_type = 3.
        WHEN OTHERS. rv_type = mv_meth_type.
      ENDCASE.
    ELSE.
      rv_type = mv_meth_type.
    ENDIF.
  ENDMETHOD.
  METHOD on_class_kw.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    CHECK sy-subrc = 0.
    mv_class_name = name_tok-str.
    mv_in_impl    = abap_false.
    mv_meth_type  = 0.
    mv_is_intf    = COND #( WHEN i_kw = 'INTERFACE' THEN abap_true ELSE abap_false ).
    CLEAR mv_super_name.
    IF i_kw = 'CLASS'.
      DATA lv_prev TYPE string.
      LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(tok).
        IF tok-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. RETURN. ENDIF.
        IF lv_prev = 'FROM'. mv_super_name = tok-str. ENDIF.
        lv_prev = tok-str.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD on_methods_sig.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    CHECK sy-subrc = 0.
    DATA(lv_line)      = name_tok-row.
    DATA(lv_meth_type) = get_meth_type( i_include ).

    READ TABLE mt_meth_defs WITH KEY name = name_tok-str TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND VALUE ts_meth_def( name = name_tok-str def_include = i_include
                                def_line = lv_line   meth_type   = lv_meth_type )
        TO mt_meth_defs.
    ENDIF.

    READ TABLE cs_source-tt_calls_line
      WITH KEY class = mv_class_name eventtype = 'METHOD' eventname = name_tok-str
      TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.

    APPEND INITIAL LINE TO cs_source-tt_calls_line ASSIGNING FIELD-SYMBOL(<cl>).
    <cl>-program     = i_program.
    <cl>-include     = i_include.
    <cl>-class       = mv_class_name.
    <cl>-eventtype   = 'METHOD'.
    <cl>-eventname   = name_tok-str.
    <cl>-meth_type   = lv_meth_type.
    <cl>-is_intf     = mv_is_intf.
    <cl>-def_include = i_include.
    <cl>-def_line    = lv_line.
    <cl>-index       = i_stmt_idx.
    <cl>-def_ind     = i_stmt_idx.  " marks declaration-only until on_block_start updates index
  ENDMETHOD.
  METHOD on_block_start.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    CHECK sy-subrc = 0.
    DATA(lv_evtype) = SWITCH string( i_kw
      WHEN 'FUNCTION' THEN 'FUNCTION' WHEN 'MODULE' THEN 'MODULE'
      WHEN 'FORM'     THEN 'FORM'     ELSE               'METHOD' ).

    IF mv_class_name IS NOT INITIAL.
      READ TABLE cs_source-tt_calls_line
        WITH KEY class = mv_class_name eventtype = lv_evtype eventname = name_tok-str
        ASSIGNING FIELD-SYMBOL(<ex>).
    ELSE.
      READ TABLE cs_source-tt_calls_line
        WITH KEY program = i_program eventtype = lv_evtype eventname = name_tok-str
        ASSIGNING <ex>.
    ENDIF.

    IF sy-subrc = 0.
      " Запись создана ON_METHODS_SIG из CU/CO/CI.
      " Обновляем include → реальный CM-инклуд.
      " index = i_stmt_idx (индекс statement, нужен для поиска в t_keywords).
      <ex>-include = i_include.
      <ex>-index   = i_stmt_idx.
    ELSE.
      " Нет предварительной записи — локальный класс, FORM, MODULE, FUNCTION
      APPEND INITIAL LINE TO cs_source-tt_calls_line ASSIGNING FIELD-SYMBOL(<cl>).
      <cl>-program   = i_program.
      <cl>-include   = i_include.
      <cl>-class     = mv_class_name.
      <cl>-eventtype = lv_evtype.
      <cl>-eventname = name_tok-str.
      <cl>-is_intf   = mv_is_intf.
      <cl>-def_line  = i_stmt_idx.
      IF i_kw = 'METHOD'.
        READ TABLE mt_meth_defs WITH KEY name = name_tok-str INTO DATA(ls_def).
        IF sy-subrc = 0.
          <cl>-def_include = ls_def-def_include.
          <cl>-index       = ls_def-def_line.
          <cl>-meth_type   = ls_def-meth_type.
        ELSE.
          " локальный класс без предварительного объявления METHODS —
          " используем i_stmt_idx (индекс statement), НЕ row токена
          <cl>-def_include = i_include.
          <cl>-index       = i_stmt_idx.
          <cl>-meth_type   = get_meth_type( i_include ).
        ENDIF.
      ELSE.
        <cl>-index = i_stmt_idx.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD zif_ace_stmt_handler~handle.
    CHECK i_stmt_idx > 0.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX stmt-from INTO DATA(kw_tok).
    CHECK sy-subrc = 0.
    DATA(lv_kw) = kw_tok-str.

    IF i_class IS NOT INITIAL.
      mv_class_name = i_class.
      CLEAR mv_is_intf.
    ELSEIF i_interface IS NOT INITIAL.
      mv_class_name = i_interface.
      mv_is_intf = abap_true.
    ENDIF.

    CASE lv_kw.
      WHEN 'CLASS' OR 'INTERFACE'.

        IF lv_kw = 'CLASS'.
          LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(ls_t).
            IF ls_t-str = 'DEFERRED'. RETURN. ENDIF.
          ENDLOOP.
        ENDIF.

        on_class_kw( io_scan = io_scan i_stmt_idx = i_stmt_idx i_kw = lv_kw ).

        IF mv_class_name IS NOT INITIAL AND mv_in_impl = abap_false.
          DATA(lv_def_line) = COND i( WHEN stmt IS NOT INITIAL THEN io_scan->tokens[ stmt-from ]-row ELSE 0 ).
          READ TABLE cs_source-tt_class_defs WITH KEY class = mv_class_name ASSIGNING FIELD-SYMBOL(<cd>).
          IF sy-subrc = 0.
            <cd>-super       = mv_super_name.
            <cd>-is_intf     = mv_is_intf.
            <cd>-program     = i_program.
            <cd>-def_include = i_include.
            <cd>-def_line    = lv_def_line.
          ELSE.
            APPEND VALUE zif_ace_parse_data=>ts_class_def(
              class       = mv_class_name
              super       = mv_super_name
              is_intf     = mv_is_intf
              program     = i_program
              def_include = i_include
              def_line    = lv_def_line )
              TO cs_source-tt_class_defs.
          ENDIF.
        ENDIF.

        IF mv_in_impl = abap_true AND mv_class_name IS NOT INITIAL.
          DATA(lv_impl_line) = COND i( WHEN stmt IS NOT INITIAL THEN io_scan->tokens[ stmt-from ]-row ELSE 0 ).
          READ TABLE cs_source-tt_class_defs WITH KEY class = mv_class_name ASSIGNING <cd>.
          IF sy-subrc = 0.
            <cd>-impl_include = i_include.
            <cd>-impl_line    = lv_impl_line.
          ELSE.
            APPEND VALUE zif_ace_parse_data=>ts_class_def(
              class        = mv_class_name
              program      = i_program
              impl_include = i_include
              impl_line    = lv_impl_line )
              TO cs_source-tt_class_defs.
          ENDIF.
        ENDIF.

      WHEN 'PUBLIC' OR 'PROTECTED' OR 'PRIVATE'.
        IF mv_in_impl = abap_false AND mv_class_name IS NOT INITIAL.
          CASE lv_kw.
            WHEN 'PUBLIC'.    mv_meth_type = 1.
            WHEN 'PROTECTED'. mv_meth_type = 2.
            WHEN 'PRIVATE'.   mv_meth_type = 3.
          ENDCASE.
          DATA(lv_sec_line) = COND i( WHEN stmt IS NOT INITIAL THEN io_scan->tokens[ stmt-from ]-row ELSE 0 ).
          READ TABLE cs_source-tt_sections
            WITH KEY class = mv_class_name section = lv_kw
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND VALUE zif_ace_parse_data=>ts_section(
              class   = mv_class_name
              section = lv_kw
              include = i_include
              line    = lv_sec_line )
              TO cs_source-tt_sections.
          ENDIF.
        ENDIF.

      WHEN 'METHODS' OR 'CLASS-METHODS'.
        IF mv_in_impl = abap_false.
          on_methods_sig( EXPORTING io_scan    = io_scan
                                    i_stmt_idx = i_stmt_idx
                                    i_program  = i_program
                                    i_include  = i_include
                          CHANGING  cs_source  = cs_source ).
        ENDIF.
      WHEN 'FORM' OR 'METHOD' OR 'MODULE' OR 'FUNCTION'.
        on_block_start( EXPORTING io_scan    = io_scan
                                  i_stmt_idx = i_stmt_idx
                                  i_program  = i_program
                                  i_include  = i_include
                                  i_kw       = lv_kw
                        CHANGING  cs_source  = cs_source ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS ZCL_ACE_PARSE_CALLS IMPLEMENTATION.
  METHOD zif_ace_stmt_handler~handle.

    CHECK i_stmt_idx > 0.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
    CHECK sy-subrc = 0.

    mv_class_name = i_class.
    mv_event_name = i_ev_name.
    mv_event_type = i_evtype.

*    CASE ls_kw-str.
**      WHEN 'CLASS' OR 'INTERFACE'.
**        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_name).
**        IF sy-subrc = 0.
**          IF mv_class_name <> ls_name-str. CLEAR: mv_super_cls, mv_super. ENDIF.
**          mv_class_name = ls_name-str.
**          mv_in_impl    = abap_false.
**          IF ls_kw-str = 'CLASS'.
**            LOOP AT io_scan->tokens FROM ls_stmt-from TO ls_stmt-to INTO DATA(ls_t).
**              IF ls_t-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. RETURN. ENDIF.
**            ENDLOOP.
**          ENDIF.
**        ENDIF.
**        RETURN.
**      WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
**        CLEAR: mv_class_name, mv_in_impl, mv_event_type, mv_event_name, mv_super_cls, mv_super.
**        RETURN.
*      WHEN 'METHOD'.
*        mv_event_type = 'METHOD'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO data(ls_name).
*        IF sy-subrc = 0. mv_event_name = ls_name-str. ENDIF.
*        RETURN.
**      WHEN 'ENDMETHOD'.
**        CLEAR: mv_event_type, mv_event_name. RETURN.
**      WHEN 'FORM'.
**        mv_event_type = 'FORM'.
**        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
**        IF sy-subrc = 0. mv_event_name = ls_name-str. ENDIF.
**        RETURN.
**      WHEN 'ENDFORM'.
**        CLEAR: mv_event_type, mv_event_name. RETURN.
*      WHEN 'FUNCTION'.
*        mv_event_type = 'FUNCTION'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
*        IF sy-subrc = 0.
*          mv_event_name = ls_name-str.
*          REPLACE ALL OCCURRENCES OF '''' IN mv_event_name WITH ''.
*        ENDIF.
*        RETURN.
**      WHEN 'ENDFUNCTION'.
**        CLEAR: mv_event_type, mv_event_name. RETURN.
*      WHEN 'MODULE'.
*        mv_event_type = 'MODULE'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
*        IF sy-subrc = 0. mv_event_name = ls_name-str. ENDIF.
*        RETURN.
**      WHEN 'ENDMODULE'.
**        CLEAR: mv_event_type, mv_event_name. RETURN.
**      WHEN 'START-OF-SELECTION' OR 'END-OF-SELECTION'
**        OR 'INITIALIZATION' OR 'TOP-OF-PAGE' OR 'END-OF-PAGE'
**        OR 'AT' OR 'GET'.
**        mv_event_type = 'EVENT'.
**        mv_event_name = ls_kw-str.
**        RETURN.
*    ENDCASE.
*
*    CHECK mv_event_type IS NOT INITIAL.

    parse_stmt_calls(
      EXPORTING io_scan    = io_scan
                i_stmt_idx = i_stmt_idx
                i_program  = i_program
                i_include  = i_include
      CHANGING  cs_source  = cs_source ).

  ENDMETHOD.
  METHOD get_super.
    CHECK mv_class_name IS NOT INITIAL.
    IF mv_super_cls = mv_class_name.
      rv_super = mv_super. RETURN.
    ENDIF.
    READ TABLE is_source-tt_class_defs WITH KEY class = mv_class_name INTO DATA(ls_cd).
    IF sy-subrc = 0 AND ls_cd-super IS NOT INITIAL.
      rv_super = ls_cd-super.
    ELSE.
      SELECT SINGLE refclsname FROM seometarel
        WHERE clsname = @mv_class_name AND reltype = '1'
        INTO @rv_super.
    ENDIF.
    mv_super_cls = mv_class_name.
    mv_super     = rv_super.
  ENDMETHOD.
  METHOD resolve_var_type.
    " t_vars is pre-sorted by (program, eventtype, eventname, name)
    " before this pass runs — so READ with BINARY SEARCH is O(log n).

    " 1. Local scope (restricted to the current class when known,
    "    otherwise a same-named local of another class could match)
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               class     = i_class
               eventtype = i_evtype
               eventname = i_evname
               name      = i_varname
      INTO DATA(ls_var).

    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
      RETURN.
    ENDIF.

    " 2. Attributes of the current class (eventtype = '', eventname = '')
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               class     = i_class
               eventtype = ''
               eventname = ''
               name      = i_varname
      INTO ls_var.
    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
      RETURN.
    ENDIF.

    " 3. Globals (class = '', eventtype = '', eventname = '')
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               class     = ''
               eventtype = ''
               eventname = ''
               name      = i_varname
      INTO ls_var.
    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
    ENDIF.
  ENDMETHOD.
  METHOD resolve_chain.
    " OBJ->ATTR[->ATTR2…] or CLS=>ATTR[->…]: resolve the head, then walk
    " each attribute through the type of the previous segment.
    DATA lt_seg TYPE string_table.
    DATA lv_cur TYPE string.

    SPLIT i_chain AT '->' INTO TABLE lt_seg.
    READ TABLE lt_seg INDEX 1 INTO DATA(lv_head).
    IF sy-subrc <> 0 OR lv_head IS INITIAL. RETURN. ENDIF.

    IF lv_head CS '=>'.
      SPLIT lv_head AT '=>' INTO DATA(lv_hcls) DATA(lv_hattr).
      LOOP AT is_source-t_vars INTO DATA(ls_hv)
        WHERE class = lv_hcls AND name = lv_hattr AND type IS NOT INITIAL.
        lv_cur = ls_hv-type. EXIT.
      ENDLOOP.
    ELSEIF lv_head = 'ME'.
      lv_cur = mv_class_name.
    ELSE.
      lv_cur = resolve_var_type(
        is_source = is_source i_program = i_program i_include = i_program
        i_evtype  = i_evtype  i_evname  = i_evname
        i_varname = lv_head   i_class   = mv_class_name ).
    ENDIF.
    IF lv_cur IS INITIAL. RETURN. ENDIF.

    LOOP AT lt_seg FROM 2 INTO DATA(lv_attr).
      DATA(lv_next) = ``.
      LOOP AT is_source-t_vars INTO DATA(ls_av)
        WHERE class = lv_cur AND name = lv_attr AND type IS NOT INITIAL.
        lv_next = ls_av-type. EXIT.
      ENDLOOP.
      IF lv_next IS INITIAL. RETURN. ENDIF.
      lv_cur = lv_next.
    ENDLOOP.
    rv_type = lv_cur.
  ENDMETHOD.
  METHOD is_builtin.
    IF mv_builtin_funcs IS INITIAL.
      mv_builtin_funcs =
        ` ABS CEIL FLOOR FRAC SIGN TRUNC IPOW NMAX NMIN SQRT EXP LOG LOG10 SIN COS TAN`
        && ` ASIN ACOS ATAN SINH COSH TANH ROUND RESCALE CHARLEN DBMAXLEN NUMOFCHAR STRLEN`
        && ` XSTRLEN LINES BOOLC BOOLX XSDBOOL CONCAT_LINES_OF CONDENSE ESCAPE MATCH REPEAT`
        && ` REPLACE REVERSE SEGMENT SHIFT_LEFT SHIFT_RIGHT SUBSTRING SUBSTRING_AFTER`
        && ` SUBSTRING_BEFORE SUBSTRING_FROM SUBSTRING_TO TO_LOWER TO_MIXED TO_UPPER FROM_MIXED`
        && ` TRANSLATE CMAX CMIN COUNT COUNT_ANY_OF COUNT_ANY_NOT_OF DISTANCE FIND FIND_END`
        && ` FIND_ANY_OF FIND_ANY_NOT_OF CONTAINS CONTAINS_ANY_OF CONTAINS_ANY_NOT_OF`
        && ` LINE_EXISTS LINE_INDEX UTCLONG_CURRENT UTCLONG_ADD UTCLONG_DIFF`
        && ` CORRESPONDING FILTER REDUCE EXACT VALUE CONV REF CAST COND SWITCH DATA FINAL FIELD-SYMBOL `.
    ENDIF.
    rv_builtin = xsdbool( mv_builtin_funcs CS | { i_name } | ).
  ENDMETHOD.
METHOD collect_method_calls.
    DATA lv_tstr     TYPE string.
    DATA lv_arrow    TYPE string.
    DATA lv_left     TYPE string.
    DATA lv_right    TYPE string.
    DATA lv_rpart    TYPE string.
    DATA lv_dummy    TYPE string.
    DATA ls_prev     LIKE LINE OF io_scan->tokens.
    DATA ls_next     LIKE LINE OF io_scan->tokens.
    DATA lv_c        TYPE zcl_ace=>ts_calls.
    DATA lv_rtype    TYPE string.
    DATA lt_bind     TYPE zcl_ace=>tt_param_bindings.
    DATA ls_b        TYPE zcl_ace=>ts_param_binding.
    DATA lv_single   TYPE string.
    DATA lv_pos      TYPE abap_bool.
    DATA lv_lhs      TYPE string.
    DATA lv_call_cls TYPE string.
    DATA lv_pref     TYPE string.
    DATA lv_ret      TYPE string.
    DATA lv_scan     TYPE i.
    DATA ls_sa       LIKE LINE OF io_scan->tokens.
    DATA ls_eq       LIKE LINE OF io_scan->tokens.
    DATA ls_val      LIKE LINE OF io_scan->tokens.
    DATA lv_sa_str   TYPE string.
    DATA lv_val_str  TYPE string.

    " Для statement вида  VAR = expr  токен[from] — это LHS-переменная,
    " токен[from+1] = '='.  Такой первый токен нужно пропустить как вызов,
    " но НЕ ограничивать поиск одной позицией — в правой части может быть
    " несколько вызовов: RV = A * FUNC1(...) + FUNC2(...).
    DATA(lv_ti) = i_stmt-from.

    WHILE lv_ti <= i_stmt-to.
      READ TABLE io_scan->tokens INDEX lv_ti INTO DATA(ls_t).
      IF sy-subrc <> 0. EXIT. ENDIF.
      lv_tstr = ls_t-str.
      CLEAR: lv_arrow, lv_left, lv_right, lv_rpart, lv_dummy.

      " ── Распознаём токен вызова ───────────────────────────────────
      " Split at the LAST arrow so that multi-level access
      " (obj->attr->meth( / cls=>attr->meth() yields the real method name
      " and the full reference chain on the left.
      DATA lv_p_inst TYPE i.
      DATA lv_p_stat TYPE i.
      lv_p_inst = find( val = lv_tstr sub = '->' occ = -1 ).
      lv_p_stat = find( val = lv_tstr sub = '=>' occ = -1 ).

      IF lv_p_stat >= 0 AND lv_p_stat > lv_p_inst AND lv_tstr CS '('.
        lv_arrow = '=>'.
        lv_left  = substring( val = lv_tstr len = lv_p_stat ).
        lv_rpart = substring( val = lv_tstr off = lv_p_stat + 2 ).
        SPLIT lv_rpart AT '(' INTO lv_right lv_dummy.

      ELSEIF lv_p_inst >= 0 AND lv_tstr CS '('.
        lv_arrow = '->'.
        lv_left  = substring( val = lv_tstr len = lv_p_inst ).
        lv_rpart = substring( val = lv_tstr off = lv_p_inst + 2 ).
        SPLIT lv_rpart AT '(' INTO lv_right lv_dummy.
        IF lv_left IS INITIAL OR lv_left CO ')'.
          READ TABLE io_scan->tokens INDEX lv_ti - 1 INTO DATA(ls_m1).
          READ TABLE io_scan->tokens INDEX lv_ti - 2 INTO DATA(ls_m2).
          IF ls_m2-str = 'NEW' AND ls_m1-str CS '('.
            lv_left = ls_m1-str.
            REPLACE ALL OCCURRENCES OF '(' IN lv_left WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN lv_left WITH ''.
            CONDENSE lv_left NO-GAPS.
            lv_arrow = '=>'.
          ELSE.
            READ TABLE io_scan->tokens INDEX lv_ti - 3 INTO DATA(ls_m3).
            READ TABLE io_scan->tokens INDEX lv_ti - 4 INTO DATA(ls_m4).
            IF ls_m4-str = 'NEW' AND ls_m2-str = '('.
              lv_left  = ls_m3-str.
              lv_arrow = '=>'.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF lv_tstr = '=>' OR lv_tstr = '->'.
        lv_arrow = lv_tstr.
        READ TABLE io_scan->tokens INDEX lv_ti - 1 INTO ls_prev.
        READ TABLE io_scan->tokens INDEX lv_ti + 1 INTO ls_next.
        IF sy-subrc = 0 AND ls_next-str CS '(' AND NOT ls_next-str CO '()'.
          lv_left  = ls_prev-str.
          lv_right = ls_next-str.
          REPLACE ALL OCCURRENCES OF '(' IN lv_right WITH ''.
          lv_ti = lv_ti + 1.
          IF lv_tstr = '->' AND ( ls_prev-str IS INITIAL OR ls_prev-str CO ')' ).
            READ TABLE io_scan->tokens INDEX lv_ti - 2 INTO DATA(ls_nk1).
            READ TABLE io_scan->tokens INDEX lv_ti - 3 INTO DATA(ls_nk2).
            READ TABLE io_scan->tokens INDEX lv_ti - 4 INTO DATA(ls_nk3).
            IF ls_nk2-str = 'NEW' AND ls_nk1-str CS '('.
              lv_left = ls_nk1-str.
              REPLACE ALL OCCURRENCES OF '(' IN lv_left WITH ''.
              CONDENSE lv_left NO-GAPS.
              lv_arrow = '=>'.
            ELSEIF ls_nk3-str = 'NEW' AND ls_nk1-str = '('.
              lv_left  = ls_nk2-str.
              lv_arrow = '=>'.
            ENDIF.
          ENDIF.
        ELSE.
          lv_ti = lv_ti + 1. CONTINUE.
        ENDIF.

      ELSEIF lv_tstr = 'NEW'.
        READ TABLE io_scan->tokens INDEX lv_ti + 1 INTO ls_next.
        IF sy-subrc = 0 AND ls_next-str CS '('.
          lv_arrow = '=>'.
          lv_left  = ls_next-str.
          REPLACE ALL OCCURRENCES OF '(' IN lv_left WITH ''.
          CONDENSE lv_left NO-GAPS.
          " NEW #( ) — the class is inferred by the compiler, nothing to record
          IF lv_left = '#'.
            lv_ti = lv_ti + 1. CONTINUE.
          ENDIF.
          lv_right = 'CONSTRUCTOR'.
          lv_ti = lv_ti + 1.
        ELSE.
          lv_ti = lv_ti + 1. CONTINUE.
        ENDIF.

      ELSEIF lv_tstr CA '(' AND NOT lv_tstr CS '->' AND NOT lv_tstr CS '=>'.
        READ TABLE io_scan->tokens INDEX lv_ti + 1 INTO DATA(ls_after).
        IF ls_after-str = '='.
          lv_ti = lv_ti + 1. CONTINUE.
        ENDIF.
        " The method name is everything before the first '(' — a token like
        " FOO(BAR) must not collapse into FOOBAR, and DATA(LV_X) / VALUE(…)
        " style tokens are filtered out via the builtin list.
        DATA(lv_par_off) = find( val = lv_tstr sub = '(' ).
        IF lv_par_off <= 0.
          lv_ti = lv_ti + 1. CONTINUE.
        ENDIF.
        lv_right = substring( val = lv_tstr len = lv_par_off ).
        CONDENSE lv_right NO-GAPS.
        " Built-in functions and constructor expressions are not method calls
        IF lv_right IS INITIAL OR is_builtin( lv_right ) = abap_true.
          CLEAR lv_right.
          lv_ti = lv_ti + 1. CONTINUE.
        ENDIF.
        " Length/offset specifications lv_x(10) / lv_x+2(3) are not calls
        DATA(lv_par_rem) = substring( val = lv_tstr off = lv_par_off + 1 ).
        REPLACE ALL OCCURRENCES OF ')' IN lv_par_rem WITH ''.
        CONDENSE lv_par_rem NO-GAPS.
        IF lv_right CA '+'
          OR ( lv_par_rem IS NOT INITIAL AND lv_par_rem CO '0123456789*' ).
          CLEAR lv_right.
          lv_ti = lv_ti + 1. CONTINUE.
        ENDIF.
        lv_arrow = '->'.
        lv_left  = 'ME'.

      ELSE.
        lv_ti = lv_ti + 1. CONTINUE.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '(' IN lv_right WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN lv_right WITH ''.
      CONDENSE lv_right NO-GAPS.
      IF lv_right IS INITIAL. lv_ti = lv_ti + 1. CONTINUE. ENDIF.

      " ── Строим запись вызова ──────────────────────────────────────
      CLEAR lv_c.
      lv_c-event = 'METHOD'.
      lv_c-name  = lv_right.
      " Leftover ')' from a chained call ( a->b( )->c( ) ) is not a variable —
      " record the call by name only instead of a garbage outer reference.
      IF lv_left CO ')' AND lv_left IS NOT INITIAL.
        CLEAR lv_left.
      ENDIF.

      IF lv_left = 'ME'.
        lv_c-class = mv_class_name.
      ELSEIF lv_left = 'SUPER'.
        lv_c-super = abap_true.
        lv_c-class = COND #( WHEN mv_super IS NOT INITIAL THEN mv_super ELSE mv_class_name ).
      ELSEIF lv_left CS '->' OR lv_left CS '=>'.
        " Multi-level access: obj->attr->meth( / cls=>attr->meth(
        lv_rtype = resolve_chain(
          is_source = cs_source i_program = i_program
          i_evtype  = lv_c-event i_evname = mv_event_name
          i_chain   = lv_left ).
        lv_c-class = lv_rtype.
        lv_c-outer = lv_left.
        lv_c-inner = lv_right.
      ELSE.
        lv_rtype = resolve_var_type(
          is_source = cs_source i_program = i_program i_include = i_program
          i_evtype  = lv_c-event i_evname = mv_event_name
          i_varname = lv_left   i_class   = mv_class_name ).
        IF lv_rtype IS NOT INITIAL.
          lv_c-class = lv_rtype.
          lv_c-outer = lv_left.
          lv_c-inner = lv_right.
        ELSEIF lv_arrow = '=>'.
          lv_c-class = lv_left.
        ELSE.
          lv_c-outer = lv_left.
          lv_c-inner = lv_right.
        ENDIF.
      ENDIF.

      " ── CONSTRUCTOR: записываем только если он реально определён ──
      IF lv_c-name = 'CONSTRUCTOR' AND lv_c-class IS NOT INITIAL.
        READ TABLE cs_source-tt_calls_line
          WITH KEY class     = lv_c-class
                   eventtype = 'METHOD'
                   eventname = 'CONSTRUCTOR'
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          lv_ti = lv_ti + 1. CONTINUE.
        ENDIF.
      ENDIF.

      lv_call_cls = COND #( WHEN lv_c-class IS NOT INITIAL THEN lv_c-class ELSE mv_class_name ).

      " ── LHS: lv_x = meth(…) → RETURNING ──────────────────────────
      " Сначала проверяем токен непосредственно перед вызовом (простой случай).
      " Если он не '=', ищем '=' у начала statement — случай
      " rv_payment = iv_amount * get_factor(  где '=' далеко назад.
      CLEAR lv_lhs.
      DATA(lv_lhs_pos) = lv_ti - 1.
      IF lv_lhs_pos >= i_stmt-from.
        READ TABLE io_scan->tokens INDEX lv_lhs_pos INTO DATA(ls_leq).
        IF ls_leq-str = '='.
          READ TABLE io_scan->tokens INDEX lv_lhs_pos - 1 INTO DATA(ls_lvar).
          IF sy-subrc = 0.
            lv_lhs = ls_lvar-str.
            REPLACE ALL OCCURRENCES OF 'DATA(' IN lv_lhs WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN lv_lhs WITH ''.
            CONDENSE lv_lhs NO-GAPS.
          ENDIF.
        ENDIF.
      ENDIF.
      " Fallback: rv_x = a * b * get_factor( — '=' стоит на позиции from+1
      IF lv_lhs IS INITIAL.
        DATA(lv_stmt_eq_pos) = i_stmt-from + 1.
        IF lv_stmt_eq_pos <= i_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_stmt_eq_pos INTO DATA(ls_stmt_eq).
          IF ls_stmt_eq-str = '='.
            READ TABLE io_scan->tokens INDEX i_stmt-from INTO DATA(ls_stmt_lhs).
            IF sy-subrc = 0.
              lv_lhs = ls_stmt_lhs-str.
              REPLACE ALL OCCURRENCES OF 'DATA(' IN lv_lhs WITH ''.
              REPLACE ALL OCCURRENCES OF ')' IN lv_lhs WITH ''.
              CONDENSE lv_lhs NO-GAPS.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      " ── Линейный сбор аргументов ──────────────────────────────────
      CLEAR: lt_bind, lv_single, lv_pos.
      lv_pos  = abap_true.
      lv_scan = lv_ti + 1.
      DATA lv_cur_sec TYPE string.
      CLEAR lv_cur_sec.
      WHILE lv_scan <= i_stmt-to.
        READ TABLE io_scan->tokens INDEX lv_scan INTO ls_sa.
        IF sy-subrc <> 0. EXIT. ENDIF.
        lv_sa_str = ls_sa-str.
        IF lv_sa_str = ')' OR lv_sa_str CO ')'. EXIT. ENDIF.
        IF lv_sa_str = '(' OR lv_sa_str = ','. lv_scan = lv_scan + 1. CONTINUE. ENDIF.
        " EXCEPTIONS entries are not data bindings — stop collecting
        IF lv_sa_str = 'EXCEPTIONS'. EXIT. ENDIF.
        IF lv_sa_str = 'EXPORTING' OR lv_sa_str = 'IMPORTING' OR
           lv_sa_str = 'CHANGING'  OR lv_sa_str = 'RECEIVING'.
          lv_cur_sec = lv_sa_str.
          lv_pos = abap_false.
          lv_scan = lv_scan + 1. CONTINUE.
        ENDIF.
        CLEAR ls_eq.
        READ TABLE io_scan->tokens INDEX lv_scan + 1 INTO ls_eq.
        IF ls_eq-str = '='.
          CLEAR ls_val.
          READ TABLE io_scan->tokens INDEX lv_scan + 2 INTO ls_val.
          IF sy-subrc = 0.
            lv_pos = abap_false.
            lv_val_str = ls_val-str.
            REPLACE ALL OCCURRENCES OF ')' IN lv_val_str WITH ''.
            CONDENSE lv_val_str NO-GAPS.
            DATA(lv_bind_dir) = SWITCH char1( lv_cur_sec
              WHEN 'EXPORTING'  THEN 'I'
              WHEN 'IMPORTING'  THEN 'E'
              WHEN 'RECEIVING'  THEN 'E'
              WHEN 'CHANGING'   THEN 'C'
              ELSE                   'I' ).
            CLEAR ls_b. ls_b-inner = lv_sa_str. ls_b-outer = lv_val_str.
            ls_b-dir = lv_bind_dir.
            APPEND ls_b TO lt_bind.
            lv_scan = lv_scan + 3. CONTINUE.
          ENDIF.
        ENDIF.
        IF lv_pos = abap_true AND lv_single IS INITIAL AND lv_sa_str IS NOT INITIAL.
          lv_single = lv_sa_str.
          REPLACE ALL OCCURRENCES OF ')' IN lv_single WITH ''.
          CONDENSE lv_single NO-GAPS.
        ENDIF.
        lv_scan = lv_scan + 1.
      ENDWHILE.

      IF lv_pos = abap_true AND lv_single IS NOT INITIAL.
        CLEAR lv_pref.
        LOOP AT cs_source-t_params INTO DATA(ls_pm)
          WHERE program = i_program
            AND include = i_program
            AND class = lv_call_cls
            AND event = 'METHOD'
            AND name = lv_c-name
            AND type  = 'I'.
          IF ls_pm-preferred = 'X' OR lv_pref IS INITIAL. lv_pref = ls_pm-param. ENDIF.
          IF ls_pm-preferred = 'X'. EXIT. ENDIF.
        ENDLOOP.
        CLEAR ls_b. ls_b-outer = lv_single. ls_b-inner = lv_pref.
        APPEND ls_b TO lt_bind.
      ENDIF.

      " ── RETURNING: добавляем биндинг всегда, если параметр существует ──
      " inner = имя RETURNING-параметра; outer = LHS-переменная (или пусто)
      CLEAR lv_ret.
      LOOP AT cs_source-t_params INTO DATA(ls_ret)
        WHERE class = lv_call_cls AND event = 'METHOD'
          AND name  = lv_c-name   AND type  = 'R'.
        lv_ret = ls_ret-param. EXIT.
      ENDLOOP.
      IF lv_ret IS NOT INITIAL.
        CLEAR ls_b.
        ls_b-inner = lv_ret.
        ls_b-outer = lv_lhs.   " пусто, если нет явного присваивания
        ls_b-dir   = 'E'.
        APPEND ls_b TO lt_bind.
      ENDIF.

      lv_c-bindings = lt_bind.
      APPEND lv_c TO ct_calls.
      lv_ti = lv_ti + 1.
    ENDWHILE.
  ENDMETHOD.
  METHOD parse_stmt_calls.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw_tok).
    CHECK sy-subrc = 0.

    DATA(lv_kw) = SWITCH string( ls_stmt-type
      WHEN 'C' THEN 'COMPUTE'
      WHEN 'D' THEN 'COMPUTE'
      WHEN 'A' THEN '+CALL_METHOD'
      ELSE          ls_kw_tok-str ).

    IF lv_kw = 'CALL'.
      READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok2).
      IF sy-subrc = 0. lv_kw = |CALL { ls_tok2-str }|. ENDIF.
    ENDIF.

    IF lv_kw = 'RAISE'.
      READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_tok2.
      IF sy-subrc = 0.
        CASE ls_tok2-str.
          WHEN 'EVENT'.     lv_kw = 'RAISE EVENT'.
          WHEN 'EXCEPTION'. lv_kw = 'RAISE EXCEPTION'.
          WHEN 'SHORTDUMP'. lv_kw = 'RAISE EXCEPTION'.
        ENDCASE.
      ENDIF.
    ENDIF.

    IF lv_kw = 'NEW'. lv_kw = 'COMPUTE'. ENDIF.

    " CREATE OBJECT may be classified as a generic call statement — force the
    " dedicated branch so the CONSTRUCTOR of the referenced class is recorded.
    IF ls_kw_tok-str = 'CREATE'.
      READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_tok2.
      IF sy-subrc = 0 AND ls_tok2-str = 'OBJECT'. lv_kw = 'CREATE OBJECT'. ENDIF.
    ENDIF.

    DATA(lv_super) = get_super( is_source = cs_source ).
    DATA lt_new_calls TYPE zcl_ace=>tt_calls.

    CASE lv_kw.

      " ── PERFORM ──────────────────────────────────────────────────
      WHEN 'PERFORM'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
        CHECK sy-subrc = 0.
        DATA ls_pf_call  TYPE zcl_ace=>ts_calls.
        DATA lv_pf_sec   TYPE string.
        DATA lv_pf_act_i TYPE i.
        DATA ls_pf_bind  TYPE zif_ace_parse_data=>ts_param_binding.
        ls_pf_call-event = 'FORM'.
        ls_pf_call-name  = ls_tok-str.
        DATA lt_pf_actuals   TYPE string_table.
        DATA lt_pf_act_dirs  TYPE TABLE OF char1 WITH EMPTY KEY.
        DATA lv_pf_cur_dir   TYPE char1 VALUE 'I'.
        DATA(lv_pf_i) = ls_stmt-from + 2.
        WHILE lv_pf_i <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_pf_i INTO DATA(ls_pf_t).
          IF sy-subrc <> 0. EXIT. ENDIF.
          CASE ls_pf_t-str.
            WHEN 'USING'.    lv_pf_cur_dir = 'I'.
            WHEN 'CHANGING'. lv_pf_cur_dir = 'C'.
            WHEN 'TABLES'.   lv_pf_cur_dir = 'C'.
            WHEN OTHERS.
              IF ls_pf_t-str IS NOT INITIAL.
                APPEND ls_pf_t-str TO lt_pf_actuals.
                APPEND lv_pf_cur_dir TO lt_pf_act_dirs.
              ENDIF.
          ENDCASE.
          lv_pf_i = lv_pf_i + 1.
        ENDWHILE.
        DATA lt_pf_params TYPE TABLE OF zcl_ace=>ts_params WITH EMPTY KEY.
        lt_pf_params = VALUE #( FOR p IN cs_source-t_params
          WHERE ( event = 'FORM' AND name = ls_pf_call-name ) ( p ) ).
        SORT lt_pf_params BY line.
        lv_pf_act_i = 1.
        LOOP AT lt_pf_params INTO DATA(ls_pf_p).
          READ TABLE lt_pf_actuals  INDEX lv_pf_act_i INTO DATA(lv_pf_act).
          READ TABLE lt_pf_act_dirs INDEX lv_pf_act_i INTO DATA(lv_pf_dir).
          CLEAR ls_pf_bind.
          ls_pf_bind-outer = lv_pf_act.
          ls_pf_bind-inner = ls_pf_p-param.
          ls_pf_bind-dir   = COND #( WHEN lv_pf_dir IS NOT INITIAL THEN lv_pf_dir ELSE 'I' ).
          APPEND ls_pf_bind TO ls_pf_call-bindings.
          lv_pf_act_i = lv_pf_act_i + 1.
        ENDLOOP.
        IF lt_pf_params IS INITIAL.
          lv_pf_act_i = 1.
          LOOP AT lt_pf_actuals INTO DATA(lv_pf_only).
            READ TABLE lt_pf_act_dirs INDEX lv_pf_act_i INTO DATA(lv_pf_only_dir).
            CLEAR ls_pf_bind.
            ls_pf_bind-outer = lv_pf_only.
            ls_pf_bind-dir   = COND #( WHEN lv_pf_only_dir IS NOT INITIAL THEN lv_pf_only_dir ELSE 'I' ).
            APPEND ls_pf_bind TO ls_pf_call-bindings.
            lv_pf_act_i = lv_pf_act_i + 1.
          ENDLOOP.
        ENDIF.
        APPEND ls_pf_call TO lt_new_calls.

      " ── CALL FUNCTION ────────────────────────────────────────────
      WHEN 'CALL FUNCTION'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0.
        DATA(lv_fname) = ls_tok-str.
        REPLACE ALL OCCURRENCES OF '''' IN lv_fname WITH ''.
        DATA(ls_cf_call) = VALUE zcl_ace=>ts_calls( event = 'FUNCTION' name = lv_fname ).
        " Collect parameter bindings (formal = actual) per section
        DATA lv_cf_sec TYPE string.
        DATA lv_cf_i   TYPE i.
        CLEAR lv_cf_sec.
        lv_cf_i = ls_stmt-from + 3.
        WHILE lv_cf_i <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_cf_i INTO DATA(ls_cf_t).
          IF sy-subrc <> 0. EXIT. ENDIF.
          CASE ls_cf_t-str.
            WHEN 'EXCEPTIONS'. EXIT.
            WHEN 'EXPORTING' OR 'IMPORTING' OR 'CHANGING' OR 'TABLES'.
              lv_cf_sec = ls_cf_t-str.
            WHEN 'DESTINATION' OR 'STARTING' OR 'IN' OR 'PERFORMING' OR 'CALLING'.
              " control clauses, no bindings here
            WHEN '='. " skip
            WHEN OTHERS.
              IF lv_cf_sec IS NOT INITIAL AND ls_cf_t-str IS NOT INITIAL.
                DATA ls_cf_eq LIKE LINE OF io_scan->tokens.
                CLEAR ls_cf_eq.
                READ TABLE io_scan->tokens INDEX lv_cf_i + 1 INTO ls_cf_eq.
                IF sy-subrc = 0 AND ls_cf_eq-str = '='.
                  READ TABLE io_scan->tokens INDEX lv_cf_i + 2 INTO DATA(ls_cf_val).
                  IF sy-subrc = 0 AND ls_cf_val-str IS NOT INITIAL.
                    DATA(lv_cf_act) = ls_cf_val-str.
                    REPLACE ALL OCCURRENCES OF ')' IN lv_cf_act WITH ''.
                    CONDENSE lv_cf_act NO-GAPS.
                    APPEND VALUE zif_ace_parse_data=>ts_param_binding(
                      inner = ls_cf_t-str
                      outer = lv_cf_act
                      dir   = SWITCH char1( lv_cf_sec
                                WHEN 'EXPORTING' THEN 'I'
                                WHEN 'IMPORTING' THEN 'E'
                                WHEN 'TABLES'    THEN 'C'
                                WHEN 'CHANGING'  THEN 'C'
                                ELSE                  'I' ) )
                      TO ls_cf_call-bindings.
                    lv_cf_i = lv_cf_i + 2.
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.
          lv_cf_i = lv_cf_i + 1.
        ENDWHILE.
        APPEND ls_cf_call TO lt_new_calls.

      " ── CALL METHOD / CALL BADI ──────────────────────────────────
      WHEN 'CALL METHOD' OR 'CALL BADI'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0 AND ls_tok-str IS NOT INITIAL.
        DATA(lv_call) = VALUE zcl_ace=>ts_calls( event = 'METHOD' ).
        DATA(lv_str)  = ls_tok-str.
        " Split at the LAST arrow so obj->attr->meth keeps its full chain
        DATA(lv_cm_pi) = find( val = lv_str sub = '->' occ = -1 ).
        DATA(lv_cm_ps) = find( val = lv_str sub = '=>' occ = -1 ).
        IF lv_cm_pi >= 0 AND lv_cm_pi > lv_cm_ps.
          lv_call-class = substring( val = lv_str len = lv_cm_pi ).
          lv_call-name  = substring( val = lv_str off = lv_cm_pi + 2 ).
        ELSEIF lv_cm_ps >= 0.
          lv_call-class = substring( val = lv_str len = lv_cm_ps ).
          lv_call-name  = substring( val = lv_str off = lv_cm_ps + 2 ).
        ELSE.
          lv_call-name = lv_str.
        ENDIF.
        REPLACE ALL OCCURRENCES OF '(' IN lv_call-name WITH ''.
        CONDENSE lv_call-name NO-GAPS.
        IF lv_call-class = 'ME'.
          lv_call-class = mv_class_name.
        ELSEIF lv_call-class = 'SUPER'.
          lv_call-super = abap_true.
          lv_call-class = COND #( WHEN lv_super IS NOT INITIAL THEN lv_super ELSE mv_class_name ).
        ELSEIF lv_call-class CS '->' OR lv_call-class CS '=>'.
          DATA(lv_resolved) = resolve_chain(
            is_source = cs_source i_program = i_program
            i_evtype  = mv_event_type i_evname = mv_event_name
            i_chain   = lv_call-class ).
          IF lv_resolved IS NOT INITIAL.
            lv_call-outer = lv_call-class.
            lv_call-inner = lv_call-name.
            lv_call-class = lv_resolved.
          ENDIF.
        ELSEIF lv_call-class IS NOT INITIAL.
          " The variable is looked up in the scope of the CONTAINING method,
          " not the called one
          lv_resolved = resolve_var_type(
            is_source = cs_source i_program = i_program i_include = i_program
            i_evtype  = mv_event_type i_evname = mv_event_name
            i_varname = lv_call-class i_class = mv_class_name ).
          IF lv_resolved IS NOT INITIAL.
            lv_call-outer = lv_call-class.
            lv_call-inner = lv_call-name.
            lv_call-class = lv_resolved.
          ENDIF.
        ENDIF.
        DATA(lv_section_cm) = ``.
        DATA(lv_tok_cm)     = ls_stmt-from + 3.
        WHILE lv_tok_cm <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_tok_cm INTO DATA(ls_t_cm).
          IF sy-subrc <> 0. EXIT. ENDIF.
          CASE ls_t_cm-str.
            WHEN 'EXPORTING' OR 'IMPORTING' OR 'CHANGING' OR 'RECEIVING'.
              lv_section_cm = ls_t_cm-str.
            WHEN '='. " skip
            WHEN OTHERS.
              IF lv_section_cm IS NOT INITIAL AND ls_t_cm-str IS NOT INITIAL.
                DATA ls_eq_cm LIKE LINE OF io_scan->tokens.
                CLEAR ls_eq_cm.
                READ TABLE io_scan->tokens INDEX lv_tok_cm + 1 INTO ls_eq_cm.
                IF sy-subrc = 0 AND ls_eq_cm-str = '='.
                  READ TABLE io_scan->tokens INDEX lv_tok_cm + 2 INTO DATA(ls_var_cm).
                  IF sy-subrc = 0 AND ls_var_cm-str IS NOT INITIAL.
                    DATA(lv_cm_actual) = ls_var_cm-str.
                    REPLACE ALL OCCURRENCES OF ')' IN lv_cm_actual WITH ''.
                    CONDENSE lv_cm_actual NO-GAPS.
                    DATA(lv_cm_dir) = SWITCH char1( lv_section_cm
                      WHEN 'EXPORTING'  THEN 'I'
                      WHEN 'IMPORTING'  THEN 'E'
                      WHEN 'RECEIVING'  THEN 'E'
                      WHEN 'CHANGING'   THEN 'C'
                      ELSE                   'I' ).
                    APPEND VALUE zif_ace_parse_data=>ts_param_binding(
                      inner = ls_t_cm-str outer = lv_cm_actual dir = lv_cm_dir )
                      TO lv_call-bindings.
                    lv_tok_cm = lv_tok_cm + 2.
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.
          lv_tok_cm = lv_tok_cm + 1.
        ENDWHILE.
        APPEND lv_call TO lt_new_calls.

      " ── RAISE EVENT ──────────────────────────────────────────────
      WHEN 'RAISE EVENT'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0.
        DATA(lv_ev_name) = ls_tok-str.
        LOOP AT cs_source-tt_handler_map INTO DATA(ls_hm)
          WHERE event_name = lv_ev_name.
          APPEND VALUE zcl_ace=>ts_calls(
            event = 'METHOD' class = ls_hm-hdl_class name = ls_hm-hdl_method type = 'H' )
            TO lt_new_calls.
        ENDLOOP.
        IF lt_new_calls IS INITIAL.
          APPEND VALUE zcl_ace=>ts_calls( event = 'EVENT' name = lv_ev_name class = mv_class_name )
            TO lt_new_calls.
        ENDIF.

      " ── COMPUTE / NEW ────────────────────────────────────────────
      WHEN 'COMPUTE'.
        DATA lv_ci TYPE i.
        DATA ls_ct LIKE LINE OF io_scan->tokens.
        DATA ls_cn LIKE LINE OF io_scan->tokens.
        DATA lv_cn TYPE string.
        lv_ci = ls_stmt-from.
        WHILE lv_ci <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_ci INTO ls_ct.
          IF sy-subrc <> 0. EXIT. ENDIF.
          IF ls_ct-str = 'NEW'.
            READ TABLE io_scan->tokens INDEX lv_ci + 1 INTO ls_cn.
            IF sy-subrc = 0 AND ls_cn-str CS '('.
              lv_cn = ls_cn-str.
              REPLACE ALL OCCURRENCES OF '(' IN lv_cn WITH ''.
              CONDENSE lv_cn NO-GAPS.
              " NEW #( ) — inferred type, no class to record
              IF lv_cn IS NOT INITIAL AND lv_cn <> '#'.
                APPEND VALUE zcl_ace=>ts_calls(
                  event = 'METHOD' class = lv_cn name = 'CONSTRUCTOR' ) TO lt_new_calls.
              ENDIF.
              lv_ci = lv_ci + 1.
            ENDIF.
          ENDIF.
          lv_ci = lv_ci + 1.
        ENDWHILE.
        collect_method_calls(
          EXPORTING io_scan = io_scan i_stmt = ls_stmt i_program = i_program
          CHANGING  cs_source = cs_source ct_calls = lt_new_calls ).

      " ── CREATE OBJECT ────────────────────────────────────────────
      " CREATE OBJECT obj [TYPE cls] [EXPORTING p = a ...] → CONSTRUCTOR call.
      " The class is taken from an explicit TYPE clause, otherwise from the
      " declared type (TYPE REF TO cls) of the object variable. This lets the
      " flow descend into the constructor of a global class that is not part
      " of the current program.
      WHEN 'CREATE OBJECT'.
        DATA lv_co_i     TYPE i.
        DATA ls_co_tok   LIKE LINE OF io_scan->tokens.
        DATA lv_co_class TYPE string.
        DATA lv_co_sec   TYPE string.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO DATA(ls_co_var).
        CHECK sy-subrc = 0 AND ls_co_var-str IS NOT INITIAL.
        DATA(lv_co_var) = ls_co_var-str.
        CONDENSE lv_co_var NO-GAPS.
        " Strip an object prefix (ME->attr / obj->attr) so the attribute name
        " alone is resolved against the variable table. The prefix identifies
        " the owning class: ME → the current class, otherwise the referenced
        " object variable.
        DATA lv_co_pref TYPE string.
        CLEAR lv_co_pref.
        IF lv_co_var CS '->'.
          SPLIT lv_co_var AT '->' INTO lv_co_pref lv_co_var.
        ENDIF.

        " Explicit TYPE <class> overrides the declared reference type
        CLEAR lv_co_class.
        lv_co_i = ls_stmt-from + 3.
        WHILE lv_co_i <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_co_i INTO ls_co_tok.
          IF sy-subrc <> 0. EXIT. ENDIF.
          IF ls_co_tok-str = 'EXPORTING'. EXIT. ENDIF.
          IF ls_co_tok-str = 'TYPE'.
            READ TABLE io_scan->tokens INDEX lv_co_i + 1 INTO DATA(ls_co_type).
            IF sy-subrc = 0. lv_co_class = ls_co_type-str. ENDIF.
            EXIT.
          ENDIF.
          lv_co_i = lv_co_i + 1.
        ENDWHILE.

        IF lv_co_class IS INITIAL.
          lv_co_class = resolve_var_type(
            is_source = cs_source i_program = i_program i_include = i_program
            i_evtype  = 'METHOD' i_evname = mv_event_name i_varname = lv_co_var
            i_class   = mv_class_name ).
        ENDIF.

        " Fallback: resolve the attribute in the context of its owning class.
        " For ME-> the owner is the current (real) class, not literally 'ME';
        " for obj-> it is the type of that object variable.
        IF lv_co_class IS INITIAL AND lv_co_pref IS NOT INITIAL.
          DATA(lv_co_owner) = COND string(
            WHEN lv_co_pref = 'ME' OR lv_co_pref = 'SUPER' THEN mv_class_name
            ELSE resolve_var_type(
              is_source = cs_source i_program = i_program i_include = i_program
              i_evtype  = 'METHOD' i_evname = mv_event_name i_varname = lv_co_pref
              i_class   = mv_class_name ) ).
          IF lv_co_owner IS NOT INITIAL.
            LOOP AT cs_source-t_vars INTO DATA(ls_co_attr)
              WHERE class = lv_co_owner AND name = lv_co_var AND type IS NOT INITIAL.
              lv_co_class = ls_co_attr-type. EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.

        REPLACE ALL OCCURRENCES OF '''' IN lv_co_class WITH ''.
        REPLACE ALL OCCURRENCES OF '(' IN lv_co_class WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN lv_co_class WITH ''.
        CONDENSE lv_co_class NO-GAPS.
        CHECK lv_co_class IS NOT INITIAL.

        DATA(ls_co_call) = VALUE zcl_ace=>ts_calls(
          event = 'METHOD' class = lv_co_class name = 'CONSTRUCTOR' ).

        " Collect EXPORTING bindings (actual → formal, direction 'I')
        CLEAR lv_co_sec.
        lv_co_i = ls_stmt-from + 3.
        WHILE lv_co_i <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_co_i INTO ls_co_tok.
          IF sy-subrc <> 0. EXIT. ENDIF.
          CASE ls_co_tok-str.
            WHEN 'EXPORTING'. lv_co_sec = ls_co_tok-str.
            WHEN '='. " skip
            WHEN OTHERS.
              IF lv_co_sec IS NOT INITIAL AND ls_co_tok-str IS NOT INITIAL.
                READ TABLE io_scan->tokens INDEX lv_co_i + 1 INTO DATA(ls_co_eq).
                IF ls_co_eq-str = '='.
                  READ TABLE io_scan->tokens INDEX lv_co_i + 2 INTO DATA(ls_co_val).
                  IF sy-subrc = 0 AND ls_co_val-str IS NOT INITIAL.
                    DATA(lv_co_act) = ls_co_val-str.
                    REPLACE ALL OCCURRENCES OF ')' IN lv_co_act WITH ''.
                    CONDENSE lv_co_act NO-GAPS.
                    APPEND VALUE zif_ace_parse_data=>ts_param_binding(
                      inner = ls_co_tok-str outer = lv_co_act dir = 'I' )
                      TO ls_co_call-bindings.
                    lv_co_i = lv_co_i + 2.
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.
          lv_co_i = lv_co_i + 1.
        ENDWHILE.

        APPEND ls_co_call TO lt_new_calls.

      " ── +CALL_METHOD ─────────────────────────────────────────────
      WHEN '+CALL_METHOD'.
        collect_method_calls(
          EXPORTING io_scan = io_scan i_stmt = ls_stmt i_program = i_program
          CHANGING  cs_source = cs_source ct_calls = lt_new_calls ).

      " ── RAISE EXCEPTION / RAISE SHORTDUMP ────────────────────────
      " RAISE EXCEPTION TYPE cls [EXPORTING …] → CONSTRUCTOR call;
      " RAISE EXCEPTION NEW cls( … ) is picked up by collect_method_calls.
      WHEN 'RAISE EXCEPTION'.
        DATA lv_rx_i TYPE i.
        lv_rx_i = ls_stmt-from + 2.
        WHILE lv_rx_i <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_rx_i INTO DATA(ls_rx_t).
          IF sy-subrc <> 0. EXIT. ENDIF.
          IF ls_rx_t-str = 'TYPE'.
            READ TABLE io_scan->tokens INDEX lv_rx_i + 1 INTO DATA(ls_rx_cls).
            IF sy-subrc = 0 AND ls_rx_cls-str IS NOT INITIAL.
              APPEND VALUE zcl_ace=>ts_calls(
                event = 'METHOD' class = ls_rx_cls-str name = 'CONSTRUCTOR' )
                TO lt_new_calls.
            ENDIF.
            EXIT.
          ENDIF.
          lv_rx_i = lv_rx_i + 1.
        ENDWHILE.
        collect_method_calls(
          EXPORTING io_scan = io_scan i_stmt = ls_stmt i_program = i_program
          CHANGING  cs_source = cs_source ct_calls = lt_new_calls ).

      " ── Fallback: functional calls inside any other statement ────
      " IF check( ) = abap_true. / WHILE has_next( ). / APPEND build( ) TO …
      " / RETURN meth( ). / string templates — everything that is neither a
      " COMPUTE nor an explicit call statement. Declarations and SQL are
      " skipped via C_SKIP_KEYWORDS.
      WHEN OTHERS.
        IF mv_skip_keywords IS INITIAL.
          mv_skip_keywords =
            `;CLASS;ENDCLASS;INTERFACE;ENDINTERFACE;INTERFACES;ALIASES;METHODS;CLASS-METHODS;`
            && `METHOD;ENDMETHOD;EVENTS;CLASS-EVENTS;DATA;CLASS-DATA;TYPES;CONSTANTS;STATICS;`
            && `FIELD-SYMBOLS;PARAMETERS;SELECT-OPTIONS;SELECTION-SCREEN;TABLES;RANGES;NODES;`
            && `FORM;ENDFORM;FUNCTION;ENDFUNCTION;MODULE;ENDMODULE;REPORT;PROGRAM;INCLUDE;`
            && `TYPE-POOLS;SELECT;ENDSELECT;WITH;UPDATE;DELETE;MODIFY;INSERT;OPEN;FETCH;CLOSE;`
            && `EXEC;ENDEXEC;DEFINE;END-OF-DEFINITION;`.
        ENDIF.
        IF mv_skip_keywords NS |;{ lv_kw };|.
          DATA lv_fb_i    TYPE i.
          DATA lv_fb_hit  TYPE abap_bool.
          CLEAR lv_fb_hit.
          lv_fb_i = ls_stmt-from.
          WHILE lv_fb_i <= ls_stmt-to.
            READ TABLE io_scan->tokens INDEX lv_fb_i INTO DATA(ls_fb_t).
            IF sy-subrc <> 0. EXIT. ENDIF.
            IF ls_fb_t-str CS '->' OR ls_fb_t-str CS '=>'
              OR ls_fb_t-str = 'NEW'
              OR ( ls_fb_t-str CA '(' AND lv_fb_i > ls_stmt-from ).
              lv_fb_hit = abap_true.
              EXIT.
            ENDIF.
            lv_fb_i = lv_fb_i + 1.
          ENDWHILE.
          IF lv_fb_hit = abap_true.
            collect_method_calls(
              EXPORTING io_scan = io_scan i_stmt = ls_stmt i_program = i_program
              CHANGING  cs_source = cs_source ct_calls = lt_new_calls ).
          ENDIF.
        ENDIF.

    ENDCASE.

    CHECK lt_new_calls IS NOT INITIAL.

    LOOP AT cs_source-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE include = i_include.
      READ TABLE <prog>-t_keywords WITH KEY index = i_stmt_idx ASSIGNING FIELD-SYMBOL(<kw>) BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT lt_new_calls INTO DATA(ls_nc).
          READ TABLE <kw>-tt_calls WITH KEY event = ls_nc-event
                                            name  = ls_nc-name
                                            class = ls_nc-class
            TRANSPORTING NO FIELDS BINARY SEARCH.
          IF sy-subrc <> 0.
            APPEND ls_nc TO <kw>-tt_calls.
          ENDIF.
        ENDLOOP.
      ENDIF.
      EXIT.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS ZCL_ACE_PARSE_CALCS IMPLEMENTATION.
  METHOD zif_ace_stmt_handler~handle.
    CHECK i_stmt_idx > 0.

    DATA(lv_is_call_method) = abap_false.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    DATA(lv_stmt_kw) = SWITCH string( ls_stmt-type
      WHEN 'A' THEN '+CALL_METHOD'
      ELSE '' ).
    IF lv_stmt_kw = '+CALL_METHOD'. lv_is_call_method = abap_true. ENDIF.

    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
    CHECK sy-subrc = 0.

    DATA(lv_line) = ls_kw-row.

    " ── Ищем первый '=' ──────────────────────────────────────────
    DATA lv_eq_idx  TYPE i VALUE 0.
    DATA lv_tok_pos TYPE i.
    lv_tok_pos = ls_stmt-from.
    WHILE lv_tok_pos <= ls_stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_pos INTO DATA(ls_tok).
      IF sy-subrc <> 0. EXIT. ENDIF.
      IF ls_tok-str = '='. lv_eq_idx = lv_tok_pos. EXIT. ENDIF.
      lv_tok_pos = lv_tok_pos + 1.
    ENDWHILE.
    CHECK lv_eq_idx > 0 OR lv_is_call_method = abap_true.

    IF lv_is_call_method = abap_false.
    " ── LHS → t_calculated ───────────────────────────────────────
    lv_tok_pos = ls_stmt-from.
    WHILE lv_tok_pos < lv_eq_idx.
      READ TABLE io_scan->tokens INDEX lv_tok_pos INTO ls_tok.
      IF sy-subrc <> 0. EXIT. ENDIF.
      DATA(lv_lhs) = ls_tok-str.
      REPLACE ALL OCCURRENCES OF 'DATA('         IN lv_lhs WITH ''.
      REPLACE ALL OCCURRENCES OF 'FIELD-SYMBOL(' IN lv_lhs WITH ''.
      REPLACE ALL OCCURRENCES OF 'FINAL('        IN lv_lhs WITH ''.
      REPLACE ALL OCCURRENCES OF ')'             IN lv_lhs WITH ''.
      CONDENSE lv_lhs NO-GAPS.
      IF lv_lhs CS '-'.
        DATA lv_dummy TYPE string.
        SPLIT lv_lhs AT '-' INTO lv_lhs lv_dummy.
      ENDIF.
      IF is_varname( lv_lhs ) = abap_true.
        append_calc( EXPORTING i_name      = lv_lhs
                               i_program   = i_program
                               i_include   = i_include
                               i_class     = i_class
                               i_eventtype = i_evtype
                               i_eventname = i_ev_name
                               i_line      = lv_line
                     CHANGING  cs_source   = cs_source ).
      ENDIF.
      lv_tok_pos = lv_tok_pos + 1.
    ENDWHILE.

    " ── RHS → t_composed (только переменные вне вызовов) ─────────
    DATA lv_prev_arrow  TYPE abap_bool.
    DATA lv_skip_next   TYPE abap_bool.
    DATA lv_call_depth  TYPE i VALUE 0.
    lv_tok_pos = lv_eq_idx + 1.
    WHILE lv_tok_pos <= ls_stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_pos INTO ls_tok.
      IF sy-subrc <> 0. EXIT. ENDIF.
      DATA(lv_rhs) = ls_tok-str.

      IF ( lv_rhs CS '->' OR lv_rhs CS '=>' ) AND lv_rhs CS '('.
        lv_call_depth = lv_call_depth + 1.
        lv_prev_arrow = abap_false.
        lv_tok_pos = lv_tok_pos + 1. CONTINUE.
      ENDIF.

      IF lv_rhs = '->' OR lv_rhs = '=>'.
        lv_prev_arrow = abap_true.
        lv_tok_pos = lv_tok_pos + 1. CONTINUE.
      ENDIF.

      IF lv_prev_arrow = abap_true.
        lv_prev_arrow = abap_false.
        IF lv_rhs CS '('.
          READ TABLE io_scan->tokens INDEX lv_tok_pos - 2 INTO DATA(ls_prev_obj).
          IF sy-subrc = 0.
            DELETE cs_source-t_composed WHERE program = i_program
              AND include = i_include AND class = i_class
              AND eventtype = i_evtype AND eventname = i_ev_name
              AND line = lv_line AND name = ls_prev_obj-str.
          ENDIF.
          lv_call_depth = lv_call_depth + 1.
        ENDIF.
        lv_tok_pos = lv_tok_pos + 1. CONTINUE.
      ENDIF.

      IF lv_rhs CS '(' AND NOT lv_rhs = '('.
        IF NOT ( lv_rhs CP 'DATA(*' OR lv_rhs CP 'FIELD-SYMBOL(*' OR
                 lv_rhs CP 'FINAL(*' OR lv_rhs CP 'VALUE(*' OR
                 lv_rhs CP 'CONV(*'  OR lv_rhs CP 'CAST(*'  OR
                 lv_rhs CP 'COND(*'  OR lv_rhs CP 'SWITCH(*' OR
                 lv_rhs CP 'REF(*' ).
          lv_call_depth = lv_call_depth + 1.
          lv_tok_pos = lv_tok_pos + 1. CONTINUE.
        ENDIF.
      ENDIF.

      DATA(lv_close_count) = strlen( lv_rhs )
        - strlen( replace( val = lv_rhs sub = ')' with = '' occ = 0 ) ).
      IF lv_close_count > 0.
        IF lv_call_depth >= lv_close_count.
          lv_call_depth = lv_call_depth - lv_close_count.
        ELSE.
          lv_call_depth = 0.
        ENDIF.
      ENDIF.

      IF lv_call_depth > 0.
        lv_tok_pos = lv_tok_pos + 1. CONTINUE.
      ENDIF.

      IF lv_rhs = 'NEW' OR lv_rhs = 'CAST' OR lv_rhs = 'REF'.
        lv_skip_next = abap_true.
        lv_tok_pos = lv_tok_pos + 1. CONTINUE.
      ENDIF.
      IF lv_skip_next = abap_true.
        lv_skip_next = abap_false.
        lv_tok_pos = lv_tok_pos + 1. CONTINUE.
      ENDIF.

      DATA(lv_comp) = lv_rhs.
      REPLACE ALL OCCURRENCES OF ')' IN lv_comp WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN lv_comp WITH ''.
      CONDENSE lv_comp NO-GAPS.
      IF lv_comp CS '-'.
        SPLIT lv_comp AT '-' INTO lv_comp lv_dummy.
      ENDIF.
      IF is_varname( lv_comp ) = abap_true.
        append_comp( EXPORTING i_name      = lv_comp
                               i_program   = i_program
                               i_include   = i_include
                               i_class     = i_class
                               i_eventtype = i_evtype
                               i_eventname = i_ev_name
                               i_line      = lv_line
                     CHANGING  cs_source   = cs_source ).
      ENDIF.
      lv_tok_pos = lv_tok_pos + 1.
    ENDWHILE.

    ENDIF. " lv_is_call_method = abap_false

    " ── TT_CALLS BINDINGS → t_composed / t_calculated ────────────
    LOOP AT cs_source-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE include = i_include.
      READ TABLE <prog>-t_keywords WITH KEY index = i_stmt_idx
        ASSIGNING FIELD-SYMBOL(<kword>) BINARY SEARCH.
      IF sy-subrc <> 0.
       " MESSAGE |CALCS: stmt={ i_stmt_idx } line={ lv_line } — keyword NOT FOUND in t_keywords| TYPE 'I'.
        EXIT.
      ENDIF.

      DATA(lv_calls_cnt) = lines( <kword>-tt_calls ).
      "MESSAGE |CALCS: stmt={ i_stmt_idx } line={ lv_line } kw={ <kword>-name } calls={ lv_calls_cnt }| TYPE 'I'.

      LOOP AT <kword>-tt_calls INTO DATA(ls_call).
        DATA(lv_bind_cnt) = lines( ls_call-bindings ).
        DATA(lv_has_e_bind) = abap_false.
        LOOP AT ls_call-bindings INTO DATA(ls_bind).
          DATA(lv_outer) = ls_bind-outer.
          CHECK lv_outer IS NOT INITIAL.
          REPLACE ALL OCCURRENCES OF ')' IN lv_outer WITH ''.
          CONDENSE lv_outer NO-GAPS.
          IF lv_outer CS '-'.
            SPLIT lv_outer AT '-' INTO lv_outer lv_dummy.
          ENDIF.
          CHECK is_varname( lv_outer ) = abap_true.
          CASE ls_bind-dir.
            WHEN 'I'.
              append_comp( EXPORTING i_name      = lv_outer
                                     i_program   = i_program
                                     i_include   = i_include
                                     i_class     = i_class
                                     i_eventtype = i_evtype
                                     i_eventname = i_ev_name
                                     i_line      = lv_line
                           CHANGING  cs_source   = cs_source ).
            WHEN 'E' OR 'C'.
              lv_has_e_bind = abap_true.
              append_comp( EXPORTING i_name      = lv_outer
                                     i_program   = i_program
                                     i_include   = i_include
                                     i_class     = i_class
                                     i_eventtype = i_evtype
                                     i_eventname = i_ev_name
                                     i_line      = lv_line
                           CHANGING  cs_source   = cs_source ).
              append_calc( EXPORTING i_name      = lv_outer
                                     i_program   = i_program
                                     i_include   = i_include
                                     i_class     = i_class
                                     i_eventtype = i_evtype
                                     i_eventname = i_ev_name
                                     i_line      = lv_line
                           CHANGING  cs_source   = cs_source ).
          ENDCASE.
        ENDLOOP.
        " Нет binding dir='E' — вызов встроен в выражение (rv = A * meth(...)).
        " Добавляем RETURNING-параметр метода в t_calculated чтобы
        " propagate_vars_backward мог по нему найти входные параметры.
        IF lv_has_e_bind = abap_false.
          DATA(lv_ret_cls) = COND string(
            WHEN ls_call-class IS NOT INITIAL THEN ls_call-class
            ELSE i_class ).
          LOOP AT cs_source-t_params INTO DATA(ls_ret_p)
            WHERE class = lv_ret_cls
              AND event = 'METHOD'
              AND name  = ls_call-name
              AND type  = 'R'.
            append_calc( EXPORTING i_name      = ls_ret_p-param
                                   i_program   = i_program
                                   i_include   = i_include
                                   i_class     = i_class
                                   i_eventtype = i_evtype
                                   i_eventname = i_ev_name
                                   i_line      = lv_line
                         CHANGING  cs_source   = cs_source ).
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
      EXIT.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_varname.
    CHECK i_str IS NOT INITIAL.
    CHECK NOT ( i_str CO '0123456789+-*/%&|<>=!()[]{}.,;:' ).
    CHECK i_str+0(1) <> ''''.
    CHECK i_str+0(1) <> '`'.
    CHECK i_str+0(1) <> '|'.
    CHECK i_str+0(1) <> '#'.
    CASE to_upper( i_str ).
      WHEN 'ABAP_TRUE' OR 'ABAP_FALSE' OR 'ABAP_ON' OR 'ABAP_OFF'
        OR 'SPACE' OR 'TRUE' OR 'FALSE'
        OR 'IF' OR 'ELSE' OR 'ENDIF' OR 'AND' OR 'OR' OR 'NOT'
        OR 'WHEN' OR 'THEN' OR 'COND' OR 'SWITCH' OR 'VALUE' OR 'CONV'
        OR 'LINES' OR 'LINE_INDEX' OR 'LINE_EXISTS'
        OR 'STRLEN' OR 'XSTRLEN' OR 'NUMOFCHAR'
        OR 'TO' OR 'FROM' OR 'IN' OR 'OF' OR 'BY' OR 'UP'
        OR 'EQ' OR 'NE' OR 'LT' OR 'LE' OR 'GT' OR 'GE'
        OR 'CO' OR 'CN' OR 'CA' OR 'NA' OR 'CS' OR 'NS' OR 'CP' OR 'NP'
        OR 'BIT-AND' OR 'BIT-OR' OR 'BIT-XOR' OR 'BIT-NOT'
        OR 'INITIAL' OR 'BOUND' OR 'SUPPLIED' OR 'REQUESTED'
        OR 'IS' OR 'BETWEEN' OR 'NEW' OR 'CAST' OR 'REF'
        OR 'DATA' OR 'FIELD-SYMBOL' OR 'FINAL' OR 'TABLE'
        OR 'EXACT' OR 'BASE' OR 'CORRESPONDING'
        OR 'XSDBOOL' OR 'BOOLC' OR 'BOOLX'
        OR 'ME' OR 'SUPER' OR 'RESULT'.
        rv_yes = abap_false. RETURN.
    ENDCASE.
    rv_yes = abap_true.
  ENDMETHOD.
  METHOD append_calc.
    " Дедупликация делается в GET_CODE_FLOW через SORT + DELETE ADJACENT DUPLICATES
    APPEND VALUE zcl_ace=>ts_var(
      program = i_program include = i_include
      class = i_class eventtype = i_eventtype eventname = i_eventname
      line = i_line name = i_name )
      TO cs_source-t_calculated.

  ENDMETHOD.
  METHOD append_comp.
    " Дедупликация делается в GET_CODE_FLOW через SORT + DELETE ADJACENT DUPLICATES
    APPEND VALUE zcl_ace=>ts_var(
      program = i_program include = i_include
      class = i_class eventtype = i_eventtype eventname = i_eventname
      line = i_line name = i_name )
      TO cs_source-t_composed.

  ENDMETHOD.
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
      " No keyword pre-filter here: zcl_ace_parse_calls dispatches by itself
      " (incl. the generic fallback for calls inside IF/WHILE/APPEND/…)
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
            i_class = i_class i_evtype = i_evtype i_ev_name = i_ev_name
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
    volume      = format_f2( lv_tot_vol )
    difficulty  = format_f2( ls_result-incl_difficulty )
    effort      = format_f2( lv_tot_eff )
    time_t      = format_time( lv_tot_time_t )
    bugs        = format_f2( lv_tot_bugs )
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

  " --- Section 2: Total (built first so header can reference its values) ---
  " Compute per-group subtotals for Events, Forms, each Class
  TYPES: BEGIN OF lty_cls_sub,
           name    TYPE string,
           units   TYPE i,
           cc      TYPE i,
           loc     TYPE i,
           lloc    TYPE i,
           cloc    TYPE i,
           n1      TYPE i,
           n2      TYPE i,
           vol     TYPE f,
           eff     TYPE f,
           time_t  TYPE f,
           bugs    TYPE f,
         END OF lty_cls_sub.
  DATA lt_cls_sub  TYPE TABLE OF lty_cls_sub WITH EMPTY KEY.
  DATA ls_ev_sub   TYPE lty_cls_sub.
  DATA ls_fo_sub   TYPE lty_cls_sub.
  ls_ev_sub-name = 'Events TOTAL'.
  ls_fo_sub-name = 'Forms TOTAL'.

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
        <cls_sub>-units = <cls_sub>-units + 1.
        <cls_sub>-cc = <cls_sub>-cc + ls_u-cyclomatic.
        <cls_sub>-loc = <cls_sub>-loc + ls_u-loc.
        <cls_sub>-lloc = <cls_sub>-lloc + ls_u-lloc.
        <cls_sub>-cloc = <cls_sub>-cloc + ls_u-cloc.
        <cls_sub>-n1 = <cls_sub>-n1 + ls_u-n1.
        <cls_sub>-n2 = <cls_sub>-n2 + ls_u-n2.
        <cls_sub>-vol     = <cls_sub>-vol  + ls_u-volume.
        <cls_sub>-eff     = <cls_sub>-eff  + ls_u-effort.
        <cls_sub>-time_t  = <cls_sub>-time_t + ls_u-time_t.
        <cls_sub>-bugs    = <cls_sub>-bugs + ls_u-bugs.
      WHEN 'FORM'.
        ls_fo_sub-units = ls_fo_sub-units + 1.
        ls_fo_sub-cc = ls_fo_sub-cc + ls_u-cyclomatic.
        ls_fo_sub-loc = ls_fo_sub-loc + ls_u-loc.
        ls_fo_sub-lloc = ls_fo_sub-lloc + ls_u-lloc.
        ls_fo_sub-cloc = ls_fo_sub-cloc + ls_u-cloc.
        ls_fo_sub-n1 = ls_fo_sub-n1 + ls_u-n1.
        ls_fo_sub-n2 = ls_fo_sub-n2 + ls_u-n2.
        ls_fo_sub-vol     = ls_fo_sub-vol  + ls_u-volume.
        ls_fo_sub-eff     = ls_fo_sub-eff  + ls_u-effort.
        ls_fo_sub-time_t  = ls_fo_sub-time_t + ls_u-time_t.
        ls_fo_sub-bugs    = ls_fo_sub-bugs + ls_u-bugs.
      WHEN OTHERS.
        ls_ev_sub-units = ls_ev_sub-units + 1.
        ls_ev_sub-cc = ls_ev_sub-cc + ls_u-cyclomatic.
        ls_ev_sub-loc = ls_ev_sub-loc + ls_u-loc.
        ls_ev_sub-lloc = ls_ev_sub-lloc + ls_u-lloc.
        ls_ev_sub-cloc = ls_ev_sub-cloc + ls_u-cloc.
        ls_ev_sub-n1 = ls_ev_sub-n1 + ls_u-n1.
        ls_ev_sub-n2 = ls_ev_sub-n2 + ls_u-n2.
        ls_ev_sub-vol     = ls_ev_sub-vol  + ls_u-volume.
        ls_ev_sub-eff     = ls_ev_sub-eff  + ls_u-effort.
        ls_ev_sub-time_t  = ls_ev_sub-time_t + ls_u-time_t.
        ls_ev_sub-bugs    = ls_ev_sub-bugs + ls_u-bugs.
    ENDCASE.
  ENDLOOP.

  " Helper macro: append a subtotal row from lty_cls_sub
  DATA lt_total TYPE tt_row.
  DATA ls_sub   TYPE lty_cls_sub.

  IF ls_ev_sub-units > 0.
    DATA(lv_ev_ratio) = COND string( WHEN ls_ev_sub-loc > 0
      THEN |{ CONV decfloat16( ls_ev_sub-cloc * 100 / ls_ev_sub-loc ) DECIMALS = 1 }%| ELSE '-' ).
    APPEND VALUE ts_row(
      name = 'Events'  units = ls_ev_sub-units  cc = ls_ev_sub-cc
      n1 = ls_ev_sub-n1  n2 = ls_ev_sub-n2
      loc = ls_ev_sub-loc  lloc = ls_ev_sub-lloc  cloc = ls_ev_sub-cloc  cloc_ratio = lv_ev_ratio
      volume = format_f2( ls_ev_sub-vol )  effort = format_f2( ls_ev_sub-eff )
      time_t = format_time( ls_ev_sub-time_t )  bugs = format_f2( ls_ev_sub-bugs )
    ) TO lt_total.
  ENDIF.

  IF ls_fo_sub-units > 0.
    DATA(lv_fo_ratio) = COND string( WHEN ls_fo_sub-loc > 0
      THEN |{ CONV decfloat16( ls_fo_sub-cloc * 100 / ls_fo_sub-loc ) DECIMALS = 1 }%| ELSE '-' ).
    APPEND VALUE ts_row(
      name = 'Forms'  units = ls_fo_sub-units  cc = ls_fo_sub-cc
      n1 = ls_fo_sub-n1  n2 = ls_fo_sub-n2
      loc = ls_fo_sub-loc  lloc = ls_fo_sub-lloc  cloc = ls_fo_sub-cloc  cloc_ratio = lv_fo_ratio
      volume = format_f2( ls_fo_sub-vol )  effort = format_f2( ls_fo_sub-eff )
      time_t = format_time( ls_fo_sub-time_t )  bugs = format_f2( ls_fo_sub-bugs )
    ) TO lt_total.
  ENDIF.

  LOOP AT lt_cls_sub INTO ls_sub.
    DATA(lv_cls_ratio) = COND string( WHEN ls_sub-loc > 0
      THEN |{ CONV decfloat16( ls_sub-cloc * 100 / ls_sub-loc ) DECIMALS = 1 }%| ELSE '-' ).
    READ TABLE ls_result-class_totals WITH KEY class_name = ls_sub-name INTO DATA(ls_ct2).
    IF sy-subrc <> 0. CLEAR ls_ct2. ENDIF.
    APPEND VALUE ts_row(
      name = ls_sub-name  units = ls_sub-units  cc = ls_sub-cc
      n1 = ls_sub-n1  n2 = ls_sub-n2
      eta1 = ls_ct2-cls_big_n1  eta2 = ls_ct2-cls_big_n2
      vocab = ls_ct2-cls_vocabulary  length = ls_ct2-cls_prog_length
      loc = ls_sub-loc  lloc = ls_sub-lloc  cloc = ls_sub-cloc  cloc_ratio = lv_cls_ratio
      volume = format_f2( ls_sub-vol )  difficulty = format_f2( ls_ct2-cls_difficulty )
      effort = format_f2( ls_sub-eff )
      time_t = format_time( ls_sub-time_t )  bugs = format_f2( ls_sub-bugs )
    ) TO lt_total.
  ENDLOOP.

  " Grand total row
  APPEND VALUE ts_row(
    name        = |{ i_program } TOTAL|
    units       = lines( ls_result-units )
    cc          = lv_tot_cc
    n1          = lv_tot_n1        n2     = lv_tot_n2
    eta1        = ls_result-incl_big_n1
    eta2        = ls_result-incl_big_n2
    vocab       = ls_result-incl_vocabulary
    length      = ls_result-incl_prog_length
    loc         = lv_tot_loc       lloc   = lv_tot_lloc   cloc = lv_tot_cloc
    cloc_ratio  = lv_ratio
    volume      = format_f2( lv_tot_vol )
    difficulty  = format_f2( ls_result-incl_difficulty )
    effort      = format_f2( lv_tot_eff )
    time_t      = format_time( lv_tot_time_t )
    bugs        = format_f2( lv_tot_bugs )
  ) TO lt_total.
  DATA(ls_tot) = lt_total[ lines( lt_total ) ].

  " --- Section 1: Summary ---
  APPEND |<h2>Code Metrics: { i_program }</h2>| TO rv.
  APPEND |<p>Units analysed: <b>{ lines( ls_result-units ) }</b></p>| TO rv.
  APPEND |<p>Total Cyclomatic Complexity: <b>{ lv_tot_cc }</b>| TO rv.
  APPEND |&nbsp;&nbsp;Avg CC / unit: | &&
         |<b>{ format_f2( ls_result-avg_cyclomatic ) }</b></p>| TO rv.
  APPEND |<p>Halstead Volume: <b>{ ls_tot-volume }</b>| TO rv.
  APPEND |&nbsp;&nbsp;Effort: <b>{ ls_tot-effort }</b></p>| TO rv.
  APPEND |<p>Time: <b>{ ls_tot-time_t }</b>| &&
         |&nbsp;&nbsp;Expected Bugs: <b>{ ls_tot-bugs }</b></p>| TO rv.
  APPEND |<p>LOC / LLOC / CLOC / CLOC%: | &&
         |<b>{ lv_tot_loc }</b> / <b>{ lv_tot_lloc }</b> / | TO rv.
  APPEND |<b>{ lv_tot_cloc }</b> / <b>{ lv_ratio }</b></p>| TO rv.

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
      name        = ls_u-unit_name  units = 1
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
    IF ls_ev_sub-units > 1.
      DATA(lv_evr) = COND string( WHEN ls_ev_sub-loc > 0
        THEN |{ CONV decfloat16( ls_ev_sub-cloc * 100 / ls_ev_sub-loc ) DECIMALS = 1 }%| ELSE '-' ).
      APPEND VALUE ts_row(
        name = 'TOTAL'  units = ls_ev_sub-units  cc = ls_ev_sub-cc
        n1 = ls_ev_sub-n1  n2 = ls_ev_sub-n2
        loc = ls_ev_sub-loc  lloc = ls_ev_sub-lloc  cloc = ls_ev_sub-cloc  cloc_ratio = lv_evr
        volume = format_f2( ls_ev_sub-vol )  effort = format_f2( ls_ev_sub-eff )
        time_t = format_time( ls_ev_sub-time_t )  bugs = format_f2( ls_ev_sub-bugs )
      ) TO lt_events.
    ENDIF.
    html_section( EXPORTING i_name = 'Events' it_rows = lt_events i_numbered = abap_true CHANGING ct_html = rv ).
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
      name        = ls_u-unit_name  units = 1
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
    IF ls_fo_sub-units > 1.
      DATA(lv_for) = COND string( WHEN ls_fo_sub-loc > 0
        THEN |{ CONV decfloat16( ls_fo_sub-cloc * 100 / ls_fo_sub-loc ) DECIMALS = 1 }%| ELSE '-' ).
      APPEND VALUE ts_row(
        name = 'TOTAL'  units = ls_fo_sub-units  cc = ls_fo_sub-cc
        n1 = ls_fo_sub-n1  n2 = ls_fo_sub-n2
        loc = ls_fo_sub-loc  lloc = ls_fo_sub-lloc  cloc = ls_fo_sub-cloc  cloc_ratio = lv_for
        volume = format_f2( ls_fo_sub-vol )  effort = format_f2( ls_fo_sub-eff )
        time_t = format_time( ls_fo_sub-time_t )  bugs = format_f2( ls_fo_sub-bugs )
      ) TO lt_forms.
    ENDIF.
    html_section( EXPORTING i_name = 'Forms' it_rows = lt_forms i_numbered = abap_true CHANGING ct_html = rv ).
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
        units       = 1
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
      units       = lines( lt_rows )
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

    html_section( EXPORTING i_name = lv_cls it_rows = lt_rows i_numbered = abap_true CHANGING ct_html = rv ).
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
      i_name     = 'All Methods (sorted by CC)'
      it_rows    = lt_all
      i_numbered = abap_true
      CHANGING ct_html = rv ).
  ENDIF.

  " --- Legend ---
  APPEND '<h3>LOC / LLOC / CLOC</h3><pre>' TO rv.
  APPEND '  LOC   - Lines of Code (total lines including blanks and comments)' TO rv.
  APPEND '  LLOC  - Logical Lines of Code (executable statements only)' TO rv.
  APPEND '  CLOC  - Comment Lines of Code (lines containing comments)' TO rv.
  APPEND '  CLOC% - Comment density = CLOC / LOC * 100' TO rv.
  APPEND '</pre>' TO rv.
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
    lv_num = lv_num + 1.
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

CLASS ZCL_ACE_METRICS IMPLEMENTATION.
  METHOD calculate.

    rs_result-program = i_program.

    LOOP AT is_parse_data-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE program = i_program.

      DATA(lo_scan) = <prog>-scan.
      CHECK lo_scan IS BOUND.
      CHECK lo_scan->statements IS NOT INITIAL.

      TYPES: BEGIN OF ts_boundary,
               stmt_from TYPE i,
               stmt_to   TYPE i,
               unit_type TYPE string,
               unit_name TYPE string,
               class     TYPE string,
             END OF ts_boundary.
      DATA lt_boundaries TYPE SORTED TABLE OF ts_boundary
        WITH UNIQUE KEY stmt_from.
      CLEAR lt_boundaries.

      LOOP AT is_parse_data-tt_calls_line INTO DATA(ls_cl)
        WHERE include  = <prog>-include
          AND index    > 0
          AND ( eventtype = 'METHOD'   OR eventtype = 'FORM'
             OR eventtype = 'MODULE'   OR eventtype = 'FUNCTION' ).

        CHECK ls_cl-index <> ls_cl-def_ind.
        READ TABLE lt_boundaries WITH KEY stmt_from = ls_cl-index TRANSPORTING NO FIELDS.
        CHECK sy-subrc <> 0.

        DATA(lv_end_kw) = SWITCH string( ls_cl-eventtype
          WHEN 'METHOD'   THEN 'ENDMETHOD'
          WHEN 'FORM'     THEN 'ENDFORM'
          WHEN 'MODULE'   THEN 'ENDMODULE'
          WHEN 'FUNCTION' THEN 'ENDFUNCTION'
          ELSE '' ).

        DATA lv_stmt_to TYPE i VALUE 0.
        LOOP AT <prog>-t_keywords INTO DATA(ls_kw)
          WHERE index > ls_cl-index AND name = lv_end_kw.
          lv_stmt_to = ls_kw-index.
          EXIT.
        ENDLOOP.
        CHECK lv_stmt_to > 0.

        INSERT VALUE ts_boundary(
          stmt_from = ls_cl-index
          stmt_to   = lv_stmt_to
          unit_type = ls_cl-eventtype
          unit_name = ls_cl-eventname
          class     = ls_cl-class
        ) INTO TABLE lt_boundaries.

      ENDLOOP.

      LOOP AT is_parse_data-t_events INTO DATA(ls_ev)
        WHERE include    = <prog>-include
          AND stmnt_from > 0
          AND stmnt_to   > 0.

        READ TABLE lt_boundaries WITH KEY stmt_from = ls_ev-stmnt_from TRANSPORTING NO FIELDS.
        CHECK sy-subrc <> 0.

        INSERT VALUE ts_boundary(
          stmt_from = ls_ev-stmnt_from
          stmt_to   = ls_ev-stmnt_to
          unit_type = 'EVENT'
          unit_name = ls_ev-name
          class     = ''
        ) INTO TABLE lt_boundaries.

      ENDLOOP.

      CHECK lt_boundaries IS NOT INITIAL.

      " Build operand set ONCE per include, not per unit
      DATA(lt_operands) = build_operand_set(
        is_parse_data = is_parse_data
        i_include     = <prog>-include
        i_unit_type   = ''
        i_unit_name   = ''
        i_class       = '' ).

      DATA lv_first_row TYPE i.
      DATA lv_last_row  TYPE i.
      DATA lt_dist_ops  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA lt_dist_opd  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      " Include-level accumulated unique dictionaries (never cleared between units)
      DATA lt_incl_ops  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA lt_incl_opd  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      " Class-level unique dictionaries: key = class~token
      TYPES: BEGIN OF ts_cls_tok,
               cls_token TYPE string,   " |CLASSNAME~TOKEN|
             END OF ts_cls_tok.
      DATA lt_cls_ops TYPE HASHED TABLE OF ts_cls_tok WITH UNIQUE KEY cls_token.
      DATA lt_cls_opd TYPE HASHED TABLE OF ts_cls_tok WITH UNIQUE KEY cls_token.
      DATA ls_stmt_f    LIKE LINE OF lo_scan->statements.
      DATA ls_stmt_t    LIKE LINE OF lo_scan->statements.
      DATA ls_tok_f     LIKE LINE OF lo_scan->tokens.
      DATA ls_tok_t     LIKE LINE OF lo_scan->tokens.
      DATA ls_kw_tok    LIKE LINE OF lo_scan->tokens.

      LOOP AT lt_boundaries INTO DATA(ls_b).

        DATA ls_unit TYPE ts_unit_result.
        CLEAR ls_unit.
        ls_unit-program   = <prog>-program.
        ls_unit-include   = <prog>-include.
        ls_unit-unit_type = ls_b-unit_type.
        ls_unit-unit_name = COND #(
          WHEN ls_b-unit_type = 'METHOD' AND ls_b-class IS NOT INITIAL
          THEN |{ ls_b-class }=>{ ls_b-unit_name }|
          ELSE ls_b-unit_name ).
        ls_unit-cyclomatic = 1.

        CLEAR: lt_dist_ops, lt_dist_opd.
        lv_first_row = 0.
        lv_last_row  = 0.

        CLEAR: ls_stmt_f, ls_stmt_t, ls_tok_f, ls_tok_t.
        READ TABLE lo_scan->statements INDEX ls_b-stmt_from INTO ls_stmt_f.
        IF sy-subrc = 0.
          READ TABLE lo_scan->tokens INDEX ls_stmt_f-from INTO ls_tok_f.
          IF sy-subrc = 0. lv_first_row = ls_tok_f-row. ENDIF.
        ENDIF.
        READ TABLE lo_scan->statements INDEX ls_b-stmt_to INTO ls_stmt_t.
        IF sy-subrc = 0.
          READ TABLE lo_scan->tokens INDEX ls_stmt_t-to INTO ls_tok_t.
          IF sy-subrc = 0. lv_last_row = ls_tok_t-row. ENDIF.
        ENDIF.
        IF lv_last_row >= lv_first_row AND lv_first_row > 0.
          ls_unit-loc = lv_last_row - lv_first_row + 1.
        ENDIF.

        LOOP AT lo_scan->statements ASSIGNING FIELD-SYMBOL(<stmt>)
          FROM ls_b-stmt_from TO ls_b-stmt_to.

          IF <stmt>-type = 'P'.
            ADD 1 TO ls_unit-cloc.
          ELSE.
            ADD 1 TO ls_unit-lloc.
            CLEAR ls_kw_tok.
            READ TABLE lo_scan->tokens INDEX <stmt>-from INTO ls_kw_tok.
            IF sy-subrc = 0 AND is_branch_keyword( ls_kw_tok-str ) = abap_true.
              ADD 1 TO ls_unit-cyclomatic.
            ENDIF.

            IF <stmt>-type = 'C'.
              ADD 1 TO ls_unit-n1.
              INSERT CONV string( 'COMPUTE' ) INTO TABLE lt_dist_ops.
              INSERT CONV string( 'COMPUTE' ) INTO TABLE lt_incl_ops.
              IF ls_b-class IS NOT INITIAL.
                INSERT VALUE ts_cls_tok( cls_token = |{ ls_b-class }~COMPUTE| ) INTO TABLE lt_cls_ops.
              ENDIF.
              APPEND VALUE ts_token_detail(
                token    = 'COMPUTE'
                kind     = 'OPERATOR'
                stmt_idx = sy-tabix
                tok_idx  = 0
                row      = ls_kw_tok-row
              ) TO ls_unit-token_detail.
            ENDIF.

            DATA(lv_stmt_tabix) = sy-tabix.
            LOOP AT lo_scan->tokens ASSIGNING FIELD-SYMBOL(<tok>)
              FROM <stmt>-from TO <stmt>-to.

              IF <tok>-str IS NOT INITIAL.
                DATA lv_is_first TYPE boolean.
                IF sy-tabix = <stmt>-from AND <stmt>-type <> 'C'.
                  lv_is_first = abap_true.
                ELSE.
                  CLEAR lv_is_first.
                ENDIF.

                DATA(lv_tok_up)      = to_upper( <tok>-str ).
                DATA(lv_inline_name) = ``.

                IF lv_tok_up CP 'DATA(*)' AND strlen( lv_tok_up ) > 5.
                  DATA(lv_tmp) = <tok>-str+5.
                  DATA(lv_tmp_len) = strlen( lv_tmp ) - 1.
                  IF lv_tmp_len > 0.
                    lv_inline_name = lv_tmp(lv_tmp_len).
                  ENDIF.
                ELSEIF lv_tok_up CP 'FIELD-SYMBOL(<*)' AND strlen( lv_tok_up ) > 14.
                  DATA(lv_fs_tmp) = <tok>-str+14.
                  DATA(lv_fs_len) = strlen( lv_fs_tmp ) - 2.
                  IF lv_fs_len > 0.
                    lv_inline_name = lv_fs_tmp(lv_fs_len).
                  ENDIF.
                ENDIF.

                IF lv_inline_name IS NOT INITIAL.
                  DATA(lv_kw_part) = COND string(
                    WHEN lv_tok_up CP 'DATA(*)'          THEN 'DATA'
                    WHEN lv_tok_up CP 'FIELD-SYMBOL(<*)' THEN 'FIELD-SYMBOL'
                    ELSE 'DATA' ).
                  ADD 1 TO ls_unit-n1.
                  INSERT lv_kw_part INTO TABLE lt_dist_ops.
                  INSERT lv_kw_part INTO TABLE lt_incl_ops.
                  IF ls_b-class IS NOT INITIAL.
                    INSERT VALUE ts_cls_tok( cls_token = |{ ls_b-class }~{ lv_kw_part }| ) INTO TABLE lt_cls_ops.
                  ENDIF.
                  APPEND VALUE ts_token_detail(
                    token    = lv_kw_part
                    kind     = 'OPERATOR'
                    stmt_idx = lv_stmt_tabix
                    tok_idx  = sy-tabix
                    row      = <tok>-row
                  ) TO ls_unit-token_detail.
                  ADD 1 TO ls_unit-n2.
                  INSERT to_upper( lv_inline_name ) INTO TABLE lt_dist_opd.
                  INSERT to_upper( lv_inline_name ) INTO TABLE lt_incl_opd.
                  IF ls_b-class IS NOT INITIAL.
                    INSERT VALUE ts_cls_tok( cls_token = |{ ls_b-class }~{ to_upper( lv_inline_name ) }| ) INTO TABLE lt_cls_opd.
                  ENDIF.
                  APPEND VALUE ts_token_detail(
                    token    = lv_inline_name
                    kind     = 'OPERAND'
                    stmt_idx = lv_stmt_tabix
                    tok_idx  = sy-tabix
                    row      = <tok>-row
                  ) TO ls_unit-token_detail.
                ELSE.
                  DATA(lv_kind) = classify_token(
                    i_token     = <tok>-str
                    i_is_first  = lv_is_first
                    it_operands = lt_operands ).

                  IF lv_kind = 'OPERATOR'.
                    ADD 1 TO ls_unit-n1.
                    INSERT <tok>-str INTO TABLE lt_dist_ops.
                    INSERT <tok>-str INTO TABLE lt_incl_ops.
                    IF ls_b-class IS NOT INITIAL.
                      INSERT VALUE ts_cls_tok( cls_token = |{ ls_b-class }~{ <tok>-str }| ) INTO TABLE lt_cls_ops.
                    ENDIF.
                  ELSE.
                    ADD 1 TO ls_unit-n2.
                    INSERT <tok>-str INTO TABLE lt_dist_opd.
                    INSERT <tok>-str INTO TABLE lt_incl_opd.
                    IF ls_b-class IS NOT INITIAL.
                      INSERT VALUE ts_cls_tok( cls_token = |{ ls_b-class }~{ <tok>-str }| ) INTO TABLE lt_cls_opd.
                    ENDIF.
                  ENDIF.

                  APPEND VALUE ts_token_detail(
                    token    = <tok>-str
                    kind     = lv_kind
                    stmt_idx = lv_stmt_tabix
                    tok_idx  = sy-tabix
                    row      = <tok>-row
                  ) TO ls_unit-token_detail.
                ENDIF.

              ENDIF.
            ENDLOOP. " tokens
          ENDIF.
        ENDLOOP. " statements

        ls_unit-big_n1      = lines( lt_dist_ops ).
        ls_unit-big_n2      = lines( lt_dist_opd ).
        ls_unit-vocabulary  = ls_unit-big_n1 + ls_unit-big_n2.
        ls_unit-prog_length = ls_unit-n1 + ls_unit-n2.

        IF ls_unit-vocabulary > 0 AND ls_unit-prog_length > 0.
          DATA(lv_voc_f) = CONV f( ls_unit-vocabulary ).
          DATA(lv_len_f) = CONV f( ls_unit-prog_length ).
          ls_unit-volume = lv_len_f * log2( lv_voc_f ).
          IF ls_unit-big_n2 > 0.
            ls_unit-difficulty =
              ( CONV f( ls_unit-big_n1 ) / 2 )
              * ( CONV f( ls_unit-n2 ) / CONV f( ls_unit-big_n2 ) ).
          ENDIF.
          ls_unit-effort = ls_unit-difficulty * ls_unit-volume.
          ls_unit-time_t = ls_unit-effort / 18.
          ls_unit-bugs   = ls_unit-volume  / 3000.
        ENDIF.

        " Maintainability Index
        " MI = 171 - 5.2 * ln(V) - 0.23 * G - 16.2 * ln(LOC)
        IF ls_unit-volume > 0 AND ls_unit-loc > 0.
          DATA(lv_ln_vol) = log( ls_unit-volume ).
          DATA(lv_ln_loc) = log( CONV f( ls_unit-loc ) ).
          DATA(lv_cc_f)   = CONV f( ls_unit-cyclomatic ).
          ls_unit-mi = 171
            - ( CONV f( '5.2'  ) * lv_ln_vol )
            - ( CONV f( '0.23' ) * lv_cc_f   )
            - ( CONV f( '16.2' ) * lv_ln_loc ).
        ENDIF.

        APPEND ls_unit TO rs_result-units.

      ENDLOOP. " lt_boundaries

      " Fill include-level Halstead aggregates
      rs_result-incl_big_n1     = lines( lt_incl_ops ).
      rs_result-incl_big_n2     = lines( lt_incl_opd ).
      rs_result-incl_vocabulary  = rs_result-incl_big_n1 + rs_result-incl_big_n2.

      " ---------------------------------------------------------------
      " Build per-class Halstead dictionaries while lt_cls_ops/opd are
      " still in scope (inside LOOP AT tt_progs).
      " ---------------------------------------------------------------
      TYPES: BEGIN OF ts_cls_count,
               class_name TYPE string,
               big_n1     TYPE i,
               big_n2     TYPE i,
             END OF ts_cls_count.
      DATA lt_cls_counts TYPE HASHED TABLE OF ts_cls_count
        WITH UNIQUE KEY class_name.
      CLEAR lt_cls_counts.

      " Count unique operators per class
      LOOP AT lt_cls_ops INTO DATA(ls_co).
        DATA(lv_tilde_pos) = find( val = ls_co-cls_token sub = '~' ).
        CHECK lv_tilde_pos > 0.
        DATA(lv_cname) = ls_co-cls_token(lv_tilde_pos).
        READ TABLE lt_cls_counts WITH KEY class_name = lv_cname
          ASSIGNING FIELD-SYMBOL(<lcc>).
        IF sy-subrc <> 0.
          INSERT VALUE ts_cls_count( class_name = lv_cname ) INTO TABLE lt_cls_counts.
          READ TABLE lt_cls_counts WITH KEY class_name = lv_cname
            ASSIGNING <lcc>.
        ENDIF.
        ADD 1 TO <lcc>-big_n1.
      ENDLOOP.

      " Count unique operands per class
      LOOP AT lt_cls_opd INTO DATA(ls_cd).
        DATA(lv_tilde_pos2) = find( val = ls_cd-cls_token sub = '~' ).
        CHECK lv_tilde_pos2 > 0.
        DATA(lv_cname2) = ls_cd-cls_token(lv_tilde_pos2).
        READ TABLE lt_cls_counts WITH KEY class_name = lv_cname2
          ASSIGNING FIELD-SYMBOL(<lcc2>).
        IF sy-subrc <> 0.
          INSERT VALUE ts_cls_count( class_name = lv_cname2 ) INTO TABLE lt_cls_counts.
          READ TABLE lt_cls_counts WITH KEY class_name = lv_cname2
            ASSIGNING <lcc2>.
        ENDIF.
        ADD 1 TO <lcc2>-big_n2.
      ENDLOOP.

      " Transfer counts into rs_result-class_totals entries
      LOOP AT lt_cls_counts INTO DATA(ls_cc).
        READ TABLE rs_result-class_totals
          WITH KEY class_name = ls_cc-class_name
          ASSIGNING FIELD-SYMBOL(<lct_hal>).
        IF sy-subrc <> 0.
          APPEND VALUE ts_class_result( class_name = ls_cc-class_name )
            TO rs_result-class_totals.
          READ TABLE rs_result-class_totals
            WITH KEY class_name = ls_cc-class_name
            ASSIGNING <lct_hal>.
        ENDIF.
        " Accumulate across includes (in case same class spans multiple includes)
        ADD ls_cc-big_n1 TO <lct_hal>-cls_big_n1.
        ADD ls_cc-big_n2 TO <lct_hal>-cls_big_n2.
      ENDLOOP.

    ENDLOOP. " tt_progs

    DATA lv_cnt TYPE i.
    LOOP AT rs_result-units INTO DATA(ls_u).
      ADD ls_u-cyclomatic TO rs_result-total_cyclomatic.
      ADD ls_u-n1 TO rs_result-total_n1.
      ADD ls_u-n2 TO rs_result-total_n2.
      rs_result-total_volume = rs_result-total_volume + ls_u-volume.
      rs_result-total_effort = rs_result-total_effort + ls_u-effort.
      rs_result-total_time_t = rs_result-total_time_t + ls_u-time_t.
      rs_result-total_bugs   = rs_result-total_bugs   + ls_u-bugs.
      ADD ls_u-loc  TO rs_result-total_loc.
      ADD ls_u-lloc TO rs_result-total_lloc.
      ADD ls_u-cloc TO rs_result-total_cloc.
      ADD 1 TO lv_cnt.
    ENDLOOP.
    IF lv_cnt > 0.
      rs_result-avg_cyclomatic =
        CONV f( rs_result-total_cyclomatic ) / CONV f( lv_cnt ).
    ENDIF.

    " Derive incl_prog_length, volume, difficulty, effort, time_t, bugs
    rs_result-incl_prog_length = rs_result-total_n1 + rs_result-total_n2.
    IF rs_result-incl_vocabulary > 0 AND rs_result-incl_prog_length > 0.
      DATA(lv_ivoc) = CONV f( rs_result-incl_vocabulary ).
      DATA(lv_ilen) = CONV f( rs_result-incl_prog_length ).
      rs_result-incl_volume = lv_ilen * log2( lv_ivoc ).
      IF rs_result-incl_big_n2 > 0.
        rs_result-incl_difficulty =
          ( CONV f( rs_result-incl_big_n1 ) / 2 )
          * ( CONV f( rs_result-total_n2 ) / CONV f( rs_result-incl_big_n2 ) ).
      ENDIF.
      rs_result-incl_effort = rs_result-incl_difficulty * rs_result-incl_volume.
      rs_result-incl_time_t = rs_result-incl_effort / 18.
      rs_result-incl_bugs   = rs_result-incl_volume  / 3000.
    ENDIF.

    " ---------------------------------------------------------------
    " Build per-class totals from rs_result-units + finalize Halstead
    " ---------------------------------------------------------------
    LOOP AT rs_result-units INTO DATA(ls_cu)
      WHERE unit_type = 'METHOD'.

      " Extract class name: unit_name has format "CLASSNAME=>METHODNAME"
      DATA(lv_cls_sep) = find( val = ls_cu-unit_name sub = '=>' ).
      CHECK lv_cls_sep > 0.
      DATA(lv_cls_name) = to_upper( ls_cu-unit_name(lv_cls_sep) ).

      READ TABLE rs_result-class_totals
        WITH KEY class_name = lv_cls_name
        ASSIGNING FIELD-SYMBOL(<lct>).
      IF sy-subrc <> 0.
        APPEND VALUE ts_class_result( class_name = lv_cls_name )
          TO rs_result-class_totals.
        READ TABLE rs_result-class_totals
          WITH KEY class_name = lv_cls_name
          ASSIGNING <lct>.
      ENDIF.

      ADD ls_cu-cyclomatic TO <lct>-total_cyclomatic.
      ADD ls_cu-n1         TO <lct>-total_n1.
      ADD ls_cu-n2         TO <lct>-total_n2.
      <lct>-total_volume = <lct>-total_volume + ls_cu-volume.
      " effort / time_t / bugs — sum across methods (correct semantics)
      <lct>-total_effort = <lct>-total_effort + ls_cu-effort.
      <lct>-total_time_t = <lct>-total_time_t + ls_cu-time_t.
      <lct>-total_bugs   = <lct>-total_bugs   + ls_cu-bugs.
      ADD ls_cu-loc  TO <lct>-total_loc.
      ADD ls_cu-lloc TO <lct>-total_lloc.
      ADD ls_cu-cloc TO <lct>-total_cloc.

    ENDLOOP.

    " Derive averages and class-scope Halstead for each class
    LOOP AT rs_result-class_totals ASSIGNING FIELD-SYMBOL(<lct2>).

      " avg_cyclomatic
      DATA(lv_cls_cnt) = 0.
      LOOP AT rs_result-units INTO DATA(ls_cu2)
        WHERE unit_type = 'METHOD'.
        DATA(lv_sep2) = find( val = ls_cu2-unit_name sub = '=>' ).
        IF lv_sep2 > 0 AND to_upper( ls_cu2-unit_name(lv_sep2) ) = <lct2>-class_name.
          ADD 1 TO lv_cls_cnt.
        ENDIF.
      ENDLOOP.
      IF lv_cls_cnt > 0.
        <lct2>-avg_cyclomatic =
          CONV f( <lct2>-total_cyclomatic ) / CONV f( lv_cls_cnt ).
      ENDIF.

      " cls_big_n1 / cls_big_n2 filled above — unique operator/operand dictionaries
      " across all methods of the class (used for structural complexity analysis)
      <lct2>-cls_vocabulary  = <lct2>-cls_big_n1 + <lct2>-cls_big_n2.
      <lct2>-cls_prog_length = <lct2>-total_n1   + <lct2>-total_n2.

      IF <lct2>-cls_vocabulary > 0 AND <lct2>-cls_prog_length > 0.
        DATA(lv_cvoc) = CONV f( <lct2>-cls_vocabulary ).
        DATA(lv_clen) = CONV f( <lct2>-cls_prog_length ).
        <lct2>-cls_volume = lv_clen * log2( lv_cvoc ).
        IF <lct2>-cls_big_n2 > 0.
          <lct2>-cls_difficulty =
            ( CONV f( <lct2>-cls_big_n1 ) / 2 )
            * ( CONV f( <lct2>-total_n2 ) / CONV f( <lct2>-cls_big_n2 ) ).
        ENDIF.
        " cls_effort / cls_time_t / cls_bugs — derived from class-scope volume/difficulty.
        " These are ANALYTICAL metrics showing vocabulary-based complexity.
        " For effort planning use total_effort / total_time_t / total_bugs (sum of methods).
        <lct2>-cls_effort = <lct2>-cls_difficulty * <lct2>-cls_volume.
        <lct2>-cls_time_t = <lct2>-cls_effort / 18.
        <lct2>-cls_bugs   = <lct2>-cls_volume  / 3000.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
  METHOD is_branch_keyword.
    CASE i_kw.
      WHEN 'IF' OR 'ELSEIF' OR 'WHEN' OR 'CATCH'
        OR 'LOOP' OR 'WHILE' OR 'DO'
        OR 'CHECK' OR 'AT' OR 'ON'.
        rv = abap_true.
      WHEN OTHERS.
        rv = abap_false.
    ENDCASE.
  ENDMETHOD.
  METHOD log2.
    IF i_val <= 0. RETURN. ENDIF.
    rv = log( i_val ) / log( CONV f( 2 ) ).
  ENDMETHOD.
  METHOD build_operand_set.
    " Operands = all known named identifiers:
    "   1. Variable/field-symbol names from t_vars (this include)
    "   2. Parameter names from t_params (this include)
    "   3. Unit names (eventname) from tt_calls_line (this include)
    "   4. Class/interface names from tt_class_defs
    " No scope filtering by class/method — a token found in code is checked
    " against these sets by name only.

    " Operands = all known named identifiers:
    "   1. Variable/field-symbol names from t_vars (this include)
    "   2. Parameter names from t_params (this include)
    "   3. Unit names (eventname) from tt_calls_line (this include)
    "   4. Class/interface names from tt_class_defs
    " No scope filtering by class/method — a token found in code is checked
    " against these sets by name only.

    FIELD-SYMBOLS: <ls_v>  LIKE LINE OF is_parse_data-t_vars,
                  <ls_p>  LIKE LINE OF is_parse_data-t_params,
                  <ls_cl> LIKE LINE OF is_parse_data-tt_calls_line,
                  <ls_cd> LIKE LINE OF is_parse_data-tt_class_defs.

    LOOP AT is_parse_data-t_vars ASSIGNING <ls_v>
      WHERE include = i_include.
      IF <ls_v>-name IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_v>-name ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

    LOOP AT is_parse_data-t_params ASSIGNING <ls_p>
      WHERE include = i_include.
      IF <ls_p>-param IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_p>-param ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

    LOOP AT is_parse_data-tt_calls_line ASSIGNING <ls_cl>
      WHERE include = i_include.
      IF <ls_cl>-eventname IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_cl>-eventname ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

    LOOP AT is_parse_data-tt_class_defs ASSIGNING <ls_cd>.
      IF <ls_cd>-class IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_cd>-class ) ) INTO TABLE rt_ops.
      ENDIF.
      IF <ls_cd>-super IS NOT INITIAL.
        INSERT VALUE ts_known_operand( name = to_upper( <ls_cd>-super ) ) INTO TABLE rt_ops.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD classify_token.
    " Keyword classification now sourced from ZCL_ACE_KEYWORDS — derived 1:1
    " from the abaplint grammar (Combi.listKeywords over ZCL_ACE_STMTS/EXPRS).
    " The static fill_statements list has been removed.
    IF zcl_ace_keywords=>is_keyword( i_token ) = abap_true.
      rv_kind = 'OPERATOR'.
    ELSE.
      rv_kind = 'OPERAND'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS ZCL_ACE_MERMAID IMPLEMENTATION.
  METHOD check_mermaid.
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING clskey = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
      EXCEPTIONS not_specified = 1 not_existing = 2 i_interface = 3
                 no_text = 4 inconsistent = 5 OTHERS = 6.
    IF sy-subrc = 0.
      zcl_ace=>i_mermaid_active = abap_true.
    ELSE.
      CLEAR zcl_ace=>i_mermaid_active.
    ENDIF.
  ENDMETHOD.
  method FORMAT_NODE_LABEL.

    CONSTANTS lc_br    TYPE string VALUE `<br/>`.
    CONSTANTS lc_br_ph TYPE string VALUE `##BR##`.

    " Effective max length: use parameter value, but treat 50 (old default) as 100
    "DATA(lv_maxlen) = COND i( WHEN I_MAXLEN = 50 OR I_MAXLEN = 0 THEN 100 ELSE I_MAXLEN ).
DATA(lv_maxlen) = 200.
    RV_LABEL = I_CODE.

    " Protect existing <br/> tags before any text transformations
    REPLACE ALL OCCURRENCES OF lc_br IN RV_LABEL WITH lc_br_ph IN CHARACTER MODE.

    " Truncate only if lv_maxlen > 0
    IF lv_maxlen > 0 AND strlen( RV_LABEL ) > lv_maxlen.
      RV_LABEL = RV_LABEL+0(lv_maxlen).
    ENDIF.

    REPLACE ALL OCCURRENCES OF `PERFORM`       IN RV_LABEL WITH `FORM`     IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN RV_LABEL WITH `FUNCTION` IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `CALL METHOD`   IN RV_LABEL WITH `METHOD`   IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `-`             IN RV_LABEL WITH ` `        IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF ` `             IN RV_LABEL WITH `&nbsp;`   IN CHARACTER MODE.

    " Restore <br/> tags
    REPLACE ALL OCCURRENCES OF lc_br_ph IN RV_LABEL WITH lc_br IN CHARACTER MODE.

  endmethod.
  method BUILD_NODES.

    DATA: box_s        TYPE string,
          box_e        TYPE string,
          opened       TYPE i,
          pre_stack    TYPE mo_viewer->ts_line,
          times        TYPE i,
          lt_sg_opened TYPE TABLE OF flag WITH EMPTY KEY.

    LOOP AT CT_LINES ASSIGNING FIELD-SYMBOL(<line>) WHERE cond <> 'ELSE' AND cond <> 'ELSEIF' AND cond <> 'WHEN'.
      DATA(ind) = <line>-ind.
      DATA(lv_tabix) = sy-tabix.

      IF <line>-cond IS INITIAL.
        box_s = '('. box_e = ')'.
      ELSE.
        box_s = '{'. box_e = '}'.
      ENDIF.

      IF pre_stack IS INITIAL.
        pre_stack = <line>.
      ENDIF.

      " Close subgraphs when stack level decreases or event changes
      IF ( pre_stack-stack > <line>-stack OR pre_stack-ev_name <> <line>-ev_name )
         AND opened > 0.
        IF pre_stack-stack = <line>-stack AND pre_stack-ev_name <> <line>-ev_name.
          times = 1.
        ELSE.
          times = pre_stack-stack - <line>-stack.
        ENDIF.
        DO times TIMES.
          CV_MM_STRING = |{ CV_MM_STRING } end\n|.
          opened = opened - 1.
          IF opened = 0. EXIT. ENDIF.
        ENDDO.
      ENDIF.

      " LOOP/DO/WHILE — only subgraph, no node
      IF <line>-cond = 'LOOP' OR <line>-cond = 'DO' OR <line>-cond = 'WHILE'.

        REPLACE ALL OCCURRENCES OF `-` IN <line>-code WITH ` ` IN CHARACTER MODE.
        pre_stack = <line>.

        DATA(name) = format_node_label( i_code = <line>-code ).

        " Only open subgraph if next line is not immediately END*
        READ TABLE CT_LINES INDEX lv_tabix + 1 INTO DATA(line2).
        IF sy-subrc = 0
           AND line2-cond <> 'ENDLOOP' AND line2-cond <> 'ENDDO' AND line2-cond <> 'ENDWHILE'.
          CV_MM_STRING = |{ CV_MM_STRING } subgraph S{ ind }["{ name }"]\n  direction { I_DIRECTION }\n|.
          opened = opened + 1.
          APPEND abap_true TO lt_sg_opened.
        ELSE.
          APPEND abap_false TO lt_sg_opened.
        ENDIF.
        CONTINUE.

      ENDIF.

      " PERFORM/CALL FUNCTION/CALL METHOD etc.
      IF <line>-subname IS NOT INITIAL.

        READ TABLE CT_LINES INDEX lv_tabix + 1 INTO line2.
        DATA(lv_has_children) = xsdbool( sy-subrc = 0 AND line2-stack > <line>-stack ).

        IF lv_has_children = abap_true.
          " Call goes deeper (stack+1): show only the call signature without parameters
          " (the parameters are visible in the child nodes / subgraph below).
          " Strip everything from the first opening parenthesis onward.
          DATA(lv_call_label) = <line>-code.

          FIND FIRST OCCURRENCE OF ` = ` IN lv_call_label MATCH OFFSET DATA(lv_eq_off).
          IF sy-subrc = 0.
            DATA(lv_rhs) = lv_call_label+lv_eq_off.
            FIND FIRST OCCURRENCE OF '(' IN lv_rhs MATCH OFFSET DATA(lv_rhs_off).
            IF sy-subrc = 0.
              DATA(lv_abs_paren) = lv_eq_off + lv_rhs_off + 1.
              lv_call_label = |{ lv_call_label(lv_abs_paren) } )|.
            ELSE.
              lv_call_label = |{ lv_call_label }( )|.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF '(' IN lv_call_label MATCH OFFSET DATA(lv_off).
            IF sy-subrc = 0.
              lv_call_label = |{ lv_call_label(lv_off) }( )|.
            ELSE.
              lv_call_label = |{ lv_call_label }( )|.
            ENDIF.
          ENDIF.

          DATA(name2) = format_node_label( i_code = lv_call_label i_maxlen = 0 ).
          CV_MM_STRING = |{ CV_MM_STRING }{ ind }{ box_s }"{ name2 }"{ box_e }\n|.

          DATA(lv_sg_title) = format_node_label( i_code = <line>-subname i_maxlen = 0 ).
          CV_MM_STRING = |{ CV_MM_STRING } subgraph S{ ind }["{ lv_sg_title }"]\n  direction { I_DIRECTION }\n|.
          opened = opened + 1.
        ELSE.
          " Same-level call (no children): show the full source line including all parameters.
          " <line>-code was built with an early EXIT at USING/EXPORTING/IMPORTING/CHANGING,
          " so we re-read all tokens directly from the scan to get the complete text.
          DATA(lv_label_code) = ``.

          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
            WITH KEY include = <line>-include INTO DATA(ls_prog_full).
          IF sy-subrc = 0.
            READ TABLE ls_prog_full-t_keywords
              WITH KEY line = <line>-line INTO DATA(ls_kw_full).
            IF sy-subrc = 0.
              " Read all tokens for this keyword span (no early exit on USING/EXPORTING/…)
              LOOP AT ls_prog_full-scan->tokens
                FROM ls_kw_full-from TO ls_kw_full-to
                INTO DATA(ls_tok_full).
                IF lv_label_code IS INITIAL.
                  lv_label_code = ls_tok_full-str.
                ELSE.
                  lv_label_code = |{ lv_label_code } { ls_tok_full-str }|.
                ENDIF.
              ENDLOOP.
              REPLACE ALL OCCURRENCES OF '"' IN lv_label_code WITH ``.
            ENDIF.
          ENDIF.

          " Fall back to the pre-built code if the re-read yielded nothing.
          IF lv_label_code IS INITIAL.
            lv_label_code = <line>-code.
          ENDIF.

          " If the call has bindings (named parameters), insert <br/> before each
          " parameter so every parameter starts on a new line in the Mermaid node label.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
            WITH KEY include = <line>-include INTO DATA(ls_prog_bn).
          IF sy-subrc = 0.
            READ TABLE ls_prog_bn-t_keywords
              WITH KEY line = <line>-line INTO DATA(ls_kw_bn).
            IF sy-subrc = 0 AND ls_kw_bn-tt_calls IS NOT INITIAL.
              " Use bindings from the first call entry that has named parameters.
              LOOP AT ls_kw_bn-tt_calls INTO DATA(ls_call_bn).
                IF ls_call_bn-bindings IS NOT INITIAL.
                  LOOP AT ls_call_bn-bindings INTO DATA(ls_bind_bn).
                    IF ls_bind_bn-inner IS INITIAL. CONTINUE. ENDIF.
                    " Insert <br/> before " INNER =" pattern in the code string.
                    DATA(lv_pattern_bn) = | { ls_bind_bn-inner } =|.
                    REPLACE ALL OCCURRENCES OF lv_pattern_bn
                      IN lv_label_code
                      WITH |<br/>{ lv_pattern_bn }|
                      IN CHARACTER MODE.
                  ENDLOOP.
                  EXIT. " only process bindings of the first matching call
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.

          DATA(lv_label) = format_node_label( i_code = lv_label_code i_maxlen = 0 ).
          CV_MM_STRING = |{ CV_MM_STRING }{ ind }{ box_s }"{ lv_label }"{ box_e }\n|.
          CLEAR <line>-arrow.
        ENDIF.

        pre_stack = <line>.
        CONTINUE.

      ENDIF.

      " END* — only close if subgraph was actually opened
      IF <line>-cond = 'ENDLOOP' OR <line>-cond = 'ENDDO' OR <line>-cond = 'ENDWHILE'.
        DATA(lv_last) = lines( lt_sg_opened ).
        IF lv_last > 0.
          READ TABLE lt_sg_opened INDEX lv_last INTO DATA(lv_sg_flag).
          DELETE lt_sg_opened INDEX lv_last.
          IF lv_sg_flag = abap_true.
            opened = opened - 1.
            CV_MM_STRING = |{ CV_MM_STRING } end\n|.
          ENDIF.
        ENDIF.
        CONTINUE.
      ENDIF.

      " Regular node
      REPLACE ALL OCCURRENCES OF `-` IN <line>-code WITH ` ` IN CHARACTER MODE.
      DATA(lv_reg_label) = format_node_label( i_code = <line>-code ).
      CV_MM_STRING = |{ CV_MM_STRING }{ ind }{ box_s }"{ lv_reg_label }"{ box_e }\n|.
      pre_stack = <line>.

    ENDLOOP.

    " Close any remaining open subgraphs
    DO opened TIMES.
      CV_MM_STRING = |{ CV_MM_STRING } end\n|.
    ENDDO.

  endmethod.
  method BUILD_EDGES.

    " IT_LINES contains only non-LOOP/DO/WHILE lines (already filtered by caller).
    " Fields used:
    "   ind        - sequential index within results (set in GET_CODE_FLOW)
    "   cond       - IF / ELSE / ELSEIF / ENDIF / CASE / WHEN / ENDCASE / <empty>
    "   ev_name    - event/method name this line belongs to
    "   stack      - call stack level (1 = root event, no incoming arrow from another context)
    "   els_before - ind of node before this ELSE/ELSEIF/WHEN (set in GET_CODE_FLOW)
    "   els_after  - ind of first real node after this branch (set in GET_CODE_FLOW)
    "   arrow      - label for edge (variable assignments)
    " mt_if (from mo_viewer) - stack of IF/CASE structures: if_ind, end_ind

    " Work with a local copy so we don't corrupt shared state
    DATA(lt_if) = mo_viewer->mt_if.

    DATA: if_stack   TYPE TABLE OF i,      " stack of indices into lt_if
          if_ptr     TYPE i,               " current index in lt_if
          pre_ind    TYPE i,               " ind of previous drawable node
          pre_cond   TYPE string,          " cond of previous node
          pre_ev     TYPE string,          " ev_name of previous drawable node
          sub        TYPE string,          " edge label
          last_els   TYPE i.               " last els_after handled (to skip duplicate edges)

    " Track which (stack=1, ev_name) contexts have already had at least one node
    " drawn. Only the very first node of a root context must not get an incoming
    " cross-context arrow; subsequent nodes in the same context may receive arrows
    " from nodes that temporarily "dipped" into a deeper ev_name (subgraph call).
    DATA lt_started_ev TYPE TABLE OF string WITH EMPTY KEY.

    LOOP AT IT_LINES INTO DATA(line).

      " Skip LOOP/DO/WHILE and their END* — they are subgraphs, not nodes
      IF line-cond = 'LOOP' OR line-cond = 'DO'    OR line-cond = 'WHILE' OR
         line-cond = 'ENDLOOP' OR line-cond = 'ENDDO' OR line-cond = 'ENDWHILE'.
        CONTINUE.
      ENDIF.

      " ----- IF / CASE: push onto stack -----
      IF line-cond = 'IF' OR line-cond = 'CASE'.
        if_ptr = if_ptr + 1.
        READ TABLE lt_if INDEX if_ptr INTO DATA(ls_if).
        APPEND if_ptr TO if_stack.
      ENDIF.

      " ----- ENDIF / ENDCASE: pop stack -----
      IF line-cond = 'ENDIF' OR line-cond = 'ENDCASE'.
        DATA(lv_top) = 0.
        READ TABLE if_stack INDEX lines( if_stack ) INTO lv_top.
        IF sy-subrc = 0.
          DELETE if_stack INDEX lines( if_stack ).
          " re-read current top after pop
          READ TABLE if_stack INDEX lines( if_stack ) INTO lv_top.
          IF sy-subrc = 0.
            READ TABLE lt_if INDEX lv_top INTO ls_if.
          ELSE.
            CLEAR ls_if.
          ENDIF.
        ENDIF.
        " draw edge from last node before ENDIF to ENDIF node
        IF pre_ind > 0 AND pre_cond <> 'ELSE' AND pre_cond <> 'ELSEIF' AND pre_cond <> 'WHEN'
           AND NOT ( last_els = line-ind ).
          " Block only the very first appearance of a root-level (stack=1) ev_name context
          DATA(lv_block_endif) = abap_false.
          IF line-stack = 1.
            READ TABLE lt_started_ev WITH KEY table_line = line-ev_name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              lv_block_endif = abap_true.
            ENDIF.
          ENDIF.
          IF lv_block_endif = abap_false.
            CV_MM_STRING = |{ CV_MM_STRING }{ pre_ind }-->{ sub }{ line-ind }\n|.
            CLEAR sub.
          ENDIF.
        ENDIF.
        " Mark this ev_name as started once we output (or skip) its first node
        IF line-stack = 1.
          READ TABLE lt_started_ev WITH KEY table_line = line-ev_name TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0. APPEND line-ev_name TO lt_started_ev. ENDIF.
        ENDIF.
        pre_ind  = line-ind.
        pre_cond = line-cond.
        pre_ev   = line-ev_name.
        CONTINUE.
      ENDIF.

      " ----- ELSE / ELSEIF / WHEN: branch edges -----
      IF line-cond = 'ELSE' OR line-cond = 'ELSEIF' OR line-cond = 'WHEN'.

        " get current IF/CASE node
        READ TABLE if_stack INDEX lines( if_stack ) INTO lv_top.
        IF sy-subrc = 0.
          READ TABLE lt_if INDEX lv_top INTO ls_if.
        ENDIF.

        DATA(bool) = '|' && line-code && '|'.

        " edge from IF/CASE head to branch target
        IF line-els_after IS NOT INITIAL AND line-els_after > 0.
          CV_MM_STRING = |{ CV_MM_STRING }{ ls_if-if_ind }-->{ bool }{ line-els_after }\n|.
          last_els = line-els_after.
        ELSE.
          " no nodes in this branch — edge to ENDIF/ENDCASE
          IF ls_if-end_ind > 0.
            CV_MM_STRING = |{ CV_MM_STRING }{ ls_if-if_ind }-->{ bool }{ ls_if-end_ind }\n|.
          ENDIF.
        ENDIF.

        " edge from previous branch's last node to ENDIF/ENDCASE (fall-through)
        IF line-els_before IS NOT INITIAL AND line-els_before <> ls_if-if_ind AND ls_if-end_ind > 0.
          CV_MM_STRING = |{ CV_MM_STRING }{ line-els_before }-->{ ls_if-end_ind }\n|.
        ENDIF.

        " if next node is not ENDIF/ENDCASE, reset pre so next regular node
        " doesn't get a spurious edge from IF head
        DATA(lv_next_ind) = line-ind + 1.
        READ TABLE IT_LINES WITH KEY ind = lv_next_ind INTO DATA(next_line).
        IF sy-subrc = 0
           AND next_line-cond <> 'ENDIF'
           AND next_line-cond <> 'ENDCASE'.
          CLEAR pre_ind.
        ELSE.
          pre_ind  = line-ind.
        ENDIF.
        pre_cond = line-cond.
        pre_ev   = line-ev_name.
        CLEAR sub.
        CONTINUE.
      ENDIF.

      " ----- Regular node -----
      IF pre_ind > 0
         AND pre_cond <> 'ELSE' AND pre_cond <> 'ELSEIF' AND pre_cond <> 'WHEN'
         AND NOT ( last_els = line-ind ).
        " Block only the very first node of a root-level (stack=1) context when
        " it is being encountered for the first time — i.e. it has no predecessor
        " within its own ev_name yet. Subsequent appearances of the same ev_name
        " (e.g. after returning from a nested subgraph call) are allowed arrows.
        DATA(lv_block) = abap_false.
        IF line-stack = 1.
          READ TABLE lt_started_ev WITH KEY table_line = line-ev_name TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            " First node of this root context — no incoming cross-context arrow
            lv_block = abap_true.
          ENDIF.
        ENDIF.
        IF lv_block = abap_false.
          CV_MM_STRING = |{ CV_MM_STRING }{ pre_ind }-->{ sub }{ line-ind }\n|.
        ENDIF.
      ENDIF.

      " Mark this ev_name as started once we process its first node
      IF line-stack = 1.
        READ TABLE lt_started_ev WITH KEY table_line = line-ev_name TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0. APPEND line-ev_name TO lt_started_ev. ENDIF.
      ENDIF.

      sub = COND string( WHEN line-arrow IS NOT INITIAL THEN '|"' && line-arrow && '"|' ).
      pre_ind  = line-ind.
      pre_cond = line-cond.
      pre_ev   = line-ev_name.

    ENDLOOP.

  endmethod.
  METHOD magic_search.

    DATA: mm_string TYPE string,
          direction TYPE string.

    CLEAR mo_viewer->mt_if.
    DATA(lines) = mo_viewer->get_code_flow( i_calc_path = i_calc_path ).
    "CHECK lines IS NOT INITIAL.

    direction = COND string(
      WHEN i_direction IS NOT INITIAL THEN i_direction
      WHEN lines( lines ) < 100       THEN 'LR'
      ELSE                                 'TB' ).

    mm_string = |graph { direction }\n |.

    build_nodes( EXPORTING i_direction = direction
                 CHANGING  ct_lines = lines cv_mm_string = mm_string ).

    build_edges( EXPORTING it_lines = lines
                 CHANGING  cv_mm_string = mm_string ).

    " --- Highlight active blocks (active_root = X) with a light-blue fill ---
    DATA: lv_active_ids TYPE string.
    LOOP AT lines INTO DATA(line) WHERE active_root = abap_true AND cond <> 'ELSE'
                                                                AND cond <> 'ELSEIF'
                                                                AND cond <> 'WHEN'.
      IF lv_active_ids IS INITIAL.
        lv_active_ids = |{ line-ind }|.
      ELSE.
        lv_active_ids = |{ lv_active_ids },{ line-ind }|.
      ENDIF.
    ENDLOOP.

    IF lv_active_ids IS NOT INITIAL.
      mm_string = |{ mm_string }\nclassDef activeNode fill:#AEE6FF,stroke:#3399CC,color:#000\n|.
      mm_string = |{ mm_string }class { lv_active_ids } activeNode\n|.
    ENDIF.

    mm_string = |{ mm_string }\n|.
    open_mermaid( mm_string ).

  ENDMETHOD.
  METHOD class_map.

    TYPES: BEGIN OF lty_meth,
             class      TYPE string,
             real_class TYPE string,
             disp_class TYPE string,
             name      TYPE string,
             disp_name TYPE string,
             program   TYPE program,
             include   TYPE program,
             meth_type TYPE i,
             stmt_from TYPE i,
             stmt_to   TYPE i,
             row_from  TYPE i,
             row_to    TYPE i,
             node_id   TYPE string,
             agg_id    TYPE string,
           END OF lty_meth,
           BEGIN OF lty_edge,
             from_id  TYPE string,
             to_id    TYPE string,
             external TYPE abap_bool,
           END OF lty_edge,
           BEGIN OF lty_ext,
             node_id TYPE string,
             label   TYPE string,
             key     TYPE string,
           END OF lty_ext,
           BEGIN OF lty_nmap,
             kind    TYPE string,
             name    TYPE string,
             node_id TYPE string,
           END OF lty_nmap,
           BEGIN OF lty_rank,
             node_id TYPE string,
             rank    TYPE i,
           END OF lty_rank.

    DATA: mm_string  TYPE string,
          direction  TYPE string,
          lt_meth    TYPE STANDARD TABLE OF lty_meth WITH DEFAULT KEY,
          lt_edge    TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY,
          lt_ext     TYPE STANDARD TABLE OF lty_ext  WITH DEFAULT KEY,
          lt_cls     TYPE STANDARD TABLE OF string   WITH DEFAULT KEY,
          lv_to      TYPE i,
          lv_rf      TYPE i,
          lv_rt      TYPE i,
          lv_sel     TYPE i,
          lv_sel_id  TYPE string,
          lv_ext_seq TYPE i,
          lv_sgseq   TYPE i.

    direction = COND string( WHEN i_direction IS NOT INITIAL THEN i_direction ELSE 'LR' ).
    DATA(lo_win) = mo_viewer->mo_window.
    DATA(lv_focus) = mo_viewer->mv_cmap_focus.
    DATA(lv_focus_prog) = VALUE progname( ).

    IF lv_focus IS NOT INITIAL AND mo_viewer->mv_package IS NOT INITIAL AND mo_viewer->mt_pkg_objects IS INITIAL.
      mo_viewer->ensure_package_parsed( ).
    ENDIF.

    IF lv_focus IS NOT INITIAL.
      READ TABLE mo_viewer->mt_pkg_objects INTO DATA(ls_focus_obj) WITH KEY prog = lv_focus.
      IF sy-subrc = 0 AND ls_focus_obj-obj_type = 'PROG'.
        lv_focus_prog = lv_focus.
        CLEAR lv_focus.
      ENDIF.
    ENDIF.

    " Package mode: no focused class -> build the whole package (parse everything once).
    " After a double-click on a class, mv_cmap_focus scopes the graph to that class only.
    IF lv_focus IS INITIAL.
      mo_viewer->ensure_package_parsed( ).
    ENDIF.

    DATA(lv_pkg_mode) = xsdbool( lv_focus IS INITIAL
                            AND ( mo_viewer->mv_package IS NOT INITIAL
                               OR mo_viewer->mt_pkg_objects IS NOT INITIAL ) ).

    " --- 1. Collect all METHOD units directly from the parsed scan
    "     (METHOD..ENDMETHOD in t_keywords). This does NOT depend on
    "     tt_calls_line-index, which is only filled where code_execution_scanner
    "     has actually traced, so plain-parsed package classes are covered too. ---
    LOOP AT lo_win->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
      IF lv_focus IS NOT INITIAL AND <prog>-program <> lv_focus.
        CONTINUE.
      ENDIF.
      DATA(lo_scan) = <prog>-scan.
      CHECK lo_scan IS BOUND.

      " class name = program stripped of the =...CP / =...IP class-pool suffix
      DATA(lv_cls) = CONV string( <prog>-program ).
      REPLACE REGEX '=+(CP|IP)$' IN lv_cls WITH ``.

      LOOP AT <prog>-t_keywords INTO DATA(ls_mkw) WHERE name = 'METHOD'.
        READ TABLE lo_scan->tokens INDEX ls_mkw-from + 1 INTO DATA(ls_mtok).
        CHECK sy-subrc = 0 AND ls_mtok-str IS NOT INITIAL.

        lv_to = 0.
        LOOP AT <prog>-t_keywords INTO DATA(ls_ekw)
          WHERE index > ls_mkw-index AND name = 'ENDMETHOD'.
          lv_to = ls_ekw-index.
          EXIT.
        ENDLOOP.
        CHECK lv_to > 0.

        CLEAR: lv_rf, lv_rt.
        READ TABLE lo_scan->tokens INDEX ls_mkw-from INTO DATA(ls_tf).
        IF sy-subrc = 0. lv_rf = ls_tf-row. ENDIF.
        READ TABLE <prog>-t_keywords WITH KEY index = lv_to INTO ls_ekw.
        READ TABLE lo_scan->tokens INDEX ls_ekw-to INTO DATA(ls_tt).
        IF sy-subrc = 0. lv_rt = ls_tt-row. ENDIF.

        APPEND VALUE #( class     = lv_cls
                        real_class = lv_cls
                        name      = ls_mtok-str
                        program   = <prog>-program
                        include   = <prog>-include
                        stmt_from = ls_mkw-index
                        stmt_to   = lv_to
                        row_from  = lv_rf
                        row_to    = lv_rt ) TO lt_meth.
      ENDLOOP.

      IF lv_pkg_mode = abap_true AND lv_cls = CONV string( <prog>-program ).
        READ TABLE mo_viewer->mt_pkg_objects INTO DATA(ls_prog_obj) WITH KEY prog = <prog>-program.
        IF sy-subrc = 0 AND ls_prog_obj-obj_type = 'PROG'
           AND to_upper( CONV string( <prog>-program ) ) <> 'Z_ACE_STANDALONE'
           AND NOT line_exists( lo_win->ms_sources-tt_class_defs[ program = <prog>-program ] ).
          DATA(lv_first_stmt) = 0.
          DATA(lv_last_stmt)  = 0.
          LOOP AT <prog>-t_keywords INTO DATA(ls_pkw).
            IF lv_first_stmt = 0. lv_first_stmt = ls_pkw-index. ENDIF.
            lv_last_stmt = ls_pkw-index.
          ENDLOOP.
          IF lv_first_stmt > 0 AND lv_last_stmt > 0.
            APPEND VALUE #( class      = ''
                            real_class = ''
                            name       = COND string( WHEN ls_prog_obj-obj_name IS NOT INITIAL
                                                      THEN ls_prog_obj-obj_name
                                                      ELSE <prog>-program )
                            program    = <prog>-program
                            include    = <prog>-include
                            stmt_from  = lv_first_stmt
                            stmt_to    = lv_last_stmt ) TO lt_meth.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lt_meth IS INITIAL.
      open_mermaid( |graph { direction }\n  none["No methods found"]\n| ).
      RETURN.
    ENDIF.

    LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<m>).
      <m>-node_id = |M{ sy-tabix }|.
    ENDLOOP.

    " --- Package overview (no focus): aggregate to program/class nodes.
    "     Methods remain only as analysis points for parsing their calls. ---
    IF lv_pkg_mode = abap_true.
      DATA lt_nmap TYPE HASHED TABLE OF lty_nmap WITH UNIQUE KEY kind name.
      TYPES: BEGIN OF lty_pmap,
               program TYPE program,
               node_id TYPE string,
             END OF lty_pmap.
      DATA lt_pmap TYPE HASHED TABLE OF lty_pmap WITH UNIQUE KEY program.
      DATA lt_hidden_prog TYPE HASHED TABLE OF program WITH UNIQUE KEY table_line.
      DATA lv_nseq TYPE i.
      DATA lv_pseq TYPE i.
      DATA ls_po   TYPE zif_ace_parse_data=>ts_pkg_obj.
      DATA lt_pub    TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA lt_cls_ok TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

      " Diagram-only exclusions: keep these programs in package parsing/tree.
      LOOP AT lt_meth INTO DATA(ls_lm).
        DATA(lv_lowner) = ls_lm-program.
        READ TABLE lo_win->ms_sources-tt_progs WITH KEY include = ls_lm-include INTO DATA(ls_lop).
        IF sy-subrc = 0 AND ls_lop-program IS NOT INITIAL. lv_lowner = ls_lop-program. ENDIF.
        CLEAR ls_po.
        READ TABLE mo_viewer->mt_pkg_objects INTO ls_po WITH KEY prog = lv_lowner.
        IF sy-subrc = 0 AND ls_po-obj_type = 'PROG'
           AND ( to_upper( CONV string( lv_lowner ) ) = 'Z_ACE_STANDALONE'
              OR line_exists( lo_win->ms_sources-tt_class_defs[ program = lv_lowner ] ) ).
          INSERT lv_lowner INTO TABLE lt_hidden_prog.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<pm>).
        " Resolve the real owning program via the include (tt_calls_line-program
        " may be empty for local classes, so include -> tt_progs is authoritative)
        DATA(lv_owner) = <pm>-program.
        READ TABLE lo_win->ms_sources-tt_progs WITH KEY include = <pm>-include INTO DATA(ls_op).
        IF sy-subrc = 0 AND ls_op-program IS NOT INITIAL.
          lv_owner = ls_op-program.
        ENDIF.
        <pm>-program = lv_owner.
        IF line_exists( lt_hidden_prog[ table_line = lv_owner ] ).
          <pm>-node_id = ''.
          CONTINUE.
        ENDIF.

        CLEAR ls_po.
        READ TABLE mo_viewer->mt_pkg_objects INTO ls_po WITH KEY prog = lv_owner.
        DATA(lv_is_class) = xsdbool( sy-subrc = 0
                             AND ( ls_po-obj_type = 'CLAS' OR ls_po-obj_type = 'INTF' ) ).
        IF lv_is_class = abap_true.
          " global class / interface method — keep only public ones in the overview.
          " If RTTI knew this class, honour it; otherwise fall back to keeping the method.
          IF <pm>-name NS '~' AND line_exists( lt_cls_ok[ table_line = <pm>-class ] ).
            " interface methods (name~method) are always public — keep them
            READ TABLE lt_pub WITH KEY table_line = |{ <pm>-class }~{ <pm>-name }|
              TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0. <pm>-node_id = ''. ENDIF.
          ENDIF.
        ELSE.
          " program / function group / local class inside a report — collapse to one block
          READ TABLE lt_pmap WITH KEY program = lv_owner INTO DATA(ls_pm).
          IF sy-subrc <> 0.
            lv_pseq = lv_pseq + 1.
            ls_pm-program = lv_owner.
            ls_pm-node_id = |P{ lv_pseq }|.
            INSERT ls_pm INTO TABLE lt_pmap.
          ENDIF.
          <pm>-class   = 'Programs'.
          <pm>-disp_name = COND string( WHEN ls_po-obj_name IS NOT INITIAL THEN ls_po-obj_name ELSE lv_owner ).
          <pm>-node_id = ls_pm-node_id.
        ENDIF.

        DATA(lv_kind) = COND string( WHEN lv_is_class = abap_true THEN 'CLAS' ELSE 'PROG' ).
        DATA(lv_name) = COND string(
          WHEN lv_is_class = abap_true AND ls_po-obj_name IS NOT INITIAL THEN ls_po-obj_name
          WHEN lv_is_class = abap_true THEN <pm>-class
          WHEN ls_po-obj_name IS NOT INITIAL THEN ls_po-obj_name
          ELSE lv_owner ).
        READ TABLE lt_nmap INTO DATA(ls_nm) WITH KEY kind = lv_kind name = lv_name.
        IF sy-subrc <> 0.
          lv_nseq = lv_nseq + 1.
          ls_nm-kind    = lv_kind.
          ls_nm-name    = lv_name.
          ls_nm-node_id = |N{ lv_nseq }|.
          INSERT ls_nm INTO TABLE lt_nmap.
        ENDIF.
        <pm>-disp_class = COND string( WHEN lv_is_class = abap_true THEN 'Classes' ELSE 'Programs' ).
        <pm>-disp_name = lv_name.
        <pm>-agg_id  = ls_nm-node_id.
      ENDLOOP.
      DELETE lt_meth WHERE node_id IS INITIAL.
      IF lt_meth IS INITIAL.
        open_mermaid( |graph { direction }\n  none["No package objects found"]\n| ).
        RETURN.
      ENDIF.
    ENDIF.

    " --- 2. Selected method from cursor position (m_prg-line inside a method) ---
    lv_sel = 0.
    IF lv_pkg_mode = abap_false.
      LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<ms>)
        WHERE include = lo_win->m_prg-include AND row_from > 0.
        IF lo_win->m_prg-line >= <ms>-row_from AND lo_win->m_prg-line <= <ms>-row_to.
          lv_sel    = sy-tabix.
          lv_sel_id = <ms>-node_id.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " --- 3. Collect edges: parse calls on demand for each method in scope ---
    lv_ext_seq = 0.
    LOOP AT lt_meth INTO DATA(ls_meth).
      IF lv_sel > 0 AND ls_meth-node_id <> lv_sel_id. CONTINUE. ENDIF.

      LOOP AT lo_win->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog2>)
        WHERE include = ls_meth-include.

        LOOP AT <prog2>-t_keywords INTO DATA(ls_key)
          WHERE index >= ls_meth-stmt_from AND index <= ls_meth-stmt_to.

          IF ls_key-calls_parsed = abap_false.
            zcl_ace_parser=>parse_tokens(
              EXPORTING
                i_program  = CONV #( ls_key-program )
                i_include  = CONV #( ls_key-include )
                i_stmt_idx = ls_key-index
                i_class    = COND string( WHEN ls_meth-real_class IS NOT INITIAL THEN ls_meth-real_class ELSE ls_meth-class )
                i_evtype   = 'METHOD'
                i_ev_name  = ls_meth-name
              CHANGING
                cs_source  = lo_win->ms_sources ).
            READ TABLE <prog2>-t_keywords WITH KEY index = ls_key-index INTO ls_key.
          ENDIF.

          LOOP AT ls_key-tt_calls INTO DATA(ls_call).
            CHECK ls_call-name IS NOT INITIAL.

            DATA lv_int TYPE i.
            lv_int = 0.
            IF ls_call-event = 'METHOD'.
              IF ls_call-class IS NOT INITIAL.
                DATA(lv_call_class) = to_upper( CONV string( ls_call-class ) ).
                REPLACE REGEX '=+(CP|IP)$' IN lv_call_class WITH ``.
                LOOP AT lt_meth INTO DATA(ls_tgt)
                  WHERE name = ls_call-name.
                  CHECK to_upper( ls_tgt-real_class ) = lv_call_class.
                  lv_int = sy-tabix.
                  EXIT.
                ENDLOOP.
              ENDIF.
              " prefer a target in the same program (handles duplicate method names)
              IF lv_int = 0.
                LOOP AT lt_meth INTO ls_tgt
                  WHERE name = ls_call-name AND program = ls_meth-program.
                  lv_int = sy-tabix.
                  EXIT.
                ENDLOOP.
              ENDIF.
              IF lv_int = 0.
                LOOP AT lt_meth INTO ls_tgt WHERE name = ls_call-name.
                  lv_int = sy-tabix.
                  EXIT.
                ENDLOOP.
              ENDIF.
            ENDIF.

            IF lv_int > 0.
              READ TABLE lt_meth INTO ls_tgt INDEX lv_int.
              CHECK ls_tgt-node_id <> ls_meth-node_id.       " no self-loops
              APPEND VALUE #( from_id  = ls_meth-node_id
                              to_id    = ls_tgt-node_id
                              external = abap_false ) TO lt_edge.
            ELSE.
              DATA(lv_key) = |{ ls_call-event }:{ ls_call-class }:{ ls_call-name }|.
              READ TABLE lt_ext INTO DATA(ls_e) WITH KEY key = lv_key.
              IF sy-subrc <> 0.
                lv_ext_seq = lv_ext_seq + 1.
                CLEAR ls_e.
                ls_e-node_id = |E{ lv_ext_seq }|.
                ls_e-key     = lv_key.
                ls_e-label   = COND string(
                  WHEN ls_call-class IS NOT INITIAL
                  THEN |{ ls_call-class }: { ls_call-name }|
                  ELSE |{ ls_call-event } { ls_call-name }| ).
                APPEND ls_e TO lt_ext.
              ENDIF.
              APPEND VALUE #( from_id  = ls_meth-node_id
                              to_id    = ls_e-node_id
                              external = abap_true ) TO lt_edge.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    IF lv_pkg_mode = abap_true.
      TYPES: BEGIN OF lty_call_stack,
               stacklevel TYPE i,
               node_id    TYPE string,
             END OF lty_call_stack.
      DATA lt_flow_edge TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY.
      DATA lt_method_edge TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY.
      DATA lt_static_edge TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY.
      DATA lt_call_stack TYPE STANDARD TABLE OF lty_call_stack WITH DEFAULT KEY.
      DATA lt_pkg_root_prog TYPE HASHED TABLE OF program WITH UNIQUE KEY table_line.
      DATA lv_flow_used TYPE abap_bool.

      lt_method_edge = lt_edge.

      IF mv_all_methods = abap_false.
        LOOP AT lt_meth INTO DATA(ls_root_meth)
          WHERE node_id IS NOT INITIAL AND disp_class = 'Programs'.
          INSERT ls_root_meth-program INTO TABLE lt_pkg_root_prog.
        ENDLOOP.
      ENDIF.

      IF mv_all_methods = abap_false AND lt_pkg_root_prog IS NOT INITIAL.
        DATA(lv_old_depth) = lo_win->m_hist_depth.
        DATA(lt_old_steps) = mo_viewer->mt_steps.
        DATA(lv_old_step)  = mo_viewer->m_step.
        DATA(lt_old_stack) = lo_win->mt_stack.
        DATA(lt_old_calls) = lo_win->mt_calls.

        LOOP AT lt_pkg_root_prog INTO DATA(lv_root_prog).
          CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, lo_win->mt_stack, lo_win->mt_calls, lt_call_stack.
          zcl_ace_source_parser=>code_execution_scanner(
            i_program = lv_root_prog
            i_include = lv_root_prog
            io_debugger = mo_viewer ).

          LOOP AT mo_viewer->mt_steps INTO DATA(ls_flow_step).
            DATA(lv_flow_node) = ``.
            IF ls_flow_step-eventtype = 'METHOD' AND ls_flow_step-class IS NOT INITIAL.
              DATA(lv_step_class) = to_upper( CONV string( ls_flow_step-class ) ).
              REPLACE REGEX '=+(CP|IP)$' IN lv_step_class WITH ``.
              LOOP AT lt_meth INTO DATA(ls_flow_meth)
                WHERE agg_id IS NOT INITIAL.
                CHECK to_upper( ls_flow_meth-real_class ) = lv_step_class.
                lv_flow_node = ls_flow_meth-agg_id.
                EXIT.
              ENDLOOP.
            ELSE.
              READ TABLE lt_meth INTO DATA(ls_flow_prog)
                WITH KEY program = ls_flow_step-program disp_class = 'Programs'.
              IF sy-subrc = 0.
                lv_flow_node = ls_flow_prog-agg_id.
              ENDIF.
            ENDIF.
            IF lv_flow_node IS INITIAL. CONTINUE. ENDIF.

            IF ls_flow_step-stacklevel > 1.
              DATA(lv_parent_stack) = ls_flow_step-stacklevel - 1.
              READ TABLE lt_call_stack INTO DATA(ls_flow_caller)
                WITH KEY stacklevel = lv_parent_stack.
              IF sy-subrc = 0 AND ls_flow_caller-node_id <> lv_flow_node.
                APPEND VALUE #( from_id  = ls_flow_caller-node_id
                                to_id    = lv_flow_node
                                external = abap_false ) TO lt_flow_edge.
              ENDIF.
            ENDIF.

            DELETE lt_call_stack WHERE stacklevel >= ls_flow_step-stacklevel.
            APPEND VALUE #( stacklevel = ls_flow_step-stacklevel
                            node_id    = lv_flow_node ) TO lt_call_stack.
          ENDLOOP.
        ENDLOOP.

        lo_win->m_hist_depth = lv_old_depth.
        mo_viewer->mt_steps = lt_old_steps.
        mo_viewer->m_step   = lv_old_step.
        lo_win->mt_stack    = lt_old_stack.
        lo_win->mt_calls    = lt_old_calls.
        IF lt_flow_edge IS NOT INITIAL.
          lt_edge = lt_flow_edge.
          lv_flow_used = abap_true.
        ENDIF.
      ENDIF.

      LOOP AT lt_method_edge INTO DATA(ls_edge_raw).
        READ TABLE lt_meth INTO DATA(ls_from_meth) WITH KEY node_id = ls_edge_raw-from_id.
        IF sy-subrc <> 0 OR ls_from_meth-agg_id IS INITIAL. CONTINUE. ENDIF.
        IF ls_edge_raw-external = abap_true.
          APPEND VALUE #( from_id  = ls_from_meth-agg_id
                          to_id    = ls_edge_raw-to_id
                          external = abap_true ) TO lt_static_edge.
        ELSE.
          READ TABLE lt_meth INTO DATA(ls_to_meth) WITH KEY node_id = ls_edge_raw-to_id.
          IF sy-subrc <> 0 OR ls_to_meth-agg_id IS INITIAL. CONTINUE. ENDIF.
          CHECK ls_from_meth-agg_id <> ls_to_meth-agg_id.
          APPEND VALUE #( from_id  = ls_from_meth-agg_id
                          to_id    = ls_to_meth-agg_id
                          external = abap_false ) TO lt_static_edge.
        ENDIF.
      ENDLOOP.

      lt_edge = lt_static_edge.
      IF lv_flow_used = abap_true.
        APPEND LINES OF lt_flow_edge TO lt_edge.
      ENDIF.

      LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<ma>) WHERE agg_id IS NOT INITIAL.
        <ma>-node_id = <ma>-agg_id.
      ENDLOOP.

      IF lv_focus_prog IS NOT INITIAL.
        DATA lt_reach TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
        DATA lt_edge_focus TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY.
        READ TABLE lt_meth INTO DATA(ls_focus_meth)
          WITH KEY program = lv_focus_prog disp_class = 'Programs'.
        IF sy-subrc = 0 AND ls_focus_meth-node_id IS NOT INITIAL.
          INSERT ls_focus_meth-node_id INTO TABLE lt_reach.
          DATA(lv_reach_changed) = abap_true.
          WHILE lv_reach_changed = abap_true.
            lv_reach_changed = abap_false.
            LOOP AT lt_edge INTO DATA(ls_reach_edge) WHERE external = abap_false.
              READ TABLE lt_reach WITH KEY table_line = ls_reach_edge-from_id TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0. CONTINUE. ENDIF.
              READ TABLE lt_reach WITH KEY table_line = ls_reach_edge-to_id TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                INSERT ls_reach_edge-to_id INTO TABLE lt_reach.
                lv_reach_changed = abap_true.
              ENDIF.
            ENDLOOP.
          ENDWHILE.

          LOOP AT lt_edge INTO DATA(ls_focus_edge).
            READ TABLE lt_reach WITH KEY table_line = ls_focus_edge-from_id TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0. CONTINUE. ENDIF.
            IF ls_focus_edge-external = abap_false.
              READ TABLE lt_reach WITH KEY table_line = ls_focus_edge-to_id TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0. CONTINUE. ENDIF.
            ENDIF.
            APPEND ls_focus_edge TO lt_edge_focus.
          ENDLOOP.
          lt_edge = lt_edge_focus.

          LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<mf>) WHERE node_id IS NOT INITIAL.
            READ TABLE lt_reach WITH KEY table_line = <mf>-node_id TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0. CLEAR <mf>-node_id. ENDIF.
          ENDLOOP.
          DELETE lt_meth WHERE node_id IS INITIAL.
        ENDIF.
      ENDIF.

      DATA lt_rank TYPE HASHED TABLE OF lty_rank WITH UNIQUE KEY node_id.
      LOOP AT lt_meth INTO DATA(ls_rank_meth)
        WHERE node_id IS NOT INITIAL AND disp_class = 'Programs'.
        INSERT VALUE #( node_id = ls_rank_meth-node_id rank = 0 ) INTO TABLE lt_rank.
      ENDLOOP.

      DATA(lv_changed) = abap_true.
      WHILE lv_changed = abap_true.
        lv_changed = abap_false.
        LOOP AT lt_edge INTO DATA(ls_rank_edge) WHERE external = abap_false.
          READ TABLE lt_rank INTO DATA(ls_rank_from) WITH KEY node_id = ls_rank_edge-from_id.
          IF sy-subrc <> 0. CONTINUE. ENDIF.
          READ TABLE lt_rank INTO DATA(ls_rank_to) WITH KEY node_id = ls_rank_edge-to_id.
          IF sy-subrc <> 0.
            INSERT VALUE #( node_id = ls_rank_edge-to_id rank = ls_rank_from-rank + 1 ) INTO TABLE lt_rank.
            lv_changed = abap_true.
          ELSEIF ls_rank_to-rank > ls_rank_from-rank + 1.
            DELETE TABLE lt_rank WITH TABLE KEY node_id = ls_rank_edge-to_id.
            INSERT VALUE #( node_id = ls_rank_edge-to_id rank = ls_rank_from-rank + 1 ) INTO TABLE lt_rank.
            lv_changed = abap_true.
          ENDIF.
        ENDLOOP.
      ENDWHILE.

      LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<mr>)
        WHERE node_id IS NOT INITIAL AND disp_class = 'Classes'.
        READ TABLE lt_rank INTO DATA(ls_rank_node) WITH KEY node_id = <mr>-node_id.
        IF sy-subrc = 0 AND ls_rank_node-rank > 0.
          <mr>-disp_class = |Classes L{ ls_rank_node-rank }|.
        ELSE.
          <mr>-disp_class = 'Classes other'.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT lt_edge BY from_id to_id external.
    DELETE ADJACENT DUPLICATES FROM lt_edge COMPARING from_id to_id external.

    " --- 4. Render ---
    mm_string = |graph { direction }\n|.

    " which classes have visible internal nodes
    LOOP AT lt_meth INTO ls_meth.
      IF lv_sel > 0 AND ls_meth-node_id <> lv_sel_id
         AND line_index( lt_edge[ to_id = ls_meth-node_id ] ) = 0.
        CONTINUE.
      ENDIF.
      COLLECT COND string( WHEN ls_meth-disp_class IS NOT INITIAL THEN ls_meth-disp_class ELSE ls_meth-class ) INTO lt_cls.
    ENDLOOP.

    DATA lt_seen TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    DATA lv_node_label TYPE string.
    IF lv_pkg_mode = abap_true.
      LOOP AT lt_meth INTO ls_meth.
        IF ls_meth-disp_class <> 'Programs'. CONTINUE. ENDIF.
        READ TABLE lt_seen WITH KEY table_line = ls_meth-node_id TRANSPORTING NO FIELDS.
        IF sy-subrc = 0. CONTINUE. ENDIF.
        INSERT ls_meth-node_id INTO TABLE lt_seen.
        lv_node_label = COND string( WHEN ls_meth-disp_name IS NOT INITIAL THEN ls_meth-disp_name ELSE ls_meth-name ).
        mm_string = |{ mm_string }  { ls_meth-node_id }["{ replace( val = lv_node_label sub = `~` with = `-` ) }"]:::prog\n|.
      ENDLOOP.
      LOOP AT lt_cls INTO lv_cls.
        IF lv_cls = 'Programs'. CONTINUE. ENDIF.
        LOOP AT lt_meth INTO ls_meth.
          CHECK COND string( WHEN ls_meth-disp_class IS NOT INITIAL THEN ls_meth-disp_class ELSE ls_meth-class ) = lv_cls.
          READ TABLE lt_seen WITH KEY table_line = ls_meth-node_id TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. CONTINUE. ENDIF.
          INSERT ls_meth-node_id INTO TABLE lt_seen.
          lv_node_label = COND string( WHEN ls_meth-disp_name IS NOT INITIAL THEN ls_meth-disp_name ELSE ls_meth-name ).
          mm_string = |{ mm_string }  { ls_meth-node_id }["{ replace( val = lv_node_label sub = `~` with = `-` ) }"]:::cls\n|.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      LOOP AT lt_cls INTO lv_cls.
        lv_sgseq = lv_sgseq + 1.
        DATA(lv_title) = replace( val = COND string( WHEN lv_cls IS NOT INITIAL THEN lv_cls ELSE 'GLOBAL' )
                                  sub = `~` with = `-` ).
        mm_string = |{ mm_string }  subgraph SG{ lv_sgseq }["{ lv_title }"]\n|.
        LOOP AT lt_meth INTO ls_meth.
          CHECK COND string( WHEN ls_meth-disp_class IS NOT INITIAL THEN ls_meth-disp_class ELSE ls_meth-class ) = lv_cls.
          IF lv_sel > 0 AND ls_meth-node_id <> lv_sel_id
             AND line_index( lt_edge[ to_id = ls_meth-node_id ] ) = 0
             AND line_index( lt_edge[ from_id = ls_meth-node_id ] ) = 0.
            CONTINUE.
          ENDIF.
          READ TABLE lt_seen WITH KEY table_line = ls_meth-node_id TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. CONTINUE. ENDIF.
          INSERT ls_meth-node_id INTO TABLE lt_seen.
          DATA(lv_node_label2) = COND string( WHEN ls_meth-disp_name IS NOT INITIAL THEN ls_meth-disp_name ELSE ls_meth-name ).
          mm_string = |{ mm_string }    { ls_meth-node_id }["{ replace( val = lv_node_label2 sub = `~` with = `-` ) }"]\n|.
        ENDLOOP.
        mm_string = |{ mm_string }  end\n|.
      ENDLOOP.
    ENDIF.

    IF mv_show_ext = abap_true.
      LOOP AT lt_ext INTO ls_e.
        mm_string = |{ mm_string }  { ls_e-node_id }["{ replace( val = ls_e-label sub = `~` with = `-` ) }"]:::ext\n|.
      ENDLOOP.
    ENDIF.

    " At most one line between any two nodes (ignore direction / duplicates)
    DATA lt_pair TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    LOOP AT lt_edge INTO DATA(ls_ed).
      IF ls_ed-external = abap_true AND mv_show_ext = abap_false. CONTINUE. ENDIF.
      DATA(lv_pair) = COND string(
        WHEN ls_ed-from_id < ls_ed-to_id
        THEN |{ ls_ed-from_id }->{ ls_ed-to_id }|
        ELSE |{ ls_ed-to_id }->{ ls_ed-from_id }| ).
      IF line_exists( lt_pair[ table_line = lv_pair ] ). CONTINUE. ENDIF.
      INSERT lv_pair INTO TABLE lt_pair.
      mm_string = |{ mm_string }  { ls_ed-from_id } --> { ls_ed-to_id }\n|.
    ENDLOOP.

    IF mv_show_ext = abap_true.
      mm_string = |{ mm_string }classDef ext fill:#FDECEA,stroke:#E06666,color:#000\n|.
    ENDIF.
    IF lv_pkg_mode = abap_true.
      mm_string = |{ mm_string }classDef prog fill:#FFF2CC,stroke:#D6B656,color:#000\n|.
      mm_string = |{ mm_string }classDef cls fill:#E8E6FF,stroke:#9673A6,color:#000\n|.
    ENDIF.

    open_mermaid( mm_string ).

  ENDMETHOD.
  method ADD_TOOLBAR_BUTTONS.

      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.

      DATA(lv_depth) = mo_viewer->mo_window->m_hist_depth.

      button  = VALUE #(
       ( function = 'TB'       icon = CONV #( icon_view_expand_vertical )   quickinfo = 'Vertical'   text = '' )
       ( function = 'LR'       icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
       ( butn_type = 3 )
       ( function = 'CALLS'        icon = CONV #( icon_workflow_process ) quickinfo = 'Calls Flow'              text = 'Calls Flow' )
       ( function = 'FLOW'         icon = CONV #( icon_wizard )           quickinfo = 'Code Flow'               text = 'Code Flow' )
       ( function = 'CMAP'         icon = CONV #( icon_structure )        quickinfo = 'Static method call map'   text = 'Class Map' )
       ( butn_type = 3 )
       ( function = 'TOGGLE_CALC'  icon = CONV #( icon_biw_formula )      quickinfo = 'Toggle: show all steps / only calculated' text = 'Show All Steps' )
       ( function = 'TOGGLE_PARAMS' icon = CONV #( icon_parameter )       quickinfo = 'Toggle: show / hide call parameters'      text = 'Show Params' )
       ( function = 'TOGGLE_EXT'   icon = CONV #( icon_connect )          quickinfo = 'Toggle: show / hide external calls (Class Map)' text = 'Show External' )
       ( function = 'TOGGLE_ALLM'  icon = CONV #( icon_complete )         quickinfo = 'Toggle: include calls from all methods in Class Map' text = 'All Methods' )
       ( butn_type = 3 )
       ( function = 'DEPTH_M'  icon = CONV #( icon_arrow_left )            quickinfo = 'Decrease depth' text = '' )
       ( function = 'DEPTH'    icon = CONV #( icon_next_hierarchy_level )  quickinfo = 'Depth level' text = |Depth { lv_depth }| )
       ( function = 'DEPTH_P'  icon = CONV #( icon_arrow_right )           quickinfo = 'Increase depth' text = '' )
       ( butn_type = 3 )
       ( function = 'TEXT'     icon = CONV #( icon_wd_caption )            quickinfo = 'Mermaid Diagram text' text = '' )
       ( function = 'PERR'     icon = CONV #( icon_message_error_small )   quickinfo = 'Show last mermaid parse error' text = '' )
                      ).

      mo_toolbar->add_button_group( button ).

      event-eventid = cl_gui_toolbar=>m_id_function_selected.
      event-appl_event = space.
      APPEND event TO events.

      mo_toolbar->set_registered_events( events = events ).
      SET HANDLER me->hnd_toolbar FOR mo_toolbar.

  endmethod.
  method CONSTRUCTOR.

      DATA  text TYPE text100.

      super->constructor( ).

      mo_viewer = io_debugger.
      mv_type = i_type.

      CHECK ZCL_ACE=>i_mermaid_active = abap_true.

      CASE mv_type.
        WHEN 'CALLS'. text = 'Calls flow'.
        WHEN 'FLOW'.  text = 'Calculations sequence'.
        WHEN 'CMAP'.  text = 'Class map'.
      ENDCASE.

      IF mo_box IS INITIAL.
        mo_box = create( i_name = text i_width = 1000 i_hight = 300 ).

        APPEND INITIAL LINE TO ZCL_ACE=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
        <popup>-parent = mo_viewer->mo_window->mo_box.
        <popup>-child = mo_box.

        SET HANDLER on_box_close FOR mo_box.

        CREATE OBJECT mo_splitter
          EXPORTING parent = mo_box rows = 2 columns = 1
          EXCEPTIONS OTHERS = 1.

        mo_splitter->get_container( EXPORTING row = 2 column = 1 RECEIVING container = mo_mm_container ).
        mo_splitter->get_container( EXPORTING row = 1 column = 1 RECEIVING container = mo_mm_toolbar ).
        mo_splitter->set_row_height( id = 1 height = '3' ).
        mo_splitter->set_row_height( id = 2 height = '70' ).
        mo_splitter->set_row_sash( id = 1 type = 0 value = 0 ).

        CREATE OBJECT mo_toolbar EXPORTING parent = mo_mm_toolbar.
        add_toolbar_buttons( ).
        mo_toolbar->set_visible( 'X' ).
      ENDIF.

      CASE mv_type.
        WHEN 'CALLS'. steps_flow( i_with_params = mv_with_params i_calc_path = mv_calc_path ).
        WHEN 'FLOW'.  magic_search( i_calc_path = mv_calc_path ).
        WHEN 'CMAP'.  class_map( ).
      ENDCASE.

      IF mo_box IS NOT INITIAL.
        mo_box->set_focus( mo_box ).
      ENDIF.

  endmethod.
  method HND_TOOLBAR.

      IF fcode = 'TEXT'.
        DATA: mm_string TYPE string,
              ref       TYPE REF TO data.
        CALL METHOD mo_diagram->('GET_SOURCE_CODE_STRING') RECEIVING result = mm_string.
        GET REFERENCE OF mm_string INTO ref.
        NEW ZCL_ACE_TEXT_VIEWER( ref ).
        RETURN.
      ENDIF.

      IF fcode = 'PERR'.
        DATA: lv_perr TYPE string,
              ref_err TYPE REF TO data.
        TRY.
            CALL METHOD mo_diagram->('GET_LAST_PARSE_ERROR') RECEIVING result = lv_perr.
          CATCH cx_root.
        ENDTRY.
        IF lv_perr IS INITIAL.
          lv_perr = 'No mermaid parse error recorded (diagram rendered OK or not yet displayed).'.
        ENDIF.
        GET REFERENCE OF lv_perr INTO ref_err.
        NEW zcl_ace_text_viewer( ref_err ).
        RETURN.
      ENDIF.

      IF fcode = 'LR' OR fcode = 'TB'.
        mv_direction = fcode.
      ELSEIF fcode = 'TOGGLE_CALC'.
        mv_calc_path = COND #( WHEN mv_calc_path = abap_true THEN abap_false ELSE abap_true ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'TOGGLE_CALC'
                    text  = COND #( WHEN mv_calc_path = abap_true
                                    THEN 'Only Calculated'
                                    ELSE 'Show All Steps' ) ).
      ELSEIF fcode = 'TOGGLE_PARAMS'.
        mv_with_params = COND #( WHEN mv_with_params = abap_true THEN abap_false ELSE abap_true ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'TOGGLE_PARAMS'
                    text  = COND #( WHEN mv_with_params = abap_true
                                    THEN 'Hide Params'
                                    ELSE 'Show Params' ) ).
      ELSEIF fcode = 'TOGGLE_EXT'.
        mv_show_ext = COND #( WHEN mv_show_ext = abap_true THEN abap_false ELSE abap_true ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'TOGGLE_EXT'
                    text  = COND #( WHEN mv_show_ext = abap_true
                                    THEN 'Hide External'
                                    ELSE 'Show External' ) ).
      ELSEIF fcode = 'TOGGLE_ALLM'.
        mv_all_methods = COND #( WHEN mv_all_methods = abap_true THEN abap_false ELSE abap_true ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'TOGGLE_ALLM'
                    text  = COND #( WHEN mv_all_methods = abap_true
                                    THEN 'Path Only'
                                    ELSE 'All Methods' ) ).
      ELSEIF fcode = 'DEPTH_M'.
        IF mo_viewer->mo_window->m_hist_depth > 0.
          mo_viewer->mo_window->m_hist_depth = mo_viewer->mo_window->m_hist_depth - 1.
        ENDIF.
        mo_viewer->mo_window->apply_depth( ).
        IF mo_box IS NOT INITIAL.
          mo_box->set_focus( mo_box ).
        ENDIF.
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'DEPTH'
                    text  = |Depth { mo_viewer->mo_window->m_hist_depth }| ).
        RETURN.
      ELSEIF fcode = 'DEPTH_P'.
        IF mo_viewer->mo_window->m_hist_depth < 99.
          mo_viewer->mo_window->m_hist_depth = mo_viewer->mo_window->m_hist_depth + 1.
        ENDIF.
        mo_viewer->mo_window->apply_depth( ).
        IF mo_box IS NOT INITIAL.
          mo_box->set_focus( mo_box ).
        ENDIF.
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'DEPTH'
                    text  = |Depth { mo_viewer->mo_window->m_hist_depth }| ).
        RETURN.
      ELSEIF fcode = 'DEPTH'.
        DATA: lv_answer TYPE c LENGTH 1, lv_value1 TYPE spop-varvalue1.
        CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
          EXPORTING
            textline1   = |Current depth: { mo_viewer->mo_window->m_hist_depth }. Enter new value (0-99):|
            titel       = 'Set Depth'
            valuelength = '2'
          IMPORTING
            answer      = lv_answer
            value1      = lv_value1
          EXCEPTIONS
            OTHERS      = 1.
        IF sy-subrc <> 0 OR lv_answer <> 'J' OR lv_value1 IS INITIAL. RETURN. ENDIF.
        DATA(lv_new_depth) = CONV i( lv_value1 ).
        IF lv_new_depth < 0.
          lv_new_depth = 0.
        ELSEIF lv_new_depth > 99.
          lv_new_depth = 99.
        ENDIF.
        mo_viewer->mo_window->m_hist_depth = lv_new_depth.
        mo_viewer->mo_window->apply_depth( ).
        IF mo_box IS NOT INITIAL.
          mo_box->set_focus( mo_box ).
        ENDIF.
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'DEPTH'
                    text  = |Depth { mo_viewer->mo_window->m_hist_depth }| ).
        RETURN.
      ELSE.
        mv_type = fcode.
      ENDIF.

      refresh( ).

  endmethod.
  method OPEN_MERMAID.

      CHECK ZCL_ACE=>i_mermaid_active = abap_true.

      TRY.
          IF mo_diagram IS INITIAL.
            CREATE OBJECT mo_diagram TYPE ('ZCL_WD_GUI_MERMAID_JS_DIAGRAM')
              EXPORTING parent = mo_mm_container hide_scrollbars = abap_false.
            " Raise mermaid limits so large package graphs still render
            DATA lv_cfg TYPE string.
            CALL METHOD mo_diagram->('GET_CONFIGURATION_JSON') RECEIVING result = lv_cfg.
            IF lv_cfg NP '*maxEdges*'.
              REPLACE FIRST OCCURRENCE OF '{' IN lv_cfg
                WITH '{"maxEdges":100000,"maxTextSize":90000000,'.
              CALL METHOD mo_diagram->('SET_CONFIGURATION_JSON') EXPORTING config_json = lv_cfg.
            ENDIF.
          ENDIF.
          CALL METHOD mo_diagram->('SET_SOURCE_CODE_STRING') EXPORTING source_code = i_mm_string.
          CALL METHOD mo_diagram->('DISPLAY').
        CATCH cx_root.
          CLEAR: mo_diagram,
                 mo_toolbar,
                 mo_splitter,
                 mo_mm_container,
                 mo_mm_toolbar,
                 mo_box.
      ENDTRY.

  endmethod.
  method REFRESH.

      CASE mv_type.
        WHEN 'CALLS'.
          steps_flow( i_direction   = mv_direction
                      i_with_params = mv_with_params
                      i_calc_path   = mv_calc_path ).
        WHEN 'FLOW'.
          magic_search( i_direction = mv_direction
                        i_calc_path = mv_calc_path ).
        WHEN 'CMAP'.
          class_map( i_direction = mv_direction ).
      ENDCASE.

  endmethod.
  METHOD steps_flow.

    TYPES: BEGIN OF lty_entity,
             include   TYPE string,
             class     TYPE string,
             event     TYPE string,
             name      TYPE string,
             style     TYPE string,
             eventname TYPE string,   " raw method/form/function name for binding lookup
           END OF lty_entity,
           BEGIN OF t_ind,
             from TYPE i,
             to   TYPE i,
           END OF t_ind,
           BEGIN OF t_stack_entry,
             stacklevel TYPE i,
             entity_idx TYPE i,    " индекс ноды в таблице entities
             name       TYPE string,
           END OF t_stack_entry.

    CONSTANTS: c_style_event    TYPE string VALUE 'event',
               c_style_method   TYPE string VALUE 'method',
               c_style_form     TYPE string VALUE 'form',
               c_style_constr   TYPE string VALUE 'constr',
               c_style_enh      TYPE string VALUE 'enh',
               c_style_function TYPE string VALUE 'func'.

    DATA: mm_string    TYPE string,
          entities     TYPE TABLE OF lty_entity,
          entity       TYPE lty_entity,
          ind          TYPE t_ind,
          indexes      TYPE TABLE OF t_ind,
          ids_event    TYPE TABLE OF string,
          ids_method   TYPE TABLE OF string,
          ids_form     TYPE TABLE OF string,
          ids_constr   TYPE TABLE OF string,
          ids_enh      TYPE TABLE OF string,
          ids_function TYPE TABLE OF string,
          call_stack   TYPE TABLE OF t_stack_entry.

    DATA(copy) = mo_viewer->mt_steps.

    " Filter steps to only calculated ones when requested
    IF i_calc_path = abap_true.
      DATA(lt_flow) = mo_viewer->get_code_flow( i_calc_path = abap_true ).
      DATA lt_active_ev TYPE TABLE OF string WITH EMPTY KEY.
      LOOP AT lt_flow INTO DATA(ls_fl) WHERE active_root = abap_true.
        READ TABLE lt_active_ev WITH KEY table_line = ls_fl-ev_name TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_fl-ev_name TO lt_active_ev.
        ENDIF.
      ENDLOOP.
      DATA lt_copy_filt LIKE copy.
      LOOP AT copy INTO DATA(ls_cp_filt).
        READ TABLE lt_active_ev WITH KEY table_line = ls_cp_filt-eventname TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND ls_cp_filt TO lt_copy_filt.
        ENDIF.
      ENDLOOP.
      copy = lt_copy_filt.
    ENDIF.

    " ── Step 1: collect unique nodes ────────────────────────────────
    LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
      entity-event     = <copy>-eventtype.
      entity-eventname = <copy>-eventname.   " save raw name before overwrite below

      IF <copy>-eventtype = 'METHOD'.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
          WITH KEY include   = <copy>-include
                   eventtype = 'METHOD'
                   eventname = <copy>-eventname
                   class     = <copy>-class
          INTO DATA(call_line).
        entity-name  = |"{ call_line-class }->{ <copy>-eventname }"|.
        entity-style = COND string(
          WHEN <copy>-eventname = 'CONSTRUCTOR' OR <copy>-eventname = 'CLASS_CONSTRUCTOR'
          THEN c_style_constr ELSE c_style_method ).
      ELSE.
        entity-name = SWITCH string( <copy>-eventtype
          WHEN 'FUNCTION'    THEN |"FUNCTION:{ <copy>-eventname }"|
          WHEN 'SCREEN'      THEN |"CALL SCREEN { <copy>-eventname }"|
          WHEN 'MODULE'      THEN |"MODULE { <copy>-eventname }"|
          WHEN 'FORM'        THEN |"FORM { <copy>-eventname }"|
          WHEN 'ENHANCEMENT' THEN |"ENH { <copy>-eventname }"|
          ELSE                    |"{ <copy>-program }:{ <copy>-eventname }"| ).
        entity-style = SWITCH string( <copy>-eventtype
          WHEN 'FUNCTION'    THEN c_style_function
          WHEN 'FORM'        THEN c_style_form
          WHEN 'ENHANCEMENT' THEN c_style_enh
          WHEN 'MODULE'      THEN c_style_form
          ELSE                    c_style_event ).
      ENDIF.

      <copy>-eventname   = entity-name.
      entity-include = <copy>-include.
      entity-class   = <copy>-class.

      READ TABLE entities
        WITH KEY include = entity-include class = entity-class name = entity-name
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND entity TO entities.
        DATA(lv_node_id) = |{ lines( entities ) }|.
        CASE entity-style.
          WHEN c_style_event.    APPEND lv_node_id TO ids_event.
          WHEN c_style_method.   APPEND lv_node_id TO ids_method.
          WHEN c_style_form.     APPEND lv_node_id TO ids_form.
          WHEN c_style_constr.   APPEND lv_node_id TO ids_constr.
          WHEN c_style_enh.      APPEND lv_node_id TO ids_enh.
          WHEN c_style_function. APPEND lv_node_id TO ids_function.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    mm_string = |graph { COND string( WHEN i_direction IS NOT INITIAL THEN i_direction ELSE 'TD' ) }\n |.

    " ── Шаг 2: явно объявляем все ноды ─────────────────────────────
    DATA(lv_idx) = 0.
    LOOP AT entities INTO entity.
      lv_idx = lv_idx + 1.
      mm_string = |{ mm_string }{ lv_idx }({ entity-name })\n|.
    ENDLOOP.

    " ── Шаг 3: строим стрелки через явный стек вызовов ─────────────
    " call_stack хранит ноды по уровням.
    " Когда приходит новый шаг с stacklevel=N:
    "   - caller = нода в стеке на уровне N-1
    "   - рисуем стрелку caller → текущая нода (если ещё не было)
    "   - обновляем стек: на уровне N теперь текущая нода

    DATA lv_prev_stack TYPE i.

    LOOP AT copy INTO DATA(step2).

      READ TABLE entities
        WITH KEY name = step2-eventname
        TRANSPORTING NO FIELDS.
      DATA(lv_cur_idx) = sy-tabix.

      DATA(lv_level) = step2-stacklevel.

      " Ищем caller — нода на уровне lv_level - 1 в call_stack
      IF lv_level > 1.
        READ TABLE call_stack
          WITH KEY stacklevel = lv_level - 1
          INTO DATA(ls_caller).
        IF sy-subrc = 0 AND ls_caller-entity_idx <> lv_cur_idx.
          " Draw edge only if not yet drawn
          ind-from = ls_caller-entity_idx.
          ind-to   = lv_cur_idx.
          READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            DATA(lv_edge_label) = ``.

            IF i_with_params = abap_true.
              " Look up parameter bindings: search caller's keywords for a call to callee
              DATA(ls_caller_ent) = entities[ ind-from ].
              DATA(ls_callee_ent) = entities[ ind-to ].
              READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
                WITH KEY include = ls_caller_ent-include
                INTO DATA(ls_prog_wp).
              IF sy-subrc = 0.
                LOOP AT ls_prog_wp-t_keywords INTO DATA(ls_kw_wp).
                  LOOP AT ls_kw_wp-tt_calls INTO DATA(ls_call_wp)
                    WHERE name  = ls_callee_ent-eventname
                      AND class = ls_callee_ent-class.
                    LOOP AT ls_call_wp-bindings INTO DATA(ls_bind_wp).
                      IF lv_edge_label IS INITIAL.
                        lv_edge_label = |{ ls_bind_wp-inner }={ ls_bind_wp-outer }|.
                      ELSE.
                        lv_edge_label = |{ lv_edge_label }<br/>{ ls_bind_wp-inner }={ ls_bind_wp-outer }|.
                      ENDIF.
                    ENDLOOP.
                    IF lv_edge_label IS NOT INITIAL. EXIT. ENDIF.
                  ENDLOOP.
                  IF lv_edge_label IS NOT INITIAL. EXIT. ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.

            IF lv_edge_label IS NOT INITIAL.
              DATA(lv_el_fmt) = format_node_label( i_code = lv_edge_label i_maxlen = 0 ).
              mm_string = |{ mm_string }{ ind-from } -->\|"{ lv_el_fmt }"\|{ ind-to }\n|.
            ELSE.
              mm_string = |{ mm_string }{ ind-from } --> { ind-to }\n|.
            ENDIF.
            APPEND ind TO indexes.
          ENDIF.
        ENDIF.
      ENDIF.

      " Обновляем стек: удаляем все уровни >= lv_level и добавляем текущий
      DELETE call_stack WHERE stacklevel >= lv_level.
      APPEND VALUE t_stack_entry(
        stacklevel = lv_level
        entity_idx = lv_cur_idx
        name       = step2-eventname
      ) TO call_stack.

      lv_prev_stack = lv_level.
    ENDLOOP.

    " ── Шаг 4: стили ────────────────────────────────────────────────
    IF mv_type = 'CMAP' AND mo_viewer->mv_cmap_focus IS NOT INITIAL.
      DATA(lv_enrich_from) = 0.
      LOOP AT entities INTO DATA(ls_enrich_src).
        lv_enrich_from = lv_enrich_from + 1.
        CHECK ls_enrich_src-style = c_style_method.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
          WITH KEY include   = ls_enrich_src-include
                   eventtype = 'METHOD'
                   eventname = ls_enrich_src-eventname
                   class     = ls_enrich_src-class
          INTO DATA(ls_enrich_line).
        CHECK sy-subrc = 0.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = ls_enrich_line-include
          INTO DATA(ls_enrich_prog).
        CHECK sy-subrc = 0.

        LOOP AT ls_enrich_prog-t_keywords INTO DATA(ls_enrich_kw)
          WHERE index >= ls_enrich_line-index AND index <= ls_enrich_line-end_idx.
          IF ls_enrich_kw-calls_parsed = abap_false.
            zcl_ace_parser=>parse_tokens(
              EXPORTING
                i_program  = CONV #( ls_enrich_kw-program )
                i_include  = CONV #( ls_enrich_kw-include )
                i_stmt_idx = ls_enrich_kw-index
                i_class    = ls_enrich_src-class
                i_evtype   = 'METHOD'
                i_ev_name  = ls_enrich_src-eventname
              CHANGING
                cs_source  = mo_viewer->mo_window->ms_sources ).
            READ TABLE ls_enrich_prog-t_keywords WITH KEY index = ls_enrich_kw-index INTO ls_enrich_kw.
          ENDIF.

          LOOP AT ls_enrich_kw-tt_calls INTO DATA(ls_enrich_call)
            WHERE event = 'METHOD' AND name IS NOT INITIAL.
            DATA(lv_enrich_to) = 0.
            LOOP AT entities INTO DATA(ls_enrich_tgt).
              CHECK ls_enrich_tgt-style = c_style_method
                AND ls_enrich_tgt-eventname = ls_enrich_call-name.
              IF ls_enrich_call-class IS NOT INITIAL
                 AND to_upper( ls_enrich_tgt-class ) <> to_upper( CONV string( ls_enrich_call-class ) ).
                CONTINUE.
              ENDIF.
              lv_enrich_to = sy-tabix.
              EXIT.
            ENDLOOP.
            CHECK lv_enrich_to > 0 AND lv_enrich_to <> lv_enrich_from.
            READ TABLE indexes WITH KEY from = lv_enrich_from to = lv_enrich_to TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              mm_string = |{ mm_string }{ lv_enrich_from } -.-> { lv_enrich_to }\n|.
              APPEND VALUE #( from = lv_enrich_from to = lv_enrich_to ) TO indexes.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    mm_string = |{ mm_string } classDef event    fill:#FFE0B2,stroke:#E65100\n|.
    mm_string = |{ mm_string } classDef method   fill:#BBDEFB,stroke:#1565C0\n|.
    mm_string = |{ mm_string } classDef form     fill:#EEEEEE,stroke:#616161\n|.
    mm_string = |{ mm_string } classDef constr   fill:#E1BEE7,stroke:#6A1B9A\n|.
    mm_string = |{ mm_string } classDef enh      fill:#FCE4EC,stroke:#AD1457\n|.
    mm_string = |{ mm_string } classDef func     fill:#C8E6C9,stroke:#2E7D32\n|.

    DATA(lv_ids) = ``.
    IF ids_event    IS NOT INITIAL. CONCATENATE LINES OF ids_event    INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } event\n|.    ENDIF.
    IF ids_method   IS NOT INITIAL. CONCATENATE LINES OF ids_method   INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } method\n|.   ENDIF.
    IF ids_form     IS NOT INITIAL. CONCATENATE LINES OF ids_form     INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } form\n|.     ENDIF.
    IF ids_constr   IS NOT INITIAL. CONCATENATE LINES OF ids_constr   INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } constr\n|.   ENDIF.
    IF ids_enh      IS NOT INITIAL. CONCATENATE LINES OF ids_enh      INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } enh\n|.      ENDIF.
    IF ids_function IS NOT INITIAL. CONCATENATE LINES OF ids_function INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } func\n|.     ENDIF.

    mm_string = |{ mm_string }\n|.
    open_mermaid( mm_string ).

  ENDMETHOD.
ENDCLASS.

CLASS zcl_ace_keywords IMPLEMENTATION.

  METHOD get_all.
    IF mv_cached = abap_false.
      build( ).
    ENDIF.
    result = mt_cache.
  ENDMETHOD.

  METHOD is_keyword.
    IF mv_cached = abap_false.
      build( ).
    ENDIF.
    DATA(up) = to_upper( token ).
    result = boolc( line_exists( mt_cache[ word = up ] ) ).
  ENDMETHOD.

  METHOD reset.
    CLEAR: mt_cache, mv_cached, mt_visited_exprs.
  ENDMETHOD.

  METHOD build.
    CLEAR: mt_cache, mt_visited_exprs.
    collect_from_class( class_name    = 'ZCL_ACE_STMTS'
                        method_prefix = 'STMT_' ).
    collect_from_class( class_name    = 'ZCL_ACE_EXPRS'
                        method_prefix = 'EXPR_' ).
    mv_cached = abap_true.
  ENDMETHOD.

  METHOD collect_from_class.
    " RTTI: enumerate all class-methods matching the prefix and dynamically invoke each
    DATA(class_descr) = CAST cl_abap_classdescr(
      cl_abap_typedescr=>describe_by_name( class_name ) ).

    LOOP AT class_descr->methods INTO DATA(method_descr).
      CHECK method_descr-name CP |{ method_prefix }*|.
      CHECK method_descr-is_class = abap_true.
      CHECK method_descr-visibility = cl_abap_classdescr=>public.

      DATA node TYPE REF TO zcl_ace_combi_node.
      TRY.
          CALL METHOD (class_name)=>(method_descr-name)
            RECEIVING
              r = node.
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      " For Expression methods (EXPR_*) mark as visited so that recursive
      " references back through expr( ) don't re-walk them.
      IF method_prefix = 'EXPR_'.
        DATA(expr_name) = substring( val = method_descr-name
                                     off = strlen( method_prefix ) ).
        INSERT expr_name INTO TABLE mt_visited_exprs.
      ENDIF.

      walk_node( node ).
    ENDLOOP.
  ENDMETHOD.

  METHOD walk_node.
    CHECK node IS BOUND.

    " Mirrors zcl_ace_combi_node->list_keywords( ) but ALSO follows
    " Expression references so we capture keywords contributed by
    " sub-expressions reached only via expr( ).
    CASE node->kind.
      WHEN zcl_ace_combi_node=>c_kind_word
        OR zcl_ace_combi_node=>c_kind_wseq.
        add_keyword( node->value ).

      WHEN zcl_ace_combi_node=>c_kind_token
        OR zcl_ace_combi_node=>c_kind_regex.
        " contributes nothing
        RETURN.

      WHEN zcl_ace_combi_node=>c_kind_expr.
        " Expression reference — walk the referenced expression once
        DATA(name) = node->value.
        IF line_exists( mt_visited_exprs[ table_line = name ] ).
          RETURN.
        ENDIF.
        INSERT name INTO TABLE mt_visited_exprs.
        DATA(method_name) = |EXPR_{ name }|.
        DATA child TYPE REF TO zcl_ace_combi_node.
        TRY.
            CALL METHOD ('ZCL_ACE_EXPRS')=>(method_name)
              RECEIVING
                r = child.
            walk_node( child ).
          CATCH cx_root.
            " Expression not yet ported — skip
            RETURN.
        ENDTRY.

      WHEN OTHERS.
        " seq / alt / opt / star / plus / per / vers — recurse into children
        LOOP AT node->children INTO DATA(c).
          walk_node( c ).
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.

  METHOD add_keyword.
    DATA(up) = to_upper( raw ).
    " Always store the literal (single word OR full phrase) — useful for callers
    " that want exact phrase matching (e.g. "END OF").
    DATA is_phrase TYPE abap_bool.
    is_phrase = boolc( up CS ` ` OR up CS `-` ).
    INSERT VALUE ts_keyword( word = up is_phrase = is_phrase )
      INTO TABLE mt_cache.

    " For multi-word phrases also store individual words so that token-by-token
    " classification (1 token from the scanner) finds them.
    IF is_phrase = abap_true.
      DATA(parts) = up.
      REPLACE ALL OCCURRENCES OF `-` IN parts WITH ` `.
      SPLIT parts AT ` ` INTO TABLE DATA(words).
      LOOP AT words INTO DATA(w).
        CHECK w IS NOT INITIAL.
        INSERT VALUE ts_keyword( word = w is_phrase = abap_false )
          INTO TABLE mt_cache.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ZCL_ACE_HTML_VIEWER IMPLEMENTATION.
  method CONSTRUCTOR.

      super->constructor( ).
      mo_box = create( i_name = i_title i_width = i_width i_hight = i_height ).
      IF mo_box IS INITIAL. RETURN. ENDIF.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_box
          rows    = 1
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter->get_container(
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_variables_container ).

      SET HANDLER on_box_close FOR mo_box.

      CREATE OBJECT mo_html
        EXPORTING
          parent             = mo_variables_container
        EXCEPTIONS
          cntl_error         = 1
          cntl_install_error = 2
          dp_install_error   = 3
          dp_error           = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        on_box_close( mo_box ).
        RETURN.
      ENDIF.

      DATA lt_data TYPE w3htmltab.
      DATA lv_url  TYPE w3url.
      lt_data = it_html.

      mo_html->load_data(
        IMPORTING
          assigned_url = lv_url
        CHANGING
          data_table   = lt_data
        EXCEPTIONS
          OTHERS       = 1 ).

      mo_html->show_url( url = lv_url ).
      cl_gui_cfw=>flush( ).
  endmethod.
ENDCLASS.

CLASS zcl_ace_exprs IMPLEMENTATION.

  METHOD expr_cond.
    " Cond.ts grammar boils down to comparisons joined by AND/OR/NOT/EQUIV +
    " parentheses. Keyword-relevant tokens:
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>str( `AND` ) )
      ( zcl_ace_combi=>str( `OR` ) )
      ( zcl_ace_combi=>str( `NOT` ) )
      ( zcl_ace_combi=>str( `EQUIV` ) )
      ( zcl_ace_combi=>expr( `COMPARE` ) ) ) ).
  ENDMETHOD.

  METHOD expr_compare.
    " Compare.ts — basic shape: Source CompareOperator Source, with IS/BETWEEN/IN/LIKE variants
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>str( `IS` ) )
      ( zcl_ace_combi=>str( `INITIAL` ) )
      ( zcl_ace_combi=>str( `BOUND` ) )
      ( zcl_ace_combi=>str( `ASSIGNED` ) )
      ( zcl_ace_combi=>str( `SUPPLIED` ) )
      ( zcl_ace_combi=>str( `INSTANCE OF` ) )
      ( zcl_ace_combi=>str( `BETWEEN` ) )
      ( zcl_ace_combi=>str( `IN` ) )
      ( zcl_ace_combi=>str( `LIKE` ) )
      ( zcl_ace_combi=>str( `CO` ) )
      ( zcl_ace_combi=>str( `CN` ) )
      ( zcl_ace_combi=>str( `CA` ) )
      ( zcl_ace_combi=>str( `NA` ) )
      ( zcl_ace_combi=>str( `CS` ) )
      ( zcl_ace_combi=>str( `NS` ) )
      ( zcl_ace_combi=>str( `CP` ) )
      ( zcl_ace_combi=>str( `NP` ) )
      ( zcl_ace_combi=>expr( `COMPARE_OPERATOR` ) ) ) ).
  ENDMETHOD.

  METHOD expr_compare_operator.
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>str( `EQ` ) )
      ( zcl_ace_combi=>str( `NE` ) )
      ( zcl_ace_combi=>str( `LT` ) )
      ( zcl_ace_combi=>str( `LE` ) )
      ( zcl_ace_combi=>str( `GT` ) )
      ( zcl_ace_combi=>str( `GE` ) ) ) ).
  ENDMETHOD.

  METHOD expr_source.
    " Source — value expression. For keyword purposes, the constructor expressions:
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>str( `VALUE` ) )
      ( zcl_ace_combi=>str( `NEW` ) )
      ( zcl_ace_combi=>str( `REF` ) )
      ( zcl_ace_combi=>str( `CONV` ) )
      ( zcl_ace_combi=>str( `CAST` ) )
      ( zcl_ace_combi=>str( `EXACT` ) )
      ( zcl_ace_combi=>str( `COND` ) )
      ( zcl_ace_combi=>str( `SWITCH` ) )
      ( zcl_ace_combi=>str( `REDUCE` ) )
      ( zcl_ace_combi=>str( `FILTER` ) )
      ( zcl_ace_combi=>str( `CORRESPONDING` ) )
      ( zcl_ace_combi=>str( `BOOLC` ) )
      ( zcl_ace_combi=>str( `XSDBOOL` ) )
      ( zcl_ace_combi=>str( `LET` ) )
      ( zcl_ace_combi=>str( `IN` ) )
      ( zcl_ace_combi=>str( `THEN` ) )
      ( zcl_ace_combi=>str( `ELSE` ) ) ) ).
  ENDMETHOD.

  METHOD expr_target.
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>expr( `FIELD_CHAIN` ) )
      ( zcl_ace_combi=>expr( `INLINE_DATA` ) )
      ( zcl_ace_combi=>expr( `FIELD_SYMBOL` ) ) ) ).
  ENDMETHOD.

  METHOD expr_field.
    r = zcl_ace_combi=>tok( `Identifier` ).
  ENDMETHOD.

  METHOD expr_field_chain.
    r = zcl_ace_combi=>expr( `FIELD` ).
  ENDMETHOD.

  METHOD expr_field_symbol.
    r = zcl_ace_combi=>tok( `Identifier` ).
  ENDMETHOD.

  METHOD expr_data_definition.
    " DataDefinition — name + TYPE/LIKE clause + value
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>expr( `FIELD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>alt( VALUE #(
          ( zcl_ace_combi=>str( `TYPE` ) )
          ( zcl_ace_combi=>str( `LIKE` ) ) ) ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `READ-ONLY` ) ) ) ) ).
  ENDMETHOD.

  METHOD expr_inline_data.
    r = zcl_ace_combi=>tok( `Identifier` ).
  ENDMETHOD.

  METHOD expr_loop_source.
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>expr( `FIELD_CHAIN` ) )
      ( zcl_ace_combi=>str( `SCREEN` ) ) ) ).
  ENDMETHOD.

  METHOD expr_loop_target.
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `INTO` ) )
          ( zcl_ace_combi=>expr( `TARGET` ) ) ) ) )
      ( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `ASSIGNING` ) )
          ( zcl_ace_combi=>expr( `FIELD_SYMBOL` ) ) ) ) )
      ( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `REFERENCE INTO` ) )
          ( zcl_ace_combi=>expr( `TARGET` ) ) ) ) ) ) ).
  ENDMETHOD.

  METHOD expr_for.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FOR` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EACH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) ) ) ).
  ENDMETHOD.

ENDCLASS.

CLASS zcl_ace_combi_node IMPLEMENTATION.

  METHOD constructor.
    me->kind     = kind.
    me->value    = value.
    me->children = children.
  ENDMETHOD.

  METHOD new_word.
    r = NEW #( kind = c_kind_word value = to_upper( s ) ).
  ENDMETHOD.

  METHOD new_wseq.
    r = NEW #( kind = c_kind_wseq value = to_upper( s ) ).
  ENDMETHOD.

  METHOD new_token.
    r = NEW #( kind = c_kind_token value = token_name ).
  ENDMETHOD.

  METHOD new_regex.
    r = NEW #( kind = c_kind_regex value = pattern ).
  ENDMETHOD.

  METHOD new_seq.
    r = NEW #( kind = c_kind_seq children = children ).
  ENDMETHOD.

  METHOD new_alt.
    r = NEW #( kind = c_kind_alt children = children ).
  ENDMETHOD.

  METHOD new_opt.
    r = NEW #( kind = c_kind_opt children = VALUE #( ( child ) ) ).
  ENDMETHOD.

  METHOD new_star.
    r = NEW #( kind = c_kind_star children = VALUE #( ( child ) ) ).
  ENDMETHOD.

  METHOD new_plus.
    r = NEW #( kind = c_kind_plus children = VALUE #( ( child ) ) ).
  ENDMETHOD.

  METHOD new_per.
    r = NEW #( kind = c_kind_per children = children ).
  ENDMETHOD.

  METHOD new_vers.
    r = NEW #( kind = c_kind_vers children = VALUE #( ( child ) ) ).
  ENDMETHOD.

  METHOD new_expr.
    r = NEW #( kind = c_kind_expr value = name ).
  ENDMETHOD.

  METHOD list_keywords.
    " 1:1 with combi.ts:
    "   Word.listKeywords()         → [this.s]
    "   WordSequence.listKeywords() → [this.stri]   (full phrase as one entry)
    "   Token / Regex               → []
    "   Sequence/Alt/Opt/Star/Plus/Per/Vers → recurse into children
    "   Expression                  → []  (handled by aggregator separately)
    CASE me->kind.
      WHEN c_kind_word OR c_kind_wseq.
        APPEND me->value TO result.
      WHEN c_kind_token OR c_kind_regex OR c_kind_expr.
        " no keywords contributed
        RETURN.
      WHEN OTHERS.
        LOOP AT me->children INTO DATA(child).
          IF child IS BOUND.
            APPEND LINES OF child->list_keywords( ) TO result.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

CLASS zcl_ace_combi IMPLEMENTATION.

  METHOD str.
    " Mirrors combi.ts: if (s.indexOf(" ") > 0 || s.indexOf("-") > 0) WordSequence else Word
    IF s CS ` ` OR s CS `-`.
      result = zcl_ace_combi_node=>new_wseq( s ).
    ELSE.
      result = zcl_ace_combi_node=>new_word( s ).
    ENDIF.
  ENDMETHOD.

  METHOD tok.
    result = zcl_ace_combi_node=>new_token( token_name ).
  ENDMETHOD.

  METHOD regex.
    result = zcl_ace_combi_node=>new_regex( pattern ).
  ENDMETHOD.

  METHOD seq.
    result = zcl_ace_combi_node=>new_seq( children ).
  ENDMETHOD.

  METHOD alt.
    result = zcl_ace_combi_node=>new_alt( children ).
  ENDMETHOD.

  METHOD opt.
    result = zcl_ace_combi_node=>new_opt( child ).
  ENDMETHOD.

  METHOD star.
    result = zcl_ace_combi_node=>new_star( child ).
  ENDMETHOD.

  METHOD plus.
    result = zcl_ace_combi_node=>new_plus( child ).
  ENDMETHOD.

  METHOD per.
    result = zcl_ace_combi_node=>new_per( children ).
  ENDMETHOD.

  METHOD ver.
    result = zcl_ace_combi_node=>new_vers( child ).
  ENDMETHOD.

  METHOD expr.
    result = zcl_ace_combi_node=>new_expr( name ).
  ENDMETHOD.

ENDCLASS.

CLASS ZCL_ACE_ALV_COMMON IMPLEMENTATION.
  method GET_SELECTED.
      i_obj->get_selected_cells( IMPORTING et_cell = DATA(sel_cells) ).
      IF lines( sel_cells ) > 0.
        e_index = sel_cells[ 1 ]-row_id.
      ELSE.
        i_obj->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
        IF lines( sel_rows ) > 0.
          e_index = sel_rows[ 1 ]-index.
        ENDIF.
      ENDIF.
  endmethod.
  method REFRESH.
      DATA l_stable TYPE lvc_s_stbl.
      l_stable = 'XX'.
      IF i_layout IS SUPPLIED.
        i_obj->set_frontend_layout( i_layout ).
      ENDIF.
      i_obj->refresh_table_display( EXPORTING is_stable = l_stable i_soft_refresh = i_soft ).
  endmethod.
  method TRANSLATE_FIELD.
      DATA: fields_info TYPE TABLE OF dfies.

      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = c_fld-tabname
          fieldname      = c_fld-fieldname
          langu          = i_lang
        TABLES
          dfies_tab      = fields_info
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.

      IF sy-subrc = 0.
        READ TABLE fields_info INDEX 1 INTO DATA(l_info).
        IF l_info-scrtext_l IS INITIAL AND l_info-scrtext_m IS INITIAL AND l_info-scrtext_s IS INITIAL.
          IF l_info-fieldtext IS NOT INITIAL.
            MOVE l_info-fieldtext TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
          ELSE.
            MOVE l_info-fieldname TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
          ENDIF.
        ELSE.
          c_fld-scrtext_l = l_info-scrtext_l.
          c_fld-scrtext_m = l_info-scrtext_m.
          c_fld-scrtext_s = l_info-scrtext_s.
          IF l_info-reptext IS NOT INITIAL.
            c_fld-reptext   = l_info-reptext.
          ENDIF.
        ENDIF.
      ENDIF.
  endmethod.
ENDCLASS.

CLASS ZCL_ACE IMPLEMENTATION.
  METHOD constructor.
    CONSTANTS: c_mask TYPE x VALUE '01'.
    mv_prog = i_prog. mv_package = i_package. mv_show_parse_time = i_show_parse_time. i_step = abap_on.
    zcl_ace_mermaid=>check_mermaid( ). zcl_ace_sel_opt=>init_icons_table( ).
    mo_window = NEW zcl_ace_window( me ). mo_window->mv_new_parser = i_new_parser.
    mo_tree_local = NEW zcl_ace_rtti_tree( i_header = 'Objects & Code Flow' i_type = 'L'
                                           i_cont = mo_window->mo_locals_container i_debugger = me ).
    IF mv_package IS NOT INITIAL.
      show_package( ).
    ELSE.
      show( ).
    ENDIF.
  ENDMETHOD.

  METHOD show_package.
    NEW zcl_ace_tree_builder(
      io_window = mo_window
      io_tree   = mo_tree_local )->build_package( i_package = mv_package ).
  ENDMETHOD.

  METHOD ensure_package_parsed.
    " Parse every object of the package once (used by package-level Class Map)
    CHECK mv_package IS NOT INITIAL AND mv_pkg_parsed = abap_false.
    mt_pkg_objects = NEW zcl_ace_tree_builder(
      io_window = mo_window
      io_tree   = mo_tree_local )->get_package_objects( mv_package ).
    LOOP AT mt_pkg_objects INTO DATA(ls_o).
      TRY.
          mo_window->set_program( CONV #( ls_o-prog ) ).
        CATCH cx_root.
      ENDTRY.
    ENDLOOP.
    mv_pkg_parsed = abap_true.
  ENDMETHOD.
  METHOD mark_active_root.
    DATA lv_tabix TYPE i.
    TYPES: BEGIN OF ts_pair, if_idx TYPE i, end_idx TYPE i, depth TYPE i, END OF ts_pair.
    DATA: lt_pairs TYPE TABLE OF ts_pair WITH EMPTY KEY, ls_pair TYPE ts_pair,
          lt_if_stack TYPE TABLE OF i WITH EMPTY KEY.
    LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<ln>).
      lv_tabix = sy-tabix.
      CASE <ln>-cond.
        WHEN 'IF' OR 'CASE'. APPEND lv_tabix TO lt_if_stack.
        WHEN 'ENDIF' OR 'ENDCASE'.
          IF lt_if_stack IS NOT INITIAL.
            DATA(lv_if_tabix) = lt_if_stack[ lines( lt_if_stack ) ].
            DELETE lt_if_stack INDEX lines( lt_if_stack ).
            ls_pair-if_idx = lv_if_tabix. ls_pair-end_idx = lv_tabix. ls_pair-depth = lines( lt_if_stack ).
            APPEND ls_pair TO lt_pairs.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    SORT lt_pairs BY depth DESCENDING if_idx ASCENDING.
    TYPES: BEGIN OF ts_branch_hdr, tabix TYPE i, cond TYPE string, END OF ts_branch_hdr.
    DATA: lt_hdrs TYPE TABLE OF ts_branch_hdr WITH EMPTY KEY, ls_hdr TYPE ts_branch_hdr,
          lv_if_i TYPE i, lv_end_i TYPE i, lv_inner_depth TYPE i, lv_i TYPE i,
          lv_block_active TYPE flag, lv_hdr_cnt TYPE i, lv_h TYPE i,
          lv_branch_active TYPE flag, lv_scan TYPE i, lv_scan_inner TYPE i.
    LOOP AT lt_pairs INTO ls_pair.
      lv_if_i = ls_pair-if_idx. lv_end_i = ls_pair-end_idx. CLEAR: lt_hdrs, lv_inner_depth.
      lv_i = lv_if_i.
      WHILE lv_i <= lv_end_i.
        READ TABLE ct_results INDEX lv_i ASSIGNING <ln>. IF sy-subrc <> 0. EXIT. ENDIF.
        CASE <ln>-cond.
          WHEN 'IF' OR 'CASE'.
            IF lv_i = lv_if_i. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs.
            ELSE. lv_inner_depth = lv_inner_depth + 1. ENDIF.
          WHEN 'ENDIF' OR 'ENDCASE'.
            IF lv_i = lv_end_i. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs.
            ELSE. lv_inner_depth = lv_inner_depth - 1. ENDIF.
          WHEN 'ELSEIF' OR 'ELSE' OR 'WHEN'.
            IF lv_inner_depth = 0. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs. ENDIF.
        ENDCASE.
        lv_i = lv_i + 1.
      ENDWHILE.
      CLEAR lv_block_active. lv_hdr_cnt = lines( lt_hdrs ). lv_h = 1.
      WHILE lv_h < lv_hdr_cnt.
        DATA(ls_hdr_cur) = lt_hdrs[ lv_h ]. DATA(ls_hdr_next) = lt_hdrs[ lv_h + 1 ].
        CLEAR: lv_branch_active, lv_scan_inner. lv_scan = ls_hdr_cur-tabix + 1.
        WHILE lv_scan < ls_hdr_next-tabix.
          READ TABLE ct_results INDEX lv_scan ASSIGNING FIELD-SYMBOL(<scan_ln>). IF sy-subrc <> 0. EXIT. ENDIF.
          CASE <scan_ln>-cond.
            WHEN 'IF' OR 'CASE'. lv_scan_inner = lv_scan_inner + 1.
            WHEN 'ENDIF' OR 'ENDCASE'. lv_scan_inner = lv_scan_inner - 1.
            WHEN OTHERS. IF lv_scan_inner = 0 AND <scan_ln>-active_root = abap_true. lv_branch_active = abap_true. ENDIF.
          ENDCASE.
          IF lv_branch_active = abap_true. EXIT. ENDIF.
          lv_scan = lv_scan + 1.
        ENDWHILE.
        ASSIGN ct_results[ ls_hdr_cur-tabix ] TO FIELD-SYMBOL(<hdr_ln>).
        IF sy-subrc = 0.
          IF lv_branch_active = abap_true. <hdr_ln>-active_root = abap_true. lv_block_active = abap_true.
          ELSE. CLEAR <hdr_ln>-active_root. ENDIF.
        ENDIF.
        lv_h = lv_h + 1.
      ENDWHILE.
      ASSIGN ct_results[ lv_end_i ] TO FIELD-SYMBOL(<endif_ln>).
      IF sy-subrc = 0. <endif_ln>-active_root = lv_block_active. ENDIF.
      ASSIGN ct_results[ lv_if_i ] TO FIELD-SYMBOL(<if_ln>).
      IF sy-subrc = 0.
        IF lv_block_active = abap_false. CLEAR <if_ln>-active_root.
        ELSE. <if_ln>-active_root = abap_true. ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
METHOD get_code_flow.
    " 0. Parse selected call if needed
    parse_sel_call( ).

    " 1. Prepare steps: deduplicate and sort
    DATA(steps) = CONV tt_steps( mt_steps ).
    SORT steps BY line eventtype eventname.
    DELETE ADJACENT DUPLICATES FROM steps.
    SORT steps BY step.

    " 2. Expand variable filter forward (bindings propagation)
    DATA(lt_selected_var) = CONV tt_sel_var( mt_selected_var ).
    expand_selected_vars_forward(
      EXPORTING it_steps        = steps
      CHANGING  ct_selected_var = lt_selected_var ).

    " 3. Remove empty block pairs (DO/ENDDO, LOOP/ENDLOOP etc. with nothing inside)
    remove_empty_block_pairs( CHANGING ct_steps = steps ).

    " 4. Propagate variables backward through the sorted-descending steps
    propagate_vars_backward(
      EXPORTING it_steps        = steps
      CHANGING  ct_selected_var = lt_selected_var ).

    SORT lt_selected_var. DELETE ADJACENT DUPLICATES FROM lt_selected_var.
    CLEAR mo_window->mt_coverage.

    " 5. Build result lines
    DATA(lv_no_filter) = xsdbool( mt_selected_var IS INITIAL AND i_calc_path = abap_false ).
    build_result_lines(
      EXPORTING it_steps        = steps
                iv_no_filter    = lv_no_filter
                it_selected_var = lt_selected_var
      CHANGING  ct_results      = results ).

    " 6. Remove empty loop pairs from results
    remove_empty_loop_pairs( CHANGING ct_results = results ).

    " 7. Enrich result lines (code text, arrows, subnames, if/else indices)
    enrich_result_lines( CHANGING ct_results = results ).

    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
      INSERT ms_if INTO mt_if INDEX 1.
    ENDIF.
    IF lines( results ) > 0.
      IF results[ lines( results ) ]-arrow IS NOT INITIAL.
        CLEAR results[ lines( results ) ]-arrow.
      ENDIF.
    ENDIF.

    " 8. Mark active_root (IF/CASE branch highlighting)
    mark_active_root( EXPORTING i_calc_path = i_calc_path CHANGING ct_results = results ).

    " 9. Apply calc_path filter if requested
    IF i_calc_path = abap_true.
      apply_calc_path_filter( CHANGING ct_results = results ).
    ENDIF.
  ENDMETHOD.
  METHOD get_code_mix.
    DATA: flow_lines TYPE sci_include, splits TYPE TABLE OF string, ind TYPE i, prev_flow TYPE ts_line.
    DATA(lines) = get_code_flow( i_calc_path = i_calc_path ).
    READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = 'Code_Flow_Mix' ASSIGNING FIELD-SYMBOL(<prog_mix>).
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mo_window->ms_sources-tt_progs ASSIGNING <prog_mix>.
      INSERT INITIAL LINE INTO mo_window->mt_stack INDEX 1 ASSIGNING FIELD-SYMBOL(<stack_mix>).
      <prog_mix>-include = <stack_mix>-program = <stack_mix>-include = 'Code_Flow_Mix'.
    ENDIF.
    LOOP AT lines INTO DATA(flow_line).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = flow_line-include INTO DATA(prog).
      READ TABLE prog-t_keywords WITH KEY line = flow_line-line INTO DATA(keyword).
      APPEND INITIAL LINE TO <prog_mix>-t_keywords ASSIGNING FIELD-SYMBOL(<keyword_mix>).
      <keyword_mix> = keyword. <keyword_mix>-include = flow_line-include. <keyword_mix>-program = flow_line-program.
      DATA(from_row) = prog-scan->tokens[ keyword-from ]-row.
      DATA(to_row)   = prog-scan->tokens[ keyword-to   ]-row.
      DATA(spaces)   = repeat( val = | | occ = ( flow_line-stack - 1 ) * 3 ).
      DATA(dashes)   = repeat( val = |-| occ = ( flow_line-stack ) ).
      IF prev_flow-ev_name <> flow_line-ev_name OR prev_flow-ev_type <> flow_line-ev_type
      OR prev_flow-class   <> flow_line-class   OR prev_flow-stack   <> flow_line-stack.
        SPLIT flow_line-include AT '=' INTO TABLE splits.
        APPEND INITIAL LINE TO flow_lines ASSIGNING FIELD-SYMBOL(<flow>).
        ind = sy-tabix.
        IF flow_line-class IS INITIAL.
          <flow> = |"{ dashes } { flow_line-ev_type } { flow_line-ev_name } in { splits[ 1 ] }|.
        ELSE.
          <flow> = |"{ dashes } { flow_line-ev_type } { flow_line-ev_name } in { flow_line-class }|.
        ENDIF.
      ENDIF.
      <keyword_mix>-v_line = ind + 1.
      LOOP AT prog-source_tab FROM from_row TO to_row INTO DATA(source_line).
        APPEND INITIAL LINE TO flow_lines ASSIGNING <flow>. ind = sy-tabix. <flow> = |{ spaces }{ source_line }|.
      ENDLOOP.
      prev_flow = flow_line.
    ENDLOOP.
    mo_window->mo_code_viewer->set_text( table = flow_lines ). <prog_mix>-source_tab = flow_lines.
    mo_window->m_prg-include = 'Code_Flow_Mix'. mo_window->set_mixprog_line( ). mo_window->show_stack( ).
    mo_window->mo_box->set_caption( |Code Mix: { lines( lines ) } statements| ).
  ENDMETHOD.
METHOD show.
    IF mo_window->m_prg-include IS INITIAL. mo_window->m_prg-program = mo_window->m_prg-include = mv_prog. ENDIF.
    mo_window->set_program( mo_window->m_prg-include ).
    IF mo_window->m_prg-include <> 'Code_Flow_Mix'. mo_window->show_coverage( ). ENDIF.
    IF mo_window->m_prg-line IS INITIAL AND mo_window->mt_stack IS NOT INITIAL.
      mo_window->m_prg-line = mo_window->mt_stack[ 1 ]-line.
    ENDIF.
    mo_window->set_program_line( 1 ).
    DELETE mo_window->ms_sources-tt_progs WHERE t_keywords IS INITIAL.
    mo_window->show_stack( ).

    NEW zcl_ace_tree_builder(
  io_window = mo_window
  io_tree   = mo_tree_local )->build( ).
    "test
  ENDMETHOD.
METHOD apply_calc_path_filter.
    DATA lt_active_ev TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<line>) WHERE active_root = abap_true.
      READ TABLE lt_active_ev WITH KEY table_line = <line>-ev_name TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0. INSERT <line>-ev_name INTO TABLE lt_active_ev. ENDIF.
    ENDLOOP.
    DATA lt_active_sub TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    LOOP AT lt_active_ev INTO DATA(lv_ev).
      READ TABLE lt_active_sub WITH KEY table_line = lv_ev TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0. INSERT lv_ev INTO TABLE lt_active_sub. ENDIF.
    ENDLOOP.
    DATA lv_expanded TYPE boolean.
    DO.
      CLEAR lv_expanded.
      LOOP AT ct_results ASSIGNING <line>
        WHERE subname IS NOT INITIAL AND active_root IS INITIAL.
        READ TABLE lt_active_sub WITH KEY table_line = <line>-subname TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          READ TABLE lt_active_sub WITH KEY table_line = <line>-ev_name TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            INSERT <line>-ev_name INTO TABLE lt_active_sub. lv_expanded = abap_true.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_expanded = abap_false. EXIT. ENDIF.
    ENDDO.
    LOOP AT ct_results ASSIGNING <line> WHERE active_root IS INITIAL.
      IF <line>-subname IS NOT INITIAL.
        READ TABLE lt_active_sub WITH KEY table_line = <line>-subname TRANSPORTING NO FIELDS.
        IF sy-subrc = 0. CONTINUE. ENDIF.
      ENDIF.
      IF <line>-cond IS NOT INITIAL.
        READ TABLE lt_active_ev WITH KEY table_line = <line>-ev_name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0. CONTINUE. ENDIF.
      ENDIF.
      <line>-del = abap_true.
    ENDLOOP.
    DELETE ct_results WHERE del = abap_true.
    LOOP AT ct_results ASSIGNING <line>. <line>-ind = sy-tabix. ENDLOOP.
  ENDMETHOD.
METHOD build_result_lines.
    DATA: line TYPE ts_line, inserted TYPE boolean,
          prog TYPE LINE OF zif_ace_parse_data=>tt_progs,
          key TYPE ts_kword, ind TYPE i.
    LOOP AT it_steps INTO DATA(step).
      CLEAR inserted.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO key.
      CLEAR line-cond.
      IF key-name = 'IF'    OR key-name = 'ELSE'    OR key-name = 'ENDIF'   OR
         key-name = 'ELSEIF' OR key-name = 'CASE'   OR key-name = 'WHEN'    OR
         key-name = 'ENDCASE' OR key-name = 'DO'    OR key-name = 'ENDDO'   OR
         key-name = 'LOOP'  OR key-name = 'ENDLOOP' OR
         key-name = 'WHILE' OR key-name = 'ENDWHILE'.
        APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).
        <watch>-program = step-program. <watch>-line = line-line = step-line.
        INSERT line INTO ct_results INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-cond    = key-name.          <line>-ev_name  = step-eventname.
        <line>-stack   = step-stacklevel.   <line>-include  = step-include.
        <line>-class   = step-class.        inserted = abap_true.
      ENDIF.
      CLEAR ind.
      LOOP AT mo_window->ms_sources-t_calculated INTO DATA(calc_var)
          WHERE include = step-include
            AND class = step-class AND eventtype = step-eventtype AND eventname = step-eventname
            AND line = step-line.
        ADD 1 TO ind.
        LOOP AT mo_window->ms_sources-t_composed INTO DATA(comp_var)
            WHERE include = step-include
              AND class = step-class AND eventtype = step-eventtype AND eventname = step-eventname
              AND line = step-line.
          READ TABLE it_selected_var WITH KEY name = comp_var-name TRANSPORTING NO FIELDS.
          " composed var already in filter - no action needed here (filter is read-only in this method)
        ENDLOOP.
        READ TABLE it_selected_var WITH KEY name = calc_var-name
          class = calc_var-class eventtype = calc_var-eventtype eventname = calc_var-eventname
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          READ TABLE it_selected_var WITH KEY name = calc_var-name
            class = '' eventtype = '' eventname = ''
            TRANSPORTING NO FIELDS.
        ENDIF.
        IF sy-subrc = 0 OR iv_no_filter = abap_true.
          APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING <watch>.
          <watch>-program = step-program. <watch>-line = line-line = step-line.
          IF ind = 1.
            IF key-name <> 'PUBLIC'    AND key-name <> 'ENDCLASS'  AND
               key-name <> 'ENDFORM'   AND key-name <> 'FORM'      AND
               key-name <> 'METHOD'    AND key-name <> 'METHODS'   AND
               key-name <> 'ENDMETHOD' AND key-name <> 'MODULE'    AND
               inserted = abap_false.
              line-ev_name     = step-eventname.    line-stack  = step-stacklevel.
              line-include     = step-include.      line-class  = step-class.
              line-ev_type     = step-eventtype.    line-active_root = abap_true.
              INSERT line INTO ct_results INDEX 1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF inserted = abap_false.
        IF key-name <> 'PUBLIC'    AND key-name <> 'ENDCLASS'  AND
           key-name <> 'ENDFORM'   AND key-name <> 'ENDMETHOD' AND
           key-name <> 'METHOD'    AND key-name <> 'METHODS'   AND
           key-name <> 'MODULE'    AND key-name <> 'FORM'.
          READ TABLE ct_results WITH KEY line     = step-line
                                         include  = step-include
                                         ev_type  = step-eventtype
                                         ev_name  = step-eventname
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            line-line        = step-line.       line-ev_name  = step-eventname.
            line-stack       = step-stacklevel. line-include  = step-include.
            line-ev_type     = step-eventtype.  line-class    = step-class.
            line-active_root = abap_false.
            INSERT line INTO ct_results INDEX 1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE ct_results WHERE del = abap_true.
  ENDMETHOD.
METHOD enrich_result_lines.
    DATA: prog      TYPE LINE OF zif_ace_parse_data=>tt_progs,
          keyword   TYPE ts_kword,
          call      TYPE ts_calls,
          els_before TYPE i,
          if_depth   TYPE i,
          when_count TYPE i,
          line_tmp   TYPE ts_line,
          counter    TYPE i.
    LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<line>).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = <line>-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = <line>-line INTO keyword.
      LOOP AT prog-scan->tokens FROM keyword-from TO keyword-to INTO DATA(token).
        IF token-str = 'USING' OR token-str = 'EXPORTING' OR
           token-str = 'IMPORTING' OR token-str = 'CHANGING'. EXIT. ENDIF.
        IF <line>-code IS INITIAL. <line>-code = token-str.
        ELSE. <line>-code = |{ <line>-code } { token-str }|. ENDIF.
      ENDLOOP.
      IF keyword-tt_calls IS NOT INITIAL.
        DATA(lv_arrow_cnt) = 0.
        LOOP AT keyword-tt_calls INTO call.
          IF <line>-subname IS INITIAL.
            <line>-subname = call-name.
            REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
            REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
          ENDIF.
          REPLACE ALL OCCURRENCES OF '"' IN <line>-code WITH ''.
          LOOP AT call-bindings INTO DATA(ls_b).
            CHECK ls_b-outer IS NOT INITIAL OR ls_b-inner IS NOT INITIAL.
            IF lv_arrow_cnt > 0. <line>-arrow = |{ <line>-arrow }, |. ENDIF.
            DATA(lv_sep) = SWITCH string( ls_b-dir
              WHEN 'I' THEN '->'    WHEN 'E' THEN '<-'
              WHEN 'C' THEN '-> <-' ELSE '--' ).
            <line>-arrow = |{ <line>-arrow } { ls_b-outer } { lv_sep } { ls_b-inner }|.
            lv_arrow_cnt = lv_arrow_cnt + 1.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
      REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-arrow   WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-arrow   WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
    ENDLOOP.
    LOOP AT ct_results ASSIGNING <line>. <line>-ind = sy-tabix. ENDLOOP.
    LOOP AT ct_results ASSIGNING <line>
      WHERE code <> 'DO'   AND code <> 'ENDDO'   AND code <> 'WHILE' AND
            code <> 'ENDWHILE' AND code <> 'LOOP' AND code <> 'ENDLOOP'.
      FIELD-SYMBOLS: <if> TYPE ts_if.
      IF <line>-cond = 'IF' OR <line>-cond = 'CASE'.
        ADD 1 TO if_depth. CLEAR when_count.
        APPEND INITIAL LINE TO mt_if ASSIGNING <if>. <if>-if_ind = <line>-ind.
      ENDIF.
      IF <line>-cond = 'ENDIF' OR <line>-cond = 'ENDCASE'.
        IF <if> IS ASSIGNED.
          <if>-end_ind = <line>-ind. SUBTRACT 1 FROM if_depth.
          LOOP AT mt_if ASSIGNING <if> WHERE end_ind = 0. ENDLOOP.
        ENDIF.
      ENDIF.
      IF <line>-cond = 'WHEN'. ADD 1 TO when_count. ENDIF.
      IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.
        <line>-els_before = els_before. <line>-els_after = <line>-ind.
        counter = <line>-ind + 1.
        DO.
          READ TABLE ct_results INDEX counter INTO line_tmp.
          IF sy-subrc <> 0. CLEAR <line>-els_after. EXIT. ENDIF.
          IF line_tmp-cond = 'ELSE' OR line_tmp-cond = 'ELSEIF'.
            CLEAR <line>-els_after. EXIT.
          ELSEIF line_tmp-cond <> 'DO'      AND line_tmp-cond <> 'ENDDO'   AND
                 line_tmp-cond <> 'WHILE'   AND line_tmp-cond <> 'ENDWHILE' AND
                 line_tmp-cond <> 'LOOP'    AND line_tmp-cond <> 'ENDLOOP'.
            <line>-els_after = counter. EXIT.
          ELSE. ADD 1 TO counter. ENDIF.
        ENDDO.
      ENDIF.
      IF <line>-cond = 'WHEN'.
        <line>-els_before = els_before. <line>-els_after = <line>-ind.
        counter = <line>-ind + 1.
        DO.
          READ TABLE ct_results INDEX counter INTO line_tmp.
          IF sy-subrc <> 0. CLEAR <line>-els_after. EXIT. ENDIF.
          IF line_tmp-cond = 'WHEN'. CLEAR <line>-els_after. EXIT.
          ELSEIF line_tmp-cond <> 'DO'      AND line_tmp-cond <> 'ENDDO'   AND
                 line_tmp-cond <> 'WHILE'   AND line_tmp-cond <> 'ENDWHILE' AND
                 line_tmp-cond <> 'LOOP'    AND line_tmp-cond <> 'ENDLOOP'.
            <line>-els_after = counter. EXIT.
          ELSE. ADD 1 TO counter. ENDIF.
        ENDDO.
        IF when_count = 1.
          IF <if> IS ASSIGNED. <if>-if_ind = els_before. ENDIF.
          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.
      IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
        els_before = <line>-ind.
      ELSE. CLEAR els_before. ENDIF.
    ENDLOOP.
    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
      INSERT ms_if INTO mt_if INDEX 1.
    ENDIF.
    IF lines( ct_results ) > 0.
      IF ct_results[ lines( ct_results ) ]-arrow IS NOT INITIAL.
        CLEAR ct_results[ lines( ct_results ) ]-arrow.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD expand_selected_vars_forward.
    DATA: keyword TYPE ts_kword, prog TYPE LINE OF zif_ace_parse_data=>tt_progs.
    DATA yes TYPE xfeld.
    LOOP AT it_steps INTO DATA(step).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.

      " Check if any binding in this step matches ct_selected_var (or no filter at all)
      CLEAR yes.
      LOOP AT keyword-tt_calls INTO DATA(call).
        LOOP AT call-bindings INTO DATA(ls_b_chk).
          READ TABLE ct_selected_var WITH KEY name = ls_b_chk-outer TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.
            yes = abap_true.
          ENDIF.
          READ TABLE ct_selected_var WITH KEY name = ls_b_chk-inner TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.
            yes = abap_true.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      CHECK yes = abap_true.

      LOOP AT keyword-tt_calls INTO call.
        LOOP AT call-bindings INTO DATA(ls_b_add).
          IF mt_selected_var IS INITIAL.
            " No filter - add both outer and inner with uniqueness check
            READ TABLE ct_selected_var WITH KEY name = ls_b_add-outer TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO ct_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
              <sel>-name = ls_b_add-outer.
            ENDIF.
            READ TABLE ct_selected_var WITH KEY name = ls_b_add-inner TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO ct_selected_var ASSIGNING <sel>.
              <sel>-name = ls_b_add-inner.
            ENDIF.
          ELSE.
            " Filter active - add only the opposite side of the matched binding
            READ TABLE ct_selected_var WITH KEY name = ls_b_add-outer TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              " outer matched - add inner
              READ TABLE ct_selected_var WITH KEY name = ls_b_add-inner TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                APPEND INITIAL LINE TO ct_selected_var ASSIGNING FIELD-SYMBOL(<sel_i>).
                <sel_i>-name = ls_b_add-inner.
              ENDIF.
            ENDIF.
            READ TABLE ct_selected_var WITH KEY name = ls_b_add-inner TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              " inner matched - add outer
              READ TABLE ct_selected_var WITH KEY name = ls_b_add-outer TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                APPEND INITIAL LINE TO ct_selected_var ASSIGNING FIELD-SYMBOL(<sel_o>).
                <sel_o>-name = ls_b_add-outer.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
METHOD parse_sel_call.
    CHECK mo_window->ms_sel_call IS NOT INITIAL.
    CLEAR: mt_steps, mo_window->mt_calls.
    IF mo_window->ms_sel_call-eventtype = 'FORM'.
      zcl_ace_source_parser=>parse_call_form(
        EXPORTING i_call_name = mo_window->ms_sel_call-eventname
                  i_program   = mo_window->ms_sel_call-program
                  i_include   = mo_window->ms_sel_call-include
                  i_stack     = 0
                  io_debugger = mo_window->mo_viewer ).
    ELSE.
      zcl_ace_source_parser=>parse_call(
        EXPORTING i_index     = mo_window->ms_sel_call-index
                  i_e_name    = mo_window->ms_sel_call-eventname
                  i_e_type    = mo_window->ms_sel_call-eventtype
                  i_program   = mo_window->ms_sel_call-program
                  i_include   = mo_window->ms_sel_call-include
                  i_class     = mo_window->ms_sel_call-class
                  i_stack     = 0
                  io_debugger = mo_window->mo_viewer ).
    ENDIF.
  ENDMETHOD.
METHOD propagate_vars_backward.
    DATA: prog TYPE LINE OF zif_ace_parse_data=>tt_progs, keyword TYPE ts_kword, ind TYPE i.
    LOOP AT it_steps INTO DATA(step).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      LOOP AT mo_window->ms_sources-t_calculated INTO DATA(calc_var)
          WHERE include = step-include
            AND class = step-class AND eventtype = step-eventtype AND eventname = step-eventname
            AND line = step-line.
        ADD 1 TO ind.
        READ TABLE ct_selected_var WITH KEY name = calc_var-name
          class = calc_var-class eventtype = calc_var-eventtype eventname = calc_var-eventname
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          READ TABLE ct_selected_var WITH KEY name = calc_var-name
            class = '' eventtype = '' eventname = ''
            TRANSPORTING NO FIELDS.
        ENDIF.
        IF sy-subrc = 0.
          " calc_var found in ct_selected_var - add all composed vars of this line
          LOOP AT mo_window->ms_sources-t_composed INTO DATA(comp_var)
              WHERE include = step-include
                AND class = step-class  AND eventtype = step-eventtype AND eventname = step-eventname
                AND line = step-line.
            READ TABLE ct_selected_var WITH KEY name = comp_var-name
              class = comp_var-class eventtype = comp_var-eventtype eventname = comp_var-eventname
              TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO ct_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
              <sel>-name      = comp_var-name.
              <sel>-class     = comp_var-class.
              <sel>-eventtype = comp_var-eventtype.
              <sel>-eventname = comp_var-eventname.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.
      LOOP AT keyword-tt_calls INTO DATA(call).
        LOOP AT call-bindings INTO DATA(b_prop).
          " backward: inner (parameter) found - add outer (caller variable)
          READ TABLE ct_selected_var WITH KEY name = b_prop-inner TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            READ TABLE ct_selected_var WITH KEY name = b_prop-outer TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO ct_selected_var ASSIGNING <sel>.
              <sel>-name = b_prop-outer.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
METHOD remove_empty_block_pairs.
    DATA: prev TYPE t_step_counter, pre_key TYPE string,
          prog TYPE LINE OF zif_ace_parse_data=>tt_progs.
    LOOP AT ct_steps ASSIGNING FIELD-SYMBOL(<step>).
      DATA(ind) = sy-tabix.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = <step>-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = <step>-line INTO DATA(key).
      IF prev IS NOT INITIAL.
        IF ( key-name = 'ENDDO'   OR key-name = 'ENDWHILE' OR
             key-name = 'ENDLOOP' OR key-name = 'ENDIF' ) AND
           ( pre_key  = 'DO'      OR pre_key  = 'LOOP'    OR
             pre_key  = 'WHILE'   OR pre_key  = 'IF' ).
          <step>-first = 'D'.
          READ TABLE ct_steps INDEX ind - 1 ASSIGNING FIELD-SYMBOL(<prev_step>).
          <prev_step>-first = 'D'.
        ENDIF.
      ENDIF.
      prev = <step>. pre_key = key-name.
    ENDLOOP.
    DELETE ct_steps WHERE first = 'D'.
    SORT ct_steps BY step DESCENDING.
  ENDMETHOD.
METHOD remove_empty_loop_pairs.
    DATA lv_changed TYPE boolean.
    DO.
      CLEAR lv_changed.
      LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<line>).
        DATA(lv_ti) = sy-tabix.
        IF <line>-cond = 'LOOP' OR <line>-cond = 'DO' OR <line>-cond = 'WHILE'.
          READ TABLE ct_results INDEX lv_ti + 1 ASSIGNING FIELD-SYMBOL(<next>).
          IF sy-subrc = 0.
            IF ( <line>-cond = 'LOOP'  AND <next>-cond = 'ENDLOOP'  ) OR
               ( <line>-cond = 'DO'    AND <next>-cond = 'ENDDO'    ) OR
               ( <line>-cond = 'WHILE' AND <next>-cond = 'ENDWHILE' ).
              DELETE ct_results INDEX lv_ti + 1.
              DELETE ct_results INDEX lv_ti.
              lv_changed = abap_true. EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_changed = abap_false. EXIT. ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

  " & Multi-windows program for ABAP code analysis
  " &----------------------------------------------------------------------
  " & version: beta 0.5
  " & Git https://github.com/ysichov/ACE
  " & Written by Yurii Sychov
  " & e-mail:   ysichov@gmail.com
  " & blog:     https://ysychov.wordpress.com/blog/
  " & LinkedIn: https://www.linkedin.com/in/ysychov/
  " &----------------------------------------------------------------------

  " & External resources
  " & https://github.com/WegnerDan/abapMermaid
  " & https://github.com/oisee/vibing-steampunk

  " Main input fields for selecting the source object
  SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE TEXT-004.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT (29) TEXT-002 FOR FIELD p_prog.
      SELECTION-SCREEN POSITION 33.
      PARAMETERS: p_prog  TYPE progname MATCHCODE OBJECT progname MODIF ID prg.
      SELECTION-SCREEN COMMENT (70) TEXT-001 FOR FIELD p_prog.
    SELECTION-SCREEN END OF LINE.
    PARAMETERS: p_pack  TYPE devclass.
    PARAMETERS: p_class  TYPE seoclsname MATCHCODE OBJECT sfbeclname.
    PARAMETERS: p_func  TYPE seoclsname MATCHCODE OBJECT cacs_function.
    PARAMETERS: p_odata  TYPE seoclsname MATCHCODE OBJECT /iwbep/sh_sbdm_project.
    PARAMETERS: p_wdc  TYPE string.
  SELECTION-SCREEN END OF BLOCK s1.

  PARAMETERS: n_parser NO-DISPLAY. "AS CHECKBOX DEFAULT ' '.
  PARAMETERS: n_time NO-DISPLAY . "AS CHECKBOX DEFAULT ' ' .

  SELECTION-SCREEN SKIP.

  INITIALIZATION.

    PERFORM supress_button. "supressing F8 button
    DATA itab TYPE TABLE OF sy-ucomm.

    APPEND: 'ONLI' TO itab.
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = itab.
  " Resolve class name to generated class/interface program
  AT SELECTION-SCREEN ON p_class.

    IF p_class IS NOT INITIAL.
      SELECT SINGLE clstype
        FROM seoclass
       WHERE clsname = @p_class
        INTO @DATA(clstype).
      IF sy-subrc = 0.

        p_prog = p_class && repeat( val = `=` occ = 30 - strlen( p_class ) ).
        IF clstype = '1'.
          p_prog = p_prog && 'IP'.
        ELSE.
          p_prog = p_prog && 'CP'.
        ENDIF.
      ENDIF.

    ENDIF.

  " Resolve OData service or WDC component to backend class
  AT SELECTION-SCREEN ON p_odata.

    IF p_odata IS NOT INITIAL.
      DATA(serv) = p_odata && '_SRV'.

      SELECT SINGLE class_name INTO p_class
        FROM /iwbep/i_mgw_srh WHERE technical_name = serv.
    ENDIF.

    IF p_wdc IS NOT INITIAL.
      p_class = cl_wdy_wb_naming_service=>get_classname_for_component( p_component = CONV #( p_wdc ) ).
    ENDIF.

  " Resolve function module to generated include program
  AT SELECTION-SCREEN ON p_func.

    IF p_func IS NOT INITIAL.
      SELECT SINGLE pname, include
        FROM tfdir
       WHERE funcname = @p_func
        INTO ( @DATA(func_incl), @DATA(incl_num) ).

      IF sy-subrc = 0.
        SHIFT func_incl LEFT BY 3 PLACES.
        p_prog = func_incl && 'U' && incl_num.
      ENDIF.

    ENDIF.

  " Trigger ACE execution after selection-screen validation
  AT SELECTION-SCREEN.

    CHECK sy-ucomm <> 'DUMMY'.
    PERFORM run_ace.

  " Provide F4 help for custom Web Dynpro components
  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_wdc.

    DATA: gt_tab TYPE TABLE OF rseui_f4.
    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
      EXPORTING
        object_type      = 'YC'
        object_name      = 'Z*'
      TABLES
        objects_selected = gt_tab
      EXCEPTIONS
        cancel           = 1
        wrong_type       = 2
        OTHERS           = 3.

    IF sy-subrc = 0.
      p_wdc = gt_tab[ 1 ]-obj_name.

    ENDIF.

  FORM supress_button. "supressing F8 button

    DATA itab TYPE TABLE OF sy-ucomm.

    APPEND: 'ONLI' TO itab.
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = itab.
  ENDFORM.

  " Run ACE only when target program exists in repository
  FORM run_ace.

    CHECK sy-ucomm IS INITIAL.

    " Package mode: parse all objects found in the package
    IF p_pack IS NOT INITIAL.
      DATA(gv_ace_pkg) = NEW zcl_ace( i_package = p_pack i_new_parser = n_parser i_show_parse_time = n_time ).
      RETURN.
    ENDIF.

    SELECT COUNT( * ) FROM reposrc WHERE progname = p_prog.

    IF sy-dbcnt <> 0.
      DATA(gv_ace) = NEW zcl_ace( i_prog = p_prog i_new_parser = n_parser i_show_parse_time = n_time ).
    ELSE.
      MESSAGE 'Program is not found' TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.

  ENDFORM.

****************************************************
INTERFACE lif_abapmerge_marker.
* abapmerge 0.16.7 - 2026-07-19T10:49:01.034Z
  CONSTANTS c_merge_timestamp TYPE string VALUE `2026-07-19T10:49:01.034Z`.
  CONSTANTS c_abapmerge_version TYPE string VALUE `0.16.7`.
ENDINTERFACE.
****************************************************
