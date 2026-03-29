  REPORT z_ace. " ACE - Abap Code Explorer
INTERFACE zif_ace_stmt_handler DEFERRED.
INTERFACE zif_ace_parse_data DEFERRED.
CLASS zcl_ace_window DEFINITION DEFERRED.
CLASS zcl_ace_text_viewer DEFINITION DEFERRED.
CLASS zcl_ace_table_viewer DEFINITION DEFERRED.
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
      line      TYPE i,
      name(100) TYPE c,
      type      TYPE string,
    END OF ts_var .
  TYPES:
    tt_calculated TYPE STANDARD TABLE OF ts_var WITH KEY program include line name .
  TYPES:
    tt_composed   TYPE STANDARD TABLE OF ts_var WITH KEY program include line name .

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
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
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
    TYPES:
      BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string,
        high        TYPE string,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row .
    TYPES:
      BEGIN OF sign_option_icon_s,
        sign          TYPE tvarv_sign,
        option        TYPE tvarv_opti,
        icon_name(64) TYPE c,
        icon          TYPE aqadh_type_of_icon,
      END OF sign_option_icon_s .

    TYPES:
      BEGIN OF t_obj,
        name       TYPE string,
        alv_viewer TYPE REF TO zcl_ace_table_viewer,
      END OF t_obj .
    TYPES:
      BEGIN OF t_popup,
        parent TYPE REF TO cl_gui_dialogbox_container,
        child  TYPE REF TO cl_gui_dialogbox_container,
      END OF t_popup .
    TYPES:
      BEGIN OF t_classes_types,
        name TYPE string,
        full TYPE string,
        type TYPE char1,
        key  TYPE salv_de_node_key,
      END OF t_classes_types .
    TYPES:
      BEGIN OF t_lang,
        spras(4),
        sptxt    TYPE sptxt,
      END OF t_lang .
    TYPES:
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
    TYPES:
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

    " Aligned with zif_ace_parse_data=>ts_param_binding (dir added)
    TYPES:
      BEGIN OF ts_param_binding,
        outer TYPE string,
        inner TYPE string,
        dir   TYPE char1,
      END OF ts_param_binding .
    TYPES:
      tt_param_bindings TYPE STANDARD TABLE OF ts_param_binding WITH EMPTY KEY .

    TYPES:
      ts_calls TYPE zif_ace_parse_data=>ts_calls .
    TYPES:
      tt_calls TYPE zif_ace_parse_data=>tt_calls .
    TYPES:
      ts_kword TYPE zif_ace_parse_data=>ts_kword .
    TYPES:
      tt_kword TYPE zif_ace_parse_data=>tt_kword .
    TYPES:
      ts_calls_line TYPE zif_ace_parse_data=>ts_calls_line .
    TYPES:
      tt_calls_line TYPE zif_ace_parse_data=>tt_calls_line .
    TYPES:
      ts_vars TYPE zif_ace_parse_data=>ts_vars .
    TYPES:
      BEGIN OF ts_var,
        program   TYPE string,
        include   TYPE string,
        line      TYPE i,
        name(100) TYPE c,
        type      TYPE string,
      END OF ts_var .
    TYPES:
      tt_calculated TYPE STANDARD TABLE OF ts_var WITH KEY program include line name .
    TYPES:
      tt_composed   TYPE STANDARD TABLE OF ts_var WITH KEY program include line name .
    TYPES:
      BEGIN OF ts_int_tabs,
        eventtype TYPE string,
        eventname TYPE string,
        name      TYPE string,
        type      TYPE string,
      END OF ts_int_tabs .
    TYPES:
      tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY .
    TYPES:
      ts_params TYPE zif_ace_parse_data=>ts_params .
    TYPES:
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
    TYPES:
      BEGIN OF ts_tree,
        kind(1),
        value    TYPE string,
        param    TYPE string,
        program  TYPE program,
        include  TYPE program,
        ev_type  TYPE string,
        ev_name  TYPE string,
        enh_id   TYPE i,
        var_name TYPE string,
      END OF ts_tree .
    TYPES:
      BEGIN OF ts_call,
        include TYPE string,
        ev_name TYPE string,
        class   TYPE string,
      END OF ts_call .
    TYPES:
      BEGIN OF t_sel_var,
        name   TYPE string,
        i_sel  TYPE boolean,
        refval TYPE REF TO data,
      END OF t_sel_var .
    TYPES:
      BEGIN OF ts_if,
        if_ind      TYPE i,
        end_ind     TYPE i,
        before_else TYPE i,
      END OF ts_if .
    TYPES:
      tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY .
    TYPES:
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
    TYPES:
      tt_line TYPE TABLE OF ts_line WITH EMPTY KEY .

    CLASS-DATA:
      m_option_icons   TYPE TABLE OF sign_option_icon_s .
    CLASS-DATA:
      mt_lang          TYPE TABLE OF t_lang .
    CLASS-DATA:
      mt_obj           TYPE TABLE OF t_obj .
    CLASS-DATA:
      mt_popups        TYPE TABLE OF t_popup .
    CLASS-DATA i_mermaid_active TYPE boolean .
    CLASS-DATA:
      mt_sel TYPE TABLE OF selection_display_s .

    DATA mv_prog TYPE prog .
    DATA mv_show_prog TYPE prog .
    DATA mv_show_parse_time TYPE abap_bool .
    DATA:
      mt_compo          TYPE TABLE OF scompo .
    DATA mt_locals TYPE tpda_scr_locals_it .
    DATA mt_globals TYPE tpda_scr_globals_it .
    DATA mt_ret_exp TYPE tpda_scr_locals_it .
    DATA m_counter TYPE i .
    DATA:
      mt_steps          TYPE  TABLE OF zcl_ace=>t_step_counter WITH NON-UNIQUE KEY program include line eventtype eventname .
    DATA m_step TYPE i .
    DATA m_i_find TYPE boolean .
    "DATA m_stop_stack TYPE i .
    "DATA m_debug TYPE x .
    DATA m_refresh TYPE boolean .
    DATA m_update TYPE boolean .
    DATA i_step TYPE boolean .
    DATA ms_stack_prev TYPE zcl_ace=>t_stack .
    DATA ms_stack TYPE zcl_ace=>t_stack .
    "DATA i_history TYPE boolean .
    DATA m_hist_step TYPE i .
    DATA m_step_delta TYPE i .
    DATA mv_recurse TYPE i .
    DATA:
      mt_classes_types  TYPE TABLE OF zcl_ace=>t_classes_types .
    DATA mo_window TYPE REF TO zcl_ace_window .
    DATA mv_f7_stop TYPE boolean .
    DATA m_f6_level TYPE i .
    DATA m_target_stack TYPE i .
    DATA mo_tree_local TYPE REF TO zcl_ace_rtti_tree .
    DATA:
      mt_selected_var   TYPE TABLE OF t_sel_var .
    DATA mv_stack_changed TYPE boolean .
    DATA m_variable TYPE REF TO data .
    DATA:
      mt_new_string     TYPE TABLE OF  string .
    DATA m_quick TYPE tpda_scr_quick_info .
    DATA:
      mr_statements     TYPE RANGE OF string .
    DATA ms_if TYPE ts_if .
    DATA mt_if TYPE tt_if .

    CLASS-METHODS init_icons_table .
    CLASS-METHODS check_mermaid .
    CLASS-METHODS open_int_table
      IMPORTING
        !it_tab    TYPE ANY TABLE OPTIONAL
        !it_ref    TYPE REF TO data OPTIONAL
        !i_name    TYPE string
        !io_window TYPE REF TO zcl_ace_window .

    METHODS constructor
      IMPORTING
        !i_prog            TYPE prog
        !i_new_parser      TYPE abap_bool DEFAULT abap_false
        !i_show_parse_time TYPE abap_bool DEFAULT abap_false .
    METHODS show .
    METHODS add_class
      IMPORTING
        !i_class      TYPE string
        !i_refnode    TYPE salv_de_node_key
        !no_locals    TYPE boolean OPTIONAL
        !i_tree       TYPE zcl_ace=>ts_tree OPTIONAL
        !i_type       TYPE flag OPTIONAL
      RETURNING
        VALUE(r_node) TYPE salv_de_node_key .
    METHODS build_local_classes_node
      IMPORTING
        !i_program          TYPE string
        !i_excl_class       TYPE string
        !i_refnode          TYPE salv_de_node_key
      RETURNING
        VALUE(r_locals_rel) TYPE salv_de_node_key .
    METHODS get_code_flow
      IMPORTING
        !i_calc_path   TYPE boolean OPTIONAL
      RETURNING
        VALUE(results) TYPE tt_line .
    METHODS get_code_mix
      IMPORTING
        !i_calc_path TYPE boolean OPTIONAL .
    METHODS mark_active_root
      IMPORTING
        !i_calc_path TYPE boolean OPTIONAL
      CHANGING
        !ct_results  TYPE tt_line .
  PROTECTED SECTION.
  PRIVATE SECTION.
  data mv_dummy type i.
    METHODS get_include_prefix
      IMPORTING
        !i_class         TYPE string
      RETURNING
        VALUE(rv_prefix) TYPE string .
    CONSTANTS:
      BEGIN OF c_kind,
        struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
        table  LIKE cl_abap_typedescr=>kind_table  VALUE cl_abap_typedescr=>kind_table,
        elem   LIKE cl_abap_typedescr=>kind_elem   VALUE cl_abap_typedescr=>kind_elem,
        class  LIKE cl_abap_typedescr=>kind_class  VALUE cl_abap_typedescr=>kind_class,
        intf   LIKE cl_abap_typedescr=>kind_intf   VALUE cl_abap_typedescr=>kind_intf,
        ref    LIKE cl_abap_typedescr=>kind_ref    VALUE cl_abap_typedescr=>kind_ref,
      END OF c_kind .
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
CLASS zcl_ace_metrics DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.

    "--- result per code unit (method / form / module / program-level) ---
    TYPES:
      BEGIN OF ts_unit_result,
        program         TYPE program,
        include         TYPE program,
        unit_type       TYPE string,   " METHOD / FORM / MODULE / FUNCTION / PROGRAM
        unit_name       TYPE string,
        " McCabe cyclomatic complexity
        cyclomatic      TYPE i,
        " Halstead raw counts
        n1              TYPE i,        " total operators
        n2              TYPE i,        " total operands
        big_n1          TYPE i,        " distinct operators
        big_n2          TYPE i,        " distinct operands
        " Halstead derived
        vocabulary      TYPE i,        " η = η1 + η2
        prog_length     TYPE i,        " N = N1 + N2
        volume          TYPE f,        " V = N * log2(η)
        difficulty      TYPE f,        " D = (η1/2) * (N2/η2)
        effort          TYPE f,        " E = D * V
        " Lines of code
        loc             TYPE i,        " total lines in unit
        lloc            TYPE i,        " logical LOC (statements)
        cloc            TYPE i,        " comment lines
      END OF ts_unit_result.
    TYPES:
      tt_unit_results TYPE STANDARD TABLE OF ts_unit_result WITH EMPTY KEY.

    "--- aggregate result for whole program ---
    TYPES:
      BEGIN OF ts_result,
        program              TYPE program,
        units                TYPE tt_unit_results,
        total_cyclomatic     TYPE i,
        total_volume         TYPE f,
        total_effort         TYPE f,
        total_loc            TYPE i,
        total_lloc           TYPE i,
        total_cloc           TYPE i,
        avg_cyclomatic       TYPE f,
      END OF ts_result.

    CLASS-METHODS calculate
      IMPORTING
        is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
        i_program     TYPE program
      RETURNING
        VALUE(rs_result) TYPE ts_result.

  PRIVATE SECTION.

    CLASS-METHODS is_branch_keyword
      IMPORTING i_kw      TYPE string
      RETURNING VALUE(rv) TYPE abap_bool.

    CLASS-METHODS log2
      IMPORTING i_val      TYPE f
      RETURNING VALUE(rv)  TYPE f.

ENDCLASS.
CLASS zcl_ace_metrics_window DEFINITION
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
      IMPORTING i_name    TYPE string
                i_program TYPE program
                i_include TYPE program
                i_line    TYPE i
      CHANGING  cs_source TYPE zif_ace_parse_data=>ts_parse_data.

    METHODS append_comp
      IMPORTING i_name    TYPE string
                i_program TYPE program
                i_include TYPE program
                i_line    TYPE i
      CHANGING  cs_source TYPE zif_ace_parse_data=>ts_parse_data.

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

  methods RESOLVE_VAR_TYPE
    importing
      !IS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_EVTYPE type STRING
      !I_EVNAME type STRING
      !I_VARNAME type STRING
    returning
      value(RV_TYPE) type STRING .
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
CLASS zcl_ace_mermaid DEFINITION
  inheriting from ZCL_ACE_POPUP
  create public .

public section.

  data MO_VIEWER type ref to ZCL_ACE .
  data MO_MM_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_MM_TOOLBAR type ref to CL_GUI_CONTAINER .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_DIAGRAM type ref to OBJECT .
  data MV_TYPE type STRING .
  data MV_CALC_PATH type BOOLEAN .
  data MV_DIRECTION type UI_FUNC .

  methods CONSTRUCTOR
    importing
      !IO_DEBUGGER type ref to ZCL_ACE
      !I_TYPE type STRING .
  methods STEPS_FLOW
    importing
      !I_DIRECTION type UI_FUNC optional .
  methods MAGIC_SEARCH
    importing
      !I_DIRECTION type UI_FUNC optional
      !I_CALC_PATH type BOOLEAN optional .
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
private section.

  constants:
    BEGIN OF c_kind,
                   struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                   table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                   elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                   class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                   intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                   ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
                 END OF c_kind .
  data TREE_TABLE type TT_TABLE .

  methods HNDL_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TREE
    importing
      !NODE_KEY .
  methods HNDL_EXPAND_EMPTY
    for event EXPAND_EMPTY_FOLDER of CL_SALV_EVENTS_TREE
    importing
      !NODE_KEY .
  methods HNDL_USER_COMMAND
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .

  "-- HNDL_DOUBLE_CLICK sub-handlers: one per node type --
  methods DBLCLK_EVENT
    importing
      !I_PROGRAM type ANY
      !I_INCLUDE type ANY
      !I_VALUE   type ANY
      !I_EV_NAME type ANY .
  methods DBLCLK_VARS_LAZY
    importing
      !I_PARAM   type STRING
      !I_PROGRAM type ANY
      !I_INCLUDE type ANY
      !I_EV_TYPE type ANY
      !I_EV_NAME type ANY
      !I_KIND    type ANY
      !IO_NODE   type ref to CL_SALV_NODE .
  methods DBLCLK_MODULE
    importing
      !I_INCLUDE type ANY
      !I_VALUE   type ANY .
  methods DBLCLK_FORM_PLAIN
    importing
      !I_INCLUDE type ANY
      !I_VALUE   type ANY .
  methods DBLCLK_METHOD_PLAIN
    importing
      !I_INCLUDE type ANY
      !I_EV_NAME type ANY
      !I_VALUE   type ANY .
  methods DBLCLK_FORM_ENH
    importing
      !I_PARAM  type ANY
      !I_ENH_ID type ANY .
  methods DBLCLK_METHOD_ENH
    importing
      !I_PARAM   type ANY
      !I_EV_NAME type ANY
      !I_VALUE   type ANY
      !I_EV_TYPE type ANY .
  methods DBLCLK_VAR_LEAF
    importing
      !I_INCLUDE  type ANY
      !I_VALUE    type ANY
      !I_VAR_NAME type ANY
      !IO_NODE    type ref to CL_SALV_NODE .
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
  data M_HIST_DEPTH type I .
  data M_START_STACK type I .
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
       ( function = 'CODEMIX'   icon = CONV #( icon_wizard )              quickinfo = 'Full code flow sequence'          text = 'Code Flow' )
       ( function = 'CALCONLY'  icon = CONV #( icon_biw_formula )         quickinfo = 'Only lines that calculate values'  text = 'Calculation only' )
       ( function = 'HANDLERS'  icon = CONV #( icon_oo_event )            quickinfo = 'Event Handlers flow'              text = 'Handlers' )
       ( function = 'CODE'      icon = CONV #( icon_customer_warehouse )  quickinfo = 'Only Z'                           text = 'Only Z' )
       ( function = 'DEPTH_M'   icon = CONV #( icon_arrow_left )          quickinfo = 'Decrease depth'                   text = '' )
       ( function = 'DEPTH'     icon = CONV #( icon_next_hierarchy_level ) quickinfo = 'History depth level' text = |Depth { m_hist_depth }| )
       ( function = 'DEPTH_P'   icon = CONV #( icon_arrow_right )         quickinfo = 'Increase depth'                   text = '' )
       ( butn_type = 3  )
       ( function = 'METRICS'     icon = CONV #( icon_report )            quickinfo = 'Code Metrics (McCabe CC + Halstead)' text = 'Metrics' )
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
      m_hist_depth = 1.
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
        SELECT COUNT(*) INTO @lv_count FROM reposrc WHERE progname = @lv_prog AND subc = '1'.
        IF lv_count = 1. SUBMIT (lv_prog) VIA SELECTION-SCREEN AND RETURN. ENDIF.

      WHEN 'DEPTH_M'.
        IF m_hist_depth > 0. m_hist_depth -= 1. ENDIF.
        apply_depth( ).

      WHEN 'DEPTH_P'.
        IF m_hist_depth < 99. m_hist_depth += 1. ENDIF.
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
        IF mo_mermaid IS INITIAL OR mo_mermaid->mo_box IS INITIAL.
          " Diagram window does not exist yet — create it.
          mo_mermaid = NEW zcl_ace_mermaid( io_debugger = mo_viewer i_type = 'CALLS' ).
        ELSE.
          " Diagram window is already open — bring it to the front.
          mo_mermaid->mo_box->set_focus( mo_mermaid->mo_box ).
        ENDIF.

      WHEN 'CODEMIX'.
        " Full code flow: all executed statements grouped by method/form scope.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
        apply_depth( ).
        mo_viewer->get_code_mix( ).
        mo_viewer->mo_window->show_stack( ).

      WHEN 'CALCONLY'.
        " Calculation only: same as Code Flow but filtered to lines that
        " actually assign or compute values (i_calc_path = true).
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
        apply_depth( ).
        mo_viewer->get_code_mix( i_calc_path = abap_true ).
        mo_viewer->mo_window->show_stack( ).

      WHEN 'HANDLERS'.
        " Each handler gets two steps:
        "   stack=1 → virtual event node  (EVENT: clicked)
        "   stack=2 → the handler method itself
        " This produces an arrow  EVENT → handler  in the diagram.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.

        LOOP AT mo_viewer->mo_window->ms_sources-tt_handler_map
          INTO DATA(ls_hm).

          CHECK ls_hm-hdl_method IS NOT INITIAL.

          DATA(lv_hdl_class) = ls_hm-hdl_class.

          " If the class is not filled — look it up in calls_line by method name.
          IF lv_hdl_class IS INITIAL.
            LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line
              INTO DATA(ls_cl_hdl)
              WHERE eventname = ls_hm-hdl_method AND eventtype = 'METHOD'.
              lv_hdl_class = ls_cl_hdl-class. EXIT.
            ENDLOOP.
          ENDIF.

          " Find the handler entry point in calls_line.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
            INTO DATA(ls_call_hdl)
            WITH KEY class     = lv_hdl_class
                     eventtype = 'METHOD'
                     eventname = ls_hm-hdl_method.
          CHECK sy-subrc = 0.

          " Add a virtual step for the event at stack=1.
          ADD 1 TO mo_viewer->m_step.
          APPEND VALUE zcl_ace=>t_step_counter(
            step       = mo_viewer->m_step
            stacklevel = 1
            eventtype  = 'EVENT'
            eventname  = |EVENT:{ ls_hm-event_name }|
            program    = ls_call_hdl-program
            include    = ls_call_hdl-include
          ) TO mo_viewer->mt_steps.

          " Handler at stack=2 — child of the event node.
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
        IF mo_mermaid IS NOT INITIAL. mo_mermaid->refresh( ). ENDIF.
        IF mo_viewer->mo_window->m_prg-include = 'Code_Flow_Mix'.
          mo_viewer->get_code_mix( ).
          mo_viewer->mo_window->show_stack( ).
        ENDIF.

      WHEN 'METRICS'.
        zcl_ace_metrics_window=>show(
          is_parse_data = mo_viewer->mo_window->ms_sources
          i_program     = mo_viewer->mo_window->m_prg-program ).

      WHEN 'INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
        l_url = 'https://github.com/ysichov/Smart-Debugger'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.

      WHEN 'STEPS'.
        zcl_ace=>open_int_table( i_name = 'Steps' it_tab = mo_viewer->mt_steps io_window = mo_viewer->mo_window ).

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
    CHECK sy-subrc = 0.
    DATA(lr_kw) = REF #( prog-t_keywords ).
    IF prog-v_keywords IS NOT INITIAL. lr_kw = REF #( prog-v_keywords ). ENDIF.

    LOOP AT lr_kw->* INTO DATA(kw) WHERE v_line = fr_line. EXIT. ENDLOOP.
    CHECK sy-subrc = 0.

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

    ENDCASE.

    CHECK lv_target_vline > 0.

    IF lv_target_include <> m_prg-include AND lv_target_include IS NOT INITIAL.
      m_prg-include = lv_target_include.
      set_program( lv_target_include ).
    ENDIF.
    set_program_line( lv_target_vline ).

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
  method SHOW_COVERAGE.
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
        LOOP AT ms_sources-tt_progs INTO DATA(prog).
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
            SELECT SINGLE funcname INTO @<stack>-eventname FROM tfdir
              WHERE pname_main = @<stack>-program AND include = @incl.
            IF sy-subrc = 0. <stack>-eventtype = 'FUNCTION'. CONTINUE. ENDIF.
          ENDIF.
          DATA: cl_key TYPE seoclskey, meth_includes TYPE seop_methods_w_include.
          cl_key = <stack>-program.
          CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
            EXPORTING clskey = cl_key IMPORTING includes = meth_includes
            EXCEPTIONS _internal_class_not_existing = 1 OTHERS = 2.
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
  endmethod.
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
            i_include = mo_viewer->mo_window->m_prg-program io_debugger = mo_viewer
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
            i_include = mo_viewer->mo_window->m_prg-program io_debugger = mo_viewer ).
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
    IF mo_mermaid IS NOT INITIAL. mo_mermaid->refresh( ). ENDIF.
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

CLASS ZCL_ACE_SOURCE_PARSER IMPLEMENTATION.
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
    CLEAR: io_debugger->mt_steps, io_debugger->m_step.
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
              IF io_debugger->mo_window->m_zcode IS INITIAL OR
               ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( func+0(1) = 'Z' OR func+0(1) = 'Y' ) ).
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
      SELECT include FROM d010inc INTO TABLE @DATA(lt_cm_includes)
        WHERE master = @lv_class_prog AND include LIKE @lv_cm_pattern.
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
        "READ TABLE prog-t_keywords WITH KEY index = statement INTO key.
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
            IF io_debugger->mo_window->m_zcode IS INITIAL OR
              ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( func+0(1) = 'Z' OR func+0(1) = 'Y' ) ).
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

    READ TABLE io_debugger->mo_window->mt_calls
      WITH KEY include = lv_inc ev_name = i_call_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0. RETURN.
    ELSE.
      APPEND INITIAL LINE TO io_debugger->mo_window->mt_calls ASSIGNING FIELD-SYMBOL(<mc>).
      <mc>-include = lv_inc. <mc>-ev_name = i_call_name.
    ENDIF.

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
            IF io_debugger->mo_window->m_zcode IS INITIAL OR func+0(1) = 'Z' OR func+0(1) = 'Y'.
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

    IF io_debugger->mo_window->m_zcode IS INITIAL OR
     ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( i_call-class+0(1) = 'Z' OR i_call-class+0(1) = 'Y' ) )
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
    ASSIGN COMPONENT 'EV_TYPE'  OF STRUCTURE <row> TO FIELD-SYMBOL(<ev_type>).
    ASSIGN COMPONENT 'EV_NAME'  OF STRUCTURE <row> TO FIELD-SYMBOL(<ev_name>).
    ASSIGN COMPONENT 'ENH_ID'   OF STRUCTURE <row> TO FIELD-SYMBOL(<enh_id>).
    ASSIGN COMPONENT 'VAR_NAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<var_name>).

    " kind='C' = global class node — no navigation, no highlight
    CHECK <kind> <> 'C'.

    DATA(lv_param) = CONV string( <param> ).

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

    IF <ev_type> = 'METHOD' OR <ev_type> = 'FORM' OR <ev_type> = 'MODULE'.
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
    IF mo_viewer->mo_window->mo_mermaid IS NOT INITIAL.
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
      READ TABLE mo_viewer->mt_selected_var WITH KEY name = lv_var_name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE mo_viewer->mt_selected_var WHERE name = lv_var_name.
        io_node->set_row_style( if_salv_c_tree_style=>default ).
      ELSE.
        io_node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
        APPEND INITIAL LINE TO mo_viewer->mt_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
        <sel>-name  = lv_var_name.
        <sel>-i_sel = abap_true.
      ENDIF.
    ENDIF.
  endmethod.
  method HNDL_EXPAND_EMPTY.

    DATA(o_node) = mo_tree->get_nodes( )->get_node( node_key ).
    DATA r_row   TYPE REF TO data.
    r_row = o_node->get_data_row( ).
    ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT 'PARAM'   OF STRUCTURE <row> TO FIELD-SYMBOL(<param>).
    ASSIGN COMPONENT 'PROGRAM' OF STRUCTURE <row> TO FIELD-SYMBOL(<program>).

    ASSIGN COMPONENT 'KIND'    OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
    ASSIGN COMPONENT 'EV_TYPE' OF STRUCTURE <row> TO FIELD-SYMBOL(<ev_type>).
    ASSIGN COMPONENT 'EV_NAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<ev_name>).
    ASSIGN COMPONENT 'INCLUDE' OF STRUCTURE <row> TO FIELD-SYMBOL(<include>).

    IF <kind> = 'M' AND <ev_type> = 'METHOD' AND <param> IS INITIAL AND <ev_name> IS NOT INITIAL.
      DATA(lv_m_include) = CONV program( <include> ).
      DATA(lv_m_method)  = CONV string( <ev_name> ).
      READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
        WITH KEY include = lv_m_include eventtype = 'METHOD' eventname = lv_m_method
        INTO DATA(ls_m_cl).
      IF sy-subrc = 0.
          zcl_ace_source_parser=>parse_call(
            i_index      = ls_m_cl-index
            i_e_name     = lv_m_method
            i_e_type     = 'METHOD'
            i_class      = ls_m_cl-class
            i_program    = ls_m_cl-program
            i_include    = lv_m_include
            i_stack      = 0
            i_no_steps   = abap_true
            io_debugger  = mo_viewer ).
        DATA(lv_m_var_cnt) = 0.
        LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_mv)
          WHERE program = ls_m_cl-program AND class = ls_m_cl-class
            AND eventtype = 'METHOD' AND eventname = lv_m_method.
          lv_m_var_cnt += 1.
        ENDLOOP.
        IF lv_m_var_cnt > 0.
          DATA(lv_m_vars_node) = add_node(
            i_name = |Local vars ({ lv_m_var_cnt })|
            i_icon = CONV #( icon_folder )
            i_rel  = node_key
            i_tree = VALUE #( program = ls_m_cl-program include = lv_m_include
                              ev_type = 'VARS' ev_name = lv_m_method
                              param   = |VARS:{ ls_m_cl-class }:{ lv_m_method }| ) ).
          APPEND lv_m_vars_node TO mt_lazy_nodes.
          TRY.
              mo_tree->get_nodes( )->get_node( lv_m_vars_node )->set_expander( abap_true ).
            CATCH cx_root.
          ENDTRY.
          TRY.
              mo_tree->get_nodes( )->get_node( node_key )->expand( ).
            CATCH cx_root.
          ENDTRY.
        ENDIF.
      ENDIF.
      RETURN.
    ENDIF.

    CHECK <param> IS NOT INITIAL.
    DATA(lv_param) = CONV string( <param> ).

    IF lv_param+0(5) = 'VARS:'.
      SPLIT lv_param AT ':' INTO DATA(lv_pfx) DATA(lv_class) DATA(lv_meth).
      READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
        WITH KEY class = lv_class eventtype = 'METHOD' eventname = lv_meth
        INTO DATA(ls_vars_cl).
      IF sy-subrc = 0.
        zcl_ace_source_parser=>parse_call(
          i_index     = ls_vars_cl-index
          i_e_name    = lv_meth
          i_e_type    = 'METHOD'
          i_class     = lv_class
          i_program   = ls_vars_cl-program
          i_include   = ls_vars_cl-include
          i_stack     = 0
          i_no_steps  = abap_true
          io_debugger = mo_viewer ).
      ENDIF.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_v)
        WHERE program = <program> AND class = lv_class AND eventtype = 'METHOD' AND eventname = lv_meth.
        add_node( i_name = lv_v-name i_icon = lv_v-icon i_rel = node_key
                  i_tree = VALUE #( value = lv_v-line include = lv_v-include var_name = lv_v-name ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 10 AND lv_param+0(10) = 'INTF_VARS:'.
      DATA(lv_intf_name) = lv_param+10.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_iv)
        WHERE class = lv_intf_name AND eventname IS INITIAL.
        add_node( i_name = lv_iv-name i_icon = lv_iv-icon i_rel = node_key
                  i_tree = VALUE #( value = lv_iv-line include = lv_iv-include var_name = lv_iv-name ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 10 AND lv_param+0(10) = 'LVARS:FORM'.
      SPLIT lv_param AT ':' INTO DATA(lv_lv_pfx) DATA(lv_lv_type) DATA(lv_lv_form).
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_fv)
        WHERE program = <program> AND eventtype = 'FORM' AND eventname = lv_lv_form.
        add_node( i_name = lv_fv-name i_icon = lv_fv-icon i_rel = node_key
                  i_tree = VALUE #( value = lv_fv-line include = lv_fv-include var_name = lv_fv-name ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    IF lv_param+0(5) = 'ATTR:'.
      DATA(lv_attr_class) = lv_param+5.
      READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
        WITH KEY class = lv_attr_class eventtype = 'METHOD'
        INTO DATA(lv_sub).
      CHECK sy-subrc = 0.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_a)
        WHERE program = lv_sub-program AND class = lv_attr_class AND eventname IS INITIAL.
        add_node( i_name = lv_a-name i_icon = lv_a-icon i_rel = node_key
                  i_tree = VALUE #( value = lv_a-line include = lv_a-include var_name = lv_a-name ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 6 AND lv_param+0(6) = 'INCLS:'.
      DATA(lv_incls_main) = CONV program( lv_param+6 ).
      LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(ls_incl_prog)
        WHERE include <> 'VIRTUAL'
          AND include <> lv_incls_main.
        add_node(
          i_name = CONV #( ls_incl_prog-include )
          i_icon = CONV #( icon_document )
          i_rel  = node_key
          i_tree = VALUE #( kind    = 'M'
                            include = ls_incl_prog-include
                            program = ls_incl_prog-program
                            value   = 1 ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    IF lv_param = 'GVARS:'.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_g)
        WHERE program = <program> AND eventtype IS INITIAL AND class IS INITIAL.
        add_node( i_name = lv_g-name i_icon = lv_g-icon i_rel = node_key
                  i_tree = VALUE #( value = lv_g-line include = lv_g-include var_name = lv_g-name ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 6 AND lv_param+0(6) = 'FORMS:'.
      DATA(lv_forms_prog) = lv_param+6.
      LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_fs)
        WHERE eventtype = 'FORM' AND program = lv_forms_prog.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_fs-include INTO DATA(lv_fp).
        READ TABLE lv_fp-t_keywords WITH KEY index = lv_fs-index INTO DATA(lv_fkw).
        DATA(lv_fn) = add_node(
          i_name = lv_fs-eventname i_icon = CONV #( icon_biw_info_source_ina ) i_rel = node_key
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
        DATA(lv_fvar_cnt) = 0.
        LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_fvc)
          WHERE program = lv_fs-program AND eventtype = 'FORM' AND eventname = lv_fs-eventname.
          lv_fvar_cnt += 1.
        ENDLOOP.
        IF lv_fvar_cnt > 0.
          DATA(lv_fvn) = add_node(
            i_name = |Local vars ({ lv_fvar_cnt })|
            i_icon = CONV #( icon_header )
            i_rel  = lv_fn
            i_tree = VALUE #( param = |LVARS:FORM:{ lv_fs-eventname }| program = lv_fs-program ) ).
          APPEND lv_fvn TO mt_lazy_nodes.
          TRY.
              mo_tree->get_nodes( )->get_node( lv_fvn )->set_expander( abap_true ).
            CATCH cx_root.
          ENDTRY.
        ENDIF.
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 5 AND lv_param+0(5) = 'MODS:'.
      DATA(lv_mods_prog) = lv_param+5.
      LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_ms)
        WHERE eventtype = 'MODULE' AND program = lv_mods_prog.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_ms-include INTO DATA(lv_mp).
        READ TABLE lv_mp-t_keywords WITH KEY index = lv_ms-index INTO DATA(lv_mkw).
        add_node(
          i_name = lv_ms-eventname i_icon = CONV #( icon_biw_info_source_ina ) i_rel = node_key
          i_tree = VALUE #( kind = 'M' value = lv_mkw-v_line include = lv_ms-include
                            program = lv_ms-program ev_type = lv_ms-eventtype ev_name = lv_ms-eventname ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 9 AND lv_param+0(9) = 'LCLASSES:'.
      DATA(lv_lc_prog) = lv_param+9.
      DATA(lv_lc_splits) = VALUE string_table( ).
      SPLIT lv_lc_prog AT '=' INTO TABLE lv_lc_splits.
      DATA(lv_main_class) = lv_lc_splits[ 1 ].
      LOOP AT mo_viewer->mo_window->ms_sources-tt_class_defs INTO DATA(lv_lc_cd)
        WHERE is_intf  = abap_false
          AND program  = lv_lc_prog
          AND class   <> lv_main_class.
        DATA(lv_cls_inc)  = COND program(
          WHEN lv_lc_cd-def_include IS NOT INITIAL THEN lv_lc_cd-def_include
          ELSE lv_lc_cd-include ).
        DATA(lv_cls_line) = lv_lc_cd-def_line.
        DATA(lv_cls_node) = add_node(
          i_name = CONV #( lv_lc_cd-class )
          i_icon = CONV #( icon_folder )
          i_rel  = node_key
          i_tree = VALUE #( kind    = 'M'
                            value   = lv_cls_line
                            include = lv_cls_inc
                            program = lv_lc_cd-program
                            param   = |CLASS:{ lv_lc_cd-class }| ) ).
        APPEND lv_cls_node TO mt_lazy_nodes.
        TRY.
            mo_tree->get_nodes( )->get_node( lv_cls_node )->set_expander( abap_true ).
          CATCH cx_root.
        ENDTRY.
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 7 AND lv_param+0(7) = 'LINTFS:'.
      DATA(lv_li_prog) = lv_param+7.
      DATA(lv_li_splits) = VALUE string_table( ).
      SPLIT lv_li_prog AT '=' INTO TABLE lv_li_splits.
      DATA(lv_li_main) = lv_li_splits[ 1 ].
      LOOP AT mo_viewer->mo_window->ms_sources-tt_class_defs INTO DATA(lv_li_cd)
        WHERE is_intf  = abap_true
          AND program  = lv_li_prog
          AND class   <> lv_li_main.
        DATA(lv_li_inc)  = COND program(
          WHEN lv_li_cd-def_include IS NOT INITIAL THEN lv_li_cd-def_include
          ELSE lv_li_cd-include ).
        DATA(lv_li_line) = lv_li_cd-def_line.
        DATA(lv_li_node) = add_node(
          i_name = CONV #( lv_li_cd-class )
          i_icon = CONV #( icon_oo_connection )
          i_rel  = node_key
          i_tree = VALUE #( kind    = 'M'
                            value   = lv_li_line
                            include = lv_li_inc
                            program = lv_li_cd-program
                            param   = |INTF:{ lv_li_cd-class }| ) ).
        APPEND lv_li_node TO mt_lazy_nodes.
        TRY.
            mo_tree->get_nodes( )->get_node( lv_li_node )->set_expander( abap_true ).
          CATCH cx_root.
        ENDTRY.
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 5 AND lv_param+0(5) = 'SECT:'.
      DATA(lv_sect_rest) = lv_param+5.
      SPLIT lv_sect_rest AT ':' INTO DATA(lv_sect_class) DATA(lv_sect_key).
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_sv)
        WHERE class = lv_sect_class AND section = lv_sect_key AND eventname IS INITIAL.
        add_node( i_name = lv_sv-name i_icon = lv_sv-icon i_rel = node_key
                  i_tree = VALUE #( value = lv_sv-line include = lv_sv-include var_name = lv_sv-name ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 5 AND lv_param+0(5) = 'INTF:'.
      DATA(lv_intf_cls) = lv_param+5.

      DATA(lv_iv_cnt) = 0.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_ivm)
        WHERE class = lv_intf_cls AND eventname IS INITIAL.
        lv_iv_cnt += 1.
      ENDLOOP.
      IF lv_iv_cnt > 0.
        DATA(lv_imemb_node) = add_node(
          i_name = |Members ({ lv_iv_cnt })|
          i_icon = CONV #( icon_header )
          i_rel  = node_key
          i_tree = VALUE #( param = |INTF_VARS:{ lv_intf_cls }| ) ).
        APPEND lv_imemb_node TO mt_lazy_nodes.
        TRY.
            mo_tree->get_nodes( )->get_node( lv_imemb_node )->set_expander( abap_true ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.

      LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_im)
        WHERE class = lv_intf_cls AND eventtype = 'METHOD'.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_im-include INTO DATA(lv_improg).
        READ TABLE lv_improg-t_keywords WITH KEY index = lv_im-index INTO DATA(lv_imkw).
        DATA(lv_im_node) = add_node(
          i_name = lv_im-eventname i_icon = CONV #( icon_oo_inst_method ) i_rel = node_key
          i_tree = VALUE #( kind = 'M' value = lv_imkw-v_line include = lv_im-include
                            program = lv_im-program ev_type = lv_im-eventtype ev_name = lv_im-eventname ) ).
        LOOP AT mo_viewer->mo_window->ms_sources-t_params INTO DATA(lv_imp)
          WHERE class = lv_intf_cls AND event = 'METHOD' AND name = lv_im-eventname AND param IS NOT INITIAL.
          DATA(lv_imp_icon) = COND salv_de_tree_image(
            WHEN lv_imp-type = 'I' THEN CONV #( icon_parameter_import )
            ELSE                        CONV #( icon_parameter_export ) ).
          add_node( i_name = lv_imp-param i_icon = lv_imp_icon i_rel = lv_im_node
                    i_tree = VALUE #( value = lv_imp-line include = lv_imp-include var_name = lv_imp-param ) ).
        ENDLOOP.
      ENDLOOP.
      RETURN.
    ENDIF.

    IF strlen( lv_param ) > 6 AND lv_param+0(6) = 'CLASS:'.
      DATA(lv_cls_name) = lv_param+6.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_class_defs
        WITH KEY class = lv_cls_name INTO DATA(lv_cls_cd).
      DATA(lv_is_intf) = COND abap_bool(
        WHEN sy-subrc = 0 AND lv_cls_cd-is_intf = abap_true THEN abap_true
        ELSE abap_false ).

      IF lv_is_intf = abap_true.
        DATA(lv_iv2_cnt) = 0.
        LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_iv2)
          WHERE class = lv_cls_name AND eventname IS INITIAL.
          lv_iv2_cnt += 1.
        ENDLOOP.
        IF lv_iv2_cnt > 0.
          DATA(lv_memb2_node) = add_node(
            i_name = |Members ({ lv_iv2_cnt })|
            i_icon = CONV #( icon_header )
            i_rel  = node_key
            i_tree = VALUE #( param = |INTF_VARS:{ lv_cls_name }| ) ).
          APPEND lv_memb2_node TO mt_lazy_nodes.
          TRY.
              mo_tree->get_nodes( )->get_node( lv_memb2_node )->set_expander( abap_true ).
            CATCH cx_root.
          ENDTRY.
        ENDIF.
      ELSE.
        DATA(lv_sec_labels) = VALUE string_table(
          ( `Public Section` ) ( `Protected Section` ) ( `Private Section` ) ).
        DATA(lv_sec_keys) = VALUE string_table(
          ( `PUBLIC` ) ( `PROTECTED` ) ( `PRIVATE` ) ).
        DATA(lv_si) = 0.
        LOOP AT lv_sec_keys INTO DATA(lv_sec_key).
          lv_si += 1.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_sections
            WITH KEY class = lv_cls_name section = lv_sec_key
            INTO DATA(ls_sec).
          IF sy-subrc = 0.
            add_node(
              i_name = lv_sec_labels[ lv_si ]
              i_icon = CONV #( icon_open_folder )
              i_rel  = node_key
              i_tree = VALUE #( kind = 'M' include = ls_sec-include value = ls_sec-line ) ).
          ENDIF.
        ENDLOOP.
      ENDIF.

      LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_cm)
        WHERE class = lv_cls_name AND eventtype = 'METHOD'.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_cm-include INTO DATA(lv_cmprog).
        READ TABLE lv_cmprog-t_keywords WITH KEY index = lv_cm-index INTO DATA(lv_cmkw).
        DATA(lv_micon) = COND salv_de_tree_image(
          WHEN lv_is_intf = abap_true OR lv_cm-is_intf = abap_true THEN CONV #( icon_oo_inst_method )
          WHEN lv_cm-redefined = abap_false THEN
            COND #( WHEN lv_cm-meth_type = 0 OR lv_cm-meth_type = 1 THEN CONV #( icon_led_green )
                    WHEN lv_cm-meth_type = 2                          THEN CONV #( icon_led_yellow )
                    WHEN lv_cm-meth_type = 3                          THEN CONV #( icon_led_red )
                    WHEN lv_cm-eventname = 'CONSTRUCTOR'              THEN CONV #( icon_tools )
                    ELSE                                                   CONV #( icon_led_green ) )
          ELSE CONV #( icon_oo_overwrite ) ).
        DATA(lv_meth_node) = add_node(
          i_name = lv_cm-eventname i_icon = lv_micon i_rel = node_key
          i_tree = VALUE #( kind = 'M' value = lv_cmkw-v_line include = lv_cm-include
                            program = lv_cm-program ev_type = lv_cm-eventtype ev_name = lv_cm-eventname ) ).
        LOOP AT mo_viewer->mo_window->ms_sources-t_params INTO DATA(lv_mprm)
          WHERE class = lv_cls_name AND event = 'METHOD' AND name = lv_cm-eventname AND param IS NOT INITIAL.
          DATA(lv_mpicon) = COND salv_de_tree_image(
            WHEN lv_mprm-type = 'I' THEN CONV #( icon_parameter_import )
            ELSE                         CONV #( icon_parameter_export ) ).
          add_node( i_name = lv_mprm-param i_icon = lv_mpicon i_rel = lv_meth_node
                    i_tree = VALUE #( value = lv_mprm-line include = lv_mprm-include var_name = lv_mprm-param ) ).
        ENDLOOP.
        IF lv_cm-include IS NOT INITIAL.
          READ TABLE mo_viewer->mo_window->mt_calls
            WITH KEY include = lv_cm-include ev_name = lv_cm-eventname class = lv_cm-class
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            zcl_ace_source_parser=>parse_call(
              i_index     = lv_cm-index
              i_e_name    = lv_cm-eventname
              i_e_type    = 'METHOD'
              i_class     = lv_cm-class
              i_program   = lv_cm-program
              i_include   = lv_cm-include
              i_stack     = 0
              i_no_steps  = abap_true
              io_debugger = mo_viewer ).
          ENDIF.
        ENDIF.
        DATA(lv_mv_cnt) = 0.
        LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO lv_mv
          WHERE program = lv_cm-program AND class = lv_cls_name
            AND eventtype = 'METHOD' AND eventname = lv_cm-eventname.
          lv_mv_cnt += 1.
        ENDLOOP.
        IF lv_mv_cnt > 0.
          DATA(lv_vars_node) = add_node(
            i_name = |Local vars ({ lv_mv_cnt })|
            i_icon = CONV #( icon_folder )
            i_rel  = lv_meth_node
            i_tree = VALUE #( program = lv_cm-program include = lv_cm-include
                              ev_type = 'VARS' ev_name = lv_cm-eventname
                              param   = |VARS:{ lv_cls_name }:{ lv_cm-eventname }| ) ).
          APPEND lv_vars_node TO mt_lazy_nodes.
          TRY.
              mo_tree->get_nodes( )->get_node( lv_vars_node )->set_expander( abap_true ).
            CATCH cx_root.
          ENDTRY.
        ENDIF.
      ENDLOOP.
      RETURN.
    ENDIF.

  endmethod.
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

        WHEN 'REFRESH'.
          mo_viewer->mo_tree_local->display( ).
          RETURN.

      ENDCASE.

  endmethod.
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
    CHECK sy-subrc = 0.
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
              ENDIF.
              EXIT.
            ENDIF.
            IF lv_up_i = 'NEW' OR lv_up_i = 'CAST'.
              IF lv_up_i = 'NEW'.  lv_new_next  = abap_true. ENDIF.
              IF lv_up_i = 'CAST'. lv_cast_next = abap_true. ENDIF.
            ENDIF.
          ENDLOOP.
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
        lv_tok_idx += 1.
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
        lv_tok_idx += 1.
        READ TABLE io_scan->tokens INDEX lv_tok_idx INTO tok.
        IF sy-subrc = 0. lv_ev_name = tok-str. ENDIF.
        CLEAR: lv_section, lv_pname, lv_ptype, lv_ref, lv_after_type.
        lv_tok_idx += 1.
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

      lv_tok_idx += 1.
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
            lv_i += 1.
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
                lv_k += 1.
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
            lv_j += 1.
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
      SELECT SINGLE refclsname FROM seometarel INTO @rv_super
        WHERE clsname = @mv_class_name AND reltype = '1'.
    ENDIF.
    mv_super_cls = mv_class_name.
    mv_super     = rv_super.
  ENDMETHOD.
  METHOD resolve_var_type.
    " t_vars is pre-sorted by (program, eventtype, eventname, name)
    " before this pass runs — so READ with BINARY SEARCH is O(log n).

    " 1. Local scope
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               eventtype = i_evtype
               eventname = i_evname
               name      = i_varname
      INTO DATA(ls_var).

    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
      RETURN.
    ENDIF.

    " 2. Class attributes (eventtype = '', eventname = '')
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
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
      IF lv_tstr CS '=>' AND lv_tstr CS '('.
        lv_arrow = '=>'.
        SPLIT lv_tstr AT '=>' INTO lv_left lv_rpart.
        SPLIT lv_rpart AT '(' INTO lv_right lv_dummy.

      ELSEIF lv_tstr CS '->' AND lv_tstr CS '('.
        lv_arrow = '->'.
        SPLIT lv_tstr AT '->' INTO lv_left lv_rpart.
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
          lv_ti += 1.
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
          lv_ti += 1. CONTINUE.
        ENDIF.

      ELSEIF lv_tstr = 'NEW'.
        READ TABLE io_scan->tokens INDEX lv_ti + 1 INTO ls_next.
        IF sy-subrc = 0 AND ls_next-str CS '('.
          lv_arrow = '=>'.
          lv_left  = ls_next-str.
          REPLACE ALL OCCURRENCES OF '(' IN lv_left WITH ''.
          CONDENSE lv_left NO-GAPS.
          lv_right = 'CONSTRUCTOR'.
          lv_ti += 1.
        ELSE.
          lv_ti += 1. CONTINUE.
        ENDIF.

      ELSEIF lv_tstr CA '(' AND NOT lv_tstr CS '->' AND NOT lv_tstr CS '=>'.
        " Токен вида NAME( без стрелки.
        " Пропускаем если это LHS-переменная (следующий токен = '=')
        " или это оператор/конструктор языка.
        READ TABLE io_scan->tokens INDEX lv_ti + 1 INTO DATA(ls_after).
        IF ls_after-str = '='.
          " Это LHS: VAR( ... ) = ... — не вызов метода
          lv_ti += 1. CONTINUE.
        ENDIF.
        " Пропускаем первый токен statement если он — простая переменная
        " без скобки в конце (т.е. LHS в VAR = expr), уже обработано выше.
        " Пропускаем языковые конструкторы.
        IF lv_tstr CP 'DATA(*'
          OR lv_tstr CP 'FIELD-SYMBOL(*'
          OR lv_tstr CP 'FINAL(*'
          OR lv_tstr CP 'VALUE(*'
          OR lv_tstr CP 'CONV(*'
          OR lv_tstr CP 'REF(*'
          OR lv_tstr CP 'CAST(*'
          OR lv_tstr CP 'COND(*'
          OR lv_tstr CP 'SWITCH(*'.
          lv_ti += 1. CONTINUE.
        ENDIF.
        " Для первого токена statement дополнительно проверяем:
        " если за ним стоит '=', это LHS — пропускаем.
        " (уже обработано выше, но оставим на случай склеенного токена)
        lv_arrow = '->'.
        lv_left  = 'ME'.
        lv_right = lv_tstr.
        REPLACE ALL OCCURRENCES OF '(' IN lv_right WITH ''.
        CONDENSE lv_right NO-GAPS.

      ELSE.
        lv_ti += 1. CONTINUE.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '(' IN lv_right WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN lv_right WITH ''.
      CONDENSE lv_right NO-GAPS.
      IF lv_right IS INITIAL. lv_ti += 1. CONTINUE. ENDIF.

      " ── Строим запись вызова ──────────────────────────────────────
      CLEAR lv_c.
      lv_c-event = 'METHOD'.
      lv_c-name  = lv_right.
      IF lv_left = 'ME'.
        lv_c-class = mv_class_name.
      ELSEIF lv_left = 'SUPER'.
        lv_c-super = abap_true.
        lv_c-class = COND #( WHEN mv_super IS NOT INITIAL THEN mv_super ELSE mv_class_name ).
      ELSE.
        lv_rtype = resolve_var_type(
          is_source = cs_source i_program = i_program i_include = i_program
          i_evtype  = lv_c-event i_evname = mv_event_name
          i_varname = lv_left ).
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
          lv_ti += 1. CONTINUE.
        ENDIF.
      ENDIF.

      lv_call_cls = COND #( WHEN lv_c-class IS NOT INITIAL THEN lv_c-class ELSE mv_class_name ).

      " ── LHS: lv_x = meth(…) → RETURNING ──────────────────────────
      " Ищем ближайший '=' левее текущей позиции — это LHS для данного вызова.
      " Важно: в выражении A * FUNC1(...) + FUNC2(...) у каждого вызова свой
      " контекст bindings, но RETURNING пишется только если вызов прямо после '='.
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
        IF lv_sa_str = '(' OR lv_sa_str = ','. lv_scan += 1. CONTINUE. ENDIF.
        IF lv_sa_str = 'EXPORTING' OR lv_sa_str = 'IMPORTING' OR
           lv_sa_str = 'CHANGING'  OR lv_sa_str = 'RECEIVING'.
          lv_cur_sec = lv_sa_str.
          lv_pos = abap_false.
          lv_scan += 1. CONTINUE.
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
            lv_scan += 3. CONTINUE.
          ENDIF.
        ENDIF.
        IF lv_pos = abap_true AND lv_single IS INITIAL AND lv_sa_str IS NOT INITIAL.
          lv_single = lv_sa_str.
          REPLACE ALL OCCURRENCES OF ')' IN lv_single WITH ''.
          CONDENSE lv_single NO-GAPS.
        ENDIF.
        lv_scan += 1.
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

      IF lv_lhs IS NOT INITIAL.
        CLEAR lv_ret.
        LOOP AT cs_source-t_params INTO DATA(ls_ret)
          WHERE class = lv_call_cls AND event = 'METHOD'
            AND name = lv_c-name   AND type  = 'R'.
          lv_ret = ls_ret-param. EXIT.
        ENDLOOP.
        CLEAR ls_b. ls_b-outer = lv_lhs. ls_b-inner = lv_ret.
        ls_b-dir = 'E'.
        APPEND ls_b TO lt_bind.
      ENDIF.

      lv_c-bindings = lt_bind.
      APPEND lv_c TO ct_calls.
      lv_ti += 1.
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
      IF sy-subrc = 0 AND ls_tok2-str = 'EVENT'. lv_kw = 'RAISE EVENT'. ENDIF.
    ENDIF.

    IF lv_kw = 'NEW'. lv_kw = 'COMPUTE'. ENDIF.

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
          lv_pf_i += 1.
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
          lv_pf_act_i += 1.
        ENDLOOP.
        IF lt_pf_params IS INITIAL.
          lv_pf_act_i = 1.
          LOOP AT lt_pf_actuals INTO DATA(lv_pf_only).
            READ TABLE lt_pf_act_dirs INDEX lv_pf_act_i INTO DATA(lv_pf_only_dir).
            CLEAR ls_pf_bind.
            ls_pf_bind-outer = lv_pf_only.
            ls_pf_bind-dir   = COND #( WHEN lv_pf_only_dir IS NOT INITIAL THEN lv_pf_only_dir ELSE 'I' ).
            APPEND ls_pf_bind TO ls_pf_call-bindings.
            lv_pf_act_i += 1.
          ENDLOOP.
        ENDIF.
        APPEND ls_pf_call TO lt_new_calls.

      " ── CALL FUNCTION ────────────────────────────────────────────
      WHEN 'CALL FUNCTION'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0.
        DATA(lv_fname) = ls_tok-str.
        REPLACE ALL OCCURRENCES OF '''' IN lv_fname WITH ''.
        APPEND VALUE zcl_ace=>ts_calls( event = 'FUNCTION' name = lv_fname )
          TO lt_new_calls.

      " ── CALL METHOD ──────────────────────────────────────────────
      WHEN 'CALL METHOD'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0 AND ls_tok-str IS NOT INITIAL.
        DATA(lv_call) = VALUE zcl_ace=>ts_calls( event = 'METHOD' ).
        DATA(lv_str)  = ls_tok-str.
        IF lv_str CS '->'.
          SPLIT lv_str AT '->' INTO lv_call-class lv_call-name.
        ELSEIF lv_str CS '=>'.
          SPLIT lv_str AT '=>' INTO lv_call-class lv_call-name.
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
        ELSEIF lv_call-class IS NOT INITIAL.
          DATA(lv_resolved) = resolve_var_type(
            is_source = cs_source i_program = i_program i_include = i_program
            i_evtype = 'METHOD' i_evname = lv_call-name i_varname = lv_call-class ).
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
                READ TABLE io_scan->tokens INDEX lv_tok_cm + 1 INTO DATA(ls_eq_cm).
                IF ls_eq_cm-str = '='.
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
                    lv_tok_cm += 2.
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.
          lv_tok_cm += 1.
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
              IF lv_cn IS NOT INITIAL.
                APPEND VALUE zcl_ace=>ts_calls(
                  event = 'METHOD' class = lv_cn name = 'CONSTRUCTOR' ) TO lt_new_calls.
              ENDIF.
              lv_ci += 1.
            ENDIF.
          ENDIF.
          lv_ci += 1.
        ENDWHILE.
        collect_method_calls(
          EXPORTING io_scan = io_scan i_stmt = ls_stmt i_program = i_program
          CHANGING  cs_source = cs_source ct_calls = lt_new_calls ).

      " ── +CALL_METHOD ─────────────────────────────────────────────
      WHEN '+CALL_METHOD'.
        collect_method_calls(
          EXPORTING io_scan = io_scan i_stmt = ls_stmt i_program = i_program
          CHANGING  cs_source = cs_source ct_calls = lt_new_calls ).

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

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
    CHECK sy-subrc = 0.

    " ── Контекст ─────────────────────────────────────────────────
*    CASE ls_kw-str.
*      WHEN 'CLASS'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_n).
*        IF sy-subrc = 0. mv_class = ls_n-str. ENDIF.
*        LOOP AT io_scan->tokens FROM ls_stmt-from TO ls_stmt-to INTO DATA(ls_ct).
*          IF ls_ct-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. EXIT. ENDIF.
*        ENDLOOP.
*        RETURN.
*      WHEN 'ENDCLASS'.
*        CLEAR: mv_class, mv_in_impl, mv_eventtype, mv_eventname. RETURN.
*      WHEN 'METHOD'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_n.
*        IF sy-subrc = 0. mv_eventtype = 'METHOD'. mv_eventname = ls_n-str. ENDIF.
*        RETURN.
*      WHEN 'ENDMETHOD'. CLEAR: mv_eventtype, mv_eventname. RETURN.
*      WHEN 'FORM'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_n.
*        IF sy-subrc = 0. mv_eventtype = 'FORM'. mv_eventname = ls_n-str. ENDIF.
*        RETURN.
*      WHEN 'ENDFORM'. CLEAR: mv_eventtype, mv_eventname. RETURN.
*      WHEN 'FUNCTION'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_n.
*        IF sy-subrc = 0.
*          mv_eventtype = 'FUNCTION'. mv_eventname = ls_n-str.
*          REPLACE ALL OCCURRENCES OF '''' IN mv_eventname WITH ''.
*        ENDIF.
*        RETURN.
*      WHEN 'ENDFUNCTION'. CLEAR: mv_eventtype, mv_eventname. RETURN.
*      WHEN 'MODULE'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_n.
*        IF sy-subrc = 0. mv_eventtype = 'MODULE'. mv_eventname = ls_n-str. ENDIF.
*        RETURN.
*      WHEN 'ENDMODULE'. CLEAR: mv_eventtype, mv_eventname. RETURN.
*    ENDCASE.

    " ── Только COMPUTE (тип C и D) ───────────────────────────────
    "CHECK ls_stmt-type = 'C' OR ls_stmt-type = 'D'.

    DATA(lv_line) = ls_kw-row.

    " ── Ищем первый '=' ──────────────────────────────────────────
    DATA lv_eq_idx  TYPE i VALUE 0.
    DATA lv_tok_pos TYPE i.
    lv_tok_pos = ls_stmt-from.
    WHILE lv_tok_pos <= ls_stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_pos INTO DATA(ls_tok).
      IF sy-subrc <> 0. EXIT. ENDIF.
      IF ls_tok-str = '='. lv_eq_idx = lv_tok_pos. EXIT. ENDIF.
      lv_tok_pos += 1.
    ENDWHILE.
    CHECK lv_eq_idx > 0.

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
        append_calc( EXPORTING i_name    = lv_lhs
                               i_program = i_program
                               i_include = i_include
                               i_line    = lv_line
                     CHANGING  cs_source = cs_source ).
      ENDIF.
      lv_tok_pos += 1.
    ENDWHILE.

    " ── RHS → t_composed ─────────────────────────────────────────
    DATA lv_prev_arrow TYPE abap_bool.
    DATA lv_skip_next  TYPE abap_bool.
    lv_tok_pos = lv_eq_idx + 1.
    WHILE lv_tok_pos <= ls_stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_pos INTO ls_tok.
      IF sy-subrc <> 0. EXIT. ENDIF.
      DATA(lv_rhs) = ls_tok-str.

      " Токен содержит '->' или '=>': obj->attr / obj->meth( / cls=>meth(
      IF lv_rhs CS '->' OR lv_rhs CS '=>'.
        DATA lv_al TYPE string.
        DATA lv_ar TYPE string.
        IF lv_rhs CS '->'.
          SPLIT lv_rhs AT '->' INTO lv_al lv_ar.
          " Атрибут obj->attr (нет скобки) — obj является переменной
          IF NOT lv_ar CS '(' AND is_varname( lv_al ) = abap_true.
            append_comp( EXPORTING i_name    = lv_al
                                   i_program = i_program
                                   i_include = i_include
                                   i_line    = lv_line
                         CHANGING  cs_source = cs_source ).
          ENDIF.
          " obj->meth( — вызов, obj не нужен
        ENDIF.
        " CLS=>anything — класс, пропускаем
        lv_prev_arrow = abap_false.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      " Стрелка отдельным токеном
      IF lv_rhs = '->' OR lv_rhs = '=>'.
        lv_prev_arrow = abap_true.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      " Токен сразу после отдельной стрелки
      IF lv_prev_arrow = abap_true.
        lv_prev_arrow = abap_false.
        IF lv_rhs CS '('.
          " obj -> meth( — вызов: убираем obj, который был добавлен как обычный токен
          READ TABLE io_scan->tokens INDEX lv_tok_pos - 2 INTO DATA(ls_prev_obj).
          IF sy-subrc = 0.
            DELETE cs_source-t_composed WHERE program = i_program
              AND include = i_include AND line = lv_line AND name = ls_prev_obj-str.
          ENDIF.
        ENDIF.
        " obj -> attr — obj оставляем (добавлен ранее), атрибут не добавляем
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      " NEW/CAST/REF — следующий токен это имя типа, не переменная
      IF lv_rhs = 'NEW' OR lv_rhs = 'CAST' OR lv_rhs = 'REF'.
        lv_skip_next = abap_true.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.
      IF lv_skip_next = abap_true.
        lv_skip_next = abap_false.
        lv_tok_pos += 1. CONTINUE.
      ENDIF.

      " Обычный токен — кандидат в переменную
      DATA(lv_comp) = lv_rhs.
      REPLACE ALL OCCURRENCES OF ')' IN lv_comp WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN lv_comp WITH ''.
      CONDENSE lv_comp NO-GAPS.
      IF lv_comp CS '-'.
        SPLIT lv_comp AT '-' INTO lv_comp lv_dummy.
      ENDIF.
      IF is_varname( lv_comp ) = abap_true.
        append_comp( EXPORTING i_name    = lv_comp
                               i_program = i_program
                               i_include = i_include
                               i_line    = lv_line
                     CHANGING  cs_source = cs_source ).
      ENDIF.
      lv_tok_pos += 1.
    ENDWHILE.
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
      program = i_program include = i_include line = i_line name = i_name )
      TO cs_source-t_calculated.

  ENDMETHOD.
  METHOD append_comp.
    " Дедупликация делается в GET_CODE_FLOW через SORT + DELETE ADJACENT DUPLICATES
    APPEND VALUE zcl_ace=>ts_var(
      program = i_program include = i_include line = i_line name = i_name )
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
      IF lv_key2-name = 'DATA' OR lv_key2-name = 'CLASS-DATA' OR lv_key2-name = 'COMPUTE'.
        DATA(lo_vars2) = NEW zcl_ace_parse_vars( ).
        lo_vars2->zif_ace_stmt_handler~handle(
          EXPORTING io_scan = <prog2>-scan i_stmt_idx = i_stmt_idx
            i_program = lv_prg2 i_include = lv_inc2
            i_class = i_class i_evtype = i_evtype i_ev_name = i_ev_name
          CHANGING cs_source = cs_source ).
      ENDIF.
      IF lv_key2-name = 'COMPUTE'.
        DATA(lo_calcs2) = NEW zcl_ace_parse_calcs( ).
        lo_calcs2->zif_ace_stmt_handler~handle(
          EXPORTING io_scan = <prog2>-scan i_stmt_idx = i_stmt_idx
            i_program = lv_prg2 i_include = lv_inc2
          CHANGING cs_source = cs_source ).
      ENDIF.
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
      READ TABLE <prog2>-t_keywords WITH KEY index = i_stmt_idx ASSIGNING FIELD-SYMBOL(<kw2>).
      IF sy-subrc = 0. <kw2>-calls_parsed = abap_true. ENDIF.
      RETURN.
    ENDIF.

    READ TABLE cs_source-tt_progs WITH KEY include = i_include TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.

    IF i_class IS NOT INITIAL. lv_class = i_class. ENDIF.
    " Для инклудов глобального класса (CU/CO/CI/CP) — класс определяем из имени инклуда
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

      " Skip comment statements: 'P'
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

      " Определяем имя класса/интерфейса для vars:
      " — внутри CLASS...ENDCLASS: lv_class заполнен, lv_section тоже (PUBLIC/PROTECTED/PRIVATE)
      " — внутри INTERFACE...ENDINTERFACE: lv_class пуст, lv_interface заполнен, lv_section пуст
      " Для интерфейса запускаем vars безусловно (нет секций), для класса — только внутри секции
      DATA(lv_vars_class) = COND string(
        WHEN lv_class     IS NOT INITIAL THEN lv_class
        WHEN lv_interface IS NOT INITIAL THEN lv_interface
        ELSE '' ).

*      IF lv_eff_kw = 'DATA'       OR lv_eff_kw = 'CLASS-DATA'
*      OR lv_eff_kw = 'PARAMETERS' OR lv_eff_kw = 'SELECT-OPTIONS'
*        OR lv_eff_kw = 'CONSTANTS' .
        IF lv_vars_class IS NOT INITIAL.
          " Для интерфейса — запускаем всегда (нет секций)
          " Для класса — только если lv_section установлен (находимся внутри DEFINITION)
          IF lv_interface IS NOT INITIAL OR lv_section IS NOT INITIAL.
            lo_vars->zif_ace_stmt_handler~handle(
              EXPORTING io_scan = lo_scan i_stmt_idx = lv_kw_idx
                i_program = i_program i_include = i_include
                i_class = lv_vars_class i_evtype = lv_eventtype i_ev_name = lv_eventname
                i_section = lv_section
              CHANGING cs_source = cs_source ).
          ENDIF.
        ELSE.
          " Нет класса/интерфейса — глобальные или локальные переменные программы
          lo_vars->zif_ace_stmt_handler~handle(
            EXPORTING io_scan = lo_scan i_stmt_idx = lv_kw_idx
              i_program = i_program i_include = i_include
              i_class = lv_vars_class i_evtype = lv_eventtype i_ev_name = lv_eventname
              i_section = lv_section
            CHANGING cs_source = cs_source ).
        ENDIF.
     " ENDIF.

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

  " ---------------------------------------------------------------
  " Row type
  " ---------------------------------------------------------------
  TYPES: BEGIN OF ts_row,
           name        TYPE string,
           cc          TYPE i,
           risk        TYPE string,
           n1          TYPE i,
           n2          TYPE i,
           eta1        TYPE i,
           eta2        TYPE i,
           vocab       TYPE i,
           length      TYPE i,
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
  cl_demo_output=>write_text( '  eta1/eta2 = distinct operators/operands' ).
  cl_demo_output=>write_text( '  N1/N2     = total operators/operands' ).
  cl_demo_output=>write_text( '  Vocab=eta1+eta2  Length=N1+N2' ).
  cl_demo_output=>write_text( '  Volume=Length*log2(Vocab)  Diff=(eta1/2)*(N2/eta2)  Effort=Diff*Volume' ).
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

CLASS ZCL_ACE_METRICS IMPLEMENTATION.
  METHOD calculate.

    rs_result-program = i_program.

    LOOP AT is_parse_data-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE program = i_program.

      DATA(lo_scan) = <prog>-scan.
      CHECK lo_scan IS BOUND.
      CHECK lo_scan->statements IS NOT INITIAL.

      " ---- build ABAP keyword set (= operator vocabulary) ----
      DATA lt_ops TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      CLEAR lt_ops.
      LOOP AT lo_scan->statements INTO DATA(ls_s_op).
        CHECK ls_s_op-type <> 'P'.
        READ TABLE lo_scan->tokens INDEX ls_s_op-from INTO DATA(ls_t_op).
        CHECK sy-subrc = 0.
        INSERT ls_t_op-str INTO TABLE lt_ops.
      ENDLOOP.

      " ---- collect unit boundaries ----
      " Sources:
      "   1. tt_calls_line  → METHOD / FORM / MODULE / FUNCTION blocks
      "   2. t_events       → START-OF-SELECTION / INITIALIZATION / AT ... event blocks
      " index = def_ind means no implementation (interface/abstract) → skip
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

      " --- source 1: calls_line ---
      LOOP AT is_parse_data-tt_calls_line INTO DATA(ls_cl)
        WHERE include  = <prog>-include
          AND index    > 0
          AND ( eventtype = 'METHOD'   OR eventtype = 'FORM'
             OR eventtype = 'MODULE'   OR eventtype = 'FUNCTION' ).

        CHECK ls_cl-index <> ls_cl-def_ind.

        READ TABLE lt_boundaries WITH KEY stmt_from = ls_cl-index
          TRANSPORTING NO FIELDS.
        CHECK sy-subrc <> 0.

        DATA(lv_end_kw) = SWITCH string( ls_cl-eventtype
          WHEN 'METHOD'   THEN 'ENDMETHOD'
          WHEN 'FORM'     THEN 'ENDFORM'
          WHEN 'MODULE'   THEN 'ENDMODULE'
          WHEN 'FUNCTION' THEN 'ENDFUNCTION'
          ELSE                 '' ).

        DATA lv_stmt_to TYPE i.
        lv_stmt_to = 0.
        LOOP AT <prog>-t_keywords INTO DATA(ls_kw)
          WHERE index > ls_cl-index
            AND name  = lv_end_kw.
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

      " --- source 2: t_events (START-OF-SELECTION, INITIALIZATION, AT ...) ---
      LOOP AT is_parse_data-t_events INTO DATA(ls_ev)
        WHERE include     = <prog>-include
          AND stmnt_from  > 0
          AND stmnt_to    > 0.

        READ TABLE lt_boundaries WITH KEY stmt_from = ls_ev-stmnt_from
          TRANSPORTING NO FIELDS.
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

      " ---- metric calculation per unit ----
      DATA lv_first_row TYPE i.
      DATA lv_last_row  TYPE i.
      DATA lv_si        TYPE i.
      DATA lv_ti        TYPE i.
      DATA lt_dist_ops  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA lt_dist_opd  TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA ls_stmt_f    LIKE LINE OF lo_scan->statements.
      DATA ls_stmt_t    LIKE LINE OF lo_scan->statements.
      DATA ls_tok_f     LIKE LINE OF lo_scan->tokens.
      DATA ls_tok_t     LIKE LINE OF lo_scan->tokens.
      DATA ls_stmt      LIKE LINE OF lo_scan->statements.
      DATA ls_kw_tok    LIKE LINE OF lo_scan->tokens.
      DATA ls_tok       LIKE LINE OF lo_scan->tokens.

      LOOP AT lt_boundaries INTO DATA(ls_b).

        DATA ls_unit TYPE ts_unit_result.
        CLEAR ls_unit.
        ls_unit-program   = <prog>-program.
        ls_unit-include   = <prog>-include.
        ls_unit-unit_type = ls_b-unit_type.
        ls_unit-unit_name = COND #(
          WHEN ls_b-unit_type =  'METHOD'
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

        lv_si = ls_b-stmt_from.
        WHILE lv_si <= ls_b-stmt_to.
          CLEAR ls_stmt.
          READ TABLE lo_scan->statements INDEX lv_si INTO ls_stmt.
          IF sy-subrc <> 0. EXIT. ENDIF.

          IF ls_stmt-type = 'P'.
            ADD 1 TO ls_unit-cloc.
          ELSE.
            ADD 1 TO ls_unit-lloc.
            CLEAR ls_kw_tok.
            READ TABLE lo_scan->tokens INDEX ls_stmt-from INTO ls_kw_tok.
            IF sy-subrc = 0 AND is_branch_keyword( ls_kw_tok-str ) = abap_true.
              ADD 1 TO ls_unit-cyclomatic.
            ENDIF.

            lv_ti = ls_stmt-from.
            WHILE lv_ti <= ls_stmt-to.
              CLEAR ls_tok.
              READ TABLE lo_scan->tokens INDEX lv_ti INTO ls_tok.
              IF sy-subrc <> 0. EXIT. ENDIF.
              IF ls_tok-str IS NOT INITIAL.
                READ TABLE lt_ops WITH TABLE KEY table_line = ls_tok-str
                  TRANSPORTING NO FIELDS.
                IF sy-subrc = 0.
                  ADD 1 TO ls_unit-n1.
                  INSERT ls_tok-str INTO TABLE lt_dist_ops.
                ELSE.
                  CASE ls_tok-str.
                    WHEN '+' OR '-' OR '*' OR '/' OR '**' OR '&&'
                      OR '=' OR '<>' OR '<' OR '>' OR '<=' OR '>='
                      OR '(' OR ')' OR ',' OR ':' OR '.' OR '->' OR '=>'.
                      ADD 1 TO ls_unit-n1.
                      INSERT ls_tok-str INTO TABLE lt_dist_ops.
                    WHEN OTHERS.
                      ADD 1 TO ls_unit-n2.
                      INSERT ls_tok-str INTO TABLE lt_dist_opd.
                  ENDCASE.
                ENDIF.
              ENDIF.
              ADD 1 TO lv_ti.
            ENDWHILE.
          ENDIF.
          ADD 1 TO lv_si.
        ENDWHILE.

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
        ENDIF.

        APPEND ls_unit TO rs_result-units.

      ENDLOOP.

    ENDLOOP.

    " ---- aggregate totals ----
    DATA lv_cnt TYPE i.
    LOOP AT rs_result-units INTO DATA(ls_u).
      ADD ls_u-cyclomatic TO rs_result-total_cyclomatic.
      rs_result-total_volume = rs_result-total_volume + ls_u-volume.
      rs_result-total_effort = rs_result-total_effort + ls_u-effort.
      ADD ls_u-loc  TO rs_result-total_loc.
      ADD ls_u-lloc TO rs_result-total_lloc.
      ADD ls_u-cloc TO rs_result-total_cloc.
      ADD 1 TO lv_cnt.
    ENDLOOP.
    IF lv_cnt > 0.
      rs_result-avg_cyclomatic =
        CONV f( rs_result-total_cyclomatic ) / CONV f( lv_cnt ).
    ENDIF.

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
ENDCLASS.

CLASS ZCL_ACE_MERMAID IMPLEMENTATION.
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
          opened -= 1.
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
          opened += 1.
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
          opened += 1.
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
            opened -= 1.
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
        if_ptr += 1.
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
    CHECK lines IS NOT INITIAL.

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
  method ADD_TOOLBAR_BUTTONS.

      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.

      DATA(lv_depth) = mo_viewer->mo_window->m_hist_depth.

      button  = VALUE #(
       ( function = 'TB'       icon = CONV #( icon_view_expand_vertical )   quickinfo = 'Vertical'   text = '' )
       ( function = 'LR'       icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
       ( butn_type = 3 )
       ( function = 'CALLS'    icon = CONV #( icon_workflow_process )       quickinfo = 'Calls Flow'  text = 'Calls Flow' )
       ( function = 'FLOW'     icon = CONV #( icon_wizard )                 quickinfo = 'Calculations flow sequence' text = 'Code Flow' )
       ( function = 'CALCPATH' icon = CONV #( icon_workflow_process )       quickinfo = 'Calc Path - only assigned variables' text = 'Calc Path' )
       ( butn_type = 3 )
       ( function = 'DEPTH_M'  icon = CONV #( icon_arrow_left )            quickinfo = 'Decrease depth' text = '' )
       ( function = 'DEPTH'    icon = CONV #( icon_next_hierarchy_level )  quickinfo = 'Depth level' text = |Depth { lv_depth }| )
       ( function = 'DEPTH_P'  icon = CONV #( icon_arrow_right )           quickinfo = 'Increase depth' text = '' )
       ( butn_type = 3 )
       ( function = 'TEXT'     icon = CONV #( icon_wd_caption )            quickinfo = 'Mermaid Diagram text' text = '' )
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
        WHEN 'CALLS'. steps_flow( ).
        WHEN 'FLOW'.  magic_search( ).
      ENDCASE.

      mo_box->set_focus( mo_box ).

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

      IF fcode = 'LR' OR fcode = 'TB'.
        mv_direction = fcode.
      ELSEIF fcode = 'CALCPATH'.
        mv_type = 'CALCPATH'.
        mv_calc_path = abap_true.
      ELSEIF fcode = 'DEPTH_M'.
        IF mo_viewer->mo_window->m_hist_depth > 0.
          mo_viewer->mo_window->m_hist_depth -= 1.
        ENDIF.
        mo_viewer->mo_window->apply_depth( ).
        mo_box->set_focus( mo_box ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'DEPTH'
                    text  = |Depth { mo_viewer->mo_window->m_hist_depth }| ).
        RETURN.
      ELSEIF fcode = 'DEPTH_P'.
        IF mo_viewer->mo_window->m_hist_depth < 99.
          mo_viewer->mo_window->m_hist_depth += 1.
        ENDIF.
        mo_viewer->mo_window->apply_depth( ).
        mo_box->set_focus( mo_box ).
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
        mo_box->set_focus( mo_box ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'DEPTH'
                    text  = |Depth { mo_viewer->mo_window->m_hist_depth }| ).
        RETURN.
      ELSE.
        mv_type = fcode.
        CLEAR mv_calc_path.
      ENDIF.

      refresh( ).

  endmethod.
  method OPEN_MERMAID.

      CHECK ZCL_ACE=>i_mermaid_active = abap_true.

      TRY.
          IF mo_diagram IS INITIAL.
            CREATE OBJECT mo_diagram TYPE ('ZCL_WD_GUI_MERMAID_JS_DIAGRAM')
              EXPORTING parent = mo_mm_container hide_scrollbars = abap_false.
          ENDIF.
          CALL METHOD mo_diagram->('SET_SOURCE_CODE_STRING') EXPORTING source_code = i_mm_string.
          CALL METHOD mo_diagram->('DISPLAY').
        CATCH cx_root INTO DATA(error).
          MESSAGE error TYPE 'E'.
      ENDTRY.

  endmethod.
  method REFRESH.

      CASE mv_type.
        WHEN 'CALLS'.    steps_flow( mv_direction ).
        WHEN 'FLOW'.     magic_search( i_direction = mv_direction ).
        WHEN 'CALCPATH'. magic_search( i_direction = mv_direction i_calc_path = abap_true ).
      ENDCASE.

  endmethod.
  METHOD steps_flow.

    TYPES: BEGIN OF lty_entity,
             include TYPE string,
             class   TYPE string,
             event   TYPE string,
             name    TYPE string,
             style   TYPE string,
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

    " ── Шаг 1: собираем уникальные ноды ────────────────────────────
    LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
      entity-event = <copy>-eventtype.

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
      lv_idx += 1.
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
          " Рисуем стрелку только если ещё не было такой
          ind-from = ls_caller-entity_idx.
          ind-to   = lv_cur_idx.
          READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            mm_string = |{ mm_string }{ ind-from } --> { ind-to }\n|.
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
  METHOD check_mermaid.
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING clskey = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
      EXCEPTIONS not_specified = 1 not_existing = 2 i_interface = 3
                 no_text = 4 inconsistent = 5 OTHERS = 6.
    IF sy-subrc = 0. i_mermaid_active = abap_true. ENDIF.
  ENDMETHOD.
  METHOD init_icons_table.
    m_option_icons = VALUE #(
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
  ENDMETHOD.
  METHOD open_int_table.
    DATA r_tab TYPE REF TO data.
    IF it_ref IS BOUND. r_tab = it_ref. ELSE. GET REFERENCE OF it_tab INTO r_tab. ENDIF.
    APPEND INITIAL LINE TO zcl_ace=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW zcl_ace_table_viewer( i_additional_name = i_name ir_tab = r_tab io_window = io_window ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
  ENDMETHOD.
  METHOD constructor.
    CONSTANTS: c_mask TYPE x VALUE '01'.
    mv_prog = i_prog. mv_show_parse_time = i_show_parse_time. i_step = abap_on.
    zcl_ace=>check_mermaid( ). zcl_ace=>init_icons_table( ).
    mo_window = NEW zcl_ace_window( me ). mo_window->mv_new_parser = i_new_parser.
    mo_tree_local = NEW zcl_ace_rtti_tree( i_header = 'Objects & Code Flow' i_type = 'L'
                                           i_cont = mo_window->mo_locals_container i_debugger = me ).
    show( ).
  ENDMETHOD.
  METHOD get_include_prefix.
    IF strlen( i_class ) >= 30. rv_prefix = i_class.
    ELSE. rv_prefix = i_class && repeat( val = `=` occ = 30 - strlen( i_class ) ) && `======`. ENDIF.
  ENDMETHOD.
  METHOD build_local_classes_node.
    DATA: test_rel  TYPE salv_de_node_key,
          intf_rel  TYPE salv_de_node_key,
          lv_prefix TYPE string,
          lv_ccau   TYPE string.
    lv_prefix = get_include_prefix( i_excl_class ).
    lv_ccau   = lv_prefix && 'CCAU'.

    " Итерируем по tt_class_defs — туда попадают ВСЕ CLASS/INTERFACE DEFINITION.
    " Фильтр по program — берём только объекты этой программы.
    LOOP AT mo_window->ms_sources-tt_class_defs INTO DATA(ls_cd)
      WHERE class <> i_excl_class
        AND program = i_program.

      IF ls_cd-def_include = lv_ccau.
        IF test_rel IS INITIAL.
          test_rel = mo_tree_local->add_node( i_name = 'Unit Test Classes' i_icon = CONV #( icon_folder )
            i_rel = i_refnode i_tree = VALUE #( ) ).
        ENDIF.
        add_class( i_class = ls_cd-class i_refnode = test_rel no_locals = abap_true i_type = 'T' ).

      ELSEIF ls_cd-is_intf = abap_true.
        IF intf_rel IS INITIAL.
          intf_rel = mo_tree_local->add_node( i_name = 'Interfaces' i_icon = CONV #( icon_oo_connection )
            i_rel = i_refnode i_tree = VALUE #( ) ).
        ENDIF.
        add_class( i_class = ls_cd-class i_refnode = intf_rel no_locals = abap_true i_type = 'I' ).

      ELSE.
        IF r_locals_rel IS INITIAL.
          r_locals_rel = mo_tree_local->add_node( i_name = 'Local Classes' i_icon = CONV #( icon_folder )
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
          attr_rel    TYPE salv_de_node_key,
          include     TYPE string,
          prefix      TYPE string.

    IF i_type = 'I'. icon = icon_oo_connection.
    ELSEIF i_type = 'T'. icon = icon_test.
    ELSE. icon = icon_folder. ENDIF.

    LOOP AT mo_window->ms_sources-t_classes INTO DATA(ls_class) WHERE clsname = i_class AND reltype = '1'.
      IF class_rel IS INITIAL.
        class_rel = mo_tree_local->add_node( i_name = i_class i_icon = icon i_rel = i_refnode i_tree = i_tree ).
      ENDIF.
      add_class( i_class = CONV #( ls_class-refclsname ) i_refnode = class_rel no_locals = abap_true i_type = 'I' ).
    ENDLOOP.

    " Координаты CLASS/INTERFACE name DEFINITION
    DATA lv_def_inc  TYPE program.
    DATA lv_def_line TYPE i.
    IF i_tree-kind = 'C'.
      lv_def_inc  = i_tree-include.
      lv_def_line = CONV i( i_tree-value ).
    ELSE.
      READ TABLE mo_window->ms_sources-tt_class_defs
        WITH KEY class = i_class INTO DATA(ls_cd).
      IF sy-subrc = 0.
        lv_def_inc  = ls_cd-def_include.
        lv_def_line = ls_cd-def_line.
      ENDIF.
    ENDIF.

    " Создаём ноду класса/интерфейса
    IF class_rel IS INITIAL.
      IF i_tree-kind = 'C'.
        class_rel = mo_tree_local->add_node( i_name = i_class i_icon = icon i_rel = i_refnode
                      i_tree = VALUE #( kind    = 'C'
                                       include = lv_def_inc
                                       value   = lv_def_line ) ).
      ELSE.
        class_rel = mo_tree_local->add_node( i_name = i_class i_icon = icon i_rel = i_refnode
                      i_tree = VALUE #( param   = |CLASS:{ i_class }|
                                       include = lv_def_inc
                                       value   = lv_def_line ) ).
      ENDIF.
    ENDIF.

    IF i_type = 'I'.
      " Для интерфейса — секций нет, показываем Members с атрибутами напрямую
      DATA(lv_intf_var_cnt) = 0.
      LOOP AT mo_window->ms_sources-t_vars INTO DATA(lv_iv)
        WHERE class = i_class AND eventname IS INITIAL.
        lv_intf_var_cnt += 1.
      ENDLOOP.
      IF lv_intf_var_cnt > 0.
        DATA(lv_members_node) = mo_tree_local->add_node(
          i_name = |Members ({ lv_intf_var_cnt })|
          i_icon = CONV #( icon_header )
          i_rel  = class_rel
          i_tree = VALUE #( param = |INTF_VARS:{ i_class }| ) ).
        APPEND lv_members_node TO mo_tree_local->mt_lazy_nodes.
      ENDIF.
    ELSEIF i_type <> 'T'.
      " Для классов — секции PUBLIC/PROTECTED/PRIVATE из tt_sections
      DATA(lv_sec_labels) = VALUE string_table(
        ( `Public Section` ) ( `Protected Section` ) ( `Private Section` ) ).
      DATA(lv_sec_keys) = VALUE string_table(
        ( `PUBLIC` ) ( `PROTECTED` ) ( `PRIVATE` ) ).
      DATA(lv_si) = 0.
      LOOP AT lv_sec_keys INTO DATA(lv_sec_key).
        lv_si += 1.
        READ TABLE mo_window->ms_sources-tt_sections
          WITH KEY class = i_class section = lv_sec_key
          INTO DATA(ls_sec).
        IF sy-subrc = 0.
          DATA(lv_sec_node) = mo_tree_local->add_node(
            i_name = lv_sec_labels[ lv_si ]
            i_icon = CONV #( icon_open_folder )
            i_rel  = class_rel
            i_tree = VALUE #( kind = 'M' include = ls_sec-include value = ls_sec-line
                              param = |SECT:{ i_class }:{ lv_sec_key }| ) ).
          READ TABLE mo_window->ms_sources-t_vars WITH KEY class = i_class section = lv_sec_key
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND lv_sec_node TO mo_tree_local->mt_lazy_nodes.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Методы
    LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs) WHERE class = i_class AND eventtype = 'METHOD'.

      SPLIT subs-include AT '=' INTO TABLE splits_incl.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs-include INTO DATA(prog).
      READ TABLE prog-t_keywords WITH KEY index = subs-index INTO DATA(keyword).

      tree-kind = 'M'.
      IF subs-is_intf = abap_true AND subs-def_line > 0. tree-value = subs-def_line.
      ELSEIF keyword-v_line > 0. tree-value = keyword-v_line.
      ELSEIF keyword-line > 0. tree-value = keyword-line.
      ELSE.
        LOOP AT prog-t_keywords INTO DATA(lv_kw_fb) WHERE name = 'METHOD'.
          READ TABLE prog-scan->statements INDEX lv_kw_fb-index INTO DATA(ls_s_fb).
          IF sy-subrc = 0.
            READ TABLE prog-scan->tokens INDEX ls_s_fb-from + 1 INTO DATA(ls_t_fb).
            IF sy-subrc = 0 AND ls_t_fb-str = subs-eventname. tree-value = lv_kw_fb-line. EXIT. ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF tree-value IS INITIAL AND subs-def_line > 0. tree-value = subs-def_line. ENDIF.

      tree-include = subs-include. tree-program = subs-program.
      tree-ev_type = subs-eventtype. tree-ev_name = subs-eventname. CLEAR tree-param.

      IF i_type = 'I'. icon = icon_oo_inst_method.
      ELSEIF subs-redefined = abap_false.
        CASE subs-meth_type.
          WHEN 0 OR 1. icon = icon_led_green.
          WHEN 2.      icon = icon_led_yellow.
          WHEN 3.      icon = icon_led_red.
          WHEN OTHERS. IF subs-eventname = 'CONSTRUCTOR'. icon = icon_tools. ENDIF.
        ENDCASE.
      ELSE. icon = icon_oo_overwrite. ENDIF.

      DATA(event_node) = mo_tree_local->add_node( i_name = subs-eventname i_icon = icon i_rel = class_rel i_tree = tree ).
      APPEND event_node TO mo_tree_local->mt_lazy_nodes.

      LOOP AT mo_window->ms_sources-t_params INTO DATA(lv_p)
        WHERE class = subs-class AND event = 'METHOD' AND name = subs-eventname AND param IS NOT INITIAL.
        DATA(lv_p_icon) = COND salv_de_tree_image( WHEN lv_p-type = 'I' THEN CONV #( icon_parameter_import )
                                                   ELSE                      CONV #( icon_parameter_export ) ).
        mo_tree_local->add_node( i_name = lv_p-param i_icon = lv_p_icon i_rel = event_node
          i_tree = VALUE #( value = lv_p-line include = lv_p-include var_name = lv_p-param ) ).
      ENDLOOP.

      " Парсим метод чтобы заполнить t_vars, затем добавляем Local vars если есть
      IF subs-include IS NOT INITIAL.
        READ TABLE mo_window->mt_calls
          WITH KEY include = subs-include ev_name = subs-eventname class = subs-class
          TRANSPORTING NO FIELDS.
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
        lv_var_cnt += 1.
      ENDLOOP.
      IF lv_var_cnt > 0.
        DATA(lv_vars_node) = mo_tree_local->add_node(
          i_name = |Local vars ({ lv_var_cnt })| i_icon = CONV #( icon_folder ) i_rel = event_node
          i_tree = VALUE #( program = subs-program include = subs-include ev_type = 'VARS'
                            ev_name = subs-eventname param = |VARS:{ subs-class }:{ subs-eventname }| ) ).
        APPEND lv_vars_node TO mo_tree_local->mt_lazy_nodes.
      ENDIF.
    ENDLOOP.

    IF no_locals = abap_false.
      prefix = get_include_prefix( i_class ). include = prefix && 'CP'.
      build_local_classes_node( i_program = include i_excl_class = i_class i_refnode = class_rel ).
    ENDIF.

    r_node = class_rel.
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
            ELSE. lv_inner_depth += 1. ENDIF.
          WHEN 'ENDIF' OR 'ENDCASE'.
            IF lv_i = lv_end_i. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs.
            ELSE. lv_inner_depth -= 1. ENDIF.
          WHEN 'ELSEIF' OR 'ELSE' OR 'WHEN'.
            IF lv_inner_depth = 0. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs. ENDIF.
        ENDCASE.
        lv_i += 1.
      ENDWHILE.
      CLEAR lv_block_active. lv_hdr_cnt = lines( lt_hdrs ). lv_h = 1.
      WHILE lv_h < lv_hdr_cnt.
        DATA(ls_hdr_cur) = lt_hdrs[ lv_h ]. DATA(ls_hdr_next) = lt_hdrs[ lv_h + 1 ].
        CLEAR: lv_branch_active, lv_scan_inner. lv_scan = ls_hdr_cur-tabix + 1.
        WHILE lv_scan < ls_hdr_next-tabix.
          READ TABLE ct_results INDEX lv_scan ASSIGNING FIELD-SYMBOL(<scan_ln>). IF sy-subrc <> 0. EXIT. ENDIF.
          CASE <scan_ln>-cond.
            WHEN 'IF' OR 'CASE'. lv_scan_inner += 1.
            WHEN 'ENDIF' OR 'ENDCASE'. lv_scan_inner -= 1.
            WHEN OTHERS. IF lv_scan_inner = 0 AND <scan_ln>-active_root = abap_true. lv_branch_active = abap_true. ENDIF.
          ENDCASE.
          IF lv_branch_active = abap_true. EXIT. ENDIF.
          lv_scan += 1.
        ENDWHILE.
        ASSIGN ct_results[ ls_hdr_cur-tabix ] TO FIELD-SYMBOL(<hdr_ln>).
        IF sy-subrc = 0.
          IF lv_branch_active = abap_true. <hdr_ln>-active_root = abap_true. lv_block_active = abap_true.
          ELSE. CLEAR <hdr_ln>-active_root. ENDIF.
        ENDIF.
        lv_h += 1.
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
    DATA: add         TYPE boolean, sub TYPE string, form TYPE string, direction TYPE string,
          ind2        TYPE i, start TYPE i, end TYPE i, bool TYPE string, block_first TYPE i,
          els_before  TYPE i, inserted TYPE boolean.
    DATA: line      TYPE ts_line, pre_stack TYPE ts_line, opened TYPE i.
    READ TABLE mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(prog).
    DATA(lt_selected_var) = mt_selected_var.
    IF mo_window->ms_sel_call IS NOT INITIAL.
      CLEAR: mt_steps, mo_window->mt_calls.
      IF mo_window->ms_sel_call-eventtype = 'FORM'.
        zcl_ace_source_parser=>parse_call_form( EXPORTING i_call_name = mo_window->ms_sel_call-eventname
          i_program = mo_window->ms_sel_call-program i_include = mo_window->ms_sel_call-include
          i_stack = 0 io_debugger = mo_window->mo_viewer ).
      ELSE.
        zcl_ace_source_parser=>parse_call( EXPORTING i_index = mo_window->ms_sel_call-index
          i_e_name = mo_window->ms_sel_call-eventname i_e_type = mo_window->ms_sel_call-eventtype
          i_program = mo_window->ms_sel_call-program i_include = mo_window->ms_sel_call-include
          i_class = mo_window->ms_sel_call-class i_stack = 0 io_debugger = mo_window->mo_viewer ).
      ENDIF.
    ENDIF.
    DATA(steps) = mt_steps.
    SORT steps BY line eventtype eventname. DELETE ADJACENT DUPLICATES FROM steps. SORT steps BY step.
    DATA: yes TYPE xfeld.
    LOOP AT steps INTO DATA(step).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO DATA(keyword).
      LOOP AT keyword-tt_calls INTO DATA(call).
        " Check whether any binding's outer or inner side matches a selected variable.
        " If the variable filter is empty every call qualifies unconditionally.
        LOOP AT call-bindings INTO DATA(ls_b_chk).
          READ TABLE lt_selected_var WITH KEY name = ls_b_chk-outer TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL. yes = abap_true. ENDIF.
          READ TABLE lt_selected_var WITH KEY name = ls_b_chk-inner TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL. yes = abap_true. ENDIF.
        ENDLOOP.
      ENDLOOP.
      IF yes = abap_true.
        " Add all binding sides to the selected-variable set so that downstream
        " steps that reference the same variables are also included.
        LOOP AT keyword-tt_calls INTO call.
          LOOP AT call-bindings INTO DATA(ls_b_add).
            READ TABLE lt_selected_var WITH KEY name = ls_b_add-outer TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO lt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
              <selected>-name = ls_b_add-outer.
            ENDIF.
            READ TABLE lt_selected_var WITH KEY name = ls_b_add-inner TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO lt_selected_var ASSIGNING <selected>.
              <selected>-name = ls_b_add-inner.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    DATA: prev    LIKE LINE OF mt_steps, pre_key TYPE string.
    READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
    LOOP AT steps ASSIGNING FIELD-SYMBOL(<step>).
      DATA(ind) = sy-tabix.
      READ TABLE prog-t_keywords WITH KEY line = <step>-line INTO DATA(key).
      IF prev IS NOT INITIAL.
        IF ( key-name = 'ENDDO' OR key-name = 'ENDWHILE' OR key-name = 'ENDLOOP' OR key-name = 'ENDIF' ) AND
           ( pre_key = 'DO' OR pre_key = 'LOOP' OR pre_key = 'WHILE' OR pre_key = 'IF' ).
          <step>-first = 'D'.
          READ TABLE mt_steps INDEX ind - 1 ASSIGNING FIELD-SYMBOL(<step_prev>). <step_prev>-first = 'D'.
        ENDIF.
      ENDIF.
      prev = <step>. pre_key = key-name.
    ENDLOOP.
    DELETE steps WHERE first = 'D'. SORT steps BY step DESCENDING.
    LOOP AT steps INTO step.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      LOOP AT mo_window->ms_sources-t_calculated INTO DATA(calculated_var) WHERE line = step-line.
        ADD 1 TO ind.
        LOOP AT mo_window->ms_sources-t_composed INTO DATA(composed_var) WHERE line = step-line.
          READ TABLE lt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. APPEND INITIAL LINE TO lt_selected_var ASSIGNING <selected>. <selected>-name = composed_var-name. ENDIF.
        ENDLOOP.
        READ TABLE lt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO lt_selected_var ASSIGNING <selected>.
          " Propagate the calculated variable name (inner side of the last processed binding).
          READ TABLE prog-t_keywords WITH KEY line = step-line INTO DATA(kw_calc).
          LOOP AT kw_calc-tt_calls INTO DATA(call_calc).
            LOOP AT call_calc-bindings INTO DATA(ls_b_calc).
              IF ls_b_calc-inner IS NOT INITIAL.
                <selected>-name = ls_b_calc-inner.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.
      " For each call, trace bindings: when the outer side is already selected,
      " add the corresponding inner side to the propagation set.
      LOOP AT keyword-tt_calls INTO call.
        LOOP AT call-bindings INTO DATA(ls_b_prop).
          READ TABLE lt_selected_var WITH KEY name = ls_b_prop-outer TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO lt_selected_var ASSIGNING <selected>.
            <selected>-name = ls_b_prop-inner.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
    SORT lt_selected_var. DELETE ADJACENT DUPLICATES FROM lt_selected_var. CLEAR mo_window->mt_coverage.

    " In calc_path mode with no variable filter, 'mt_selected_var IS INITIAL' would
    " unconditionally mark every line that touches t_calculated as active_root — including
    " CATCH, MESSAGE, WRITE and other non-assignment statements.
    " Use a local flag so the 'OR IS INITIAL' shortcut is suppressed when calc_path is on.
    DATA(lv_no_filter) = xsdbool( mt_selected_var IS INITIAL AND i_calc_path = abap_false ).

    LOOP AT steps INTO step.
      CLEAR inserted.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO key.
      CLEAR line-cond.
      IF key-name = 'IF' OR key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ELSEIF' OR
         key-name = 'CASE' OR key-name = 'WHEN' OR key-name = 'ENDCASE' OR
         key-name = 'DO' OR key-name = 'ENDDO' OR key-name = 'LOOP' OR key-name = 'ENDLOOP' OR
         key-name = 'WHILE' OR key-name = 'ENDWHILE'.
        APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).
        <watch>-program = step-program. <watch>-line = line-line = step-line.
        INSERT line INTO results INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-cond = key-name. <line>-ev_name = step-eventname. <line>-stack = step-stacklevel.
        <line>-include = step-include. <line>-class = step-class. inserted = abap_true.
      ENDIF.
      CLEAR ind.
      LOOP AT mo_window->ms_sources-t_calculated INTO calculated_var WHERE line = step-line.
        ADD 1 TO ind.
        LOOP AT mo_window->ms_sources-t_composed INTO composed_var WHERE line = step-line.
          READ TABLE lt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. APPEND INITIAL LINE TO lt_selected_var ASSIGNING <selected>. <selected>-name = composed_var-name. ENDIF.
        ENDLOOP.
        READ TABLE lt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
        " In normal mode (no calc_path): accept any line when no variable filter is set.
        " In calc_path mode: only accept lines whose assigned variable is already selected —
        " 'OR IS INITIAL' is deliberately suppressed to avoid false positives from
        " CATCH / MESSAGE / WRITE that happen to read a t_calculated variable.
        IF sy-subrc = 0 OR lv_no_filter = abap_true.
          APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING <watch>.
          <watch>-program = step-program. <watch>-line = line-line = step-line.
          IF ind = 1.
            IF key-name <> 'PUBLIC' AND key-name <> 'ENDCLASS' AND key-name <> 'ENDFORM' AND
               key-name <> 'FORM' AND key-name <> 'METHOD' AND key-name <> 'METHODS' AND
               key-name <> 'ENDMETHOD' AND key-name <> 'MODULE' AND inserted = abap_false.
              line-ev_name = step-eventname. line-stack = step-stacklevel. line-include = step-include.
              line-class = step-class. line-ev_type = step-eventtype. line-active_root = abap_true.
              INSERT line INTO results INDEX 1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF inserted = abap_false.
        IF key-name <> 'PUBLIC' AND key-name <> 'ENDCLASS' AND key-name <> 'ENDFORM' AND
           key-name <> 'ENDMETHOD' AND key-name <> 'METHOD' AND key-name <> 'METHODS' AND
           key-name <> 'MODULE' AND key-name <> 'FORM' AND inserted = abap_false.
          READ TABLE results WITH KEY line = step-line include = step-include
            ev_type = step-eventtype ev_name = step-eventname TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            line-line = step-line. line-ev_name = step-eventname. line-stack = step-stacklevel.
            line-include = step-include. line-ev_type = step-eventtype. line-class = step-class.
            line-active_root = abap_false. INSERT line INTO results INDEX 1.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR inserted.
    ENDLOOP.
    DELETE results WHERE del = abap_true.
    DATA lv_changed TYPE boolean.
    DO.
      lv_changed = abap_false.
      LOOP AT results ASSIGNING <line>.
        DATA(lv_ti) = sy-tabix.
        IF <line>-cond = 'LOOP' OR <line>-cond = 'DO' OR <line>-cond = 'WHILE'.
          READ TABLE results INDEX lv_ti + 1 ASSIGNING FIELD-SYMBOL(<next>).
          IF sy-subrc = 0.
            IF ( <line>-cond = 'LOOP' AND <next>-cond = 'ENDLOOP' ) OR
               ( <line>-cond = 'DO'   AND <next>-cond = 'ENDDO'   ) OR
               ( <line>-cond = 'WHILE' AND <next>-cond = 'ENDWHILE' ).
              DELETE results INDEX lv_ti + 1. DELETE results INDEX lv_ti. lv_changed = abap_true. EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_changed = abap_false. EXIT. ENDIF.
    ENDDO.
    LOOP AT results ASSIGNING <line>.
      ind = sy-tabix.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = <line>-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = <line>-line INTO keyword.
      LOOP AT prog-scan->tokens FROM keyword-from TO keyword-to INTO DATA(token).
        IF token-str = 'USING' OR token-str = 'EXPORTING' OR token-str = 'IMPORTING' OR token-str = 'CHANGING'.
          EXIT.
        ENDIF.
        IF <line>-code IS INITIAL.
          <line>-code = token-str.
        ELSE.
          <line>-code = |{ <line>-code } { token-str }|.
        ENDIF.
      ENDLOOP.
      IF keyword-tt_calls IS NOT INITIAL.
        " Build the subname and arrow label from call bindings.
        DATA(lv_arrow_cnt) = 0.
        LOOP AT keyword-tt_calls INTO call.
          " Derive the display name of the called method from the first call entry.
          IF <line>-subname IS INITIAL.
            <line>-subname = call-name.
            REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
            REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
          ENDIF.
          REPLACE ALL OCCURRENCES OF '"' IN <line>-code WITH ''.

          " Iterate over the BINDINGS table of the current call.
          " Each row carries: outer (caller-side variable), inner (callee-side parameter), dir.
          " dir values: 'I' = importing/using  (outer -> inner)
          "             'E' = exporting/returning (outer <- inner)
          "             'C' = changing            (outer <-> inner)
          LOOP AT call-bindings INTO DATA(ls_b).
            CHECK ls_b-outer IS NOT INITIAL OR ls_b-inner IS NOT INITIAL.
            IF lv_arrow_cnt > 0. <line>-arrow = |{ <line>-arrow }, |. ENDIF.
            DATA(lv_sep) = SWITCH string( ls_b-dir
              WHEN 'I' THEN '->'
              WHEN 'E' THEN '<-'
              WHEN 'C' THEN '-> <-'
              ELSE          '--' ).
            <line>-arrow = |{ <line>-arrow } { ls_b-outer } { lv_sep } { ls_b-inner }|.
            lv_arrow_cnt += 1.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
      REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
    ENDLOOP.
    DATA: if_depth TYPE i, when_count TYPE i.
    LOOP AT results ASSIGNING <line>. <line>-ind = sy-tabix. ENDLOOP.
    LOOP AT results ASSIGNING <line>
      WHERE code <> 'DO' AND code <> 'ENDDO' AND code <> 'WHILE' AND code <> 'ENDWHILE'
        AND code <> 'LOOP' AND code <> 'ENDLOOP'.
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
        DATA(counter) = <line>-ind + 1.
        DO.
          READ TABLE results INDEX counter INTO line.
          IF sy-subrc <> 0. CLEAR <line>-els_after. EXIT. ENDIF.
          IF line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
            CLEAR <line>-els_after. EXIT.
          ELSEIF line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND
                 line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after = counter. EXIT.
          ELSE.
            ADD 1 TO counter.
          ENDIF.
        ENDDO.
      ENDIF.
      IF <line>-cond = 'WHEN'.
        <line>-els_before = els_before. <line>-els_after = <line>-ind.
        counter = <line>-ind + 1.
        DO.
          READ TABLE results INDEX counter INTO line.
          IF sy-subrc <> 0. CLEAR <line>-els_after. EXIT. ENDIF.
          IF line-cond = 'WHEN'.
            CLEAR <line>-els_after. EXIT.
          ELSEIF line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND
                 line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after = counter. EXIT.
          ELSE.
            ADD 1 TO counter.
          ENDIF.
        ENDDO.
        IF when_count = 1. IF <if> IS ASSIGNED. <if>-if_ind = els_before. ENDIF. CLEAR <line>-els_before. ENDIF.
      ENDIF.
      IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
        els_before = <line>-ind.
      ELSE.
        CLEAR els_before.
      ENDIF.
    ENDLOOP.
    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL. INSERT ms_if INTO mt_if INDEX 1. ENDIF.
    IF lines( results ) > 0.
      IF results[ lines( results ) ]-arrow IS NOT INITIAL. CLEAR results[ lines( results ) ]-arrow. ENDIF.
    ENDIF.
    CALL METHOD mark_active_root EXPORTING i_calc_path = i_calc_path CHANGING ct_results = results.

    IF i_calc_path = abap_true.
      " -----------------------------------------------------------------------
      " CALC PATH filter: keep only lines that directly participate in a
      " calculation or form the structural call-chain leading to one.
      "
      " A line is kept when it satisfies AT LEAST ONE of these conditions:
      "   (A) active_root = X  — the line IS a calculation/assignment.
      "   (B) subname IS NOT INITIAL AND subname is in the active-subname set
      "       — the line is a call-site whose callee contains calculations.
      "   (C) cond IS NOT INITIAL (IF/ENDIF/LOOP/…)
      "       AND the line's ev_name has at least one active_root sibling
      "       — structural keyword inside an active scope (kept for context).
      "
      " Plain non-calculating nodes (active_root = false, subname empty,
      " cond empty) are removed even when their scope is otherwise active.
      " -----------------------------------------------------------------------

      " 1. Collect ev_names that own at least one active_root line.
      DATA lt_active_ev TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
      LOOP AT results ASSIGNING <line> WHERE active_root = abap_true.
        READ TABLE lt_active_ev WITH KEY table_line = <line>-ev_name TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0. INSERT <line>-ev_name INTO TABLE lt_active_ev. ENDIF.
      ENDLOOP.

      " 2. Build the active-subname set (transitive closure over call-sites).
      "    Seed: every ev_name that is already active is also a reachable subname.
      DATA lt_active_subnames TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
      LOOP AT lt_active_ev INTO DATA(lv_ev).
        READ TABLE lt_active_subnames WITH KEY table_line = lv_ev TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0. INSERT lv_ev INTO TABLE lt_active_subnames. ENDIF.
      ENDLOOP.

      " Expand: if a call-site's subname is already reachable, its caller scope
      " becomes reachable too — repeat until no new entries are added.
      DATA lv_expanded TYPE boolean.
      DO.
        CLEAR lv_expanded.
        LOOP AT results ASSIGNING <line> WHERE subname IS NOT INITIAL AND active_root IS INITIAL.
          READ TABLE lt_active_subnames WITH KEY table_line = <line>-subname TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            READ TABLE lt_active_subnames WITH KEY table_line = <line>-ev_name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              INSERT <line>-ev_name INTO TABLE lt_active_subnames.
              lv_expanded = abap_true.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lv_expanded = abap_false. EXIT. ENDIF.
      ENDDO.

      " 3. Mark irrelevant lines for deletion.
      "    A line without active_root must satisfy (B) or (C) to survive;
      "    otherwise it is a plain grey node that adds no information.
      LOOP AT results ASSIGNING <line> WHERE active_root IS INITIAL.

        " (B) call-site into an active scope — keep.
        IF <line>-subname IS NOT INITIAL.
          READ TABLE lt_active_subnames WITH KEY table_line = <line>-subname TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. CONTINUE. ENDIF.
        ENDIF.

        " (C) structural keyword (IF/LOOP/…) inside an active scope — keep.
        IF <line>-cond IS NOT INITIAL.
          READ TABLE lt_active_ev WITH KEY table_line = <line>-ev_name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. CONTINUE. ENDIF.
        ENDIF.

        " Condition (A) is already excluded by the WHERE clause.
        " None of (B)/(C) matched — remove this irrelevant node.
        <line>-del = abap_true.
      ENDLOOP.
      DELETE results WHERE del = abap_true.

      " 4. Renumber ind sequentially after deletions.
      LOOP AT results ASSIGNING <line>. <line>-ind = sy-tabix. ENDLOOP.
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
    DATA: tree TYPE zcl_ace=>ts_tree, cl_name TYPE string, icon TYPE salv_de_tree_image,
          forms_rel TYPE salv_de_node_key, modules_rel TYPE salv_de_node_key,
          f_modules TYPE salv_de_node_key, func_rel TYPE salv_de_node_key,
          classes_rel TYPE salv_de_node_key, class_rel TYPE salv_de_node_key,
          events_rel TYPE salv_de_node_key, globals_rel TYPE salv_de_node_key,
          enh_rel TYPE salv_de_node_key, locals_rel TYPE salv_de_node_key,
          keyword TYPE zcl_ace=>ts_kword, splits_prg TYPE TABLE OF string, splits_incl TYPE TABLE OF string.
    CONSTANTS: c_lazy_threshold TYPE i VALUE 10.
    IF mo_window->m_prg-include IS INITIAL. mo_window->m_prg-program = mo_window->m_prg-include = mv_prog. ENDIF.
    mo_window->set_program( mo_window->m_prg-include ).
    IF mo_window->m_prg-include <> 'Code_Flow_Mix'. mo_window->show_coverage( ). ENDIF.
    IF mo_window->m_prg-line IS INITIAL AND mo_window->mt_stack IS NOT INITIAL.
      mo_window->m_prg-line = mo_window->mt_stack[ 1 ]-line.
    ENDIF.
    mo_window->set_program_line( 1 ).
    DELETE mo_window->ms_sources-tt_progs WHERE t_keywords IS INITIAL.
    mo_window->show_stack( ). mo_tree_local->clear( ).
    SPLIT mo_window->m_prg-program AT '=' INTO TABLE splits_prg. CHECK splits_prg IS NOT INITIAL.
    cl_gui_cfw=>set_new_ok_code( 'DUMMY' ). cl_gui_cfw=>dispatch( ).
    mo_tree_local->main_node_key = mo_tree_local->add_node(
      i_name = CONV #( mo_window->m_prg-program ) i_icon = CONV #( icon_folder ) i_tree = VALUE #( ) ).

    " --- Includes branch (always lazy, first) ---
    DATA(lv_main_prog) = CONV program( splits_prg[ 1 ] ).
    DATA lv_incl_cnt TYPE i.
    LOOP AT mo_window->ms_sources-tt_progs TRANSPORTING NO FIELDS
      WHERE include <> 'VIRTUAL' AND include <> lv_main_prog.
      lv_incl_cnt += 1.
    ENDLOOP.
    IF lv_incl_cnt > 0.
      DATA(lv_incl_rel) = mo_tree_local->add_node(
        i_name = |Includes ({ lv_incl_cnt })|
        i_icon = CONV #( icon_list )
        i_rel  = mo_tree_local->main_node_key
        i_tree = VALUE #( param = |INCLS:{ mv_prog }| program = mv_prog ) ).
      APPEND lv_incl_rel TO mo_tree_local->mt_lazy_nodes.
    ENDIF.
    LOOP AT mo_window->ms_sources-tt_progs INTO DATA(prog_enh).
      IF prog_enh-enh_collected = abap_false.
        zcl_ace_source_parser=>collect_enhancements( i_program = prog_enh-include io_debugger = me ).
      ENDIF.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = prog_enh-include
        INTO prog_enh.  " перечитываем — enh_blocks теперь заполнены
      LOOP AT prog_enh-tt_enh_blocks INTO DATA(enh_blk).
        IF enh_rel IS INITIAL.
          enh_rel = mo_tree_local->add_node( i_name = 'Enhancements' i_icon = CONV #( icon_folder )
            i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
        ENDIF.
        mo_tree_local->add_node( i_name = |{ enh_blk-enh_name } { enh_blk-ev_type } { enh_blk-ev_name } ({ enh_blk-position })|
          i_icon = CONV #( icon_modify ) i_rel = enh_rel
          i_tree = VALUE #( kind = 'M' value = enh_blk-position include = prog_enh-include program = prog_enh-program
                            ev_type = enh_blk-ev_type ev_name = enh_blk-ev_name param = enh_blk-enh_include enh_id = enh_blk-enh_id ) ).
      ENDLOOP.
    ENDLOOP.
    DATA(lv_gvar_cnt) = 0.
    LOOP AT mo_window->ms_sources-t_vars INTO DATA(var)
      WHERE program = mo_window->m_prg-program AND eventtype IS INITIAL AND class IS INITIAL.
      lv_gvar_cnt += 1.
    ENDLOOP.
    IF lv_gvar_cnt > 0.
      globals_rel = mo_tree_local->add_node( i_name = |Global Vars ({ lv_gvar_cnt })| i_icon = CONV #( icon_header )
        i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = 'GVARS:' program = mo_window->m_prg-program ) ).
      APPEND globals_rel TO mo_tree_local->mt_lazy_nodes.
    ENDIF.
    READ TABLE mt_steps INDEX 1 INTO DATA(first_step).
    IF first_step-line IS NOT INITIAL AND first_step-program = mo_window->m_prg-program
                                      AND lines( splits_prg ) = 1.
      events_rel = mo_tree_local->add_node( i_name = 'Events' i_icon = CONV #( icon_folder )
        i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
      " kind='E', ev_type='EVENT', ev_name = event name — needed for single-event scanner on dblclick
      mo_tree_local->add_node( i_name = 'Code Flow start line' i_icon = CONV #( icon_oo_event ) i_rel = events_rel
        i_tree = VALUE #( kind    = 'E'
                          value   = first_step-line
                          include = first_step-include
                          program = mo_window->m_prg-program
                          ev_type = 'EVENT'
                          ev_name = first_step-eventname ) ).
    ENDIF.
    IF lines( splits_prg ) = 1.
      LOOP AT mo_window->ms_sources-t_events INTO DATA(event) WHERE program = mo_window->m_prg-program.
        IF events_rel IS INITIAL.
          events_rel = mo_tree_local->add_node( i_name = 'Events' i_icon = CONV #( icon_folder )
            i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
        ENDIF.
        " kind='E', ev_type='EVENT', ev_name = event name — needed for single-event scanner on dblclick
        mo_tree_local->add_node( i_name = event-name i_icon = CONV #( icon_oo_event ) i_rel = events_rel
          i_tree = VALUE #( kind    = 'E'
                            include = event-include
                            value   = event-line
                            program = mo_window->m_prg-program
                            ev_type = 'EVENT'
                            ev_name = event-name ) ).
      ENDLOOP.
    ENDIF.
    LOOP AT mo_window->ms_sources-tt_progs INTO DATA(prog) WHERE program+0(4) = 'SAPL'.
      DATA: fname TYPE rs38l_fnam, exception_list TYPE TABLE OF rsexc, export_parameter TYPE TABLE OF rsexp,
            import_parameter TYPE TABLE OF rsimp, changing_parameter TYPE TABLE OF rscha,
            tables_parameter TYPE TABLE OF rstbl, incl_nr TYPE includenr.
      DATA(len) = strlen( prog-include ) - 2. incl_nr = prog-include+len(2).
      SELECT SINGLE funcname INTO @DATA(funcname) FROM tfdir WHERE pname = @prog-program AND include = @incl_nr.
      CHECK sy-subrc = 0.
      IF f_modules IS INITIAL.
        f_modules = mo_tree_local->add_node( i_name = 'Function Modules' i_icon = CONV #( icon_folder )
          i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
      ENDIF.
      func_rel = mo_tree_local->add_node( i_name = CONV #( funcname ) i_icon = CONV #( icon_folder )
        i_rel = f_modules i_tree = VALUE #( ) ).
      fname = funcname.
      CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE' EXPORTING funcname = fname
        TABLES exception_list = exception_list export_parameter = export_parameter
               import_parameter = import_parameter changing_parameter = changing_parameter
               tables_parameter = tables_parameter EXCEPTIONS OTHERS = 4.
      IF sy-subrc = 0.
        LOOP AT import_parameter   INTO DATA(imp).    mo_tree_local->add_node( i_name = CONV #( imp-parameter    ) i_icon = CONV #( icon_parameter_import   ) i_rel = func_rel ). ENDLOOP.
        LOOP AT export_parameter   INTO DATA(exp).    mo_tree_local->add_node( i_name = CONV #( exp-parameter    ) i_icon = CONV #( icon_parameter_export   ) i_rel = func_rel ). ENDLOOP.
        LOOP AT mo_window->ms_sources-t_params INTO DATA(change). mo_tree_local->add_node( i_name = CONV #( change-param ) i_icon = CONV #( icon_parameter_changing ) i_rel = func_rel ). ENDLOOP.
        LOOP AT tables_parameter   INTO DATA(table_p). mo_tree_local->add_node( i_name = CONV #( table_p-parameter ) i_icon = CONV #( icon_parameter_table   ) i_rel = func_rel ). ENDLOOP.
      ENDIF.
    ENDLOOP.
    IF lines( splits_prg ) = 1.
      " Считаем локальные классы и интерфейсы через tt_class_defs.
      " Фильтр по program = mv_prog — только объекты этой программы.
      DATA(lv_cls_cnt)  = 0.
      DATA(lv_intf_cnt) = 0.
      LOOP AT mo_window->ms_sources-tt_class_defs INTO DATA(ls_cd)
        WHERE class   <> splits_prg[ 1 ]
          AND program =  mv_prog.
        IF ls_cd-is_intf = abap_true.
          lv_intf_cnt += 1.
        ELSE.
          lv_cls_cnt  += 1.
        ENDIF.
      ENDLOOP.
      DATA(lv_loc_cnt) = lv_cls_cnt + lv_intf_cnt.
      IF lv_loc_cnt <= c_lazy_threshold.
        locals_rel = build_local_classes_node( i_program = CONV #( mv_prog ) i_excl_class = splits_prg[ 1 ]
          i_refnode = mo_tree_local->main_node_key ).
      ELSE.
        IF lv_cls_cnt > 0.
          locals_rel = mo_tree_local->add_node( i_name = |Local Classes ({ lv_cls_cnt })| i_icon = CONV #( icon_folder )
            i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = |LCLASSES:{ mv_prog }| program = mv_prog ) ).
          APPEND locals_rel TO mo_tree_local->mt_lazy_nodes.
        ENDIF.
        IF lv_intf_cnt > 0.
          DATA(lv_intf_rel) = mo_tree_local->add_node( i_name = |Interfaces ({ lv_intf_cnt })| i_icon = CONV #( icon_oo_connection )
            i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = |LINTFS:{ mv_prog }| program = mv_prog ) ).
          APPEND lv_intf_rel TO mo_tree_local->mt_lazy_nodes.
        ENDIF.
      ENDIF.
    ENDIF.
    IF class_rel IS INITIAL. classes_rel = class_rel = mo_tree_local->main_node_key. ENDIF.
    SORT mo_window->ms_sources-tt_calls_line BY program class eventtype meth_type eventname.
    cl_name = splits_prg[ 1 ].
    READ TABLE mo_window->ms_sources-tt_class_defs WITH KEY class = CONV #( cl_name )
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
    DO.
      READ TABLE mo_window->ms_sources-t_classes WITH KEY clsname = cl_name reltype = '2' INTO DATA(ls_class).
      IF sy-subrc <> 0. EXIT. ENDIF. cl_name = ls_class-refclsname.
    ENDDO.
    DO.
      DATA lv_show_def_inc  TYPE program.
      DATA lv_show_def_line TYPE i.
      LOOP AT mo_window->ms_sources-tt_progs INTO DATA(lv_dp).
        LOOP AT lv_dp-t_keywords INTO DATA(lv_dk) WHERE name = 'CLASS'.
          LOOP AT lv_dp-scan->statements INTO DATA(lv_ds).
            READ TABLE lv_dp-scan->tokens INDEX lv_ds-from INTO DATA(lv_dt0).
            IF sy-subrc = 0 AND lv_dt0-row = lv_dk-line AND lv_dt0-str = 'CLASS'.
              READ TABLE lv_dp-scan->tokens INDEX lv_ds-from + 1 INTO DATA(lv_dt1).
              IF sy-subrc = 0 AND lv_dt1-str = cl_name.
                READ TABLE lv_dp-scan->tokens INDEX lv_ds-from + 2 INTO DATA(lv_dt2).
                IF sy-subrc = 0 AND lv_dt2-str = 'DEFINITION'.
                  READ TABLE lv_dp-scan->tokens INDEX lv_ds-from + 3 INTO DATA(lv_dt3).
                  IF NOT ( sy-subrc = 0 AND lv_dt3-str = 'DEFERRED' ).
                    lv_show_def_inc  = lv_dp-include.
                    lv_show_def_line = COND i( WHEN lv_dk-v_line > 0 THEN lv_dk-v_line ELSE lv_dk-line ).
                  ENDIF.
                ENDIF.
              ENDIF.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF lv_show_def_inc IS NOT INITIAL. EXIT. ENDIF.
        ENDLOOP.
        IF lv_show_def_inc IS NOT INITIAL. EXIT. ENDIF.
      ENDLOOP.
      add_class( i_class = cl_name i_refnode = classes_rel
                 i_tree = VALUE #( kind    = 'C'
                                   include = lv_show_def_inc
                                   value   = lv_show_def_line ) ).
      READ TABLE mo_window->ms_sources-t_classes WITH KEY refclsname = cl_name reltype = '2' INTO ls_class.
      IF sy-subrc <> 0. EXIT. ENDIF. cl_name = ls_class-clsname.
    ENDDO.
    ENDIF.
    DATA(lv_form_cnt) = 0. DATA(lv_mod_cnt) = 0.
    LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs2).
      IF subs2-eventtype = 'FORM'   AND subs2-program = splits_prg[ 1 ]. lv_form_cnt += 1. ENDIF.
      IF subs2-eventtype = 'MODULE' AND subs2-program = splits_prg[ 1 ]. lv_mod_cnt  += 1. ENDIF.
    ENDLOOP.
    IF lv_form_cnt > 0.
      IF lv_form_cnt <= c_lazy_threshold.
        LOOP AT mo_window->ms_sources-tt_calls_line INTO subs2 WHERE eventtype = 'FORM' AND program = splits_prg[ 1 ].
          IF forms_rel IS INITIAL.
            forms_rel = mo_tree_local->add_node( i_name = 'Subroutines' i_icon = CONV #( icon_folder )
              i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
          ENDIF.
          SPLIT subs2-include AT '=' INTO TABLE splits_incl.
          READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs2-include INTO prog.
          READ TABLE prog-t_keywords WITH KEY index = subs2-index INTO keyword.
          DATA(event_node) = mo_tree_local->add_node( i_name = subs2-eventname i_icon = CONV #( icon_biw_info_source_ina )
            i_rel = forms_rel i_tree = VALUE #( kind = 'M' value = keyword-v_line include = subs2-include
                                                program = subs2-program ev_type = subs2-eventtype ev_name = subs2-eventname ) ).
          LOOP AT mo_window->ms_sources-t_params INTO DATA(param) WHERE event = 'FORM' AND name = subs2-eventname AND param IS NOT INITIAL.
            CASE param-type. WHEN 'I'. icon = icon_parameter_import. WHEN 'E'. icon = icon_parameter_export. WHEN OTHERS. icon = icon_parameter_changing. ENDCASE.
            mo_tree_local->add_node( i_name = param-param i_icon = icon i_rel = event_node i_tree = VALUE #( param = param-param ) ).
          ENDLOOP.
          DATA(lv_form_var_cnt) = 0.
          LOOP AT mo_window->ms_sources-t_vars INTO DATA(form_var)
            WHERE program = subs2-program AND eventtype = 'FORM' AND eventname = subs2-eventname.
            lv_form_var_cnt += 1.
          ENDLOOP.
          IF lv_form_var_cnt > 0.
            DATA(form_locals_rel) = mo_tree_local->add_node( i_name = |Local vars ({ lv_form_var_cnt })| i_icon = CONV #( icon_header )
              i_rel = event_node i_tree = VALUE #( param = |LVARS:FORM:{ subs2-eventname }| program = subs2-program ) ).
            APPEND form_locals_rel TO mo_tree_local->mt_lazy_nodes.
          ENDIF.
        ENDLOOP.
      ELSE.
        forms_rel = mo_tree_local->add_node( i_name = |Subroutines ({ lv_form_cnt })| i_icon = CONV #( icon_folder )
          i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = |FORMS:{ splits_prg[ 1 ] }| program = splits_prg[ 1 ] ) ).
        APPEND forms_rel TO mo_tree_local->mt_lazy_nodes.
      ENDIF.
    ENDIF.
    IF lv_mod_cnt > 0.
      IF lv_mod_cnt <= c_lazy_threshold.
        LOOP AT mo_window->ms_sources-tt_calls_line INTO subs2 WHERE eventtype = 'MODULE' AND program = splits_prg[ 1 ].
          IF modules_rel IS INITIAL.
            modules_rel = mo_tree_local->add_node( i_name = 'Modules' i_icon = CONV #( icon_folder )
              i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
          ENDIF.
          SPLIT subs2-include AT '=' INTO TABLE splits_incl.
          READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs2-include INTO prog.
          READ TABLE prog-t_keywords WITH KEY index = subs2-index INTO keyword.
          mo_tree_local->add_node( i_name = subs2-eventname i_icon = CONV #( icon_biw_info_source_ina )
            i_rel = modules_rel i_tree = VALUE #( kind = 'M' value = keyword-v_line include = subs2-include
                                                  program = subs2-program ev_type = subs2-eventtype ev_name = subs2-eventname ) ).
        ENDLOOP.
      ELSE.
        modules_rel = mo_tree_local->add_node( i_name = |Modules ({ lv_mod_cnt })| i_icon = CONV #( icon_folder )
          i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = |MODS:{ splits_prg[ 1 ] }| program = splits_prg[ 1 ] ) ).
        APPEND modules_rel TO mo_tree_local->mt_lazy_nodes.
      ENDIF.
    ENDIF.
    mo_tree_local->display( ).
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

  SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE TEXT-004.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT (29) TEXT-002 FOR FIELD p_prog.
      SELECTION-SCREEN POSITION 33.
      PARAMETERS: p_prog  TYPE progname MATCHCODE OBJECT progname MODIF ID prg.
      SELECTION-SCREEN COMMENT (70) TEXT-001 FOR FIELD p_prog.
    SELECTION-SCREEN END OF LINE.
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

  AT SELECTION-SCREEN.
    CHECK sy-ucomm <> 'DUMMY'.

    IF p_odata IS NOT INITIAL.
      DATA(serv) = p_odata && '_SRV'.

      SELECT SINGLE class_name INTO p_class
        FROM /iwbep/i_mgw_srh WHERE technical_name = serv.
    ENDIF.

    IF p_wdc IS NOT INITIAL.
      p_class = cl_wdy_wb_naming_service=>get_classname_for_component( p_component = CONV #( p_wdc ) ).
    ENDIF.

    IF p_class IS NOT INITIAL.
      SELECT SINGLE clstype INTO @DATA(clstype)
        FROM seoclass
       WHERE clsname = @p_class.
      IF sy-subrc = 0.

        p_prog = p_class && repeat( val = `=` occ = 30 - strlen( p_class ) ).
        IF clstype = '1'.
          p_prog = p_prog && 'IP'.
        ELSE.
          p_prog = p_prog && 'CP'.
        ENDIF.
      ENDIF.

    ENDIF.

    IF p_func IS NOT INITIAL.
      SELECT SINGLE pname, include INTO ( @DATA(func_incl), @DATA(incl_num) )
        FROM tfdir
       WHERE funcname = @p_func.

      IF sy-subrc = 0.
        SHIFT func_incl LEFT BY 3 PLACES.
        p_prog = func_incl && 'U' && incl_num.
      ENDIF.

    ENDIF.

    CHECK sy-ucomm IS INITIAL.
    SELECT COUNT( * ) FROM reposrc WHERE progname = p_prog.

    IF sy-dbcnt <> 0.
      DATA(gv_ace) = NEW zcl_ace( i_prog = p_prog i_new_parser = n_parser i_show_parse_time = n_time ).
    ELSE.
      MESSAGE 'Program is not found' TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.

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

****************************************************
INTERFACE lif_abapmerge_marker.
* abapmerge 0.16.7 - 2026-03-29T14:23:05.810Z
  CONSTANTS c_merge_timestamp TYPE string VALUE `2026-03-29T14:23:05.810Z`.
  CONSTANTS c_abapmerge_version TYPE string VALUE `0.16.7`.
ENDINTERFACE.
****************************************************
