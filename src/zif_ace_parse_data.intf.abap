INTERFACE zif_ace_parse_data PUBLIC.

  " --- calls (used in ts_kword) ---
  TYPES:
    BEGIN OF ts_param_binding,
      outer TYPE string,
      inner TYPE string,
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
      index       TYPE i,
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
      stmnt_from TYPE string,
      stmnt_to   TYPE string,
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
