CLASS zcl_ace_tree_builder DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_window TYPE REF TO zcl_ace_window
        io_tree   TYPE REF TO zcl_ace_rtti_tree.
    METHODS build.

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

CLASS zcl_ace_tree_builder IMPLEMENTATION.
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
        lv_intf_var_cnt += 1.
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
        lv_si += 1.
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
          i_tree = VALUE #( value = lv_p-line include = lv_p-include var_name = lv_p-param ) ).
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
        lv_var_cnt += 1.
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
    DATA(lv_main_prog) = i_prog.
    DATA lv_cnt TYPE i.
    LOOP AT mo_window->ms_sources-tt_progs TRANSPORTING NO FIELDS
      WHERE include <> 'VIRTUAL' AND include <> lv_main_prog.
      lv_cnt += 1.
    ENDLOOP.
    CHECK lv_cnt > 0.
    DATA(lv_node) = mo_tree->add_node(
      i_name = |Includes ({ lv_cnt })|
      i_icon = CONV #( icon_list )
      i_rel  = i_root_key
      i_tree = VALUE #( param = |INCLS:{ i_prog }| program = i_prog ) ).
    APPEND lv_node TO mo_tree->mt_lazy_nodes.
  ENDMETHOD.

  METHOD show_tree_enhancements.
    DATA lv_enh_rel TYPE salv_de_node_key.
    LOOP AT mo_window->ms_sources-tt_progs INTO DATA(prog_enh).
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
      lv_cnt += 1.
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
    LOOP AT mo_window->ms_sources-tt_progs INTO DATA(prog) WHERE program+0(4) = 'SAPL'.
      DATA(len) = strlen( prog-include ) - 2.
      incl_nr = prog-include+len(2).
      SELECT SINGLE funcname INTO @DATA(funcname) FROM tfdir WHERE pname = @prog-program AND include = @incl_nr.
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
        lv_intf_cnt += 1.
      ELSE.
        lv_cls_cnt += 1.
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
      lv_form_cnt += 1.
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
        mo_tree->add_node( i_name = param-param i_icon = lv_icon i_rel = lv_ev_node i_tree = VALUE #( param = param-param ) ).
      ENDLOOP.
      DATA lv_var_cnt TYPE i.
      CLEAR lv_var_cnt.
      LOOP AT mo_window->ms_sources-t_vars TRANSPORTING NO FIELDS
        WHERE program = subs-program AND eventtype = 'FORM' AND eventname = subs-eventname.
        lv_var_cnt += 1.
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
      lv_mod_cnt += 1.
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
