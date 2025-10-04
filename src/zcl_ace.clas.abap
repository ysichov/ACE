CLASS zcl_ace DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_obj,
        name TYPE string,
        obj  TYPE string,
      END OF t_obj .
    TYPES:
      BEGIN OF t_sel_var,
        name   TYPE string,
        is_sel TYPE xfeld,
        refval TYPE REF TO data,
      END OF t_sel_var .

    DATA:
      mt_obj            TYPE TABLE OF t_obj .
    DATA:
      mt_compo          TYPE TABLE OF scompo .
    DATA mt_locals TYPE tpda_scr_locals_it .
    DATA mt_globals TYPE tpda_scr_globals_it .
    DATA mt_ret_exp TYPE tpda_scr_locals_it .
    DATA m_counter TYPE i .
    DATA:
      mt_steps          TYPE  TABLE OF zcl_ace_appl=>t_step_counter .
    DATA:
      mt_var_step       TYPE  TABLE OF zcl_ace_appl=>var_table_h .
    DATA m_step TYPE i .
    DATA m_is_find TYPE xfeld .
    DATA m_stop_stack TYPE i .
    DATA m_debug TYPE x .
    DATA m_refresh TYPE xfeld .
    DATA m_update TYPE xfeld .
    DATA is_step TYPE xfeld .
    DATA ms_stack_prev TYPE zcl_ace_appl=>t_stack .
    DATA ms_stack TYPE zcl_ace_appl=>t_stack .
    DATA is_history TYPE xfeld .
    DATA m_hist_step TYPE i .
    DATA m_step_delta TYPE i .
    DATA:
      mt_vars_hist_view TYPE STANDARD TABLE OF zcl_ace_appl=>var_table .
    DATA:
      mt_vars_hist      TYPE STANDARD TABLE OF zcl_ace_appl=>var_table .
    DATA:
      mt_state          TYPE STANDARD TABLE OF zcl_ace_appl=>var_table .
    DATA mv_recurse TYPE i .
    DATA:
      mt_classes_types  TYPE TABLE OF zcl_ace_appl=>t_classes_types .
    DATA mo_window TYPE REF TO zcl_ace_window .
    DATA mv_f7_stop TYPE xfeld .
    DATA m_f6_level TYPE i .
    DATA m_target_stack TYPE i .
    DATA mo_tree_local TYPE REF TO zcl_ace_rtti_tree .
    DATA:
      mt_selected_var   TYPE TABLE OF t_sel_var .
    DATA mv_stack_changed TYPE xfeld .
    DATA m_variable TYPE REF TO data .
    DATA:
      mt_new_string     TYPE TABLE OF  string .
    DATA m_quick TYPE tpda_scr_quick_info .
    DATA:
      mr_statements     TYPE RANGE OF string .
    DATA mv_prog TYPE program .
    DATA mv_dest TYPE text255 .
    DATA mv_model TYPE text255 .
    DATA mv_apikey TYPE text255 .

    METHODS constructor
      IMPORTING
        !iv_prog   TYPE prog
        !iv_dest   TYPE text255
        !iv_model  TYPE text255
        !iv_apikey TYPE text255 .
    METHODS hndl_script_buttons
      IMPORTING
        !iv_stack_changed TYPE xfeld
      RETURNING
        VALUE(rv_stop)    TYPE xfeld .
    METHODS show .
protected section.
PRIVATE SECTION.

  CONSTANTS:
    BEGIN OF c_kind,
      struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
      table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
      elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
      class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
      intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
      ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
    END OF c_kind .
ENDCLASS.



CLASS ZCL_ACE IMPLEMENTATION.


  METHOD constructor.

    CONSTANTS: c_mask TYPE x VALUE '01'.

    mv_prog = iv_prog.
    mv_dest = iv_dest.
    mv_model = iv_model.
    mv_apikey = iv_apikey.

    zcl_ace_appl=>init_lang( ).
    zcl_ace_appl=>init_icons_table( ).

    is_step = abap_on.
    zcl_ace_appl=>check_mermaid( ).
    zcl_ace_appl=>init_lang( ).
    zcl_ace_appl=>init_icons_table( ).

    mo_window = NEW zcl_ace_window( me ).


    mo_tree_local = NEW zcl_ace_rtti_tree( i_header   = 'Objects & Code Flow'
                                       i_type     = 'L'
                                       i_cont     = mo_window->mo_locals_container
                                       i_debugger = me ).
    show( ).

  ENDMETHOD.


  METHOD hndl_script_buttons.

    IF m_is_find = abap_true.
      rv_stop = abap_true.
      CLEAR m_is_find.
      RETURN.
    ENDIF.

    IF mo_window->m_debug_button = 'F5'.
      rv_stop = abap_true.

    ELSEIF mo_window->m_debug_button = 'F6'.
      IF m_f6_level IS NOT INITIAL AND m_f6_level = ms_stack-stacklevel OR mo_window->m_history IS INITIAL.
        CLEAR m_f6_level.
        rv_stop = abap_true.
      ENDIF.

    ELSEIF mo_window->m_debug_button = 'F6END'.
      IF mo_window->m_prg-flag_eoev IS NOT INITIAL AND m_target_stack = ms_stack-stacklevel.
        rv_stop = abap_true.
      ENDIF.
    ELSEIF mo_window->m_debug_button = 'F7'.

      IF m_target_stack = ms_stack-stacklevel.
        CLEAR m_target_stack.
        rv_stop = abap_true.
      ENDIF.

    ELSEIF mo_window->m_debug_button IS NOT INITIAL.
      READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = mo_window->m_prg-include linesrc = mo_window->m_prg-line INTO DATA(gs_break).
      IF sy-subrc = 0.
        rv_stop = abap_true.
      ELSE.

        IF mo_window->m_debug_button = 'F6BEG' AND m_target_stack = ms_stack-stacklevel.
          rv_stop = abap_true.
        ELSE.
          IF mo_window->m_history IS NOT INITIAL.
            IF ms_stack-stacklevel = mo_window->m_hist_depth +  mo_window->m_start_stack.
              "f6( )."to refactor
            ELSE.
              "f5( )."to refactor
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      rv_stop = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD show.

    IF  mo_window->m_prg-include IS INITIAL.
      mo_window->m_prg-program =  mo_window->m_prg-include = mv_prog.
    ENDIF.
    mo_window->set_program(  mo_window->m_prg-include ).
    mo_window->show_coverage( ).
    mo_window->set_program_line( mo_window->m_prg-line ).

    mo_window->show_stack( ).
    mo_tree_local->clear( ).
    mo_tree_local->main_node_key = mo_tree_local->add_node( iv_name = CONV #( mv_prog ) iv_icon = CONV #( icon_folder ) ).

    mo_tree_local->add_node( iv_name = 'Local Classes' iv_icon = CONV #( icon_folder ) iv_rel = mo_tree_local->main_node_key ).
    mo_tree_local->add_node( iv_name = 'Global Fields' iv_icon = CONV #( icon_header ) iv_rel = mo_tree_local->main_node_key ).
    mo_tree_local->add_node( iv_name = 'Events' iv_icon = CONV #( icon_oo_event ) iv_rel = mo_tree_local->main_node_key ).
    DATA(lv_forms_rel) = mo_tree_local->add_node( iv_name = 'Subroutines' iv_icon = CONV #( icon_folder ) iv_rel = mo_tree_local->main_node_key ).
    mo_tree_local->add_node( iv_name = 'Code Flow' iv_icon = CONV #( icon_enhanced_bo ) iv_rel = mo_tree_local->main_node_key ).

    LOOP AT mo_window->ms_sources-t_params INTO DATA(ls_subs) WHERE event = 'FORM' .
      DATA(lv_form_name) = ls_subs-name.
      AT NEW name.
        mo_tree_local->add_node( iv_name = lv_form_name iv_icon = CONV #( icon_biw_info_source_ina ) iv_rel = lv_forms_rel ).

      ENDAT.
    ENDLOOP.

    mo_tree_local->display( ).


  ENDMETHOD.
ENDCLASS.
