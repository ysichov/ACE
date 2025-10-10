CLASS zcl_ace_rtti_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_classes_leaf,
        name TYPE string,
        type TYPE char1,
        key  TYPE salv_de_node_key,
      END OF t_classes_leaf .
    TYPES:
      BEGIN OF ts_table,
        ref      TYPE REF TO data,
        kind(1),
        value    TYPE string,
        typename TYPE abap_abstypename,
        fullname TYPE string,
        path     TYPE string,
      END OF ts_table .
    TYPES:
      tt_table TYPE STANDARD TABLE OF ts_table
            WITH NON-UNIQUE DEFAULT KEY .

    DATA main_node_key TYPE salv_de_node_key .
    DATA m_refresh TYPE xfeld .
    DATA m_leaf TYPE string .
    DATA m_hide TYPE x .
    DATA m_clear TYPE flag .
    DATA m_locals TYPE x .
    DATA m_globals TYPE x .
    DATA m_syst TYPE x .
    DATA m_class_data TYPE x .
    DATA m_ldb TYPE x .
    DATA m_locals_key TYPE salv_de_node_key .
    DATA m_globals_key TYPE salv_de_node_key .
    DATA m_class_key TYPE salv_de_node_key .
    DATA m_syst_key TYPE salv_de_node_key .
    DATA m_ldb_key TYPE salv_de_node_key .
    DATA m_icon TYPE salv_de_tree_image .
    DATA:
      mt_vars         TYPE STANDARD TABLE OF zcl_ace_appl=>var_table .
    DATA:
      mt_classes_leaf TYPE TABLE OF t_classes_leaf .
    DATA m_prg_info TYPE tpda_scr_prg_info .
    DATA mo_viewer TYPE REF TO zcl_ace .
    DATA tree TYPE REF TO cl_salv_tree .

    METHODS constructor
      IMPORTING
        !i_header   TYPE clike DEFAULT 'View'
        !i_type     TYPE xfeld OPTIONAL
        !i_cont     TYPE REF TO cl_gui_container OPTIONAL
        !i_debugger TYPE REF TO zcl_ace OPTIONAL .
    METHODS del_variable
      IMPORTING
        !iv_full_name TYPE string
        !i_state      TYPE xfeld OPTIONAL .
    METHODS clear .
    METHODS add_buttons
      IMPORTING
        !iv_type TYPE xfeld .
    METHODS add_node
      IMPORTING
        !iv_name       TYPE string
        !iv_rel        TYPE salv_de_node_key OPTIONAL
        !iv_icon       TYPE salv_de_tree_image OPTIONAL
      RETURNING
        VALUE(rv_node) TYPE salv_de_node_key .
    METHODS add_obj_nodes
      IMPORTING
        !is_var           TYPE zcl_ace_appl=>var_table
      RETURNING
        VALUE(e_root_key) TYPE salv_de_node_key .
    METHODS delete_node
      IMPORTING
        !iv_key TYPE salv_de_node_key .
    METHODS display
      IMPORTING
        !io_debugger TYPE REF TO zcl_ace OPTIONAL .
    METHODS traverse
      IMPORTING
        !io_type_descr    TYPE REF TO cl_abap_typedescr
        !iv_parent_key    TYPE salv_de_node_key
        !iv_rel           TYPE salv_de_node_relation
        !is_var           TYPE zcl_ace_appl=>var_table
        !ir_up            TYPE REF TO data OPTIONAL
        !iv_parent_name   TYPE string OPTIONAL
        !iv_struc_name    TYPE string OPTIONAL
      RETURNING
        VALUE(e_root_key) TYPE salv_de_node_key .
    METHODS traverse_struct
      IMPORTING
        !io_type_descr    TYPE REF TO cl_abap_typedescr
        !iv_parent_key    TYPE salv_de_node_key
        !iv_rel           TYPE salv_de_node_relation
        !is_var           TYPE zcl_ace_appl=>var_table
        !ir_up            TYPE REF TO data OPTIONAL
        !iv_parent_name   TYPE string OPTIONAL
        !iv_struc_name    TYPE string OPTIONAL
      RETURNING
        VALUE(e_root_key) TYPE salv_de_node_key .
    METHODS traverse_elem
      IMPORTING
        !io_type_descr    TYPE REF TO cl_abap_typedescr
        !iv_parent_key    TYPE salv_de_node_key
        !iv_rel           TYPE salv_de_node_relation
        !is_var           TYPE zcl_ace_appl=>var_table
        !iv_value         TYPE any OPTIONAL
        !iv_parent_name   TYPE string OPTIONAL
      RETURNING
        VALUE(e_root_key) TYPE salv_de_node_key .
    METHODS traverse_obj
      IMPORTING
        !iv_parent_key    TYPE salv_de_node_key
        !iv_rel           TYPE salv_de_node_relation
        !is_var           TYPE zcl_ace_appl=>var_table
        !iv_value         TYPE any OPTIONAL
        !ir_up            TYPE REF TO data OPTIONAL
        !iv_parent_name   TYPE string OPTIONAL
      RETURNING
        VALUE(e_root_key) TYPE salv_de_node_key .
    METHODS traverse_table
      IMPORTING
        !io_type_descr    TYPE REF TO cl_abap_typedescr
        !iv_parent_key    TYPE salv_de_node_key
        !iv_rel           TYPE salv_de_node_relation
        !is_var           TYPE zcl_ace_appl=>var_table
        !ir_up            TYPE REF TO data OPTIONAL
        !iv_parent_name   TYPE string OPTIONAL
      RETURNING
        VALUE(e_root_key) TYPE salv_de_node_key .
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
  methods HNDL_USER_COMMAND
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
ENDCLASS.



CLASS ZCL_ACE_RTTI_TREE IMPLEMENTATION.


  method ADD_BUTTONS.


    DATA(lo_functions) = tree->get_functions( ).
    lo_functions->set_all( ).

    lo_functions->set_group_layout( abap_false ).
    lo_functions->set_group_aggregation( abap_false ).
    lo_functions->set_group_print( abap_false ).

    CHECK mo_viewer IS NOT INITIAL AND iv_type = 'L'.

    lo_functions->add_function(
      name     = 'REFRESH'
      icon     = CONV #( icon_refresh )
      text     = ''
      tooltip  = 'Refresh'
      position = if_salv_c_function_position=>left_of_salv_functions ).


  endmethod.


  method ADD_NODE.


    rv_node =
          tree->get_nodes( )->add_node(
            related_node   = iv_rel
            collapsed_icon = iv_icon
            expanded_icon = iv_icon
            relationship   = if_salv_c_node_relation=>last_child
            row_style = if_salv_c_tree_style=>intensified
            text           = CONV #( iv_name )
            folder         = abap_true
          )->get_key( ).


  endmethod.


  method ADD_OBJ_NODES.


    DATA lt_match TYPE match_result_tab.
    FIND ALL OCCURRENCES OF  '-' IN is_var-name RESULTS lt_match. "Only first level of instance should be here
    IF lines( lt_match ) > 1.
      RETURN.
    ENDIF.

    DATA lv_text TYPE lvc_value.
    DATA lv_node_key TYPE salv_de_node_key.
    DATA lv_icon TYPE salv_de_tree_image.

    CASE is_var-cl_leaf.
      WHEN 1.
        lv_icon = icon_led_green.
        lv_text = 'Public'.
      WHEN 2.
        lv_icon = icon_led_red.
        lv_text = 'Private'.
      WHEN 3.
        lv_icon = icon_led_yellow.
        lv_text = 'Protected'.
    ENDCASE.

    READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf ASSIGNING FIELD-SYMBOL(<class>).
    IF sy-subrc NE 0.

      READ TABLE mt_vars WITH KEY path = is_var-parent INTO DATA(ls_var).
      lv_node_key =
        tree->get_nodes( )->add_node(
          related_node   = ls_var-key
          relationship   = if_salv_c_node_relation=>last_child
          collapsed_icon = lv_icon
          expanded_icon  = lv_icon
          text           = lv_text
          folder         = abap_true
        )->get_key( ).

      APPEND INITIAL LINE TO mt_classes_leaf ASSIGNING <class>.
      <class>-name = is_var-parent.
      <class>-key = lv_node_key.
      <class>-type = is_var-cl_leaf.
    ENDIF.


  endmethod.


  method CLEAR.


    tree->get_nodes( )->delete_all( ).

    CLEAR: m_globals_key,
           m_locals_key,
           m_syst_key,
           m_ldb_key,
           m_class_key,
           mt_vars,
           mt_classes_leaf.


  endmethod.


  method CONSTRUCTOR.

    super->constructor( ).
    mo_viewer = i_debugger.

    cl_salv_tree=>factory(
      EXPORTING
        r_container = i_cont
      IMPORTING
        r_salv_tree = tree
      CHANGING
        t_table     = tree_table ).

    DATA(lo_setting) =  tree->get_tree_settings( ).
    lo_setting->set_hierarchy_header( i_header ).
    lo_setting->set_hierarchy_size( 30 ).
    lo_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

    DATA(lo_columns) = tree->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    lo_columns->get_column( 'VALUE' )->set_short_text( 'Value' ).
    lo_columns->get_column( 'FULLNAME' )->set_visible( abap_false ).
    lo_columns->get_column( 'PATH' )->set_visible( abap_false ).
    lo_columns->get_column( 'TYPENAME' )->set_short_text( 'Type' ).
    lo_columns->get_column( 'TYPENAME' )->set_medium_text( 'Absolute Type' ).

    add_buttons( i_type ).

    DATA(lo_event) = tree->get_event( ) .
    SET HANDLER hndl_double_click
                hndl_user_command FOR lo_event.

    m_globals = '01'.
    tree->display( ).


  endmethod.


  method DELETE_NODE.


    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(l_node) =  lo_nodes->get_node( iv_key ).
    IF l_node IS NOT INITIAL.
      l_node->delete( ).

    ENDIF.


  endmethod.


  method DEL_VARIABLE.


    DATA(lt_hist) = mo_viewer->mt_vars_hist.
    SORT lt_hist BY step DESCENDING.
    LOOP AT lt_hist INTO DATA(ls_hist) WHERE name = iv_full_name.
      IF ls_hist-del IS INITIAL.
        CLEAR: ls_hist-ref, ls_hist-first.
        ls_hist-del = abap_true.
        ls_hist-step = mo_viewer->m_hist_step - 1.
        INSERT ls_hist INTO mo_viewer->mt_vars_hist INDEX 1.
      ENDIF.
    ENDLOOP.

    DATA(lo_nodes) = tree->get_nodes( ).
    READ TABLE mo_viewer->mt_state WITH KEY name = iv_full_name ASSIGNING FIELD-SYMBOL(<var>).
    IF sy-subrc = 0.

      TRY.
          DATA(l_node) =  lo_nodes->get_node( <var>-key ).
        CATCH cx_salv_msg.
      ENDTRY.

      DELETE mt_vars WHERE name = iv_full_name.
      DELETE mt_classes_leaf WHERE name = iv_full_name.
      IF i_state = abap_true.
        DELETE mo_viewer->mt_state WHERE name = iv_full_name.
      ENDIF.

      DATA(l_nam) = iv_full_name && '-'.
      DELETE mt_vars WHERE name CS l_nam.
      DELETE mt_classes_leaf WHERE name  CS l_nam.
      IF i_state = abap_true.
        DELETE mo_viewer->mt_state WHERE name CS l_nam.
      ENDIF.
      TRY.
          IF l_node IS NOT INITIAL.
            l_node->delete( ).
          ENDIF.
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.


  endmethod.


  method DISPLAY.


    DATA(lo_columns) = tree->get_columns( ).
    lo_columns->get_column( 'KIND' )->set_visible( abap_false ).

    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(lt_nodes) =  lo_nodes->get_all_nodes( ).


    DATA lt_sub TYPE salv_t_nodes.
    LOOP AT lt_nodes INTO DATA(l_node).
      READ TABLE lt_sub WITH KEY node = l_node-node TRANSPORTING NO FIELDS. "expanding only first level nodes.
      IF sy-subrc NE 0.
        TRY.
            l_node-node->expand( ).
            lt_sub = l_node-node->get_subtree( ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.
    ENDLOOP.
    tree->display( ).


  endmethod.


  method HNDL_DOUBLE_CLICK.


    DATA(lo_nodes) = tree->get_nodes( ).
    DATA(l_node) =  lo_nodes->get_node( node_key ).
    DATA r_row TYPE REF TO data.

    r_row = l_node->get_data_row( ).
    ASSIGN r_row->* TO FIELD-SYMBOL(<row>).
    ASSIGN COMPONENT 'REF' OF STRUCTURE <row> TO FIELD-SYMBOL(<ref>).
    ASSIGN COMPONENT 'KIND' OF STRUCTURE <row> TO FIELD-SYMBOL(<kind>).
    ASSIGN COMPONENT 'FULLNAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<fullname>).
    ASSIGN COMPONENT 'PATH' OF STRUCTURE <row> TO FIELD-SYMBOL(<path>).

    IF <fullname> IS NOT INITIAL.
      READ TABLE mo_viewer->mt_selected_var WITH KEY name =  <fullname> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE mo_viewer->mt_selected_var WHERE name = <fullname>.
        l_node->set_row_style( if_salv_c_tree_style=>default ).
      ELSE.
        l_node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
        APPEND INITIAL LINE TO mo_viewer->mt_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
        <sel>-name = <fullname>.
        <sel>-i_sel = abap_true.
      ENDIF.

      CASE <kind>.
        WHEN cl_abap_datadescr=>typekind_table.
          zcl_ace_appl=>open_int_table( iv_name = <fullname> it_ref = <ref> io_window = mo_viewer->mo_window ).
        WHEN cl_abap_datadescr=>typekind_string.
          NEW zcl_ace_text_viewer( <ref> ).
      ENDCASE.
    ENDIF.


  endmethod.


  method HNDL_USER_COMMAND.


    CONSTANTS: c_mask TYPE x VALUE '01'.

    CASE e_salv_function.

      WHEN 'REFRESH'."
        m_refresh = abap_true.
        "mo_viewer->run_script_hist( mo_viewer->m_hist_step ).
        mo_viewer->mo_tree_local->display( ).
        RETURN.

    ENDCASE.



  endmethod.


  method TRAVERSE.


    ASSIGN ir_up->* TO FIELD-SYMBOL(<new>).
    IF <new> IS INITIAL AND m_hide IS NOT INITIAL.
      me->del_variable( CONV #( is_var-name )  ).
      RETURN.
    ENDIF.

    CASE io_type_descr->kind.
      WHEN c_kind-struct.
        IF iv_struc_name IS SUPPLIED.
          e_root_key = traverse_struct( io_type_descr  = io_type_descr
                                        iv_parent_key  = iv_parent_key
                                        iv_rel         = iv_rel
                                        is_var         = is_var
                                        ir_up          = ir_up
                                        iv_parent_name = iv_parent_name
                                        iv_struc_name  = iv_struc_name ).
        ELSE.
          e_root_key = traverse_struct( io_type_descr  = io_type_descr
                                        iv_parent_key  = iv_parent_key
                                        iv_rel         = iv_rel
                                        is_var         = is_var
                                        ir_up          = ir_up
                                        iv_parent_name = iv_parent_name ).
        ENDIF.

      WHEN c_kind-table.
        e_root_key = traverse_table( io_type_descr  = io_type_descr
                                     iv_parent_key  = iv_parent_key
                                     iv_rel         = iv_rel
                                     is_var         = is_var
                                     ir_up          = ir_up
                                     iv_parent_name = iv_parent_name ).
      WHEN c_kind-elem.
        e_root_key = traverse_elem( io_type_descr  = io_type_descr
                                    iv_parent_key  = iv_parent_key
                                    iv_rel         = iv_rel
                                    is_var         = is_var
                                    iv_parent_name = iv_parent_name ).

    ENDCASE.


  endmethod.


  METHOD traverse_elem.


    DATA: lo_elem_descr TYPE REF TO cl_abap_elemdescr,
          ls_tree       TYPE ts_table,
          lv_text       TYPE lvc_value,
          lv_icon       TYPE salv_de_tree_image,
          l_key         TYPE salv_de_node_key,
          l_rel         TYPE salv_de_node_relation.

    lo_elem_descr ?= io_type_descr.
    ls_tree-ref = is_var-ref.
    l_rel = iv_rel.

    IF is_var-instance NE '{A:initial}'.
      ls_tree-typename = lo_elem_descr->absolute_name.
      REPLACE FIRST OCCURRENCE OF '\TYPE=' IN ls_tree-typename WITH ''.
      IF ls_tree-typename+0(1) = '%'.
        ls_tree-typename = |{ lo_elem_descr->type_kind }({ lo_elem_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    ls_tree-kind = lo_elem_descr->type_kind.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    IF iv_value IS SUPPLIED.
      ls_tree-value = iv_value.
    ELSE.
      IF <new_value> IS NOT INITIAL.
        ls_tree-value = <new_value>.
      ENDIF.
    ENDIF.

    CASE lo_elem_descr->type_kind.
      WHEN 'D'.
        lv_icon = icon_date.
      WHEN 'T'.
        lv_icon = icon_bw_time_sap.
      WHEN 'C'.
        lv_icon = icon_wd_input_field.
      WHEN 'P'.
        lv_icon = icon_increase_decimal.
      WHEN 'g'.
        lv_icon = icon_text_act.
      WHEN 'N' OR 'I'.
        lv_icon = icon_pm_order.
      WHEN OTHERS.
        lv_icon = icon_element.
    ENDCASE.

    lv_text = is_var-short.
    ls_tree-fullname = is_var-name."is_var-path.
    ls_tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(ls_leaf).
      IF sy-subrc = 0.
        l_key = ls_leaf-key.
      ENDIF.
    ELSE.
      l_key = iv_parent_key.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = iv_parent_key.
      l_rel = iv_rel.
    ENDIF.

    DATA(lt_nodes) = tree->get_nodes( )->get_all_nodes( ).
    LOOP AT lt_nodes INTO DATA(ls_nodes).
      DATA(lv_name) = ls_nodes-node->get_text( ).
      DATA(lr_row) = ls_nodes-node->get_data_row( ).
      FIELD-SYMBOLS <ls_row> TYPE ts_table.
      ASSIGN lr_row->* TO <ls_row>.
      IF <ls_row>-fullname = is_var-name.
        DATA(l_node) = ls_nodes-node.
        EXIT.
      ENDIF.
    ENDLOOP.


    IF l_node IS NOT INITIAL.
      READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
      IF sy-subrc = 0.
        TRY.
            FIELD-SYMBOLS: <old_value> TYPE any.
            ASSIGN l_var-ref->* TO <old_value>.
            IF sy-subrc = 0.
              IF is_var-type = l_var-type.
                IF <old_value> NE <new_value>.
                  l_key = l_var-key.
                  l_rel = if_salv_c_node_relation=>next_sibling.
                  DELETE mt_vars WHERE name = is_var-name.
                ELSE.
                  IF ( <new_value> IS INITIAL AND m_hide IS NOT INITIAL ).
                  ELSE.
                    RETURN.
                  ENDIF.
                ENDIF.
              ELSE.
                l_key = l_var-key.
                l_rel = if_salv_c_node_relation=>next_sibling.
                DELETE mt_vars WHERE name = is_var-name.
              ENDIF.
            ENDIF.
          CATCH cx_root.
            DELETE mt_vars WHERE name = is_var-name.
        ENDTRY.
      ENDIF.
    ENDIF.

    DATA(lo_nodes) = tree->get_nodes( ).

    TRY.
        CALL METHOD lo_nodes->add_node
          EXPORTING
            related_node   = l_key
            relationship   = l_rel
            data_row       = ls_tree
            collapsed_icon = lv_icon
            expanded_icon  = lv_icon
            text           = lv_text
            folder         = abap_false
          RECEIVING
            node           = DATA(lo_node).

        IF sy-subrc = 0.
          e_root_key = lo_node->get_key( ).

          APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
          <vars>-stack = mo_viewer->mo_window->mt_stack[ 1 ]-stacklevel.
          <vars>-step = mo_viewer->m_step - mo_viewer->m_step_delta.
          <vars>-program = mo_viewer->mo_window->m_prg-program.
          <vars>-eventtype = mo_viewer->mo_window->m_prg-eventtype.
          <vars>-eventname = mo_viewer->mo_window->m_prg-eventname.
          <vars>-leaf = m_leaf.
          <vars>-name = is_var-name.
          <vars>-short = is_var-short.
          <vars>-key = e_root_key.
          <vars>-ref = is_var-ref.
          <vars>-cl_leaf = is_var-cl_leaf.
          <vars>-type = lo_elem_descr->absolute_name.
          <vars>-path = is_var-path.

          IF l_rel = if_salv_c_node_relation=>next_sibling AND l_node IS NOT INITIAL.
            IF l_node IS NOT INITIAL.
              l_node->delete( ).
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_salv_msg.
    ENDTRY.


  ENDMETHOD.


  method TRAVERSE_OBJ.


    DATA: ls_tree TYPE ts_table,
          lv_text TYPE lvc_value,
          lv_icon TYPE salv_de_tree_image,
          l_key   TYPE salv_de_node_key,
          l_rel   TYPE salv_de_node_relation.

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).

    IF sy-subrc = 0.
      DATA(lo_nodes) = tree->get_nodes( ).
      DATA(l_node) =  lo_nodes->get_node( l_var-key ).

      IF l_var-ref = ir_up.
        RETURN.
      ENDIF.

    ELSE.
      l_rel = iv_rel.
    ENDIF.


    lv_icon = icon_oo_object.
    lv_text = is_var-short.
    ls_tree-fullname = is_var-name.
    ls_tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(ls_leaf).
      IF sy-subrc = 0.
        l_key = ls_leaf-key.
      ENDIF.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = iv_parent_key.
      l_rel = iv_rel.
    ENDIF.

    e_root_key = tree->get_nodes( )->add_node(
     related_node   = l_key
     relationship   = l_rel
     data_row       = ls_tree
     collapsed_icon = lv_icon
     expanded_icon  = lv_icon
     text           = lv_text
     folder         = abap_false )->get_key( ).

    APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
    <vars>-stack = mo_viewer->mo_window->mt_stack[ 1 ]-stacklevel.
    <vars>-step = mo_viewer->m_step - mo_viewer->m_step_delta.
    <vars>-program = mo_viewer->mo_window->m_prg-program.
    <vars>-eventtype = mo_viewer->mo_window->m_prg-eventtype.
    <vars>-eventname = mo_viewer->mo_window->m_prg-eventname.
    <vars>-leaf = m_leaf.
    <vars>-name = is_var-name.
    <vars>-short = is_var-short.
    <vars>-key = e_root_key.
    <vars>-cl_leaf = is_var-cl_leaf.
    <vars>-path = is_var-path.

    IF l_node IS NOT INITIAL.
      l_node->delete( ).
    ENDIF.


  endmethod.


  METHOD traverse_struct.


    DATA: lt_component    TYPE abap_component_tab,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          ls_tree         TYPE ts_table,
          lv_text         TYPE lvc_value,
          l_key           TYPE salv_de_node_key,
          l_rel           TYPE salv_de_node_relation,
          lv_icon         TYPE salv_de_tree_image.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    l_rel = iv_rel.
    lo_struct_descr ?= io_type_descr.
    ls_tree-ref =  ir_up.
    IF is_var-instance NE '{A:initial}'.
      ls_tree-typename = lo_struct_descr->absolute_name.
      REPLACE FIRST OCCURRENCE OF '\TYPE=' IN ls_tree-typename+0(6) WITH ''.
      IF ls_tree-typename+0(1) = '%'.
        ls_tree-typename = |{ lo_struct_descr->type_kind }({ lo_struct_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    ls_tree-kind = lo_struct_descr->type_kind.

    IF m_icon IS INITIAL.
      lv_icon = icon_structure.
    ELSE.
      lv_icon = m_icon.
    ENDIF.

    lv_text = is_var-short.
    ls_tree-fullname = is_var-name.
    ls_tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(ls_leaf).
      IF sy-subrc = 0.
        l_key = ls_leaf-key.
      ENDIF.
    ELSE.
      l_key = iv_parent_key.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = iv_parent_key.
      l_rel = iv_rel.
    ENDIF.

    IF  ( iv_struc_name IS SUPPLIED AND iv_struc_name IS NOT INITIAL ) OR iv_struc_name IS NOT SUPPLIED.
      IF lv_text IS NOT INITIAL.


        DATA(lt_nodes) = tree->get_nodes( )->get_all_nodes( ).
        LOOP AT lt_nodes INTO DATA(ls_nodes).
          DATA(lr_row) = ls_nodes-node->get_data_row( ).
          FIELD-SYMBOLS <ls_row> TYPE ts_table.
          ASSIGN lr_row->* TO <ls_row>.
          IF <ls_row>-fullname = is_var-name.
            DATA(l_node) = ls_nodes-node.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF l_node IS NOT INITIAL.
          READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
          IF sy-subrc = 0.
            IF l_node IS NOT INITIAL.
              TRY.
                  FIELD-SYMBOLS: <old_value> TYPE any.
                  ASSIGN l_var-ref->* TO <old_value>.
                  IF sy-subrc = 0.
                    IF is_var-type = l_var-type.
                      RETURN.
                    ELSE.
                      l_key = l_var-key.
                      l_rel = if_salv_c_node_relation=>next_sibling.
                      DELETE mt_vars WHERE name = is_var-name.
                    ENDIF.
                  ENDIF.
                CATCH cx_root.
                  DELETE mt_vars WHERE name = is_var-name.
              ENDTRY.

            ENDIF.
          ENDIF.
          "RETURN.
        ENDIF.
      ENDIF.

      e_root_key = tree->get_nodes( )->add_node(
             related_node   = l_key
             relationship   = l_rel
             data_row       = ls_tree
             collapsed_icon = lv_icon
             expanded_icon  = lv_icon
             text           = lv_text
             folder         = abap_false )->get_key( ).

      APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
      <vars>-key = e_root_key.
      <vars>-stack = mo_viewer->mo_window->mt_stack[ 1 ]-stacklevel.
      <vars>-step  = mo_viewer->m_step - mo_viewer->m_step_delta.
      <vars>-program   = mo_viewer->mo_window->m_prg-program.
      <vars>-eventtype = mo_viewer->mo_window->m_prg-eventtype.
      <vars>-eventname = mo_viewer->mo_window->m_prg-eventname.
      <vars>-leaf  = m_leaf.
      <vars>-name  = is_var-name.
      <vars>-short = is_var-short.
      <vars>-ref  = ir_up.
      <vars>-cl_leaf = is_var-cl_leaf.
      <vars>-type = lo_struct_descr->absolute_name.
      <vars>-path = is_var-path.
    ENDIF.

    IF l_rel = if_salv_c_node_relation=>next_sibling AND l_node IS NOT INITIAL.
      IF l_node IS NOT INITIAL.

        l_node->delete( ).
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD traverse_table.


    DATA: lo_table_descr TYPE REF TO cl_abap_tabledescr,
          ls_tree        TYPE ts_table,
          lv_text        TYPE lvc_value,
          lv_icon        TYPE salv_de_tree_image,
          l_key          TYPE salv_de_node_key,
          l_rel          TYPE salv_de_node_relation.

    FIELD-SYMBOLS: <tab> TYPE ANY TABLE.

    ASSIGN ir_up->* TO <tab>.
    DATA(lines) = lines( <tab> ).
    ls_tree-ref = ir_up.
    l_key = iv_parent_key.

    lo_table_descr ?= io_type_descr.

    ls_tree-fullname = |{ is_var-short } ({ lines })|.
    ls_tree-kind = lo_table_descr->type_kind.
    IF is_var-instance NE '{A:initial}'.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = mo_viewer->ms_stack-include INTO DATA(ls_prog).
      READ TABLE mo_viewer->mo_window->ms_sources-tt_tabs WITH KEY name = is_var-short INTO DATA(ls_tab).
      IF sy-subrc <> 0.
        ls_tree-typename = replace( val = lo_table_descr->absolute_name sub = '\TYPE=' with = '' ).
      ELSE.
        ls_tree-typename = ls_tab-type.
      ENDIF.
    ENDIF.
    lv_icon = icon_view_table.

    IF is_var-name IS NOT INITIAL.
      lv_text = ls_tree-fullname.
    ELSE.
      lv_text = ls_tree-typename.
    ENDIF.

    l_rel = iv_rel.
    ASSIGN ir_up->* TO FIELD-SYMBOL(<new_value>).

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
    DATA(lt_nodes) = tree->get_nodes( )->get_all_nodes( ).
    LOOP AT lt_nodes INTO DATA(ls_nodes).
      DATA(lr_row) = ls_nodes-node->get_data_row( ).
      FIELD-SYMBOLS <ls_row> TYPE ts_table.
      ASSIGN lr_row->* TO <ls_row>.
      IF <ls_row>-fullname = is_var-name.
        DATA(l_node) = ls_nodes-node.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF l_node IS NOT INITIAL.
      TRY.
          FIELD-SYMBOLS: <old_value> TYPE any.
          ASSIGN l_var-ref->* TO <old_value>.
          IF sy-subrc = 0.
            IF <old_value> NE <new_value>.
              l_key = l_var-key.
              l_rel = if_salv_c_node_relation=>next_sibling.
              DELETE mt_vars WHERE name = is_var-name.
            ELSE.
              IF ( <new_value> IS INITIAL AND m_hide IS NOT INITIAL ).
                me->del_variable( CONV #( is_var-name )  ).
              ENDIF.
            ENDIF.
          ENDIF.

          IF <new_value> IS INITIAL AND m_hide IS NOT INITIAL.
            me->del_variable( CONV #( is_var-name ) ).
            RETURN.
          ENDIF.
        CATCH cx_root.
          me->del_variable( CONV #( is_var-name )  ).
      ENDTRY.
    ELSE.

      IF <new_value> IS INITIAL AND m_hide IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(ls_leaf).
      IF sy-subrc = 0.
        l_key = ls_leaf-key.
      ENDIF.
    ELSE.
      l_key = iv_parent_key.
    ENDIF.

    READ TABLE mt_vars WITH KEY name = iv_parent_name TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.

      ls_tree-fullname = is_var-name.
      e_root_key =
        tree->get_nodes( )->add_node(
          related_node   = l_key
          relationship   = iv_rel
          collapsed_icon = lv_icon
          expanded_icon  = lv_icon
          data_row       = ls_tree
          text           = lv_text
          folder         = abap_true
        )->get_key( ).

      APPEND INITIAL LINE TO mt_vars ASSIGNING FIELD-SYMBOL(<vars>).
      <vars>-stack = mo_viewer->mo_window->mt_stack[ 1 ]-stacklevel.
      <vars>-leaf = m_leaf.
      <vars>-name = is_var-name.
      <vars>-program = mo_viewer->mo_window->m_prg-program.
      <vars>-eventtype = mo_viewer->mo_window->m_prg-eventtype.
      <vars>-eventname = mo_viewer->mo_window->m_prg-eventname.
      <vars>-short = is_var-short.
      <vars>-key = e_root_key.
      <vars>-ref = ir_up.
      <vars>-step = mo_viewer->m_step - mo_viewer->m_step_delta.
      <vars>-cl_leaf = is_var-cl_leaf.
      <vars>-path = is_var-path.

      IF l_rel = if_salv_c_node_relation=>next_sibling AND l_node IS NOT INITIAL.
        IF l_node IS NOT INITIAL.
          l_node->delete( ).
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
