class ZCL_ACE_RTTI_TREE definition
  public
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
        name     = 'REFRESH'
        icon     = CONV #( icon_refresh )
        text     = ''
        tooltip  = 'Refresh'
        position = if_salv_c_function_position=>left_of_salv_functions ).


  endmethod.


  method ADD_NODE.

      " intensified style for lazy-load folders (have a param marker)
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

      .

      DATA(o_setting) =  mo_tree->get_tree_settings( ).
      o_setting->set_hierarchy_header( i_header ).
      o_setting->set_hierarchy_size( 30 ).
      o_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

      DATA(o_columns) = mo_tree->get_columns( ).
      o_columns->set_optimize( abap_true ).

*      o_columns->get_column( 'VALUE' )->set_visible( abap_false ).
*      o_columns->get_column( 'PARAM' )->set_visible( abap_false ).
*      o_columns->get_column( 'INCLUDE' )->set_visible( abap_false ).

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

      " Set expander on lazy-load folders
      LOOP AT mt_lazy_nodes INTO DATA(lv_lazy_key).
        TRY.
            mo_tree->get_nodes( )->get_node( lv_lazy_key )->set_expander( abap_true ).
          CATCH cx_root.
        ENDTRY.
      ENDLOOP.

      " Expand root node
      TRY.
          mo_tree->get_nodes( )->get_node( main_node_key )->expand( ).
        CATCH cx_root.
      ENDTRY.

      " Expand 2nd-level nodes — but skip lazy ones
      TRY.
          DATA(o_nodes_obj) = mo_tree->get_nodes( ).
          DATA(lv_child_key) = o_nodes_obj->get_node( main_node_key )->get_first_child( )->get_key( ).
          DO.
            TRY.
                DATA(o_child) = o_nodes_obj->get_node( lv_child_key ).
                " Only expand if not in lazy list
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

      DATA(o_nodes) = mo_tree->get_nodes( ).
      DATA(o_node)  = o_nodes->get_node( node_key ).
      DATA r_row TYPE REF TO data.
      DATA ls_clear_row TYPE ZCL_ACE=>ts_tree.

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

      " --- VARS: lazy-load folder ---
      IF lv_param IS NOT INITIAL AND lv_param+0(5) = 'VARS:'.
        SPLIT lv_param AT ':' INTO DATA(lv_pfx) DATA(lv_lazy_class) DATA(lv_lazy_meth).
        DATA(lv_lazy_prog) = CONV program( <program> ).
        ls_clear_row-kind    = <kind>.
        ls_clear_row-program = <program>.
        ls_clear_row-include = <include>.
        ls_clear_row-ev_type = <ev_type>.
        ls_clear_row-ev_name = <ev_name>.
        o_node->set_data_row( REF #( ls_clear_row ) ).
        DATA(lv_added) = 0.
        LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_ev)
          WHERE program = lv_lazy_prog AND class = lv_lazy_class AND eventtype = 'METHOD' AND eventname = lv_lazy_meth.
          add_node( i_name = lv_ev-name i_icon = lv_ev-icon i_rel = node_key
                    i_tree = VALUE ZCL_ACE=>ts_tree( value = lv_ev-line include = lv_ev-include ) ).
          lv_added = lv_added + 1.
        ENDLOOP.
        IF lv_added > 0.
          TRY. o_node->expand( ). CATCH cx_root. ENDTRY.
        ENDIF.
        RETURN.
      ENDIF.

      " --- ATTR: lazy-load folder ---
      IF lv_param IS NOT INITIAL AND lv_param+0(5) = 'ATTR:'.
        DATA(lv_attr_class) = lv_param+5.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
          WITH KEY class = lv_attr_class eventtype = 'METHOD' INTO DATA(lv_attr_sub).
        IF sy-subrc = 0.
          LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_av)
            WHERE program = lv_attr_sub-program AND class = lv_attr_class AND eventname IS INITIAL.
            add_node( i_name = lv_av-name i_icon = lv_av-icon i_rel = node_key
                      i_tree = VALUE ZCL_ACE=>ts_tree( value = lv_av-line include = lv_av-include ) ).
          ENDLOOP.
        ENDIF.
        CLEAR ls_clear_row.
        o_node->set_data_row( REF #( ls_clear_row ) ).
        TRY. o_node->expand( ). CATCH cx_root. ENDTRY.
        RETURN.
      ENDIF.

      " --- Structural markers: navigate without highlight ---
      IF lv_param IS NOT INITIAL AND ( lv_param+0(6) = 'CLASS:' OR lv_param+0(9) = 'LCLASSES:' ).
        IF <include> IS NOT INITIAL.
          mo_viewer->mo_window->set_program( CONV #( <include> ) ).
        ENDIF.
        mo_viewer->mo_window->set_program_line( CONV #( <value> ) ).
        RETURN.
      ENDIF.

      " --- Other lazy-load markers (GVARS/FORMS/MODS): no action on dblclick ---
      IF lv_param IS NOT INITIAL AND (
          lv_param+0(6) = 'GVARS:' OR lv_param+0(6) = 'FORMS:' OR lv_param+0(5) = 'MODS:' ).
        RETURN.
      ENDIF.

      " --- Navigation nodes ---
      READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
        WITH KEY program = <program> include = <include> eventname = <ev_name> eventtype = <ev_type>
        INTO mo_viewer->mo_window->ms_sel_call.

      IF <kind> = 'M' AND <param> IS INITIAL AND <ev_type> = 'MODULE' AND <include> IS NOT INITIAL.
        DATA(lv_mod_include) = CONV program( <include> ).
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_mod_include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ZCL_ACE_SOURCE_PARSER=>parse_tokens(
            i_program = lv_mod_include i_include = lv_mod_include io_debugger = mo_viewer ).
        ENDIF.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_mod_include ASSIGNING FIELD-SYMBOL(<mod_prog>).
        IF sy-subrc = 0.
          LOOP AT mo_viewer->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<mp>). CLEAR <mp>-selected. ENDLOOP.
          <mod_prog>-selected = abap_true.
          mo_viewer->mo_window->m_prg-include = lv_mod_include.
          IF <mod_prog>-v_source IS NOT INITIAL.
            mo_viewer->mo_window->mo_code_viewer->set_text( table = <mod_prog>-v_source ).
            DATA(lv_mod_orig_line) = CONV i( <value> ).
            DATA(lv_mod_vline)     = lv_mod_orig_line.
            READ TABLE <mod_prog>-v_keywords
              WITH KEY include = lv_mod_include line = lv_mod_orig_line INTO DATA(ls_mod_vkw).
            IF sy-subrc = 0. lv_mod_vline = ls_mod_vkw-v_line. ENDIF.
            mo_viewer->mo_window->set_program_line( lv_mod_vline ).
          ELSE.
            mo_viewer->mo_window->mo_code_viewer->set_text( table = <mod_prog>-source_tab ).
            mo_viewer->mo_window->set_program_line( CONV #( <value> ) ).
          ENDIF.
        ENDIF.
        RETURN.
      ENDIF.

      IF <kind> = 'M' AND <param> IS INITIAL AND <ev_type> = 'FORM' AND <include> IS NOT INITIAL.
        DATA(lv_form_include) = CONV program( <include> ).
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_form_include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ZCL_ACE_SOURCE_PARSER=>parse_tokens(
            i_program = lv_form_include i_include = lv_form_include io_debugger = mo_viewer ).
        ENDIF.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_form_include ASSIGNING FIELD-SYMBOL(<form_prog>).
        IF sy-subrc = 0.
          LOOP AT mo_viewer->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<fp>). CLEAR <fp>-selected. ENDLOOP.
          <form_prog>-selected = abap_true.
          mo_viewer->mo_window->m_prg-include = lv_form_include.
          IF <form_prog>-v_source IS NOT INITIAL.
            mo_viewer->mo_window->mo_code_viewer->set_text( table = <form_prog>-v_source ).
            DATA(lv_form_orig_line) = CONV i( <value> ).
            DATA(lv_form_vline)     = lv_form_orig_line.
            READ TABLE <form_prog>-v_keywords
              WITH KEY include = lv_form_include line = lv_form_orig_line INTO DATA(ls_form_vkw).
            IF sy-subrc = 0. lv_form_vline = ls_form_vkw-v_line. ENDIF.
            mo_viewer->mo_window->set_program_line( lv_form_vline ).
          ELSE.
            mo_viewer->mo_window->mo_code_viewer->set_text( table = <form_prog>-source_tab ).
            mo_viewer->mo_window->set_program_line( CONV #( <value> ) ).
          ENDIF.
        ENDIF.
        RETURN.
      ENDIF.

      IF <kind> = 'M' AND <param> IS INITIAL AND <ev_type> = 'METHOD' AND <include> IS NOT INITIAL.
        DATA(lv_cm_include) = CONV program( <include> ).
        DATA(lv_cm_method)  = CONV string( <ev_name> ).
        DATA(lv_cm_value)   = CONV i( <value> ).
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_cm_include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ZCL_ACE_SOURCE_PARSER=>parse_tokens(
            i_program = lv_cm_include i_include = lv_cm_include io_debugger = mo_viewer ).
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
                  ZCL_ACE_SOURCE_PARSER=>parse_tokens(
                    i_program = CONV #( lv_enh_eimp_ow ) i_include = CONV #( lv_enh_eimp_ow )
                    io_debugger = mo_viewer ).
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
                ZCL_ACE_SOURCE_PARSER=>parse_tokens(
                  i_program = CONV #( lv_enh_eimp2 ) i_include = CONV #( lv_enh_eimp2 )
                  io_debugger = mo_viewer ).
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
            RETURN.
          ENDIF.
        ENDIF.
        mo_viewer->mo_window->set_program( lv_cm_include ).
        mo_viewer->mo_window->set_program_line( lv_cm_value ).
        RETURN.
      ENDIF.

      IF <kind> = 'M' AND <param> IS NOT INITIAL AND <ev_type> = 'FORM'.
        DATA(lv_enh_include_f) = CONV program( <param> ).
        DATA(lv_form_enh_id)   = CONV i( <enh_id> ).
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_enh_include_f TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ZCL_ACE_SOURCE_PARSER=>parse_tokens(
            i_program = CONV #( lv_enh_include_f ) i_include = CONV #( lv_enh_include_f )
            io_debugger = mo_viewer ).
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
        RETURN.
      ENDIF.

      IF <kind> = 'M' AND <param> IS NOT INITIAL AND <ev_type> = 'METHOD'.
        DATA(lv_eimp_include) = CONV program( <param> ).
        DATA(lv_eimp_method)  = CONV string( <ev_name> ).
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_eimp_include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ZCL_ACE_SOURCE_PARSER=>parse_tokens(
            i_program = CONV #( lv_eimp_include ) i_include = CONV #( lv_eimp_include )
            io_debugger = mo_viewer ).
        ENDIF.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_eimp_include INTO DATA(ls_eimp_prog).
        IF sy-subrc = 0.
          DATA(lv_enh_pos)     = CONV string( <value> ).
          DATA(lv_impl_prefix) = COND string(
            WHEN lv_enh_pos = 'BEGIN'     AND <ev_type> = 'METHOD' THEN 'IPR_'
            WHEN lv_enh_pos = 'END'       AND <ev_type> = 'METHOD' THEN 'IPO_'
            WHEN lv_enh_pos = 'OVERWRITE' AND <ev_type> = 'METHOD' THEN 'IOW_'
            WHEN lv_enh_pos = 'BEGIN'     AND <ev_type> = 'FORM'   THEN 'IPF_'
            WHEN lv_enh_pos = 'END'       AND <ev_type> = 'FORM'   THEN 'IPF_'
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
        RETURN.
      ENDIF.

      " --- Fallback: navigate + highlight for var/param leaf nodes ---
      IF <include> IS NOT INITIAL.
        mo_viewer->mo_window->set_program( CONV #( <include> ) ).
      ENDIF.
      mo_viewer->mo_window->set_program_line( CONV #( <value> ) ).

      DATA(lv_var_name) = CONV string( <var_name> ).
      IF lv_var_name IS NOT INITIAL.
        READ TABLE mo_viewer->mt_selected_var WITH KEY name = lv_var_name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          DELETE mo_viewer->mt_selected_var WHERE name = lv_var_name.
          o_node->set_row_style( if_salv_c_tree_style=>default ).
        ELSE.
          o_node->set_row_style( if_salv_c_tree_style=>emphasized_b ).
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

    CHECK <param> IS NOT INITIAL.
    DATA(lv_param) = CONV string( <param> ).

    " ---- VARS:{class}:{method} ----
    IF lv_param+0(5) = 'VARS:'.
      SPLIT lv_param AT ':' INTO DATA(lv_pfx) DATA(lv_class) DATA(lv_meth).
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_v)
        WHERE program = <program> AND class = lv_class AND eventtype = 'METHOD' AND eventname = lv_meth.
        add_node( i_name = lv_v-name i_icon = lv_v-icon i_rel = node_key
                  i_tree = VALUE #( value = lv_v-line include = lv_v-include var_name = lv_v-name ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    " ---- LVARS:FORM:{formname} ----
    IF strlen( lv_param ) > 10 AND lv_param+0(10) = 'LVARS:FORM'.
      SPLIT lv_param AT ':' INTO DATA(lv_lv_pfx) DATA(lv_lv_type) DATA(lv_lv_form).
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_fv)
        WHERE program = <program> AND eventtype = 'FORM' AND eventname = lv_lv_form.
        add_node( i_name = lv_fv-name i_icon = lv_fv-icon i_rel = node_key
                  i_tree = VALUE #( value = lv_fv-line include = lv_fv-include var_name = lv_fv-name ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    " ---- ATTR:{class} ----
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

    " ---- GVARS: ----
    IF lv_param = 'GVARS:'.
      LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_g)
        WHERE program = <program> AND eventtype IS INITIAL AND class IS INITIAL.
        add_node( i_name = lv_g-name i_icon = lv_g-icon i_rel = node_key
                  i_tree = VALUE #( value = lv_g-line include = lv_g-include var_name = lv_g-name ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    " ---- FORMS:{program} ----
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
        " ---- Local vars for FORM (lazy) ----
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

    " ---- MODS:{program} ----
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

    " ---- LCLASSES:{program} ----
    IF strlen( lv_param ) > 9 AND lv_param+0(9) = 'LCLASSES:'.
      DATA(lv_lc_prog) = lv_param+9.
      DATA(lv_lc_prev) = ``.
      DATA(lv_lc_splits) = VALUE string_table( ).
      SPLIT lv_lc_prog AT '=' INTO TABLE lv_lc_splits.
      DATA(lv_main_class) = lv_lc_splits[ 1 ].
      LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_lc)
        WHERE program = lv_lc_prog AND eventtype = 'METHOD' AND is_intf = abap_false.
        CHECK lv_lc-class <> lv_main_class.
        IF lv_lc_prev <> lv_lc-class.
          DATA(lv_cls_inc)  = COND program( WHEN lv_lc-def_include IS NOT INITIAL
                                            THEN lv_lc-def_include ELSE lv_lc-include ).
          DATA(lv_cls_line) = COND i( WHEN lv_lc-def_line > 0 THEN lv_lc-def_line ELSE 0 ).
          DATA(lv_cls_node) = add_node(
            i_name = CONV #( lv_lc-class )
            i_icon = CONV #( icon_folder )
            i_rel  = node_key
            i_tree = VALUE #( kind    = 'M'
                              value   = lv_cls_line
                              include = lv_cls_inc
                              program = lv_lc-program
                              param   = |CLASS:{ lv_lc-class }| ) ).
          APPEND lv_cls_node TO mt_lazy_nodes.
          TRY.
              mo_tree->get_nodes( )->get_node( lv_cls_node )->set_expander( abap_true ).
            CATCH cx_root.
          ENDTRY.
        ENDIF.
        lv_lc_prev = lv_lc-class.
      ENDLOOP.
      RETURN.
    ENDIF.

    " ---- LINTFS:{program} ----
    IF strlen( lv_param ) > 7 AND lv_param+0(7) = 'LINTFS:'.
      DATA(lv_li_prog) = lv_param+7.
      DATA(lv_li_prev) = ``.
      DATA(lv_li_splits) = VALUE string_table( ).
      SPLIT lv_li_prog AT '=' INTO TABLE lv_li_splits.
      DATA(lv_li_main) = lv_li_splits[ 1 ].
      LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_li)
        WHERE program = lv_li_prog AND eventtype = 'METHOD' AND is_intf = abap_true.
        CHECK lv_li-class <> lv_li_main.
        IF lv_li_prev <> lv_li-class.
          DATA(lv_li_inc)  = COND program( WHEN lv_li-def_include IS NOT INITIAL
                                           THEN lv_li-def_include ELSE lv_li-include ).
          DATA(lv_li_line) = COND i( WHEN lv_li-def_line > 0 THEN lv_li-def_line ELSE 0 ).
          DATA(lv_li_node) = add_node(
            i_name = CONV #( lv_li-class )
            i_icon = CONV #( icon_oo_connection )
            i_rel  = node_key
            i_tree = VALUE #( kind    = 'M'
                              value   = lv_li_line
                              include = lv_li_inc
                              program = lv_li-program
                              param   = |CLASS:{ lv_li-class }| ) ).
          APPEND lv_li_node TO mt_lazy_nodes.
          TRY.
              mo_tree->get_nodes( )->get_node( lv_li_node )->set_expander( abap_true ).
            CATCH cx_root.
          ENDTRY.
        ENDIF.
        lv_li_prev = lv_li-class.
      ENDLOOP.
      RETURN.
    ENDIF.

    " ---- CLASS:{classname} ----
    IF strlen( lv_param ) > 6 AND lv_param+0(6) = 'CLASS:'.
      DATA(lv_cls_name) = lv_param+6.
      DATA(lv_attr_cnt) = 0.
      READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
        WITH KEY class = lv_cls_name eventtype = 'METHOD'
        INTO DATA(lv_cls_sub).
      IF sy-subrc = 0.
        LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_ca)
          WHERE program = lv_cls_sub-program AND class = lv_cls_name AND eventname IS INITIAL.
          lv_attr_cnt += 1.
        ENDLOOP.
        IF lv_attr_cnt > 0.
          DATA(lv_attr_node) = add_node(
            i_name = |Attributes ({ lv_attr_cnt })|
            i_icon = CONV #( icon_folder )
            i_rel  = node_key
            i_tree = VALUE #( param = |ATTR:{ lv_cls_name }| ) ).
          APPEND lv_attr_node TO mt_lazy_nodes.
          TRY.
              mo_tree->get_nodes( )->get_node( lv_attr_node )->set_expander( abap_true ).
            CATCH cx_root.
          ENDTRY.
        ENDIF.
      ENDIF.
      LOOP AT mo_viewer->mo_window->ms_sources-tt_calls_line INTO DATA(lv_cm)
        WHERE class = lv_cls_name AND eventtype = 'METHOD'.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = lv_cm-include INTO DATA(lv_cmprog).
        READ TABLE lv_cmprog-t_keywords WITH KEY index = lv_cm-index INTO DATA(lv_cmkw).
        DATA(lv_micon) = COND salv_de_tree_image(
          WHEN lv_cm-redefined = abap_false THEN
            COND #( WHEN lv_cm-meth_type = 0 OR lv_cm-meth_type = 1 THEN CONV #( icon_led_green )
                    WHEN lv_cm-meth_type = 2                          THEN CONV #( icon_led_yellow )
                    WHEN lv_cm-meth_type = 3                          THEN CONV #( icon_led_red )
                    WHEN lv_cm-eventname = 'CONSTRUCTOR'              THEN CONV #( icon_tools )
                    ELSE                                                   CONV #( icon_led_green ) )
          ELSE CONV #( icon_oo_overwrite ) ).
        IF lv_cm-is_intf = abap_true.
          lv_micon = icon_oo_inst_method.
        ENDIF.
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

        DATA(lv_mv_cnt) = 0.
        LOOP AT mo_viewer->mo_window->ms_sources-t_vars INTO DATA(lv_mv)
          WHERE program = lv_cm-program AND class = lv_cls_name AND eventname = lv_cm-eventname.
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

      CASE e_salv_function.

        WHEN 'REFRESH'."
          mo_viewer->mo_tree_local->display( ).
          RETURN.

      ENDCASE.


  endmethod.
ENDCLASS.
