REPORT z_ace. "ACE - Abap Code Explorer
*  & Multi-windows program for ABAP code analysis
*  &---------------------------------------------------------------------*
*  & version: beta 0.3
*  & Git https://github.com/ysichov/ACE

*  & Written by Yurii Sychov
*  & e-mail:   ysichov@gmail.com
*  & blog:     https://ysychov.wordpress.com/blog/
*  & LinkedIn: https://www.linkedin.com/in/ysychov/
*  &---------------------------------------------------------------------*

*  & External resources
*  & https://github.com/WegnerDan/abapMermaid

PARAMETERS: p_prog   TYPE progname  MATCHCODE OBJECT progname MODIF ID prg OBLIGATORY,
            p_dest   TYPE text255 MEMORY ID dest,
            p_model  TYPE text255 MEMORY ID model,
            p_apikey TYPE text255  MEMORY ID api.

CLASS lcl_ai DEFINITION DEFERRED.
CLASS lcl_data_receiver DEFINITION DEFERRED.
CLASS lcl_data_transmitter DEFINITION DEFERRED.
CLASS lcl_rtti_tree DEFINITION DEFERRED.
CLASS lcl_ace_window DEFINITION DEFERRED.
CLASS lcl_table_viewer DEFINITION DEFERRED.
CLASS lcl_mermaid DEFINITION DEFERRED.

*CLASS lcl_box_handler DEFINITION."for memory clearing
*
*  PUBLIC SECTION.
*    METHODS: on_table_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.
*
*ENDCLASS.

CLASS lcl_appl DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF selection_display_s,
        ind         TYPE i,
        field_label TYPE lvc_fname,
        int_type(1),
        inherited   TYPE aqadh_type_of_icon,
        emitter     TYPE aqadh_type_of_icon,
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
        transmitter TYPE REF TO lcl_data_transmitter,
        receiver    TYPE REF TO lcl_data_receiver,
        color       TYPE lvc_t_scol,
        style       TYPE lvc_t_styl,
      END OF selection_display_s,
      BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string, "aqadh_range_value,
        high        TYPE string, "aqadh_range_value,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row.

    TYPES: BEGIN OF sign_option_icon_s,
             sign          TYPE tvarv_sign,
             option        TYPE tvarv_opti,
             icon_name(64) TYPE c,
             icon          TYPE aqadh_type_of_icon,
           END OF sign_option_icon_s,

           BEGIN OF var_table,
             step          TYPE i,
             stack         TYPE i,
             program(40)   TYPE c,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             first         TYPE xfeld,
             is_appear     TYPE xfeld,
             del           TYPE xfeld,
             leaf          TYPE string,
             name(1000)               ,
             path          TYPE string,
             short         TYPE string,
             key           TYPE salv_de_node_key,
             parent        TYPE string,
             cl_leaf       TYPE int4,
             ref           TYPE REF TO data,
             type          TYPE string,
             instance      TYPE string,
             objname       TYPE string,
             done          TYPE xfeld,
           END OF var_table,

           t_var_table TYPE STANDARD TABLE OF var_table WITH NON-UNIQUE DEFAULT KEY,

           BEGIN OF var_table_temp,
             step          TYPE i,
             stack         TYPE i,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             name          TYPE string,
             value         TYPE string,
             first         TYPE xfeld,
             is_appear     TYPE xfeld,
             del           TYPE xfeld,
             program(40)   TYPE c,
             leaf          TYPE string,
             path          TYPE string,
             type          TYPE string,
             instance      TYPE string,
             objname       TYPE string,
             ref           TYPE REF TO data,
           END OF var_table_temp,

           BEGIN OF var_table_h,
             step          TYPE i,
             program(40)   TYPE c,
             eventtype(30) TYPE c,
             eventname(61) TYPE c,
             leaf          TYPE string,
             name          TYPE string,
             path          TYPE string,
             parent        TYPE string,
             short         TYPE string,
             cl_leaf       TYPE int4,  "?
             ref           TYPE REF TO data,
             tree          TYPE REF TO lcl_rtti_tree,
             time          LIKE sy-uname,
           END OF var_table_h,

           BEGIN OF t_obj,
             name       TYPE string,
             alv_viewer TYPE REF TO lcl_table_viewer,
           END OF t_obj,

           BEGIN OF t_popup,
             parent TYPE REF TO cl_gui_dialogbox_container,
             child  TYPE REF TO cl_gui_dialogbox_container,
           END OF t_popup,

           BEGIN OF t_classes_types,
             name TYPE string,
             full TYPE string,
             type TYPE char1,
             key  TYPE salv_de_node_key,
           END OF t_classes_types,

           BEGIN OF t_lang,
             spras(4),
             sptxt    TYPE sptxt,
           END OF t_lang,

           BEGIN OF t_stack,
             step       TYPE i,
             stacklevel TYPE tpda_stack_level,
             line       TYPE tpda_sc_line,
             program    TYPE tpda_program,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             include    TYPE tpda_include,
           END OF t_stack,

           BEGIN OF t_step_counter,
             step       TYPE i,
             stacklevel TYPE tpda_stack_level,
             line       TYPE tpda_sc_line,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             first      TYPE xfeld,
             last       TYPE xfeld,
             program    TYPE tpda_program,
             include    TYPE tpda_include,
             time       LIKE sy-uzeit,
           END OF t_step_counter.

    CLASS-DATA: m_option_icons    TYPE TABLE OF sign_option_icon_s,
                mt_lang           TYPE TABLE OF t_lang,
                mt_obj            TYPE TABLE OF t_obj, "main object table
                mt_popups         TYPE TABLE OF t_popup, "dependents popups
                c_dragdropalv     TYPE REF TO cl_dragdrop,
                is_mermaid_active TYPE xfeld.

    CLASS-DATA: mt_sel TYPE TABLE OF selection_display_s.

    CLASS-METHODS:
      init_icons_table,
      init_lang,
      check_mermaid,
      open_int_table IMPORTING it_tab    TYPE ANY TABLE OPTIONAL
                               it_ref    TYPE REF TO data OPTIONAL
                               iv_name   TYPE string
                               io_window TYPE REF TO lcl_ace_window.

ENDCLASS.

CLASS lcl_popup DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA m_counter              TYPE i.
    DATA: m_additional_name      TYPE string,
          mo_box                 TYPE REF TO cl_gui_dialogbox_container,
          mo_splitter            TYPE REF TO cl_gui_splitter_container,
          mo_splitter_imp_exp    TYPE REF TO cl_gui_splitter_container,
          mo_variables_container TYPE REF TO cl_gui_container,
          mo_tables_container    TYPE REF TO cl_gui_container.

    METHODS: constructor IMPORTING i_additional_name TYPE string OPTIONAL,
      create IMPORTING i_width       TYPE i
                       i_hight       TYPE i
                       i_name        TYPE text100 OPTIONAL
             RETURNING VALUE(ro_box) TYPE REF TO cl_gui_dialogbox_container,
      on_box_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.

ENDCLASS.


CLASS lcl_popup IMPLEMENTATION.

  METHOD constructor.
    m_additional_name = i_additional_name.

  ENDMETHOD.

  METHOD create.

    DATA: l_top  TYPE i,
          l_left TYPE i.

    ADD 1 TO m_counter.
    l_top  = l_left =  100 -  20 * ( m_counter DIV 5 ) - ( m_counter MOD 5 ) * 20.
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

  ENDMETHOD.

  METHOD on_box_close.
    LOOP AT lcl_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>) WHERE parent = sender .
      <popup>-child->free( ).
      CLEAR <popup>-child.
    ENDLOOP.
    IF sy-subrc <> 0.
      DELETE  lcl_appl=>mt_popups WHERE child = sender.
    ENDIF.
    DELETE lcl_appl=>mt_popups WHERE child IS INITIAL.
    sender->free( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_ddic DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: get_text_table IMPORTING i_tname TYPE tabname
                                  EXPORTING e_tab   TYPE tabname.
ENDCLASS.

CLASS lcl_ddic IMPLEMENTATION.

  METHOD get_text_table.
    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING
        tabname   = i_tname
      IMPORTING
        texttable = e_tab.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_dd_data DEFINITION."drag&drop data

  PUBLIC  SECTION.
    DATA: m_row    TYPE i,
          m_column TYPE lvc_s_col.

ENDCLASS.

CLASS lcl_dragdrop DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      drag FOR EVENT ondrag OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row e_column ,
      drop FOR EVENT ondrop OF cl_gui_alv_grid IMPORTING e_dragdropobj e_row.

ENDCLASS.

CLASS lcl_alv_common DEFINITION.

  PUBLIC SECTION.
    CONSTANTS: c_white(4) TYPE x VALUE '00000001'. "white background

    CLASS-METHODS:
      refresh IMPORTING i_obj TYPE REF TO cl_gui_alv_grid i_layout TYPE lvc_s_layo OPTIONAL i_soft TYPE char01 OPTIONAL,
      translate_field IMPORTING i_lang TYPE ddlanguage OPTIONAL CHANGING c_fld TYPE lvc_s_fcat,
      get_selected IMPORTING i_obj TYPE REF TO cl_gui_alv_grid RETURNING VALUE(e_index) TYPE i.

ENDCLASS.

CLASS lcl_alv_common IMPLEMENTATION.

  METHOD refresh.

    DATA l_stable TYPE lvc_s_stbl.
    l_stable = 'XX'.
    IF i_layout IS SUPPLIED.
      i_obj->set_frontend_layout( i_layout ).
    ENDIF.
    i_obj->refresh_table_display( EXPORTING is_stable = l_stable i_soft_refresh = i_soft ).

  ENDMETHOD.

  METHOD translate_field.

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

  ENDMETHOD.

  METHOD get_selected.

    i_obj->get_selected_cells( IMPORTING et_cell = DATA(sel_cells) ).
    IF lines( sel_cells ) > 0.
      e_index = sel_cells[ 1 ]-row_id.
    ELSE.
      i_obj->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
      IF lines( sel_rows ) > 0.
        e_index = sel_rows[ 1 ]-index.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_rtti DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_table_by_name IMPORTING i_tname TYPE tabname
                           CHANGING  c_table TYPE REF TO data,

      create_struc_handle IMPORTING i_tname  TYPE tabname
                          EXPORTING e_t_comp TYPE abap_component_tab
                                    e_handle TYPE REF TO cl_abap_structdescr.

ENDCLASS.

CLASS lcl_ace DEFINITION DEFERRED.

CLASS lcl_source_parser DEFINITION.

  PUBLIC SECTION.

    "CLASS-DATA: mv_step TYPE i.
    CLASS-METHODS: parse_tokens IMPORTING iv_program TYPE program io_debugger TYPE REF TO lcl_ace iv_class TYPE string OPTIONAL iv_evname TYPE string OPTIONAL,
      parse_call IMPORTING iv_program TYPE program iv_index TYPE i iv_stack TYPE i iv_ev_name TYPE string iv_ev_type TYPE string iv_class TYPE string OPTIONAL io_debugger TYPE REF TO lcl_ace,
      code_execution_scanner IMPORTING iv_program TYPE program iv_evname TYPE string OPTIONAL iv_evtype TYPE string OPTIONAL
        iv_stack TYPE i OPTIONAL io_debugger TYPE REF TO lcl_ace.


ENDCLASS.

CLASS lcl_ace DEFINITION.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_obj,
             name TYPE string,
             obj  TYPE string,
           END OF t_obj,

           BEGIN OF t_sel_var,
             name   TYPE string,
             is_sel TYPE xfeld,
             refval TYPE REF TO data,
           END OF t_sel_var.

    DATA: mv_prog           TYPE prog,
          mv_dest           TYPE text255,
          mv_model          TYPE text255,
          mv_apikey         TYPE text255,
          mt_obj            TYPE TABLE OF t_obj,
          mt_compo          TYPE TABLE OF scompo,
          mt_locals         TYPE tpda_scr_locals_it,
          mt_globals        TYPE tpda_scr_globals_it,
          mt_ret_exp        TYPE tpda_scr_locals_it,
          m_counter         TYPE i,
          mt_steps          TYPE  TABLE OF lcl_appl=>t_step_counter, "source code steps
          mt_var_step       TYPE  TABLE OF lcl_appl=>var_table_h,
          m_step            TYPE i,
          m_is_find         TYPE xfeld,
          m_stop_stack      TYPE i,
          m_debug           TYPE x,
          m_refresh         TYPE xfeld, "to refactor
          m_update          TYPE xfeld,
          is_step           TYPE xfeld,
          ms_stack_prev     TYPE   lcl_appl=>t_stack,
          ms_stack          TYPE   lcl_appl=>t_stack,
          is_history        TYPE xfeld,
          m_hist_step       TYPE i,
          m_step_delta      TYPE i,
          mt_vars_hist_view TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_vars_hist      TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_state          TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mv_recurse        TYPE i,
          mt_classes_types  TYPE TABLE OF lcl_appl=>t_classes_types,
          mo_window         TYPE REF TO lcl_ace_window,
          mv_f7_stop        TYPE xfeld,
          m_f6_level        TYPE i,
          m_target_stack    TYPE i,
          mo_tree_local     TYPE REF TO lcl_rtti_tree,
          mt_selected_var   TYPE TABLE OF t_sel_var,
          mv_stack_changed  TYPE xfeld,
          m_variable        TYPE REF TO data,
          mt_new_string     TYPE TABLE OF  string,
          m_quick           TYPE tpda_scr_quick_info,
          mr_statements     TYPE RANGE OF string.

    METHODS:
      constructor IMPORTING iv_prog   TYPE prog
                            iv_dest   TYPE text255
                            iv_model  TYPE text255
                            iv_apikey TYPE text255,


      hndl_script_buttons IMPORTING iv_stack_changed TYPE xfeld
                          RETURNING VALUE(rv_stop)   TYPE xfeld,
      show.

  PRIVATE SECTION.

    CONSTANTS: BEGIN OF c_kind,
                 struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                 table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                 elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                 class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                 intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                 ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
               END OF c_kind.


ENDCLASS.

CLASS lcl_mermaid DEFINITION INHERITING FROM lcl_popup FRIENDS  lcl_ace.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_if,
             if_ind      TYPE i,
             end_ind     TYPE i,
             before_else TYPE i,
           END OF ts_if,
           tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY.

    DATA: mo_viewer       TYPE REF TO lcl_ace,
          mo_mm_container TYPE REF TO cl_gui_container,
          mo_mm_toolbar   TYPE REF TO cl_gui_container,
          mo_toolbar      TYPE REF TO cl_gui_toolbar,
          mo_diagram      TYPE REF TO zcl_wd_gui_mermaid_js_diagram,
          mv_type         TYPE string,
          ms_if           TYPE ts_if,
          mt_if           TYPE tt_if.

    METHODS: constructor IMPORTING io_debugger TYPE REF TO lcl_ace
                                   iv_type     TYPE string,

      steps_flow IMPORTING iv_direction TYPE ui_func OPTIONAL,
      magic_search IMPORTING iv_direction TYPE ui_func OPTIONAL,
      add_toolbar_buttons,
      hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      open_mermaid IMPORTING iv_mm_string TYPE string.

ENDCLASS.


CLASS lcl_ai_api DEFINITION.

  PUBLIC SECTION.

    METHODS:      constructor IMPORTING
                                iv_dest   TYPE text255
                                iv_model  TYPE text255
                                iv_apikey TYPE text255 ,
      call_openai   IMPORTING iv_prompt TYPE string RETURNING VALUE(rv_answer) TYPE string,


      build_request
        IMPORTING
          iv_prompt  TYPE string
        EXPORTING
          ev_payload TYPE string ,

      send_request
        IMPORTING
          iv_payload  TYPE string
        EXPORTING
          ev_response TYPE string
          ev_error    TYPE xfeld,
      output
        IMPORTING
                  iv_prompt        TYPE string
                  iv_content       TYPE string
        RETURNING VALUE(rv_answer) TYPE string.

  PRIVATE SECTION.
    DATA mv_api_key TYPE string .
    DATA mv_dest TYPE text255 .
    DATA mv_model TYPE string .

ENDCLASS.

CLASS lcl_ai_api IMPLEMENTATION.

  METHOD constructor.

    mv_dest = iv_dest.
    mv_model = iv_model.
    mv_api_key = iv_apikey.

  ENDMETHOD.


  METHOD call_openai.
    DATA: prompt   TYPE string,
          payload  TYPE string,
          response TYPE string.

    "Build payload
    CALL METHOD build_request
      EXPORTING
        iv_prompt  = iv_prompt
      IMPORTING
        ev_payload = payload.

    CALL METHOD me->send_request
      EXPORTING
        iv_payload  = payload
      IMPORTING
        ev_response = response
        ev_error    = DATA(error).

    IF  error IS NOT INITIAL.
      rv_answer =  response.
    ELSE.
      rv_answer = output(
        EXPORTING
          iv_prompt  = iv_prompt
          iv_content =  response ).
    ENDIF.
  ENDMETHOD.

  METHOD build_request.

    DATA:  payload TYPE string.

    payload = |{ '{ "model": "' && p_model && '", "messages": [{ "role": "user", "content": "' && iv_prompt &&  '" }], "max_tokens": 10000 } ' }|.

    ev_payload =  payload.
  ENDMETHOD.

  METHOD send_request.

    DATA: lo_http_client TYPE REF TO if_http_client,
          response_body  TYPE string,
          header         TYPE string.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = p_dest
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 13.
    IF sy-subrc = 2.
      ev_response = 'Destination not found. Please check it in SM59 transaction'.
      ev_error = abap_true.
      RETURN.
    ELSEIF sy-subrc <> 0.
      ev_response = |cl_http_client=>create_by_destination error №' { sy-subrc }|.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "mv_api_key = 'lmstudio'. "any name for local LLMs or secret key for external
    mv_api_key = p_apikey.
    "set request header
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { mv_api_key }| ).

    lo_http_client->request->set_method('POST').

    "set payload
    lo_http_client->request->set_cdata( iv_payload ).

    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc = 0.
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.
      "Get response
      IF sy-subrc <> 0.
        response_body = lo_http_client->response->get_data( ).
        ev_response =  response_body.
      ELSE.
        response_body = lo_http_client->response->get_data( ).
        IF  response_body IS NOT INITIAL.
          ev_response =  response_body.
        ELSE.
          ev_response = 'Call was succeesful, but got no response'.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD output.

    DATA: text(1000) TYPE c,
          string     TYPE string,
          content    TYPE string,
          reasoning  TYPE string.

    TYPES: BEGIN OF lty_s_message,
             role              TYPE string,
             content           TYPE string,
             reasoning_content TYPE string,
           END           OF lty_s_message,
           lty_t_message TYPE STANDARD TABLE OF lty_s_message WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF lty_s_choice,
             index         TYPE string,
             message       TYPE lty_s_message,
             logprobs      TYPE string,
             finish_reason TYPE string,
           END      OF lty_s_choice,
           BEGIN OF lty_s_base_chatgpt_res,
             id      TYPE string,
             object  TYPE string,
             created TYPE string,
             model   TYPE string,
             choices TYPE TABLE OF lty_s_choice WITH NON-UNIQUE DEFAULT KEY,
           END OF lty_s_base_chatgpt_res.

    DATA response TYPE lty_s_base_chatgpt_res.

    DATA:  binary TYPE xstring.

    DATA: lo_x2c TYPE REF TO cl_abap_conv_in_ce.
    lo_x2c = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    binary = iv_content.
    lo_x2c->convert( EXPORTING input =  binary
                     IMPORTING data  =  string ).

    /ui2/cl_json=>deserialize( EXPORTING json =  string CHANGING data = response ).

    IF  response-choices IS NOT INITIAL.
      content = response-choices[ 1 ]-message-content.
      reasoning = response-choices[ 1 ]-message-reasoning_content.
    ELSE.
      content =  string.
      cl_abap_browser=>show_html(  html_string =  content title = 'Error (' ).
      RETURN.
    ENDIF.

    rv_answer =  content.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_rtti_tree DEFINITION FINAL. " INHERITING FROM lcl_popup.

  PUBLIC SECTION.

    TYPES: BEGIN OF t_classes_leaf,
             name TYPE string,
             type TYPE char1,
             key  TYPE salv_de_node_key,
           END OF t_classes_leaf.

    TYPES: BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
             path     TYPE string,
           END OF ts_table.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: main_node_key   TYPE salv_de_node_key,
          m_refresh       TYPE xfeld,
          m_leaf          TYPE string,
          m_hide          TYPE x,
          m_clear         TYPE flag,
          m_locals        TYPE x,
          m_globals       TYPE x,
          m_syst          TYPE x,
          m_class_data    TYPE x,
          m_ldb           TYPE x,
          m_locals_key    TYPE salv_de_node_key,
          m_globals_key   TYPE salv_de_node_key,
          m_class_key     TYPE salv_de_node_key,
          m_syst_key      TYPE salv_de_node_key,
          m_ldb_key       TYPE salv_de_node_key,
          m_icon          TYPE salv_de_tree_image,
          mt_vars         TYPE STANDARD TABLE OF lcl_appl=>var_table,
          mt_classes_leaf TYPE TABLE OF t_classes_leaf,
          m_prg_info      TYPE tpda_scr_prg_info,
          mo_viewer       TYPE REF TO lcl_ace,
          mo_tree         TYPE REF TO cl_salv_tree.

    METHODS constructor IMPORTING i_header   TYPE clike DEFAULT 'View'
                                  i_type     TYPE xfeld OPTIONAL
                                  i_cont     TYPE REF TO cl_gui_container OPTIONAL
                                  i_debugger TYPE REF TO lcl_ace OPTIONAL.

    METHODS del_variable IMPORTING  iv_full_name TYPE string i_state TYPE xfeld OPTIONAL.

    METHODS clear.

    METHODS add_buttons IMPORTING iv_type TYPE xfeld.
    METHODS add_node
      IMPORTING
                iv_name        TYPE string
                iv_rel         TYPE salv_de_node_key OPTIONAL
                iv_icon        TYPE salv_de_tree_image OPTIONAL
      RETURNING VALUE(rv_node) TYPE salv_de_node_key.

    METHODS add_obj_nodes
      IMPORTING
                is_var            TYPE lcl_appl=>var_table
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS delete_node IMPORTING iv_key TYPE salv_de_node_key.
    METHODS display IMPORTING io_debugger TYPE REF TO lcl_ace OPTIONAL.

    METHODS traverse
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
                iv_struc_name     TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_struct
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
                iv_struc_name     TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_elem
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                iv_value          TYPE any OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_obj
      IMPORTING
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                iv_value          TYPE any OPTIONAL
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

    METHODS traverse_table
      IMPORTING
                io_type_descr     TYPE REF TO cl_abap_typedescr
                iv_parent_key     TYPE salv_de_node_key
                iv_rel            TYPE salv_de_node_relation
                is_var            TYPE lcl_appl=>var_table
                ir_up             TYPE REF TO data OPTIONAL
                iv_parent_name    TYPE string OPTIONAL
      RETURNING VALUE(e_root_key) TYPE salv_de_node_key.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_kind,
                 struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                 table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                 elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                 class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                 intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                 ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
               END OF c_kind.

    DATA: tree_table TYPE tt_table.


    METHODS: hndl_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key,
      hndl_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.

ENDCLASS.

CLASS lcl_ai DEFINITION INHERITING FROM lcl_popup.

  PUBLIC SECTION.
    DATA: mo_ai_box               TYPE REF TO cl_gui_dialogbox_container,
          mo_ai_splitter          TYPE REF TO cl_gui_splitter_container,
          mo_ai_toolbar_container TYPE REF TO cl_gui_container,
          mo_ai_toolbar           TYPE REF TO cl_gui_toolbar,
          mo_prompt_container     TYPE REF TO cl_gui_container,
          mo_answer_container     TYPE REF TO cl_gui_container,
          mo_prompt_text          TYPE REF TO cl_gui_textedit,
          mo_answer_text          TYPE REF TO cl_gui_textedit,
          mv_prompt               TYPE string,
          mv_answer               TYPE string.

    METHODS:  constructor IMPORTING io_source TYPE REF TO cl_ci_source_include
                                    io_parent TYPE REF TO cl_gui_dialogbox_container,
      add_ai_toolbar_buttons,
      hnd_ai_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode.

ENDCLASS.

CLASS lcl_ai IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_ai_box = create( i_name = 'ACE: Abap Code Explorer - AI chat' i_width = 1400 i_hight = 400 ).
    CREATE OBJECT mo_ai_splitter
      EXPORTING
        parent  = mo_ai_box
        rows    = 3
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    "save new popup ref
    APPEND INITIAL LINE TO lcl_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
    <popup>-parent = io_parent.
    <popup>-child = mo_ai_box.

    SET HANDLER on_box_close FOR mo_ai_box.

    mo_ai_splitter->get_container(
         EXPORTING
           row       = 1
           column    = 1
         RECEIVING
           container = mo_ai_toolbar_container ).

    mo_ai_splitter->get_container(
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = mo_prompt_container ).

    mo_ai_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_answer_container  ).

    mo_ai_splitter->set_row_height( id = 1 height = '3' ).

    mo_ai_splitter->set_row_sash( id    = 1
                                  type  = 0
                                  value = 0 ).


    SET HANDLER on_box_close FOR mo_ai_box.


    CREATE OBJECT mo_prompt_text
      EXPORTING
        parent                 = mo_prompt_container
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

    CREATE OBJECT mo_answer_text
      EXPORTING
        parent                 = mo_answer_container
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

    mo_answer_text->set_readonly_mode( ).

    CREATE OBJECT mo_ai_toolbar EXPORTING parent = mo_ai_toolbar_container.
    add_ai_toolbar_buttons( ).
    mo_ai_toolbar->set_visible( 'X' ).

    "set prompt
    DATA string TYPE TABLE OF char255.

    APPEND INITIAL LINE TO string ASSIGNING FIELD-SYMBOL(<str>).
    <str> = 'Explain please the meaning of this ABAP code and provide a code review'.
    mv_prompt = <str>.
    APPEND INITIAL LINE TO string ASSIGNING <str>.


    LOOP AT io_source->lines INTO DATA(line).
      APPEND INITIAL LINE TO string ASSIGNING <str>.
      <str> = line.
      mv_prompt = mv_prompt && <str>.
    ENDLOOP.

    mo_prompt_text->set_text_as_r3table( string ).
    cl_gui_control=>set_focus( mo_ai_box ).

  ENDMETHOD.

  METHOD add_ai_toolbar_buttons.

    DATA: button TYPE ttb_button,
          events TYPE cntl_simple_events,
          event  LIKE LINE OF events.

    button  = VALUE #(
     ( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' ) ).

    mo_ai_toolbar->add_button_group( button ).

*   Register events
    event-eventid = cl_gui_toolbar=>m_id_function_selected.
    event-appl_event = space.
    APPEND event TO events.

    mo_ai_toolbar->set_registered_events( events = events ).
    SET HANDLER me->hnd_ai_toolbar FOR mo_ai_toolbar.

  ENDMETHOD.

  METHOD hnd_ai_toolbar.

    DATA:  prompt TYPE string.

    CASE fcode.

      WHEN 'AI'.

        DATA(lo_ai) = NEW lcl_ai_api( iv_model = p_model iv_dest = p_dest iv_apikey = p_apikey ).

        DATA text TYPE TABLE OF char255.
        CALL METHOD mo_prompt_text->get_text_as_stream
          IMPORTING
            text = text.
        CLEAR mv_prompt.
        LOOP AT text INTO DATA(line).
          CONCATENATE mv_prompt  line
                      "cl_abap_char_utilities=>newline
                 INTO mv_prompt.
        ENDLOOP.

        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN mv_prompt WITH ''.
        REPLACE ALL OCCURRENCES OF '#' IN mv_prompt WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN mv_prompt WITH ''''.
        DO 50 TIMES.
          REPLACE ALL OCCURRENCES OF '/' IN mv_prompt WITH ''.
        ENDDO.
        REPLACE ALL OCCURRENCES OF REGEX '[[:cntrl:]]' IN mv_prompt WITH ' '.

        mv_answer = lo_ai->call_openai( mv_prompt ).
        mo_answer_text->set_textstream( mv_answer ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_ace_window DEFINITION INHERITING FROM lcl_popup .

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
           END OF ts_table,

           BEGIN OF ts_calls,
             class TYPE string,
             event TYPE string,
             type  TYPE string,
             name  TYPE string,
             outer TYPE string,
             inner TYPE string,
           END OF ts_calls,
           tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer,

           BEGIN OF ts_calls_line,
             program   TYPE string,
             class     TYPE string,
             eventtype TYPE string,
             eventname TYPE string,
             index     TYPE i,
           END OF ts_calls_line,
           tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY,

           BEGIN OF ts_kword,
             index     TYPE i,
             line      TYPE i,
             name      TYPE string,
             from      TYPE i,
             to        TYPE i,
             tt_calls  TYPE tt_calls,
             to_prog   TYPE string,
             to_class  TYPE string,
             to_evtype TYPE string,
             to_evname TYPE string,
             "to_prog   type string,
           END OF ts_kword,

           BEGIN OF ts_calculated,
             program    TYPE string,
             line       TYPE i,
             calculated TYPE string,
           END OF ts_calculated,

           BEGIN OF ts_composing,
             program   TYPE string,
             line      TYPE i,
             composing TYPE string,
           END OF ts_composing,

           BEGIN OF ts_refvar,
             program TYPE string,
             name    TYPE string,
             class   TYPE string,
           END OF ts_refvar,

           tt_kword      TYPE STANDARD TABLE OF ts_kword WITH EMPTY KEY,
           tt_calculated TYPE STANDARD TABLE OF ts_calculated WITH EMPTY KEY,
           tt_composed   TYPE STANDARD TABLE OF ts_composing WITH EMPTY KEY,
           tt_refvar     TYPE STANDARD TABLE OF ts_refvar WITH EMPTY KEY,

           BEGIN OF ts_params,
             class     TYPE string,
             event     TYPE string,
             name      TYPE string,
             param     TYPE string,
             type      TYPE char1,
             preferred TYPE char1,
           END OF ts_params,
           tt_params TYPE STANDARD TABLE OF ts_params WITH EMPTY KEY,

           BEGIN OF ts_int_tabs,
             eventtype TYPE string,
             eventname TYPE string,
             name      TYPE string,
             type      TYPE string,
           END OF ts_int_tabs,
           tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY,

           BEGIN OF ts_prog,
             include    TYPE program,
             source     TYPE REF TO cl_ci_source_include,
             scan       TYPE REF TO cl_ci_scan,
             t_keywords TYPE tt_kword,
             selected   TYPE xfeld,
           END OF ts_prog,
           tt_progs TYPE STANDARD TABLE OF ts_prog WITH EMPTY KEY,

           BEGIN OF ts_source,
             tt_progs      TYPE tt_progs,
             t_calculated  TYPE tt_calculated,
             t_composed    TYPE tt_composed,
             t_params      TYPE tt_params,
             tt_tabs       TYPE tt_tabs,
             tt_calls_line TYPE tt_calls_line,
             tt_refvar     TYPE tt_refvar,
           END OF ts_source,

           BEGIN OF ts_locals,
             program    TYPE tpda_program,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             loc_fill   TYPE xfeld,
             locals_tab TYPE tpda_scr_locals_it,
             mt_fs      TYPE tpda_scr_locals_it,
           END OF ts_locals,

           BEGIN OF ts_globals,
             program     TYPE tpda_program,
             glob_fill   TYPE xfeld,
             globals_tab TYPE tpda_scr_globals_it,
             mt_fs       TYPE tpda_scr_locals_it,
           END OF ts_globals,

           BEGIN OF ts_watch,
             program TYPE string,
             line    TYPE i,
           END OF ts_watch,
           tt_watch TYPE STANDARD  TABLE OF ts_watch WITH EMPTY KEY,

           BEGIN OF ts_bpoint,
             program TYPE string,
             include TYPE string,
             line    TYPE i,
             type    TYPE char1,
             del     TYPE char1,
           END OF ts_bpoint,
           tt_bpoints TYPE STANDARD TABLE OF ts_bpoint WITH EMPTY KEY.

    TYPES tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY.

    DATA: m_version              TYPE x, " 0 - alpha, 01 - beta
          m_history              TYPE x,
          m_visualization        TYPE x,
          m_varhist              TYPE x,
          m_zcode                TYPE x,
          m_direction            TYPE x,
          m_prg                  TYPE tpda_scr_prg_info,
          m_debug_button         LIKE sy-ucomm,
          m_show_step            TYPE xfeld,
          mt_bpoints             TYPE tt_bpoints,
          mo_viewer              TYPE REF TO lcl_ace,
          mo_splitter_code       TYPE REF TO cl_gui_splitter_container,
          mo_splitter_var        TYPE REF TO cl_gui_splitter_container,
          mo_splitter_steps      TYPE REF TO cl_gui_splitter_container,
          mo_toolbar_container   TYPE REF TO cl_gui_container,
          mo_importing_container TYPE REF TO cl_gui_container,
          mo_locals_container    TYPE REF TO cl_gui_container,
          mo_exporting_container TYPE REF TO cl_gui_container,
          mo_code_container      TYPE REF TO cl_gui_container,
          mo_imp_exp_container   TYPE REF TO cl_gui_container,
          mo_editor_container    TYPE REF TO cl_gui_container,
          mo_steps_container     TYPE REF TO cl_gui_container,
          mo_stack_container     TYPE REF TO cl_gui_container,
          mo_hist_container      TYPE REF TO cl_gui_container,
          mo_code_viewer         TYPE REF TO cl_gui_abapedit,
          mt_stack               TYPE TABLE OF lcl_appl=>t_stack,
          mo_toolbar             TYPE REF TO cl_gui_toolbar,
          mo_salv_stack          TYPE REF TO cl_salv_table,
          mo_salv_steps          TYPE REF TO cl_salv_table,
          mo_salv_hist           TYPE REF TO cl_salv_table,
          mt_breaks              TYPE tpda_bp_persistent_it,
          mt_watch               TYPE tt_watch,
          mt_coverage            TYPE tt_watch,
          m_hist_depth           TYPE i,
          m_start_stack          TYPE i,
          mt_source              TYPE STANDARD  TABLE OF ts_source,
          ms_sources             TYPE ts_source,
          mt_params              TYPE STANDARD  TABLE OF ts_params,
          mt_locals_set          TYPE STANDARD TABLE OF ts_locals,
          mt_globals_set         TYPE STANDARD TABLE OF ts_globals.

    METHODS: constructor IMPORTING i_debugger TYPE REF TO lcl_ace i_additional_name TYPE string OPTIONAL,
      add_toolbar_buttons,
      hnd_toolbar FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
      set_program IMPORTING iv_program TYPE program,
      set_program_line IMPORTING iv_line LIKE sy-index OPTIONAL,
      create_code_viewer,
      show_stack,
      show_coverage,
      on_stack_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      on_editor_double_click  FOR EVENT dblclick OF cl_gui_abapedit IMPORTING sender,
      on_editor_border_click  FOR EVENT border_click OF cl_gui_abapedit IMPORTING line cntrl_pressed_set shift_pressed_set.

ENDCLASS.

CLASS lcl_ace IMPLEMENTATION.



  METHOD constructor.

    CONSTANTS: c_mask TYPE x VALUE '01'.

    mv_prog = iv_prog.
    mv_dest = iv_dest.
    mv_model = iv_model.
    mv_apikey = iv_apikey.

    is_step = abap_on.
    lcl_appl=>check_mermaid( ).
    lcl_appl=>init_lang( ).
    lcl_appl=>init_icons_table( ).

    mo_window = NEW lcl_ace_window( me ).


    mo_tree_local = NEW lcl_rtti_tree( i_header   = 'Objects & Code Flow'
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
    mo_window->set_program( CONV #( mo_window->m_prg-include ) ).
    mo_window->show_coverage( ).
    mo_window->set_program_line( mo_window->m_prg-line ).

    mo_window->show_stack( ).
    mo_tree_local->clear( ).
    mo_tree_local->main_node_key = mo_tree_local->add_node( iv_name = CONV #( mv_prog ) iv_icon = CONV #( icon_folder ) ).

    mo_tree_local->add_node( iv_name = 'Local Classes' iv_icon = CONV #( icon_folder ) iv_rel = mo_tree_local->main_node_key ).
    mo_tree_local->add_node( iv_name = 'Global Fields' iv_icon = CONV #( icon_header ) iv_rel = mo_tree_local->main_node_key ).
    mo_tree_local->add_node( iv_name = 'Events' iv_icon = CONV #( icon_oo_event ) iv_rel = mo_tree_local->main_node_key ).
    DATA(forms_rel) = mo_tree_local->add_node( iv_name = 'Subroutines' iv_icon = CONV #( icon_folder ) iv_rel = mo_tree_local->main_node_key ).
    mo_tree_local->add_node( iv_name = 'Code Flow' iv_icon = CONV #( icon_enhanced_bo ) iv_rel = mo_tree_local->main_node_key ).

    LOOP AT mo_window->ms_sources-t_params INTO DATA(subs) WHERE event = 'FORM' .
      DATA(form_name) = subs-name.
      AT NEW name.
        mo_tree_local->add_node( iv_name =  form_name iv_icon = CONV #( icon_biw_info_source_ina ) iv_rel =  forms_rel ).

      ENDAT.
    ENDLOOP.

    mo_tree_local->display( ).

  ENDMETHOD.

ENDCLASS.                    "lcl_ace IMPLEMENTATION

CLASS lcl_ace_window IMPLEMENTATION.

  METHOD constructor.

    DATA:  text TYPE char100.
    text = i_debugger->mv_prog.

    super->constructor( ).
    mo_viewer = i_debugger.
    m_history = m_varhist =  m_zcode  = '01'.
    m_hist_depth = 9.

    mo_box = create( i_name =  text i_width = 1100 i_hight = 300 ).
    SET HANDLER on_box_close FOR mo_box.
    CREATE OBJECT mo_splitter
      EXPORTING
        parent  = mo_box
        rows    = 3
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->get_container(
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = mo_code_container ).

    mo_splitter->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_toolbar_container ).

    mo_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_tables_container ).

    mo_splitter->set_row_height( id = 1 height = '4' ).
    mo_splitter->set_row_height( id = 2 height = '70' ).

    mo_splitter->set_row_sash( id    = 1
                               type  = 0
                               value = 0 ).

    CREATE OBJECT mo_splitter_code
      EXPORTING
        parent  = mo_code_container
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_code->get_container(
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = mo_editor_container ).

    mo_splitter_code->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_locals_container ).

    mo_splitter_code->set_column_width( EXPORTING id = 1 width = '25' ).

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_toolbar EXPORTING parent = mo_toolbar_container.
    add_toolbar_buttons( ).
    mo_toolbar->set_visible( 'X' ).
    create_code_viewer( ).

  ENDMETHOD.

  METHOD add_toolbar_buttons.

    DATA: button TYPE ttb_button,
          events TYPE cntl_simple_events,
          event  LIKE LINE OF events.

    button  = VALUE #(
     ( COND #( WHEN p_dest IS NOT INITIAL
      THEN VALUE #( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' ) ) )

     ( COND #( WHEN lcl_appl=>is_mermaid_active = abap_true
      THEN VALUE #( function = 'DIAGRAM' icon = CONV #( icon_workflow_process ) quickinfo = ' Calls Flow' text = 'Diagram' ) ) )
     ( function = 'SMART' icon = CONV #( icon_wizard ) quickinfo = 'Calculations sequence' text = 'Calculations Flow' )
     ( function = 'CODE' icon = CONV #( icon_customer_warehouse ) quickinfo = 'Only Z' text = 'Only Z' )
     "( function = 'COVERAGE' icon = CONV #( icon_wizard ) quickinfo = 'Coverage ' text = 'Coverage' )
     ( butn_type = 3  )
     ( function = 'STEPS' icon = CONV #( icon_next_step ) quickinfo = 'Steps table' text = 'Steps' )
     ( butn_type = 3  )
     ( function = 'INFO' icon = CONV #( icon_bw_gis ) quickinfo = 'Documentation' text = '' )
                    ).

    mo_toolbar->add_button_group( button ).

*   Register events
    event-eventid = cl_gui_toolbar=>m_id_function_selected.
    event-appl_event = space.
    APPEND event TO events.

    mo_toolbar->set_registered_events( events = events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.

  ENDMETHOD.

  METHOD set_program.

    lcl_source_parser=>parse_tokens( iv_program = iv_program io_debugger = mo_viewer ).

    LOOP AT ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
      CLEAR <prog>-selected.
    ENDLOOP.

    READ TABLE ms_sources-tt_progs WITH KEY include = iv_program ASSIGNING <prog>.
    IF sy-subrc = 0.

      <prog>-selected = abap_true.
      mo_code_viewer->set_text( table = <prog>-source->lines ).
    ENDIF.
  ENDMETHOD.

  METHOD set_program_line.

    TYPES: lntab TYPE STANDARD TABLE OF i.
    DATA lines TYPE lntab.

    mo_code_viewer->remove_all_marker( 2 ).
    mo_code_viewer->remove_all_marker( 4 ).

*    "session breakpoints
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING
        main_program         = mo_viewer->mo_window->m_prg-include
      IMPORTING
        breakpoints_complete = DATA(points)
      EXCEPTIONS
        c_call_error         = 1
        generate             = 2
        wrong_parameters     = 3
        OTHERS               = 4.

    LOOP AT points INTO DATA(point). "WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
      <line> = point-line.

      APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
      MOVE-CORRESPONDING point TO <point>.
      <point>-type = 'S'.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).

*    "exernal breakpoints
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING
        main_program         = mo_viewer->mo_window->m_prg-include
        flag_other_session   = abap_true
      IMPORTING
        breakpoints_complete = points
      EXCEPTIONS
        c_call_error         = 1
        generate             = 2
        wrong_parameters     = 3
        OTHERS               = 4.

    CLEAR lines.

    LOOP AT points INTO point. "WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lines ASSIGNING <line>.
      <line> = point-line.

      APPEND INITIAL LINE TO mt_bpoints ASSIGNING <point>.
      MOVE-CORRESPONDING point TO <point>.
      <point>-type = 'E'.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines ).

*    "watchpoints or coverage
*    CLEAR lines.
*    LOOP AT mt_watch INTO DATA(watch).
*      APPEND INITIAL LINE TO lines ASSIGNING <line>.
*      <line> = watch-line.
*    ENDLOOP.
*
*    "coverage
*    LOOP AT mt_coverage INTO DATA(coverage).
*      APPEND INITIAL LINE TO lines ASSIGNING <line>.
*      <line> = coverage-line.
*    ENDLOOP.

    CLEAR lines.
    "blue arrow - current line
    APPEND INITIAL LINE TO lines ASSIGNING <line>.
    <line> = iv_line.
    mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lines ).

    IF iv_line IS NOT INITIAL.
      mo_code_viewer->select_lines( EXPORTING from_line = iv_line to_line = iv_line ).
    ENDIF.

    mo_code_viewer->clear_line_markers( 'S' ).
    mo_code_viewer->draw( ).
  ENDMETHOD.

  METHOD create_code_viewer.

    DATA: events TYPE cntl_simple_events,
          event  TYPE cntl_simple_event.

    CHECK mo_code_viewer IS INITIAL.

    CREATE OBJECT mo_code_viewer
      EXPORTING
        parent           = mo_editor_container
        max_number_chars = 100.

    mo_code_viewer->init_completer( ).
    mo_code_viewer->upload_properties(
      EXCEPTIONS
        dp_error_create  = 1
        dp_error_general = 2
        dp_error_send    = 3
        OTHERS           = 4 ).

    event-eventid    = cl_gui_textedit=>event_double_click.
    APPEND event TO events.

    mo_code_viewer->set_registered_events( events ).
    mo_code_viewer->register_event_border_click( ).
    mo_code_viewer->register_event_break_changed( ).

    SET HANDLER on_editor_double_click FOR mo_code_viewer.
    SET HANDLER on_editor_border_click FOR mo_code_viewer.

    mo_code_viewer->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
    mo_code_viewer->create_document( ).
    mo_code_viewer->set_readonly_mode( 1 ).

  ENDMETHOD.

  METHOD show_stack.

    IF mo_salv_stack IS INITIAL.

      cl_salv_table=>factory(
        EXPORTING
          r_container  = mo_tables_container
        IMPORTING
          r_salv_table = mo_salv_stack
        CHANGING
          t_table      = mt_stack ).

      DATA:  lo_column  TYPE REF TO cl_salv_column.

      DATA(lo_columns) = mo_salv_stack->get_columns( ).
      "lo_columns->set_optimize( 'X' ).

      lo_column ?= lo_columns->get_column( 'STEP' ).
      lo_column->set_output_length( '3' ).
      lo_column->set_short_text( 'STEP' ).

      lo_column ?= lo_columns->get_column( 'STACKLEVEL' ).
      lo_column->set_output_length( '5' ).

      lo_column ?= lo_columns->get_column( 'PROGRAM' ).
      lo_column->set_output_length( '20' ).
      lo_column->set_long_text( 'Program/Class' ).
      lo_column->set_medium_text( 'Program/Class' ).

      lo_column ?= lo_columns->get_column( 'INCLUDE' ).
      lo_column->set_output_length( '40' ).

      lo_column ?= lo_columns->get_column( 'EVENTTYPE' ).
      lo_column->set_output_length( '20' ).

      lo_column ?= lo_columns->get_column( 'EVENTNAME' ).
      lo_column->set_output_length( '30' ).

      DATA(lo_event) =  mo_salv_stack->get_event( ).

      " Event double click
      SET HANDLER on_stack_double_click FOR lo_event.
      mo_salv_stack->display( ).
    ELSE.
      mo_salv_stack->refresh( ).
    ENDIF.

  ENDMETHOD.

  METHOD show_coverage.

    DATA: split TYPE TABLE OF string.
    CLEAR: mt_watch, mt_coverage,mt_stack.
    LOOP AT mo_viewer->mt_steps INTO DATA(step).

      READ TABLE mt_stack WITH KEY include = step-include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO mt_stack ASSIGNING FIELD-SYMBOL(<stack>).
        MOVE-CORRESPONDING step TO <stack>.

        SPLIT <stack>-program  AT '=' INTO TABLE split.
        <stack>-program = split[ 1 ].
      ENDIF.

      IF step-include <> mo_viewer->mo_window->m_prg-include.
        CONTINUE.
      ENDIF.

      "APPEND INITIAL LINE TO mt_coverage ASSIGNING FIELD-SYMBOL(<coverage>).
      "<coverage>-line = step-line.
    ENDLOOP.

    SORT mt_coverage.
    DELETE ADJACENT DUPLICATES FROM mt_coverage.

  ENDMETHOD.

  METHOD on_stack_double_click.

    READ TABLE mo_viewer->mo_window->mt_stack INDEX row INTO DATA(stack).
    "only for coverage stack selection should work.
    "CHECK mo_viewer->mo_window->mt_coverage IS NOT INITIAL.

    MOVE-CORRESPONDING stack TO mo_viewer->mo_window->m_prg.
    MOVE-CORRESPONDING stack TO mo_viewer->ms_stack.

    show_coverage( ).
    mo_viewer->show( ).

  ENDMETHOD.

  METHOD on_editor_double_click.
    sender->get_selection_pos( IMPORTING from_line = DATA(fr_line) from_pos = DATA(fr_pos) to_line = DATA(to_line) to_pos = DATA(to_pos) ).

  ENDMETHOD.

  METHOD on_editor_border_click.

    DATA:  type    TYPE char1.

    IF cntrl_pressed_set IS INITIAL.
      type = 'S'.
    ELSE.
      type = 'E'.
    ENDIF.

    LOOP AT mt_bpoints ASSIGNING FIELD-SYMBOL(<point>) WHERE line = line.
      type = <point>-type.

      CALL FUNCTION 'RS_DELETE_BREAKPOINT'
        EXPORTING
          index        = line
          mainprog     = m_prg-program
          program      = m_prg-include
          bp_type      = type
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.

      IF sy-subrc = 0.
        <point>-del = abap_true.
      ENDIF.
    ENDLOOP.

    IF sy-subrc <> 0. "create
      CALL FUNCTION 'RS_SET_BREAKPOINT'
        EXPORTING
          index        = line
          program      = m_prg-include
          mainprogram  = m_prg-program
          bp_type      = type
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.

    ENDIF.
    DELETE mt_bpoints WHERE del IS NOT INITIAL.
    set_program_line( ).
  ENDMETHOD.

  METHOD hnd_toolbar.

    CONSTANTS: c_mask TYPE x VALUE '01'.
    FIELD-SYMBOLS: <any> TYPE any.
    m_debug_button = fcode.
    READ TABLE mt_stack INDEX 1 INTO DATA(stack).
    CASE fcode.

      WHEN 'AI'.

        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY selected = abap_true INTO DATA(prog).
        NEW lcl_ai( io_source = prog-source io_parent =  mo_viewer->mo_window->mo_box ).

      WHEN 'DIAGRAM'.
        DATA(lo_mermaid) = NEW lcl_mermaid( io_debugger = mo_viewer iv_type =  'DIAG' ).

      WHEN 'SMART'.
        lo_mermaid = NEW lcl_mermaid( io_debugger = mo_viewer iv_type =  'SMART' ).

*      WHEN 'COVERAGE'.
*        show_coverage( ).
*        mo_viewer->show( ).

      WHEN 'CODE'.
        m_zcode = m_zcode BIT-XOR c_mask.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(source).
        lcl_source_parser=>code_execution_scanner( iv_program = source-include io_debugger = mo_viewer ).
        IF m_zcode IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Z & Standard' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Only Z code' ).
        ENDIF.

      WHEN 'INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.

        l_url = 'https://github.com/ysichov/Smart-Debugger'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.


      WHEN 'STEPS'.

        lcl_appl=>open_int_table( iv_name = 'Steps' it_tab = mo_viewer->mt_steps io_window = mo_viewer->mo_window ).


    ENDCASE.


  ENDMETHOD.

ENDCLASS.

CLASS lcl_sel_opt DEFINITION DEFERRED.

CLASS lcl_rtti IMPLEMENTATION.

  METHOD create_struc_handle.
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = i_tname
                                         RECEIVING  p_descr_ref    = DATA(lo_descr)
                                         EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 0.
      e_handle ?= lo_descr.
    ELSE.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD create_table_by_name.

    DATA: lo_new_tab  TYPE REF TO cl_abap_tabledescr,
          lo_new_type TYPE REF TO cl_abap_structdescr.

    create_struc_handle( EXPORTING i_tname = i_tname IMPORTING e_handle = lo_new_type ).
    lo_new_tab = cl_abap_tabledescr=>create(
      p_line_type  = lo_new_type
      p_table_kind = cl_abap_tabledescr=>tablekind_std
      p_unique     = abap_false ).
    CREATE DATA c_table TYPE HANDLE lo_new_tab.  "Create a New table type
  ENDMETHOD.

ENDCLASS.

CLASS lcl_data_transmitter DEFINITION.

  PUBLIC SECTION.
    EVENTS: data_changed EXPORTING VALUE(e_row) TYPE lcl_appl=>t_sel_row,
      col_changed EXPORTING VALUE(e_column) TYPE lvc_fname.
    METHODS: emit IMPORTING e_row TYPE lcl_appl=>t_sel_row,
      emit_col IMPORTING e_column TYPE lvc_fname.

ENDCLASS.

CLASS lcl_data_transmitter IMPLEMENTATION.

  METHOD  emit.
    RAISE EVENT data_changed EXPORTING e_row = e_row.

  ENDMETHOD.

  METHOD emit_col.
    RAISE EVENT col_changed EXPORTING e_column = e_column.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_data_receiver DEFINITION.

  PUBLIC SECTION.
    DATA: mo_transmitter TYPE REF TO lcl_data_transmitter,
          lo_tab_from    TYPE REF TO lcl_table_viewer,
          lo_sel_to      TYPE REF TO lcl_sel_opt,
          m_from_field   TYPE lvc_fname,
          m_to_field     TYPE lvc_fname.
    METHODS: constructor
      IMPORTING io_transmitter TYPE REF TO lcl_data_transmitter OPTIONAL
                io_tab_from    TYPE REF TO lcl_table_viewer OPTIONAL
                io_sel_to      TYPE REF TO lcl_sel_opt OPTIONAL
                i_from_field   TYPE lvc_fname OPTIONAL
                i_to_field     TYPE lvc_fname OPTIONAL,
      shut_down,
      update FOR EVENT data_changed OF lcl_data_transmitter IMPORTING e_row,
      update_col FOR EVENT col_changed OF lcl_data_transmitter IMPORTING e_column,
      on_grid_button_click
        FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no.

ENDCLASS.

CLASS lcl_sel_opt DEFINITION.

  PUBLIC SECTION.
    DATA: mo_viewer  TYPE REF TO lcl_table_viewer,
          mo_sel_alv TYPE REF TO cl_gui_alv_grid,
          mt_fcat    TYPE lvc_t_fcat,
          mt_sel_tab TYPE TABLE OF lcl_appl=>selection_display_s,
          ms_layout  TYPE lvc_s_layo.

    EVENTS: selection_done.
    METHODS:
      constructor IMPORTING io_viewer TYPE REF TO lcl_table_viewer io_container TYPE REF TO cl_gui_container,
      raise_selection_done,
      update_sel_tab,
      set_value IMPORTING  i_field TYPE any i_low TYPE any OPTIONAL i_high TYPE any OPTIONAL i_clear TYPE xfeld DEFAULT abap_true ,
      update_sel_row CHANGING c_sel_row TYPE lcl_appl=>selection_display_s.

  PRIVATE SECTION.
    METHODS:
      init_fcat IMPORTING i_dd_handle TYPE i,
      handle_sel_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname es_row_no er_event_data,
      on_grid_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no,
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING  er_data_changed,
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
      handle_context_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid IMPORTING e_object.

ENDCLASS.

CLASS lcl_table_viewer DEFINITION INHERITING FROM lcl_popup.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_column_emitter,
             column  TYPE lvc_fname,
             emitter TYPE REF TO lcl_data_transmitter,
           END OF t_column_emitter,
           BEGIN OF t_elem,
             field TYPE fieldname,
             elem  TYPE ddobjname,
           END OF t_elem.

    DATA: m_lang             TYPE ddlanguage,
          m_tabname          TYPE tabname,
          mo_alv             TYPE REF TO cl_gui_alv_grid,
          mo_sel             TYPE REF TO lcl_sel_opt,
          mr_table           TYPE REF TO data,
          mo_sel_parent      TYPE REF TO cl_gui_container,
          mo_alv_parent      TYPE REF TO cl_gui_container,
          mt_alv_catalog     TYPE lvc_t_fcat,
          mt_fields          TYPE TABLE OF t_elem,
          mo_column_emitters TYPE TABLE OF t_column_emitter,
          mo_sel_width       TYPE i,
          m_visible,
          m_std_tbar         TYPE x,
          m_show_empty       TYPE i,
          mo_window          TYPE REF TO lcl_ace_window.

    METHODS:
      constructor IMPORTING i_tname           TYPE any OPTIONAL
                            i_additional_name TYPE string OPTIONAL
                            ir_tab            TYPE REF TO data OPTIONAL
                            io_window         TYPE REF TO lcl_ace_window,
      refresh_table FOR EVENT selection_done OF lcl_sel_opt.

  PRIVATE SECTION.
    METHODS:
      create_popup,
      create_alv,
      create_sel_alv,
      set_header,
      create_field_cat IMPORTING i_tname           TYPE tabname
                       RETURNING VALUE(et_catalog) TYPE lvc_t_fcat,
      translate_field IMPORTING i_lang TYPE ddlanguage CHANGING c_fld TYPE lvc_s_fcat,
      handle_tab_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid  IMPORTING e_object,
      before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_doubleclick FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_column es_row_no,
      on_table_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.

ENDCLASS.

CLASS lcl_text_viewer DEFINITION FINAL INHERITING FROM lcl_popup.

  PUBLIC SECTION.
    DATA: mo_text     TYPE REF TO cl_gui_textedit.
    METHODS: constructor IMPORTING ir_str TYPE REF TO data.
ENDCLASS.

CLASS lcl_text_viewer IMPLEMENTATION.

  METHOD constructor.
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

  ENDMETHOD.

ENDCLASS.

CLASS lcl_data_receiver IMPLEMENTATION.

  METHOD constructor.

    lo_sel_to = io_sel_to.
    m_from_field =  i_from_field.
    m_to_field =  i_to_field.
    lo_tab_from = io_tab_from.
    mo_transmitter = io_transmitter.

    IF mo_transmitter IS NOT INITIAL.
      IF lo_tab_from IS INITIAL.
        SET HANDLER me->update FOR io_transmitter.
      ELSE.
        SET HANDLER me->update_col FOR io_transmitter.
      ENDIF.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES.
    ENDIF.

  ENDMETHOD.

  METHOD shut_down.

    IF mo_transmitter IS NOT INITIAL.
      SET HANDLER me->update FOR mo_transmitter  ACTIVATION space.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES  ACTIVATION space.
    ENDIF.
    CLEAR lo_sel_to.

  ENDMETHOD.

  METHOD on_grid_button_click.

    FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE.

    CHECK m_from_field = es_col_id-fieldname.
    ASSIGN lo_tab_from->mr_table->* TO <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    ASSIGN COMPONENT es_col_id-fieldname OF STRUCTURE <tab> TO  FIELD-SYMBOL(<f_field>).
    CHECK lo_sel_to IS NOT INITIAL.
    lo_sel_to->set_value( i_field = m_to_field i_low = <f_field> ).
    lo_sel_to->raise_selection_done( ).

  ENDMETHOD.

  METHOD  update.

    DATA: l_updated.

    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    IF <to>-range[] = e_row-range[].
      l_updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    MOVE-CORRESPONDING e_row TO <to>.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = e_row ).
    ENDIF.
    lo_sel_to->raise_selection_done( ).

  ENDMETHOD.

  METHOD update_col.

    DATA: l_updated,
          sel_row   TYPE lcl_appl=>t_sel_row.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <field> TYPE any.

    CHECK lo_sel_to IS NOT INITIAL.
    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    DATA(old_range) = <to>-range.
    CLEAR: <to>-sign, <to>-opti, <to>-low, <to>-high, <to>-range.
    ASSIGN lo_tab_from->mr_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT e_column OF STRUCTURE <row> TO <field>.
      IF line_exists( <to>-range[ low = <field> ] ).
        APPEND VALUE #( sign = 'I' opti = 'EQ' low = <field> ) TO <to>-range.
      ENDIF.
    ENDLOOP.

    IF sy-subrc NE 0." empty column
      APPEND VALUE #( sign = 'I' opti = 'EQ' low = '' ) TO <to>-range.
    ENDIF.

    LOOP AT <to>-range ASSIGNING FIELD-SYMBOL(<sel>).
      <to>-low = <sel>-low.
      lo_sel_to->update_sel_row( CHANGING c_sel_row = <to> ).
      EXIT.
    ENDLOOP.

    MOVE-CORRESPONDING <to> TO sel_row.
    IF <to>-range = old_range.
      l_updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = sel_row ).
      lo_sel_to->raise_selection_done( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*CLASS lcl_box_handler IMPLEMENTATION.
*
*   METHOD on_table_close.
*    DATA:  tabix LIKE sy-tabix.
*    sender->free( ).
*
*    "Free Memory
*    LOOP AT lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
*      IF <obj>-alv_viewer->mo_box = sender.
*         tabix = sy-tabix.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*    IF sy-subrc = 0.
*      FREE <obj>-alv_viewer->mr_table.
*      FREE <obj>-alv_viewer->mo_alv.
*
*      "shutdown receivers.
*      IF <obj>-alv_viewer->mo_sel IS NOT INITIAL.
*        LOOP AT <obj>-alv_viewer->mo_sel->mt_sel_tab INTO DATA(l_sel).
*          IF l_sel-receiver IS BOUND.
*            l_sel-receiver->shut_down( ).
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*      FREE <obj>-alv_viewer.
*      IF  tabix NE 0.
*        DELETE lcl_appl=>mt_obj INDEX  tabix.
*      ENDIF.
*    ENDIF.
*  ENDMETHOD.                    "ON_BOX_CLOSE
*
*ENDCLASS.               "lcl_box_handler

CLASS lcl_table_viewer IMPLEMENTATION.

  METHOD constructor.

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
          lo_new_tab     TYPE REF TO cl_abap_tabledescr.

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
      lcl_rtti=>create_table_by_name( EXPORTING i_tname = m_tabname CHANGING c_table = mr_table ).
    ELSE.
      FIELD-SYMBOLS:<any> TYPE any.
      ASSIGN ir_tab->* TO <any>.
      DATA lo_tabl  TYPE REF TO cl_abap_tabledescr.
      DATA lo_struc TYPE REF TO cl_abap_structdescr.
      lo_tabl ?= cl_abap_typedescr=>describe_by_data( <any> ).
      TRY.
          lo_struc ?= lo_tabl->get_table_line_type( ).
          ASSIGN ir_tab->* TO <any_tab>.
          TRY.
              LOOP AT lo_struc->components INTO DATA(component).

                IF component-type_kind NE 'h'.
                  comp-name = component-name.
                  comp-type ?= lo_struc->get_component_type( component-name ).
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

              lo_new_tab = cl_abap_tabledescr=>create(
                p_line_type  = handle_notab
                p_table_kind = cl_abap_tabledescr=>tablekind_std
                p_unique     = abap_false ).

              CREATE DATA l_notab TYPE HANDLE lo_new_tab.

              lo_new_tab = cl_abap_tabledescr=>create(
                p_line_type  = handle_tab2str
                p_table_kind = cl_abap_tabledescr=>tablekind_std
                p_unique     = abap_false ).

              CREATE DATA l_tab2str TYPE HANDLE lo_new_tab.

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
          lo_new_tab = cl_abap_tabledescr=>create(
            p_line_type  = handle_tab2str
            p_table_kind = cl_abap_tabledescr=>tablekind_std
            p_unique     = abap_false ).

          CREATE DATA l_tab2str TYPE HANDLE lo_new_tab.
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

  ENDMETHOD.

  METHOD create_popup.

    mo_box = create( i_width = 800 i_hight = 150 ).

    "save new popup ref
    APPEND INITIAL LINE TO lcl_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
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

  ENDMETHOD.

  METHOD create_alv.

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
    CREATE OBJECT lcl_appl=>c_dragdropalv.
    effect = cl_dragdrop=>move + cl_dragdrop=>copy.

    CALL METHOD lcl_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line' ##NO_TEXT
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD lcl_appl=>c_dragdropalv->get_handle IMPORTING handle = DATA(handle_alv).
    layout-s_dragdrop-grid_ddid = handle_alv.

    SET HANDLER   before_user_command
                  handle_user_command
                  handle_tab_toolbar
                  handle_doubleclick
                  lcl_dragdrop=>drag
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
      lcl_alv_common=>translate_field( CHANGING c_fld = <cat> ).
    ENDLOOP.

    mo_alv->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = mt_alv_catalog ).
    me->handle_user_command( EXPORTING e_ucomm = 'TECH' ).
    me->handle_user_command( EXPORTING e_ucomm = 'SHOW' ).
    mo_alv->set_toolbar_interactive( ).

  ENDMETHOD.

  METHOD translate_field.

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

  ENDMETHOD.

  METHOD create_sel_alv.

    IF mo_sel IS INITIAL.
      mo_sel     = NEW #( io_viewer = me io_container = mo_sel_parent ).
      SET HANDLER refresh_table FOR mo_sel.
    ELSE.
      mo_sel->update_sel_tab( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_header.

    DATA: text       TYPE as4text,
          header(80) TYPE c.

    SELECT SINGLE ddtext INTO  text
      FROM dd02t
     WHERE tabname = m_tabname
       AND ddlanguage = m_lang.

    header = |{ m_tabname } - {  text } { m_additional_name }|.
    mo_box->set_caption(  header ).

  ENDMETHOD.

  METHOD handle_tab_toolbar.

    IF m_visible IS INITIAL.
      DATA(toolbar) = VALUE ttb_button(
       ( function = 'SEL_ON' icon = icon_arrow_left quickinfo = 'Show Select-Options'  butn_type = 0 )
       ( butn_type = 3 ) ).
    ENDIF.

    APPEND VALUE #( function = 'TECH' icon = icon_wd_caption quickinfo = 'Tech names'  butn_type = 0 ) TO toolbar.

    LOOP AT lcl_appl=>mt_lang INTO DATA(lang).
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

  ENDMETHOD.

  METHOD create_field_cat.

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
    lcl_ddic=>get_text_table( EXPORTING i_tname = i_tname IMPORTING e_tab = l_texttab ).

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
      <catalog>-style = lcl_alv_common=>c_white.
      <catalog>-fieldname = ls-name.
      <catalog>-f4availabl = abap_true.

      IF l_dd04 IS INITIAL.
        <catalog>-scrtext_s = <catalog>-scrtext_m = <catalog>-scrtext_l = <catalog>-reptext = <catalog>-fieldname = ls-name.
      ELSE.
        MOVE-CORRESPONDING l_dd04 TO <catalog>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_doubleclick.

*    DATA: lo_table_descr TYPE REF TO cl_tpda_script_tabledescr,
*          table_clone    TYPE REF TO data.
*    FIELD-SYMBOLS: <f_tab>  TYPE STANDARD TABLE.
*
*    CHECK es_row_no-row_id IS NOT INITIAL.
*    ASSIGN mr_table->* TO  <f_tab>.
*    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
*    ASSIGN COMPONENT e_column-fieldname  OF STRUCTURE <tab> TO FIELD-SYMBOL(<val>).
*
*    CASE e_column-fieldname.
*      WHEN 'VALUE'.
*        IF sy-subrc = 0.
*          IF <val> = 'Table'.
*            ASSIGN COMPONENT 'REF'  OF STRUCTURE <tab> TO FIELD-SYMBOL(<ref>).
*            lcl_appl=>open_int_table( EXPORTING iv_name = CONV #( e_column-fieldname ) it_ref = <ref> io_window = mo_window ).
*          ENDIF.
*        ELSE.
*          TRY.
*              lo_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
*              table_clone = lo_table_descr->elem_clone( ).
*              lcl_appl=>open_int_table( EXPORTING iv_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
*            CATCH cx_sy_move_cast_error.
*          ENDTRY.
*        ENDIF.
*      WHEN 'STEP'.
*        MOVE-CORRESPONDING <tab> TO mo_window->m_prg.
*        MOVE-CORRESPONDING <tab> TO mo_window->mo_viewer->ms_stack.
*
*        mo_window->show_coverage( ).
*        mo_window->mo_viewer->show( ).
*      WHEN OTHERS. "check if it is an embedded table.
*        TRY.
*            lo_table_descr ?= cl_tpda_script_data_descr=>factory( |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| ).
*            table_clone = lo_table_descr->elem_clone( ).
*            lcl_appl=>open_int_table( EXPORTING iv_name = |{ m_additional_name }[ { es_row_no-row_id } ]-{ e_column-fieldname }| it_ref = table_clone io_window = mo_window ).
*          CATCH cx_sy_move_cast_error.
*        ENDTRY.
*    ENDCASE.
*
  ENDMETHOD.

  METHOD on_table_close.
    DATA:  tabix LIKE sy-tabix.
    sender->free( ).

    "Free Memory
    LOOP AT lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
      IF <obj>-alv_viewer->mo_box = sender.
        tabix = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0.
      FREE <obj>-alv_viewer->mr_table.
      FREE <obj>-alv_viewer->mo_alv.

      "shutdown receivers.
      IF <obj>-alv_viewer->mo_sel IS NOT INITIAL.
        LOOP AT <obj>-alv_viewer->mo_sel->mt_sel_tab INTO DATA(l_sel).
          IF l_sel-receiver IS BOUND.
            l_sel-receiver->shut_down( ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      FREE <obj>-alv_viewer.
      IF  tabix NE 0.
        DELETE lcl_appl=>mt_obj INDEX  tabix.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD before_user_command.

    CASE e_ucomm.
      WHEN '&INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
    ENDCASE.

  ENDMETHOD.

  METHOD handle_user_command.

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
            IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
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

    IF line_exists( lcl_appl=>mt_lang[ spras = e_ucomm ] ).
      m_lang = e_ucomm.
      set_header( ).
      mo_sel->set_value( i_field = 'SPRSL' i_low = m_lang ).
    ENDIF.

    CALL METHOD mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = it_fields[].

    lcl_alv_common=>refresh( mo_alv ).
    IF mo_sel IS BOUND.
      IF  e_ucomm = 'HIDE' OR e_ucomm = 'SHOW' OR e_ucomm = 'UPDATE' .
        mo_sel->update_sel_tab( ).
      ENDIF.
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
      mo_sel->mo_sel_alv->refresh_table_display(  ).
    ENDIF.

  ENDMETHOD.                           "handle_user_command

  METHOD refresh_table.

    DATA: row    TYPE lcl_appl=>t_sel_row,
          filter TYPE lvc_t_filt.

    CLEAR filter.
    set_header( ).

    LOOP AT mo_sel->mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO row.
        <sel>-transmitter->emit( e_row = row ).
      ENDIF.
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
      lcl_alv_common=>refresh( mo_sel->mo_sel_alv ).
      lcl_alv_common=>refresh( mo_alv ).
      mo_sel->mo_viewer->handle_user_command( 'SHOW' ).
      LOOP AT mo_column_emitters INTO DATA(l_emit).
        l_emit-emitter->emit_col( l_emit-column ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sel_opt IMPLEMENTATION.
  METHOD constructor.
    DATA: effect     TYPE i,
          handle_alv TYPE i.

    mo_viewer = io_viewer.
    mo_sel_alv = NEW #( i_parent = io_container ).
    update_sel_tab( ).
    CREATE OBJECT lcl_appl=>c_dragdropalv.
    effect =  cl_dragdrop=>copy. " + cl_dragdrop=>move.

    CALL METHOD lcl_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line'
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD lcl_appl=>c_dragdropalv->get_handle IMPORTING handle = handle_alv.
    ms_layout-s_dragdrop-col_ddid = handle_alv.
    init_fcat( handle_alv ).
    ms_layout-cwidth_opt = abap_true.
    ms_layout-col_opt = abap_true.
    ms_layout-ctab_fname = 'COLOR'.
    ms_layout-stylefname = 'STYLE'.

    "fields for F4 event handling
    DATA(gt_f4) = VALUE  lvc_t_f4( register   = abap_true chngeafter = abap_true
                             ( fieldname  = 'LOW'  )
                             ( fieldname  = 'HIGH'  ) ).

    mo_sel_alv->register_f4_for_fields( it_f4 = gt_f4 ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    mo_sel_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    SET HANDLER handle_user_command
                handle_sel_toolbar
                lcl_dragdrop=>drag
                lcl_dragdrop=>drop
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

  ENDMETHOD.

  METHOD init_fcat.

    mt_fcat = VALUE #(
     ( fieldname = 'IND'         coltext = '№'  outputlen = 3 style = '00000003' )
     ( fieldname = 'FIELD_LABEL' coltext = 'Label'  outputlen = 30 dragdropid = i_dd_handle )
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

  ENDMETHOD.

  METHOD raise_selection_done.

    DATA: row TYPE lcl_appl=>t_sel_row.

    lcl_alv_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    LOOP AT mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO row.
        <sel>-transmitter->emit( e_row = row ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD update_sel_tab.

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
      lcl_alv_common=>translate_field( EXPORTING i_lang = mo_viewer->m_lang CHANGING c_fld = l_catalog ).
      <sel_tab>-name = l_catalog-scrtext_l.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_sel_toolbar.

    e_object->mt_toolbar[] = VALUE #( butn_type = 0 disabled = ''
     ( function = 'SEL_OFF' icon = icon_arrow_right    quickinfo = 'Hide' )
     ( function = 'SEL_CLEAR' icon = icon_delete_row    quickinfo = 'Clear Select-Options' ) ).

  ENDMETHOD.

  METHOD set_value.

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
    IF <to>-transmitter IS BOUND.
      DATA: row TYPE lcl_appl=>t_sel_row.
      MOVE-CORRESPONDING <to> TO row.
      <to>-transmitter->emit( EXPORTING e_row = row ).
    ENDIF.

  ENDMETHOD.

  METHOD handle_doubleclick.

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

  ENDMETHOD.

  METHOD update_sel_row. "select patterns rules

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
        c_sel_row-option_icon = lcl_appl=>m_option_icons[ sign = c_sel_row-sign option = c_sel_row-opti ]-icon_name.
      CATCH cx_sy_itab_line_not_found.                  "#EC NO_HANDLER
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
                date_external            = <field>
              IMPORTING
                date_internal            = <field>
              EXCEPTIONS
                date_external_is_invalid = 1
                OTHERS                   = 2.
          ELSE.
            REPLACE ALL OCCURRENCES OF ':' IN <field> WITH ''.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
    c_sel_row-more_icon = COND #( WHEN c_sel_row-range IS INITIAL THEN icon_enter_more    ELSE icon_display_more  ).

    IF c_sel_row-receiver IS BOUND AND c_sel_row-inherited IS INITIAL.
      c_sel_row-inherited = icon_businav_value_chain.
    ENDIF.

  ENDMETHOD.

  METHOD on_f4.

    DATA: return_tab TYPE STANDARD TABLE OF ddshretval,
          objects    TYPE TABLE OF objec,
          objec      TYPE objec,
          l_otype    TYPE otype,
          l_plvar    TYPE plvar,
          l_multiple TYPE xfeld,
          l_clear    TYPE xfeld.

    IF e_fieldname = 'LOW'.
      l_multiple = abap_true.
    ENDIF.

    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
    DATA(l_fname) =  <sel>-field_label.

    lcl_appl=>mt_sel[] = mt_sel_tab[].
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
          callback_form     = 'CALLBACK_F4_SEL' "callback_method - doesn't work for local class
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

  ENDMETHOD.

  METHOD on_grid_button_click.

    DATA: l_tabfield TYPE rstabfield,
          opt        TYPE rsoptions VALUE 'XXXXXXXXXX',
          sign       TYPE raldb_sign,
          option     TYPE raldb_opti.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    CASE es_col_id.
      WHEN 'OPTION_ICON'. "edit select logical expression type
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
      WHEN 'MORE_ICON'. "edit ranges
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

  ENDMETHOD.

  METHOD on_data_changed.

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
    lcl_alv_common=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout ).
    raise_selection_done( ).

  ENDMETHOD.

  METHOD on_data_changed_finished.

    CHECK e_modified IS NOT INITIAL.
    RAISE EVENT selection_done.

  ENDMETHOD.

  METHOD handle_context_menu_request.

    DATA: func  TYPE ui_func,
          funcs TYPE ui_functions.

    DATA(l_index) = lcl_alv_common=>get_selected( mo_sel_alv ).

    IF l_index IS NOT INITIAL.
      READ TABLE mt_sel_tab INTO DATA(l_sel) INDEX l_index.
    ENDIF.

    e_object->get_functions( IMPORTING fcodes = DATA(fcodes) ). "Inactivate all standard functions

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

    IF l_sel-receiver IS NOT INITIAL OR l_index IS INITIAL.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DELR'
          text  = 'Delete receiver'.
    ENDIF.

  ENDMETHOD.

  METHOD handle_user_command.

    DATA:  sel_width TYPE i.

    IF e_ucomm = 'SEL_OFF'. "Hide select-options alv

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

    IF e_ucomm = 'SEL_CLEAR' OR e_ucomm = 'DELR'. "clear all selections
      mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).

      LOOP AT sel_rows INTO DATA(l_row).
        READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX l_row-index.
        IF e_ucomm = 'SEL_CLEAR'.
          CLEAR : <sel>-low, <sel>-high, <sel>-sign, <sel>-opti, <sel>-range.
        ELSEIF e_ucomm = 'DELR'.
          IF <sel>-receiver IS NOT INITIAL.
            <sel>-receiver->shut_down( ).
            FREE <sel>-receiver.
            CLEAR <sel>-receiver.
            CLEAR <sel>-inherited.
          ENDIF.
        ENDIF.
        update_sel_row( CHANGING c_sel_row = <sel> ).
      ENDLOOP.
      RAISE EVENT selection_done.
    ENDIF.

    lcl_alv_common=>refresh( mo_viewer->mo_alv ).
    RAISE EVENT selection_done.

  ENDMETHOD.                           "handle_user_command

ENDCLASS.

CLASS lcl_appl IMPLEMENTATION.

  METHOD init_icons_table.

    m_option_icons = VALUE #(
     ( sign = space option = space  icon_name = icon_led_inactive )
     ( sign = 'I'   option = 'EQ'   icon_name = icon_equal_green )
     ( sign = 'I'   option = 'NE'   icon_name = icon_not_equal_green )
     ( sign = 'I'   option = 'LT'   icon_name = icon_less_green )
     ( sign = 'I'   option = 'LE'   icon_name = icon_less_equal_green )
     ( sign = 'I'   option = 'GT'   icon_name = icon_greater_green )
     ( sign = 'I'   option = 'GE'   icon_name = icon_greater_equal_green )
     ( sign = 'I'   option = 'CP'   icon_name = icon_pattern_include_green )
     ( sign = 'I'   option = 'NP'   icon_name = icon_pattern_exclude_green )
     ( sign = 'I'   option = 'BT'   icon_name = icon_interval_include_green )
     ( sign = 'I'   option = 'NB'   icon_name = icon_interval_exclude_green )
     ( sign = 'E'   option = 'EQ'   icon_name = icon_equal_red )
     ( sign = 'E'   option = 'NE'   icon_name = icon_not_equal_red )
     ( sign = 'E'   option = 'LT'   icon_name = icon_less_red )
     ( sign = 'E'   option = 'LE'   icon_name = icon_less_equal_red )
     ( sign = 'E'   option = 'GT'   icon_name = icon_greater_red )
     ( sign = 'E'   option = 'GE'   icon_name = icon_greater_equal_red )
     ( sign = 'E'   option = 'CP'   icon_name = icon_pattern_include_red )
     ( sign = 'E'   option = 'NP'   icon_name = icon_pattern_exclude_red )
     ( sign = 'E'   option = 'BT'   icon_name = icon_interval_include_red )
     ( sign = 'E'   option = 'NB'   icon_name = icon_interval_exclude_red ) ).

  ENDMETHOD.

  METHOD init_lang.
*    SELECT c~spras t~sptxt INTO CORRESPONDING FIELDS OF TABLE mt_lang
*      FROM t002c AS c
*      INNER JOIN t002t AS t
*      ON c~spras = t~sprsl
*      WHERE t~spras = sy-langu
*      ORDER BY c~ladatum DESCENDING c~lauzeit DESCENDING.
  ENDMETHOD.

  METHOD check_mermaid.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.

    IF sy-subrc = 0.
      is_mermaid_active = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD open_int_table.

    DATA r_tab TYPE REF TO data.
    IF it_ref IS BOUND.
      r_tab = it_ref.
    ELSE.
      GET REFERENCE OF it_tab INTO r_tab.
    ENDIF.
    APPEND INITIAL LINE TO lcl_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW #(  i_additional_name = iv_name ir_tab = r_tab io_window = io_window ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_rtti_tree IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    mo_viewer = i_debugger.

    cl_salv_tree=>factory(
      EXPORTING
        r_container = i_cont
      IMPORTING
        r_salv_tree = mo_tree
      CHANGING
        t_table     = tree_table ).

    DATA(lo_setting) =  mo_tree->get_tree_settings( ).
    lo_setting->set_hierarchy_header( i_header ).
    lo_setting->set_hierarchy_size( 30 ).
    lo_setting->set_hierarchy_icon( CONV #( icon_tree ) ).

    DATA(lo_columns) = mo_tree->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    lo_columns->get_column( 'VALUE' )->set_short_text( 'Value' ).
    lo_columns->get_column( 'FULLNAME' )->set_visible( abap_false ).
    lo_columns->get_column( 'PATH' )->set_visible( abap_false ).
    lo_columns->get_column( 'TYPENAME' )->set_short_text( 'Type' ).
    lo_columns->get_column( 'TYPENAME' )->set_medium_text( 'Absolute Type' ).

    add_buttons( i_type ).

    DATA(lo_event) = mo_tree->get_event( ) .
    SET HANDLER hndl_double_click
                hndl_user_command FOR lo_event.

    m_globals = '01'.
    mo_tree->display( ).

  ENDMETHOD.

  METHOD add_buttons.

    DATA(lo_functions) = mo_tree->get_functions( ).
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

  ENDMETHOD.

  METHOD clear.

    mo_tree->get_nodes( )->delete_all( ).

    CLEAR: m_globals_key,
           m_locals_key,
           m_syst_key,
           m_ldb_key,
           m_class_key,
           mt_vars,
           mt_classes_leaf.

  ENDMETHOD.

  METHOD traverse.

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

  ENDMETHOD.

  METHOD traverse_struct.

    DATA: component       TYPE abap_component_tab,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          tree            TYPE ts_table,
          text            TYPE lvc_value,
          l_key           TYPE salv_de_node_key,
          l_rel           TYPE salv_de_node_relation,
          icon            TYPE salv_de_tree_image.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    l_rel = iv_rel.
    lo_struct_descr ?= io_type_descr.
    tree-ref =  ir_up.
    IF is_var-instance NE '{A:initial}'.
      tree-typename = lo_struct_descr->absolute_name.
      REPLACE FIRST OCCURRENCE OF '\TYPE=' IN tree-typename+0(6) WITH ''.
      IF tree-typename+0(1) = '%'.
        tree-typename = |{ lo_struct_descr->type_kind }({ lo_struct_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    tree-kind = lo_struct_descr->type_kind.

    IF m_icon IS INITIAL.
      icon = icon_structure.
    ELSE.
      icon = m_icon.
    ENDIF.

    text = is_var-short.
    tree-fullname = is_var-name.
    tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        l_key = leaf-key.
      ENDIF.
    ELSE.
      l_key = iv_parent_key.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = iv_parent_key.
      l_rel = iv_rel.
    ENDIF.

    IF  ( iv_struc_name IS SUPPLIED AND iv_struc_name IS NOT INITIAL ) OR iv_struc_name IS NOT SUPPLIED.
      IF  text IS NOT INITIAL.


        DATA(all_nodes) = mo_tree->get_nodes( )->get_all_nodes( ).
        LOOP AT all_nodes INTO DATA(nodes).
          DATA(lr_row) = nodes-node->get_data_row( ).
          DATA row TYPE ts_table.
          FIELD-SYMBOLS <ls_row> TYPE ts_table.
          ASSIGN lr_row->* TO <ls_row>.
          IF <ls_row>-fullname = is_var-name.
            DATA(l_node) = nodes-node.
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

      e_root_key = mo_tree->get_nodes( )->add_node(
             related_node   = l_key
             relationship   = l_rel
             data_row       = tree
             collapsed_icon =  icon
             expanded_icon  =  icon
             text           =  text
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

  METHOD traverse_elem.

    DATA: lo_elem_descr TYPE REF TO cl_abap_elemdescr,
          tree          TYPE ts_table,
          text          TYPE lvc_value,
          icon          TYPE salv_de_tree_image,
          l_key         TYPE salv_de_node_key,
          l_rel         TYPE salv_de_node_relation.

    lo_elem_descr ?= io_type_descr.
    tree-ref = is_var-ref.
    l_rel = iv_rel.

    IF is_var-instance NE '{A:initial}'.
      tree-typename = lo_elem_descr->absolute_name.
      REPLACE FIRST OCCURRENCE OF '\TYPE=' IN tree-typename WITH ''.
      IF tree-typename+0(1) = '%'.
        tree-typename = |{ lo_elem_descr->type_kind }({ lo_elem_descr->length / 2 })|.
      ENDIF.
    ENDIF.

    tree-kind = lo_elem_descr->type_kind.

    ASSIGN is_var-ref->* TO FIELD-SYMBOL(<new_value>).
    IF iv_value IS SUPPLIED.
      tree-value = iv_value.
    ELSE.
      IF <new_value> IS NOT INITIAL.
        tree-value = <new_value>.
      ENDIF.
    ENDIF.

    CASE lo_elem_descr->type_kind.
      WHEN 'D'.
        icon = icon_date.
      WHEN 'T'.
        icon = icon_bw_time_sap.
      WHEN 'C'.
        icon = icon_wd_input_field.
      WHEN 'P'.
        icon = icon_increase_decimal.
      WHEN 'g'.
        icon = icon_text_act.
      WHEN 'N' OR 'I'.
        icon = icon_pm_order.
      WHEN OTHERS.
        icon = icon_element.
    ENDCASE.

    text = is_var-short.
    tree-fullname = is_var-name."is_var-path.
    tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        l_key = leaf-key.
      ENDIF.
    ELSE.
      l_key = iv_parent_key.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = iv_parent_key.
      l_rel = iv_rel.
    ENDIF.

    DATA(all_nodes) = mo_tree->get_nodes( )->get_all_nodes( ).
    LOOP AT all_nodes INTO DATA(nodes).
      DATA(name) = nodes-node->get_text( ).
      DATA(lr_row) = nodes-node->get_data_row( ).
      FIELD-SYMBOLS <ls_row> TYPE ts_table.
      ASSIGN lr_row->* TO <ls_row>.
      IF <ls_row>-fullname = is_var-name.
        DATA(l_node) = nodes-node.
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

    DATA(lo_nodes) = mo_tree->get_nodes( ).

    TRY.
        CALL METHOD lo_nodes->add_node
          EXPORTING
            related_node   = l_key
            relationship   = l_rel
            data_row       = tree
            collapsed_icon = icon
            expanded_icon  = icon
            text           = text
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

  METHOD traverse_obj.

    DATA: tree  TYPE ts_table,
          text  TYPE lvc_value,
          icon  TYPE salv_de_tree_image,
          l_key TYPE salv_de_node_key,
          l_rel TYPE salv_de_node_relation.

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).

    IF sy-subrc = 0.
      DATA(lo_nodes) = mo_tree->get_nodes( ).
      DATA(l_node) =  lo_nodes->get_node( l_var-key ).

      IF l_var-ref = ir_up.
        RETURN.
      ENDIF.

    ELSE.
      l_rel = iv_rel.
    ENDIF.


    icon = icon_oo_object.
    text = is_var-short.
    tree-fullname = is_var-name.
    tree-path = is_var-path.

    "own new method
    IF is_var-cl_leaf IS NOT INITIAL.

      add_obj_nodes( EXPORTING is_var = is_var ).

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        l_key = leaf-key.
      ENDIF.
    ENDIF.

    IF l_key IS INITIAL.
      l_key = iv_parent_key.
      l_rel = iv_rel.
    ENDIF.

    e_root_key = mo_tree->get_nodes( )->add_node(
     related_node   = l_key
     relationship   = l_rel
     data_row       = tree
     collapsed_icon =  icon
     expanded_icon  =  icon
     text           =  text
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

  ENDMETHOD.

  METHOD traverse_table.

    DATA: lo_table_descr TYPE REF TO cl_abap_tabledescr,
          tree           TYPE ts_table,
          text           TYPE lvc_value,
          icon           TYPE salv_de_tree_image,
          l_key          TYPE salv_de_node_key,
          l_rel          TYPE salv_de_node_relation.

    FIELD-SYMBOLS: <tab> TYPE ANY TABLE.

    ASSIGN ir_up->* TO <tab>.
    DATA(lines) = lines( <tab> ).
    tree-ref = ir_up.
    l_key = iv_parent_key.

    lo_table_descr ?= io_type_descr.

    tree-fullname = |{ is_var-short } ({ lines })|.
    tree-kind = lo_table_descr->type_kind.
    IF is_var-instance NE '{A:initial}'.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = mo_viewer->ms_stack-include INTO DATA(prog).
      READ TABLE mo_viewer->mo_window->ms_sources-tt_tabs WITH KEY name = is_var-short INTO DATA(tab).
      IF sy-subrc <> 0.
        tree-typename = replace( val = lo_table_descr->absolute_name sub = '\TYPE=' with = '' ).
      ELSE.
        tree-typename = tab-type.
      ENDIF.
    ENDIF.
    icon = icon_view_table.

    IF is_var-name IS NOT INITIAL.
      text = tree-fullname.
    ELSE.
      text = tree-typename.
    ENDIF.

    l_rel = iv_rel.
    ASSIGN ir_up->* TO FIELD-SYMBOL(<new_value>).

    READ TABLE mt_vars WITH KEY name = is_var-name INTO DATA(l_var).
    DATA(all_nodes) = mo_tree->get_nodes( )->get_all_nodes( ).
    LOOP AT all_nodes INTO DATA(nodes).
      DATA(lr_row) = nodes-node->get_data_row( ).
      FIELD-SYMBOLS <ls_row> TYPE ts_table.
      ASSIGN lr_row->* TO <ls_row>.
      IF <ls_row>-fullname = is_var-name.
        DATA(l_node) = nodes-node.
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

      READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf INTO DATA(leaf).
      IF sy-subrc = 0.
        l_key = leaf-key.
      ENDIF.
    ELSE.
      l_key = iv_parent_key.
    ENDIF.

    READ TABLE mt_vars WITH KEY name = iv_parent_name TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.

      tree-fullname = is_var-name.
      e_root_key =
        mo_tree->get_nodes( )->add_node(
          related_node   = l_key
          relationship   = iv_rel
          collapsed_icon =  icon
          expanded_icon  =  icon
          data_row       = tree
          text           =  text
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

  METHOD add_node.

    rv_node =
          mo_tree->get_nodes( )->add_node(
            related_node   = iv_rel
            collapsed_icon = iv_icon
            expanded_icon = iv_icon
            relationship   = if_salv_c_node_relation=>last_child
            row_style = if_salv_c_tree_style=>intensified
            text           = CONV #( iv_name )
            folder         = abap_true
          )->get_key( ).

  ENDMETHOD.

  METHOD add_obj_nodes.

    DATA match TYPE match_result_tab.
    FIND ALL OCCURRENCES OF  '-' IN is_var-name RESULTS match. "Only first level of instance should be here
    IF lines( match ) > 1.
      RETURN.
    ENDIF.

    DATA  text TYPE lvc_value.
    DATA  node_key TYPE salv_de_node_key.
    DATA  icon TYPE salv_de_tree_image.

    CASE is_var-cl_leaf.
      WHEN 1.
        icon = icon_led_green.
        text = 'Public'.
      WHEN 2.
        icon = icon_led_red.
        text = 'Private'.
      WHEN 3.
        icon = icon_led_yellow.
        text = 'Protected'.
    ENDCASE.

    READ TABLE mt_classes_leaf WITH KEY name = is_var-parent type = is_var-cl_leaf ASSIGNING FIELD-SYMBOL(<class>).
    IF sy-subrc NE 0.

      READ TABLE mt_vars WITH KEY path = is_var-parent INTO DATA(var).
      node_key =
       mo_tree->get_nodes( )->add_node(
         related_node   = var-key
         relationship   = if_salv_c_node_relation=>last_child
         collapsed_icon =  icon
         expanded_icon  =  icon
         text           =  text
         folder         = abap_true
       )->get_key( ).

      APPEND INITIAL LINE TO mt_classes_leaf ASSIGNING <class>.
      <class>-name = is_var-parent.
      <class>-key =  node_key.
      <class>-type = is_var-cl_leaf.
    ENDIF.

  ENDMETHOD.

  METHOD delete_node.

    DATA(lo_nodes) = mo_tree->get_nodes( ).
    DATA(l_node) =  lo_nodes->get_node( iv_key ).
    IF l_node IS NOT INITIAL.
      l_node->delete( ).

    ENDIF.

  ENDMETHOD.

  METHOD display.

    DATA(lo_columns) = mo_tree->get_columns( ).
    lo_columns->get_column( 'KIND' )->set_visible( abap_false ).

    DATA(lo_nodes) = mo_tree->get_nodes( ).
    DATA(nodes) =  lo_nodes->get_all_nodes( ).


    DATA sub TYPE salv_t_nodes.
    LOOP AT nodes INTO DATA(l_node).
      READ TABLE sub WITH KEY node = l_node-node TRANSPORTING NO FIELDS. "expanding only first level nodes.
      IF sy-subrc NE 0.
        TRY.
            l_node-node->expand( ).
            sub = l_node-node->get_subtree( ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.
    ENDLOOP.
    mo_tree->display( ).

  ENDMETHOD.

  METHOD hndl_user_command.

    CONSTANTS: c_mask TYPE x VALUE '01'.

    CASE e_salv_function.

      WHEN 'REFRESH'."
        m_refresh = abap_true.
        "mo_viewer->run_script_hist( mo_viewer->m_hist_step ).
        mo_viewer->mo_tree_local->display( ).
        RETURN.

    ENDCASE.


  ENDMETHOD.

  METHOD hndl_double_click.

    DATA(lo_nodes) = mo_tree->get_nodes( ).
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
        <sel>-is_sel = abap_true.
      ENDIF.

      CASE <kind>.
        WHEN cl_abap_datadescr=>typekind_table.
          lcl_appl=>open_int_table( iv_name = <fullname> it_ref = <ref> io_window = mo_viewer->mo_window ).
        WHEN cl_abap_datadescr=>typekind_string.
          NEW lcl_text_viewer( <ref> ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD del_variable.

    DATA(vars_hist) = mo_viewer->mt_vars_hist.
    SORT vars_hist BY step DESCENDING.
    LOOP AT vars_hist INTO DATA(hist) WHERE name = iv_full_name.
      IF hist-del IS INITIAL.
        CLEAR: hist-ref, hist-first.
        hist-del = abap_true.
        hist-step = mo_viewer->m_hist_step - 1.
        INSERT hist INTO mo_viewer->mt_vars_hist INDEX 1.
      ENDIF.
    ENDLOOP.

    DATA(lo_nodes) = mo_tree->get_nodes( ).
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

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dragdrop IMPLEMENTATION.

  METHOD drag.

    DATA(dataobj) = NEW lcl_dd_data( ).
    dataobj->m_row = e_row-index.
    dataobj->m_column = e_column.
    e_dragdropobj->object = dataobj.

  ENDMETHOD.

  METHOD drop."It should be refactored someday...

    DATA: row          TYPE lcl_appl=>t_sel_row,
          set_receiver.

    LOOP AT lcl_appl=>mt_obj INTO DATA(lo).
      "to
      IF lo-alv_viewer->mo_sel IS BOUND.
        IF e_dragdropobj->droptargetctrl = lo-alv_viewer->mo_sel->mo_sel_alv.
          DATA(lo_to) = lo-alv_viewer->mo_sel.
        ENDIF.
      ENDIF.

      "from tab
      IF lo-alv_viewer->mo_alv = e_dragdropobj->dragsourcectrl.
        DATA(lo_from_tab) = lo-alv_viewer.
        CONTINUE.
      ENDIF.

      IF e_dragdropobj->dragsourcectrl = lo-alv_viewer->mo_sel->mo_sel_alv.
        DATA(lo_from_sel) = lo-alv_viewer->mo_sel.
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(sel_rows) ).
        lo-alv_viewer->mo_sel->mo_sel_alv->get_selected_cells( IMPORTING et_cell = DATA(sel_cells) ).
      ENDIF.
    ENDLOOP.

    IF lo_from_tab IS BOUND." tab to select
      FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE,
                     <f_field> TYPE any.
      lo_from_tab->mo_alv->get_selected_cells( IMPORTING et_cell = sel_cells ).
      lo_from_tab->mo_alv->get_selected_columns( IMPORTING et_index_columns = DATA(sel_col) ).

      LOOP AT sel_col INTO DATA(l_col).
        TRY.
            lo_from_tab->mt_alv_catalog[ fieldname = l_col-fieldname ]-style = cl_gui_alv_grid=>mc_style_button.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        READ TABLE lo_from_tab->mo_column_emitters WITH KEY column = l_col ASSIGNING FIELD-SYMBOL(<emitter>).
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO lo_from_tab->mo_column_emitters ASSIGNING <emitter>.
          <emitter>-column = l_col.
          <emitter>-emitter = NEW #( ).
        ENDIF.
      ENDLOOP.

      IF sy-subrc = 0.
        set_receiver = abap_true.
        CALL METHOD lo_from_tab->mo_alv->set_frontend_fieldcatalog EXPORTING it_fieldcatalog = lo_from_tab->mt_alv_catalog.
      ENDIF.

      TRY.
          ASSIGN lo_from_tab->mr_table->* TO <f_tab>.
          READ TABLE lo_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to_tab>) INDEX e_row.
          LOOP AT sel_cells INTO DATA(l_cell).
            IF sy-tabix = 1.
              DATA(l_colname) = l_cell-col_id-fieldname.
            ENDIF.
            READ TABLE <f_tab> INDEX l_cell-row_id ASSIGNING FIELD-SYMBOL(<f_str>).
            ASSIGN COMPONENT l_colname OF STRUCTURE <f_str> TO <f_field>.
            IF sy-subrc = 0.
              IF  set_receiver IS NOT INITIAL.
                IF <to_tab>-receiver IS BOUND.
                  <to_tab>-receiver->shut_down( ).
                ENDIF.
                CREATE OBJECT <to_tab>-receiver
                  EXPORTING
                    io_transmitter = <emitter>-emitter
                    i_from_field   = CONV #( sel_cells[ 1 ]-col_id )
                    i_to_field     = <to_tab>-field_label
                    io_sel_to      = lo_to
                    io_tab_from    = lo_from_tab.
                SET HANDLER <to_tab>-receiver->on_grid_button_click FOR lo_from_tab->mo_alv.
              ENDIF.

              IF <to_tab>-range IS INITIAL.
                <to_tab>-low = <f_field>.
              ENDIF.
              IF NOT line_exists( <to_tab>-range[ low = <f_field> ] ).
                APPEND VALUE #( sign = 'I' opti = 'EQ' low = <f_field>  ) TO <to_tab>-range.
              ENDIF.
            ENDIF.
          ENDLOOP.
          lo_to->update_sel_row( CHANGING c_sel_row = <to_tab> ).
        CATCH cx_sy_itab_line_not_found.                "#EC NO_HANDLER
      ENDTRY.
    ENDIF.

    "select to select
    IF lo_from_sel NE lo_to.
      IF sel_rows[] IS INITIAL.
        DELETE sel_cells WHERE col_id NE 'FIELD_LABEL'.
        LOOP AT sel_cells INTO DATA(l_sel).
          APPEND INITIAL LINE TO sel_rows ASSIGNING FIELD-SYMBOL(<row>).
          <row>-index = l_sel-row_id-index.
        ENDLOOP.
      ENDIF.

      LOOP AT sel_rows ASSIGNING <row>.
        READ TABLE lo_from_sel->mt_sel_tab ASSIGNING FIELD-SYMBOL(<from_tab>) INDEX <row>-index.
        IF lines( sel_rows ) = 1.
          READ TABLE lo_to->mt_sel_tab ASSIGNING <to_tab> INDEX e_row.
        ELSE.
          READ TABLE lo_to->mt_sel_tab ASSIGNING <to_tab> WITH KEY field_label = <from_tab>-field_label.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        MOVE-CORRESPONDING <from_tab> TO row.
        MOVE-CORRESPONDING row TO <to_tab>.
        <from_tab>-emitter = icon_workflow_external_event.
        <to_tab>-inherited = icon_businav_value_chain.
        IF <from_tab>-transmitter IS INITIAL.
          CREATE OBJECT <from_tab>-transmitter.
        ENDIF.
        IF <to_tab>-receiver IS NOT INITIAL.
          <to_tab>-receiver->shut_down( ). "receiver clearing
          FREE <to_tab>-receiver.
        ENDIF.
        CREATE OBJECT <to_tab>-receiver
          EXPORTING
            io_transmitter = <from_tab>-transmitter
            io_sel_to      = lo_to
            i_to_field     = <to_tab>-field_label.
      ENDLOOP.
    ENDIF.

    DATA(lo_alv) = CAST cl_gui_alv_grid( e_dragdropobj->dragsourcectrl ).
    lcl_alv_common=>refresh( EXPORTING i_obj = lo_alv ).

    lo_alv ?= e_dragdropobj->droptargetctrl.
    lo_to->raise_selection_done( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_source_parser IMPLEMENTATION.

  METHOD parse_tokens.

    DATA: lr_scan       TYPE REF TO cl_ci_scan,
          prev          TYPE string,
          change        TYPE string,
          split         TYPE TABLE OF string,
          lo_scan       TYPE REF TO cl_ci_scan,
          lo_statement  TYPE REF TO if_ci_kzn_statement_iterator,
          lo_procedure  TYPE REF TO if_ci_kzn_statement_iterator,
          token         TYPE lcl_ace_window=>ts_kword,
          calculated   TYPE lcl_ace_window=>ts_calculated,
          composed      TYPE lcl_ace_window=>ts_composing,
          tokens        TYPE lcl_ace_window=>tt_kword,
          lt_calculated TYPE lcl_ace_window=>tt_calculated,
          lt_composed   TYPE lcl_ace_window=>tt_composed,
          call          TYPE lcl_ace_window=>ts_calls,
          call_line     TYPE lcl_ace_window=>ts_calls_line,
          tab          TYPE lcl_ace_window=>ts_int_tabs,
          tabs          TYPE lcl_ace_window=>tt_tabs,
          eventtype     TYPE string,
          eventname     TYPE string,
          param         TYPE lcl_ace_window=>ts_params,
          par           TYPE char1,
          type          TYPE char1,
          class         TYPE xfeld,
          cl_name       TYPE string,
          preferred     TYPE xfeld.

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = iv_program INTO DATA(prog).
    IF sy-subrc <> 0.
      prog-source = cl_ci_source_include=>create( p_name = iv_program ).
      lo_scan = NEW cl_ci_scan( p_include = prog-source ).

      prog-include = iv_program.

      lo_statement = cl_cikzn_scan_iterator_factory=>get_statement_iterator( ciscan = lo_scan ).
      lo_procedure = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = lo_scan ).


      "methods in definition should be overwritten by Implementation section
      IF iv_class IS NOT INITIAL.
        class = abap_true.
        call_line-class = param-class = iv_class.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = iv_evname eventtype = 'METHOD' ASSIGNING FIELD-SYMBOL(<call_line>).
        IF sy-subrc = 0.
          <call_line>-index = lo_procedure->statement_index + 1.
        ENDIF.
      ENDIF.

      TRY.
          lo_statement->next( ).
        CATCH cx_scan_iterator_reached_end.
          EXIT.
      ENDTRY.

      DATA(kw) = lo_statement->get_keyword( ).

      DATA(word) = lo_statement->get_token( offset = 2 ).

      lo_procedure->statement_index = lo_statement->statement_index.
      lo_procedure->statement_type = lo_statement->statement_type.

      DATA(max) = lines( lo_scan->statements ).
      DO.
        CLEAR token-tt_calls.
        "IF sy-index <> 1.
        TRY.
            lo_procedure->next( ).
          CATCH cx_scan_iterator_reached_end.
        ENDTRY.
        kw = lo_procedure->get_keyword( ).

        token-name = kw.
        token-index = lo_procedure->statement_index.
        READ TABLE lo_scan->statements INDEX lo_procedure->statement_index INTO DATA(statement).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        READ TABLE lo_scan->tokens INDEX statement-from INTO DATA(l_token).
        token-line = calculated-line = composed-line = l_token-row.
        calculated-program = composed-program = iv_program.

        DATA  new TYPE xfeld.

        IF kw = 'CLASS'.
          class = abap_true.
        ENDIF.

        IF kw = 'FORM' OR kw = 'METHOD' OR kw = 'METHODS' OR kw = 'CLASS-METHODS'.
          tab-eventtype =  eventtype = param-event =  kw.

          CLEAR  eventname.
          IF kw = 'FORM'.
            CLEAR:  class, param-class.
          ELSE.
            tab-eventtype =  eventtype = param-event =  'METHOD'.
          ENDIF.
        ENDIF.

        IF kw = 'ENDFORM' OR kw = 'ENDMETHOD'.
          CLEAR:  eventtype,  eventname, tabs.
          IF param-param IS INITIAL. "No params - save empty row if no params
            READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = param-event name = param-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              CLEAR param-type.
              APPEND param TO io_debugger->mo_window->ms_sources-t_params.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR  prev.
        IF kw = 'ASSIGN' OR kw = 'ADD' OR kw = 'SUBTRACT' .
          DATA(count) = 0.
        ENDIF.
        CLEAR:  new, token-to_evname, token-to_evtype, token-to_class .


        WHILE 1 = 1.
          IF kw IS INITIAL.
            EXIT.
          ENDIF.
          CLEAR  change.
          word = lo_procedure->get_token( offset = sy-index ).

          IF ( word CS '(' AND ( NOT word CS ')' ) ) OR word CS '->' OR word CS '=>'."can be method call
            call-name = word.
            call-event = 'METHOD'.
            REPLACE ALL OCCURRENCES OF '(' IN call-name WITH ''.
            FIND FIRST OCCURRENCE OF '->' IN  call-name.
            IF sy-subrc = 0.
              SPLIT call-name  AT '->' INTO TABLE split.
              call-class = split[ 1 ].
              call-name = split[ 2 ].
            ENDIF.

            FIND FIRST OCCURRENCE OF '=>' IN  call-name.
            IF sy-subrc = 0.
              SPLIT call-name  AT '=>' INTO TABLE split.
              call-class = split[ 1 ].
              call-name = split[ 2 ].
            ENDIF.

            IF call-class = 'ME' AND iv_class IS NOT INITIAL.
              call-class  =  iv_class.
            ENDIF.

            IF call-class IS INITIAL AND iv_class IS NOT INITIAL.
              call-class  =  iv_class.
            ENDIF.

            token-to_evname = call-name.
            token-to_evtype = call-event = 'METHOD'.
            IF  new = abap_true.
              call-class = call-name.
              call-name =  token-to_evname = 'CONSTRUCTOR'.
            ENDIF.
            IF  new = abap_true.
              READ TABLE lt_calculated WITH KEY line = l_token-row program = iv_program INTO DATA(calc).
              IF sy-subrc = 0.
                APPEND INITIAL LINE TO  io_debugger->mo_window->ms_sources-tt_refvar ASSIGNING FIELD-SYMBOL(<refvar>).
                <refvar>-name = calc-calculated.
                <refvar>-class = call-class.
                call-class = call-class.
              ENDIF.
            ENDIF.

            READ TABLE io_debugger->mo_window->ms_sources-tt_refvar WITH KEY name = call-class INTO DATA(refvar).
            IF sy-subrc = 0.
              call-class = refvar-class.
            ENDIF.

            token-to_class = call-class.
          ENDIF.

          IF sy-index = 1 AND token-name = word.
            CONTINUE.
          ENDIF.

          IF sy-index = 2 AND ( kw = 'DATA' OR kw = 'PARAMETERS' ).
            WRITE: 'var =', word.
            tab-name = word.
          ENDIF.

          IF sy-index = 2 AND kw = 'PERFORM'.
            token-to_evname = call-name = word.
            token-to_evtype = call-event = 'FORM'.
          ENDIF.

          IF sy-index = 2 AND  class = abap_true AND param-class IS INITIAL.
            call_line-class = param-class = word.
          ENDIF.

          IF sy-index = 2 AND  eventtype IS NOT INITIAL AND  eventname IS INITIAL.
            tab-eventname =  eventname = param-name = word.

            MOVE-CORRESPONDING tab TO call_line.
            call_line-index = lo_procedure->statement_index + 1.
            "methods in definition should be overwritten by Implementation section
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = call_line-eventname eventtype = call_line-eventtype ASSIGNING <call_line>.
            IF sy-subrc = 0.
              <call_line> = call_line.
            ELSE.
              IF iv_class IS INITIAL.
                call_line-program = iv_program.
              ENDIF.
              APPEND call_line TO io_debugger->mo_window->ms_sources-tt_calls_line.
            ENDIF.

          ENDIF.

          IF word = ''.
            IF call IS NOT INITIAL.
              APPEND call TO token-tt_calls.
            ENDIF.
            CLEAR call.
            CASE kw.
              WHEN 'COMPUTE'.
                IF  NOT  prev CO '0123456789.+-/* '.
                  composed-composing =  prev.
                  APPEND  composed TO lt_composed.
                ENDIF.
              WHEN 'CLEAR' OR 'SORT' OR 'CONDENSE'."no logic
              WHEN 'FORM'.
                IF param-name IS NOT INITIAL.
                  APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                  CLEAR param.
                ENDIF.
            ENDCASE.
            EXIT.
          ENDIF.

          IF word = 'USING' OR word = 'IMPORTING'.
            param-type = 'I'.
            CLEAR:  type,  par.
          ELSEIF word = 'CHANGING' OR word = 'EXPORTING' OR word = 'RETURNING'.

            IF param-param IS NOT INITIAL.
              APPEND param TO io_debugger->mo_window->ms_sources-t_params.
              CLEAR:  type,  par, param-param.
            ENDIF.

            param-type = 'E'.
            CLEAR:  type,  par.
          ELSEIF word = 'OPTIONAL' OR word = 'PREFERRED'.
            CONTINUE.
          ELSEIF word = 'PARAMETER'.
            preferred = abap_true.
            CONTINUE.
          ENDIF.

          IF  preferred = abap_true.
            READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = 'METHOD' name = param-name param = word ASSIGNING FIELD-SYMBOL(<param>).
            IF sy-subrc = 0.
              <param>-preferred = abap_true.
            ENDIF.

            CLEAR  preferred.
            CONTINUE.
          ENDIF.

          IF word <> 'CHANGING' AND word <> 'EXPORTING' AND word <> 'RETURNING' AND word <> 'IMPORTING' AND word <> 'USING'.
            IF kw = 'FORM' OR kw = 'METHODS' OR kw = 'CLASS-METHODS'.
              IF  par = abap_true AND  type IS INITIAL AND word NE 'TYPE'.

                APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                CLEAR:  par, param-param.
              ENDIF.

              IF  par IS INITIAL AND sy-index > 3.
                param-param = word.
                par = abap_true.
                CONTINUE.
              ENDIF.
              IF  par = abap_true AND  type IS INITIAL AND word = 'TYPE'.
                type = abap_true.
                CONTINUE.
              ENDIF.
              IF  par = abap_true AND  type = abap_true.

                APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                CLEAR:  type,  par, param-param.
              ENDIF.
            ENDIF.
          ENDIF.

          DATA  temp TYPE char30.
          temp = word.

          IF  temp+0(5) = 'DATA('.
            SHIFT  temp LEFT BY 5 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN  temp WITH ''.
          ENDIF.

          IF  temp+0(6) = '@DATA('.
            SHIFT  temp LEFT BY 6 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN  temp WITH ''.
          ENDIF.

          IF  temp+0(13) = 'FIELD-SYMBOL('.
            SHIFT  temp LEFT BY 13 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN  temp WITH ''.
          ENDIF.

          IF word = 'NEW'.
            new = abap_true.

          ENDIF.

          FIND FIRST OCCURRENCE OF '->' IN word.
          IF sy-subrc = 0.
            CLEAR  new.
          ENDIF.

          CASE kw.
            WHEN 'DATA' OR 'PARAMETERS'.
              IF (   prev = 'OF' ) AND  temp <> 'TABLE' AND  temp <> 'OF'.
                tab-type =  temp.
                APPEND tab TO tabs.
              ENDIF.

            WHEN 'COMPUTE'.
              IF  temp CA '=' AND  new IS INITIAL..
                change =  prev.
              ENDIF.

              IF (  prev = '=' OR  prev CA '+-/*' ) AND  temp <> 'NEW'.
                IF NOT  temp  CA '()' .
                  IF NOT  temp  CO '0123456789. '.
                    composed-composing =  temp.
                    APPEND  composed TO lt_composed.
                    IF call IS NOT INITIAL.
                      call-outer =  temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'PERFORM' .

              IF   temp = 'USING' OR  temp = 'CHANGING' .
                CLEAR  prev.
              ENDIF.

              IF   prev = 'USING' OR  prev = 'CHANGING' .

                IF NOT  temp  CA '()' .
                  IF NOT  temp  CO '0123456789. '.
                    call-outer =  temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.
                    change =  temp.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'CREATE' OR 'CALL'.
              DATA: import TYPE xfeld,
                    export.

              IF  prev = 'FUNCTION' AND kw = 'CALL'.
                token-to_evtype =   call-event = 'FUNCTION'.
                token-to_evname =  call-name = word.
                REPLACE ALL OCCURRENCES OF '''' IN  token-to_evname WITH ''.
              ENDIF.

              IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
                export = abap_true.
                CLEAR  import.
                CONTINUE.

              ELSEIF word = 'IMPORTING'.
                import = abap_true.
                CLEAR  export.
                CONTINUE.

              ENDIF.

              IF  prev = 'OBJECT'.
                "WRITE : 'value',  temp.
*          CONTINUE.
              ENDIF.

              IF   prev = '='.
                IF NOT  temp  CA '()'.
                  IF NOT  temp  CO '0123456789. '.
                    IF  import = abap_true.
                      call-outer =  temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                      calculated-calculated =  temp.
                      APPEND  calculated TO lt_calculated.
                    ELSEIF  export = abap_true.
                      call-outer =  temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                      composed-composing =  temp.
                      APPEND  composed TO lt_composed.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSE.
                IF NOT  temp  CO '0123456789. ' AND  temp <> '=' AND (  import = abap_true OR  export = abap_true ).
                  call-inner =  temp.
                ENDIF.
              ENDIF.

            WHEN 'CLEAR' OR 'SORT'.
              change =  temp.
            WHEN  'CONDENSE'.

              IF  temp <> 'NO-GAPS'.
                change =  temp.
              ENDIF.
            WHEN 'ASSIGN' OR 'UNASSIGN'.
              ADD 1 TO  count.
              IF  count <> 2.
                change =  temp.
              ENDIF.
            WHEN 'ADD' OR 'SUBTRACT'.
              ADD 1 TO  count.
              IF  count = 1.
                IF  NOT  temp CO '0123456789.() '.
                  composed-composing =  temp.
                  APPEND  composed TO lt_composed.
                ENDIF.
              ENDIF.
              IF  count = 3.
                change =  temp.
              ENDIF.
            WHEN 'READ'.
              IF  prev =  'INTO' OR  prev =  'ASSIGNING'.
                change =  temp.
              ENDIF.

            WHEN 'SELECT'.
              IF  (  prev =  'INTO' OR  prev =  '(' ) AND (  temp <> 'TABLE' AND  temp <> '('  AND  temp <> ')' AND   temp <> ',' ).
                change =  temp.
              ENDIF.

            WHEN OTHERS.

          ENDCASE.
          IF call-event = 'METHOD'.
            IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
              export = abap_true.
              CLEAR  import.
              CONTINUE.

            ELSEIF word = 'IMPORTING'.
              import = abap_true.
              CLEAR  export.
              CONTINUE.
            ENDIF.

            IF   temp = 'USING' OR  temp = 'CHANGING' .
              CLEAR  prev.
            ENDIF.

            IF   prev = 'USING' OR  prev = 'CHANGING' .

              IF NOT  temp  CA '()' .
                IF NOT  temp  CO '0123456789. '.
                  call-outer =  temp.
                  READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND call TO token-tt_calls.
                  ENDIF.
                  change =  temp.
                ENDIF.
              ENDIF.
            ENDIF.

            IF   prev = '='.
              IF NOT  temp  CA '()'.
                IF NOT  temp  CO '0123456789. '.
                  IF  import = abap_true.
                    call-outer =  temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.

                    calculated-calculated =  temp.
                    APPEND  calculated TO lt_calculated.
                  ELSEIF  export = abap_true.
                    call-outer =  temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.
                    composed-composing =  temp.
                    APPEND  composed TO lt_composed.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              IF NOT  temp  CO '0123456789. ' AND  temp <> '=' AND (  import = abap_true OR  export = abap_true ).
                call-inner =  temp.
              ENDIF.
            ENDIF.

          ENDIF.

          IF  temp = '(' .
            prev =  temp.
            CONTINUE.
          ENDIF.

          IF  NOT  temp  CA '()'.
            IF  temp <> 'TABLE' AND  temp <> 'NEW'  AND  prev <> '('.
              IF  kw <> 'PERFORM'.
                prev =  temp.
              ELSEIF word = 'USING' OR word = 'CHANGING'.
                prev =  temp.
              ENDIF.
            ENDIF.
          ENDIF.

          IF  change IS NOT INITIAL.
            calculated-calculated =  change.
            APPEND calculated TO lt_calculated.

            IF  change+0(1) = '<'.

              SPLIT  change AT '-' INTO TABLE split.
              change = split[ 1 ].
              IF  eventtype IS INITIAL. "Global fs
                READ TABLE io_debugger->mo_window->mt_globals_set WITH KEY program = iv_program ASSIGNING FIELD-SYMBOL(<globals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_globals_set ASSIGNING <globals_set>.
                  <globals_set>-program = iv_program.
                ENDIF.
                READ TABLE  <globals_set>-mt_fs WITH KEY name =  change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO  <globals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<gl_fs>).
                  <gl_fs>-name =  change.
                ENDIF.

              ELSE."local fs
                READ TABLE io_debugger->mo_window->mt_locals_set
                 WITH KEY program = iv_program eventtype =  eventtype eventname =  eventname
                 ASSIGNING FIELD-SYMBOL(<locals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_locals_set ASSIGNING <locals_set>.
                  <locals_set>-program = iv_program.
                  <locals_set>-eventname =  eventname.
                  <locals_set>-eventtype =  eventtype.
                ENDIF.
                READ TABLE <locals_set>-mt_fs WITH KEY name =  change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
                  <loc_fs>-name =  change.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDWHILE.
        token-from = statement-from.
        token-to = statement-to.
        IF iv_class IS INITIAL.
          token-to_prog = iv_program.
        ENDIF.
        "check class names

        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line INTO call_line WITH KEY eventname = token-to_evname  eventtype = token-to_evtype .
        IF sy-subrc = 0.
          token-to_class = call_line-class.
        ENDIF.

        APPEND token TO tokens.
        IF lo_procedure->statement_index =  max.
          EXIT.
        ENDIF.

      ENDDO.

      "Fill keyword links for calls

      LOOP AT tokens ASSIGNING FIELD-SYMBOL(<s_token>) WHERE tt_calls IS NOT INITIAL.

        READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
        DATA(index) = 0.
        LOOP AT io_debugger->mo_window->ms_sources-t_params INTO param WHERE event = call-event AND name = call-name .
          ADD 1 TO  index.
          READ TABLE <s_token>-tt_calls INDEX  index ASSIGNING FIELD-SYMBOL(<call>).
          IF sy-subrc = 0.
            <call>-inner = param-param.
            IF param-type = 'I'.
              <call>-type = '>'.
            ELSE.
              <call>-type = '<'.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

      "clear value(var) to var.
      LOOP AT io_debugger->mo_window->ms_sources-t_params ASSIGNING <param>.
        REPLACE ALL OCCURRENCES OF 'VALUE(' IN <param>-param WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN <param>-param WITH ''.
      ENDLOOP.

      APPEND LINES OF lt_calculated TO io_debugger->mo_window->ms_sources-t_calculated.
      APPEND LINES OF lt_composed TO io_debugger->mo_window->ms_sources-t_composed.

      "ls_source-tt_tabs = tabs.
      DATA line LIKE LINE OF io_debugger->mo_window->ms_sources-tt_progs.
      prog-scan = lo_scan.
      prog-t_keywords = tokens.
      APPEND prog TO io_debugger->mo_window->ms_sources-tt_progs.

      IF io_debugger->m_step IS INITIAL.
        code_execution_scanner( iv_program = iv_program io_debugger = io_debugger ).



        "Fill keyword links for calls
        LOOP AT io_debugger->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
          LOOP AT prog-t_keywords ASSIGNING <s_token> WHERE tt_calls IS NOT INITIAL.

            READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
            index = 0.
            LOOP AT io_debugger->mo_window->ms_sources-t_params INTO param WHERE event = call-event AND name = call-name .
              ADD 1 TO  index.
              READ TABLE <s_token>-tt_calls INDEX  index ASSIGNING <call>.
              IF sy-subrc = 0.
                <call>-inner = param-param.
                IF param-type = 'I'.
                  <call>-type = '>'.
                ELSE.
                  <call>-type = '<'.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.


      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD code_execution_scanner.
    "code execution scanner
    DATA: max       TYPE i,
          call_line TYPE lcl_ace_window=>ts_calls_line,
          program   TYPE program,
          prefix    TYPE string,
          event     TYPE string,
          stack     TYPE i,
          statement TYPE i,
          include   TYPE program.

    READ TABLE io_debugger->mt_steps WITH KEY program = iv_program eventname = iv_evname eventtype = iv_evtype TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    stack =  iv_stack + 1.
    "CHECK  stack < 20.

    lcl_source_parser=>parse_tokens( iv_program = iv_program io_debugger = io_debugger ).
    READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = iv_program INTO DATA(prog).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA: structures LIKE prog-scan->structures.

    READ TABLE prog-scan->structures WITH KEY type = 'E' TRANSPORTING  NO FIELDS.
    IF sy-subrc = 0.
      structures = prog-scan->structures.
      DELETE structures WHERE type <> 'E'.
      SORT structures BY stmnt_type ASCENDING.
    ELSE.
      CLEAR  max.
      LOOP AT prog-scan->structures INTO DATA(str) WHERE type <> 'P' AND type <> 'C' .
        IF  max < str-stmnt_to.
          max = str-stmnt_to.
          APPEND str TO structures.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT structures INTO str.

      READ TABLE prog-t_keywords WITH KEY index =  str-stmnt_from INTO DATA(key).

      IF str-type = 'E'.
        statement = str-stmnt_from + 1.
        event = key-name.
      ELSE.
        statement = str-stmnt_from.
      ENDIF.

      WHILE  statement <= str-stmnt_to.
        READ TABLE prog-t_keywords WITH KEY index =   statement INTO key.

        IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS' OR key-name IS INITIAL OR sy-subrc <> 0.
          ADD 1 TO  statement.
          CONTINUE.
        ENDIF.
        ADD 1 TO io_debugger->m_step.
        APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).

        <step>-step = io_debugger->m_step.
        <step>-line = key-line.
        IF iv_evtype IS INITIAL.
          <step>-eventtype = 'EVENT'.
          <step>-eventname =  event.
        ELSE.
          <step>-eventtype = iv_evtype.
          <step>-eventname = iv_evname.
        ENDIF.
        <step>-stacklevel =  stack.
        <step>-program = iv_program.
        <step>-include = iv_program.

        IF key-to_evname IS NOT INITIAL AND NOT ( key-to_evtype = 'METHOD' AND key-to_class IS INITIAL ).

          IF key-to_evtype = 'FORM'.
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = key-to_evname eventtype = key-to_evtype INTO call_line.
            IF sy-subrc = 0.
              lcl_source_parser=>parse_call( EXPORTING iv_index = call_line-index
                                               iv_ev_name = call_line-eventname
                                               iv_ev_type = call_line-eventtype
                                               iv_program = iv_program
                                               iv_stack   =  stack
                                               io_debugger = io_debugger ).
            ENDIF.
          ELSEIF key-to_evtype = 'FUNCTION'.
            DATA:  func TYPE rs38l_fnam.
            func = key-to_evname.
            IF io_debugger->mo_window->m_zcode IS INITIAL OR
             ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND (  func+0(1) = 'Z' OR  func+0(1) = 'Y' ) ) .

              CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                CHANGING
                  funcname            = func
                  include             = include
                EXCEPTIONS
                  function_not_exists = 1
                  include_not_exists  = 2
                  group_not_exists    = 3
                  no_selections       = 4
                  no_function_include = 5
                  OTHERS              = 6.

              code_execution_scanner( iv_program =  include iv_stack =  stack iv_evtype = key-to_evtype iv_evname = key-to_evname io_debugger = io_debugger ).
            ENDIF.
          ELSE. "Method call

            DATA: cl_key TYPE seoclskey,
                  meth_includes   TYPE seop_methods_w_include.
            cl_key = key-to_class.
            CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
              EXPORTING
                clskey                       = cl_key
              IMPORTING
                includes                     = meth_includes
              EXCEPTIONS
                _internal_class_not_existing = 1
                OTHERS                       = 2.


            IF io_debugger->mo_window->m_zcode IS INITIAL OR
             ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( key-to_class+0(1) = 'Z' OR key-to_class+0(1) = 'Y' ) )
              OR meth_includes IS INITIAL.


              IF lines( meth_includes ) > 0.
                prefix = key-to_class && repeat( val = `=` occ = 30 - strlen( key-to_class ) ).
                program =  prefix && 'CU'.
                lcl_source_parser=>parse_tokens( iv_program =  program io_debugger = io_debugger iv_class = key-to_class ).

                program =  prefix && 'CI'.
                lcl_source_parser=>parse_tokens( iv_program =  program io_debugger = io_debugger iv_class = key-to_class ).

                program =  prefix && 'CO'.
                lcl_source_parser=>parse_tokens( iv_program =  program io_debugger = io_debugger iv_class = key-to_class ).

                READ TABLE meth_includes[] WITH KEY cpdkey-cpdname = key-to_evname INTO DATA(incl).                        .
                IF sy-subrc = 0.
                  program = incl-incname.
                  lcl_source_parser=>parse_tokens( iv_program =  program io_debugger = io_debugger iv_class = key-to_class iv_evname = key-to_evname ).
                ENDIF.
              ELSE.
                program = iv_program.
              ENDIF.
              READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = key-to_class eventtype = 'METHOD' eventname = key-to_evname INTO call_line.
              IF sy-subrc = 0.
                lcl_source_parser=>parse_call( EXPORTING iv_index = call_line-index
                                      iv_ev_name = call_line-eventname
                                      iv_ev_type = call_line-eventtype
                                      iv_program =  program
                                      iv_class = key-to_class
                                      iv_stack   =  stack
                                      io_debugger = io_debugger ).
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

        ADD 1 TO  statement.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD parse_call.
    DATA: statement TYPE i,
          stack     TYPE i,
          include   TYPE progname,
          prefix    TYPE string,
          program   TYPE program.

    READ TABLE io_debugger->mt_steps WITH KEY program = iv_program eventname = iv_ev_name eventtype = iv_ev_type TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    DATA: cl_key TYPE seoclskey,
          meth_includes   TYPE seop_methods_w_include.
    cl_key = iv_class.
    CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
      EXPORTING
        clskey                       = cl_key
      IMPORTING
        includes                     = meth_includes
      EXCEPTIONS
        _internal_class_not_existing = 1
        OTHERS                       = 2.

    IF lines( meth_includes ) IS INITIAL.
      statement = iv_index.
    ELSE.
      statement = 1.
    ENDIF.

    stack = iv_stack + 1.
    "CHECK  stack < 20.

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = iv_program INTO DATA(prog).
    DATA(max) = lines( prog-t_keywords ).
    DO.
      IF  statement >  max.
        EXIT.
      ENDIF.
      READ TABLE prog-t_keywords WITH KEY index =   statement INTO DATA(key).
      IF sy-subrc <> 0.
        ADD 1 TO  statement.
        CONTINUE.
      ENDIF.
      IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS' OR key-name IS INITIAL.
        ADD 1 TO  statement.
        CONTINUE.
      ENDIF.
      ADD 1 TO io_debugger->m_step.
      APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).

      <step>-step = io_debugger->m_step.
      <step>-line = key-line.
      <step>-eventname = iv_ev_name.
      <step>-eventtype = iv_ev_type.
      <step>-stacklevel =  stack.
      <step>-program = iv_program.
      <step>-include = iv_program.

      IF key-to_evname IS NOT INITIAL AND NOT ( key-to_evtype = 'METHOD' AND key-to_class IS INITIAL ).
        .
        IF key-to_evtype = 'FORM'.

          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = key-to_evname eventtype = key-to_evtype INTO DATA(call_line).
          IF sy-subrc = 0.
            lcl_source_parser=>parse_call( EXPORTING iv_index = call_line-index
                                                     iv_ev_name = call_line-eventname
                                                     iv_ev_type = call_line-eventtype
                                                     iv_program = iv_program
                                                     iv_stack   =  stack
                                                     io_debugger = io_debugger ).
          ENDIF.

        ELSEIF key-to_evtype = 'FUNCTION'.
          DATA:  func TYPE rs38l_fnam.
          func = key-to_evname.
          IF io_debugger->mo_window->m_zcode IS INITIAL OR
            ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND (  func+0(1) = 'Z' OR  func+0(1) = 'Y' ) ) .

            CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
              CHANGING
                funcname            = func
                include             = include
              EXCEPTIONS
                function_not_exists = 1
                include_not_exists  = 2
                group_not_exists    = 3
                no_selections       = 4
                no_function_include = 5
                OTHERS              = 6.

            code_execution_scanner( iv_program =  include iv_stack =  stack iv_evtype = key-to_evtype iv_evname = key-to_evname io_debugger = io_debugger ).
          ENDIF.
        ELSE. "METHOD CALL
          cl_key = key-to_class.
          CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
            EXPORTING
              clskey                       = cl_key
            IMPORTING
              includes                     = meth_includes
            EXCEPTIONS
              _internal_class_not_existing = 1
              OTHERS                       = 2.

          IF io_debugger->mo_window->m_zcode IS INITIAL OR
           ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( key-to_class+0(1) = 'Z' OR key-to_class+0(1) = 'Y' ) )
             OR meth_includes IS INITIAL.

            IF  lines( meth_includes ) > 0.

              prefix = key-to_class && repeat( val = `=` occ = 30 - strlen( key-to_class ) ).
              program =  prefix && 'CU'.
              lcl_source_parser=>parse_tokens( iv_program =  program io_debugger = io_debugger iv_class = key-to_class ).

              program =  prefix && 'CI'.
              lcl_source_parser=>parse_tokens( iv_program =  program io_debugger = io_debugger iv_class = key-to_class ).

              program =  prefix && 'CO'.
              lcl_source_parser=>parse_tokens( iv_program =  program io_debugger = io_debugger iv_class = key-to_class ).

              READ TABLE meth_includes[] WITH KEY cpdkey-cpdname = key-to_evname INTO DATA(incl).                        .
              IF sy-subrc = 0.
                program = incl-incname.
                lcl_source_parser=>parse_tokens( iv_program =  program io_debugger = io_debugger iv_class = key-to_class iv_evname = iv_ev_name ).
              ENDIF.
            ELSE.
              program = iv_program.
            ENDIF.
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = key-to_class eventtype = 'METHOD' eventname = key-to_evname INTO call_line.
            IF sy-subrc = 0.
              lcl_source_parser=>parse_call( EXPORTING iv_index = call_line-index
                                    iv_ev_name = call_line-eventname
                                    iv_ev_type = call_line-eventtype
                                    iv_program =  program
                                    iv_class = key-to_class
                                    iv_stack   =  stack
                                    io_debugger = io_debugger ).
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

      IF key-name = 'ENDFORM' OR key-name = 'ENDMETHOD'.
        RETURN.
      ENDIF.

      ADD 1 TO  statement.
    ENDDO.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_mermaid IMPLEMENTATION.

  METHOD constructor.

    DATA  text TYPE text100.

    super->constructor( ).

    mo_viewer = io_debugger.
    mv_type = iv_type.

    CHECK lcl_appl=>is_mermaid_active = abap_true.

    CASE mv_type.
      WHEN 'DIAG'.
        text = 'Calls flow'.
      WHEN 'SMART'.
        text = 'Calculations sequence'.
    ENDCASE.

    IF mo_box IS INITIAL.
      mo_box = create( i_name =  text i_width = 1000 i_hight = 300 ).

      "save new popup ref
      APPEND INITIAL LINE TO lcl_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
      <popup>-parent = mo_viewer->mo_window->mo_box.
      <popup>-child = mo_box.

      SET HANDLER on_box_close FOR mo_box.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_box
          rows    = 2
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter->get_container(
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_mm_container ).

      mo_splitter->get_container(
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_mm_toolbar ).

      mo_splitter->set_row_height( id = 1 height = '3' ).
      mo_splitter->set_row_height( id = 2 height = '70' ).

      mo_splitter->set_row_sash( id    = 1
                                 type  = 0
                                 value = 0 ).

      CREATE OBJECT mo_toolbar EXPORTING parent = mo_mm_toolbar.
      add_toolbar_buttons( ).
      mo_toolbar->set_visible( 'X' ).
    ENDIF.
    CASE mv_type.
      WHEN 'DIAG'.
        steps_flow( ).
      WHEN 'SMART'.
        magic_search( ).
    ENDCASE.

  ENDMETHOD.

  METHOD steps_flow.

    TYPES: BEGIN OF lty_entity,
             event TYPE string,
             name  TYPE string,
           END OF lty_entity,
           BEGIN OF t_ind,
             from TYPE i,
             to   TYPE i,
           END OF t_ind  .

    DATA: mm_string TYPE string,
          name      TYPE string,
          entities  TYPE TABLE OF lty_entity,
          entity    TYPE lty_entity,
          parts     TYPE TABLE OF string,
          step      LIKE LINE OF mo_viewer->mt_steps,
          ind       TYPE t_ind,
          indexes   TYPE TABLE OF t_ind.

    DATA(copy) = mo_viewer->mt_steps.

    LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
      IF <copy>-eventtype = 'METHOD'.
        SPLIT <copy>-program AT '=' INTO TABLE parts.
        <copy>-eventname = entity-name = |"{ parts[ 1 ] }->{ <copy>-eventname }"|.
        entity-event = <copy>-eventtype.

      ELSEIF <copy>-eventtype = 'FUNCTION'.
        <copy>-eventname = entity-name = |"{ <copy>-eventtype }:{ <copy>-eventname }"|.
      ELSE.
        <copy>-eventname = entity-name = |"{ <copy>-program }:{ <copy>-eventname }"|.
      ENDIF.

      COLLECT entity INTO entities.
    ENDLOOP.

    CLEAR step.

    IF iv_direction IS INITIAL.
      mm_string = |graph TD\n |.
    ELSE.
      mm_string = |graph { iv_direction }\n |.
    ENDIF.

    LOOP AT copy INTO DATA(step2).
      IF step IS INITIAL.
        step = step2.
        CONTINUE.
      ENDIF.
      IF step2-stacklevel > step-stacklevel.

        READ TABLE entities WITH KEY name = step-eventname TRANSPORTING NO FIELDS.
        ind-from = sy-tabix.
        READ TABLE entities WITH KEY name = step2-eventname TRANSPORTING NO FIELDS.
        ind-to = sy-tabix.
        READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "REPLACE ALL OCCURRENCES OF `-` IN step-eventname WITH `~` IN CHARACTER MODE.
          "REPLACE ALL OCCURRENCES OF `-` IN step2-eventname WITH `~` IN CHARACTER MODE.
          mm_string = |{  mm_string }{ ind-from }({ step-eventname }) --> { ind-to }({ step2-eventname })\n|.
          APPEND ind TO indexes.
        ENDIF.
      ENDIF.
      step = step2.
    ENDLOOP.

    open_mermaid(  mm_string ).

  ENDMETHOD.

  METHOD magic_search.

    DATA: add         TYPE xfeld,
          mm_string   TYPE string,
          sub         TYPE string,
          form        TYPE string,
          direction   TYPE string,
          box_s       TYPE string,
          box_e       TYPE string,
          ind2        TYPE i,
          start       TYPE i,
          end         TYPE i,
          bool        TYPE string,
          block_first TYPE i,
          els_before  TYPE i.

    TYPES: BEGIN OF ts_line,
             cond       TYPE string,
             include    TYPE string,
             line       TYPE i,
             ind        TYPE i,
             event      TYPE string,
             stack      TYPE i,
             code       TYPE string,
             arrow      TYPE string,
             subname    TYPE string,
             del        TYPE flag,
             els_before TYPE i,
             els_after  TYPE i,
           END OF ts_line.

    DATA: line       TYPE ts_line,
          lines      TYPE STANDARD TABLE OF ts_line,
          prev_stack TYPE ts_line,
          opened     TYPE i.

    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(prog).

    LOOP AT mo_viewer->mt_steps INTO DATA(step).
      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO DATA(keyword).
      LOOP AT keyword-tt_calls INTO DATA(call).

        READ TABLE mo_viewer->mt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
          <selected>-name = call-outer.
        ENDIF.

        READ TABLE mo_viewer->mt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
          <selected>-name = call-inner.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    DATA(steps) = mo_viewer->mt_steps.

    SORT steps BY step DESCENDING.

    "collecting dependents variables
    LOOP AT steps INTO step.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.

      LOOP AT mo_viewer->mo_window->ms_sources-t_calculated INTO DATA(calculated) WHERE line = step-line AND program = prog-include.
        READ TABLE mo_viewer->mt_selected_var WITH KEY name = calculated-calculated TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
          <selected>-name = calculated-calculated.
        ENDIF.
        LOOP AT mo_viewer->mo_window->ms_sources-t_composed INTO DATA(composed) WHERE line = step-line AND program = prog-include.
          READ TABLE mo_viewer->mt_selected_var WITH KEY name = composed-composing TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
            <selected>-name = composed-composing.
          ENDIF.
        ENDLOOP.
        "adding returning values
        LOOP AT mo_viewer->mo_window->ms_sources-t_params INTO DATA(param).
          READ TABLE mo_viewer->mt_selected_var WITH KEY name =  param-param TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
            <selected>-name =  param-param.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.
      LOOP AT keyword-tt_calls INTO call.

        READ TABLE mo_viewer->mt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
          <selected>-name = call-inner.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
    SORT mo_viewer->mt_selected_var.
    DELETE ADJACENT DUPLICATES FROM mo_viewer->mt_selected_var.

    "collecting watchpoints
    "CLEAR mo_viewer->mo_window->mt_coverage.

    LOOP AT  steps INTO step.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO DATA(key).

      CLEAR line-cond.
      IF key-name = 'IF' OR key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ELSEIF' OR
         key-name = 'CASE' OR key-name = 'WHEN' OR key-name = 'ENDCASE' OR
          key-name = 'DO' OR key-name = 'ENDDO'  OR key-name = 'LOOP'  OR key-name = 'ENDLOOP'
         OR key-name = 'WHILE' OR key-name = 'ENDWHILE'
         OR key-tt_calls IS NOT INITIAL.
        APPEND INITIAL LINE TO mo_viewer->mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).
        <watch>-program = step-program.
        <watch>-line = line-line = step-line.
        IF key-tt_calls IS INITIAL.
          line-cond = key-name.
        ENDIF.
        line-event = step-eventname.
        line-stack = step-stacklevel.
        line-include = step-include.
        INSERT line INTO lines INDEX 1.

      ENDIF.

      LOOP AT  mo_viewer->mo_window->ms_sources-t_calculated INTO calculated WHERE line = step-line AND program = prog-include.

        LOOP AT mo_viewer->mo_window->ms_sources-t_composed INTO composed WHERE line = step-line AND program = prog-include.
          READ TABLE mo_viewer->mt_selected_var WITH KEY name = composed-composing TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
            <selected>-name = composed-composing.
          ENDIF.
        ENDLOOP.

        READ TABLE mo_viewer->mt_selected_var WITH KEY name = calculated-calculated TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.

          APPEND INITIAL LINE TO mo_viewer->mo_window->mt_watch ASSIGNING <watch>.
          <watch>-program = step-program.
          <watch>-line = line-line = step-line.

          LOOP AT lines ASSIGNING FIELD-SYMBOL(<line>) WHERE line = line-line AND event = step-eventname AND stack = step-stacklevel .
            <line>-del = abap_true.
          ENDLOOP.

          line-event = step-eventname.
          line-stack = step-stacklevel.
          line-include = step-include.
          INSERT line INTO lines INDEX 1.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    DELETE lines WHERE del = abap_true.

    "delete empty blocks
    LOOP AT lines ASSIGNING <line>.
      IF <line>-cond = 'IF' OR <line>-cond = 'DO' OR <line>-cond = 'LOOP' OR <line>-cond = 'WHILE'.
        READ TABLE lines INDEX sy-tabix + 1 ASSIGNING FIELD-SYMBOL(<line2>).
        IF <line2>-cond = 'ENDIF' OR <line2>-cond = 'ENDDO' OR <line2>-cond = 'ENDLOOP' OR <line2>-cond = 'ENDWHILE'.
          <line>-del = <line2>-del = abap_true.
        ENDIF.
      ENDIF.

    ENDLOOP.
    DELETE lines WHERE del = abap_true.


    "getting code texts and calls params
    LOOP AT lines ASSIGNING <line>.
      DATA(ind) = sy-tabix.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = <line>-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = <line>-line INTO keyword.
      LOOP AT prog-scan->tokens FROM keyword-from TO keyword-to INTO DATA(token).
        IF token-str = 'USING' OR token-str = 'EXPORTING' OR token-str = 'IMPORTING' OR token-str = 'CHANGING'.
          EXIT.
        ENDIF.
        IF <line>-code IS INITIAL.
          <line>-code = token-str.
        ELSE.
          <line>-code = |{  <line>-code } { token-str }|.
        ENDIF.
      ENDLOOP.
      REPLACE ALL OCCURRENCES OF '`' IN  <line>-code WITH ''.
      REPLACE ALL OCCURRENCES OF '"' IN  <line>-code WITH ''.

      SORT keyword-tt_calls BY outer.
      DELETE ADJACENT DUPLICATES FROM keyword-tt_calls.
      IF keyword-to_evname IS NOT INITIAL.
        LOOP AT keyword-tt_calls INTO call. "WHERE type IS NOT INITIAL.
          IF sy-tabix <> 1.
            <line>-arrow = |{ <line>-arrow }, |.
          ENDIF.
          "<line>-arrow  = |{ <line>-arrow  } { call-outer } { call-type } { call-inner }|.
          <line>-arrow  = |{ <line>-arrow  } { call-outer } as  { call-inner }|.
          <line>-subname = call-name.
        ENDLOOP.
      ENDIF.
      REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
    ENDLOOP.

    "check subform execution steps existance and if/case structures build

    DATA: if_depth   TYPE i,
          when_count TYPE i.
    LOOP AT lines ASSIGNING <line> WHERE code <> 'DO' AND code <> 'ENDDO' AND code <> 'WHILE' AND code <> 'ENDWHILE' AND code <> 'LOOP' AND code <> 'ENDLOOP' .
      <line>-ind = sy-tabix.

      FIELD-SYMBOLS: <if> TYPE ts_if.
      IF <line>-cond = 'IF' OR  <line>-cond = 'CASE'.
        ADD 1 TO if_depth.
        CLEAR when_count.
        APPEND INITIAL LINE TO mt_if  ASSIGNING <if>.
        <if>-if_ind = <line>-ind.

      ENDIF.

      IF <line>-cond = 'ENDIF' OR <line>-cond = 'ENDCASE'.
        <if>-end_ind = <line>-ind.
        SUBTRACT 1 FROM if_depth.
        LOOP AT mt_if  ASSIGNING <if> WHERE end_ind = 0.
        ENDLOOP.
        "READ TABLE mt_if INDEX if_depth ASSIGNING <if>.
      ENDIF.

      IF <line>-cond = 'WHEN'.
        ADD 1 TO when_count.
      ENDIF.

      IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.

        <line>-els_before =  els_before.
        <line>-els_after = <line>-ind.
        DATA(counter) = <line>-ind + 1.
        DO.
          READ TABLE lines INDEX  counter INTO line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after =  counter.
            EXIT.
          ELSE.
            ADD 1 TO  counter.

          ENDIF.
        ENDDO.
        IF when_count = 1.
          <if>-if_ind =  els_before.
          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond = 'WHEN'.

        <line>-els_before =  els_before.
        <line>-els_after = <line>-ind.
        counter = <line>-ind + 1.
        DO.
          READ TABLE lines INDEX  counter INTO line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF line-cond = 'WHEN'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after =  counter.
            EXIT.
          ELSE.
            ADD 1 TO  counter.

          ENDIF.
        ENDDO.
        IF when_count = 1."to refactor
*          <if>-if_ind =  els_before.
*          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
        els_before = <line>-ind.
      ELSE.
        CLEAR    els_before.
      ENDIF.

      READ TABLE lines WITH KEY event = <line>-subname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR <line>-arrow.
      ENDIF.
    ENDLOOP.

    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
      INSERT ms_if INTO mt_if INDEX 1.
    ENDIF.

    IF lines( lines ) > 0.
      IF lines[ lines( lines ) ]-arrow IS NOT INITIAL.
        CLEAR lines[ lines( lines ) ]-arrow .
      ENDIF.
    ENDIF.

    "creating mermaid code
    CHECK lines IS NOT INITIAL.

    IF iv_direction IS INITIAL.
      IF lines( lines ) < 100.
        direction = 'LR'.
      ELSE.
        direction = 'TB'.
      ENDIF.
    ELSE.
      direction = iv_direction.
    ENDIF.

    mm_string = |graph {  direction }\n |.

    LOOP AT lines INTO line WHERE cond <> 'ELSE' AND cond <> 'ELSEIF' AND  cond <> 'WHEN'.
      ind = sy-tabix.

      IF line-cond IS INITIAL.
        box_s = '('.
        box_e = ')'.
      ELSE.
        box_s = '{'.
        box_e = '}'.
      ENDIF.

      IF prev_stack IS INITIAL.
        prev_stack = line.
      ENDIF.

      IF ( prev_stack-stack > line-stack OR prev_stack-event <> line-event ) AND  opened > 0 AND  sub IS INITIAL.
        IF prev_stack-stack = line-stack AND prev_stack-event <> line-event.
          DATA(times) = 1.
        ELSE.
          times = prev_stack-stack - line-stack.
        ENDIF.

        DO  times TIMES.
          mm_string = |{  mm_string } end\n|.
          SUBTRACT 1 FROM  opened.
          IF  opened = 0.
            EXIT.
          ENDIF.
        ENDDO.

      ENDIF.
      DATA:  name TYPE string.
      IF    line-cond = 'LOOP' OR line-cond = 'DO' OR line-cond = 'WHILE' OR line-arrow IS NOT INITIAL .

        IF line-arrow IS NOT INITIAL.
          mm_string = |{  mm_string }{  ind }{  box_s }"{ line-code }"{  box_e }\n|.
          prev_stack = line.
        ENDIF.

        IF strlen( line-code ) > 50.
          name = line-code+0(50).
        ELSE.
          name = line-code.
        ENDIF.
        REPLACE ALL OCCURRENCES OF `PERFORM` IN  name WITH `FORM` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN  name WITH `FUNCTION` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL METHOD` IN  name WITH `METHOD` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `-` IN  name WITH `~` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF ` ` IN  name WITH `&nbsp;` IN CHARACTER MODE.

        mm_string = |{  mm_string } subgraph S{  ind }["{  name }"]\n  direction {  direction }\n|.
        ADD 1 TO  opened.
        start =  ind.
        CONTINUE.
      ENDIF.

      IF line-cond = 'ENDLOOP' OR line-cond = 'ENDDO' OR line-cond = 'ENDWHILE'.
        SUBTRACT 1 FROM  opened.
        mm_string = |{  mm_string } end\n|.
        CONTINUE.
      ENDIF.

      mm_string = |{  mm_string }{  ind }{  box_s }"{ line-code }"{  box_e }\n|.
      prev_stack = line.

    ENDLOOP.

    DO  opened TIMES.
      mm_string = |{  mm_string } end\n|.
      SUBTRACT 1 FROM  opened.
    ENDDO.


    DATA: if_ind      TYPE i.
    CLEAR prev_stack.
    LOOP AT lines INTO line WHERE cond <> 'LOOP' AND cond <> 'DO' AND cond <> 'WHILE' AND cond <> 'ENDLOOP' AND cond <> 'ENDDO' AND cond <> 'ENDWHILE'.

      IF line-cond = 'IF' OR line-cond = 'CASE' .
        ADD 1 TO if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.


      IF prev_stack IS INITIAL.
        IF line-cond = 'WHEN' OR line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
          IF <if> IS ASSIGNED.
            prev_stack = lines[ <if>-if_ind ].
          ELSE.
            CLEAR prev_stack.
          ENDIF.
        ELSE.
          prev_stack = line.

          IF line-arrow IS NOT INITIAL.
            sub = '|"' && line-arrow && '"|'.
          ELSE.
            CLEAR  sub.
          ENDIF.

          CONTINUE.
        ENDIF.

      ENDIF.

      IF line-cond = 'ELSE' OR line-cond = 'ELSEIF' OR line-cond = 'WHEN'.
        bool = '|' && line-code && '|'.
        IF line-els_after IS NOT INITIAL.
          mm_string = |{  mm_string }{ ms_if-if_ind }-->{  bool }{ line-els_after }\n|.
          DATA(diff) = ms_if-end_ind - line-els_after.
          DATA(last_els) = line-els_after.
*          IF line-cond <> 'WHEN' AND line-cond <> 'ELSEIF'  AND   diff > 1 AND line-els_after <> ms_if-end_ind.
*             mm_string = |{  mm_string }{  line-els_after }-->{ ms_if-end_ind }\n|.
*          ENDIF.
        ELSE.
          mm_string = |{  mm_string }{ ms_if-if_ind }-->{  bool }{ ms_if-end_ind }\n|.
        ENDIF.

        IF line-els_before IS NOT INITIAL AND line-els_before <> ms_if-if_ind.
          mm_string = |{  mm_string }{ line-els_before }-->{ ms_if-end_ind }\n|.
        ENDIF.

        IF lines[ line-ind + 1 ]-cond <> 'ENDIF' AND lines[ line-ind + 1 ]-cond <> 'ENDCASE'.
          CLEAR prev_stack.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF   prev_stack-cond NE 'ELSE' AND prev_stack-cond NE 'ELSEIF' AND prev_stack-cond NE 'WHEN' AND NOT (  last_els = line-ind ).

        mm_string = |{  mm_string }{ prev_stack-ind }-->{  sub }{ line-ind }\n|.

        IF line-arrow IS NOT INITIAL.
          sub = '|"' && line-arrow && '"|'.
        ELSE.
          CLEAR  sub.
        ENDIF.

      ENDIF.

      prev_stack = line.

      IF line-cond = 'ENDIF' OR line-cond = 'ENDCASE'.
        DELETE mt_if INDEX if_ind.
        SUBTRACT 1 FROM if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.

    ENDLOOP.

    open_mermaid(  mm_string ).

  ENDMETHOD.

  METHOD add_toolbar_buttons.

    DATA: button TYPE ttb_button,
          events TYPE cntl_simple_events,
          event LIKE LINE OF events.

    button  = VALUE #(
     ( function = 'TB' icon = CONV #( icon_view_expand_vertical ) quickinfo = 'Vertical' text = '' )
     ( function = 'LR' icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
     ( butn_type = 3  )
     ( function = 'TEXT' icon = CONV #( icon_wd_caption ) quickinfo = 'Mermaid Diagram text' text = '' )
                    ).

    mo_toolbar->add_button_group( button ).

*   Register events
    event-eventid = cl_gui_toolbar=>m_id_function_selected.
    event-appl_event = space.
    APPEND event TO events.

    mo_toolbar->set_registered_events( events = events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.

  ENDMETHOD.

  METHOD hnd_toolbar.


    IF fcode = 'TEXT'.
      DATA: mm_string TYPE string,
            ref       TYPE REF TO data.
      mm_string = mo_diagram->get_source_code_string( ).
      GET REFERENCE OF  mm_string INTO  ref.
      NEW lcl_text_viewer(  ref ).

      RETURN.
    ENDIF.

    CASE mv_type.
      WHEN 'DIAG'.
        steps_flow( fcode ).
      WHEN 'SMART'.
        magic_search( fcode ).

    ENDCASE.

  ENDMETHOD.

  METHOD open_mermaid.

    CHECK lcl_appl=>is_mermaid_active = abap_true.

    TRY.
        IF mo_diagram IS INITIAL.
          mo_diagram = NEW zcl_wd_gui_mermaid_js_diagram( parent = mo_mm_container ).
        ENDIF.
        mo_diagram->set_source_code_string( iv_mm_string ).
        mo_diagram->display( ).

      CATCH zcx_wd_gui_mermaid_js_diagram INTO DATA(error).
        MESSAGE error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.

  lcl_appl=>init_lang( ).
  lcl_appl=>init_icons_table( ).
  WRITE 1.

AT SELECTION-SCREEN.

  SET PARAMETER ID 'API' FIELD p_apikey.

  DATA(gv_ace) = NEW lcl_ace( iv_prog = p_prog iv_dest = p_dest iv_model = p_model iv_apikey = p_apikey ).
