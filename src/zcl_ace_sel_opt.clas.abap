CLASS zcl_ace_sel_opt DEFINITION
  PUBLIC
  CREATE PUBLIC .

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
        transmitter TYPE REF TO zcl_ace_data_transmitter,
        receiver    TYPE REF TO zcl_ace_data_receiver,
        color       TYPE lvc_t_scol,
        style       TYPE lvc_t_styl,
      END OF selection_display_s .
    TYPES:
      BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string, "aqadh_range_value,
        high        TYPE string, "aqadh_range_value,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row .

    CLASS-DATA:
      mt_sel TYPE TABLE OF zcl_ace_sel_opt=>selection_display_s .

    DATA mo_viewer TYPE REF TO zcl_ace_table_viewer .
    DATA mo_sel_alv TYPE REF TO cl_gui_alv_grid .
    DATA mt_fcat TYPE lvc_t_fcat .
    DATA:
      mt_sel_tab TYPE TABLE OF zcl_ace_sel_opt=>selection_display_s .
    DATA ms_layout TYPE lvc_s_layo .

    EVENTS selection_done .

    METHODS constructor
      IMPORTING
        !io_viewer    TYPE REF TO zcl_ace_table_viewer
        !io_container TYPE REF TO cl_gui_container .
    METHODS raise_selection_done .
    METHODS update_sel_tab .
    METHODS set_value
      IMPORTING
        !i_field TYPE any
        !i_low   TYPE any OPTIONAL
        !i_high  TYPE any OPTIONAL
        !i_clear TYPE xfeld DEFAULT abap_true .
    METHODS update_sel_row
      CHANGING
        !c_sel_row TYPE zcl_ace_sel_opt=>selection_display_s .
protected section.
PRIVATE SECTION.

  METHODS init_fcat
    IMPORTING
      !i_dd_handle TYPE i .
  METHODS handle_sel_toolbar
    FOR EVENT toolbar OF cl_gui_alv_grid
    IMPORTING
      !e_object .
  METHODS on_f4
    FOR EVENT onf4 OF cl_gui_alv_grid
    IMPORTING
      !er_event_data
      !es_row_no
      !e_fieldname .
  METHODS on_grid_button_click
    FOR EVENT button_click OF cl_gui_alv_grid
    IMPORTING
      !es_col_id
      !es_row_no .
  METHODS on_data_changed
    FOR EVENT data_changed OF cl_gui_alv_grid
    IMPORTING
      !er_data_changed .
  METHODS on_data_changed_finished
    FOR EVENT data_changed_finished OF cl_gui_alv_grid
    IMPORTING
      !e_modified .
  METHODS handle_user_command
    FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING
      !e_ucomm .
  METHODS handle_doubleclick
    FOR EVENT double_click OF cl_gui_alv_grid
    IMPORTING
      !es_row_no
      !e_column .
  METHODS handle_context_menu_request
    FOR EVENT context_menu_request OF cl_gui_alv_grid
    IMPORTING
      !e_object .
ENDCLASS.



CLASS ZCL_ACE_SEL_OPT IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: effect     TYPE i,
          handle_alv TYPE i.

    mo_viewer = io_viewer.
    mo_sel_alv = NEW #( i_parent = io_container ).
    update_sel_tab( ).
    CREATE OBJECT zcl_ace_appl=>c_dragdropalv.
    effect =  cl_dragdrop=>copy. " + cl_dragdrop=>move.

    CALL METHOD zcl_ace_appl=>c_dragdropalv->add
      EXPORTING
        flavor     = 'Line'
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = effect.

    CALL METHOD zcl_ace_appl=>c_dragdropalv->get_handle IMPORTING handle = handle_alv.
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
                zcl_ace_dragdrop=>drag
                zcl_ace_dragdrop=>drop
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


    DATA: ls_func TYPE ui_func,
          lt_func TYPE ui_functions.

    DATA(l_index) = zcl_ace_alv_common=>get_selected( mo_sel_alv ).

    IF l_index IS NOT INITIAL.
      READ TABLE mt_sel_tab INTO DATA(l_sel) INDEX l_index.
    ENDIF.

    e_object->get_functions( IMPORTING fcodes = DATA(lt_fcodes) ). "Inactivate all standard functions

    LOOP AT lt_fcodes INTO DATA(ls_fcode) WHERE fcode NE '&OPTIMIZE'.
      ls_func = ls_fcode-fcode.
      APPEND ls_func TO lt_func.
    ENDLOOP.

    e_object->hide_functions( lt_func ).
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


    DATA: lv_sel_width TYPE i.

    IF e_ucomm = 'SEL_OFF'. "Hide select-options alv

      mo_viewer->m_visible = ''.

      lv_sel_width = 0.
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
          width = lv_sel_width.
      mo_viewer->mo_alv->set_toolbar_interactive( ).
      RETURN.
    ENDIF.

    IF e_ucomm = 'SEL_CLEAR' OR e_ucomm = 'DELR'. "clear all selections
      mo_sel_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).

      LOOP AT lt_sel_rows INTO DATA(l_row).
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

    zcl_ace_alv_common=>refresh( mo_viewer->mo_alv ).
    RAISE EVENT selection_done.


  endmethod.


  method INIT_FCAT.


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


  endmethod.


  method ON_DATA_CHANGED.


    DATA: l_start TYPE i,
          lv_time TYPE sy-uzeit.

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
          DATA: lv_date TYPE sy-datum.
          CALL FUNCTION 'CONVERT_DATE_INPUT'
            EXPORTING
              input                     = <ls_cells>-value
              plausibility_check        = abap_true
            IMPORTING
              output                    = lv_date
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.

          IF sy-subrc = 0.
            <ls_cells>-value = |{ lv_date DATE = USER }|.
          ENDIF.
        ELSEIF <tab>-int_type = 'T'.
          CALL FUNCTION 'CONVERT_TIME_INPUT'
            EXPORTING
              input                     = <ls_cells>-value
            IMPORTING
              output                    = lv_time
            EXCEPTIONS
              plausibility_check_failed = 1
              wrong_format_in_input     = 2
              OTHERS                    = 3.
          <ls_cells>-value = lv_time+0(2) && ':' && lv_time+2(2) && ':' && lv_time+4(2).
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
    zcl_ace_alv_common=>refresh( EXPORTING i_obj = mo_sel_alv i_layout = ms_layout ).
    raise_selection_done( ).


  endmethod.


  method ON_DATA_CHANGED_FINISHED.


    CHECK e_modified IS NOT INITIAL.
    RAISE EVENT selection_done.


  endmethod.


  method ON_F4.


    DATA: return_tab TYPE STANDARD TABLE OF ddshretval,
          lt_objec   TYPE TABLE OF objec,
          ls_objec   TYPE objec,
          l_otype    TYPE otype,
          l_plvar    TYPE plvar,
          l_multiple TYPE xfeld,
          l_clear    TYPE xfeld.

    IF e_fieldname = 'LOW'.
      l_multiple = abap_true.
    ENDIF.

    READ TABLE mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel>) INDEX es_row_no-row_id.
    DATA(l_fname) =  <sel>-field_label.

    zcl_ace_sel_opt=>mt_sel[] = mt_sel_tab[].
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
          sel_object       = ls_objec
        TABLES
          sel_hrobject_tab = lt_objec
        EXCEPTIONS
          OTHERS           = 6.
      IF sy-subrc = 0.
        l_clear = abap_true.
        LOOP AT lt_objec INTO ls_objec.
          IF e_fieldname = 'LOW'.
            set_value( EXPORTING i_field = <sel>-field_label i_low = ls_objec-objid i_clear = l_clear ).
            CLEAR l_clear.
          ELSE.
            set_value( EXPORTING i_field = <sel>-field_label i_high = ls_objec-objid i_clear = l_clear ).
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


  endmethod.


  method ON_GRID_BUTTON_CLICK.


    DATA: l_tabfield TYPE rstabfield,
          ls_opt     TYPE rsoptions VALUE 'XXXXXXXXXX',
          lv_sign    TYPE raldb_sign,
          lv_option  TYPE raldb_opti.

    READ TABLE mt_sel_tab INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    CASE es_col_id.
      WHEN 'OPTION_ICON'. "edit select logical expression type
        CALL FUNCTION 'SELECT_OPTION_OPTIONS'
          EXPORTING
            selctext     = 'nnnn'
            option_list  = ls_opt
          IMPORTING
            sign         = lv_sign
            option       = lv_option
          EXCEPTIONS
            delete_line  = 1
            not_executed = 2
            illegal_sign = 3
            OTHERS       = 4.
        IF sy-subrc = 0.
          <tab>-sign = lv_sign.
          <tab>-opti = lv_option.
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


  endmethod.


  method RAISE_SELECTION_DONE.


    DATA: ls_row TYPE zcl_ace_sel_opt=>t_sel_row.

    zcl_ace_alv_common=>refresh( mo_sel_alv ).
    RAISE EVENT selection_done.
    LOOP AT mt_sel_tab  ASSIGNING FIELD-SYMBOL(<sel>).
      IF <sel>-transmitter IS NOT INITIAL.
        MOVE-CORRESPONDING <sel> TO ls_row.
        <sel>-transmitter->emit( e_row = ls_row ).
      ENDIF.
    ENDLOOP.


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
    IF <to>-transmitter IS BOUND.
      DATA: ls_row TYPE zcl_ace_sel_opt=>t_sel_row.
      MOVE-CORRESPONDING <to> TO ls_row.
      <to>-transmitter->emit( EXPORTING e_row = ls_row ).
    ENDIF.


  endmethod.


  method UPDATE_SEL_ROW.
 "select patterns rules

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
        c_sel_row-option_icon = zcl_ace_appl=>m_option_icons[ sign = c_sel_row-sign option = c_sel_row-opti ]-icon_name.
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


  endmethod.


  method UPDATE_SEL_TAB.


    IF mt_sel_tab[] IS NOT INITIAL.
      DATA(lt_copy) = mt_sel_tab.
    ENDIF.
    CLEAR mt_sel_tab[].
    mo_viewer->mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = mo_viewer->mt_alv_catalog ).
    LOOP AT mo_viewer->mt_alv_catalog INTO DATA(l_catalog) WHERE domname NE 'MANDT'.
      DATA(lv_ind) = sy-tabix.
      APPEND INITIAL LINE TO mt_sel_tab ASSIGNING FIELD-SYMBOL(<sel_tab>).
      READ TABLE lt_copy INTO DATA(ls_copy) WITH KEY field_label = l_catalog-fieldname.

      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_copy TO <sel_tab>.
      ELSE.
        <sel_tab>-option_icon = icon_led_inactive.
        <sel_tab>-more_icon = icon_enter_more.
      ENDIF.

      <sel_tab>-ind = lv_ind.
      <sel_tab>-field_label = l_catalog-fieldname.
      <sel_tab>-int_type = l_catalog-inttype.
      <sel_tab>-element = l_catalog-rollname.
      <sel_tab>-domain =  l_catalog-domname.
      <sel_tab>-datatype = l_catalog-datatype.
      <sel_tab>-length = l_catalog-outputlen.
      zcl_ace_alv_common=>translate_field( EXPORTING i_lang = mo_viewer->m_lang CHANGING c_fld = l_catalog ).
      <sel_tab>-name = l_catalog-scrtext_l.
    ENDLOOP.


  endmethod.
ENDCLASS.
