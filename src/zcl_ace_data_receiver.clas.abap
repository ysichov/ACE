CLASS zcl_ace_data_receiver DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mo_transmitter TYPE REF TO zcl_ace_data_transmitter .
    DATA lo_tab_from TYPE REF TO zcl_ace_table_viewer .
    DATA lo_sel_to TYPE REF TO zcl_ace_sel_opt .
    DATA m_from_field TYPE lvc_fname .
    DATA m_to_field TYPE lvc_fname .

    METHODS constructor
      IMPORTING
        !io_transmitter TYPE REF TO zcl_ace_data_transmitter OPTIONAL
        !io_tab_from    TYPE REF TO zcl_ace_table_viewer OPTIONAL
        !io_sel_to      TYPE REF TO zcl_ace_sel_opt OPTIONAL
        !i_from_field   TYPE lvc_fname OPTIONAL
        !i_to_field     TYPE lvc_fname OPTIONAL .
    METHODS shut_down .
    METHODS update
      FOR EVENT data_changed OF zcl_ace_data_transmitter
      IMPORTING
        !e_row .
    METHODS update_col
      FOR EVENT col_changed OF zcl_ace_data_transmitter
      IMPORTING
        !e_column .
    METHODS on_grid_button_click
      FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING
        !es_col_id
        !es_row_no .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_DATA_RECEIVER IMPLEMENTATION.


  method CONSTRUCTOR.


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


  endmethod.


  method ON_GRID_BUTTON_CLICK.


    FIELD-SYMBOLS: <f_tab>   TYPE STANDARD TABLE.

    CHECK m_from_field = es_col_id-fieldname.
    ASSIGN lo_tab_from->mr_table->* TO <f_tab>.
    READ TABLE <f_tab> INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<tab>).
    ASSIGN COMPONENT es_col_id-fieldname OF STRUCTURE <tab> TO  FIELD-SYMBOL(<f_field>).
    CHECK lo_sel_to IS NOT INITIAL.
    lo_sel_to->set_value( i_field = m_to_field i_low = <f_field> ).
    lo_sel_to->raise_selection_done( ).


  endmethod.


  method SHUT_DOWN.


    IF mo_transmitter IS NOT INITIAL.
      SET HANDLER me->update FOR mo_transmitter  ACTIVATION space.
    ELSE.
      SET HANDLER me->update FOR ALL INSTANCES  ACTIVATION space.
    ENDIF.
    CLEAR lo_sel_to.


  endmethod.


  method UPDATE.


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


  endmethod.


  method UPDATE_COL.


    DATA: l_updated,
          lt_sel_row   TYPE zcl_ace_sel_opt=>t_sel_row.

    FIELD-SYMBOLS: <tab>   TYPE STANDARD TABLE,
                   <field> TYPE any.

    CHECK lo_sel_to IS NOT INITIAL.
    READ TABLE lo_sel_to->mt_sel_tab ASSIGNING FIELD-SYMBOL(<to>) WITH KEY field_label = m_to_field.
    DATA(lt_old_range) = <to>-range.
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

    MOVE-CORRESPONDING <to> TO lt_sel_row.
    IF <to>-range = lt_old_range.
      l_updated = abap_true."so as not to have an infinite event loop
    ENDIF.
    IF <to>-transmitter IS BOUND AND l_updated IS INITIAL.
      <to>-transmitter->emit( EXPORTING e_row = lt_sel_row ).
      lo_sel_to->raise_selection_done( ).
    ENDIF.


  endmethod.
ENDCLASS.
