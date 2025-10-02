CLASS zcl_ace_data_transmitter DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    EVENTS data_changed
      EXPORTING
        VALUE(e_row) TYPE zcl_ace_sel_opt=>t_sel_row .
    EVENTS col_changed
      EXPORTING
        VALUE(e_column) TYPE lvc_fname .

    METHODS emit
      IMPORTING
        !e_row TYPE zcl_ace_sel_opt=>t_sel_row .
    METHODS emit_col
      IMPORTING
        !e_column TYPE lvc_fname .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_DATA_TRANSMITTER IMPLEMENTATION.


  method EMIT.

    RAISE EVENT data_changed EXPORTING e_row = e_row.


  endmethod.


  method EMIT_COL.

    RAISE EVENT col_changed EXPORTING e_column = e_column.

  endmethod.
ENDCLASS.
