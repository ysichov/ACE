class ZCL_ACE_DATA_TRANSMITTER definition
  public
  create public .

public section.

  events DATA_CHANGED
    exporting
      value(E_ROW) type zCL_ace_TYPES=>T_SEL_ROW .
  events COL_CHANGED
    exporting
      value(E_COLUMN) type LVC_FNAME .

  methods EMIT
    importing
      !E_ROW type zCL_ace_TYPES=>T_SEL_ROW .
  methods EMIT_COL
    importing
      !E_COLUMN type LVC_FNAME .
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
