INTERFACE zif_ace_stmt_handler PUBLIC.

  METHODS handle
    IMPORTING
      !io_scan     TYPE REF TO cl_ci_scan
      !i_stmt_idx  TYPE i
      !i_program   TYPE program
      !i_include   TYPE program
      !i_class     TYPE string OPTIONAL
      !i_interface TYPE string OPTIONAL
      !i_evtype    TYPE string OPTIONAL
      !i_ev_name   TYPE string OPTIONAL
    CHANGING
      !cs_source   TYPE zif_ace_parse_data=>ts_parse_data .

ENDINTERFACE.
