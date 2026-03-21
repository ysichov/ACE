class ZCL_ACE_PARSE_EVENTS definition
  public
  create public .

public section.

  interfaces ZIF_ACE_STMT_HANDLER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_PARSE_EVENTS IMPLEMENTATION.


  method ZIF_ACE_STMT_HANDLER~HANDLE.

    CHECK i_stmt_idx = 0.

    LOOP AT io_scan->structures INTO DATA(struc) WHERE type = 'E'.
      DATA lv_name TYPE string.
      DATA lv_line TYPE i.
      CLEAR: lv_name, lv_line.

      READ TABLE io_scan->statements
        INDEX struc-stmnt_from
        INTO DATA(stmt).
      IF sy-subrc = 0.
        DATA lv_from TYPE i.
        DATA lv_to   TYPE i.
        lv_from = stmt-from.
        lv_to   = stmt-to.
        lv_line = io_scan->tokens[ lv_from ]-row.
        LOOP AT io_scan->tokens FROM lv_from TO lv_to INTO DATA(tok).
          IF lv_name IS INITIAL.
            lv_name = tok-str.
          ELSE.
            lv_name = |{ lv_name } { tok-str }|.
          ENDIF.
        ENDLOOP.
      ENDIF.

      APPEND VALUE zif_ace_parse_data=>ts_event(
        program    = i_program
        include    = i_include
        type       = struc-type
        stmnt_type = struc-stmnt_type
        stmnt_from = struc-stmnt_from
        stmnt_to   = struc-stmnt_to
        name       = lv_name
        line       = lv_line
      ) TO cs_source-t_events.
    ENDLOOP.

  endmethod.
ENDCLASS.
