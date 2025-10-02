class ZCL_ACE_BOX_HANDLER definition
  public
  create public .

public section.

  methods ON_BOX_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
  methods ON_TABLE_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_BOX_HANDLER IMPLEMENTATION.


  method ON_BOX_CLOSE.



  endmethod.


  method ON_TABLE_CLOSE.

    DATA: lv_tabix LIKE sy-tabix.
    sender->free( ).

    "Free Memory
    LOOP AT zcl_ace_appl=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>) WHERE alv_viewer IS NOT INITIAL.
      IF <obj>-alv_viewer->mo_box = sender.
        lv_tabix = sy-tabix.
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
      IF lv_tabix NE 0.
        DELETE zcl_ace_appl=>mt_obj INDEX lv_tabix.
      ENDIF.
    ENDIF.

  endmethod.
ENDCLASS.
