class ZCL_ACE_POPUP definition
  public
  create public .

public section.

  class-data M_COUNTER type I .
  data M_ADDITIONAL_NAME type STRING .
  data MO_BOX type ref to CL_GUI_DIALOGBOX_CONTAINER .
  data MO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_IMP_EXP type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VARIABLES_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_TABLES_CONTAINER type ref to CL_GUI_CONTAINER .

  methods CONSTRUCTOR
    importing
      !I_ADDITIONAL_NAME type STRING optional .
  methods CREATE
    importing
      !I_WIDTH type I
      !I_HIGHT type I
      !I_NAME type TEXT100 optional
    returning
      value(RO_BOX) type ref to CL_GUI_DIALOGBOX_CONTAINER .
  methods ON_BOX_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_POPUP IMPLEMENTATION.


  method CONSTRUCTOR.

    m_additional_name = i_additional_name.


  endmethod.


  method CREATE.


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


  endmethod.


  method ON_BOX_CLOSE.

    LOOP AT zcl_ace_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>) WHERE parent = sender .
      <popup>-child->free( ).
      CLEAR <popup>-child.
    ENDLOOP.
    IF sy-subrc <> 0.
      DELETE  zcl_ace_appl=>mt_popups WHERE child = sender.
    ENDIF.
    DELETE zcl_ace_appl=>mt_popups WHERE child IS INITIAL.
    sender->free( ).


  endmethod.
ENDCLASS.
