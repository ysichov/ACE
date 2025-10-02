class ZCL_ACE_EVENT_HANDLER definition
  public
  create public .

public section.

  data MO_VIEWER type ref to ZCL_ACE .

  methods CONSTRUCTOR
    importing
      !IO_DEBUGGER type ref to ZCL_ACE .
  methods ON_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !COLUMN
      !ROW .
  methods ON_EDITOR_DOUBLE_CLICK
    for event DBLCLICK of CL_GUI_ABAPEDIT
    importing
      !SENDER .
  methods ON_EDITOR_BORDER_CLICK
    for event BORDER_CLICK of CL_GUI_ABAPEDIT
    importing
      !CNTRL_PRESSED_SET
      !LINE
      !SHIFT_PRESSED_SET .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_EVENT_HANDLER IMPLEMENTATION.


  method CONSTRUCTOR.

    mo_viewer = io_debugger.

  endmethod.


  method ON_DOUBLE_CLICK.


    READ TABLE mo_viewer->mo_window->mt_stack INDEX row INTO DATA(ls_stack).
    "only for coverage stack selection should work.
    "CHECK mo_viewer->mo_window->mt_coverage IS NOT INITIAL.


    MOVE-CORRESPONDING ls_stack TO mo_viewer->mo_window->m_prg.
    MOVE-CORRESPONDING ls_stack TO mo_viewer->ms_stack.

    mo_viewer->mo_window->show_coverage( ).
    mo_viewer->show( ).


  endmethod.


  method ON_EDITOR_BORDER_CLICK.


    DATA: lv_type    TYPE char1.

    IF cntrl_pressed_set IS INITIAL.
      lv_type = 'S'.
    ELSE.
      lv_type = 'E'.
    ENDIF.

    LOOP AT mo_viewer->mo_window->mt_bpoints ASSIGNING FIELD-SYMBOL(<point>) WHERE line = line.
      lv_type = <point>-type.

      CALL FUNCTION 'RS_DELETE_BREAKPOINT'
        EXPORTING
          index        = line
          mainprog     = mo_viewer->mo_window->m_prg-program
          program      = mo_viewer->mo_window->m_prg-include
          bp_type      = lv_type
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
          program      = mo_viewer->mo_window->m_prg-include
          mainprogram  = mo_viewer->mo_window->m_prg-program
          bp_type      = lv_type
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.

    ENDIF.
    DELETE mo_viewer->mo_window->mt_bpoints WHERE del IS NOT INITIAL.
    mo_viewer->mo_window->set_program_line( ).

  endmethod.


  method ON_EDITOR_DOUBLE_CLICK.

    sender->get_selection_pos( IMPORTING from_line = DATA(fr_line) from_pos = DATA(fr_pos) to_line = DATA(to_line) to_pos = DATA(to_pos) ).


  endmethod.
ENDCLASS.
