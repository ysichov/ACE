class ZCL_ACE_SRC_POPUP definition
  public
  inheriting from ZCL_ACE_POPUP
  final
  create public .

  public section.

    data MO_EDITOR type ref to CL_GUI_ABAPEDIT .
    data MV_PROGRAM type PROGRAM .
    data MV_INCLUDE type PROGRAM .
    data MV_FROM type I .
    data MO_SCAN type ref to CL_CI_SCAN .

    methods CONSTRUCTOR
      importing
        !I_TITLE   type TEXT100
        !IT_SRC    type STRING_TABLE
        !I_PROGRAM type PROGRAM
        !I_INCLUDE type PROGRAM
        !I_FROM    type I .
    methods ON_BORDER_CLICK
      for event BORDER_CLICK of CL_GUI_ABAPEDIT
      importing
        !CNTRL_PRESSED_SET
        !LINE
        !SHIFT_PRESSED_SET .
    methods REFRESH_BREAKPOINTS .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_ACE_SRC_POPUP IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( ).
    mv_program = i_program.
    mv_include = i_include.
    mv_from    = COND #( WHEN i_from > 0 THEN i_from ELSE 1 ).
    DATA(lo_src) = cl_ci_source_include=>create( p_name = i_include ).
    mo_scan = NEW cl_ci_scan( p_include = lo_src ).

    mo_box = create( i_width = 700 i_hight = 500 i_name = i_title ).
    IF mo_box IS INITIAL. RETURN. ENDIF.
    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_editor
      EXPORTING parent = mo_box max_number_chars = 100
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. RETURN. ENDIF.
    mo_editor->register_event_border_click( ).
    mo_editor->register_event_break_changed( ).
    SET HANDLER on_border_click FOR mo_editor.
    mo_editor->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
    mo_editor->create_document( ).
    mo_editor->set_readonly_mode( 1 ).

    DATA lt_src TYPE STANDARD TABLE OF string.
    lt_src = it_src.
    mo_editor->set_text( table = lt_src ).
    refresh_breakpoints( ).
  endmethod.


  method ON_BORDER_CLICK.
    DATA lv_type TYPE char1.
    DATA lv_abs  TYPE i.
    lv_abs = mv_from + line - 1.
    CHECK lv_abs > 0.

    " A breakpoint belongs to the first line of the ABAP statement.
    " Continuation lines (for example TO ... after APPEND VALUE) must
    " therefore be mapped back to the statement's first token row.
    IF mo_scan IS BOUND.
      LOOP AT mo_scan->statements INTO DATA(ls_stmt).
        READ TABLE mo_scan->tokens INDEX ls_stmt-from INTO DATA(ls_first_token).
        READ TABLE mo_scan->tokens INDEX ls_stmt-to   INTO DATA(ls_last_token).
        IF sy-subrc = 0 AND lv_abs BETWEEN ls_first_token-row AND ls_last_token-row.
          lv_abs = ls_first_token-row.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Already a session breakpoint here? → remove it.
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING main_program         = mv_program
      IMPORTING breakpoints_complete = DATA(points)
      EXCEPTIONS OTHERS               = 4.
    LOOP AT points INTO DATA(point) WHERE include = mv_include AND line = lv_abs.
      CALL FUNCTION 'RS_DELETE_BREAKPOINT'
        EXPORTING index = lv_abs mainprog = mv_program program = mv_include bp_type = 'S'
        EXCEPTIONS not_executed = 1 OTHERS = 2.
      refresh_breakpoints( ).
      RETURN.
    ENDLOOP.

    " Already an external breakpoint here? → remove it.
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING main_program         = mv_include
                flag_other_session   = abap_true
      IMPORTING breakpoints_complete = points
      EXCEPTIONS OTHERS               = 4.
    LOOP AT points INTO point WHERE include = mv_include AND line = lv_abs.
      CALL FUNCTION 'RS_DELETE_BREAKPOINT'
        EXPORTING index = lv_abs mainprog = mv_include program = mv_include bp_type = 'E'
        EXCEPTIONS not_executed = 1 OTHERS = 2.
      refresh_breakpoints( ).
      RETURN.
    ENDLOOP.

    " None yet → set one. Ctrl-click makes it an external (other-session) breakpoint.
    lv_type = COND #( WHEN cntrl_pressed_set IS INITIAL THEN 'S' ELSE 'E' ).
    CALL FUNCTION 'RS_SET_BREAKPOINT'
      EXPORTING index = lv_abs program = mv_include mainprogram = mv_program bp_type = lv_type
      EXCEPTIONS not_executed = 1 OTHERS = 2.
    refresh_breakpoints( ).
  endmethod.


  method REFRESH_BREAKPOINTS.
    TYPES lntab TYPE STANDARD TABLE OF i.
    DATA lines_s TYPE lntab.
    DATA lines_e TYPE lntab.
    DATA lv_ln   TYPE i.

    IF mo_editor IS INITIAL. RETURN. ENDIF.
    mo_editor->remove_all_marker( 2 ).
    mo_editor->remove_all_marker( 4 ).

    " Session breakpoints (current user)
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING main_program         = mv_program
      IMPORTING breakpoints_complete = DATA(points)
      EXCEPTIONS OTHERS               = 4.
    LOOP AT points INTO DATA(point) WHERE include = mv_include.
      lv_ln = point-line - mv_from + 1.
      IF lv_ln > 0. APPEND lv_ln TO lines_s. ENDIF.
    ENDLOOP.
    mo_editor->set_marker( EXPORTING marker_number = 2 marker_lines = lines_s ).

    " External / other-session breakpoints
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING main_program         = mv_include
                flag_other_session   = abap_true
      IMPORTING breakpoints_complete = points
      EXCEPTIONS OTHERS               = 4.
    LOOP AT points INTO point WHERE include = mv_include.
      lv_ln = point-line - mv_from + 1.
      IF lv_ln > 0. APPEND lv_ln TO lines_e. ENDIF.
    ENDLOOP.
    mo_editor->set_marker( EXPORTING marker_number = 4 marker_lines = lines_e ).

    mo_editor->draw( ).
  endmethod.

ENDCLASS.
