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
    data MT_SRC type STRING_TABLE .
    " Diagnostic trace shown in the dialog caption (MESSAGE is swallowed
    " inside CFW event handlers).
    data MV_DBG type STRING .

    methods CONSTRUCTOR
      importing
        !I_TITLE   type TEXT100
        !IT_SRC    type STRING_TABLE
        !I_PROGRAM type PROGRAM
        !I_INCLUDE type PROGRAM
        !I_FROM    type I
        !IO_SCAN   type ref to CL_CI_SCAN optional .
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
    mt_src     = it_src.
    " Prefer the already parsed scan of this include — re-scanning from the
    " database can fail or differ for generated / class-pool includes.
    IF io_scan IS BOUND.
      mo_scan = io_scan.
    ELSE.
      TRY.
          DATA(lo_src) = cl_ci_source_include=>create( p_name = i_include ).
          mo_scan = NEW cl_ci_scan( p_include = lo_src ).
        CATCH cx_root.
          CLEAR mo_scan.
      ENDTRY.
    ENDIF.

    DATA(lv_height) = lines( it_src ) * 18 + 100.
    IF lv_height < 260. lv_height = 260. ENDIF.
    IF lv_height > 400. lv_height = 400. ENDIF.

    mo_box = create( i_width = 700 i_hight = lv_height i_name = i_title ).
    IF mo_box IS INITIAL. RETURN. ENDIF.
    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_editor
      EXPORTING parent = mo_box max_number_chars = 100
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. RETURN. ENDIF.
    " Same init sequence as the main code viewer — without upload_properties
    " and set_registered_events the control never raises BORDER_CLICK, so the
    " editor handles the border click itself and places the breakpoint wrong.
    mo_editor->init_completer( ).
    mo_editor->upload_properties(
      EXCEPTIONS dp_error_create = 1 dp_error_general = 2 dp_error_send = 3 OTHERS = 4 ).
    DATA: lt_events TYPE cntl_simple_events,
          ls_event  TYPE cntl_simple_event.
    ls_event-eventid = cl_gui_textedit=>event_double_click.
    APPEND ls_event TO lt_events.
    mo_editor->set_registered_events( lt_events ).
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

    " A breakpoint belongs to the FIRST line of the ABAP statement. A click on
    " a continuation line (e.g. `TO cs_source-t_composed.` of a multi-line
    " APPEND VALUE ... ) must be mapped back to the statement's first token row,
    " otherwise the breakpoint is set on a line the debugger never stops at.
    DATA(lv_raw)    = lv_abs.
    DATA(lv_mapped) = abap_false.
    DATA lv_dbg_f TYPE i.
    DATA lv_dbg_t TYPE i.
    IF mo_scan IS BOUND.
      LOOP AT mo_scan->statements INTO DATA(ls_stmt).
        READ TABLE mo_scan->tokens INDEX ls_stmt-from INTO DATA(ls_first_token).
        IF sy-subrc <> 0. CONTINUE. ENDIF.
        READ TABLE mo_scan->tokens INDEX ls_stmt-to INTO DATA(ls_last_token).
        IF sy-subrc <> 0. CONTINUE. ENDIF.
        IF lv_abs BETWEEN ls_first_token-row AND ls_last_token-row.
          lv_dbg_f   = ls_first_token-row.
          lv_dbg_t   = ls_last_token-row.
          lv_abs     = ls_first_token-row.
          lv_mapped  = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      " MESSAGE does not surface from a CFW event handler — show the trace in
      " the dialog caption instead.
      mv_dbg = |L{ line } raw{ lv_raw } st{ lines( mo_scan->statements ) }| &&
               | map{ lv_mapped } { lv_dbg_f }..{ lv_dbg_t } abs{ lv_abs }|.
    ELSE.
      mv_dbg = |L{ line } raw{ lv_raw } NOSCAN|.
    ENDIF.
    IF mo_box IS NOT INITIAL.
      mo_box->set_caption( CONV #( mv_dbg ) ).
    ENDIF.

    " Fallback ONLY when the scan could not resolve the statement (e.g. the
    " include is generated and not covered by the scan): use the displayed
    " indentation — a continuation line belongs to the nearest preceding
    " line with a smaller indent.
    IF lv_mapped = abap_false AND line > 0 AND line <= lines( mt_src ).
      DATA(lv_clicked) = mt_src[ line ].
      IF lv_clicked IS NOT INITIAL.
        DATA(lv_indent) = strlen( lv_clicked ) - strlen( condense( lv_clicked ) ).
        DATA(lv_row) = line - 1.
        WHILE lv_row > 0.
          DATA(lv_prev) = mt_src[ lv_row ].
          IF lv_prev IS NOT INITIAL.
            DATA(lv_prev_indent) = strlen( lv_prev ) - strlen( condense( lv_prev ) ).
            IF lv_prev_indent < lv_indent.
              lv_abs = mv_from + lv_row - 1.
              EXIT.
            ENDIF.
          ENDIF.
          lv_row = lv_row - 1.
        ENDWHILE.
      ENDIF.
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
      IF mo_box IS NOT INITIAL.
        mo_box->set_caption( CONV #( |{ mv_dbg } DELs rc{ sy-subrc }| ) ).
      ENDIF.
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
    IF mo_box IS NOT INITIAL.
      mo_box->set_caption( CONV #( |{ mv_dbg } SET{ lv_abs } rc{ sy-subrc }| ) ).
    ENDIF.
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
    DATA lv_dbg TYPE string.
    LOOP AT points INTO DATA(point) WHERE include = mv_include.
      lv_ln = point-line - mv_from + 1.
      lv_dbg = |{ lv_dbg } { point-line }>{ lv_ln }|.
      IF lv_ln > 0. APPEND lv_ln TO lines_s. ENDIF.
    ENDLOOP.
    IF mo_box IS NOT INITIAL.
      mo_box->set_caption( CONV #( |{ mv_dbg } pts:{ lv_dbg }| ) ).
    ENDIF.
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
