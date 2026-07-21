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
    " Same Classic / HTML switch as the main source window
    data MO_SPLIT type ref to CL_GUI_SPLITTER_CONTAINER .
    data MO_TB_CONT type ref to CL_GUI_CONTAINER .
    data MO_SRC_CONT type ref to CL_GUI_CONTAINER .
    data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
    data MO_HTML type ref to CL_GUI_HTML_VIEWER .
    data MV_HTML_MODE type ABAP_BOOL .
    data MV_FOLDED type ABAP_BOOL .

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
    methods HND_TOOLBAR
      for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
      importing
        !FCODE .
    " Re-renders the shown source into the HTML control
    methods REFRESH_HTML .
    " Hyperlink / gutter click in the HTML view
    methods ON_HTML_SAPEVENT
      for event SAPEVENT of CL_GUI_HTML_VIEWER
      importing
        !ACTION
        !QUERY_TABLE .
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

    " Size the dialog to the code: ~40 lines fill the 400 maximum, shorter
    " snippets shrink proportionally so small methods get a small window.
    DATA(lv_height) = lines( it_src ) * 9 + 60.
    IF lv_height > 400. lv_height = 400. ENDIF.
    IF lv_height < 120. lv_height = 120. ENDIF.

    " Room for the toolbar on top of the code
    lv_height = lv_height + 30.
    IF lv_height > 430. lv_height = 430. ENDIF.

    mo_box = create( i_width = 700 i_hight = lv_height i_name = i_title ).
    IF mo_box IS INITIAL. RETURN. ENDIF.
    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_split
      EXPORTING parent = mo_box rows = 2 columns = 1 EXCEPTIONS OTHERS = 1.
    mo_split->get_container( EXPORTING row = 1 column = 1 RECEIVING container = mo_tb_cont ).
    mo_split->get_container( EXPORTING row = 2 column = 1 RECEIVING container = mo_src_cont ).
    " Absolute: a percentage of a small popup clips the buttons
    mo_split->set_row_mode( mode = cl_gui_splitter_container=>mode_absolute ).
    mo_split->set_row_height( id = 1 height = 28 ).
    mo_split->set_row_sash( id = 1 type = 0 value = 0 ).

    CREATE OBJECT mo_toolbar EXPORTING parent = mo_tb_cont.
    DATA: lt_btn TYPE ttb_button,
          lt_tbev TYPE cntl_simple_events,
          ls_tbev LIKE LINE OF lt_tbev.
    lt_btn = VALUE #(
      ( function = 'VIEWMODE' icon = CONV #( icon_htm )
        quickinfo = 'Toggle source rendering: Classic editor / HTML with links and folding'
        text = 'Classic view' )
      ( butn_type = 3 )
      ( function = 'FOLDALL' icon = CONV #( icon_collapse )
        quickinfo = 'Collapse / expand all control structures (HTML view only)'
        text = 'Collapse all' ) ).
    mo_toolbar->add_button_group( lt_btn ).
    ls_tbev-eventid = cl_gui_toolbar=>m_id_function_selected.
    ls_tbev-appl_event = space.
    APPEND ls_tbev TO lt_tbev.
    mo_toolbar->set_registered_events( events = lt_tbev ).
    SET HANDLER hnd_toolbar FOR mo_toolbar.
    mo_toolbar->set_visible( 'X' ).

    CREATE OBJECT mo_editor
      EXPORTING parent = mo_src_cont max_number_chars = 100
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

    " Note: the z-order of SAPGUI dialog windows is owned by the frontend.
    " set_focus only moves the input focus inside the control set, so already
    " open popups cannot be raised from ABAP — the user has to click them.
    mo_box->set_focus( mo_box ).
  endmethod.


  method HND_TOOLBAR.

    IF fcode = 'FOLDALL'.
      IF mv_html_mode IS INITIAL.
        MESSAGE 'Folding is available in HTML view only' TYPE 'S'.
        RETURN.
      ENDIF.
      mv_folded = xsdbool( mv_folded IS INITIAL ).
      mo_toolbar->set_button_info(
        EXPORTING fcode = 'FOLDALL'
                  text  = COND #( WHEN mv_folded = abap_true
                                  THEN 'Expand all' ELSE 'Collapse all' ) ).
      refresh_html( ).
      RETURN.
    ENDIF.

    CHECK fcode = 'VIEWMODE'.
    IF mv_html_mode = abap_true.
      mv_html_mode = abap_false.
      IF mo_html IS NOT INITIAL. mo_html->set_visible( space ). ENDIF.
      mo_editor->set_visible( 'X' ).
      mo_toolbar->set_button_info( EXPORTING fcode = 'VIEWMODE' text = 'Classic view' ).
    ELSE.
      mv_html_mode = abap_true.
      mo_editor->set_visible( space ).
      refresh_html( ).
      mo_toolbar->set_button_info( EXPORTING fcode = 'VIEWMODE' text = 'HTML view' ).
    ENDIF.
    cl_gui_cfw=>flush( ).

  endmethod.


  method REFRESH_HTML.

    CHECK mv_html_mode = abap_true.

    IF mo_html IS INITIAL.
      CREATE OBJECT mo_html
        EXPORTING parent = mo_src_cont
        EXCEPTIONS OTHERS = 1.
      IF sy-subrc <> 0.
        mv_html_mode = abap_false.
        mo_editor->set_visible( 'X' ).
        MESSAGE 'HTML view is not available in this GUI' TYPE 'I'.
        RETURN.
      ENDIF.
      DATA: lt_ev TYPE cntl_simple_events,
            ls_ev TYPE cntl_simple_event.
      ls_ev-eventid    = cl_gui_html_viewer=>m_id_sapevent.
      ls_ev-appl_event = abap_true.
      APPEND ls_ev TO lt_ev.
      mo_html->set_registered_events( events = lt_ev ).
      SET HANDLER on_html_sapevent FOR mo_html.
    ENDIF.

    " Breakpoints of this include, mapped onto the displayed slice
    DATA lt_bp_s TYPE zcl_ace_code_html=>tt_lines.
    DATA lt_bp_e TYPE zcl_ace_code_html=>tt_lines.
    DATA lv_ln TYPE i.
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING main_program         = mv_program
      IMPORTING breakpoints_complete = DATA(points)
      EXCEPTIONS OTHERS               = 4.
    LOOP AT points INTO DATA(point) WHERE include = mv_include.
      lv_ln = point-line - mv_from + 1.
      IF lv_ln > 0. APPEND lv_ln TO lt_bp_s. ENDIF.
    ENDLOOP.
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING main_program         = mv_include
                flag_other_session   = abap_true
      IMPORTING breakpoints_complete = points
      EXCEPTIONS OTHERS               = 4.
    LOOP AT points INTO point WHERE include = mv_include.
      lv_ln = point-line - mv_from + 1.
      IF lv_ln > 0. APPEND lv_ln TO lt_bp_e. ENDIF.
    ENDLOOP.

    " The popup shows a slice of the include, so the renderer needs to know
    " which absolute line the slice starts on to line the scan up with it.
    DATA(lt_html) = zcl_ace_code_html=>build(
      it_source = mt_src
      io_scan   = mo_scan
      i_title   = |{ mv_include }|
      it_bp_s   = lt_bp_s
      it_bp_e   = lt_bp_e
      i_folded  = mv_folded
      i_offset  = mv_from ).

    DATA lv_url TYPE w3url.
    mo_html->load_data(
      IMPORTING assigned_url = lv_url
      CHANGING  data_table   = lt_html
      EXCEPTIONS OTHERS      = 1 ).
    CHECK sy-subrc = 0.
    mo_html->show_url( url = lv_url ).
    mo_html->set_visible( 'X' ).
    cl_gui_cfw=>flush( ).

  endmethod.


  method ON_HTML_SAPEVENT.
    " Only the breakpoint gutter is wired here: navigation would have to
    " leave this popup, which is exactly what it exists to avoid.
    DATA lv_line TYPE i.
    DATA lv_ctrl TYPE abap_bool.
    CHECK to_upper( action ) = 'BP'.
    LOOP AT query_table INTO DATA(ls_q).
      CASE to_lower( ls_q-name ).
        WHEN 'l'. lv_line = ls_q-value.
        WHEN 'c'. lv_ctrl = xsdbool( ls_q-value = '1' ).
      ENDCASE.
    ENDLOOP.
    CHECK lv_line > 0.
    on_border_click( cntrl_pressed_set = COND #( WHEN lv_ctrl = abap_true THEN 'X' ELSE space )
                     line              = lv_line
                     shift_pressed_set = space ).
    refresh_html( ).
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
    DATA(lv_mapped) = abap_false.
    IF mo_scan IS BOUND.
      LOOP AT mo_scan->statements INTO DATA(ls_stmt).
        READ TABLE mo_scan->tokens INDEX ls_stmt-from INTO DATA(ls_first_token).
        IF sy-subrc <> 0. CONTINUE. ENDIF.
        READ TABLE mo_scan->tokens INDEX ls_stmt-to INTO DATA(ls_last_token).
        IF sy-subrc <> 0. CONTINUE. ENDIF.
        IF lv_abs BETWEEN ls_first_token-row AND ls_last_token-row.
          lv_abs    = ls_first_token-row.
          lv_mapped = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
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
