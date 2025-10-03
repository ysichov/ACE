class ZCL_ACE_WINDOW definition
  public
  inheriting from ZCL_ACE_POPUP
  create public .

public section.

  types:
    BEGIN OF ts_table,
             ref      TYPE REF TO data,
             kind(1),
             value    TYPE string,
             typename TYPE abap_abstypename,
             fullname TYPE string,
           END OF ts_table .
  types:
    BEGIN OF ts_calls,
             class TYPE string,
             event TYPE string,
             type  TYPE string,
             name  TYPE string,
             outer TYPE string,
             inner TYPE string,
           END OF ts_calls .
  types:
    tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer .
  types:
    BEGIN OF ts_calls_line,
             program   TYPE string,
             class     TYPE string,
             eventtype TYPE string,
             eventname TYPE string,
             index     TYPE i,
           END OF ts_calls_line .
  types:
    tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY .
  types:
    BEGIN OF ts_kword,
             index     TYPE i,
             line      TYPE i,
             name      TYPE string,
             from      TYPE i,
             to        TYPE i,
             tt_calls  TYPE tt_calls,
             to_prog   TYPE string,
             to_class  TYPE string,
             to_evtype TYPE string,
             to_evname TYPE string,
             "to_prog   type string,
           END OF ts_kword .
  types:
    BEGIN OF ts_calculated,
             program    TYPE string,
             line       TYPE i,
             calculated TYPE string,
           END OF ts_calculated .
  types:
    BEGIN OF ts_composing,
             program   TYPE string,
             line      TYPE i,
             composing TYPE string,
           END OF ts_composing .
  types:
    BEGIN OF ts_refvar,
             program TYPE string,
             name    TYPE string,
             class   TYPE string,
           END OF ts_refvar .
  types:
    tt_kword      TYPE STANDARD TABLE OF ts_kword WITH EMPTY KEY .
  types:
    tt_calculated TYPE STANDARD TABLE OF ts_calculated WITH EMPTY KEY .
  types:
    tt_composed   TYPE STANDARD TABLE OF ts_composing WITH EMPTY KEY .
  types:
    tt_refvar     TYPE STANDARD TABLE OF ts_refvar WITH EMPTY KEY .
  types:
    BEGIN OF ts_params,
             class     TYPE string,
             event     TYPE string,
             name      TYPE string,
             param     TYPE string,
             type      TYPE char1,
             preferred TYPE char1,
           END OF ts_params .
  types:
    tt_params TYPE STANDARD TABLE OF ts_params WITH EMPTY KEY .
  types:
    BEGIN OF ts_int_tabs,
             eventtype TYPE string,
             eventname TYPE string,
             name      TYPE string,
             type      TYPE string,
           END OF ts_int_tabs .
  types:
    tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY .
  types:
    BEGIN OF ts_prog,
             include    TYPE program,
             source     TYPE REF TO cl_ci_source_include,
             scan       TYPE REF TO cl_ci_scan,
             t_keywords TYPE tt_kword,
             selected   TYPE xfeld,
           END OF ts_prog .
  types:
    tt_progs TYPE STANDARD TABLE OF ts_prog WITH EMPTY KEY .
  types:
    BEGIN OF ts_source,
             tt_progs      TYPE tt_progs,
             t_calculated  TYPE tt_calculated,
             t_composed    TYPE tt_composed,
             t_params      TYPE tt_params,
             tt_tabs       TYPE tt_tabs,
             tt_calls_line TYPE tt_calls_line,
             tt_refvar     TYPE tt_refvar,
           END OF ts_source .
  types:
    BEGIN OF ts_locals,
             program    TYPE tpda_program,
             eventtype  TYPE tpda_event_type,
             eventname  TYPE tpda_event,
             loc_fill   TYPE xfeld,
             locals_tab TYPE tpda_scr_locals_it,
             mt_fs      TYPE tpda_scr_locals_it,
           END OF ts_locals .
  types:
    BEGIN OF ts_globals,
             program     TYPE tpda_program,
             glob_fill   TYPE xfeld,
             globals_tab TYPE tpda_scr_globals_it,
             mt_fs       TYPE tpda_scr_locals_it,
           END OF ts_globals .
  types:
    BEGIN OF ts_watch,
             program TYPE string,
             line    TYPE i,
           END OF ts_watch .
  types:
    tt_watch TYPE STANDARD  TABLE OF ts_watch WITH EMPTY KEY .
  types:
    BEGIN OF ts_bpoint,
             program TYPE string,
             include TYPE string,
             line    TYPE i,
             type    TYPE char1,
             del     TYPE char1,
           END OF ts_bpoint .
  types:
    tt_bpoints TYPE STANDARD TABLE OF ts_bpoint WITH EMPTY KEY .
  types:
    tt_table TYPE STANDARD TABLE OF ts_table
          WITH NON-UNIQUE DEFAULT KEY .

  data M_VERSION type X .
  data M_HISTORY type X .
  data M_VISUALIZATION type X .
  data M_VARHIST type X .
  data M_ZCODE type X .
  data M_DIRECTION type X .
  data M_PRG type TPDA_SCR_PRG_INFO .
  data M_DEBUG_BUTTON like SY-UCOMM .
  data M_SHOW_STEP type XFELD .
  data MT_BPOINTS type TT_BPOINTS .
  data MO_VIEWER type ref to ZCL_ACE .
  data MO_SPLITTER_CODE type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_VAR type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_STEPS type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_TOOLBAR_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_IMPORTING_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_LOCALS_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_EXPORTING_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_CODE_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_IMP_EXP_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_EDITOR_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_STEPS_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_STACK_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_HIST_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_CODE_VIEWER type ref to CL_GUI_ABAPEDIT .
  data:
    mt_stack               TYPE TABLE OF zcl_ace_appl=>t_stack .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_SALV_STACK type ref to CL_SALV_TABLE .
  data MO_SALV_STEPS type ref to CL_SALV_TABLE .
  data MO_SALV_HIST type ref to CL_SALV_TABLE .
  data MT_BREAKS type TPDA_BP_PERSISTENT_IT .
  data MT_WATCH type TT_WATCH .
  data MT_COVERAGE type TT_WATCH .
  data M_HIST_DEPTH type I .
  data M_START_STACK type I .
  data:
    mt_source              TYPE STANDARD  TABLE OF ts_source .
  data MS_SOURCES type TS_SOURCE .
  data:
    mt_params              TYPE STANDARD  TABLE OF ts_params .
  data:
    mt_locals_set          TYPE STANDARD TABLE OF ts_locals .
  data:
    mt_globals_set         TYPE STANDARD TABLE OF ts_globals .

  methods CONSTRUCTOR
    importing
      !I_DEBUGGER type ref to ZCL_ACE
      !I_ADDITIONAL_NAME type STRING optional .
  methods ADD_TOOLBAR_BUTTONS .
  methods HND_TOOLBAR
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods SET_PROGRAM
    importing
      !IV_PROGRAM type PROGRAM .
  methods SET_PROGRAM_LINE
    importing
      !IV_LINE like SY-INDEX optional .
  methods CREATE_CODE_VIEWER .
  methods SHOW_STACK .
  methods SHOW_COVERAGE .
  methods ON_STACK_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
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



CLASS ZCL_ACE_WINDOW IMPLEMENTATION.


  method ADD_TOOLBAR_BUTTONS.


    DATA: lt_button TYPE ttb_button,
          lt_events TYPE cntl_simple_events,
          ls_events LIKE LINE OF lt_events.

    lt_button  = VALUE #(
     "( COND #( WHEN p_dest IS NOT INITIAL
      "THEN VALUE #
      ( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' )
       ")      ")

     ( COND #( WHEN zcl_ace_appl=>is_mermaid_active = abap_true
      THEN VALUE #( function = 'DIAGRAM' icon = CONV #( icon_workflow_process ) quickinfo = ' Calls Flow' text = 'Diagram' ) ) )
     ( function = 'SMART' icon = CONV #( icon_wizard ) quickinfo = 'Calculations sequence' text = 'Calculations Flow' )
     ( function = 'CODE' icon = CONV #( icon_customer_warehouse ) quickinfo = 'Only Z' text = 'Only Z' )
     "( function = 'COVERAGE' icon = CONV #( icon_wizard ) quickinfo = 'Coverage ' text = 'Coverage' )
     ( butn_type = 3  )
     ( function = 'STEPS' icon = CONV #( icon_next_step ) quickinfo = 'Steps table' text = 'Steps' )
     ( butn_type = 3  )
     ( function = 'INFO' icon = CONV #( icon_bw_gis ) quickinfo = 'Documentation' text = '' )
                    ).

    mo_toolbar->add_button_group( lt_button ).

*   Register events
    ls_events-eventid = cl_gui_toolbar=>m_id_function_selected.
    ls_events-appl_event = space.
    APPEND ls_events TO lt_events.

    mo_toolbar->set_registered_events( events = lt_events ).
    SET HANDLER me->hnd_toolbar FOR mo_toolbar.


  endmethod.


  method CONSTRUCTOR.


    DATA: lv_text TYPE char100.
    lv_text = i_debugger->mv_prog.

    super->constructor( ).
    mo_viewer = i_debugger.
    m_history = m_varhist =  m_zcode  = '01'.
    m_hist_depth = 9.

    mo_box = create( i_name = lv_text i_width = 1100 i_hight = 300 ).
    SET HANDLER on_box_close FOR mo_box.
    CREATE OBJECT mo_splitter
      EXPORTING
        parent  = mo_box
        rows    = 3
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter->get_container(
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = mo_code_container ).

    mo_splitter->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_toolbar_container ).

    mo_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_tables_container ).

    mo_splitter->set_row_height( id = 1 height = '4' ).
    mo_splitter->set_row_height( id = 2 height = '70' ).

    mo_splitter->set_row_sash( id    = 1
                               type  = 0
                               value = 0 ).

    CREATE OBJECT mo_splitter_code
      EXPORTING
        parent  = mo_code_container
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    mo_splitter_code->get_container(
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = mo_editor_container ).

    mo_splitter_code->get_container(
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = mo_locals_container ).

    mo_splitter_code->set_column_width( EXPORTING id = 1 width = '25' ).

    SET HANDLER on_box_close FOR mo_box.

    CREATE OBJECT mo_toolbar EXPORTING parent = mo_toolbar_container.
    add_toolbar_buttons( ).
    mo_toolbar->set_visible( 'X' ).
    create_code_viewer( ).


  endmethod.


  method CREATE_CODE_VIEWER.


    DATA: lt_events TYPE cntl_simple_events,
          ls_event  TYPE cntl_simple_event.

    CHECK mo_code_viewer IS INITIAL.

    CREATE OBJECT mo_code_viewer
      EXPORTING
        parent           = mo_editor_container
        max_number_chars = 100.

    mo_code_viewer->init_completer( ).
    mo_code_viewer->upload_properties(
      EXCEPTIONS
        dp_error_create  = 1
        dp_error_general = 2
        dp_error_send    = 3
        OTHERS           = 4 ).

    ls_event-eventid    = cl_gui_textedit=>event_double_click.
    APPEND ls_event TO lt_events.

    mo_code_viewer->set_registered_events( lt_events ).
    mo_code_viewer->register_event_border_click( ).
    mo_code_viewer->register_event_break_changed( ).

    SET HANDLER on_editor_double_click FOR mo_code_viewer.
    SET HANDLER on_editor_border_click FOR mo_code_viewer.

    mo_code_viewer->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
    mo_code_viewer->create_document( ).
    mo_code_viewer->set_readonly_mode( 1 ).


  endmethod.


  method HND_TOOLBAR.


    CONSTANTS: c_mask TYPE x VALUE '01'.
    FIELD-SYMBOLS: <fs_any> TYPE any.
    m_debug_button = fcode.
    READ TABLE mt_stack INDEX 1 INTO DATA(ls_stack).
    CASE fcode.

      WHEN 'AI'.

        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY selected = abap_true INTO DATA(ls_prog).
        NEW Zcl_ace_ai( io_source = ls_prog-source io_parent =  mo_viewer->mo_window->mo_box  io_viewer = mo_viewer ).

      WHEN 'DIAGRAM'.
        DATA(lo_mermaid) = NEW zcl_ace_mermaid( io_debugger = mo_viewer iv_type =  'DIAG' ).

      WHEN 'SMART'.
        lo_mermaid = NEW zcl_ace_mermaid( io_debugger = mo_viewer iv_type =  'SMART' ).

*      WHEN 'COVERAGE'.
*        show_coverage( ).
*        mo_viewer->show( ).

      WHEN 'CODE'.
        m_zcode = m_zcode BIT-XOR c_mask.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(ls_source).
        zcl_ace_source_parser=>code_execution_scanner( iv_program = ls_source-include io_debugger = mo_viewer ).
        IF m_zcode IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Z & Standard' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Only Z code' ).
        ENDIF.

      WHEN 'INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.

        l_url = 'https://github.com/ysichov/Smart-Debugger'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.


      WHEN 'STEPS'.

        zcl_ace_appl=>open_int_table( iv_name = 'Steps' it_tab = mo_viewer->mt_steps io_window = mo_viewer->mo_window ).


    ENDCASE.



  endmethod.


  method SET_PROGRAM.


    zcl_ace_source_parser=>parse_tokens( iv_program = iv_program io_debugger = mo_viewer ).

    LOOP AT ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
      CLEAR <prog>-selected.
    ENDLOOP.

    READ TABLE ms_sources-tt_progs WITH KEY include = iv_program ASSIGNING <prog>.
    IF sy-subrc = 0.

      <prog>-selected = abap_true.
      mo_code_viewer->set_text( table = <prog>-source->lines ).
    ENDIF.

  endmethod.


  method SET_PROGRAM_LINE.


    TYPES: lntab TYPE STANDARD TABLE OF i.
    DATA lt_lines TYPE lntab.

    mo_code_viewer->remove_all_marker( 2 ).
    mo_code_viewer->remove_all_marker( 4 ).

*    "session breakpoints
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING
        main_program         = mo_viewer->mo_window->m_prg-include
      IMPORTING
        breakpoints_complete = DATA(lt_points)
      EXCEPTIONS
        c_call_error         = 1
        generate             = 2
        wrong_parameters     = 3
        OTHERS               = 4.

    LOOP AT lt_points INTO DATA(ls_point). "WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<line>).
      <line> = ls_point-line.

      APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
      MOVE-CORRESPONDING ls_point TO <point>.
      <point>-type = 'S'.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lt_lines ).

*    "exernal breakpoints
    CALL METHOD cl_abap_debugger=>read_breakpoints
      EXPORTING
        main_program         = mo_viewer->mo_window->m_prg-include
        flag_other_session   = abap_true
      IMPORTING
        breakpoints_complete = lt_points
      EXCEPTIONS
        c_call_error         = 1
        generate             = 2
        wrong_parameters     = 3
        OTHERS               = 4.

    CLEAR lt_lines.

    LOOP AT lt_points INTO ls_point. "WHERE inclnamesrc = m_prg-include.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
      <line> = ls_point-line.

      APPEND INITIAL LINE TO mt_bpoints ASSIGNING <point>.
      MOVE-CORRESPONDING ls_point TO <point>.
      <point>-type = 'E'.
    ENDLOOP.
    mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lt_lines ).

*    "watchpoints or coverage
*    CLEAR lt_lines.
*    LOOP AT mt_watch INTO DATA(ls_watch).
*      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
*      <line> = ls_watch-line.
*    ENDLOOP.
*
*    "coverage
*    LOOP AT mt_coverage INTO DATA(ls_coverage).
*      APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
*      <line> = ls_coverage-line.
*    ENDLOOP.

    CLEAR lt_lines.
    "blue arrow - current line
    APPEND INITIAL LINE TO lt_lines ASSIGNING <line>.
    <line> = iv_line.
    mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lt_lines ).

    IF iv_line IS NOT INITIAL.
      mo_code_viewer->select_lines( EXPORTING from_line = iv_line to_line = iv_line ).
    ENDIF.

    mo_code_viewer->clear_line_markers( 'S' ).
    mo_code_viewer->draw( ).

  endmethod.


  method SHOW_COVERAGE.


    DATA: lt_split TYPE TABLE OF string.
    CLEAR: mt_watch, mt_coverage,mt_stack.
    LOOP AT mo_viewer->mt_steps INTO DATA(ls_step).

      READ TABLE mt_stack WITH KEY include = ls_step-include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO mt_stack ASSIGNING FIELD-SYMBOL(<stack>).
        MOVE-CORRESPONDING ls_step TO <stack>.

        SPLIT <stack>-program  AT '=' INTO TABLE lt_split.
        <stack>-program = lt_split[ 1 ].
      ENDIF.

      IF ls_step-include <> mo_viewer->mo_window->m_prg-include.
        CONTINUE.
      ENDIF.

      "APPEND INITIAL LINE TO mt_coverage ASSIGNING FIELD-SYMBOL(<fs_coverage>).
      "<fs_coverage>-line = ls_step-line.
    ENDLOOP.

    SORT mt_coverage.
    DELETE ADJACENT DUPLICATES FROM mt_coverage.


  endmethod.


  METHOD show_stack.

    IF mo_salv_stack IS INITIAL.

      cl_salv_table=>factory(
        EXPORTING
          r_container  = mo_tables_container
        IMPORTING
          r_salv_table = mo_salv_stack
        CHANGING
          t_table      = mt_stack ).

      DATA:  lo_column  TYPE REF TO cl_salv_column.

      DATA(lo_columns) = mo_salv_stack->get_columns( ).
      "lo_columns->set_optimize( 'X' ).

      lo_column ?= lo_columns->get_column( 'STEP' ).
      lo_column->set_output_length( '3' ).
      lo_column->set_short_text( 'STEP' ).

      lo_column ?= lo_columns->get_column( 'STACKLEVEL' ).
      lo_column->set_output_length( '5' ).

      lo_column ?= lo_columns->get_column( 'PROGRAM' ).
      lo_column->set_output_length( '20' ).
      lo_column->set_long_text( 'Program/Class' ).
      lo_column->set_medium_text( 'Program/Class' ).

      lo_column ?= lo_columns->get_column( 'INCLUDE' ).
      lo_column->set_output_length( '40' ).

      lo_column ?= lo_columns->get_column( 'EVENTTYPE' ).
      lo_column->set_output_length( '20' ).

      lo_column ?= lo_columns->get_column( 'EVENTNAME' ).
      lo_column->set_output_length( '30' ).

      DATA(lo_event) =  mo_salv_stack->get_event( ).

      SET HANDLER on_stack_double_click FOR lo_event.

      mo_salv_stack->display( ).
    ELSE.
      mo_salv_stack->refresh( ).
    ENDIF.

  ENDMETHOD.


  method ON_EDITOR_BORDER_CLICK.
     DATA: lv_type    TYPE char1.

    IF cntrl_pressed_set IS INITIAL.
      lv_type = 'S'.
    ELSE.
      lv_type = 'E'.
    ENDIF.

    LOOP AT mt_bpoints ASSIGNING FIELD-SYMBOL(<point>) WHERE line = line.
      lv_type = <point>-type.

      CALL FUNCTION 'RS_DELETE_BREAKPOINT'
        EXPORTING
          index        = line
          mainprog     = m_prg-program
          program      = m_prg-include
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
          program      = m_prg-include
          mainprogram  = m_prg-program
          bp_type      = lv_type
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.

    ENDIF.
    DELETE mt_bpoints WHERE del IS NOT INITIAL.
    set_program_line( ).
  endmethod.


  method ON_EDITOR_DOUBLE_CLICK.

     sender->get_selection_pos( IMPORTING from_line = DATA(fr_line) from_pos = DATA(fr_pos) to_line = DATA(to_line) to_pos = DATA(to_pos) ).

  endmethod.


  METHOD on_stack_double_click.

    READ TABLE mo_viewer->mo_window->mt_stack INDEX row INTO DATA(ls_stack).
    "only for coverage stack selection should work.
    "CHECK mo_viewer->mo_window->mt_coverage IS NOT INITIAL.

    MOVE-CORRESPONDING ls_stack TO mo_viewer->mo_window->m_prg.
    MOVE-CORRESPONDING ls_stack TO mo_viewer->ms_stack.

    show_coverage( ).
    mo_viewer->show( ).

  ENDMETHOD.
ENDCLASS.
