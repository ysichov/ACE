CLASS zcl_ace_window DEFINITION
  PUBLIC
  INHERITING FROM zcl_ace_popup
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_table,
        ref      TYPE REF TO data,
        kind(1),
        value    TYPE string,
        typename TYPE abap_abstypename,
        fullname TYPE string,
      END OF ts_table .
    TYPES:
      BEGIN OF ts_calls,
        class TYPE string,
        event TYPE string,
        type  TYPE string,
        name  TYPE string,
        outer TYPE string,
        inner TYPE string,
      END OF ts_calls .
    TYPES:
      tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer .
    TYPES:
      BEGIN OF ts_calls_line,
        program   TYPE string,
        class     TYPE string,
        eventtype TYPE string,
        eventname TYPE string,
        index     TYPE i,
      END OF ts_calls_line .
    TYPES:
      tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY .
    TYPES:
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
    TYPES:
      BEGIN OF ts_calculated_var,
        program TYPE string,
        line    TYPE i,
        name    TYPE string,
      END OF ts_calculated_var .
    TYPES:
      BEGIN OF ts_composed_var,
        program TYPE string,
        line    TYPE i,
        name    TYPE string,
      END OF ts_composed_var .
    TYPES:
      BEGIN OF ts_refvar,
        program TYPE string,
        name    TYPE string,
        class   TYPE string,
      END OF ts_refvar .
    TYPES:
      tt_kword      TYPE STANDARD TABLE OF ts_kword WITH EMPTY KEY .
    TYPES:
      tt_calculated TYPE STANDARD TABLE OF ts_calculated_var WITH EMPTY KEY .
    TYPES:
      tt_composed   TYPE STANDARD TABLE OF ts_composed_var WITH EMPTY KEY .
    TYPES:
      tt_refvar     TYPE STANDARD TABLE OF ts_refvar WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_params,
        class     TYPE string,
        event     TYPE string,
        name      TYPE string,
        param     TYPE string,
        type      TYPE char1,
        preferred TYPE char1,
      END OF ts_params .
    TYPES:
      tt_params TYPE STANDARD TABLE OF ts_params WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_int_tabs,
        eventtype TYPE string,
        eventname TYPE string,
        name      TYPE string,
        type      TYPE string,
      END OF ts_int_tabs .
    TYPES:
      tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_prog,
        include    TYPE program,
        "source     TYPE REF TO cl_ci_source_include,
        source_tab TYPE sci_include,
        scan       TYPE REF TO cl_ci_scan,
        t_keywords TYPE tt_kword,
        selected   TYPE boolean,
      END OF ts_prog .
    TYPES:
      tt_progs TYPE STANDARD TABLE OF ts_prog WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_source,
        tt_progs      TYPE tt_progs,
        t_calculated  TYPE tt_calculated,
        t_composed    TYPE tt_composed,
        t_params      TYPE tt_params,
        tt_tabs       TYPE tt_tabs,
        tt_calls_line TYPE tt_calls_line,
        tt_refvar     TYPE tt_refvar,
      END OF ts_source .
    TYPES:
      BEGIN OF ts_locals,
        program    TYPE tpda_program,
        eventtype  TYPE tpda_event_type,
        eventname  TYPE tpda_event,
        loc_fill   TYPE boolean,
        locals_tab TYPE tpda_scr_locals_it,
        mt_fs      TYPE tpda_scr_locals_it,
      END OF ts_locals .
    TYPES:
      BEGIN OF ts_globals,
        program     TYPE tpda_program,
        glob_fill   TYPE boolean,
        globals_tab TYPE tpda_scr_globals_it,
        mt_fs       TYPE tpda_scr_locals_it,
      END OF ts_globals .
    TYPES:
      BEGIN OF ts_watch,
        program TYPE string,
        line    TYPE i,
      END OF ts_watch .
    TYPES:
      tt_watch TYPE STANDARD  TABLE OF ts_watch WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_bpoint,
        program TYPE string,
        include TYPE string,
        line    TYPE i,
        type    TYPE char1,
        del     TYPE char1,
      END OF ts_bpoint .
    TYPES:
      tt_bpoints TYPE STANDARD TABLE OF ts_bpoint WITH EMPTY KEY .
    TYPES:
      tt_table TYPE STANDARD TABLE OF ts_table
              WITH NON-UNIQUE DEFAULT KEY .

    DATA m_version TYPE x .
    DATA m_history TYPE x .
    DATA m_visualization TYPE x .
    DATA m_varhist TYPE x .
    DATA m_zcode TYPE x .
    DATA m_direction TYPE x .
    DATA m_prg TYPE tpda_scr_prg_info .
    DATA m_debug_button LIKE sy-ucomm .
    DATA m_show_step TYPE boolean .
    DATA mt_bpoints TYPE tt_bpoints .
    DATA mo_viewer TYPE REF TO zcl_ace .
    DATA mo_splitter_code TYPE REF TO cl_gui_splitter_container .
    DATA mo_splitter_var TYPE REF TO cl_gui_splitter_container .
    DATA mo_splitter_steps TYPE REF TO cl_gui_splitter_container .
    DATA mo_toolbar_container TYPE REF TO cl_gui_container .
    DATA mo_importing_container TYPE REF TO cl_gui_container .
    DATA mo_locals_container TYPE REF TO cl_gui_container .
    DATA mo_exporting_container TYPE REF TO cl_gui_container .
    DATA mo_code_container TYPE REF TO cl_gui_container .
    DATA mo_imp_exp_container TYPE REF TO cl_gui_container .
    DATA mo_editor_container TYPE REF TO cl_gui_container .
    DATA mo_steps_container TYPE REF TO cl_gui_container .
    DATA mo_stack_container TYPE REF TO cl_gui_container .
    DATA mo_hist_container TYPE REF TO cl_gui_container .
    DATA mo_code_viewer TYPE REF TO cl_gui_abapedit .
    DATA:
      mt_stack               TYPE TABLE OF zcl_ace_appl=>t_stack .
    DATA mo_toolbar TYPE REF TO cl_gui_toolbar .
    DATA mo_salv_stack TYPE REF TO cl_salv_table .
    DATA mo_salv_steps TYPE REF TO cl_salv_table .
    DATA mo_salv_hist TYPE REF TO cl_salv_table .
    DATA mt_breaks TYPE tpda_bp_persistent_it .
    DATA mt_watch TYPE tt_watch .
    DATA mt_coverage TYPE tt_watch .
    DATA m_hist_depth TYPE i .
    DATA m_start_stack TYPE i .
    DATA:
      mt_source              TYPE STANDARD  TABLE OF ts_source .
    DATA ms_sources TYPE ts_source .
    DATA:
      mt_params              TYPE STANDARD  TABLE OF ts_params .
    DATA:
      mt_locals_set          TYPE STANDARD TABLE OF ts_locals .
    DATA:
      mt_globals_set         TYPE STANDARD TABLE OF ts_globals .

    METHODS constructor
      IMPORTING
        !i_debugger        TYPE REF TO zcl_ace
        !i_additional_name TYPE string OPTIONAL .
    METHODS add_toolbar_buttons .
    METHODS hnd_toolbar
      FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
        !fcode .
    METHODS set_program
      IMPORTING
        !i_include TYPE program .
    METHODS set_program_line
      IMPORTING
        !i_line LIKE sy-index OPTIONAL .
    METHODS create_code_viewer .
    METHODS show_stack .
    METHODS show_coverage .
    METHODS on_stack_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        !column
        !row .
    METHODS on_editor_double_click
      FOR EVENT dblclick OF cl_gui_abapedit
      IMPORTING
        !sender .
    METHODS on_editor_border_click
      FOR EVENT border_click OF cl_gui_abapedit
      IMPORTING
        !cntrl_pressed_set
        !line
        !shift_pressed_set .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_WINDOW IMPLEMENTATION.


  method ADD_TOOLBAR_BUTTONS.


      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.

      button  = VALUE #(
       ( function = 'RUN' icon = CONV #( icon_execute_object ) quickinfo = 'Run report' )
       ( COND #( WHEN mo_viewer->mv_dest IS NOT INITIAL
        THEN VALUE #( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' ) ) )

       ( COND #( WHEN zcl_ace_appl=>is_mermaid_active = abap_true
        THEN VALUE #( function = 'CALLS' icon = CONV #( icon_workflow_process ) quickinfo = ' Calls Flow' text = 'Diagrams' ) ) )
       ( function = 'CODEMIX' icon = CONV #( icon_wizard ) quickinfo = 'Calculations flow sequence' text = 'CodeMix' )
       ( function = 'CODE' icon = CONV #( icon_customer_warehouse ) quickinfo = 'Only Z' text = 'Only Z' )
       "( function = 'COVERAGE' icon = CONV #( icon_wizard ) quickinfo = 'Coverage ' text = 'Coverage' )
       ( butn_type = 3  )
       ( function = 'STEPS' icon = CONV #( icon_next_step ) quickinfo = 'Steps table' text = 'Steps' )
       ( butn_type = 3  )
       ( function = 'INFO' icon = CONV #( icon_bw_gis ) quickinfo = 'Documentation' text = '' )
                      ).

      mo_toolbar->add_button_group( button ).

*   Register events
      event-eventid = cl_gui_toolbar=>m_id_function_selected.
      event-appl_event = space.
      APPEND event TO events.

      mo_toolbar->set_registered_events( events = events ).
      SET HANDLER me->hnd_toolbar FOR mo_toolbar.


  endmethod.


  method CONSTRUCTOR.


      DATA:  text TYPE char100.
      text = i_debugger->mv_prog.

      super->constructor( ).
      mo_viewer = i_debugger.
      m_history = m_varhist =  m_zcode  = '01'.
      m_hist_depth = 9.

      mo_box = create( i_name =  text i_width = 1300 i_hight = 350 ).
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


      DATA: events TYPE cntl_simple_events,
            event  TYPE cntl_simple_event.

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

      event-eventid    = cl_gui_textedit=>event_double_click.
      APPEND event TO events.

      mo_code_viewer->set_registered_events( events ).
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
      FIELD-SYMBOLS: <any> TYPE any.
      m_debug_button = fcode.
      READ TABLE mt_stack INDEX 1 INTO DATA(stack).
      CASE fcode.

        WHEN 'AI'.

          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY selected = abap_true INTO DATA(prog).
          NEW zcl_ace_ai( i_source = prog-source_tab io_parent =  mo_viewer->mo_window->mo_box ).

        WHEN 'RUN'.

          DATA: lt_source TYPE STANDARD TABLE OF text255,
                lv_prog   TYPE progname VALUE 'Z_SMART_DEBUGGER_SCRIPT'.


          READ REPORT lv_prog INTO lt_source.
          IF sy-subrc = 0.
            CALL FUNCTION 'CLPB_EXPORT'
              TABLES
                data_tab   = lt_source
              EXCEPTIONS
                clpb_error = 1
                OTHERS     = 2.

          ENDIF.
          lv_prog = mo_viewer->mv_prog.
          SELECT COUNT(*) INTO @DATA(count) FROM reposrc WHERE progname = @lv_prog AND subc = '1'.

          IF count = 1.
            SUBMIT (lv_prog) VIA SELECTION-SCREEN AND RETURN.
          ENDIF.

        WHEN 'CALLS'.
          DATA(o_mermaid) = NEW zcl_ace_mermaid( io_debugger = mo_viewer i_type =  'CALLS' ).

        WHEN 'CODEMIX'.

          mo_viewer->get_code_mix( ).

*      WHEN 'COVERAGE'.
*        show_coverage( ).
*        mo_viewer->show( ).

        WHEN 'CODE'.
          m_zcode = m_zcode BIT-XOR c_mask.
          CLEAR: mo_viewer->mt_steps, mo_viewer->m_step.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(source).
          zcl_ace_source_parser=>code_execution_scanner( i_program = source-include i_include = source-include io_debugger = mo_viewer ).
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


  method ON_EDITOR_BORDER_CLICK.


      DATA:  type    TYPE char1.

      IF cntrl_pressed_set IS INITIAL.
        type = 'S'.
      ELSE.
        type = 'E'.
      ENDIF.

      LOOP AT mt_bpoints ASSIGNING FIELD-SYMBOL(<point>) WHERE line = line.
        type = <point>-type.

        CALL FUNCTION 'RS_DELETE_BREAKPOINT'
          EXPORTING
            index        = line
            mainprog     = m_prg-program
            program      = m_prg-include
            bp_type      = type
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
            bp_type      = type
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


  method ON_STACK_DOUBLE_CLICK.


      READ TABLE mo_viewer->mo_window->mt_stack INDEX row INTO DATA(stack).
      "only for coverage stack selection should work.
      "CHECK mo_viewer->mo_window->mt_coverage IS NOT INITIAL.

      MOVE-CORRESPONDING stack TO mo_viewer->mo_window->m_prg.
      MOVE-CORRESPONDING stack TO mo_viewer->ms_stack.

      mo_viewer->mo_window->m_prg-program = stack-prg.

      show_coverage( ).
      mo_viewer->show( ).


  endmethod.


  method SET_PROGRAM.


      zcl_ace_source_parser=>parse_tokens( i_include = i_include io_debugger = mo_viewer ).

      LOOP AT ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
        CLEAR <prog>-selected.
      ENDLOOP.

      READ TABLE ms_sources-tt_progs WITH KEY include = i_include ASSIGNING <prog>.
      IF sy-subrc = 0.

        <prog>-selected = abap_true.
*      IF m_prg-line IS INITIAL.
*        m_prg-line = <prog>-t_keywords[ 1 ]-line.
*      ENDIF.
        mo_code_viewer->set_text( table = <prog>-source_tab ).
      ENDIF.

  endmethod.


  method SET_PROGRAM_LINE.


      TYPES: lntab TYPE STANDARD TABLE OF i.
      DATA: lines    TYPE lntab,
            line_num TYPE i.

      mo_code_viewer->remove_all_marker( 2 ).
      mo_code_viewer->remove_all_marker( 4 ).

*    "session breakpoints
      CALL METHOD cl_abap_debugger=>read_breakpoints
        EXPORTING
          main_program         = mo_viewer->mo_window->m_prg-program
        IMPORTING
          breakpoints_complete = DATA(points)
        EXCEPTIONS
          c_call_error         = 1
          generate             = 2
          wrong_parameters     = 3
          OTHERS               = 4.

      LOOP AT points INTO DATA(point) WHERE  include = m_prg-include.
        APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
        <line> = point-line.

        APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
        MOVE-CORRESPONDING point TO <point>.
        <point>-type = 'S'.
      ENDLOOP.
      mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).

*    "exernal breakpoints
      CALL METHOD cl_abap_debugger=>read_breakpoints
        EXPORTING
          main_program         = mo_viewer->mo_window->m_prg-include
          flag_other_session   = abap_true
        IMPORTING
          breakpoints_complete = points
        EXCEPTIONS
          c_call_error         = 1
          generate             = 2
          wrong_parameters     = 3
          OTHERS               = 4.

      CLEAR lines.

      LOOP AT points INTO point. "WHERE inclnamesrc = m_prg-include.
        APPEND INITIAL LINE TO lines ASSIGNING <line>.
        <line> = point-line.

        APPEND INITIAL LINE TO mt_bpoints ASSIGNING <point>.
        MOVE-CORRESPONDING point TO <point>.
        <point>-type = 'E'.
      ENDLOOP.
      mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines ).

*    "watchpoints or coverage
*    CLEAR lines.
*    LOOP AT mt_watch INTO DATA(watch).
*      APPEND INITIAL LINE TO lines ASSIGNING <line>.
*      <line> = watch-line.
*    ENDLOOP.
*
*    "coverage
*    LOOP AT mt_coverage INTO DATA(coverage).
*      APPEND INITIAL LINE TO lines ASSIGNING <line>.
*      <line> = coverage-line.
*    ENDLOOP.

      IF i_line IS NOT INITIAL.

        IF i_line IS NOT INITIAL.
          line_num = i_line.
        ELSE.
          line_num = m_prg-line.
        ENDIF.

        CLEAR lines.
        "blue arrow - current line
        APPEND INITIAL LINE TO lines ASSIGNING <line>.
        <line> = i_line.

        mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lines ).
        mo_code_viewer->select_lines( EXPORTING from_line = i_line to_line = i_line ).
      ENDIF.
      mo_code_viewer->clear_line_markers( 'S' ).
      mo_code_viewer->draw( ).

  endmethod.


  method SHOW_COVERAGE.


      DATA: split TYPE TABLE OF string.
      CLEAR: mt_watch, mt_coverage. "mt_stack.
      CHECK mt_stack IS INITIAL.
      LOOP AT mo_viewer->mt_steps INTO DATA(step).

        READ TABLE mt_stack WITH KEY include = step-include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO mt_stack ASSIGNING FIELD-SYMBOL(<stack>).
          MOVE-CORRESPONDING step TO <stack>.


          SPLIT <stack>-program  AT '=' INTO TABLE split.
          <stack>-prg = <stack>-program.
          <stack>-program = split[ 1 ].
        ENDIF.

        IF step-include <> mo_viewer->mo_window->m_prg-include.
          CONTINUE.
        ENDIF.

        "APPEND INITIAL LINE TO mt_coverage ASSIGNING FIELD-SYMBOL(<coverage>).
        "<coverage>-line = step-line.
      ENDLOOP.

      SORT mt_coverage.
      DELETE ADJACENT DUPLICATES FROM mt_coverage.


  endmethod.


  method SHOW_STACK.


      IF mo_salv_stack IS INITIAL.

        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_tables_container
          IMPORTING
            r_salv_table = mo_salv_stack
          CHANGING
            t_table      = mt_stack ).

        DATA:  o_column  TYPE REF TO cl_salv_column.

        DATA(o_columns) = mo_salv_stack->get_columns( ).
        "o_columns->set_optimize( 'X' ).

        o_column ?= o_columns->get_column( 'STEP' ).
        o_column->set_output_length( '3' ).
        o_column->set_short_text( 'STEP' ).

        o_column ?= o_columns->get_column( 'STACKLEVEL' ).
        o_column->set_output_length( '5' ).

        o_column ?= o_columns->get_column( 'PROGRAM' ).
        o_column->set_output_length( '20' ).
        o_column->set_long_text( 'Program/Class' ).
        o_column->set_medium_text( 'Program/Class' ).

        o_column ?= o_columns->get_column( 'INCLUDE' ).
        o_column->set_output_length( '40' ).

        o_column ?= o_columns->get_column( 'EVENTTYPE' ).
        o_column->set_output_length( '20' ).

        o_column ?= o_columns->get_column( 'EVENTNAME' ).
        o_column->set_output_length( '30' ).

        DATA(o_event) =  mo_salv_stack->get_event( ).

        " Event double click
        SET HANDLER on_stack_double_click FOR o_event.
        mo_salv_stack->display( ).
      ELSE.
        mo_salv_stack->refresh( ).
      ENDIF.


  endmethod.
ENDCLASS.
