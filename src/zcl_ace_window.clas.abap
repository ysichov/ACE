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
               super TYPE boolean,
             END OF ts_calls .
  types:
    tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer .

  types:
    BEGIN OF ts_event,
               program    TYPE program,
               include    TYPE program,
               type       TYPE string,
               stmnt_type TYPE string,
               stmnt_from TYPE string,
               stmnt_to   TYPE string,
               name       TYPE string,
               line       TYPE i,
             END OF ts_event .
  types:
    BEGIN OF ts_var,
               program   TYPE string,
               include   TYPE string,
               line      TYPE i,
               name(100) TYPE c,
               type      TYPE string,
             END OF ts_var .
  types:
    BEGIN OF ts_refvar,
               program TYPE string,
               name    TYPE string,
               class   TYPE string,
             END OF ts_refvar .
  types:
    tt_kword      TYPE STANDARD TABLE OF ZCL_ACE=>TS_KWORD WITH EMPTY KEY .
  types:
    tt_vars       TYPE SORTED TABLE OF ZCL_ACE=>ts_vars WITH UNIQUE KEY program include class eventtype eventname name.

  types:
    tt_calculated TYPE STANDARD TABLE OF ts_var WITH KEY program  include line name .
  types:
    tt_composed   TYPE STANDARD TABLE OF ts_var WITH KEY program  include line name .
  types:
    tt_events     TYPE STANDARD TABLE OF ts_event WITH EMPTY KEY .
  types:
    tt_refvar     TYPE STANDARD TABLE OF ts_refvar WITH EMPTY KEY.
   types:
    tt_params     TYPE SORTED TABLE OF zcl_ace=>ts_params WITH UNIQUE KEY program include class event name param  .

  types:
    BEGIN OF ts_meta,
               clsname    TYPE seoclsname,
               refclsname TYPE seoclsname,
               reltype    TYPE seoreltype,
               node       TYPE salv_de_node_key,
             END OF ts_meta .

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
    BEGIN OF ts_enh_block,
               ev_type     TYPE string,
               ev_name     TYPE string,
               position    TYPE string,
               enh_name    TYPE string,
               enh_include TYPE program,
               enh_id      TYPE i,
               from_line   TYPE i,
               to_line     TYPE i,
             END OF ts_enh_block .
  types:
    tt_enh_blocks TYPE STANDARD TABLE OF ts_enh_block WITH EMPTY KEY .
  types:
    BEGIN OF ts_prog,
               stack         TYPE i,
               program       TYPE program,
               include       TYPE program,
               source_tab    TYPE sci_include,
               v_source      TYPE sci_include,
               v_keywords    TYPE tt_kword,
               scan          TYPE REF TO cl_ci_scan,
               t_keywords    TYPE tt_kword,
               selected      TYPE boolean,
               enh_collected TYPE boolean,
               tt_enh_blocks TYPE tt_enh_blocks,
               evtype        TYPE string,
               evname        TYPE string,
               class         TYPE string,
             END OF ts_prog .
  types:
    BEGIN OF ts_virtual_block,
               source   TYPE sci_include,
               keywords TYPE tt_kword,
             END OF ts_virtual_block .
  types:
    tt_progs   TYPE STANDARD TABLE OF ts_prog WITH EMPTY KEY .
  types:
    tt_classes TYPE STANDARD TABLE OF ts_meta WITH EMPTY KEY .
  types:
    BEGIN OF ts_class_def,
               class   TYPE string,
               super   TYPE string,
               include TYPE program,
               line    TYPE i,
             END OF ts_class_def .
  types:
    tt_class_defs TYPE STANDARD TABLE OF ts_class_def WITH NON-UNIQUE KEY class .
  types:
    BEGIN OF ts_source,
               tt_progs      TYPE tt_progs,
               t_events      TYPE tt_events,
               t_calculated  TYPE tt_calculated,
               t_composed    TYPE tt_composed,
               t_params      TYPE tt_params,
               tt_tabs       TYPE tt_tabs,
               tt_calls_line TYPE ZCL_ACE=>tt_calls_line,
               t_vars        TYPE tt_vars,
               tt_refvar     TYPE tt_refvar,
               t_classes     TYPE tt_classes,
               tt_class_defs TYPE tt_class_defs,
               enh_collected TYPE boolean,
             END OF ts_source .
  types:
    BEGIN OF ts_locals,
               program    TYPE tpda_program,
               eventtype  TYPE tpda_event_type,
               eventname  TYPE tpda_event,
               loc_fill   TYPE boolean,
               locals_tab TYPE tpda_scr_locals_it,
               mt_fs      TYPE tpda_scr_locals_it,
             END OF ts_locals .
  types:
    BEGIN OF ts_globals,
               program     TYPE tpda_program,
               glob_fill   TYPE boolean,
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
  data M_SHOW_STEP type BOOLEAN .
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
    mt_stack               TYPE TABLE OF ZCL_ACE=>T_STACK .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_SALV_STACK type ref to CL_SALV_TABLE .
  data MO_SALV_STEPS type ref to CL_SALV_TABLE .
  data MO_SALV_HIST type ref to CL_SALV_TABLE .
  data MT_BREAKS type TPDA_BP_PERSISTENT_IT .
  data MT_WATCH type TT_WATCH .
  data MT_COVERAGE type TT_WATCH .
  data:
    mt_calls               TYPE TABLE OF ZCL_ACE=>TS_CALL .
  data M_HIST_DEPTH type I .
  data M_START_STACK type I .
  data:
    mt_source              TYPE STANDARD  TABLE OF ts_source .
  data MS_SOURCES type TS_SOURCE .
  data:
    mt_params              TYPE STANDARD  TABLE OF ZCL_ACE=>ts_params .
  data:
    mt_locals_set          TYPE STANDARD TABLE OF ts_locals .
  data:
    mt_globals_set         TYPE STANDARD TABLE OF ts_globals .
  data MS_SEL_CALL type ZCL_ACE=>TS_CALLS_LINE .
  types:
    BEGIN OF ts_code_context,
               evtype TYPE string,
               evname TYPE string,
               class  TYPE string,
             END OF ts_code_context .
  data MS_CODE_CONTEXT type TS_CODE_CONTEXT .
  data MV_NEW_PARSER type ABAP_BOOL .
  data MV_SHOW_PARSE_TIME type ABAP_BOOL .

  methods CONSTRUCTOR
    importing
      !I_DEBUGGER        type ref to ZCL_ACE
      !I_ADDITIONAL_NAME type STRING optional .
  methods ADD_TOOLBAR_BUTTONS .
  methods HND_TOOLBAR
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods SHOW_PARSE_TIME
    importing
      !I_TS1 type TIMESTAMPL .
  methods SET_PROGRAM
    importing
      !I_INCLUDE type PROGRAM .
  methods SET_PROGRAM_LINE
    importing
      !I_LINE like SY-INDEX optional .
  methods SET_MIXPROG_LINE
    importing
      !I_LINE like SY-INDEX optional .
  methods CREATE_CODE_VIEWER .
  methods APPLY_DEPTH .
  methods SHOW_STACK .
  methods SHOW_COVERAGE .
  methods ON_STACK_DOUBLE_CLICK
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



CLASS ZCL_ACE_WINDOW IMPLEMENTATION.


  method ADD_TOOLBAR_BUTTONS.


      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.

      button  = VALUE #(
       ( function = 'RUN' icon = CONV #( icon_execute_object ) quickinfo = 'Run report' )
       ( COND #( WHEN mo_viewer->mv_dest IS NOT INITIAL
        THEN VALUE #( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' ) ) )
       ( COND #( WHEN ZCL_ACE=>I_MERMAID_ACTIVE = abap_true
        THEN VALUE #( function = 'CALLS' icon = CONV #( icon_workflow_process ) quickinfo = ' Calls Flow' text = 'Diagrams' ) ) )
       ( function = 'CODEMIX' icon = CONV #( icon_wizard ) quickinfo = 'Calculations flow sequence' text = 'CodeMix' )
       ( function = 'CODE' icon = CONV #( icon_customer_warehouse ) quickinfo = 'Only Z' text = 'Only Z' )
       ( function = 'DEPTH_M' icon = CONV #( icon_arrow_left ) quickinfo = 'Decrease depth' text = '' )
       ( function = 'DEPTH' icon = CONV #( icon_next_hierarchy_level ) quickinfo = 'History depth level' text = |Depth { m_hist_depth }| )
       ( function = 'DEPTH_P' icon = CONV #( icon_arrow_right ) quickinfo = 'Increase depth' text = '' )
       ( butn_type = 3  )
       ( function = 'STEPS' icon = CONV #( icon_next_step ) quickinfo = 'Steps table' text = 'Steps' )
       ( butn_type = 3  )
       ( function = 'WHOLE_CLASS' icon = CONV #( icon_select_all ) quickinfo = 'Get local class from Global' text = 'Get whole Class' )
       ( function = 'INFO' icon = CONV #( icon_bw_gis ) quickinfo = 'Documentation' text = '' )
                      ).

      mo_toolbar->add_button_group( button ).

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
      m_history = m_varhist = m_zcode = '01'.
      m_hist_depth = 1.

      mo_box = create( i_name = text i_width = 1300 i_hight = 350 ).
      SET HANDLER on_box_close FOR mo_box.
      CREATE OBJECT mo_splitter
        EXPORTING parent = mo_box rows = 3 columns = 1 EXCEPTIONS OTHERS = 1.

      mo_splitter->get_container( EXPORTING row = 2 column = 1 RECEIVING container = mo_code_container ).
      mo_splitter->get_container( EXPORTING row = 1 column = 1 RECEIVING container = mo_toolbar_container ).
      mo_splitter->get_container( EXPORTING row = 3 column = 1 RECEIVING container = mo_tables_container ).

      mo_splitter->set_row_height( id = 1 height = '4' ).
      mo_splitter->set_row_height( id = 2 height = '70' ).
      mo_splitter->set_row_sash( id = 1 type = 0 value = 0 ).

      CREATE OBJECT mo_splitter_code
        EXPORTING parent = mo_code_container rows = 1 columns = 2 EXCEPTIONS OTHERS = 1.

      mo_splitter_code->get_container( EXPORTING row = 1 column = 2 RECEIVING container = mo_editor_container ).
      mo_splitter_code->get_container( EXPORTING row = 1 column = 1 RECEIVING container = mo_locals_container ).
      mo_splitter_code->set_column_width( EXPORTING id = 1 width = '35' ).

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
        EXPORTING parent = mo_editor_container max_number_chars = 100.

      mo_code_viewer->init_completer( ).
      mo_code_viewer->upload_properties(
        EXCEPTIONS dp_error_create = 1 dp_error_general = 2 dp_error_send = 3 OTHERS = 4 ).

      event-eventid = cl_gui_textedit=>event_double_click.
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


  METHOD hnd_toolbar.

    data: lv_evname type string,
          lv_evtype type string,
          lv_class  type string.

    CONSTANTS: c_mask TYPE x VALUE '01'.
    FIELD-SYMBOLS: <any> TYPE any.
    m_debug_button = fcode.
    READ TABLE mt_stack INDEX 1 INTO DATA(stack).
    CASE fcode.

      WHEN 'RUN'.
        DATA: lt_source TYPE STANDARD TABLE OF text255,
              lv_prog   TYPE progname VALUE 'Z_SMART_DEBUGGER_SCRIPT'.

        READ REPORT lv_prog INTO lt_source.
        DELETE lt_source INDEX 2.
        IF sy-subrc = 0.
          CALL FUNCTION 'CLPB_EXPORT'
            TABLES
              data_tab   = lt_source
            EXCEPTIONS
              clpb_error = 1
              OTHERS     = 2.
        ENDIF.
        lv_prog = mo_viewer->mv_prog.
        DATA(lv_count) = 0.
        SELECT COUNT(*) INTO @lv_count FROM reposrc WHERE progname = @lv_prog AND subc = '1'.
        IF lv_count = 1.
          SUBMIT (lv_prog) VIA SELECTION-SCREEN AND RETURN.
        ENDIF.

      WHEN 'DEPTH_M'.
        IF m_hist_depth > 0. m_hist_depth -= 1. ENDIF.
        apply_depth( ).

      WHEN 'DEPTH_P'.
        IF m_hist_depth < 99. m_hist_depth += 1. ENDIF.
        apply_depth( ).

      WHEN 'DEPTH'.
        DATA: lv_answer TYPE c LENGTH 1,
              lv_value1 TYPE spop-varvalue1.

        CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
          EXPORTING
            textline1   = |Current depth: { m_hist_depth }. Enter new value (0-99):|
            titel       = 'Set History Depth'
            valuelength = '2'
          IMPORTING
            answer      = lv_answer
            value1      = lv_value1
          EXCEPTIONS
            OTHERS      = 1.

        IF sy-subrc <> 0 OR lv_answer <> 'J' OR lv_value1 IS INITIAL.
          RETURN.
        ENDIF.

        DATA(lv_new_depth) = CONV i( lv_value1 ).
        IF lv_new_depth < 0.
          lv_new_depth = 0.
        ELSEIF lv_new_depth > 99.
          lv_new_depth = 99.
        ENDIF.
        m_hist_depth = lv_new_depth.
        apply_depth( ).

      WHEN 'CALLS'.
        IF mo_mermaid IS INITIAL OR mo_mermaid->mo_box IS INITIAL.
          mo_mermaid = NEW zcl_ace_mermaid( io_debugger = mo_viewer i_type = 'CALLS' ).
        ENDIF.

      WHEN 'CODEMIX'.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
        DATA(ls_ctx) = mo_viewer->mo_window->ms_code_context.
        IF ls_ctx-evtype IS NOT INITIAL.
          " Context known from dblclick — use PARSE_CALL to start from method/form entry point
          DATA(ls_sc) = mo_viewer->mo_window->ms_sel_call.
          zcl_ace_source_parser=>parse_call(
            i_index     = ls_sc-index
            i_e_name    = ls_ctx-evname
            i_e_type    = ls_ctx-evtype
            i_class     = ls_ctx-class
            i_program   = CONV #( ls_sc-program )
            i_include   = CONV #( ls_sc-include )
            i_stack     = 0
            io_debugger = mo_viewer ).
        ELSE.
          " No context — full program scan
          zcl_ace_source_parser=>code_execution_scanner(
            i_program   = mo_viewer->mo_window->m_prg-program
            i_include   = mo_viewer->mo_window->m_prg-program
            io_debugger = mo_viewer ).
        ENDIF.

        mo_viewer->get_code_mix( ).
        mo_viewer->mo_window->show_stack( ).

      WHEN 'CODE'.
        m_zcode = m_zcode BIT-XOR c_mask.
        CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, mo_viewer->mo_window->mt_calls.
        DATA(ls_ctx_code) = mo_viewer->mo_window->ms_code_context.
        zcl_ace_source_parser=>code_execution_scanner(
          i_program   = mo_viewer->mo_window->m_prg-program
          i_include   = mo_viewer->mo_window->m_prg-program
          io_debugger = mo_viewer
          i_evname    = ls_ctx_code-evname
          i_evtype    = ls_ctx_code-evtype
          i_class     = ls_ctx_code-class ).
        IF m_zcode IS INITIAL.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Z & Standard' ).
        ELSE.
          mo_toolbar->set_button_info( EXPORTING fcode = 'CODE' text = 'Only Z code' ).
        ENDIF.
        mo_viewer->mo_window->show_stack( ).
        IF mo_mermaid IS NOT INITIAL. mo_mermaid->refresh( ). ENDIF.
        IF mo_viewer->mo_window->m_prg-include = 'Code_Flow_Mix'.
          mo_viewer->get_code_mix( ).
          mo_viewer->mo_window->show_stack( ).
        ENDIF.

      WHEN 'INFO'.
        DATA(l_url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
        l_url = 'https://github.com/ysichov/Smart-Debugger'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.

      WHEN 'STEPS'.
        zcl_ace=>open_int_table(
          i_name = 'Steps' it_tab = mo_viewer->mt_steps io_window = mo_viewer->mo_window ).

      WHEN 'WHOLE_CLASS'.
        DATA: lt_whole_class  TYPE sci_include,
              lv_class_name   TYPE string,
              lv_in_methods   TYPE boolean VALUE abap_false,
              lv_section_done TYPE boolean VALUE abap_false,
              lv_cu_first     TYPE boolean VALUE abap_true.

        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(ls_wc_prog).

        " First: add CCMAC (macros) at the very top
        LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(ls_mac)
          WHERE program = ls_wc_prog-program.
          CHECK to_upper( CONV string( ls_mac-include ) ) CP '*CCMAC'.
          CHECK ls_mac-source_tab IS NOT INITIAL.
          APPEND LINES OF ls_mac-source_tab TO lt_whole_class.
          APPEND INITIAL LINE TO lt_whole_class.
        ENDLOOP.

        LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(ls_prog_wc)
          WHERE program = ls_wc_prog-program.

          DATA(lv_include_raw) = ls_prog_wc-include.
          DATA(lv_include)     = to_upper( CONV string( lv_include_raw ) ).
          DATA(lv_is_cp)     = xsdbool( lv_include CP '*CP' ).
          DATA(lv_is_cu)     = xsdbool( lv_include CP '*CU' ).
          DATA(lv_is_method) = xsdbool( lv_include CP '*CM*' ).

          " Skip CP, enhancement and CC* includes (added at the end separately)
          IF lv_is_cp = abap_true OR lv_include CP '*====E' OR lv_include CS 'EIMP'
            OR lv_include CP '*CCMAC' OR lv_include CP '*CCIMP' OR lv_include CP '*CCAU'.
            CONTINUE.
          ENDIF.

          CHECK ls_prog_wc-source_tab IS NOT INITIAL.

          " Transition: after sections, before first method
          IF lv_is_method = abap_true AND lv_in_methods = abap_false.
            lv_in_methods = abap_true.
            APPEND INITIAL LINE TO lt_whole_class.
            APPEND |ENDCLASS.| TO lt_whole_class.
            APPEND INITIAL LINE TO lt_whole_class.
            APPEND |CLASS { lv_class_name } IMPLEMENTATION.| TO lt_whole_class.
            APPEND INITIAL LINE TO lt_whole_class.
            lv_section_done = abap_false.
          ENDIF.

          " Blank separator between includes
          IF lv_section_done = abap_true.
            APPEND INITIAL LINE TO lt_whole_class.
          ENDIF.
          lv_section_done = abap_true.

          " CU include: extract class name for caption, then append lines as-is
          IF lv_is_cu = abap_true AND lv_cu_first = abap_true.
            lv_cu_first = abap_false.
            DATA(lv_eq_pos) = find( val = ls_prog_wc-include sub = '=' ).
            IF lv_eq_pos > 0.
              lv_class_name = substring( val = ls_prog_wc-include len = lv_eq_pos ).
            ENDIF.
          ENDIF.

          APPEND LINES OF ls_prog_wc-source_tab TO lt_whole_class.
        ENDLOOP.

        " Close final block
        APPEND INITIAL LINE TO lt_whole_class.
        APPEND |ENDCLASS.| TO lt_whole_class.

        " Append CC* includes at the end: macros, local class impl, test classes
        LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(ls_cc)
          WHERE program = ls_wc_prog-program.
          DATA(lv_cc_inc) = to_upper( CONV string( ls_cc-include ) ).
          CHECK lv_cc_inc CP '*CCIMP' OR lv_cc_inc CP '*CCAU'.
          CHECK ls_cc-source_tab IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_whole_class.
          APPEND LINES OF ls_cc-source_tab TO lt_whole_class.
        ENDLOOP.

        mo_code_viewer->set_text( table = lt_whole_class ).
        mo_box->set_caption( |Whole Class: { lv_class_name }| ).

    ENDCASE.


  ENDMETHOD.


  method ON_EDITOR_BORDER_CLICK.


      DATA: type      TYPE char1,
            program   TYPE program,
            include   TYPE program,
            code_line TYPE i.

      IF cntrl_pressed_set IS INITIAL.
        type = 'S'.
      ELSE.
        type = 'E'.
      ENDIF.

      IF m_prg-include = 'Code_Flow_Mix'.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = 'Code_Flow_Mix' INTO DATA(prog_mix).
        READ TABLE prog_mix-t_keywords WITH KEY v_line = line INTO DATA(keyword).
      ELSE.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = m_prg-include INTO prog_mix.
        IF prog_mix-v_keywords IS NOT INITIAL.
          LOOP AT prog_mix-v_keywords INTO keyword WHERE v_line = line. EXIT. ENDLOOP.
        ELSE.
          LOOP AT prog_mix-t_keywords INTO keyword WHERE v_line = line. EXIT. ENDLOOP.
        ENDIF.
      ENDIF.

      CHECK keyword-include IS NOT INITIAL.
      program   = m_prg-program.
      include   = keyword-include.
      code_line = keyword-line.
      IF include IS INITIAL.
        program = m_prg-program. include = m_prg-include. code_line = line.
      ENDIF.

      LOOP AT mt_bpoints ASSIGNING FIELD-SYMBOL(<point>) WHERE line = code_line AND include = include.
        type = <point>-type.
        CALL FUNCTION 'RS_DELETE_BREAKPOINT'
          EXPORTING index = code_line mainprog = program program = include bp_type = type
          EXCEPTIONS not_executed = 1 OTHERS = 2.
        IF sy-subrc = 0. <point>-del = abap_true. ENDIF.
      ENDLOOP.

      IF sy-subrc <> 0.
        CALL FUNCTION 'RS_SET_BREAKPOINT'
          EXPORTING index = code_line program = include mainprogram = program bp_type = type
          EXCEPTIONS not_executed = 1 OTHERS = 2.
      ENDIF.
      DELETE mt_bpoints WHERE del IS NOT INITIAL.

      IF m_prg-include = 'Code_Flow_Mix'.
        set_mixprog_line( ).
      ELSE.
        set_program_line( ).
      ENDIF.


  endmethod.


  method ON_EDITOR_DOUBLE_CLICK.

      sender->get_selection_pos(
        IMPORTING from_line = DATA(fr_line) from_pos = DATA(fr_pos)
                  to_line   = DATA(to_line)   to_pos = DATA(to_pos) ).

  endmethod.


  method ON_STACK_DOUBLE_CLICK.


      READ TABLE mo_viewer->mo_window->mt_stack INDEX row INTO DATA(stack).
      MOVE-CORRESPONDING stack TO mo_viewer->mo_window->m_prg.
      MOVE-CORRESPONDING stack TO mo_viewer->ms_stack.
      mo_viewer->mo_window->m_prg-program = stack-prg.
      mo_viewer->show( ).
      CASE stack-eventtype.
        WHEN 'FUNCTION'.
          mo_viewer->mo_window->mo_box->set_caption( |FM: { stack-eventname }| ).
        WHEN OTHERS.
          mo_viewer->mo_window->mo_box->set_caption( |{ stack-program } : { stack-eventname }| ).
      ENDCASE.

  endmethod.


  method SET_MIXPROG_LINE.


      TYPES: lntab TYPE STANDARD TABLE OF i.
      DATA: lines    TYPE lntab,
            line_num TYPE i,
            flag     TYPE boolean,
            programs TYPE TABLE OF program.

      mo_code_viewer->remove_all_marker( 2 ).
      mo_code_viewer->remove_all_marker( 4 ).

      LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO DATA(prog)
        WHERE include <> 'Code_Flow_Mix'.
        COLLECT prog-program INTO programs.
      ENDLOOP.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
        WITH KEY include = 'Code_Flow_Mix' INTO prog.

      flag = abap_true.
      DO 2 TIMES.
        LOOP AT programs INTO DATA(program).
          CALL METHOD cl_abap_debugger=>read_breakpoints
            EXPORTING main_program = program flag_other_session = flag
            IMPORTING breakpoints_complete = DATA(points)
            EXCEPTIONS c_call_error = 1 generate = 2 wrong_parameters = 3 OTHERS = 4.

          LOOP AT points INTO DATA(point).
            CLEAR lines.
            READ TABLE prog-t_keywords WITH KEY include = point-include line = point-line
              INTO DATA(keyword).
            IF sy-subrc = 0.
              APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
              <line> = keyword-v_line.
              APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
              MOVE-CORRESPONDING point TO <point>.
              IF flag IS INITIAL. <point>-type = 'S'. ELSE. <point>-type = 'E'. ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        IF flag IS NOT INITIAL.
          mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines ).
        ELSE.
          mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).
        ENDIF.
        CLEAR flag.
      ENDDO.

      IF i_line IS NOT INITIAL.
        CLEAR lines.
        APPEND INITIAL LINE TO lines ASSIGNING <line>.
        <line> = i_line.
        mo_code_viewer->set_marker( EXPORTING marker_number = 7 marker_lines = lines ).
        mo_code_viewer->select_lines( EXPORTING from_line = i_line to_line = i_line ).
      ENDIF.
      mo_code_viewer->clear_line_markers( 'S' ).
      mo_code_viewer->draw( ).


  endmethod.


  METHOD show_parse_time.
    DATA: lv_ts2 TYPE timestampl,
          lv_sec TYPE tzntstmpl,
          lv_str(20) TYPE c.
    GET TIME STAMP FIELD lv_ts2.
    CALL METHOD cl_abap_tstmp=>subtract
      EXPORTING tstmp1 = lv_ts2 tstmp2 = i_ts1
      RECEIVING r_secs = lv_sec.
    WRITE lv_sec TO lv_str LEFT-JUSTIFIED DECIMALS 3.
    CONDENSE lv_str NO-GAPS.
    MESSAGE |parse_tokens: { lv_str } sec| TYPE 'I'.
  ENDMETHOD.


  METHOD set_program.

    IF i_include = 'VIRTUAL'.
      LOOP AT ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<sp_virt>).
        CLEAR <sp_virt>-selected.
      ENDLOOP.
      READ TABLE ms_sources-tt_progs WITH KEY include = 'VIRTUAL' ASSIGNING <sp_virt>.
      IF sy-subrc = 0.
        <sp_virt>-selected = abap_true.
        mo_code_viewer->set_text( table = <sp_virt>-source_tab ).
      ENDIF.
      RETURN.
    ENDIF.

    DATA lv_ts1 TYPE timestampl.
    IF mo_viewer->mv_show_parse_time = abap_true.
      GET TIME STAMP FIELD lv_ts1.
    ENDIF.

    zcl_ace_source_parser=>parse_tokens(
      i_main = abap_true i_program = i_include i_include = i_include io_debugger = mo_viewer ).

    IF mo_viewer->mv_show_parse_time = abap_true.
      show_parse_time( lv_ts1 ).
    ENDIF.

    IF mo_viewer->m_step IS INITIAL.
      DATA(ls_ctx) = ms_code_context.
      zcl_ace_source_parser=>code_execution_scanner(
        i_program   = i_include
        i_include   = i_include
        io_debugger = mo_viewer
        i_evtype    = ls_ctx-evtype
        i_evname    = ls_ctx-evname
        i_class     = ls_ctx-class ).
    ENDIF.

    LOOP AT ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<sp_prog>).
      CLEAR <sp_prog>-selected.
    ENDLOOP.

    READ TABLE ms_sources-tt_progs WITH KEY include = i_include ASSIGNING <sp_prog>.
    IF sy-subrc = 0.
      <sp_prog>-selected = abap_true.
      IF <sp_prog>-v_source IS NOT INITIAL.
        mo_code_viewer->set_text( table = <sp_prog>-v_source ).
      ELSE.
        mo_code_viewer->set_text( table = <sp_prog>-source_tab ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method SET_PROGRAM_LINE.


      TYPES: lntab TYPE STANDARD TABLE OF i.
      DATA: lines    TYPE lntab,
            line_num TYPE i.

      mo_code_viewer->remove_all_marker( 2 ).
      mo_code_viewer->remove_all_marker( 4 ).
      mo_code_viewer->remove_all_marker( 7 ).
      CLEAR mt_bpoints.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
        WITH KEY include = m_prg-include INTO DATA(prog_cur).

      DATA(lr_kw) = REF #( prog_cur-t_keywords ).
      IF prog_cur-v_keywords IS NOT INITIAL.
        lr_kw = REF #( prog_cur-v_keywords ).
      ENDIF.

      DATA lt_includes TYPE STANDARD TABLE OF program WITH EMPTY KEY.
      DATA lv_inc      TYPE program.
      LOOP AT lr_kw->* INTO DATA(lv_kw_inc).
        lv_inc = lv_kw_inc-include.
        IF lv_inc IS NOT INITIAL.
          READ TABLE lt_includes WITH KEY table_line = lv_inc TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0. APPEND lv_inc TO lt_includes. ENDIF.
        ENDIF.
      ENDLOOP.
      IF lt_includes IS INITIAL.
        lv_inc = m_prg-include. APPEND lv_inc TO lt_includes.
      ENDIF.

      CALL METHOD cl_abap_debugger=>read_breakpoints
        EXPORTING main_program = mo_viewer->mo_window->m_prg-program
        IMPORTING breakpoints_complete = DATA(points)
        EXCEPTIONS c_call_error = 1 generate = 2 wrong_parameters = 3 OTHERS = 4.

      LOOP AT points INTO DATA(point).
        READ TABLE lt_includes WITH KEY table_line = point-include TRANSPORTING NO FIELDS.
        CHECK sy-subrc = 0.
        LOOP AT lr_kw->* INTO DATA(bp_kw)
          WHERE include = point-include AND line = point-line. EXIT. ENDLOOP.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO lines ASSIGNING FIELD-SYMBOL(<line>).
          <line> = bp_kw-v_line.
          READ TABLE mt_bpoints TRANSPORTING NO FIELDS
            WITH KEY include = point-include line = point-line.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO mt_bpoints ASSIGNING FIELD-SYMBOL(<point>).
            MOVE-CORRESPONDING point TO <point>.
            <point>-type = 'S'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      mo_code_viewer->set_marker( EXPORTING marker_number = 2 marker_lines = lines ).

      CLEAR lines.
      LOOP AT lt_includes INTO lv_inc.
        CALL METHOD cl_abap_debugger=>read_breakpoints
          EXPORTING main_program = lv_inc flag_other_session = abap_true
          IMPORTING breakpoints_complete = points
          EXCEPTIONS c_call_error = 1 generate = 2 wrong_parameters = 3 OTHERS = 4.

        LOOP AT points INTO point WHERE include = lv_inc.
          LOOP AT lr_kw->* INTO bp_kw
            WHERE include = point-include AND line = point-line. EXIT. ENDLOOP.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO lines ASSIGNING <line>.
            <line> = bp_kw-v_line.
            READ TABLE mt_bpoints TRANSPORTING NO FIELDS
              WITH KEY include = point-include line = point-line.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO mt_bpoints ASSIGNING <point>.
              MOVE-CORRESPONDING point TO <point>.
              <point>-type = 'E'.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      mo_code_viewer->set_marker( EXPORTING marker_number = 4 marker_lines = lines ).

      IF i_line IS NOT INITIAL.
        CLEAR lines.
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
      CLEAR: mt_watch, mt_coverage.
      LOOP AT mo_viewer->mt_steps INTO DATA(step).
        READ TABLE mt_stack WITH KEY include = step-include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO mt_stack ASSIGNING FIELD-SYMBOL(<stack>).
          MOVE-CORRESPONDING step TO <stack>.
          SPLIT <stack>-program AT '=' INTO TABLE split.
          <stack>-prg = <stack>-program.
          <stack>-program = split[ 1 ].
        ENDIF.
        IF step-include <> mo_viewer->mo_window->m_prg-include. CONTINUE. ENDIF.
      ENDLOOP.

      IF mt_stack IS INITIAL.
        SORT ms_sources-tt_progs BY stack.
        LOOP AT ms_sources-tt_progs INTO DATA(prog).
          CHECK prog-t_keywords IS NOT INITIAL.
          APPEND INITIAL LINE TO mt_stack ASSIGNING <stack>.
          MOVE-CORRESPONDING prog TO <stack>.
          SPLIT <stack>-program AT '=' INTO TABLE split.
          <stack>-prg = <stack>-program.
          <stack>-program = split[ 1 ].
          <stack>-stacklevel = prog-stack.

          DATA(pos) = strlen( <stack>-program ).
          pos = pos - 2.
          IF pos > 0.
            DATA(incl) = <stack>-include+pos(2).
            SELECT SINGLE funcname INTO @<stack>-eventname FROM tfdir
              WHERE pname_main = @<stack>-program AND include = @incl.
            IF sy-subrc = 0. <stack>-eventtype = 'FUNCTION'. CONTINUE. ENDIF.
          ENDIF.

          DATA: cl_key        TYPE seoclskey,
                meth_includes TYPE seop_methods_w_include.
          cl_key = <stack>-program.
          CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
            EXPORTING clskey = cl_key
            IMPORTING includes = meth_includes
            EXCEPTIONS _internal_class_not_existing = 1 OTHERS = 2.
          IF sy-subrc = 0.
            READ TABLE meth_includes[] WITH KEY incname = <stack>-include INTO DATA(include).
            IF sy-subrc = 0.
              <stack>-eventtype = 'METHOD'.
              <stack>-eventname = include-cpdkey-cpdname.
            ENDIF.
          ENDIF.

          SPLIT <stack>-include AT '=' INTO TABLE split.
          CASE split[ lines( split ) ].
            WHEN 'CP'.   <stack>-eventtype = 'Class Pool'.
            WHEN 'CU'.   <stack>-eventtype = 'Public Section'.
            WHEN 'CI'.   <stack>-eventtype = 'Private Section'.
            WHEN 'CO'.   <stack>-eventtype = 'Protected Section'.
            WHEN 'IU'.   <stack>-eventtype = 'Interface Public Section'.
            WHEN 'CCAU'. <stack>-eventtype = 'Unit Test Classes'.
            WHEN 'CCIMP'.<stack>-eventtype = 'Local helper classes'.
            WHEN 'CCDEF'.<stack>-eventtype = 'Local Definitions/Implementations'.
            WHEN 'CCMAC'.<stack>-eventtype = 'Macros'.
          ENDCASE.
        ENDLOOP.
      ENDIF.

      SORT mt_coverage.
      DELETE ADJACENT DUPLICATES FROM mt_coverage.


  endmethod.


  method APPLY_DEPTH.

      CLEAR: mo_viewer->mt_steps, mo_viewer->m_step,
             mo_viewer->mo_window->mt_stack, mo_viewer->mo_window->mt_calls.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
        WITH KEY selected = abap_true INTO DATA(source).
      IF sy-subrc <> 0 OR source-include = 'Code_Flow_Mix' OR source-include = 'VIRTUAL'.
        LOOP AT mo_viewer->mo_window->ms_sources-tt_progs INTO source
          WHERE include <> 'Code_Flow_Mix' AND include <> 'VIRTUAL'.
          EXIT.
        ENDLOOP.
      ENDIF.

      CLEAR: mo_viewer->mt_steps, mo_viewer->m_step,
             mo_viewer->mo_window->mt_stack, mo_viewer->mo_window->mt_calls.

      DATA(ls_ctx) = mo_viewer->mo_window->ms_code_context.
      IF ls_ctx-evtype IS NOT INITIAL.
        DATA(ls_sc) = mo_viewer->mo_window->ms_sel_call.
        zcl_ace_source_parser=>parse_call(
          i_index     = ls_sc-index
          i_e_name    = ls_ctx-evname
          i_e_type    = ls_ctx-evtype
          i_class     = ls_ctx-class
          i_program   = CONV #( ls_sc-program )
          i_include   = CONV #( ls_sc-include )
          i_stack     = 0
          io_debugger = mo_viewer ).
      ELSE.
        zcl_ace_source_parser=>code_execution_scanner(
          i_program   = mo_viewer->mo_window->m_prg-program
          i_include   = mo_viewer->mo_window->m_prg-program
          io_debugger = mo_viewer ).
      ENDIF.

      mo_viewer->mo_window->show_coverage( ).
      mo_viewer->mo_window->show_stack( ).
      IF mo_mermaid IS NOT INITIAL. mo_mermaid->refresh( ). ENDIF.
      mo_toolbar->set_button_info( EXPORTING fcode = 'DEPTH' text = |Depth { m_hist_depth }| ).
      IF mo_viewer->mo_window->m_prg-include = 'Code_Flow_Mix'.
        mo_viewer->get_code_mix( ).
        mo_viewer->mo_window->show_stack( ).
      ENDIF.

  endmethod.


  method SHOW_STACK.


      IF mo_salv_stack IS INITIAL.

        cl_salv_table=>factory(
          EXPORTING r_container = mo_tables_container
          IMPORTING r_salv_table = mo_salv_stack
          CHANGING  t_table = mt_stack ).

        DATA: o_column TYPE REF TO cl_salv_column.
        DATA(o_columns) = mo_salv_stack->get_columns( ).

        o_column ?= o_columns->get_column( 'STEP' ).
        o_column->set_output_length( '3' ). o_column->set_short_text( 'STEP' ).
        o_column ?= o_columns->get_column( 'STACKLEVEL' ).
        o_column->set_output_length( '5' ).
        o_column ?= o_columns->get_column( 'PROGRAM' ).
        o_column->set_output_length( '20' ).
        o_column->set_long_text( 'Program/Class' ). o_column->set_medium_text( 'Program/Class' ).
        o_column ?= o_columns->get_column( 'EVENTTYPE' ).
        o_column->set_output_length( '10' ).
        o_column->set_long_text( 'Code TYPE' ). o_column->set_medium_text( 'Code TYPE' ).
        o_column ?= o_columns->get_column( 'INCLUDE' ).
        o_column->set_output_length( '40' ).
        o_column ?= o_columns->get_column( 'EVENTTYPE' ).
        o_column->set_output_length( '20' ).
        o_column ?= o_columns->get_column( 'EVENTNAME' ).
        o_column->set_output_length( '30' ).

        DATA(o_event) = mo_salv_stack->get_event( ).
        SET HANDLER on_stack_double_click FOR o_event.
        mo_salv_stack->display( ).
      ELSE.
        mo_salv_stack->refresh( ).
      ENDIF.


  endmethod.
ENDCLASS.
