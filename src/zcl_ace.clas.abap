class ZCL_ACE definition
  public
  create public .

public section.

  types:
    BEGIN OF selection_display_s,
        ind         TYPE i,
        field_label TYPE lvc_fname,
        int_type(1),
        inherited   TYPE aqadh_type_of_icon,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string,
        high        TYPE string,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
        name        TYPE reptext,
        element     TYPE text60,
        domain      TYPE text60,
        datatype    TYPE string,
        length      TYPE i,
        color       TYPE lvc_t_scol,
        style       TYPE lvc_t_styl,
      END OF selection_display_s .
  types:
    BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string,
        high        TYPE string,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row .
  types:
    BEGIN OF sign_option_icon_s,
        sign          TYPE tvarv_sign,
        option        TYPE tvarv_opti,
        icon_name(64) TYPE c,
        icon          TYPE aqadh_type_of_icon,
      END OF sign_option_icon_s .
  types:
    BEGIN OF t_obj,
        name       TYPE string,
        alv_viewer TYPE REF TO zcl_ace_table_viewer,
      END OF t_obj .
  types:
    BEGIN OF t_popup,
        parent TYPE REF TO cl_gui_dialogbox_container,
        child  TYPE REF TO cl_gui_dialogbox_container,
      END OF t_popup .
  types:
    BEGIN OF t_classes_types,
        name TYPE string,
        full TYPE string,
        type TYPE char1,
        key  TYPE salv_de_node_key,
      END OF t_classes_types .
  types:
    BEGIN OF t_lang,
        spras(4),
        sptxt    TYPE sptxt,
      END OF t_lang .
  types:
    BEGIN OF t_stack,
        step       TYPE i,
        stacklevel TYPE tpda_stack_level,
        line       TYPE tpda_sc_line,
        program    TYPE tpda_program,
        eventtype  TYPE string,
        eventname  TYPE tpda_event,
        prg        TYPE program,
        include    TYPE tpda_include,
      END OF t_stack .
  types:
    BEGIN OF t_step_counter,
        step       TYPE i,
        stacklevel TYPE tpda_stack_level,
        line       TYPE tpda_sc_line,
        eventtype  TYPE string,
        eventname  TYPE string,
        class      TYPE string,
        first      TYPE boolean,
        last       TYPE boolean,
        program    TYPE tpda_program,
        include    TYPE tpda_include,
        time       LIKE sy-uzeit,
      END OF t_step_counter .
  types:
    " Aligned with zif_ace_parse_data=>ts_param_binding (dir added)
    BEGIN OF ts_param_binding,
        outer TYPE string,
        inner TYPE string,
        dir   TYPE char1,
      END OF ts_param_binding .
  types:
    tt_param_bindings TYPE STANDARD TABLE OF ts_param_binding WITH EMPTY KEY .
  types TS_CALLS type ZIF_ACE_PARSE_DATA=>TS_CALLS .
  types TT_CALLS type ZIF_ACE_PARSE_DATA=>TT_CALLS .
  types TS_KWORD type ZIF_ACE_PARSE_DATA=>TS_KWORD .
  types TT_KWORD type ZIF_ACE_PARSE_DATA=>TT_KWORD .
  types TS_CALLS_LINE type ZIF_ACE_PARSE_DATA=>TS_CALLS_LINE .
  types TT_CALLS_LINE type ZIF_ACE_PARSE_DATA=>TT_CALLS_LINE .
  types TS_VARS type ZIF_ACE_PARSE_DATA=>TS_VARS .
  types:
    BEGIN OF ts_var,
        program   TYPE string,
        include   TYPE string,
        class     TYPE string,
        eventtype TYPE string,
        eventname TYPE string,
        line      TYPE i,
        name(100) TYPE c,
        type      TYPE string,
      END OF ts_var .
  types:
    tt_calculated TYPE STANDARD TABLE OF ts_var WITH KEY program include class eventtype eventname line name .
  types:
    tt_composed   TYPE STANDARD TABLE OF ts_var WITH KEY program include class eventtype eventname line name .
  types:
    BEGIN OF ts_int_tabs,
        eventtype TYPE string,
        eventname TYPE string,
        name      TYPE string,
        type      TYPE string,
      END OF ts_int_tabs .
  types:
    tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY .
  types TS_PARAMS type ZIF_ACE_PARSE_DATA=>TS_PARAMS .
  types:
    BEGIN OF ts_parse_state,
        prev            TYPE string,
        change          TYPE string,
        kw              TYPE string,
        word            TYPE string,
        new             TYPE boolean,
        count           TYPE i,
        lv_default      TYPE boolean,
        ref             TYPE boolean,
        class           TYPE boolean,
        preferred       TYPE boolean,
        method_type     TYPE i,
        class_name      TYPE string,
        eventtype       TYPE string,
        eventname       TYPE string,
        token           TYPE ts_kword,
        call            TYPE ts_calls,
        call_line       TYPE ts_calls_line,
        variable        TYPE ts_vars,
        tab             TYPE ts_int_tabs,
        tabs            TYPE tt_tabs,
        composed        TYPE ts_var,
        composed_vars   TYPE tt_composed,
        calculated      TYPE ts_var,
        calculated_vars TYPE tt_calculated,
        param           TYPE ts_params,
      END OF ts_parse_state .
  types:
    BEGIN OF ts_tree,
        kind(1),
        value    TYPE string,
        param    TYPE string,
        program  TYPE program,
        include  TYPE program,
        ev_type  TYPE string,
        ev_name  TYPE string,
        enh_id   TYPE i,
        var_name TYPE string,
      END OF ts_tree .
  types:
    BEGIN OF ts_call,
        include TYPE string,
        ev_name TYPE string,
        class   TYPE string,
      END OF ts_call .
  types:
    BEGIN OF t_sel_var,
        name   TYPE string,
        i_sel  TYPE boolean,
        refval TYPE REF TO data,
      END OF t_sel_var .
  types:
    BEGIN OF ts_if,
        if_ind      TYPE i,
        end_ind     TYPE i,
        before_else TYPE i,
      END OF ts_if .
  types:
    tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY .
  types:
    BEGIN OF ts_line,
        cond        TYPE string,
        program     TYPE string,
        include     TYPE string,
        line        TYPE i,
        ind         TYPE i,
        class       TYPE string,
        ev_name     TYPE string,
        ev_type     TYPE string,
        stack       TYPE i,
        code        TYPE string,
        arrow       TYPE string,
        subname     TYPE string,
        del         TYPE flag,
        els_before  TYPE i,
        els_after   TYPE i,
        active_root TYPE flag,
      END OF ts_line .
  types:
    tt_line TYPE TABLE OF ts_line WITH EMPTY KEY .

  class-data:
    m_option_icons   TYPE TABLE OF sign_option_icon_s .
  class-data:
    mt_lang          TYPE TABLE OF t_lang .
  class-data:
    mt_obj           TYPE TABLE OF t_obj .
  class-data:
    mt_popups        TYPE TABLE OF t_popup .
  class-data I_MERMAID_ACTIVE type BOOLEAN .
  class-data:
    mt_sel TYPE TABLE OF selection_display_s .
  data MV_PROG type PROG .
  data MV_SHOW_PROG type PROG .
  data MV_SHOW_PARSE_TIME type ABAP_BOOL .
  data:
    mt_compo          TYPE TABLE OF scompo .
  data MT_LOCALS type TPDA_SCR_LOCALS_IT .
  data MT_GLOBALS type TPDA_SCR_GLOBALS_IT .
  data MT_RET_EXP type TPDA_SCR_LOCALS_IT .
  data M_COUNTER type I .
  data:
    mt_steps          TYPE  TABLE OF zcl_ace=>t_step_counter WITH NON-UNIQUE KEY program include line eventtype eventname .
  data M_STEP type I .
  data M_I_FIND type BOOLEAN .
    "DATA m_stop_stack TYPE i .
    "DATA m_debug TYPE x .
  data M_REFRESH type BOOLEAN .
  data M_UPDATE type BOOLEAN .
  data I_STEP type BOOLEAN .
  data MS_STACK_PREV type ZCL_ACE=>T_STACK .
  data MS_STACK type ZCL_ACE=>T_STACK .
    "DATA i_history TYPE boolean .
  data M_HIST_STEP type I .
  data M_STEP_DELTA type I .
  data MV_RECURSE type I .
  data:
    mt_classes_types  TYPE TABLE OF zcl_ace=>t_classes_types .
  data MO_WINDOW type ref to ZCL_ACE_WINDOW .
  data MV_F7_STOP type BOOLEAN .
  data M_F6_LEVEL type I .
  data M_TARGET_STACK type I .
  data MO_TREE_LOCAL type ref to ZCL_ACE_RTTI_TREE .
  data:
    mt_selected_var   TYPE TABLE OF t_sel_var .
  data MV_STACK_CHANGED type BOOLEAN .
  data M_VARIABLE type ref to DATA .
  data:
    mt_new_string     TYPE TABLE OF  string .
  data M_QUICK type TPDA_SCR_QUICK_INFO .
  data:
    mr_statements     TYPE RANGE OF string .
  data MS_IF type TS_IF .
  data MT_IF type TT_IF .

    " open_int_table moved to ZCL_ACE_TABLE_VIEWER
  methods CONSTRUCTOR
    importing
      !I_PROG type PROG
      !I_NEW_PARSER type ABAP_BOOL default ABAP_FALSE
      !I_SHOW_PARSE_TIME type ABAP_BOOL default ABAP_FALSE .
  methods SHOW .
  methods GET_CODE_FLOW
    importing
      !I_CALC_PATH type BOOLEAN optional
    returning
      value(RESULTS) type TT_LINE .
  methods GET_CODE_MIX
    importing
      !I_CALC_PATH type BOOLEAN optional .
  methods MARK_ACTIVE_ROOT
    importing
      !I_CALC_PATH type BOOLEAN optional
    changing
      !CT_RESULTS type TT_LINE .
  PROTECTED SECTION.
private section.

  types:
    tt_steps   TYPE STANDARD TABLE OF t_step_counter WITH EMPTY KEY .
  types:
    tt_sel_var TYPE STANDARD TABLE OF t_sel_var      WITH EMPTY KEY .

  data MV_DUMMY type I .
  constants:
    BEGIN OF c_kind,
        struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
        table  LIKE cl_abap_typedescr=>kind_table  VALUE cl_abap_typedescr=>kind_table,
        elem   LIKE cl_abap_typedescr=>kind_elem   VALUE cl_abap_typedescr=>kind_elem,
        class  LIKE cl_abap_typedescr=>kind_class  VALUE cl_abap_typedescr=>kind_class,
        intf   LIKE cl_abap_typedescr=>kind_intf   VALUE cl_abap_typedescr=>kind_intf,
        ref    LIKE cl_abap_typedescr=>kind_ref    VALUE cl_abap_typedescr=>kind_ref,
      END OF c_kind .

    " --- get_code_flow helpers ---
  methods PARSE_SEL_CALL .
  methods EXPAND_SELECTED_VARS_FORWARD
    importing
      !IT_STEPS type TT_STEPS
    changing
      !CT_SELECTED_VAR type TT_SEL_VAR .
  methods REMOVE_EMPTY_BLOCK_PAIRS
    changing
      !CT_STEPS type TT_STEPS .
  methods PROPAGATE_VARS_BACKWARD
    importing
      !IT_STEPS type TT_STEPS
    changing
      !CT_SELECTED_VAR type TT_SEL_VAR .
  methods BUILD_RESULT_LINES
    importing
      !IT_STEPS type TT_STEPS
      !IV_NO_FILTER type ABAP_BOOL
      !IT_SELECTED_VAR type TT_SEL_VAR
    changing
      !CT_RESULTS type TT_LINE .
  methods REMOVE_EMPTY_LOOP_PAIRS
    changing
      !CT_RESULTS type TT_LINE .
  methods ENRICH_RESULT_LINES
    changing
      !CT_RESULTS type TT_LINE .
  methods APPLY_CALC_PATH_FILTER
    changing
      !CT_RESULTS type TT_LINE .
ENDCLASS.



CLASS ZCL_ACE IMPLEMENTATION.


  METHOD constructor.
    CONSTANTS: c_mask TYPE x VALUE '01'.
    mv_prog = i_prog. mv_show_parse_time = i_show_parse_time. i_step = abap_on.
    zcl_ace_mermaid=>check_mermaid( ). zcl_ace_sel_opt=>init_icons_table( ).
    mo_window = NEW zcl_ace_window( me ). mo_window->mv_new_parser = i_new_parser.
    mo_tree_local = NEW zcl_ace_rtti_tree( i_header = 'Objects & Code Flow' i_type = 'L'
                                           i_cont = mo_window->mo_locals_container i_debugger = me ).
    show( ).
  ENDMETHOD.


  METHOD mark_active_root.
    DATA lv_tabix TYPE i.
    TYPES: BEGIN OF ts_pair, if_idx TYPE i, end_idx TYPE i, depth TYPE i, END OF ts_pair.
    DATA: lt_pairs TYPE TABLE OF ts_pair WITH EMPTY KEY, ls_pair TYPE ts_pair,
          lt_if_stack TYPE TABLE OF i WITH EMPTY KEY.
    LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<ln>).
      lv_tabix = sy-tabix.
      CASE <ln>-cond.
        WHEN 'IF' OR 'CASE'. APPEND lv_tabix TO lt_if_stack.
        WHEN 'ENDIF' OR 'ENDCASE'.
          IF lt_if_stack IS NOT INITIAL.
            DATA(lv_if_tabix) = lt_if_stack[ lines( lt_if_stack ) ].
            DELETE lt_if_stack INDEX lines( lt_if_stack ).
            ls_pair-if_idx = lv_if_tabix. ls_pair-end_idx = lv_tabix. ls_pair-depth = lines( lt_if_stack ).
            APPEND ls_pair TO lt_pairs.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    SORT lt_pairs BY depth DESCENDING if_idx ASCENDING.
    TYPES: BEGIN OF ts_branch_hdr, tabix TYPE i, cond TYPE string, END OF ts_branch_hdr.
    DATA: lt_hdrs TYPE TABLE OF ts_branch_hdr WITH EMPTY KEY, ls_hdr TYPE ts_branch_hdr,
          lv_if_i TYPE i, lv_end_i TYPE i, lv_inner_depth TYPE i, lv_i TYPE i,
          lv_block_active TYPE flag, lv_hdr_cnt TYPE i, lv_h TYPE i,
          lv_branch_active TYPE flag, lv_scan TYPE i, lv_scan_inner TYPE i.
    LOOP AT lt_pairs INTO ls_pair.
      lv_if_i = ls_pair-if_idx. lv_end_i = ls_pair-end_idx. CLEAR: lt_hdrs, lv_inner_depth.
      lv_i = lv_if_i.
      WHILE lv_i <= lv_end_i.
        READ TABLE ct_results INDEX lv_i ASSIGNING <ln>. IF sy-subrc <> 0. EXIT. ENDIF.
        CASE <ln>-cond.
          WHEN 'IF' OR 'CASE'.
            IF lv_i = lv_if_i. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs.
            ELSE. lv_inner_depth += 1. ENDIF.
          WHEN 'ENDIF' OR 'ENDCASE'.
            IF lv_i = lv_end_i. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs.
            ELSE. lv_inner_depth -= 1. ENDIF.
          WHEN 'ELSEIF' OR 'ELSE' OR 'WHEN'.
            IF lv_inner_depth = 0. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs. ENDIF.
        ENDCASE.
        lv_i += 1.
      ENDWHILE.
      CLEAR lv_block_active. lv_hdr_cnt = lines( lt_hdrs ). lv_h = 1.
      WHILE lv_h < lv_hdr_cnt.
        DATA(ls_hdr_cur) = lt_hdrs[ lv_h ]. DATA(ls_hdr_next) = lt_hdrs[ lv_h + 1 ].
        CLEAR: lv_branch_active, lv_scan_inner. lv_scan = ls_hdr_cur-tabix + 1.
        WHILE lv_scan < ls_hdr_next-tabix.
          READ TABLE ct_results INDEX lv_scan ASSIGNING FIELD-SYMBOL(<scan_ln>). IF sy-subrc <> 0. EXIT. ENDIF.
          CASE <scan_ln>-cond.
            WHEN 'IF' OR 'CASE'. lv_scan_inner += 1.
            WHEN 'ENDIF' OR 'ENDCASE'. lv_scan_inner -= 1.
            WHEN OTHERS. IF lv_scan_inner = 0 AND <scan_ln>-active_root = abap_true. lv_branch_active = abap_true. ENDIF.
          ENDCASE.
          IF lv_branch_active = abap_true. EXIT. ENDIF.
          lv_scan += 1.
        ENDWHILE.
        ASSIGN ct_results[ ls_hdr_cur-tabix ] TO FIELD-SYMBOL(<hdr_ln>).
        IF sy-subrc = 0.
          IF lv_branch_active = abap_true. <hdr_ln>-active_root = abap_true. lv_block_active = abap_true.
          ELSE. CLEAR <hdr_ln>-active_root. ENDIF.
        ENDIF.
        lv_h += 1.
      ENDWHILE.
      ASSIGN ct_results[ lv_end_i ] TO FIELD-SYMBOL(<endif_ln>).
      IF sy-subrc = 0. <endif_ln>-active_root = lv_block_active. ENDIF.
      ASSIGN ct_results[ lv_if_i ] TO FIELD-SYMBOL(<if_ln>).
      IF sy-subrc = 0.
        IF lv_block_active = abap_false. CLEAR <if_ln>-active_root.
        ELSE. <if_ln>-active_root = abap_true. ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


METHOD get_code_flow.
    " 0. Parse selected call if needed
    parse_sel_call( ).

    " 1. Prepare steps: deduplicate and sort
    DATA(steps) = CONV tt_steps( mt_steps ).
    SORT steps BY line eventtype eventname.
    DELETE ADJACENT DUPLICATES FROM steps.
    SORT steps BY step.

    " 2. Expand variable filter forward (bindings propagation)
    DATA(lt_selected_var) = CONV tt_sel_var( mt_selected_var ).
    expand_selected_vars_forward(
      EXPORTING it_steps        = steps
      CHANGING  ct_selected_var = lt_selected_var ).

    " 3. Remove empty block pairs (DO/ENDDO, LOOP/ENDLOOP etc. with nothing inside)
    remove_empty_block_pairs( CHANGING ct_steps = steps ).

    " 4. Propagate variables backward through the sorted-descending steps
    propagate_vars_backward(
      EXPORTING it_steps        = steps
      CHANGING  ct_selected_var = lt_selected_var ).

    SORT lt_selected_var. DELETE ADJACENT DUPLICATES FROM lt_selected_var.
    CLEAR mo_window->mt_coverage.

    " 5. Build result lines
    DATA(lv_no_filter) = xsdbool( mt_selected_var IS INITIAL AND i_calc_path = abap_false ).
    build_result_lines(
      EXPORTING it_steps        = steps
                iv_no_filter    = lv_no_filter
                it_selected_var = lt_selected_var
      CHANGING  ct_results      = results ).

    " 6. Remove empty loop pairs from results
    remove_empty_loop_pairs( CHANGING ct_results = results ).

    " 7. Enrich result lines (code text, arrows, subnames, if/else indices)
    enrich_result_lines( CHANGING ct_results = results ).

    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
      INSERT ms_if INTO mt_if INDEX 1.
    ENDIF.
    IF lines( results ) > 0.
      IF results[ lines( results ) ]-arrow IS NOT INITIAL.
        CLEAR results[ lines( results ) ]-arrow.
      ENDIF.
    ENDIF.

    " 8. Mark active_root (IF/CASE branch highlighting)
    mark_active_root( EXPORTING i_calc_path = i_calc_path CHANGING ct_results = results ).

    " 9. Apply calc_path filter if requested
    IF i_calc_path = abap_true.
      apply_calc_path_filter( CHANGING ct_results = results ).
    ENDIF.
  ENDMETHOD.


  METHOD get_code_mix.
    DATA: flow_lines TYPE sci_include, splits TYPE TABLE OF string, ind TYPE i, prev_flow TYPE ts_line.
    DATA(lines) = get_code_flow( i_calc_path = i_calc_path ).
    READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = 'Code_Flow_Mix' ASSIGNING FIELD-SYMBOL(<prog_mix>).
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mo_window->ms_sources-tt_progs ASSIGNING <prog_mix>.
      INSERT INITIAL LINE INTO mo_window->mt_stack INDEX 1 ASSIGNING FIELD-SYMBOL(<stack_mix>).
      <prog_mix>-include = <stack_mix>-program = <stack_mix>-include = 'Code_Flow_Mix'.
    ENDIF.
    LOOP AT lines INTO DATA(flow_line).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = flow_line-include INTO DATA(prog).
      READ TABLE prog-t_keywords WITH KEY line = flow_line-line INTO DATA(keyword).
      APPEND INITIAL LINE TO <prog_mix>-t_keywords ASSIGNING FIELD-SYMBOL(<keyword_mix>).
      <keyword_mix> = keyword. <keyword_mix>-include = flow_line-include. <keyword_mix>-program = flow_line-program.
      DATA(from_row) = prog-scan->tokens[ keyword-from ]-row.
      DATA(to_row)   = prog-scan->tokens[ keyword-to   ]-row.
      DATA(spaces)   = repeat( val = | | occ = ( flow_line-stack - 1 ) * 3 ).
      DATA(dashes)   = repeat( val = |-| occ = ( flow_line-stack ) ).
      IF prev_flow-ev_name <> flow_line-ev_name OR prev_flow-ev_type <> flow_line-ev_type
      OR prev_flow-class   <> flow_line-class   OR prev_flow-stack   <> flow_line-stack.
        SPLIT flow_line-include AT '=' INTO TABLE splits.
        APPEND INITIAL LINE TO flow_lines ASSIGNING FIELD-SYMBOL(<flow>).
        ind = sy-tabix.
        IF flow_line-class IS INITIAL.
          <flow> = |"{ dashes } { flow_line-ev_type } { flow_line-ev_name } in { splits[ 1 ] }|.
        ELSE.
          <flow> = |"{ dashes } { flow_line-ev_type } { flow_line-ev_name } in { flow_line-class }|.
        ENDIF.
      ENDIF.
      <keyword_mix>-v_line = ind + 1.
      LOOP AT prog-source_tab FROM from_row TO to_row INTO DATA(source_line).
        APPEND INITIAL LINE TO flow_lines ASSIGNING <flow>. ind = sy-tabix. <flow> = |{ spaces }{ source_line }|.
      ENDLOOP.
      prev_flow = flow_line.
    ENDLOOP.
    mo_window->mo_code_viewer->set_text( table = flow_lines ). <prog_mix>-source_tab = flow_lines.
    mo_window->m_prg-include = 'Code_Flow_Mix'. mo_window->set_mixprog_line( ). mo_window->show_stack( ).
    mo_window->mo_box->set_caption( |Code Mix: { lines( lines ) } statements| ).
  ENDMETHOD.


METHOD show.
    IF mo_window->m_prg-include IS INITIAL. mo_window->m_prg-program = mo_window->m_prg-include = mv_prog. ENDIF.
    mo_window->set_program( mo_window->m_prg-include ).
    IF mo_window->m_prg-include <> 'Code_Flow_Mix'. mo_window->show_coverage( ). ENDIF.
    IF mo_window->m_prg-line IS INITIAL AND mo_window->mt_stack IS NOT INITIAL.
      mo_window->m_prg-line = mo_window->mt_stack[ 1 ]-line.
    ENDIF.
    mo_window->set_program_line( 1 ).
    DELETE mo_window->ms_sources-tt_progs WHERE t_keywords IS INITIAL.
    mo_window->show_stack( ).

    NEW zcl_ace_tree_builder(
  io_window = mo_window
  io_tree   = mo_tree_local )->build( ).
  ENDMETHOD.


METHOD apply_calc_path_filter.
    DATA lt_active_ev TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<line>) WHERE active_root = abap_true.
      READ TABLE lt_active_ev WITH KEY table_line = <line>-ev_name TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0. INSERT <line>-ev_name INTO TABLE lt_active_ev. ENDIF.
    ENDLOOP.
    DATA lt_active_sub TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    LOOP AT lt_active_ev INTO DATA(lv_ev).
      READ TABLE lt_active_sub WITH KEY table_line = lv_ev TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0. INSERT lv_ev INTO TABLE lt_active_sub. ENDIF.
    ENDLOOP.
    DATA lv_expanded TYPE boolean.
    DO.
      CLEAR lv_expanded.
      LOOP AT ct_results ASSIGNING <line>
        WHERE subname IS NOT INITIAL AND active_root IS INITIAL.
        READ TABLE lt_active_sub WITH KEY table_line = <line>-subname TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          READ TABLE lt_active_sub WITH KEY table_line = <line>-ev_name TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            INSERT <line>-ev_name INTO TABLE lt_active_sub. lv_expanded = abap_true.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_expanded = abap_false. EXIT. ENDIF.
    ENDDO.
    LOOP AT ct_results ASSIGNING <line> WHERE active_root IS INITIAL.
      IF <line>-subname IS NOT INITIAL.
        READ TABLE lt_active_sub WITH KEY table_line = <line>-subname TRANSPORTING NO FIELDS.
        IF sy-subrc = 0. CONTINUE. ENDIF.
      ENDIF.
      IF <line>-cond IS NOT INITIAL.
        READ TABLE lt_active_ev WITH KEY table_line = <line>-ev_name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0. CONTINUE. ENDIF.
      ENDIF.
      <line>-del = abap_true.
    ENDLOOP.
    DELETE ct_results WHERE del = abap_true.
    LOOP AT ct_results ASSIGNING <line>. <line>-ind = sy-tabix. ENDLOOP.
  ENDMETHOD.


METHOD build_result_lines.
    DATA: line TYPE ts_line, inserted TYPE boolean,
          prog TYPE LINE OF zif_ace_parse_data=>tt_progs,
          key TYPE ts_kword, ind TYPE i.
    LOOP AT it_steps INTO DATA(step).
      CLEAR inserted.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO key.
      CLEAR line-cond.
      IF key-name = 'IF'    OR key-name = 'ELSE'    OR key-name = 'ENDIF'   OR
         key-name = 'ELSEIF' OR key-name = 'CASE'   OR key-name = 'WHEN'    OR
         key-name = 'ENDCASE' OR key-name = 'DO'    OR key-name = 'ENDDO'   OR
         key-name = 'LOOP'  OR key-name = 'ENDLOOP' OR
         key-name = 'WHILE' OR key-name = 'ENDWHILE'.
        APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).
        <watch>-program = step-program. <watch>-line = line-line = step-line.
        INSERT line INTO ct_results INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-cond    = key-name.          <line>-ev_name  = step-eventname.
        <line>-stack   = step-stacklevel.   <line>-include  = step-include.
        <line>-class   = step-class.        inserted = abap_true.
      ENDIF.
      CLEAR ind.
      LOOP AT mo_window->ms_sources-t_calculated INTO DATA(calc_var)
          WHERE include = step-include AND class = step-class
            AND eventtype = step-eventtype AND eventname = step-eventname AND line = step-line.
        ADD 1 TO ind.
        LOOP AT mo_window->ms_sources-t_composed INTO DATA(comp_var)
            WHERE include = step-include AND class = step-class
              AND eventtype = step-eventtype AND eventname = step-eventname AND line = step-line.
          READ TABLE it_selected_var WITH KEY name = comp_var-name TRANSPORTING NO FIELDS.
          " composed var already in filter - no action needed here (filter is read-only in this method)
        ENDLOOP.
        READ TABLE it_selected_var WITH KEY name = calc_var-name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0 OR iv_no_filter = abap_true.
          APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING <watch>.
          <watch>-program = step-program. <watch>-line = line-line = step-line.
          IF ind = 1.
            IF key-name <> 'PUBLIC'    AND key-name <> 'ENDCLASS'  AND
               key-name <> 'ENDFORM'   AND key-name <> 'FORM'      AND
               key-name <> 'METHOD'    AND key-name <> 'METHODS'   AND
               key-name <> 'ENDMETHOD' AND key-name <> 'MODULE'    AND
               inserted = abap_false.
              line-ev_name     = step-eventname.    line-stack  = step-stacklevel.
              line-include     = step-include.      line-class  = step-class.
              line-ev_type     = step-eventtype.    line-active_root = abap_true.
              INSERT line INTO ct_results INDEX 1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF inserted = abap_false.
        IF key-name <> 'PUBLIC'    AND key-name <> 'ENDCLASS'  AND
           key-name <> 'ENDFORM'   AND key-name <> 'ENDMETHOD' AND
           key-name <> 'METHOD'    AND key-name <> 'METHODS'   AND
           key-name <> 'MODULE'    AND key-name <> 'FORM'.
          READ TABLE ct_results WITH KEY line     = step-line
                                         include  = step-include
                                         ev_type  = step-eventtype
                                         ev_name  = step-eventname
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            line-line        = step-line.       line-ev_name  = step-eventname.
            line-stack       = step-stacklevel. line-include  = step-include.
            line-ev_type     = step-eventtype.  line-class    = step-class.
            line-active_root = abap_false.
            INSERT line INTO ct_results INDEX 1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE ct_results WHERE del = abap_true.
  ENDMETHOD.


METHOD enrich_result_lines.
    DATA: prog      TYPE LINE OF zif_ace_parse_data=>tt_progs,
          keyword   TYPE ts_kword,
          call      TYPE ts_calls,
          els_before TYPE i,
          if_depth   TYPE i,
          when_count TYPE i,
          line_tmp   TYPE ts_line,
          counter    TYPE i.
    LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<line>).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = <line>-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = <line>-line INTO keyword.
      LOOP AT prog-scan->tokens FROM keyword-from TO keyword-to INTO DATA(token).
        IF token-str = 'USING' OR token-str = 'EXPORTING' OR
           token-str = 'IMPORTING' OR token-str = 'CHANGING'. EXIT. ENDIF.
        IF <line>-code IS INITIAL. <line>-code = token-str.
        ELSE. <line>-code = |{ <line>-code } { token-str }|. ENDIF.
      ENDLOOP.
      IF keyword-tt_calls IS NOT INITIAL.
        DATA(lv_arrow_cnt) = 0.
        LOOP AT keyword-tt_calls INTO call.
          IF <line>-subname IS INITIAL.
            <line>-subname = call-name.
            REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
            REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
          ENDIF.
          REPLACE ALL OCCURRENCES OF '"' IN <line>-code WITH ''.
          LOOP AT call-bindings INTO DATA(ls_b).
            CHECK ls_b-outer IS NOT INITIAL OR ls_b-inner IS NOT INITIAL.
            IF lv_arrow_cnt > 0. <line>-arrow = |{ <line>-arrow }, |. ENDIF.
            DATA(lv_sep) = SWITCH string( ls_b-dir
              WHEN 'I' THEN '->'    WHEN 'E' THEN '<-'
              WHEN 'C' THEN '-> <-' ELSE '--' ).
            <line>-arrow = |{ <line>-arrow } { ls_b-outer } { lv_sep } { ls_b-inner }|.
            lv_arrow_cnt += 1.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
      REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-arrow   WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-arrow   WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
    ENDLOOP.
    LOOP AT ct_results ASSIGNING <line>. <line>-ind = sy-tabix. ENDLOOP.
    LOOP AT ct_results ASSIGNING <line>
      WHERE code <> 'DO'   AND code <> 'ENDDO'   AND code <> 'WHILE' AND
            code <> 'ENDWHILE' AND code <> 'LOOP' AND code <> 'ENDLOOP'.
      FIELD-SYMBOLS: <if> TYPE ts_if.
      IF <line>-cond = 'IF' OR <line>-cond = 'CASE'.
        ADD 1 TO if_depth. CLEAR when_count.
        APPEND INITIAL LINE TO mt_if ASSIGNING <if>. <if>-if_ind = <line>-ind.
      ENDIF.
      IF <line>-cond = 'ENDIF' OR <line>-cond = 'ENDCASE'.
        IF <if> IS ASSIGNED.
          <if>-end_ind = <line>-ind. SUBTRACT 1 FROM if_depth.
          LOOP AT mt_if ASSIGNING <if> WHERE end_ind = 0. ENDLOOP.
        ENDIF.
      ENDIF.
      IF <line>-cond = 'WHEN'. ADD 1 TO when_count. ENDIF.
      IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.
        <line>-els_before = els_before. <line>-els_after = <line>-ind.
        counter = <line>-ind + 1.
        DO.
          READ TABLE ct_results INDEX counter INTO line_tmp.
          IF sy-subrc <> 0. CLEAR <line>-els_after. EXIT. ENDIF.
          IF line_tmp-cond = 'ELSE' OR line_tmp-cond = 'ELSEIF'.
            CLEAR <line>-els_after. EXIT.
          ELSEIF line_tmp-cond <> 'DO'      AND line_tmp-cond <> 'ENDDO'   AND
                 line_tmp-cond <> 'WHILE'   AND line_tmp-cond <> 'ENDWHILE' AND
                 line_tmp-cond <> 'LOOP'    AND line_tmp-cond <> 'ENDLOOP'.
            <line>-els_after = counter. EXIT.
          ELSE. ADD 1 TO counter. ENDIF.
        ENDDO.
      ENDIF.
      IF <line>-cond = 'WHEN'.
        <line>-els_before = els_before. <line>-els_after = <line>-ind.
        counter = <line>-ind + 1.
        DO.
          READ TABLE ct_results INDEX counter INTO line_tmp.
          IF sy-subrc <> 0. CLEAR <line>-els_after. EXIT. ENDIF.
          IF line_tmp-cond = 'WHEN'. CLEAR <line>-els_after. EXIT.
          ELSEIF line_tmp-cond <> 'DO'      AND line_tmp-cond <> 'ENDDO'   AND
                 line_tmp-cond <> 'WHILE'   AND line_tmp-cond <> 'ENDWHILE' AND
                 line_tmp-cond <> 'LOOP'    AND line_tmp-cond <> 'ENDLOOP'.
            <line>-els_after = counter. EXIT.
          ELSE. ADD 1 TO counter. ENDIF.
        ENDDO.
        IF when_count = 1.
          IF <if> IS ASSIGNED. <if>-if_ind = els_before. ENDIF.
          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.
      IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
        els_before = <line>-ind.
      ELSE. CLEAR els_before. ENDIF.
    ENDLOOP.
    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
      INSERT ms_if INTO mt_if INDEX 1.
    ENDIF.
    IF lines( ct_results ) > 0.
      IF ct_results[ lines( ct_results ) ]-arrow IS NOT INITIAL.
        CLEAR ct_results[ lines( ct_results ) ]-arrow.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD expand_selected_vars_forward.
    DATA: keyword TYPE ts_kword, prog TYPE LINE OF zif_ace_parse_data=>tt_progs.
    DATA yes TYPE xfeld.
    LOOP AT it_steps INTO DATA(step).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.

      " Check if any binding in this step matches ct_selected_var (or no filter at all)
      CLEAR yes.
      LOOP AT keyword-tt_calls INTO DATA(call).
        LOOP AT call-bindings INTO DATA(ls_b_chk).
          READ TABLE ct_selected_var WITH KEY name = ls_b_chk-outer TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.
            yes = abap_true.
          ENDIF.
          READ TABLE ct_selected_var WITH KEY name = ls_b_chk-inner TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.
            yes = abap_true.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      CHECK yes = abap_true.

      LOOP AT keyword-tt_calls INTO call.
        LOOP AT call-bindings INTO DATA(ls_b_add).
          IF mt_selected_var IS INITIAL.
            " No filter - add both outer and inner with uniqueness check
            READ TABLE ct_selected_var WITH KEY name = ls_b_add-outer TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO ct_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
              <sel>-name = ls_b_add-outer.
            ENDIF.
            READ TABLE ct_selected_var WITH KEY name = ls_b_add-inner TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO ct_selected_var ASSIGNING <sel>.
              <sel>-name = ls_b_add-inner.
            ENDIF.
          ELSE.
            " Filter active - add only the opposite side of the matched binding
            READ TABLE ct_selected_var WITH KEY name = ls_b_add-outer TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              " outer matched - add inner
              READ TABLE ct_selected_var WITH KEY name = ls_b_add-inner TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                APPEND INITIAL LINE TO ct_selected_var ASSIGNING FIELD-SYMBOL(<sel_i>).
                <sel_i>-name = ls_b_add-inner.
              ENDIF.
            ENDIF.
            READ TABLE ct_selected_var WITH KEY name = ls_b_add-inner TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              " inner matched - add outer
              READ TABLE ct_selected_var WITH KEY name = ls_b_add-outer TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                APPEND INITIAL LINE TO ct_selected_var ASSIGNING FIELD-SYMBOL(<sel_o>).
                <sel_o>-name = ls_b_add-outer.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


METHOD parse_sel_call.
    CHECK mo_window->ms_sel_call IS NOT INITIAL.
    CLEAR: mt_steps, mo_window->mt_calls.
    IF mo_window->ms_sel_call-eventtype = 'FORM'.
      zcl_ace_source_parser=>parse_call_form(
        EXPORTING i_call_name = mo_window->ms_sel_call-eventname
                  i_program   = mo_window->ms_sel_call-program
                  i_include   = mo_window->ms_sel_call-include
                  i_stack     = 0
                  io_debugger = mo_window->mo_viewer ).
    ELSE.
      zcl_ace_source_parser=>parse_call(
        EXPORTING i_index     = mo_window->ms_sel_call-index
                  i_e_name    = mo_window->ms_sel_call-eventname
                  i_e_type    = mo_window->ms_sel_call-eventtype
                  i_program   = mo_window->ms_sel_call-program
                  i_include   = mo_window->ms_sel_call-include
                  i_class     = mo_window->ms_sel_call-class
                  i_stack     = 0
                  io_debugger = mo_window->mo_viewer ).
    ENDIF.
  ENDMETHOD.


METHOD propagate_vars_backward.
    DATA: prog TYPE LINE OF zif_ace_parse_data=>tt_progs, keyword TYPE ts_kword, ind TYPE i.
    LOOP AT it_steps INTO DATA(step).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      LOOP AT mo_window->ms_sources-t_calculated INTO DATA(calc_var)
          WHERE include = step-include AND class = step-class
            AND eventtype = step-eventtype AND eventname = step-eventname AND line = step-line.
        ADD 1 TO ind.
        READ TABLE ct_selected_var WITH KEY name = calc_var-name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          " calc_var found in ct_selected_var - add all composed vars of this line
          LOOP AT mo_window->ms_sources-t_composed INTO DATA(comp_var)
              WHERE include = step-include AND class = step-class
                AND eventtype = step-eventtype AND eventname = step-eventname AND line = step-line.
            READ TABLE ct_selected_var WITH KEY name = comp_var-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO ct_selected_var ASSIGNING FIELD-SYMBOL(<sel>).
              <sel>-name = comp_var-name.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.
      LOOP AT keyword-tt_calls INTO DATA(call).
        LOOP AT call-bindings INTO DATA(b_prop).
          " backward: inner (parameter) found - add outer (caller variable)
          READ TABLE ct_selected_var WITH KEY name = b_prop-inner TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            READ TABLE ct_selected_var WITH KEY name = b_prop-outer TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO ct_selected_var ASSIGNING <sel>.
              <sel>-name = b_prop-outer.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


METHOD remove_empty_block_pairs.
    DATA: prev TYPE t_step_counter, pre_key TYPE string,
          prog TYPE LINE OF zif_ace_parse_data=>tt_progs.
    LOOP AT ct_steps ASSIGNING FIELD-SYMBOL(<step>).
      DATA(ind) = sy-tabix.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = <step>-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = <step>-line INTO DATA(key).
      IF prev IS NOT INITIAL.
        IF ( key-name = 'ENDDO'   OR key-name = 'ENDWHILE' OR
             key-name = 'ENDLOOP' OR key-name = 'ENDIF' ) AND
           ( pre_key  = 'DO'      OR pre_key  = 'LOOP'    OR
             pre_key  = 'WHILE'   OR pre_key  = 'IF' ).
          <step>-first = 'D'.
          READ TABLE ct_steps INDEX ind - 1 ASSIGNING FIELD-SYMBOL(<prev_step>).
          <prev_step>-first = 'D'.
        ENDIF.
      ENDIF.
      prev = <step>. pre_key = key-name.
    ENDLOOP.
    DELETE ct_steps WHERE first = 'D'.
    SORT ct_steps BY step DESCENDING.
  ENDMETHOD.


METHOD remove_empty_loop_pairs.
    DATA lv_changed TYPE boolean.
    DO.
      CLEAR lv_changed.
      LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<line>).
        DATA(lv_ti) = sy-tabix.
        IF <line>-cond = 'LOOP' OR <line>-cond = 'DO' OR <line>-cond = 'WHILE'.
          READ TABLE ct_results INDEX lv_ti + 1 ASSIGNING FIELD-SYMBOL(<next>).
          IF sy-subrc = 0.
            IF ( <line>-cond = 'LOOP'  AND <next>-cond = 'ENDLOOP'  ) OR
               ( <line>-cond = 'DO'    AND <next>-cond = 'ENDDO'    ) OR
               ( <line>-cond = 'WHILE' AND <next>-cond = 'ENDWHILE' ).
              DELETE ct_results INDEX lv_ti + 1.
              DELETE ct_results INDEX lv_ti.
              lv_changed = abap_true. EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_changed = abap_false. EXIT. ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
