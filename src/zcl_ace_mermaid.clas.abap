class ZCL_ACE_MERMAID definition
  public
  inheriting from ZCL_ACE_POPUP
  create public .

public section.

  class-methods CHECK_MERMAID .

  data MO_VIEWER type ref to ZCL_ACE .
  data MO_MM_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_MM_TOOLBAR type ref to CL_GUI_CONTAINER .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_DIAGRAM type ref to OBJECT .
  data MV_TYPE type STRING .
  data MV_CALC_PATH type BOOLEAN .
  data MV_WITH_PARAMS type BOOLEAN .
  data MV_SHOW_EXT type BOOLEAN .
  data MV_ALL_METHODS type BOOLEAN .
  data MV_DIRECTION type UI_FUNC .

  methods CONSTRUCTOR
    importing
      !IO_DEBUGGER type ref to ZCL_ACE
      !I_TYPE type STRING .
  methods STEPS_FLOW
    importing
      !I_DIRECTION   type UI_FUNC    optional
      !I_WITH_PARAMS type BOOLEAN    optional
      !I_CALC_PATH   type BOOLEAN    optional .
  methods MAGIC_SEARCH
    importing
      !I_DIRECTION type UI_FUNC optional
      !I_CALC_PATH type BOOLEAN optional .
  methods CLASS_MAP
    importing
      !I_DIRECTION type UI_FUNC optional .
  methods ADD_TOOLBAR_BUTTONS .
  methods HND_TOOLBAR
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods OPEN_MERMAID
    importing
      !I_MM_STRING type STRING .
  methods REFRESH .

protected section.
private section.

  methods FORMAT_NODE_LABEL
    importing
      !I_CODE   type STRING
      !I_MAXLEN type i default 50
    returning
      value(RV_LABEL) type STRING .

  methods BUILD_NODES
    importing
      !I_DIRECTION type STRING
    changing
      !CT_LINES     type mo_viewer->tt_line
      !CV_MM_STRING type STRING .

  methods BUILD_EDGES
    importing
      !IT_LINES     type mo_viewer->tt_line
    changing
      !CV_MM_STRING type STRING .

ENDCLASS.



CLASS ZCL_ACE_MERMAID IMPLEMENTATION.


  METHOD check_mermaid.
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING clskey = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
      EXCEPTIONS not_specified = 1 not_existing = 2 i_interface = 3
                 no_text = 4 inconsistent = 5 OTHERS = 6.
    IF sy-subrc = 0.
      zcl_ace=>i_mermaid_active = abap_true.
    ELSE.
      CLEAR zcl_ace=>i_mermaid_active.
    ENDIF.
  ENDMETHOD.


  method FORMAT_NODE_LABEL.

    CONSTANTS lc_br    TYPE string VALUE `<br/>`.
    CONSTANTS lc_br_ph TYPE string VALUE `##BR##`.

    " Effective max length: use parameter value, but treat 50 (old default) as 100
    "DATA(lv_maxlen) = COND i( WHEN I_MAXLEN = 50 OR I_MAXLEN = 0 THEN 100 ELSE I_MAXLEN ).
DATA(lv_maxlen) = 200.
    RV_LABEL = I_CODE.

    " Protect existing <br/> tags before any text transformations
    REPLACE ALL OCCURRENCES OF lc_br IN RV_LABEL WITH lc_br_ph IN CHARACTER MODE.

    " Truncate only if lv_maxlen > 0
    IF lv_maxlen > 0 AND strlen( RV_LABEL ) > lv_maxlen.
      RV_LABEL = RV_LABEL+0(lv_maxlen).
    ENDIF.

    REPLACE ALL OCCURRENCES OF `PERFORM`       IN RV_LABEL WITH `FORM`     IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN RV_LABEL WITH `FUNCTION` IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `CALL METHOD`   IN RV_LABEL WITH `METHOD`   IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `-`             IN RV_LABEL WITH ` `        IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF ` `             IN RV_LABEL WITH `&nbsp;`   IN CHARACTER MODE.

    " Restore <br/> tags
    REPLACE ALL OCCURRENCES OF lc_br_ph IN RV_LABEL WITH lc_br IN CHARACTER MODE.

  endmethod.


  method BUILD_NODES.

    DATA: box_s        TYPE string,
          box_e        TYPE string,
          opened       TYPE i,
          pre_stack    TYPE mo_viewer->ts_line,
          times        TYPE i,
          lt_sg_opened TYPE TABLE OF flag WITH EMPTY KEY.

    LOOP AT CT_LINES ASSIGNING FIELD-SYMBOL(<line>) WHERE cond <> 'ELSE' AND cond <> 'ELSEIF' AND cond <> 'WHEN'.
      DATA(ind) = <line>-ind.
      DATA(lv_tabix) = sy-tabix.

      IF <line>-cond IS INITIAL.
        box_s = '('. box_e = ')'.
      ELSE.
        box_s = '{'. box_e = '}'.
      ENDIF.

      IF pre_stack IS INITIAL.
        pre_stack = <line>.
      ENDIF.

      " Close subgraphs when stack level decreases or event changes
      IF ( pre_stack-stack > <line>-stack OR pre_stack-ev_name <> <line>-ev_name )
         AND opened > 0.
        IF pre_stack-stack = <line>-stack AND pre_stack-ev_name <> <line>-ev_name.
          times = 1.
        ELSE.
          times = pre_stack-stack - <line>-stack.
        ENDIF.
        DO times TIMES.
          CV_MM_STRING = |{ CV_MM_STRING } end\n|.
          opened -= 1.
          IF opened = 0. EXIT. ENDIF.
        ENDDO.
      ENDIF.

      " LOOP/DO/WHILE — only subgraph, no node
      IF <line>-cond = 'LOOP' OR <line>-cond = 'DO' OR <line>-cond = 'WHILE'.

        REPLACE ALL OCCURRENCES OF `-` IN <line>-code WITH ` ` IN CHARACTER MODE.
        pre_stack = <line>.

        DATA(name) = format_node_label( i_code = <line>-code ).

        " Only open subgraph if next line is not immediately END*
        READ TABLE CT_LINES INDEX lv_tabix + 1 INTO DATA(line2).
        IF sy-subrc = 0
           AND line2-cond <> 'ENDLOOP' AND line2-cond <> 'ENDDO' AND line2-cond <> 'ENDWHILE'.
          CV_MM_STRING = |{ CV_MM_STRING } subgraph S{ ind }["{ name }"]\n  direction { I_DIRECTION }\n|.
          opened += 1.
          APPEND abap_true TO lt_sg_opened.
        ELSE.
          APPEND abap_false TO lt_sg_opened.
        ENDIF.
        CONTINUE.

      ENDIF.

      " PERFORM/CALL FUNCTION/CALL METHOD etc.
      IF <line>-subname IS NOT INITIAL.

        READ TABLE CT_LINES INDEX lv_tabix + 1 INTO line2.
        DATA(lv_has_children) = xsdbool( sy-subrc = 0 AND line2-stack > <line>-stack ).

        IF lv_has_children = abap_true.
          " Call goes deeper (stack+1): show only the call signature without parameters
          " (the parameters are visible in the child nodes / subgraph below).
          " Strip everything from the first opening parenthesis onward.
          DATA(lv_call_label) = <line>-code.

          FIND FIRST OCCURRENCE OF ` = ` IN lv_call_label MATCH OFFSET DATA(lv_eq_off).
          IF sy-subrc = 0.
            DATA(lv_rhs) = lv_call_label+lv_eq_off.
            FIND FIRST OCCURRENCE OF '(' IN lv_rhs MATCH OFFSET DATA(lv_rhs_off).
            IF sy-subrc = 0.
              DATA(lv_abs_paren) = lv_eq_off + lv_rhs_off + 1.
              lv_call_label = |{ lv_call_label(lv_abs_paren) } )|.
            ELSE.
              lv_call_label = |{ lv_call_label }( )|.
            ENDIF.
          ELSE.
            FIND FIRST OCCURRENCE OF '(' IN lv_call_label MATCH OFFSET DATA(lv_off).
            IF sy-subrc = 0.
              lv_call_label = |{ lv_call_label(lv_off) }( )|.
            ELSE.
              lv_call_label = |{ lv_call_label }( )|.
            ENDIF.
          ENDIF.

          DATA(name2) = format_node_label( i_code = lv_call_label i_maxlen = 0 ).
          CV_MM_STRING = |{ CV_MM_STRING }{ ind }{ box_s }"{ name2 }"{ box_e }\n|.

          DATA(lv_sg_title) = format_node_label( i_code = <line>-subname i_maxlen = 0 ).
          CV_MM_STRING = |{ CV_MM_STRING } subgraph S{ ind }["{ lv_sg_title }"]\n  direction { I_DIRECTION }\n|.
          opened += 1.
        ELSE.
          " Same-level call (no children): show the full source line including all parameters.
          " <line>-code was built with an early EXIT at USING/EXPORTING/IMPORTING/CHANGING,
          " so we re-read all tokens directly from the scan to get the complete text.
          DATA(lv_label_code) = ``.

          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
            WITH KEY include = <line>-include INTO DATA(ls_prog_full).
          IF sy-subrc = 0.
            READ TABLE ls_prog_full-t_keywords
              WITH KEY line = <line>-line INTO DATA(ls_kw_full).
            IF sy-subrc = 0.
              " Read all tokens for this keyword span (no early exit on USING/EXPORTING/…)
              LOOP AT ls_prog_full-scan->tokens
                FROM ls_kw_full-from TO ls_kw_full-to
                INTO DATA(ls_tok_full).
                IF lv_label_code IS INITIAL.
                  lv_label_code = ls_tok_full-str.
                ELSE.
                  lv_label_code = |{ lv_label_code } { ls_tok_full-str }|.
                ENDIF.
              ENDLOOP.
              REPLACE ALL OCCURRENCES OF '"' IN lv_label_code WITH ``.
            ENDIF.
          ENDIF.

          " Fall back to the pre-built code if the re-read yielded nothing.
          IF lv_label_code IS INITIAL.
            lv_label_code = <line>-code.
          ENDIF.

          " If the call has bindings (named parameters), insert <br/> before each
          " parameter so every parameter starts on a new line in the Mermaid node label.
          READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
            WITH KEY include = <line>-include INTO DATA(ls_prog_bn).
          IF sy-subrc = 0.
            READ TABLE ls_prog_bn-t_keywords
              WITH KEY line = <line>-line INTO DATA(ls_kw_bn).
            IF sy-subrc = 0 AND ls_kw_bn-tt_calls IS NOT INITIAL.
              " Use bindings from the first call entry that has named parameters.
              LOOP AT ls_kw_bn-tt_calls INTO DATA(ls_call_bn).
                IF ls_call_bn-bindings IS NOT INITIAL.
                  LOOP AT ls_call_bn-bindings INTO DATA(ls_bind_bn).
                    IF ls_bind_bn-inner IS INITIAL. CONTINUE. ENDIF.
                    " Insert <br/> before " INNER =" pattern in the code string.
                    DATA(lv_pattern_bn) = | { ls_bind_bn-inner } =|.
                    REPLACE ALL OCCURRENCES OF lv_pattern_bn
                      IN lv_label_code
                      WITH |<br/>{ lv_pattern_bn }|
                      IN CHARACTER MODE.
                  ENDLOOP.
                  EXIT. " only process bindings of the first matching call
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.

          DATA(lv_label) = format_node_label( i_code = lv_label_code i_maxlen = 0 ).
          CV_MM_STRING = |{ CV_MM_STRING }{ ind }{ box_s }"{ lv_label }"{ box_e }\n|.
          CLEAR <line>-arrow.
        ENDIF.

        pre_stack = <line>.
        CONTINUE.

      ENDIF.

      " END* — only close if subgraph was actually opened
      IF <line>-cond = 'ENDLOOP' OR <line>-cond = 'ENDDO' OR <line>-cond = 'ENDWHILE'.
        DATA(lv_last) = lines( lt_sg_opened ).
        IF lv_last > 0.
          READ TABLE lt_sg_opened INDEX lv_last INTO DATA(lv_sg_flag).
          DELETE lt_sg_opened INDEX lv_last.
          IF lv_sg_flag = abap_true.
            opened -= 1.
            CV_MM_STRING = |{ CV_MM_STRING } end\n|.
          ENDIF.
        ENDIF.
        CONTINUE.
      ENDIF.

      " Regular node
      REPLACE ALL OCCURRENCES OF `-` IN <line>-code WITH ` ` IN CHARACTER MODE.
      DATA(lv_reg_label) = format_node_label( i_code = <line>-code ).
      CV_MM_STRING = |{ CV_MM_STRING }{ ind }{ box_s }"{ lv_reg_label }"{ box_e }\n|.
      pre_stack = <line>.

    ENDLOOP.

    " Close any remaining open subgraphs
    DO opened TIMES.
      CV_MM_STRING = |{ CV_MM_STRING } end\n|.
    ENDDO.

  endmethod.


  method BUILD_EDGES.

    " IT_LINES contains only non-LOOP/DO/WHILE lines (already filtered by caller).
    " Fields used:
    "   ind        - sequential index within results (set in GET_CODE_FLOW)
    "   cond       - IF / ELSE / ELSEIF / ENDIF / CASE / WHEN / ENDCASE / <empty>
    "   ev_name    - event/method name this line belongs to
    "   stack      - call stack level (1 = root event, no incoming arrow from another context)
    "   els_before - ind of node before this ELSE/ELSEIF/WHEN (set in GET_CODE_FLOW)
    "   els_after  - ind of first real node after this branch (set in GET_CODE_FLOW)
    "   arrow      - label for edge (variable assignments)
    " mt_if (from mo_viewer) - stack of IF/CASE structures: if_ind, end_ind

    " Work with a local copy so we don't corrupt shared state
    DATA(lt_if) = mo_viewer->mt_if.

    DATA: if_stack   TYPE TABLE OF i,      " stack of indices into lt_if
          if_ptr     TYPE i,               " current index in lt_if
          pre_ind    TYPE i,               " ind of previous drawable node
          pre_cond   TYPE string,          " cond of previous node
          pre_ev     TYPE string,          " ev_name of previous drawable node
          sub        TYPE string,          " edge label
          last_els   TYPE i.               " last els_after handled (to skip duplicate edges)

    " Track which (stack=1, ev_name) contexts have already had at least one node
    " drawn. Only the very first node of a root context must not get an incoming
    " cross-context arrow; subsequent nodes in the same context may receive arrows
    " from nodes that temporarily "dipped" into a deeper ev_name (subgraph call).
    DATA lt_started_ev TYPE TABLE OF string WITH EMPTY KEY.

    LOOP AT IT_LINES INTO DATA(line).

      " Skip LOOP/DO/WHILE and their END* — they are subgraphs, not nodes
      IF line-cond = 'LOOP' OR line-cond = 'DO'    OR line-cond = 'WHILE' OR
         line-cond = 'ENDLOOP' OR line-cond = 'ENDDO' OR line-cond = 'ENDWHILE'.
        CONTINUE.
      ENDIF.

      " ----- IF / CASE: push onto stack -----
      IF line-cond = 'IF' OR line-cond = 'CASE'.
        if_ptr += 1.
        READ TABLE lt_if INDEX if_ptr INTO DATA(ls_if).
        APPEND if_ptr TO if_stack.
      ENDIF.

      " ----- ENDIF / ENDCASE: pop stack -----
      IF line-cond = 'ENDIF' OR line-cond = 'ENDCASE'.
        DATA(lv_top) = 0.
        READ TABLE if_stack INDEX lines( if_stack ) INTO lv_top.
        IF sy-subrc = 0.
          DELETE if_stack INDEX lines( if_stack ).
          " re-read current top after pop
          READ TABLE if_stack INDEX lines( if_stack ) INTO lv_top.
          IF sy-subrc = 0.
            READ TABLE lt_if INDEX lv_top INTO ls_if.
          ELSE.
            CLEAR ls_if.
          ENDIF.
        ENDIF.
        " draw edge from last node before ENDIF to ENDIF node
        IF pre_ind > 0 AND pre_cond <> 'ELSE' AND pre_cond <> 'ELSEIF' AND pre_cond <> 'WHEN'
           AND NOT ( last_els = line-ind ).
          " Block only the very first appearance of a root-level (stack=1) ev_name context
          DATA(lv_block_endif) = abap_false.
          IF line-stack = 1.
            READ TABLE lt_started_ev WITH KEY table_line = line-ev_name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              lv_block_endif = abap_true.
            ENDIF.
          ENDIF.
          IF lv_block_endif = abap_false.
            CV_MM_STRING = |{ CV_MM_STRING }{ pre_ind }-->{ sub }{ line-ind }\n|.
            CLEAR sub.
          ENDIF.
        ENDIF.
        " Mark this ev_name as started once we output (or skip) its first node
        IF line-stack = 1.
          READ TABLE lt_started_ev WITH KEY table_line = line-ev_name TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0. APPEND line-ev_name TO lt_started_ev. ENDIF.
        ENDIF.
        pre_ind  = line-ind.
        pre_cond = line-cond.
        pre_ev   = line-ev_name.
        CONTINUE.
      ENDIF.

      " ----- ELSE / ELSEIF / WHEN: branch edges -----
      IF line-cond = 'ELSE' OR line-cond = 'ELSEIF' OR line-cond = 'WHEN'.

        " get current IF/CASE node
        READ TABLE if_stack INDEX lines( if_stack ) INTO lv_top.
        IF sy-subrc = 0.
          READ TABLE lt_if INDEX lv_top INTO ls_if.
        ENDIF.

        DATA(bool) = '|' && line-code && '|'.

        " edge from IF/CASE head to branch target
        IF line-els_after IS NOT INITIAL AND line-els_after > 0.
          CV_MM_STRING = |{ CV_MM_STRING }{ ls_if-if_ind }-->{ bool }{ line-els_after }\n|.
          last_els = line-els_after.
        ELSE.
          " no nodes in this branch — edge to ENDIF/ENDCASE
          IF ls_if-end_ind > 0.
            CV_MM_STRING = |{ CV_MM_STRING }{ ls_if-if_ind }-->{ bool }{ ls_if-end_ind }\n|.
          ENDIF.
        ENDIF.

        " edge from previous branch's last node to ENDIF/ENDCASE (fall-through)
        IF line-els_before IS NOT INITIAL AND line-els_before <> ls_if-if_ind AND ls_if-end_ind > 0.
          CV_MM_STRING = |{ CV_MM_STRING }{ line-els_before }-->{ ls_if-end_ind }\n|.
        ENDIF.

        " if next node is not ENDIF/ENDCASE, reset pre so next regular node
        " doesn't get a spurious edge from IF head
        DATA(lv_next_ind) = line-ind + 1.
        READ TABLE IT_LINES WITH KEY ind = lv_next_ind INTO DATA(next_line).
        IF sy-subrc = 0
           AND next_line-cond <> 'ENDIF'
           AND next_line-cond <> 'ENDCASE'.
          CLEAR pre_ind.
        ELSE.
          pre_ind  = line-ind.
        ENDIF.
        pre_cond = line-cond.
        pre_ev   = line-ev_name.
        CLEAR sub.
        CONTINUE.
      ENDIF.

      " ----- Regular node -----
      IF pre_ind > 0
         AND pre_cond <> 'ELSE' AND pre_cond <> 'ELSEIF' AND pre_cond <> 'WHEN'
         AND NOT ( last_els = line-ind ).
        " Block only the very first node of a root-level (stack=1) context when
        " it is being encountered for the first time — i.e. it has no predecessor
        " within its own ev_name yet. Subsequent appearances of the same ev_name
        " (e.g. after returning from a nested subgraph call) are allowed arrows.
        DATA(lv_block) = abap_false.
        IF line-stack = 1.
          READ TABLE lt_started_ev WITH KEY table_line = line-ev_name TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            " First node of this root context — no incoming cross-context arrow
            lv_block = abap_true.
          ENDIF.
        ENDIF.
        IF lv_block = abap_false.
          CV_MM_STRING = |{ CV_MM_STRING }{ pre_ind }-->{ sub }{ line-ind }\n|.
        ENDIF.
      ENDIF.

      " Mark this ev_name as started once we process its first node
      IF line-stack = 1.
        READ TABLE lt_started_ev WITH KEY table_line = line-ev_name TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0. APPEND line-ev_name TO lt_started_ev. ENDIF.
      ENDIF.

      sub = COND string( WHEN line-arrow IS NOT INITIAL THEN '|"' && line-arrow && '"|' ).
      pre_ind  = line-ind.
      pre_cond = line-cond.
      pre_ev   = line-ev_name.

    ENDLOOP.

  endmethod.


  METHOD magic_search.

    DATA: mm_string TYPE string,
          direction TYPE string.

    CLEAR mo_viewer->mt_if.
    DATA(lines) = mo_viewer->get_code_flow( i_calc_path = i_calc_path ).
    "CHECK lines IS NOT INITIAL.

    direction = COND string(
      WHEN i_direction IS NOT INITIAL THEN i_direction
      WHEN lines( lines ) < 100       THEN 'LR'
      ELSE                                 'TB' ).

    mm_string = |graph { direction }\n |.

    build_nodes( EXPORTING i_direction = direction
                 CHANGING  ct_lines = lines cv_mm_string = mm_string ).

    build_edges( EXPORTING it_lines = lines
                 CHANGING  cv_mm_string = mm_string ).

    " --- Highlight active blocks (active_root = X) with a light-blue fill ---
    DATA: lv_active_ids TYPE string.
    LOOP AT lines INTO DATA(line) WHERE active_root = abap_true AND cond <> 'ELSE'
                                                                AND cond <> 'ELSEIF'
                                                                AND cond <> 'WHEN'.
      IF lv_active_ids IS INITIAL.
        lv_active_ids = |{ line-ind }|.
      ELSE.
        lv_active_ids = |{ lv_active_ids },{ line-ind }|.
      ENDIF.
    ENDLOOP.

    IF lv_active_ids IS NOT INITIAL.
      mm_string = |{ mm_string }\nclassDef activeNode fill:#AEE6FF,stroke:#3399CC,color:#000\n|.
      mm_string = |{ mm_string }class { lv_active_ids } activeNode\n|.
    ENDIF.

    mm_string = |{ mm_string }\n|.
    open_mermaid( mm_string ).

  ENDMETHOD.


  METHOD class_map.

    TYPES: BEGIN OF lty_meth,
             class      TYPE string,
             real_class TYPE string,
             disp_class TYPE string,
             name      TYPE string,
             disp_name TYPE string,
             program   TYPE program,
             include   TYPE program,
             meth_type TYPE i,
             stmt_from TYPE i,
             stmt_to   TYPE i,
             row_from  TYPE i,
             row_to    TYPE i,
             node_id   TYPE string,
             agg_id    TYPE string,
           END OF lty_meth,
           BEGIN OF lty_edge,
             from_id  TYPE string,
             to_id    TYPE string,
             external TYPE abap_bool,
           END OF lty_edge,
           BEGIN OF lty_ext,
             node_id TYPE string,
             label   TYPE string,
             key     TYPE string,
           END OF lty_ext,
           BEGIN OF lty_nmap,
             kind    TYPE string,
             name    TYPE string,
             node_id TYPE string,
           END OF lty_nmap,
           BEGIN OF lty_rank,
             node_id TYPE string,
             rank    TYPE i,
           END OF lty_rank.

    DATA: mm_string  TYPE string,
          direction  TYPE string,
          lt_meth    TYPE STANDARD TABLE OF lty_meth WITH DEFAULT KEY,
          lt_edge    TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY,
          lt_ext     TYPE STANDARD TABLE OF lty_ext  WITH DEFAULT KEY,
          lt_cls     TYPE STANDARD TABLE OF string   WITH DEFAULT KEY,
          lv_to      TYPE i,
          lv_rf      TYPE i,
          lv_rt      TYPE i,
          lv_sel     TYPE i,
          lv_sel_id  TYPE string,
          lv_ext_seq TYPE i,
          lv_sgseq   TYPE i.

    direction = COND string( WHEN i_direction IS NOT INITIAL THEN i_direction ELSE 'LR' ).
    DATA(lo_win) = mo_viewer->mo_window.
    DATA(lv_focus) = mo_viewer->mv_cmap_focus.
    DATA(lv_focus_prog) = VALUE progname( ).

    IF lv_focus IS NOT INITIAL AND mo_viewer->mv_package IS NOT INITIAL AND mo_viewer->mt_pkg_objects IS INITIAL.
      mo_viewer->ensure_package_parsed( ).
    ENDIF.

    IF lv_focus IS NOT INITIAL.
      READ TABLE mo_viewer->mt_pkg_objects INTO DATA(ls_focus_obj) WITH KEY prog = lv_focus.
      IF sy-subrc = 0 AND ls_focus_obj-obj_type = 'PROG'.
        lv_focus_prog = lv_focus.
        CLEAR lv_focus.
      ENDIF.
    ENDIF.

    " Package mode: no focused class -> build the whole package (parse everything once).
    " After a double-click on a class, mv_cmap_focus scopes the graph to that class only.
    IF lv_focus IS INITIAL.
      mo_viewer->ensure_package_parsed( ).
    ENDIF.

    DATA(lv_pkg_mode) = xsdbool( lv_focus IS INITIAL
                            AND ( mo_viewer->mv_package IS NOT INITIAL
                               OR mo_viewer->mt_pkg_objects IS NOT INITIAL ) ).

    " --- 1. Collect all METHOD units directly from the parsed scan
    "     (METHOD..ENDMETHOD in t_keywords). This does NOT depend on
    "     tt_calls_line-index, which is only filled where code_execution_scanner
    "     has actually traced, so plain-parsed package classes are covered too. ---
    LOOP AT lo_win->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
      IF lv_focus IS NOT INITIAL AND <prog>-program <> lv_focus.
        CONTINUE.
      ENDIF.
      DATA(lo_scan) = <prog>-scan.
      CHECK lo_scan IS BOUND.

      " class name = program stripped of the =...CP / =...IP class-pool suffix
      DATA(lv_cls) = CONV string( <prog>-program ).
      REPLACE REGEX '=+(CP|IP)$' IN lv_cls WITH ``.

      LOOP AT <prog>-t_keywords INTO DATA(ls_mkw) WHERE name = 'METHOD'.
        READ TABLE lo_scan->tokens INDEX ls_mkw-from + 1 INTO DATA(ls_mtok).
        CHECK sy-subrc = 0 AND ls_mtok-str IS NOT INITIAL.

        lv_to = 0.
        LOOP AT <prog>-t_keywords INTO DATA(ls_ekw)
          WHERE index > ls_mkw-index AND name = 'ENDMETHOD'.
          lv_to = ls_ekw-index.
          EXIT.
        ENDLOOP.
        CHECK lv_to > 0.

        CLEAR: lv_rf, lv_rt.
        READ TABLE lo_scan->tokens INDEX ls_mkw-from INTO DATA(ls_tf).
        IF sy-subrc = 0. lv_rf = ls_tf-row. ENDIF.
        READ TABLE <prog>-t_keywords WITH KEY index = lv_to INTO ls_ekw.
        READ TABLE lo_scan->tokens INDEX ls_ekw-to INTO DATA(ls_tt).
        IF sy-subrc = 0. lv_rt = ls_tt-row. ENDIF.

        APPEND VALUE #( class     = lv_cls
                        real_class = lv_cls
                        name      = ls_mtok-str
                        program   = <prog>-program
                        include   = <prog>-include
                        stmt_from = ls_mkw-index
                        stmt_to   = lv_to
                        row_from  = lv_rf
                        row_to    = lv_rt ) TO lt_meth.
      ENDLOOP.

      IF lv_pkg_mode = abap_true AND lv_cls = CONV string( <prog>-program ).
        READ TABLE mo_viewer->mt_pkg_objects INTO DATA(ls_prog_obj) WITH KEY prog = <prog>-program.
        IF sy-subrc = 0 AND ls_prog_obj-obj_type = 'PROG'
           AND to_upper( CONV string( <prog>-program ) ) <> 'Z_ACE_STANDALONE'
           AND NOT line_exists( lo_win->ms_sources-tt_class_defs[ program = <prog>-program ] ).
          DATA(lv_first_stmt) = 0.
          DATA(lv_last_stmt)  = 0.
          LOOP AT <prog>-t_keywords INTO DATA(ls_pkw).
            IF lv_first_stmt = 0. lv_first_stmt = ls_pkw-index. ENDIF.
            lv_last_stmt = ls_pkw-index.
          ENDLOOP.
          IF lv_first_stmt > 0 AND lv_last_stmt > 0.
            APPEND VALUE #( class      = ''
                            real_class = ''
                            name       = COND string( WHEN ls_prog_obj-obj_name IS NOT INITIAL
                                                      THEN ls_prog_obj-obj_name
                                                      ELSE <prog>-program )
                            program    = <prog>-program
                            include    = <prog>-include
                            stmt_from  = lv_first_stmt
                            stmt_to    = lv_last_stmt ) TO lt_meth.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lt_meth IS INITIAL.
      open_mermaid( |graph { direction }\n  none["No methods found"]\n| ).
      RETURN.
    ENDIF.

    LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<m>).
      <m>-node_id = |M{ sy-tabix }|.
    ENDLOOP.

    " --- Package overview (no focus): aggregate to program/class nodes.
    "     Methods remain only as analysis points for parsing their calls. ---
    IF lv_pkg_mode = abap_true.
      DATA lt_nmap TYPE HASHED TABLE OF lty_nmap WITH UNIQUE KEY kind name.
      TYPES: BEGIN OF lty_pmap,
               program TYPE program,
               node_id TYPE string,
             END OF lty_pmap.
      DATA lt_pmap TYPE HASHED TABLE OF lty_pmap WITH UNIQUE KEY program.
      DATA lt_hidden_prog TYPE HASHED TABLE OF program WITH UNIQUE KEY table_line.
      DATA lv_nseq TYPE i.
      DATA lv_pseq TYPE i.
      DATA ls_po   TYPE zif_ace_parse_data=>ts_pkg_obj.
      DATA lt_pub    TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      DATA lt_cls_ok TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

      " Diagram-only exclusions: keep these programs in package parsing/tree.
      LOOP AT lt_meth INTO DATA(ls_lm).
        DATA(lv_lowner) = ls_lm-program.
        READ TABLE lo_win->ms_sources-tt_progs WITH KEY include = ls_lm-include INTO DATA(ls_lop).
        IF sy-subrc = 0 AND ls_lop-program IS NOT INITIAL. lv_lowner = ls_lop-program. ENDIF.
        CLEAR ls_po.
        READ TABLE mo_viewer->mt_pkg_objects INTO ls_po WITH KEY prog = lv_lowner.
        IF sy-subrc = 0 AND ls_po-obj_type = 'PROG'
           AND ( to_upper( CONV string( lv_lowner ) ) = 'Z_ACE_STANDALONE'
              OR line_exists( lo_win->ms_sources-tt_class_defs[ program = lv_lowner ] ) ).
          INSERT lv_lowner INTO TABLE lt_hidden_prog.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<pm>).
        " Resolve the real owning program via the include (tt_calls_line-program
        " may be empty for local classes, so include -> tt_progs is authoritative)
        DATA(lv_owner) = <pm>-program.
        READ TABLE lo_win->ms_sources-tt_progs WITH KEY include = <pm>-include INTO DATA(ls_op).
        IF sy-subrc = 0 AND ls_op-program IS NOT INITIAL.
          lv_owner = ls_op-program.
        ENDIF.
        <pm>-program = lv_owner.
        IF line_exists( lt_hidden_prog[ table_line = lv_owner ] ).
          <pm>-node_id = ''.
          CONTINUE.
        ENDIF.

        CLEAR ls_po.
        READ TABLE mo_viewer->mt_pkg_objects INTO ls_po WITH KEY prog = lv_owner.
        DATA(lv_is_class) = xsdbool( sy-subrc = 0
                             AND ( ls_po-obj_type = 'CLAS' OR ls_po-obj_type = 'INTF' ) ).
        IF lv_is_class = abap_true.
          " global class / interface method — keep only public ones in the overview.
          " If RTTI knew this class, honour it; otherwise fall back to keeping the method.
          IF <pm>-name NS '~' AND line_exists( lt_cls_ok[ table_line = <pm>-class ] ).
            " interface methods (name~method) are always public — keep them
            READ TABLE lt_pub WITH KEY table_line = |{ <pm>-class }~{ <pm>-name }|
              TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0. <pm>-node_id = ''. ENDIF.
          ENDIF.
        ELSE.
          " program / function group / local class inside a report — collapse to one block
          READ TABLE lt_pmap WITH KEY program = lv_owner INTO DATA(ls_pm).
          IF sy-subrc <> 0.
            lv_pseq += 1.
            ls_pm-program = lv_owner.
            ls_pm-node_id = |P{ lv_pseq }|.
            INSERT ls_pm INTO TABLE lt_pmap.
          ENDIF.
          <pm>-class   = 'Programs'.
          <pm>-disp_name = COND string( WHEN ls_po-obj_name IS NOT INITIAL THEN ls_po-obj_name ELSE lv_owner ).
          <pm>-node_id = ls_pm-node_id.
        ENDIF.

        DATA(lv_kind) = COND string( WHEN lv_is_class = abap_true THEN 'CLAS' ELSE 'PROG' ).
        DATA(lv_name) = COND string(
          WHEN lv_is_class = abap_true AND ls_po-obj_name IS NOT INITIAL THEN ls_po-obj_name
          WHEN lv_is_class = abap_true THEN <pm>-class
          WHEN ls_po-obj_name IS NOT INITIAL THEN ls_po-obj_name
          ELSE lv_owner ).
        READ TABLE lt_nmap INTO DATA(ls_nm) WITH KEY kind = lv_kind name = lv_name.
        IF sy-subrc <> 0.
          lv_nseq += 1.
          ls_nm-kind    = lv_kind.
          ls_nm-name    = lv_name.
          ls_nm-node_id = |N{ lv_nseq }|.
          INSERT ls_nm INTO TABLE lt_nmap.
        ENDIF.
        <pm>-disp_class = COND string( WHEN lv_is_class = abap_true THEN 'Classes' ELSE 'Programs' ).
        <pm>-disp_name = lv_name.
        <pm>-agg_id  = ls_nm-node_id.
      ENDLOOP.
      DELETE lt_meth WHERE node_id IS INITIAL.
      IF lt_meth IS INITIAL.
        open_mermaid( |graph { direction }\n  none["No package objects found"]\n| ).
        RETURN.
      ENDIF.
    ENDIF.

    " --- 2. Selected method from cursor position (m_prg-line inside a method) ---
    lv_sel = 0.
    IF lv_pkg_mode = abap_false.
      LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<ms>)
        WHERE include = lo_win->m_prg-include AND row_from > 0.
        IF lo_win->m_prg-line >= <ms>-row_from AND lo_win->m_prg-line <= <ms>-row_to.
          lv_sel    = sy-tabix.
          lv_sel_id = <ms>-node_id.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " --- 3. Collect edges: parse calls on demand for each method in scope ---
    lv_ext_seq = 0.
    LOOP AT lt_meth INTO DATA(ls_meth).
      IF lv_sel > 0 AND ls_meth-node_id <> lv_sel_id. CONTINUE. ENDIF.

      LOOP AT lo_win->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog2>)
        WHERE include = ls_meth-include.

        LOOP AT <prog2>-t_keywords INTO DATA(ls_key)
          WHERE index >= ls_meth-stmt_from AND index <= ls_meth-stmt_to.

          IF ls_key-calls_parsed = abap_false.
            zcl_ace_parser=>parse_tokens(
              EXPORTING
                i_program  = CONV #( ls_key-program )
                i_include  = CONV #( ls_key-include )
                i_stmt_idx = ls_key-index
                i_class    = COND string( WHEN ls_meth-real_class IS NOT INITIAL THEN ls_meth-real_class ELSE ls_meth-class )
                i_evtype   = 'METHOD'
                i_ev_name  = ls_meth-name
              CHANGING
                cs_source  = lo_win->ms_sources ).
            READ TABLE <prog2>-t_keywords WITH KEY index = ls_key-index INTO ls_key.
          ENDIF.

          LOOP AT ls_key-tt_calls INTO DATA(ls_call).
            CHECK ls_call-name IS NOT INITIAL.

            DATA lv_int TYPE i.
            lv_int = 0.
            IF ls_call-event = 'METHOD'.
              IF ls_call-class IS NOT INITIAL.
                DATA(lv_call_class) = to_upper( CONV string( ls_call-class ) ).
                REPLACE REGEX '=+(CP|IP)$' IN lv_call_class WITH ``.
                LOOP AT lt_meth INTO DATA(ls_tgt)
                  WHERE name = ls_call-name.
                  CHECK to_upper( ls_tgt-real_class ) = lv_call_class.
                  lv_int = sy-tabix.
                  EXIT.
                ENDLOOP.
              ENDIF.
              " prefer a target in the same program (handles duplicate method names)
              IF lv_int = 0.
                LOOP AT lt_meth INTO ls_tgt
                  WHERE name = ls_call-name AND program = ls_meth-program.
                  lv_int = sy-tabix.
                  EXIT.
                ENDLOOP.
              ENDIF.
              IF lv_int = 0.
                LOOP AT lt_meth INTO ls_tgt WHERE name = ls_call-name.
                  lv_int = sy-tabix.
                  EXIT.
                ENDLOOP.
              ENDIF.
            ENDIF.

            IF lv_int > 0.
              READ TABLE lt_meth INTO ls_tgt INDEX lv_int.
              CHECK ls_tgt-node_id <> ls_meth-node_id.       " no self-loops
              APPEND VALUE #( from_id  = ls_meth-node_id
                              to_id    = ls_tgt-node_id
                              external = abap_false ) TO lt_edge.
            ELSE.
              DATA(lv_key) = |{ ls_call-event }:{ ls_call-class }:{ ls_call-name }|.
              READ TABLE lt_ext INTO DATA(ls_e) WITH KEY key = lv_key.
              IF sy-subrc <> 0.
                lv_ext_seq = lv_ext_seq + 1.
                CLEAR ls_e.
                ls_e-node_id = |E{ lv_ext_seq }|.
                ls_e-key     = lv_key.
                ls_e-label   = COND string(
                  WHEN ls_call-class IS NOT INITIAL
                  THEN |{ ls_call-class }: { ls_call-name }|
                  ELSE |{ ls_call-event } { ls_call-name }| ).
                APPEND ls_e TO lt_ext.
              ENDIF.
              APPEND VALUE #( from_id  = ls_meth-node_id
                              to_id    = ls_e-node_id
                              external = abap_true ) TO lt_edge.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    IF lv_pkg_mode = abap_true.
      TYPES: BEGIN OF lty_call_stack,
               stacklevel TYPE i,
               node_id    TYPE string,
             END OF lty_call_stack.
      DATA lt_flow_edge TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY.
      DATA lt_method_edge TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY.
      DATA lt_static_edge TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY.
      DATA lt_call_stack TYPE STANDARD TABLE OF lty_call_stack WITH DEFAULT KEY.
      DATA lt_pkg_root_prog TYPE HASHED TABLE OF program WITH UNIQUE KEY table_line.
      DATA lv_flow_used TYPE abap_bool.

      lt_method_edge = lt_edge.

      IF mv_all_methods = abap_false.
        LOOP AT lt_meth INTO DATA(ls_root_meth)
          WHERE node_id IS NOT INITIAL AND disp_class = 'Programs'.
          INSERT ls_root_meth-program INTO TABLE lt_pkg_root_prog.
        ENDLOOP.
      ENDIF.

      IF mv_all_methods = abap_false AND lt_pkg_root_prog IS NOT INITIAL.
        DATA(lv_old_depth) = lo_win->m_hist_depth.
        DATA(lt_old_steps) = mo_viewer->mt_steps.
        DATA(lv_old_step)  = mo_viewer->m_step.
        DATA(lt_old_stack) = lo_win->mt_stack.
        DATA(lt_old_calls) = lo_win->mt_calls.

        LOOP AT lt_pkg_root_prog INTO DATA(lv_root_prog).
          CLEAR: mo_viewer->mt_steps, mo_viewer->m_step, lo_win->mt_stack, lo_win->mt_calls, lt_call_stack.
          zcl_ace_source_parser=>code_execution_scanner(
            i_program = lv_root_prog
            i_include = lv_root_prog
            io_debugger = mo_viewer ).

          LOOP AT mo_viewer->mt_steps INTO DATA(ls_flow_step).
            DATA(lv_flow_node) = ``.
            IF ls_flow_step-eventtype = 'METHOD' AND ls_flow_step-class IS NOT INITIAL.
              DATA(lv_step_class) = to_upper( CONV string( ls_flow_step-class ) ).
              REPLACE REGEX '=+(CP|IP)$' IN lv_step_class WITH ``.
              LOOP AT lt_meth INTO DATA(ls_flow_meth)
                WHERE agg_id IS NOT INITIAL.
                CHECK to_upper( ls_flow_meth-real_class ) = lv_step_class.
                lv_flow_node = ls_flow_meth-agg_id.
                EXIT.
              ENDLOOP.
            ELSE.
              READ TABLE lt_meth INTO DATA(ls_flow_prog)
                WITH KEY program = ls_flow_step-program disp_class = 'Programs'.
              IF sy-subrc = 0.
                lv_flow_node = ls_flow_prog-agg_id.
              ENDIF.
            ENDIF.
            IF lv_flow_node IS INITIAL. CONTINUE. ENDIF.

            IF ls_flow_step-stacklevel > 1.
              DATA(lv_parent_stack) = ls_flow_step-stacklevel - 1.
              READ TABLE lt_call_stack INTO DATA(ls_flow_caller)
                WITH KEY stacklevel = lv_parent_stack.
              IF sy-subrc = 0 AND ls_flow_caller-node_id <> lv_flow_node.
                APPEND VALUE #( from_id  = ls_flow_caller-node_id
                                to_id    = lv_flow_node
                                external = abap_false ) TO lt_flow_edge.
              ENDIF.
            ENDIF.

            DELETE lt_call_stack WHERE stacklevel >= ls_flow_step-stacklevel.
            APPEND VALUE #( stacklevel = ls_flow_step-stacklevel
                            node_id    = lv_flow_node ) TO lt_call_stack.
          ENDLOOP.
        ENDLOOP.

        lo_win->m_hist_depth = lv_old_depth.
        mo_viewer->mt_steps = lt_old_steps.
        mo_viewer->m_step   = lv_old_step.
        lo_win->mt_stack    = lt_old_stack.
        lo_win->mt_calls    = lt_old_calls.
        IF lt_flow_edge IS NOT INITIAL.
          lt_edge = lt_flow_edge.
          lv_flow_used = abap_true.
        ENDIF.
      ENDIF.

      LOOP AT lt_method_edge INTO DATA(ls_edge_raw).
        READ TABLE lt_meth INTO DATA(ls_from_meth) WITH KEY node_id = ls_edge_raw-from_id.
        IF sy-subrc <> 0 OR ls_from_meth-agg_id IS INITIAL. CONTINUE. ENDIF.
        IF ls_edge_raw-external = abap_true.
          APPEND VALUE #( from_id  = ls_from_meth-agg_id
                          to_id    = ls_edge_raw-to_id
                          external = abap_true ) TO lt_static_edge.
        ELSE.
          READ TABLE lt_meth INTO DATA(ls_to_meth) WITH KEY node_id = ls_edge_raw-to_id.
          IF sy-subrc <> 0 OR ls_to_meth-agg_id IS INITIAL. CONTINUE. ENDIF.
          CHECK ls_from_meth-agg_id <> ls_to_meth-agg_id.
          APPEND VALUE #( from_id  = ls_from_meth-agg_id
                          to_id    = ls_to_meth-agg_id
                          external = abap_false ) TO lt_static_edge.
        ENDIF.
      ENDLOOP.

      lt_edge = lt_static_edge.
      IF lv_flow_used = abap_true.
        APPEND LINES OF lt_flow_edge TO lt_edge.
      ENDIF.

      DATA lv_agg_cnt TYPE i.
      lv_agg_cnt = 0.
      LOOP AT lt_meth TRANSPORTING NO FIELDS WHERE agg_id IS NOT INITIAL.
        lv_agg_cnt += 1.
      ENDLOOP.
      MESSAGE |DBG map: allm={ mv_all_methods } meth={ lines( lt_meth ) } agg={ lv_agg_cnt } raw={ lines( lt_method_edge ) } static={ lines( lt_static_edge ) } flow={ lines( lt_flow_edge ) }| TYPE 'I'.

      LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<ma>) WHERE agg_id IS NOT INITIAL.
        <ma>-node_id = <ma>-agg_id.
      ENDLOOP.

      IF lv_focus_prog IS NOT INITIAL.
        DATA lt_reach TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
        DATA lt_edge_focus TYPE STANDARD TABLE OF lty_edge WITH DEFAULT KEY.
        READ TABLE lt_meth INTO DATA(ls_focus_meth)
          WITH KEY program = lv_focus_prog disp_class = 'Programs'.
        IF sy-subrc = 0 AND ls_focus_meth-node_id IS NOT INITIAL.
          INSERT ls_focus_meth-node_id INTO TABLE lt_reach.
          DATA(lv_reach_changed) = abap_true.
          WHILE lv_reach_changed = abap_true.
            lv_reach_changed = abap_false.
            LOOP AT lt_edge INTO DATA(ls_reach_edge) WHERE external = abap_false.
              READ TABLE lt_reach WITH KEY table_line = ls_reach_edge-from_id TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0. CONTINUE. ENDIF.
              READ TABLE lt_reach WITH KEY table_line = ls_reach_edge-to_id TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                INSERT ls_reach_edge-to_id INTO TABLE lt_reach.
                lv_reach_changed = abap_true.
              ENDIF.
            ENDLOOP.
          ENDWHILE.

          LOOP AT lt_edge INTO DATA(ls_focus_edge).
            READ TABLE lt_reach WITH KEY table_line = ls_focus_edge-from_id TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0. CONTINUE. ENDIF.
            IF ls_focus_edge-external = abap_false.
              READ TABLE lt_reach WITH KEY table_line = ls_focus_edge-to_id TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0. CONTINUE. ENDIF.
            ENDIF.
            APPEND ls_focus_edge TO lt_edge_focus.
          ENDLOOP.
          lt_edge = lt_edge_focus.

          LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<mf>) WHERE node_id IS NOT INITIAL.
            READ TABLE lt_reach WITH KEY table_line = <mf>-node_id TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0. CLEAR <mf>-node_id. ENDIF.
          ENDLOOP.
          DELETE lt_meth WHERE node_id IS INITIAL.
        ENDIF.
      ENDIF.

      DATA lt_rank TYPE HASHED TABLE OF lty_rank WITH UNIQUE KEY node_id.
      LOOP AT lt_meth INTO DATA(ls_rank_meth)
        WHERE node_id IS NOT INITIAL AND disp_class = 'Programs'.
        INSERT VALUE #( node_id = ls_rank_meth-node_id rank = 0 ) INTO TABLE lt_rank.
      ENDLOOP.

      DATA(lv_changed) = abap_true.
      WHILE lv_changed = abap_true.
        lv_changed = abap_false.
        LOOP AT lt_edge INTO DATA(ls_rank_edge) WHERE external = abap_false.
          READ TABLE lt_rank INTO DATA(ls_rank_from) WITH KEY node_id = ls_rank_edge-from_id.
          IF sy-subrc <> 0. CONTINUE. ENDIF.
          READ TABLE lt_rank INTO DATA(ls_rank_to) WITH KEY node_id = ls_rank_edge-to_id.
          IF sy-subrc <> 0.
            INSERT VALUE #( node_id = ls_rank_edge-to_id rank = ls_rank_from-rank + 1 ) INTO TABLE lt_rank.
            lv_changed = abap_true.
          ELSEIF ls_rank_to-rank > ls_rank_from-rank + 1.
            DELETE TABLE lt_rank WITH TABLE KEY node_id = ls_rank_edge-to_id.
            INSERT VALUE #( node_id = ls_rank_edge-to_id rank = ls_rank_from-rank + 1 ) INTO TABLE lt_rank.
            lv_changed = abap_true.
          ENDIF.
        ENDLOOP.
      ENDWHILE.

      LOOP AT lt_meth ASSIGNING FIELD-SYMBOL(<mr>)
        WHERE node_id IS NOT INITIAL AND disp_class = 'Classes'.
        READ TABLE lt_rank INTO DATA(ls_rank_node) WITH KEY node_id = <mr>-node_id.
        IF sy-subrc = 0 AND ls_rank_node-rank > 0.
          <mr>-disp_class = |Classes L{ ls_rank_node-rank }|.
        ELSE.
          <mr>-disp_class = 'Classes other'.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT lt_edge BY from_id to_id external.
    DELETE ADJACENT DUPLICATES FROM lt_edge COMPARING from_id to_id external.

    " --- 4. Render ---
    mm_string = |graph { direction }\n|.

    " which classes have visible internal nodes
    LOOP AT lt_meth INTO ls_meth.
      IF lv_sel > 0 AND ls_meth-node_id <> lv_sel_id
         AND line_index( lt_edge[ to_id = ls_meth-node_id ] ) = 0.
        CONTINUE.
      ENDIF.
      COLLECT COND string( WHEN ls_meth-disp_class IS NOT INITIAL THEN ls_meth-disp_class ELSE ls_meth-class ) INTO lt_cls.
    ENDLOOP.

    DATA lt_seen TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    DATA lv_node_label TYPE string.
    IF lv_pkg_mode = abap_true.
      LOOP AT lt_meth INTO ls_meth.
        IF ls_meth-disp_class <> 'Programs'. CONTINUE. ENDIF.
        READ TABLE lt_seen WITH KEY table_line = ls_meth-node_id TRANSPORTING NO FIELDS.
        IF sy-subrc = 0. CONTINUE. ENDIF.
        INSERT ls_meth-node_id INTO TABLE lt_seen.
        lv_node_label = COND string( WHEN ls_meth-disp_name IS NOT INITIAL THEN ls_meth-disp_name ELSE ls_meth-name ).
        mm_string = |{ mm_string }  { ls_meth-node_id }["{ replace( val = lv_node_label sub = `~` with = `-` ) }"]:::prog\n|.
      ENDLOOP.
      LOOP AT lt_cls INTO lv_cls.
        IF lv_cls = 'Programs'. CONTINUE. ENDIF.
        LOOP AT lt_meth INTO ls_meth.
          CHECK COND string( WHEN ls_meth-disp_class IS NOT INITIAL THEN ls_meth-disp_class ELSE ls_meth-class ) = lv_cls.
          READ TABLE lt_seen WITH KEY table_line = ls_meth-node_id TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. CONTINUE. ENDIF.
          INSERT ls_meth-node_id INTO TABLE lt_seen.
          lv_node_label = COND string( WHEN ls_meth-disp_name IS NOT INITIAL THEN ls_meth-disp_name ELSE ls_meth-name ).
          mm_string = |{ mm_string }  { ls_meth-node_id }["{ replace( val = lv_node_label sub = `~` with = `-` ) }"]:::cls\n|.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      LOOP AT lt_cls INTO lv_cls.
        lv_sgseq = lv_sgseq + 1.
        DATA(lv_title) = replace( val = COND string( WHEN lv_cls IS NOT INITIAL THEN lv_cls ELSE 'GLOBAL' )
                                  sub = `~` with = `-` ).
        mm_string = |{ mm_string }  subgraph SG{ lv_sgseq }["{ lv_title }"]\n|.
        LOOP AT lt_meth INTO ls_meth.
          CHECK COND string( WHEN ls_meth-disp_class IS NOT INITIAL THEN ls_meth-disp_class ELSE ls_meth-class ) = lv_cls.
          IF lv_sel > 0 AND ls_meth-node_id <> lv_sel_id
             AND line_index( lt_edge[ to_id = ls_meth-node_id ] ) = 0
             AND line_index( lt_edge[ from_id = ls_meth-node_id ] ) = 0.
            CONTINUE.
          ENDIF.
          READ TABLE lt_seen WITH KEY table_line = ls_meth-node_id TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. CONTINUE. ENDIF.
          INSERT ls_meth-node_id INTO TABLE lt_seen.
          DATA(lv_node_label2) = COND string( WHEN ls_meth-disp_name IS NOT INITIAL THEN ls_meth-disp_name ELSE ls_meth-name ).
          mm_string = |{ mm_string }    { ls_meth-node_id }["{ replace( val = lv_node_label2 sub = `~` with = `-` ) }"]\n|.
        ENDLOOP.
        mm_string = |{ mm_string }  end\n|.
      ENDLOOP.
    ENDIF.

    IF mv_show_ext = abap_true.
      LOOP AT lt_ext INTO ls_e.
        mm_string = |{ mm_string }  { ls_e-node_id }["{ replace( val = ls_e-label sub = `~` with = `-` ) }"]:::ext\n|.
      ENDLOOP.
    ENDIF.

    " At most one line between any two nodes (ignore direction / duplicates)
    DATA lt_pair TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    LOOP AT lt_edge INTO DATA(ls_ed).
      IF ls_ed-external = abap_true AND mv_show_ext = abap_false. CONTINUE. ENDIF.
      DATA(lv_pair) = COND string(
        WHEN ls_ed-from_id < ls_ed-to_id
        THEN |{ ls_ed-from_id }->{ ls_ed-to_id }|
        ELSE |{ ls_ed-to_id }->{ ls_ed-from_id }| ).
      IF line_exists( lt_pair[ table_line = lv_pair ] ). CONTINUE. ENDIF.
      INSERT lv_pair INTO TABLE lt_pair.
      mm_string = |{ mm_string }  { ls_ed-from_id } --> { ls_ed-to_id }\n|.
    ENDLOOP.

    IF mv_show_ext = abap_true.
      mm_string = |{ mm_string }classDef ext fill:#FDECEA,stroke:#E06666,color:#000\n|.
    ENDIF.
    IF lv_pkg_mode = abap_true.
      mm_string = |{ mm_string }classDef prog fill:#FFF2CC,stroke:#D6B656,color:#000\n|.
      mm_string = |{ mm_string }classDef cls fill:#E8E6FF,stroke:#9673A6,color:#000\n|.
    ENDIF.

    open_mermaid( mm_string ).

  ENDMETHOD.


  method ADD_TOOLBAR_BUTTONS.

      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.

      DATA(lv_depth) = mo_viewer->mo_window->m_hist_depth.

      button  = VALUE #(
       ( function = 'TB'       icon = CONV #( icon_view_expand_vertical )   quickinfo = 'Vertical'   text = '' )
       ( function = 'LR'       icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
       ( butn_type = 3 )
       ( function = 'CALLS'        icon = CONV #( icon_workflow_process ) quickinfo = 'Calls Flow'              text = 'Calls Flow' )
       ( function = 'FLOW'         icon = CONV #( icon_wizard )           quickinfo = 'Code Flow'               text = 'Code Flow' )
       ( function = 'CMAP'         icon = CONV #( icon_structure )        quickinfo = 'Static method call map'   text = 'Class Map' )
       ( butn_type = 3 )
       ( function = 'TOGGLE_CALC'  icon = CONV #( icon_biw_formula )      quickinfo = 'Toggle: show all steps / only calculated' text = 'Show All Steps' )
       ( function = 'TOGGLE_PARAMS' icon = CONV #( icon_parameter )       quickinfo = 'Toggle: show / hide call parameters'      text = 'Show Params' )
       ( function = 'TOGGLE_EXT'   icon = CONV #( icon_connect )          quickinfo = 'Toggle: show / hide external calls (Class Map)' text = 'Show External' )
       ( function = 'TOGGLE_ALLM'  icon = CONV #( icon_complete )         quickinfo = 'Toggle: include calls from all methods in Class Map' text = 'All Methods' )
       ( butn_type = 3 )
       ( function = 'DEPTH_M'  icon = CONV #( icon_arrow_left )            quickinfo = 'Decrease depth' text = '' )
       ( function = 'DEPTH'    icon = CONV #( icon_next_hierarchy_level )  quickinfo = 'Depth level' text = |Depth { lv_depth }| )
       ( function = 'DEPTH_P'  icon = CONV #( icon_arrow_right )           quickinfo = 'Increase depth' text = '' )
       ( butn_type = 3 )
       ( function = 'TEXT'     icon = CONV #( icon_wd_caption )            quickinfo = 'Mermaid Diagram text' text = '' )
       ( function = 'PERR'     icon = CONV #( icon_message_error_small )   quickinfo = 'Show last mermaid parse error' text = '' )
                      ).

      mo_toolbar->add_button_group( button ).

      event-eventid = cl_gui_toolbar=>m_id_function_selected.
      event-appl_event = space.
      APPEND event TO events.

      mo_toolbar->set_registered_events( events = events ).
      SET HANDLER me->hnd_toolbar FOR mo_toolbar.

  endmethod.


  method CONSTRUCTOR.

      DATA  text TYPE text100.

      super->constructor( ).

      mo_viewer = io_debugger.
      mv_type = i_type.

      CHECK ZCL_ACE=>i_mermaid_active = abap_true.

      CASE mv_type.
        WHEN 'CALLS'. text = 'Calls flow'.
        WHEN 'FLOW'.  text = 'Calculations sequence'.
        WHEN 'CMAP'.  text = 'Class map'.
      ENDCASE.

      IF mo_box IS INITIAL.
        mo_box = create( i_name = text i_width = 1000 i_hight = 300 ).

        APPEND INITIAL LINE TO ZCL_ACE=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
        <popup>-parent = mo_viewer->mo_window->mo_box.
        <popup>-child = mo_box.

        SET HANDLER on_box_close FOR mo_box.

        CREATE OBJECT mo_splitter
          EXPORTING parent = mo_box rows = 2 columns = 1
          EXCEPTIONS OTHERS = 1.

        mo_splitter->get_container( EXPORTING row = 2 column = 1 RECEIVING container = mo_mm_container ).
        mo_splitter->get_container( EXPORTING row = 1 column = 1 RECEIVING container = mo_mm_toolbar ).
        mo_splitter->set_row_height( id = 1 height = '3' ).
        mo_splitter->set_row_height( id = 2 height = '70' ).
        mo_splitter->set_row_sash( id = 1 type = 0 value = 0 ).

        CREATE OBJECT mo_toolbar EXPORTING parent = mo_mm_toolbar.
        add_toolbar_buttons( ).
        mo_toolbar->set_visible( 'X' ).
      ENDIF.

      CASE mv_type.
        WHEN 'CALLS'. steps_flow( i_with_params = mv_with_params i_calc_path = mv_calc_path ).
        WHEN 'FLOW'.  magic_search( i_calc_path = mv_calc_path ).
        WHEN 'CMAP'.  class_map( ).
      ENDCASE.

      IF mo_box IS NOT INITIAL.
        mo_box->set_focus( mo_box ).
      ENDIF.

  endmethod.


  method HND_TOOLBAR.

      IF fcode = 'TEXT'.
        DATA: mm_string TYPE string,
              ref       TYPE REF TO data.
        CALL METHOD mo_diagram->('GET_SOURCE_CODE_STRING') RECEIVING result = mm_string.
        GET REFERENCE OF mm_string INTO ref.
        NEW ZCL_ACE_TEXT_VIEWER( ref ).
        RETURN.
      ENDIF.

      IF fcode = 'PERR'.
        DATA: lv_perr TYPE string,
              ref_err TYPE REF TO data.
        TRY.
            CALL METHOD mo_diagram->('GET_LAST_PARSE_ERROR') RECEIVING result = lv_perr.
          CATCH cx_root.
        ENDTRY.
        IF lv_perr IS INITIAL.
          lv_perr = 'No mermaid parse error recorded (diagram rendered OK or not yet displayed).'.
        ENDIF.
        GET REFERENCE OF lv_perr INTO ref_err.
        NEW zcl_ace_text_viewer( ref_err ).
        RETURN.
      ENDIF.

      IF fcode = 'LR' OR fcode = 'TB'.
        mv_direction = fcode.
      ELSEIF fcode = 'TOGGLE_CALC'.
        mv_calc_path = COND #( WHEN mv_calc_path = abap_true THEN abap_false ELSE abap_true ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'TOGGLE_CALC'
                    text  = COND #( WHEN mv_calc_path = abap_true
                                    THEN 'Only Calculated'
                                    ELSE 'Show All Steps' ) ).
      ELSEIF fcode = 'TOGGLE_PARAMS'.
        mv_with_params = COND #( WHEN mv_with_params = abap_true THEN abap_false ELSE abap_true ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'TOGGLE_PARAMS'
                    text  = COND #( WHEN mv_with_params = abap_true
                                    THEN 'Hide Params'
                                    ELSE 'Show Params' ) ).
      ELSEIF fcode = 'TOGGLE_EXT'.
        mv_show_ext = COND #( WHEN mv_show_ext = abap_true THEN abap_false ELSE abap_true ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'TOGGLE_EXT'
                    text  = COND #( WHEN mv_show_ext = abap_true
                                    THEN 'Hide External'
                                    ELSE 'Show External' ) ).
      ELSEIF fcode = 'TOGGLE_ALLM'.
        mv_all_methods = COND #( WHEN mv_all_methods = abap_true THEN abap_false ELSE abap_true ).
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'TOGGLE_ALLM'
                    text  = COND #( WHEN mv_all_methods = abap_true
                                    THEN 'Path Only'
                                    ELSE 'All Methods' ) ).
      ELSEIF fcode = 'DEPTH_M'.
        IF mo_viewer->mo_window->m_hist_depth > 0.
          mo_viewer->mo_window->m_hist_depth -= 1.
        ENDIF.
        mo_viewer->mo_window->apply_depth( ).
        IF mo_box IS NOT INITIAL.
          mo_box->set_focus( mo_box ).
        ENDIF.
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'DEPTH'
                    text  = |Depth { mo_viewer->mo_window->m_hist_depth }| ).
        RETURN.
      ELSEIF fcode = 'DEPTH_P'.
        IF mo_viewer->mo_window->m_hist_depth < 99.
          mo_viewer->mo_window->m_hist_depth += 1.
        ENDIF.
        mo_viewer->mo_window->apply_depth( ).
        IF mo_box IS NOT INITIAL.
          mo_box->set_focus( mo_box ).
        ENDIF.
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'DEPTH'
                    text  = |Depth { mo_viewer->mo_window->m_hist_depth }| ).
        RETURN.
      ELSEIF fcode = 'DEPTH'.
        DATA: lv_answer TYPE c LENGTH 1, lv_value1 TYPE spop-varvalue1.
        CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
          EXPORTING
            textline1   = |Current depth: { mo_viewer->mo_window->m_hist_depth }. Enter new value (0-99):|
            titel       = 'Set Depth'
            valuelength = '2'
          IMPORTING
            answer      = lv_answer
            value1      = lv_value1
          EXCEPTIONS
            OTHERS      = 1.
        IF sy-subrc <> 0 OR lv_answer <> 'J' OR lv_value1 IS INITIAL. RETURN. ENDIF.
        DATA(lv_new_depth) = CONV i( lv_value1 ).
        IF lv_new_depth < 0.
          lv_new_depth = 0.
        ELSEIF lv_new_depth > 99.
          lv_new_depth = 99.
        ENDIF.
        mo_viewer->mo_window->m_hist_depth = lv_new_depth.
        mo_viewer->mo_window->apply_depth( ).
        IF mo_box IS NOT INITIAL.
          mo_box->set_focus( mo_box ).
        ENDIF.
        mo_toolbar->set_button_info(
          EXPORTING fcode = 'DEPTH'
                    text  = |Depth { mo_viewer->mo_window->m_hist_depth }| ).
        RETURN.
      ELSE.
        mv_type = fcode.
      ENDIF.

      refresh( ).

  endmethod.


  method OPEN_MERMAID.

      CHECK ZCL_ACE=>i_mermaid_active = abap_true.

      TRY.
          IF mo_diagram IS INITIAL.
            CREATE OBJECT mo_diagram TYPE ('ZCL_WD_GUI_MERMAID_JS_DIAGRAM')
              EXPORTING parent = mo_mm_container hide_scrollbars = abap_false.
            " Raise mermaid limits so large package graphs still render
            DATA lv_cfg TYPE string.
            CALL METHOD mo_diagram->('GET_CONFIGURATION_JSON') RECEIVING result = lv_cfg.
            IF lv_cfg NP '*maxEdges*'.
              REPLACE FIRST OCCURRENCE OF '{' IN lv_cfg
                WITH '{"maxEdges":100000,"maxTextSize":90000000,'.
              CALL METHOD mo_diagram->('SET_CONFIGURATION_JSON') EXPORTING config_json = lv_cfg.
            ENDIF.
          ENDIF.
          CALL METHOD mo_diagram->('SET_SOURCE_CODE_STRING') EXPORTING source_code = i_mm_string.
          CALL METHOD mo_diagram->('DISPLAY').
        CATCH cx_root.
          CLEAR: mo_diagram,
                 mo_toolbar,
                 mo_splitter,
                 mo_mm_container,
                 mo_mm_toolbar,
                 mo_box.
      ENDTRY.

  endmethod.


  method REFRESH.

      CASE mv_type.
        WHEN 'CALLS'.
          steps_flow( i_direction   = mv_direction
                      i_with_params = mv_with_params
                      i_calc_path   = mv_calc_path ).
        WHEN 'FLOW'.
          magic_search( i_direction = mv_direction
                        i_calc_path = mv_calc_path ).
        WHEN 'CMAP'.
          class_map( i_direction = mv_direction ).
      ENDCASE.

  endmethod.


  METHOD steps_flow.

    TYPES: BEGIN OF lty_entity,
             include   TYPE string,
             class     TYPE string,
             event     TYPE string,
             name      TYPE string,
             style     TYPE string,
             eventname TYPE string,   " raw method/form/function name for binding lookup
           END OF lty_entity,
           BEGIN OF t_ind,
             from TYPE i,
             to   TYPE i,
           END OF t_ind,
           BEGIN OF t_stack_entry,
             stacklevel TYPE i,
             entity_idx TYPE i,    " индекс ноды в таблице entities
             name       TYPE string,
           END OF t_stack_entry.

    CONSTANTS: c_style_event    TYPE string VALUE 'event',
               c_style_method   TYPE string VALUE 'method',
               c_style_form     TYPE string VALUE 'form',
               c_style_constr   TYPE string VALUE 'constr',
               c_style_enh      TYPE string VALUE 'enh',
               c_style_function TYPE string VALUE 'func'.

    DATA: mm_string    TYPE string,
          entities     TYPE TABLE OF lty_entity,
          entity       TYPE lty_entity,
          ind          TYPE t_ind,
          indexes      TYPE TABLE OF t_ind,
          ids_event    TYPE TABLE OF string,
          ids_method   TYPE TABLE OF string,
          ids_form     TYPE TABLE OF string,
          ids_constr   TYPE TABLE OF string,
          ids_enh      TYPE TABLE OF string,
          ids_function TYPE TABLE OF string,
          call_stack   TYPE TABLE OF t_stack_entry.

    DATA(copy) = mo_viewer->mt_steps.

    " Filter steps to only calculated ones when requested
    IF i_calc_path = abap_true.
      DATA(lt_flow) = mo_viewer->get_code_flow( i_calc_path = abap_true ).
      DATA lt_active_ev TYPE TABLE OF string WITH EMPTY KEY.
      LOOP AT lt_flow INTO DATA(ls_fl) WHERE active_root = abap_true.
        READ TABLE lt_active_ev WITH KEY table_line = ls_fl-ev_name TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_fl-ev_name TO lt_active_ev.
        ENDIF.
      ENDLOOP.
      DATA lt_copy_filt LIKE copy.
      LOOP AT copy INTO DATA(ls_cp_filt).
        READ TABLE lt_active_ev WITH KEY table_line = ls_cp_filt-eventname TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND ls_cp_filt TO lt_copy_filt.
        ENDIF.
      ENDLOOP.
      copy = lt_copy_filt.
    ENDIF.

    " ── Step 1: collect unique nodes ────────────────────────────────
    LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
      entity-event     = <copy>-eventtype.
      entity-eventname = <copy>-eventname.   " save raw name before overwrite below

      IF <copy>-eventtype = 'METHOD'.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
          WITH KEY include   = <copy>-include
                   eventtype = 'METHOD'
                   eventname = <copy>-eventname
                   class     = <copy>-class
          INTO DATA(call_line).
        entity-name  = |"{ call_line-class }->{ <copy>-eventname }"|.
        entity-style = COND string(
          WHEN <copy>-eventname = 'CONSTRUCTOR' OR <copy>-eventname = 'CLASS_CONSTRUCTOR'
          THEN c_style_constr ELSE c_style_method ).
      ELSE.
        entity-name = SWITCH string( <copy>-eventtype
          WHEN 'FUNCTION'    THEN |"FUNCTION:{ <copy>-eventname }"|
          WHEN 'SCREEN'      THEN |"CALL SCREEN { <copy>-eventname }"|
          WHEN 'MODULE'      THEN |"MODULE { <copy>-eventname }"|
          WHEN 'FORM'        THEN |"FORM { <copy>-eventname }"|
          WHEN 'ENHANCEMENT' THEN |"ENH { <copy>-eventname }"|
          ELSE                    |"{ <copy>-program }:{ <copy>-eventname }"| ).
        entity-style = SWITCH string( <copy>-eventtype
          WHEN 'FUNCTION'    THEN c_style_function
          WHEN 'FORM'        THEN c_style_form
          WHEN 'ENHANCEMENT' THEN c_style_enh
          WHEN 'MODULE'      THEN c_style_form
          ELSE                    c_style_event ).
      ENDIF.

      <copy>-eventname   = entity-name.
      entity-include = <copy>-include.
      entity-class   = <copy>-class.

      READ TABLE entities
        WITH KEY include = entity-include class = entity-class name = entity-name
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND entity TO entities.
        DATA(lv_node_id) = |{ lines( entities ) }|.
        CASE entity-style.
          WHEN c_style_event.    APPEND lv_node_id TO ids_event.
          WHEN c_style_method.   APPEND lv_node_id TO ids_method.
          WHEN c_style_form.     APPEND lv_node_id TO ids_form.
          WHEN c_style_constr.   APPEND lv_node_id TO ids_constr.
          WHEN c_style_enh.      APPEND lv_node_id TO ids_enh.
          WHEN c_style_function. APPEND lv_node_id TO ids_function.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    mm_string = |graph { COND string( WHEN i_direction IS NOT INITIAL THEN i_direction ELSE 'TD' ) }\n |.

    " ── Шаг 2: явно объявляем все ноды ─────────────────────────────
    DATA(lv_idx) = 0.
    LOOP AT entities INTO entity.
      lv_idx += 1.
      mm_string = |{ mm_string }{ lv_idx }({ entity-name })\n|.
    ENDLOOP.

    " ── Шаг 3: строим стрелки через явный стек вызовов ─────────────
    " call_stack хранит ноды по уровням.
    " Когда приходит новый шаг с stacklevel=N:
    "   - caller = нода в стеке на уровне N-1
    "   - рисуем стрелку caller → текущая нода (если ещё не было)
    "   - обновляем стек: на уровне N теперь текущая нода

    DATA lv_prev_stack TYPE i.

    LOOP AT copy INTO DATA(step2).

      READ TABLE entities
        WITH KEY name = step2-eventname
        TRANSPORTING NO FIELDS.
      DATA(lv_cur_idx) = sy-tabix.

      DATA(lv_level) = step2-stacklevel.

      " Ищем caller — нода на уровне lv_level - 1 в call_stack
      IF lv_level > 1.
        READ TABLE call_stack
          WITH KEY stacklevel = lv_level - 1
          INTO DATA(ls_caller).
        IF sy-subrc = 0 AND ls_caller-entity_idx <> lv_cur_idx.
          " Draw edge only if not yet drawn
          ind-from = ls_caller-entity_idx.
          ind-to   = lv_cur_idx.
          READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            DATA(lv_edge_label) = ``.

            IF i_with_params = abap_true.
              " Look up parameter bindings: search caller's keywords for a call to callee
              DATA(ls_caller_ent) = entities[ ind-from ].
              DATA(ls_callee_ent) = entities[ ind-to ].
              READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
                WITH KEY include = ls_caller_ent-include
                INTO DATA(ls_prog_wp).
              IF sy-subrc = 0.
                LOOP AT ls_prog_wp-t_keywords INTO DATA(ls_kw_wp).
                  LOOP AT ls_kw_wp-tt_calls INTO DATA(ls_call_wp)
                    WHERE name  = ls_callee_ent-eventname
                      AND class = ls_callee_ent-class.
                    LOOP AT ls_call_wp-bindings INTO DATA(ls_bind_wp).
                      IF lv_edge_label IS INITIAL.
                        lv_edge_label = |{ ls_bind_wp-inner }={ ls_bind_wp-outer }|.
                      ELSE.
                        lv_edge_label = |{ lv_edge_label }<br/>{ ls_bind_wp-inner }={ ls_bind_wp-outer }|.
                      ENDIF.
                    ENDLOOP.
                    IF lv_edge_label IS NOT INITIAL. EXIT. ENDIF.
                  ENDLOOP.
                  IF lv_edge_label IS NOT INITIAL. EXIT. ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.

            IF lv_edge_label IS NOT INITIAL.
              DATA(lv_el_fmt) = format_node_label( i_code = lv_edge_label i_maxlen = 0 ).
              mm_string = |{ mm_string }{ ind-from } -->\|"{ lv_el_fmt }"\|{ ind-to }\n|.
            ELSE.
              mm_string = |{ mm_string }{ ind-from } --> { ind-to }\n|.
            ENDIF.
            APPEND ind TO indexes.
          ENDIF.
        ENDIF.
      ENDIF.

      " Обновляем стек: удаляем все уровни >= lv_level и добавляем текущий
      DELETE call_stack WHERE stacklevel >= lv_level.
      APPEND VALUE t_stack_entry(
        stacklevel = lv_level
        entity_idx = lv_cur_idx
        name       = step2-eventname
      ) TO call_stack.

      lv_prev_stack = lv_level.
    ENDLOOP.

    " ── Шаг 4: стили ────────────────────────────────────────────────
    IF mv_type = 'CMAP' AND mo_viewer->mv_cmap_focus IS NOT INITIAL.
      DATA(lv_enrich_from) = 0.
      LOOP AT entities INTO DATA(ls_enrich_src).
        lv_enrich_from += 1.
        CHECK ls_enrich_src-style = c_style_method.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_calls_line
          WITH KEY include   = ls_enrich_src-include
                   eventtype = 'METHOD'
                   eventname = ls_enrich_src-eventname
                   class     = ls_enrich_src-class
          INTO DATA(ls_enrich_line).
        CHECK sy-subrc = 0.
        READ TABLE mo_viewer->mo_window->ms_sources-tt_progs
          WITH KEY include = ls_enrich_line-include
          INTO DATA(ls_enrich_prog).
        CHECK sy-subrc = 0.

        LOOP AT ls_enrich_prog-t_keywords INTO DATA(ls_enrich_kw)
          WHERE index >= ls_enrich_line-index AND index <= ls_enrich_line-end_idx.
          IF ls_enrich_kw-calls_parsed = abap_false.
            zcl_ace_parser=>parse_tokens(
              EXPORTING
                i_program  = CONV #( ls_enrich_kw-program )
                i_include  = CONV #( ls_enrich_kw-include )
                i_stmt_idx = ls_enrich_kw-index
                i_class    = ls_enrich_src-class
                i_evtype   = 'METHOD'
                i_ev_name  = ls_enrich_src-eventname
              CHANGING
                cs_source  = mo_viewer->mo_window->ms_sources ).
            READ TABLE ls_enrich_prog-t_keywords WITH KEY index = ls_enrich_kw-index INTO ls_enrich_kw.
          ENDIF.

          LOOP AT ls_enrich_kw-tt_calls INTO DATA(ls_enrich_call)
            WHERE event = 'METHOD' AND name IS NOT INITIAL.
            DATA(lv_enrich_to) = 0.
            LOOP AT entities INTO DATA(ls_enrich_tgt).
              CHECK ls_enrich_tgt-style = c_style_method
                AND ls_enrich_tgt-eventname = ls_enrich_call-name.
              IF ls_enrich_call-class IS NOT INITIAL
                 AND to_upper( ls_enrich_tgt-class ) <> to_upper( CONV string( ls_enrich_call-class ) ).
                CONTINUE.
              ENDIF.
              lv_enrich_to = sy-tabix.
              EXIT.
            ENDLOOP.
            CHECK lv_enrich_to > 0 AND lv_enrich_to <> lv_enrich_from.
            READ TABLE indexes WITH KEY from = lv_enrich_from to = lv_enrich_to TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              mm_string = |{ mm_string }{ lv_enrich_from } -.-> { lv_enrich_to }\n|.
              APPEND VALUE #( from = lv_enrich_from to = lv_enrich_to ) TO indexes.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    mm_string = |{ mm_string } classDef event    fill:#FFE0B2,stroke:#E65100\n|.
    mm_string = |{ mm_string } classDef method   fill:#BBDEFB,stroke:#1565C0\n|.
    mm_string = |{ mm_string } classDef form     fill:#EEEEEE,stroke:#616161\n|.
    mm_string = |{ mm_string } classDef constr   fill:#E1BEE7,stroke:#6A1B9A\n|.
    mm_string = |{ mm_string } classDef enh      fill:#FCE4EC,stroke:#AD1457\n|.
    mm_string = |{ mm_string } classDef func     fill:#C8E6C9,stroke:#2E7D32\n|.

    DATA(lv_ids) = ``.
    IF ids_event    IS NOT INITIAL. CONCATENATE LINES OF ids_event    INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } event\n|.    ENDIF.
    IF ids_method   IS NOT INITIAL. CONCATENATE LINES OF ids_method   INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } method\n|.   ENDIF.
    IF ids_form     IS NOT INITIAL. CONCATENATE LINES OF ids_form     INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } form\n|.     ENDIF.
    IF ids_constr   IS NOT INITIAL. CONCATENATE LINES OF ids_constr   INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } constr\n|.   ENDIF.
    IF ids_enh      IS NOT INITIAL. CONCATENATE LINES OF ids_enh      INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } enh\n|.      ENDIF.
    IF ids_function IS NOT INITIAL. CONCATENATE LINES OF ids_function INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } func\n|.     ENDIF.

    mm_string = |{ mm_string }\n|.
    open_mermaid( mm_string ).

  ENDMETHOD.
ENDCLASS.
