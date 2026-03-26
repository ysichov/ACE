class ZCL_ACE_MERMAID definition
  public
  inheriting from ZCL_ACE_POPUP
  create public .

public section.

  data MO_VIEWER type ref to ZCL_ACE .
  data MO_MM_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_MM_TOOLBAR type ref to CL_GUI_CONTAINER .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_DIAGRAM type ref to OBJECT .
  data MV_TYPE type STRING .
  data MV_CALC_PATH type BOOLEAN .
  data MV_DIRECTION type UI_FUNC .

  methods CONSTRUCTOR
    importing
      !IO_DEBUGGER type ref to ZCL_ACE
      !I_TYPE type STRING .
  methods STEPS_FLOW
    importing
      !I_DIRECTION type UI_FUNC optional .
  methods MAGIC_SEARCH
    importing
      !I_DIRECTION type UI_FUNC optional
      !I_CALC_PATH type BOOLEAN optional .
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


  method FORMAT_NODE_LABEL.

    RV_LABEL = I_CODE.
    IF strlen( RV_LABEL ) > I_MAXLEN.
      RV_LABEL = RV_LABEL+0(I_MAXLEN).
    ENDIF.
    REPLACE ALL OCCURRENCES OF `PERFORM`       IN RV_LABEL WITH `FORM`     IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN RV_LABEL WITH `FUNCTION` IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `CALL METHOD`   IN RV_LABEL WITH `METHOD`   IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `-`             IN RV_LABEL WITH ` `        IN CHARACTER MODE.
    " Replace spaces with &nbsp; but preserve ( ) so that run( ) stays readable
    REPLACE ALL OCCURRENCES OF ` `             IN RV_LABEL WITH `&nbsp;`   IN CHARACTER MODE.

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

      " PERFORM/CALL FUNCTION/CALL METHOD etc. — draw node using subname only (no params),
      " then open subgraph for nested rows. Params belong only to the edge label.
      IF <line>-subname IS NOT INITIAL.

        DATA(name2) = format_node_label( i_code = <line>-subname ).

        " Node label shows only the method/function name — parameters go on the edge
        CV_MM_STRING = |{ CV_MM_STRING }{ ind }{ box_s }"{ name2 }"{ box_e }\n|.

        READ TABLE CT_LINES INDEX lv_tabix + 1 INTO line2.
        IF sy-subrc = 0 AND line2-stack > <line>-stack.
          CV_MM_STRING = |{ CV_MM_STRING } subgraph S{ ind }["{ name2 }"]\n  direction { I_DIRECTION }\n|.
          opened += 1.
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
      DATA(lv_label) = format_node_label( i_code = <line>-code ).
      CV_MM_STRING = |{ CV_MM_STRING }{ ind }{ box_s }"{ lv_label }"{ box_e }\n|.
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
    CHECK lines IS NOT INITIAL.

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




  method ADD_TOOLBAR_BUTTONS.

      DATA: button TYPE ttb_button,
            events TYPE cntl_simple_events,
            event  LIKE LINE OF events.

      button  = VALUE #(
       ( function = 'TB' icon = CONV #( icon_view_expand_vertical ) quickinfo = 'Vertical' text = '' )
       ( function = 'LR' icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
       ( butn_type = 3  )
       ( function = 'CALLS' icon = CONV #( icon_workflow_process ) quickinfo = 'Calls Flow' text = 'Calls Flow' )
       ( function = 'FLOW' icon = CONV #( icon_wizard ) quickinfo = 'Calculations flow sequence' text = 'Code Flow' )
       ( function = 'CALCPATH' icon = CONV #( icon_workflow_process ) quickinfo = 'Calc Path - only assigned variables' text = 'Calc Path' )
       ( butn_type = 3  )
       ( function = 'TEXT' icon = CONV #( icon_wd_caption ) quickinfo = 'Mermaid Diagram text' text = '' )
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
        WHEN 'CALLS'. steps_flow( ).
        WHEN 'FLOW'.  magic_search( ).
      ENDCASE.

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

      IF fcode = 'LR' OR fcode = 'TB'.
        mv_direction = fcode.
      ELSEIF fcode = 'CALCPATH'.
        mv_type = 'CALCPATH'.
        mv_calc_path = abap_true.
      ELSE.
        mv_type = fcode.
        CLEAR mv_calc_path.
      ENDIF.

      refresh( ).

  endmethod.


  method OPEN_MERMAID.

      CHECK ZCL_ACE=>i_mermaid_active = abap_true.

      TRY.
          IF mo_diagram IS INITIAL.
            CREATE OBJECT mo_diagram TYPE ('ZCL_WD_GUI_MERMAID_JS_DIAGRAM')
              EXPORTING parent = mo_mm_container hide_scrollbars = abap_false.
          ENDIF.
          CALL METHOD mo_diagram->('SET_SOURCE_CODE_STRING') EXPORTING source_code = i_mm_string.
          CALL METHOD mo_diagram->('DISPLAY').
        CATCH cx_root INTO DATA(error).
          MESSAGE error TYPE 'E'.
      ENDTRY.

  endmethod.


  method REFRESH.

      CASE mv_type.
        WHEN 'CALLS'.    steps_flow( mv_direction ).
        WHEN 'FLOW'.     magic_search( i_direction = mv_direction ).
        WHEN 'CALCPATH'. magic_search( i_direction = mv_direction i_calc_path = abap_true ).
      ENDCASE.

  endmethod.


  METHOD steps_flow.

    TYPES: BEGIN OF lty_entity,
             include TYPE string,
             class   TYPE string,
             event   TYPE string,
             name    TYPE string,
             style   TYPE string,
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

    " ── Шаг 1: собираем уникальные ноды ────────────────────────────
    LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
      entity-event = <copy>-eventtype.

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
          " Рисуем стрелку только если ещё не было такой
          ind-from = ls_caller-entity_idx.
          ind-to   = lv_cur_idx.
          READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            mm_string = |{ mm_string }{ ind-from } --> { ind-to }\n|.
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
