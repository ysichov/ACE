class ZCL_ACE_MERMAID definition
  public
  inheriting from ZCL_ace_POPUP
  create public .

public section.

  types:
    BEGIN OF ts_if,
             if_ind      TYPE i,
             end_ind     TYPE i,
             before_else TYPE i,
           END OF ts_if .
  types:
    tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY .

  data MO_VIEWER type ref to ZCL_ACE .
  data MO_MM_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_MM_TOOLBAR type ref to CL_GUI_CONTAINER .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_DIAGRAM type ref to ZCL_WD_GUI_MERMAID_JS_DIAGRAM .
  data MV_TYPE type STRING .
  data MS_IF type TS_IF .
  data MT_IF type TT_IF .

  methods CONSTRUCTOR
    importing
      !IO_DEBUGGER type ref to ZCL_ACE
      !IV_TYPE type STRING .
  methods STEPS_FLOW
    importing
      !IV_DIRECTION type UI_FUNC optional .
  methods MAGIC_SEARCH
    importing
      !IV_DIRECTION type UI_FUNC optional .
  methods ADD_TOOLBAR_BUTTONS .
  methods HND_TOOLBAR
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods OPEN_MERMAID
    importing
      !IV_MM_STRING type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_MERMAID IMPLEMENTATION.


  method ADD_TOOLBAR_BUTTONS.


    DATA: lt_button TYPE ttb_button,
          lt_events TYPE cntl_simple_events,
          ls_events LIKE LINE OF lt_events.

    lt_button  = VALUE #(
     ( function = 'TB' icon = CONV #( icon_view_expand_vertical ) quickinfo = 'Vertical' text = '' )
     ( function = 'LR' icon = CONV #( icon_view_expand_horizontal ) quickinfo = 'Horizontal' text = '' )
     ( butn_type = 3  )
     ( function = 'TEXT' icon = CONV #( icon_wd_caption ) quickinfo = 'Mermaid Diagram text' text = '' )
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


    DATA lv_text TYPE text100.

    super->constructor( ).

    mo_viewer = io_debugger.
    mv_type = iv_type.

    CHECK zcl_ace_appl=>is_mermaid_active = abap_true.

    CASE mv_type.
      WHEN 'DIAG'.
        lv_text = 'Calls flow'.
      WHEN 'SMART'.
        lv_text = 'Calculations sequence'.
    ENDCASE.

    IF mo_box IS INITIAL.
      mo_box = create( i_name = lv_text i_width = 1000 i_hight = 300 ).

      "save new popup ref
      APPEND INITIAL LINE TO zcl_ace_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
      <popup>-parent = mo_viewer->mo_window->mo_box.
      <popup>-child = mo_box.

      SET HANDLER on_box_close FOR mo_box.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_box
          rows    = 2
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter->get_container(
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_mm_container ).

      mo_splitter->get_container(
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_mm_toolbar ).

      mo_splitter->set_row_height( id = 1 height = '3' ).
      mo_splitter->set_row_height( id = 2 height = '70' ).

      mo_splitter->set_row_sash( id    = 1
                                 type  = 0
                                 value = 0 ).

      CREATE OBJECT mo_toolbar EXPORTING parent = mo_mm_toolbar.
      add_toolbar_buttons( ).
      mo_toolbar->set_visible( 'X' ).
    ENDIF.
    CASE mv_type.
      WHEN 'DIAG'.
        steps_flow( ).
      WHEN 'SMART'.
        magic_search( ).
    ENDCASE.


  endmethod.


  method HND_TOOLBAR.



    IF fcode = 'TEXT'.
      DATA: lv_mm_string TYPE string,
            lv_ref       TYPE REF TO data.
      lv_mm_string = mo_diagram->get_source_code_string( ).
      GET REFERENCE OF lv_mm_string INTO lv_ref.
      NEW zcl_ace_text_viewer( lv_ref ).

      RETURN.
    ENDIF.

    CASE mv_type.
      WHEN 'DIAG'.
        steps_flow( fcode ).
      WHEN 'SMART'.
        magic_search( fcode ).

    ENDCASE.


  endmethod.


  method MAGIC_SEARCH.


    DATA: lv_add         TYPE xfeld,
          lv_mm_string   TYPE string,
          lv_sub         TYPE string,
          lv_form        TYPE string,
          lv_direction   TYPE string,
          lv_box_s       TYPE string,
          lv_box_e       TYPE string,
          lv_ind2        TYPE i,
          lv_start       TYPE i,
          lv_end         TYPE i,
          lv_bool        TYPE string,
          lv_block_first TYPE i,
          lv_els_before  TYPE i.

    TYPES: BEGIN OF ts_line,
             cond       TYPE string,
             include    TYPE string,
             line       TYPE i,
             ind        TYPE i,
             event      TYPE string,
             stack      TYPE i,
             code       TYPE string,
             arrow      TYPE string,
             subname    TYPE string,
             del        TYPE flag,
             els_before TYPE i,
             els_after  TYPE i,
           END OF ts_line.

    DATA: ls_line       TYPE ts_line,
          lt_lines      TYPE STANDARD TABLE OF ts_line,
          ls_prev_stack TYPE ts_line,
          lv_opened     TYPE i.

    READ TABLE mo_viewer->mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(ls_prog).

    LOOP AT mo_viewer->mt_steps INTO DATA(ls_step).
      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = ls_step-include INTO ls_prog.
      READ TABLE ls_prog-t_keywords WITH KEY line = ls_step-line INTO DATA(ls_keyword).
      LOOP AT ls_keyword-tt_calls INTO DATA(ls_call).

        READ TABLE mo_viewer->mt_selected_var WITH KEY name = ls_call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
          <selected>-name = ls_call-outer.
        ENDIF.

        READ TABLE mo_viewer->mt_selected_var WITH KEY name = ls_call-inner TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
          <selected>-name = ls_call-inner.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    DATA(lt_steps) = mo_viewer->mt_steps.

    SORT lt_steps BY step DESCENDING.

    "collecting dependents variables
    LOOP AT lt_steps INTO ls_step.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = ls_step-include INTO ls_prog.

      LOOP AT mo_viewer->mo_window->ms_sources-t_calculated INTO DATA(ls_calculated) WHERE line = ls_step-line AND program = ls_prog-include.
        READ TABLE mo_viewer->mt_selected_var WITH KEY name = ls_calculated-calculated TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
          <selected>-name = ls_calculated-calculated.
        ENDIF.
        LOOP AT mo_viewer->mo_window->ms_sources-t_composed INTO DATA(ls_composed) WHERE line = ls_step-line AND program = ls_prog-include.
          READ TABLE mo_viewer->mt_selected_var WITH KEY name = ls_composed-composing TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
            <selected>-name = ls_composed-composing.
          ENDIF.
        ENDLOOP.
        "adding returning values
        LOOP AT mo_viewer->mo_window->ms_sources-t_params INTO DATA(lv_param).
          READ TABLE mo_viewer->mt_selected_var WITH KEY name = lv_param-param TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
            <selected>-name = lv_param-param.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      READ TABLE ls_prog-t_keywords WITH KEY line = ls_step-line INTO ls_keyword.
      LOOP AT ls_keyword-tt_calls INTO ls_call.

        READ TABLE mo_viewer->mt_selected_var WITH KEY name = ls_call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
          <selected>-name = ls_call-inner.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
    SORT mo_viewer->mt_selected_var.
    DELETE ADJACENT DUPLICATES FROM mo_viewer->mt_selected_var.

    "collecting watchpoints
    "CLEAR mo_viewer->mo_window->mt_coverage.

    LOOP AT  lt_steps INTO ls_step.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = ls_step-include INTO ls_prog.
      READ TABLE ls_prog-t_keywords WITH KEY line = ls_step-line INTO DATA(ls_key).

      CLEAR ls_line-cond.
      IF ls_key-name = 'IF' OR ls_key-name = 'ELSE' OR ls_key-name = 'ENDIF' OR ls_key-name = 'ELSEIF' OR
         ls_key-name = 'CASE' OR ls_key-name = 'WHEN' OR ls_key-name = 'ENDCASE' OR
          ls_key-name = 'DO' OR ls_key-name = 'ENDDO'  OR ls_key-name = 'LOOP'  OR ls_key-name = 'ENDLOOP'
         OR ls_key-name = 'WHILE' OR ls_key-name = 'ENDWHILE'
         OR ls_key-tt_calls IS NOT INITIAL.
        APPEND INITIAL LINE TO mo_viewer->mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).
        <watch>-program = ls_step-program.
        <watch>-line = ls_line-line = ls_step-line.
        IF ls_key-tt_calls IS INITIAL.
          ls_line-cond = ls_key-name.
        ENDIF.
        ls_line-event = ls_step-eventname.
        ls_line-stack = ls_step-stacklevel.
        ls_line-include = ls_step-include.
        INSERT ls_line INTO lt_lines INDEX 1.

      ENDIF.

      LOOP AT  mo_viewer->mo_window->ms_sources-t_calculated INTO ls_calculated WHERE line = ls_step-line AND program = ls_prog-include.

        LOOP AT mo_viewer->mo_window->ms_sources-t_composed INTO ls_composed WHERE line = ls_step-line AND program = ls_prog-include.
          READ TABLE mo_viewer->mt_selected_var WITH KEY name = ls_composed-composing TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mo_viewer->mt_selected_var ASSIGNING <selected>.
            <selected>-name = ls_composed-composing.
          ENDIF.
        ENDLOOP.

        READ TABLE mo_viewer->mt_selected_var WITH KEY name = ls_calculated-calculated TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.

          APPEND INITIAL LINE TO mo_viewer->mo_window->mt_watch ASSIGNING <watch>.
          <watch>-program = ls_step-program.
          <watch>-line = ls_line-line = ls_step-line.

          LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<line>) WHERE line = ls_line-line AND event = ls_step-eventname AND stack = ls_step-stacklevel .
            <line>-del = abap_true.
          ENDLOOP.

          ls_line-event = ls_step-eventname.
          ls_line-stack = ls_step-stacklevel.
          ls_line-include = ls_step-include.
          INSERT ls_line INTO lt_lines INDEX 1.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    DELETE lt_lines WHERE del = abap_true.

    "delete empty blocks
    LOOP AT lt_lines ASSIGNING <line>.
      IF <line>-cond = 'IF' OR <line>-cond = 'DO' OR <line>-cond = 'LOOP' OR <line>-cond = 'WHILE'.
        READ TABLE lt_lines INDEX sy-tabix + 1 ASSIGNING FIELD-SYMBOL(<line2>).
        IF <line2>-cond = 'ENDIF' OR <line2>-cond = 'ENDDO' OR <line2>-cond = 'ENDLOOP' OR <line2>-cond = 'ENDWHILE'.
          <line>-del = <line2>-del = abap_true.
        ENDIF.
      ENDIF.

    ENDLOOP.
    DELETE lt_lines WHERE del = abap_true.


    "getting code texts and calls params
    LOOP AT lt_lines ASSIGNING <line>.
      DATA(lv_ind) = sy-tabix.

      READ TABLE mo_viewer->mo_window->ms_sources-tt_progs WITH KEY include = <line>-include INTO ls_prog.
      READ TABLE ls_prog-t_keywords WITH KEY line = <line>-line INTO ls_keyword.
      LOOP AT ls_prog-scan->tokens FROM ls_keyword-from TO ls_keyword-to INTO DATA(ls_token).
        IF ls_token-str = 'USING' OR ls_token-str = 'EXPORTING' OR ls_token-str = 'IMPORTING' OR ls_token-str = 'CHANGING'.
          EXIT.
        ENDIF.
        IF <line>-code IS INITIAL.
          <line>-code = ls_token-str.
        ELSE.
          <line>-code = |{  <line>-code } { ls_token-str }|.
        ENDIF.
      ENDLOOP.
      REPLACE ALL OCCURRENCES OF '`' IN  <line>-code WITH ''.
      REPLACE ALL OCCURRENCES OF '"' IN  <line>-code WITH ''.

      SORT ls_keyword-tt_calls BY outer.
      DELETE ADJACENT DUPLICATES FROM ls_keyword-tt_calls.
      IF ls_keyword-to_evname IS NOT INITIAL.
        LOOP AT ls_keyword-tt_calls INTO ls_call. "WHERE type IS NOT INITIAL.
          IF sy-tabix <> 1.
            <line>-arrow = |{ <line>-arrow }, |.
          ENDIF.
          "<line>-arrow  = |{ <line>-arrow  } { ls_call-outer } { ls_call-type } { ls_call-inner }|.
          <line>-arrow  = |{ <line>-arrow  } { ls_call-outer } as  { ls_call-inner }|.
          <line>-subname = ls_call-name.
        ENDLOOP.
      ENDIF.
      REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
    ENDLOOP.

    "check subform execution steps existance and if/case structures build

    DATA: if_depth   TYPE i,
          when_count TYPE i.
    LOOP AT lt_lines ASSIGNING <line> WHERE code <> 'DO' AND code <> 'ENDDO' AND code <> 'WHILE' AND code <> 'ENDWHILE' AND code <> 'LOOP' AND code <> 'ENDLOOP' .
      <line>-ind = sy-tabix.

      FIELD-SYMBOLS: <if> TYPE ts_if.
      IF <line>-cond = 'IF' OR  <line>-cond = 'CASE'.
        ADD 1 TO if_depth.
        CLEAR when_count.
        APPEND INITIAL LINE TO mt_if  ASSIGNING <if>.
        <if>-if_ind = <line>-ind.

      ENDIF.

      IF <line>-cond = 'ENDIF' OR <line>-cond = 'ENDCASE'.
        <if>-end_ind = <line>-ind.
        SUBTRACT 1 FROM if_depth.
        LOOP AT mt_if  ASSIGNING <if> WHERE end_ind = 0.
        ENDLOOP.
        "READ TABLE mt_if INDEX if_depth ASSIGNING <if>.
      ENDIF.

      IF <line>-cond = 'WHEN'.
        ADD 1 TO when_count.
      ENDIF.

      IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.

        <line>-els_before = lv_els_before.
        <line>-els_after = <line>-ind.
        DATA(lv_counter) = <line>-ind + 1.
        DO.
          READ TABLE lt_lines INDEX lv_counter INTO ls_line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF ls_line-cond = 'ELSE' OR ls_line-cond = 'ELSEIF'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  ls_line-cond <> 'DO' AND ls_line-cond <> 'ENDDO' AND ls_line-cond <> 'WHILE' AND ls_line-cond <> 'ENDWHILE' AND ls_line-cond <> 'LOOP' AND ls_line-cond <> 'ENDLOOP'.
            <line>-els_after = lv_counter.
            EXIT.
          ELSE.
            ADD 1 TO lv_counter.

          ENDIF.
        ENDDO.
        IF when_count = 1.
          <if>-if_ind = lv_els_before.
          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond = 'WHEN'.

        <line>-els_before = lv_els_before.
        <line>-els_after = <line>-ind.
        lv_counter = <line>-ind + 1.
        DO.
          READ TABLE lt_lines INDEX lv_counter INTO ls_line.
          IF sy-subrc <> 0.
            CLEAR <line>-els_after.
            EXIT.
          ENDIF.

          IF ls_line-cond = 'WHEN'.
            CLEAR <line>-els_after.
            EXIT.
          ELSEIF  ls_line-cond <> 'DO' AND ls_line-cond <> 'ENDDO' AND ls_line-cond <> 'WHILE' AND ls_line-cond <> 'ENDWHILE' AND ls_line-cond <> 'LOOP' AND ls_line-cond <> 'ENDLOOP'.
            <line>-els_after = lv_counter.
            EXIT.
          ELSE.
            ADD 1 TO lv_counter.

          ENDIF.
        ENDDO.
        IF when_count = 1."to refactor
*          <if>-if_ind = lv_els_before.
*          CLEAR <line>-els_before.
        ENDIF.
      ENDIF.

      IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
        lv_els_before = <line>-ind.
      ELSE.
        CLEAR   lv_els_before.
      ENDIF.

      READ TABLE lt_lines WITH KEY event = <line>-subname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR <line>-arrow.
      ENDIF.
    ENDLOOP.

    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
      INSERT ms_if INTO mt_if INDEX 1.
    ENDIF.

    IF lines( lt_lines ) > 0.
      IF lt_lines[ lines( lt_lines ) ]-arrow IS NOT INITIAL.
        CLEAR lt_lines[ lines( lt_lines ) ]-arrow .
      ENDIF.
    ENDIF.

    "creating mermaid code
    CHECK lt_lines IS NOT INITIAL.

    IF iv_direction IS INITIAL.
      IF lines( lt_lines ) < 100.
        lv_direction = 'LR'.
      ELSE.
        lv_direction = 'TB'.
      ENDIF.
    ELSE.
      lv_direction = iv_direction.
    ENDIF.

    lv_mm_string = |graph { lv_direction }\n |.

    LOOP AT lt_lines INTO ls_line WHERE cond <> 'ELSE' AND cond <> 'ELSEIF' AND  cond <> 'WHEN'.
      lv_ind = sy-tabix.

      IF ls_line-cond IS INITIAL.
        lv_box_s = '('.
        lv_box_e = ')'.
      ELSE.
        lv_box_s = '{'.
        lv_box_e = '}'.
      ENDIF.

      IF ls_prev_stack IS INITIAL.
        ls_prev_stack = ls_line.
      ENDIF.

      IF ( ls_prev_stack-stack > ls_line-stack OR ls_prev_stack-event <> ls_line-event ) AND lv_opened > 0 AND lv_sub IS INITIAL.
        IF ls_prev_stack-stack = ls_line-stack AND ls_prev_stack-event <> ls_line-event.
          DATA(lv_times) = 1.
        ELSE.
          lv_times = ls_prev_stack-stack - ls_line-stack.
        ENDIF.

        DO lv_times TIMES.
          lv_mm_string = |{ lv_mm_string } end\n|.
          SUBTRACT 1 FROM lv_opened.
          IF lv_opened = 0.
            EXIT.
          ENDIF.
        ENDDO.

      ENDIF.
      DATA: lv_name TYPE string.
      IF    ls_line-cond = 'LOOP' OR ls_line-cond = 'DO' OR ls_line-cond = 'WHILE' OR ls_line-arrow IS NOT INITIAL .

        IF ls_line-arrow IS NOT INITIAL.
          lv_mm_string = |{ lv_mm_string }{ lv_ind }{ lv_box_s }"{ ls_line-code }"{ lv_box_e }\n|.
          ls_prev_stack = ls_line.
        ENDIF.

        IF strlen( ls_line-code ) > 50.
          lv_name = ls_line-code+0(50).
        ELSE.
          lv_name = ls_line-code.
        ENDIF.
        REPLACE ALL OCCURRENCES OF `PERFORM` IN lv_name WITH `FORM` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN lv_name WITH `FUNCTION` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `CALL METHOD` IN lv_name WITH `METHOD` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF `-` IN lv_name WITH `~` IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF ` ` IN lv_name WITH `&nbsp;` IN CHARACTER MODE.

        lv_mm_string = |{ lv_mm_string } subgraph S{ lv_ind }["{ lv_name }"]\n  direction { lv_direction }\n|.
        ADD 1 TO lv_opened.
        lv_start = lv_ind.
        CONTINUE.
      ENDIF.

      IF ls_line-cond = 'ENDLOOP' OR ls_line-cond = 'ENDDO' OR ls_line-cond = 'ENDWHILE'.
        SUBTRACT 1 FROM lv_opened.
        lv_mm_string = |{ lv_mm_string } end\n|.
        CONTINUE.
      ENDIF.

      lv_mm_string = |{ lv_mm_string }{ lv_ind }{ lv_box_s }"{ ls_line-code }"{ lv_box_e }\n|.
      ls_prev_stack = ls_line.

    ENDLOOP.

    DO lv_opened TIMES.
      lv_mm_string = |{ lv_mm_string } end\n|.
      SUBTRACT 1 FROM lv_opened.
    ENDDO.


    DATA: if_ind      TYPE i.
    CLEAR ls_prev_stack.
    LOOP AT lt_lines INTO ls_line WHERE cond <> 'LOOP' AND cond <> 'DO' AND cond <> 'WHILE' AND cond <> 'ENDLOOP' AND cond <> 'ENDDO' AND cond <> 'ENDWHILE'.

      IF ls_line-cond = 'IF' OR ls_line-cond = 'CASE' .
        ADD 1 TO if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.


      IF ls_prev_stack IS INITIAL.
        IF ls_line-cond = 'WHEN' OR ls_line-cond = 'ELSE' OR ls_line-cond = 'ELSEIF'.
          IF <if> IS ASSIGNED.
            ls_prev_stack = lt_lines[ <if>-if_ind ].
          ELSE.
            CLEAR ls_prev_stack.
          ENDIF.
        ELSE.
          ls_prev_stack = ls_line.

          IF ls_line-arrow IS NOT INITIAL.
            lv_sub = '|"' && ls_line-arrow && '"|'.
          ELSE.
            CLEAR lv_sub.
          ENDIF.

          CONTINUE.
        ENDIF.

      ENDIF.

      IF ls_line-cond = 'ELSE' OR ls_line-cond = 'ELSEIF' OR ls_line-cond = 'WHEN'.
        lv_bool = '|' && ls_line-code && '|'.
        IF ls_line-els_after IS NOT INITIAL.
          lv_mm_string = |{ lv_mm_string }{ ms_if-if_ind }-->{ lv_bool }{ ls_line-els_after }\n|.
          DATA(lv_diff) = ms_if-end_ind - ls_line-els_after.
          DATA(lv_last_els) = ls_line-els_after.
*          IF ls_line-cond <> 'WHEN' AND ls_line-cond <> 'ELSEIF'  AND  lv_diff > 1 AND ls_line-els_after <> ms_if-end_ind.
*            lv_mm_string = |{ lv_mm_string }{  ls_line-els_after }-->{ ms_if-end_ind }\n|.
*          ENDIF.
        ELSE.
          lv_mm_string = |{ lv_mm_string }{ ms_if-if_ind }-->{ lv_bool }{ ms_if-end_ind }\n|.
        ENDIF.

        IF ls_line-els_before IS NOT INITIAL AND ls_line-els_before <> ms_if-if_ind.
          lv_mm_string = |{ lv_mm_string }{ ls_line-els_before }-->{ ms_if-end_ind }\n|.
        ENDIF.

        IF lt_lines[ ls_line-ind + 1 ]-cond <> 'ENDIF' AND lt_lines[ ls_line-ind + 1 ]-cond <> 'ENDCASE'.
          CLEAR ls_prev_stack.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF   ls_prev_stack-cond NE 'ELSE' AND ls_prev_stack-cond NE 'ELSEIF' AND ls_prev_stack-cond NE 'WHEN' AND NOT ( lv_last_els = ls_line-ind ).

        lv_mm_string = |{ lv_mm_string }{ ls_prev_stack-ind }-->{ lv_sub }{ ls_line-ind }\n|.

        IF ls_line-arrow IS NOT INITIAL.
          lv_sub = '|"' && ls_line-arrow && '"|'.
        ELSE.
          CLEAR lv_sub.
        ENDIF.

      ENDIF.

      ls_prev_stack = ls_line.

      IF ls_line-cond = 'ENDIF' OR ls_line-cond = 'ENDCASE'.
        DELETE mt_if INDEX if_ind.
        SUBTRACT 1 FROM if_ind.
        READ TABLE mt_if INDEX if_ind INTO ms_if.
      ENDIF.

    ENDLOOP.

    open_mermaid( lv_mm_string ).


  endmethod.


  method OPEN_MERMAID.


    CHECK zcl_ace_appl=>is_mermaid_active = abap_true.

    TRY.
        IF mo_diagram IS INITIAL.
          mo_diagram = NEW zcl_wd_gui_mermaid_js_diagram( parent = mo_mm_container ).
        ENDIF.
        mo_diagram->set_source_code_string( iv_mm_string ).
        mo_diagram->display( ).

      CATCH zcx_wd_gui_mermaid_js_diagram INTO DATA(error).
        MESSAGE error TYPE 'E'.
    ENDTRY.


  endmethod.


  method STEPS_FLOW.


    TYPES: BEGIN OF lty_entity,
             event TYPE string,
             name  TYPE string,
           END OF lty_entity,
           BEGIN OF t_ind,
             from TYPE i,
             to   TYPE i,
           END OF t_ind  .

    DATA: lv_mm_string TYPE string,
          lv_name      TYPE string,
          lt_entities  TYPE TABLE OF lty_entity,
          ls_entity    TYPE lty_entity,
          lt_parts     TYPE TABLE OF string,
          ls_step      LIKE LINE OF mo_viewer->mt_steps,
          ls_ind       TYPE t_ind,
          lt_indexes   TYPE TABLE OF t_ind.

    DATA(lt_copy) = mo_viewer->mt_steps.

    LOOP AT lt_copy ASSIGNING FIELD-SYMBOL(<copy>).
      IF <copy>-eventtype = 'METHOD'.
        SPLIT <copy>-program AT '=' INTO TABLE lt_parts.
        <copy>-eventname = ls_entity-name = |"{ lt_parts[ 1 ] }->{ <copy>-eventname }"|.
        ls_entity-event = <copy>-eventtype.

      ELSEIF <copy>-eventtype = 'FUNCTION'.
        <copy>-eventname = ls_entity-name = |"{ <copy>-eventtype }:{ <copy>-eventname }"|.
      ELSE.
        <copy>-eventname = ls_entity-name = |"{ <copy>-program }:{ <copy>-eventname }"|.
      ENDIF.

      COLLECT ls_entity INTO lt_entities.
    ENDLOOP.

    CLEAR ls_step.

    IF iv_direction IS INITIAL.
      lv_mm_string = |graph TD\n |.
    ELSE.
      lv_mm_string = |graph { iv_direction }\n |.
    ENDIF.

    LOOP AT lt_copy INTO DATA(ls_step2).
      IF ls_step IS INITIAL.
        ls_step = ls_step2.
        CONTINUE.
      ENDIF.
      IF ls_step2-stacklevel > ls_step-stacklevel.

        READ TABLE lt_entities WITH KEY name = ls_step-eventname TRANSPORTING NO FIELDS.
        ls_ind-from = sy-tabix.
        READ TABLE lt_entities WITH KEY name = ls_step2-eventname TRANSPORTING NO FIELDS.
        ls_ind-to = sy-tabix.
        READ TABLE lt_indexes WITH KEY from = ls_ind-from to = ls_ind-to TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "REPLACE ALL OCCURRENCES OF `-` IN ls_step-eventname WITH `~` IN CHARACTER MODE.
          "REPLACE ALL OCCURRENCES OF `-` IN ls_step2-eventname WITH `~` IN CHARACTER MODE.
          lv_mm_string = |{ lv_mm_string }{ ls_ind-from }({ ls_step-eventname }) --> { ls_ind-to }({ ls_step2-eventname })\n|.
          APPEND ls_ind TO lt_indexes.
        ENDIF.
      ENDIF.
      ls_step = ls_step2.
    ENDLOOP.

    open_mermaid( lv_mm_string ).


  endmethod.
ENDCLASS.
