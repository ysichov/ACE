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
      !I_DIRECTION type UI_FUNC optional .
  methods ADD_TOOLBAR_BUTTONS .
  methods HND_TOOLBAR
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods OPEN_MERMAID
    importing
      !I_MM_STRING type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_MERMAID IMPLEMENTATION.


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
       ( butn_type = 3  )
       ( function = 'TEXT' icon = CONV #( icon_wd_caption ) quickinfo = 'Mermaid Diagram text' text = '' )
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


      DATA  text TYPE text100.

      super->constructor( ).

      mo_viewer = io_debugger.
      mv_type = i_type.

      CHECK zcl_ace_appl=>is_mermaid_active = abap_true.

      CASE mv_type.
        WHEN 'CALLS'.
          text = 'Calls flow'.
        WHEN 'FLOW'.
          text = 'Calculations sequence'.
      ENDCASE.

      IF mo_box IS INITIAL.
        mo_box = create( i_name =  text i_width = 1000 i_hight = 300 ).

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
        WHEN 'CALLS'.
          steps_flow( ).
        WHEN 'FLOW'.
          magic_search( ).
      ENDCASE.


  endmethod.


  method HND_TOOLBAR.


      IF fcode = 'TEXT'.
        DATA: mm_string TYPE string,
              ref       TYPE REF TO data.
        mm_string = mo_diagram->('GET_SOURCE_CODE_STRING').
        GET REFERENCE OF  mm_string INTO  ref.
        NEW zcl_ace_text_viewer(  ref ).

        RETURN.
      ENDIF.

      IF fcode =  'LR' OR fcode =  'TB'.
        mv_direction = fcode.
      ELSE.
        mv_type = fcode.
      ENDIF.

      CASE mv_type.
        WHEN 'CALLS'.
          steps_flow( mv_direction ).
        WHEN 'FLOW'.
          magic_search( mv_direction ).

      ENDCASE.


  endmethod.


  method MAGIC_SEARCH.


      FIELD-SYMBOLS: <if> TYPE mo_viewer->ts_if.

      DATA: add         TYPE boolean,
            mm_string   TYPE string,
            sub         TYPE string,
            form        TYPE string,
            direction   TYPE string,
            box_s       TYPE string,
            box_e       TYPE string,
            ind2        TYPE i,
            start       TYPE i,
            end         TYPE i,
            bool        TYPE string,
            block_first TYPE i,
            els_before  TYPE i.

      DATA: line      TYPE mo_viewer->ts_line,
            pre_stack TYPE mo_viewer->ts_line,
            opened    TYPE i.

      DATA(lines) = mo_viewer->get_code_flow( ).

      "creating mermaid code
      CHECK lines IS NOT INITIAL.

      IF i_direction IS INITIAL.
        IF lines( lines ) < 100.
          direction = 'LR'.
        ELSE.
          direction = 'TB'.
        ENDIF.
      ELSE.
        direction = i_direction.
      ENDIF.

      mm_string = |graph {  direction }\n |.

      LOOP AT lines INTO line WHERE cond <> 'ELSE' AND cond <> 'ELSEIF' AND  cond <> 'WHEN'.
        DATA(ind) = sy-tabix.

        IF line-cond IS INITIAL.
          box_s = '('.
          box_e = ')'.
        ELSE.
          box_s = '{'.
          box_e = '}'.
        ENDIF.

        IF pre_stack IS INITIAL.
          pre_stack = line.
        ENDIF.

        IF ( pre_stack-stack > line-stack OR pre_stack-ev_name <> line-ev_name ) AND  opened > 0 AND  sub IS INITIAL.
          IF pre_stack-stack = line-stack AND pre_stack-ev_name <> line-ev_name.
            DATA(times) = 1.
          ELSE.
            times = pre_stack-stack - line-stack.
          ENDIF.

          DO  times TIMES.
            mm_string = |{  mm_string } end\n|.
            SUBTRACT 1 FROM  opened.
            IF  opened = 0.
              EXIT.
            ENDIF.
          ENDDO.

        ENDIF.
        DATA:  name TYPE string.
        IF    line-cond = 'LOOP' OR line-cond = 'DO' OR line-cond = 'WHILE' OR line-arrow IS NOT INITIAL .

          IF line-arrow IS NOT INITIAL.
            mm_string = |{  mm_string }{  ind }{  box_s }"{ line-code }"{  box_e }\n|.
            pre_stack = line.
          ENDIF.

          IF strlen( line-code ) > 50.
            name = line-code+0(50).
          ELSE.
            name = line-code.
          ENDIF.
          REPLACE ALL OCCURRENCES OF `PERFORM` IN  name WITH `FORM` IN CHARACTER MODE.
          REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN  name WITH `FUNCTION` IN CHARACTER MODE.
          REPLACE ALL OCCURRENCES OF `CALL METHOD` IN  name WITH `METHOD` IN CHARACTER MODE.
          REPLACE ALL OCCURRENCES OF `-` IN  name WITH `~` IN CHARACTER MODE.
          REPLACE ALL OCCURRENCES OF ` ` IN  name WITH `&nbsp;` IN CHARACTER MODE.

          mm_string = |{  mm_string } subgraph S{  ind }["{  name }"]\n  direction {  direction }\n|.
          ADD 1 TO  opened.
          start =  ind.
          CONTINUE.
        ENDIF.

        IF line-cond = 'ENDLOOP' OR line-cond = 'ENDDO' OR line-cond = 'ENDWHILE'.
          SUBTRACT 1 FROM  opened.
          mm_string = |{  mm_string } end\n|.
          CONTINUE.
        ENDIF.

        mm_string = |{  mm_string }{  ind }{  box_s }"{ line-code }"{  box_e }\n|.
        pre_stack = line.

      ENDLOOP.

      DO  opened TIMES.
        mm_string = |{  mm_string } end\n|.
        SUBTRACT 1 FROM  opened.
      ENDDO.


      DATA: if_ind      TYPE i.
      CLEAR pre_stack.
      LOOP AT lines INTO line WHERE cond <> 'LOOP' AND cond <> 'DO' AND cond <> 'WHILE' AND cond <> 'ENDLOOP' AND cond <> 'ENDDO' AND cond <> 'ENDWHILE'.

        IF line-cond = 'IF' OR line-cond = 'CASE' .
          ADD 1 TO if_ind.
          READ TABLE mo_viewer->mt_if INDEX if_ind INTO mo_viewer->ms_if.
        ENDIF.


        IF pre_stack IS INITIAL.
          IF line-cond = 'WHEN' OR line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
            IF <if> IS ASSIGNED.
              pre_stack = lines[ <if>-if_ind ].
            ELSE.
              CLEAR pre_stack.
            ENDIF.
          ELSE.
            pre_stack = line.

            IF line-arrow IS NOT INITIAL.
              sub = '|"' && line-arrow && '"|'.
            ELSE.
              CLEAR  sub.
            ENDIF.

            CONTINUE.
          ENDIF.

        ENDIF.

        IF line-cond = 'ELSE' OR line-cond = 'ELSEIF' OR line-cond = 'WHEN'.
          bool = '|' && line-code && '|'.
          IF line-els_after IS NOT INITIAL.
            mm_string = |{  mm_string }{ mo_viewer->ms_if-if_ind }-->{  bool }{ line-els_after }\n|.
            DATA(diff) = mo_viewer->ms_if-end_ind - line-els_after.
            DATA(last_els) = line-els_after.
*          IF line-cond <> 'WHEN' AND line-cond <> 'ELSEIF'  AND   diff > 1 AND line-els_after <> ms_if-end_ind.
*             mm_string = |{  mm_string }{  line-els_after }-->{ ms_if-end_ind }\n|.
*          ENDIF.
          ELSE.
            mm_string = |{  mm_string }{ mo_viewer->ms_if-if_ind }-->{  bool }{ mo_viewer->ms_if-end_ind }\n|.
          ENDIF.

          IF line-els_before IS NOT INITIAL AND line-els_before <> mo_viewer->ms_if-if_ind.
            mm_string = |{  mm_string }{ line-els_before }-->{ mo_viewer->ms_if-end_ind }\n|.
          ENDIF.

          IF lines[ line-ind + 1 ]-cond <> 'ENDIF' AND lines[ line-ind + 1 ]-cond <> 'ENDCASE'.
            CLEAR pre_stack.
          ENDIF.
          CONTINUE.
        ENDIF.

        IF   pre_stack-cond NE 'ELSE' AND pre_stack-cond NE 'ELSEIF' AND pre_stack-cond NE 'WHEN' AND NOT (  last_els = line-ind ).

          mm_string = |{  mm_string }{ pre_stack-ind }-->{  sub }{ line-ind }\n|.

          IF line-arrow IS NOT INITIAL.
            sub = '|"' && line-arrow && '"|'.
          ELSE.
            CLEAR  sub.
          ENDIF.

        ENDIF.

        pre_stack = line.

        IF line-cond = 'ENDIF' OR line-cond = 'ENDCASE'.
          DELETE mo_viewer->mt_if INDEX if_ind.
          SUBTRACT 1 FROM if_ind.
          READ TABLE mo_viewer->mt_if INDEX if_ind INTO mo_viewer->ms_if.
        ENDIF.

      ENDLOOP.

      open_mermaid(  mm_string ).


  endmethod.


  method OPEN_MERMAID.


      CHECK zcl_ace_appl=>is_mermaid_active = abap_true.

      TRY.
          IF mo_diagram IS INITIAL.
            CREATE OBJECT mo_diagram TYPE ('ZCL_WD_GUI_MERMAID_JS_DIAGRAM') EXPORTING parent = mo_mm_container.
          ENDIF.
          CALL METHOD mo_diagram->('SET_SOURCE_CODE_STRING') EXPORTING source_code = i_mm_string.
          CALL METHOD mo_diagram->('DISPLAY').

        CATCH cx_root INTO DATA(error).
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

      DATA: mm_string TYPE string,
            name      TYPE string,
            entities  TYPE TABLE OF lty_entity,
            entity    TYPE lty_entity,
            parts     TYPE TABLE OF string,
            step      LIKE LINE OF mo_viewer->mt_steps,
            ind       TYPE t_ind,
            indexes   TYPE TABLE OF t_ind.

      DATA(copy) = mo_viewer->mt_steps.

      LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
        IF <copy>-eventtype = 'METHOD'.
          SPLIT <copy>-program AT '=' INTO TABLE parts.
          <copy>-eventname = entity-name = |"{ parts[ 1 ] }->{ <copy>-eventname }"|.
          entity-event = <copy>-eventtype.

        ELSEIF <copy>-eventtype = 'FUNCTION'.
          <copy>-eventname = entity-name = |"{ <copy>-eventtype }:{ <copy>-eventname }"|.
        ELSE.
          <copy>-eventname = entity-name = |"{ <copy>-program }:{ <copy>-eventname }"|.
        ENDIF.

        COLLECT entity INTO entities.
      ENDLOOP.

      CLEAR step.

      IF i_direction IS INITIAL.
        mm_string = |graph TD\n |.
      ELSE.
        mm_string = |graph { i_direction }\n |.
      ENDIF.

      LOOP AT copy INTO DATA(step2).
        IF step IS INITIAL.
          step = step2.
          CONTINUE.
        ENDIF.
        IF step2-stacklevel > step-stacklevel.

          READ TABLE entities WITH KEY name = step-eventname TRANSPORTING NO FIELDS.
          ind-from = sy-tabix.
          READ TABLE entities WITH KEY name = step2-eventname TRANSPORTING NO FIELDS.
          ind-to = sy-tabix.
          READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            "REPLACE ALL OCCURRENCES OF `-` IN step-eventname WITH `~` IN CHARACTER MODE.
            "REPLACE ALL OCCURRENCES OF `-` IN step2-eventname WITH `~` IN CHARACTER MODE.
            mm_string = |{  mm_string }{ ind-from }({ step-eventname }) --> { ind-to }({ step2-eventname })\n|.
            APPEND ind TO indexes.
          ENDIF.
        ENDIF.
        step = step2.
      ENDLOOP.

      open_mermaid(  mm_string ).


  endmethod.
ENDCLASS.
