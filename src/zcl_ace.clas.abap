class ZCL_ACE definition
  public
  create public .

public section.

  types:
    BEGIN OF t_obj,
               name TYPE string,
               obj  TYPE string,
             END OF t_obj .
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
               cond       TYPE string,
               include    TYPE string,
               line       TYPE i,
               ind        TYPE i,
               ev_name    TYPE string,
               ev_type    TYPE string,
               stack      TYPE i,
               code       TYPE string,
               arrow      TYPE string,
               subname    TYPE string,
               del        TYPE flag,
               els_before TYPE i,
               els_after  TYPE i,
             END OF ts_line .
  types:
    tt_line TYPE TABLE OF ts_line WITH EMPTY KEY .

  data MV_PROG type PROG .
  data MV_DEST type TEXT255 .
  data MV_MODEL type TEXT255 .
  data MV_APIKEY type TEXT255 .
  data:
    mt_obj            TYPE TABLE OF t_obj .
  data:
    mt_compo          TYPE TABLE OF scompo .
  data MT_LOCALS type TPDA_SCR_LOCALS_IT .
  data MT_GLOBALS type TPDA_SCR_GLOBALS_IT .
  data MT_RET_EXP type TPDA_SCR_LOCALS_IT .
  data M_COUNTER type I .
  data:
    mt_steps          TYPE  TABLE OF Zcl_ace_appl=>t_step_counter .
  data:
    mt_var_step       TYPE  TABLE OF Zcl_ace_appl=>var_table_h .
  data M_STEP type I .
  data M_I_FIND type BOOLEAN .
  data M_STOP_STACK type I .
  data M_DEBUG type X .
  data M_REFRESH type BOOLEAN .
  data M_UPDATE type BOOLEAN .
  data I_STEP type BOOLEAN .
  data MS_STACK_PREV type Zcl_ace_appl=>T_STACK .
  data MS_STACK type Zcl_ace_appl=>T_STACK .
  data I_HISTORY type BOOLEAN .
  data M_HIST_STEP type I .
  data M_STEP_DELTA type I .
  data:
    mt_vars_hist_view TYPE STANDARD TABLE OF Zcl_ace_appl=>var_table .
  data:
    mt_vars_hist      TYPE STANDARD TABLE OF Zcl_ace_appl=>var_table .
  data:
    mt_state          TYPE STANDARD TABLE OF Zcl_ace_appl=>var_table .
  data MV_RECURSE type I .
  data:
    mt_classes_types  TYPE TABLE OF Zcl_ace_appl=>t_classes_types .
  data MO_WINDOW type ref to zCL_ACE_WINDOW .
  data MV_F7_STOP type BOOLEAN .
  data M_F6_LEVEL type I .
  data M_TARGET_STACK type I .
  data MO_TREE_LOCAL type ref to zCL_ace_RTTI_TREE .
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

  methods CONSTRUCTOR
    importing
      !I_PROG type PROG
      !I_DEST type TEXT255
      !I_MODEL type TEXT255
      !I_APIKEY type TEXT255 .
  methods HNDL_SCRIPT_BUTTONS
    importing
      !I_STACK_CHANGED type BOOLEAN
    returning
      value(RV_STOP) type BOOLEAN .
  methods SHOW .
  methods GET_CODE_FLOW
    returning
      value(RESULTS) type TT_LINE .
  methods GET_CODE_MIX .
protected section.
private section.

  constants:
    BEGIN OF c_kind,
                   struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
                   table  LIKE cl_abap_typedescr=>kind_table VALUE cl_abap_typedescr=>kind_table,
                   elem   LIKE cl_abap_typedescr=>kind_elem VALUE cl_abap_typedescr=>kind_elem,
                   class  LIKE cl_abap_typedescr=>kind_class VALUE cl_abap_typedescr=>kind_class,
                   intf   LIKE cl_abap_typedescr=>kind_intf VALUE cl_abap_typedescr=>kind_intf,
                   ref    LIKE cl_abap_typedescr=>kind_ref VALUE cl_abap_typedescr=>kind_ref,
                 END OF c_kind .
ENDCLASS.



CLASS ZCL_ACE IMPLEMENTATION.


  method CONSTRUCTOR.


      CONSTANTS: c_mask TYPE x VALUE '01'.

      mv_prog = i_prog.
      mv_dest = i_dest.
      mv_model = i_model.
      mv_apikey = i_apikey.

      i_step = abap_on.
      Zcl_ace_appl=>check_mermaid( ).
      Zcl_ace_appl=>init_lang( ).
      Zcl_ace_appl=>init_icons_table( ).

      mo_window = NEW zcl_ace_window( me ).


      mo_tree_local = NEW zcl_ace_rtti_tree( i_header   = 'Objects & Code Flow'
                                         i_type     = 'L'
                                         i_cont     = mo_window->mo_locals_container
                                         i_debugger = me ).


      show( ).


  endmethod.


  method GET_CODE_FLOW.


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

      DATA: line      TYPE ts_line,
            pre_stack TYPE ts_line,
            opened    TYPE i.

      READ TABLE mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(prog).

      LOOP AT mt_steps INTO DATA(step).
        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY line = step-line INTO DATA(keyword).
        LOOP AT keyword-tt_calls INTO DATA(call).

          READ TABLE mt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
            <selected>-name = call-outer.
          ENDIF.

          READ TABLE mt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO  mt_selected_var ASSIGNING <selected>.
            <selected>-name = call-inner.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      DATA(steps) = mt_steps.

      SORT steps BY step DESCENDING.

      "collecting dependents variables
      LOOP AT steps INTO step.

        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.

        LOOP AT mo_window->ms_sources-t_calculated INTO DATA(calculated_var) WHERE line = step-line AND program = prog-include.
          READ TABLE mt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO mt_selected_var ASSIGNING <selected>.
            <selected>-name = calculated_var-name.
          ENDIF.
          LOOP AT mo_window->ms_sources-t_composed INTO DATA(composed_var) WHERE line = step-line AND program = prog-include.
            READ TABLE mt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO mt_selected_var ASSIGNING <selected>.
              <selected>-name = composed_var-name.
            ENDIF.
          ENDLOOP.
          "adding returning values
          LOOP AT mo_window->ms_sources-t_params INTO DATA(param).
            READ TABLE mt_selected_var WITH KEY name =  param-param TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO mt_selected_var ASSIGNING <selected>.
              <selected>-name =  param-param.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.
        LOOP AT keyword-tt_calls INTO call.

          READ TABLE mt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO  mt_selected_var ASSIGNING <selected>.
            <selected>-name = call-inner.
          ENDIF.
        ENDLOOP.

      ENDLOOP.
      SORT mo_window->mo_viewer->mt_selected_var.
      DELETE ADJACENT DUPLICATES FROM mo_window->mo_viewer->mt_selected_var.

      "collecting watchpoints
      "CLEAR mo_viewer->mo_window->mt_coverage.

      LOOP AT  steps INTO step.

        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY line = step-line INTO DATA(key).

        CLEAR line-cond.
        IF key-name = 'IF' OR key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ELSEIF' OR
           key-name = 'CASE' OR key-name = 'WHEN' OR key-name = 'ENDCASE' OR
            key-name = 'DO' OR key-name = 'ENDDO'  OR key-name = 'LOOP'  OR key-name = 'ENDLOOP'
           OR key-name = 'WHILE' OR key-name = 'ENDWHILE'
           OR key-tt_calls IS NOT INITIAL.
          APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).
          <watch>-program = step-program.
          <watch>-line = line-line = step-line.
          IF key-tt_calls IS INITIAL.
            line-cond = key-name.
          ENDIF.
          line-ev_name = step-eventname.
          line-ev_type = step-eventtype.
          line-stack = step-stacklevel.
          line-include = step-include.
          INSERT line INTO results INDEX 1.

        ENDIF.

        LOOP AT  mo_window->ms_sources-t_calculated INTO calculated_var WHERE line = step-line AND program = prog-include.

          LOOP AT mo_window->ms_sources-t_composed INTO composed_var WHERE line = step-line AND program = prog-include.
            READ TABLE mt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO  mt_selected_var ASSIGNING <selected>.
              <selected>-name = composed_var-name.
            ENDIF.
          ENDLOOP.

          READ TABLE mt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.

            APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING <watch>.
            <watch>-program = step-program.
            <watch>-line = line-line = step-line.

            LOOP AT results ASSIGNING FIELD-SYMBOL(<line>) WHERE line = line-line AND ev_name = step-eventname AND stack = step-stacklevel .
              <line>-del = abap_true.
            ENDLOOP.

            line-ev_name = step-eventname.
            line-ev_type = step-eventtype.
            line-stack = step-stacklevel.
            line-include = step-include.
            INSERT line INTO results INDEX 1.

          ENDIF.

        ENDLOOP.

      ENDLOOP.

      DELETE results WHERE del = abap_true.

      "delete empty blocks
      LOOP AT results ASSIGNING <line>.
        IF <line>-cond = 'IF' OR <line>-cond = 'DO' OR <line>-cond = 'LOOP' OR <line>-cond = 'WHILE'.
          READ TABLE results INDEX sy-tabix + 1 ASSIGNING FIELD-SYMBOL(<line2>).
          IF <line2>-cond = 'ENDIF' OR <line2>-cond = 'ENDDO' OR <line2>-cond = 'ENDLOOP' OR <line2>-cond = 'ENDWHILE'.
            <line>-del = <line2>-del = abap_true.
          ENDIF.
        ENDIF.

      ENDLOOP.
      DELETE results WHERE del = abap_true.


      "getting code texts and calls params
      LOOP AT results ASSIGNING <line>.
        DATA(ind) = sy-tabix.

        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = <line>-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY line = <line>-line INTO keyword.
        LOOP AT prog-scan->tokens FROM keyword-from TO keyword-to INTO DATA(token).
          IF token-str = 'USING' OR token-str = 'EXPORTING' OR token-str = 'IMPORTING' OR token-str = 'CHANGING'.
            EXIT.
          ENDIF.
          IF <line>-code IS INITIAL.
            <line>-code = token-str.
          ELSE.
            <line>-code = |{  <line>-code } { token-str }|.
          ENDIF.
        ENDLOOP.
        REPLACE ALL OCCURRENCES OF '`' IN  <line>-code WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN  <line>-code WITH ''.

        SORT keyword-tt_calls BY outer.
        DELETE ADJACENT DUPLICATES FROM keyword-tt_calls.
        IF keyword-to_evname IS NOT INITIAL.
          LOOP AT keyword-tt_calls INTO call. "WHERE type IS NOT INITIAL.
            IF sy-tabix <> 1.
              <line>-arrow = |{ <line>-arrow }, |.
            ENDIF.
            "<line>-arrow  = |{ <line>-arrow  } { call-outer } { call-type } { call-inner }|.
            <line>-arrow  = |{ <line>-arrow  } { call-outer } as  { call-inner }|.
            <line>-subname = call-name.
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
      LOOP AT results ASSIGNING <line> WHERE code <> 'DO' AND code <> 'ENDDO' AND code <> 'WHILE' AND code <> 'ENDWHILE' AND code <> 'LOOP' AND code <> 'ENDLOOP' .
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

          <line>-els_before =  els_before.
          <line>-els_after = <line>-ind.
          DATA(counter) = <line>-ind + 1.
          DO.
            READ TABLE results INDEX  counter INTO line.
            IF sy-subrc <> 0.
              CLEAR <line>-els_after.
              EXIT.
            ENDIF.

            IF line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
              CLEAR <line>-els_after.
              EXIT.
            ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
              <line>-els_after =  counter.
              EXIT.
            ELSE.
              ADD 1 TO  counter.

            ENDIF.
          ENDDO.
          IF when_count = 1.
            <if>-if_ind =  els_before.
            CLEAR <line>-els_before.
          ENDIF.
        ENDIF.

        IF <line>-cond = 'WHEN'.

          <line>-els_before =  els_before.
          <line>-els_after = <line>-ind.
          counter = <line>-ind + 1.
          DO.
            READ TABLE results INDEX  counter INTO line.
            IF sy-subrc <> 0.
              CLEAR <line>-els_after.
              EXIT.
            ENDIF.

            IF line-cond = 'WHEN'.
              CLEAR <line>-els_after.
              EXIT.
            ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
              <line>-els_after =  counter.
              EXIT.
            ELSE.
              ADD 1 TO  counter.

            ENDIF.
          ENDDO.
          IF when_count = 1."to refactor
*          <if>-if_ind =  els_before.
*          CLEAR <line>-els_before.
          ENDIF.
        ENDIF.

        IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
          els_before = <line>-ind.
        ELSE.
          CLEAR    els_before.
        ENDIF.

        READ TABLE results WITH KEY ev_name = <line>-subname TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CLEAR <line>-arrow.
        ENDIF.
      ENDLOOP.

      IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL.
        INSERT ms_if INTO mt_if INDEX 1.
      ENDIF.

      IF lines( results ) > 0.
        IF results[ lines( results ) ]-arrow IS NOT INITIAL.
          CLEAR results[ lines( results ) ]-arrow .
        ENDIF.
      ENDIF.


  endmethod.


  method GET_CODE_MIX.


      "code flow
      DATA: flow_lines TYPE sci_include,
            splits     TYPE TABLE OF string,
            form       TYPE string.

      CLEAR form.

      DATA(lines) = get_code_flow( ).
      LOOP AT lines INTO DATA(line).
        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = line-include INTO DATA(prog).
        READ TABLE prog-t_keywords WITH KEY line = line-line INTO DATA(keyword).
        DATA(from_row) = prog-scan->tokens[ keyword-from ]-row.
        DATA(to_row) = prog-scan->tokens[ keyword-to ]-row.
        DATA(spaces) = repeat( val = | | occ = ( line-stack - 1 ) * 3 ).
        IF form <> line-ev_name. "new event
          SPLIT line-include AT '=' INTO TABLE splits.

          APPEND INITIAL LINE TO flow_lines ASSIGNING FIELD-SYMBOL(<flow>).
          <flow> =  | "{ line-ev_type } { line-ev_name } in { splits[ 1 ] }|.
        ENDIF.

        LOOP AT prog-source_tab FROM from_row TO to_row INTO DATA(source_line).
          APPEND INITIAL LINE TO flow_lines ASSIGNING <flow>.
          <flow> = |{ spaces }{ source_line }|.
        ENDLOOP.
        form = line-ev_name.
      ENDLOOP.

      mo_window->mo_code_viewer->set_text( table = flow_lines ).

      APPEND INITIAL LINE TO mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
      INSERT INITIAL LINE INTO mo_window->mt_stack INDEX 1 ASSIGNING FIELD-SYMBOL(<stack>).
      <prog>-include = <stack>-program = <stack>-include = 'Code Flow Mix'.
      <prog>-source_tab = flow_lines.
      mo_window->show_stack( ).


  endmethod.


  method HNDL_SCRIPT_BUTTONS.


      IF m_i_find = abap_true.
        rv_stop = abap_true.
        CLEAR m_i_find.
        RETURN.
      ENDIF.

      IF mo_window->m_debug_button = 'F5'.
        rv_stop = abap_true.

      ELSEIF mo_window->m_debug_button = 'F6'.
        IF m_f6_level IS NOT INITIAL AND m_f6_level = ms_stack-stacklevel OR mo_window->m_history IS INITIAL.
          CLEAR m_f6_level.
          rv_stop = abap_true.
        ENDIF.

      ELSEIF mo_window->m_debug_button = 'F6END'.
        IF mo_window->m_prg-flag_eoev IS NOT INITIAL AND m_target_stack = ms_stack-stacklevel.
          rv_stop = abap_true.
        ENDIF.
      ELSEIF mo_window->m_debug_button = 'F7'.

        IF m_target_stack = ms_stack-stacklevel.
          CLEAR m_target_stack.
          rv_stop = abap_true.
        ENDIF.

      ELSEIF mo_window->m_debug_button IS NOT INITIAL.
        READ TABLE mo_window->mt_breaks WITH KEY inclnamesrc = mo_window->m_prg-include linesrc = mo_window->m_prg-line INTO DATA(gs_break).
        IF sy-subrc = 0.
          rv_stop = abap_true.
        ELSE.

          IF mo_window->m_debug_button = 'F6BEG' AND m_target_stack = ms_stack-stacklevel.
            rv_stop = abap_true.
          ELSE.
            IF mo_window->m_history IS NOT INITIAL.
              IF ms_stack-stacklevel = mo_window->m_hist_depth +  mo_window->m_start_stack.
                "f6( )."to refactor
              ELSE.
                "f5( )."to refactor
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        rv_stop = abap_true.
      ENDIF.


  endmethod.


  method SHOW.

      IF  mo_window->m_prg-include IS INITIAL.
        mo_window->m_prg-program =  mo_window->m_prg-include = mv_prog.
      ENDIF.
      mo_window->set_program( CONV #( mo_window->m_prg-include ) ).
      mo_window->show_coverage( ).
      IF  mo_window->m_prg-line IS INITIAL.
        mo_window->m_prg-line = mo_window->mt_stack[ 1 ]-line.
      ENDIF.
      mo_window->set_program_line( mo_window->m_prg-line ).

      mo_window->show_stack( ).
      mo_tree_local->clear( ).
      mo_tree_local->main_node_key = mo_tree_local->add_node( iv_name = CONV #( mv_prog ) iv_icon = CONV #( icon_folder ) ).

      mo_tree_local->add_node( iv_name = 'Local Classes' iv_icon = CONV #( icon_folder ) iv_rel = mo_tree_local->main_node_key ).
      mo_tree_local->add_node( iv_name = 'Global Fields' iv_icon = CONV #( icon_header ) iv_rel = mo_tree_local->main_node_key ).
      mo_tree_local->add_node( iv_name = 'Events' iv_icon = CONV #( icon_oo_event ) iv_rel = mo_tree_local->main_node_key ).
      DATA(forms_rel) = mo_tree_local->add_node( iv_name = 'Subroutines' iv_icon = CONV #( icon_folder ) iv_rel = mo_tree_local->main_node_key ).
      mo_tree_local->add_node( iv_name = 'Code Flow' iv_icon = CONV #( icon_enhanced_bo ) iv_rel = mo_tree_local->main_node_key ).

      LOOP AT mo_window->ms_sources-t_params INTO DATA(subs) WHERE event = 'FORM' .
        DATA(form_name) = subs-name.
        AT NEW name.
          mo_tree_local->add_node( iv_name =  form_name iv_icon = CONV #( icon_biw_info_source_ina ) iv_rel =  forms_rel ).

        ENDAT.
      ENDLOOP.

      mo_tree_local->display( ).


  endmethod.
ENDCLASS.
