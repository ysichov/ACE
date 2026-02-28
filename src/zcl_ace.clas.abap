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
               program    TYPE string,
               include    TYPE string,
               line       TYPE i,
               ind        TYPE i,
               class      TYPE string,
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
    mt_steps          TYPE  TABLE OF ZCL_ACE_APPL=>t_step_counter WITH NON-UNIQUE KEY program include line eventtype eventname .
  data:
    mt_var_step       TYPE  TABLE OF ZCL_ACE_APPL=>var_table_h .
  data M_STEP type I .
  data M_I_FIND type BOOLEAN .
  data M_STOP_STACK type I .
  data M_DEBUG type X .
  data M_REFRESH type BOOLEAN .
  data M_UPDATE type BOOLEAN .
  data I_STEP type BOOLEAN .
  data MS_STACK_PREV type ZCL_ACE_APPL=>T_STACK .
  data MS_STACK type ZCL_ACE_APPL=>T_STACK .
  data I_HISTORY type BOOLEAN .
  data M_HIST_STEP type I .
  data M_STEP_DELTA type I .
  data:
    mt_vars_hist_view TYPE STANDARD TABLE OF ZCL_ACE_APPL=>var_table .
  data:
    mt_vars_hist      TYPE STANDARD TABLE OF ZCL_ACE_APPL=>var_table .
  data:
    mt_state          TYPE STANDARD TABLE OF ZCL_ACE_APPL=>var_table .
  data MV_RECURSE type I .
  data:
    mt_classes_types  TYPE TABLE OF ZCL_ACE_APPL=>t_classes_types .
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

  methods CONSTRUCTOR
    importing
      !I_PROG type PROG .
  methods SHOW .
  methods ADD_CLASS
    importing
      !I_CLASS type STRING
      !I_REFNODE type SALV_DE_NODE_KEY
      !NO_LOCALS type BOOLEAN optional
      !I_TREE type ZCL_ACE_APPL=>TS_TREE optional
      !I_TYPE type FLAG optional .
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


  method ADD_CLASS.

      DATA: tree        TYPE ZCL_ACE_APPL=>ts_tree,
            splits_incl TYPE TABLE OF string,
            icon        TYPE salv_de_tree_image,
            locals_rel  TYPE salv_de_node_key,
            test_rel    TYPE salv_de_node_key,
            class_rel   TYPE salv_de_node_key,
            var_rel     TYPE salv_de_node_key,
            attr_rel    TYPE salv_de_node_key,
            include     TYPE string,
            prefix      TYPE string.

      IF i_type = 'I'."interface
        icon = icon_oo_connection.
      ELSEIF i_type = 'T'. "test class
        icon = icon_test.
      ELSE.
        icon = icon_folder.
      ENDIF.

      "interfaces
      LOOP AT mo_window->ms_sources-t_classes INTO DATA(ls_class) WHERE clsname = i_class AND reltype = '1'.
        IF class_rel IS INITIAL.
          class_rel = mo_tree_local->add_node( i_name = CONV #( i_class ) i_icon =  icon i_rel = i_refnode i_tree = i_tree ).
        ENDIF.

        add_class( i_class = CONV #( ls_class-refclsname ) i_refnode = class_rel no_locals = abap_true i_type = 'I' ).
      ENDLOOP.

      LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs) WHERE class =  i_class AND eventtype = 'METHOD'.
        IF class_rel IS INITIAL.
          class_rel = mo_tree_local->add_node( i_name = CONV #( i_class ) i_icon = CONV #( icon ) i_rel = i_refnode i_tree = i_tree ).

          "Attributes
          CLEAR tree.
          LOOP AT mo_window->ms_sources-t_vars INTO DATA(var) WHERE program = subs-program AND class = subs-class AND eventname IS INITIAL.
            IF attr_rel IS INITIAL.
              attr_rel = mo_tree_local->add_node( i_name =  'Attributes' i_icon = CONV #( icon_folder ) i_rel =  class_rel i_tree = tree ).
            ENDIF.
            tree-value = var-line.
            tree-include = var-include.
            mo_tree_local->add_node( i_name = var-name i_icon = var-icon i_rel =  attr_rel i_tree = tree ).
          ENDLOOP.
        ENDIF.

        SPLIT subs-include AT '=' INTO TABLE splits_incl.
        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs-include INTO DATA(prog).
        READ TABLE prog-t_keywords WITH KEY index = subs-index INTO DATA(keyword).

        CLEAR tree.
        tree-kind = 'M'.
        tree-value = keyword-v_line.
        tree-include = subs-include.
        tree-program = subs-program.
        tree-ev_type = subs-eventtype.
        tree-ev_name = subs-eventname.

        IF i_type = 'I'.
          icon = icon_oo_inst_method.
        ELSEIF subs-redefined = abap_false.

          CASE subs-meth_type.
            WHEN 0 OR 1.
              icon = icon_led_green.
            WHEN 2.
              icon = icon_led_yellow.
            WHEN 3.
              icon = icon_led_red.
            WHEN OTHERS.
              IF subs-eventname = 'CONSTRUCTOR'.
                icon = icon_tools.
              ENDIF.
          ENDCASE.
        ELSE.
          icon = icon_oo_overwrite.
        ENDIF.

        DATA(event_node) = mo_tree_local->add_node( i_name =  subs-eventname i_icon = icon i_rel =  class_rel i_tree = tree )."method name

        "local vars
        CLEAR var_rel.
        CLEAR tree.
        LOOP AT mo_window->ms_sources-t_vars INTO var WHERE program = subs-program AND class = subs-class AND eventname = subs-eventname .
          IF var_rel IS INITIAL.
            var_rel = mo_tree_local->add_node( i_name =  'Local vars' i_icon = CONV #( icon_folder ) i_rel =  event_node i_tree = tree ).
          ENDIF.
          tree-value = var-line.
          tree-include = var-include.
          mo_tree_local->add_node( i_name = var-name i_icon = var-icon i_rel =  var_rel i_tree = tree ).
        ENDLOOP.

        CLEAR tree-value.
        LOOP AT mo_window->ms_sources-t_params INTO DATA(param) WHERE class = subs-class AND event = 'METHOD' AND name = subs-eventname  AND param IS NOT INITIAL.

          CASE param-type.
            WHEN 'I'.
              icon = icon_parameter_import.
            WHEN 'E'.
              icon = icon_parameter_export.
          ENDCASE.
          "tree-param = param-param.

          tree-value = param-line.
          tree-include = param-include.

          mo_tree_local->add_node( i_name =  param-param i_icon = icon i_rel =  event_node i_tree = tree ).
        ENDLOOP.
        CLEAR tree.

      ENDLOOP.

      IF no_locals = abap_false.
        prefix = i_class && repeat( val = `=` occ = 30 - strlen( i_class ) ).
        include =  prefix && 'CP'."local classes

        DATA: local TYPE string.
        LOOP AT mo_window->ms_sources-tt_calls_line INTO subs WHERE program = include AND class <>  i_class AND eventtype = 'METHOD'.
          IF local <> subs-class.
            IF locals_rel IS INITIAL.
              tree-kind = 'F'.
              locals_rel = mo_tree_local->add_node( i_name = 'Local Classes' i_icon = CONV #( icon_folder ) i_rel = class_rel i_tree = tree ).
            ENDIF.
            add_class( i_class = CONV #( subs-class ) i_refnode = locals_rel  no_locals = abap_true ).
          ENDIF.
          local = subs-class.
        ENDLOOP.

      ENDIF.


  endmethod.


  method CONSTRUCTOR.


      CONSTANTS: c_mask TYPE x VALUE '01'.

      mv_prog = i_prog.
      i_step = abap_on.
      ZCL_ACE_APPL=>check_mermaid( ).
      ZCL_ACE_APPL=>init_icons_table( ).

      mo_window = NEW ZCL_ACE_WINDOW( me ).

      mo_tree_local = NEW ZCL_ACE_RTTI_TREE( i_header   = 'Objects & Code Flow'
                                         i_type     = 'L'
                                         i_cont     = mo_window->mo_locals_container
                                         i_debugger = me ).

      show( ).


  endmethod.


  method GET_CODE_FLOW.


      DATA: add         TYPE boolean,
            sub         TYPE string,
            form        TYPE string,
            direction   TYPE string,
            ind2        TYPE i,
            start       TYPE i,
            end         TYPE i,
            bool        TYPE string,
            block_first TYPE i,
            els_before  TYPE i.

      DATA: line      TYPE ts_line,
            pre_stack TYPE ts_line,
            opened    TYPE i.

      DELETE mo_window->ms_sources-t_calculated WHERE name+0(1) = ''''.
      DELETE mo_window->ms_sources-t_composed WHERE name+0(1) = ''''.

      DELETE mo_window->ms_sources-t_calculated WHERE name+0(1) = '='.
      DELETE mo_window->ms_sources-t_composed WHERE name+0(1) = '='.


      SORT mo_window->ms_sources-t_calculated.
      DELETE ADJACENT DUPLICATES FROM mo_window->ms_sources-t_calculated.

      SORT mo_window->ms_sources-t_composed.
      DELETE ADJACENT DUPLICATES FROM mo_window->ms_sources-t_composed.

      READ TABLE mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(prog).
      DATA(lt_selected_var) = mt_selected_var.

      IF mo_window->ms_sel_call IS NOT INITIAL.
        CLEAR: mt_steps, mo_window->mt_calls.
        IF mo_window->ms_sel_call-eventtype = 'FORM'.
          ZCL_ACE_SOURCE_PARSER=>parse_call_form(
            EXPORTING i_call_name = mo_window->ms_sel_call-eventname
                      i_program   = CONV #( mo_window->ms_sel_call-program )
                      i_include   = CONV #( mo_window->ms_sel_call-include )
                      i_stack     = 0
                      io_debugger = mo_window->mo_viewer ).
        ELSE.
          ZCL_ACE_SOURCE_PARSER=>parse_call( EXPORTING i_index = mo_window->ms_sel_call-index
                                                    i_e_name = mo_window->ms_sel_call-eventname
                                                    i_e_type = mo_window->ms_sel_call-eventtype
                                                    i_program = CONV #( mo_window->ms_sel_call-program )
                                                    i_include = CONV #( mo_window->ms_sel_call-include )
                                                    i_stack   =  0
                                                    io_debugger = mo_window->mo_viewer ).
        ENDIF.
      ENDIF.

      DATA(steps) = mt_steps.
      SORT steps BY line eventtype eventname.
      DELETE ADJACENT DUPLICATES FROM steps.
      SORT steps BY step.
      DATA: yes TYPE xfeld.
      LOOP AT steps INTO DATA(step).
        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY line = step-line INTO DATA(keyword).
        LOOP AT keyword-tt_calls INTO DATA(call).

          READ TABLE lt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.
            yes = abap_true.
          ENDIF.

          READ TABLE lt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.
            yes = abap_true.
          ENDIF.
        ENDLOOP.
        IF yes = abap_true.
          LOOP AT keyword-tt_calls INTO call.
            READ TABLE lt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO  lt_selected_var ASSIGNING FIELD-SYMBOL(<selected>).
              <selected>-name = call-outer.
            ENDIF.

            READ TABLE lt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO  lt_selected_var ASSIGNING <selected>.
              <selected>-name = call-inner.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      "deleting empty cycles.
      DATA: prev    LIKE LINE OF mt_steps,
            pre_key TYPE string.

      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.

      LOOP AT steps ASSIGNING FIELD-SYMBOL(<step>).
        DATA(ind) = sy-tabix.
        READ TABLE prog-t_keywords WITH KEY line = <step>-line INTO DATA(key).
        IF prev IS NOT INITIAL.
          IF ( key-name = 'ENDDO' OR key-name = 'ENDWHILE' OR key-name = 'ENDLOOP' OR key-name = 'ENDIF' )  AND
             ( pre_key = 'DO' OR pre_key = 'LOOP'  OR pre_key = 'WHILE'  OR pre_key = 'IF' ).
            <step>-first = 'D'."to delete
            READ TABLE mt_steps INDEX ind - 1 ASSIGNING FIELD-SYMBOL(<step_prev>).
            <step_prev>-first = 'D'.
          ENDIF.
        ENDIF.
        prev = <step>.
        pre_key = key-name.
      ENDLOOP.

      DELETE steps WHERE first = 'D'.

      SORT steps BY step DESCENDING.

      "collecting dependents variables
      LOOP AT steps INTO step.

        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.

        LOOP AT mo_window->ms_sources-t_calculated INTO DATA(calculated_var) WHERE line = step-line.
          READ TABLE lt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.

            LOOP AT mo_window->ms_sources-t_composed INTO DATA(composed_var) WHERE line = step-line.
              READ TABLE lt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                APPEND INITIAL LINE TO  lt_selected_var ASSIGNING <selected>.
                <selected>-name = composed_var-name.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

        READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.
        LOOP AT keyword-tt_calls INTO call.

          READ TABLE lt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO  lt_selected_var ASSIGNING <selected>.
            <selected>-name = call-inner.
          ENDIF.
        ENDLOOP.

      ENDLOOP.
      SORT lt_selected_var.
      DELETE ADJACENT DUPLICATES FROM lt_selected_var.

      "collecting watchpoints
      CLEAR mo_window->mt_coverage.

      LOOP AT  steps INTO step.

        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY line = step-line INTO key.

        CLEAR line-cond.
        IF key-name = 'IF' OR key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ELSEIF' OR
           key-name = 'CASE' OR key-name = 'WHEN' OR key-name = 'ENDCASE' OR
            key-name = 'DO' OR key-name = 'ENDDO'  OR key-name = 'LOOP'  OR key-name = 'ENDLOOP' OR key-name = 'WHILE' OR key-name = 'ENDWHILE'.
          APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).

          <watch>-program = step-program.
          <watch>-line = line-line = step-line.

          INSERT line INTO results INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
          <line>-cond = key-name.
          <line>-ev_name = step-eventname.
          <line>-stack = step-stacklevel.
          <line>-include = step-include.
          <line>-class = step-class.
        ENDIF.

        CLEAR ind.
        LOOP AT  mo_window->ms_sources-t_calculated INTO calculated_var WHERE line = step-line.
          ADD 1 TO ind.
          LOOP AT mo_window->ms_sources-t_composed INTO composed_var WHERE line = step-line.
            READ TABLE lt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              APPEND INITIAL LINE TO  lt_selected_var ASSIGNING <selected>.
              <selected>-name = composed_var-name.
            ENDIF.
          ENDLOOP.

          READ TABLE lt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 OR mt_selected_var IS INITIAL.

            APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING <watch>.
            <watch>-program = step-program.
            <watch>-line = line-line = step-line.

            "should be commented for Smart debugger
*          LOOP AT lines ASSIGNING <line> WHERE line = line-line AND event = step-eventname AND stack = step-stacklevel .
*            <line>-del = abap_true.
*          ENDLOOP.
            IF ind = 1.

              IF key-name <> 'PUBLIC' AND key-name <> 'ENDCLASS' AND  key-name <> 'ENDFORM' AND key-name <> 'FORM' AND
                key-name <> 'METHOD' AND key-name <> 'METHODS' AND key-name <> 'ENDMETHOD' AND key-name <> 'MODULE' .
                line-ev_name = step-eventname.
                line-stack = step-stacklevel.
                line-include = step-include.
                line-class = step-class.
                line-ev_type = step-eventtype.
                INSERT line INTO results INDEX 1.
              ENDIF.

            ENDIF.
          ENDIF.

        ENDLOOP.
        "if no variable - whole CodeMix flow
        IF sy-subrc <> 0 AND mt_selected_var IS INITIAL.

          IF key-name <> 'PUBLIC' AND key-name <> 'ENDCLASS' AND  key-name <> 'ENDFORM' AND  key-name <> 'ENDMETHOD' AND
            key-name <> 'METHOD' AND key-name <> 'METHODS' AND key-name <> 'MODULE' AND  key-name <> 'FORM'.
            READ TABLE results WITH KEY line = step-line include = step-include ev_type = step-eventtype ev_name = step-eventname TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              line-line = step-line.
              line-ev_name = step-eventname.
              line-stack = step-stacklevel.
              line-include = step-include.
              line-ev_type = step-eventtype.
              line-class = step-class.
              INSERT line INTO results INDEX 1.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

      DELETE results WHERE del = abap_true.

      "getting code texts and calls params
      LOOP AT results ASSIGNING <line>.
        ind = sy-tabix.

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

        IF keyword-tt_calls IS NOT INITIAL.
          SORT keyword-tt_calls BY outer.
          DELETE ADJACENT DUPLICATES FROM keyword-tt_calls.
          LOOP AT keyword-tt_calls INTO call.
            <line>-subname = call-name.
            CHECK call-outer IS NOT INITIAL AND call-inner IS NOT INITIAL.
            IF sy-tabix <> 1.
              <line>-arrow = |{ <line>-arrow }, |.
            ENDIF.
            <line>-arrow  = |{ <line>-arrow  } { call-outer } { call-type } { call-inner }|.

            REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
            REPLACE ALL OCCURRENCES OF '"' IN  <line>-code WITH ''.
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
          IF <if> IS ASSIGNED.
            <if>-end_ind = <line>-ind.
            SUBTRACT 1 FROM if_depth.
            LOOP AT mt_if  ASSIGNING <if> WHERE end_ind = 0.
            ENDLOOP.
          ENDIF.
        ENDIF.

        IF <line>-cond = 'WHEN'.
          ADD 1 TO when_count.
        ENDIF.

        IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.

          <line>-els_before = els_before.
          <line>-els_after = <line>-ind.
          DATA(counter) = <line>-ind + 1.
          DO.
            READ TABLE results INDEX counter INTO line.
            IF sy-subrc <> 0.
              CLEAR <line>-els_after.
              EXIT.
            ENDIF.

            IF line-cond = 'ELSE' OR line-cond = 'ELSEIF'.
              CLEAR <line>-els_after.
              EXIT.
            ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
              <line>-els_after = counter.
              EXIT.
            ELSE.
              ADD 1 TO counter.

            ENDIF.
          ENDDO.
          IF when_count = 1. "to refactor
*          <if>-if_ind = els_before.
*          CLEAR <line>-els_before.
          ENDIF.
        ENDIF.

        IF <line>-cond = 'WHEN'.

          <line>-els_before = els_before.
          <line>-els_after = <line>-ind.
          counter = <line>-ind + 1.
          DO.
            READ TABLE results INDEX counter INTO line.
            IF sy-subrc <> 0.
              CLEAR <line>-els_after.
              EXIT.
            ENDIF.

            IF line-cond = 'WHEN'.
              CLEAR <line>-els_after.
              EXIT.
            ELSEIF  line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
              <line>-els_after = counter.
              EXIT.
            ELSE.
              ADD 1 TO counter.

            ENDIF.
          ENDDO.
          IF when_count = 1.
            IF <if> IS ASSIGNED. "to refactor
              <if>-if_ind = els_before.
            ENDIF.
            CLEAR <line>-els_before.
          ENDIF.
        ENDIF.

        IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
          els_before = <line>-ind.
        ELSE.
          CLEAR   els_before.
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
      "remove duplicate lines (same line+include, keep first occurrence) quick fix instead of fixing real bug )) to refactor
      DATA lt_seen TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      LOOP AT results ASSIGNING FIELD-SYMBOL(<res>).
        DATA(lv_key) = |{ <res>-include }:{ <res>-line }|.
        INSERT lv_key INTO TABLE lt_seen.
        IF sy-subrc <> 0.
          <res>-del = abap_true.
        ENDIF.
      ENDLOOP.
      DELETE results WHERE del = abap_true.


  endmethod.


  method GET_CODE_MIX.

      DATA: flow_lines TYPE sci_include,
            splits     TYPE TABLE OF string,

            ind        TYPE i,
            prev_line  TYPE ts_line.

      DATA(lines) = get_code_flow( ).
      LOOP AT mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog_mix>).
        CLEAR <prog_mix>-selected.
      ENDLOOP.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = 'Code_Flow_Mix' ASSIGNING <prog_mix>.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO mo_window->ms_sources-tt_progs ASSIGNING <prog_mix>.
        INSERT INITIAL LINE INTO mo_window->mt_stack INDEX 1 ASSIGNING FIELD-SYMBOL(<stack_mix>).
        <prog_mix>-include = <stack_mix>-program = <stack_mix>-include = 'Code_Flow_Mix'.
      ENDIF.

      LOOP AT lines INTO DATA(line).
        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = line-include INTO DATA(prog).
        READ TABLE prog-t_keywords WITH KEY line = line-line INTO DATA(keyword).

        APPEND INITIAL LINE TO <prog_mix>-t_keywords ASSIGNING FIELD-SYMBOL(<keyword_mix>).
        <keyword_mix> = keyword.
        <keyword_mix>-include = line-include.
        <keyword_mix>-program = line-program.

        DATA(from_row) = prog-scan->tokens[ keyword-from ]-row.
        DATA(to_row) = prog-scan->tokens[ keyword-to ]-row.
        DATA(spaces) = repeat( val = | | occ = ( line-stack - 1 ) * 3 ).
        DATA(dashes) = repeat( val = |-| occ = ( line-stack ) ).
        IF prev_line-ev_name <> line-ev_name OR prev_line-ev_type <> line-ev_type OR prev_line-class <> line-class OR prev_line-stack <> line-stack. "new event
          SPLIT line-include AT '=' INTO TABLE splits.

          APPEND INITIAL LINE TO flow_lines ASSIGNING FIELD-SYMBOL(<flow>).
          ind  = sy-tabix.
          IF line-class IS INITIAL.
            <flow> =  |"{ dashes } { line-ev_type } { line-ev_name } in { splits[ 1 ] }|.
          ELSE.
            <flow> =  |"{ dashes } { line-ev_type } { line-ev_name } in { line-class }|.
          ENDIF.
        ENDIF.

        <keyword_mix>-v_line = ind + 1.

        LOOP AT prog-source_tab FROM from_row TO to_row INTO DATA(source_line).
          APPEND INITIAL LINE TO flow_lines ASSIGNING <flow>.
          ind = sy-tabix.
          <flow> = |{ spaces }{ source_line }|.
        ENDLOOP.
        prev_line = line.
      ENDLOOP.

      mo_window->mo_code_viewer->set_text( table = flow_lines ).
      <prog_mix>-source_tab = flow_lines.
      <prog_mix>-selected = abap_true.
      mo_window->m_prg-include = 'Code_Flow_Mix'.
      mo_window->set_mixprog_line( ).
      mo_window->show_stack( ).
      mo_window->mo_box->set_caption( |Code Mix: { lines( lines ) } statements| ).


  endmethod.


  method SHOW.


      DATA: tree        TYPE ZCL_ACE_APPL=>ts_tree,
            cl_name     TYPE string,
            icon        TYPE salv_de_tree_image,
            forms_rel   TYPE salv_de_node_key,
            modules_rel TYPE salv_de_node_key,
            f_modules   TYPE salv_de_node_key,
            func_rel    TYPE salv_de_node_key,
            classes_rel TYPE salv_de_node_key,
            class_rel   TYPE salv_de_node_key,
            events_rel  TYPE salv_de_node_key,
            globals_rel TYPE salv_de_node_key,
            splits_prg  TYPE TABLE OF string,
            splits_incl TYPE TABLE OF string,
            splits_intf TYPE TABLE OF string.

      IF  mo_window->m_prg-include IS INITIAL.
        mo_window->m_prg-program =  mo_window->m_prg-include = mv_prog.
      ENDIF.
      mo_window->set_program( CONV #( mo_window->m_prg-include ) ).
      IF mo_window->m_prg-include <> 'Code_Flow_Mix'.
        mo_window->show_coverage( ).
      ENDIF.
      IF  mo_window->m_prg-line IS INITIAL AND mo_window->mt_stack IS NOT INITIAL.
        mo_window->m_prg-line = mo_window->mt_stack[ 1 ]-line.
      ENDIF.
      mo_window->set_program_line( 1 ).

      SORT mo_window->ms_sources-t_params BY class event type DESCENDING param ASCENDING.
      SORT mo_window->ms_sources-tt_progs BY stack program.
      DELETE mo_window->ms_sources-tt_progs WHERE t_keywords IS INITIAL.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY program = mo_window->m_prg-program INTO DATA(prog).

      mo_window->show_stack( ).
      CHECK mo_tree_local->main_node_key IS INITIAL.
      mo_tree_local->clear( ).
      SPLIT mo_window->m_prg-program AT '=' INTO TABLE splits_prg.
      CHECK splits_prg IS NOT INITIAL.
      tree-kind = 'F'. "Folder - pre expanded

      mo_tree_local->main_node_key = mo_tree_local->add_node( i_name = CONV #( mo_window->m_prg-program ) i_icon = CONV #( icon_folder )  i_tree = tree ).

      " Enhancements branch - first in tree
      DATA: enh_rel      TYPE salv_de_node_key,
            enh_form_rel TYPE salv_de_node_key.
      LOOP AT mo_window->ms_sources-tt_progs INTO DATA(prog_enh).
        LOOP AT prog_enh-tt_enh_blocks INTO DATA(enh_blk).
          IF enh_rel IS INITIAL.
            CLEAR tree.
            tree-kind = 'F'.
            enh_rel = mo_tree_local->add_node( i_name = 'Enhancements' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
          ENDIF.
          DATA(enh_node_name) = |{ enh_blk-enh_name } { enh_blk-ev_type } { enh_blk-ev_name } ({ enh_blk-position })|.
          CLEAR tree.
          tree-kind    = 'M'.
          tree-value   = enh_blk-position.  " position: BEGIN/END/OVERWRITE for double-click
          tree-include = prog_enh-include.
          tree-program = prog_enh-program.
          tree-ev_type = enh_blk-ev_type.
          tree-ev_name = enh_blk-ev_name.
          tree-param   = enh_blk-enh_include.  " EIMP include for double-click navigation
          tree-enh_id  = enh_blk-enh_id.
          mo_tree_local->add_node( i_name = enh_node_name i_icon = CONV #( icon_modify ) i_rel = enh_rel i_tree = tree ).
        ENDLOOP.
      ENDLOOP.
      CLEAR tree.
      LOOP AT  mo_window->ms_sources-t_vars INTO DATA(var) WHERE program = mo_window->m_prg-program AND eventtype IS INITIAL AND class IS INITIAL.
        IF globals_rel IS INITIAL.
          globals_rel = mo_tree_local->add_node( i_name = 'Global Vars' i_icon = CONV #( icon_header ) i_rel = mo_tree_local->main_node_key ).
        ENDIF.
        tree-value = var-line.
        tree-param = var-name.
        mo_tree_local->add_node( i_name = var-name i_icon = var-icon i_rel = globals_rel i_tree = tree ).
      ENDLOOP.

      "Virtual Start event - first executable step
      READ TABLE mo_window->mo_viewer->mt_steps INDEX 1 INTO DATA(step).
      IF step-line IS NOT INITIAL AND step-program = mo_window->m_prg-program.
        IF events_rel IS INITIAL.
          tree-kind = 'F'.
          events_rel = mo_tree_local->add_node( i_name = 'Events' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
        ENDIF.
        tree-value = step-line.
        tree-kind = 'E'.
        tree-include = step-include.
        mo_tree_local->add_node( i_name = 'Code Flow start line' i_icon = CONV #( icon_oo_event ) i_rel = events_rel i_tree = tree ).
      ENDIF.

      CLEAR tree.
      LOOP AT mo_window->ms_sources-t_events INTO DATA(event).
        IF events_rel IS INITIAL.
          tree-kind = 'F'.
          events_rel = mo_tree_local->add_node( i_name = 'Events' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
        ENDIF.
        tree-include = event-include.
        tree-value = event-line.
        mo_tree_local->add_node( i_name = event-name i_icon = CONV #( icon_oo_event ) i_rel = events_rel i_tree = tree ).
      ENDLOOP.

      "FUNCTIONS
      LOOP AT mo_window->ms_sources-tt_progs INTO prog WHERE program+0(4) = 'SAPL'  .

        DATA: fname              TYPE rs38l_fnam,
              exception_list     TYPE TABLE OF  rsexc,
              export_parameter   TYPE TABLE OF  rsexp,
              import_parameter   TYPE TABLE OF  rsimp,
              changing_parameter TYPE TABLE OF    rscha,
              tables_parameter   TYPE TABLE OF    rstbl,
              search_name        TYPE string,
              incl_nr            TYPE includenr.

        DATA(len) = strlen( prog-include ).
        len = len - 2.
        incl_nr = prog-include+len(2).

        SELECT SINGLE funcname INTO @DATA(funcname)
          FROM tfdir
         WHERE pname = @prog-program
           AND include = @incl_nr.

        CHECK sy-subrc = 0.
        IF f_modules IS INITIAL.
          tree-kind = 'F'.
          f_modules = mo_tree_local->add_node( i_name =  'Function Modules' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key  i_tree = tree ).
        ENDIF.
        CLEAR tree.
        func_rel =  mo_tree_local->add_node( i_name =  CONV #( funcname ) i_icon = CONV #( icon_folder ) i_rel = f_modules  i_tree = tree ).

        fname = funcname.
        CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
          EXPORTING
            funcname           = fname
          TABLES
            exception_list     = exception_list
            export_parameter   = export_parameter
            import_parameter   = import_parameter
            changing_parameter = changing_parameter
            tables_parameter   = tables_parameter
          EXCEPTIONS
            error_message      = 1
            function_not_found = 2
            invalid_name       = 3
            OTHERS             = 4.
        IF sy-subrc = 0.

          LOOP AT import_parameter INTO DATA(imp).
            mo_tree_local->add_node( i_name =  CONV #( imp-parameter ) i_icon = CONV #( icon_parameter_import ) i_rel = func_rel  i_tree = tree ).
          ENDLOOP.

          LOOP AT export_parameter INTO DATA(exp).
            mo_tree_local->add_node( i_name =  CONV #( exp-parameter ) i_icon = CONV #( icon_parameter_export ) i_rel = func_rel  i_tree = tree ).
          ENDLOOP.
          LOOP AT changing_parameter INTO DATA(change).
            mo_tree_local->add_node( i_name =  CONV #( change-parameter ) i_icon = CONV #( icon_parameter_changing ) i_rel = func_rel  i_tree = tree ).
          ENDLOOP.
          LOOP AT tables_parameter INTO DATA(table).
            mo_tree_local->add_node( i_name =  CONV #( table-parameter ) i_icon = CONV #( icon_parameter_table ) i_rel = func_rel  i_tree = tree ).
          ENDLOOP.
        ENDIF.

      ENDLOOP.

      IF lines( splits_prg ) = 1.
        DATA: local      TYPE string,
              locals_rel TYPE salv_de_node_key.
        LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs) WHERE program = mv_prog AND eventtype = 'METHOD' AND class  <> splits_prg[ 1 ].
          IF local <> subs-class.
            IF locals_rel IS INITIAL.
              tree-kind = 'F'.
              locals_rel = mo_tree_local->add_node( i_name = 'Local Classes' i_icon = CONV #( icon_folder ) i_rel = class_rel i_tree = tree ).
            ENDIF.
            add_class( i_class = CONV #( subs-class ) i_refnode = locals_rel  no_locals = abap_true ).
          ENDIF.
          local = subs-class.
        ENDLOOP.
      ENDIF.

      IF class_rel IS INITIAL.
        classes_rel = class_rel = mo_tree_local->main_node_key.
      ENDIF.

      SORT mo_window->ms_sources-tt_calls_line BY program class eventtype meth_type eventname .

      cl_name = splits_prg[ 1 ]."let's find hierarchy start

      DO.
        READ TABLE mo_window->ms_sources-t_classes WITH KEY clsname = cl_name reltype = '2' INTO DATA(ls_class).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        cl_name = ls_class-refclsname.
      ENDDO.

      DO.
        "tree-kind = 'F'.
        add_class( i_class = CONV #( cl_name ) i_refnode = classes_rel i_tree = tree ).
        CLEAR tree.

        READ TABLE mo_window->ms_sources-t_classes WITH KEY refclsname = cl_name reltype = '2' INTO ls_class.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        cl_name = ls_class-clsname.

      ENDDO.

      classes_rel = class_rel = mo_tree_local->main_node_key.

      LOOP AT mo_window->ms_sources-tt_calls_line INTO subs. "WHERE include IS NOT INITIAL.
        SPLIT subs-include AT '=' INTO TABLE splits_incl.
        READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs-include INTO prog.
        READ TABLE prog-t_keywords WITH KEY index = subs-index INTO DATA(keyword).
        DATA(form_name) = subs-eventname.

        IF subs-eventtype = 'FORM'.
          IF subs-program = splits_prg[ 1 ].
            IF forms_rel IS INITIAL.
              tree-kind = 'F'.
              forms_rel = mo_tree_local->add_node( i_name = 'Subroutines' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
            ENDIF.
            CLEAR tree.
            tree-kind    = 'M'.
            tree-value   = keyword-v_line.
            tree-include = subs-include.
            tree-program = subs-program.
            tree-ev_type = subs-eventtype.
            tree-ev_name = subs-eventname.
            DATA(event_node) = mo_tree_local->add_node( i_name = form_name i_icon = CONV #( icon_biw_info_source_ina ) i_rel = forms_rel i_tree = tree ).

            CLEAR tree.
            LOOP AT mo_window->ms_sources-t_params INTO DATA(param) WHERE event = 'FORM' AND name = subs-eventname  AND param IS NOT INITIAL.

              CASE param-type.
                WHEN 'I'.
                  icon = icon_parameter_import.
                WHEN 'E'.
                  icon = icon_parameter_export.
              ENDCASE.
              tree-param = param-param.

              mo_tree_local->add_node( i_name =  param-param i_icon = icon i_rel =  event_node i_tree = tree ).

            ENDLOOP.
            CLEAR tree.

          ENDIF.
          .
        ENDIF.

        IF subs-eventtype = 'MODULE'.
          IF subs-program = splits_prg[ 1 ].
            IF modules_rel IS INITIAL.
              tree-kind = 'F'.
              modules_rel = mo_tree_local->add_node( i_name = 'Modules' i_icon = CONV #( icon_folder ) i_rel = mo_tree_local->main_node_key i_tree = tree ).
            ENDIF.
            CLEAR tree.
            tree-kind    = 'M'.
            tree-value   = keyword-v_line.
            tree-include = subs-include.
            tree-program = subs-program.
            tree-ev_type = subs-eventtype.
            tree-ev_name = subs-eventname.
            mo_tree_local->add_node( i_name = subs-eventname i_icon = CONV #( icon_biw_info_source_ina ) i_rel = modules_rel i_tree = tree ).
            CLEAR tree.
          ENDIF.
        ENDIF.
        cl_name = splits_prg[ 1 ].
      ENDLOOP.

      mo_tree_local->display( ).


  endmethod.
ENDCLASS.
