class ZCL_ACE_SOURCE_PARSER definition
  public
  create public .

public section.

  class-methods PARSE_TOKENS
    importing
      !I_PROGRAM type PROGRAM
      !I_MAIN type BOOLEAN optional
      !I_INCLUDE type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE
      !I_CLASS type STRING optional
      !I_EVNAME type STRING optional
      !I_STACK type I optional
      !I_MAIN_PROG type PROGRAM optional
      !I_RELTYPE type SEORELTYPE optional .
  class-methods PARSE_CALL
    importing
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_INDEX type I
      !I_STACK type I
      !I_E_NAME type STRING
      !I_E_TYPE type STRING
      !I_CLASS type STRING optional
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PARSE_CALL_FORM
    importing
      !I_CALL_NAME type STRING
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_STACK type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PARSE_CLASS
    importing
      !KEY type ZCL_ACE=>TS_KWORD
      !I_INCLUDE type PROGRAM
      !I_CALL type ZCL_ACE=>TS_CALLS
      !I_STACK type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PARSE_SCREEN
    importing
      !KEY type ZCL_ACE=>TS_KWORD
      !I_STACK type I
      !I_CALL type ZCL_ACE=>TS_CALLS
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods CODE_EXECUTION_SCANNER
    importing
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_EVNAME type STRING optional
      !I_EVTYPE type STRING optional
      !I_STACK type I optional
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods LINK_CALLS_TO_PARAMS
    importing
      !IO_DEBUGGER type ref to ZCL_ACE
    changing
      !CT_TOKENS type ZCL_ACE=>TT_KWORD .
  class-methods PROCESS_SUPER_AND_INTERFACES
    importing
      !I_CLASS type STRING
      !I_PROGRAM type PROGRAM
      !I_STACK type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods WORD_DEPENDENCIES_ANALYSIS
    importing
      !KW type STRING
      !TEMP type CHAR30
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE
      !L_TOKEN_ROW type I
    changing
      !CS_STATE type ZCL_ACE=>TS_PARSE_STATE
    returning
      value(RV_CONTINUE) type BOOLEAN .
  class-methods DETECT_METHOD_CALL
    importing
      !WORD type STRING
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_CLASS type STRING
      !IO_DEBUGGER type ref to ZCL_ACE
      !L_TOKEN_ROW type I
      !NEW type BOOLEAN
    changing
      !CALL type ZCL_ACE=>TS_CALLS
      !CALL_LINE type ZCL_ACE=>TS_CALLS_LINE
      !CALCULATED type ZCL_ACE=>TS_VAR
      !CALCULATED_VARS type ZCL_ACE=>TT_CALCULATED
      !CLASS_NAME type STRING
      !TOKEN type ZCL_ACE=>TS_KWORD .
  class-methods REGISTER_FIELD_SYMBOL
    importing
      !I_INCLUDE type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE
    changing
      !CS_STATE type ZCL_ACE=>TS_PARSE_STATE .
  class-methods COLLECT_EVENTS
    importing
      !IO_SCAN type ref to CL_CI_SCAN
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods COLLECT_ENHANCEMENTS
    importing
      !I_PROGRAM type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods COLLECT_METHOD_ENHANCEMENTS
    importing
      !I_ENHNAME type ENHNAME
      !I_ENHINCLUDE type PROGRAM
      !I_METHOD type STRING
      !I_CLASS type STRING
      !I_METH_POS type STRING
      !I_ID type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PROCESS_WORDS
    importing
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_CLASS type STRING
      !I_MAIN_PROG type PROGRAM
      !I_RELTYPE type SEORELTYPE optional
      !IO_DEBUGGER type ref to ZCL_ACE
      !L_TOKEN_ROW type I
      !O_PROCEDURE type ref to IF_CI_KZN_STATEMENT_ITERATOR
    changing
      !CS_STATE type ZCL_ACE=>TS_PARSE_STATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_SOURCE_PARSER IMPLEMENTATION.


  method CODE_EXECUTION_SCANNER.

      DATA: max       TYPE i,
            call_line TYPE ZCL_ACE=>ts_calls_line,
            program   TYPE program,
            include   TYPE program,
            prefix    TYPE string,
            event     TYPE string,
            stack     TYPE i,
            statement TYPE i,
            prog      TYPE ZCL_ACE_WINDOW=>ts_prog.

      SORT io_debugger->mo_window->ms_sources-tt_calls_line.

      READ TABLE io_debugger->mt_steps WITH KEY program = i_include eventname = i_evname eventtype = i_evtype TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      stack =  i_stack + 1.
      CHECK  stack <=  io_debugger->mo_window->m_hist_depth.

      ZCL_ACE_SOURCE_PARSER=>parse_tokens( i_stack = stack i_program = i_program i_include = i_include io_debugger = io_debugger ).
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include ASSIGNING FIELD-SYMBOL(<prog>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      DATA: structures LIKE <prog>-scan->structures.

      LOOP AT <prog>-scan->structures INTO DATA(structure) WHERE type = 'E' AND ( stmnt_type = '1' OR stmnt_type = '2' OR stmnt_type = '3' ) .
      ENDLOOP.

      IF sy-subrc = 0.
        structures = <prog>-scan->structures.
        DELETE structures WHERE type <> 'E'.
        LOOP AT structures  ASSIGNING FIELD-SYMBOL(<structure>) WHERE stmnt_type = 'g'.
          CLEAR <structure>-stmnt_type.
        ENDLOOP.
        SORT structures BY stmnt_type ASCENDING.
      ELSE.
        CLEAR  max.
        LOOP AT <prog>-scan->structures INTO DATA(str) WHERE type <> 'C' AND type <> 'R'.
          IF str-type = 'P' AND  str-stmnt_type = '?'.
            CONTINUE.
          ENDIF.
          IF  max < str-stmnt_to.
            max = str-stmnt_to.
            APPEND str TO structures.
          ENDIF.
        ENDLOOP.
      ENDIF.

      LOOP AT structures INTO str.

        IF str-type = 'E'.
          READ TABLE io_debugger->mo_window->ms_sources-t_events WITH KEY program = i_program stmnt_type = str-stmnt_type  stmnt_from = str-stmnt_from ASSIGNING FIELD-SYMBOL(<event>).
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = <event>-include INTO prog.

          READ TABLE prog-scan->statements INDEX <event>-stmnt_from INTO DATA(command).
          READ TABLE prog-scan->levels INDEX command-level INTO DATA(level).
          CLEAR event.
          LOOP AT prog-scan->tokens FROM command-from TO command-to INTO DATA(word).
            IF event IS INITIAL.
              event = word-str.
            ELSE.
              event = |{ event } { word-str }|.
            ENDIF.
          ENDLOOP.

          <event>-name = event.
          <event>-line = word-row.

          statement = <event>-stmnt_from + 1.

        ELSE.
          statement = str-stmnt_from.
          prog = <prog>.
        ENDIF.

        READ TABLE prog-t_keywords WITH KEY index =  str-stmnt_from INTO DATA(key).
        IF key IS NOT INITIAL.
          ZCL_ACE_SOURCE_PARSER=>parse_tokens( i_stack = stack i_program = CONV #( key-program ) i_include = CONV #( key-include ) io_debugger = io_debugger ).
        ENDIF.

        WHILE  statement <= str-stmnt_to.
          READ TABLE prog-t_keywords WITH KEY index = statement INTO key.

          IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS'
            OR key-name = 'PARAMETERS' OR key-name = 'INCLUDE' OR key-name = 'REPORT'
            OR key-name = 'PUBLIC' OR key-name = 'PROTECTED' OR key-name = 'PRIVATE'
            OR key-name IS INITIAL OR sy-subrc <> 0 OR key-sub IS NOT INITIAL.

            ADD 1 TO  statement.
            CONTINUE.
          ENDIF.
          ADD 1 TO io_debugger->m_step.

          READ TABLE io_debugger->mt_steps WITH KEY line = key-line program = i_program include = key-include TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).

            <step>-step = io_debugger->m_step.
            <step>-line = key-line.
            IF i_evtype IS INITIAL.
              <step>-eventtype = 'EVENT'.
              <step>-eventname =  event.
            ELSE.
              <step>-eventtype = i_evtype.
              <step>-eventname = i_evname.
            ENDIF.
            <step>-stacklevel =  stack.
            <step>-program = i_program.
            <step>-include = key-include.
            IF  <step>-eventtype = 'METHOD'.

            ENDIF.
          ENDIF.

          LOOP AT key-tt_calls INTO DATA(call).
            IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).

              IF call-event = 'FORM'.
                parse_call_form( i_call_name = call-name
                                 i_program   = CONV #( call_line-program )
                                 i_include   = CONV #( call_line-include )
                                 i_stack     = stack
                                 io_debugger = io_debugger ).
              ELSEIF call-event = 'FUNCTION'.
                DATA:  func TYPE rs38l_fnam.
                func = call-name.
                IF io_debugger->mo_window->m_zcode IS INITIAL OR
                 ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND (  func+0(1) = 'Z' OR  func+0(1) = 'Y' ) ) .

                  CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                    CHANGING
                      funcname            = func
                      include             = include
                    EXCEPTIONS
                      function_not_exists = 1
                      include_not_exists  = 2
                      group_not_exists    = 3
                      no_selections       = 4
                      no_function_include = 5
                      OTHERS              = 6.

                  code_execution_scanner( i_program =  include i_include =  include i_stack =  stack i_evtype = call-event i_evname = call-name io_debugger = io_debugger ).
                ENDIF.
              ELSEIF call-event = 'METHOD'.
                parse_class( i_include = i_include i_call = call i_stack = stack io_debugger = io_debugger key = key ).
              ELSEIF call-event = 'SCREEN'.
                parse_screen( i_stack = stack i_call = call io_debugger = io_debugger key = key ).
              ENDIF.
            ENDIF.
          ENDLOOP.

          ADD 1 TO  statement.
        ENDWHILE.

      ENDLOOP.


  endmethod.


  method COLLECT_ENHANCEMENTS.


      DATA: form_name    TYPE string,
            position     TYPE string,
            enh_prog     TYPE program,
            tabix        TYPE i.

      TYPES: BEGIN OF ts_form_offset,
               form_name TYPE string,
               include   TYPE program,
               offset    TYPE i,
             END OF ts_form_offset.
      DATA lt_form_offsets TYPE STANDARD TABLE OF ts_form_offset.
      DATA lv_offset       TYPE i.

      DATA(lv_enh_prog) = i_program.
      SELECT SINGLE master FROM d010inc
        INTO @DATA(lv_master)
        WHERE include = @i_program.
      IF sy-subrc = 0 AND lv_master IS NOT INITIAL.
        lv_enh_prog = lv_master.
      ENDIF.

      DATA(lv_prog_enh_tabix) = 0.
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = i_program
        ASSIGNING FIELD-SYMBOL(<prog_enh>).
      IF sy-subrc = 0.
        lv_prog_enh_tabix = sy-tabix.
        IF <prog_enh>-enh_collected = abap_true.
          RETURN.
        ENDIF.
      ENDIF.

      SELECT programname, enhname, enhinclude, id, full_name, enhmode
        FROM d010enh
        INTO TABLE @DATA(lt_enh)
        WHERE programname = @lv_enh_prog
          AND version = 'A'.

      CHECK lt_enh IS NOT INITIAL.

      TYPES: BEGIN OF ts_enh_ext,
               programname  TYPE d010enh-programname,
               enhname      TYPE d010enh-enhname,
               enhinclude   TYPE d010enh-enhinclude,
               id           TYPE d010enh-id,
               full_name    TYPE d010enh-full_name,
               enhmode      TYPE d010enh-enhmode,
               enhtype      TYPE i,
               full_name_30 TYPE c LENGTH 30,
             END OF ts_enh_ext.
      DATA lt_enh_ext TYPE STANDARD TABLE OF ts_enh_ext.
      LOOP AT lt_enh INTO DATA(ls_enh_raw).
        DATA(ls_ext) = CORRESPONDING ts_enh_ext( ls_enh_raw ).
        ls_ext-enhtype = COND i(
          WHEN ls_ext-enhmode = 'D' AND ls_ext-full_name CS '%_BEGIN'    THEN 1
          WHEN ls_ext-enhmode = 'D' AND ls_ext-full_name CS '%_END'      THEN 2
          WHEN ls_ext-enhmode <> 'D' AND ls_ext-full_name CS '\SE:BEGIN' THEN 1
          WHEN ls_ext-enhmode <> 'D' AND ls_ext-full_name CS '\SE:END'   THEN 2
          ELSE 1 ).
        ls_ext-full_name_30 = ls_ext-full_name.
        APPEND ls_ext TO lt_enh_ext.
      ENDLOOP.
      SORT lt_enh_ext BY full_name_30 ASCENDING enhtype ASCENDING.

      LOOP AT lt_enh_ext INTO DATA(ls_enh).
        CLEAR: form_name, position.

        DATA(lv_full) = ls_enh-full_name.

        IF ls_enh-enhmode = 'D'.
          DATA(lv_class_name) = ``.
          DATA(lv_method_name) = ``.
          FIND FIRST OCCURRENCE OF REGEX '\\TY:([^\\]+)' IN lv_full SUBMATCHES lv_class_name.
          FIND FIRST OCCURRENCE OF REGEX '\\ME:([^\\]+)' IN lv_full SUBMATCHES lv_method_name.
          FIND FIRST OCCURRENCE OF REGEX '\\SE:([^\\]+)' IN lv_full SUBMATCHES position.
          CHECK lv_class_name IS NOT INITIAL AND lv_method_name IS NOT INITIAL AND position IS NOT INITIAL.

          DATA(lv_meth_pos) = SWITCH string(
            position
            WHEN '%_BEGIN' THEN 'BEGIN'
            WHEN '%_END'   THEN 'END'
            ELSE `` ).
          CHECK lv_meth_pos IS NOT INITIAL.

          DATA(lv_has_overwrite) = abap_false.
          IF lv_meth_pos = 'BEGIN'.
            collect_method_enhancements(
              EXPORTING i_enhname    = CONV #( ls_enh-enhname )
                        i_enhinclude = CONV #( ls_enh-enhinclude )
                        i_method     = lv_method_name
                        i_class      = lv_class_name
                        i_meth_pos   = 'OVERWRITE'
                        i_id         = CONV #( ls_enh-id )
                        io_debugger  = io_debugger ).
            READ TABLE io_debugger->mo_window->ms_sources-tt_progs TRANSPORTING NO FIELDS
              WITH KEY program = lv_class_name.
            LOOP AT io_debugger->mo_window->ms_sources-tt_progs INTO DATA(ls_prog_ow).
              READ TABLE ls_prog_ow-tt_enh_blocks TRANSPORTING NO FIELDS
                WITH KEY ev_name = lv_method_name position = 'OVERWRITE'.
              IF sy-subrc = 0.
                lv_has_overwrite = abap_true.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF lv_has_overwrite = abap_false.
            collect_method_enhancements(
              EXPORTING i_enhname    = CONV #( ls_enh-enhname )
                        i_enhinclude = CONV #( ls_enh-enhinclude )
                        i_method     = lv_method_name
                        i_class      = lv_class_name
                        i_meth_pos   = lv_meth_pos
                        i_id         = CONV #( ls_enh-id )
                        io_debugger  = io_debugger ).
          ENDIF.

        ELSE.
          FIND FIRST OCCURRENCE OF REGEX '\\FO:([^\\]+)' IN lv_full SUBMATCHES form_name.
          FIND FIRST OCCURRENCE OF REGEX '\\SE:([^\\]+)' IN lv_full SUBMATCHES position.
          CHECK form_name IS NOT INITIAL AND position IS NOT INITIAL.
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = ls_enh-enhinclude TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            ZCL_ACE_SOURCE_PARSER=>parse_tokens(
              i_program   = CONV #( ls_enh-enhinclude )
              i_include   = CONV #( ls_enh-enhinclude )
              io_debugger = io_debugger ).
          ENDIF.

          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
            WITH KEY eventtype = 'FORM' eventname = form_name
            INTO DATA(ls_call_line).
          CHECK sy-subrc = 0.

          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = ls_call_line-include
            ASSIGNING FIELD-SYMBOL(<prog>).
          CHECK sy-subrc = 0.

          DATA(lv_offset_before) = lv_offset.

          DATA(lv_form_tabix) = 0.
          DATA ls_kw_form TYPE ZCL_ACE=>ts_kword.
          LOOP AT <prog>-t_keywords INTO ls_kw_form.
            IF ls_kw_form-name = 'FORM' AND ls_kw_form-index = ls_call_line-index.
              lv_form_tabix = sy-tabix.
              EXIT.
            ENDIF.
          ENDLOOP.
          CHECK lv_form_tabix > 0.
          READ TABLE <prog>-v_keywords WITH KEY include = ls_kw_form-include
            index = ls_kw_form-index INTO DATA(ls_vkw_form).
          IF sy-subrc = 0. ls_kw_form-v_line = ls_vkw_form-v_line. ENDIF.

          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = ls_enh-enhinclude
            INTO DATA(ls_enh_prog).
          CHECK sy-subrc = 0.

          DATA lt_enh_kw TYPE ZCL_ACE_WINDOW=>tt_kword.
          DATA lv_in_block TYPE boolean.
          CLEAR: lt_enh_kw, lv_in_block.
          LOOP AT ls_enh_prog-t_keywords INTO DATA(ls_kw).
            IF ls_kw-name = 'ENHANCEMENT'.
              DATA(lv_enh_id) = CONV i( ls_enh-id ).
              READ TABLE ls_enh_prog-scan->statements INDEX ls_kw-index INTO DATA(ls_stmt).
              READ TABLE ls_enh_prog-scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
              IF CONV i( ls_tok-str ) = lv_enh_id.
                lv_in_block = abap_true.
                APPEND ls_kw TO lt_enh_kw.
              ELSE.
                lv_in_block = abap_false.
              ENDIF.
              CONTINUE.
            ENDIF.
            IF ls_kw-name = 'ENDENHANCEMENT'.
              IF lv_in_block = abap_true.
                APPEND ls_kw TO lt_enh_kw.
              ENDIF.
              CLEAR lv_in_block.
              CONTINUE.
            ENDIF.
            IF lv_in_block = abap_true.
              APPEND ls_kw TO lt_enh_kw.
            ENDIF.
          ENDLOOP.

          CHECK lt_enh_kw IS NOT INITIAL.

          DATA ls_kw_end TYPE ZCL_ACE=>ts_kword.
          CLEAR ls_kw_end.
          IF position = 'BEGIN'.
            tabix = lv_form_tabix + 1.
          ELSE.
            tabix = lv_form_tabix + 1.
            LOOP AT <prog>-t_keywords INTO ls_kw_end FROM tabix.
              IF ls_kw_end-name = 'ENDFORM'.
                tabix = sy-tabix.
                EXIT.
              ENDIF.
              CLEAR ls_kw_end.
            ENDLOOP.
            READ TABLE <prog>-v_keywords WITH KEY include = ls_kw_end-include
              index = ls_kw_end-index INTO DATA(ls_vkw_end).
            IF sy-subrc = 0. ls_kw_end-v_line = ls_vkw_end-v_line. ENDIF.
          ENDIF.

          DATA(lv_insert_line) = COND i(
            WHEN position = 'BEGIN' THEN ls_kw_form-line + 1
            ELSE ls_kw_end-line ).

          DATA(lv_vsrc_tabix) = COND i(
            WHEN position = 'BEGIN' THEN ls_kw_form-v_line + 1
            ELSE ls_kw_end-v_line ).

          DATA(lv_vkw_tabix) = 0.
          IF position = 'BEGIN'.
            LOOP AT <prog>-v_keywords INTO DATA(ls_vkw_anchor)
              WHERE include = ls_kw_form-include AND index = ls_kw_form-index AND name = 'FORM'.
              lv_vkw_tabix = sy-tabix + 1.
              EXIT.
            ENDLOOP.
          ELSE.
            LOOP AT <prog>-v_keywords INTO ls_vkw_anchor
              WHERE include = ls_kw_end-include AND index = ls_kw_end-index AND name = 'ENDFORM'.
              lv_vkw_tabix = sy-tabix.
              EXIT.
            ENDLOOP.
          ENDIF.
          IF lv_vkw_tabix = 0.
            lv_vkw_tabix = lines( <prog>-v_keywords ) + 1.
          ENDIF.

          DATA(lv_enh_inserted) = 1.
          DATA(lv_tmp_line) = lt_enh_kw[ 1 ]-line.
          DATA(lv_tmp_last) = lt_enh_kw[ lines( lt_enh_kw ) ]-line.
          WHILE lv_tmp_line <= lv_tmp_last.
            ADD 1 TO lv_enh_inserted.
            READ TABLE lt_enh_kw WITH KEY line = lv_tmp_line INTO DATA(ls_ins_pre).
            IF sy-subrc = 0 AND ls_ins_pre-name = 'ENDENHANCEMENT'.
              ADD 1 TO lv_enh_inserted.
            ENDIF.
            ADD 1 TO lv_tmp_line.
          ENDWHILE.

          LOOP AT <prog>-v_keywords ASSIGNING FIELD-SYMBOL(<kw_v>)
            WHERE v_line >= lv_vsrc_tabix.
            ADD lv_enh_inserted TO <kw_v>-v_line.
            ADD lv_enh_inserted TO <kw_v>-v_from_row.
            ADD lv_enh_inserted TO <kw_v>-v_to_row.
          ENDLOOP.

          DATA(lv_vkw_vline) = lv_vsrc_tabix + 1.
          LOOP AT lt_enh_kw INTO DATA(ls_vkw_ins).
            ls_vkw_ins-v_line     = lv_vkw_vline.
            ls_vkw_ins-v_from_row = lv_vkw_vline.
            ls_vkw_ins-v_to_row   = lv_vkw_vline.
            INSERT ls_vkw_ins INTO <prog>-v_keywords INDEX lv_vkw_tabix.
            ADD 1 TO lv_vkw_tabix.
            ADD 1 TO lv_vkw_vline.
          ENDLOOP.

          DATA(lv_src_tabix) = lv_vsrc_tabix.
          DATA lv_sep TYPE string.
          IF position = 'BEGIN'.
            lv_sep = |"{ repeat( val = `"` occ = 40 ) } ENH { lv_enh_id } { form_name } BEGIN|.
          ELSE.
            lv_sep = |"{ repeat( val = `"` occ = 40 ) } ENH { lv_enh_id } { form_name } END|.
          ENDIF.
          INSERT CONV string( lv_sep ) INTO <prog>-v_source INDEX lv_src_tabix.
          ADD 1 TO lv_src_tabix. ADD 1 TO lv_offset.
          DATA(lv_first_line) = lt_enh_kw[ 1 ]-line.
          DATA(lv_last_line)  = lt_enh_kw[ lines( lt_enh_kw ) ]-line.
          DATA(lv_cur_line)   = lv_first_line.
          WHILE lv_cur_line <= lv_last_line.
            READ TABLE ls_enh_prog-source_tab INDEX lv_cur_line INTO DATA(lv_fe_src_line).
            IF sy-subrc = 0.
              READ TABLE lt_enh_kw WITH KEY line = lv_cur_line INTO DATA(ls_ins_chk).
              IF sy-subrc = 0 AND ls_ins_chk-name = 'ENHANCEMENT'.
                REPLACE REGEX '(ENHANCEMENT\s+\d+)(\s+)\.' IN lv_fe_src_line
                  WITH `$1$2` && ls_enh-enhname && `.`.
              ENDIF.
              INSERT lv_fe_src_line INTO <prog>-v_source INDEX lv_src_tabix.
              ADD 1 TO lv_src_tabix. ADD 1 TO lv_offset.
              IF sy-subrc = 0 AND ls_ins_chk-name = 'ENDENHANCEMENT'.
                DATA(lv_end_sep) = |"{ repeat( val = `"` occ = 40 ) } ENH { lv_enh_id } END|.
                INSERT CONV string( lv_end_sep ) INTO <prog>-v_source INDEX lv_src_tabix.
                ADD 1 TO lv_src_tabix. ADD 1 TO lv_offset.
              ENDIF.
            ENDIF.
            ADD 1 TO lv_cur_line.
          ENDWHILE.

          APPEND INITIAL LINE TO <prog>-tt_enh_blocks ASSIGNING FIELD-SYMBOL(<enh_blk>).
          <enh_blk>-ev_type    = ls_call_line-eventtype.
          <enh_blk>-ev_name    = form_name.
          <enh_blk>-position   = position.
          <enh_blk>-enh_name   = ls_enh-enhname.
          <enh_blk>-enh_include = ls_enh-enhinclude.
          <enh_blk>-enh_id     = lv_enh_id.
          <enh_blk>-from_line  = 0.
          <enh_blk>-to_line    = 0.

        ENDIF.

      ENDLOOP.

      IF lv_prog_enh_tabix > 0.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs
          INDEX lv_prog_enh_tabix
          ASSIGNING <prog_enh>.
        IF sy-subrc = 0.
          <prog_enh>-enh_collected = abap_true.
        ENDIF.
      ENDIF.


  endmethod.


  method COLLECT_EVENTS.

      LOOP AT io_scan->structures INTO DATA(struc) WHERE type = 'E'.
        APPEND INITIAL LINE TO io_debugger->mo_window->ms_sources-t_events
          ASSIGNING FIELD-SYMBOL(<event>).
        <event>-program = i_program.
        MOVE-CORRESPONDING struc TO <event>.
        <event>-include = i_include.
      ENDLOOP.

  endmethod.


  method COLLECT_METHOD_ENHANCEMENTS.


      DATA(lv_enhname_trimmed)  = condense( val = CONV string( i_enhname ) ).
      DATA(lv_enhinclude_str)   = condense( val = CONV string( i_enhinclude ) ).
      DATA(lv_eimp_include) = CONV program(
        substring( val = lv_enhinclude_str len = strlen( lv_enhinclude_str ) - 1 ) && 'EIMP' ).

      DATA(lv_impl_prefix) = COND string(
        WHEN i_meth_pos = 'BEGIN'     THEN 'IPR_'
        WHEN i_meth_pos = 'END'       THEN 'IPO_'
        WHEN i_meth_pos = 'OVERWRITE' THEN 'IOW_' ).
      DATA(lv_impl_method) = lv_impl_prefix && lv_enhname_trimmed && '~' && i_method.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = lv_eimp_include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        parse_tokens(
          i_program   = lv_eimp_include
          i_include   = lv_eimp_include
          io_debugger = io_debugger ).
      ENDIF.
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = lv_eimp_include
        INTO DATA(ls_eimp_prog).


      CHECK sy-subrc = 0.

      DATA(lv_class_prog) = CONV program(
        i_class && repeat( val = '=' occ = 30 - strlen( i_class ) ) && 'CP' ).
      DATA(lv_cm_pattern) = lv_class_prog(28) && 'CM%'.
      SELECT include FROM d010inc
        INTO TABLE @DATA(lt_cm_includes)
        WHERE master = @lv_class_prog
          AND include LIKE @lv_cm_pattern.
      LOOP AT lt_cm_includes INTO DATA(ls_cm).
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs
          WITH KEY include = ls_cm-include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          parse_tokens(
            i_program   = ls_cm-include
            i_include   = ls_cm-include
            io_debugger = io_debugger ).
        ENDIF.
      ENDLOOP.

      DATA ls_call_line_m TYPE ZCL_ACE=>ts_calls_line.
      LOOP AT io_debugger->mo_window->ms_sources-tt_calls_line
        INTO ls_call_line_m
        WHERE eventtype = 'METHOD'
          AND eventname = i_method
          AND class     = i_class.
        EXIT.
      ENDLOOP.


      CHECK sy-subrc = 0.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = ls_call_line_m-include
        ASSIGNING FIELD-SYMBOL(<prog_m>).
      CHECK sy-subrc = 0.

      DATA(lv_meth_tabix) = 0.
      DATA ls_kw_meth TYPE ZCL_ACE=>ts_kword.
      LOOP AT <prog_m>-t_keywords INTO ls_kw_meth.
        IF ls_kw_meth-name = 'METHOD' AND ls_kw_meth-index = ls_call_line_m-index.
          lv_meth_tabix = sy-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
      CHECK lv_meth_tabix > 0.

      DATA lt_enh_kw TYPE ZCL_ACE_WINDOW=>tt_kword.
      DATA lv_in_block TYPE boolean.
      LOOP AT ls_eimp_prog-t_keywords INTO DATA(ls_kw).
        IF ls_kw-name = 'METHOD'.
          READ TABLE ls_eimp_prog-scan->statements INDEX ls_kw-index INTO DATA(ls_stmt).
          READ TABLE ls_eimp_prog-scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
          IF ls_tok-str = lv_impl_method OR ls_tok-str CP |IOW_*~{ i_method }|.
            lv_in_block = abap_true.
            APPEND ls_kw TO lt_enh_kw.
          ELSE.
            lv_in_block = abap_false.
          ENDIF.
          CONTINUE.
        ENDIF.
        IF ls_kw-name = 'ENDMETHOD'.
          IF lv_in_block = abap_true.
            APPEND ls_kw TO lt_enh_kw.
            EXIT.
          ENDIF.
          CLEAR lv_in_block.
          CONTINUE.
        ENDIF.
        IF lv_in_block = abap_true.
          APPEND ls_kw TO lt_enh_kw.
        ENDIF.
      ENDLOOP.
      CHECK lt_enh_kw IS NOT INITIAL.


      DATA(lv_ins_tabix) = COND i(
        WHEN i_meth_pos = 'BEGIN' THEN lv_meth_tabix + 1
        ELSE lv_meth_tabix + 1 ).
      DATA ls_kw_end TYPE ZCL_ACE=>ts_kword.
      IF i_meth_pos = 'END' OR i_meth_pos = 'OVERWRITE'.
        LOOP AT <prog_m>-t_keywords INTO ls_kw_end FROM lv_ins_tabix.
          IF ls_kw_end-name = 'ENDMETHOD'.
            lv_ins_tabix = sy-tabix + 1.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF i_meth_pos = 'OVERWRITE'.
        READ TABLE <prog_m>-tt_enh_blocks TRANSPORTING NO FIELDS
          WITH KEY ev_type = 'METHOD' ev_name = i_method position = 'OVERWRITE' enh_name = i_enhname.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO <prog_m>-tt_enh_blocks ASSIGNING FIELD-SYMBOL(<enh_blk_ow>).
          <enh_blk_ow>-ev_type     = 'METHOD'.
          <enh_blk_ow>-ev_name     = i_method.
          <enh_blk_ow>-position    = 'OVERWRITE'.
          <enh_blk_ow>-enh_name    = i_enhname.
          <enh_blk_ow>-enh_include = CONV #( |{ i_enhinclude }IMP| ).
          <enh_blk_ow>-from_line   = 0.
          <enh_blk_ow>-to_line     = 0.
        ENDIF.
        RETURN.
      ENDIF.

      DATA(lv_insert_line) = COND i(
        WHEN i_meth_pos = 'BEGIN' THEN ls_kw_meth-line + 1
        ELSE ls_kw_end-line ).

      READ TABLE <prog_m>-tt_enh_blocks TRANSPORTING NO FIELDS
        WITH KEY ev_type = 'METHOD' ev_name = i_method position = i_meth_pos enh_name = i_enhname.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO <prog_m>-tt_enh_blocks ASSIGNING FIELD-SYMBOL(<enh_blk>).
        <enh_blk>-ev_type   = 'METHOD'.
        <enh_blk>-ev_name   = i_method.
        <enh_blk>-position  = i_meth_pos.
        <enh_blk>-enh_name    = i_enhname.
        <enh_blk>-enh_include = CONV #( |{ i_enhinclude }IMP| ).
        <enh_blk>-from_line = 0.
        <enh_blk>-to_line   = 0.
      ENDIF.


  endmethod.


  method DETECT_METHOD_CALL.

      DATA: split TYPE string_table.

      IF call-event = 'METHOD' AND call-name IS NOT INITIAL.
        APPEND call TO token-tt_calls.
        CLEAR: call-event, call-type, call-name, call-outer, call-inner.
      ENDIF.

      call-name = word.
      call-event = 'METHOD'.
      REPLACE ALL OCCURRENCES OF '(' IN call-name WITH ''.
      REPLACE ALL OCCURRENCES OF 'ME->' IN call-name WITH ''.

      FIND FIRST OCCURRENCE OF '->' IN call-name.
      IF sy-subrc = 0.
        SPLIT call-name AT '->' INTO TABLE split.
        IF split[ 1 ] <> ')'.
          READ TABLE io_debugger->mo_window->ms_sources-t_vars
            WITH KEY program = i_program name = split[ 1 ]
            INTO DATA(vars).
          IF sy-subrc <> 0.
            call-class = split[ 1 ].
          ELSE.
            call-class = vars-type.
          ENDIF.
        ENDIF.
        call-name = split[ 2 ].
        IF split[ 1 ] = 'SUPER'.
          call-class = class_name.
          call-super = abap_true.
        ENDIF.
      ENDIF.

      FIND FIRST OCCURRENCE OF '=>' IN call-name.
      IF sy-subrc = 0.
        SPLIT call-name AT '=>' INTO TABLE split.
        IF split[ 1 ] <> ')'.
          call-class = split[ 1 ].
        ENDIF.
        call-name = split[ 2 ].
      ENDIF.

      IF call-class IS INITIAL.
        IF i_class IS NOT INITIAL.
          call_line-class = call-class = i_class.
        ENDIF.
        IF class_name IS NOT INITIAL.
          call_line-class = call-class = class_name.
        ENDIF.
      ENDIF.

      call-event = 'METHOD'.

      IF new = abap_true.
        call-class = call-name.
        call-name = 'CONSTRUCTOR'.
        call_line-class = call-class.
        call_line-eventname = call-name.
        call_line-eventtype = 'METHOD'.
        READ TABLE calculated_vars WITH KEY line = l_token_row program = i_include INTO DATA(calc).
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO io_debugger->mo_window->ms_sources-tt_refvar
            ASSIGNING FIELD-SYMBOL(<refvar>).
          <refvar>-name = calc-name.
          <refvar>-class = call-class.
        ENDIF.
      ENDIF.

      READ TABLE io_debugger->mo_window->ms_sources-tt_refvar
        WITH KEY name = call-class INTO DATA(refvar).
      IF sy-subrc = 0.
        call-class = refvar-class.
      ENDIF.


  endmethod.


  method LINK_CALLS_TO_PARAMS.

      FIELD-SYMBOLS: <s_token> TYPE ZCL_ACE=>ts_kword,
                     <call>    TYPE ZCL_ACE=>ts_calls.
      DATA: call  TYPE ZCL_ACE=>ts_calls,
            param TYPE ZCL_ACE_WINDOW=>ts_params,
            index TYPE i.

      LOOP AT ct_tokens ASSIGNING <s_token> WHERE tt_calls IS NOT INITIAL.
        READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
        index = 0.
        LOOP AT io_debugger->mo_window->ms_sources-t_params INTO param
          WHERE event = call-event AND name = call-name.
          ADD 1 TO index.
          READ TABLE <s_token>-tt_calls INDEX index ASSIGNING <call>.
          IF sy-subrc = 0.
            <call>-inner = param-param.
            IF param-type = 'I'.
              <call>-type = '>'.
            ELSE.
              <call>-type = '<'.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

  endmethod.


  method PARSE_CALL.

      DATA: statement TYPE i,
            stack     TYPE i,
            include   TYPE progname,
            prefix    TYPE string,
            program   TYPE program.

      stack = i_stack + 1.
      CHECK  stack <= io_debugger->mo_window->m_hist_depth.

      READ TABLE io_debugger->mt_steps WITH KEY program = i_include eventname = i_e_name eventtype = i_e_type class = i_class TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      READ TABLE io_debugger->mo_window->mt_calls WITH KEY include  = i_include ev_name = i_e_name class = i_class TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        EXIT.
      ELSE.
        APPEND INITIAL LINE TO io_debugger->mo_window->mt_calls ASSIGNING FIELD-SYMBOL(<method_call>).
        <method_call>-include = i_include.
        <method_call>-ev_name = i_e_name.
        <method_call>-class = i_class.
      ENDIF.

      DATA: cl_key        TYPE seoclskey,
            meth_includes TYPE seop_methods_w_include.
      cl_key = i_class.

      READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
        WITH KEY class = i_class
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        statement = i_index.
      ELSE.
        CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
          EXPORTING
            clskey                       = cl_key
          IMPORTING
            includes                     = meth_includes
          EXCEPTIONS
            _internal_class_not_existing = 1
            OTHERS                       = 2.
        IF lines( meth_includes ) IS INITIAL.
          statement = i_index.
        ELSE.
          statement = 1.
        ENDIF.
      ENDIF.

      IF i_include IS NOT INITIAL.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO DATA(prog).
        IF sy-subrc <> 0.
          ZCL_ACE_SOURCE_PARSER=>parse_tokens( i_stack = stack i_program = i_program i_include = i_include io_debugger = io_debugger ).
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO prog.

        ENDIF.


      ELSE.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_program INTO prog.
        IF sy-subrc <> 0.
          ZCL_ACE_SOURCE_PARSER=>parse_tokens( i_stack = stack i_program = i_program i_include = i_program io_debugger = io_debugger ).
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO prog.
        ENDIF.
      ENDIF.
      DATA(max) = lines( prog-scan->statements ).
      DO.
        IF  statement >  max.
          EXIT.
        ENDIF.
        READ TABLE prog-t_keywords WITH KEY index =   statement INTO DATA(key).
        IF sy-subrc <> 0.
          ADD 1 TO  statement.
          CONTINUE.
        ENDIF.
        IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS' OR key-name IS INITIAL.
          ADD 1 TO  statement.
          CONTINUE.
        ENDIF.
        ZCL_ACE_SOURCE_PARSER=>parse_tokens( i_stack = stack i_program = CONV #( key-program ) i_include = CONV #( key-include ) io_debugger = io_debugger ).

        READ TABLE io_debugger->mt_steps WITH KEY line = key-line program = i_program include = key-include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ADD 1 TO io_debugger->m_step.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).

          <step>-step = io_debugger->m_step.
          <step>-line = key-line.
          <step>-eventname = i_e_name.
          <step>-eventtype = i_e_type.
          <step>-stacklevel =  stack.
          <step>-program = i_program.
          <step>-include = key-include.
          <step>-class   = i_class.
        ENDIF.
        LOOP AT key-tt_calls INTO DATA(call).
          IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).
            .
            IF call-event = 'FORM'.
              parse_call_form( i_call_name = call-name
                               i_program   = i_include
                               i_include   = i_include
                               i_stack     = stack
                               io_debugger = io_debugger ).

            ELSEIF call-event = 'FUNCTION'.
              DATA:  func TYPE rs38l_fnam.
              func = call-name.
              REPLACE ALL OCCURRENCES OF '''' IN func WITH ''.
              IF io_debugger->mo_window->m_zcode IS INITIAL OR
                ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND (  func+0(1) = 'Z' OR  func+0(1) = 'Y' ) ) .

                CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                  CHANGING
                    funcname            = func
                    include             = include
                  EXCEPTIONS
                    function_not_exists = 1
                    include_not_exists  = 2
                    group_not_exists    = 3
                    no_selections       = 4
                    no_function_include = 5
                    OTHERS              = 6.

                code_execution_scanner( i_program =  include i_include =  include i_stack =  stack i_evtype = call-event i_evname = call-name io_debugger = io_debugger ).
              ENDIF.

            ELSEIF call-event = 'METHOD'.

              DATA inlude TYPE program.
              IF i_include IS INITIAL.
                include = i_program.
              ELSE.
                include = i_include.
              ENDIF.
              IF call-class = 'ME' OR call-class IS INITIAL.
                call-class = i_class.
              ENDIF.
              parse_class( i_include = include i_call = call i_stack = stack io_debugger = io_debugger key = key ).
            ELSEIF call-event = 'SCREEN'.
              parse_screen( i_stack = stack i_call = call io_debugger = io_debugger key = key ).

            ENDIF.

          ENDIF.
        ENDLOOP.

        IF key-name = 'ENDFORM' OR key-name = 'ENDMETHOD' OR key-name = 'ENDMODULE'.
          RETURN.
        ENDIF.

        ADD 1 TO  statement.
      ENDDO.

  endmethod.


  method PARSE_CALL_FORM.

      DATA call_line TYPE ZCL_ACE=>ts_calls_line.
      READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
        WITH KEY eventname = i_call_name eventtype = 'FORM'
        INTO call_line.
      CHECK sy-subrc = 0.

      DATA(lv_inc) = CONV program( call_line-include ).
      IF lv_inc IS INITIAL. lv_inc = i_include. ENDIF.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = lv_inc TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ZCL_ACE_SOURCE_PARSER=>parse_tokens(
          i_stack = i_stack i_program = lv_inc i_include = lv_inc io_debugger = io_debugger ).
      ENDIF.

      ZCL_ACE_SOURCE_PARSER=>collect_enhancements( i_program = lv_inc io_debugger = io_debugger ).

      READ TABLE io_debugger->mo_window->mt_calls
        WITH KEY include = lv_inc ev_name = i_call_name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RETURN.
      ELSE.
        APPEND INITIAL LINE TO io_debugger->mo_window->mt_calls ASSIGNING FIELD-SYMBOL(<mc>).
        <mc>-include = lv_inc.
        <mc>-ev_name = i_call_name.
      ENDIF.

      DATA(lv_stack) = i_stack + 1.
      CHECK lv_stack <= io_debugger->mo_window->m_hist_depth.

      READ TABLE io_debugger->mt_steps
        WITH KEY program = lv_inc eventname = i_call_name eventtype = 'FORM'
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = lv_inc INTO DATA(prog).
      CHECK sy-subrc = 0.

      DATA(lv_use_vkw) = abap_false.
      IF prog-v_keywords IS NOT INITIAL.
        lv_use_vkw = abap_true.
      ENDIF.

      DATA(lv_tabix) = 0.
      IF lv_use_vkw = abap_true.
        LOOP AT prog-v_keywords INTO DATA(kw).
          IF kw-name = 'FORM' AND kw-index = call_line-index.
            lv_tabix = sy-tabix.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT prog-t_keywords INTO kw.
          IF kw-name = 'FORM' AND kw-index = call_line-index.
            lv_tabix = sy-tabix.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
      CHECK lv_tabix > 0.

      DATA(lv_has_pre) = abap_false.
      READ TABLE prog-tt_enh_blocks TRANSPORTING NO FIELDS
        WITH KEY ev_name = i_call_name position = 'BEGIN'.
      IF sy-subrc = 0. lv_has_pre = abap_true. ENDIF.
      DATA(lv_body_stack) = COND i( WHEN lv_has_pre = abap_true THEN lv_stack + 1 ELSE lv_stack ).

      DATA ls_cur_enh TYPE ZCL_ACE_WINDOW=>ts_enh_block.
      CLEAR ls_cur_enh.

      FIELD-SYMBOLS <kw_tab> TYPE ZCL_ACE_WINDOW=>tt_kword.
      IF lv_use_vkw = abap_true.
        ASSIGN prog-v_keywords TO <kw_tab>.
      ELSE.
        ASSIGN prog-t_keywords TO <kw_tab>.
      ENDIF.

      LOOP AT <kw_tab> INTO kw FROM lv_tabix.
        IF kw-name = 'ENDFORM'.
          EXIT.
        ENDIF.

        IF kw-name = 'ENHANCEMENT'.
          DATA(lv_enh_id_cur) = 0.
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = kw-include INTO DATA(enh_prog).
          IF sy-subrc = 0.
            READ TABLE enh_prog-scan->statements INDEX kw-index INTO DATA(ls_enh_stmt).
            IF sy-subrc = 0.
              READ TABLE enh_prog-scan->tokens INDEX ls_enh_stmt-from + 1 INTO DATA(ls_enh_tok).
              lv_enh_id_cur = CONV i( ls_enh_tok-str ).
            ENDIF.
          ENDIF.
          READ TABLE prog-tt_enh_blocks INTO ls_cur_enh
            WITH KEY enh_include = kw-include ev_name = i_call_name enh_id = lv_enh_id_cur.
          IF sy-subrc <> 0.
            READ TABLE prog-tt_enh_blocks INTO ls_cur_enh
              WITH KEY enh_include = kw-include ev_name = i_call_name.
          ENDIF.
          CONTINUE.
        ENDIF.
        IF kw-name = 'ENDENHANCEMENT'.
          CLEAR ls_cur_enh.
          CONTINUE.
        ENDIF.
        IF kw-name = 'FORM' OR kw-name = 'DATA' OR kw-name = 'TYPES'
          OR kw-name = 'CONSTANTS' OR kw-name IS INITIAL.
          CONTINUE.
        ENDIF.

        ZCL_ACE_SOURCE_PARSER=>parse_tokens(
          i_stack = lv_body_stack i_program = CONV #( kw-program )
          i_include = CONV #( kw-include ) io_debugger = io_debugger ).

        READ TABLE io_debugger->mt_steps
          WITH KEY line = kw-line program = i_program include = kw-include
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ADD 1 TO io_debugger->m_step.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
          <step>-step    = io_debugger->m_step.
          <step>-line    = kw-line.
          <step>-program = i_program.
          <step>-include = kw-include.
          IF ls_cur_enh IS NOT INITIAL.
            <step>-eventtype = 'ENHANCEMENT'.
            <step>-eventname = |{ ls_cur_enh-enh_name } { ls_cur_enh-enh_id }|.
            IF ls_cur_enh-position = 'BEGIN'.
              <step>-stacklevel = lv_stack.
            ELSE.
              <step>-stacklevel = lv_body_stack + 1.
            ENDIF.
          ELSE.
            <step>-eventtype  = 'FORM'.
            <step>-eventname  = i_call_name.
            <step>-stacklevel = lv_body_stack.
          ENDIF.
        ENDIF.

        LOOP AT kw-tt_calls INTO DATA(call).
          IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).
            IF call-event = 'FORM'.
              ZCL_ACE_SOURCE_PARSER=>parse_call_form(
                i_call_name = call-name
                i_program   = lv_inc
                i_include   = lv_inc
                i_stack     = lv_stack
                io_debugger = io_debugger ).
            ELSEIF call-event = 'FUNCTION'.
              DATA func TYPE rs38l_fnam.
              func = call-name.
              REPLACE ALL OCCURRENCES OF '''' IN func WITH ''.
              IF io_debugger->mo_window->m_zcode IS INITIAL OR
                func+0(1) = 'Z' OR func+0(1) = 'Y'.
                DATA lv_finc TYPE progname.
                CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                  CHANGING funcname = func include = lv_finc
                  EXCEPTIONS OTHERS = 6.
                IF sy-subrc = 0.
                  ZCL_ACE_SOURCE_PARSER=>code_execution_scanner(
                    i_program = lv_finc i_include = lv_finc
                    i_stack = lv_stack i_evtype = 'FUNCTION' i_evname = CONV #( func )
                    io_debugger = io_debugger ).
                ENDIF.
              ENDIF.
            ELSEIF call-event = 'METHOD'.
              ZCL_ACE_SOURCE_PARSER=>parse_class(
                i_include = lv_inc i_call = call
                i_stack = lv_stack io_debugger = io_debugger key = kw ).
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

  endmethod.


  METHOD parse_class.


    DATA: cl_key        TYPE seoclskey,
          meth_includes TYPE seop_methods_w_include,
          prefix        TYPE string,
          program       TYPE program,
          include       TYPE progname,
          stack         TYPE i,
          class_call    TYPE zcl_ace=>ts_calls.

    cl_key = i_call-class.
    stack = i_stack.

    DATA(lv_local_exists) = abap_false.
    READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
      WITH KEY class = i_call-class
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_local_exists = abap_true.
    ENDIF.

    IF lv_local_exists = abap_false.
      CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
        EXPORTING
          clskey                       = cl_key
        IMPORTING
          includes                     = meth_includes
        EXCEPTIONS
          _internal_class_not_existing = 1
          OTHERS                       = 2.
    ENDIF.

    IF io_debugger->mo_window->m_zcode IS INITIAL OR
     ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( i_call-class+0(1) = 'Z' OR i_call-class+0(1) = 'Y' ) )
       OR meth_includes IS INITIAL.

      IF lines( meth_includes ) > 0 AND lv_local_exists = abap_false.
        prefix = i_call-class && repeat( val = `=` occ = 30 - strlen( i_call-class ) ).
        include = program = prefix && 'CP'.

        zcl_ace_source_parser=>parse_tokens( i_main = abap_true i_stack = stack i_program = program i_include = include io_debugger = io_debugger i_class = i_call-class ).

      ELSE.
        program = i_include.
      ENDIF.

      IF i_call-super IS INITIAL.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = cl_key eventtype = 'METHOD' eventname = i_call-name INTO DATA(call_line).
      ELSE.
        sy-subrc = 1.
      ENDIF.

      IF sy-subrc <> 0.
        WHILE call_line IS INITIAL.
          LOOP AT io_debugger->mo_window->ms_sources-t_classes INTO DATA(ls_class) WHERE clsname = cl_key .
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = ls_class-refclsname eventtype = 'METHOD' eventname = i_call-name INTO call_line.
            IF sy-subrc = 0.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          cl_key = ls_class-refclsname.
        ENDWHILE.
      ENDIF.

      IF call_line IS INITIAL.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = cl_key eventtype = 'METHOD' eventname = i_call-name INTO call_line.
      ENDIF.
      IF sy-subrc = 0.

        IF call_line-include IS NOT INITIAL.
          include =  call_line-include.
        ENDIF.

        "lets check Class constructor existance
        IF i_call-name = 'CONSTRUCTOR'.
          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = cl_key eventtype = 'METHOD' eventname = 'CLASS_CONSTRUCTOR' INTO DATA(call_super).
          IF sy-subrc = 0.
            zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_super-index
                                               i_e_name = 'CLASS_CONSTRUCTOR'
                                               i_e_type = call_line-eventtype
                                               i_program =  CONV #( include )
                                               i_include =  CONV #( include )
                                               i_class = call_line-class
                                               i_stack   =  i_stack
                                               io_debugger = io_debugger ).
          ENDIF.
        ENDIF.


        zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                 i_e_name = call_line-eventname
                                 i_e_type = call_line-eventtype
                                 i_program =  CONV #( include )
                                 i_include =  CONV #( include )
                                 i_class = call_line-class
                                 i_stack   =  i_stack
                                 io_debugger = io_debugger ).

      ENDIF.
    ENDIF.


  ENDMETHOD.


  method PARSE_SCREEN.


      DATA: stack    TYPE i,
            ftab     TYPE STANDARD TABLE OF d021s,
            scr_code TYPE STANDARD TABLE OF d022s,
            prog     TYPE progname,
            num(4)   TYPE n,
            fmnum    TYPE sychar04,
            code_str TYPE string,
            pbo      TYPE boolean,
            pai      TYPE boolean,
            split    TYPE TABLE OF string.

      stack = i_stack + 1.
      prog = key-program.
      fmnum = num = i_Call-name.

      CALL FUNCTION 'RS_IMPORT_DYNPRO'
        EXPORTING
          dyname = prog
          dynumb = fmnum
        TABLES
          ftab   = ftab
          pltab  = scr_code
        EXCEPTIONS
          OTHERS = 19.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      LOOP AT scr_code ASSIGNING FIELD-SYMBOL(<code>).
        CONDENSE <code>.
        FIND '"' IN <code> MATCH OFFSET DATA(pos).

        IF pos <> 0.
          <code> = <code>+0(pos).
        ENDIF.
      ENDLOOP.

      DELETE scr_code WHERE line+0(1) = '*' OR line+0(1) = '"' OR line IS INITIAL.

      LOOP AT scr_code INTO DATA(code).
        IF code_str IS INITIAL.
          code_str = code-line.
        ELSE.
          code_str = |{ code_str } { code-line }|.
        ENDIF.
      ENDLOOP.

      SPLIT code_str AT '.' INTO TABLE scr_code.

      LOOP AT scr_code INTO code.
        CONDENSE code-line.
        IF code-line = 'PROCESS BEFORE OUTPUT'.
          pbo = abap_true.
          CLEAR pai.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
          ADD 1 TO  io_debugger->m_step.
          <step>-step = io_debugger->m_step.
          <step>-line = key-line.
          <step>-eventname = i_call-name.
          <step>-eventtype = i_call-event.
          <step>-stacklevel =  stack.
          <step>-program = key-program.
          <step>-include = key-include.

          CONTINUE.
        ENDIF.
        IF code-line = 'PROCESS AFTER INPUT'.
          pai = abap_true.
          CLEAR pbo.
          CONTINUE.
        ENDIF.

        CHECK pbo IS NOT INITIAL.
        SPLIT code-line AT | | INTO TABLE split.
        CHECK split[ 1 ] = 'MODULE'.

        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY program = key-program eventtype = 'MODULE' eventname = split[ 2 ] INTO DATA(call_line).
        IF sy-subrc = 0.
          ZCL_ACE_SOURCE_PARSER=>parse_call( EXPORTING i_index = call_line-index
                                i_e_name = call_line-eventname
                                i_e_type = call_line-eventtype
                                i_program =  CONV #( call_line-program )
                                i_include =  CONV #( call_line-include )
                                i_stack   =  stack
                                io_debugger = io_debugger ).
        ENDIF.

      ENDLOOP.

      LOOP AT scr_code INTO code.
        CONDENSE code-line.
        IF code-line = 'PROCESS AFTER INPUT'.
          CLEAR pbo.
          pai = abap_true.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING <step>.
          ADD 1 TO  io_debugger->m_step.
          <step>-step = io_debugger->m_step.
          <step>-line = key-line.
          <step>-eventname = i_call-name.
          <step>-eventtype = i_call-event. <step>-stacklevel =  stack.
          <step>-program = key-program.
          <step>-include = key-include.
          CONTINUE.
        ENDIF.
        IF code-line = 'PROCESS BEFORE OUTPUT'.
          pbo = abap_true.
          CLEAR pai.
          CONTINUE.
        ENDIF.

        CHECK pai IS NOT INITIAL.
        SPLIT code-line AT | | INTO TABLE split.
        CHECK split[ 1 ] = 'MODULE'.

        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY program = key-program eventtype = 'MODULE' eventname = split[ 2 ] INTO call_line.
        IF sy-subrc = 0.
          ZCL_ACE_SOURCE_PARSER=>parse_call( EXPORTING i_index = call_line-index
                                i_e_name = call_line-eventname
                                i_e_type = call_line-eventtype
                                i_program =  CONV #( call_line-program )
                                i_include =  CONV #( call_line-include )
                                i_stack   =  stack
                                io_debugger = io_debugger ).
        ENDIF.

      ENDLOOP.


  endmethod.


  method PARSE_TOKENS.

      DATA: lr_scan     TYPE REF TO cl_ci_scan,
            o_scan      TYPE REF TO cl_ci_scan,
            o_statement TYPE REF TO if_ci_kzn_statement_iterator,
            o_procedure TYPE REF TO if_ci_kzn_statement_iterator,
            tokens      TYPE ZCL_ACE=>tt_kword,
            main_prog   TYPE program,
            stack       TYPE i,
            ls_state    TYPE ZCL_ACE=>ts_parse_state.

      IF i_main = abap_true.
        main_prog = i_program.
      ELSE.
        main_prog = i_include.
      ENDIF.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = main_prog INTO DATA(prog).
      IF sy-subrc <> 0.

        stack = i_stack + 1.
        DATA(o_source) = cl_ci_source_include=>create( p_name = i_include ).

        prog-source_tab = o_source->lines.
        o_scan = NEW cl_ci_scan( p_include = o_source ).

        ZCL_ACE_SOURCE_PARSER=>collect_events(
          io_scan     = o_scan
          i_program   = i_program
          i_include   = i_include
          io_debugger = io_debugger ).

        prog-include = i_include.

        o_statement = cl_cikzn_scan_iterator_factory=>get_statement_iterator( ciscan = o_scan ).
        o_procedure = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = o_scan ).

        IF i_class IS NOT INITIAL.
          ls_state-class = abap_true.
          IF i_main_prog IS INITIAL.
            ls_state-call_line-class = ls_state-param-class = i_class.
          ENDIF.
        ENDIF.

        ls_state-kw = o_statement->get_keyword( ).

        ls_state-word = o_statement->get_token( offset = 2 ).

        o_procedure->statement_index = o_statement->statement_index.
        o_procedure->statement_type = o_statement->statement_type.

        DATA(max) = lines( o_scan->statements ).

        DO.
          CLEAR: ls_state-token-tt_calls, ls_state-call_line-redefined.

          TRY.
              o_procedure->next( ).
            CATCH cx_scan_iterator_reached_end.
          ENDTRY.
          ls_state-kw = o_procedure->get_keyword( ).

          ls_state-token-name = ls_state-kw.
          ls_state-token-index = o_procedure->statement_index.
          READ TABLE o_scan->statements INDEX o_procedure->statement_index INTO DATA(statement).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          READ TABLE o_scan->tokens INDEX statement-from INTO DATA(l_token).
          ls_state-token-line = ls_state-calculated-line = ls_state-composed-line = l_token-row.
          ls_state-token-v_line = l_token-row.
          ls_state-token-program = i_program.

          DATA(lv_token_row) = l_token-row.
          IF ls_state-kw = 'METHODS' OR ls_state-kw = 'CLASS-METHODS'.
            READ TABLE o_scan->tokens INDEX statement-from + 1 INTO DATA(l_name_tok).
            IF sy-subrc = 0 AND l_name_tok-row > 0.
              lv_token_row = l_name_tok-row.
            ENDIF.
          ENDIF.

          READ TABLE o_scan->levels  INDEX statement-level INTO DATA(level).
          IF i_include <> level-name.
            ZCL_ACE_SOURCE_PARSER=>parse_tokens( i_class = i_class i_reltype = i_reltype i_main_prog = i_main_prog i_stack = stack i_program = CONV #( ls_state-token-program ) i_include = CONV #( level-name ) io_debugger = io_debugger ).
            ls_state-token-include = level-name.

          ELSE.
            ls_state-token-include = i_include.

            ls_state-calculated-program = ls_state-composed-program = i_include.

            CLEAR ls_state-new.

            IF ls_state-kw = 'CLASS' OR ls_state-kw = 'INTERFACE'.
              ls_state-class = abap_true.
            ENDIF.

            IF ls_state-kw = 'PUBLIC'.
              ls_state-method_type = 1.
            ENDIF.

            IF ls_state-kw = 'PROTECTED'.
              ls_state-method_type = 2.
            ENDIF.

            IF ls_state-kw = 'PRIVATE'.
              ls_state-method_type = 3.
            ENDIF.

            IF ls_state-kw = 'FORM' OR ls_state-kw = 'METHOD' OR ls_state-kw = 'METHODS' OR ls_state-kw = 'CLASS-METHODS' OR ls_state-kw = 'MODULE'.
              ls_state-variable-eventtype = ls_state-tab-eventtype =  ls_state-eventtype = ls_state-param-event =  ls_state-kw.
              ls_state-param-program = i_program.
              ls_state-param-include = i_include.
              ls_state-param-class = ls_state-class_name.
              CLEAR  ls_state-eventname.
              IF ls_state-kw = 'FORM'.
                CLEAR:  ls_state-class, ls_state-param-class.
              ELSEIF ls_state-kw = 'MODULE'.
                CLEAR:  ls_state-class, ls_state-param-class.
                ls_state-tab-eventtype =  ls_state-eventtype = ls_state-param-event =  'MODULE'.
              ELSE.
                ls_state-tab-eventtype =  ls_state-eventtype = ls_state-param-event =  'METHOD'.
              ENDIF.
            ENDIF.

            IF ls_state-kw = 'ENDCLASS'.
              CLEAR: ls_state-call_line-class, ls_state-param-class, ls_state-class_name.
            ENDIF.

            IF ls_state-kw = 'ENDINTERFACE'.
              ls_state-call_line-class   = ls_state-param-class = ls_state-class_name = ''.
              ls_state-call_line-is_intf = abap_false.
              CLEAR ls_state-class.
            ENDIF.

            IF ls_state-kw = 'ENDFORM' OR ls_state-kw = 'ENDMETHOD' OR ls_state-kw = 'ENDMODULE'.
              CLEAR:  ls_state-eventtype,  ls_state-eventname, ls_state-tabs, ls_state-variable, ls_state-token-sub.
              IF ls_state-param-param IS INITIAL.
                READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = ls_state-param-event name = ls_state-param-name TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  CLEAR ls_state-param-type.
                ENDIF.
              ENDIF.
            ENDIF.

            CLEAR  ls_state-prev.
            IF ls_state-kw = 'ASSIGN' OR ls_state-kw = 'ADD' OR ls_state-kw = 'SUBTRACT' .
              ls_state-count = 0.
            ENDIF.
            CLEAR ls_state-new.

            IF ls_state-eventname IS  NOT INITIAL OR ls_state-class IS NOT INITIAL AND ls_state-eventtype <> 'EVENT' OR ls_state-kw = 'INCLUDE' OR ls_state-kw = 'CLASS-POOL' OR ls_state-kw = 'INTERFACE-POOL'.
              ls_state-token-sub = abap_true.
            ENDIF.

            IF ls_state-kw = 'START-OF-SELECTION' OR ls_state-kw = 'INITIALISATION' OR ls_state-kw = 'END-OF-SELECTION'.
              CLEAR ls_state-token-sub.
            ENDIF.

            ZCL_ACE_SOURCE_PARSER=>process_words(
              EXPORTING
                i_program   = i_program
                i_include   = i_include
                i_class     = i_class
                i_main_prog = i_main_prog
                i_reltype   = i_reltype
                io_debugger = io_debugger
                l_token_row = lv_token_row
                o_procedure = o_procedure
              CHANGING
                cs_state    = ls_state ).
            ls_state-token-from       = statement-from.
            ls_state-token-to         = statement-to.
            ls_state-token-v_from_row = o_scan->tokens[ statement-from ]-row.
            ls_state-token-v_to_row   = o_scan->tokens[ statement-to ]-row.

            IF ls_state-token-name <> 'PUBLIC' AND ls_state-token-name <> 'PRIVATE' AND ls_state-token-name <> 'PROTECTED' AND ls_state-token-name IS NOT INITIAL AND
               ls_state-token-name <> 'INCLUDE' AND ls_state-token-name <> 'CLASS-POOL' AND ls_state-token-name <> 'INTERFACE-POOL'.
              APPEND ls_state-token TO tokens.
            ENDIF.

            IF ls_state-kw = 'ENDCLASS'.
              CLEAR: ls_state-token-sub, ls_state-class.
            ENDIF.
          ENDIF.

          IF o_procedure->statement_index =  max.
            EXIT.
          ENDIF.

        ENDDO.

        ZCL_ACE_SOURCE_PARSER=>link_calls_to_params(
          EXPORTING io_debugger = io_debugger
          CHANGING  ct_tokens   = tokens ).

        LOOP AT io_debugger->mo_window->ms_sources-t_params ASSIGNING FIELD-SYMBOL(<param>).
          REPLACE ALL OCCURRENCES OF 'VALUE(' IN <param>-param WITH ''.
          REPLACE ALL OCCURRENCES OF ')' IN <param>-param WITH ''.
        ENDLOOP.

        APPEND LINES OF ls_state-calculated_vars TO io_debugger->mo_window->ms_sources-t_calculated.
        APPEND LINES OF ls_state-composed_vars TO io_debugger->mo_window->ms_sources-t_composed.

        io_debugger->mo_window->ms_sources-tt_tabs = ls_state-tabs.
        prog-scan = o_scan.
        prog-t_keywords = tokens.
        prog-program = i_program.
        prog-stack = stack.
        prog-v_source   = prog-source_tab.
        prog-v_keywords = tokens.
        APPEND prog TO io_debugger->mo_window->ms_sources-tt_progs.
        ZCL_ACE_SOURCE_PARSER=>COLLECT_ENHANCEMENTS(
          i_program   = i_program
          io_debugger = io_debugger ).

      ENDIF.

      IF i_main IS NOT INITIAL.
        ZCL_ACE_SOURCE_PARSER=>process_super_and_interfaces(
          i_class     = ls_state-class_name
          i_program   = i_program
          i_stack     = stack
          io_debugger = io_debugger ).
      ENDIF.


  endmethod.


  method PROCESS_SUPER_AND_INTERFACES.

      DATA: suffix     TYPE string,
            lt_classes TYPE STANDARD TABLE OF ZCL_ACE_WINDOW=>ts_meta,
            prefix     TYPE string,
            program    TYPE program,
            include    TYPE program.

      SELECT clsname, refCLSNAME, reltype FROM seometarel APPENDING TABLE @lt_classes
        WHERE clsname = @i_class.

      LOOP AT lt_classes INTO DATA(interface).
        prefix = interface-refclsname && repeat( val = `=` occ = 30 - strlen( interface-refclsname ) ).
        CASE interface-reltype.
          WHEN '0' OR '1'.
            suffix = 'IU'.
          WHEN '2'.
            suffix = 'CP'.
          WHEN OTHERS.
            RETURN.
        ENDCASE.
        include = program = prefix && suffix.
        ZCL_ACE_SOURCE_PARSER=>parse_tokens(
          i_main      = abap_true
          i_reltype   = interface-reltype
          i_main_prog = i_program
          i_class     = CONV #( interface-refclsname )
          i_stack     = i_stack
          i_program   = program
          i_include   = include
          io_debugger = io_debugger ).
      ENDLOOP.

      APPEND LINES OF lt_classes TO io_debugger->mo_window->ms_sources-t_classes[].

  endmethod.


  method PROCESS_WORDS.

      DATA: split TYPE TABLE OF string,
            par   TYPE char1,
            type  TYPE char1.

      WHILE 1 = 1.
        IF cs_state-kw IS INITIAL.
          EXIT.
        ENDIF.
        CLEAR  cs_state-change.
        cs_state-word = o_procedure->get_token( offset = sy-index ).

        IF cs_state-word = 'DEFERRED'.
          CLEAR: cs_state-class, cs_state-call_line.
        ENDIF.

        IF cs_state-lv_default = abap_true.
          CLEAR cs_state-lv_default.
          CONTINUE.
        ENDIF.

        IF cs_state-word = 'EXCEPTIONS'.
          EXIT.
        ENDIF.

        IF cs_state-word = 'DEFAULT'.
          cs_state-lv_default = abap_true.
          CONTINUE.
        ENDIF.

        IF cs_state-word = 'REDEFINITION'.
          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
                WITH KEY class = cs_state-call_line-class eventname = cs_state-call_line-eventname eventtype = cs_state-call_line-eventtype ASSIGNING FIELD-SYMBOL(<call_line>).
          <call_line>-redefined = abap_true.
        ENDIF.

        IF ( cs_state-word CS '(' AND ( NOT cs_state-word CS ')' ) AND cs_state-word <> '#(' AND cs_state-word <> '=>' )  OR cs_state-word CS '->'.
          ZCL_ACE_SOURCE_PARSER=>detect_method_call(
            EXPORTING
              word            = cs_state-word
              i_program       = i_program
              i_include       = i_include
              i_class         = i_class
              io_debugger     = io_debugger
              l_token_row     = l_token_row
              new             = cs_state-new
            CHANGING
              call            = cs_state-call
              call_line       = cs_state-call_line
              calculated      = cs_state-calculated
              calculated_vars = cs_state-calculated_vars
              class_name      = cs_state-class_name
              token           = cs_state-token ).
        ENDIF.
        IF cs_state-word = '#('.
          CLEAR cs_state-new.
          READ TABLE io_debugger->mo_window->ms_sources-t_vars WITH KEY name = cs_state-calculated-name INTO DATA(ls_var).
          IF sy-subrc = 0.
            cs_state-call_line-class = cs_state-call-class = ls_var-type.
            cs_state-call_line-eventtype =  cs_state-call-event = 'METHOD'.
            cs_state-call_line-eventname = cs_state-call-name = 'CONSTRUCTOR'.
          ENDIF.
        ENDIF.

        IF sy-index = 1 AND cs_state-token-name = cs_state-word.
          CONTINUE.
        ENDIF.

        IF sy-index = 2 AND ( cs_state-kw = 'DATA' OR cs_state-kw = 'PARAMETERS'
                           OR cs_state-kw = 'CLASS-DATA' OR cs_state-kw = 'SELECT-OPTIONS' ).
          cs_state-tab-name = cs_state-word.
        ENDIF.

        IF sy-index = 2 AND cs_state-kw = 'PERFORM'.
          cs_state-call-name = cs_state-word.
          cs_state-call-event = 'FORM'.
        ENDIF.

        IF sy-index = 2 AND  cs_state-class = abap_true AND cs_state-param-class IS INITIAL.
          cs_state-call_line-class = cs_state-word.
          cs_state-param-class = cs_state-word.
        ENDIF.

        IF sy-index = 2 AND  cs_state-kw = 'CLASS'.
          cs_state-class_name          = cs_state-word.
          cs_state-call_line-is_intf   = abap_false.
        ENDIF.

        IF sy-index = 2 AND cs_state-kw = 'INTERFACE'.
          cs_state-class_name          = cs_state-word.
          cs_state-call_line-class     = cs_state-word.
          cs_state-call_line-is_intf   = abap_true.
          cs_state-param-class         = cs_state-word.
        ENDIF.

        IF cs_state-kw = 'CLASS' AND cs_state-word = 'DEFINITION' AND cs_state-class_name IS NOT INITIAL.
          READ TABLE io_debugger->mo_window->ms_sources-tt_class_defs
            WITH KEY class = cs_state-class_name TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND VALUE #( class = cs_state-class_name include = i_include line = l_token_row )
              TO io_debugger->mo_window->ms_sources-tt_class_defs.
          ENDIF.
        ENDIF.

        IF  cs_state-class_name IS INITIAL.
          SPLIT i_program AT '=' INTO TABLE split.
          IF lines( split ) > 1.
            cs_state-class_name = split[ 1 ].
            SELECT SINGLE clsname INTO cs_state-call_line-class
              FROM seoclass
             WHERE clsname = cs_state-class_name.
          ENDIF.
        ENDIF.

        IF sy-index = 2 AND  cs_state-eventtype IS NOT INITIAL AND  cs_state-eventname IS INITIAL.
          IF i_main_prog IS NOT INITIAL AND i_reltype = '1'.
          ENDIF.
          cs_state-variable-eventname = cs_state-tab-eventname = cs_state-eventname = cs_state-param-name = cs_state-word.
          cs_state-param-line = l_token_row.

          DATA(lv_is_intf) = cs_state-call_line-is_intf.
          MOVE-CORRESPONDING cs_state-tab TO cs_state-call_line.
          cs_state-call_line-is_intf = lv_is_intf.
          cs_state-call_line-index = o_procedure->statement_index.
          cs_state-call_line-class = cs_state-class_name.

          IF  cs_state-call_line-class IS NOT INITIAL.
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
             WITH KEY class = cs_state-call_line-class eventname = cs_state-call_line-eventname eventtype = cs_state-call_line-eventtype ASSIGNING <call_line>.
          ELSE.
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
             WITH KEY program = i_program eventname = cs_state-call_line-eventname eventtype = cs_state-call_line-eventtype ASSIGNING <call_line>.
          ENDIF.

          IF sy-subrc = 0.
            IF <call_line>-def_include IS INITIAL.
              <call_line>-def_include = <call_line>-include.
              <call_line>-def_line    = <call_line>-index.
            ENDIF.
            <call_line>-index   = cs_state-call_line-index.
            <call_line>-include = cs_state-token-include.
          ELSE.
            cs_state-call_line-program     = i_program.
            cs_state-call_line-include     = cs_state-token-include.
            cs_state-call_line-meth_type   = cs_state-method_type.
            cs_state-call_line-def_include = cs_state-token-include.
            cs_state-call_line-def_line    = l_token_row.
            APPEND cs_state-call_line TO io_debugger->mo_window->ms_sources-tt_calls_line.
          ENDIF.

        ENDIF.

        IF cs_state-word = ''.
          IF cs_state-call IS NOT INITIAL.
            APPEND cs_state-call TO cs_state-token-tt_calls.
          ENDIF.
          CLEAR cs_state-call.
          CASE cs_state-kw.
            WHEN 'COMPUTE'.
              IF  NOT  cs_state-prev CO '0123456789.+-/* '.
                cs_state-composed-name =  cs_state-prev.
                APPEND  cs_state-composed TO cs_state-composed_vars.
              ENDIF.
            WHEN 'CLEAR' OR 'SORT' OR 'CONDENSE'.
            WHEN 'FORM'.
              IF cs_state-param-name IS NOT INITIAL.
                APPEND cs_state-param TO io_debugger->mo_window->ms_sources-t_params.
                CLEAR cs_state-param.
              ENDIF.
          ENDCASE.
          EXIT.
        ENDIF.

        IF cs_state-word = 'REF'.
          cs_state-ref = abap_true.
        ENDIF.

        IF cs_state-word = 'USING' OR cs_state-word = 'IMPORTING'.
          cs_state-param-type = 'I'.
          CLEAR:  type,  par.
        ELSEIF cs_state-word = 'CHANGING' OR cs_state-word = 'EXPORTING' OR cs_state-word = 'RETURNING'.
          IF cs_state-param-param IS NOT INITIAL.
            APPEND cs_state-param TO io_debugger->mo_window->ms_sources-t_params.
            CLEAR:  type,  par, cs_state-param-param.
          ENDIF.
          cs_state-param-type = 'E'.
          CLEAR:  type,  par.
        ELSEIF cs_state-word = 'OPTIONAL' OR cs_state-word = 'PREFERRED' OR cs_state-word = 'REF' OR cs_state-word = 'TO'.
          CONTINUE.
        ELSEIF cs_state-word = 'PARAMETER'.
          cs_state-preferred = abap_true.
          CONTINUE.
        ENDIF.

        IF  cs_state-preferred = abap_true.
          READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = 'METHOD' name = cs_state-param-name param = cs_state-word ASSIGNING FIELD-SYMBOL(<param>).
          IF sy-subrc = 0.
            <param>-preferred = abap_true.
          ENDIF.
          CLEAR  cs_state-preferred.
          CONTINUE.
        ENDIF.

        IF cs_state-word <> 'CHANGING' AND cs_state-word <> 'EXPORTING' AND cs_state-word <> 'RETURNING' AND cs_state-word <> 'IMPORTING' AND cs_state-word <> 'USING'.
          IF cs_state-kw = 'FORM' OR cs_state-kw = 'METHODS' OR cs_state-kw = 'CLASS-METHODS'.
            IF  par = abap_true AND  type IS INITIAL AND cs_state-word NE 'TYPE'.
              APPEND cs_state-param TO io_debugger->mo_window->ms_sources-t_params.
              CLEAR:  par, cs_state-param-param.
            ENDIF.
            IF  par IS INITIAL AND sy-index > 3.
              cs_state-param-param = cs_state-word.
              par = abap_true.
              CONTINUE.
            ENDIF.
            IF  par = abap_true AND  type IS INITIAL AND cs_state-word = 'TYPE'.
              type = abap_true.
              CONTINUE.
            ENDIF.
            IF  par = abap_true AND  type = abap_true.
              REPLACE ALL OCCURRENCES OF 'VALUE(' IN cs_state-param-param WITH ''.
              REPLACE ALL OCCURRENCES OF ')' IN cs_state-param-param WITH ''.
              APPEND cs_state-param TO io_debugger->mo_window->ms_sources-t_params.
              CLEAR:  type,  par, cs_state-param-param.
            ENDIF.
          ENDIF.
        ENDIF.

        DATA  temp TYPE char30.
        temp = cs_state-word.

        IF  temp+0(5) = 'DATA('.
          SHIFT  temp LEFT BY 5 PLACES.
          REPLACE ALL OCCURRENCES OF ')' IN  temp WITH ''.
        ENDIF.

        IF  temp+0(6) = '@DATA('.
          SHIFT  temp LEFT BY 6 PLACES.
          REPLACE ALL OCCURRENCES OF ')' IN  temp WITH ''.
        ENDIF.

        IF  temp+0(13) = 'FIELD-SYMBOL('.
          SHIFT  temp LEFT BY 13 PLACES.
          REPLACE ALL OCCURRENCES OF ')' IN  temp WITH ''.
        ENDIF.

        IF cs_state-word = 'NEW'.
          cs_state-new = abap_true.
        ENDIF.

        FIND FIRST OCCURRENCE OF '->' IN cs_state-word.
        IF sy-subrc = 0.
          CLEAR  cs_state-new.
        ENDIF.

        DATA(lv_dispatch_cont) = ZCL_ACE_SOURCE_PARSER=>word_dependencies_analysis(
          EXPORTING
            kw          = cs_state-kw
            temp        = temp
            i_program   = i_program
            i_include   = i_include
            io_debugger = io_debugger
            l_token_row = l_token_row
          CHANGING
            cs_state    = cs_state ).
        IF lv_dispatch_cont = abap_true.
          CONTINUE.
        ENDIF.

        IF  temp = '(' .
          cs_state-prev =  temp.
          CONTINUE.
        ENDIF.

        IF  NOT  temp  CA '()'.
          IF  temp <> 'TABLE' AND  temp <> 'NEW'  AND  cs_state-prev <> '('.
            IF  cs_state-kw <> 'PERFORM'.
              cs_state-prev =  temp.
            ELSEIF cs_state-word = 'USING' OR cs_state-word = 'CHANGING'.
              cs_state-prev =  temp.
            ENDIF.
          ENDIF.
        ENDIF.

        IF  cs_state-change IS NOT INITIAL.
          cs_state-calculated-name =  cs_state-change.
          APPEND cs_state-calculated TO cs_state-calculated_vars.

          IF  cs_state-change+0(1) = '<'.
            ZCL_ACE_SOURCE_PARSER=>register_field_symbol(
              EXPORTING
                i_include   = i_include
                io_debugger = io_debugger
              CHANGING
                cs_state    = cs_state ).
          ENDIF.
        ENDIF.

      ENDWHILE.

  endmethod.


  method REGISTER_FIELD_SYMBOL.

      DATA: split TYPE TABLE OF string.
      SPLIT  cs_state-change AT '-' INTO TABLE split.
      cs_state-change = split[ 1 ].
      IF  cs_state-eventtype IS INITIAL.
        READ TABLE io_debugger->mo_window->mt_globals_set WITH KEY program = i_include ASSIGNING FIELD-SYMBOL(<globals_set>).
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO io_debugger->mo_window->mt_globals_set ASSIGNING <globals_set>.
          <globals_set>-program = i_include.
        ENDIF.
        READ TABLE <globals_set>-mt_fs WITH KEY name = cs_state-change TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO <globals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<gl_fs>).
          <gl_fs>-name = cs_state-change.
        ENDIF.
      ELSE.
        READ TABLE io_debugger->mo_window->mt_locals_set
          WITH KEY program = i_include eventtype = cs_state-eventtype eventname = cs_state-eventname
          ASSIGNING FIELD-SYMBOL(<locals_set>).
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO io_debugger->mo_window->mt_locals_set ASSIGNING <locals_set>.
          <locals_set>-program    = i_include.
          <locals_set>-eventname  = cs_state-eventname.
          <locals_set>-eventtype  = cs_state-eventtype.
        ENDIF.
        READ TABLE <locals_set>-mt_fs WITH KEY name = cs_state-change TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
          <loc_fs>-name = cs_state-change.
        ENDIF.
      ENDIF.

  endmethod.


  method WORD_DEPENDENCIES_ANALYSIS.

      DATA: lv_import TYPE boolean,
            lv_export TYPE boolean.

      CASE cs_state-kw.

        WHEN 'DATA' OR 'PARAMETERS' OR 'CLASS-DATA' OR 'SELECT-OPTIONS'.
          IF cs_state-kw = 'PARAMETERS' AND cs_state-prev = 'CHECKBOX'.
            cs_state-variable-name = cs_state-tab-name.
            cs_state-variable-type = 'CHECKBOX'.
            cs_state-variable-line = l_token_row.
            cs_state-variable-icon = icon_checked.
            cs_state-variable-program = i_program.
            cs_state-variable-include = i_include.
            cs_state-variable-class = cs_state-class_name.
            APPEND cs_state-variable TO io_debugger->mo_window->ms_sources-t_vars.
          ENDIF.
          IF cs_state-kw = 'SELECT-OPTIONS' AND cs_state-prev = 'FOR'.
            cs_state-variable-name = cs_state-tab-name.
            cs_state-variable-type = temp.
            cs_state-variable-line = l_token_row.
            cs_state-variable-icon = icon_select_all.
            cs_state-variable-program = i_program.
            cs_state-variable-include = i_include.
            cs_state-variable-class = cs_state-class_name.
            APPEND cs_state-variable TO io_debugger->mo_window->ms_sources-t_vars.
          ENDIF.
          IF ( cs_state-prev = 'OF' ) AND temp <> 'TABLE' AND temp <> 'OF'.
            cs_state-tab-type = temp.
            APPEND cs_state-tab TO cs_state-tabs.
            cs_state-variable-name = cs_state-tab-name.
            cs_state-variable-type = cs_state-tab-type.
            cs_state-variable-line = l_token_row.
            cs_state-variable-icon = icon_table_settings.
            APPEND cs_state-variable TO io_debugger->mo_window->ms_sources-t_vars.
          ENDIF.
          IF ( cs_state-prev = 'TYPE' ) AND temp <> 'TABLE' AND temp <> 'OF'.
            cs_state-variable-name = cs_state-tab-name.
            cs_state-variable-type = temp.
            cs_state-variable-line = l_token_row.
            CASE cs_state-variable-type.
              WHEN 'D'.
                cs_state-variable-icon = icon_date.
              WHEN 'T'.
                cs_state-variable-icon = icon_bw_time_sap.
              WHEN 'C'.
                cs_state-variable-icon = icon_wd_input_field.
              WHEN 'P'.
                cs_state-variable-icon = icon_increase_decimal.
              WHEN 'STRING'.
                cs_state-variable-icon = icon_text_act.
              WHEN 'N' OR 'I'.
                cs_state-variable-icon = icon_pm_order.
              WHEN OTHERS.
                cs_state-variable-icon = icon_element.
            ENDCASE.
            IF cs_state-ref IS NOT INITIAL.
              cs_state-variable-icon = icon_oo_class.
              CLEAR cs_state-ref.
            ENDIF.
            cs_state-variable-program = i_program.
            cs_state-variable-include = i_include.
            cs_state-variable-class = cs_state-class_name.
            APPEND cs_state-variable TO io_debugger->mo_window->ms_sources-t_vars.
          ENDIF.

        WHEN 'COMPUTE'.
          IF temp CA '=' AND NOT cs_state-word IS INITIAL.
            cs_state-change = cs_state-prev.
          ENDIF.
          IF ( cs_state-prev = '=' OR cs_state-prev CA '+-/*' ) AND temp <> 'NEW'.
            IF NOT temp CA '()'.
              IF NOT temp CO '0123456789. '.
                cs_state-composed-name = temp.
                APPEND cs_state-composed TO cs_state-composed_vars.
                IF cs_state-call IS NOT INITIAL.
                  cs_state-call-outer = temp.
                  READ TABLE cs_state-token-tt_calls WITH KEY event = cs_state-call-event name = cs_state-call-name outer = cs_state-call-outer TRANSPORTING NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND cs_state-call TO cs_state-token-tt_calls.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        WHEN 'PERFORM'.
          IF temp = 'USING' OR temp = 'CHANGING'.
            CLEAR cs_state-prev.
          ENDIF.
          IF cs_state-prev = 'USING' OR cs_state-prev = 'CHANGING'.
            IF NOT temp CA '()'.
              IF NOT temp CO '0123456789. '.
                cs_state-call-outer = temp.
                READ TABLE cs_state-token-tt_calls WITH KEY event = cs_state-call-event name = cs_state-call-name outer = cs_state-call-outer TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND cs_state-call TO cs_state-token-tt_calls.
                ENDIF.
                cs_state-change = temp.
              ENDIF.
            ENDIF.
          ENDIF.

        WHEN 'CREATE' OR 'CALL'.
          IF cs_state-prev = 'FUNCTION' AND kw = 'CALL'.
            cs_state-call_line-eventtype = cs_state-call-event = 'FUNCTION'.
            REPLACE ALL OCCURRENCES OF '''' IN cs_state-word WITH ''.
            cs_state-call_line-eventname = cs_state-call-name = cs_state-word.
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
              WITH KEY eventtype = cs_state-call_line-eventtype eventname = cs_state-call_line-eventname
              TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              CLEAR cs_state-call_line-class.
              APPEND cs_state-call_line TO io_debugger->mo_window->ms_sources-tt_calls_line.
            ENDIF.
          ENDIF.
          IF cs_state-prev = 'SCREEN' AND kw = 'CALL'.
            APPEND INITIAL LINE TO cs_state-token-tt_calls ASSIGNING FIELD-SYMBOL(<call>).
            <call>-event = 'SCREEN'.
            <call>-name = temp.
            cs_state-token-program = i_program.
          ENDIF.
          IF cs_state-word = 'EXPORTING' OR cs_state-word = 'CHANGING' OR cs_state-word = 'TABLES'.
            lv_export = abap_true. CLEAR lv_import. rv_continue = abap_true. RETURN.
          ELSEIF cs_state-word = 'IMPORTING'.
            lv_import = abap_true. CLEAR lv_export. rv_continue = abap_true. RETURN.
          ENDIF.
          IF cs_state-prev = 'OBJECT'.
            READ TABLE io_debugger->mo_window->ms_sources-t_vars
              WITH KEY program = i_program icon = icon_oo_class name = cs_state-word
              INTO DATA(var).
            IF sy-subrc = 0.
              cs_state-call-class = var-type.
              cs_state-call-event = 'METHOD'.
              cs_state-call-name = 'CONSTRUCTOR'.
            ENDIF.
          ENDIF.
          IF cs_state-prev = '='.
            IF NOT temp CA '()'.
              IF NOT temp CO '0123456789. '.
                IF lv_import = abap_true.
                  cs_state-call-outer = temp.
                  READ TABLE cs_state-token-tt_calls WITH KEY event = cs_state-call-event name = cs_state-call-name outer = cs_state-call-outer TRANSPORTING NO FIELDS.
                  IF sy-subrc <> 0. APPEND cs_state-call TO cs_state-token-tt_calls. ENDIF.
                  cs_state-calculated-name = temp.
                  APPEND cs_state-calculated TO cs_state-calculated_vars.
                ELSEIF lv_export = abap_true.
                  cs_state-call-outer = temp.
                  READ TABLE cs_state-token-tt_calls WITH KEY event = cs_state-call-event name = cs_state-call-name outer = cs_state-call-outer TRANSPORTING NO FIELDS.
                  IF sy-subrc <> 0. APPEND cs_state-call TO cs_state-token-tt_calls. ENDIF.
                  cs_state-composed-name = temp.
                  APPEND cs_state-composed TO cs_state-composed_vars.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            IF NOT temp CO '0123456789. ' AND temp <> '=' AND ( lv_import = abap_true OR lv_export = abap_true ).
              cs_state-call-inner = temp.
            ENDIF.
          ENDIF.

        WHEN 'CLEAR' OR 'SORT'.
          cs_state-change = temp.

        WHEN 'CONDENSE'.
          IF temp <> 'NO-GAPS'.
            cs_state-change = temp.
          ENDIF.

        WHEN 'ASSIGN' OR 'UNASSIGN'.
          ADD 1 TO cs_state-count.
          IF cs_state-count <> 2.
            cs_state-change = temp.
          ENDIF.

        WHEN 'ADD' OR 'SUBTRACT'.
          ADD 1 TO cs_state-count.
          IF cs_state-count = 1.
            IF NOT temp CO '0123456789.() '.
              cs_state-composed-name = temp.
              APPEND cs_state-composed TO cs_state-composed_vars.
            ENDIF.
          ENDIF.
          IF cs_state-count = 3.
            cs_state-change = temp.
          ENDIF.

        WHEN 'READ'.
          IF cs_state-prev = 'INTO' OR cs_state-prev = 'ASSIGNING'.
            cs_state-change = temp.
          ENDIF.

        WHEN 'SELECT'.
          IF ( cs_state-prev = 'INTO' OR cs_state-prev = '(' )
            AND temp <> 'TABLE' AND temp <> '(' AND temp <> ')' AND temp <> ','.
            cs_state-change = temp.
          ENDIF.

      ENDCASE.

  endmethod.
ENDCLASS.
