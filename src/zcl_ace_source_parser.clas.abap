class ZCL_ACE_SOURCE_PARSER definition
  public
  create public .

public section.

  class-methods PARSE_CALL
    importing
      !I_PROGRAM   type PROGRAM
      !I_INCLUDE   type PROGRAM
      !I_INDEX     type I
      !I_STACK     type I
      !I_E_NAME    type STRING
      !I_E_TYPE    type STRING
      !I_CLASS     type STRING optional
      !I_STMT_IDX  type I optional
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PARSE_CALL_FORM
    importing
      !I_CALL_NAME type STRING
      !I_PROGRAM   type PROGRAM
      !I_INCLUDE   type PROGRAM
      !I_STACK     type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PARSE_CLASS
    importing
      !KEY         type ZCL_ACE=>TS_KWORD
      !I_INCLUDE   type PROGRAM
      !I_CALL      type ZCL_ACE=>TS_CALLS
      !I_STACK     type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods PARSE_SCREEN
    importing
      !KEY         type ZCL_ACE=>TS_KWORD
      !I_STACK     type I
      !I_CALL      type ZCL_ACE=>TS_CALLS
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods RESOLVE_CONTEXT
    importing
      !I_INCLUDE   type PROGRAM
      !I_EVTYPE    type STRING optional
      !I_EVNAME    type STRING optional
      !IO_DEBUGGER type ref to ZCL_ACE
    exporting
      !E_EVTYPE    type STRING
      !E_EVNAME    type STRING
      !E_CLASS     type STRING .
  class-methods CODE_EXECUTION_SCANNER
    importing
      !I_PROGRAM   type PROGRAM
      !I_INCLUDE   type PROGRAM
      !I_EVNAME    type STRING optional
      !I_EVTYPE    type STRING optional
      !I_CLASS     type STRING optional
      !I_STACK     type I optional
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods LINK_CALLS_TO_PARAMS
    importing
      !IO_DEBUGGER type ref to ZCL_ACE
    changing
      !CT_TOKENS   type ZCL_ACE=>TT_KWORD .
  class-methods PROCESS_SUPER_AND_INTERFACES
    importing
      !I_CLASS     type STRING
      !I_PROGRAM   type PROGRAM
      !I_STACK     type I
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods DETECT_METHOD_CALL
    importing
      !WORD            type STRING
      !I_PROGRAM       type PROGRAM
      !I_INCLUDE       type PROGRAM
      !I_CLASS         type STRING
      !IO_DEBUGGER     type ref to ZCL_ACE
      !L_TOKEN_ROW     type I
      !NEW             type BOOLEAN
    changing
      !CALL            type ZCL_ACE=>TS_CALLS
      !CALL_LINE       type ZCL_ACE=>TS_CALLS_LINE
      !CALCULATED      type ZCL_ACE=>TS_VAR
      !CALCULATED_VARS type ZCL_ACE=>TT_CALCULATED
      !CLASS_NAME      type STRING
      !TOKEN           type ZCL_ACE=>TS_KWORD .
  class-methods REGISTER_FIELD_SYMBOL
    importing
      !I_INCLUDE   type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE
    changing
      !CS_STATE    type ZCL_ACE=>TS_PARSE_STATE .
  class-methods COLLECT_EVENTS
    importing
      !IO_SCAN     type ref to CL_CI_SCAN
      !I_PROGRAM   type PROGRAM
      !I_INCLUDE   type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods COLLECT_ENHANCEMENTS
    importing
      !I_PROGRAM   type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods COLLECT_METHOD_ENHANCEMENTS
    importing
      !I_ENHNAME    type ENHNAME
      !I_ENHINCLUDE type PROGRAM
      !I_METHOD     type STRING
      !I_CLASS      type STRING
      !I_METH_POS   type STRING
      !I_ID         type I
      !IO_DEBUGGER  type ref to ZCL_ACE .
protected section.
private section.
ENDCLASS.


CLASS ZCL_ACE_SOURCE_PARSER IMPLEMENTATION.


  METHOD resolve_context.
    e_evtype = i_evtype.
    e_evname = i_evname.
    IF i_evtype IS NOT INITIAL AND i_evname IS NOT INITIAL.
      READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
        WITH KEY include = i_include eventtype = i_evtype eventname = i_evname
        INTO DATA(ls_cl).
      IF sy-subrc = 0. e_class = ls_cl-class. RETURN. ENDIF.
      FIND '=' IN i_include.
      IF sy-subrc = 0.
        DATA(lv_splits) = VALUE string_table( ).
        SPLIT i_include AT '=' INTO TABLE lv_splits.
        e_class = lv_splits[ 1 ].
      ENDIF.
      RETURN.
    ENDIF.
    FIND '=' IN i_include.
    IF sy-subrc = 0.
      DATA(lv_sp) = VALUE string_table( ).
      SPLIT i_include AT '=' INTO TABLE lv_sp.
      e_class = lv_sp[ 1 ].
      READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
        WITH KEY include = i_include eventtype = 'METHOD'
        INTO ls_cl.
      IF sy-subrc = 0. e_evtype = 'METHOD'. e_evname = ls_cl-eventname. ENDIF.
      RETURN.
    ENDIF.
    READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
      WITH KEY include = i_include INTO ls_cl.
    IF sy-subrc = 0.
      e_class  = ls_cl-class.
      e_evtype = ls_cl-eventtype.
      e_evname = ls_cl-eventname.
    ENDIF.
  ENDMETHOD.


  METHOD code_execution_scanner.

    DATA: max       TYPE i,
          call_line TYPE zcl_ace=>ts_calls_line,
          program   TYPE program,
          include   TYPE program,
          prefix    TYPE string,
          event     TYPE string,
          stack     TYPE i,
          statement TYPE i,
          prog      TYPE zcl_ace_window=>ts_prog.

    SORT io_debugger->mo_window->ms_sources-tt_calls_line.
    CLEAR: io_debugger->mt_steps, io_debugger->m_step.
    stack = i_stack + 1.
    CHECK stack <= io_debugger->mo_window->m_hist_depth.

    zcl_ace_parser=>parse(
      EXPORTING i_program = i_program i_include = i_include i_run = 1
      CHANGING  cs_source = io_debugger->mo_window->ms_sources ).

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs
      WITH KEY include = i_include ASSIGNING FIELD-SYMBOL(<prog>).
    IF sy-subrc <> 0. RETURN. ENDIF.

    DATA: structures LIKE <prog>-scan->structures.

    LOOP AT <prog>-scan->structures INTO DATA(structure)
      WHERE type = 'E' AND ( stmnt_type = '1' OR stmnt_type = '2' OR stmnt_type = '3' ).
    ENDLOOP.

    IF sy-subrc = 0.
      structures = <prog>-scan->structures.
      DELETE structures WHERE type <> 'E'.
      LOOP AT structures ASSIGNING FIELD-SYMBOL(<structure>) WHERE stmnt_type = 'g'.
        CLEAR <structure>-stmnt_type.
      ENDLOOP.
      SORT structures BY stmnt_type ASCENDING.
    ELSE.
      CLEAR max.
      LOOP AT <prog>-scan->structures INTO DATA(str) WHERE type <> 'C' AND type <> 'R'.
        IF max < str-stmnt_to.
          max = str-stmnt_to.
          APPEND str TO structures.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT structures INTO str.

      IF str-type = 'E'.
        READ TABLE io_debugger->mo_window->ms_sources-t_events
          WITH KEY program = i_program stmnt_type = str-stmnt_type stmnt_from = str-stmnt_from
          ASSIGNING FIELD-SYMBOL(<event>).
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs
          WITH KEY include = <event>-include INTO prog.
        READ TABLE prog-scan->statements INDEX <event>-stmnt_from INTO DATA(command).
        READ TABLE prog-scan->levels INDEX command-level INTO DATA(level).
        CLEAR event.
        LOOP AT prog-scan->tokens FROM command-from TO command-to INTO DATA(word).
          IF event IS INITIAL. event = word-str. ELSE. event = |{ event } { word-str }|. ENDIF.
        ENDLOOP.
        <event>-name = event. <event>-line = word-row.
        statement = <event>-stmnt_from + 1.
      ELSE.
        statement = str-stmnt_from.
        prog = <prog>.
      ENDIF.

      READ TABLE prog-t_keywords WITH KEY index = str-stmnt_from INTO DATA(key).
      IF key IS NOT INITIAL.
        zcl_ace_parser=>parse(
          EXPORTING i_program = CONV #( key-program ) i_include = CONV #( key-include ) i_run = 1
          CHANGING  cs_source = io_debugger->mo_window->ms_sources ).
      ENDIF.

      WHILE statement <= str-stmnt_to.
        READ TABLE prog-t_keywords WITH KEY index = statement INTO key.

        IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS'
        OR key-name = 'PARAMETERS' OR key-name = 'INCLUDE' OR key-name = 'REPORT'
        OR key-name = 'PUBLIC' OR key-name = 'PROTECTED' OR key-name = 'PRIVATE'
        OR key-name IS INITIAL OR sy-subrc <> 0 OR key-sub IS NOT INITIAL.
          ADD 1 TO statement. CONTINUE.
        ENDIF.
        ADD 1 TO io_debugger->m_step.

        READ TABLE io_debugger->mt_steps
          WITH KEY line = key-line program = i_program include = key-include
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
          <step>-step = io_debugger->m_step. <step>-line = key-line.
          IF i_evtype IS INITIAL. <step>-eventtype = 'EVENT'. <step>-eventname = event.
          ELSE. <step>-eventtype = i_evtype. <step>-eventname = i_evname. ENDIF.
          <step>-stacklevel = stack. <step>-program = i_program. <step>-include = key-include.
          IF <step>-eventtype = 'METHOD'. <step>-class = i_class. ENDIF.
        ENDIF.

        " Точечный парсинг calls/calcs/vars через parse_tokens( i_stmt_idx )
        IF key-calls_parsed = abap_false.
          zcl_ace_parser=>parse_tokens(
            EXPORTING
              i_program  = CONV #( key-program )
              i_include  = CONV #( key-include )
              i_stmt_idx = key-index
              i_class    = i_class
              i_evtype   = i_evtype
              i_ev_name  = i_evname
            CHANGING
              cs_source  = io_debugger->mo_window->ms_sources ).
          " Перечитываем key — calls_parsed = true, tt_calls заполнен
          READ TABLE prog-t_keywords WITH KEY index = statement INTO key.
        ENDIF.

        LOOP AT key-tt_calls INTO DATA(call).
          IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).

            IF call-event = 'FORM'.
              parse_call_form(
                i_call_name = call-name i_program = CONV #( call_line-program )
                i_include   = CONV #( call_line-include ) i_stack = stack io_debugger = io_debugger ).

            ELSEIF call-event = 'FUNCTION'.
              DATA func TYPE rs38l_fnam.
              func = call-name.
              IF io_debugger->mo_window->m_zcode IS INITIAL OR
               ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( func+0(1) = 'Z' OR func+0(1) = 'Y' ) ).
                CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                  CHANGING funcname = func include = include
                  EXCEPTIONS function_not_exists = 1 include_not_exists = 2
                             group_not_exists = 3 no_selections = 4 no_function_include = 5 OTHERS = 6.
                code_execution_scanner( i_program = include i_include = include i_stack = stack
                  i_evtype = call-event i_evname = call-name io_debugger = io_debugger ).
              ENDIF.

            ELSEIF call-event = 'METHOD'.
              parse_class( i_include = i_include i_call = call i_stack = stack
                           io_debugger = io_debugger key = key ).

            ELSEIF call-event = 'SCREEN'.
              parse_screen( i_stack = stack i_call = call io_debugger = io_debugger key = key ).
            ENDIF.
          ENDIF.
        ENDLOOP.

        ADD 1 TO statement.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


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
          DATA(lv_meth_pos) = SWITCH string( position WHEN '%_BEGIN' THEN 'BEGIN' WHEN '%_END' THEN 'END' ELSE `` ).
          CHECK lv_meth_pos IS NOT INITIAL.
          DATA(lv_has_overwrite) = abap_false.
          IF lv_meth_pos = 'BEGIN'.
            collect_method_enhancements(
              EXPORTING i_enhname = CONV #( ls_enh-enhname ) i_enhinclude = CONV #( ls_enh-enhinclude )
                        i_method = lv_method_name i_class = lv_class_name i_meth_pos = 'OVERWRITE'
                        i_id = CONV #( ls_enh-id ) io_debugger = io_debugger ).
            READ TABLE io_debugger->mo_window->ms_sources-tt_progs TRANSPORTING NO FIELDS
              WITH KEY program = lv_class_name.
            LOOP AT io_debugger->mo_window->ms_sources-tt_progs INTO DATA(ls_prog_ow).
              READ TABLE ls_prog_ow-tt_enh_blocks TRANSPORTING NO FIELDS
                WITH KEY ev_name = lv_method_name position = 'OVERWRITE'.
              IF sy-subrc = 0. lv_has_overwrite = abap_true. EXIT. ENDIF.
            ENDLOOP.
          ENDIF.
          IF lv_has_overwrite = abap_false.
            collect_method_enhancements(
              EXPORTING i_enhname = CONV #( ls_enh-enhname ) i_enhinclude = CONV #( ls_enh-enhinclude )
                        i_method = lv_method_name i_class = lv_class_name i_meth_pos = lv_meth_pos
                        i_id = CONV #( ls_enh-id ) io_debugger = io_debugger ).
          ENDIF.

        ELSE.
          FIND FIRST OCCURRENCE OF REGEX '\\FO:([^\\]+)' IN lv_full SUBMATCHES form_name.
          FIND FIRST OCCURRENCE OF REGEX '\\SE:([^\\]+)' IN lv_full SUBMATCHES position.
          CHECK form_name IS NOT INITIAL AND position IS NOT INITIAL.
          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = ls_enh-enhinclude TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            ZCL_ACE_PARSER=>parse( EXPORTING i_program = CONV #( ls_enh-enhinclude )
              i_include = CONV #( ls_enh-enhinclude ) CHANGING cs_source = io_debugger->mo_window->ms_sources ).
          ENDIF.

          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
            WITH KEY eventtype = 'FORM' eventname = form_name INTO DATA(ls_call_line).
          CHECK sy-subrc = 0.

          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = ls_call_line-include ASSIGNING FIELD-SYMBOL(<prog>).
          CHECK sy-subrc = 0.

          DATA(lv_form_tabix) = 0.
          DATA ls_kw_form TYPE ZCL_ACE=>ts_kword.
          LOOP AT <prog>-t_keywords INTO ls_kw_form.
            IF ls_kw_form-name = 'FORM' AND ls_kw_form-index = ls_call_line-index.
              lv_form_tabix = sy-tabix. EXIT.
            ENDIF.
          ENDLOOP.
          CHECK lv_form_tabix > 0.
          READ TABLE <prog>-v_keywords WITH KEY include = ls_kw_form-include
            index = ls_kw_form-index INTO DATA(ls_vkw_form).
          IF sy-subrc = 0. ls_kw_form-v_line = ls_vkw_form-v_line. ENDIF.

          READ TABLE io_debugger->mo_window->ms_sources-tt_progs
            WITH KEY include = ls_enh-enhinclude INTO DATA(ls_enh_prog).
          CHECK sy-subrc = 0.

          DATA lt_enh_kw TYPE ZCL_ACE_WINDOW=>tt_kword.
          DATA lv_in_block TYPE boolean.
          CLEAR: lt_enh_kw, lv_in_block.
          LOOP AT ls_enh_prog-t_keywords INTO DATA(ls_kw).
            IF ls_kw-name = 'ENHANCEMENT'.
              DATA(lv_enh_id) = CONV i( ls_enh-id ).
              READ TABLE ls_enh_prog-scan->statements INDEX ls_kw-index INTO DATA(ls_stmt).
              READ TABLE ls_enh_prog-scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
              IF CONV i( ls_tok-str ) = lv_enh_id. lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw.
              ELSE. lv_in_block = abap_false. ENDIF.
              CONTINUE.
            ENDIF.
            IF ls_kw-name = 'ENDENHANCEMENT'.
              IF lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw. ENDIF.
              CLEAR lv_in_block. CONTINUE.
            ENDIF.
            IF lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw. ENDIF.
          ENDLOOP.
          CHECK lt_enh_kw IS NOT INITIAL.

          DATA ls_kw_end TYPE ZCL_ACE=>ts_kword.
          CLEAR ls_kw_end.
          IF position = 'BEGIN'.
            tabix = lv_form_tabix + 1.
          ELSE.
            tabix = lv_form_tabix + 1.
            LOOP AT <prog>-t_keywords INTO ls_kw_end FROM tabix.
              IF ls_kw_end-name = 'ENDFORM'. tabix = sy-tabix. EXIT. ENDIF.
              CLEAR ls_kw_end.
            ENDLOOP.
            READ TABLE <prog>-v_keywords WITH KEY include = ls_kw_end-include
              index = ls_kw_end-index INTO DATA(ls_vkw_end).
            IF sy-subrc = 0. ls_kw_end-v_line = ls_vkw_end-v_line. ENDIF.
          ENDIF.

          DATA(lv_vsrc_tabix) = COND i( WHEN position = 'BEGIN' THEN ls_kw_form-v_line + 1 ELSE ls_kw_end-v_line ).
          DATA(lv_vkw_tabix) = 0.
          IF position = 'BEGIN'.
            LOOP AT <prog>-v_keywords INTO DATA(ls_vkw_anchor)
              WHERE include = ls_kw_form-include AND index = ls_kw_form-index AND name = 'FORM'.
              lv_vkw_tabix = sy-tabix + 1. EXIT.
            ENDLOOP.
          ELSE.
            LOOP AT <prog>-v_keywords INTO ls_vkw_anchor
              WHERE include = ls_kw_end-include AND index = ls_kw_end-index AND name = 'ENDFORM'.
              lv_vkw_tabix = sy-tabix. EXIT.
            ENDLOOP.
          ENDIF.
          IF lv_vkw_tabix = 0. lv_vkw_tabix = lines( <prog>-v_keywords ) + 1. ENDIF.

          DATA(lv_enh_inserted) = 1.
          DATA(lv_tmp_line) = lt_enh_kw[ 1 ]-line.
          DATA(lv_tmp_last) = lt_enh_kw[ lines( lt_enh_kw ) ]-line.
          WHILE lv_tmp_line <= lv_tmp_last.
            ADD 1 TO lv_enh_inserted.
            READ TABLE lt_enh_kw WITH KEY line = lv_tmp_line INTO DATA(ls_ins_pre).
            IF sy-subrc = 0 AND ls_ins_pre-name = 'ENDENHANCEMENT'. ADD 1 TO lv_enh_inserted. ENDIF.
            ADD 1 TO lv_tmp_line.
          ENDWHILE.

          LOOP AT <prog>-v_keywords ASSIGNING FIELD-SYMBOL(<kw_v>) WHERE v_line >= lv_vsrc_tabix.
            ADD lv_enh_inserted TO <kw_v>-v_line.
            ADD lv_enh_inserted TO <kw_v>-v_from_row.
            ADD lv_enh_inserted TO <kw_v>-v_to_row.
          ENDLOOP.

          DATA(lv_vkw_vline) = lv_vsrc_tabix + 1.
          LOOP AT lt_enh_kw INTO DATA(ls_vkw_ins).
            ls_vkw_ins-v_line = lv_vkw_vline. ls_vkw_ins-v_from_row = lv_vkw_vline. ls_vkw_ins-v_to_row = lv_vkw_vline.
            INSERT ls_vkw_ins INTO <prog>-v_keywords INDEX lv_vkw_tabix.
            ADD 1 TO lv_vkw_tabix. ADD 1 TO lv_vkw_vline.
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
          INDEX lv_prog_enh_tabix ASSIGNING <prog_enh>.
        IF sy-subrc = 0. <prog_enh>-enh_collected = abap_true. ENDIF.
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
        ZCL_ACE_PARSER=>parse( EXPORTING i_program = lv_eimp_include i_include = lv_eimp_include
          CHANGING cs_source = io_debugger->mo_window->ms_sources ).
      ENDIF.
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = lv_eimp_include INTO DATA(ls_eimp_prog).
      CHECK sy-subrc = 0.

      DATA(lv_class_prog) = CONV program(
        i_class && repeat( val = '=' occ = 30 - strlen( i_class ) ) && 'CP' ).
      DATA(lv_cm_pattern) = lv_class_prog(28) && 'CM%'.
      SELECT include FROM d010inc INTO TABLE @DATA(lt_cm_includes)
        WHERE master = @lv_class_prog AND include LIKE @lv_cm_pattern.
      LOOP AT lt_cm_includes INTO DATA(ls_cm).
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs
          WITH KEY include = ls_cm-include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          ZCL_ACE_PARSER=>parse( EXPORTING i_program = ls_cm-include i_include = ls_cm-include
            CHANGING cs_source = io_debugger->mo_window->ms_sources ).
        ENDIF.
      ENDLOOP.

      DATA ls_call_line_m TYPE ZCL_ACE=>ts_calls_line.
      LOOP AT io_debugger->mo_window->ms_sources-tt_calls_line INTO ls_call_line_m
        WHERE eventtype = 'METHOD' AND eventname = i_method AND class = i_class.
        EXIT.
      ENDLOOP.
      CHECK sy-subrc = 0.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = ls_call_line_m-include ASSIGNING FIELD-SYMBOL(<prog_m>).
      CHECK sy-subrc = 0.

      DATA(lv_meth_tabix) = 0.
      DATA ls_kw_meth TYPE ZCL_ACE=>ts_kword.
      LOOP AT <prog_m>-t_keywords INTO ls_kw_meth.
        IF ls_kw_meth-name = 'METHOD' AND ls_kw_meth-index = ls_call_line_m-index.
          lv_meth_tabix = sy-tabix. EXIT.
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
            lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw.
          ELSE. lv_in_block = abap_false. ENDIF.
          CONTINUE.
        ENDIF.
        IF ls_kw-name = 'ENDMETHOD'.
          IF lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw. EXIT. ENDIF.
          CLEAR lv_in_block. CONTINUE.
        ENDIF.
        IF lv_in_block = abap_true. APPEND ls_kw TO lt_enh_kw. ENDIF.
      ENDLOOP.
      CHECK lt_enh_kw IS NOT INITIAL.

      DATA(lv_ins_tabix) = lv_meth_tabix + 1.
      DATA ls_kw_end TYPE ZCL_ACE=>ts_kword.
      IF i_meth_pos = 'END' OR i_meth_pos = 'OVERWRITE'.
        LOOP AT <prog_m>-t_keywords INTO ls_kw_end FROM lv_ins_tabix.
          IF ls_kw_end-name = 'ENDMETHOD'. lv_ins_tabix = sy-tabix + 1. EXIT. ENDIF.
        ENDLOOP.
      ENDIF.

      IF i_meth_pos = 'OVERWRITE'.
        READ TABLE <prog_m>-tt_enh_blocks TRANSPORTING NO FIELDS
          WITH KEY ev_type = 'METHOD' ev_name = i_method position = 'OVERWRITE' enh_name = i_enhname.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO <prog_m>-tt_enh_blocks ASSIGNING FIELD-SYMBOL(<enh_blk_ow>).
          <enh_blk_ow>-ev_type = 'METHOD'. <enh_blk_ow>-ev_name = i_method.
          <enh_blk_ow>-position = 'OVERWRITE'. <enh_blk_ow>-enh_name = i_enhname.
          <enh_blk_ow>-enh_include = CONV #( |{ i_enhinclude }IMP| ).
          <enh_blk_ow>-from_line = 0. <enh_blk_ow>-to_line = 0.
        ENDIF.
        RETURN.
      ENDIF.

      READ TABLE <prog_m>-tt_enh_blocks TRANSPORTING NO FIELDS
        WITH KEY ev_type = 'METHOD' ev_name = i_method position = i_meth_pos enh_name = i_enhname.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO <prog_m>-tt_enh_blocks ASSIGNING FIELD-SYMBOL(<enh_blk>).
        <enh_blk>-ev_type = 'METHOD'. <enh_blk>-ev_name = i_method.
        <enh_blk>-position = i_meth_pos. <enh_blk>-enh_name = i_enhname.
        <enh_blk>-enh_include = CONV #( |{ i_enhinclude }IMP| ).
        <enh_blk>-from_line = 0. <enh_blk>-to_line = 0.
      ENDIF.

  endmethod.


  method DETECT_METHOD_CALL.
      DATA: split TYPE string_table.
      IF call-event = 'METHOD' AND call-name IS NOT INITIAL.
        APPEND call TO token-tt_calls.
        CLEAR: call-event, call-type, call-name, call-outer, call-inner.
      ENDIF.
      call-name = word. call-event = 'METHOD'.
      REPLACE ALL OCCURRENCES OF '(' IN call-name WITH ''.
      REPLACE ALL OCCURRENCES OF 'ME->' IN call-name WITH ''.
      FIND FIRST OCCURRENCE OF '->' IN call-name.
      IF sy-subrc = 0.
        SPLIT call-name AT '->' INTO TABLE split.
        IF split[ 1 ] <> ')'.
          READ TABLE io_debugger->mo_window->ms_sources-t_vars
            WITH KEY program = i_program name = split[ 1 ] INTO DATA(vars).
          IF sy-subrc <> 0. call-class = split[ 1 ]. ELSE. call-class = vars-type. ENDIF.
        ENDIF.
        call-name = split[ 2 ].
        IF split[ 1 ] = 'SUPER'. call-class = class_name. call-super = abap_true. ENDIF.
      ENDIF.
      FIND FIRST OCCURRENCE OF '=>' IN call-name.
      IF sy-subrc = 0.
        SPLIT call-name AT '=>' INTO TABLE split.
        IF split[ 1 ] <> ')'. call-class = split[ 1 ]. ENDIF.
        call-name = split[ 2 ].
      ENDIF.
      IF call-class IS INITIAL.
        IF i_class IS NOT INITIAL. call_line-class = call-class = i_class. ENDIF.
        IF class_name IS NOT INITIAL. call_line-class = call-class = class_name. ENDIF.
      ENDIF.
      call-event = 'METHOD'.
      IF new = abap_true.
        call-class = call-name. call-name = 'CONSTRUCTOR'.
        call_line-class = call-class. call_line-eventname = call-name. call_line-eventtype = 'METHOD'.
        READ TABLE calculated_vars WITH KEY line = l_token_row program = i_include INTO DATA(calc).
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO io_debugger->mo_window->ms_sources-tt_refvar ASSIGNING FIELD-SYMBOL(<refvar>).
          <refvar>-name = calc-name. <refvar>-class = call-class.
        ENDIF.
      ENDIF.
      READ TABLE io_debugger->mo_window->ms_sources-tt_refvar WITH KEY name = call-class INTO DATA(refvar).
      IF sy-subrc = 0. call-class = refvar-class. ENDIF.
  endmethod.


  method LINK_CALLS_TO_PARAMS.
      FIELD-SYMBOLS: <s_token> TYPE ZCL_ACE=>ts_kword, <call> TYPE ZCL_ACE=>ts_calls.
      DATA: call TYPE ZCL_ACE=>ts_calls, param TYPE ZCL_ACE=>ts_params, index TYPE i.
      LOOP AT ct_tokens ASSIGNING <s_token> WHERE tt_calls IS NOT INITIAL.
        READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
        index = 0.
        LOOP AT io_debugger->mo_window->ms_sources-t_params INTO param
          WHERE event = call-event AND name = call-name.
          ADD 1 TO index.
          READ TABLE <s_token>-tt_calls INDEX index ASSIGNING <call>.
          IF sy-subrc = 0.
            <call>-inner = param-param.
            IF param-type = 'I'. <call>-type = '>'. ELSE. <call>-type = '<'. ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
  endmethod.


  METHOD parse_call.

    DATA: statement TYPE i,
          stack     TYPE i,
          include   TYPE progname,
          prefix    TYPE string,
          program   TYPE program.

    stack = i_stack + 1.
    CHECK stack <= io_debugger->mo_window->m_hist_depth.

    READ TABLE io_debugger->mt_steps
      WITH KEY program = i_include eventname = i_e_name eventtype = i_e_type class = i_class
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0. RETURN. ENDIF.

    READ TABLE io_debugger->mo_window->mt_calls
      WITH KEY include = i_include ev_name = i_e_name class = i_class
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      APPEND INITIAL LINE TO io_debugger->mo_window->mt_calls ASSIGNING FIELD-SYMBOL(<method_call>).
      <method_call>-include = i_include.
      <method_call>-ev_name = i_e_name.
      <method_call>-class   = i_class.
    ENDIF.

    DATA: cl_key        TYPE seoclskey,
          meth_includes TYPE seop_methods_w_include.
    cl_key = i_class.

    READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
      WITH KEY class = i_class TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      statement = i_index.
    ELSE.
      CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
        EXPORTING clskey = cl_key IMPORTING includes = meth_includes
        EXCEPTIONS _internal_class_not_existing = 1 OTHERS = 2.
      IF lines( meth_includes ) IS INITIAL. statement = i_index. ELSE. statement = 1. ENDIF.
    ENDIF.

    IF i_include IS NOT INITIAL.
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = i_include INTO DATA(prog).
      IF sy-subrc <> 0.
        zcl_ace_parser=>parse(
          EXPORTING i_program = i_program i_include = i_include
          CHANGING  cs_source = io_debugger->mo_window->ms_sources ).
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO prog.
      ENDIF.
    ELSE.
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs
        WITH KEY include = i_program INTO prog.
      IF sy-subrc <> 0.
        zcl_ace_parser=>parse(
          EXPORTING i_program = i_program i_include = i_program
          CHANGING  cs_source = io_debugger->mo_window->ms_sources ).
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO prog.
      ENDIF.
    ENDIF.

    DATA(max) = lines( prog-scan->statements ).
    DO.
      IF statement > max. EXIT. ENDIF.

      READ TABLE prog-t_keywords WITH KEY index = statement INTO DATA(key).
      IF sy-subrc <> 0. ADD 1 TO statement. CONTINUE. ENDIF.
      IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS' OR key-name IS INITIAL.
        ADD 1 TO statement. CONTINUE.
      ENDIF.

      " Точечный парсинг calls/calcs/vars через parse_tokens( i_stmt_idx )
      IF key-calls_parsed = abap_false.
        zcl_ace_parser=>parse_tokens(
          EXPORTING
            i_program  = CONV #( key-program )
            i_include  = CONV #( key-include )
            i_stmt_idx = key-index
            i_class    = i_class
            i_evtype   = i_e_type
            i_ev_name  = i_e_name
          CHANGING
            cs_source  = io_debugger->mo_window->ms_sources ).
        " Перечитываем key — calls_parsed = true, tt_calls заполнен
        READ TABLE prog-t_keywords WITH KEY index = statement INTO key.
      ENDIF.

      READ TABLE io_debugger->mt_steps
        WITH KEY line = key-line program = i_program include = key-include
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ADD 1 TO io_debugger->m_step.
        APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
        <step>-step       = io_debugger->m_step.
        <step>-line       = key-line.
        <step>-eventname  = i_e_name.
        <step>-eventtype  = i_e_type.
        <step>-stacklevel = stack.
        <step>-program    = i_program.
        <step>-include    = key-include.
        <step>-class      = i_class.
      ENDIF.

      LOOP AT key-tt_calls INTO DATA(call).
        IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).
          IF call-event = 'FORM'.
            parse_call_form( i_call_name = call-name i_program = i_include
              i_include = i_include i_stack = stack io_debugger = io_debugger ).

          ELSEIF call-event = 'FUNCTION'.
            DATA func TYPE rs38l_fnam.
            func = call-name.
            REPLACE ALL OCCURRENCES OF '''' IN func WITH ''.
            IF io_debugger->mo_window->m_zcode IS INITIAL OR
              ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( func+0(1) = 'Z' OR func+0(1) = 'Y' ) ).
              CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                CHANGING funcname = func include = include
                EXCEPTIONS function_not_exists = 1 include_not_exists = 2
                           group_not_exists = 3 no_selections = 4 no_function_include = 5 OTHERS = 6.
              code_execution_scanner( i_program = include i_include = include i_stack = stack
                i_evtype = call-event i_evname = call-name io_debugger = io_debugger ).
            ENDIF.

          ELSEIF call-event = 'METHOD'.
            DATA inlude TYPE program.
            IF i_include IS INITIAL. include = i_program. ELSE. include = i_include. ENDIF.
            IF call-class = 'ME' OR call-class IS INITIAL. call-class = i_class. ENDIF.
            parse_class( i_include = include i_call = call i_stack = stack
                         io_debugger = io_debugger key = key ).

          ELSEIF call-event = 'SCREEN'.
            parse_screen( i_stack = stack i_call = call io_debugger = io_debugger key = key ).
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF key-name = 'ENDFORM' OR key-name = 'ENDMETHOD' OR key-name = 'ENDMODULE'.
        RETURN.
      ENDIF.

      ADD 1 TO statement.
    ENDDO.

  ENDMETHOD.


  METHOD parse_call_form.

    DATA call_line TYPE zcl_ace=>ts_calls_line.
    READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
      WITH KEY eventname = i_call_name eventtype = 'FORM' INTO call_line.
    CHECK sy-subrc = 0.

    DATA(lv_inc) = CONV program( call_line-include ).
    IF lv_inc IS INITIAL. lv_inc = i_include. ENDIF.

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs
      WITH KEY include = lv_inc TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      zcl_ace_parser=>parse( EXPORTING i_program = lv_inc i_include = lv_inc
        CHANGING cs_source = io_debugger->mo_window->ms_sources ).
    ENDIF.

    zcl_ace_source_parser=>collect_enhancements( i_program = lv_inc io_debugger = io_debugger ).

    READ TABLE io_debugger->mo_window->mt_calls
      WITH KEY include = lv_inc ev_name = i_call_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0. RETURN.
    ELSE.
      APPEND INITIAL LINE TO io_debugger->mo_window->mt_calls ASSIGNING FIELD-SYMBOL(<mc>).
      <mc>-include = lv_inc. <mc>-ev_name = i_call_name.
    ENDIF.

    DATA(lv_stack) = i_stack + 1.
    CHECK lv_stack <= io_debugger->mo_window->m_hist_depth.

    READ TABLE io_debugger->mt_steps
      WITH KEY program = lv_inc eventname = i_call_name eventtype = 'FORM' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0. RETURN. ENDIF.

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = lv_inc INTO DATA(prog).
    CHECK sy-subrc = 0.

    DATA(lv_use_vkw) = abap_false.
    IF prog-v_keywords IS NOT INITIAL. lv_use_vkw = abap_true. ENDIF.

    DATA(lv_tabix) = 0.
    IF lv_use_vkw = abap_true.
      LOOP AT prog-v_keywords INTO DATA(kw).
        IF kw-name = 'FORM' AND kw-index = call_line-index. lv_tabix = sy-tabix. EXIT. ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT prog-t_keywords INTO kw.
        IF kw-name = 'FORM' AND kw-index = call_line-index. lv_tabix = sy-tabix. EXIT. ENDIF.
      ENDLOOP.
    ENDIF.
    CHECK lv_tabix > 0.

    DATA(lv_has_pre) = abap_false.
    READ TABLE prog-tt_enh_blocks TRANSPORTING NO FIELDS WITH KEY ev_name = i_call_name position = 'BEGIN'.
    IF sy-subrc = 0. lv_has_pre = abap_true. ENDIF.
    DATA(lv_body_stack) = COND i( WHEN lv_has_pre = abap_true THEN lv_stack + 1 ELSE lv_stack ).

    DATA ls_cur_enh TYPE zcl_ace_window=>ts_enh_block.
    CLEAR ls_cur_enh.

    FIELD-SYMBOLS <kw_tab> TYPE zcl_ace_window=>tt_kword.
    IF lv_use_vkw = abap_true. ASSIGN prog-v_keywords TO <kw_tab>.
    ELSE. ASSIGN prog-t_keywords TO <kw_tab>. ENDIF.

    LOOP AT <kw_tab> INTO kw FROM lv_tabix.
      IF kw-name = 'ENDFORM'. EXIT. ENDIF.

      IF kw-name = 'ENHANCEMENT'.
        DATA(lv_enh_id_cur) = 0.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = kw-include INTO DATA(enh_prog).
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
          READ TABLE prog-tt_enh_blocks INTO ls_cur_enh WITH KEY enh_include = kw-include ev_name = i_call_name.
        ENDIF.
        CONTINUE.
      ENDIF.
      IF kw-name = 'ENDENHANCEMENT'. CLEAR ls_cur_enh. CONTINUE. ENDIF.
      IF kw-name = 'FORM' OR kw-name = 'DATA' OR kw-name = 'TYPES'
        OR kw-name = 'CONSTANTS' OR kw-name IS INITIAL. CONTINUE. ENDIF.

      " Точечный парсинг через parse_call с i_stmt_idx
      IF kw-calls_parsed = abap_false.
        parse_call(
          EXPORTING
            i_program   = CONV #( kw-program )
            i_include   = CONV #( kw-include )
            i_index     = 0
            i_stack     = lv_stack
            i_e_name    = i_call_name
            i_e_type    = 'FORM'
            i_stmt_idx  = kw-index
            io_debugger = io_debugger ).
        " Перечитываем kw с актуальным tt_calls
        READ TABLE <kw_tab> WITH KEY index = kw-index INTO kw.
      ENDIF.

      READ TABLE io_debugger->mt_steps
        WITH KEY line = kw-line program = i_program include = kw-include TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ADD 1 TO io_debugger->m_step.
        APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
        <step>-step = io_debugger->m_step. <step>-line = kw-line.
        <step>-program = i_program. <step>-include = kw-include.
        IF ls_cur_enh IS NOT INITIAL.
          <step>-eventtype = 'ENHANCEMENT'.
          <step>-eventname = |{ ls_cur_enh-enh_name } { ls_cur_enh-enh_id }|.
          IF ls_cur_enh-position = 'BEGIN'. <step>-stacklevel = lv_stack.
          ELSE. <step>-stacklevel = lv_body_stack + 1. ENDIF.
        ELSE.
          <step>-eventtype = 'FORM'. <step>-eventname = i_call_name. <step>-stacklevel = lv_body_stack.
        ENDIF.
      ENDIF.

      LOOP AT kw-tt_calls INTO DATA(call).
        IF call-name IS NOT INITIAL AND NOT ( call-event = 'METHOD' AND call-class IS INITIAL ).
          IF call-event = 'FORM'.
            zcl_ace_source_parser=>parse_call_form( i_call_name = call-name i_program = lv_inc
              i_include = lv_inc i_stack = lv_stack io_debugger = io_debugger ).
          ELSEIF call-event = 'FUNCTION'.
            DATA func TYPE rs38l_fnam.
            func = call-name.
            REPLACE ALL OCCURRENCES OF '''' IN func WITH ''.
            IF io_debugger->mo_window->m_zcode IS INITIAL OR func+0(1) = 'Z' OR func+0(1) = 'Y'.
              DATA lv_finc TYPE progname.
              CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                CHANGING funcname = func include = lv_finc EXCEPTIONS OTHERS = 6.
              IF sy-subrc = 0.
                zcl_ace_source_parser=>code_execution_scanner( i_program = lv_finc i_include = lv_finc
                  i_stack = lv_stack i_evtype = 'FUNCTION' i_evname = CONV #( func ) io_debugger = io_debugger ).
              ENDIF.
            ENDIF.
          ELSEIF call-event = 'METHOD'.
            zcl_ace_source_parser=>parse_class( i_include = lv_inc i_call = call
              i_stack = lv_stack io_debugger = io_debugger key = kw ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


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
      WITH KEY class = i_call-class TRANSPORTING NO FIELDS.
    IF sy-subrc = 0. lv_local_exists = abap_true. ENDIF.

    IF lv_local_exists = abap_false.
      CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
        EXPORTING clskey = cl_key IMPORTING includes = meth_includes
        EXCEPTIONS _internal_class_not_existing = 1 OTHERS = 2.
    ENDIF.

    IF io_debugger->mo_window->m_zcode IS INITIAL OR
     ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( i_call-class+0(1) = 'Z' OR i_call-class+0(1) = 'Y' ) )
       OR meth_includes IS INITIAL.

      IF lines( meth_includes ) > 0 AND lv_local_exists = abap_false.
        prefix = i_call-class && repeat( val = `=` occ = 30 - strlen( i_call-class ) ).
        include = program = prefix && 'CP'.
        ZCL_ACE_PARSER=>parse( EXPORTING i_program = program i_include = include
          i_class = i_call-class CHANGING cs_source = io_debugger->mo_window->ms_sources ).
      ELSE.
        program = i_include.
      ENDIF.

      IF i_call-super IS INITIAL.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
          WITH KEY class = cl_key eventtype = 'METHOD' eventname = i_call-name INTO DATA(call_line).
      ELSE.
        sy-subrc = 1.
      ENDIF.

      IF sy-subrc <> 0.
        WHILE call_line IS INITIAL.
          LOOP AT io_debugger->mo_window->ms_sources-t_classes INTO DATA(ls_class) WHERE clsname = cl_key.
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
              WITH KEY class = ls_class-refclsname eventtype = 'METHOD' eventname = i_call-name INTO call_line.
            IF sy-subrc = 0. EXIT. ENDIF.
          ENDLOOP.
          IF sy-subrc <> 0. EXIT. ENDIF.
          cl_key = ls_class-refclsname.
        ENDWHILE.
      ENDIF.

      IF call_line IS INITIAL.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
          WITH KEY class = cl_key eventtype = 'METHOD' eventname = i_call-name INTO call_line.
      ENDIF.

      IF sy-subrc = 0.
        IF call_line-include IS NOT INITIAL. include = call_line-include. ENDIF.
        IF i_call-name = 'CONSTRUCTOR'.
          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
            WITH KEY class = cl_key eventtype = 'METHOD' eventname = 'CLASS_CONSTRUCTOR' INTO DATA(call_super).
          IF sy-subrc = 0.
            zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_super-index
              i_e_name = 'CLASS_CONSTRUCTOR' i_e_type = call_line-eventtype
              i_program = CONV #( include ) i_include = CONV #( include )
              i_class = call_line-class i_stack = i_stack io_debugger = io_debugger ).
          ENDIF.
        ENDIF.
        zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
          i_e_name = call_line-eventname i_e_type = call_line-eventtype
          i_program = CONV #( include ) i_include = CONV #( include )
          i_class = call_line-class i_stack = i_stack io_debugger = io_debugger ).
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
        EXPORTING dyname = prog dynumb = fmnum
        TABLES ftab = ftab pltab = scr_code EXCEPTIONS OTHERS = 19.
      IF sy-subrc <> 0. RETURN. ENDIF.

      LOOP AT scr_code ASSIGNING FIELD-SYMBOL(<code>).
        CONDENSE <code>.
        FIND '"' IN <code> MATCH OFFSET DATA(pos).
        IF pos <> 0. <code> = <code>+0(pos). ENDIF.
      ENDLOOP.
      DELETE scr_code WHERE line+0(1) = '*' OR line+0(1) = '"' OR line IS INITIAL.

      LOOP AT scr_code INTO DATA(code).
        IF code_str IS INITIAL. code_str = code-line.
        ELSE. code_str = |{ code_str } { code-line }|. ENDIF.
      ENDLOOP.
      SPLIT code_str AT '.' INTO TABLE scr_code.

      LOOP AT scr_code INTO code.
        CONDENSE code-line.
        IF code-line = 'PROCESS BEFORE OUTPUT'.
          pbo = abap_true. CLEAR pai.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).
          ADD 1 TO io_debugger->m_step.
          <step>-step = io_debugger->m_step. <step>-line = key-line.
          <step>-eventname = i_call-name. <step>-eventtype = i_call-event.
          <step>-stacklevel = stack. <step>-program = key-program. <step>-include = key-include.
          CONTINUE.
        ENDIF.
        IF code-line = 'PROCESS AFTER INPUT'. pai = abap_true. CLEAR pbo. CONTINUE. ENDIF.
        CHECK pbo IS NOT INITIAL.
        SPLIT code-line AT | | INTO TABLE split.
        CHECK split[ 1 ] = 'MODULE'.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
          WITH KEY program = key-program eventtype = 'MODULE' eventname = split[ 2 ] INTO DATA(call_line).
        IF sy-subrc = 0.
          ZCL_ACE_SOURCE_PARSER=>parse_call( EXPORTING i_index = call_line-index
            i_e_name = call_line-eventname i_e_type = call_line-eventtype
            i_program = CONV #( call_line-program ) i_include = CONV #( call_line-include )
            i_stack = stack io_debugger = io_debugger ).
        ENDIF.
      ENDLOOP.

      LOOP AT scr_code INTO code.
        CONDENSE code-line.
        IF code-line = 'PROCESS AFTER INPUT'.
          CLEAR pbo. pai = abap_true.
          APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING <step>.
          ADD 1 TO io_debugger->m_step.
          <step>-step = io_debugger->m_step. <step>-line = key-line.
          <step>-eventname = i_call-name. <step>-eventtype = i_call-event.
          <step>-stacklevel = stack. <step>-program = key-program. <step>-include = key-include.
          CONTINUE.
        ENDIF.
        IF code-line = 'PROCESS BEFORE OUTPUT'. pbo = abap_true. CLEAR pai. CONTINUE. ENDIF.
        CHECK pai IS NOT INITIAL.
        SPLIT code-line AT | | INTO TABLE split.
        CHECK split[ 1 ] = 'MODULE'.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line
          WITH KEY program = key-program eventtype = 'MODULE' eventname = split[ 2 ] INTO call_line.
        IF sy-subrc = 0.
          ZCL_ACE_SOURCE_PARSER=>parse_call( EXPORTING i_index = call_line-index
            i_e_name = call_line-eventname i_e_type = call_line-eventtype
            i_program = CONV #( call_line-program ) i_include = CONV #( call_line-include )
            i_stack = stack io_debugger = io_debugger ).
        ENDIF.
      ENDLOOP.

  endmethod.


  method PROCESS_SUPER_AND_INTERFACES.
      DATA: suffix TYPE string, lt_classes TYPE STANDARD TABLE OF ZCL_ACE_WINDOW=>ts_meta,
            prefix TYPE string, program TYPE program, include TYPE program.
      SELECT clsname, refCLSNAME, reltype FROM seometarel APPENDING TABLE @lt_classes WHERE clsname = @i_class.
      LOOP AT lt_classes INTO DATA(interface).
        prefix = interface-refclsname && repeat( val = `=` occ = 30 - strlen( interface-refclsname ) ).
        CASE interface-reltype.
          WHEN '0' OR '1'. suffix = 'IU'.
          WHEN '2'. suffix = 'CP'.
          WHEN OTHERS. RETURN.
        ENDCASE.
        include = program = prefix && suffix.
        ZCL_ACE_PARSER=>parse( EXPORTING i_program = program i_include = include
          i_class = CONV #( interface-refclsname ) CHANGING cs_source = io_debugger->mo_window->ms_sources ).
      ENDLOOP.
      APPEND LINES OF lt_classes TO io_debugger->mo_window->ms_sources-t_classes[].
  endmethod.


  method REGISTER_FIELD_SYMBOL.
      DATA: split TYPE TABLE OF string.
      SPLIT cs_state-change AT '-' INTO TABLE split.
      cs_state-change = split[ 1 ].
      IF cs_state-eventtype IS INITIAL.
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
          <locals_set>-program = i_include. <locals_set>-eventname = cs_state-eventname. <locals_set>-eventtype = cs_state-eventtype.
        ENDIF.
        READ TABLE <locals_set>-mt_fs WITH KEY name = cs_state-change TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
          <loc_fs>-name = cs_state-change.
        ENDIF.
      ENDIF.
  endmethod.

ENDCLASS.
