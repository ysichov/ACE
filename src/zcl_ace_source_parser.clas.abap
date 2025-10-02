class ZCL_ACE_SOURCE_PARSER definition
  public
  create public .

public section.

  class-methods PARSE_TOKENS
    importing
      !IV_PROGRAM type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE
      !IV_CLASS type STRING optional
      !IV_EVNAME type STRING optional .
  class-methods PARSE_CALL
    importing
      !IV_PROGRAM type PROGRAM
      !IV_INDEX type I
      !IV_STACK type I
      !IV_EV_NAME type STRING
      !IV_EV_TYPE type STRING
      !IV_CLASS type STRING optional
      !IO_DEBUGGER type ref to ZCL_ACE .
  class-methods CODE_EXECUTION_SCANNER
    importing
      !IV_PROGRAM type PROGRAM
      !IV_EVNAME type STRING optional
      !IV_EVTYPE type STRING optional
      !IV_STACK type I optional
      !IO_DEBUGGER type ref to ZCL_ACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_SOURCE_PARSER IMPLEMENTATION.


  method CODE_EXECUTION_SCANNER.

    "code execution scanner
    DATA: lv_max       TYPE i,
          ls_call_line TYPE zcl_ace_window=>ts_calls_line,
          lv_program   TYPE program,
          lv_prefix    TYPE string,
          lv_event     TYPE string,
          lv_stack     TYPE i,
          lv_statement TYPE i,
          lv_include   TYPE program.

    READ TABLE io_debugger->mt_steps WITH KEY program = iv_program eventname = iv_evname eventtype = iv_evtype TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    lv_stack =  iv_stack + 1.
    "CHECK lv_stack < 20.

    zcl_ace_source_parser=>parse_tokens( iv_program = iv_program io_debugger = io_debugger ).
    READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = iv_program INTO DATA(ls_prog).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA: lt_str LIKE ls_prog-scan->structures.

    READ TABLE ls_prog-scan->structures WITH KEY type = 'E' TRANSPORTING  NO FIELDS.
    IF sy-subrc = 0.
      lt_str = ls_prog-scan->structures.
      DELETE lt_str WHERE type <> 'E'.
      SORT lt_str BY stmnt_type ASCENDING.
    ELSE.
      CLEAR lv_max.
      LOOP AT ls_prog-scan->structures INTO DATA(ls_str) WHERE type <> 'P' AND type <> 'C' .
        IF lv_max < ls_str-stmnt_to.
          lv_max = ls_str-stmnt_to.
          APPEND ls_str TO lt_str.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT lt_str INTO ls_str.

      READ TABLE ls_prog-t_keywords WITH KEY index =  ls_str-stmnt_from INTO DATA(ls_key).

      IF ls_str-type = 'E'.
        lv_statement = ls_str-stmnt_from + 1.
        lv_event = ls_key-name.
      ELSE.
        lv_statement = ls_str-stmnt_from.
      ENDIF.

      WHILE lv_statement <= ls_str-stmnt_to.
        READ TABLE ls_prog-t_keywords WITH KEY index =  lv_statement INTO ls_key.

        IF ls_key-name = 'DATA' OR ls_key-name = 'TYPES' OR ls_key-name = 'CONSTANTS' OR ls_key-name IS INITIAL OR sy-subrc <> 0.
          ADD 1 TO lv_statement.
          CONTINUE.
        ENDIF.
        ADD 1 TO io_debugger->m_step.
        APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).

        <step>-step = io_debugger->m_step.
        <step>-line = ls_key-line.
        IF iv_evtype IS INITIAL.
          <step>-eventtype = 'EVENT'.
          <step>-eventname = lv_event.
        ELSE.
          <step>-eventtype = iv_evtype.
          <step>-eventname = iv_evname.
        ENDIF.
        <step>-stacklevel = lv_stack.
        <step>-program = iv_program.
        <step>-include = iv_program.

        IF ls_key-to_evname IS NOT INITIAL AND NOT ( ls_key-to_evtype = 'METHOD' AND ls_key-to_class IS INITIAL ).

          IF ls_key-to_evtype = 'FORM'.
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = ls_key-to_evname eventtype = ls_key-to_evtype INTO ls_call_line.
            IF sy-subrc = 0.
              zcl_ace_source_parser=>parse_call( EXPORTING iv_index = ls_call_line-index
                                               iv_ev_name = ls_call_line-eventname
                                               iv_ev_type = ls_call_line-eventtype
                                               iv_program = iv_program
                                               iv_stack   = lv_stack
                                               io_debugger = io_debugger ).
            ENDIF.
          ELSEIF ls_key-to_evtype = 'FUNCTION'.
            DATA: lv_func TYPE rs38l_fnam.
            lv_func = ls_key-to_evname.
            IF io_debugger->mo_window->m_zcode IS INITIAL OR
             ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( lv_func+0(1) = 'Z' OR lv_func+0(1) = 'Y' ) ) .

              CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
                CHANGING
                  funcname            = lv_func
                  include             = lv_include
                EXCEPTIONS
                  function_not_exists = 1
                  include_not_exists  = 2
                  group_not_exists    = 3
                  no_selections       = 4
                  no_function_include = 5
                  OTHERS              = 6.

              code_execution_scanner( iv_program = lv_include iv_stack = lv_stack iv_evtype = ls_key-to_evtype iv_evname = ls_key-to_evname io_debugger = io_debugger ).
            ENDIF.
          ELSE. "Method call

            DATA: lv_cl_key TYPE seoclskey,
                  lt_incl   TYPE seop_methods_w_include.
            lv_cl_key = ls_key-to_class.
            CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
              EXPORTING
                clskey                       = lv_cl_key
              IMPORTING
                includes                     = lt_incl
              EXCEPTIONS
                _internal_class_not_existing = 1
                OTHERS                       = 2.


            IF io_debugger->mo_window->m_zcode IS INITIAL OR
             ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( ls_key-to_class+0(1) = 'Z' OR ls_key-to_class+0(1) = 'Y' ) )
              OR lt_incl IS INITIAL.


              IF lines( lt_incl ) > 0.
                lv_prefix = ls_key-to_class && repeat( val = `=` occ = 30 - strlen( ls_key-to_class ) ).
                lv_program = lv_prefix && 'CU'.
                zcl_ace_source_parser=>parse_tokens( iv_program = lv_program io_debugger = io_debugger iv_class = ls_key-to_class ).

                lv_program = lv_prefix && 'CI'.
                zcl_ace_source_parser=>parse_tokens( iv_program = lv_program io_debugger = io_debugger iv_class = ls_key-to_class ).

                lv_program = lv_prefix && 'CO'.
                zcl_ace_source_parser=>parse_tokens( iv_program = lv_program io_debugger = io_debugger iv_class = ls_key-to_class ).

                READ TABLE lt_incl[] WITH KEY cpdkey-cpdname = ls_key-to_evname INTO DATA(ls_incl).                        .
                IF sy-subrc = 0.
                  lv_program = ls_incl-incname.
                  zcl_ace_source_parser=>parse_tokens( iv_program = lv_program io_debugger = io_debugger iv_class = ls_key-to_class iv_evname = ls_key-to_evname ).
                ENDIF.
              ELSE.
                lv_program = iv_program.
              ENDIF.
              READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = ls_key-to_class eventtype = 'METHOD' eventname = ls_key-to_evname INTO ls_call_line.
              IF sy-subrc = 0.
                zcl_ace_source_parser=>parse_call( EXPORTING iv_index = ls_call_line-index
                                      iv_ev_name = ls_call_line-eventname
                                      iv_ev_type = ls_call_line-eventtype
                                      iv_program = lv_program
                                      iv_class = ls_key-to_class
                                      iv_stack   = lv_stack
                                      io_debugger = io_debugger ).
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

        ADD 1 TO lv_statement.
      ENDWHILE.

    ENDLOOP.


  endmethod.


  method PARSE_CALL.

    DATA: lv_statement TYPE i,
          lv_stack     TYPE i,
          lv_include   TYPE progname,
          lv_prefix    TYPE string,
          lv_program   TYPE program.

    READ TABLE io_debugger->mt_steps WITH KEY program = iv_program eventname = iv_ev_name eventtype = iv_ev_type TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    DATA: lv_cl_key TYPE seoclskey,
          lt_incl   TYPE seop_methods_w_include.
    lv_cl_key = iv_class.
    CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
      EXPORTING
        clskey                       = lv_cl_key
      IMPORTING
        includes                     = lt_incl
      EXCEPTIONS
        _internal_class_not_existing = 1
        OTHERS                       = 2.

    IF lines( lt_incl ) IS INITIAL.
      lv_statement = iv_index.
    ELSE.
      lv_statement = 1.
    ENDIF.

    lv_stack = iv_stack + 1.
    "CHECK lv_stack < 20.

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = iv_program INTO DATA(ls_prog).
    DATA(lv_max) = lines( ls_prog-t_keywords ).
    DO.
      IF lv_statement > lv_max.
        EXIT.
      ENDIF.
      READ TABLE ls_prog-t_keywords WITH KEY index =  lv_statement INTO DATA(ls_key).
      IF sy-subrc <> 0.
        ADD 1 TO lv_statement.
        CONTINUE.
      ENDIF.
      IF ls_key-name = 'DATA' OR ls_key-name = 'TYPES' OR ls_key-name = 'CONSTANTS' OR ls_key-name IS INITIAL.
        ADD 1 TO lv_statement.
        CONTINUE.
      ENDIF.
      ADD 1 TO io_debugger->m_step.
      APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).

      <step>-step = io_debugger->m_step.
      <step>-line = ls_key-line.
      <step>-eventname = iv_ev_name.
      <step>-eventtype = iv_ev_type.
      <step>-stacklevel = lv_stack.
      <step>-program = iv_program.
      <step>-include = iv_program.

      IF ls_key-to_evname IS NOT INITIAL AND NOT ( ls_key-to_evtype = 'METHOD' AND ls_key-to_class IS INITIAL ).
        .
        IF ls_key-to_evtype = 'FORM'.

          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = ls_key-to_evname eventtype = ls_key-to_evtype INTO DATA(ls_call_line).
          IF sy-subrc = 0.
            zcl_ace_source_parser=>parse_call( EXPORTING iv_index = ls_call_line-index
                                                     iv_ev_name = ls_call_line-eventname
                                                     iv_ev_type = ls_call_line-eventtype
                                                     iv_program = iv_program
                                                     iv_stack   = lv_stack
                                                     io_debugger = io_debugger ).
          ENDIF.

        ELSEIF ls_key-to_evtype = 'FUNCTION'.
          DATA: lv_func TYPE rs38l_fnam.
          lv_func = ls_key-to_evname.
          IF io_debugger->mo_window->m_zcode IS INITIAL OR
            ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( lv_func+0(1) = 'Z' OR lv_func+0(1) = 'Y' ) ) .

            CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
              CHANGING
                funcname            = lv_func
                include             = lv_include
              EXCEPTIONS
                function_not_exists = 1
                include_not_exists  = 2
                group_not_exists    = 3
                no_selections       = 4
                no_function_include = 5
                OTHERS              = 6.

            code_execution_scanner( iv_program = lv_include iv_stack = lv_stack iv_evtype = ls_key-to_evtype iv_evname = ls_key-to_evname io_debugger = io_debugger ).
          ENDIF.
        ELSE. "METHOD CALL
          lv_cl_key = ls_key-to_class.
          CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
            EXPORTING
              clskey                       = lv_cl_key
            IMPORTING
              includes                     = lt_incl
            EXCEPTIONS
              _internal_class_not_existing = 1
              OTHERS                       = 2.

          IF io_debugger->mo_window->m_zcode IS INITIAL OR
           ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( ls_key-to_class+0(1) = 'Z' OR ls_key-to_class+0(1) = 'Y' ) )
             OR lt_incl IS INITIAL.

            IF  lines( lt_incl ) > 0.

              lv_prefix = ls_key-to_class && repeat( val = `=` occ = 30 - strlen( ls_key-to_class ) ).
              lv_program = lv_prefix && 'CU'.
              zcl_ace_source_parser=>parse_tokens( iv_program = lv_program io_debugger = io_debugger iv_class = ls_key-to_class ).

              lv_program = lv_prefix && 'CI'.
              zcl_ace_source_parser=>parse_tokens( iv_program = lv_program io_debugger = io_debugger iv_class = ls_key-to_class ).

              lv_program = lv_prefix && 'CO'.
              zcl_ace_source_parser=>parse_tokens( iv_program = lv_program io_debugger = io_debugger iv_class = ls_key-to_class ).

              READ TABLE lt_incl[] WITH KEY cpdkey-cpdname = ls_key-to_evname INTO DATA(ls_incl).                        .
              IF sy-subrc = 0.
                lv_program = ls_incl-incname.
                zcl_ace_source_parser=>parse_tokens( iv_program = lv_program io_debugger = io_debugger iv_class = ls_key-to_class iv_evname = iv_ev_name ).
              ENDIF.
            ELSE.
              lv_program = iv_program.
            ENDIF.
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = ls_key-to_class eventtype = 'METHOD' eventname = ls_key-to_evname INTO ls_call_line.
            IF sy-subrc = 0.
              zcl_ace_source_parser=>parse_call( EXPORTING iv_index = ls_call_line-index
                                    iv_ev_name = ls_call_line-eventname
                                    iv_ev_type = ls_call_line-eventtype
                                    iv_program = lv_program
                                    iv_class = ls_key-to_class
                                    iv_stack   = lv_stack
                                    io_debugger = io_debugger ).
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

      IF ls_key-name = 'ENDFORM' OR ls_key-name = 'ENDMETHOD'.
        RETURN.
      ENDIF.

      ADD 1 TO lv_statement.
    ENDDO.

  endmethod.


  method PARSE_TOKENS.


    DATA: lr_scan       TYPE REF TO cl_ci_scan,
          lv_prev       TYPE string,
          lv_change     TYPE string,
          lt_split      TYPE TABLE OF string,
          lo_scan       TYPE REF TO cl_ci_scan,
          lo_statement  TYPE REF TO if_ci_kzn_statement_iterator,
          lo_procedure  TYPE REF TO if_ci_kzn_statement_iterator,
          ls_token      TYPE zcl_ace_window=>ts_kword,
          ls_calculated TYPE zcl_ace_window=>ts_calculated,
          ls_composed   TYPE zcl_ace_window=>ts_composing,
          lt_tokens     TYPE zcl_ace_window=>tt_kword,
          lt_calculated TYPE zcl_ace_window=>tt_calculated,
          lt_composed   TYPE zcl_ace_window=>tt_composed,
          ls_call       TYPE zcl_ace_window=>ts_calls,
          ls_call_line  TYPE zcl_ace_window=>ts_calls_line,
          ls_tabs       TYPE zcl_ace_window=>ts_int_tabs,
          lt_tabs       TYPE zcl_ace_window=>tt_tabs,
          lv_eventtype  TYPE string,
          lv_eventname  TYPE string,
          ls_param      TYPE zcl_ace_window=>ts_params,
          lv_par        TYPE char1,
          lv_type       TYPE char1,
          lv_class      TYPE xfeld,
          lv_cl_name    TYPE string,
          lv_preferred  TYPE xfeld.

    READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = iv_program INTO DATA(ls_prog).
    IF sy-subrc <> 0.
      ls_prog-source = cl_ci_source_include=>create( p_name = iv_program ).
      lo_scan = NEW cl_ci_scan( p_include = ls_prog-source ).

      ls_prog-include = iv_program.

      lo_statement = cl_cikzn_scan_iterator_factory=>get_statement_iterator( ciscan = lo_scan ).
      lo_procedure = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = lo_scan ).


      "methods in definition should be overwritten by Implementation section
      IF iv_class IS NOT INITIAL.
        lv_class = abap_true.
        ls_call_line-class = ls_param-class = iv_class.
        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = iv_evname eventtype = 'METHOD' ASSIGNING FIELD-SYMBOL(<call_line>).
        IF sy-subrc = 0.
          <call_line>-index = lo_procedure->statement_index + 1.
        ENDIF.
      ENDIF.

      TRY.
          lo_statement->next( ).
        CATCH cx_scan_iterator_reached_end.
          EXIT.
      ENDTRY.

      DATA(lt_kw) = lo_statement->get_keyword( ).

      DATA(token) = lo_statement->get_token( offset = 2 ).

      lo_procedure->statement_index = lo_statement->statement_index.
      lo_procedure->statement_type = lo_statement->statement_type.

      DATA(lv_max) = lines( lo_scan->statements ).
      DO.
        CLEAR ls_token-tt_calls.
        "IF sy-index <> 1.
        TRY.
            lo_procedure->next( ).
          CATCH cx_scan_iterator_reached_end.
        ENDTRY.
        lt_kw = lo_procedure->get_keyword( ).

        ls_token-name = lt_kw.
        ls_token-index = lo_procedure->statement_index.
        READ TABLE lo_scan->statements INDEX lo_procedure->statement_index INTO DATA(ls_statement).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        READ TABLE lo_scan->tokens INDEX ls_statement-from INTO DATA(l_token).
        ls_token-line = ls_calculated-line = ls_composed-line = l_token-row.
        ls_calculated-program = ls_composed-program = iv_program.

        DATA lv_new TYPE xfeld.

        IF lt_kw = 'CLASS'.
          lv_class = abap_true.
        ENDIF.

        IF lt_kw = 'FORM' OR lt_kw = 'METHOD' OR lt_kw = 'METHODS' OR lt_kw = 'CLASS-METHODS'.
          ls_tabs-eventtype = lv_eventtype = ls_param-event =  lt_kw.

          CLEAR lv_eventname.
          IF lt_kw = 'FORM'.
            CLEAR: lv_class, ls_param-class.
          ELSE.
            ls_tabs-eventtype = lv_eventtype = ls_param-event =  'METHOD'.
          ENDIF.
        ENDIF.

        IF lt_kw = 'ENDFORM' OR lt_kw = 'ENDMETHOD'.
          CLEAR: lv_eventtype, lv_eventname, ls_tabs.
          IF ls_param-param IS INITIAL. "No params - save empty row if no params
            READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = ls_param-event name = ls_param-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              CLEAR ls_param-type.
              APPEND ls_param TO io_debugger->mo_window->ms_sources-t_params.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR lv_prev.
        IF lt_kw = 'ASSIGN' OR lt_kw = 'ADD' OR lt_kw = 'SUBTRACT' .
          DATA(lv_count) = 0.
        ENDIF.
        CLEAR: lv_new, ls_token-to_evname, ls_token-to_evtype, ls_token-to_class .


        WHILE 1 = 1.
          IF lt_kw IS INITIAL.
            EXIT.
          ENDIF.
          CLEAR lv_change.
          token = lo_procedure->get_token( offset = sy-index ).

          IF ( token CS '(' AND ( NOT token CS ')' ) ) OR token CS '->' OR token CS '=>'."can be method call
            ls_call-name = token.
            ls_call-event = 'METHOD'.
            REPLACE ALL OCCURRENCES OF '(' IN ls_call-name WITH ''.
            FIND FIRST OCCURRENCE OF '->' IN  ls_call-name.
            IF sy-subrc = 0.
              SPLIT ls_call-name  AT '->' INTO TABLE lt_split.
              ls_call-class = lt_split[ 1 ].
              ls_call-name = lt_split[ 2 ].
            ENDIF.

            FIND FIRST OCCURRENCE OF '=>' IN  ls_call-name.
            IF sy-subrc = 0.
              SPLIT ls_call-name  AT '=>' INTO TABLE lt_split.
              ls_call-class = lt_split[ 1 ].
              ls_call-name = lt_split[ 2 ].
            ENDIF.

            IF ls_call-class = 'ME' AND iv_class IS NOT INITIAL.
              ls_call-class  =  iv_class.
            ENDIF.

            IF ls_call-class IS INITIAL AND iv_class IS NOT INITIAL.
              ls_call-class  =  iv_class.
            ENDIF.

            ls_token-to_evname = ls_call-name.
            ls_token-to_evtype = ls_call-event = 'METHOD'.
            IF lv_new = abap_true.
              ls_call-class = ls_call-name.
              ls_call-name =  ls_token-to_evname = 'CONSTRUCTOR'.
            ENDIF.
            IF lv_new = abap_true.
              READ TABLE lt_calculated WITH KEY line = l_token-row program = iv_program INTO DATA(ls_calc).
              IF sy-subrc = 0.
                APPEND INITIAL LINE TO  io_debugger->mo_window->ms_sources-tt_refvar ASSIGNING FIELD-SYMBOL(<refvar>).
                <refvar>-name = ls_calc-calculated.
                <refvar>-class = ls_call-class.
                ls_call-class = ls_call-class.
              ENDIF.
            ENDIF.

            READ TABLE io_debugger->mo_window->ms_sources-tt_refvar WITH KEY name = ls_call-class INTO DATA(ls_refvar).
            IF sy-subrc = 0.
              ls_call-class = ls_refvar-class.
            ENDIF.

            ls_token-to_class = ls_call-class.
          ENDIF.

          IF sy-index = 1 AND ls_token-name = token.
            CONTINUE.
          ENDIF.

          IF sy-index = 2 AND ( lt_kw = 'DATA' OR lt_kw = 'PARAMETERS' ).
            WRITE: 'var =', token.
            ls_tabs-name = token.
          ENDIF.

          IF sy-index = 2 AND lt_kw = 'PERFORM'.
            ls_token-to_evname = ls_call-name = token.
            ls_token-to_evtype = ls_call-event = 'FORM'.
          ENDIF.

          IF sy-index = 2 AND lv_class = abap_true AND ls_param-class IS INITIAL.
            ls_call_line-class = ls_param-class = token.
          ENDIF.

          IF sy-index = 2 AND lv_eventtype IS NOT INITIAL AND lv_eventname IS INITIAL.
            ls_tabs-eventname = lv_eventname = ls_param-name =  token.

            MOVE-CORRESPONDING ls_tabs TO ls_call_line.
            ls_call_line-index = lo_procedure->statement_index + 1.
            "methods in definition should be overwritten by Implementation section
            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = ls_call_line-eventname eventtype = ls_call_line-eventtype ASSIGNING <call_line>.
            IF sy-subrc = 0.
              <call_line> = ls_call_line.
            ELSE.
              IF iv_class IS INITIAL.
                ls_call_line-program = iv_program.
              ENDIF.
              APPEND ls_call_line TO io_debugger->mo_window->ms_sources-tt_calls_line.
            ENDIF.

          ENDIF.

          IF token = ''.
            IF ls_call IS NOT INITIAL.
              APPEND ls_call TO ls_token-tt_calls.
            ENDIF.
            CLEAR ls_call.
            CASE lt_kw.
              WHEN 'COMPUTE'.
                IF  NOT lv_prev CO '0123456789.+-/* '.
                  ls_composed-composing = lv_prev.
                  APPEND  ls_composed TO lt_composed.
                ENDIF.
              WHEN 'CLEAR' OR 'SORT' OR 'CONDENSE'."no logic
              WHEN 'FORM'.
                IF ls_param-name IS NOT INITIAL.
                  APPEND ls_param TO io_debugger->mo_window->ms_sources-t_params.
                  CLEAR ls_param.
                ENDIF.
            ENDCASE.
            EXIT.
          ENDIF.

          IF token = 'USING' OR token = 'IMPORTING'.
            ls_param-type = 'I'.
            CLEAR: lv_type, lv_par.
          ELSEIF token = 'CHANGING' OR token = 'EXPORTING' OR token = 'RETURNING'.

            IF ls_param-param IS NOT INITIAL.
              APPEND ls_param TO io_debugger->mo_window->ms_sources-t_params.
              CLEAR: lv_type, lv_par, ls_param-param.
            ENDIF.

            ls_param-type = 'E'.
            CLEAR: lv_type, lv_par.
          ELSEIF token = 'OPTIONAL' OR token = 'PREFERRED'.
            CONTINUE.
          ELSEIF token = 'PARAMETER'.
            lv_preferred = abap_true.
            CONTINUE.
          ENDIF.

          IF lv_preferred = abap_true.
            READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = 'METHOD' name = ls_param-name param = token ASSIGNING FIELD-SYMBOL(<param>).
            IF sy-subrc = 0.
              <param>-preferred = abap_true.
            ENDIF.

            CLEAR lv_preferred.
            CONTINUE.
          ENDIF.

          IF token <> 'CHANGING' AND token <> 'EXPORTING' AND token <> 'RETURNING' AND token <> 'IMPORTING' AND token <> 'USING'.
            IF lt_kw = 'FORM' OR lt_kw = 'METHODS' OR lt_kw = 'CLASS-METHODS'.
              IF lv_par = abap_true AND lv_type IS INITIAL AND  token NE 'TYPE'.

                APPEND ls_param TO io_debugger->mo_window->ms_sources-t_params.
                CLEAR: lv_par, ls_param-param.
              ENDIF.

              IF lv_par IS INITIAL AND sy-index > 3.
                ls_param-param = token.
                lv_par = abap_true.
                CONTINUE.
              ENDIF.
              IF lv_par = abap_true AND lv_type IS INITIAL AND token = 'TYPE'.
                lv_type = abap_true.
                CONTINUE.
              ENDIF.
              IF lv_par = abap_true AND lv_type = abap_true.

                APPEND ls_param TO io_debugger->mo_window->ms_sources-t_params.
                CLEAR: lv_type, lv_par, ls_param-param.
              ENDIF.
            ENDIF.
          ENDIF.

          DATA lv_temp TYPE char30.
          lv_temp = token.

          IF lv_temp+0(5) = 'DATA('.
            SHIFT lv_temp LEFT BY 5 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN lv_temp WITH ''.
          ENDIF.

          IF lv_temp+0(6) = '@DATA('.
            SHIFT lv_temp LEFT BY 6 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN lv_temp WITH ''.
          ENDIF.

          IF lv_temp+0(13) = 'FIELD-SYMBOL('.
            SHIFT lv_temp LEFT BY 13 PLACES.
            REPLACE ALL OCCURRENCES OF ')' IN lv_temp WITH ''.
          ENDIF.

          IF token = 'NEW'.
            lv_new = abap_true.

          ENDIF.

          FIND FIRST OCCURRENCE OF '->' IN token.
          IF sy-subrc = 0.
            CLEAR lv_new.
          ENDIF.

          CASE lt_kw.
            WHEN 'DATA' OR 'PARAMETERS'.
              IF (  lv_prev = 'OF' ) AND lv_temp <> 'TABLE' AND lv_temp <> 'OF'.
                ls_tabs-type = lv_temp.
                APPEND ls_tabs TO lt_tabs.
              ENDIF.

            WHEN 'COMPUTE'.
              IF lv_temp CA '=' AND lv_new IS INITIAL..
                lv_change = lv_prev.
              ENDIF.

              IF ( lv_prev = '=' OR lv_prev CA '+-/*' ) AND lv_temp <> 'NEW'.
                IF NOT lv_temp  CA '()' .
                  IF NOT lv_temp  CO '0123456789. '.
                    ls_composed-composing = lv_temp.
                    APPEND  ls_composed TO lt_composed.
                    IF ls_call IS NOT INITIAL.
                      ls_call-outer = lv_temp.
                      READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND ls_call TO ls_token-tt_calls.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'PERFORM' .

              IF  lv_temp = 'USING' OR lv_temp = 'CHANGING' .
                CLEAR lv_prev.
              ENDIF.

              IF  lv_prev = 'USING' OR lv_prev = 'CHANGING' .

                IF NOT lv_temp  CA '()' .
                  IF NOT lv_temp  CO '0123456789. '.
                    ls_call-outer = lv_temp.
                    READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND ls_call TO ls_token-tt_calls.
                    ENDIF.
                    lv_change = lv_temp.
                  ENDIF.
                ENDIF.
              ENDIF.

            WHEN 'CREATE' OR 'CALL'.
              DATA: lv_import TYPE xfeld,
                    lv_export.

              IF lv_prev = 'FUNCTION' AND lt_kw = 'CALL'.
                ls_token-to_evtype =   ls_call-event = 'FUNCTION'.
                ls_token-to_evname =  ls_call-name = token.
                REPLACE ALL OCCURRENCES OF '''' IN  ls_token-to_evname WITH ''.
              ENDIF.

              IF token = 'EXPORTING' OR token = 'CHANGING' OR token = 'TABLES'.
                lv_export = abap_true.
                CLEAR lv_import.
                CONTINUE.

              ELSEIF token = 'IMPORTING'.
                lv_import = abap_true.
                CLEAR lv_export.
                CONTINUE.

              ENDIF.

              IF lv_prev = 'OBJECT'.
                "WRITE : 'value', lv_temp.
*          CONTINUE.
              ENDIF.

              IF  lv_prev = '='.
                IF NOT lv_temp  CA '()'.
                  IF NOT lv_temp  CO '0123456789. '.
                    IF lv_import = abap_true.
                      ls_call-outer = lv_temp.
                      READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND ls_call TO ls_token-tt_calls.
                      ENDIF.
                      ls_calculated-calculated = lv_temp.
                      APPEND  ls_calculated TO lt_calculated.
                    ELSEIF lv_export = abap_true.
                      ls_call-outer = lv_temp.
                      READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND ls_call TO ls_token-tt_calls.
                      ENDIF.
                      ls_composed-composing = lv_temp.
                      APPEND  ls_composed TO lt_composed.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSE.
                IF NOT lv_temp  CO '0123456789. ' AND lv_temp <> '=' AND ( lv_import = abap_true OR lv_export = abap_true ).
                  ls_call-inner = lv_temp.
                ENDIF.
              ENDIF.

            WHEN 'CLEAR' OR 'SORT'.
              lv_change = lv_temp.
            WHEN  'CONDENSE'.

              IF lv_temp <> 'NO-GAPS'.
                lv_change = lv_temp.
              ENDIF.
            WHEN 'ASSIGN' OR 'UNASSIGN'.
              ADD 1 TO lv_count.
              IF lv_count <> 2.
                lv_change = lv_temp.
              ENDIF.
            WHEN 'ADD' OR 'SUBTRACT'.
              ADD 1 TO lv_count.
              IF lv_count = 1.
                IF  NOT lv_temp CO '0123456789.() '.
                  ls_composed-composing = lv_temp.
                  APPEND  ls_composed TO lt_composed.
                ENDIF.
              ENDIF.
              IF lv_count = 3.
                lv_change = lv_temp.
              ENDIF.
            WHEN 'READ'.
              IF lv_prev =  'INTO' OR lv_prev =  'ASSIGNING'.
                lv_change = lv_temp.
              ENDIF.

            WHEN 'SELECT'.
              IF  ( lv_prev =  'INTO' OR lv_prev =  '(' ) AND ( lv_temp <> 'TABLE' AND lv_temp <> '('  AND lv_temp <> ')' AND  lv_temp <> ',' ).
                lv_change = lv_temp.
              ENDIF.

            WHEN OTHERS.

          ENDCASE.
          IF ls_call-event = 'METHOD'.
            IF token = 'EXPORTING' OR token = 'CHANGING' OR token = 'TABLES'.
              lv_export = abap_true.
              CLEAR lv_import.
              CONTINUE.

            ELSEIF token = 'IMPORTING'.
              lv_import = abap_true.
              CLEAR lv_export.
              CONTINUE.
            ENDIF.

            IF  lv_temp = 'USING' OR lv_temp = 'CHANGING' .
              CLEAR lv_prev.
            ENDIF.

            IF  lv_prev = 'USING' OR lv_prev = 'CHANGING' .

              IF NOT lv_temp  CA '()' .
                IF NOT lv_temp  CO '0123456789. '.
                  ls_call-outer = lv_temp.
                  READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND ls_call TO ls_token-tt_calls.
                  ENDIF.
                  lv_change = lv_temp.
                ENDIF.
              ENDIF.
            ENDIF.

            IF  lv_prev = '='.
              IF NOT lv_temp  CA '()'.
                IF NOT lv_temp  CO '0123456789. '.
                  IF lv_import = abap_true.
                    ls_call-outer = lv_temp.
                    READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND ls_call TO ls_token-tt_calls.
                    ENDIF.

                    ls_calculated-calculated = lv_temp.
                    APPEND  ls_calculated TO lt_calculated.
                  ELSEIF lv_export = abap_true.
                    ls_call-outer = lv_temp.
                    READ TABLE ls_token-tt_calls WITH KEY event = ls_call-event name = ls_call-name outer = ls_call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND ls_call TO ls_token-tt_calls.
                    ENDIF.
                    ls_composed-composing = lv_temp.
                    APPEND  ls_composed TO lt_composed.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              IF NOT lv_temp  CO '0123456789. ' AND lv_temp <> '=' AND ( lv_import = abap_true OR lv_export = abap_true ).
                ls_call-inner = lv_temp.
              ENDIF.
            ENDIF.

          ENDIF.

          IF lv_temp = '(' .
            lv_prev = lv_temp.
            CONTINUE.
          ENDIF.

          IF  NOT lv_temp  CA '()'.
            IF lv_temp <> 'TABLE' AND lv_temp <> 'NEW'  AND lv_prev <> '('.
              IF  lt_kw <> 'PERFORM'.
                lv_prev = lv_temp.
              ELSEIF token = 'USING' OR token = 'CHANGING'.
                lv_prev = lv_temp.
              ENDIF.
            ENDIF.
          ENDIF.

          IF lv_change IS NOT INITIAL.
            ls_calculated-calculated = lv_change.
            APPEND ls_calculated TO lt_calculated.

            IF lv_change+0(1) = '<'.

              SPLIT lv_change AT '-' INTO TABLE lt_split.
              lv_change = lt_split[ 1 ].
              IF lv_eventtype IS INITIAL. "Global fs
                READ TABLE io_debugger->mo_window->mt_globals_set WITH KEY program = iv_program ASSIGNING FIELD-SYMBOL(<globals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_globals_set ASSIGNING <globals_set>.
                  <globals_set>-program = iv_program.
                ENDIF.
                READ TABLE  <globals_set>-mt_fs WITH KEY name = lv_change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO  <globals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<gl_fs>).
                  <gl_fs>-name = lv_change.
                ENDIF.

              ELSE."local fs
                READ TABLE io_debugger->mo_window->mt_locals_set
                 WITH KEY program = iv_program eventtype = lv_eventtype eventname = lv_eventname
                 ASSIGNING FIELD-SYMBOL(<locals_set>).
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO io_debugger->mo_window->mt_locals_set ASSIGNING <locals_set>.
                  <locals_set>-program = iv_program.
                  <locals_set>-eventname = lv_eventname.
                  <locals_set>-eventtype = lv_eventtype.
                ENDIF.
                READ TABLE <locals_set>-mt_fs WITH KEY name = lv_change TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
                  <loc_fs>-name = lv_change.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDWHILE.
        ls_token-from = ls_statement-from.
        ls_token-to = ls_statement-to.
        IF iv_class IS INITIAL.
          ls_token-to_prog = iv_program.
        ENDIF.
        "check class names

        READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line INTO ls_call_line WITH KEY eventname = ls_token-to_evname  eventtype = ls_token-to_evtype .
        IF sy-subrc = 0.
          ls_token-to_class = ls_call_line-class.
        ENDIF.

        APPEND ls_token TO lt_tokens.
        IF lo_procedure->statement_index = lv_max.
          EXIT.
        ENDIF.

      ENDDO.

      "Fill keyword links for calls

      LOOP AT lt_tokens ASSIGNING FIELD-SYMBOL(<s_token>) WHERE tt_calls IS NOT INITIAL.

        READ TABLE <s_token>-tt_calls INDEX 1 INTO ls_call.
        DATA(lv_index) = 0.
        LOOP AT io_debugger->mo_window->ms_sources-t_params INTO ls_param WHERE event = ls_call-event AND name = ls_call-name .
          ADD 1 TO lv_index.
          READ TABLE <s_token>-tt_calls INDEX lv_index ASSIGNING FIELD-SYMBOL(<call>).
          IF sy-subrc = 0.
            <call>-inner = ls_param-param.
            IF ls_param-type = 'I'.
              <call>-type = '>'.
            ELSE.
              <call>-type = '<'.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

      "clear value(var) to var.
      LOOP AT io_debugger->mo_window->ms_sources-t_params ASSIGNING <param>.
        REPLACE ALL OCCURRENCES OF 'VALUE(' IN <param>-param WITH ''.
        REPLACE ALL OCCURRENCES OF ')' IN <param>-param WITH ''.
      ENDLOOP.

      APPEND LINES OF lt_calculated TO io_debugger->mo_window->ms_sources-t_calculated.
      APPEND LINES OF lt_composed TO io_debugger->mo_window->ms_sources-t_composed.

      "ls_source-tt_tabs = lt_tabs.
      DATA ls_line LIKE LINE OF io_debugger->mo_window->ms_sources-tt_progs.
      ls_prog-scan = lo_scan.
      ls_prog-t_keywords = lt_tokens.
      APPEND ls_prog TO io_debugger->mo_window->ms_sources-tt_progs.

      IF io_debugger->m_step IS INITIAL.
        code_execution_scanner( iv_program = iv_program io_debugger = io_debugger ).



        "Fill keyword links for calls
        LOOP AT io_debugger->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
          LOOP AT ls_prog-t_keywords ASSIGNING <s_token> WHERE tt_calls IS NOT INITIAL.

            READ TABLE <s_token>-tt_calls INDEX 1 INTO ls_call.
            lv_index = 0.
            LOOP AT io_debugger->mo_window->ms_sources-t_params INTO ls_param WHERE event = ls_call-event AND name = ls_call-name .
              ADD 1 TO lv_index.
              READ TABLE <s_token>-tt_calls INDEX lv_index ASSIGNING <call>.
              IF sy-subrc = 0.
                <call>-inner = ls_param-param.
                IF ls_param-type = 'I'.
                  <call>-type = '>'.
                ELSE.
                  <call>-type = '<'.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.


      ENDIF.

    ENDIF.


  endmethod.
ENDCLASS.
