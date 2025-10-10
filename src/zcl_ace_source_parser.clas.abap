class ZCL_ACE_SOURCE_PARSER definition
  public
  create public .

public section.

  class-methods PARSE_TOKENS
    importing
      !I_INCLUDE type PROGRAM
      !IO_DEBUGGER type ref to ZCL_ACE
      !I_CLASS type STRING optional
      !I_EVNAME type STRING optional .
  class-methods PARSE_CALL
    importing
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_INDEX type I
      !I_STACK type I
      !I_E_NAME type STRING
      !I_E_TYPE type STRING
      !I_CLASS type STRING optional
      !IO_DEBUGGER type ref to zCL_ACE .
  class-methods CODE_EXECUTION_SCANNER
    importing
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_EVNAME type STRING optional
      !I_EVTYPE type STRING optional
      !I_STACK type I optional
      !IO_DEBUGGER type ref to zCL_ACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_SOURCE_PARSER IMPLEMENTATION.


  method CODE_EXECUTION_SCANNER.

      "code execution scanner
      DATA: max       TYPE i,
            call_line TYPE zcl_ace_window=>ts_calls_line,
            program   TYPE program,
            prefix    TYPE string,
            event     TYPE string,
            stack     TYPE i,
            statement TYPE i,
            include   TYPE program.

      READ TABLE io_debugger->mt_steps WITH KEY program = i_include eventname = i_evname eventtype = i_evtype TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      stack =  i_stack + 1.
      "CHECK  stack < 20.

      zcl_ace_source_parser=>parse_tokens( i_include = i_include io_debugger = io_debugger ).
      READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO DATA(prog).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      DATA: structures LIKE prog-scan->structures.

      READ TABLE prog-scan->structures WITH KEY type = 'E' TRANSPORTING  NO FIELDS.
      IF sy-subrc = 0.
        structures = prog-scan->structures.
        DELETE structures WHERE type <> 'E'.
        SORT structures BY stmnt_type ASCENDING.
      ELSE.
        CLEAR  max.
        LOOP AT prog-scan->structures INTO DATA(str) WHERE type <> 'P' AND type <> 'C' .
          IF  max < str-stmnt_to.
            max = str-stmnt_to.
            APPEND str TO structures.
          ENDIF.
        ENDLOOP.
      ENDIF.

      LOOP AT structures INTO str.

        READ TABLE prog-t_keywords WITH KEY index =  str-stmnt_from INTO DATA(key).

        IF str-type = 'E'.
          statement = str-stmnt_from + 1.
          event = key-name.
        ELSE.
          statement = str-stmnt_from.
        ENDIF.

        WHILE  statement <= str-stmnt_to.
          READ TABLE prog-t_keywords WITH KEY index =   statement INTO key.

          IF key-name = 'DATA' OR key-name = 'TYPES' OR key-name = 'CONSTANTS' OR key-name IS INITIAL OR sy-subrc <> 0.
            ADD 1 TO  statement.
            CONTINUE.
          ENDIF.
          ADD 1 TO io_debugger->m_step.
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

          IF i_include IS NOT INITIAL.
            <step>-include = i_include.
          ELSE.
            <step>-include = i_program.
          ENDIF.

          IF key-to_evname IS NOT INITIAL AND NOT ( key-to_evtype = 'METHOD' AND key-to_class IS INITIAL ).

            IF key-to_evtype = 'FORM'.
              READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = key-to_evname eventtype = key-to_evtype INTO call_line.
              IF sy-subrc = 0.
                zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                                 i_e_name = call_line-eventname
                                                 i_e_type = call_line-eventtype
                                                 i_program = i_program
                                                 i_include = i_include
                                                 i_stack   =  stack
                                                 io_debugger = io_debugger ).
              ENDIF.
            ELSEIF key-to_evtype = 'FUNCTION'.
              DATA:  func TYPE rs38l_fnam.
              func = key-to_evname.
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

                code_execution_scanner( i_program =  include i_include =  include i_stack =  stack i_evtype = key-to_evtype i_evname = key-to_evname io_debugger = io_debugger ).
              ENDIF.
            ELSE. "Method call

              DATA: cl_key        TYPE seoclskey,
                    meth_includes TYPE seop_methods_w_include.
              cl_key = key-to_class.
              CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
                EXPORTING
                  clskey                       = cl_key
                IMPORTING
                  includes                     = meth_includes
                EXCEPTIONS
                  _internal_class_not_existing = 1
                  OTHERS                       = 2.


              IF io_debugger->mo_window->m_zcode IS INITIAL OR
               ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( key-to_class+0(1) = 'Z' OR key-to_class+0(1) = 'Y' ) )
                OR meth_includes IS INITIAL.


                IF lines( meth_includes ) > 0.
                  prefix = key-to_class && repeat( val = `=` occ = 30 - strlen( key-to_class ) ).
                  program =  prefix && 'CU'.
                  zcl_ace_source_parser=>parse_tokens( i_include =  program io_debugger = io_debugger i_class = key-to_class ).

                  program =  prefix && 'CI'.
                  zcl_ace_source_parser=>parse_tokens( i_include =  program io_debugger = io_debugger i_class = key-to_class ).

                  program =  prefix && 'CO'.
                  zcl_ace_source_parser=>parse_tokens( i_include =  program io_debugger = io_debugger i_class = key-to_class ).

                  program = prefix && 'CP'.

                  READ TABLE meth_includes[] WITH KEY cpdkey-cpdname = key-to_evname INTO DATA(incl).                        .
                  IF sy-subrc = 0.
                    include = incl-incname.
                    zcl_ace_source_parser=>parse_tokens( i_include =  include io_debugger = io_debugger i_class = key-to_class i_evname = key-to_evname ).
                  ENDIF.
                ELSE.
                  program = i_include.
                ENDIF.
                READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = key-to_class eventtype = 'METHOD' eventname = key-to_evname INTO call_line.
                IF sy-subrc = 0.
                  zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                        i_e_name = call_line-eventname
                                        i_e_type = call_line-eventtype
                                        i_program =  program
                                        i_include =  include
                                        i_class = key-to_class
                                        i_stack   =  stack
                                        io_debugger = io_debugger ).
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.

          ADD 1 TO  statement.
        ENDWHILE.

      ENDLOOP.


  endmethod.


  method PARSE_CALL.

      DATA: statement TYPE i,
            stack     TYPE i,
            include   TYPE progname,
            prefix    TYPE string,
            program   TYPE program.

      READ TABLE io_debugger->mt_steps WITH KEY program = i_include eventname = i_e_name eventtype = i_e_type TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      DATA: cl_key        TYPE seoclskey,
            meth_includes TYPE seop_methods_w_include.
      cl_key = i_class.
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

      stack = i_stack + 1.
      "CHECK  stack < 20.
      IF i_include IS NOT INITIAL.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO DATA(prog).
      ELSE.
        READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_program INTO prog.
      ENDIF.
      DATA(max) = lines( prog-t_keywords ).
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
        ADD 1 TO io_debugger->m_step.
        APPEND INITIAL LINE TO io_debugger->mt_steps ASSIGNING FIELD-SYMBOL(<step>).

        <step>-step = io_debugger->m_step.
        <step>-line = key-line.
        <step>-eventname = i_e_name.
        <step>-eventtype = i_e_type.
        <step>-stacklevel =  stack.
        <step>-program = i_program.
        IF i_include IS NOT INITIAL.
          <step>-include = i_include.
        ELSE.
          <step>-include = i_program.
        ENDIF.

        IF key-to_evname IS NOT INITIAL AND NOT ( key-to_evtype = 'METHOD' AND key-to_class IS INITIAL ).
          .
          IF key-to_evtype = 'FORM'.

            READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = key-to_evname eventtype = key-to_evtype INTO DATA(call_line).
            IF sy-subrc = 0.
              zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                                       i_e_name = call_line-eventname
                                                       i_e_type = call_line-eventtype
                                                       i_program = i_include
                                                       i_include = i_include
                                                       i_stack   =  stack
                                                       io_debugger = io_debugger ).
            ENDIF.

          ELSEIF key-to_evtype = 'FUNCTION'.
            DATA:  func TYPE rs38l_fnam.
            func = key-to_evname.
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

              code_execution_scanner( i_program =  include i_include =  include i_stack =  stack i_evtype = key-to_evtype i_evname = key-to_evname io_debugger = io_debugger ).
            ENDIF.
          ELSE. "METHOD CALL
            cl_key = key-to_class.
            CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
              EXPORTING
                clskey                       = cl_key
              IMPORTING
                includes                     = meth_includes
              EXCEPTIONS
                _internal_class_not_existing = 1
                OTHERS                       = 2.

            IF io_debugger->mo_window->m_zcode IS INITIAL OR
             ( io_debugger->mo_window->m_zcode IS NOT INITIAL AND ( key-to_class+0(1) = 'Z' OR key-to_class+0(1) = 'Y' ) )
               OR meth_includes IS INITIAL.

              IF  lines( meth_includes ) > 0.

                prefix = key-to_class && repeat( val = `=` occ = 30 - strlen( key-to_class ) ).
                program =  prefix && 'CU'.
                zcl_ace_source_parser=>parse_tokens( i_include =  program io_debugger = io_debugger i_class = key-to_class ).

                program =  prefix && 'CI'.
                zcl_ace_source_parser=>parse_tokens( i_include =  program io_debugger = io_debugger i_class = key-to_class ).

                program =  prefix && 'CO'.
                zcl_ace_source_parser=>parse_tokens( i_include =  program io_debugger = io_debugger i_class = key-to_class ).

                program = prefix && 'CP'.

                READ TABLE meth_includes[] WITH KEY cpdkey-cpdname = key-to_evname INTO DATA(incl).                        .
                IF sy-subrc = 0.
                  include = incl-incname.
                  zcl_ace_source_parser=>parse_tokens( i_include =  include io_debugger = io_debugger i_class = key-to_class i_evname = i_e_name ).
                ENDIF.
              ELSE.
                program = i_include.
              ENDIF.
              READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY class = key-to_class eventtype = 'METHOD' eventname = key-to_evname INTO call_line.
              IF sy-subrc = 0.
                zcl_ace_source_parser=>parse_call( EXPORTING i_index = call_line-index
                                      i_e_name = call_line-eventname
                                      i_e_type = call_line-eventtype
                                      i_program =  program
                                      i_include =  include
                                      i_class = key-to_class
                                      i_stack   =  stack
                                      io_debugger = io_debugger ).
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.

        IF key-name = 'ENDFORM' OR key-name = 'ENDMETHOD'.
          RETURN.
        ENDIF.

        ADD 1 TO  statement.
      ENDDO.

  endmethod.


  method PARSE_TOKENS.


      DATA: lr_scan         TYPE REF TO cl_ci_scan,
            prev            TYPE string,
            change          TYPE string,
            split           TYPE TABLE OF string,
            o_scan          TYPE REF TO cl_ci_scan,
            o_statement     TYPE REF TO if_ci_kzn_statement_iterator,
            o_procedure     TYPE REF TO if_ci_kzn_statement_iterator,
            token           TYPE zcl_ace_window=>ts_kword,
            calculated      TYPE zcl_ace_window=>ts_calculated_var,
            composed        TYPE zcl_ace_window=>ts_composed_var,
            tokens          TYPE zcl_ace_window=>tt_kword,
            calculated_vars TYPE zcl_ace_window=>tt_calculated,
            composed_vars   TYPE zcl_ace_window=>tt_composed,
            call            TYPE zcl_ace_window=>ts_calls,
            call_line       TYPE zcl_ace_window=>ts_calls_line,
            tab             TYPE zcl_ace_window=>ts_int_tabs,
            tabs            TYPE zcl_ace_window=>tt_tabs,
            eventtype       TYPE string,
            eventname       TYPE string,
            param           TYPE zcl_ace_window=>ts_params,
            par             TYPE char1,
            type            TYPE char1,
            class           TYPE boolean,
            cl_name         TYPE string,
            preferred       TYPE boolean.

      READ TABLE io_debugger->mo_window->ms_sources-tt_progs WITH KEY include = i_include INTO DATA(prog).
      IF sy-subrc <> 0.

        DATA(o_source) = cl_ci_source_include=>create( p_name = i_include ).
        prog-source_tab = o_source->lines.
        o_scan = NEW cl_ci_scan( p_include = o_source ).

        prog-include = i_include.

        o_statement = cl_cikzn_scan_iterator_factory=>get_statement_iterator( ciscan = o_scan ).
        o_procedure = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = o_scan ).


        "methods in definition should be overwritten by Implementation section
        IF i_class IS NOT INITIAL.
          class = abap_true.
          call_line-class = param-class = i_class.
          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = i_evname eventtype = 'METHOD' ASSIGNING FIELD-SYMBOL(<call_line>).
          IF sy-subrc = 0.
            <call_line>-index = o_procedure->statement_index + 1.
          ENDIF.
        ENDIF.

        TRY.
            o_statement->next( ).
          CATCH cx_scan_iterator_reached_end.
            EXIT.
        ENDTRY.

        DATA(kw) = o_statement->get_keyword( ).

        DATA(word) = o_statement->get_token( offset = 2 ).

        o_procedure->statement_index = o_statement->statement_index.
        o_procedure->statement_type = o_statement->statement_type.

        DATA(max) = lines( o_scan->statements ).
        DO.
          CLEAR token-tt_calls.
          "IF sy-index <> 1.
          TRY.
              o_procedure->next( ).
            CATCH cx_scan_iterator_reached_end.
          ENDTRY.
          kw = o_procedure->get_keyword( ).

          token-name = kw.
          token-index = o_procedure->statement_index.
          READ TABLE o_scan->statements INDEX o_procedure->statement_index INTO DATA(statement).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          READ TABLE o_scan->tokens INDEX statement-from INTO DATA(l_token).
          token-line = calculated-line = composed-line = l_token-row.
          calculated-program = composed-program = i_include.

          DATA  new TYPE boolean.

          IF kw = 'CLASS'.
            class = abap_true.
          ENDIF.

          IF kw = 'FORM' OR kw = 'METHOD' OR kw = 'METHODS' OR kw = 'CLASS-METHODS'.
            tab-eventtype =  eventtype = param-event =  kw.

            CLEAR  eventname.
            IF kw = 'FORM'.
              CLEAR:  class, param-class.
            ELSE.
              tab-eventtype =  eventtype = param-event =  'METHOD'.
            ENDIF.
          ENDIF.

          IF kw = 'ENDFORM' OR kw = 'ENDMETHOD'.
            CLEAR:  eventtype,  eventname, tabs.
            IF param-param IS INITIAL. "No params - save empty row if no params
              READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = param-event name = param-name TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                CLEAR param-type.
                APPEND param TO io_debugger->mo_window->ms_sources-t_params.
              ENDIF.
            ENDIF.
          ENDIF.

          CLEAR  prev.
          IF kw = 'ASSIGN' OR kw = 'ADD' OR kw = 'SUBTRACT' .
            DATA(count) = 0.
          ENDIF.
          CLEAR:  new, token-to_evname, token-to_evtype, token-to_class .


          WHILE 1 = 1.
            IF kw IS INITIAL.
              EXIT.
            ENDIF.
            CLEAR  change.
            word = o_procedure->get_token( offset = sy-index ).

            IF ( word CS '(' AND ( NOT word CS ')' ) ) OR word CS '->' OR word CS '=>'."can be method call
              call-name = word.
              call-event = 'METHOD'.
              REPLACE ALL OCCURRENCES OF '(' IN call-name WITH ''.
              FIND FIRST OCCURRENCE OF '->' IN  call-name.
              IF sy-subrc = 0.
                SPLIT call-name  AT '->' INTO TABLE split.
                call-class = split[ 1 ].
                call-name = split[ 2 ].
              ENDIF.

              FIND FIRST OCCURRENCE OF '=>' IN  call-name.
              IF sy-subrc = 0.
                SPLIT call-name  AT '=>' INTO TABLE split.
                call-class = split[ 1 ].
                call-name = split[ 2 ].
              ENDIF.

              IF call-class = 'ME' AND i_class IS NOT INITIAL.
                call-class  =  i_class.
              ENDIF.

              IF call-class IS INITIAL AND i_class IS NOT INITIAL.
                call-class  =  i_class.
              ENDIF.

              token-to_evname = call-name.
              token-to_evtype = call-event = 'METHOD'.
              IF  new = abap_true.
                call-class = call-name.
                call-name =  token-to_evname = 'CONSTRUCTOR'.
              ENDIF.
              IF  new = abap_true.
                READ TABLE calculated_vars WITH KEY line = l_token-row program = i_include INTO DATA(calc).
                IF sy-subrc = 0.
                  APPEND INITIAL LINE TO  io_debugger->mo_window->ms_sources-tt_refvar ASSIGNING FIELD-SYMBOL(<refvar>).
                  <refvar>-name = calc-name.
                  <refvar>-class = call-class.
                  call-class = call-class.
                ENDIF.
              ENDIF.

              READ TABLE io_debugger->mo_window->ms_sources-tt_refvar WITH KEY name = call-class INTO DATA(refvar).
              IF sy-subrc = 0.
                call-class = refvar-class.
              ENDIF.

              token-to_class = call-class.
            ENDIF.

            IF sy-index = 1 AND token-name = word.
              CONTINUE.
            ENDIF.

            IF sy-index = 2 AND ( kw = 'DATA' OR kw = 'PARAMETERS' ).
              WRITE: 'var =', word.
              tab-name = word.
            ENDIF.

            IF sy-index = 2 AND kw = 'PERFORM'.
              token-to_evname = call-name = word.
              token-to_evtype = call-event = 'FORM'.
            ENDIF.

            IF sy-index = 2 AND  class = abap_true AND param-class IS INITIAL.
              call_line-class = param-class = word.
            ENDIF.

            IF sy-index = 2 AND  eventtype IS NOT INITIAL AND  eventname IS INITIAL.
              tab-eventname =  eventname = param-name = word.

              MOVE-CORRESPONDING tab TO call_line.
              call_line-index = o_procedure->statement_index + 1.
              "methods in definition should be overwritten by Implementation section
              READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line WITH KEY eventname = call_line-eventname eventtype = call_line-eventtype ASSIGNING <call_line>.
              IF sy-subrc = 0.
                <call_line> = call_line.
              ELSE.
                IF i_class IS INITIAL.
                  call_line-program = i_include.
                ENDIF.
                APPEND call_line TO io_debugger->mo_window->ms_sources-tt_calls_line.
              ENDIF.

            ENDIF.

            IF word = ''.
              IF call IS NOT INITIAL.
                APPEND call TO token-tt_calls.
              ENDIF.
              CLEAR call.
              CASE kw.
                WHEN 'COMPUTE'.
                  IF  NOT  prev CO '0123456789.+-/* '.
                    composed-name =  prev.
                    APPEND  composed TO composed_vars.
                  ENDIF.
                WHEN 'CLEAR' OR 'SORT' OR 'CONDENSE'."no logic
                WHEN 'FORM'.
                  IF param-name IS NOT INITIAL.
                    APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                    CLEAR param.
                  ENDIF.
              ENDCASE.
              EXIT.
            ENDIF.

            IF word = 'USING' OR word = 'IMPORTING'.
              param-type = 'I'.
              CLEAR:  type,  par.
            ELSEIF word = 'CHANGING' OR word = 'EXPORTING' OR word = 'RETURNING'.

              IF param-param IS NOT INITIAL.
                APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                CLEAR:  type,  par, param-param.
              ENDIF.

              param-type = 'E'.
              CLEAR:  type,  par.
            ELSEIF word = 'OPTIONAL' OR word = 'PREFERRED'.
              CONTINUE.
            ELSEIF word = 'PARAMETER'.
              preferred = abap_true.
              CONTINUE.
            ENDIF.

            IF  preferred = abap_true.
              READ TABLE io_debugger->mo_window->ms_sources-t_params WITH KEY event = 'METHOD' name = param-name param = word ASSIGNING FIELD-SYMBOL(<param>).
              IF sy-subrc = 0.
                <param>-preferred = abap_true.
              ENDIF.

              CLEAR  preferred.
              CONTINUE.
            ENDIF.

            IF word <> 'CHANGING' AND word <> 'EXPORTING' AND word <> 'RETURNING' AND word <> 'IMPORTING' AND word <> 'USING'.
              IF kw = 'FORM' OR kw = 'METHODS' OR kw = 'CLASS-METHODS'.
                IF  par = abap_true AND  type IS INITIAL AND word NE 'TYPE'.

                  APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                  CLEAR:  par, param-param.
                ENDIF.

                IF  par IS INITIAL AND sy-index > 3.
                  param-param = word.
                  par = abap_true.
                  CONTINUE.
                ENDIF.
                IF  par = abap_true AND  type IS INITIAL AND word = 'TYPE'.
                  type = abap_true.
                  CONTINUE.
                ENDIF.
                IF  par = abap_true AND  type = abap_true.

                  APPEND param TO io_debugger->mo_window->ms_sources-t_params.
                  CLEAR:  type,  par, param-param.
                ENDIF.
              ENDIF.
            ENDIF.

            DATA  temp TYPE char30.
            temp = word.

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

            IF word = 'NEW'.
              new = abap_true.

            ENDIF.

            FIND FIRST OCCURRENCE OF '->' IN word.
            IF sy-subrc = 0.
              CLEAR  new.
            ENDIF.

            CASE kw.
              WHEN 'DATA' OR 'PARAMETERS'.
                IF (   prev = 'OF' ) AND  temp <> 'TABLE' AND  temp <> 'OF'.
                  tab-type =  temp.
                  APPEND tab TO tabs.
                ENDIF.

              WHEN 'COMPUTE'.
                IF  temp CA '=' AND  new IS INITIAL..
                  change =  prev.
                ENDIF.

                IF (  prev = '=' OR  prev CA '+-/*' ) AND  temp <> 'NEW'.
                  IF NOT  temp  CA '()' .
                    IF NOT  temp  CO '0123456789. '.
                      composed-name =  temp.
                      APPEND  composed TO composed_vars.
                      IF call IS NOT INITIAL.
                        call-outer =  temp.
                        READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                        IF sy-subrc <> 0.
                          APPEND call TO token-tt_calls.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.

              WHEN 'PERFORM' .

                IF   temp = 'USING' OR  temp = 'CHANGING' .
                  CLEAR  prev.
                ENDIF.

                IF   prev = 'USING' OR  prev = 'CHANGING' .

                  IF NOT  temp  CA '()' .
                    IF NOT  temp  CO '0123456789. '.
                      call-outer =  temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                      change =  temp.
                    ENDIF.
                  ENDIF.
                ENDIF.

              WHEN 'CREATE' OR 'CALL'.
                DATA: import TYPE boolean,
                      export.

                IF  prev = 'FUNCTION' AND kw = 'CALL'.
                  token-to_evtype =   call-event = 'FUNCTION'.
                  token-to_evname =  call-name = word.
                  REPLACE ALL OCCURRENCES OF '''' IN  token-to_evname WITH ''.
                ENDIF.

                IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
                  export = abap_true.
                  CLEAR  import.
                  CONTINUE.

                ELSEIF word = 'IMPORTING'.
                  import = abap_true.
                  CLEAR  export.
                  CONTINUE.

                ENDIF.

                IF  prev = 'OBJECT'.
                  "WRITE : 'value',  temp.
*          CONTINUE.
                ENDIF.

                IF   prev = '='.
                  IF NOT  temp  CA '()'.
                    IF NOT  temp  CO '0123456789. '.
                      IF  import = abap_true.
                        call-outer =  temp.
                        READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                        IF sy-subrc <> 0.
                          APPEND call TO token-tt_calls.
                        ENDIF.
                        calculated-name =  temp.
                        APPEND  calculated TO calculated_vars.
                      ELSEIF  export = abap_true.
                        call-outer =  temp.
                        READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                        IF sy-subrc <> 0.
                          APPEND call TO token-tt_calls.
                        ENDIF.
                        composed-name =  temp.
                        APPEND  composed TO composed_vars.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ELSE.
                  IF NOT  temp  CO '0123456789. ' AND  temp <> '=' AND (  import = abap_true OR  export = abap_true ).
                    call-inner =  temp.
                  ENDIF.
                ENDIF.

              WHEN 'CLEAR' OR 'SORT'.
                change =  temp.
              WHEN  'CONDENSE'.

                IF  temp <> 'NO-GAPS'.
                  change =  temp.
                ENDIF.
              WHEN 'ASSIGN' OR 'UNASSIGN'.
                ADD 1 TO  count.
                IF  count <> 2.
                  change =  temp.
                ENDIF.
              WHEN 'ADD' OR 'SUBTRACT'.
                ADD 1 TO  count.
                IF  count = 1.
                  IF  NOT  temp CO '0123456789.() '.
                    composed-name =  temp.
                    APPEND  composed TO composed_vars.
                  ENDIF.
                ENDIF.
                IF  count = 3.
                  change =  temp.
                ENDIF.
              WHEN 'READ'.
                IF  prev =  'INTO' OR  prev =  'ASSIGNING'.
                  change =  temp.
                ENDIF.

              WHEN 'SELECT'.
                IF  (  prev =  'INTO' OR  prev =  '(' ) AND (  temp <> 'TABLE' AND  temp <> '('  AND  temp <> ')' AND   temp <> ',' ).
                  change =  temp.
                ENDIF.

              WHEN OTHERS.

            ENDCASE.
            IF call-event = 'METHOD'.
              IF word = 'EXPORTING' OR word = 'CHANGING' OR word = 'TABLES'.
                export = abap_true.
                CLEAR  import.
                CONTINUE.

              ELSEIF word = 'IMPORTING'.
                import = abap_true.
                CLEAR  export.
                CONTINUE.
              ENDIF.

              IF   temp = 'USING' OR  temp = 'CHANGING' .
                CLEAR  prev.
              ENDIF.

              IF   prev = 'USING' OR  prev = 'CHANGING' .

                IF NOT  temp  CA '()' .
                  IF NOT  temp  CO '0123456789. '.
                    call-outer =  temp.
                    READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                    IF sy-subrc <> 0.
                      APPEND call TO token-tt_calls.
                    ENDIF.
                    change =  temp.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF   prev = '='.
                IF NOT  temp  CA '()'.
                  IF NOT  temp  CO '0123456789. '.
                    IF  import = abap_true.
                      call-outer =  temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.

                      calculated-name =  temp.
                      APPEND  calculated TO calculated_vars.
                    ELSEIF  export = abap_true.
                      call-outer =  temp.
                      READ TABLE token-tt_calls WITH KEY event = call-event name = call-name outer = call-outer TRANSPORTING  NO FIELDS.
                      IF sy-subrc <> 0.
                        APPEND call TO token-tt_calls.
                      ENDIF.
                      composed-name =  temp.
                      APPEND  composed TO composed_vars.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSE.
                IF NOT  temp  CO '0123456789. ' AND  temp <> '=' AND (  import = abap_true OR  export = abap_true ).
                  call-inner =  temp.
                ENDIF.
              ENDIF.

            ENDIF.

            IF  temp = '(' .
              prev =  temp.
              CONTINUE.
            ENDIF.

            IF  NOT  temp  CA '()'.
              IF  temp <> 'TABLE' AND  temp <> 'NEW'  AND  prev <> '('.
                IF  kw <> 'PERFORM'.
                  prev =  temp.
                ELSEIF word = 'USING' OR word = 'CHANGING'.
                  prev =  temp.
                ENDIF.
              ENDIF.
            ENDIF.

            IF  change IS NOT INITIAL.
              calculated-name =  change.
              APPEND calculated TO calculated_vars.

              IF  change+0(1) = '<'.

                SPLIT  change AT '-' INTO TABLE split.
                change = split[ 1 ].
                IF  eventtype IS INITIAL. "Global fs
                  READ TABLE io_debugger->mo_window->mt_globals_set WITH KEY program = i_include ASSIGNING FIELD-SYMBOL(<globals_set>).
                  IF sy-subrc <> 0.
                    APPEND INITIAL LINE TO io_debugger->mo_window->mt_globals_set ASSIGNING <globals_set>.
                    <globals_set>-program = i_include.
                  ENDIF.
                  READ TABLE  <globals_set>-mt_fs WITH KEY name =  change TRANSPORTING NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND INITIAL LINE TO  <globals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<gl_fs>).
                    <gl_fs>-name =  change.
                  ENDIF.

                ELSE."local fs
                  READ TABLE io_debugger->mo_window->mt_locals_set
                   WITH KEY program = i_include eventtype =  eventtype eventname =  eventname
                   ASSIGNING FIELD-SYMBOL(<locals_set>).
                  IF sy-subrc <> 0.
                    APPEND INITIAL LINE TO io_debugger->mo_window->mt_locals_set ASSIGNING <locals_set>.
                    <locals_set>-program = i_include.
                    <locals_set>-eventname =  eventname.
                    <locals_set>-eventtype =  eventtype.
                  ENDIF.
                  READ TABLE <locals_set>-mt_fs WITH KEY name =  change TRANSPORTING NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND INITIAL LINE TO <locals_set>-mt_fs ASSIGNING FIELD-SYMBOL(<loc_fs>).
                    <loc_fs>-name =  change.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDWHILE.
          token-from = statement-from.
          token-to = statement-to.
          IF i_class IS INITIAL.
            token-to_prog = i_include.
          ENDIF.
          "check class names

          READ TABLE io_debugger->mo_window->ms_sources-tt_calls_line INTO call_line WITH KEY eventname = token-to_evname  eventtype = token-to_evtype .
          IF sy-subrc = 0.
            token-to_class = call_line-class.
          ENDIF.

          APPEND token TO tokens.
          IF o_procedure->statement_index =  max.
            EXIT.
          ENDIF.

        ENDDO.

        "Fill keyword links for calls

        LOOP AT tokens ASSIGNING FIELD-SYMBOL(<s_token>) WHERE tt_calls IS NOT INITIAL.

          READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
          DATA(index) = 0.
          LOOP AT io_debugger->mo_window->ms_sources-t_params INTO param WHERE event = call-event AND name = call-name .
            ADD 1 TO  index.
            READ TABLE <s_token>-tt_calls INDEX  index ASSIGNING FIELD-SYMBOL(<call>).
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

        "clear value(var) to var.
        LOOP AT io_debugger->mo_window->ms_sources-t_params ASSIGNING <param>.
          REPLACE ALL OCCURRENCES OF 'VALUE(' IN <param>-param WITH ''.
          REPLACE ALL OCCURRENCES OF ')' IN <param>-param WITH ''.
        ENDLOOP.

        APPEND LINES OF calculated_vars TO io_debugger->mo_window->ms_sources-t_calculated.
        APPEND LINES OF composed_vars TO io_debugger->mo_window->ms_sources-t_composed.

        "ls_source-tt_tabs = tabs.
        DATA line LIKE LINE OF io_debugger->mo_window->ms_sources-tt_progs.
        prog-scan = o_scan.
        prog-t_keywords = tokens.
        APPEND prog TO io_debugger->mo_window->ms_sources-tt_progs.

        IF io_debugger->m_step IS INITIAL.
          code_execution_scanner( i_program = i_include i_include = i_include io_debugger = io_debugger ).

          "Fill keyword links for calls
          LOOP AT io_debugger->mo_window->ms_sources-tt_progs ASSIGNING FIELD-SYMBOL(<prog>).
            LOOP AT prog-t_keywords ASSIGNING <s_token> WHERE tt_calls IS NOT INITIAL.

              READ TABLE <s_token>-tt_calls INDEX 1 INTO call.
              index = 0.
              LOOP AT io_debugger->mo_window->ms_sources-t_params INTO param WHERE event = call-event AND name = call-name .
                ADD 1 TO  index.
                READ TABLE <s_token>-tt_calls INDEX  index ASSIGNING <call>.
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
          ENDLOOP.


        ENDIF.

      ENDIF.


  endmethod.
ENDCLASS.
