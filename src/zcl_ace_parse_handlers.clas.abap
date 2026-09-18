CLASS zcl_ace_parse_handlers DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

    " Collects the handler map for a whole include — call it on a full pass
    CLASS-METHODS collect
      IMPORTING
        io_scan   TYPE REF TO cl_ci_scan
        i_program TYPE program
        i_include TYPE program
      CHANGING
        cs_source TYPE zif_ace_parse_data=>ts_parse_data.

    " Resolves RAISE EVENT into the list of handler calls
    CLASS-METHODS resolve_raise_event
      IMPORTING
        io_scan    TYPE REF TO cl_ci_scan
        i_stmt_idx TYPE i
        i_program  TYPE program
        i_include  TYPE program
      CHANGING
        cs_source  TYPE zif_ace_parse_data=>ts_parse_data
        ct_calls   TYPE zif_ace_parse_data=>tt_calls.

ENDCLASS.


CLASS zcl_ace_parse_handlers IMPLEMENTATION.


  METHOD zif_ace_stmt_handler~handle.
    " Targeted pass for RAISE EVENT — add handlers to t_keywords->tt_calls
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
    CHECK sy-subrc = 0 AND ls_kw-str = 'RAISE'.
    READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok2).
    CHECK sy-subrc = 0 AND ls_tok2-str = 'EVENT'.

    DATA lt_calls TYPE zif_ace_parse_data=>tt_calls.
    resolve_raise_event(
      EXPORTING io_scan    = io_scan
                i_stmt_idx = i_stmt_idx
                i_program  = i_program
                i_include  = i_include
      CHANGING  cs_source  = cs_source
                ct_calls   = lt_calls ).

    CHECK lt_calls IS NOT INITIAL.

    LOOP AT cs_source-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE include = i_include.
      READ TABLE <prog>-t_keywords WITH KEY index = i_stmt_idx
        ASSIGNING FIELD-SYMBOL(<kw>).
      IF sy-subrc = 0.
        LOOP AT lt_calls INTO DATA(ls_c).
          READ TABLE <kw>-tt_calls
            WITH KEY event = ls_c-event name = ls_c-name class = ls_c-class
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_c TO <kw>-tt_calls.
          ENDIF.
        ENDLOOP.
      ENDIF.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


  METHOD collect.
    " ---------------------------------------------------------------
    " Two kinds of entry end up in tt_handler_map:
    "
    " 1. METHODS meth FOR EVENT ev_name OF class
    "    → static handler declaration in the class definition
    "
    " 2. SET HANDLER obj->method FOR src_obj
    "    → dynamic registration; types are resolved from t_vars
    " ---------------------------------------------------------------

    LOOP AT io_scan->statements INTO DATA(ls_stmt).
      READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
      CHECK sy-subrc = 0.

      CASE ls_kw-str.

        " ── METHODS meth FOR EVENT ev_name OF src_class ─────────────
        WHEN 'METHODS' OR 'CLASS-METHODS'.
          DATA(lv_i) = ls_stmt-from + 1.
          WHILE lv_i <= ls_stmt-to.
            READ TABLE io_scan->tokens INDEX lv_i INTO DATA(ls_t).
            IF sy-subrc <> 0. EXIT. ENDIF.
            IF ls_t-str = 'FOR'.
              READ TABLE io_scan->tokens INDEX lv_i + 1 INTO DATA(ls_t1).
              IF sy-subrc = 0 AND ls_t1-str = 'EVENT'.
                READ TABLE io_scan->tokens INDEX lv_i + 2 INTO DATA(ls_ev_tok).
                READ TABLE io_scan->tokens INDEX lv_i + 3 INTO DATA(ls_of_tok).
                READ TABLE io_scan->tokens INDEX lv_i + 4 INTO DATA(ls_src_tok).
                IF sy-subrc = 0 AND ls_of_tok-str = 'OF'.
                  " The method name is the second token of the statement
                  READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_meth_tok).
                  " hdl_class stays empty — filled later from calls_line or SET HANDLER
                  READ TABLE cs_source-tt_handler_map
                    WITH KEY src_class  = ls_src_tok-str
                             event_name = ls_ev_tok-str
                             hdl_method = ls_meth_tok-str
                    TRANSPORTING NO FIELDS.
                  IF sy-subrc <> 0.
                    APPEND VALUE zif_ace_parse_data=>ts_handler_map(
                      src_class  = ls_src_tok-str
                      event_name = ls_ev_tok-str
                      hdl_class  = ``
                      hdl_method = ls_meth_tok-str
                      include    = i_include
                      line       = ls_kw-row
                    ) TO cs_source-tt_handler_map.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
            lv_i += 1.
          ENDWHILE.

        " ── SET HANDLER obj->method FOR src_obj ─────────────────────
        WHEN 'SET'.
          READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok2).
          CHECK sy-subrc = 0 AND ls_tok2-str = 'HANDLER'.

          DATA(lv_j) = ls_stmt-from + 2.
          WHILE lv_j <= ls_stmt-to.
            READ TABLE io_scan->tokens INDEX lv_j INTO DATA(ls_sh).
            IF sy-subrc <> 0. EXIT. ENDIF.

            IF ls_sh-str CS '->'.
              DATA(lv_hdl_obj)  = ``.
              DATA(lv_hdl_meth) = ``.
              SPLIT ls_sh-str AT '->' INTO lv_hdl_obj lv_hdl_meth.
              CONDENSE: lv_hdl_obj, lv_hdl_meth.

              " Look for FOR → src_obj
              DATA(lv_k) = lv_j + 1.
              DATA(lv_src_obj) = ``.
              WHILE lv_k <= ls_stmt-to.
                READ TABLE io_scan->tokens INDEX lv_k INTO DATA(ls_for).
                IF sy-subrc <> 0. EXIT. ENDIF.
                IF ls_for-str = 'FOR'.
                  READ TABLE io_scan->tokens INDEX lv_k + 1 INTO DATA(ls_src).
                  IF sy-subrc = 0 AND ls_src-str <> 'ALL'. lv_src_obj = ls_src-str. ENDIF.
                  EXIT.
                ENDIF.
                lv_k += 1.
              ENDWHILE.

              " Resolve the type of the handler object
              DATA(lv_hdl_class) = ``.
              IF lv_hdl_obj = 'ME' OR lv_hdl_obj IS INITIAL.
                " Find the class by method name in calls_line
                LOOP AT cs_source-tt_calls_line INTO DATA(ls_cl_me)
                  WHERE eventname = lv_hdl_meth AND eventtype = 'METHOD'.
                  lv_hdl_class = ls_cl_me-class. EXIT.
                ENDLOOP.
              ELSE.
                " No class/event context here — SET HANDLER is matched across
                " the whole include, so widen the lookup.
                lv_hdl_class = zcl_ace_parse_calls=>resolve_var_type(
                  is_source = cs_source i_program = i_program
                  i_evtype = `` i_evname = `` i_varname = lv_hdl_obj
                  i_any_scope = abap_true ).
              ENDIF.

              " Resolve the type of the event source
              DATA(lv_src_class) = ``.
              IF lv_src_obj IS NOT INITIAL AND lv_src_obj <> '*'.
                lv_src_class = zcl_ace_parse_calls=>resolve_var_type(
                  is_source = cs_source i_program = i_program
                  i_evtype = `` i_evname = `` i_varname = lv_src_obj
                  i_any_scope = abap_true ).
              ENDIF.

              " Update the FOR EVENT entry, or append a new one
              READ TABLE cs_source-tt_handler_map
                WITH KEY hdl_method = lv_hdl_meth
                ASSIGNING FIELD-SYMBOL(<hm>).
              IF sy-subrc = 0.
                IF lv_hdl_class IS NOT INITIAL AND <hm>-hdl_class IS INITIAL.
                  <hm>-hdl_class = lv_hdl_class.
                ENDIF.
                IF lv_src_class IS NOT INITIAL AND <hm>-src_class IS INITIAL.
                  <hm>-src_class = lv_src_class.
                ENDIF.
              ELSE.
                APPEND VALUE zif_ace_parse_data=>ts_handler_map(
                  src_class  = lv_src_class
                  event_name = ``
                  hdl_class  = lv_hdl_class
                  hdl_method = lv_hdl_meth
                  include    = i_include
                  line       = ls_kw-row
                ) TO cs_source-tt_handler_map.
              ENDIF.
            ENDIF.
            lv_j += 1.
          ENDWHILE.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve_raise_event.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    " RAISE EVENT ev_name → token from+2
    READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO DATA(ls_ev).
    CHECK sy-subrc = 0.
    DATA(lv_ev_name) = ls_ev-str.

    LOOP AT cs_source-tt_handler_map INTO DATA(ls_hm)
      WHERE event_name = lv_ev_name.

      DATA(lv_class) = ls_hm-hdl_class.

      " If the class did not resolve, look it up in calls_line by method name
      IF lv_class IS INITIAL.
        LOOP AT cs_source-tt_calls_line INTO DATA(ls_cl)
          WHERE eventname = ls_hm-hdl_method AND eventtype = 'METHOD'.
          lv_class = ls_cl-class. EXIT.
        ENDLOOP.
      ENDIF.

      APPEND VALUE zif_ace_parse_data=>ts_calls(
        event = 'METHOD'
        class = lv_class
        name  = ls_hm-hdl_method
        type  = 'H'
      ) TO ct_calls.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.
