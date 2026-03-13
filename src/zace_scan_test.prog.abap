REPORT zace_calls_test_data.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS cls_meth.
     METHODS constructor .
    METHODS run IMPORTING iv_i TYPE i.
  PRIVATE SECTION.
    DATA mo_ref TYPE REF TO lcl_demo.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD cls_meth.
  ENDMETHOD.

  method constructor.
  ENDMETHOD.

  METHOD run.
    "DATA lo TYPE REF TO lcl_demo.
    mo_ref = NEW lcl_demo( ).
    "lo->run( iv_i = 1 ).
    me->run( iv_i = 2 ).
    lcl_demo=>cls_meth( ).
    PERFORM my_form USING 42.
    CALL FUNCTION 'POPUP_TO_CONFIRM'.
    "CALL METHOD lo->run( iv_i = 3 ).
  ENDMETHOD.
ENDCLASS.

FORM my_form USING p_val TYPE i.
  DATA a TYPE i.
  a = p_val.
ENDFORM.

START-OF-SELECTION.
  lcl_demo=>cls_meth( ).
  PERFORM my_form USING 1.

  DATA ls_source TYPE zcl_ace_window=>ts_source.
  zcl_ace_parser=>parse_tokens(
    EXPORTING i_program = 'ZACE_SCAN_TEST' i_include = 'ZACE_SCAN_TEST'
    CHANGING  cs_source = ls_source ).

  " t_vars dump
  DATA lv_vars TYPE string.
  LOOP AT ls_source-t_vars INTO DATA(lv).
    lv_vars = lv_vars && |[{ lv-name } evtype={ lv-eventtype } evname={ lv-eventname } type={ lv-type }]|.
  ENDLOOP.
  IF lv_vars IS INITIAL. lv_vars = '(none)'. ENDIF.
  MESSAGE |VARS: { lv_vars }| TYPE 'I'.

  " calls dump
  DATA lv_calls TYPE string.
  LOOP AT ls_source-tt_progs INTO DATA(lp).
    LOOP AT lp-t_keywords INTO DATA(lk).
      LOOP AT lk-tt_calls INTO DATA(lc).
        lv_calls = lv_calls && |[L{ lk-line } { lc-event } { lc-class }=>{ lc-name }]|.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
  IF lv_calls IS INITIAL. lv_calls = '(none)'. ENDIF.
  MESSAGE |CALLS: { lv_calls }| TYPE 'I'.
