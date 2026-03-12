class ZCL_ACE_PARSE_CALLS_LINE definition
  public
  create public .

public section.

  interfaces ZIF_ACE_STMT_HANDLER .
protected section.
private section.

  types:
    BEGIN OF ts_meth_def,
             name        TYPE string,
             def_include TYPE program,
             def_line    TYPE i,
           END OF ts_meth_def .

  data MV_CLASS_NAME type STRING .
  data MV_IN_IMPL    type ABAP_BOOL .
  data MV_IS_INTF    type ABAP_BOOL .
  data:
    mt_meth_defs TYPE STANDARD TABLE OF ts_meth_def
                      WITH NON-UNIQUE KEY name .

  methods ON_CLASS_KW
    importing
      !IO_SCAN    TYPE REF TO cl_ci_scan
      !I_STMT_IDX TYPE i
      !I_KW       TYPE string .
  methods ON_METHODS_SIG
    importing
      !IO_SCAN    TYPE REF TO cl_ci_scan
      !I_STMT_IDX TYPE i
      !I_PROGRAM  TYPE program
      !I_INCLUDE  TYPE program
    changing
      !CS_SOURCE  TYPE zcl_ace_window=>ts_source .
  methods ON_BLOCK_START
    importing
      !IO_SCAN    TYPE REF TO cl_ci_scan
      !I_STMT_IDX TYPE i
      !I_PROGRAM  TYPE program
      !I_INCLUDE  TYPE program
      !I_KW       TYPE string
    changing
      !CS_SOURCE  TYPE zcl_ace_window=>ts_source .
ENDCLASS.



CLASS ZCL_ACE_PARSE_CALLS_LINE IMPLEMENTATION.


  METHOD on_class_kw.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    CHECK sy-subrc = 0.

    mv_class_name = name_tok-str.
    mv_in_impl    = abap_false.
    mv_is_intf    = COND #( WHEN i_kw = 'INTERFACE' THEN abap_true ELSE abap_false ).

    IF i_kw = 'CLASS'.
      LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(tok).
        IF tok-str = 'IMPLEMENTATION'.
          mv_in_impl = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD on_methods_sig.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    CHECK sy-subrc = 0.
    DATA(lv_line) = io_scan->tokens[ stmt-from ]-row.

    READ TABLE mt_meth_defs WITH KEY name = name_tok-str TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND VALUE ts_meth_def(
        name        = name_tok-str
        def_include = i_include
        def_line    = lv_line
      ) TO mt_meth_defs.
    ENDIF.

    " Если внутри INTERFACE — сразу пишем в tt_calls_line с is_intf=true
    " (у интерфейсов нет METHOD...ENDMETHOD, только METHODS в сигнатуре)
    CHECK mv_is_intf = abap_true.

    READ TABLE cs_source-tt_calls_line
      WITH KEY class     = mv_class_name
               eventtype = 'METHOD'
               eventname = name_tok-str
      TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.

    APPEND INITIAL LINE TO cs_source-tt_calls_line
      ASSIGNING FIELD-SYMBOL(<cl>).
    <cl>-program     = i_program.
    <cl>-include     = i_include.
    <cl>-class       = mv_class_name.
    <cl>-eventtype   = 'METHOD'.
    <cl>-eventname   = name_tok-str.
    <cl>-is_intf     = abap_true.
    <cl>-def_include = i_include.
    <cl>-def_line    = lv_line.
    <cl>-index       = i_stmt_idx.

  ENDMETHOD.


  METHOD on_block_start.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    CHECK sy-subrc = 0.

    DATA(lv_start_line) = io_scan->tokens[ stmt-from ]-row.

    DATA(lv_evtype) = SWITCH string( i_kw
      WHEN 'FUNCTION' THEN 'FUNCTION'
      WHEN 'MODULE'   THEN 'MODULE'
      WHEN 'FORM'     THEN 'FORM'
      ELSE                 'METHOD' ).

    IF mv_class_name IS NOT INITIAL.
      READ TABLE cs_source-tt_calls_line
        WITH KEY class     = mv_class_name
                 eventtype = lv_evtype
                 eventname = name_tok-str
        TRANSPORTING NO FIELDS.
    ELSE.
      READ TABLE cs_source-tt_calls_line
        WITH KEY program   = i_program
                 eventtype = lv_evtype
                 eventname = name_tok-str
        TRANSPORTING NO FIELDS.
    ENDIF.
    CHECK sy-subrc <> 0.

    APPEND INITIAL LINE TO cs_source-tt_calls_line
      ASSIGNING FIELD-SYMBOL(<cl>).
    <cl>-program   = i_program.
    <cl>-include   = i_include.
    <cl>-class     = mv_class_name.
    <cl>-eventtype = lv_evtype.
    <cl>-eventname = name_tok-str.
    <cl>-is_intf   = mv_is_intf.
    <cl>-index     = i_stmt_idx.

    IF i_kw = 'METHOD'.
      READ TABLE mt_meth_defs WITH KEY name = name_tok-str INTO DATA(ls_def).
      IF sy-subrc = 0.
        <cl>-def_include = ls_def-def_include.
        <cl>-def_line    = ls_def-def_line.
      ELSE.
        <cl>-def_include = i_include.
        <cl>-def_line    = lv_start_line.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ace_stmt_handler~handle.

    CHECK i_stmt_idx > 0.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX stmt-from INTO DATA(kw_tok).
    CHECK sy-subrc = 0.
    DATA(lv_kw) = kw_tok-str.

    CASE lv_kw.
      WHEN 'CLASS' OR 'INTERFACE'.
        on_class_kw( io_scan    = io_scan
                     i_stmt_idx = i_stmt_idx
                     i_kw       = lv_kw ).

      WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
        CLEAR: mv_class_name, mv_in_impl, mv_is_intf.
        CLEAR mt_meth_defs.

      WHEN 'METHODS' OR 'CLASS-METHODS'.
        IF mv_in_impl = abap_false.
          on_methods_sig( EXPORTING io_scan    = io_scan
                                    i_stmt_idx = i_stmt_idx
                                    i_program  = i_program
                                    i_include  = i_include
                          CHANGING  cs_source  = cs_source ).
        ENDIF.

      WHEN 'FORM' OR 'METHOD' OR 'MODULE' OR 'FUNCTION'.
        on_block_start( EXPORTING io_scan    = io_scan
                                  i_stmt_idx = i_stmt_idx
                                  i_program  = i_program
                                  i_include  = i_include
                                  i_kw       = lv_kw
                        CHANGING  cs_source  = cs_source ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
