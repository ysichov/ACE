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
  data MV_IN_IMPL type ABAP_BOOL .
  data:
    mt_meth_defs TYPE STANDARD TABLE OF ts_meth_def
                      WITH NON-UNIQUE KEY name .

  methods ON_CLASS_KW
    importing
      !IO_SCAN type ref to CL_CI_SCAN
      !I_STMT_IDX type I .
  methods ON_METHODS_SIG
    importing
      !IO_SCAN type ref to CL_CI_SCAN
      !I_STMT_IDX type I
      !I_INCLUDE type PROGRAM .
  methods ON_BLOCK_START
    importing
      !IO_SCAN type ref to CL_CI_SCAN
      !I_STMT_IDX type I
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_KW type STRING
    changing
      !CS_SOURCE type ZCL_ACE_WINDOW=>TS_SOURCE .
ENDCLASS.



CLASS ZCL_ACE_PARSE_CALLS_LINE IMPLEMENTATION.


  method ON_BLOCK_START.

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

  endmethod.


  method ON_CLASS_KW.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    IF sy-subrc = 0.
      mv_class_name = name_tok-str.
    ENDIF.
    DATA lv_from TYPE i.
    DATA lv_to   TYPE i.
    lv_from = stmt-from.
    lv_to   = stmt-to.
    mv_in_impl = abap_false.
    LOOP AT io_scan->tokens FROM lv_from TO lv_to INTO DATA(tok).
      IF tok-str = 'IMPLEMENTATION'.
        mv_in_impl = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  endmethod.


  method ON_METHODS_SIG.

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

  endmethod.


  method ZIF_ACE_STMT_HANDLER~HANDLE.

    CHECK i_stmt_idx > 0.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX stmt-from INTO DATA(kw_tok).
    CHECK sy-subrc = 0.
    DATA(lv_kw) = kw_tok-str.

    CASE lv_kw.
      WHEN 'CLASS' OR 'INTERFACE'.
        on_class_kw( io_scan = io_scan i_stmt_idx = i_stmt_idx ).

      WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
        CLEAR: mv_class_name, mv_in_impl.
        CLEAR mt_meth_defs.

      WHEN 'METHODS' OR 'CLASS-METHODS'.
        IF mv_in_impl = abap_false.
          on_methods_sig( io_scan    = io_scan
                          i_stmt_idx = i_stmt_idx
                          i_include  = i_include ).
        ENDIF.

      WHEN 'FORM' OR 'METHOD' OR 'MODULE' OR 'FUNCTION'.
        CALL METHOD on_block_start
          EXPORTING io_scan    = io_scan
                    i_stmt_idx = i_stmt_idx
                    i_program  = i_program
                    i_include  = i_include
                    i_kw       = lv_kw
          CHANGING  cs_source  = cs_source.
    ENDCASE.

  endmethod.
ENDCLASS.
