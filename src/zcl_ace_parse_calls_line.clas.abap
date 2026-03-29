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
      meth_type   TYPE i,
    END OF ts_meth_def .

  data MV_CLASS_NAME type STRING .
  data MV_SUPER_NAME type STRING .
  data MV_IN_IMPL    type ABAP_BOOL .
  data MV_IS_INTF    type ABAP_BOOL .
  data MV_METH_TYPE  type I .
  data mt_meth_defs  TYPE STANDARD TABLE OF ts_meth_def WITH NON-UNIQUE KEY name .

  methods GET_METH_TYPE importing !I_INCLUDE TYPE program returning value(RV_TYPE) TYPE i .
  methods ON_CLASS_KW importing !IO_SCAN TYPE REF TO cl_ci_scan !I_STMT_IDX TYPE i !I_KW TYPE string .
  methods ON_METHODS_SIG
    importing !IO_SCAN TYPE REF TO cl_ci_scan !I_STMT_IDX TYPE i
              !I_PROGRAM TYPE program !I_INCLUDE TYPE program
    changing  !CS_SOURCE TYPE zif_ace_parse_data=>ts_parse_data .
  methods ON_BLOCK_START
    importing !IO_SCAN TYPE REF TO cl_ci_scan !I_STMT_IDX TYPE i
              !I_PROGRAM TYPE program !I_INCLUDE TYPE program !I_KW TYPE string
    changing  !CS_SOURCE TYPE zif_ace_parse_data=>ts_parse_data .
ENDCLASS.



CLASS ZCL_ACE_PARSE_CALLS_LINE IMPLEMENTATION.


  METHOD get_meth_type.
    DATA(lv_s)   = condense( val = CONV string( i_include ) ).
    DATA(lv_len) = strlen( lv_s ).
    IF lv_len >= 2.
      DATA(lv_off)    = lv_len - 2.
      DATA(lv_suffix) = lv_s+lv_off(2).
      CASE lv_suffix.
        WHEN 'CU'. rv_type = 1.
        WHEN 'CO'. rv_type = 2.
        WHEN 'CI'. rv_type = 3.
        WHEN OTHERS. rv_type = mv_meth_type.
      ENDCASE.
    ELSE.
      rv_type = mv_meth_type.
    ENDIF.
  ENDMETHOD.


  METHOD on_class_kw.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    CHECK sy-subrc = 0.
    mv_class_name = name_tok-str.
    mv_in_impl    = abap_false.
    mv_meth_type  = 0.
    mv_is_intf    = COND #( WHEN i_kw = 'INTERFACE' THEN abap_true ELSE abap_false ).
    CLEAR mv_super_name.
    IF i_kw = 'CLASS'.
      DATA lv_prev TYPE string.
      LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(tok).
        IF tok-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. RETURN. ENDIF.
        IF lv_prev = 'FROM'. mv_super_name = tok-str. ENDIF.
        lv_prev = tok-str.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD on_methods_sig.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    CHECK sy-subrc = 0.
    DATA(lv_line)      = name_tok-row.
    DATA(lv_meth_type) = get_meth_type( i_include ).

    READ TABLE mt_meth_defs WITH KEY name = name_tok-str TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND VALUE ts_meth_def( name = name_tok-str def_include = i_include
                                def_line = lv_line   meth_type   = lv_meth_type )
        TO mt_meth_defs.
    ENDIF.

    READ TABLE cs_source-tt_calls_line
      WITH KEY class = mv_class_name eventtype = 'METHOD' eventname = name_tok-str
      TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.

    APPEND INITIAL LINE TO cs_source-tt_calls_line ASSIGNING FIELD-SYMBOL(<cl>).
    <cl>-program     = i_program.
    <cl>-include     = i_include.
    <cl>-class       = mv_class_name.
    <cl>-eventtype   = 'METHOD'.
    <cl>-eventname   = name_tok-str.
    <cl>-meth_type   = lv_meth_type.
    <cl>-is_intf     = mv_is_intf.
    <cl>-def_include = i_include.
    <cl>-def_line    = lv_line.
    <cl>-index       = i_stmt_idx.
    <cl>-def_ind     = i_stmt_idx.  " marks declaration-only until on_block_start updates index
  ENDMETHOD.


  METHOD on_block_start.
    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
    CHECK sy-subrc = 0.
    DATA(lv_evtype) = SWITCH string( i_kw
      WHEN 'FUNCTION' THEN 'FUNCTION' WHEN 'MODULE' THEN 'MODULE'
      WHEN 'FORM'     THEN 'FORM'     ELSE               'METHOD' ).

    IF mv_class_name IS NOT INITIAL.
      READ TABLE cs_source-tt_calls_line
        WITH KEY class = mv_class_name eventtype = lv_evtype eventname = name_tok-str
        ASSIGNING FIELD-SYMBOL(<ex>).
    ELSE.
      READ TABLE cs_source-tt_calls_line
        WITH KEY program = i_program eventtype = lv_evtype eventname = name_tok-str
        ASSIGNING <ex>.
    ENDIF.

    IF sy-subrc = 0.
      " Запись создана ON_METHODS_SIG из CU/CO/CI.
      " Обновляем include → реальный CM-инклуд.
      " index = i_stmt_idx (индекс statement, нужен для поиска в t_keywords).
      <ex>-include = i_include.
      <ex>-index   = i_stmt_idx.
    ELSE.
      " Нет предварительной записи — локальный класс, FORM, MODULE, FUNCTION
      APPEND INITIAL LINE TO cs_source-tt_calls_line ASSIGNING FIELD-SYMBOL(<cl>).
      <cl>-program   = i_program.
      <cl>-include   = i_include.
      <cl>-class     = mv_class_name.
      <cl>-eventtype = lv_evtype.
      <cl>-eventname = name_tok-str.
      <cl>-is_intf   = mv_is_intf.
      <cl>-def_line  = i_stmt_idx.
      IF i_kw = 'METHOD'.
        READ TABLE mt_meth_defs WITH KEY name = name_tok-str INTO DATA(ls_def).
        IF sy-subrc = 0.
          <cl>-def_include = ls_def-def_include.
          <cl>-index       = ls_def-def_line.
          <cl>-meth_type   = ls_def-meth_type.
        ELSE.
          " локальный класс без предварительного объявления METHODS —
          " используем i_stmt_idx (индекс statement), НЕ row токена
          <cl>-def_include = i_include.
          <cl>-index       = i_stmt_idx.
          <cl>-meth_type   = get_meth_type( i_include ).
        ENDIF.
      ELSE.
        <cl>-index = i_stmt_idx.
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

    IF i_class IS NOT INITIAL.
      mv_class_name = i_class.
      CLEAR mv_is_intf.
    ELSEIF i_interface IS NOT INITIAL.
      mv_class_name = i_interface.
      mv_is_intf = abap_true.
    ENDIF.

    CASE lv_kw.
      WHEN 'CLASS' OR 'INTERFACE'.

        IF lv_kw = 'CLASS'.
          LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(ls_t).
            IF ls_t-str = 'DEFERRED'. RETURN. ENDIF.
          ENDLOOP.
        ENDIF.

        on_class_kw( io_scan = io_scan i_stmt_idx = i_stmt_idx i_kw = lv_kw ).

        IF mv_class_name IS NOT INITIAL AND mv_in_impl = abap_false.
          DATA(lv_def_line) = COND i( WHEN stmt IS NOT INITIAL THEN io_scan->tokens[ stmt-from ]-row ELSE 0 ).
          READ TABLE cs_source-tt_class_defs WITH KEY class = mv_class_name ASSIGNING FIELD-SYMBOL(<cd>).
          IF sy-subrc = 0.
            <cd>-super       = mv_super_name.
            <cd>-is_intf     = mv_is_intf.
            <cd>-program     = i_program.
            <cd>-def_include = i_include.
            <cd>-def_line    = lv_def_line.
          ELSE.
            APPEND VALUE zif_ace_parse_data=>ts_class_def(
              class       = mv_class_name
              super       = mv_super_name
              is_intf     = mv_is_intf
              program     = i_program
              def_include = i_include
              def_line    = lv_def_line )
              TO cs_source-tt_class_defs.
          ENDIF.
        ENDIF.

        IF mv_in_impl = abap_true AND mv_class_name IS NOT INITIAL.
          DATA(lv_impl_line) = COND i( WHEN stmt IS NOT INITIAL THEN io_scan->tokens[ stmt-from ]-row ELSE 0 ).
          READ TABLE cs_source-tt_class_defs WITH KEY class = mv_class_name ASSIGNING <cd>.
          IF sy-subrc = 0.
            <cd>-impl_include = i_include.
            <cd>-impl_line    = lv_impl_line.
          ELSE.
            APPEND VALUE zif_ace_parse_data=>ts_class_def(
              class        = mv_class_name
              program      = i_program
              impl_include = i_include
              impl_line    = lv_impl_line )
              TO cs_source-tt_class_defs.
          ENDIF.
        ENDIF.

      WHEN 'PUBLIC' OR 'PROTECTED' OR 'PRIVATE'.
        IF mv_in_impl = abap_false AND mv_class_name IS NOT INITIAL.
          CASE lv_kw.
            WHEN 'PUBLIC'.    mv_meth_type = 1.
            WHEN 'PROTECTED'. mv_meth_type = 2.
            WHEN 'PRIVATE'.   mv_meth_type = 3.
          ENDCASE.
          DATA(lv_sec_line) = COND i( WHEN stmt IS NOT INITIAL THEN io_scan->tokens[ stmt-from ]-row ELSE 0 ).
          READ TABLE cs_source-tt_sections
            WITH KEY class = mv_class_name section = lv_kw
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND VALUE zif_ace_parse_data=>ts_section(
              class   = mv_class_name
              section = lv_kw
              include = i_include
              line    = lv_sec_line )
              TO cs_source-tt_sections.
          ENDIF.
        ENDIF.

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
