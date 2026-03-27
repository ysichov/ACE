CLASS zcl_ace_parse_calls DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

protected section.
private section.

  data MV_CLASS_NAME type STRING .
  data MV_EVENT_TYPE type STRING .
  data MV_EVENT_NAME type STRING .
  data MV_IN_IMPL type ABAP_BOOL .
  data MV_SUPER_CLS type STRING .
  data MV_SUPER type STRING .

  methods RESOLVE_VAR_TYPE
    importing
      !IS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_EVTYPE type STRING
      !I_EVNAME type STRING
      !I_VARNAME type STRING
    returning
      value(RV_TYPE) type STRING .
  methods GET_SUPER
    importing
      !IS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
    returning
      value(RV_SUPER) type STRING .
  methods PARSE_STMT_CALLS
    importing
      !IO_SCAN type ref to CL_CI_SCAN
      !I_STMT_IDX type I
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
    changing
      !CS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA .
    " Линейный проход: распознаёт obj->meth( / cls=>meth( / NEW cls( и собирает BINDINGS
  methods COLLECT_METHOD_CALLS
    importing
      !IO_SCAN type ref to CL_CI_SCAN
      !I_STMT type SSTMNT
      !I_PROGRAM type PROGRAM
    changing
      !CS_SOURCE type ZIF_ACE_PARSE_DATA=>TS_PARSE_DATA
      !CT_CALLS type ZCL_ACE=>TT_CALLS .
ENDCLASS.



CLASS ZCL_ACE_PARSE_CALLS IMPLEMENTATION.


  METHOD zif_ace_stmt_handler~handle.

    CHECK i_stmt_idx > 0.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw).
    CHECK sy-subrc = 0.

    mv_class_name = i_class.
    mv_event_name = i_ev_name.
    mv_event_type = i_evtype.

*    CASE ls_kw-str.
**      WHEN 'CLASS' OR 'INTERFACE'.
**        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_name).
**        IF sy-subrc = 0.
**          IF mv_class_name <> ls_name-str. CLEAR: mv_super_cls, mv_super. ENDIF.
**          mv_class_name = ls_name-str.
**          mv_in_impl    = abap_false.
**          IF ls_kw-str = 'CLASS'.
**            LOOP AT io_scan->tokens FROM ls_stmt-from TO ls_stmt-to INTO DATA(ls_t).
**              IF ls_t-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. RETURN. ENDIF.
**            ENDLOOP.
**          ENDIF.
**        ENDIF.
**        RETURN.
**      WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
**        CLEAR: mv_class_name, mv_in_impl, mv_event_type, mv_event_name, mv_super_cls, mv_super.
**        RETURN.
*      WHEN 'METHOD'.
*        mv_event_type = 'METHOD'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO data(ls_name).
*        IF sy-subrc = 0. mv_event_name = ls_name-str. ENDIF.
*        RETURN.
**      WHEN 'ENDMETHOD'.
**        CLEAR: mv_event_type, mv_event_name. RETURN.
**      WHEN 'FORM'.
**        mv_event_type = 'FORM'.
**        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
**        IF sy-subrc = 0. mv_event_name = ls_name-str. ENDIF.
**        RETURN.
**      WHEN 'ENDFORM'.
**        CLEAR: mv_event_type, mv_event_name. RETURN.
*      WHEN 'FUNCTION'.
*        mv_event_type = 'FUNCTION'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
*        IF sy-subrc = 0.
*          mv_event_name = ls_name-str.
*          REPLACE ALL OCCURRENCES OF '''' IN mv_event_name WITH ''.
*        ENDIF.
*        RETURN.
**      WHEN 'ENDFUNCTION'.
**        CLEAR: mv_event_type, mv_event_name. RETURN.
*      WHEN 'MODULE'.
*        mv_event_type = 'MODULE'.
*        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_name.
*        IF sy-subrc = 0. mv_event_name = ls_name-str. ENDIF.
*        RETURN.
**      WHEN 'ENDMODULE'.
**        CLEAR: mv_event_type, mv_event_name. RETURN.
**      WHEN 'START-OF-SELECTION' OR 'END-OF-SELECTION'
**        OR 'INITIALIZATION' OR 'TOP-OF-PAGE' OR 'END-OF-PAGE'
**        OR 'AT' OR 'GET'.
**        mv_event_type = 'EVENT'.
**        mv_event_name = ls_kw-str.
**        RETURN.
*    ENDCASE.
*
*    CHECK mv_event_type IS NOT INITIAL.

    parse_stmt_calls(
      EXPORTING io_scan    = io_scan
                i_stmt_idx = i_stmt_idx
                i_program  = i_program
                i_include  = i_include
      CHANGING  cs_source  = cs_source ).

  ENDMETHOD.


  METHOD get_super.
    CHECK mv_class_name IS NOT INITIAL.
    IF mv_super_cls = mv_class_name.
      rv_super = mv_super. RETURN.
    ENDIF.
    READ TABLE is_source-tt_class_defs WITH KEY class = mv_class_name INTO DATA(ls_cd).
    IF sy-subrc = 0 AND ls_cd-super IS NOT INITIAL.
      rv_super = ls_cd-super.
    ELSE.
      SELECT SINGLE refclsname FROM seometarel INTO @rv_super
        WHERE clsname = @mv_class_name AND reltype = '1'.
    ENDIF.
    mv_super_cls = mv_class_name.
    mv_super     = rv_super.
  ENDMETHOD.


  METHOD resolve_var_type.
    " t_vars is pre-sorted by (program, eventtype, eventname, name)
    " before this pass runs — so READ with BINARY SEARCH is O(log n).

    " 1. Local scope
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               eventtype = i_evtype
               eventname = i_evname
               name      = i_varname
      INTO DATA(ls_var).

    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
      RETURN.
    ENDIF.

    " 2. Class attributes (eventtype = '', eventname = '')
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               eventtype = ''
               eventname = ''
               name      = i_varname
      INTO ls_var.
    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
      RETURN.
    ENDIF.

    " 3. Globals (class = '', eventtype = '', eventname = '')
    READ TABLE is_source-t_vars
      WITH KEY program   = i_program
               class     = ''
               eventtype = ''
               eventname = ''
               name      = i_varname
      INTO ls_var.
    IF sy-subrc = 0 AND ls_var-type IS NOT INITIAL.
      rv_type = ls_var-type.
    ENDIF.
  ENDMETHOD.


  METHOD collect_method_calls.
    DATA lv_tstr     TYPE string.
    DATA lv_arrow    TYPE string.
    DATA lv_left     TYPE string.
    DATA lv_right    TYPE string.
    DATA lv_rpart    TYPE string.
    DATA lv_dummy    TYPE string.
    DATA ls_prev     LIKE LINE OF io_scan->tokens.
    DATA ls_next     LIKE LINE OF io_scan->tokens.
    DATA lv_c        TYPE zcl_ace=>ts_calls.
    DATA lv_rtype    TYPE string.
    DATA lt_bind     TYPE zcl_ace=>tt_param_bindings.
    DATA ls_b        TYPE zcl_ace=>ts_param_binding.
    DATA lv_single   TYPE string.
    DATA lv_pos      TYPE abap_bool.
    DATA lv_lhs      TYPE string.
    DATA lv_call_cls TYPE string.
    DATA lv_pref     TYPE string.
    DATA lv_ret      TYPE string.
    DATA lv_scan     TYPE i.
    DATA ls_sa       LIKE LINE OF io_scan->tokens.
    DATA ls_eq       LIKE LINE OF io_scan->tokens.
    DATA ls_val      LIKE LINE OF io_scan->tokens.
    DATA lv_sa_str   TYPE string.
    DATA lv_val_str  TYPE string.

    " Определяем, является ли statement assignment-ом вида: VAR = func(...)
    " В таком случае токен с вызовом находится на позиции from+2, а не from
    DATA(lv_is_assign) = abap_false.
    DATA(lv_assign_rhs_pos) = i_stmt-from.
    READ TABLE io_scan->tokens INDEX i_stmt-from + 1 INTO DATA(ls_eq_tok).
    IF sy-subrc = 0 AND ls_eq_tok-str = '='.
      lv_is_assign   = abap_true.
      lv_assign_rhs_pos = i_stmt-from + 2.
    ENDIF.

    DATA(lv_ti) = i_stmt-from.

    WHILE lv_ti <= i_stmt-to.
      READ TABLE io_scan->tokens INDEX lv_ti INTO DATA(ls_t).
      IF sy-subrc <> 0. EXIT. ENDIF.
      lv_tstr = ls_t-str.
      CLEAR: lv_arrow, lv_left, lv_right, lv_rpart, lv_dummy.

      " ── Распознаём токен вызова ───────────────────────────────────
      IF lv_tstr CS '=>' AND lv_tstr CS '('.
        lv_arrow = '=>'.
        SPLIT lv_tstr AT '=>' INTO lv_left lv_rpart.
        SPLIT lv_rpart AT '(' INTO lv_right lv_dummy.

      ELSEIF lv_tstr CS '->' AND lv_tstr CS '('.
        lv_arrow = '->'.
        SPLIT lv_tstr AT '->' INTO lv_left lv_rpart.
        SPLIT lv_rpart AT '(' INTO lv_right lv_dummy.
        " lv_left пустой или ')' — перед нами NEW cls( )->meth(
        IF lv_left IS INITIAL OR lv_left CO ')'.
          READ TABLE io_scan->tokens INDEX lv_ti - 1 INTO DATA(ls_m1).
          READ TABLE io_scan->tokens INDEX lv_ti - 2 INTO DATA(ls_m2).
          IF ls_m2-str = 'NEW' AND ls_m1-str CS '('.
            lv_left = ls_m1-str.
            REPLACE ALL OCCURRENCES OF '(' IN lv_left WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN lv_left WITH ''.
            CONDENSE lv_left NO-GAPS.
            lv_arrow = '=>'.
          ELSE.
            READ TABLE io_scan->tokens INDEX lv_ti - 3 INTO DATA(ls_m3).
            READ TABLE io_scan->tokens INDEX lv_ti - 4 INTO DATA(ls_m4).
            IF ls_m4-str = 'NEW' AND ls_m2-str = '('.
              lv_left  = ls_m3-str.
              lv_arrow = '=>'.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF lv_tstr = '=>' OR lv_tstr = '->'.
        lv_arrow = lv_tstr.
        READ TABLE io_scan->tokens INDEX lv_ti - 1 INTO ls_prev.
        READ TABLE io_scan->tokens INDEX lv_ti + 1 INTO ls_next.
        IF sy-subrc = 0 AND ls_next-str CS '(' AND NOT ls_next-str CO '()'.
          lv_left  = ls_prev-str.
          lv_right = ls_next-str.
          REPLACE ALL OCCURRENCES OF '(' IN lv_right WITH ''.
          lv_ti += 1.
          IF lv_tstr = '->' AND ( ls_prev-str IS INITIAL OR ls_prev-str CO ')' ).
            READ TABLE io_scan->tokens INDEX lv_ti - 2 INTO DATA(ls_nk1).
            READ TABLE io_scan->tokens INDEX lv_ti - 3 INTO DATA(ls_nk2).
            READ TABLE io_scan->tokens INDEX lv_ti - 4 INTO DATA(ls_nk3).
            IF ls_nk2-str = 'NEW' AND ls_nk1-str CS '('.
              lv_left = ls_nk1-str.
              REPLACE ALL OCCURRENCES OF '(' IN lv_left WITH ''.
              CONDENSE lv_left NO-GAPS.
              lv_arrow = '=>'.
            ELSEIF ls_nk3-str = 'NEW' AND ls_nk1-str = '('.
              lv_left  = ls_nk2-str.
              lv_arrow = '=>'.
            ENDIF.
          ENDIF.
        ELSE.
          lv_ti += 1. CONTINUE.
        ENDIF.

      ELSEIF lv_tstr = 'NEW'.
        READ TABLE io_scan->tokens INDEX lv_ti + 1 INTO ls_next.
        IF sy-subrc = 0 AND ls_next-str CS '('.
          lv_arrow = '=>'.
          lv_left  = ls_next-str.
          REPLACE ALL OCCURRENCES OF '(' IN lv_left WITH ''.
          CONDENSE lv_left NO-GAPS.
          lv_right = 'CONSTRUCTOR'.
          lv_ti += 1.
        ELSE.
          lv_ti += 1. CONTINUE.
        ENDIF.

      ELSEIF lv_tstr CA '(' AND NOT lv_tstr CS '->' AND NOT lv_tstr CS '=>'.
        " Допускаем вызов как первый токен statement (plain call),
        " ИЛИ на позиции from+2 если statement — assignment (var = func(...))
        IF ( lv_ti = i_stmt-from OR ( lv_is_assign = abap_true AND lv_ti = lv_assign_rhs_pos ) )
          AND NOT lv_tstr CP 'DATA(*'
          AND NOT lv_tstr CP 'FIELD-SYMBOL(*'
          AND NOT lv_tstr CP 'FINAL(*'
          AND NOT lv_tstr CP 'VALUE(*'
          AND NOT lv_tstr CP 'CONV(*'
          AND NOT lv_tstr CP 'REF(*'
          AND NOT lv_tstr CP 'CAST(*'
          AND NOT lv_tstr CP 'COND(*'
          AND NOT lv_tstr CP 'SWITCH(*'.
          lv_arrow = '->'.
          lv_left  = 'ME'.
          lv_right = lv_tstr.
          REPLACE ALL OCCURRENCES OF '(' IN lv_right WITH ''.
          CONDENSE lv_right NO-GAPS.
        ELSE.
          lv_ti += 1. CONTINUE.
        ENDIF.

      ELSE.
        lv_ti += 1. CONTINUE.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '(' IN lv_right WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN lv_right WITH ''.
      CONDENSE lv_right NO-GAPS.
      IF lv_right IS INITIAL. lv_ti += 1. CONTINUE. ENDIF.

      " ── Строим запись вызова ──────────────────────────────────────
      CLEAR lv_c.
      lv_c-event = 'METHOD'.
      lv_c-name  = lv_right.
      IF lv_left = 'ME'.
        lv_c-class = mv_class_name.
      ELSEIF lv_left = 'SUPER'.
        lv_c-super = abap_true.
        lv_c-class = COND #( WHEN mv_super IS NOT INITIAL THEN mv_super ELSE mv_class_name ).
      ELSE.
        lv_rtype = resolve_var_type(
          is_source = cs_source i_program = i_program i_include = i_program
          i_evtype  = lv_c-event i_evname = mv_event_name
          i_varname = lv_left ).
        IF lv_rtype IS NOT INITIAL.
          lv_c-class = lv_rtype.
          lv_c-outer = lv_left.
          lv_c-inner = lv_right.
        ELSEIF lv_arrow = '=>'.
          lv_c-class = lv_left.
        ELSE.
          lv_c-outer = lv_left.
          lv_c-inner = lv_right.
        ENDIF.
      ENDIF.

      " ── CONSTRUCTOR: записываем только если он реально определён ──
      IF lv_c-name = 'CONSTRUCTOR' AND lv_c-class IS NOT INITIAL.
        READ TABLE cs_source-tt_calls_line
          WITH KEY class     = lv_c-class
                   eventtype = 'METHOD'
                   eventname = 'CONSTRUCTOR'
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          lv_ti += 1. CONTINUE.
        ENDIF.
      ENDIF.

      lv_call_cls = COND #( WHEN lv_c-class IS NOT INITIAL THEN lv_c-class ELSE mv_class_name ).

      " ── LHS: lv_x = meth(…) → RETURNING ──────────────────────────
      CLEAR lv_lhs.
      DATA(lv_lhs_pos) = lv_ti - 1.
      IF lv_lhs_pos >= i_stmt-from.
        READ TABLE io_scan->tokens INDEX lv_lhs_pos INTO DATA(ls_leq).
        IF ls_leq-str = '='.
          READ TABLE io_scan->tokens INDEX lv_lhs_pos - 1 INTO DATA(ls_lvar).
          IF sy-subrc = 0.
            lv_lhs = ls_lvar-str.
            REPLACE ALL OCCURRENCES OF 'DATA(' IN lv_lhs WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN lv_lhs WITH ''.
            CONDENSE lv_lhs NO-GAPS.
          ENDIF.
        ENDIF.
      ENDIF.

      " ── Линейный сбор аргументов ──────────────────────────────────
      CLEAR: lt_bind, lv_single, lv_pos.
      lv_pos  = abap_true.
      lv_scan = lv_ti + 1.
      DATA lv_cur_sec TYPE string.
      CLEAR lv_cur_sec.
      WHILE lv_scan <= i_stmt-to.
        READ TABLE io_scan->tokens INDEX lv_scan INTO ls_sa.
        IF sy-subrc <> 0. EXIT. ENDIF.
        lv_sa_str = ls_sa-str.
        IF lv_sa_str = ')' OR lv_sa_str CO ')'. EXIT. ENDIF.
        IF lv_sa_str = '(' OR lv_sa_str = ','. lv_scan += 1. CONTINUE. ENDIF.
        IF lv_sa_str = 'EXPORTING' OR lv_sa_str = 'IMPORTING' OR
           lv_sa_str = 'CHANGING'  OR lv_sa_str = 'RECEIVING'.
          lv_cur_sec = lv_sa_str.
          lv_pos = abap_false.
          lv_scan += 1. CONTINUE.
        ENDIF.
        CLEAR ls_eq.
        READ TABLE io_scan->tokens INDEX lv_scan + 1 INTO ls_eq.
        IF ls_eq-str = '='.
          CLEAR ls_val.
          READ TABLE io_scan->tokens INDEX lv_scan + 2 INTO ls_val.
          IF sy-subrc = 0.
            lv_pos = abap_false.
            lv_val_str = ls_val-str.
            REPLACE ALL OCCURRENCES OF ')' IN lv_val_str WITH ''.
            CONDENSE lv_val_str NO-GAPS.
            DATA(lv_bind_dir) = SWITCH char1( lv_cur_sec
              WHEN 'EXPORTING'  THEN 'I'
              WHEN 'IMPORTING'  THEN 'E'
              WHEN 'RECEIVING'  THEN 'E'
              WHEN 'CHANGING'   THEN 'C'
              ELSE                   'I' ).
            CLEAR ls_b. ls_b-inner = lv_sa_str. ls_b-outer = lv_val_str.
            ls_b-dir = lv_bind_dir.
            APPEND ls_b TO lt_bind.
            lv_scan += 3. CONTINUE.
          ENDIF.
        ENDIF.
        IF lv_pos = abap_true AND lv_single IS INITIAL AND lv_sa_str IS NOT INITIAL.
          lv_single = lv_sa_str.
          REPLACE ALL OCCURRENCES OF ')' IN lv_single WITH ''.
          CONDENSE lv_single NO-GAPS.
        ENDIF.
        lv_scan += 1.
      ENDWHILE.

      IF lv_pos = abap_true AND lv_single IS NOT INITIAL.
        CLEAR lv_pref.
        LOOP AT cs_source-t_params INTO DATA(ls_pm)
          WHERE program = i_program
            AND include = i_program
            AND class = lv_call_cls
            AND event = 'METHOD'
            AND name = lv_c-name
            AND type  = 'I'.
          IF ls_pm-preferred = 'X' OR lv_pref IS INITIAL. lv_pref = ls_pm-param. ENDIF.
          IF ls_pm-preferred = 'X'. EXIT. ENDIF.
        ENDLOOP.
        CLEAR ls_b. ls_b-outer = lv_single. ls_b-inner = lv_pref.
        APPEND ls_b TO lt_bind.
      ENDIF.

      IF lv_lhs IS NOT INITIAL.
        CLEAR lv_ret.
        LOOP AT cs_source-t_params INTO DATA(ls_ret)
          WHERE class = lv_call_cls AND event = 'METHOD'
            AND name = lv_c-name   AND type  = 'R'.
          lv_ret = ls_ret-param. EXIT.
        ENDLOOP.
        CLEAR ls_b. ls_b-outer = lv_lhs. ls_b-inner = lv_ret.
        ls_b-dir = 'E'.
        APPEND ls_b TO lt_bind.
      ENDIF.

      lv_c-bindings = lt_bind.
      APPEND lv_c TO ct_calls.
      lv_ti += 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD parse_stmt_calls.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(ls_stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw_tok).
    CHECK sy-subrc = 0.

    DATA(lv_kw) = SWITCH string( ls_stmt-type
      WHEN 'C' THEN 'COMPUTE'
      WHEN 'D' THEN 'COMPUTE'
      WHEN 'A' THEN '+CALL_METHOD'
      ELSE          ls_kw_tok-str ).

    IF lv_kw = 'CALL'.
      READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok2).
      IF sy-subrc = 0. lv_kw = |CALL { ls_tok2-str }|. ENDIF.
    ENDIF.

    IF lv_kw = 'RAISE'.
      READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO ls_tok2.
      IF sy-subrc = 0 AND ls_tok2-str = 'EVENT'. lv_kw = 'RAISE EVENT'. ENDIF.
    ENDIF.

    IF lv_kw = 'NEW'. lv_kw = 'COMPUTE'. ENDIF.

    DATA(lv_super) = get_super( is_source = cs_source ).
    DATA lt_new_calls TYPE zcl_ace=>tt_calls.

    CASE lv_kw.

      " ── PERFORM ──────────────────────────────────────────────────
      WHEN 'PERFORM'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 1 INTO DATA(ls_tok).
        CHECK sy-subrc = 0.
        DATA ls_pf_call  TYPE zcl_ace=>ts_calls.
        DATA lv_pf_sec   TYPE string.
        DATA lv_pf_act_i TYPE i.
        DATA ls_pf_bind  TYPE zif_ace_parse_data=>ts_param_binding.
        ls_pf_call-event = 'FORM'.
        ls_pf_call-name  = ls_tok-str.
        DATA lt_pf_actuals   TYPE string_table.
        DATA lt_pf_act_dirs  TYPE TABLE OF char1 WITH EMPTY KEY.
        DATA lv_pf_cur_dir   TYPE char1 VALUE 'I'.
        DATA(lv_pf_i) = ls_stmt-from + 2.
        WHILE lv_pf_i <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_pf_i INTO DATA(ls_pf_t).
          IF sy-subrc <> 0. EXIT. ENDIF.
          CASE ls_pf_t-str.
            WHEN 'USING'.    lv_pf_cur_dir = 'I'.
            WHEN 'CHANGING'. lv_pf_cur_dir = 'C'.
            WHEN 'TABLES'.   lv_pf_cur_dir = 'C'.
            WHEN OTHERS.
              IF ls_pf_t-str IS NOT INITIAL.
                APPEND ls_pf_t-str TO lt_pf_actuals.
                APPEND lv_pf_cur_dir TO lt_pf_act_dirs.
              ENDIF.
          ENDCASE.
          lv_pf_i += 1.
        ENDWHILE.
        DATA lt_pf_params TYPE TABLE OF zcl_ace=>ts_params WITH EMPTY KEY.
        lt_pf_params = VALUE #( FOR p IN cs_source-t_params
          WHERE ( event = 'FORM' AND name = ls_pf_call-name ) ( p ) ).
        SORT lt_pf_params BY line.
        lv_pf_act_i = 1.
        LOOP AT lt_pf_params INTO DATA(ls_pf_p).
          READ TABLE lt_pf_actuals  INDEX lv_pf_act_i INTO DATA(lv_pf_act).
          READ TABLE lt_pf_act_dirs INDEX lv_pf_act_i INTO DATA(lv_pf_dir).
          CLEAR ls_pf_bind.
          ls_pf_bind-outer = lv_pf_act.
          ls_pf_bind-inner = ls_pf_p-param.
          ls_pf_bind-dir   = COND #( WHEN lv_pf_dir IS NOT INITIAL THEN lv_pf_dir ELSE 'I' ).
          APPEND ls_pf_bind TO ls_pf_call-bindings.
          lv_pf_act_i += 1.
        ENDLOOP.
        IF lt_pf_params IS INITIAL.
          lv_pf_act_i = 1.
          LOOP AT lt_pf_actuals INTO DATA(lv_pf_only).
            READ TABLE lt_pf_act_dirs INDEX lv_pf_act_i INTO DATA(lv_pf_only_dir).
            CLEAR ls_pf_bind.
            ls_pf_bind-outer = lv_pf_only.
            ls_pf_bind-dir   = COND #( WHEN lv_pf_only_dir IS NOT INITIAL THEN lv_pf_only_dir ELSE 'I' ).
            APPEND ls_pf_bind TO ls_pf_call-bindings.
            lv_pf_act_i += 1.
          ENDLOOP.
        ENDIF.
        APPEND ls_pf_call TO lt_new_calls.

      " ── CALL FUNCTION ────────────────────────────────────────────
      WHEN 'CALL FUNCTION'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0.
        DATA(lv_fname) = ls_tok-str.
        REPLACE ALL OCCURRENCES OF '''' IN lv_fname WITH ''.
        APPEND VALUE zcl_ace=>ts_calls( event = 'FUNCTION' name = lv_fname )
          TO lt_new_calls.

      " ── CALL METHOD ──────────────────────────────────────────────
      WHEN 'CALL METHOD'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0 AND ls_tok-str IS NOT INITIAL.
        DATA(lv_call) = VALUE zcl_ace=>ts_calls( event = 'METHOD' ).
        DATA(lv_str)  = ls_tok-str.
        IF lv_str CS '->'.
          SPLIT lv_str AT '->' INTO lv_call-class lv_call-name.
        ELSEIF lv_str CS '=>'.
          SPLIT lv_str AT '=>' INTO lv_call-class lv_call-name.
        ELSE.
          lv_call-name = lv_str.
        ENDIF.
        REPLACE ALL OCCURRENCES OF '(' IN lv_call-name WITH ''.
        CONDENSE lv_call-name NO-GAPS.
        IF lv_call-class = 'ME'.
          lv_call-class = mv_class_name.
        ELSEIF lv_call-class = 'SUPER'.
          lv_call-super = abap_true.
          lv_call-class = COND #( WHEN lv_super IS NOT INITIAL THEN lv_super ELSE mv_class_name ).
        ELSEIF lv_call-class IS NOT INITIAL.
          DATA(lv_resolved) = resolve_var_type(
            is_source = cs_source i_program = i_program i_include = i_program
            i_evtype = 'METHOD' i_evname = lv_call-name i_varname = lv_call-class ).
          IF lv_resolved IS NOT INITIAL.
            lv_call-outer = lv_call-class.
            lv_call-inner = lv_call-name.
            lv_call-class = lv_resolved.
          ENDIF.
        ENDIF.
        DATA(lv_section_cm) = ``.
        DATA(lv_tok_cm)     = ls_stmt-from + 3.
        WHILE lv_tok_cm <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_tok_cm INTO DATA(ls_t_cm).
          IF sy-subrc <> 0. EXIT. ENDIF.
          CASE ls_t_cm-str.
            WHEN 'EXPORTING' OR 'IMPORTING' OR 'CHANGING' OR 'RECEIVING'.
              lv_section_cm = ls_t_cm-str.
            WHEN '='. " skip
            WHEN OTHERS.
              IF lv_section_cm IS NOT INITIAL AND ls_t_cm-str IS NOT INITIAL.
                READ TABLE io_scan->tokens INDEX lv_tok_cm + 1 INTO DATA(ls_eq_cm).
                IF ls_eq_cm-str = '='.
                  READ TABLE io_scan->tokens INDEX lv_tok_cm + 2 INTO DATA(ls_var_cm).
                  IF sy-subrc = 0 AND ls_var_cm-str IS NOT INITIAL.
                    DATA(lv_cm_actual) = ls_var_cm-str.
                    REPLACE ALL OCCURRENCES OF ')' IN lv_cm_actual WITH ''.
                    CONDENSE lv_cm_actual NO-GAPS.
                    DATA(lv_cm_dir) = SWITCH char1( lv_section_cm
                      WHEN 'EXPORTING'  THEN 'I'
                      WHEN 'IMPORTING'  THEN 'E'
                      WHEN 'RECEIVING'  THEN 'E'
                      WHEN 'CHANGING'   THEN 'C'
                      ELSE                   'I' ).
                    APPEND VALUE zif_ace_parse_data=>ts_param_binding(
                      inner = ls_t_cm-str outer = lv_cm_actual dir = lv_cm_dir )
                      TO lv_call-bindings.
                    lv_tok_cm += 2.
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.
          lv_tok_cm += 1.
        ENDWHILE.
        APPEND lv_call TO lt_new_calls.

      " ── RAISE EVENT ──────────────────────────────────────────────
      WHEN 'RAISE EVENT'.
        READ TABLE io_scan->tokens INDEX ls_stmt-from + 2 INTO ls_tok.
        CHECK sy-subrc = 0.
        DATA(lv_ev_name) = ls_tok-str.
        LOOP AT cs_source-tt_handler_map INTO DATA(ls_hm)
          WHERE event_name = lv_ev_name.
          APPEND VALUE zcl_ace=>ts_calls(
            event = 'METHOD' class = ls_hm-hdl_class name = ls_hm-hdl_method type = 'H' )
            TO lt_new_calls.
        ENDLOOP.
        IF lt_new_calls IS INITIAL.
          APPEND VALUE zcl_ace=>ts_calls( event = 'EVENT' name = lv_ev_name class = mv_class_name )
            TO lt_new_calls.
        ENDIF.

      " ── COMPUTE / NEW ────────────────────────────────────────────
      WHEN 'COMPUTE'.
        DATA lv_ci TYPE i.
        DATA ls_ct LIKE LINE OF io_scan->tokens.
        DATA ls_cn LIKE LINE OF io_scan->tokens.
        DATA lv_cn TYPE string.
        lv_ci = ls_stmt-from.
        WHILE lv_ci <= ls_stmt-to.
          READ TABLE io_scan->tokens INDEX lv_ci INTO ls_ct.
          IF sy-subrc <> 0. EXIT. ENDIF.
          IF ls_ct-str = 'NEW'.
            READ TABLE io_scan->tokens INDEX lv_ci + 1 INTO ls_cn.
            IF sy-subrc = 0 AND ls_cn-str CS '('.
              lv_cn = ls_cn-str.
              REPLACE ALL OCCURRENCES OF '(' IN lv_cn WITH ''.
              CONDENSE lv_cn NO-GAPS.
              IF lv_cn IS NOT INITIAL.
                APPEND VALUE zcl_ace=>ts_calls(
                  event = 'METHOD' class = lv_cn name = 'CONSTRUCTOR' ) TO lt_new_calls.
              ENDIF.
              lv_ci += 1.
            ENDIF.
          ENDIF.
          lv_ci += 1.
        ENDWHILE.
        collect_method_calls(
          EXPORTING io_scan = io_scan i_stmt = ls_stmt i_program = i_program
          CHANGING  cs_source = cs_source ct_calls = lt_new_calls ).

      " ── +CALL_METHOD ─────────────────────────────────────────────
      WHEN '+CALL_METHOD'.
        collect_method_calls(
          EXPORTING io_scan = io_scan i_stmt = ls_stmt i_program = i_program
          CHANGING  cs_source = cs_source ct_calls = lt_new_calls ).

    ENDCASE.

    CHECK lt_new_calls IS NOT INITIAL.

    LOOP AT cs_source-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE include = i_include.
      READ TABLE <prog>-t_keywords WITH KEY index = i_stmt_idx ASSIGNING FIELD-SYMBOL(<kw>) BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT lt_new_calls INTO DATA(ls_nc).
          READ TABLE <kw>-tt_calls WITH KEY event = ls_nc-event
                                            name  = ls_nc-name
                                            class = ls_nc-class
            TRANSPORTING NO FIELDS BINARY SEARCH.
          IF sy-subrc <> 0.
            APPEND ls_nc TO <kw>-tt_calls.
          ENDIF.
        ENDLOOP.
      ENDIF.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
