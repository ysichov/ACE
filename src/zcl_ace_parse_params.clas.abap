CLASS zcl_ace_parse_params DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ace_stmt_handler.

protected section.
  PRIVATE SECTION.

    DATA mv_class_name TYPE string.
    DATA mv_in_impl    TYPE abap_bool.

    METHODS parse_methods_stmt
      IMPORTING
        !io_scan    TYPE REF TO cl_ci_scan
        !i_stmt_idx TYPE i
        !i_program  TYPE program
        !i_include  TYPE program
        !i_kw       TYPE string
      CHANGING
        !cs_source  TYPE zif_ace_parse_data=>ts_parse_data.

    "! Appends the parameter accumulated so far to CT_PARAMS.
    "! Does nothing unless both a name and a section are known — callers may
    "! invoke it unconditionally at every point a parameter can end.
    METHODS flush_param
      IMPORTING
        !i_program TYPE program
        !i_include TYPE program
        !i_ev_name TYPE string
        !i_section TYPE string
        !i_pname   TYPE string
        !i_is_form TYPE abap_bool
        !i_line    TYPE i
      CHANGING
        !ct_params TYPE zif_ace_parse_data=>tt_params.

ENDCLASS.



CLASS ZCL_ACE_PARSE_PARAMS IMPLEMENTATION.


  METHOD zif_ace_stmt_handler~handle.

    CHECK i_stmt_idx > 0.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.
    READ TABLE io_scan->tokens INDEX stmt-from INTO DATA(kw_tok).
    CHECK sy-subrc = 0.

     mv_class_name = i_class.
    CASE kw_tok-str.

*      WHEN 'CLASS' OR 'INTERFACE'.
*        READ TABLE io_scan->tokens INDEX stmt-from + 1 INTO DATA(name_tok).
*        IF sy-subrc = 0.
*          mv_class_name = name_tok-str.
*          mv_in_impl    = abap_false.
*          IF kw_tok-str = 'CLASS'.
*            LOOP AT io_scan->tokens FROM stmt-from TO stmt-to INTO DATA(tok).
*              IF tok-str = 'IMPLEMENTATION'. mv_in_impl = abap_true. RETURN. ENDIF.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.

*      WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
*        CLEAR: mv_class_name, mv_in_impl.

      WHEN 'METHODS' OR 'CLASS-METHODS'.
        "IF mv_in_impl = abap_false.
          parse_methods_stmt(
            EXPORTING io_scan    = io_scan
                      i_stmt_idx = i_stmt_idx
                      i_program  = i_program
                      i_include  = i_include
                      i_kw       = kw_tok-str
            CHANGING  cs_source  = cs_source ).
        "ENDIF.

      WHEN 'FORM'.
        parse_methods_stmt(
          EXPORTING io_scan    = io_scan
                    i_stmt_idx = i_stmt_idx
                    i_program  = i_program
                    i_include  = i_include
                    i_kw       = kw_tok-str
          CHANGING  cs_source  = cs_source ).

    ENDCASE.

  ENDMETHOD.


  METHOD parse_methods_stmt.

    DATA: lt_params LIKE cs_source-t_params.

    READ TABLE io_scan->statements INDEX i_stmt_idx INTO DATA(stmt).
    CHECK sy-subrc = 0.

    DATA(lv_ev_name)    = ``.
    DATA(lv_section)    = ``.
    DATA(lv_pname)      = ``.
    DATA(lv_ptype)      = ``.
    DATA(lv_ref)        = abap_false.
    DATA(lv_after_type) = abap_false.
    DATA(lv_is_form)    = xsdbool( i_kw = 'FORM' ).
    DATA(lv_last_row)   = 0.
    DATA(lv_preferred)  = ``.
    DATA(lv_skip_next)  = abap_false.

    DATA(lv_tok_idx) = stmt-from + 1.

    " FORM: method name is the second token
    IF lv_is_form = abap_true.
      READ TABLE io_scan->tokens INDEX lv_tok_idx INTO DATA(ftok).
      IF sy-subrc = 0.
        lv_ev_name  = ftok-str.
        lv_last_row = ftok-row.
        lv_tok_idx += 1.
      ENDIF.
    ENDIF.

    DATA: ls_param TYPE zcl_ace=>ts_params.

    WHILE lv_tok_idx <= stmt-to.
      READ TABLE io_scan->tokens INDEX lv_tok_idx INTO DATA(tok).
      IF sy-subrc <> 0. EXIT. ENDIF.
      lv_last_row = tok-row.
      DATA(lv_str) = tok-str.

      " --- Chain separator: METHODS meth1 ..., meth2 ...
      IF lv_str = ','.
        flush_param(
          EXPORTING i_program = i_program i_include = i_include
                    i_ev_name = lv_ev_name i_section = lv_section
                    i_pname   = lv_pname   i_is_form = lv_is_form
                    i_line    = tok-row
          CHANGING  ct_params = lt_params ).
        lv_tok_idx += 1.
        READ TABLE io_scan->tokens INDEX lv_tok_idx INTO tok.
        IF sy-subrc = 0. lv_ev_name = tok-str. ENDIF.
        CLEAR: lv_section, lv_pname, lv_ptype, lv_ref, lv_after_type.
        lv_tok_idx += 1.
        CONTINUE.
      ENDIF.

      CASE lv_str.

        WHEN 'IMPORTING' OR 'EXPORTING' OR 'CHANGING' OR 'RETURNING'
          OR 'USING' OR 'TABLES'.
          " Flush previous parameter before switching section
          flush_param(
            EXPORTING i_program = i_program i_include = i_include
                      i_ev_name = lv_ev_name i_section = lv_section
                      i_pname   = lv_pname   i_is_form = lv_is_form
                      i_line    = tok-row
            CHANGING  ct_params = lt_params ).
          lv_section = lv_str.
          CLEAR: lv_pname, lv_ptype, lv_ref, lv_after_type.

        WHEN 'RAISING' OR 'EXCEPTIONS'.
          " Flush previous parameter, then stop collecting params
          flush_param(
            EXPORTING i_program = i_program i_include = i_include
                      i_ev_name = lv_ev_name i_section = lv_section
                      i_pname   = lv_pname   i_is_form = lv_is_form
                      i_line    = tok-row
            CHANGING  ct_params = lt_params ).
          CLEAR: lv_section, lv_pname, lv_ptype, lv_ref, lv_after_type.

        WHEN 'TYPE' OR 'LIKE'.
          lv_after_type = abap_true.
          lv_ref        = abap_false.

        WHEN 'REF'.
          lv_ref = abap_true.

        WHEN 'TO' OR 'OPTIONAL' OR 'DEFAULT'
          OR 'ABSTRACT' OR 'FINAL' OR 'REDEFINITION'.
          " Skip method/parameter modifiers

        WHEN 'VALUE'.
          " Single VALUE keyword (default value marker) — skip

        WHEN 'PREFERRED'.
          " Flush current parameter before processing PREFERRED.
          " The CLEAR stays inside the IF: an unflushed name must survive.
          IF lv_pname IS NOT INITIAL AND lv_section IS NOT INITIAL.
            flush_param(
              EXPORTING i_program = i_program i_include = i_include
                        i_ev_name = lv_ev_name i_section = lv_section
                        i_pname   = lv_pname   i_is_form = lv_is_form
                        i_line    = tok-row
              CHANGING  ct_params = lt_params ).
            CLEAR: lv_pname, lv_ptype, lv_ref, lv_after_type.
          ENDIF.
          lv_skip_next = abap_true.

        WHEN 'PARAMETER'.
          IF lv_skip_next = abap_true.
            READ TABLE io_scan->tokens INDEX lv_tok_idx + 1 INTO DATA(ls_pref_tok).
            IF sy-subrc = 0.
              lv_preferred = ls_pref_tok-str.
              IF lv_preferred+0(1) = '!'. lv_preferred = lv_preferred+1. ENDIF.
            ENDIF.
            lv_skip_next = abap_false.
          ENDIF.

        WHEN OTHERS.
          IF lv_str+0(1) = '!'.
            " !PARAM — explicit parameter name, flush previous
            flush_param(
              EXPORTING i_program = i_program i_include = i_include
                        i_ev_name = lv_ev_name i_section = lv_section
                        i_pname   = lv_pname   i_is_form = lv_is_form
                        i_line    = tok-row
              CHANGING  ct_params = lt_params ).
            lv_pname = lv_str+1.
            CLEAR: lv_ptype, lv_ref, lv_after_type.

          ELSEIF lv_after_type = abap_true.
            " Type name token — store type, do not start new param
            IF lv_ref = abap_true.
              lv_ptype = |REF TO { lv_str }|.
            ELSE.
              lv_ptype = lv_str.
            ENDIF.
            lv_after_type = abap_false.
            lv_ref        = abap_false.

          ELSEIF lv_section IS NOT INITIAL AND lv_pname IS INITIAL.
            " First parameter in current section (no ! prefix)
            " Also handles VALUE(param) for RETURNING
            lv_pname = lv_str.
            IF lv_pname CP 'VALUE(*'.
              REPLACE FIRST OCCURRENCE OF 'VALUE(' IN lv_pname WITH ''.
              REPLACE ALL OCCURRENCES OF ')' IN lv_pname WITH ''.
              CONDENSE lv_pname NO-GAPS.
            ENDIF.
            CLEAR: lv_ptype, lv_ref, lv_after_type.

          ELSEIF lv_section IS NOT INITIAL AND lv_pname IS NOT INITIAL
             AND lv_after_type = abap_false.
            " Next param without ! in same section — flush previous, start new
            flush_param(
              EXPORTING i_program = i_program i_include = i_include
                        i_ev_name = lv_ev_name i_section = lv_section
                        i_pname   = lv_pname   i_is_form = lv_is_form
                        i_line    = tok-row
              CHANGING  ct_params = lt_params ).
            lv_pname = lv_str.
            IF lv_pname CP 'VALUE(*'.
              REPLACE FIRST OCCURRENCE OF 'VALUE(' IN lv_pname WITH ''.
              REPLACE ALL OCCURRENCES OF ')' IN lv_pname WITH ''.
              CONDENSE lv_pname NO-GAPS.
            ENDIF.
            CLEAR: lv_ptype, lv_ref, lv_after_type.

          ELSEIF lv_ev_name IS INITIAL AND lv_is_form = abap_false.
            " Method name in METHODS chain
            lv_ev_name = lv_str.

          ENDIF.

      ENDCASE.

      lv_tok_idx += 1.
    ENDWHILE.

    " Flush last parameter
    flush_param(
      EXPORTING i_program = i_program i_include = i_include
                i_ev_name = lv_ev_name i_section = lv_section
                i_pname   = lv_pname   i_is_form = lv_is_form
                i_line    = lv_last_row
      CHANGING  ct_params = lt_params ).

    " Mark PREFERRED PARAMETER after all params are collected
    IF lv_preferred IS NOT INITIAL.
      READ TABLE lt_params WITH KEY name = lv_ev_name ASSIGNING FIELD-SYMBOL(<fp>).
      IF sy-subrc = 0.
        <fp>-preferred = 'X'.
      ENDIF.
    ENDIF.

    LOOP AT lt_params INTO ls_param.
      INSERT ls_param INTO TABLE cs_source-t_params.
    ENDLOOP.

  ENDMETHOD.


  METHOD flush_param.

    CHECK i_pname IS NOT INITIAL AND i_section IS NOT INITIAL.

    INSERT VALUE #(
      program = i_program  include = i_include
      class   = mv_class_name
      event   = COND #( WHEN i_is_form = abap_true THEN 'FORM' ELSE 'METHOD' )
      name    = i_ev_name
      type    = SWITCH #( i_section
                  WHEN 'IMPORTING' OR 'USING' THEN 'I'
                  WHEN 'EXPORTING'             THEN 'E'
                  WHEN 'CHANGING'              THEN 'C'
                  WHEN 'RETURNING'             THEN 'R'
                  ELSE 'I' )
      param   = i_pname   line = i_line )
      INTO TABLE ct_params.

  ENDMETHOD.
ENDCLASS.
