CLASS zcl_ace_metrics DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    "--- result per code unit (method / form / module / program-level) ---
    TYPES:
      BEGIN OF ts_unit_result,
        program         TYPE program,
        include         TYPE program,
        unit_type       TYPE string,   " METHOD / FORM / MODULE / FUNCTION / PROGRAM
        unit_name       TYPE string,
        " McCabe cyclomatic complexity
        cyclomatic      TYPE i,
        " Halstead raw counts
        n1              TYPE i,        " total operators
        n2              TYPE i,        " total operands
        big_n1          TYPE i,        " distinct operators
        big_n2          TYPE i,        " distinct operands
        " Halstead derived
        vocabulary      TYPE i,        " η = η1 + η2
        prog_length     TYPE i,        " N = N1 + N2
        volume          TYPE f,        " V = N * log2(η)
        difficulty      TYPE f,        " D = (η1/2) * (N2/η2)
        effort          TYPE f,        " E = D * V
        " Lines of code
        loc             TYPE i,        " total lines in unit
        lloc            TYPE i,        " logical LOC (statements)
        cloc            TYPE i,        " comment lines
      END OF ts_unit_result.
    TYPES:
      tt_unit_results TYPE STANDARD TABLE OF ts_unit_result WITH EMPTY KEY.

    "--- aggregate result for whole program ---
    TYPES:
      BEGIN OF ts_result,
        program              TYPE program,
        units                TYPE tt_unit_results,
        total_cyclomatic     TYPE i,
        total_volume         TYPE f,
        total_effort         TYPE f,
        total_loc            TYPE i,
        total_lloc           TYPE i,
        total_cloc           TYPE i,
        avg_cyclomatic       TYPE f,
      END OF ts_result.

    CLASS-METHODS calculate
      IMPORTING
        is_parse_data TYPE zif_ace_parse_data=>ts_parse_data
        i_program     TYPE program
      RETURNING
        VALUE(rs_result) TYPE ts_result.

  PRIVATE SECTION.

    CLASS-METHODS is_branch_keyword
      IMPORTING i_kw      TYPE string
      RETURNING VALUE(rv) TYPE abap_bool.

    CLASS-METHODS log2
      IMPORTING i_val      TYPE f
      RETURNING VALUE(rv)  TYPE f.

ENDCLASS.


CLASS zcl_ace_metrics IMPLEMENTATION.

  METHOD calculate.

    rs_result-program = i_program.

    LOOP AT is_parse_data-tt_progs ASSIGNING FIELD-SYMBOL(<prog>)
      WHERE program = i_program.

      DATA(lo_scan) = <prog>-scan.
      CHECK lo_scan IS BOUND.
      CHECK lo_scan->statements IS NOT INITIAL.

      " ---- build ABAP keyword set (= operator vocabulary) ----
      DATA lt_ops TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
      CLEAR lt_ops.
      LOOP AT lo_scan->statements INTO DATA(ls_s_op).
        CHECK ls_s_op-type <> 'P'.
        READ TABLE lo_scan->tokens INDEX ls_s_op-from INTO DATA(ls_t_op).
        CHECK sy-subrc = 0.
        INSERT ls_t_op-str INTO TABLE lt_ops.
      ENDLOOP.

      " ---- collect unit boundaries from tt_calls_line ----
      " Sorted by stmt_from — guarantees ascending order and unique start.
      " Each ENDMETHOD must also be unique (1 method → 1 ENDMETHOD).
      TYPES: BEGIN OF ts_boundary,
               stmt_from TYPE i,
               stmt_to   TYPE i,
               unit_type TYPE string,
               unit_name TYPE string,
               class     TYPE string,
             END OF ts_boundary.
      DATA lt_boundaries TYPE SORTED TABLE OF ts_boundary
        WITH UNIQUE KEY stmt_from.
      CLEAR lt_boundaries.

      " track which stmt_to (ENDMETHOD index) are already claimed
      DATA lt_claimed_ends TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
      CLEAR lt_claimed_ends.

      LOOP AT is_parse_data-tt_calls_line INTO DATA(ls_cl)
        WHERE include  = <prog>-include
          AND index    > 0
          AND ( eventtype = 'METHOD'   OR eventtype = 'FORM'
             OR eventtype = 'MODULE'   OR eventtype = 'FUNCTION' ).

        " skip duplicate start positions
        READ TABLE lt_boundaries WITH KEY stmt_from = ls_cl-index
          TRANSPORTING NO FIELDS.
        CHECK sy-subrc <> 0.

        DATA(lv_end_kw) = SWITCH string( ls_cl-eventtype
          WHEN 'METHOD'   THEN 'ENDMETHOD'
          WHEN 'FORM'     THEN 'ENDFORM'
          WHEN 'MODULE'   THEN 'ENDMODULE'
          WHEN 'FUNCTION' THEN 'ENDFUNCTION'
          ELSE                 '' ).

        " find first unclaimed END* after stmt_from
        DATA lv_stmt_to TYPE i.
        lv_stmt_to = 0.
        LOOP AT <prog>-t_keywords INTO DATA(ls_kw)
          WHERE index > ls_cl-index
            AND name  = lv_end_kw.
          " skip if this END is already claimed by a previous boundary
          READ TABLE lt_claimed_ends WITH TABLE KEY table_line = ls_kw-index
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            lv_stmt_to = ls_kw-index.
            EXIT.
          ENDIF.
        ENDLOOP.
        CHECK lv_stmt_to > 0.

        INSERT lv_stmt_to INTO TABLE lt_claimed_ends.
        INSERT VALUE ts_boundary(
          stmt_from = ls_cl-index
          stmt_to   = lv_stmt_to
          unit_type = ls_cl-eventtype
          unit_name = ls_cl-eventname
          class     = ls_cl-class
        ) INTO TABLE lt_boundaries.

      ENDLOOP.

      CHECK lt_boundaries IS NOT INITIAL.

      " ---- metric calculation per unit ----
      LOOP AT lt_boundaries INTO DATA(ls_b).

        DATA ls_unit TYPE ts_unit_result.
        CLEAR ls_unit.
        ls_unit-program   = <prog>-program.
        ls_unit-include   = <prog>-include.
        ls_unit-unit_type = ls_b-unit_type.
        ls_unit-unit_name = COND #(
          WHEN ls_b-class IS NOT INITIAL
          THEN |{ ls_b-class }=>{ ls_b-unit_name }|
          ELSE ls_b-unit_name ).
        ls_unit-cyclomatic = 1.

        DATA lt_dist_ops TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
        DATA lt_dist_opd TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
        CLEAR: lt_dist_ops, lt_dist_opd.

        DATA lv_first_row TYPE i.
        DATA lv_last_row  TYPE i.
        lv_first_row = 0. lv_last_row = 0.

        READ TABLE lo_scan->statements INDEX ls_b-stmt_from INTO DATA(ls_sf).
        IF sy-subrc = 0.
          READ TABLE lo_scan->tokens INDEX ls_sf-from INTO DATA(ls_tf).
          IF sy-subrc = 0. lv_first_row = ls_tf-row. ENDIF.
        ENDIF.
        READ TABLE lo_scan->statements INDEX ls_b-stmt_to INTO DATA(ls_sl).
        IF sy-subrc = 0.
          READ TABLE lo_scan->tokens INDEX ls_sl-to INTO DATA(ls_tl).
          IF sy-subrc = 0. lv_last_row = ls_tl-row. ENDIF.
        ENDIF.
        IF lv_last_row >= lv_first_row AND lv_first_row > 0.
          ls_unit-loc = lv_last_row - lv_first_row + 1.
        ENDIF.

        DATA lv_si TYPE i.
        lv_si = ls_b-stmt_from.
        WHILE lv_si <= ls_b-stmt_to.
          READ TABLE lo_scan->statements INDEX lv_si INTO DATA(ls_stmt).
          IF sy-subrc <> 0. EXIT. ENDIF.

          IF ls_stmt-type = 'P'.
            ADD 1 TO ls_unit-cloc.
          ELSE.
            ADD 1 TO ls_unit-lloc.

            READ TABLE lo_scan->tokens INDEX ls_stmt-from INTO DATA(ls_kw_tok).
            IF sy-subrc = 0 AND is_branch_keyword( ls_kw_tok-str ) = abap_true.
              ADD 1 TO ls_unit-cyclomatic.
            ENDIF.

            DATA lv_ti TYPE i.
            lv_ti = ls_stmt-from.
            WHILE lv_ti <= ls_stmt-to.
              READ TABLE lo_scan->tokens INDEX lv_ti INTO DATA(ls_tok).
              IF sy-subrc <> 0. EXIT. ENDIF.
              IF ls_tok-str IS NOT INITIAL.
                READ TABLE lt_ops WITH TABLE KEY table_line = ls_tok-str
                  TRANSPORTING NO FIELDS.
                IF sy-subrc = 0.
                  ADD 1 TO ls_unit-n1.
                  INSERT ls_tok-str INTO TABLE lt_dist_ops.
                ELSE.
                  CASE ls_tok-str.
                    WHEN '+' OR '-' OR '*' OR '/' OR '**' OR '&&'
                      OR '=' OR '<>' OR '<' OR '>' OR '<=' OR '>='
                      OR '(' OR ')' OR ',' OR ':' OR '.' OR '->' OR '=>'.
                      ADD 1 TO ls_unit-n1.
                      INSERT ls_tok-str INTO TABLE lt_dist_ops.
                    WHEN OTHERS.
                      ADD 1 TO ls_unit-n2.
                      INSERT ls_tok-str INTO TABLE lt_dist_opd.
                  ENDCASE.
                ENDIF.
              ENDIF.
              ADD 1 TO lv_ti.
            ENDWHILE.
          ENDIF.
          ADD 1 TO lv_si.
        ENDWHILE.

        ls_unit-big_n1      = lines( lt_dist_ops ).
        ls_unit-big_n2      = lines( lt_dist_opd ).
        ls_unit-vocabulary  = ls_unit-big_n1 + ls_unit-big_n2.
        ls_unit-prog_length = ls_unit-n1 + ls_unit-n2.

        IF ls_unit-vocabulary > 0 AND ls_unit-prog_length > 0.
          DATA(lv_voc_f) = CONV f( ls_unit-vocabulary ).
          DATA(lv_len_f) = CONV f( ls_unit-prog_length ).
          ls_unit-volume = lv_len_f * log2( lv_voc_f ).
          IF ls_unit-big_n2 > 0.
            ls_unit-difficulty =
              ( CONV f( ls_unit-big_n1 ) / 2 )
              * ( CONV f( ls_unit-n2 ) / CONV f( ls_unit-big_n2 ) ).
          ENDIF.
          ls_unit-effort = ls_unit-difficulty * ls_unit-volume.
        ENDIF.

        APPEND ls_unit TO rs_result-units.

      ENDLOOP.

    ENDLOOP.

    " ---- aggregate totals ----
    DATA lv_cnt TYPE i.
    LOOP AT rs_result-units INTO DATA(ls_u).
      ADD ls_u-cyclomatic TO rs_result-total_cyclomatic.
      rs_result-total_volume = rs_result-total_volume + ls_u-volume.
      rs_result-total_effort = rs_result-total_effort + ls_u-effort.
      ADD ls_u-loc  TO rs_result-total_loc.
      ADD ls_u-lloc TO rs_result-total_lloc.
      ADD ls_u-cloc TO rs_result-total_cloc.
      ADD 1 TO lv_cnt.
    ENDLOOP.
    IF lv_cnt > 0.
      rs_result-avg_cyclomatic =
        CONV f( rs_result-total_cyclomatic ) / CONV f( lv_cnt ).
    ENDIF.

  ENDMETHOD.


  METHOD is_branch_keyword.
    CASE i_kw.
      WHEN 'IF' OR 'ELSEIF' OR 'WHEN' OR 'CATCH'
        OR 'LOOP' OR 'WHILE' OR 'DO'
        OR 'CHECK' OR 'AT' OR 'ON'.
        rv = abap_true.
      WHEN OTHERS.
        rv = abap_false.
    ENDCASE.
  ENDMETHOD.


  METHOD log2.
    IF i_val <= 0. RETURN. ENDIF.
    rv = log( i_val ) / log( CONV f( 2 ) ).
  ENDMETHOD.

ENDCLASS.
