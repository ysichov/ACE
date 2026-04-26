"! ABAP statement grammars - port of @abaplint/core 2_statements/statements/*.ts
"! One CLASS-METHOD per statement, name = STMT_<statement_name>.
"! Each method returns its runnable tree (zcl_ace_combi_node) - the same data
"! that abaplint's getMatcher() returns.
"! Discovered via RTTI by zcl_ace_keywords.
"!
"! Initial seed = the most common statements. Extend by porting more files from
"! abaplint/packages/core/src/abap/2_statements/statements/*.ts using the same
"! 1:1 mapping (str->str, tok->tok, seq->seq, alt->alt, opt->opt, star->star, ...).
CLASS zcl_ace_stmts DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    " Control flow
    CLASS-METHODS stmt_if           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_elseif       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_else         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endif        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_case         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_when         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_when_others  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endcase      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_do           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_enddo        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_while        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endwhile     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_loop         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endloop      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_continue     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_exit         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_check        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_try          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endtry       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_catch        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_cleanup      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_raise        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Data
    CLASS-METHODS stmt_data         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_data_begin   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_data_end     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_constant     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_field_symbol RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_types        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_clear        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_move         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_compute      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_assign       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_unassign     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Internal tables
    CLASS-METHODS stmt_append       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_insert_internal RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_modify_internal RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_delete_internal RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_read_table   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_sort         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_collect      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " OO
    CLASS-METHODS stmt_class_def    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_class_impl   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endclass     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_interface    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endinterface RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_method_def   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_method_impl  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endmethod    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_create_object RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_method  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Subroutines / function modules
    CLASS-METHODS stmt_form         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endform      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_perform      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_function     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endfunction  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_function RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_module       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endmodule    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " SQL
    CLASS-METHODS stmt_select       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endselect    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_insert_db    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_update_db    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_delete_db    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_modify_db    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_commit       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_rollback     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Reporting
    CLASS-METHODS stmt_report       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_write        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_message      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Misc
    CLASS-METHODS stmt_assert       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_return       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_include      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_export       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_import       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Selection screen / events
    CLASS-METHODS stmt_select_options RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_parameters     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_parameter      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_ranges         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_tables         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_selection_screen RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_selection   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_user_command RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_pf          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_line_sel    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_initialization RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_start_of_sel   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_end_of_sel     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_top_of_page    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_end_of_page    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_load_of_program RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_first       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at_last        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_at             RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endat          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " String operations
    CLASS-METHODS stmt_concatenate    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_split          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_replace        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_find           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_translate      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_condense       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_overlay        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_shift          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_search         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " System / control
    CLASS-METHODS stmt_describe       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_free           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_refresh        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_wait           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_stop           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_submit         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_leave          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_generate       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_authority_check RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_break_point    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_break          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_log_point      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_set            RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_get            RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_convert        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_pack           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_unpack         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " File I/O
    CLASS-METHODS stmt_open_dataset   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_close_dataset  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_read_dataset   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_transfer       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_delete_dataset RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " More OO
    CLASS-METHODS stmt_aliases        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_interfaces     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_class_data     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_class_events   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_events         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_raise_event    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_create_data    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_catch_sys_excs RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_case_type      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_when_type      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " More reporting
    CLASS-METHODS stmt_format         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_new_line       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_new_page       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_skip           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_uline          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_position       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_print_control  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_reserve        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_back           RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_suppress       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_extract        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_field_groups   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Math / arithmetic
    CLASS-METHODS stmt_add            RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_subtract       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_multiply       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_divide         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_move_corres    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Macro / definition
    CLASS-METHODS stmt_define         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_end_of_def     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    " Misc more
    CLASS-METHODS stmt_provide        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endprovide     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_on_change      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endon          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_chain          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_endchain       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_resume         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_retry          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_function_pool  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_program        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_type_pool      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_type_pools     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_infotypes      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_controls       RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_statics        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_screen    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_transaction RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_dialog    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_set_pf_status  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_set_titlebar   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_window         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_loop_dynpro    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_process        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_field          RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_receive        RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_communication  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_set_handler    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_get_reference  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS stmt_call_badi      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

ENDCLASS.


CLASS zcl_ace_stmts IMPLEMENTATION.

  " ===== Control flow =====

  METHOD stmt_if.            " seq("IF", Cond)
    r = zcl_ace_combi=>seq( VALUE #( ( zcl_ace_combi=>str( `IF` ) )
                                     ( zcl_ace_combi=>expr( `COND` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_elseif.        " seq("ELSEIF", Cond)
    r = zcl_ace_combi=>seq( VALUE #( ( zcl_ace_combi=>str( `ELSEIF` ) )
                                     ( zcl_ace_combi=>expr( `COND` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_else.          " str("ELSE")
    r = zcl_ace_combi=>str( `ELSE` ).
  ENDMETHOD.

  METHOD stmt_endif.
    r = zcl_ace_combi=>str( `ENDIF` ).
  ENDMETHOD.

  METHOD stmt_case.          " seq("CASE", opt("TYPE OF"), Source)
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CASE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE OF` ) ) )
      ( zcl_ace_combi=>expr( `SOURCE` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_when.          " seq("WHEN", Source [OR Source]*)
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WHEN` ) )
      ( zcl_ace_combi=>expr( `SOURCE` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_when_others.
    r = zcl_ace_combi=>str( `WHEN OTHERS` ).
  ENDMETHOD.

  METHOD stmt_endcase.
    r = zcl_ace_combi=>str( `ENDCASE` ).
  ENDMETHOD.

  METHOD stmt_do.            " seq("DO", opt(Source "TIMES"), opt("VARYING" ...))
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DO` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIMES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VARYING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEXT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RANGE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_enddo.
    r = zcl_ace_combi=>str( `ENDDO` ).
  ENDMETHOD.

  METHOD stmt_while.         " seq("WHILE", Cond, opt("VARY" ...))
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WHILE` ) )
      ( zcl_ace_combi=>expr( `COND` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VARY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEXT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endwhile.
    r = zcl_ace_combi=>str( `ENDWHILE` ).
  ENDMETHOD.

  METHOD stmt_loop.
    " seq("LOOP", opt("AT" LoopSource), opt(LoopTarget), opt("WHERE" Cond),
    "      opt("USING KEY"), opt("FROM" Source), opt("TO" Source), opt("STEP" Source),
    "      opt("GROUP BY" ...))
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `LOOP` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `AT` ) )
          ( zcl_ace_combi=>expr( `LOOP_SOURCE` ) ) ) ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>expr( `LOOP_TARGET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `WHERE` ) )
          ( zcl_ace_combi=>expr( `COND` ) ) ) ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STEP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GROUP BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GROUP` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endloop.
    r = zcl_ace_combi=>str( `ENDLOOP` ).
  ENDMETHOD.

  METHOD stmt_continue.
    r = zcl_ace_combi=>str( `CONTINUE` ).
  ENDMETHOD.

  METHOD stmt_exit.
    r = zcl_ace_combi=>str( `EXIT` ).
  ENDMETHOD.

  METHOD stmt_check.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CHECK` ) )
      ( zcl_ace_combi=>expr( `COND` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_try.
    r = zcl_ace_combi=>str( `TRY` ).
  ENDMETHOD.

  METHOD stmt_endtry.
    r = zcl_ace_combi=>str( `ENDTRY` ).
  ENDMETHOD.

  METHOD stmt_catch.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CATCH` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEFORE UNWIND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_cleanup.
    r = zcl_ace_combi=>str( `CLEANUP` ).
  ENDMETHOD.

  METHOD stmt_raise.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RAISE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESUMABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEW` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EVENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHORTDUMP` ) ) ) ) ).
  ENDMETHOD.

  " ===== Data =====

  METHOD stmt_data.          " seq("DATA", DataDefinition, optPrio("%_PREDEFINED"))
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DATA` ) )
      ( zcl_ace_combi=>expr( `DATA_DEFINITION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_data_begin.    " "DATA: BEGIN OF [PUBLIC SECTION] name"
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DATA` ) )
      ( zcl_ace_combi=>str( `BEGIN OF` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMMON PART` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_data_end.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DATA` ) )
      ( zcl_ace_combi=>str( `END OF` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_constant.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONSTANTS` ) )
      ( zcl_ace_combi=>expr( `DATA_DEFINITION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_field_symbol.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FIELD-SYMBOLS` ) )
      ( zcl_ace_combi=>expr( `FIELD_SYMBOL` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STANDARD TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SORTED TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HASHED TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ANY TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX TABLE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_types.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `TYPES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REF TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STANDARD TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SORTED TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HASHED TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH UNIQUE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH NON-UNIQUE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH EMPTY KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH DEFAULT KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OCCURS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_clear.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLEAR` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH NULL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_move.
    " verNot(Cloud,"MOVE") seq("EXACT"? Source "TO" "?"? Target "%_LENGTH"?)
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MOVE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXACT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `?` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_compute.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `COMPUTE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXACT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_assign.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ASSIGN` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOCAL COPY OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL LINE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPONENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF STRUCTURE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCREMENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RANGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CASTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HANDLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DECIMALS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_unassign.
    r = zcl_ace_combi=>str( `UNASSIGN` ).
  ENDMETHOD.

  " ===== Internal tables =====

  METHOD stmt_append.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `APPEND` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STEP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SORTED BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASSIGNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CASTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_insert_internal.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INSERT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL LINE INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEFORE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AFTER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASSIGNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CASTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_modify_internal.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MODIFY` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TRANSPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_delete_internal.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DELETE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ADJACENT DUPLICATES FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPARING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_read_table.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `READ TABLE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH TABLE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPONENTS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BINARY SEARCH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TRANSPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASSIGNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CASTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ELSE UNASSIGN` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_sort.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SORT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASCENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DESCENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS TEXT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BYPASSING BUFFER` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_collect.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `COLLECT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASSIGNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE INTO` ) ) ) ) ).
  ENDMETHOD.

  " ===== OO =====

  METHOD stmt_class_def.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLASS` ) )
      ( zcl_ace_combi=>str( `DEFINITION` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PUBLIC` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INHERITING FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ABSTRACT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FINAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CREATE PUBLIC` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CREATE PROTECTED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CREATE PRIVATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED MEMORY ENABLED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR TESTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RISK LEVEL HARMLESS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RISK LEVEL DANGEROUS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RISK LEVEL CRITICAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DURATION SHORT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DURATION MEDIUM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DURATION LONG` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOAD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFERRED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOCAL FRIENDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GLOBAL FRIENDS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_class_impl.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLASS` ) )
      ( zcl_ace_combi=>str( `IMPLEMENTATION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endclass.
    r = zcl_ace_combi=>str( `ENDCLASS` ).
  ENDMETHOD.

  METHOD stmt_interface.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INTERFACE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PUBLIC` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOAD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFERRED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endinterface.
    r = zcl_ace_combi=>str( `ENDINTERFACE` ).
  ENDMETHOD.

  METHOD stmt_method_def.
    " Methods/Class-Methods declaration — many keywords, see method_def.ts
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>alt( VALUE #( ( zcl_ace_combi=>str( `METHODS` ) )
                                     ( zcl_ace_combi=>str( `CLASS-METHODS` ) ) ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FINAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ABSTRACT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFAULT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FAIL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RETURNING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RAISING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR TESTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR EVENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REDEFINITION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AMDP OPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `READ-ONLY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CDS SESSION CLIENT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_method_impl.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `METHOD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY KERNEL MODULE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY DATABASE PROCEDURE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY DATABASE FUNCTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR HDB` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LANGUAGE SQLSCRIPT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `READ-ONLY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endmethod.
    r = zcl_ace_combi=>str( `ENDMETHOD` ).
  ENDMETHOD.

  METHOD stmt_create_object.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CREATE OBJECT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AREA HANDLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_method.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL METHOD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RECEIVING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTIONS` ) ) ) ) ).
  ENDMETHOD.

  " ===== Subroutines / function modules =====

  METHOD stmt_form.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FORM` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RAISING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endform.
    r = zcl_ace_combi=>str( `ENDFORM` ).
  ENDMETHOD.

  METHOD stmt_perform.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PERFORM` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON COMMIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON ROLLBACK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEVEL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IF FOUND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_function.
    r = zcl_ace_combi=>str( `FUNCTION` ).
  ENDMETHOD.

  METHOD stmt_endfunction.
    r = zcl_ace_combi=>str( `ENDFUNCTION` ).
  ENDMETHOD.

  METHOD stmt_call_function.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL FUNCTION` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN UPDATE TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BACKGROUND TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BACKGROUND UNIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS SEPARATE UNIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `KEEPING LOGICAL UNIT OF WORK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING NEW TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CALLING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON END OF TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DESTINATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PARAMETER-TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTION-TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OTHERS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_module.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MODULE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OUTPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT EXIT-COMMAND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endmodule.
    r = zcl_ace_combi=>str( `ENDMODULE` ).
  ENDMETHOD.

  " ===== SQL =====

  METHOD stmt_select.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SELECT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SINGLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR UPDATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DISTINCT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO CORRESPONDING FIELDS OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `APPENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `APPENDING TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GROUP BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HAVING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ORDER BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UP TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ROWS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PACKAGE SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BYPASSING BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INNER JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT OUTER JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RIGHT OUTER JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CROSS JOIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UNION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTERSECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ASCENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DESCENDING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endselect.
    r = zcl_ace_combi=>str( `ENDSELECT` ).
  ENDMETHOD.

  METHOD stmt_insert_db.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INSERT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCEPTING DUPLICATE KEYS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_update_db.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `UPDATE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDICATORS SET STRUCTURE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_delete_db.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DELETE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_modify_db.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MODIFY` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONNECTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CLIENT SPECIFIED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_commit.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `COMMIT WORK` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND WAIT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_rollback.
    r = zcl_ace_combi=>str( `ROLLBACK WORK` ).
  ENDMETHOD.

  " ===== Reporting =====

  METHOD stmt_report.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `REPORT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO STANDARD PAGE HEADING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGE-ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFINING DATABASE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_write.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WRITE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING EDIT MASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING NO EDIT MASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-ZERO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-SIGN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT-JUSTIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RIGHT-JUSTIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CENTERED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UNDER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-GAP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COLOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTENSIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INVERSE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HOTSPOT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `QUICKINFO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS CHECKBOX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS SYMBOL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS ICON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS LINE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_message.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MESSAGE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NUMBER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DISPLAY LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RAISING` ) ) ) ) ).
  ENDMETHOD.

  " ===== Misc =====

  METHOD stmt_assert.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ASSERT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBKEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONDITION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_return.
    r = zcl_ace_combi=>str( `RETURN` ).
  ENDMETHOD.

  METHOD stmt_include.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INCLUDE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IF FOUND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STRUCTURE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_export.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `EXPORT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATABASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATA BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTERNAL TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPRESSION ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPRESSION OFF` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_import.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `IMPORT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATABASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SHARED BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATA BUFFER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTERNAL TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CONVERSION ERRORS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING STRUCTURE BOUNDARIES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT CHARACTER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCEPTING PADDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCEPTING TRUNCATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHAR-TO-HEX MODE` ) ) ) ) ).
  ENDMETHOD.

  " ===== Selection screen / events =====

  METHOD stmt_select_options.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SELECT-OPTIONS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFAULT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OPTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SIGN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCHCODE OBJECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODIF ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOWER CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OBLIGATORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-DISPLAY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO INTERVALS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-EXTENSION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VISIBLE LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE-REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USER-COMMAND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_parameters.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PARAMETERS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DECIMALS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFAULT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCHCODE OBJECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODIF ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOWER CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OBLIGATORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-DISPLAY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VISIBLE LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS CHECKBOX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RADIOBUTTON GROUP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS LISTBOX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE CHECK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE-REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USER-COMMAND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_parameter.
    r = zcl_ace_combi=>str( `PARAMETER` ).
  ENDMETHOD.

  METHOD stmt_ranges.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RANGES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_tables.
    r = zcl_ace_combi=>str( `TABLES` ).
  ENDMETHOD.

  METHOD stmt_selection_screen.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SELECTION-SCREEN` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEGIN OF TABBED BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF TABBED BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH FRAME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TITLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO INTERVALS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS WINDOW` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AS SUBSCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDE BLOCKS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDE PARAMETERS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDE SELECT-OPTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SKIP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ULINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMMENT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `POSITION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PUSHBUTTON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FUNCTION KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DYNAMIC SELECTIONS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_at_selection.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `AT SELECTION-SCREEN` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OUTPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BLOCK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RADIOBUTTON GROUP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE-REQUEST FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HELP-REQUEST FOR` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_at_user_command.
    r = zcl_ace_combi=>str( `AT USER-COMMAND` ).
  ENDMETHOD.

  METHOD stmt_at_pf.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `AT PF` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_at_line_sel.
    r = zcl_ace_combi=>str( `AT LINE-SELECTION` ).
  ENDMETHOD.

  METHOD stmt_initialization.
    r = zcl_ace_combi=>str( `INITIALIZATION` ).
  ENDMETHOD.

  METHOD stmt_start_of_sel.
    r = zcl_ace_combi=>str( `START-OF-SELECTION` ).
  ENDMETHOD.

  METHOD stmt_end_of_sel.
    r = zcl_ace_combi=>str( `END-OF-SELECTION` ).
  ENDMETHOD.

  METHOD stmt_top_of_page.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `TOP-OF-PAGE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DURING LINE-SELECTION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_end_of_page.
    r = zcl_ace_combi=>str( `END-OF-PAGE` ).
  ENDMETHOD.

  METHOD stmt_load_of_program.
    r = zcl_ace_combi=>str( `LOAD-OF-PROGRAM` ).
  ENDMETHOD.

  METHOD stmt_at_first.
    r = zcl_ace_combi=>str( `AT FIRST` ).
  ENDMETHOD.

  METHOD stmt_at_last.
    r = zcl_ace_combi=>str( `AT LAST` ).
  ENDMETHOD.

  METHOD stmt_at.
    " AT NEW <field> / AT END OF <field>
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `AT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEW` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END OF` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endat.
    r = zcl_ace_combi=>str( `ENDAT` ).
  ENDMETHOD.

  " ===== String operations =====

  METHOD stmt_concatenate.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONCATENATE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SEPARATED BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESPECTING BLANKS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_split.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SPLIT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_replace.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `REPLACE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIRST OCCURRENCE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL OCCURRENCES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OCCURRENCES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBSTRING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REGEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PCRE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECTION OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECTION LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESPECTING CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESULTS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_find.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FIND` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIRST OCCURRENCE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL OCCURRENCES OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBSTRING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REGEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PCRE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECTION OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECTION LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESPECTING CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCH COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCH OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCH LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MATCH LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESULTS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBMATCHES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_translate.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `TRANSLATE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO UPPER CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LOWER CASE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM CODE PAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO CODE PAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM NUMBER FORMAT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO NUMBER FORMAT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_condense.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONDENSE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-GAPS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_overlay.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `OVERLAY` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ONLY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_shift.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SHIFT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RIGHT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CIRCULAR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PLACES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DELETING LEADING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DELETING TRAILING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UP TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_search.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SEARCH` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ABBREVIATED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENDING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND MARK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) ) ) ).
  ENDMETHOD.

  " ===== System / control =====

  METHOD stmt_describe.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DESCRIBE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DISTANCE BETWEEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OCCURS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `KIND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BYTE MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN CHARACTER MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COMPONENTS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OUTPUT-LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DECIMALS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EDIT MASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HELP-ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PAGES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT MARGIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TOP LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TITLE LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HEAD LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `END LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIRST-LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DISTANCE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_free.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FREE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OBJECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_refresh.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `REFRESH` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CONTROL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM SCREEN` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_wait.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WAIT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UP TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SECONDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UNTIL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR ASYNCHRONOUS TASKS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR PUSH CHANNELS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR MESSAGING CHANNELS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_stop.
    r = zcl_ace_combi=>str( `STOP` ).
  ENDMETHOD.

  METHOD stmt_submit.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SUBMIT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VIA SELECTION-SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VIA JOB` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NUMBER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING LIST TO MEMORY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING SELECTION-SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING SELECTION-SET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING SELECTION-SETS OF PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH FREE SELECTIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH SELECTION-TABLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND RETURN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO SAP-SPOOL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SPOOL PARAMETERS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ARCHIVE PARAMETERS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITHOUT SPOOL DYNPRO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_leave.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `LEAVE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO TRANSACTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND SKIP FIRST SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LIST-PROCESSING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND RETURN TO SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIST-PROCESSING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO CURRENT TRANSACTION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_generate.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `GENERATE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBROUTINE POOL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPORT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DYNPRO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NAME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WORD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TRACE-FILE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_authority_check.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `AUTHORITY-CHECK` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OBJECT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR USER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DUMMY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_break_point.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `BREAK-POINT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_break.
    r = zcl_ace_combi=>str( `BREAK` ).
  ENDMETHOD.

  METHOD stmt_log_point.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `LOG-POINT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SUBKEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELDS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_set.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SET` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOCALE LANGUAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COUNTRY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODIFIER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BLANK LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COUNTRY-SPECIFIC CONVERSIONS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CURSOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXTENDED CHECK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HANDLER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACTIVATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HOLD DATA` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LANGUAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LEFT SCROLL-BOUNDARY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MARGIN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PARAMETER ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PF-STATUS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INCLUDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCLUDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMMEDIATELY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PROPERTY OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RUN TIME ANALYZER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RUN TIME CLOCK RESOLUTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TITLEBAR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UPDATE TASK LOCAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USER-COMMAND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_get.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `GET` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CURSOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFFSET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PARAMETER ID` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PF-STATUS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCLUDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PROPERTY OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REFERENCE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RUN TIME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD VALUE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BADI` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LOCALE LANGUAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COUNTRY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODIFIER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STAMP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELD` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENDING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LATE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_convert.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONVERT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STAMP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INVERTED-DATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIME STAMP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TIME ZONE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DAYLIGHT SAVING TIME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TEXT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO SORTABLE CODE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_pack.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PACK` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_unpack.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `UNPACK` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) ) ) ).
  ENDMETHOD.

  " ===== File I/O =====

  METHOD stmt_open_dataset.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `OPEN DATASET` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR OUTPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR APPENDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR UPDATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN BINARY MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN TEXT MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN LEGACY BINARY MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN LEGACY TEXT MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BIG ENDIAN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LITTLE ENDIAN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NATIVE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENCODING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEFAULT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UTF-8` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NON-UNICODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH SMART LINEFEED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH NATIVE LINEFEED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH UNIX LINEFEED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH WINDOWS LINEFEED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CODE PAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CONVERSION ERRORS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REPLACEMENT CHARACTER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IGNORING CONVERSION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FILTER` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT POSITION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_close_dataset.
    r = zcl_ace_combi=>str( `CLOSE DATASET` ).
  ENDMETHOD.

  METHOD stmt_read_dataset.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `READ DATASET` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MAXIMUM LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACTUAL LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_transfer.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `TRANSFER` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LENGTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO END OF LINE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_delete_dataset.
    r = zcl_ace_combi=>str( `DELETE DATASET` ).
  ENDMETHOD.

  " ===== More OO =====

  METHOD stmt_aliases.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ALIASES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_interfaces.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INTERFACES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ABSTRACT METHODS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FINAL METHODS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DATA VALUES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL METHODS ABSTRACT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALL METHODS FINAL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PARTIALLY IMPLEMENTED` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_class_data.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLASS-DATA` ) )
      ( zcl_ace_combi=>expr( `DATA_DEFINITION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_class_events.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CLASS-EVENTS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_events.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `EVENTS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_raise_event.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RAISE EVENT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_create_data.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CREATE DATA` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LIKE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HANDLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AREA HANDLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STANDARD TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SORTED TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HASHED TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ANY TABLE OF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH UNIQUE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH NON-UNIQUE KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH EMPTY KEY` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INITIAL SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `REF TO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_catch_sys_excs.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CATCH SYSTEM-EXCEPTIONS` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_case_type.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CASE TYPE OF` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_when_type.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WHEN TYPE` ) ) ) ).
  ENDMETHOD.

  " ===== More reporting =====

  METHOD stmt_format.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FORMAT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESET` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COLOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTENSIFIED` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INVERSE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HOTSPOT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FRAMES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OFF` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_new_line.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `NEW-LINE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-SCROLLING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SCROLLING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_new_page.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `NEW-PAGE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-TITLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH-TITLE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO-HEADING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH-HEADING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-COUNT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE-SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PRINT ON` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `PRINT OFF` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NO DIALOG` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NEW-SECTION` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_skip.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SKIP` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO LINE` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_uline.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ULINE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_position.
    r = zcl_ace_combi=>str( `POSITION` ).
  ENDMETHOD.

  METHOD stmt_print_control.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PRINT-CONTROL` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INDEX LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `POSITION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FUNCTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANNEL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FONT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CPI` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LPI` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `COLOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SIZE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WIDTH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `HEIGHT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_reserve.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RESERVE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `LINES` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_back.
    r = zcl_ace_combi=>str( `BACK` ).
  ENDMETHOD.

  METHOD stmt_suppress.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SUPPRESS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DIALOG` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_extract.
    r = zcl_ace_combi=>str( `EXTRACT` ).
  ENDMETHOD.

  METHOD stmt_field_groups.
    r = zcl_ace_combi=>str( `FIELD-GROUPS` ).
  ENDMETHOD.

  " ===== Math / arithmetic =====

  METHOD stmt_add.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `ADD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `THEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UNTIL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `GIVING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCORDING TO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_subtract.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SUBTRACT` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_multiply.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MULTIPLY` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_divide.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `DIVIDE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_move_corres.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `MOVE-CORRESPONDING` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `KEEPING TARGET LINES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPANDING NESTED TABLES` ) ) ) ) ).
  ENDMETHOD.

  " ===== Macro / definition =====

  METHOD stmt_define.
    r = zcl_ace_combi=>str( `DEFINE` ).
  ENDMETHOD.

  METHOD stmt_end_of_def.
    r = zcl_ace_combi=>str( `END-OF-DEFINITION` ).
  ENDMETHOD.

  " ===== Misc more =====

  METHOD stmt_provide.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PROVIDE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FIELDS` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BETWEEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_endprovide.
    r = zcl_ace_combi=>str( `ENDPROVIDE` ).
  ENDMETHOD.

  METHOD stmt_on_change.
    r = zcl_ace_combi=>str( `ON CHANGE OF` ).
  ENDMETHOD.

  METHOD stmt_endon.
    r = zcl_ace_combi=>str( `ENDON` ).
  ENDMETHOD.

  METHOD stmt_chain.
    r = zcl_ace_combi=>str( `CHAIN` ).
  ENDMETHOD.

  METHOD stmt_endchain.
    r = zcl_ace_combi=>str( `ENDCHAIN` ).
  ENDMETHOD.

  METHOD stmt_resume.
    r = zcl_ace_combi=>str( `RESUME` ).
  ENDMETHOD.

  METHOD stmt_retry.
    r = zcl_ace_combi=>str( `RETRY` ).
  ENDMETHOD.

  METHOD stmt_function_pool.
    r = zcl_ace_combi=>str( `FUNCTION-POOL` ).
  ENDMETHOD.

  METHOD stmt_program.
    r = zcl_ace_combi=>str( `PROGRAM` ).
  ENDMETHOD.

  METHOD stmt_type_pool.
    r = zcl_ace_combi=>str( `TYPE-POOL` ).
  ENDMETHOD.

  METHOD stmt_type_pools.
    r = zcl_ace_combi=>str( `TYPE-POOLS` ).
  ENDMETHOD.

  METHOD stmt_infotypes.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `INFOTYPES` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `NAME` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALID` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_controls.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CONTROLS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TYPE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLEVIEW` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABSTRIP` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CUSTOM CONTROL` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_statics.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `STATICS` ) )
      ( zcl_ace_combi=>expr( `DATA_DEFINITION` ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_screen.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL SCREEN` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENDING AT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_transaction.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL TRANSACTION` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AND SKIP FIRST SCREEN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH AUTHORITY-CHECK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITHOUT AUTHORITY-CHECK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `USING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `UPDATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MESSAGES INTO` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OPTIONS FROM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH PARAMETER` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_dialog.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL DIALOG` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_set_pf_status.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SET PF-STATUS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCLUDING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMMEDIATELY` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_set_titlebar.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SET TITLEBAR` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `OF PROGRAM` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_window.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `WINDOW` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `STARTING AT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ENDING AT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_loop_dynpro.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `LOOP` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WITH CONTROL` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CURSOR` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_process.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `PROCESS` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `BEFORE OUTPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `AFTER INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON HELP-REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON VALUE-REQUEST` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_field.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FIELD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `MODULE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON CHAIN-INPUT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ON CHAIN-REQUEST` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SELECT` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_receive.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `RECEIVE` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RESULTS FROM FUNCTION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `KEEPING TASK` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `TABLES` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXCEPTIONS` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_communication.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `COMMUNICATION` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INIT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ALLOCATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACCEPT` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `SEND` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RECEIVE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DEALLOCATE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `DESTINATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ID` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_set_handler.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `SET HANDLER` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `ACTIVATION` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `FOR ALL INSTANCES` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_get_reference.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `GET REFERENCE OF` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `INTO` ) ) ) ) ).
  ENDMETHOD.

  METHOD stmt_call_badi.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `CALL BADI` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EXPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IMPORTING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `CHANGING` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `RECEIVING` ) ) ) ) ).
  ENDMETHOD.

ENDCLASS.
