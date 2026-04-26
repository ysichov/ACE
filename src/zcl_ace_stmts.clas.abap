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

ENDCLASS.
