"! ABAP expression grammars - port of @abaplint/core 2_statements/expressions/*.ts
"! One CLASS-METHOD per expression, name = EXPR_<expression_name>.
"! Each method returns a runnable tree (zcl_ace_combi_node).
"! Discovered via RTTI by zcl_ace_keywords.
"!
"! Initial set covers the most-referenced expressions used by the seed statements
"! in zcl_ace_stmts. Extend incrementally.
CLASS zcl_ace_exprs DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    " Cond — boolean condition tree. Simplified: actual abaplint grammar is large;
    " for keyword extraction we list only its keywords.
    CLASS-METHODS expr_cond     RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_compare  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_compare_operator RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_source   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_target   RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_field    RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_field_chain RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_field_symbol RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_data_definition RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_inline_data RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_loop_source RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_loop_target RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS expr_for      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

ENDCLASS.


CLASS zcl_ace_exprs IMPLEMENTATION.

  METHOD expr_cond.
    " Cond.ts grammar boils down to comparisons joined by AND/OR/NOT/EQUIV +
    " parentheses. Keyword-relevant tokens:
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>str( `AND` ) )
      ( zcl_ace_combi=>str( `OR` ) )
      ( zcl_ace_combi=>str( `NOT` ) )
      ( zcl_ace_combi=>str( `EQUIV` ) )
      ( zcl_ace_combi=>expr( `COMPARE` ) ) ) ).
  ENDMETHOD.

  METHOD expr_compare.
    " Compare.ts — basic shape: Source CompareOperator Source, with IS/BETWEEN/IN/LIKE variants
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>str( `IS` ) )
      ( zcl_ace_combi=>str( `INITIAL` ) )
      ( zcl_ace_combi=>str( `BOUND` ) )
      ( zcl_ace_combi=>str( `ASSIGNED` ) )
      ( zcl_ace_combi=>str( `SUPPLIED` ) )
      ( zcl_ace_combi=>str( `INSTANCE OF` ) )
      ( zcl_ace_combi=>str( `BETWEEN` ) )
      ( zcl_ace_combi=>str( `IN` ) )
      ( zcl_ace_combi=>str( `LIKE` ) )
      ( zcl_ace_combi=>str( `CO` ) )
      ( zcl_ace_combi=>str( `CN` ) )
      ( zcl_ace_combi=>str( `CA` ) )
      ( zcl_ace_combi=>str( `NA` ) )
      ( zcl_ace_combi=>str( `CS` ) )
      ( zcl_ace_combi=>str( `NS` ) )
      ( zcl_ace_combi=>str( `CP` ) )
      ( zcl_ace_combi=>str( `NP` ) )
      ( zcl_ace_combi=>expr( `COMPARE_OPERATOR` ) ) ) ).
  ENDMETHOD.

  METHOD expr_compare_operator.
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>str( `EQ` ) )
      ( zcl_ace_combi=>str( `NE` ) )
      ( zcl_ace_combi=>str( `LT` ) )
      ( zcl_ace_combi=>str( `LE` ) )
      ( zcl_ace_combi=>str( `GT` ) )
      ( zcl_ace_combi=>str( `GE` ) ) ) ).
  ENDMETHOD.

  METHOD expr_source.
    " Source — value expression. For keyword purposes, the constructor expressions:
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>str( `VALUE` ) )
      ( zcl_ace_combi=>str( `NEW` ) )
      ( zcl_ace_combi=>str( `REF` ) )
      ( zcl_ace_combi=>str( `CONV` ) )
      ( zcl_ace_combi=>str( `CAST` ) )
      ( zcl_ace_combi=>str( `EXACT` ) )
      ( zcl_ace_combi=>str( `COND` ) )
      ( zcl_ace_combi=>str( `SWITCH` ) )
      ( zcl_ace_combi=>str( `REDUCE` ) )
      ( zcl_ace_combi=>str( `FILTER` ) )
      ( zcl_ace_combi=>str( `CORRESPONDING` ) )
      ( zcl_ace_combi=>str( `BOOLC` ) )
      ( zcl_ace_combi=>str( `XSDBOOL` ) )
      ( zcl_ace_combi=>str( `LET` ) )
      ( zcl_ace_combi=>str( `IN` ) )
      ( zcl_ace_combi=>str( `THEN` ) )
      ( zcl_ace_combi=>str( `ELSE` ) ) ) ).
  ENDMETHOD.

  METHOD expr_target.
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>expr( `FIELD_CHAIN` ) )
      ( zcl_ace_combi=>expr( `INLINE_DATA` ) )
      ( zcl_ace_combi=>expr( `FIELD_SYMBOL` ) ) ) ).
  ENDMETHOD.

  METHOD expr_field.
    r = zcl_ace_combi=>tok( `Identifier` ).
  ENDMETHOD.

  METHOD expr_field_chain.
    r = zcl_ace_combi=>expr( `FIELD` ).
  ENDMETHOD.

  METHOD expr_field_symbol.
    r = zcl_ace_combi=>tok( `Identifier` ).
  ENDMETHOD.

  METHOD expr_data_definition.
    " DataDefinition — name + TYPE/LIKE clause + value
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>expr( `FIELD` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>alt( VALUE #(
          ( zcl_ace_combi=>str( `TYPE` ) )
          ( zcl_ace_combi=>str( `LIKE` ) ) ) ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `VALUE` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `READ-ONLY` ) ) ) ) ).
  ENDMETHOD.

  METHOD expr_inline_data.
    r = zcl_ace_combi=>tok( `Identifier` ).
  ENDMETHOD.

  METHOD expr_loop_source.
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>expr( `FIELD_CHAIN` ) )
      ( zcl_ace_combi=>str( `SCREEN` ) ) ) ).
  ENDMETHOD.

  METHOD expr_loop_target.
    r = zcl_ace_combi=>alt( VALUE #(
      ( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `INTO` ) )
          ( zcl_ace_combi=>expr( `TARGET` ) ) ) ) )
      ( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `ASSIGNING` ) )
          ( zcl_ace_combi=>expr( `FIELD_SYMBOL` ) ) ) ) )
      ( zcl_ace_combi=>seq( VALUE #(
          ( zcl_ace_combi=>str( `REFERENCE INTO` ) )
          ( zcl_ace_combi=>expr( `TARGET` ) ) ) ) ) ) ).
  ENDMETHOD.

  METHOD expr_for.
    r = zcl_ace_combi=>seq( VALUE #(
      ( zcl_ace_combi=>str( `FOR` ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `EACH` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `IN` ) ) )
      ( zcl_ace_combi=>opt( zcl_ace_combi=>str( `WHERE` ) ) ) ) ).
  ENDMETHOD.

ENDCLASS.
