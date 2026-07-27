"! Grammar node — direct port of abaplint combi.ts combinators.
"! Single class with discriminator (kind) instead of 11 separate combinator classes.
"! Trees are walked by ZCL_ACE_KEYWORDS=>WALK_NODE, which follows Expression
"! references as well — the same algorithm as Combi.listKeywords() in
"! @abaplint/core, plus expression resolution.
"!
"! Only the kinds the ported grammar actually builds are defined here. combi.ts
"! also has Star, Plus, Per, Vers and Regex; porting a rule that needs one means
"! adding its kind constant and factory below, plus the matching combinator in
"! ZCL_ACE_COMBI.
CLASS zcl_ace_combi_node DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES tt_children TYPE STANDARD TABLE OF REF TO zcl_ace_combi_node WITH EMPTY KEY.

    " Discriminator values mirror combi.ts class names
    CONSTANTS:
      c_kind_word  TYPE c LENGTH 1 VALUE 'W',  " Word          → contributes to listKeywords
      c_kind_wseq  TYPE c LENGTH 1 VALUE 'Q',  " WordSequence  → contributes to listKeywords
      c_kind_token TYPE c LENGTH 1 VALUE 'T',  " Token  (tok)  → no keywords
      c_kind_seq   TYPE c LENGTH 1 VALUE 'S',  " Sequence      → recurse
      c_kind_alt   TYPE c LENGTH 1 VALUE 'A',  " Alternative   → recurse
      c_kind_opt   TYPE c LENGTH 1 VALUE 'O',  " Optional      → recurse
      c_kind_expr  TYPE c LENGTH 1 VALUE 'E'.  " Expression reference → resolved at aggregation time

    DATA kind     TYPE c LENGTH 1 READ-ONLY.
    DATA value    TYPE string     READ-ONLY.   " word literal / token class name / regex / expression name
    DATA children TYPE tt_children READ-ONLY.

    " Factory methods — one per combinator kind in use
    CLASS-METHODS new_word    IMPORTING s TYPE string                  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_wseq    IMPORTING s TYPE string                  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_token   IMPORTING token_name TYPE string         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_seq     IMPORTING children TYPE tt_children      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_alt     IMPORTING children TYPE tt_children      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_opt     IMPORTING child TYPE REF TO zcl_ace_combi_node RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_expr    IMPORTING name TYPE string               RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    METHODS constructor
      IMPORTING
        kind     TYPE c
        value    TYPE string     OPTIONAL
        children TYPE tt_children OPTIONAL.

ENDCLASS.


CLASS zcl_ace_combi_node IMPLEMENTATION.

  METHOD constructor.
    me->kind     = kind.
    me->value    = value.
    me->children = children.
  ENDMETHOD.

  METHOD new_word.
    r = NEW #( kind = c_kind_word value = to_upper( s ) ).
  ENDMETHOD.

  METHOD new_wseq.
    r = NEW #( kind = c_kind_wseq value = to_upper( s ) ).
  ENDMETHOD.

  METHOD new_token.
    r = NEW #( kind = c_kind_token value = token_name ).
  ENDMETHOD.

  METHOD new_seq.
    r = NEW #( kind = c_kind_seq children = children ).
  ENDMETHOD.

  METHOD new_alt.
    r = NEW #( kind = c_kind_alt children = children ).
  ENDMETHOD.

  METHOD new_opt.
    r = NEW #( kind = c_kind_opt children = VALUE #( ( child ) ) ).
  ENDMETHOD.

  METHOD new_expr.
    r = NEW #( kind = c_kind_expr value = name ).
  ENDMETHOD.

ENDCLASS.
