"! Grammar node — direct port of abaplint combi.ts combinators.
"! Single class with discriminator (kind) instead of 11 separate combinator classes.
"! list_keywords( ) walks the tree and returns all str() literals — same algorithm
"! as Combi.listKeywords() in @abaplint/core.
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
      c_kind_regex TYPE c LENGTH 1 VALUE 'R',  " Regex         → no keywords
      c_kind_seq   TYPE c LENGTH 1 VALUE 'S',  " Sequence      → recurse
      c_kind_alt   TYPE c LENGTH 1 VALUE 'A',  " Alternative   → recurse
      c_kind_opt   TYPE c LENGTH 1 VALUE 'O',  " Optional      → recurse
      c_kind_star  TYPE c LENGTH 1 VALUE '*',  " Star          → recurse
      c_kind_plus  TYPE c LENGTH 1 VALUE '+',  " Plus          → recurse
      c_kind_per   TYPE c LENGTH 1 VALUE 'P',  " Permutation   → recurse
      c_kind_vers  TYPE c LENGTH 1 VALUE 'V',  " Vers / VersNot → recurse (single child)
      c_kind_expr  TYPE c LENGTH 1 VALUE 'E'.  " Expression reference → resolved at aggregation time

    DATA kind     TYPE c LENGTH 1 READ-ONLY.
    DATA value    TYPE string     READ-ONLY.   " word literal / token class name / regex / expression name
    DATA children TYPE tt_children READ-ONLY.

    " Factory methods — one per combinator type
    CLASS-METHODS new_word    IMPORTING s TYPE string                  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_wseq    IMPORTING s TYPE string                  RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_token   IMPORTING token_name TYPE string         RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_regex   IMPORTING pattern TYPE string            RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_seq     IMPORTING children TYPE tt_children      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_alt     IMPORTING children TYPE tt_children      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_opt     IMPORTING child TYPE REF TO zcl_ace_combi_node RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_star    IMPORTING child TYPE REF TO zcl_ace_combi_node RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_plus    IMPORTING child TYPE REF TO zcl_ace_combi_node RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_per     IMPORTING children TYPE tt_children      RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_vers    IMPORTING child TYPE REF TO zcl_ace_combi_node RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.
    CLASS-METHODS new_expr    IMPORTING name TYPE string               RETURNING VALUE(r) TYPE REF TO zcl_ace_combi_node.

    "! Recursively collects all keyword literals from str() / WordSequence nodes.
    "! Mirrors Combi.listKeywords() in @abaplint/core (combi.ts).
    "! Expression nodes return their NAME prefixed with "expression/" — the aggregator
    "! resolves them in a separate pass to avoid infinite recursion.
    METHODS list_keywords RETURNING VALUE(result) TYPE string_table.

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

  METHOD new_regex.
    r = NEW #( kind = c_kind_regex value = pattern ).
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

  METHOD new_star.
    r = NEW #( kind = c_kind_star children = VALUE #( ( child ) ) ).
  ENDMETHOD.

  METHOD new_plus.
    r = NEW #( kind = c_kind_plus children = VALUE #( ( child ) ) ).
  ENDMETHOD.

  METHOD new_per.
    r = NEW #( kind = c_kind_per children = children ).
  ENDMETHOD.

  METHOD new_vers.
    r = NEW #( kind = c_kind_vers children = VALUE #( ( child ) ) ).
  ENDMETHOD.

  METHOD new_expr.
    r = NEW #( kind = c_kind_expr value = name ).
  ENDMETHOD.

  METHOD list_keywords.
    " 1:1 with combi.ts:
    "   Word.listKeywords()         → [this.s]
    "   WordSequence.listKeywords() → [this.stri]   (full phrase as one entry)
    "   Token / Regex               → []
    "   Sequence/Alt/Opt/Star/Plus/Per/Vers → recurse into children
    "   Expression                  → []  (handled by aggregator separately)
    CASE me->kind.
      WHEN c_kind_word OR c_kind_wseq.
        APPEND me->value TO result.
      WHEN c_kind_token OR c_kind_regex OR c_kind_expr.
        " no keywords contributed
        RETURN.
      WHEN OTHERS.
        LOOP AT me->children INTO DATA(child).
          IF child IS BOUND.
            APPEND LINES OF child->list_keywords( ) TO result.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
