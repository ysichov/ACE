"! Combinator factory — port of the exported functions in abaplint combi.ts.
"! Covers the combinators the ported grammar uses: str(), tok(), seq(), alt(),
"! opt(), expr(). The remaining combi.ts combinators (regex, star, plus, per,
"! ver) are not needed by any rule ported so far; add them here together with
"! the matching factory in ZCL_ACE_COMBI_NODE when a rule requires one.
"! Returns ZCL_ACE_COMBI_NODE trees, walked by ZCL_ACE_KEYWORDS.
CLASS zcl_ace_combi DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    " Declared via REF TO (not zcl_ace_combi_node=>tt_children) so the merged
    " standalone works — a DEFERRED class allows REF TO but not =>type access.
    TYPES tt_nodes TYPE STANDARD TABLE OF REF TO zcl_ace_combi_node WITH EMPTY KEY.

    "! str("WORD")  → Word        (single literal)
    "! str("END OF") → WordSequence (multi-word phrase, 1 entry in listKeywords)
    "! Replicates: indexOf(" ")>0 || indexOf("-")>0 → WordSequence else Word
    CLASS-METHODS str
      IMPORTING s             TYPE string
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! tok(TokenClassName) → Token (matches by token class name, no keyword)
    CLASS-METHODS tok
      IMPORTING token_name    TYPE string
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.


    "! seq( a, b, c, ... )
    CLASS-METHODS seq
      IMPORTING children      TYPE tt_nodes
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! alt( a, b, c, ... )  — also covers altPrio (same keywords)
    CLASS-METHODS alt
      IMPORTING children      TYPE tt_nodes
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! opt( a )  — also covers optPrio
    CLASS-METHODS opt
      IMPORTING child         TYPE REF TO zcl_ace_combi_node
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.


    "! Reference to an Expression class — by name (e.g. 'COND', 'SOURCE', 'TARGET').
    "! In abaplint, mapInput(s) auto-instantiates the Expression. In ABAP we use
    "! a string name and resolve via dynamic call zcl_ace_exprs=>expr_<name>( ).
    CLASS-METHODS expr
      IMPORTING name          TYPE string
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

ENDCLASS.


CLASS zcl_ace_combi IMPLEMENTATION.

  METHOD str.
    " Mirrors combi.ts: if (s.indexOf(" ") > 0 || s.indexOf("-") > 0) WordSequence else Word
    IF s CS ` ` OR s CS `-`.
      result = zcl_ace_combi_node=>new_wseq( s ).
    ELSE.
      result = zcl_ace_combi_node=>new_word( s ).
    ENDIF.
  ENDMETHOD.

  METHOD tok.
    result = zcl_ace_combi_node=>new_token( token_name ).
  ENDMETHOD.

  METHOD seq.
    result = zcl_ace_combi_node=>new_seq( children ).
  ENDMETHOD.

  METHOD alt.
    result = zcl_ace_combi_node=>new_alt( children ).
  ENDMETHOD.

  METHOD opt.
    result = zcl_ace_combi_node=>new_opt( child ).
  ENDMETHOD.

  METHOD expr.
    result = zcl_ace_combi_node=>new_expr( name ).
  ENDMETHOD.

ENDCLASS.
