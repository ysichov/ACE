"! Combinator factory — direct port of exported functions in abaplint combi.ts:
"!   str(), tok(), regex(), seq(), alt(), opt(), star(), plus(), per(), ver(), expr().
"! Returns ZCL_ACE_COMBI_NODE trees that can be walked via list_keywords( ).
CLASS zcl_ace_combi DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES tt_nodes TYPE zcl_ace_combi_node=>tt_children.

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

    "! regex(/.../) → Regex (no keyword)
    CLASS-METHODS regex
      IMPORTING pattern       TYPE string
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

    "! star( a ) — also covers starPrio
    CLASS-METHODS star
      IMPORTING child         TYPE REF TO zcl_ace_combi_node
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! plus( a ) — also covers plusPrio
    CLASS-METHODS plus
      IMPORTING child         TYPE REF TO zcl_ace_combi_node
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! per( a, b, ... )
    CLASS-METHODS per
      IMPORTING children      TYPE tt_nodes
      RETURNING VALUE(result) TYPE REF TO zcl_ace_combi_node.

    "! ver(version, a) / verNot — for keyword extraction we ignore the version
    "! filter (we want all keywords across all versions)
    CLASS-METHODS ver
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

  METHOD regex.
    result = zcl_ace_combi_node=>new_regex( pattern ).
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

  METHOD star.
    result = zcl_ace_combi_node=>new_star( child ).
  ENDMETHOD.

  METHOD plus.
    result = zcl_ace_combi_node=>new_plus( child ).
  ENDMETHOD.

  METHOD per.
    result = zcl_ace_combi_node=>new_per( children ).
  ENDMETHOD.

  METHOD ver.
    result = zcl_ace_combi_node=>new_vers( child ).
  ENDMETHOD.

  METHOD expr.
    result = zcl_ace_combi_node=>new_expr( name ).
  ENDMETHOD.

ENDCLASS.
