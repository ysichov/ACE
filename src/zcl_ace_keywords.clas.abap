"! Aggregates all ABAP keywords from the grammar definitions in
"! ZCL_ACE_STMTS (statements) and ZCL_ACE_EXPRS (expressions),
"! mirroring abaplint's Combi.listKeywords() walk over every getMatcher().
"!
"! Discovery: RTTI scan for class-methods named STMT_* and EXPR_*.
"! Expression resolution: when a node's kind = E (Expression reference),
"! the corresponding EXPR_<name>( ) method is invoked once and its tree
"! walked too. A visited set prevents infinite recursion.
"!
"! Multi-word phrases ("END OF", "INNER JOIN") are split into individual
"! words too — needed for token-by-token classification in metrics.
"! The full phrases are also kept (under is_phrase = abap_true) for callers
"! that need them.
CLASS zcl_ace_keywords DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_keyword,
        word      TYPE string,        " uppercased
        is_phrase TYPE abap_bool,     " abap_true if originally a multi-word str()
      END OF ts_keyword,
      tt_keywords TYPE HASHED TABLE OF ts_keyword WITH UNIQUE KEY word.

    "! Returns the union of all keyword literals reachable from any
    "! statement matcher or expression. Lazily computed and cached.
    CLASS-METHODS get_all
      RETURNING VALUE(result) TYPE tt_keywords.

    "! True iff the (case-insensitive) word is in the keyword set.
    "! Drop-in replacement for the static-list check in ZCL_ACE_METRICS.
    CLASS-METHODS is_keyword
      IMPORTING token         TYPE string
      RETURNING VALUE(result) TYPE abap_bool.

    "! Forces reset of the cache. Useful after adding new STMT_/EXPR_ methods.
    CLASS-METHODS reset.

  PRIVATE SECTION.

    CLASS-DATA mt_cache         TYPE tt_keywords.
    CLASS-DATA mv_cached        TYPE abap_bool.
    " Names of expressions already walked during the current build,
    " to prevent infinite recursion through Expression references.
    CLASS-DATA mt_visited_exprs TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    CLASS-METHODS build.

    CLASS-METHODS collect_from_class
      IMPORTING class_name   TYPE string
                method_prefix TYPE string.

    CLASS-METHODS walk_node
      IMPORTING node TYPE REF TO zcl_ace_combi_node.

    CLASS-METHODS add_keyword
      IMPORTING raw TYPE string.

ENDCLASS.


CLASS zcl_ace_keywords IMPLEMENTATION.

  METHOD get_all.
    IF mv_cached = abap_false.
      build( ).
    ENDIF.
    result = mt_cache.
  ENDMETHOD.

  METHOD is_keyword.
    IF mv_cached = abap_false.
      build( ).
    ENDIF.
    DATA(up) = to_upper( token ).
    result = boolc( line_exists( mt_cache[ word = up ] ) ).
  ENDMETHOD.

  METHOD reset.
    CLEAR: mt_cache, mv_cached, mt_visited_exprs.
  ENDMETHOD.

  METHOD build.
    CLEAR: mt_cache, mt_visited_exprs.
    collect_from_class( class_name    = 'ZCL_ACE_STMTS'
                        method_prefix = 'STMT_' ).
    collect_from_class( class_name    = 'ZCL_ACE_EXPRS'
                        method_prefix = 'EXPR_' ).
    mv_cached = abap_true.
  ENDMETHOD.

  METHOD collect_from_class.
    " RTTI: enumerate all class-methods matching the prefix and dynamically invoke each
    DATA(class_descr) = CAST cl_abap_classdescr(
      cl_abap_typedescr=>describe_by_name( class_name ) ).

    LOOP AT class_descr->methods INTO DATA(method_descr).
      CHECK method_descr-name CP |{ method_prefix }*|.
      CHECK method_descr-is_class = abap_true.
      CHECK method_descr-visibility = cl_abap_classdescr=>public.

      DATA node TYPE REF TO zcl_ace_combi_node.
      TRY.
          CALL METHOD (class_name)=>(method_descr-name)
            RECEIVING
              r = node.
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      " For Expression methods (EXPR_*) mark as visited so that recursive
      " references back through expr( ) don't re-walk them.
      IF method_prefix = 'EXPR_'.
        DATA(expr_name) = substring( val = method_descr-name
                                     off = strlen( method_prefix ) ).
        INSERT expr_name INTO TABLE mt_visited_exprs.
      ENDIF.

      walk_node( node ).
    ENDLOOP.
  ENDMETHOD.

  METHOD walk_node.
    CHECK node IS BOUND.

    " Mirrors zcl_ace_combi_node->list_keywords( ) but ALSO follows
    " Expression references so we capture keywords contributed by
    " sub-expressions reached only via expr( ).
    CASE node->kind.
      WHEN zcl_ace_combi_node=>c_kind_word
        OR zcl_ace_combi_node=>c_kind_wseq.
        add_keyword( node->value ).

      WHEN zcl_ace_combi_node=>c_kind_token
        OR zcl_ace_combi_node=>c_kind_regex.
        " contributes nothing
        RETURN.

      WHEN zcl_ace_combi_node=>c_kind_expr.
        " Expression reference — walk the referenced expression once
        DATA(name) = node->value.
        IF line_exists( mt_visited_exprs[ table_line = name ] ).
          RETURN.
        ENDIF.
        INSERT name INTO TABLE mt_visited_exprs.
        DATA(method_name) = |EXPR_{ name }|.
        DATA child TYPE REF TO zcl_ace_combi_node.
        TRY.
            CALL METHOD ('ZCL_ACE_EXPRS')=>(method_name)
              RECEIVING
                r = child.
            walk_node( child ).
          CATCH cx_root.
            " Expression not yet ported — skip
            RETURN.
        ENDTRY.

      WHEN OTHERS.
        " seq / alt / opt / star / plus / per / vers — recurse into children
        LOOP AT node->children INTO DATA(c).
          walk_node( c ).
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.

  METHOD add_keyword.
    DATA(up) = to_upper( raw ).
    " Always store the literal (single word OR full phrase) — useful for callers
    " that want exact phrase matching (e.g. "END OF").
    DATA is_phrase TYPE abap_bool.
    is_phrase = boolc( up CS ` ` OR up CS `-` ).
    INSERT VALUE ts_keyword( word = up is_phrase = is_phrase )
      INTO TABLE mt_cache.

    " For multi-word phrases also store individual words so that token-by-token
    " classification (1 token from the scanner) finds them.
    IF is_phrase = abap_true.
      DATA(parts) = up.
      REPLACE ALL OCCURRENCES OF `-` IN parts WITH ` `.
      SPLIT parts AT ` ` INTO TABLE DATA(words).
      LOOP AT words INTO DATA(w).
        CHECK w IS NOT INITIAL.
        INSERT VALUE ts_keyword( word = w is_phrase = abap_false )
          INTO TABLE mt_cache.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
