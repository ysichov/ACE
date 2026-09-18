"! <p class="shorttext synchronized">Code-flow diagram, without a GUI</p>
"! The flow picture as a pure function of the step table and the parse. It
"! was a class-method of ZCL_ACE_MERMAID, which inherits from ZCL_ACE_POPUP
"! and so could not be loaded where there is no SAP GUI - although the
"! drawing itself never touched a control. ZCL_ACE_MERMAID keeps the window
"! and calls this; an ADT resource calls it with no window at all.
CLASS zcl_ace_flow DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Maps a mermaid node id to the source unit it represents, so a click
    " on the rendered block can open its source in a popup.
    TYPES:
      BEGIN OF ts_node_map,
        node_id TYPE string,
        class   TYPE string,
        event   TYPE string,
        name    TYPE string,
        include TYPE program,
      END OF ts_node_map .
    TYPES tt_node_map TYPE STANDARD TABLE OF ts_node_map WITH KEY node_id .
    " The step table as a drawing takes it. ZIF_ACE_WALK keeps its own with a
    " longer key; this is the plain list, which is all the picture needs.
    TYPES tt_flow_steps TYPE STANDARD TABLE OF zif_ace_parse_data=>ts_step_counter WITH EMPTY KEY .

    " Which unit calls which, in the order the code would run. It is a
    " function of the steps and the parse and of nothing else - no window, no
    " viewer, no control - so it also answers where there is no SAP GUI at all.
    " CHANGING, not IMPORTING: drawing the focused map dips back into the
    " parser for call bindings that were never resolved, and that fills them
    " into the parse. The caller keeps the work rather than paying for it again.
    CLASS-METHODS build_steps_flow
      IMPORTING
        !it_steps      TYPE tt_flow_steps
        !i_direction   TYPE ui_func OPTIONAL
        !i_with_params TYPE boolean OPTIONAL
        !i_all_methods TYPE boolean DEFAULT abap_false
        !i_type        TYPE string DEFAULT 'CALLS'
        !i_focus       TYPE progname OPTIONAL
      EXPORTING
        !et_node_map   TYPE tt_node_map
      CHANGING
        !cs_parse_data TYPE zif_ace_parse_data=>ts_parse_data
      RETURNING
        VALUE(rv_mm)   TYPE string .

    " Strips CR/LF/TAB from a node label - such characters leak in from
    " CRLF source tokens and break mermaid parsing inside a label string.
    CLASS-METHODS clean_label
      IMPORTING
        !i_text        TYPE string
      RETURNING
        VALUE(rv_text) TYPE string .

  PRIVATE SECTION.

    " Static: the drawing is a class-method, and the short form it calls them
    " by is only open to static methods. Neither touches an instance.
    CLASS-METHODS format_node_label
      IMPORTING
        !i_code         TYPE string
        !i_maxlen       TYPE i DEFAULT 50
      RETURNING
        VALUE(rv_label) TYPE string .

ENDCLASS.


CLASS zcl_ace_flow IMPLEMENTATION.

  METHOD build_steps_flow.

    TYPES: BEGIN OF lty_entity,
             include   TYPE string,
             class     TYPE string,
             event     TYPE string,
             name      TYPE string,
             style     TYPE string,
             eventname TYPE string,   " raw method/form/function name for binding lookup
           END OF lty_entity,
           BEGIN OF t_ind,
             from TYPE i,
             to   TYPE i,
           END OF t_ind,
           BEGIN OF t_stack_entry,
             stacklevel TYPE i,
             entity_idx TYPE i,    " index of the node in the entities table
             name       TYPE string,
           END OF t_stack_entry.

    CONSTANTS: c_style_event    TYPE string VALUE 'event',
               c_style_method   TYPE string VALUE 'method',
               c_style_form     TYPE string VALUE 'form',
               c_style_constr   TYPE string VALUE 'constr',
               c_style_enh      TYPE string VALUE 'enh',
               c_style_function TYPE string VALUE 'func'.

    DATA: mm_string    TYPE string,
          entities     TYPE TABLE OF lty_entity,
          entity       TYPE lty_entity,
          ind          TYPE t_ind,
          indexes      TYPE TABLE OF t_ind,
          ids_event    TYPE TABLE OF string,
          ids_method   TYPE TABLE OF string,
          ids_form     TYPE TABLE OF string,
          ids_constr   TYPE TABLE OF string,
          ids_enh      TYPE TABLE OF string,
          ids_function TYPE TABLE OF string,
          call_stack   TYPE TABLE OF t_stack_entry.

    DATA(copy) = it_steps.
    CLEAR et_node_map.


    " Toggle OFF (default) → aggregate the flow to program/class blocks;
    " Toggle ON ("All Blocks") → keep event/form/method-level detail.
    DATA(lv_agg) = xsdbool( i_all_methods = abap_false ).

    " ── Step 1: collect unique nodes ────────────────────────────────
    LOOP AT copy ASSIGNING FIELD-SYMBOL(<copy>).
      entity-event     = <copy>-eventtype.
      entity-eventname = <copy>-eventname.   " save raw name before overwrite below

      IF lv_agg = abap_true.
        " Collapse to the owning class (methods) or program (everything else).
        " Never emit an empty label — mermaid rejects `id("")` and the whole
        " diagram fails to parse; fall back to include / eventname / '?'.
        IF <copy>-eventtype = 'METHOD' AND <copy>-class IS NOT INITIAL.
          DATA(lv_agg_lbl) = CONV string( <copy>-class ).
          entity-style = c_style_method.
        ELSE.
          lv_agg_lbl = COND string(
            WHEN <copy>-program   IS NOT INITIAL THEN CONV string( <copy>-program )
            WHEN <copy>-class     IS NOT INITIAL THEN <copy>-class
            WHEN <copy>-include   IS NOT INITIAL THEN CONV string( <copy>-include )
            WHEN <copy>-eventname IS NOT INITIAL THEN <copy>-eventname
            ELSE '?' ).
          entity-style = c_style_event.
        ENDIF.
        entity-name  = |"{ lv_agg_lbl }"|.
        entity-class = lv_agg_lbl.
        <copy>-eventname = entity-name.
        entity-include   = ''.   " collapse across includes of the same unit

      ELSEIF <copy>-eventtype = 'METHOD'.
        READ TABLE cs_parse_data-tt_calls_line
          WITH KEY include   = <copy>-include
                   eventtype = 'METHOD'
                   eventname = <copy>-eventname
                   class     = <copy>-class
          INTO DATA(call_line).
        entity-name  = |"{ call_line-class }->{ <copy>-eventname }"|.
        entity-style = COND string(
          WHEN <copy>-eventname = 'CONSTRUCTOR' OR <copy>-eventname = 'CLASS_CONSTRUCTOR'
          THEN c_style_constr ELSE c_style_method ).
        <copy>-eventname = entity-name.
        entity-include = <copy>-include.
        entity-class   = <copy>-class.

      ELSE.
        entity-name = SWITCH string( <copy>-eventtype
          WHEN 'FUNCTION'    THEN |"FUNCTION:{ <copy>-eventname }"|
          WHEN 'SCREEN'      THEN |"CALL SCREEN { <copy>-eventname }"|
          WHEN 'MODULE'      THEN |"MODULE { <copy>-eventname }"|
          WHEN 'FORM'        THEN |"FORM { <copy>-eventname }"|
          WHEN 'ENHANCEMENT' THEN |"ENH { <copy>-eventname }"|
          ELSE                    |"{ <copy>-program }:{ <copy>-eventname }"| ).
        entity-style = SWITCH string( <copy>-eventtype
          WHEN 'FUNCTION'    THEN c_style_function
          WHEN 'FORM'        THEN c_style_form
          WHEN 'ENHANCEMENT' THEN c_style_enh
          WHEN 'MODULE'      THEN c_style_form
          ELSE                    c_style_event ).
        <copy>-eventname   = entity-name.
        entity-include = <copy>-include.
        entity-class   = <copy>-class.
      ENDIF.

      READ TABLE entities
        WITH KEY include = entity-include class = entity-class name = entity-name
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND entity TO entities.
        DATA(lv_node_id) = |{ lines( entities ) }|.
        CASE entity-style.
          WHEN c_style_event.    APPEND lv_node_id TO ids_event.
          WHEN c_style_method.   APPEND lv_node_id TO ids_method.
          WHEN c_style_form.     APPEND lv_node_id TO ids_form.
          WHEN c_style_constr.   APPEND lv_node_id TO ids_constr.
          WHEN c_style_enh.      APPEND lv_node_id TO ids_enh.
          WHEN c_style_function. APPEND lv_node_id TO ids_function.
        ENDCASE.
        " Remember the real source unit behind this node for click navigation
        " (use the raw step values, not the aggregated label).
        APPEND VALUE ts_node_map( node_id = lv_node_id
                                  class   = <copy>-class
                                  event   = entity-event
                                  name    = entity-eventname
                                  include = <copy>-include ) TO et_node_map.
      ENDIF.
    ENDLOOP.

    mm_string = |graph { COND string( WHEN i_direction IS NOT INITIAL THEN i_direction ELSE 'TD' ) }\n |.

    " ── Step 2: declare every node explicitly ──────────────────────
    DATA(lv_idx) = 0.
    LOOP AT entities INTO entity.
      lv_idx += 1.
      " Strip control chars, then guard against an empty label `id("")`
      " — both break mermaid parsing.
      DATA(lv_lbl) = clean_label( entity-name ).
      IF lv_lbl IS INITIAL OR lv_lbl = `""` OR lv_lbl = `" "`.
        lv_lbl = `"?"`.
      ENDIF.
      mm_string = |{ mm_string }{ lv_idx }({ lv_lbl })\n|.
    ENDLOOP.

    " ── Step 3: draw the arrows from an explicit call stack ────────
    " call_stack holds one node per level.
    " When a step arrives with stacklevel = N:
    "   - caller = the node held at level N-1
    "   - draw caller → current node (unless already drawn)
    "   - update the stack: level N now holds the current node

    DATA lv_prev_stack TYPE i.

    LOOP AT copy INTO DATA(step2).

      READ TABLE entities
        WITH KEY name = step2-eventname
        TRANSPORTING NO FIELDS.
      DATA(lv_cur_idx) = sy-tabix.

      DATA(lv_level) = step2-stacklevel.

      " Find the caller — the call_stack node at level lv_level - 1
      IF lv_level > 1.
        READ TABLE call_stack
          WITH KEY stacklevel = lv_level - 1
          INTO DATA(ls_caller).
        IF sy-subrc = 0 AND ls_caller-entity_idx <> lv_cur_idx.
          " Draw edge only if not yet drawn
          ind-from = ls_caller-entity_idx.
          ind-to   = lv_cur_idx.
          READ TABLE indexes WITH KEY from = ind-from to = ind-to TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            DATA(lv_edge_label) = ``.

            IF i_with_params = abap_true.
              " Look up parameter bindings: search caller's keywords for a call to callee
              DATA(ls_caller_ent) = entities[ ind-from ].
              DATA(ls_callee_ent) = entities[ ind-to ].
              READ TABLE cs_parse_data-tt_progs
                WITH KEY include = ls_caller_ent-include
                INTO DATA(ls_prog_wp).
              IF sy-subrc = 0.
                LOOP AT ls_prog_wp-t_keywords INTO DATA(ls_kw_wp).
                  LOOP AT ls_kw_wp-tt_calls INTO DATA(ls_call_wp)
                    WHERE name  = ls_callee_ent-eventname
                      AND class = ls_callee_ent-class.
                    LOOP AT ls_call_wp-bindings INTO DATA(ls_bind_wp).
                      IF lv_edge_label IS INITIAL.
                        lv_edge_label = |{ ls_bind_wp-inner }={ ls_bind_wp-outer }|.
                      ELSE.
                        lv_edge_label = |{ lv_edge_label }<br/>{ ls_bind_wp-inner }={ ls_bind_wp-outer }|.
                      ENDIF.
                    ENDLOOP.
                    IF lv_edge_label IS NOT INITIAL. EXIT. ENDIF.
                  ENDLOOP.
                  IF lv_edge_label IS NOT INITIAL. EXIT. ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.

            IF lv_edge_label IS NOT INITIAL.
              DATA(lv_el_fmt) = format_node_label( i_code = lv_edge_label i_maxlen = 0 ).
              mm_string = |{ mm_string }{ ind-from } -->\|"{ lv_el_fmt }"\|{ ind-to }\n|.
            ELSE.
              mm_string = |{ mm_string }{ ind-from } --> { ind-to }\n|.
            ENDIF.
            APPEND ind TO indexes.
          ENDIF.
        ENDIF.
      ENDIF.

      " Update the stack: drop every level >= lv_level, then push the current one
      DELETE call_stack WHERE stacklevel >= lv_level.
      APPEND VALUE t_stack_entry(
        stacklevel = lv_level
        entity_idx = lv_cur_idx
        name       = step2-eventname
      ) TO call_stack.

      lv_prev_stack = lv_level.
    ENDLOOP.

    " ── Step 4: styles ──────────────────────────────────────────────
    IF i_type = 'CMAP' AND i_focus IS NOT INITIAL.
      DATA(lv_enrich_from) = 0.
      LOOP AT entities INTO DATA(ls_enrich_src).
        lv_enrich_from += 1.
        CHECK ls_enrich_src-style = c_style_method.
        READ TABLE cs_parse_data-tt_calls_line
          WITH KEY include   = ls_enrich_src-include
                   eventtype = 'METHOD'
                   eventname = ls_enrich_src-eventname
                   class     = ls_enrich_src-class
          INTO DATA(ls_enrich_line).
        CHECK sy-subrc = 0.
        READ TABLE cs_parse_data-tt_progs
          WITH KEY include = ls_enrich_line-include
          INTO DATA(ls_enrich_prog).
        CHECK sy-subrc = 0.

        LOOP AT ls_enrich_prog-t_keywords INTO DATA(ls_enrich_kw)
          WHERE index >= ls_enrich_line-index AND index <= ls_enrich_line-end_idx.
          IF ls_enrich_kw-calls_parsed = abap_false.
            zcl_ace_parser=>parse_tokens(
              EXPORTING
                i_program  = CONV #( ls_enrich_kw-program )
                i_include  = CONV #( ls_enrich_kw-include )
                i_stmt_idx = ls_enrich_kw-index
                i_class    = ls_enrich_src-class
                i_evtype   = 'METHOD'
                i_ev_name  = ls_enrich_src-eventname
              CHANGING
                cs_source  = cs_parse_data ).
            READ TABLE ls_enrich_prog-t_keywords WITH KEY index = ls_enrich_kw-index INTO ls_enrich_kw.
          ENDIF.

          LOOP AT ls_enrich_kw-tt_calls INTO DATA(ls_enrich_call)
            WHERE event = 'METHOD' AND name IS NOT INITIAL.
            DATA(lv_enrich_to) = 0.
            LOOP AT entities INTO DATA(ls_enrich_tgt).
              CHECK ls_enrich_tgt-style = c_style_method
                AND ls_enrich_tgt-eventname = ls_enrich_call-name.
              IF ls_enrich_call-class IS NOT INITIAL
                 AND to_upper( ls_enrich_tgt-class ) <> to_upper( CONV string( ls_enrich_call-class ) ).
                CONTINUE.
              ENDIF.
              lv_enrich_to = sy-tabix.
              EXIT.
            ENDLOOP.
            CHECK lv_enrich_to > 0 AND lv_enrich_to <> lv_enrich_from.
            READ TABLE indexes WITH KEY from = lv_enrich_from to = lv_enrich_to TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              mm_string = |{ mm_string }{ lv_enrich_from } -.-> { lv_enrich_to }\n|.
              APPEND VALUE #( from = lv_enrich_from to = lv_enrich_to ) TO indexes.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    mm_string = |{ mm_string } classDef event    fill:#FFE0B2,stroke:#E65100\n|.
    mm_string = |{ mm_string } classDef method   fill:#BBDEFB,stroke:#1565C0\n|.
    mm_string = |{ mm_string } classDef form     fill:#EEEEEE,stroke:#616161\n|.
    mm_string = |{ mm_string } classDef constr   fill:#E1BEE7,stroke:#6A1B9A\n|.
    mm_string = |{ mm_string } classDef enh      fill:#FCE4EC,stroke:#AD1457\n|.
    mm_string = |{ mm_string } classDef func     fill:#C8E6C9,stroke:#2E7D32\n|.

    DATA(lv_ids) = ``.
    IF ids_event    IS NOT INITIAL. CONCATENATE LINES OF ids_event    INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } event\n|.    ENDIF.
    IF ids_method   IS NOT INITIAL. CONCATENATE LINES OF ids_method   INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } method\n|.   ENDIF.
    IF ids_form     IS NOT INITIAL. CONCATENATE LINES OF ids_form     INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } form\n|.     ENDIF.
    IF ids_constr   IS NOT INITIAL. CONCATENATE LINES OF ids_constr   INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } constr\n|.   ENDIF.
    IF ids_enh      IS NOT INITIAL. CONCATENATE LINES OF ids_enh      INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } enh\n|.      ENDIF.
    IF ids_function IS NOT INITIAL. CONCATENATE LINES OF ids_function INTO lv_ids SEPARATED BY ','. mm_string = |{ mm_string } class { lv_ids } func\n|.     ENDIF.

    mm_string = |{ mm_string }\n|.
    rv_mm = mm_string.

  ENDMETHOD.

  method FORMAT_NODE_LABEL.

    CONSTANTS lc_br    TYPE string VALUE `<br/>`.
    CONSTANTS lc_br_ph TYPE string VALUE `##BR##`.

    " Effective max length: use parameter value, but treat 50 (old default) as 100
    "DATA(lv_maxlen) = COND i( WHEN I_MAXLEN = 50 OR I_MAXLEN = 0 THEN 100 ELSE I_MAXLEN ).
DATA(lv_maxlen) = 200.
    RV_LABEL = I_CODE.

    " Protect existing <br/> tags before any text transformations
    REPLACE ALL OCCURRENCES OF lc_br IN RV_LABEL WITH lc_br_ph IN CHARACTER MODE.

    " Truncate only if lv_maxlen > 0
    IF lv_maxlen > 0 AND strlen( RV_LABEL ) > lv_maxlen.
      RV_LABEL = RV_LABEL+0(lv_maxlen).
    ENDIF.

    REPLACE ALL OCCURRENCES OF `PERFORM`       IN RV_LABEL WITH `FORM`     IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `CALL FUNCTION` IN RV_LABEL WITH `FUNCTION` IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `CALL METHOD`   IN RV_LABEL WITH `METHOD`   IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF `-`             IN RV_LABEL WITH ` `        IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF ` `             IN RV_LABEL WITH `&nbsp;`   IN CHARACTER MODE.

    " Restore <br/> tags
    REPLACE ALL OCCURRENCES OF lc_br_ph IN RV_LABEL WITH lc_br IN CHARACTER MODE.

  endmethod.

  method CLEAN_LABEL.
    rv_text = i_text.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN rv_text WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN rv_text WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv_text WITH ` `.
    " bare CR (first byte of CR_LF) if it survived on its own
    DATA(lv_cr) = substring( val = cl_abap_char_utilities=>cr_lf off = 0 len = 1 ).
    REPLACE ALL OCCURRENCES OF lv_cr IN rv_text WITH ` `.
    CONDENSE rv_text.
  endmethod.

ENDCLASS.
