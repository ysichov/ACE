"! Renders ABAP source as HTML for the code window's "HTML view" mode.
"!
"! Two things the SAP GUI editor control (CL_GUI_ABAPEDIT) cannot do and
"! that this renderer provides:
"!   * every identifier (call, variable, object name) becomes a hyperlink
"!     that reports its viewer line back through sapevent, so the window
"!     can reuse the very same navigation logic as a double-click;
"!   * branch-aware folding — an IF can be collapsed down to its ELSEIF /
"!     ELSE headers and a CASE down to its WHEN headers, each of which can
"!     then be expanded on its own.
"!
"! Segment model: for every foldable header line the builder precomputes
"! the last line of the block that belongs to that header (data-end).
"! For IF/ELSEIF/ELSE and WHEN that block stops at the *next branch header
"! of the same nesting depth*, which is exactly what gives the "collapse to
"! the branch headers" behaviour. Folding itself is plain JS on data-end.
CLASS zcl_ace_code_html DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES tt_lines TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    "! Builds a complete HTML page for the given source lines.
    "! @parameter it_source | source as shown in the editor (1 line = 1 viewer line)
    "! @parameter i_title   | caption printed above the code
    "! @parameter it_kw     | parsed statement keywords of the include; folding
    "!                        is derived from these, never from the raw text
    "! @parameter it_bp_s   | viewer lines carrying a session breakpoint
    "! @parameter it_bp_e   | viewer lines carrying an external breakpoint
    CLASS-METHODS build
      IMPORTING it_source     TYPE STANDARD TABLE
                it_kw         TYPE zif_ace_parse_data=>tt_kword OPTIONAL
                io_scan       TYPE REF TO cl_ci_scan OPTIONAL
                i_title       TYPE string OPTIONAL
                it_bp_s       TYPE tt_lines OPTIONAL
                it_bp_e       TYPE tt_lines OPTIONAL
                i_focus       TYPE i OPTIONAL
                i_folded      TYPE abap_bool DEFAULT abap_false
                i_offset      TYPE i DEFAULT 1
      RETURNING VALUE(rt_html) TYPE w3htmltab.

    "! Mermaid flowchart of the control structure of the same source:
    "! IF/ELSEIF/ELSE, CASE/WHEN, LOOP/DO/WHILE, TRY/CATCH and the enclosing
    "! METHOD/FORM. Plain statements are left out — the point is the shape of
    "! the branch, not its every line.
    CLASS-METHODS build_scheme
      IMPORTING it_source     TYPE STANDARD TABLE
                it_kw         TYPE zif_ace_parse_data=>tt_kword OPTIONAL
                io_scan       TYPE REF TO cl_ci_scan OPTIONAL
                i_title       TYPE string OPTIONAL
                it_expanded   TYPE tt_lines OPTIONAL
                i_expand_all  TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_mm)  TYPE string.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_line,
        line  TYPE i,
        text  TYPE string,
        word  TYPE string,   " first word, uppercased
        depth TYPE i,
        kind  TYPE char1,   " 'O'=opener 'B'=branch 'C'=closer 'P'=plain
                            " 'S'=whole block on one line (scheme only)
        end   TYPE i,       " last line of this header's own branch segment
        all   TYPE i,       " openers: last line before the matching closer
        call  TYPE abap_bool, " statement invokes something (from TT_CALLS)
        call_names TYPE string, " what it invokes, blank separated, uppercase
      END OF ts_line,
      tt_line TYPE STANDARD TABLE OF ts_line WITH EMPTY KEY.

    " Openers are only accepted once their own closer has actually been
    " found. SELECT and AT matter here: "SELECT ... INTO TABLE" and
    " "AT line-selection" have no closer, and counting them as blocks used
    " to shift the nesting level of everything that followed.
    TYPES:
      BEGIN OF ts_open,
        line   TYPE i,
        word   TYPE string,
        closer TYPE string,
      END OF ts_open,
      tt_open TYPE STANDARD TABLE OF ts_open WITH EMPTY KEY.

    " One entry per statement, in program order — several statements can
    " share a line ("IF x. y. ENDIF." written on one line).
    TYPES:
      BEGIN OF ts_stmt,
        index TYPE i,
        line  TYPE i,
        word  TYPE string,
      END OF ts_stmt,
      tt_stmt TYPE STANDARD TABLE OF ts_stmt WITH EMPTY KEY.

    " A confirmed block: opener and closer found, on different lines.
    TYPES:
      BEGIN OF ts_block,
        open  TYPE i,
        close TYPE i,
        word  TYPE string,
      END OF ts_block,
      tt_block TYPE STANDARD TABLE OF ts_block WITH EMPTY KEY.

    " Quote characters as constants: literals like `'` and '`' inline in the
    " code are the kind of thing that trips up serialization round-trips.
    CONSTANTS c_apos  TYPE c LENGTH 1 VALUE ''''.
    CONSTANTS c_btick TYPE c LENGTH 1 VALUE '`'.
    " A blank MUST stay a string literal: as TYPE c its trailing blank is
    " stripped, which turns a REPLACE pattern into an empty one and loops
    " forever (CX_SY_REPLACE_INFINITE_LOOP).
    CONSTANTS c_blank TYPE string VALUE ` `.

    " Declarations are not execution: they never appear in the scheme and do
    " not count towards the "N operations" between two branches.
    CONSTANTS c_decls TYPE string VALUE ' DATA CLASS-DATA CONSTANTS STATICS FIELD-SYMBOLS TYPES TYPE-POOLS TABLES RANGES INCLUDE METHODS CLASS-METHODS EVENTS INTERFACES ALIASES DEFINE PARAMETERS SELECT-OPTIONS NODES INFOTYPES '.

    CONSTANTS c_branches TYPE string VALUE
      ' ELSEIF ELSE WHEN CATCH CLEANUP '.
    CONSTANTS c_closers TYPE string VALUE
      ' ENDIF ENDCASE ENDLOOP ENDDO ENDWHILE ENDTRY ENDMETHOD ENDFORM ENDMODULE ENDSELECT ENDAT ENDPROVIDE '.

    "! Classifies every line and computes the fold segments.
    "! i_offset: first source line shown, when it_source is only a slice of
    "! the include (the source popup shows one method) — scan rows are
    "! absolute and have to be shifted onto the slice.
    CLASS-METHODS analyze
      IMPORTING it_source      TYPE STANDARD TABLE
                it_kw          TYPE zif_ace_parse_data=>tt_kword
                io_scan        TYPE REF TO cl_ci_scan OPTIONAL
                i_offset       TYPE i DEFAULT 1
      RETURNING VALUE(rt_lines) TYPE tt_line.

    "! First word of a statement line, uppercased ('' for comments/blank).
    "! Only used when the parser's keyword table is unavailable.
    CLASS-METHODS first_word
      IMPORTING i_text        TYPE string
      RETURNING VALUE(r_word) TYPE string.

    "! Closing keyword expected for an opening one, '' if not a block opener.
    CLASS-METHODS closer_of
      IMPORTING i_word         TYPE string
      RETURNING VALUE(r_closer) TYPE string.

    "! Source line → syntax-highlighted HTML with identifier hyperlinks.
    CLASS-METHODS render_line
      IMPORTING i_text        TYPE string
                i_line        TYPE i
                i_calls       TYPE string OPTIONAL
      RETURNING VALUE(r_html) TYPE string.

    CLASS-METHODS escape
      IMPORTING i_text        TYPE string
      RETURNING VALUE(r_text) TYPE string.

    "! Mermaid arrow, with the branch condition on it when there is one.
    CLASS-METHODS arrow
      IMPORTING i_label       TYPE string
      RETURNING VALUE(r_text) TYPE string.

    "! Emits an "N operations" node for the executable statements between two
    "! lines and wires it after i_prev. Returns the node the chain now ends
    "! on — the new node, or i_prev when there was nothing in between.
    CLASS-METHODS ops_node
      IMPORTING i_from        TYPE i
                i_to          TYPE i
                i_id          TYPE string
                i_prev        TYPE string
                i_label       TYPE string OPTIONAL
                it_ops        TYPE tt_lines
                it_lines      TYPE tt_line
                it_expanded   TYPE tt_lines OPTIONAL
                i_expand_all  TYPE abap_bool DEFAULT abap_false
      CHANGING  cv_mm         TYPE string
                cv_edges      TYPE string
                cv_clicks     TYPE string
                cv_styles     TYPE string
      RETURNING VALUE(r_node) TYPE string.

    "! Renders the statements collected since the last call as one node —
    "! their count, or the statement itself when there is only one.
    CLASS-METHODS flush_pending
      IMPORTING it_pend       TYPE tt_lines
                it_lines      TYPE tt_line
                i_anchor      TYPE i
                i_prev        TYPE string
                i_label       TYPE string OPTIONAL
      CHANGING  cv_mm         TYPE string
                cv_edges      TYPE string
                cv_clicks     TYPE string
                cv_first      TYPE string
      RETURNING VALUE(r_node) TYPE string.

    "! Whole statement text: the line plus its continuation lines, which
    "! carry no keyword of their own. A call shown as just "obj->meth(" says
    "! nothing about what is passed to it.
    CLASS-METHODS stmt_text
      IMPORTING it_lines      TYPE tt_line
                i_line        TYPE i
      RETURNING VALUE(r_text) TYPE string.

    "! Source text of a structure line, trimmed and stripped of the
    "! characters that would break a mermaid label.
    CLASS-METHODS scheme_label
      IMPORTING i_text        TYPE string
      RETURNING VALUE(r_text) TYPE string.

    "! Appends a string to the w3htmltab, splitting on the 255 char limit.
    CLASS-METHODS add
      IMPORTING i_text TYPE string
      CHANGING  ct_html TYPE w3htmltab.

ENDCLASS.



CLASS zcl_ace_code_html IMPLEMENTATION.


  METHOD build.

    DATA(lt_lines) = analyze( it_source = it_source it_kw = it_kw io_scan = io_scan i_offset = i_offset ).

    add( EXPORTING i_text = '<html><head><meta http-equiv="X-UA-Compatible" content="IE=edge">'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '<style>' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'body{font-family:Consolas,"Courier New",monospace;font-size:12px;' &&
                            'background:#ffffff;color:#000000;margin:0;padding:0}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.hdr{background:#eef3f8;border-bottom:1px solid #c0c8d0;padding:3px 6px;' &&
                            'font-weight:bold}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.hdr button{font-family:inherit;font-size:11px;margin-left:6px}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.ratio{float:right;font-weight:normal;color:#40607f}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.ln{white-space:pre;padding-left:2px}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.ln:hover{background:#f2f7ff}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.num{display:inline-block;width:44px;text-align:right;color:#808080;' &&
                            'background:#f4f4f4;margin-right:6px;cursor:pointer}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.num:hover{background:#dde6f0;color:#000000}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.bp{display:inline-block;width:9px;height:9px;margin-right:3px;' &&
                            'border-radius:5px;vertical-align:middle}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.bps{background:#d00000}.bpe{background:#0060d0}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.tg{display:inline-block;width:12px;color:#3070c0;cursor:pointer;' &&
                            'font-weight:bold}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.tgx{display:inline-block;width:12px}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.k{color:#0000c0;font-weight:bold}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.c{color:#3f7f5f;font-style:italic}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.s{color:#a31515}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.n{color:#098658}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'a.id{color:#101010;text-decoration:none;border-bottom:1px dotted #8090a0}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'a.id:hover{color:#0050d0;background:#e8f0ff;border-bottom:1px solid #0050d0}'
         CHANGING ct_html = rt_html ).
    " What the line invokes, told apart from ordinary identifiers
    add( EXPORTING i_text = 'a.call{color:#800000;font-weight:bold;border-bottom:1px solid #b06060}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.fold{color:#3070c0;font-style:italic}' CHANGING ct_html = rt_html ).
    " Global folding is one class switch on the container, not a per-row DOM
    " walk: on a 4000-line include the walk made tens of thousands of
    " getElementById calls and the GUI's script engine gave up half way.
    add( EXPORTING i_text = '#code.folded .f{display:none}' CHANGING ct_html = rt_html ).
    " Line counts precomputed in ABAP, shown only while globally folded
    add( EXPORTING i_text = '.gf{display:none;color:#3070c0;font-style:italic}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '#code.folded .gf{display:inline}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '</style></head><body>' CHANGING ct_html = rt_html ).

    " Folding is driven from the window toolbar, not from inside the page,
    " so there is a single button and a single state.
    " How much of the source is control flow: characters left standing when
    " everything inside a control structure is folded away, against the whole
    " source. A low ratio means the shape of the method is a small part of it.
    DATA(lv_all_ch)   = 0.
    DATA(lv_shown_ch) = 0.
    LOOP AT lt_lines INTO DATA(ls_ratio).
      DATA(lv_len) = strlen( condense( ls_ratio-text ) ).
      lv_all_ch = lv_all_ch + lv_len.
      CHECK NOT ( ls_ratio-kind = 'P' AND ls_ratio-depth > 0 ).
      lv_shown_ch = lv_shown_ch + lv_len.
    ENDLOOP.
    DATA(lv_ratio) = COND i( WHEN lv_all_ch > 0
                             THEN lv_shown_ch * 100 / lv_all_ch
                             ELSE 0 ).

    add( EXPORTING i_text = |<div class="hdr">{ escape( i_title ) }| &&
                            |<span class="ratio" title="Characters left when all control| &&
                            | structures are collapsed, against the whole source">| &&
                            |Main Logic Token Ratio: { lv_ratio }%</span></div>|
         CHANGING ct_html = rt_html ).

    " Folded state is rendered straight into the markup — no startup script
    add( EXPORTING i_text = COND string( WHEN i_folded = abap_true
                                         THEN '<div id="code" class="folded">'
                                         ELSE '<div id="code">' )
         CHANGING ct_html = rt_html ).

    LOOP AT lt_lines INTO DATA(ls_line).
      DATA(lv_toggle) = COND string(
        WHEN ls_line-end > ls_line-line OR ls_line-all > ls_line-line
        THEN |<span class="tg" id="t{ ls_line-line }" title="Click: fold this branch only| &&
             | / Ctrl+click: fold every branch of the block"| &&
             | onclick="tog({ ls_line-line },event)">-</span>|
        ELSE '<span class="tgx"></span>' ).
      " Breakpoint dot + line number: click toggles a session breakpoint,
      " Ctrl+click an external one — same semantics as the editor's border.
      " Explicitly typed: an inline DATA( ) would inherit the literal's
      " fixed C length and truncate the longer breakpoint variants mid-tag.
      DATA lv_bp TYPE string.
      lv_bp = '<span class="bp"></span>'.
      READ TABLE it_bp_s TRANSPORTING NO FIELDS WITH KEY table_line = ls_line-line.
      IF sy-subrc = 0.
        lv_bp = '<span class="bp bps" title="Session breakpoint"></span>'.
      ELSE.
        READ TABLE it_bp_e TRANSPORTING NO FIELDS WITH KEY table_line = ls_line-line.
        IF sy-subrc = 0.
          lv_bp = '<span class="bp bpe" title="External breakpoint"></span>'.
        ENDIF.
      ENDIF.

      " Statements sitting inside a control structure carry class "f" — that
      " is all the global Collapse all needs to hide them.
      DATA(lv_cls) = COND string(
        WHEN ls_line-kind = 'P' AND ls_line-depth > 0 THEN 'ln f' ELSE 'ln' ).

      add( EXPORTING i_text = |<div class="{ lv_cls }" id="r{ ls_line-line }" data-end="{ ls_line-end }"| &&
                              | data-all="{ ls_line-all }" data-d="{ ls_line-depth }"| &&
                              | data-kind="{ ls_line-kind }">{ lv_bp }| &&
                              |<span class="num" title="Click: session breakpoint| &&
                              | / Ctrl+click: external breakpoint"| &&
                              | onclick="bp({ ls_line-line },event)">{ ls_line-line }</span>| &&
                              lv_toggle && render_line( i_text = ls_line-text i_line = ls_line-line
                                           i_calls = ls_line-call_names ) &&
                              |<span class="fold" id="f{ ls_line-line }"></span>| &&
                              COND string( WHEN ls_line-end > ls_line-line
                                THEN |<span class="gf">&nbsp;&nbsp;... { ls_line-end - ls_line-line } lines</span>|
                                ELSE '' ) && '</div>'
           CHANGING ct_html = rt_html ).
    ENDLOOP.

    add( EXPORTING i_text = '</div>' CHANGING ct_html = rt_html ).

    " --- folding script (ES5 — the GUI control is IE based) ---
    add( EXPORTING i_text = '<script type="text/javascript">' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'function row(n){return document.getElementById("r"+n);}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'function att(e,n){return parseInt(e.getAttribute(n),10);}'
         CHANGING ct_html = rt_html ).
    " Show restores display:"block" rather than "": clearing the inline style
    " would hand control back to the global .folded rule and the block the
    " user just expanded would stay hidden.
    add( EXPORTING i_text = 'function setSeg(n,hide){var e=row(n);if(!e)return;' &&
                            'var end=att(e,"data-end");if(end<=n)return;var i;' &&
                            'for(i=n+1;i<=end;i++){var r=row(i);' &&
                            'if(r)r.style.display=hide?"none":"block";' &&
                            'var t=document.getElementById("t"+i);if(t)t.innerHTML="-";' &&
                            'var f=document.getElementById("f"+i);if(f)f.innerHTML="";}' &&
                            'var tg=document.getElementById("t"+n);if(tg)tg.innerHTML=hide?"+":"-";' &&
                            'var fl=document.getElementById("f"+n);' &&
                            'if(fl)fl.innerHTML=hide?("  ... "+(end-n)+" lines"):"";}'
         CHANGING ct_html = rt_html ).
    " Chain fold: the header itself plus every sibling branch of the same
    " block (ELSEIF/ELSE of an IF, all WHENs of a CASE), so what remains
    " visible is exactly the branch headers.
    add( EXPORTING i_text = 'function foldChain(n,hide){var e=row(n);if(!e)return;' &&
                            'var d=att(e,"data-d");var a=att(e,"data-all");' &&
                            'if(!(a>n))a=att(e,"data-end");setSeg(n,hide);var i;' &&
                            'for(i=n+1;i<=a;i++){var r=row(i);if(!r)continue;' &&
                            'if(att(r,"data-d")==d&&r.getAttribute("data-kind")=="B")setSeg(i,hide);}}'
         CHANGING ct_html = rt_html ).
    " Plain click folds only this header's own branch — for an IF that is the
    " body down to the first ELSEIF/ELSE. Ctrl+click folds every branch of the
    " block at once, leaving just the branch headers. A header with no branch
    " of its own (CASE, whose first WHEN follows immediately) always chains.
    add( EXPORTING i_text = 'function tog(n,ev){var e=row(n);var tg=document.getElementById("t"+n);' &&
                            'var evt=ev?ev:window.event;var ctrl=evt&&evt.ctrlKey;' &&
                            'var hide=tg.innerHTML!="+";var b=att(e,"data-end");' &&
                            'if(ctrl||!(b>n))foldChain(n,hide);else setSeg(n,hide);}'
         CHANGING ct_html = rt_html ).
    " The trailing timestamp matters: without it a second click builds the
    " very same URL, the browser treats it as "already there" and no sapevent
    " ever reaches ABAP — the breakpoint would set but never clear.
    add( EXPORTING i_text = 'function bp(n,ev){var evt=ev?ev:window.event;' &&
                            'var c=(evt&&evt.ctrlKey)?1:0;' &&
                            'window.location.href="sapevent:bp?l="+n+"&c="+c' &&
                            '+"&t="+(new Date()).getTime();return false;}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'function nav(n,w){window.location.href="sapevent:nav?l="+n+"&w="+w' &&
                            '+"&t="+(new Date()).getTime();return false;}'
         CHANGING ct_html = rt_html ).
    " Global include-wide toggle, independent of the per-click folding above:
    " hides every statement that sits inside a control structure, so what is
    " left standing is the skeleton — LOOP/DO/WHILE/IF/ELSE/CASE/WHEN/TRY and
    " METHOD/FORM with their closers, nested ones included.
    add( EXPORTING i_text = 'function foldAll(){var d=document.getElementById("code");' &&
                            'd.className=(d.className=="folded")?"":"folded";}'
         CHANGING ct_html = rt_html ).
    " Setting a breakpoint round-trips through sapevent, which reloads the
    " page — scroll back to the line the user clicked so the view stays put.
    IF i_focus > 0.
      add( EXPORTING i_text = |var fr=row({ i_focus });| &&
                              'if(fr)window.scrollTo(0,fr.offsetTop-120);'
           CHANGING ct_html = rt_html ).
    ENDIF.
    add( EXPORTING i_text = '</script></body></html>' CHANGING ct_html = rt_html ).

  ENDMETHOD.


  METHOD analyze.

    FIELD-SYMBOLS <lv_src> TYPE any.
    DATA lv_depth TYPE i.
    DATA lt_stack TYPE tt_open.
    DATA lt_stmt  TYPE tt_stmt.
    DATA lt_block TYPE tt_block.

    " Pass 1 — one entry per source line.
    LOOP AT it_source ASSIGNING <lv_src>.
      APPEND INITIAL LINE TO rt_lines ASSIGNING FIELD-SYMBOL(<ls_line>).
      <ls_line>-line = sy-tabix.
      <ls_line>-text = <lv_src>.
      " 'P' rather than a blank: a string template drops trailing blanks,
      " so data-kind=" " would reach the page as an empty attribute.
      <ls_line>-kind = 'P'.
    ENDLOOP.

    " Pass 2 — statement keywords come from the parser's scan, so a line is
    " only a structure line if a statement really starts there. Reading the
    " first word of the raw text instead misreads continuation lines of
    " multi-line statements (WRITE: / ..., chained calls) as keywords.
    " The scan is not always at hand though — the whole-class view and any
    " include that was not parsed arrive without keywords — so fall back to
    " the first word rather than showing no folding at all.
    IF it_kw IS NOT INITIAL.
      LOOP AT it_kw INTO DATA(ls_kw).
        DATA(lv_vline) = COND i( WHEN ls_kw-v_line > 0 THEN ls_kw-v_line ELSE ls_kw-line ).
        lv_vline = lv_vline - i_offset + 1.
        CHECK lv_vline > 0 AND lv_vline <= lines( rt_lines ).
        APPEND VALUE #( index = ls_kw-index
                        line  = lv_vline
                        word  = to_upper( ls_kw-name ) ) TO lt_stmt.
        " The parser already resolved what this statement invokes — the same
        " table the editor's double-click navigates by. No need to recognise
        " calls by their first token.
        CHECK ls_kw-tt_calls IS NOT INITIAL.
        READ TABLE rt_lines ASSIGNING <ls_line> INDEX lv_vline.
        CHECK sy-subrc = 0.
        <ls_line>-call = abap_true.
        " Names of what is invoked here, so the renderer can pick those
        " identifiers out of the line and mark them as calls.
        LOOP AT ls_kw-tt_calls INTO DATA(ls_call).
          CHECK ls_call-name IS NOT INITIAL.
          <ls_line>-call_names = |{ <ls_line>-call_names } { to_upper( ls_call-name ) }|.
        ENDLOOP.
        <ls_line>-call_names = |{ <ls_line>-call_names } |.
      ENDLOOP.
      SORT lt_stmt BY index.
    ELSEIF io_scan IS BOUND.
      " No keyword table (source popup): the scan alone is enough — every
      " statement's first token carries both its row and its keyword.
      LOOP AT io_scan->statements INTO DATA(ls_sst).
        DATA(lv_sidx) = sy-tabix.
        READ TABLE io_scan->tokens INDEX ls_sst-from INTO DATA(ls_stok).
        CHECK sy-subrc = 0.
        DATA(lv_srow) = ls_stok-row - i_offset + 1.
        CHECK lv_srow > 0 AND lv_srow <= lines( rt_lines ).
        APPEND VALUE #( index = lv_sidx
                        line  = lv_srow
                        word  = to_upper( ls_stok-str ) ) TO lt_stmt.
      ENDLOOP.
    ELSE.
      LOOP AT rt_lines ASSIGNING <ls_line>.
        DATA(lv_fw) = first_word( <ls_line>-text ).
        CHECK lv_fw IS NOT INITIAL.
        APPEND VALUE #( index = sy-tabix line = sy-tabix word = lv_fw ) TO lt_stmt.
      ENDLOOP.
    ENDIF.

    " The line's own word is the FIRST statement starting on it — that is
    " what the fold marker and the scheme label refer to.
    LOOP AT lt_stmt INTO DATA(ls_first).
      READ TABLE rt_lines ASSIGNING <ls_line> INDEX ls_first-line.
      CHECK sy-subrc = 0.
      IF <ls_line>-word IS INITIAL. <ls_line>-word = ls_first-word. ENDIF.
    ENDLOOP.

    " Pass 3 — blocks. The scanner already knows them: CL_CI_SCAN->STRUCTURES
    " holds one entry per block with its first/last statement and the opening
    " keyword, so nothing has to be paired up by hand.
    IF io_scan IS BOUND.
      LOOP AT io_scan->structures INTO DATA(ls_struc).
        READ TABLE lt_stmt INTO DATA(ls_sf) WITH KEY index = ls_struc-stmnt_from.
        CHECK sy-subrc = 0.
        READ TABLE lt_stmt INTO DATA(ls_st) WITH KEY index = ls_struc-stmnt_to.
        CHECK sy-subrc = 0.
        " KEY_START is a flag (domain BOOLEAN), not the keyword — the opening
        " word comes from the statement itself. This also filters out the
        " structures that are not blocks at all.
        DATA(lv_key) = ls_sf-word.
        CHECK closer_of( lv_key ) IS NOT INITIAL.
        " A block written on one line holds nothing to fold, but it is still
        " part of the control structure and belongs in the scheme: mark it
        " 'S' — no toggle, no nesting effect, but visible to BUILD_SCHEME.
        IF ls_sf-line = ls_st-line.
          READ TABLE rt_lines ASSIGNING <ls_line> INDEX ls_sf-line.
          IF sy-subrc = 0 AND <ls_line>-kind = 'P'. <ls_line>-kind = 'S'. ENDIF.
          CONTINUE.
        ENDIF.
        APPEND VALUE #( open = ls_sf-line close = ls_st-line word = lv_key ) TO lt_block.
      ENDLOOP.
    ENDIF.

    " Fallback when no scan is at hand (whole-class view): pair openers with
    " closers over the STATEMENT list — not over lines, because "IF ... ENDIF."
    " on one line is two statements sharing one line.
    LOOP AT lt_stmt INTO DATA(ls_stmt).
      CHECK io_scan IS NOT BOUND.
      IF c_closers CS | { ls_stmt-word } |.
        " The stack grows at index 1, so index 1 is the innermost block:
        " take the FIRST match, or a nested ENDIF would pair with an outer IF.
        DATA(lv_hit) = 0.
        LOOP AT lt_stack INTO DATA(ls_open).
          IF ls_open-closer = ls_stmt-word. lv_hit = sy-tabix. EXIT. ENDIF.
        ENDLOOP.
        IF lv_hit > 0.
          READ TABLE lt_stack INTO ls_open INDEX lv_hit.
          " A block opened and closed on the same line holds nothing to fold
          IF ls_open-line < ls_stmt-line.
            APPEND VALUE #( open = ls_open-line
                            close = ls_stmt-line
                            word = ls_open-word ) TO lt_block.
          ENDIF.
          " Drop the match and everything stacked on top of it (those never
          " got a closer). FROM would delete towards the table end, i.e. the
          " enclosing blocks — that is how ENDIF used to swallow its LOOP.
          DELETE lt_stack TO lv_hit.
        ENDIF.
        CONTINUE.
      ENDIF.

      DATA(lv_closer) = closer_of( ls_stmt-word ).
      IF lv_closer IS NOT INITIAL.
        INSERT VALUE #( line = ls_stmt-line word = ls_stmt-word closer = lv_closer )
          INTO lt_stack INDEX 1.
      ENDIF.
    ENDLOOP.

    " Pass 4 — depth as a running sum over the confirmed blocks, and the
    " opener/closer marks that go with them.
    DATA lt_delta TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DO lines( rt_lines ) TIMES.
      APPEND 0 TO lt_delta.
    ENDDO.
    LOOP AT lt_block INTO DATA(ls_block).
      READ TABLE rt_lines ASSIGNING <ls_line> INDEX ls_block-open.
      IF sy-subrc = 0. <ls_line>-kind = 'O'. ENDIF.
      READ TABLE rt_lines ASSIGNING <ls_line> INDEX ls_block-close.
      IF sy-subrc = 0. <ls_line>-kind = 'C'. ENDIF.
      " Everything strictly inside the block sits one level deeper
      READ TABLE lt_delta ASSIGNING FIELD-SYMBOL(<lv_d>) INDEX ls_block-open + 1.
      IF sy-subrc = 0. <lv_d> = <lv_d> + 1. ENDIF.
      READ TABLE lt_delta ASSIGNING <lv_d> INDEX ls_block-close.
      IF sy-subrc = 0. <lv_d> = <lv_d> - 1. ENDIF.
    ENDLOOP.

    lv_depth = 0.
    LOOP AT rt_lines ASSIGNING <ls_line>.
      READ TABLE lt_delta INTO DATA(lv_step) INDEX sy-tabix.
      IF sy-subrc = 0. lv_depth = lv_depth + lv_step. ENDIF.
      IF lv_depth < 0. lv_depth = 0. ENDIF.
      <ls_line>-depth = lv_depth.
    ENDLOOP.

    " Pass 5 — branches, attached to the block that encloses them.
    DATA lt_owner TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    LOOP AT rt_lines ASSIGNING <ls_line>.
      DATA(lv_line_no) = sy-tabix.
      LOOP AT lt_block INTO ls_block WHERE open = lv_line_no.
        INSERT ls_block-word INTO lt_owner INDEX 1.
      ENDLOOP.
      LOOP AT lt_block INTO ls_block WHERE close = lv_line_no.
        DELETE lt_owner INDEX 1.
      ENDLOOP.

      CHECK <ls_line>-kind = 'P'.
      CHECK <ls_line>-word IS NOT INITIAL.
      CHECK c_branches CS | { <ls_line>-word } |.
      READ TABLE lt_owner INDEX 1 INTO DATA(lv_owner).
      CHECK sy-subrc = 0.
      " ELSEIF/ELSE belong to IF, WHEN to CASE, CATCH/CLEANUP to TRY
      DATA(lv_ok) = xsdbool(
        (    ( <ls_line>-word = 'ELSEIF' OR <ls_line>-word = 'ELSE' ) AND lv_owner = 'IF' )
        OR ( <ls_line>-word = 'WHEN' AND lv_owner = 'CASE' )
        OR ( ( <ls_line>-word = 'CATCH' OR <ls_line>-word = 'CLEANUP' ) AND lv_owner = 'TRY' ) ).
      CHECK lv_ok = abap_true.
      <ls_line>-kind  = 'B'.
      <ls_line>-depth = <ls_line>-depth - 1.
      IF <ls_line>-depth < 0. <ls_line>-depth = 0. ENDIF.
    ENDLOOP.

    " Pass 5 — for every header, the segment ends right before the next
    " line at the same depth that is a branch or a closer.
    LOOP AT rt_lines ASSIGNING <ls_line> WHERE kind = 'O' OR kind = 'B'.
      DATA(lv_start) = sy-tabix.
      DATA(lv_end)   = <ls_line>-line.
      DATA(lv_from)  = lv_start + 1.
      LOOP AT rt_lines ASSIGNING FIELD-SYMBOL(<ls_next>) FROM lv_from.
        IF <ls_next>-depth <= <ls_line>-depth
          AND ( <ls_next>-kind = 'B' OR <ls_next>-kind = 'C' ).
          EXIT.
        ENDIF.
        lv_end = <ls_next>-line.
      ENDLOOP.
      <ls_line>-end = lv_end.
    ENDLOOP.

    " Pass 6 — openers additionally get the whole-block extent, ending on
    " the line before their matching closer (the closer itself stays
    " visible, so a folded block still reads as IF ... ENDIF).
    LOOP AT rt_lines ASSIGNING <ls_line> WHERE kind = 'O'.
      DATA(lv_ostart) = sy-tabix.
      DATA(lv_ofrom)  = lv_ostart + 1.
      LOOP AT rt_lines ASSIGNING <ls_next> FROM lv_ofrom.
        IF <ls_next>-kind = 'C' AND <ls_next>-depth = <ls_line>-depth.
          <ls_line>-all = <ls_next>-line - 1.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF <ls_line>-all < <ls_line>-line. <ls_line>-all = <ls_line>-line. ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_scheme.

    DATA(lt_lines) = analyze( it_source = it_source it_kw = it_kw io_scan = io_scan ).

    " Running count of plain statements, so the number of operations between
    " any two lines is one subtraction rather than a scan.
    DATA lt_ops TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA(lv_run) = 0.
    LOOP AT lt_lines INTO DATA(ls_cnt).
      IF ls_cnt-kind = 'P' AND ls_cnt-word IS NOT INITIAL
        AND c_decls NS | { ls_cnt-word } |.
        lv_run = lv_run + 1.
      ENDIF.
      APPEND lv_run TO lt_ops.
    ENDLOOP.

    " Left to right by default: a branch reads as a sequence, and the wide
    " condition labels waste far less space than stacked vertically.
    rv_mm = |flowchart LR\n|.

    " When the source is a single unit, its METHOD/FORM line is the root and
    " carries the qualified name — no extra start node above it.
    DATA(lv_root) = 0.
    LOOP AT lt_lines INTO DATA(ls_root)
      WHERE kind = 'O' AND ( word = 'METHOD' OR word = 'FORM' OR word = 'MODULE' ).
      lv_root = ls_root-line.
      EXIT.
    ENDLOOP.

    " Edges are collected separately and appended after every node and frame
    " has been declared: an edge written inside a subgraph pulls its nodes
    " into that subgraph, which silently wrecks the nesting.
    DATA lv_edges TYPE string.
    " click directives go last, after the nodes they refer to
    DATA lv_clicks TYPE string.
    " Condition of the branch just entered, waiting to be put on its arrow
    DATA lv_lbl TYPE string.
    " classDef / class lines, emitted after everything they refer to
    DATA lv_styles TYPE string.

    DATA(lv_prev_node)  = ||.
    DATA(lv_prev_depth) = 0.
    DATA(lv_prev_line)  = 0.
    IF lv_root = 0.
      rv_mm = rv_mm && |  start(["{ scheme_label( i_title ) }"])\n|.
      lv_prev_node = |start|.
    ENDIF.

    " Open subgraphs, innermost first, with the line their block ends on
    TYPES: BEGIN OF ts_sub,
             endline TYPE i,
           END OF ts_sub.
    DATA lt_sub TYPE STANDARD TABLE OF ts_sub WITH EMPTY KEY.

    " Open IF/CASE blocks, innermost first: the header every branch fans out
    " from, and the tail of each branch already walked, so that they can be
    " joined back together on the closing statement.
    TYPES: BEGIN OF ts_cond,
             closeline TYPE i,
             depth     TYPE i,
             word      TYPE string,
             header    TYPE string,
             seen      TYPE abap_bool,   " first branch already passed
             tails     TYPE string_table,
           END OF ts_cond.
    DATA lt_cond TYPE STANDARD TABLE OF ts_cond WITH EMPTY KEY.

    LOOP AT lt_lines INTO DATA(ls_line).

      " Join the branches of every IF/CASE that closes on this line. Without
      " it the branches ran on as one chain and the IF looked as if it never
      " ended.
      WHILE lines( lt_cond ) > 0.
        READ TABLE lt_cond INTO DATA(ls_cond) INDEX 1.
        IF ls_cond-closeline <> ls_line-line. EXIT. ENDIF.
        " Statements after the last structure of the branch still belong to
        " it — without this they fell out of the picture entirely.
        APPEND ops_node( EXPORTING i_from  = lv_prev_line
                                   i_to    = ls_line-line
                                   i_id    = |x{ ls_line-line }|
                                   i_prev  = lv_prev_node
                                   i_label = lv_lbl
                                   it_ops  = lt_ops
                                   it_lines = lt_lines
                                   it_expanded = it_expanded
                                   i_expand_all = i_expand_all
                         CHANGING  cv_mm     = rv_mm
                                   cv_edges  = lv_edges
                                   cv_clicks = lv_clicks
                                                 cv_styles = lv_styles ) TO ls_cond-tails.
        CLEAR lv_lbl.
        " The closing statement is worth a node of its own — unlike ENDLOOP,
        " where the frame around the body already shows where it ends.
        DATA(lv_join) = |j{ ls_line-line }|.
        rv_mm = rv_mm && |  { lv_join }("{ scheme_label( ls_line-text ) }")\n|.
        LOOP AT ls_cond-tails INTO DATA(lv_tail).
          CHECK lv_tail IS NOT INITIAL.
          lv_edges = lv_edges && |  { lv_tail } --> { lv_join }\n|.
        ENDLOOP.
        lv_prev_node  = lv_join.
        lv_prev_line  = ls_line-line.
        lv_prev_depth = ls_cond-depth.
        DELETE lt_cond INDEX 1.
      ENDWHILE.

      " Close every subgraph whose block ended before this line. Without the
      " frame, the body of a nested LOOP was drawn as a sibling chain and the
      " nesting was impossible to see.
      WHILE lines( lt_sub ) > 0.
        READ TABLE lt_sub INTO DATA(ls_sub) INDEX 1.
        IF ls_sub-endline >= ls_line-line. EXIT. ENDIF.
        rv_mm = rv_mm && |  end\n|.
        DELETE lt_sub INDEX 1.
      ENDWHILE.

      " Only the structure itself — plain statements would drown the picture.
      " 'S' is a block written on a single line: not foldable, but it is a
      " branch all the same and has to show up here.
      CHECK ls_line-kind = 'O' OR ls_line-kind = 'B' OR ls_line-kind = 'S'.

      DATA(lv_node)  = |n{ ls_line-line }|.
      " The unit line shows the qualified name (CLASS=>METHOD) instead of the
      " bare METHOD statement — class and method read as one block.
      DATA(lv_label) = COND string(
        WHEN ls_line-line = lv_root AND i_title IS NOT INITIAL
        THEN scheme_label( i_title )
        ELSE scheme_label( ls_line-text ) ).

      " A loop becomes the frame around its body — but only if that body
      " holds structure of its own. A loop over plain statements would give
      " an empty frame with nothing to draw inside and no edge reaching it,
      " which is how loops ended up floating unconnected; those stay nodes.
      " TRY gets a frame of its own too: its CATCH blocks are separate paths
      " out of the protected code, and a frame is what makes that readable.
      DATA(lv_is_loop) = xsdbool(
        ls_line-kind = 'O' AND ls_line-all > ls_line-line
        AND ( ls_line-word = 'LOOP' OR ls_line-word = 'DO' OR ls_line-word = 'WHILE'
           OR ls_line-word = 'TRY' ) ).

      IF lv_is_loop = abap_true.
        DATA(lv_inner) = 0.
        LOOP AT lt_lines TRANSPORTING NO FIELDS
          WHERE line > ls_line-line AND line <= ls_line-all
            AND ( kind = 'O' OR kind = 'B' OR kind = 'S' ).
          lv_inner = 1.
          EXIT.
        ENDLOOP.
        " A loop over plain statements is a node — until it is clicked, then
        " its body is drawn inside a frame like any other loop.
        IF lv_inner = 0.
          READ TABLE it_expanded TRANSPORTING NO FIELDS WITH KEY table_line = ls_line-line.
          IF sy-subrc <> 0. lv_is_loop = abap_false. ENDIF.
        ENDIF.
      ENDIF.

      " Statements that ran BEFORE this block must be drawn before its frame
      " is opened, or they land inside a frame they never belonged to: the
      " code between an ENDTRY and the next TRY used to end up in that next
      " TRY, because it was only flushed when the next structure line came.
      IF lv_is_loop = abap_true.
        lv_prev_node = ops_node( EXPORTING i_from      = lv_prev_line
                                           i_to        = ls_line-line
                                           i_id        = |o{ ls_line-line }|
                                           i_prev      = lv_prev_node
                                           i_label     = lv_lbl
                                           it_ops      = lt_ops
                                           it_lines    = lt_lines
                                           it_expanded = it_expanded
                                           i_expand_all = i_expand_all
                                 CHANGING  cv_mm     = rv_mm
                                           cv_edges  = lv_edges
                                           cv_clicks = lv_clicks
                                                 cv_styles = lv_styles ).
        CLEAR lv_lbl.
        lv_prev_line = ls_line-line.
      ENDIF.

      IF lv_is_loop = abap_true AND lv_inner = 1.
        rv_mm = rv_mm && |  subgraph g{ ls_line-line }["{ lv_label }"]\n|.
        rv_mm = rv_mm && |  direction LR\n|.
        " A protected block is coloured apart from a loop
        IF ls_line-word = 'TRY'.
          lv_styles = lv_styles && |class g{ ls_line-line } tryblk\n|.
        ENDIF.
        INSERT VALUE #( endline = ls_line-all ) INTO lt_sub INDEX 1.
        CONTINUE.
      ENDIF.

      " Expanded loop over plain statements: the frame is drawn here and
      " filled with its own statements, since there is no structure inside
      " that would otherwise be rendered into it.
      IF lv_is_loop = abap_true.
        rv_mm = rv_mm && |  subgraph g{ ls_line-line }["{ lv_label }"]\n|.
        rv_mm = rv_mm && |  direction LR\n|.
        DATA(lv_lchain) = ||.
        DATA(lv_lfirst) = ||.
        LOOP AT lt_lines INTO DATA(ls_lop)
          WHERE line > ls_line-line AND line <= ls_line-all
            AND kind = 'P' AND word IS NOT INITIAL.
          CHECK c_decls NS | { ls_lop-word } |.
          DATA(lv_lopn) = |p{ ls_lop-line }|.
          rv_mm = rv_mm && |  { lv_lopn }("{ scheme_label( ls_lop-text ) }")\n|.
          IF lv_lchain IS NOT INITIAL.
            lv_edges = lv_edges && |  { lv_lchain } --> { lv_lopn }\n|.
          ELSE.
            lv_lfirst = lv_lopn.
          ENDIF.
          lv_lchain = lv_lopn.
        ENDLOOP.
        rv_mm = rv_mm && |  end\n|.

        IF lv_prev_node IS NOT INITIAL AND lv_lfirst IS NOT INITIAL.
          lv_edges = lv_edges && |  { lv_prev_node }{ arrow( lv_lbl ) }{ lv_lfirst }\n|.
          CLEAR lv_lbl.
        ENDIF.
        " Clicking the first statement folds the loop back into one node
        IF lv_lfirst IS NOT INITIAL.
          lv_clicks = lv_clicks && |  click { lv_lfirst }| &&
                      | "sapevent:aceexp_{ ls_line-line }" _self\n|.
          lv_prev_node = lv_lchain.
          lv_prev_line = ls_line-all.
          lv_prev_depth = ls_line-depth.
        ENDIF.
        CONTINUE.
      ENDIF.

      " A branch is not a box of its own: its condition rides on the arrow
      " leaving the IF/CASE, which is both shorter and how the flow reads.
      FIELD-SYMBOLS <ls_br> TYPE ts_cond.
      UNASSIGN <ls_br>.
      IF ls_line-kind = 'B'.
        READ TABLE lt_cond ASSIGNING <ls_br> INDEX 1.
        " Only when the branch really belongs to that IF/CASE. A CATCH sits
        " one level down inside its TRY and used to grab the enclosing IF —
        " that is how TRY/CATCH ended up tangled with the conditions.
        IF sy-subrc = 0 AND <ls_br>-depth <> ls_line-depth.
          UNASSIGN <ls_br>.
        ENDIF.
        IF <ls_br> IS ASSIGNED.
          " The branch just walked ends here: its trailing statements, then
          " its tail is remembered for the join at the closing statement.
          DATA(lv_tail_node) = ops_node( EXPORTING i_from  = lv_prev_line
                                                   i_to    = ls_line-line
                                                   i_id    = |y{ ls_line-line }|
                                                   i_prev  = lv_prev_node
                                                   i_label = lv_lbl
                                                   it_ops  = lt_ops
                                                   it_lines = lt_lines
                                                   it_expanded = it_expanded
                                                   i_expand_all = i_expand_all
                                         CHANGING  cv_mm     = rv_mm
                                                   cv_edges  = lv_edges
                                                   cv_clicks = lv_clicks
                                                 cv_styles = lv_styles ).
          IF <ls_br>-word = 'CASE' AND <ls_br>-seen = abap_false.
            " Statements between CASE and its first WHEN run unconditionally,
            " before the dispatch — so they are not a branch, and every WHEN
            " has to fan out from the end of them rather than from the CASE.
            IF lv_tail_node <> <ls_br>-header.
              <ls_br>-header = lv_tail_node.
            ENDIF.
          ELSEIF lv_tail_node <> <ls_br>-header.
            " Still standing on the header means the branch was empty, and an
            " empty path only adds an arrow that says nothing.
            APPEND lv_tail_node TO <ls_br>-tails.
          ENDIF.
          <ls_br>-seen = abap_true.
          " This branch's own condition goes on the arrow leaving the IF/CASE
          lv_lbl        = lv_label.
          lv_prev_node  = <ls_br>-header.
          lv_prev_line  = ls_line-line.
          lv_prev_depth = ls_line-depth.
          CONTINUE.
        ENDIF.
      ENDIF.

      " Shape carries the meaning: decisions are rhombi, units are boxed,
      " branches are rounded.
      DATA(lv_shape) = SWITCH string( ls_line-word
        WHEN 'IF' OR 'CASE'                      THEN |{ lv_node }\{"{ lv_label }"\}|
        WHEN 'LOOP' OR 'DO' OR 'WHILE'           THEN |{ lv_node }[/"{ lv_label }"/]|
        WHEN 'METHOD' OR 'FORM' OR 'MODULE'      THEN |{ lv_node }[["{ lv_label }"]]|
        ELSE                                          |{ lv_node }("{ lv_label }")| ).
      rv_mm = rv_mm && |  { lv_shape }\n|.
      IF ls_line-word = 'LOOP' OR ls_line-word = 'DO' OR ls_line-word = 'WHILE'.
        " A loop drawn as a node holds its body hidden: click opens it up
        lv_clicks = lv_clicks && |  click { lv_node }| &&
                    | "sapevent:aceexp_{ ls_line-line }" _self\n|.
      ELSE.
        " Clicking a structure node selects that stretch of code in the source
        " window. Line numbers ride in the action name because the control
        " percent encodes '?', '=' and '&', leaving a query string unreadable.
        lv_clicks = lv_clicks && |  click { lv_node } "sapevent:acego_{ ls_line-line }_| &&
                    |{ COND i( WHEN ls_line-all > ls_line-line THEN ls_line-all
                               WHEN ls_line-end > ls_line-line THEN ls_line-end
                               ELSE ls_line-line ) }" _self\n|.
      ENDIF.

      " Edge source. A branch header does not continue the previous branch —
      " it fans out from the IF/CASE itself, and the branch just walked is
      " remembered so it can be joined at the closing statement.
      DATA(lv_from)      = lv_prev_node.
      DATA(lv_from_line) = lv_prev_line.

      " The root has nothing above it
      IF lv_from IS NOT INITIAL.
        " Plain statements between the two structure lines become one node,
        " so the picture keeps a sense of how much code sits in between.
        DATA(lv_after_ops) = ops_node( EXPORTING i_from     = lv_from_line
                                                 i_to       = ls_line-line
                                                 i_id       = |o{ ls_line-line }|
                                                 i_prev     = lv_from
                                                 i_label    = lv_lbl
                                                 it_ops     = lt_ops
                                                 it_lines   = lt_lines
                                                 it_expanded = it_expanded
                                   i_expand_all = i_expand_all
                                       CHANGING  cv_mm     = rv_mm
                                                 cv_edges  = lv_edges
                                                 cv_clicks = lv_clicks
                                                 cv_styles = lv_styles ).
        IF lv_after_ops <> lv_from.
          CLEAR lv_lbl.
          lv_edges = lv_edges && |  { lv_after_ops } --> { lv_node }\n|.
        ELSE.
          lv_edges = lv_edges && |  { lv_from }{ arrow( lv_lbl ) }{ lv_node }\n|.
          CLEAR lv_lbl.
        ENDIF.
      ENDIF.

      " An IF/CASE opens a fan-out that has to be closed again
      IF ls_line-kind = 'O' AND ls_line-all > ls_line-line
        AND ( ls_line-word = 'IF' OR ls_line-word = 'CASE' ).
        INSERT VALUE #( closeline = ls_line-all + 1
                        depth     = ls_line-depth
                        word      = ls_line-word
                        header    = lv_node ) INTO lt_cond INDEX 1.
      ENDIF.

      lv_prev_node  = lv_node.
      lv_prev_depth = ls_line-depth.
      lv_prev_line  = ls_line-line.
    ENDLOOP.

    " Frames still open at the end of the source
    WHILE lines( lt_sub ) > 0.
      rv_mm = rv_mm && |  end\n|.
      DELETE lt_sub INDEX 1.
    ENDWHILE.

    IF lv_styles IS NOT INITIAL.
      rv_mm = rv_mm && |classDef tryblk fill:#eaf6ea,stroke:#2e7d32,color:#000\n| && lv_styles.
    ENDIF.
    " Clickable nodes read as links rather than revealing themselves on hover
    rv_mm = rv_mm && |classDef default color:#12369e\n|.
    " Calls stand out: they are where the method hands work to someone else
    rv_mm = rv_mm && |classDef callnode fill:#fbeeee,stroke:#800000,color:#800000\n|.
    rv_mm = rv_mm && lv_edges && lv_clicks.

  ENDMETHOD.


  METHOD arrow.
    IF i_label IS INITIAL.
      r_text = ` --> `.
    ELSE.
      " Quoted: an unquoted edge label ends at the first bracket, and
      " conditions like m_tabname+0(3) = 'HRP' are full of them.
      r_text = | -->\|"{ i_label }"\| |.
    ENDIF.
  ENDMETHOD.


  METHOD ops_node.

    r_node = i_prev.
    CHECK i_prev IS NOT INITIAL.
    CHECK i_from > 0 AND i_to > i_from + 1.

    READ TABLE it_ops INTO DATA(lv_to_cnt) INDEX i_to - 1.
    CHECK sy-subrc = 0.
    READ TABLE it_ops INTO DATA(lv_fr_cnt) INDEX i_from.
    CHECK sy-subrc = 0.

    DATA(lv_ops) = lv_to_cnt - lv_fr_cnt.
    CHECK lv_ops > 0.

    READ TABLE it_expanded TRANSPORTING NO FIELDS WITH KEY table_line = i_to.
    DATA(lv_expanded) = xsdbool( sy-subrc = 0 OR i_expand_all = abap_true ).

    " Calls are what the flow is about, so they always get a node of their
    " own; the linear work around them is what gets aggregated. Everything
    " is shown one by one while the stretch is expanded, and a single
    " statement is never hidden — one line behind "1 operation" saves no
    " space and costs a click.
    DATA lt_pend TYPE tt_lines.
    DATA(lv_chain) = i_prev.
    DATA(lv_first) = ||.

    LOOP AT it_lines INTO DATA(ls_op)
      WHERE line > i_from AND line < i_to
        AND kind = 'P' AND word IS NOT INITIAL.
      CHECK c_decls NS | { ls_op-word } |.

      " A call is what the parser recorded in TT_CALLS — the same table the
      " editor navigates by on double-click. No guessing from the text.
      DATA(lv_txt) = stmt_text( it_lines = it_lines i_line = ls_op-line ).
      DATA(lv_is_call) = ls_op-call.

      IF lv_expanded = abap_false AND lv_is_call = abap_false.
        APPEND ls_op-line TO lt_pend.
        CONTINUE.
      ENDIF.

      " Flush what was collected before this call
      lv_chain = flush_pending( EXPORTING it_pend  = lt_pend
                                          it_lines = it_lines
                                          i_anchor = i_to
                                          i_prev   = lv_chain
                                          i_label  = COND string( WHEN lv_chain = i_prev
                                                                  THEN i_label ELSE `` )
                                CHANGING  cv_mm     = cv_mm
                                          cv_edges  = cv_edges
                                          cv_clicks = cv_clicks
                                          cv_first  = lv_first ).
      CLEAR lt_pend.

      DATA(lv_opn) = |p{ ls_op-line }|.
      cv_mm = cv_mm && |  { lv_opn }("{ scheme_label( lv_txt ) }")\n|.
      IF lv_is_call = abap_true.
        cv_styles = cv_styles && |class { lv_opn } callnode\n|.
      ENDIF.
      cv_edges = cv_edges && |  { lv_chain }{ COND string(
        WHEN lv_chain = i_prev THEN arrow( i_label ) ELSE ` --> ` ) }{ lv_opn }\n|.
      IF lv_first IS INITIAL. lv_first = lv_opn. ENDIF.
      lv_chain = lv_opn.
    ENDLOOP.

    " Whatever is left over after the last call
    lv_chain = flush_pending( EXPORTING it_pend  = lt_pend
                                        it_lines = it_lines
                                        i_anchor = i_to
                                        i_prev   = lv_chain
                                        i_label  = COND string( WHEN lv_chain = i_prev
                                                                THEN i_label ELSE `` )
                              CHANGING  cv_mm     = cv_mm
                                        cv_edges  = cv_edges
                                        cv_clicks = cv_clicks
                                        cv_first  = lv_first ).

    " Expanded stretches fold back up from their first node
    IF lv_expanded = abap_true AND lv_first IS NOT INITIAL.
      cv_clicks = cv_clicks && |click { lv_first } "sapevent:aceexp_{ i_to }" _self\n|.
    ENDIF.
    r_node = lv_chain.

  ENDMETHOD.


  METHOD flush_pending.

    r_node = i_prev.
    CHECK it_pend IS NOT INITIAL.

    IF lines( it_pend ) = 1.
      " A single statement is shown in full rather than counted
      READ TABLE it_pend INTO DATA(lv_one) INDEX 1.
      READ TABLE it_lines INTO DATA(ls_one) WITH KEY line = lv_one.
      CHECK sy-subrc = 0.
      DATA(lv_id1) = |p{ lv_one }|.
      cv_mm = cv_mm && |  { lv_id1 }("{ scheme_label( stmt_text( it_lines = it_lines i_line = lv_one ) ) }")\n|.
      cv_edges = cv_edges && |  { i_prev }{ arrow( i_label ) }{ lv_id1 }\n|.
      IF cv_first IS INITIAL. cv_first = lv_id1. ENDIF.
      r_node = lv_id1.
      RETURN.
    ENDIF.

    READ TABLE it_pend INTO DATA(lv_from_line) INDEX 1.
    DATA(lv_id) = |o{ lv_from_line }|.
    cv_mm = cv_mm && |  { lv_id }["{ lines( it_pend ) } operations"]\n|.
    cv_edges = cv_edges && |  { i_prev }{ arrow( i_label ) }{ lv_id }\n|.
    " Opens the whole stretch this chunk belongs to
    cv_clicks = cv_clicks && |click { lv_id } "sapevent:aceexp_{ i_anchor }" _self\n|.
    IF cv_first IS INITIAL. cv_first = lv_id. ENDIF.
    r_node = lv_id.

  ENDMETHOD.


  METHOD stmt_text.
    LOOP AT it_lines INTO DATA(ls_l) WHERE line >= i_line.
      DATA(lv_part) = condense( ls_l-text ).
      " A continuation line has no statement of its own; a blank one or the
      " next statement ends the run.
      IF ls_l-line > i_line AND ( ls_l-word IS NOT INITIAL OR lv_part IS INITIAL ).
        EXIT.
      ENDIF.
      IF lv_part IS INITIAL. CONTINUE. ENDIF.
      r_text = COND string( WHEN r_text IS INITIAL THEN lv_part
                            ELSE |{ r_text } { lv_part }| ).
      IF strlen( r_text ) > 120. EXIT. ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD scheme_label.
    r_text = condense( i_text ).
    " The diagram source passes through HTML twice, so angle brackets are
    " read as tags: FIELD-SYMBOL(<WATCH>) loses everything from the '<' on,
    " and the browser injects closing tags into the mermaid text. Entities
    " do not survive either — they decode back to '<' on the second pass —
    " so the brackets are removed outright.
    " Operators keep their meaning; the bare brackets that remain are almost
    " always field symbols, which read fine as (name).
    REPLACE ALL OCCURRENCES OF '<>' IN r_text WITH ' NE '.
    REPLACE ALL OCCURRENCES OF '<=' IN r_text WITH ' LE '.
    REPLACE ALL OCCURRENCES OF '>=' IN r_text WITH ' GE '.
    REPLACE ALL OCCURRENCES OF '->' IN r_text WITH '.'.
    REPLACE ALL OCCURRENCES OF '=>' IN r_text WITH '.'.
    REPLACE ALL OCCURRENCES OF '<' IN r_text WITH '('.
    REPLACE ALL OCCURRENCES OF '>' IN r_text WITH ')'.
    " Characters that would end a mermaid node or its label
    REPLACE ALL OCCURRENCES OF '"' IN r_text WITH c_apos.
    REPLACE ALL OCCURRENCES OF '[' IN r_text WITH '('.
    REPLACE ALL OCCURRENCES OF ']' IN r_text WITH ')'.
    REPLACE ALL OCCURRENCES OF '{' IN r_text WITH '('.
    REPLACE ALL OCCURRENCES OF '}' IN r_text WITH ')'.
    REPLACE ALL OCCURRENCES OF '|' IN r_text WITH '/'.
    REPLACE ALL OCCURRENCES OF ';' IN r_text WITH ' '.
    IF strlen( r_text ) > 80.
      r_text = |{ r_text(77) }...|.
    ENDIF.
  ENDMETHOD.


  METHOD first_word.
    DATA(lv_text) = condense( i_text ).
    IF lv_text IS INITIAL OR lv_text(1) = '*'. RETURN. ENDIF.
    IF lv_text(1) = '"'. RETURN. ENDIF.
    SPLIT to_upper( lv_text ) AT space INTO r_word DATA(lv_rest).
    " Strip a trailing period so "ELSE." and "TRY." are recognised too
    IF r_word CS '.'.
      SPLIT r_word AT '.' INTO r_word lv_rest.
    ENDIF.
  ENDMETHOD.


  METHOD closer_of.
    CASE i_word.
      WHEN 'IF'.      r_closer = 'ENDIF'.
      WHEN 'CASE'.    r_closer = 'ENDCASE'.
      WHEN 'LOOP'.    r_closer = 'ENDLOOP'.
      WHEN 'DO'.      r_closer = 'ENDDO'.
      WHEN 'WHILE'.   r_closer = 'ENDWHILE'.
      WHEN 'TRY'.     r_closer = 'ENDTRY'.
      WHEN 'METHOD'.  r_closer = 'ENDMETHOD'.
      WHEN 'FORM'.    r_closer = 'ENDFORM'.
      WHEN 'MODULE'.  r_closer = 'ENDMODULE'.
      WHEN 'SELECT'.  r_closer = 'ENDSELECT'.
      WHEN 'AT'.      r_closer = 'ENDAT'.
      WHEN 'PROVIDE'. r_closer = 'ENDPROVIDE'.
    ENDCASE.
  ENDMETHOD.


  METHOD render_line.

    DATA lv_pos  TYPE i.
    DATA lv_char TYPE c LENGTH 1.
    DATA lv_word TYPE string.

    DATA(lv_len) = strlen( i_text ).
    IF lv_len = 0. r_html = '&nbsp;'. RETURN. ENDIF.

    " Full-line comment
    DATA(lv_trim) = condense( i_text ).
    IF lv_trim IS NOT INITIAL AND lv_trim(1) = '*'.
      r_html = |<span class="c">{ escape( i_text ) }</span>|.
      RETURN.
    ENDIF.

    WHILE lv_pos < lv_len.
      lv_char = i_text+lv_pos(1).

      CASE lv_char.
        WHEN c_apos OR c_btick.
          " String literal up to the matching quote (or end of line)
          DATA(lv_quote) = lv_char.
          DATA(lv_from)  = lv_pos.
          lv_pos = lv_pos + 1.
          WHILE lv_pos < lv_len AND i_text+lv_pos(1) <> lv_quote.
            lv_pos = lv_pos + 1.
          ENDWHILE.
          IF lv_pos < lv_len. lv_pos = lv_pos + 1. ENDIF.
          r_html = r_html && |<span class="s">| &&
                   escape( substring( val = i_text off = lv_from len = lv_pos - lv_from ) ) && '</span>'.

        WHEN '"'.
          " Trailing comment — rest of the line
          r_html = r_html && |<span class="c">| &&
                   escape( substring( val = i_text off = lv_pos ) ) && '</span>'.
          RETURN.

        WHEN OTHERS.
          IF lv_char CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_/~'.
            lv_from = lv_pos.
            WHILE lv_pos < lv_len.
              lv_char = i_text+lv_pos(1).
              IF lv_char CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_/~'.
                lv_pos = lv_pos + 1.
              ELSEIF lv_char = '-' AND lv_pos + 1 < lv_len AND i_text+lv_pos(2) <> '->'.
                " structure component (SY-SUBRC) stays one token, but the
                " object operator -> splits obj and method into two links
                lv_pos = lv_pos + 1.
              ELSE.
                EXIT.
              ENDIF.
            ENDWHILE.
            lv_word = substring( val = i_text off = lv_from len = lv_pos - lv_from ).

            IF zcl_ace_keywords=>is_keyword( to_upper( lv_word ) ) = abap_true.
              r_html = r_html && |<span class="k">{ escape( lv_word ) }</span>|.
            ELSEIF lv_word CO '0123456789'.
              r_html = r_html && |<span class="n">{ escape( lv_word ) }</span>|.
            ELSE.
              " Identifier — hyperlink carrying the viewer line and the word,
              " which is all ZCL_ACE_WINDOW needs to resolve the target.
              " Routed through nav() for the same reason as bp(): repeating
              " an identical sapevent URL would be swallowed by the browser.
              " A call is marked as such: the same TT_CALLS the double-click
              " navigates by decides it, no guessing from the text.
              DATA(lv_cls_id) = COND string(
                WHEN i_calls CS | { to_upper( lv_word ) } | THEN 'id call' ELSE 'id' ).
              r_html = r_html && |<a class="{ lv_cls_id }" href="#" onclick="return nav(| &&
                       |{ i_line },'{ to_upper( lv_word ) }')">| &&
                       escape( lv_word ) && '</a>'.
            ENDIF.
          ELSEIF lv_char = space.
            " c -> string conversion would drop the blank, and a blank at a
            " w3htmltab row boundary would be lost too: emit it as an entity
            r_html = r_html && '&nbsp;'.
            lv_pos = lv_pos + 1.
          ELSE.
            r_html = r_html && escape( CONV string( lv_char ) ).
            lv_pos = lv_pos + 1.
          ENDIF.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD escape.
    r_text = i_text.
    REPLACE ALL OCCURRENCES OF '&' IN r_text WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN r_text WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN r_text WITH '&gt;'.
    " Blanks become entities: the HTML is shipped through a fixed-length
    " character table, where a blank landing on a row boundary is dropped.
    REPLACE ALL OCCURRENCES OF c_blank IN r_text WITH '&nbsp;'.
  ENDMETHOD.


  METHOD add.
    DATA lv_rest TYPE string.
    lv_rest = i_text.
    WHILE strlen( lv_rest ) > 255.
      " Never let a row end on a blank — the fixed-length row would drop it
      " and glue two tag attributes together.
      DATA(lv_cut) = 255.
      WHILE lv_cut > 1 AND substring( val = lv_rest off = lv_cut - 1 len = 1 ) = c_blank.
        lv_cut = lv_cut - 1.
      ENDWHILE.
      APPEND lv_rest(lv_cut) TO ct_html.
      lv_rest = lv_rest+lv_cut.
    ENDWHILE.
    APPEND lv_rest TO ct_html.
  ENDMETHOD.

ENDCLASS.
