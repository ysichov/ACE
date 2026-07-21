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
                i_title       TYPE string OPTIONAL
                it_bp_s       TYPE tt_lines OPTIONAL
                it_bp_e       TYPE tt_lines OPTIONAL
                i_focus       TYPE i OPTIONAL
                i_folded      TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rt_html) TYPE w3htmltab.

    "! Mermaid flowchart of the control structure of the same source:
    "! IF/ELSEIF/ELSE, CASE/WHEN, LOOP/DO/WHILE, TRY/CATCH and the enclosing
    "! METHOD/FORM. Plain statements are left out — the point is the shape of
    "! the branch, not its every line.
    CLASS-METHODS build_scheme
      IMPORTING it_source     TYPE STANDARD TABLE
                it_kw         TYPE zif_ace_parse_data=>tt_kword OPTIONAL
                i_title       TYPE string OPTIONAL
      RETURNING VALUE(rv_mm)  TYPE string.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_line,
        line  TYPE i,
        text  TYPE string,
        word  TYPE string,   " first word, uppercased
        depth TYPE i,
        kind  TYPE char1,   " 'O'=opener 'B'=branch 'C'=closer 'P'=plain
        end   TYPE i,       " last line of this header's own branch segment
        all   TYPE i,       " openers: last line before the matching closer
      END OF ts_line,
      tt_line TYPE STANDARD TABLE OF ts_line WITH EMPTY KEY.

    " Openers are only accepted once their own closer has actually been
    " found. SELECT and AT matter here: "SELECT ... INTO TABLE" and
    " "AT line-selection" have no closer, and counting them as blocks used
    " to shift the nesting level of everything that followed.
    TYPES:
      BEGIN OF ts_open,
        tabix  TYPE i,
        closer TYPE string,
      END OF ts_open,
      tt_open TYPE STANDARD TABLE OF ts_open WITH EMPTY KEY.

    " Quote characters as constants: literals like `'` and '`' inline in the
    " code are the kind of thing that trips up serialization round-trips.
    CONSTANTS c_apos  TYPE c LENGTH 1 VALUE ''''.
    CONSTANTS c_btick TYPE c LENGTH 1 VALUE '`'.
    " A blank MUST stay a string literal: as TYPE c its trailing blank is
    " stripped, which turns a REPLACE pattern into an empty one and loops
    " forever (CX_SY_REPLACE_INFINITE_LOOP).
    CONSTANTS c_blank TYPE string VALUE ` `.

    CONSTANTS c_branches TYPE string VALUE
      ' ELSEIF ELSE WHEN CATCH CLEANUP '.
    CONSTANTS c_closers TYPE string VALUE
      ' ENDIF ENDCASE ENDLOOP ENDDO ENDWHILE ENDTRY ENDMETHOD ENDFORM ENDMODULE ENDSELECT ENDAT ENDPROVIDE '.

    "! Classifies every line and computes the fold segments.
    CLASS-METHODS analyze
      IMPORTING it_source      TYPE STANDARD TABLE
                it_kw          TYPE zif_ace_parse_data=>tt_kword
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
      RETURNING VALUE(r_html) TYPE string.

    CLASS-METHODS escape
      IMPORTING i_text        TYPE string
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

    DATA(lt_lines) = analyze( it_source = it_source it_kw = it_kw ).

    add( EXPORTING i_text = '<html><head><meta http-equiv="X-UA-Compatible" content="IE=edge">'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '<style>' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'body{font-family:Consolas,"Courier New",monospace;font-size:12px;' &&
                            'background:#ffffff;color:#000000;margin:0;padding:0}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.hdr{background:#eef3f8;border-bottom:1px solid #c0c8d0;padding:3px 6px;' &&
                            'font-weight:bold}' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = '.hdr button{font-family:inherit;font-size:11px;margin-left:6px}'
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
    add( EXPORTING i_text = |<div class="hdr">{ escape( i_title ) }</div>|
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
                              lv_toggle && render_line( i_text = ls_line-text i_line = ls_line-line ) &&
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
        READ TABLE rt_lines ASSIGNING <ls_line> INDEX lv_vline.
        CHECK sy-subrc = 0.
        <ls_line>-word = to_upper( ls_kw-name ).
      ENDLOOP.
    ELSE.
      LOOP AT rt_lines ASSIGNING <ls_line>.
        <ls_line>-word = first_word( <ls_line>-text ).
      ENDLOOP.
    ENDIF.

    " Pass 3 — pair openers with their closers. An opener counts as a block
    " only once its own closer shows up: SELECT ... INTO TABLE and
    " AT line-selection have none, and treating them as blocks used to shift
    " the nesting level of everything below them.
    LOOP AT rt_lines ASSIGNING <ls_line>.
      DATA(lv_tabix) = sy-tabix.
      CHECK <ls_line>-word IS NOT INITIAL.

      IF c_closers CS | { <ls_line>-word } |.
        " The stack grows at index 1, so index 1 is the innermost block:
        " take the FIRST match, not the last, or a nested ENDIF would be
        " paired with an outer IF instead of its own.
        DATA(lv_hit) = 0.
        LOOP AT lt_stack INTO DATA(ls_open).
          IF ls_open-closer = <ls_line>-word. lv_hit = sy-tabix. EXIT. ENDIF.
        ENDLOOP.
        IF lv_hit > 0.
          READ TABLE lt_stack INTO ls_open INDEX lv_hit.
          READ TABLE rt_lines ASSIGNING FIELD-SYMBOL(<ls_open>) INDEX ls_open-tabix.
          IF sy-subrc = 0. <ls_open>-kind = 'O'. ENDIF.
          <ls_line>-kind = 'C'.
          " Drop the match and everything stacked on top of it (those never
          " got a closer). FROM would delete towards the table end, i.e. the
          " enclosing blocks — that is how ENDIF used to swallow its LOOP.
          DELETE lt_stack TO lv_hit.
        ENDIF.
        CONTINUE.
      ENDIF.

      DATA(lv_closer) = closer_of( <ls_line>-word ).
      IF lv_closer IS NOT INITIAL.
        INSERT VALUE #( tabix = lv_tabix closer = lv_closer ) INTO lt_stack INDEX 1.
      ENDIF.
    ENDLOOP.

    " Pass 4 — nesting level, plus branches attached to their owning block.
    DATA lt_owner TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    LOOP AT rt_lines ASSIGNING <ls_line>.
      CASE <ls_line>-kind.
        WHEN 'C'.
          lv_depth = lv_depth - 1.
          IF lv_depth < 0. lv_depth = 0. ENDIF.
          DELETE lt_owner INDEX 1.
          <ls_line>-depth = lv_depth.
        WHEN 'O'.
          <ls_line>-depth = lv_depth.
          INSERT <ls_line>-word INTO lt_owner INDEX 1.
          lv_depth = lv_depth + 1.
        WHEN OTHERS.
          <ls_line>-depth = lv_depth.
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
          <ls_line>-depth = lv_depth - 1.
          IF <ls_line>-depth < 0. <ls_line>-depth = 0. ENDIF.
      ENDCASE.
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

    TYPES: BEGIN OF ts_prev,
             depth TYPE i,
             node  TYPE string,
           END OF ts_prev.
    DATA lt_prev TYPE STANDARD TABLE OF ts_prev WITH EMPTY KEY.

    DATA(lt_lines) = analyze( it_source = it_source it_kw = it_kw ).

    rv_mm = |flowchart TD\n|.
    rv_mm = rv_mm && |  start(["{ scheme_label( i_title ) }"])\n|.

    DATA(lv_prev_node) = |start|.
    DATA(lv_prev_depth) = 0.

    LOOP AT lt_lines INTO DATA(ls_line).
      " Only the structure itself — plain statements would drown the picture
      CHECK ls_line-kind = 'O' OR ls_line-kind = 'B'.

      DATA(lv_node)  = |n{ ls_line-line }|.
      DATA(lv_label) = scheme_label( ls_line-text ).

      " Shape carries the meaning: decisions are rhombi, loops are barrels,
      " units are boxed, branches are rounded.
      DATA(lv_shape) = SWITCH string( ls_line-word
        WHEN 'IF' OR 'CASE'                      THEN |{ lv_node }\{"{ lv_label }"\}|
        WHEN 'LOOP' OR 'DO' OR 'WHILE'           THEN |{ lv_node }[/"{ lv_label }"/]|
        WHEN 'METHOD' OR 'FORM' OR 'MODULE'      THEN |{ lv_node }[["{ lv_label }"]]|
        ELSE                                          |{ lv_node }("{ lv_label }")| ).
      rv_mm = rv_mm && |  { lv_shape }\n|.

      " Edge source: the previous node of the same nesting level, or the
      " enclosing header when this is the first line of a block.
      DATA(lv_from) = lv_prev_node.
      LOOP AT lt_prev INTO DATA(ls_prev) WHERE depth = ls_line-depth.
        lv_from = ls_prev-node.
      ENDLOOP.
      IF ls_line-depth > lv_prev_depth.
        lv_from = lv_prev_node.
      ENDIF.
      rv_mm = rv_mm && |  { lv_from } --> { lv_node }\n|.

      DELETE lt_prev WHERE depth >= ls_line-depth.
      APPEND VALUE #( depth = ls_line-depth node = lv_node ) TO lt_prev.
      lv_prev_node  = lv_node.
      lv_prev_depth = ls_line-depth.
    ENDLOOP.

  ENDMETHOD.


  METHOD scheme_label.
    r_text = condense( i_text ).
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
              r_html = r_html && |<a class="id" href="#" onclick="return nav(| &&
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
