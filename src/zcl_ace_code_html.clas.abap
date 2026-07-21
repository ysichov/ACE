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
    "! @parameter it_bp_s   | viewer lines carrying a session breakpoint
    "! @parameter it_bp_e   | viewer lines carrying an external breakpoint
    CLASS-METHODS build
      IMPORTING it_source     TYPE STANDARD TABLE
                i_title       TYPE string OPTIONAL
                it_bp_s       TYPE tt_lines OPTIONAL
                it_bp_e       TYPE tt_lines OPTIONAL
                i_focus       TYPE i OPTIONAL
      RETURNING VALUE(rt_html) TYPE w3htmltab.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_line,
        line  TYPE i,
        text  TYPE string,
        depth TYPE i,
        kind  TYPE char1,   " 'O'=opener 'B'=branch 'C'=closer ' '=plain
        end   TYPE i,       " last line of this header's own branch segment
        all   TYPE i,       " openers: last line before the matching closer
      END OF ts_line,
      tt_line TYPE STANDARD TABLE OF ts_line WITH EMPTY KEY.

    CONSTANTS c_openers TYPE string VALUE
      ' IF CASE LOOP DO WHILE TRY METHOD FORM MODULE SELECT AT PROVIDE '.
    CONSTANTS c_branches TYPE string VALUE
      ' ELSEIF ELSE WHEN CATCH CLEANUP '.
    CONSTANTS c_closers TYPE string VALUE
      ' ENDIF ENDCASE ENDLOOP ENDDO ENDWHILE ENDTRY ENDMETHOD ENDFORM ENDMODULE ENDSELECT ENDAT ENDPROVIDE '.

    "! Classifies every line and computes the fold segments.
    CLASS-METHODS analyze
      IMPORTING it_source      TYPE STANDARD TABLE
      RETURNING VALUE(rt_lines) TYPE tt_line.

    "! First word of a statement line, uppercased ('' for comments/blank).
    CLASS-METHODS first_word
      IMPORTING i_text        TYPE string
      RETURNING VALUE(r_word) TYPE string.

    "! Source line → syntax-highlighted HTML with identifier hyperlinks.
    CLASS-METHODS render_line
      IMPORTING i_text        TYPE string
                i_line        TYPE i
      RETURNING VALUE(r_html) TYPE string.

    CLASS-METHODS escape
      IMPORTING i_text        TYPE string
      RETURNING VALUE(r_text) TYPE string.

    "! Appends a string to the w3htmltab, splitting on the 255 char limit.
    CLASS-METHODS add
      IMPORTING i_text TYPE string
      CHANGING  ct_html TYPE w3htmltab.

ENDCLASS.



CLASS zcl_ace_code_html IMPLEMENTATION.


  METHOD build.

    DATA(lt_lines) = analyze( it_source ).

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
    add( EXPORTING i_text = '</style></head><body>' CHANGING ct_html = rt_html ).

    add( EXPORTING i_text = |<div class="hdr">{ escape( i_title ) }| &&
                            '<button onclick="foldAll(1)">Collapse branches</button>' &&
                            '<button onclick="foldAll(0)">Expand all</button></div>'
         CHANGING ct_html = rt_html ).

    add( EXPORTING i_text = '<div id="code">' CHANGING ct_html = rt_html ).

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

      add( EXPORTING i_text = |<div class="ln" id="r{ ls_line-line }" data-end="{ ls_line-end }"| &&
                              | data-all="{ ls_line-all }" data-d="{ ls_line-depth }"| &&
                              | data-kind="{ ls_line-kind }">{ lv_bp }| &&
                              |<span class="num" title="Click: session breakpoint| &&
                              | / Ctrl+click: external breakpoint"| &&
                              | onclick="bp({ ls_line-line },event)">{ ls_line-line }</span>| &&
                              lv_toggle && render_line( i_text = ls_line-text i_line = ls_line-line ) &&
                              |<span class="fold" id="f{ ls_line-line }"></span></div>|
           CHANGING ct_html = rt_html ).
    ENDLOOP.

    add( EXPORTING i_text = '</div>' CHANGING ct_html = rt_html ).

    " --- folding script (ES5 — the GUI control is IE based) ---
    add( EXPORTING i_text = '<script type="text/javascript">' CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'function row(n){return document.getElementById("r"+n);}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'function att(e,n){return parseInt(e.getAttribute(n),10);}'
         CHANGING ct_html = rt_html ).
    add( EXPORTING i_text = 'function setSeg(n,hide){var e=row(n);if(!e)return;' &&
                            'var end=att(e,"data-end");if(end<=n)return;var i;' &&
                            'for(i=n+1;i<=end;i++){var r=row(i);if(r)r.style.display=hide?"none":"";' &&
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
    add( EXPORTING i_text = 'function foldAll(hide){var d=document.getElementById("code");' &&
                            'var ch=d.childNodes;var i;var ids=[];' &&
                            'for(i=0;i<ch.length;i++){if(ch[i].id&&ch[i].id.charAt(0)=="r")' &&
                            'ids.push(parseInt(ch[i].id.substring(1),10));}' &&
                            'if(!hide){for(i=0;i<ids.length;i++){var r=row(ids[i]);r.style.display="";' &&
                            'var t=document.getElementById("t"+ids[i]);if(t)t.innerHTML="-";' &&
                            'var f=document.getElementById("f"+ids[i]);if(f)f.innerHTML="";}return;}' &&
                            'for(i=ids.length-1;i>=0;i--){setSeg(ids[i],1);}}'
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

    " Pass 1 — classify each line and assign its nesting depth.
    LOOP AT it_source ASSIGNING <lv_src>.
      APPEND INITIAL LINE TO rt_lines ASSIGNING FIELD-SYMBOL(<ls_line>).
      <ls_line>-line = sy-tabix.
      <ls_line>-text = <lv_src>.
      <ls_line>-kind = ' '.

      DATA(lv_word) = first_word( <ls_line>-text ).
      IF lv_word IS INITIAL.
        <ls_line>-depth = lv_depth.
        CONTINUE.
      ENDIF.

      IF c_closers CS | { lv_word } |.
        lv_depth = lv_depth - 1.
        IF lv_depth < 0. lv_depth = 0. ENDIF.
        <ls_line>-kind = 'C'.
        <ls_line>-depth = lv_depth.
      ELSEIF c_branches CS | { lv_word } |.
        " A branch header lives one level up from the body it introduces.
        <ls_line>-kind = 'B'.
        <ls_line>-depth = lv_depth - 1.
        IF <ls_line>-depth < 0. <ls_line>-depth = 0. ENDIF.
      ELSEIF c_openers CS | { lv_word } |.
        " Single-line SELECT ... INTO / AT ... without a block form: only
        " treat as an opener when no period closes it on the same line is
        " not decidable here, so rely on the matching closer instead — an
        " unmatched opener simply keeps its segment until the next closer.
        <ls_line>-kind = 'O'.
        <ls_line>-depth = lv_depth.
        lv_depth = lv_depth + 1.
      ELSE.
        <ls_line>-depth = lv_depth.
      ENDIF.
    ENDLOOP.

    " Pass 2 — for every header, the segment ends right before the next
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

    " Pass 3 — openers additionally get the whole-block extent, ending on
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


  METHOD first_word.
    DATA(lv_text) = condense( i_text ).
    IF lv_text IS INITIAL OR lv_text(1) = '*'. RETURN. ENDIF.
    IF lv_text(1) = '"'. RETURN. ENDIF.
    SPLIT to_upper( lv_text ) AT space INTO r_word DATA(lv_rest).
    " Strip a trailing period so "ELSE." and "TRY." are recognised too.
    IF r_word CS '.'.
      SPLIT r_word AT '.' INTO r_word lv_rest.
    ENDIF.
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
        WHEN `'` OR '`'.
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
    REPLACE ALL OCCURRENCES OF ` ` IN r_text WITH '&nbsp;'.
  ENDMETHOD.


  METHOD add.
    DATA lv_rest TYPE string.
    lv_rest = i_text.
    WHILE strlen( lv_rest ) > 255.
      " Never let a row end on a blank — the fixed-length row would drop it
      " and glue two tag attributes together.
      DATA(lv_cut) = 255.
      WHILE lv_cut > 1 AND substring( val = lv_rest off = lv_cut - 1 len = 1 ) = ` `.
        lv_cut = lv_cut - 1.
      ENDWHILE.
      APPEND lv_rest(lv_cut) TO ct_html.
      lv_rest = lv_rest+lv_cut.
    ENDWHILE.
    APPEND lv_rest TO ct_html.
  ENDMETHOD.

ENDCLASS.
