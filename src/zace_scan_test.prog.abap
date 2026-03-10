REPORT zace_scan_test.

INTERFACE lif.
  METHODS:
    get_elements,
    set_elements_changeable.
ENDINTERFACE.

START-OF-SELECTION.
DATA o_source TYPE REF TO cl_ci_source_include.
DATA o_scan   TYPE REF TO cl_ci_scan.
DATA o_proc   TYPE REF TO if_ci_kzn_statement_iterator.
DATA cnt      TYPE i.

o_source = cl_ci_source_include=>create( p_name = sy-repid ).
o_scan   = NEW cl_ci_scan( p_include = o_source ).
o_proc   = cl_cikzn_scan_iterator_factory=>get_procedure_iterator( ciscan = o_scan ).

DO.
  TRY. o_proc->next( ). CATCH cx_scan_iterator_reached_end. EXIT. ENDTRY.
  cnt += 1.
  DATA(kw)  = o_proc->get_keyword( ).
  IF kw <> 'METHODS' AND kw <> 'CLASS-METHODS'. CONTINUE. ENDIF.

  DATA(idx) = o_proc->statement_index.
  READ TABLE o_scan->statements INDEX idx INTO DATA(stmt).

  " tokens at from, from+1, from+2, from+3
  DATA(t0s) = o_scan->tokens[ stmt-from     ]-str.
  DATA(t0r) = o_scan->tokens[ stmt-from     ]-row.
  DATA(t1s) = o_scan->tokens[ stmt-from + 1 ]-str.
  DATA(t1r) = o_scan->tokens[ stmt-from + 1 ]-row.
  DATA(t2s) = o_scan->tokens[ stmt-from + 2 ]-str.
  DATA(t2r) = o_scan->tokens[ stmt-from + 2 ]-row.
  DATA(t3s) = o_scan->tokens[ stmt-from + 3 ]-str.
  DATA(t3r) = o_scan->tokens[ stmt-from + 3 ]-row.

  " get_token offsets
  DATA(g1) = o_proc->get_token( offset = 1 ).
  DATA(g2) = o_proc->get_token( offset = 2 ).
  DATA(g3) = o_proc->get_token( offset = 3 ).
  DATA(g4) = o_proc->get_token( offset = 4 ).

  WRITE: / |--- stmt[{ idx }] from={ stmt-from } to={ stmt-to }|.
  WRITE: / |  t[from+0]={ t0s } row={ t0r }|.
  WRITE: / |  t[from+1]={ t1s } row={ t1r }|.
  WRITE: / |  t[from+2]={ t2s } row={ t2r }|.
  WRITE: / |  t[from+3]={ t3s } row={ t3r }|.
  WRITE: / |  get_token(1)={ g1 } (2)={ g2 } (3)={ g3 } (4)={ g4 }|.
ENDDO.
