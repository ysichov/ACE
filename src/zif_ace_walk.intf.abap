"! <p class="shorttext synchronized">Code-flow walk context</p>
"! What ZCL_ACE_SOURCE_PARSER reads and writes while it walks a program.
"! It used to reach these six through a ZCL_ACE reference, which made the
"! walk - pure analysis - depend on the GUI controller and on a window it
"! never draws in. ZCL_ACE_WINDOW implements this interface and aliases the
"! names it already had, so every existing MO_WINDOW->MS_SOURCES keeps
"! meaning what it meant; ZCL_ACE_WALK implements it with nothing else, for
"! callers that have no SAP GUI at all.
INTERFACE zif_ace_walk PUBLIC.

  " The parse the walk reads from and fills as it resolves calls.
  DATA ms_sources TYPE zif_ace_parse_data=>ts_parse_data .
  " "Only Z" - whether the walk stays out of SAP's own code. Set, like the
  " window's constructor has always set it: a walk that starts cleared
  " descends into the standard, which is not what any caller has asked for.
  DATA m_zcode TYPE x VALUE '01' .
  " How deep the walk follows a call chain before it stops.
  DATA m_hist_depth TYPE i VALUE 19 .
  " Calls already descended into, so a cycle ends.
  DATA mt_calls TYPE zif_ace_parse_data=>tt_call .
  " The walk's answer: the units in the order the code would run them.
  DATA mt_steps TYPE zif_ace_parse_data=>tt_step_counter .
  " Number of the step written last.
  DATA m_step TYPE i .

ENDINTERFACE.
