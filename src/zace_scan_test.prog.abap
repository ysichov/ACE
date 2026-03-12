REPORT zace_scan_test.

data gv type i.

gv = gv + 1.

INTERFACE lif_abapmerge.
  METHODS: test_int2.
* abapmerge 0.16.7 - 2026-03-06T16:29:01.742Z
  CONSTANTS c_merge_timestamp TYPE string VALUE `2026-03-06T16:29:01.742Z`.
  CONSTANTS c_abapmerge_version TYPE string VALUE `0.16.7`.
ENDINTERFACE.
****************************************************

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS: run, helper.
  PRIVATE SECTION.
    METHODS: internal_calc.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA x TYPE i.
    x = 1.
  ENDMETHOD.

  METHOD helper.
    DATA y TYPE i.
    y = 2.
  ENDMETHOD.

  METHOD internal_calc.
    DATA z TYPE i.
    z = 3.
  ENDMETHOD.
ENDCLASS.

****************************************************
INTERFACE lif_abapmerge_marker.
  METHODS: test_int.
* abapmerge 0.16.7 - 2026-03-06T16:29:01.742Z
  CONSTANTS c_merge_timestamp TYPE string VALUE `2026-03-06T16:29:01.742Z`.
  CONSTANTS c_abapmerge_version TYPE string VALUE `0.16.7`.
ENDINTERFACE.
****************************************************


FORM my_form USING p_val TYPE i.
  DATA a TYPE i.
  a = p_val.
ENDFORM.

START-OF-SELECTION.
  " dummy - real logic in ZADT_SCAN_DEBUG FM below
  WRITE: / 'run ZADT_SCAN_DEBUG via RFC'.

  DATA(l_url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
