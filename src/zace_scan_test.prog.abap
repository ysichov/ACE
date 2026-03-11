REPORT zace_scan_test.

data gv type i.

gv = gv + 1.

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

FORM my_form USING p_val TYPE i.
  DATA a TYPE i.
  a = p_val.
ENDFORM.

START-OF-SELECTION.
  " dummy - real logic in ZADT_SCAN_DEBUG FM below
  WRITE: / 'run ZADT_SCAN_DEBUG via RFC'.

  DATA(l_url) = 'https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/'.
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = l_url.
