*&---------------------------------------------------------------------*
*& Program  : Z_CALC_DEMO
*& Description: Annuity credit calculator demo (local OOP)
*&---------------------------------------------------------------------*
PROGRAM z_calc_demo.
*----------------------------------------------------------------------*
* Local type definitions
*----------------------------------------------------------------------*
TYPES:
  ty_amount TYPE p LENGTH 13 DECIMALS 2,
  ty_rate   TYPE p LENGTH 5  DECIMALS 2,

  BEGIN OF ty_schedule_line,
    month     TYPE i,
    payment   TYPE ty_amount,
    principal TYPE ty_amount,
    interest  TYPE ty_amount,
    balance   TYPE ty_amount,
  END OF ty_schedule_line,

  ty_schedule TYPE STANDARD TABLE OF ty_schedule_line WITH DEFAULT KEY.


DATA lv_monthly TYPE ty_amount.
DATA lv_total   TYPE ty_amount.
DATA lv_over    TYPE ty_amount.


*----------------------------------------------------------------------*
* CLASS lcl_validator
*----------------------------------------------------------------------*
CLASS lcl_validator DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS validate
      IMPORTING iv_amount    TYPE ty_amount
                iv_months    TYPE i
                iv_rate_year TYPE ty_rate
      RAISING   cx_parameter_invalid.
ENDCLASS.

CLASS lcl_validator IMPLEMENTATION.
  METHOD validate.
    IF iv_amount <= 0.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          parameter = 'AMOUNT'.
    ENDIF.
    IF iv_months <= 0 OR iv_months > 360.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          parameter = 'MONTHS'.
    ENDIF.
    IF iv_rate_year <= 0 OR iv_rate_year > 100.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          parameter = 'RATE_YEAR'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_annuity — pure annuity math
*----------------------------------------------------------------------*
CLASS lcl_annuity DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get_factor
      IMPORTING iv_rate_monthly  TYPE f
                iv_months        TYPE i
      RETURNING VALUE(rv_factor) TYPE f.

    CLASS-METHODS monthly_payment
      IMPORTING iv_amount         TYPE ty_amount
                iv_rate_monthly   TYPE f
                iv_months         TYPE i
      RETURNING VALUE(rv_payment) TYPE ty_amount.

    CLASS-METHODS total_payment
      IMPORTING iv_monthly_payment TYPE ty_amount
                iv_months          TYPE i
      RETURNING VALUE(rv_total)    TYPE ty_amount.

    CLASS-METHODS overpayment
      IMPORTING iv_total              TYPE ty_amount
                iv_amount             TYPE ty_amount
      RETURNING VALUE(rv_overpayment) TYPE ty_amount.
ENDCLASS.

CLASS lcl_annuity IMPLEMENTATION.
  METHOD get_factor.
    DATA(lv_r) = iv_rate_monthly.
    DATA(lv_n) = iv_months.
    rv_factor = lv_r * ( 1 + lv_r ) ** lv_n
                     / ( ( 1 + lv_r ) ** lv_n - 1 ).
  ENDMETHOD.

  METHOD monthly_payment.
    DATA(lv_rate_monthly) = iv_rate_monthly.
    rv_payment = iv_amount * get_factor(
                   iv_rate_monthly = lv_rate_monthly
                   iv_months       = iv_months ).
  ENDMETHOD.

  METHOD total_payment.
    rv_total = iv_monthly_payment * iv_months.
  ENDMETHOD.

  METHOD overpayment.
    rv_overpayment = iv_total - iv_amount.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_schedule_builder
*----------------------------------------------------------------------*
CLASS lcl_schedule_builder DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS build
      IMPORTING iv_amount          TYPE ty_amount
                iv_months          TYPE i
                iv_monthly_payment TYPE ty_amount
                iv_rate_monthly    TYPE f
      EXPORTING et_schedule        TYPE ty_schedule.
ENDCLASS.

CLASS lcl_schedule_builder IMPLEMENTATION.
  METHOD build.
    DATA lv_balance   TYPE f.
    DATA lv_payment   TYPE f.
    DATA lv_interest  TYPE f.
    DATA lv_principal TYPE f.
    DATA ls_line      TYPE ty_schedule_line.

    CLEAR et_schedule.
    lv_balance = iv_amount.
    lv_payment = iv_monthly_payment.

    DO iv_months TIMES.
      ls_line-month = sy-index.
      lv_interest   = lv_balance * iv_rate_monthly.
      lv_principal  = lv_payment - lv_interest.
      lv_balance    = lv_balance - lv_principal.

      IF sy-index = iv_months.
        lv_principal = lv_principal + lv_balance.
        lv_balance   = 0.
      ENDIF.

      ls_line-payment   = lv_payment.
      ls_line-interest  = lv_interest.
      ls_line-principal = lv_principal.
      ls_line-balance   = lv_balance.
      APPEND ls_line TO et_schedule.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_display
*----------------------------------------------------------------------*
CLASS lcl_display DEFINITION FINAL.
  PUBLIC SECTION.
*    CLASS-METHODS show_summary
*      IMPORTING iv_amount          TYPE ty_amount
*                iv_months          TYPE i
*                iv_rate_year       TYPE ty_rate
*                iv_monthly_payment TYPE ty_amount
*                iv_total_payment   TYPE ty_amount
*                iv_overpayment     TYPE ty_amount.

*    CLASS-METHODS show_schedule
*      IMPORTING it_schedule TYPE ty_schedule.
ENDCLASS.

CLASS lcl_display IMPLEMENTATION.
*  METHOD show_summary.
*    WRITE: / '==================================================='.
*    WRITE: / '       CREDIT CALCULATION  (annuity method)'.
*    WRITE: / '==================================================='.
*    WRITE: / 'Principal:       ', iv_amount          CURRENCY 'USD'.
*    WRITE: / 'Term (months):   ', iv_months.
*    WRITE: / 'Annual rate:     ', iv_rate_year, '%'.
*    WRITE: / '---------------------------------------------------'.
*    WRITE: / 'Monthly payment: ', iv_monthly_payment CURRENCY 'USD'.
*    WRITE: / 'Total payment:   ', iv_total_payment   CURRENCY 'USD'.
*    WRITE: / 'Overpayment:     ', iv_overpayment     CURRENCY 'USD'.
*    WRITE: / '==================================================='.
*    SKIP.
*  ENDMETHOD.
*
*  METHOD show_schedule.
*    WRITE: / 'PAYMENT SCHEDULE:'.
*    WRITE: /   sy-vline, 'Mon'      ,  6 sy-vline,
*                         'Payment'  , 20 sy-vline,
*                         'Principal', 34 sy-vline,
*                         'Interest' , 48 sy-vline,
*                         'Balance'  , 62 sy-vline.
*    WRITE: / '--------------------------------------------------------------'.
*
*    LOOP AT it_schedule INTO DATA(ls_line).
*      WRITE: /   sy-vline, ls_line-month              ,
*              6  sy-vline, ls_line-payment   CURRENCY 'USD',
*              20 sy-vline, ls_line-principal CURRENCY 'USD',
*              34 sy-vline, ls_line-interest  CURRENCY 'USD',
*              48 sy-vline, ls_line-balance   CURRENCY 'USD',
*              62 sy-vline.
*    ENDLOOP.
*
*    WRITE: / '--------------------------------------------------------------'.
*  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_calculator — orchestrates all calculation steps
*----------------------------------------------------------------------*
CLASS lcl_calculator DEFINITION FINAL.
  PUBLIC SECTION.
    " Step 1: convert annual rate % to monthly fraction
    CLASS-METHODS get_rate_monthly
      IMPORTING iv_rate_year   TYPE ty_rate
      RETURNING VALUE(rv_rate) TYPE f.

    " Step 2: annuity monthly payment
    CLASS-METHODS get_monthly_payment
      IMPORTING iv_amount         TYPE ty_amount
                iv_rate_monthly   TYPE f
                iv_months         TYPE i
      RETURNING VALUE(rv_payment) TYPE ty_amount.

    " Step 3: total amount paid over full term
    CLASS-METHODS get_total_payment
      IMPORTING iv_monthly_payment TYPE ty_amount
                iv_months          TYPE i
      RETURNING VALUE(rv_total)    TYPE ty_amount.

    " Step 4: overpayment = total - principal
    CLASS-METHODS get_overpayment
      IMPORTING iv_total_payment TYPE ty_amount
                iv_amount        TYPE ty_amount
      RETURNING VALUE(rv_over)   TYPE ty_amount.

    " Orchestrator — validates input, computes all results, displays output
    CLASS-METHODS run
      IMPORTING iv_amount          TYPE ty_amount
                iv_months          TYPE i
                iv_rate_year       TYPE ty_rate
      EXPORTING ev_monthly_payment TYPE ty_amount
                ev_total_payment   TYPE ty_amount
                ev_overpayment     TYPE ty_amount
      RAISING   cx_parameter_invalid.
ENDCLASS.

CLASS lcl_calculator IMPLEMENTATION.

  METHOD get_rate_monthly.
    rv_rate = iv_rate_year / 100 / 12.
  ENDMETHOD.

  METHOD get_monthly_payment.
    rv_payment = lcl_annuity=>monthly_payment(
      iv_amount       = iv_amount
      iv_rate_monthly = iv_rate_monthly
      iv_months       = iv_months ).
  ENDMETHOD.

  METHOD get_total_payment.
    rv_total = lcl_annuity=>total_payment(
      iv_monthly_payment = iv_monthly_payment
      iv_months          = iv_months ).
  ENDMETHOD.

  METHOD get_overpayment.
    rv_over = lcl_annuity=>overpayment(
      iv_total  = iv_total_payment
      iv_amount = iv_amount ).
  ENDMETHOD.

  METHOD run.
    " Validate all input parameters before calculation
*    lcl_validator=>validate(
*      iv_amount    = iv_amount
*      iv_months    = iv_months
*      iv_rate_year = iv_rate_year ).

    " Step 1: annual rate → monthly fraction
    DATA(lv_rate) = get_rate_monthly(
      iv_rate_year = iv_rate_year ).

    " Step 2: monthly annuity payment
    ev_monthly_payment = get_monthly_payment(
      iv_amount       = iv_amount
      iv_rate_monthly = lv_rate
      iv_months       = iv_months ).

    " Step 3: total amount over full term
    ev_total_payment = get_total_payment(
      iv_monthly_payment = ev_monthly_payment
      iv_months          = iv_months ).

    " Step 4: overpayment = total - principal
    ev_overpayment = get_overpayment(
      iv_total_payment = ev_total_payment
      iv_amount        = iv_amount ).

    " Build and display the payment schedule
*    DATA lt_schedule TYPE ty_schedule.
*    lcl_schedule_builder=>build(
*      EXPORTING iv_amount          = iv_amount
*                iv_months          = iv_months
*                iv_monthly_payment = ev_monthly_payment
*                iv_rate_monthly    = lv_rate
*      IMPORTING et_schedule        = lt_schedule ).
*
*    lcl_display=>show_summary(
*      iv_amount          = iv_amount
*      iv_months          = iv_months
*      iv_rate_year       = iv_rate_year
*      iv_monthly_payment = ev_monthly_payment
*      iv_total_payment   = ev_total_payment
*      iv_overpayment     = ev_overpayment ).
*
*    lcl_display=>show_schedule( lt_schedule ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS:
    p_amount TYPE ty_amount DEFAULT '500000.00',
    p_months TYPE i         DEFAULT 24,
    p_rate   TYPE ty_rate   DEFAULT '12.00'.
SELECTION-SCREEN END OF BLOCK b1.

*AT SELECTION-SCREEN.
*  TRY.
*      lcl_validator=>validate(
*        iv_amount    = p_amount
*        iv_months    = p_months
*        iv_rate_year = p_rate ).
*    CATCH cx_parameter_invalid INTO DATA(lx).
*      MESSAGE |Invalid parameter: { lx->parameter }| TYPE 'E'.
*  ENDTRY.

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.


  TRY.
      lcl_calculator=>run(
        EXPORTING iv_amount    = p_amount
                  iv_months    = p_months
                  iv_rate_year = p_rate
        IMPORTING ev_monthly_payment = lv_monthly
                  ev_total_payment   = lv_total
                  ev_overpayment     = lv_over ).
    CATCH cx_parameter_invalid INTO DATA(lx_err).
      MESSAGE |Calculation error: { lx_err->parameter }| TYPE 'E'.
  ENDTRY.
