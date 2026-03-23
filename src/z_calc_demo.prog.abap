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

  BEGIN OF ty_input,
    amount    TYPE ty_amount,  " Loan principal
    months    TYPE i,           " Term in months (1..360)
    rate_year TYPE ty_rate,    " Annual interest rate (%)
  END OF ty_input,

  BEGIN OF ty_result,
    monthly_payment TYPE ty_amount,  " Annuity monthly payment
    total_payment   TYPE ty_amount,  " Total paid over full term
    overpayment     TYPE ty_amount,  " Total interest paid
  END OF ty_result,

  BEGIN OF ty_schedule_line,
    month     TYPE i,
    payment   TYPE ty_amount,   " Monthly payment
    principal TYPE ty_amount,   " Principal portion
    interest  TYPE ty_amount,   " Interest portion
    balance   TYPE ty_amount,   " Remaining balance
  END OF ty_schedule_line,

  ty_schedule TYPE STANDARD TABLE OF ty_schedule_line WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* CLASS lcl_validator — checks input parameters
*----------------------------------------------------------------------*
CLASS lcl_validator DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS validate
      IMPORTING is_input TYPE ty_input
      RAISING   cx_parameter_invalid.
ENDCLASS.

CLASS lcl_validator IMPLEMENTATION.
  METHOD validate.
    IF is_input-amount <= 0.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING parameter = 'AMOUNT'.
    ENDIF.
    IF is_input-months <= 0 OR is_input-months > 360.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING parameter = 'MONTHS'.
    ENDIF.
    IF is_input-rate_year <= 0 OR is_input-rate_year > 100.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING parameter = 'RATE_YEAR'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_annuity — pure annuity math
*----------------------------------------------------------------------*
CLASS lcl_annuity DEFINITION FINAL.
  PUBLIC SECTION.
    " Computes annuity factor: r*(1+r)^n / ((1+r)^n - 1)
    CLASS-METHODS get_factor
      IMPORTING iv_rate_monthly TYPE f
                iv_months       TYPE i
      RETURNING VALUE(rv_factor) TYPE f.

    " Monthly payment = principal * annuity factor
    CLASS-METHODS monthly_payment
      IMPORTING iv_amount       TYPE ty_amount
                iv_rate_monthly TYPE f
                iv_months       TYPE i
      RETURNING VALUE(rv_payment) TYPE ty_amount.

    " Total paid = monthly payment * months
    CLASS-METHODS total_payment
      IMPORTING iv_monthly_payment TYPE ty_amount
                iv_months          TYPE i
      RETURNING VALUE(rv_total) TYPE ty_amount.

    " Overpayment = total paid - principal
    CLASS-METHODS overpayment
      IMPORTING iv_total  TYPE ty_amount
                iv_amount TYPE ty_amount
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
* CLASS lcl_schedule_builder — builds monthly payment schedule
*----------------------------------------------------------------------*
CLASS lcl_schedule_builder DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS build
      IMPORTING is_input           TYPE ty_input
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
    lv_balance = is_input-amount.
    lv_payment = iv_monthly_payment.

    DO is_input-months TIMES.
      ls_line-month = sy-index.
      lv_interest   = lv_balance * iv_rate_monthly.
      lv_principal  = lv_payment - lv_interest.
      lv_balance    = lv_balance - lv_principal.

      " Absorb rounding remainder into last payment
      IF sy-index = is_input-months.
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
* CLASS lcl_display — output to screen
*----------------------------------------------------------------------*
CLASS lcl_display DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS show_summary
      IMPORTING is_input  TYPE ty_input
                is_result TYPE ty_result.

    CLASS-METHODS show_schedule
      IMPORTING it_schedule TYPE ty_schedule.
ENDCLASS.

CLASS lcl_display IMPLEMENTATION.
  METHOD show_summary.
    WRITE: / '==================================================='.
    WRITE: / '       CREDIT CALCULATION  (annuity method)'.
    WRITE: / '==================================================='.
    WRITE: / 'Principal:       ', is_input-amount        CURRENCY 'USD'.
    WRITE: / 'Term (months):   ', is_input-months.
    WRITE: / 'Annual rate:     ', is_input-rate_year, '%'.
    WRITE: / '---------------------------------------------------'.
    WRITE: / 'Monthly payment: ', is_result-monthly_payment CURRENCY 'USD'.
    WRITE: / 'Total payment:   ', is_result-total_payment   CURRENCY 'USD'.
    WRITE: / 'Overpayment:     ', is_result-overpayment     CURRENCY 'USD'.
    WRITE: / '==================================================='.
    SKIP.
  ENDMETHOD.

  METHOD show_schedule.
    WRITE: / 'PAYMENT SCHEDULE:'.
    WRITE: /   sy-vline, 'Mon'     ,  6 sy-vline,
                         'Payment' , 20 sy-vline,
                         'Principal', 34 sy-vline,
                         'Interest' , 48 sy-vline,
                         'Balance'  , 62 sy-vline.
    WRITE: / '--------------------------------------------------------------'.

    LOOP AT it_schedule INTO DATA(ls_line).
      WRITE: /   sy-vline, ls_line-month              ,
              6  sy-vline, ls_line-payment   CURRENCY 'USD',
              20 sy-vline, ls_line-principal CURRENCY 'USD',
              34 sy-vline, ls_line-interest  CURRENCY 'USD',
              48 sy-vline, ls_line-balance   CURRENCY 'USD',
              62 sy-vline.
    ENDLOOP.

    WRITE: / '--------------------------------------------------------------'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_calculator — facade: orchestrates all other local classes
*----------------------------------------------------------------------*
CLASS lcl_calculator DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING is_input TYPE ty_input
      RAISING   cx_parameter_invalid.

    METHODS run.

  PRIVATE SECTION.
    DATA ms_input        TYPE ty_input.
    DATA ms_result       TYPE ty_result.
    DATA mt_schedule     TYPE ty_schedule.
    DATA mv_rate_monthly TYPE f.
ENDCLASS.

CLASS lcl_calculator IMPLEMENTATION.
  METHOD constructor.
    lcl_validator=>validate( is_input ).
    ms_input        = is_input.
    mv_rate_monthly = ms_input-rate_year / 100 / 12.
  ENDMETHOD.

  METHOD run.
    " Calculate each result component via dedicated class
    ms_result-monthly_payment = lcl_annuity=>monthly_payment(
      iv_amount       = ms_input-amount
      iv_rate_monthly = mv_rate_monthly
      iv_months       = ms_input-months ).

    ms_result-total_payment = lcl_annuity=>total_payment(
      iv_monthly_payment = ms_result-monthly_payment
      iv_months          = ms_input-months ).

    ms_result-overpayment = lcl_annuity=>overpayment(
      iv_total  = ms_result-total_payment
      iv_amount = ms_input-amount ).

    lcl_schedule_builder=>build(
      EXPORTING is_input           = ms_input
                iv_monthly_payment = ms_result-monthly_payment
                iv_rate_monthly    = mv_rate_monthly
      IMPORTING et_schedule        = mt_schedule ).

    lcl_display=>show_summary(  is_input = ms_input  is_result = ms_result ).
    lcl_display=>show_schedule( mt_schedule ).
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

AT SELECTION-SCREEN.
  DATA(ls_check) = VALUE ty_input(
    amount    = p_amount
    months    = p_months
    rate_year = p_rate ).
  TRY.
      lcl_validator=>validate( ls_check ).
    CATCH cx_parameter_invalid INTO DATA(lx).
      MESSAGE |Invalid parameter: { lx->parameter }| TYPE 'E'.
  ENDTRY.

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.
  TRY.
      DATA(lo_calc) = NEW lcl_calculator(
        VALUE ty_input(
          amount    = p_amount
          months    = p_months
          rate_year = p_rate ) ).
      lo_calc->run( ).
    CATCH cx_parameter_invalid INTO DATA(lx_err).
      MESSAGE |Calculation error: { lx_err->parameter }| TYPE 'E'.
  ENDTRY.
