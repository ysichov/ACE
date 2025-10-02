CLASS zcl_ace_alv_common DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      c_white(4) TYPE x VALUE '00000001' ##NO_TEXT.

    CLASS-METHODS refresh
      IMPORTING
        !i_obj    TYPE REF TO cl_gui_alv_grid
        !i_layout TYPE lvc_s_layo OPTIONAL
        !i_soft   TYPE char01 OPTIONAL .
    CLASS-METHODS translate_field
      IMPORTING
        !i_lang TYPE ddlanguage OPTIONAL
      CHANGING
        !c_fld  TYPE lvc_s_fcat .
    CLASS-METHODS get_selected
      IMPORTING
        !i_obj         TYPE REF TO cl_gui_alv_grid
      RETURNING
        VALUE(e_index) TYPE i .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_ALV_COMMON IMPLEMENTATION.


  METHOD get_selected.

    i_obj->get_selected_cells( IMPORTING et_cell = DATA(lt_sel_cells) ).
    IF lines( lt_sel_cells ) > 0.
      e_index = lt_sel_cells[ 1 ]-row_id.
    ELSE.
      i_obj->get_selected_rows( IMPORTING et_index_rows = DATA(lt_sel_rows) ).
      IF lines( lt_sel_rows ) > 0.
        e_index = lt_sel_rows[ 1 ]-index.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD refresh.

    DATA l_stable TYPE lvc_s_stbl.
    l_stable = 'XX'.
    IF i_layout IS SUPPLIED.
      i_obj->set_frontend_layout( i_layout ).
    ENDIF.
    i_obj->refresh_table_display( EXPORTING is_stable = l_stable i_soft_refresh = i_soft ).

  ENDMETHOD.


  METHOD translate_field.

    DATA: lt_field_info TYPE TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = c_fld-tabname
        fieldname      = c_fld-fieldname
        langu          = i_lang
      TABLES
        dfies_tab      = lt_field_info
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      READ TABLE lt_field_info INDEX 1 INTO DATA(l_info).
      IF l_info-scrtext_l IS INITIAL AND l_info-scrtext_m IS INITIAL AND l_info-scrtext_s IS INITIAL.
        IF l_info-fieldtext IS NOT INITIAL.
          MOVE l_info-fieldtext TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ELSE.
          MOVE l_info-fieldname TO: c_fld-reptext, c_fld-scrtext_l, c_fld-scrtext_m, c_fld-scrtext_s .
        ENDIF.
      ELSE.
        c_fld-scrtext_l = l_info-scrtext_l.
        c_fld-scrtext_m = l_info-scrtext_m.
        c_fld-scrtext_s = l_info-scrtext_s.
        IF l_info-reptext IS NOT INITIAL.
          c_fld-reptext   = l_info-reptext.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
