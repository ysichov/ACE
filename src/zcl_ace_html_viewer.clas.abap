class ZCL_ACE_HTML_VIEWER definition
  public
  inheriting from ZCL_ACE_POPUP
  final
  create public .

public section.

  data MO_HTML type ref to CL_GUI_HTML_VIEWER .

  methods CONSTRUCTOR
    importing
      !IT_HTML type W3HTMLTAB
      !I_TITLE type TEXT100 default 'HTML'
      !I_WIDTH type I default 800
      !I_HEIGHT type I default 400 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_HTML_VIEWER IMPLEMENTATION.


  method CONSTRUCTOR.

      super->constructor( ).
      mo_box = create( i_name = i_title i_width = i_width i_hight = i_height ).
      IF mo_box IS INITIAL. RETURN. ENDIF.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_box
          rows    = 1
          columns = 1
        EXCEPTIONS
          OTHERS  = 1.

      mo_splitter->get_container(
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_variables_container ).

      SET HANDLER on_box_close FOR mo_box.

      CREATE OBJECT mo_html
        EXPORTING
          parent             = mo_variables_container
        EXCEPTIONS
          cntl_error         = 1
          cntl_install_error = 2
          dp_install_error   = 3
          dp_error           = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        on_box_close( mo_box ).
        RETURN.
      ENDIF.

      DATA lt_data TYPE w3htmltab.
      DATA lv_url  TYPE w3url.
      lt_data = it_html.

      mo_html->load_data(
        IMPORTING
          assigned_url = lv_url
        CHANGING
          data_table   = lt_data
        EXCEPTIONS
          OTHERS       = 1 ).

      mo_html->show_url( url = lv_url ).
      cl_gui_cfw=>flush( ).


  endmethod.
ENDCLASS.
