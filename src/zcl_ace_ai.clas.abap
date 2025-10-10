class ZCL_ACE_AI definition
  public
  inheriting from ZCL_ACE_POPUP
  create public .

public section.

  data MO_AI_BOX type ref to CL_GUI_DIALOGBOX_CONTAINER .
  data MO_AI_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_AI_TOOLBAR_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_AI_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_PROMPT_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_ANSWER_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_PROMPT_TEXT type ref to CL_GUI_TEXTEDIT .
  data MO_ANSWER_TEXT type ref to CL_GUI_TEXTEDIT .
  data MV_PROMPT type STRING .
  data MV_ANSWER type STRING .
  data MO_VIEWER type ref to ZCL_ACE .

  methods CONSTRUCTOR
    importing
      !I_SOURCE type SCI_INCLUDE
      !IO_PARENT type ref to CL_GUI_DIALOGBOX_CONTAINER .
  methods ADD_AI_TOOLBAR_BUTTONS .
  methods HND_AI_TOOLBAR
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_AI IMPLEMENTATION.


  METHOD add_ai_toolbar_buttons.

    DATA: buttons TYPE ttb_button,
          events TYPE cntl_simple_events,
          event LIKE LINE OF events.

   buttons  = VALUE #(
     ( function = 'AI' icon = CONV #( icon_manikin_unknown_gender ) quickinfo = 'Ask AI' text = 'Ask AI' ) ).

    mo_ai_toolbar->add_button_group( buttons ).

*   Register events
    event-eventid = cl_gui_toolbar=>m_id_function_selected.
    event-appl_event = space.
    APPEND event TO events.

    mo_ai_toolbar->set_registered_events( events = events ).
    SET HANDLER me->hnd_ai_toolbar FOR mo_ai_toolbar.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mo_ai_box = create( i_name = 'ACE: Abap Code Explorer - AI chat' i_width = 1400 i_hight = 400 ).
    CREATE OBJECT mo_ai_splitter
      EXPORTING
        parent  = mo_ai_box
        rows    = 3
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    "save new popup ref
    APPEND INITIAL LINE TO zcl_ace_appl=>mt_popups ASSIGNING FIELD-SYMBOL(<popup>).
    <popup>-parent = io_parent.
    <popup>-child = mo_ai_box.

    SET HANDLER on_box_close FOR mo_ai_box.

    mo_ai_splitter->get_container(
         EXPORTING
           row       = 1
           column    = 1
         RECEIVING
           container = mo_ai_toolbar_container ).

    mo_ai_splitter->get_container(
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = mo_prompt_container ).

    mo_ai_splitter->get_container(
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = mo_answer_container  ).

    mo_ai_splitter->set_row_height( id = 1 height = '3' ).

    mo_ai_splitter->set_row_sash( id    = 1
                                  type  = 0
                                  value = 0 ).


    SET HANDLER on_box_close FOR mo_ai_box.


    CREATE OBJECT mo_prompt_text
      EXPORTING
        parent                 = mo_prompt_container
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      on_box_close( mo_box ).
    ENDIF.

    CREATE OBJECT mo_answer_text
      EXPORTING
        parent                 = mo_answer_container
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      on_box_close( mo_box ).
    ENDIF.

    mo_answer_text->set_readonly_mode( ).

    CREATE OBJECT mo_ai_toolbar EXPORTING parent = mo_ai_toolbar_container.
    add_ai_toolbar_buttons( ).
    mo_ai_toolbar->set_visible( 'X' ).

    "set prompt
    DATA lt_string TYPE TABLE OF char255.

    APPEND INITIAL LINE TO lt_string ASSIGNING FIELD-SYMBOL(<str>).
    <str> = 'Explain please the meaning of this ABAP code and provide a code review'.
    mv_prompt = <str>.
    APPEND INITIAL LINE TO lt_string ASSIGNING <str>.


    LOOP AT i_source INTO DATA(ls_line).
      APPEND INITIAL LINE TO lt_string ASSIGNING <str>.
      <str> = ls_line.
      mv_prompt = mv_prompt && <str>.
    ENDLOOP.

    mo_prompt_text->set_text_as_r3table( lt_string ).
    cl_gui_control=>set_focus( mo_ai_box ).


  ENDMETHOD.


  METHOD hnd_ai_toolbar.

    DATA: prompt TYPE string.

    CASE fcode.

      WHEN 'AI'.

        cl_gui_cfw=>flush( ).
        DATA(lo_ai) = NEW zcl_ace_ai_api( iv_model =  mo_viewer->mv_model  iv_dest = mo_viewer->mv_dest iv_apikey = mo_viewer->mv_apikey ).

        DATA lt_text TYPE TABLE OF char255.
        CALL METHOD mo_prompt_text->get_text_as_stream
          IMPORTING
            text = lt_text.
        CLEAR mv_prompt.
        LOOP AT lt_text INTO DATA(line).
          CONCATENATE mv_prompt line
                      "cl_abap_char_utilities=>newline
                 INTO mv_prompt.
        ENDLOOP.

        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN mv_prompt WITH ''.
        REPLACE ALL OCCURRENCES OF '#' IN mv_prompt WITH ''.
        REPLACE ALL OCCURRENCES OF '"' IN mv_prompt WITH ''''.
        REPLACE ALL OCCURRENCES OF REGEX '[[:cntrl:]]' IN mv_prompt WITH ' '.

        mv_answer = lo_ai->call_openai( mv_prompt ).
        mo_answer_text->set_textstream( mv_answer ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
