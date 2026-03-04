class ZCL_ACE_APPL definition
  public
  create public .

public section.

  types:
    BEGIN OF selection_display_s,
          ind         TYPE i,
          field_label TYPE lvc_fname, "label
          int_type(1),
          inherited   TYPE aqadh_type_of_icon,
          sign        TYPE tvarv_sign,
          opti        TYPE tvarv_opti,
          option_icon TYPE aqadh_type_of_icon,
          low         TYPE string,
          high        TYPE string,
          more_icon   TYPE aqadh_type_of_icon,
          range       TYPE aqadh_t_ranges,
          name        TYPE reptext,
          element     TYPE text60,
          domain      TYPE text60,
          datatype    TYPE string,
          length      TYPE i,
          color       TYPE lvc_t_scol,
          style       TYPE lvc_t_styl,
        END OF selection_display_s .
  types:
    BEGIN OF t_sel_row,
          sign        TYPE tvarv_sign,
          opti        TYPE tvarv_opti,
          option_icon TYPE aqadh_type_of_icon,
          low         TYPE string, "aqadh_range_value,
          high        TYPE string, "aqadh_range_value,
          more_icon   TYPE aqadh_type_of_icon,
          range       TYPE aqadh_t_ranges,
        END OF t_sel_row .
  types:
    BEGIN OF sign_option_icon_s,
               sign          TYPE tvarv_sign,
               option        TYPE tvarv_opti,
               icon_name(64) TYPE c,
               icon          TYPE aqadh_type_of_icon,
             END OF sign_option_icon_s .
  types:
    BEGIN OF var_table,
               step          TYPE i,
               stack         TYPE i,
               program(40)   TYPE c,
               eventtype(30) TYPE c,
               eventname(61) TYPE c,
               first         TYPE boolean,
               i_appear      TYPE boolean,
               del           TYPE boolean,
               leaf          TYPE string,
               name(1000)               ,
               path          TYPE string,
               short         TYPE string,
               key           TYPE salv_de_node_key,
               parent        TYPE string,
               cl_leaf       TYPE int4,
               ref           TYPE REF TO data,
               type          TYPE string,
               instance      TYPE string,
               objname       TYPE string,
               done          TYPE boolean,
             END OF var_table .
  types:
    t_var_table TYPE STANDARD TABLE OF var_table WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF var_table_temp,
               step          TYPE i,
               stack         TYPE i,
               eventtype(30) TYPE c,
               eventname(61) TYPE c,
               name          TYPE string,
               value         TYPE string,
               first         TYPE boolean,
               i_appear      TYPE boolean,
               del           TYPE boolean,
               program(40)   TYPE c,
               leaf          TYPE string,
               path          TYPE string,
               type          TYPE string,
               instance      TYPE string,
               objname       TYPE string,
               ref           TYPE REF TO data,
             END OF var_table_temp .
  types:
    BEGIN OF var_table_h,
               step          TYPE i,
               program(40)   TYPE c,
               eventtype(30) TYPE c,
               eventname(61) TYPE c,
               leaf          TYPE string,
               name          TYPE string,
               path          TYPE string,
               parent        TYPE string,
               short         TYPE string,
               cl_leaf       TYPE int4,  "?
               ref           TYPE REF TO data,
               tree          TYPE REF TO ZCL_ACE_RTTI_TREE,
               time          LIKE sy-uname,
             END OF var_table_h .
  types:
    BEGIN OF t_obj,
               name       TYPE string,
               alv_viewer TYPE REF TO ZCL_ACE_TABLE_VIEWER,
             END OF t_obj .
  types:
    BEGIN OF t_popup,
               parent TYPE REF TO cl_gui_dialogbox_container,
               child  TYPE REF TO cl_gui_dialogbox_container,
             END OF t_popup .
  types:
    BEGIN OF t_classes_types,
               name TYPE string,
               full TYPE string,
               type TYPE char1,
               key  TYPE salv_de_node_key,
             END OF t_classes_types .
  types:
    BEGIN OF t_lang,
               spras(4),
               sptxt    TYPE sptxt,
             END OF t_lang .
  types:
    BEGIN OF t_stack,
               step       TYPE i,
               stacklevel TYPE tpda_stack_level,
               line       TYPE tpda_sc_line,
               program    TYPE tpda_program,
               eventtype  TYPE tpda_event_type,
               eventname  TYPE tpda_event,
               prg        TYPE program,
               include    TYPE tpda_include,
             END OF t_stack .
  types:
    BEGIN OF t_step_counter,
               step       TYPE i,
               stacklevel TYPE tpda_stack_level,
               line       TYPE tpda_sc_line,
               eventtype  TYPE string,
               eventname  TYPE string,
               class      TYPE string,
               first      TYPE boolean,
               last       TYPE boolean,
               program    TYPE tpda_program,
               include    TYPE tpda_include,
               time       LIKE sy-uzeit,
             END OF t_step_counter .
  types:
    BEGIN OF ts_calls,
               class TYPE string,
               event TYPE string,
               type  TYPE string,
               name  TYPE string,
               outer TYPE string,
               inner TYPE string,
               super TYPE boolean,
             END OF ts_calls .
  types:
    tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer .
  types:
    BEGIN OF ts_kword,
               program    TYPE string,
               include    TYPE string,
               index      TYPE i,
               line       TYPE i,
               v_line     TYPE i, "virtual line in code Mix
               v_from_row TYPE i, "virtual from_row after enhancement inserts
               v_to_row   TYPE i, "virtual to_row after enhancement inserts
               sub        TYPE boolean, "subcode: class/form...
               name       TYPE string,
               from       TYPE i,
               to         TYPE i,
               tt_calls   TYPE tt_calls,
             END OF ts_kword .
  types:
    tt_kword TYPE STANDARD TABLE OF ts_kword WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ts_calls_line,
               program     TYPE program,
               include     TYPE program,
               class       TYPE string,
               eventtype   TYPE string,
               meth_type   TYPE i,
               eventname   TYPE string,
               redefined   TYPE boolean,
               index       TYPE i,
               def_include TYPE program,  "include of CLASS DEFINITION section
               def_line    TYPE i,        "line of METHODS statement in definition section
             END OF ts_calls_line .
  types:
    tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY .
  types:
    BEGIN OF ts_vars,
               program   TYPE program,
               include   TYPE program,
               class     TYPE string,
               eventtype TYPE string,
               eventname TYPE string,
               line      TYPE i,
               name      TYPE string,
               type      TYPE string,
               icon      TYPE salv_de_tree_image,
             END OF ts_vars .
  types:
    BEGIN OF ts_var,
               program   TYPE string,
               include   TYPE string,
               line      TYPE i,
               name(100) TYPE c,
               type      TYPE string,
             END OF ts_var .
  types:
    tt_calculated TYPE STANDARD TABLE OF ts_var WITH KEY program include line name .
  types:
    tt_composed   TYPE STANDARD TABLE OF ts_var WITH KEY program include line name .
  types:
    BEGIN OF ts_int_tabs,
               eventtype TYPE string,
               eventname TYPE string,
               name      TYPE string,
               type      TYPE string,
             END OF ts_int_tabs .
  types:
    tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY .
  types:
    BEGIN OF ts_params,
               program   TYPE program,
               include   TYPE program,
               class     TYPE string,
               event     TYPE string,
               name      TYPE string,
               type      TYPE char1,
               param     TYPE string,
               preferred TYPE char1,
               line      TYPE i,
             END OF ts_params .
  types:
    BEGIN OF ts_parse_state,
               prev            TYPE string,
               change          TYPE string,
               kw              TYPE string,
               word            TYPE string,
               new             TYPE boolean,
               count           TYPE i,
               lv_default      TYPE boolean,
               ref             TYPE boolean,
               class           TYPE boolean,
               preferred       TYPE boolean,
               method_type     TYPE i,
               class_name      TYPE string,
               eventtype       TYPE string,
               eventname       TYPE string,
               token           TYPE ts_kword,
               call            TYPE ts_calls,
               call_line       TYPE ts_calls_line,
               variable        TYPE ts_vars,
               tab             TYPE ts_int_tabs,
               tabs            TYPE tt_tabs,
               composed        TYPE ts_var,
               composed_vars   TYPE tt_composed,
               calculated      TYPE ts_var,
               calculated_vars TYPE tt_calculated,
               param           TYPE ts_params,
             END OF ts_parse_state .
  types:
    BEGIN OF ts_tree,
               kind(1),
               value    TYPE string,
               param    TYPE string,
               program  TYPE program,
               include  TYPE program,
               ev_type  TYPE string,
               ev_name  TYPE string,
               enh_id   TYPE i,
               var_name TYPE string,
             END OF ts_tree .
  types:
    BEGIN OF ts_call,
               include TYPE string,
               ev_name TYPE string,
             END OF ts_call .

  class-data:
    m_option_icons   TYPE TABLE OF sign_option_icon_s .
  class-data:
    mt_lang          TYPE TABLE OF t_lang .
  class-data:
    mt_obj           TYPE TABLE OF t_obj .
  class-data:
    mt_popups        TYPE TABLE OF t_popup .
  class-data I_MERMAID_ACTIVE type BOOLEAN .
  class-data:
    mt_sel TYPE TABLE OF selection_display_s .

  class-methods INIT_ICONS_TABLE .
  class-methods CHECK_MERMAID .
  class-methods OPEN_INT_TABLE
    importing
      !IT_TAB type ANY TABLE optional
      !IT_REF type ref to DATA optional
      !I_NAME type STRING
      !IO_WINDOW type ref to ZCL_ACE_WINDOW .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ACE_APPL IMPLEMENTATION.


  method CHECK_MERMAID.


      CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
        EXPORTING
          clskey        = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
        EXCEPTIONS
          not_specified = 1
          not_existing  = 2
          i_interface   = 3
          no_text       = 4
          inconsistent  = 5
          OTHERS        = 6.

      IF sy-subrc = 0.
        i_mermaid_active = abap_true.
      ENDIF.


  endmethod.


  method INIT_ICONS_TABLE.


      m_option_icons = VALUE #(
       ( sign = space option = space  icon_name = icon_led_inactive )
       ( sign = 'I'   option = 'EQ'   icon_name = icon_equal_green )
       ( sign = 'I'   option = 'NE'   icon_name = icon_not_equal_green )
       ( sign = 'I'   option = 'LT'   icon_name = icon_less_green )
       ( sign = 'I'   option = 'LE'   icon_name = icon_less_equal_green )
       ( sign = 'I'   option = 'GT'   icon_name = icon_greater_green )
       ( sign = 'I'   option = 'GE'   icon_name = icon_greater_equal_green )
       ( sign = 'I'   option = 'CP'   icon_name = icon_pattern_include_green )
       ( sign = 'I'   option = 'NP'   icon_name = icon_pattern_exclude_green )
       ( sign = 'I'   option = 'BT'   icon_name = icon_interval_include_green )
       ( sign = 'I'   option = 'NB'   icon_name = icon_interval_exclude_green )
       ( sign = 'E'   option = 'EQ'   icon_name = icon_equal_red )
       ( sign = 'E'   option = 'NE'   icon_name = icon_not_equal_red )
       ( sign = 'E'   option = 'LT'   icon_name = icon_less_red )
       ( sign = 'E'   option = 'LE'   icon_name = icon_less_equal_red )
       ( sign = 'E'   option = 'GT'   icon_name = icon_greater_red )
       ( sign = 'E'   option = 'GE'   icon_name = icon_greater_equal_red )
       ( sign = 'E'   option = 'CP'   icon_name = icon_pattern_include_red )
       ( sign = 'E'   option = 'NP'   icon_name = icon_pattern_exclude_red )
       ( sign = 'E'   option = 'BT'   icon_name = icon_interval_include_red )
       ( sign = 'E'   option = 'NB'   icon_name = icon_interval_exclude_red ) ).


  endmethod.


  method OPEN_INT_TABLE.


      DATA r_tab TYPE REF TO data.
      IF it_ref IS BOUND.
        r_tab = it_ref.
      ELSE.
        GET REFERENCE OF it_tab INTO r_tab.
      ENDIF.
      APPEND INITIAL LINE TO ZCL_ACE_APPL=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
      <obj>-alv_viewer = NEW #(  i_additional_name = i_name ir_tab = r_tab io_window = io_window ).
      <obj>-alv_viewer->mo_sel->raise_selection_done( ).


  endmethod.
ENDCLASS.
