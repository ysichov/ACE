CLASS zcl_ace DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF selection_display_s,
        ind         TYPE i,
        field_label TYPE lvc_fname,
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
    TYPES:
      BEGIN OF t_sel_row,
        sign        TYPE tvarv_sign,
        opti        TYPE tvarv_opti,
        option_icon TYPE aqadh_type_of_icon,
        low         TYPE string,
        high        TYPE string,
        more_icon   TYPE aqadh_type_of_icon,
        range       TYPE aqadh_t_ranges,
      END OF t_sel_row .
    TYPES:
      BEGIN OF sign_option_icon_s,
        sign          TYPE tvarv_sign,
        option        TYPE tvarv_opti,
        icon_name(64) TYPE c,
        icon          TYPE aqadh_type_of_icon,
      END OF sign_option_icon_s .
    TYPES:
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
    TYPES:
      t_var_table TYPE STANDARD TABLE OF var_table WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
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
    TYPES:
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
        cl_leaf       TYPE int4,
        ref           TYPE REF TO data,
        tree          TYPE REF TO zcl_ace_rtti_tree,
        time          LIKE sy-uname,
      END OF var_table_h .
    TYPES:
      BEGIN OF t_obj,
        name       TYPE string,
        alv_viewer TYPE REF TO zcl_ace_table_viewer,
      END OF t_obj .
    TYPES:
      BEGIN OF t_popup,
        parent TYPE REF TO cl_gui_dialogbox_container,
        child  TYPE REF TO cl_gui_dialogbox_container,
      END OF t_popup .
    TYPES:
      BEGIN OF t_classes_types,
        name TYPE string,
        full TYPE string,
        type TYPE char1,
        key  TYPE salv_de_node_key,
      END OF t_classes_types .
    TYPES:
      BEGIN OF t_lang,
        spras(4),
        sptxt    TYPE sptxt,
      END OF t_lang .
    TYPES:
      BEGIN OF t_stack,
        step       TYPE i,
        stacklevel TYPE tpda_stack_level,
        line       TYPE tpda_sc_line,
        program    TYPE tpda_program,
        eventtype  TYPE string,
        eventname  TYPE tpda_event,
        prg        TYPE program,
        include    TYPE tpda_include,
      END OF t_stack .
    TYPES:
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

    TYPES:
      BEGIN OF ts_param_binding,
        outer TYPE string,
        inner TYPE string,
      END OF ts_param_binding .
    TYPES:
      tt_param_bindings TYPE STANDARD TABLE OF ts_param_binding WITH EMPTY KEY .

    TYPES:
      BEGIN OF ts_calls,
        class    TYPE string,
        event    TYPE string,
        type     TYPE string,
        name     TYPE string,
        outer    TYPE string,
        inner    TYPE string,
        super    TYPE boolean,
        bindings TYPE tt_param_bindings,
      END OF ts_calls .
    TYPES:
      tt_calls TYPE STANDARD TABLE OF ts_calls WITH NON-UNIQUE KEY outer .
    TYPES:
      BEGIN OF ts_kword,
        program      TYPE string,
        include      TYPE string,
        index        TYPE i,
        line         TYPE i,
        v_line       TYPE i,
        v_from_row   TYPE i,
        v_to_row     TYPE i,
        sub          TYPE boolean,
        name         TYPE string,
        from         TYPE i,
        to           TYPE i,
        tt_calls     TYPE tt_calls,
        calls_parsed TYPE abap_bool,
      END OF ts_kword .
    TYPES:
      tt_kword TYPE STANDARD TABLE OF ts_kword WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ts_calls_line,
        program     TYPE program,
        include     TYPE program,
        class       TYPE string,
        eventtype   TYPE string,
        meth_type   TYPE i,
        eventname   TYPE string,
        redefined   TYPE boolean,
        index       TYPE i,
        def_include TYPE program,
        def_line    TYPE i,
        is_intf     TYPE boolean,
        end_idx     TYPE i,
        run2_done   TYPE abap_bool,
      END OF ts_calls_line .
    TYPES:
      tt_calls_line TYPE STANDARD TABLE OF ts_calls_line WITH NON-UNIQUE EMPTY KEY .
    TYPES:
      BEGIN OF ts_vars,
        program   TYPE program,
        include   TYPE program,
        class     TYPE string,
        eventtype TYPE string,
        eventname TYPE string,
        name      TYPE string,
        line      TYPE i,
        type      TYPE string,
        icon      TYPE salv_de_tree_image,
      END OF ts_vars .
    TYPES:
      BEGIN OF ts_var,
        program   TYPE string,
        include   TYPE string,
        line      TYPE i,
        name(100) TYPE c,
        type      TYPE string,
      END OF ts_var .
    TYPES:
      tt_calculated TYPE STANDARD TABLE OF ts_var WITH KEY program include line name .
    TYPES:
      tt_composed   TYPE STANDARD TABLE OF ts_var WITH KEY program include line name .
    TYPES:
      BEGIN OF ts_int_tabs,
        eventtype TYPE string,
        eventname TYPE string,
        name      TYPE string,
        type      TYPE string,
      END OF ts_int_tabs .
    TYPES:
      tt_tabs TYPE STANDARD TABLE OF ts_int_tabs WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_params,
        program   TYPE program,
        include   TYPE program,
        class     TYPE string,
        event     TYPE string,
        name      TYPE string,
        param     TYPE string,
        type      TYPE char1,
        preferred TYPE char1,
        line      TYPE i,
      END OF ts_params .
    TYPES:
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
    TYPES:
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
    TYPES:
      BEGIN OF ts_call,
        include TYPE string,
        ev_name TYPE string,
        class   TYPE string,
      END OF ts_call .
    TYPES:
      BEGIN OF t_sel_var,
        name   TYPE string,
        i_sel  TYPE boolean,
        refval TYPE REF TO data,
      END OF t_sel_var .
    TYPES:
      BEGIN OF ts_if,
        if_ind      TYPE i,
        end_ind     TYPE i,
        before_else TYPE i,
      END OF ts_if .
    TYPES:
      tt_if TYPE STANDARD TABLE OF ts_if WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_line,
        cond        TYPE string,
        program     TYPE string,
        include     TYPE string,
        line        TYPE i,
        ind         TYPE i,
        class       TYPE string,
        ev_name     TYPE string,
        ev_type     TYPE string,
        stack       TYPE i,
        code        TYPE string,
        arrow       TYPE string,
        subname     TYPE string,
        del         TYPE flag,
        els_before  TYPE i,
        els_after   TYPE i,
        active_root TYPE flag,
      END OF ts_line .
    TYPES:
      tt_line TYPE TABLE OF ts_line WITH EMPTY KEY .

    CLASS-DATA:
      m_option_icons   TYPE TABLE OF sign_option_icon_s .
    CLASS-DATA:
      mt_lang          TYPE TABLE OF t_lang .
    CLASS-DATA:
      mt_obj           TYPE TABLE OF t_obj .
    CLASS-DATA:
      mt_popups        TYPE TABLE OF t_popup .
    CLASS-DATA i_mermaid_active TYPE boolean .
    CLASS-DATA:
      mt_sel TYPE TABLE OF selection_display_s .

    DATA mv_prog TYPE prog .
    DATA mv_show_prog TYPE prog .
    DATA mv_show_parse_time TYPE abap_bool .
    DATA mv_dest TYPE text255 .
    DATA mv_model TYPE text255 .
    DATA mv_apikey TYPE text255 .
    DATA:
      mt_compo          TYPE TABLE OF scompo .
    DATA mt_locals TYPE tpda_scr_locals_it .
    DATA mt_globals TYPE tpda_scr_globals_it .
    DATA mt_ret_exp TYPE tpda_scr_locals_it .
    DATA m_counter TYPE i .
    DATA:
      mt_steps          TYPE  TABLE OF zcl_ace=>t_step_counter WITH NON-UNIQUE KEY program include line eventtype eventname .
    DATA:
      mt_var_step       TYPE  TABLE OF zcl_ace=>var_table_h .
    DATA m_step TYPE i .
    DATA m_i_find TYPE boolean .
    DATA m_stop_stack TYPE i .
    DATA m_debug TYPE x .
    DATA m_refresh TYPE boolean .
    DATA m_update TYPE boolean .
    DATA i_step TYPE boolean .
    DATA ms_stack_prev TYPE zcl_ace=>t_stack .
    DATA ms_stack TYPE zcl_ace=>t_stack .
    DATA i_history TYPE boolean .
    DATA m_hist_step TYPE i .
    DATA m_step_delta TYPE i .
    DATA:
      mt_vars_hist_view TYPE STANDARD TABLE OF zcl_ace=>var_table .
    DATA:
      mt_vars_hist      TYPE STANDARD TABLE OF zcl_ace=>var_table .
    DATA:
      mt_state          TYPE STANDARD TABLE OF zcl_ace=>var_table .
    DATA mv_recurse TYPE i .
    DATA:
      mt_classes_types  TYPE TABLE OF zcl_ace=>t_classes_types .
    DATA mo_window TYPE REF TO zcl_ace_window .
    DATA mv_f7_stop TYPE boolean .
    DATA m_f6_level TYPE i .
    DATA m_target_stack TYPE i .
    DATA mo_tree_local TYPE REF TO zcl_ace_rtti_tree .
    DATA:
      mt_selected_var   TYPE TABLE OF t_sel_var .
    DATA mv_stack_changed TYPE boolean .
    DATA m_variable TYPE REF TO data .
    DATA:
      mt_new_string     TYPE TABLE OF  string .
    DATA m_quick TYPE tpda_scr_quick_info .
    DATA:
      mr_statements     TYPE RANGE OF string .
    DATA ms_if TYPE ts_if .
    DATA mt_if TYPE tt_if .

    CLASS-METHODS init_icons_table .
    CLASS-METHODS check_mermaid .
    CLASS-METHODS open_int_table
      IMPORTING
        !it_tab    TYPE ANY TABLE OPTIONAL
        !it_ref    TYPE REF TO data OPTIONAL
        !i_name    TYPE string
        !io_window TYPE REF TO zcl_ace_window .

    METHODS constructor
      IMPORTING
        !i_prog            TYPE prog
        !i_new_parser      TYPE abap_bool DEFAULT abap_false
        !i_show_parse_time TYPE abap_bool DEFAULT abap_false .
    METHODS show .
    METHODS add_class
      IMPORTING
        !i_class      TYPE string
        !i_refnode    TYPE salv_de_node_key
        !no_locals    TYPE boolean OPTIONAL
        !i_tree       TYPE zcl_ace=>ts_tree OPTIONAL
        !i_type       TYPE flag OPTIONAL
      RETURNING
        VALUE(r_node) TYPE salv_de_node_key .
    METHODS build_local_classes_node
      IMPORTING
        !i_program          TYPE string
        !i_excl_class       TYPE string
        !i_refnode          TYPE salv_de_node_key
      RETURNING
        VALUE(r_locals_rel) TYPE salv_de_node_key .
    METHODS get_code_flow
      IMPORTING
        !i_calc_path   TYPE boolean OPTIONAL
      RETURNING
        VALUE(results) TYPE tt_line .
    METHODS get_code_mix
      IMPORTING
        !i_calc_path TYPE boolean OPTIONAL .
    METHODS mark_active_root
      IMPORTING
        !i_calc_path TYPE boolean OPTIONAL
      CHANGING
        !ct_results  TYPE tt_line .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_include_prefix
      IMPORTING
        !i_class         TYPE string
      RETURNING
        VALUE(rv_prefix) TYPE string .
    CONSTANTS:
      BEGIN OF c_kind,
        struct LIKE cl_abap_typedescr=>kind_struct VALUE cl_abap_typedescr=>kind_struct,
        table  LIKE cl_abap_typedescr=>kind_table  VALUE cl_abap_typedescr=>kind_table,
        elem   LIKE cl_abap_typedescr=>kind_elem   VALUE cl_abap_typedescr=>kind_elem,
        class  LIKE cl_abap_typedescr=>kind_class  VALUE cl_abap_typedescr=>kind_class,
        intf   LIKE cl_abap_typedescr=>kind_intf   VALUE cl_abap_typedescr=>kind_intf,
        ref    LIKE cl_abap_typedescr=>kind_ref    VALUE cl_abap_typedescr=>kind_ref,
      END OF c_kind .
ENDCLASS.

CLASS zcl_ace IMPLEMENTATION.
  METHOD check_mermaid.
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING clskey = 'ZCL_WD_GUI_MERMAID_JS_DIAGRAM '
      EXCEPTIONS not_specified = 1 not_existing = 2 i_interface = 3
                 no_text = 4 inconsistent = 5 OTHERS = 6.
    IF sy-subrc = 0. i_mermaid_active = abap_true. ENDIF.
  ENDMETHOD.
  METHOD init_icons_table.
    m_option_icons = VALUE #(
     ( sign = space option = space  icon_name = icon_led_inactive )
     ( sign = 'I' option = 'EQ' icon_name = icon_equal_green )
     ( sign = 'I' option = 'NE' icon_name = icon_not_equal_green )
     ( sign = 'I' option = 'LT' icon_name = icon_less_green )
     ( sign = 'I' option = 'LE' icon_name = icon_less_equal_green )
     ( sign = 'I' option = 'GT' icon_name = icon_greater_green )
     ( sign = 'I' option = 'GE' icon_name = icon_greater_equal_green )
     ( sign = 'I' option = 'CP' icon_name = icon_pattern_include_green )
     ( sign = 'I' option = 'NP' icon_name = icon_pattern_exclude_green )
     ( sign = 'I' option = 'BT' icon_name = icon_interval_include_green )
     ( sign = 'I' option = 'NB' icon_name = icon_interval_exclude_green )
     ( sign = 'E' option = 'EQ' icon_name = icon_equal_red )
     ( sign = 'E' option = 'NE' icon_name = icon_not_equal_red )
     ( sign = 'E' option = 'LT' icon_name = icon_less_red )
     ( sign = 'E' option = 'LE' icon_name = icon_less_equal_red )
     ( sign = 'E' option = 'GT' icon_name = icon_greater_red )
     ( sign = 'E' option = 'GE' icon_name = icon_greater_equal_red )
     ( sign = 'E' option = 'CP' icon_name = icon_pattern_include_red )
     ( sign = 'E' option = 'NP' icon_name = icon_pattern_exclude_red )
     ( sign = 'E' option = 'BT' icon_name = icon_interval_include_red )
     ( sign = 'E' option = 'NB' icon_name = icon_interval_exclude_red ) ).
  ENDMETHOD.
  METHOD open_int_table.
    DATA r_tab TYPE REF TO data.
    IF it_ref IS BOUND. r_tab = it_ref. ELSE. GET REFERENCE OF it_tab INTO r_tab. ENDIF.
    APPEND INITIAL LINE TO zcl_ace=>mt_obj ASSIGNING FIELD-SYMBOL(<obj>).
    <obj>-alv_viewer = NEW #( i_additional_name = i_name ir_tab = r_tab io_window = io_window ).
    <obj>-alv_viewer->mo_sel->raise_selection_done( ).
  ENDMETHOD.
  METHOD constructor.
    CONSTANTS: c_mask TYPE x VALUE '01'.
    mv_prog = i_prog. mv_show_parse_time = i_show_parse_time. i_step = abap_on.
    zcl_ace=>check_mermaid( ). zcl_ace=>init_icons_table( ).
    mo_window = NEW zcl_ace_window( me ). mo_window->mv_new_parser = i_new_parser.
    mo_tree_local = NEW zcl_ace_rtti_tree( i_header = 'Objects & Code Flow' i_type = 'L'
                                           i_cont = mo_window->mo_locals_container i_debugger = me ).
    show( ).
  ENDMETHOD.
  METHOD get_include_prefix.
    IF strlen( i_class ) >= 30. rv_prefix = i_class.
    ELSE. rv_prefix = i_class && repeat( val = `=` occ = 30 - strlen( i_class ) ) && `======`. ENDIF.
  ENDMETHOD.
  METHOD build_local_classes_node.
    DATA: local TYPE string, test_rel TYPE salv_de_node_key,
          intf_rel TYPE salv_de_node_key, lv_prefix TYPE string, lv_ccau TYPE string.
    lv_prefix = get_include_prefix( i_excl_class ). lv_ccau = lv_prefix && 'CCAU'.
    LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs)
      WHERE program = i_program AND class <> i_excl_class AND eventtype = 'METHOD'.
      IF local <> subs-class.
        IF subs-include = lv_ccau.
          IF test_rel IS INITIAL.
            test_rel = mo_tree_local->add_node( i_name = 'Unit Test Classes' i_icon = CONV #( icon_folder ) i_rel = i_refnode i_tree = VALUE #( ) ).
          ENDIF.
          add_class( i_class = subs-class i_refnode = test_rel no_locals = abap_true i_type = 'T' ).
        ELSEIF subs-is_intf = abap_true.
          IF intf_rel IS INITIAL.
            intf_rel = mo_tree_local->add_node( i_name = 'Interfaces' i_icon = CONV #( icon_oo_connection ) i_rel = i_refnode i_tree = VALUE #( ) ).
          ENDIF.
          add_class( i_class = subs-class i_refnode = intf_rel no_locals = abap_true i_type = 'I' ).
        ELSE.
          IF r_locals_rel IS INITIAL.
            r_locals_rel = mo_tree_local->add_node( i_name = 'Local Classes' i_icon = CONV #( icon_folder ) i_rel = i_refnode i_tree = VALUE #( ) ).
          ENDIF.
          add_class( i_class = subs-class i_refnode = r_locals_rel no_locals = abap_true ).
        ENDIF.
      ENDIF.
      local = subs-class.
    ENDLOOP.
  ENDMETHOD.
  METHOD add_class.
    DATA: tree TYPE zcl_ace=>ts_tree, splits_incl TYPE TABLE OF string,
          icon TYPE salv_de_tree_image, class_rel TYPE salv_de_node_key,
          attr_rel TYPE salv_de_node_key, include TYPE string, prefix TYPE string.
    IF i_type = 'I'. icon = icon_oo_connection.
    ELSEIF i_type = 'T'. icon = icon_test.
    ELSE. icon = icon_folder. ENDIF.
    LOOP AT mo_window->ms_sources-t_classes INTO DATA(ls_class) WHERE clsname = i_class AND reltype = '1'.
      IF class_rel IS INITIAL.
        class_rel = mo_tree_local->add_node( i_name = i_class i_icon = icon i_rel = i_refnode i_tree = i_tree ).
      ENDIF.
      add_class( i_class = CONV #( ls_class-refclsname ) i_refnode = class_rel no_locals = abap_true i_type = 'I' ).
    ENDLOOP.
    LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs) WHERE class = i_class AND eventtype = 'METHOD'.
      IF class_rel IS INITIAL.
        IF i_tree-kind = 'C'.
          class_rel = mo_tree_local->add_node( i_name = i_class i_icon = icon i_rel = i_refnode i_tree = i_tree ).
        ELSE.
          READ TABLE mo_window->ms_sources-tt_class_defs WITH KEY class = i_class INTO DATA(ls_cls_def).
          DATA(lv_cls_inc) = COND program( WHEN sy-subrc = 0 AND ls_cls_def-include IS NOT INITIAL THEN ls_cls_def-include
                                           WHEN subs-def_include IS NOT INITIAL THEN subs-def_include ELSE subs-include ).
          DATA(lv_cls_line) = COND i( WHEN sy-subrc = 0 AND ls_cls_def-line > 0 THEN ls_cls_def-line
                                      WHEN subs-def_line > 0 THEN subs-def_line ELSE 0 ).
          class_rel = mo_tree_local->add_node( i_name = i_class i_icon = icon i_rel = i_refnode
                        i_tree = VALUE #( param = |CLASS:{ i_class }| include = lv_cls_inc value = lv_cls_line ) ).
        ENDIF.
        IF i_tree-kind = 'C' AND i_type <> 'I' AND i_type <> 'T'.
          DATA(lv_sec_prefix) = get_include_prefix( i_class ).
          DATA(lt_sections) = VALUE string_table( ( lv_sec_prefix && `CU` ) ( lv_sec_prefix && `CO` ) ( lv_sec_prefix && `CI` ) ).
          DATA(lt_sec_labels) = VALUE string_table( ( `Public Section` ) ( `Protected Section` ) ( `Private Section` ) ).
          DATA(lv_sec_idx) = 0.
          LOOP AT lt_sections INTO DATA(lv_sec_inc).
            lv_sec_idx += 1.
            DATA(lv_sec_incl) = CONV program( lv_sec_inc ).
            READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = lv_sec_incl TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              mo_tree_local->add_node( i_name = lt_sec_labels[ lv_sec_idx ] i_icon = CONV #( icon_open_folder )
                i_rel = class_rel i_tree = VALUE #( kind = 'M' include = lv_sec_incl value = '1' ) ).
            ENDIF.
          ENDLOOP.
        ENDIF.
        DATA(lv_attr_cnt) = 0.
        LOOP AT mo_window->ms_sources-t_vars INTO DATA(var_cnt)
          WHERE program = subs-program AND class = subs-class AND eventname IS INITIAL.
          lv_attr_cnt += 1.
        ENDLOOP.
        IF lv_attr_cnt > 0.
          attr_rel = mo_tree_local->add_node( i_name = |Attributes ({ lv_attr_cnt })| i_icon = CONV #( icon_folder )
            i_rel = class_rel i_tree = VALUE #( param = |ATTR:{ i_class }| ) ).
          APPEND attr_rel TO mo_tree_local->mt_lazy_nodes.
        ENDIF.
      ENDIF.
      SPLIT subs-include AT '=' INTO TABLE splits_incl.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs-include INTO DATA(prog).
      READ TABLE prog-t_keywords WITH KEY index = subs-index INTO DATA(keyword).
      tree-kind = 'M'.
      IF subs-is_intf = abap_true AND subs-def_line > 0. tree-value = subs-def_line.
      ELSEIF keyword-v_line > 0. tree-value = keyword-v_line.
      ELSEIF keyword-line > 0. tree-value = keyword-line.
      ELSE.
        LOOP AT prog-t_keywords INTO DATA(lv_kw_fb) WHERE name = 'METHOD'.
          READ TABLE prog-scan->statements INDEX lv_kw_fb-index INTO DATA(ls_s_fb).
          IF sy-subrc = 0.
            READ TABLE prog-scan->tokens INDEX ls_s_fb-from + 1 INTO DATA(ls_t_fb).
            IF sy-subrc = 0 AND ls_t_fb-str = subs-eventname. tree-value = lv_kw_fb-line. EXIT. ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      tree-include = subs-include. tree-program = subs-program.
      tree-ev_type = subs-eventtype. tree-ev_name = subs-eventname. CLEAR tree-param.
      IF i_type = 'I'. icon = icon_oo_inst_method.
      ELSEIF subs-redefined = abap_false.
        CASE subs-meth_type.
          WHEN 0 OR 1. icon = icon_led_green.
          WHEN 2.      icon = icon_led_yellow.
          WHEN 3.      icon = icon_led_red.
          WHEN OTHERS. IF subs-eventname = 'CONSTRUCTOR'. icon = icon_tools. ENDIF.
        ENDCASE.
      ELSE. icon = icon_oo_overwrite. ENDIF.
      DATA(event_node) = mo_tree_local->add_node( i_name = subs-eventname i_icon = icon i_rel = class_rel i_tree = tree ).
      LOOP AT mo_window->ms_sources-t_params INTO DATA(lv_p)
        WHERE class = subs-class AND event = 'METHOD' AND name = subs-eventname AND param IS NOT INITIAL.
        DATA(lv_p_icon) = COND salv_de_tree_image( WHEN lv_p-type = 'I' THEN CONV #( icon_parameter_import )
                                                   ELSE                      CONV #( icon_parameter_export ) ).
        mo_tree_local->add_node( i_name = lv_p-param i_icon = lv_p_icon i_rel = event_node
          i_tree = VALUE #( value = lv_p-line include = lv_p-include var_name = lv_p-param ) ).
      ENDLOOP.
      DATA(lv_var_cnt) = 0.
      LOOP AT mo_window->ms_sources-t_vars INTO DATA(lv_v)
        WHERE program = subs-program AND class = subs-class AND eventtype = subs-eventtype AND eventname = subs-eventname.
        lv_var_cnt += 1.
      ENDLOOP.
      IF lv_var_cnt > 0.
        DATA(lv_vars_node) = mo_tree_local->add_node(
          i_name = |Local vars ({ lv_var_cnt })| i_icon = CONV #( icon_folder ) i_rel = event_node
          i_tree = VALUE #( program = subs-program include = subs-include ev_type = 'VARS'
                            ev_name = subs-eventname param = |VARS:{ subs-class }:{ subs-eventname }| ) ).
        APPEND lv_vars_node TO mo_tree_local->mt_lazy_nodes.
      ENDIF.
    ENDLOOP.
    IF no_locals = abap_false.
      prefix = get_include_prefix( i_class ). include = prefix && 'CP'.
      build_local_classes_node( i_program = include i_excl_class = i_class i_refnode = class_rel ).
    ENDIF.
    r_node = class_rel.
  ENDMETHOD.
  METHOD mark_active_root.
    DATA lv_tabix TYPE i.
    TYPES: BEGIN OF ts_pair, if_idx TYPE i, end_idx TYPE i, depth TYPE i, END OF ts_pair.
    DATA: lt_pairs TYPE TABLE OF ts_pair WITH EMPTY KEY, ls_pair TYPE ts_pair,
          lt_if_stack TYPE TABLE OF i WITH EMPTY KEY.
    LOOP AT ct_results ASSIGNING FIELD-SYMBOL(<ln>).
      lv_tabix = sy-tabix.
      CASE <ln>-cond.
        WHEN 'IF' OR 'CASE'. APPEND lv_tabix TO lt_if_stack.
        WHEN 'ENDIF' OR 'ENDCASE'.
          IF lt_if_stack IS NOT INITIAL.
            DATA(lv_if_tabix) = lt_if_stack[ lines( lt_if_stack ) ].
            DELETE lt_if_stack INDEX lines( lt_if_stack ).
            ls_pair-if_idx = lv_if_tabix. ls_pair-end_idx = lv_tabix. ls_pair-depth = lines( lt_if_stack ).
            APPEND ls_pair TO lt_pairs.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    SORT lt_pairs BY depth DESCENDING if_idx ASCENDING.
    TYPES: BEGIN OF ts_branch_hdr, tabix TYPE i, cond TYPE string, END OF ts_branch_hdr.
    DATA: lt_hdrs TYPE TABLE OF ts_branch_hdr WITH EMPTY KEY, ls_hdr TYPE ts_branch_hdr,
          lv_if_i TYPE i, lv_end_i TYPE i, lv_inner_depth TYPE i, lv_i TYPE i,
          lv_block_active TYPE flag, lv_hdr_cnt TYPE i, lv_h TYPE i,
          lv_branch_active TYPE flag, lv_scan TYPE i, lv_scan_inner TYPE i.
    LOOP AT lt_pairs INTO ls_pair.
      lv_if_i = ls_pair-if_idx. lv_end_i = ls_pair-end_idx. CLEAR: lt_hdrs, lv_inner_depth.
      lv_i = lv_if_i.
      WHILE lv_i <= lv_end_i.
        READ TABLE ct_results INDEX lv_i ASSIGNING <ln>. IF sy-subrc <> 0. EXIT. ENDIF.
        CASE <ln>-cond.
          WHEN 'IF' OR 'CASE'.
            IF lv_i = lv_if_i. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs.
            ELSE. lv_inner_depth += 1. ENDIF.
          WHEN 'ENDIF' OR 'ENDCASE'.
            IF lv_i = lv_end_i. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs.
            ELSE. lv_inner_depth -= 1. ENDIF.
          WHEN 'ELSEIF' OR 'ELSE' OR 'WHEN'.
            IF lv_inner_depth = 0. ls_hdr-tabix = lv_i. ls_hdr-cond = <ln>-cond. APPEND ls_hdr TO lt_hdrs. ENDIF.
        ENDCASE.
        lv_i += 1.
      ENDWHILE.
      CLEAR lv_block_active. lv_hdr_cnt = lines( lt_hdrs ). lv_h = 1.
      WHILE lv_h < lv_hdr_cnt.
        DATA(ls_hdr_cur) = lt_hdrs[ lv_h ]. DATA(ls_hdr_next) = lt_hdrs[ lv_h + 1 ].
        CLEAR: lv_branch_active, lv_scan_inner. lv_scan = ls_hdr_cur-tabix + 1.
        WHILE lv_scan < ls_hdr_next-tabix.
          READ TABLE ct_results INDEX lv_scan ASSIGNING FIELD-SYMBOL(<scan_ln>). IF sy-subrc <> 0. EXIT. ENDIF.
          CASE <scan_ln>-cond.
            WHEN 'IF' OR 'CASE'. lv_scan_inner += 1.
            WHEN 'ENDIF' OR 'ENDCASE'. lv_scan_inner -= 1.
            WHEN OTHERS. IF lv_scan_inner = 0 AND <scan_ln>-active_root = abap_true. lv_branch_active = abap_true. ENDIF.
          ENDCASE.
          IF lv_branch_active = abap_true. EXIT. ENDIF.
          lv_scan += 1.
        ENDWHILE.
        ASSIGN ct_results[ ls_hdr_cur-tabix ] TO FIELD-SYMBOL(<hdr_ln>).
        IF sy-subrc = 0.
          IF lv_branch_active = abap_true. <hdr_ln>-active_root = abap_true. lv_block_active = abap_true.
          ELSE. CLEAR <hdr_ln>-active_root. ENDIF.
        ENDIF.
        lv_h += 1.
      ENDWHILE.
      ASSIGN ct_results[ lv_end_i ] TO FIELD-SYMBOL(<endif_ln>).
      IF sy-subrc = 0. <endif_ln>-active_root = lv_block_active. ENDIF.
      ASSIGN ct_results[ lv_if_i ] TO FIELD-SYMBOL(<if_ln>).
      IF sy-subrc = 0.
        IF lv_block_active = abap_false. CLEAR <if_ln>-active_root.
        ELSE. <if_ln>-active_root = abap_true. ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_code_flow.
    DATA: add TYPE boolean, sub TYPE string, form TYPE string, direction TYPE string,
          ind2 TYPE i, start TYPE i, end TYPE i, bool TYPE string, block_first TYPE i,
          els_before TYPE i, inserted TYPE boolean.
    DATA: line TYPE ts_line, pre_stack TYPE ts_line, opened TYPE i.
    READ TABLE mo_window->ms_sources-tt_progs INDEX 1 INTO DATA(prog).
    DATA(lt_selected_var) = mt_selected_var.
    IF mo_window->ms_sel_call IS NOT INITIAL.
      CLEAR: mt_steps, mo_window->mt_calls.
      IF mo_window->ms_sel_call-eventtype = 'FORM'.
        zcl_ace_source_parser=>parse_call_form( EXPORTING i_call_name = mo_window->ms_sel_call-eventname
          i_program = mo_window->ms_sel_call-program i_include = mo_window->ms_sel_call-include
          i_stack = 0 io_debugger = mo_window->mo_viewer ).
      ELSE.
        zcl_ace_source_parser=>parse_call( EXPORTING i_index = mo_window->ms_sel_call-index
          i_e_name = mo_window->ms_sel_call-eventname i_e_type = mo_window->ms_sel_call-eventtype
          i_program = mo_window->ms_sel_call-program i_include = mo_window->ms_sel_call-include
          i_class = mo_window->ms_sel_call-class i_stack = 0 io_debugger = mo_window->mo_viewer ).
      ENDIF.
    ENDIF.
    DATA(steps) = mt_steps.
    SORT steps BY line eventtype eventname. DELETE ADJACENT DUPLICATES FROM steps. SORT steps BY step.
    DATA: yes TYPE xfeld.
    LOOP AT steps INTO DATA(step).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO DATA(keyword).
      LOOP AT keyword-tt_calls INTO DATA(call).
        READ TABLE lt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0 OR mt_selected_var IS INITIAL. yes = abap_true. ENDIF.
        READ TABLE lt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
        IF sy-subrc = 0 OR mt_selected_var IS INITIAL. yes = abap_true. ENDIF.
      ENDLOOP.
      IF yes = abap_true.
        LOOP AT keyword-tt_calls INTO call.
          READ TABLE lt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0. APPEND INITIAL LINE TO lt_selected_var ASSIGNING FIELD-SYMBOL(<selected>). <selected>-name = call-outer. ENDIF.
          READ TABLE lt_selected_var WITH KEY name = call-inner TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0. APPEND INITIAL LINE TO lt_selected_var ASSIGNING <selected>. <selected>-name = call-inner. ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    DATA: prev LIKE LINE OF mt_steps, pre_key TYPE string.
    READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
    LOOP AT steps ASSIGNING FIELD-SYMBOL(<step>).
      DATA(ind) = sy-tabix.
      READ TABLE prog-t_keywords WITH KEY line = <step>-line INTO DATA(key).
      IF prev IS NOT INITIAL.
        IF ( key-name = 'ENDDO' OR key-name = 'ENDWHILE' OR key-name = 'ENDLOOP' OR key-name = 'ENDIF' ) AND
           ( pre_key = 'DO' OR pre_key = 'LOOP' OR pre_key = 'WHILE' OR pre_key = 'IF' ).
          <step>-first = 'D'.
          READ TABLE mt_steps INDEX ind - 1 ASSIGNING FIELD-SYMBOL(<step_prev>). <step_prev>-first = 'D'.
        ENDIF.
      ENDIF.
      prev = <step>. pre_key = key-name.
    ENDLOOP.
    DELETE steps WHERE first = 'D'. SORT steps BY step DESCENDING.
    LOOP AT steps INTO step.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      LOOP AT mo_window->ms_sources-t_calculated INTO DATA(calculated_var) WHERE line = step-line.
        READ TABLE lt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          LOOP AT mo_window->ms_sources-t_composed INTO DATA(composed_var) WHERE line = step-line.
            READ TABLE lt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0. APPEND INITIAL LINE TO lt_selected_var ASSIGNING <selected>. <selected>-name = composed_var-name. ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO keyword.
      LOOP AT keyword-tt_calls INTO call.
        READ TABLE lt_selected_var WITH KEY name = call-outer TRANSPORTING NO FIELDS.
        IF sy-subrc = 0. APPEND INITIAL LINE TO lt_selected_var ASSIGNING <selected>. <selected>-name = call-inner. ENDIF.
      ENDLOOP.
    ENDLOOP.
    SORT lt_selected_var. DELETE ADJACENT DUPLICATES FROM lt_selected_var. CLEAR mo_window->mt_coverage.
    LOOP AT steps INTO step.
      CLEAR inserted.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = step-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = step-line INTO key.
      CLEAR line-cond.
      IF key-name = 'IF' OR key-name = 'ELSE' OR key-name = 'ENDIF' OR key-name = 'ELSEIF' OR
         key-name = 'CASE' OR key-name = 'WHEN' OR key-name = 'ENDCASE' OR
         key-name = 'DO' OR key-name = 'ENDDO' OR key-name = 'LOOP' OR key-name = 'ENDLOOP' OR
         key-name = 'WHILE' OR key-name = 'ENDWHILE'.
        APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING FIELD-SYMBOL(<watch>).
        <watch>-program = step-program. <watch>-line = line-line = step-line.
        INSERT line INTO results INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-cond = key-name. <line>-ev_name = step-eventname. <line>-stack = step-stacklevel.
        <line>-include = step-include. <line>-class = step-class. inserted = abap_true.
      ENDIF.
      CLEAR ind.
      LOOP AT mo_window->ms_sources-t_calculated INTO calculated_var WHERE line = step-line.
        ADD 1 TO ind.
        LOOP AT mo_window->ms_sources-t_composed INTO composed_var WHERE line = step-line.
          READ TABLE lt_selected_var WITH KEY name = composed_var-name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0. APPEND INITIAL LINE TO lt_selected_var ASSIGNING <selected>. <selected>-name = composed_var-name. ENDIF.
        ENDLOOP.
        READ TABLE lt_selected_var WITH KEY name = calculated_var-name TRANSPORTING NO FIELDS.
        IF sy-subrc = 0 OR mt_selected_var IS INITIAL.
          APPEND INITIAL LINE TO mo_window->mt_watch ASSIGNING <watch>.
          <watch>-program = step-program. <watch>-line = line-line = step-line.
          IF ind = 1.
            IF key-name <> 'PUBLIC' AND key-name <> 'ENDCLASS' AND key-name <> 'ENDFORM' AND
               key-name <> 'FORM' AND key-name <> 'METHOD' AND key-name <> 'METHODS' AND
               key-name <> 'ENDMETHOD' AND key-name <> 'MODULE' AND inserted = abap_false.
              line-ev_name = step-eventname. line-stack = step-stacklevel. line-include = step-include.
              line-class = step-class. line-ev_type = step-eventtype. line-active_root = abap_true.
              INSERT line INTO results INDEX 1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF inserted = abap_false.
        IF key-name <> 'PUBLIC' AND key-name <> 'ENDCLASS' AND key-name <> 'ENDFORM' AND
           key-name <> 'ENDMETHOD' AND key-name <> 'METHOD' AND key-name <> 'METHODS' AND
           key-name <> 'MODULE' AND key-name <> 'FORM' AND inserted = abap_false.
          READ TABLE results WITH KEY line = step-line include = step-include
            ev_type = step-eventtype ev_name = step-eventname TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            line-line = step-line. line-ev_name = step-eventname. line-stack = step-stacklevel.
            line-include = step-include. line-ev_type = step-eventtype. line-class = step-class.
            line-active_root = abap_false. INSERT line INTO results INDEX 1.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR inserted.
    ENDLOOP.
    DELETE results WHERE del = abap_true.
    DATA lv_changed TYPE boolean.
    DO.
      lv_changed = abap_false.
      LOOP AT results ASSIGNING <line>.
        DATA(lv_ti) = sy-tabix.
        IF <line>-cond = 'LOOP' OR <line>-cond = 'DO' OR <line>-cond = 'WHILE'.
          READ TABLE results INDEX lv_ti + 1 ASSIGNING FIELD-SYMBOL(<next>).
          IF sy-subrc = 0.
            IF ( <line>-cond = 'LOOP' AND <next>-cond = 'ENDLOOP' ) OR
               ( <line>-cond = 'DO'   AND <next>-cond = 'ENDDO'   ) OR
               ( <line>-cond = 'WHILE' AND <next>-cond = 'ENDWHILE' ).
              DELETE results INDEX lv_ti + 1. DELETE results INDEX lv_ti. lv_changed = abap_true. EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_changed = abap_false. EXIT. ENDIF.
    ENDDO.
    LOOP AT results ASSIGNING <line>.
      ind = sy-tabix.
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = <line>-include INTO prog.
      READ TABLE prog-t_keywords WITH KEY line = <line>-line INTO keyword.
      LOOP AT prog-scan->tokens FROM keyword-from TO keyword-to INTO DATA(token).
        IF token-str = 'USING' OR token-str = 'EXPORTING' OR token-str = 'IMPORTING' OR token-str = 'CHANGING'. EXIT. ENDIF.
        IF <line>-code IS INITIAL. <line>-code = token-str. ELSE. <line>-code = |{ <line>-code } { token-str }|. ENDIF.
      ENDLOOP.
      IF keyword-tt_calls IS NOT INITIAL.
        SORT keyword-tt_calls BY outer. DELETE ADJACENT DUPLICATES FROM keyword-tt_calls.
        LOOP AT keyword-tt_calls INTO call.
          <line>-subname = call-name.
          CHECK call-outer IS NOT INITIAL AND call-inner IS NOT INITIAL.
          IF sy-tabix <> 1. <line>-arrow = |{ <line>-arrow }, |. ENDIF.
          <line>-arrow = |{ <line>-arrow } { call-outer } { call-type } { call-inner }|.
          REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
          REPLACE ALL OCCURRENCES OF '"' IN <line>-code WITH ''.
        ENDLOOP.
      ENDIF.
      REPLACE ALL OCCURRENCES OF '''' IN <line>-subname WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-arrow WITH ''. REPLACE ALL OCCURRENCES OF ')' IN <line>-arrow WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <line>-subname WITH ''. REPLACE ALL OCCURRENCES OF ')' IN <line>-subname WITH ''.
    ENDLOOP.
    DATA: if_depth TYPE i, when_count TYPE i.
    LOOP AT results ASSIGNING <line>. <line>-ind = sy-tabix. ENDLOOP.
    LOOP AT results ASSIGNING <line>
      WHERE code <> 'DO' AND code <> 'ENDDO' AND code <> 'WHILE' AND code <> 'ENDWHILE'
        AND code <> 'LOOP' AND code <> 'ENDLOOP'.
      FIELD-SYMBOLS: <if> TYPE ts_if.
      IF <line>-cond = 'IF' OR <line>-cond = 'CASE'.
        ADD 1 TO if_depth. CLEAR when_count.
        APPEND INITIAL LINE TO mt_if ASSIGNING <if>. <if>-if_ind = <line>-ind.
      ENDIF.
      IF <line>-cond = 'ENDIF' OR <line>-cond = 'ENDCASE'.
        IF <if> IS ASSIGNED. <if>-end_ind = <line>-ind. SUBTRACT 1 FROM if_depth.
          LOOP AT mt_if ASSIGNING <if> WHERE end_ind = 0. ENDLOOP. ENDIF.
      ENDIF.
      IF <line>-cond = 'WHEN'. ADD 1 TO when_count. ENDIF.
      IF <line>-cond = 'ELSE' OR <line>-cond = 'ELSEIF'.
        <line>-els_before = els_before. <line>-els_after = <line>-ind.
        DATA(counter) = <line>-ind + 1.
        DO.
          READ TABLE results INDEX counter INTO line. IF sy-subrc <> 0. CLEAR <line>-els_after. EXIT. ENDIF.
          IF line-cond = 'ELSE' OR line-cond = 'ELSEIF'. CLEAR <line>-els_after. EXIT.
          ELSEIF line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND
                 line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after = counter. EXIT.
          ELSE. ADD 1 TO counter. ENDIF.
        ENDDO.
      ENDIF.
      IF <line>-cond = 'WHEN'.
        <line>-els_before = els_before. <line>-els_after = <line>-ind.
        counter = <line>-ind + 1.
        DO.
          READ TABLE results INDEX counter INTO line. IF sy-subrc <> 0. CLEAR <line>-els_after. EXIT. ENDIF.
          IF line-cond = 'WHEN'. CLEAR <line>-els_after. EXIT.
          ELSEIF line-cond <> 'DO' AND line-cond <> 'ENDDO' AND line-cond <> 'WHILE' AND
                 line-cond <> 'ENDWHILE' AND line-cond <> 'LOOP' AND line-cond <> 'ENDLOOP'.
            <line>-els_after = counter. EXIT.
          ELSE. ADD 1 TO counter. ENDIF.
        ENDDO.
        IF when_count = 1. IF <if> IS ASSIGNED. <if>-if_ind = els_before. ENDIF. CLEAR <line>-els_before. ENDIF.
      ENDIF.
      IF <line>-cond <> 'ELSE' AND <line>-cond <> 'ELSEIF' AND <line>-cond <> 'WHEN'.
        els_before = <line>-ind.
      ELSE. CLEAR els_before. ENDIF.
    ENDLOOP.
    IF mt_if IS INITIAL AND ms_if-if_ind IS NOT INITIAL. INSERT ms_if INTO mt_if INDEX 1. ENDIF.
    IF lines( results ) > 0.
      IF results[ lines( results ) ]-arrow IS NOT INITIAL. CLEAR results[ lines( results ) ]-arrow. ENDIF.
    ENDIF.
    CALL METHOD mark_active_root EXPORTING i_calc_path = i_calc_path CHANGING ct_results = results.
    IF i_calc_path = abap_true.
      DELETE results WHERE active_root IS INITIAL.
      LOOP AT results ASSIGNING <line>. <line>-ind = sy-tabix. ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_code_mix.
    DATA: flow_lines TYPE sci_include, splits TYPE TABLE OF string, ind TYPE i, prev_flow TYPE ts_line.
    DATA(lines) = get_code_flow( i_calc_path = i_calc_path ).
    READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = 'Code_Flow_Mix' ASSIGNING FIELD-SYMBOL(<prog_mix>).
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mo_window->ms_sources-tt_progs ASSIGNING <prog_mix>.
      INSERT INITIAL LINE INTO mo_window->mt_stack INDEX 1 ASSIGNING FIELD-SYMBOL(<stack_mix>).
      <prog_mix>-include = <stack_mix>-program = <stack_mix>-include = 'Code_Flow_Mix'.
    ENDIF.
    LOOP AT lines INTO DATA(flow_line).
      READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = flow_line-include INTO DATA(prog).
      READ TABLE prog-t_keywords WITH KEY line = flow_line-line INTO DATA(keyword).
      APPEND INITIAL LINE TO <prog_mix>-t_keywords ASSIGNING FIELD-SYMBOL(<keyword_mix>).
      <keyword_mix> = keyword. <keyword_mix>-include = flow_line-include. <keyword_mix>-program = flow_line-program.
      DATA(from_row) = prog-scan->tokens[ keyword-from ]-row.
      DATA(to_row)   = prog-scan->tokens[ keyword-to   ]-row.
      DATA(spaces)   = repeat( val = | | occ = ( flow_line-stack - 1 ) * 3 ).
      DATA(dashes)   = repeat( val = |-| occ = ( flow_line-stack ) ).
      IF prev_flow-ev_name <> flow_line-ev_name OR prev_flow-ev_type <> flow_line-ev_type
      OR prev_flow-class   <> flow_line-class   OR prev_flow-stack   <> flow_line-stack.
        SPLIT flow_line-include AT '=' INTO TABLE splits.
        APPEND INITIAL LINE TO flow_lines ASSIGNING FIELD-SYMBOL(<flow>).
        ind = sy-tabix.
        IF flow_line-class IS INITIAL.
          <flow> = |"{ dashes } { flow_line-ev_type } { flow_line-ev_name } in { splits[ 1 ] }|.
        ELSE.
          <flow> = |"{ dashes } { flow_line-ev_type } { flow_line-ev_name } in { flow_line-class }|.
        ENDIF.
      ENDIF.
      <keyword_mix>-v_line = ind + 1.
      LOOP AT prog-source_tab FROM from_row TO to_row INTO DATA(source_line).
        APPEND INITIAL LINE TO flow_lines ASSIGNING <flow>. ind = sy-tabix. <flow> = |{ spaces }{ source_line }|.
      ENDLOOP.
      prev_flow = flow_line.
    ENDLOOP.
    mo_window->mo_code_viewer->set_text( table = flow_lines ). <prog_mix>-source_tab = flow_lines.
    mo_window->m_prg-include = 'Code_Flow_Mix'. mo_window->set_mixprog_line( ). mo_window->show_stack( ).
    mo_window->mo_box->set_caption( |Code Mix: { lines( lines ) } statements| ).
  ENDMETHOD.
  METHOD show.
    DATA: tree TYPE zcl_ace=>ts_tree, cl_name TYPE string, icon TYPE salv_de_tree_image,
          forms_rel TYPE salv_de_node_key, modules_rel TYPE salv_de_node_key,
          f_modules TYPE salv_de_node_key, func_rel TYPE salv_de_node_key,
          classes_rel TYPE salv_de_node_key, class_rel TYPE salv_de_node_key,
          events_rel TYPE salv_de_node_key, globals_rel TYPE salv_de_node_key,
          enh_rel TYPE salv_de_node_key, locals_rel TYPE salv_de_node_key,
          keyword TYPE zcl_ace=>ts_kword, splits_prg TYPE TABLE OF string, splits_incl TYPE TABLE OF string.
    CONSTANTS: c_lazy_threshold TYPE i VALUE 10.
    IF mo_window->m_prg-include IS INITIAL. mo_window->m_prg-program = mo_window->m_prg-include = mv_prog. ENDIF.
    mo_window->set_program( mo_window->m_prg-include ).
    IF mo_window->m_prg-include <> 'Code_Flow_Mix'. mo_window->show_coverage( ). ENDIF.
    IF mo_window->m_prg-line IS INITIAL AND mo_window->mt_stack IS NOT INITIAL.
      mo_window->m_prg-line = mo_window->mt_stack[ 1 ]-line.
    ENDIF.
    mo_window->set_program_line( 1 ).
    DELETE mo_window->ms_sources-tt_progs WHERE t_keywords IS INITIAL.
    mo_window->show_stack( ). mo_tree_local->clear( ).
    SPLIT mo_window->m_prg-program AT '=' INTO TABLE splits_prg. CHECK splits_prg IS NOT INITIAL.
    cl_gui_cfw=>set_new_ok_code( 'DUMMY' ). cl_gui_cfw=>dispatch( ).
    mo_tree_local->main_node_key = mo_tree_local->add_node(
      i_name = CONV #( mo_window->m_prg-program ) i_icon = CONV #( icon_folder ) i_tree = VALUE #( ) ).
    LOOP AT mo_window->ms_sources-tt_progs INTO DATA(prog_enh).
      LOOP AT prog_enh-tt_enh_blocks INTO DATA(enh_blk).
        IF enh_rel IS INITIAL.
          enh_rel = mo_tree_local->add_node( i_name = 'Enhancements' i_icon = CONV #( icon_folder )
            i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
        ENDIF.
        mo_tree_local->add_node( i_name = |{ enh_blk-enh_name } { enh_blk-ev_type } { enh_blk-ev_name } ({ enh_blk-position })|
          i_icon = CONV #( icon_modify ) i_rel = enh_rel
          i_tree = VALUE #( kind = 'M' value = enh_blk-position include = prog_enh-include program = prog_enh-program
                            ev_type = enh_blk-ev_type ev_name = enh_blk-ev_name param = enh_blk-enh_include enh_id = enh_blk-enh_id ) ).
      ENDLOOP.
    ENDLOOP.
    DATA(lv_gvar_cnt) = 0.
    LOOP AT mo_window->ms_sources-t_vars INTO DATA(var)
      WHERE program = mo_window->m_prg-program AND eventtype IS INITIAL AND class IS INITIAL.
      lv_gvar_cnt += 1.
    ENDLOOP.
    IF lv_gvar_cnt > 0.
      globals_rel = mo_tree_local->add_node( i_name = |Global Vars ({ lv_gvar_cnt })| i_icon = CONV #( icon_header )
        i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = 'GVARS:' program = mo_window->m_prg-program ) ).
      APPEND globals_rel TO mo_tree_local->mt_lazy_nodes.
    ENDIF.
    READ TABLE mt_steps INDEX 1 INTO DATA(first_step).
    IF first_step-line IS NOT INITIAL AND first_step-program = mo_window->m_prg-program.
      events_rel = mo_tree_local->add_node( i_name = 'Events' i_icon = CONV #( icon_folder )
        i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
      mo_tree_local->add_node( i_name = 'Code Flow start line' i_icon = CONV #( icon_oo_event ) i_rel = events_rel
        i_tree = VALUE #( kind = 'E' value = first_step-line include = first_step-include ) ).
    ENDIF.
    LOOP AT mo_window->ms_sources-t_events INTO DATA(event) WHERE program = mo_window->m_prg-program.
      IF events_rel IS INITIAL.
        events_rel = mo_tree_local->add_node( i_name = 'Events' i_icon = CONV #( icon_folder )
          i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
      ENDIF.
      mo_tree_local->add_node( i_name = event-name i_icon = CONV #( icon_oo_event ) i_rel = events_rel
        i_tree = VALUE #( include = event-include value = event-line ) ).
    ENDLOOP.
    LOOP AT mo_window->ms_sources-tt_progs INTO DATA(prog) WHERE program+0(4) = 'SAPL'.
      DATA: fname TYPE rs38l_fnam, exception_list TYPE TABLE OF rsexc, export_parameter TYPE TABLE OF rsexp,
            import_parameter TYPE TABLE OF rsimp, changing_parameter TYPE TABLE OF rscha,
            tables_parameter TYPE TABLE OF rstbl, incl_nr TYPE includenr.
      DATA(len) = strlen( prog-include ) - 2. incl_nr = prog-include+len(2).
      SELECT SINGLE funcname INTO @DATA(funcname) FROM tfdir WHERE pname = @prog-program AND include = @incl_nr.
      CHECK sy-subrc = 0.
      IF f_modules IS INITIAL.
        f_modules = mo_tree_local->add_node( i_name = 'Function Modules' i_icon = CONV #( icon_folder )
          i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
      ENDIF.
      func_rel = mo_tree_local->add_node( i_name = CONV #( funcname ) i_icon = CONV #( icon_folder )
        i_rel = f_modules i_tree = VALUE #( ) ).
      fname = funcname.
      CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE' EXPORTING funcname = fname
        TABLES exception_list = exception_list export_parameter = export_parameter
               import_parameter = import_parameter changing_parameter = changing_parameter
               tables_parameter = tables_parameter EXCEPTIONS OTHERS = 4.
      IF sy-subrc = 0.
        LOOP AT import_parameter   INTO DATA(imp).    mo_tree_local->add_node( i_name = CONV #( imp-parameter    ) i_icon = CONV #( icon_parameter_import   ) i_rel = func_rel ). ENDLOOP.
        LOOP AT export_parameter   INTO DATA(exp).    mo_tree_local->add_node( i_name = CONV #( exp-parameter    ) i_icon = CONV #( icon_parameter_export   ) i_rel = func_rel ). ENDLOOP.
        LOOP AT changing_parameter INTO DATA(change). mo_tree_local->add_node( i_name = CONV #( change-parameter ) i_icon = CONV #( icon_parameter_changing ) i_rel = func_rel ). ENDLOOP.
        LOOP AT tables_parameter   INTO DATA(table).  mo_tree_local->add_node( i_name = CONV #( table-parameter  ) i_icon = CONV #( icon_parameter_table   ) i_rel = func_rel ). ENDLOOP.
      ENDIF.
    ENDLOOP.
    IF lines( splits_prg ) = 1.
      DATA(lv_cls_cnt) = 0. DATA(lv_intf_cnt) = 0. DATA(lv_loc_prev) = ``.
      LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs_cnt)
        WHERE program = mv_prog AND eventtype = 'METHOD' AND class <> splits_prg[ 1 ].
        IF lv_loc_prev <> subs_cnt-class.
          IF subs_cnt-is_intf = abap_true. lv_intf_cnt += 1. ELSE. lv_cls_cnt += 1. ENDIF.
        ENDIF.
        lv_loc_prev = subs_cnt-class.
      ENDLOOP.
      DATA(lv_loc_cnt) = lv_cls_cnt + lv_intf_cnt.
      IF lv_loc_cnt <= c_lazy_threshold.
        locals_rel = build_local_classes_node( i_program = CONV #( mv_prog ) i_excl_class = splits_prg[ 1 ]
          i_refnode = mo_tree_local->main_node_key ).
      ELSE.
        IF lv_cls_cnt > 0.
          locals_rel = mo_tree_local->add_node( i_name = |Local Classes ({ lv_cls_cnt })| i_icon = CONV #( icon_folder )
            i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = |LCLASSES:{ mv_prog }| program = mv_prog ) ).
          APPEND locals_rel TO mo_tree_local->mt_lazy_nodes.
        ENDIF.
        IF lv_intf_cnt > 0.
          DATA(lv_intf_rel) = mo_tree_local->add_node( i_name = |Interfaces ({ lv_intf_cnt })| i_icon = CONV #( icon_oo_connection )
            i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = |LINTFS:{ mv_prog }| program = mv_prog ) ).
          APPEND lv_intf_rel TO mo_tree_local->mt_lazy_nodes.
        ENDIF.
      ENDIF.
    ENDIF.
    IF class_rel IS INITIAL. classes_rel = class_rel = mo_tree_local->main_node_key. ENDIF.
    SORT mo_window->ms_sources-tt_calls_line BY program class eventtype meth_type eventname.
    cl_name = splits_prg[ 1 ].
    DO.
      READ TABLE mo_window->ms_sources-t_classes WITH KEY clsname = cl_name reltype = '2' INTO DATA(ls_class).
      IF sy-subrc <> 0. EXIT. ENDIF. cl_name = ls_class-refclsname.
    ENDDO.
    DO.
      add_class( i_class = cl_name i_refnode = classes_rel i_tree = VALUE #( kind = 'C' ) ).
      READ TABLE mo_window->ms_sources-t_classes WITH KEY refclsname = cl_name reltype = '2' INTO ls_class.
      IF sy-subrc <> 0. EXIT. ENDIF. cl_name = ls_class-clsname.
    ENDDO.
    classes_rel = class_rel = mo_tree_local->main_node_key.
    DATA(lv_form_cnt) = 0. DATA(lv_mod_cnt) = 0.
    LOOP AT mo_window->ms_sources-tt_calls_line INTO DATA(subs2).
      IF subs2-eventtype = 'FORM'   AND subs2-program = splits_prg[ 1 ]. lv_form_cnt += 1. ENDIF.
      IF subs2-eventtype = 'MODULE' AND subs2-program = splits_prg[ 1 ]. lv_mod_cnt  += 1. ENDIF.
    ENDLOOP.
    IF lv_form_cnt > 0.
      IF lv_form_cnt <= c_lazy_threshold.
        LOOP AT mo_window->ms_sources-tt_calls_line INTO subs2 WHERE eventtype = 'FORM' AND program = splits_prg[ 1 ].
          IF forms_rel IS INITIAL.
            forms_rel = mo_tree_local->add_node( i_name = 'Subroutines' i_icon = CONV #( icon_folder )
              i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
          ENDIF.
          SPLIT subs2-include AT '=' INTO TABLE splits_incl.
          READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs2-include INTO prog.
          READ TABLE prog-t_keywords WITH KEY index = subs2-index INTO keyword.
          DATA(event_node) = mo_tree_local->add_node( i_name = subs2-eventname i_icon = CONV #( icon_biw_info_source_ina )
            i_rel = forms_rel i_tree = VALUE #( kind = 'M' value = keyword-v_line include = subs2-include
                                                program = subs2-program ev_type = subs2-eventtype ev_name = subs2-eventname ) ).
          LOOP AT mo_window->ms_sources-t_params INTO DATA(param) WHERE event = 'FORM' AND name = subs2-eventname AND param IS NOT INITIAL.
            CASE param-type. WHEN 'I'. icon = icon_parameter_import. WHEN 'E'. icon = icon_parameter_export. WHEN OTHERS. icon = icon_parameter_changing. ENDCASE.
            mo_tree_local->add_node( i_name = param-param i_icon = icon i_rel = event_node i_tree = VALUE #( param = param-param ) ).
          ENDLOOP.
          DATA(lv_form_var_cnt) = 0.
          LOOP AT mo_window->ms_sources-t_vars INTO DATA(form_var)
            WHERE program = subs2-program AND eventtype = 'FORM' AND eventname = subs2-eventname.
            lv_form_var_cnt += 1.
          ENDLOOP.
          IF lv_form_var_cnt > 0.
            DATA(form_locals_rel) = mo_tree_local->add_node( i_name = |Local vars ({ lv_form_var_cnt })| i_icon = CONV #( icon_header )
              i_rel = event_node i_tree = VALUE #( param = |LVARS:FORM:{ subs2-eventname }| program = subs2-program ) ).
            APPEND form_locals_rel TO mo_tree_local->mt_lazy_nodes.
          ENDIF.
        ENDLOOP.
      ELSE.
        forms_rel = mo_tree_local->add_node( i_name = |Subroutines ({ lv_form_cnt })| i_icon = CONV #( icon_folder )
          i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = |FORMS:{ splits_prg[ 1 ] }| program = splits_prg[ 1 ] ) ).
        APPEND forms_rel TO mo_tree_local->mt_lazy_nodes.
      ENDIF.
    ENDIF.
    IF lv_mod_cnt > 0.
      IF lv_mod_cnt <= c_lazy_threshold.
        LOOP AT mo_window->ms_sources-tt_calls_line INTO subs2 WHERE eventtype = 'MODULE' AND program = splits_prg[ 1 ].
          IF modules_rel IS INITIAL.
            modules_rel = mo_tree_local->add_node( i_name = 'Modules' i_icon = CONV #( icon_folder )
              i_rel = mo_tree_local->main_node_key i_tree = VALUE #( ) ).
          ENDIF.
          SPLIT subs2-include AT '=' INTO TABLE splits_incl.
          READ TABLE mo_window->ms_sources-tt_progs WITH KEY include = subs2-include INTO prog.
          READ TABLE prog-t_keywords WITH KEY index = subs2-index INTO keyword.
          mo_tree_local->add_node( i_name = subs2-eventname i_icon = CONV #( icon_biw_info_source_ina )
            i_rel = modules_rel i_tree = VALUE #( kind = 'M' value = keyword-v_line include = subs2-include
                                                  program = subs2-program ev_type = subs2-eventtype ev_name = subs2-eventname ) ).
        ENDLOOP.
      ELSE.
        modules_rel = mo_tree_local->add_node( i_name = |Modules ({ lv_mod_cnt })| i_icon = CONV #( icon_folder )
          i_rel = mo_tree_local->main_node_key i_tree = VALUE #( param = |MODS:{ splits_prg[ 1 ] }| program = splits_prg[ 1 ] ) ).
        APPEND modules_rel TO mo_tree_local->mt_lazy_nodes.
      ENDIF.
    ENDIF.
    mo_tree_local->display( ).
  ENDMETHOD.
ENDCLASS.
