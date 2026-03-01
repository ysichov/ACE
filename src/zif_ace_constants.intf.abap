INTERFACE zif_ace_constants PUBLIC.

  CONSTANTS enh_mode_dynamic TYPE char1 VALUE 'D'.
  CONSTANTS enh_mode_form    TYPE char1 VALUE 'F'.

  CONSTANTS enh_pos_begin     TYPE string VALUE 'BEGIN'.
  CONSTANTS enh_pos_end       TYPE string VALUE 'END'.
  CONSTANTS enh_pos_overwrite TYPE string VALUE 'OVERWRITE'.

  CONSTANTS enh_pattern_method_pre    TYPE string VALUE '%_BEGIN'.
  CONSTANTS enh_pattern_method_post   TYPE string VALUE '%_END'.
  CONSTANTS enh_pattern_form_begin    TYPE string VALUE '\SE:BEGIN'.
  CONSTANTS enh_pattern_form_end      TYPE string VALUE '\SE:END'.

  CONSTANTS type_interface   TYPE char1 VALUE 'I'.
  CONSTANTS type_testclass   TYPE char1 VALUE 'T'.

  CONSTANTS inc_class_pool    TYPE string VALUE 'CP'.
  CONSTANTS inc_class_def     TYPE string VALUE 'CU'.
  CONSTANTS inc_class_priv    TYPE string VALUE 'CI'.
  CONSTANTS inc_class_prot    TYPE string VALUE 'CO'.
  CONSTANTS inc_interface     TYPE string VALUE 'IU'.
  CONSTANTS inc_class_test    TYPE string VALUE 'CCAU'.
  CONSTANTS inc_class_helper  TYPE string VALUE 'CCIMP'.

  CONSTANTS inc_code_flow_mix TYPE string VALUE 'Code_Flow_Mix'.
  CONSTANTS inc_virtual       TYPE string VALUE 'VIRTUAL'.

  CONSTANTS kw_if       TYPE string VALUE 'IF'.
  CONSTANTS kw_elseif   TYPE string VALUE 'ELSEIF'.
  CONSTANTS kw_else     TYPE string VALUE 'ELSE'.
  CONSTANTS kw_endif    TYPE string VALUE 'ENDIF'.
  CONSTANTS kw_case     TYPE string VALUE 'CASE'.
  CONSTANTS kw_when     TYPE string VALUE 'WHEN'.
  CONSTANTS kw_endcase  TYPE string VALUE 'ENDCASE'.
  CONSTANTS kw_do       TYPE string VALUE 'DO'.
  CONSTANTS kw_enddo    TYPE string VALUE 'ENDDO'.
  CONSTANTS kw_loop     TYPE string VALUE 'LOOP'.
  CONSTANTS kw_endloop  TYPE string VALUE 'ENDLOOP'.
  CONSTANTS kw_while    TYPE string VALUE 'WHILE'.
  CONSTANTS kw_endwhile TYPE string VALUE 'ENDWHILE'.

  CONSTANTS kw_data       TYPE string VALUE 'DATA'.
  CONSTANTS kw_types      TYPE string VALUE 'TYPES'.
  CONSTANTS kw_constants  TYPE string VALUE 'CONSTANTS'.
  CONSTANTS kw_parameters TYPE string VALUE 'PARAMETERS'.
  CONSTANTS kw_include    TYPE string VALUE 'INCLUDE'.
  CONSTANTS kw_report     TYPE string VALUE 'REPORT'.
  CONSTANTS kw_program    TYPE string VALUE 'PROGRAM'.

  CONSTANTS kw_public    TYPE string VALUE 'PUBLIC'.
  CONSTANTS kw_protected TYPE string VALUE 'PROTECTED'.
  CONSTANTS kw_private   TYPE string VALUE 'PRIVATE'.

  CONSTANTS kw_form        TYPE string VALUE 'FORM'.
  CONSTANTS kw_endform     TYPE string VALUE 'ENDFORM'.
  CONSTANTS kw_method      TYPE string VALUE 'METHOD'.
  CONSTANTS kw_endmethod   TYPE string VALUE 'ENDMETHOD'.
  CONSTANTS kw_methods     TYPE string VALUE 'METHODS'.
  CONSTANTS kw_class_method TYPE string VALUE 'CLASS-METHODS'.
  CONSTANTS kw_module      TYPE string VALUE 'MODULE'.
  CONSTANTS kw_endmodule   TYPE string VALUE 'ENDMODULE'.

  CONSTANTS kw_class     TYPE string VALUE 'CLASS'.
  CONSTANTS kw_endclass  TYPE string VALUE 'ENDCLASS'.
  CONSTANTS kw_interface TYPE string VALUE 'INTERFACE'.

  CONSTANTS kw_enhancement    TYPE string VALUE 'ENHANCEMENT'.
  CONSTANTS kw_endenhancement TYPE string VALUE 'ENDENHANCEMENT'.

  CONSTANTS param_importing  TYPE string VALUE 'IMPORTING'.
  CONSTANTS param_exporting  TYPE string VALUE 'EXPORTING'.
  CONSTANTS param_changing   TYPE string VALUE 'CHANGING'.
  CONSTANTS param_using      TYPE string VALUE 'USING'.
  CONSTANTS param_returning  TYPE string VALUE 'RETURNING'.

  CONSTANTS param_type_import TYPE char1 VALUE 'I'.
  CONSTANTS param_type_export TYPE char1 VALUE 'E'.

  CONSTANTS call_type_method    TYPE string VALUE 'METHOD'.
  CONSTANTS call_type_form      TYPE string VALUE 'FORM'.
  CONSTANTS call_type_function  TYPE string VALUE 'FUNCTION'.
  CONSTANTS call_type_module    TYPE string VALUE 'MODULE'.
  CONSTANTS call_type_screen    TYPE string VALUE 'SCREEN'.
  CONSTANTS call_type_event     TYPE string VALUE 'EVENT'.

  CONSTANTS call_dir_import  TYPE char1 VALUE '>'.
  CONSTANTS call_dir_export  TYPE char1 VALUE '<'.

  CONSTANTS max_history_depth     TYPE i VALUE 9.
  CONSTANTS default_history_depth TYPE i VALUE 9.

  CONSTANTS filter_z_code_only TYPE x VALUE '01'.
  CONSTANTS filter_all_code    TYPE x VALUE '00'.

  CONSTANTS marker_breakpoint_session  TYPE i VALUE 2.
  CONSTANTS marker_breakpoint_external TYPE i VALUE 4.
  CONSTANTS marker_current_line        TYPE i VALUE 7.

  CONSTANTS struct_type_event     TYPE char1 VALUE 'E'.
  CONSTANTS struct_type_call      TYPE char1 VALUE 'C'.
  CONSTANTS struct_type_procedure TYPE char1 VALUE 'P'.

  CONSTANTS stmt_type_include TYPE char1 VALUE '1'.
  CONSTANTS stmt_type_report  TYPE char1 VALUE '2'.
  CONSTANTS stmt_type_program TYPE char1 VALUE '3'.

  CONSTANTS method_type_public    TYPE i VALUE 1.
  CONSTANTS method_type_protected TYPE i VALUE 2.
  CONSTANTS method_type_private   TYPE i VALUE 3.

  CONSTANTS reltype_interface_impl TYPE seoreltype VALUE '0'.
  CONSTANTS reltype_interface_used TYPE seoreltype VALUE '1'.
  CONSTANTS reltype_inheritance    TYPE seoreltype VALUE '2'.

ENDINTERFACE.
