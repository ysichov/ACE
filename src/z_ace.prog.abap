  REPORT z_ace. " ACE - Abap Code Explorer
  " & Multi-windows program for ABAP code analysis
  " &----------------------------------------------------------------------
  " & version: beta 0.5
  " & Git https://github.com/ysichov/ACE

  " & Written by Yurii Sychov
  " & e-mail:   ysichov@gmail.com
  " & blog:     https://ysychov.wordpress.com/blog/
  " & LinkedIn: https://www.linkedin.com/in/ysychov/
  " &----------------------------------------------------------------------

  " & External resources
  " & https://github.com/WegnerDan/abapMermaid
  " & https://github.com/oisee/vibing-steampunk

  SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE TEXT-004.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT (29) TEXT-002 FOR FIELD p_prog.
      SELECTION-SCREEN POSITION 33.
      PARAMETERS: p_prog  TYPE progname MATCHCODE OBJECT progname MODIF ID prg.
      SELECTION-SCREEN COMMENT (70) TEXT-001 FOR FIELD p_prog.
    SELECTION-SCREEN END OF LINE.
    PARAMETERS: p_class  TYPE seoclsname MATCHCODE OBJECT sfbeclname.
    PARAMETERS: p_func  TYPE seoclsname MATCHCODE OBJECT cacs_function.
    PARAMETERS: p_odata  TYPE seoclsname MATCHCODE OBJECT /iwbep/sh_sbdm_project.
    PARAMETERS: p_wdc  TYPE string.
  SELECTION-SCREEN END OF BLOCK s1.

  PARAMETERS: n_parser NO-DISPLAY. "AS CHECKBOX DEFAULT ' '.
  PARAMETERS: n_time NO-DISPLAY . "AS CHECKBOX DEFAULT ' ' .

  SELECTION-SCREEN SKIP.

  INITIALIZATION.

    PERFORM supress_button. "supressing F8 button
    DATA itab TYPE TABLE OF sy-ucomm.

    APPEND: 'ONLI' TO itab.
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = itab.


  AT SELECTION-SCREEN ON p_class.

    IF p_class IS NOT INITIAL.
      SELECT SINGLE clstype INTO @DATA(clstype)
        FROM seoclass
       WHERE clsname = @p_class.
      IF sy-subrc = 0.

        p_prog = p_class && repeat( val = `=` occ = 30 - strlen( p_class ) ).
        IF clstype = '1'.
          p_prog = p_prog && 'IP'.
        ELSE.
          p_prog = p_prog && 'CP'.
        ENDIF.
      ENDIF.

    ENDIF.

AT SELECTION-SCREEN ON p_odata.

   IF p_odata IS NOT INITIAL.
      DATA(serv) = p_odata && '_SRV'.

      SELECT SINGLE class_name INTO p_class
        FROM /iwbep/i_mgw_srh WHERE technical_name = serv.
    ENDIF.

    IF p_wdc IS NOT INITIAL.
      p_class = cl_wdy_wb_naming_service=>get_classname_for_component( p_component = CONV #( p_wdc ) ).
    ENDIF.

AT SELECTION-SCREEN ON p_func.

    IF p_func IS NOT INITIAL.
      SELECT SINGLE pname, include INTO ( @DATA(func_incl), @DATA(incl_num) )
        FROM tfdir
       WHERE funcname = @p_func.

      IF sy-subrc = 0.
        SHIFT func_incl LEFT BY 3 PLACES.
        p_prog = func_incl && 'U' && incl_num.
      ENDIF.

    ENDIF.

  AT SELECTION-SCREEN.

    CHECK sy-ucomm <> 'DUMMY'.
    perform run_ace.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_wdc.

    DATA: gt_tab TYPE TABLE OF rseui_f4.
    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
      EXPORTING
        object_type      = 'YC'
        object_name      = 'Z*'
      TABLES
        objects_selected = gt_tab
      EXCEPTIONS
        cancel           = 1
        wrong_type       = 2
        OTHERS           = 3.

    IF sy-subrc = 0.
      p_wdc = gt_tab[ 1 ]-obj_name.

    ENDIF.

  FORM supress_button. "supressing F8 button

    DATA itab TYPE TABLE OF sy-ucomm.

    APPEND: 'ONLI' TO itab.
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = itab.
  ENDFORM.

  form run_ace.

    CHECK sy-ucomm IS INITIAL.
    SELECT COUNT( * ) FROM reposrc WHERE progname = p_prog.

    IF sy-dbcnt <> 0.
      DATA(gv_ace) = NEW zcl_ace( i_prog = p_prog i_new_parser = n_parser i_show_parse_time = n_time ).
    ELSE.
      MESSAGE 'Program is not found' TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.

  endform.
