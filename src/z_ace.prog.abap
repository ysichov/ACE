REPORT z_ace. "ACE - Abap Code Explorer
*  & Multi-windows program for ABAP code analysis
*  &---------------------------------------------------------------------*
*  & version: beta 0.3
*  & Git https://github.com/ysichov/ACE

*  & Written by Yurii Sychov
*  & e-mail:   ysichov@gmail.com
*  & blog:     https://ysychov.wordpress.com/blog/
*  & LinkedIn: https://www.linkedin.com/in/ysychov/
*  &---------------------------------------------------------------------*

*  & External resources
*  & https://github.com/WegnerDan/abapMermaid

PARAMETERS: p_prog   TYPE progname  MATCHCODE OBJECT progname MODIF ID prg OBLIGATORY,
            p_dest   TYPE text255 MEMORY ID dest,
            p_model  TYPE text255 MEMORY ID model,
            p_apikey TYPE text255  MEMORY ID api.

AT SELECTION-SCREEN.

data(lo_ace) =  NEW zcl_ace( i_prog = p_prog i_dest = p_dest i_model = p_model i_apikey = p_apikey ).
