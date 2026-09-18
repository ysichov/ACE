"! <p class="shorttext synchronized">Code-flow walk context, without a GUI</p>
"! ZIF_ACE_WALK and nothing else. A caller with no SAP GUI - an ADT resource,
"! a batch job - hands one of these to ZCL_ACE_SOURCE_PARSER and reads the
"! step table back off it, instead of building a ZCL_ACE whose only purpose
"! was to own those six fields.
CLASS zcl_ace_walk DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_ace_walk .

    ALIASES ms_sources   FOR zif_ace_walk~ms_sources .
    ALIASES m_zcode      FOR zif_ace_walk~m_zcode .
    ALIASES m_hist_depth FOR zif_ace_walk~m_hist_depth .
    ALIASES mt_calls     FOR zif_ace_walk~mt_calls .
    ALIASES mt_steps     FOR zif_ace_walk~mt_steps .
    ALIASES m_step       FOR zif_ace_walk~m_step .
ENDCLASS.


CLASS zcl_ace_walk IMPLEMENTATION.
ENDCLASS.
