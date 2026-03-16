interface ZIF_ACE_STMT_HANDLER
  public .


  methods HANDLE
    importing
      !IO_SCAN type ref to CL_CI_SCAN
      !I_STMT_IDX type I
      !I_PROGRAM type PROGRAM
      !I_INCLUDE type PROGRAM
      !I_CLASS type STRING optional
      !I_INTERFACE type STRING optional
      !I_EVTYPE type STRING optional
      !I_EV_NAME type STRING optional
    changing
      !CS_SOURCE type ZCL_ACE_WINDOW=>TS_SOURCE .
endinterface.
