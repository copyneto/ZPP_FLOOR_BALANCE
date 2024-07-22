FUNCTION zfmpp_print_to_spool.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_PRINTER) TYPE  RSPOLNAME OPTIONAL
*"     VALUE(IV_DEST) TYPE  RSPOPNAME OPTIONAL
*"     VALUE(IV_PAGES) TYPE  I
*"     REFERENCE(IV_PDF_DATA) TYPE  XSTRING
*"     VALUE(IV_NAME) TYPE  TSP01-RQ0NAME OPTIONAL
*"     VALUE(IV_SUFFIX1) TYPE  TSP01-RQ1NAME OPTIONAL
*"     VALUE(IV_SUFFIX2) TYPE  TSP01-RQ2NAME OPTIONAL
*"     VALUE(IV_COPIES) TYPE  TSP01-RQCOPIES OPTIONAL
*"     VALUE(IV_PRIO) TYPE  TSP01-RQPRIO OPTIONAL
*"     VALUE(IV_IMMEDIATE_PRINT) TYPE  TSP01-RQ1DISPO OPTIONAL
*"     VALUE(IV_AUTO_DELETE) TYPE  TSP01-RQ2DISPO OPTIONAL
*"     VALUE(IV_TITLELINE) TYPE  TSP01-RQTITLE OPTIONAL
*"     VALUE(IV_RECEIVER) TYPE  TSP01-RQRECEIVER OPTIONAL
*"     VALUE(IV_DIVISION) TYPE  TSP01-RQDIVISION OPTIONAL
*"     VALUE(IV_AUTHORITY) TYPE  TSP01-RQAUTH OPTIONAL
*"     VALUE(IV_LIFETIME) TYPE  C DEFAULT '0'
*"     VALUE(IV_APPEND) TYPE  C OPTIONAL
*"     VALUE(IV_FINAL) TYPE  C OPTIONAL
*"     VALUE(IV_SPOOLID) TYPE  RSPOID OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_SPOOLID) TYPE  RSPOID
*"  EXCEPTIONS
*"      NO_DATA
*"      NOT_PDF
*"      WRONG_DEVTYPE
*"      OPERATION_FAILED
*"      CANNOT_WRITE_FILE
*"      DEVICE_MISSING
*"      NO_SUCH_DEVICE
*"----------------------------------------------------------------------

*INCLUDE fp_spool_constants.

  DATA: lv_handle TYPE sy-tabix.
  DATA: lv_extension TYPE string.
  DATA: lv_fullname(256).
  DATA: lv_filename  TYPE localfile.
  DATA: lv_object_name TYPE rstsoname.
  DATA: lv_partname TYPE adspart.
  DATA: lv_size TYPE i.
  CONSTANTS: lc_pdf(4) TYPE x VALUE '25504446'.
  DATA: lv_help_buff(4) TYPE x.
  DATA: lv_patype TYPE rspoptype.
  DATA: lv_adstype TYPE string.
  DATA: lv_lifetime_int TYPE i.

  lv_size = xstrlen( iv_pdf_data ).
  IF lv_size = 0.
    MESSAGE e036 RAISING no_data.
  ENDIF.

  TRY.
      lv_help_buff = iv_pdf_data(4).
    CATCH cx_sy_range_out_of_bounds.
  ENDTRY.

  IF lv_help_buff <> lc_pdf.
    MESSAGE e037 RAISING not_pdf.
  ENDIF.

  lv_extension = c_file_ext_pdf.

  IF iv_printer IS NOT INITIAL.
    SELECT SINGLE patype FROM tsp03d INTO lv_patype WHERE name = iv_printer.
  ELSE.
    IF iv_dest IS NOT INITIAL.
      SELECT SINGLE name patype FROM tsp03d INTO (iv_printer, lv_patype) WHERE padest = iv_dest.
    ENDIF.
  ENDIF.

  IF sy-subrc <> 0.
    IF iv_printer IS INITIAL.
      iv_printer = iv_dest.
    ENDIF.
    MESSAGE e122(xm) WITH iv_printer RAISING no_such_device.
  ENDIF.

  CALL FUNCTION 'ADS_GET_DEVTYPE_ATTRIBUTES'
    EXPORTING
      prtype  = lv_patype
*     COLOR   = ' '
    IMPORTING
      adstype = lv_adstype.

  IF lv_adstype <> c_file_typ_pdf.
    MESSAGE e038 WITH lv_patype c_file_typ_pdf RAISING wrong_devtype.
  ENDIF.

* check lifetime
  TRY.
      lv_lifetime_int = iv_lifetime.
    CATCH cx_root.
      CLEAR lv_lifetime_int.
  ENDTRY.
  IF lv_lifetime_int <= 0.
*   Read default value from rspo/req_lifetime
    iv_lifetime = cl_rspo_utilities=>get_default_lifetime( ).
  ENDIF.
  CONDENSE iv_lifetime.

  CALL FUNCTION 'ADS_SR_OPEN'
    EXPORTING
      ldest            = iv_printer
      name             = iv_name
      suffix1          = iv_suffix1
      suffix2          = iv_suffix2
      copies           = iv_copies
      prio             = iv_prio
      immediate_print  = iv_immediate_print
      auto_delete      = iv_auto_delete
      titleline        = iv_titleline
      receiver         = iv_receiver
      division         = iv_division
      authority        = iv_authority
      lifetime         = iv_lifetime
      doctype          = c_ads_doctype_spool
      append           = iv_append
      usespoolid       = iv_spoolid
    IMPORTING
      handle           = lv_handle
      spoolid          = ev_spoolid
      partname         = lv_partname
    EXCEPTIONS
      device_missing   = 1
      no_such_device   = 2
      operation_failed = 3
      wrong_doctype    = 4
      wrong_devicetype = 5
      OTHERS           = 6.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1.
      MESSAGE e121(xm) RAISING device_missing.
    WHEN 2.
      MESSAGE e122(xm) WITH iv_printer RAISING no_such_device.
    WHEN OTHERS.
      RAISE operation_failed.
  ENDCASE.

  CONCATENATE lv_partname lv_extension INTO lv_filename.

  CALL FUNCTION 'ADS_WRITE_TO_FILE'
    EXPORTING
      filename                     = lv_filename
      buffer                       = iv_pdf_data
      append                       = iv_append
*     USEGLOBALDIR                 = 'X'
*     CLIENT                       =
    EXCEPTIONS
      cannot_open_file             = 1
      open_dataset_no_authority    = 2
      open_dataset_internal_error  = 3
      open_dataset_too_many_files  = 4
      dataset_cant_close           = 5
      close_dataset_internal_error = 6
      cannot_close_file            = 7
      cannot_transfer_data         = 8
      transfer_internal_error      = 9
      dataset_write_error          = 10
      OTHERS                       = 11.
  IF sy-subrc <> 0.
    PERFORM f_delete_spoolreq USING ev_spoolid.
    MESSAGE e757(po) WITH lv_filename RAISING cannot_write_file.
  ENDIF.

  CALL FUNCTION 'ADS_SR_CONFIRM'
    EXPORTING
      handle           = lv_handle
      partname         = lv_partname
      size             = lv_size
      pages            = iv_pages
*     NO_PDF           = ' '
*     NUMBERDOCS       = 1
*     DESCRIPTION      = ' '
* IMPORTING
*     NEW_PARTNAME     =
    EXCEPTIONS
      handle_not_valid = 1
      operation_failed = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    PERFORM f_delete_spoolreq USING ev_spoolid.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        RAISING operation_failed.
  ENDIF.

  CALL FUNCTION 'ADS_SR_CLOSE'
    EXPORTING
      handle           = lv_handle
      final            = iv_final
    EXCEPTIONS
      handle_not_valid = 1
      operation_failed = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    PERFORM f_delete_spoolreq USING ev_spoolid.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        RAISING operation_failed.
  ENDIF.

ENDFUNCTION.


FORM f_delete_spoolreq USING uv_spoolid.

  DATA lv_rqident TYPE rqident.

  CHECK uv_spoolid > 0.

  lv_rqident = uv_spoolid.

  CALL FUNCTION 'RSPO_R_RDELETE_SPOOLREQ'
    EXPORTING
      spoolid = lv_rqident
*   IMPORTING
*     RC      =
*     STATUS  =
*     ERROR_MESSAGE       =
    .


ENDFORM.
