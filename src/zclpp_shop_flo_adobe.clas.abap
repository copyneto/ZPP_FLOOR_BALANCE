CLASS zclpp_shop_flo_adobe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_adobe
      IMPORTING
                is_header          TYPE    zpps_ordem_producao
      RETURNING VALUE(rv_pdf_file) TYPE xstring.
  PROTECTED SECTION.
private section.

  methods GET_NUMBER_OF_PAGES
    importing
      !IV_PAGES type STRING
    returning
      value(RV_QTD_PAGES) type I .
ENDCLASS.



CLASS ZCLPP_SHOP_FLO_ADOBE IMPLEMENTATION.


  METHOD get_adobe.
    DATA lv_fmname TYPE funcname.
    DATA lv_formulario TYPE fpname VALUE 'ZAFPP_ORDEM_PRODUCAO'.
    DATA ls_outputpar TYPE sfpoutputparams.
    DATA ls_docparams TYPE sfpdocparams.
    DATA ls_langu TYPE spras VALUE 'P'.
    DATA lr_error TYPE string VALUE 'E'.
    DATA lo_exc_api TYPE REF TO cx_fp_api.
    DATA ls_control_parameters TYPE ssfctrlop.
    DATA ls_joboutput          TYPE sfpjoboutput.
    DATA lv_err_string TYPE string.
    DATA ls_fp_formoutput TYPE fpformoutput.
    DATA ls_output_param_user TYPE sfpoutpar.
    DATA: ls_fp_outputparams TYPE sfpoutputparams,
          lt_formoutput      TYPE tfpcontent,
          lv_rc              TYPE i,
          lv_merged_document TYPE xstring,
          lv_total_pages     TYPE i,
          lv_spoolid         TYPE rspoid,
          lt_rowindex        TYPE lvc_t_row,
          lv_total_row       TYPE i,
          lv_lines           TYPE i.

    DATA: lo_pdf_merger TYPE REF TO cl_rspo_pdf_merge.

    IF sy-tcode EQ 'CO02'.
      CALL FUNCTION 'FPCOMP_SHOW_DIALOG'
        CHANGING
          ie_outpar      = ls_output_param_user
        EXCEPTIONS
          cancel         = 1
          usage_error    = 2
          system_error   = 3
          internal_error = 4
          OTHERS         = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE sy-msgty.
        RETURN.
      ENDIF.

      ls_fp_outputparams = VALUE #(
        dest    = ls_output_param_user-dest
        authority = ls_output_param_user-authority
        lifetime = ls_output_param_user-lifetime
        arcmode  = ls_output_param_user-arcmode
        copies    = ls_output_param_user-copies
        receiver = ls_output_param_user-receiver
        division = ls_output_param_user-division
        cover    = ls_output_param_user-cover
      ).
    ENDIF.

    TRY.
        CREATE OBJECT lo_pdf_merger.
      CATCH cx_rspo_pdf_merge.
        RETURN.
    ENDTRY.

*    IF lv_size IS NOT INITIAL.
*      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*        EXPORTING
*          input_length = lv_size
*        IMPORTING
*          buffer       = lv_pdf_server
*        TABLES
*          binary_tab   = lt_pdf_server
*        EXCEPTIONS
*          failed       = 1
*          OTHERS       = 2.
*      IF sy-subrc EQ 0.
*        lo_pdf_merger->add_document( lv_pdf_server ).
*      ENDIF.
*    ENDIF.

    lo_pdf_merger->merge_documents( IMPORTING merged_document = lv_merged_document rc = lv_rc ).

*    lo_pdf_merger->get_page_numbers( IMPORTING page_numbers = DATA(lv_page) ).
*
*    lv_total_pages = me->get_number_of_pages( iv_pages = lv_page ).

    ls_fp_outputparams-nodialog = abap_true.
    ls_fp_outputparams-getpdf   = 'M'.
    ls_fp_outputparams-bumode   = 'M'.
    ls_fp_outputparams-assemble = 'S'.

    " Abrir job para o formulário
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_fp_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    IF NOT sy-subrc IS INITIAL.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    TRY.
        " Obter o nome da função do formulário
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = lv_formulario
          IMPORTING
            e_funcname = lv_fmname.
*          EXCEPTIONS
*            no_form        = 1
*            no_function    = 2
*            internal_error = 3
*            OTHERS         = 4.


      CATCH cx_fp_api_repository
      cx_fp_api_usage
      cx_fp_api_internal
      INTO lo_exc_api.

        lv_err_string = lo_exc_api->get_text( ).

        MESSAGE lv_err_string TYPE lr_error.

        IF sy-subrc <> 0.

          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        ENDIF.

    ENDTRY.

    ls_docparams-langu = ls_langu.
    ls_control_parameters-no_open  = abap_true.
    ls_control_parameters-no_close = abap_true.

    " Abrir o formulário
    CALL FUNCTION lv_fmname
      EXPORTING
        /1bcdwb/docparams  = ls_docparams
        gs_dados           = is_header
        control_parameters = ls_control_parameters
      IMPORTING
        /1bcdwb/formoutput = ls_fp_formoutput
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.

    rv_pdf_file = ls_fp_formoutput-pdf.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Fechar o job
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'FP_GET_PDF_TABLE'
      IMPORTING
        e_pdf_table = lt_formoutput.

*   Add documents to attribute table of PDF merger
    LOOP AT lt_formoutput ASSIGNING FIELD-SYMBOL(<fs_form>).
      lo_pdf_merger->add_document( <fs_form> ).
    ENDLOOP.

    lo_pdf_merger->merge_documents( IMPORTING merged_document = lv_merged_document
                                                           rc = lv_rc ).

*    lo_pdf_merger->get_page_numbers( IMPORTING page_numbers = DATA(lv_pages) ).

*    lv_total_pages = me->get_number_of_pages( iv_pages = lv_pages ).
    lv_total_pages = 1.



    IF ls_output_param_user-preview EQ abap_true.
      CALL FUNCTION 'HR_IT_DISPLAY_WITH_PDF'
        EXPORTING
          iv_pdf = lv_merged_document.
    ELSE.

      CLEAR lv_spoolid.
      DATA(lv_copies) = CONV rspocopies( ls_fp_outputparams-copies ).
      DATA(lv_lifetime) = CONV char1( ls_fp_outputparams-lifetime ).


      IF sy-tcode EQ 'CO02'.
        DATA(lv_final) = abap_true.
      ELSE.
        IMPORT lt_rowindex  TO lt_rowindex  FROM MEMORY ID 'ROWS'.
        IMPORT lv_total_row TO lv_total_row FROM MEMORY ID 'TOTAL_ROW'.
        IMPORT lv_spoolid   TO lv_spoolid   FROM MEMORY ID 'LV_SPOOLID'.

        DESCRIBE TABLE lt_rowindex LINES lv_lines.

        DATA(lv_append) = COND xfeld( WHEN lv_lines LT lv_total_row THEN abap_true ELSE abap_false ).
        lv_final = COND xfeld( WHEN lv_lines EQ 1 THEN abap_true ELSE abap_false ).
      ENDIF.

*      CALL FUNCTION 'ADS_CREATE_PDF_SPOOLJOB'
      CALL FUNCTION 'ZFMPP_PRINT_TO_SPOOL'
        EXPORTING
          iv_dest            = ls_fp_outputparams-dest
          iv_pages           = lv_total_pages
          iv_pdf_data        = lv_merged_document
          iv_name            = ls_fp_outputparams-dataset
          iv_suffix1         = ls_fp_outputparams-suffix1
          iv_suffix2         = ls_fp_outputparams-suffix2
          iv_copies          = lv_copies
*         IV_PRIO            = ps_op-
          iv_immediate_print = ls_fp_outputparams-reqimm
          iv_auto_delete     = ls_fp_outputparams-reqdel
          iv_titleline       = ls_fp_outputparams-covtitle
          iv_receiver        = ls_fp_outputparams-receiver
          iv_division        = ls_fp_outputparams-division
          iv_authority       = ls_fp_outputparams-authority
          iv_lifetime        = lv_lifetime
          iv_append          = lv_append
          iv_final           = lv_final
          iv_spoolid         = lv_spoolid
        IMPORTING
          ev_spoolid         = lv_spoolid
        EXCEPTIONS
          no_data            = 1
          not_pdf            = 2
          wrong_devtype      = 3
          operation_failed   = 4
          cannot_write_file  = 5
          device_missing     = 6
          no_such_device     = 7
          OTHERS             = 8.
      IF sy-subrc EQ 0.
        IF sy-tcode NE 'CO02'.
          DELETE lt_rowindex INDEX 1.
          EXPORT lt_rowindex  = lt_rowindex TO MEMORY ID 'ROWS'.

          IF lv_final EQ abap_true.
            CLEAR lv_spoolid.
          ENDIF.
          EXPORT lv_spoolid   = lv_spoolid  TO MEMORY ID 'LV_SPOOLID'.
        ENDIF.

        MESSAGE s433(td) WITH lv_spoolid ls_fp_outputparams-dest.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE sy-msgty.
        RETURN.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD get_number_of_pages.

    DATA lv_total_pages TYPE i.
    DATA(lv_page) = iv_pages.

    SPLIT lv_page AT ';' INTO TABLE DATA(lt_page).

    LOOP AT lt_page ASSIGNING FIELD-SYMBOL(<fs_page>).
      lv_total_pages = lv_total_pages + <fs_page>.
    ENDLOOP.

    rv_qtd_pages = lv_total_pages.

  ENDMETHOD.
ENDCLASS.
