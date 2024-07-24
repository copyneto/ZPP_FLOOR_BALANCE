CLASS zclpp_shop_floor_balanca DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS send
      IMPORTING
        !iv_destination TYPE char20
        !iv_uri         TYPE string
        !iv_metodo      TYPE string OPTIONAL
        "!IS_INTERFACE type ZI_CA_GET_URL_CPI_FILTER optional
        !it_table       TYPE ANY TABLE OPTIONAL
        !is_structure   TYPE any OPTIONAL
        "!IT_CASE_SENSTV type ZCTGCA_CASE_SENSVT_CPI optional
      EXPORTING
        "!ES_INTERFACE type ZI_CA_GET_URL_CPI_FILTER
        !ev_request     TYPE string
        !ev_result      TYPE string
        !ev_code        TYPE i
        !ev_reason      TYPE string
        !et_return      TYPE bapiret2_t.

ENDCLASS.



CLASS zclpp_shop_floor_balanca IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    DATA: lo_cl_somu_form_services TYPE REF TO cl_somu_form_services.

    DATA: lt_tab         TYPE TABLE OF zi_pp_shop_floor_balanca,
          lt_return      TYPE bapiret2_t,
          lt_master_keys TYPE cl_somu_form_services=>ty_gt_key,
          lt_keys        TYPE cl_somu_form_services=>ty_gt_key..

    DATA: lv_content     TYPE xstring,
          lv_destination TYPE char20,
          lv_uri         TYPE string,
          lv_metodo      TYPE string,
          lv_request     TYPE string,
          lv_result      TYPE string,
          lv_code        TYPE i,
          lv_reason      TYPE string.

    TRY.
        "Requested data
        IF io_request->is_data_requested(  ).
          "Paginacao
          DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
          DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
          DATA(lv_max_rows) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited
                                      THEN 0 ELSE lv_page_size )  .
          "Recupera filtros
          TRY.
              TRY.
                  DATA(lt_parameters) = io_request->get_parameters( ). "#EC CI_CONV_OK
                  "DATA(lt_filters) = io_request->get_filter( )->get_as_ranges( ). "#EC CI_CONV_OK
                CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
                  DATA(lv_exp_msg) = lo_ex_filter->get_longtext( ).
              ENDTRY.

              "///////////////////// Logica de peso //////////////////
              lv_metodo = if_http_entity=>co_request_method_get.
              lv_destination = 'NONE'.
              lv_uri = '//192.168.6.152'.

              me->send(
                EXPORTING
                  iv_destination = lv_destination
                  iv_uri       = lv_uri
                  iv_metodo    = lv_metodo
                IMPORTING
                  ev_request   = lv_request
                  ev_result    = lv_result
                  ev_code      = lv_code
                  ev_reason    = lv_reason
                  et_return    = lt_return
              ).

              APPEND VALUE #(
                peso  = '10'"sy-timlo+1(2)
              ) TO lt_tab.

              "/////////////////////////////////////////////////////////
              io_response->set_total_number_of_records( 1 ).

*  " -------------- Send the response back to UI------------
              io_response->set_data( lt_tab ).

            CATCH cx_rap_query_filter_no_range INTO DATA(lv_range).
              DATA(lv_msg) = lv_range->get_text( ).
          ENDTRY.


        ENDIF.
      CATCH cx_rap_query_provider.
    ENDTRY.
  ENDMETHOD.

  METHOD send.
    CONSTANTS: lc_content          TYPE string VALUE 'Content-Type',
               lc_contentval       TYPE string VALUE 'application/json',
               lc_x_requested_with TYPE string VALUE 'X-Requested-With'.

    DATA: lo_client TYPE REF TO if_http_client.

    FREE: et_return, ev_code, ev_reason, ev_request, ev_result. ", es_interface.

    cl_http_client=>create_by_destination( EXPORTING  destination              = iv_destination "tabela de parametro
                                           IMPORTING  client                   = lo_client
                                           EXCEPTIONS argument_not_found       = 1
                                                      destination_not_found    = 2
                                                      destination_no_authority = 3
                                                      plugin_not_active        = 4
                                                      internal_error           = 5
                                                      OTHERS                   = 6 ).

    cl_http_utility=>set_request_uri( EXPORTING request = lo_client->request
                                                uri     = iv_uri ).

    IF sy-subrc IS NOT INITIAL.
      cl_http_client=>create_by_url(
        EXPORTING
          url                    = iv_uri
        IMPORTING
          client                 = lo_client
        EXCEPTIONS
          argument_not_found     = 1
          plugin_not_active      = 2
          internal_error         = 3
          pse_not_found          = 4
          pse_not_distrib        = 5
          pse_errors             = 6
          OTHERS                 = 7
      ).
      IF sy-subrc IS NOT INITIAL.
        RETURN.
      ENDIF.

    ENDIF.

    lo_client->request->set_method( iv_metodo ).
    "lo_client->request->set_content_type( EXPORTING content_type = if_rest_media_type=>gc_appl_json ).

    lo_client->send( EXCEPTIONS http_communication_failure = 1 OTHERS = 99 ).
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lo_client->receive( EXCEPTIONS http_communication_failure = 1 OTHERS = 4 ).
    DATA(lv_response) = lo_client->response->get_data( ).
    lo_client->response->get_status( IMPORTING code = ev_code  reason = ev_reason ).
    ev_result = cl_bcs_convert=>xstring_to_string( iv_cp = cl_sx_mime_singlepart=>get_sx_node_codepage( )  iv_xstr = lv_response ).

    IF ev_code NE '200' AND ev_code NE '201'.
      et_return = VALUE #( BASE et_return ( type = 'E'
                                            id = 'ZPP_SHOP_FLOOR'
                                            number = '001'
                                            message_v1 = |{ CONV symsgv( ev_code ) ALPHA = OUT }|
                                            message_v2 = CONV #( ev_reason )
                                            message_v3 = ev_result
                                          ) ).
      RETURN.
    ENDIF.

    "ev_code: 200 = ok  | ev_code: 201 Criado com sucesso
    et_return = VALUE #( BASE et_return ( type = 'S' id = 'ZPP_SHOP_FLOOR' number = ev_code ) ).

  ENDMETHOD.

ENDCLASS.
