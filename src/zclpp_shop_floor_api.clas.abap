CLASS zclpp_shop_floor_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_dados,
             _order_i_d          TYPE zspp_shop_floor_api_post-_order_i_d,
             _material           TYPE char18,
             _plant              TYPE zspp_shop_floor_api_post-_plant,
             _production_unit    TYPE zspp_shop_floor_api_post-_production_unit,
             _confirmation_group TYPE zspp_shop_floor_api_post-_confirmation_group,
             _sequence           TYPE zspp_shop_floor_api_post-_sequence,
             _order_operation    TYPE zspp_shop_floor_api_post-_order_operation,
             "confirmationyieldquantity TYPE   a_productionorderconf_2-confirmationyieldquantity,
           END OF ty_dados.

    INTERFACES if_rap_query_provider .

  PROTECTED SECTION.

  PRIVATE SECTION.

    "! Faz o tratamento dos dados do QR-Code e chama o metodo de envio
    "! @parameter iv_dadosqr | Dados do QR-Code
    "! @parameter iv_quantidade | Peso da balança
    "! @parameter ev_msgtype | Tipo da mensagem de retorno
    "! @parameter ev_result | Resposta da API (XML)
    "! @parameter ev_http_status | Status
    "! @parameter ev_status_text | Descrição do Status
    METHODS http_req
      IMPORTING
        iv_dadosqr     TYPE ty_dados
        iv_quantidade  TYPE string
      EXPORTING
        ev_msgtype     TYPE char1
        ev_result      TYPE string
        ev_http_status TYPE i
        ev_status_text TYPE string.

    "! Faz a chamada da API e envia os dados
    "! @parameter iv_uri | URL da API
    "! @parameter is_structure | Dados que serão enviados
    "! @parameter ev_request | Corpo do envio
    "! @parameter ev_result |  Resposta da API (XML)
    "! @parameter ev_code | Status da requisição. 201, 404, 500...
    "! @parameter ev_reason | Descrição do Status
    "! @parameter et_return | Tabela de mensages de erro
    METHODS send
      IMPORTING
        !iv_uri       TYPE string
        !is_structure TYPE any OPTIONAL
      EXPORTING
        !ev_request   TYPE string
        !ev_result    TYPE string
        !ev_code      TYPE i
        !ev_reason    TYPE string
        !et_return    TYPE bapiret2_t.

    "! Retorna se o peso é válido
    "! @parameter iv_dadosqr | Dados do QR-Code
    "! @parameter iv_quantidade | Peso da balança
    "! @parameter ev_msgtype | Tipo da mensagem de retorno
    "! @parameter ev_status_text | Descrição do Status
    METHODS valida_peso
      IMPORTING
        iv_dadosqr     TYPE ty_dados
        iv_quantidade  TYPE dec_16_08_s
      EXPORTING
        ev_msgtype     TYPE char1
        ev_status_text TYPE string.

    METHODS get_messagem
      IMPORTING
        is_sy            TYPE sy OPTIONAL
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t.

    "! Converte uma estrutura em JSON
    "! @parameter iv_data | Estrutura
    "! @parameter iv_compress |
    "! @parameter rv_json | String no formato JSON
    METHODS conv_data_to_json
      IMPORTING
        !iv_data       TYPE data
        !iv_compress   TYPE /ui2/cl_json=>bool DEFAULT ''
      RETURNING
        VALUE(rv_json) TYPE string .

    METHODS conv_json_to_data
      IMPORTING
        !iv_json          TYPE string
        !it_name_mappings TYPE /ui2/cl_json=>name_mappings OPTIONAL
      EXPORTING
        !es_structure     TYPE any
        !et_table         TYPE ANY TABLE .

ENDCLASS.

CLASS zclpp_shop_floor_api IMPLEMENTATION.

  METHOD if_rap_query_provider~select.
    DATA: lt_tab    TYPE TABLE OF zi_pp_shop_floor_ce,
          lt_return TYPE bapiret2_t.
    DATA: lt_master_keys           TYPE cl_somu_form_services=>ty_gt_key.
    DATA: lv_content               TYPE xstring.
    DATA: lo_cl_somu_form_services TYPE REF TO cl_somu_form_services,
          lt_keys                  TYPE cl_somu_form_services=>ty_gt_key.


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
              "Busca os parametros da custom entity
              DATA ls_dados TYPE ty_dados.
              DATA lv_tag   TYPE char100.

              lv_tag        =   VALUE #( lt_parameters[ parameter_name =  'P_TAG' ]-value OPTIONAL ).
              DATA(lv_quantidade)  =   VALUE #( lt_parameters[ parameter_name =  'P_QUANTIDADE' ]-value OPTIONAL ).

              "///////////////////// Logica de ciação //////////////////
              ls_dados = lv_tag.

              me->http_req(
                EXPORTING
                  iv_dadosqr      = ls_dados
                  iv_quantidade   = lv_quantidade
                IMPORTING
                  ev_msgtype     = DATA(lv_msgtype)
                  ev_result      = DATA(lv_result)
                  ev_http_status = DATA(lv_http_status)
                  ev_status_text = DATA(lv_status_text)
              ).

              APPEND VALUE #(
                msgty  = lv_msgtype
                message = lv_status_text
                result_xml = lv_result
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

  METHOD http_req.
    DATA: ls_data      TYPE zspp_shop_floor_api_post.
    DATA: lv_request TYPE string,
          lv_result  TYPE string,
          lv_code    TYPE i,
          lv_reason  TYPE string,
          lt_return  TYPE bapiret2_t.

    "DATA(lo_api) = NEW zclpp_shop_floor_api( ).
    DATA(lv_url) = |/sap/opu/odata/sap/API_PROD_ORDER_CONFIRMATION_2_SRV/ProdnOrdConf2|.
    MOVE-CORRESPONDING iv_dadosqr TO ls_data.

    ls_data-_confirmation_unit = iv_dadosqr-_production_unit.
    ls_data-_confirmation_yield_quantity = iv_quantidade.

    me->valida_peso(
      EXPORTING
        iv_dadosqr     = iv_dadosqr
        iv_quantidade  = CONV dec_16_08_s( iv_quantidade )
    IMPORTING
      ev_msgtype     = ev_msgtype
      ev_status_text = ev_status_text
    ).

    IF ev_msgtype EQ 'E'.
        RETURN.
    ENDIF.

    me->send(
      EXPORTING
        iv_uri       = lv_url
        is_structure = ls_data
      IMPORTING
        ev_request   = lv_request
        ev_result    = ev_result
        ev_code      = lv_code
        ev_reason    = lv_reason
        et_return    = lt_return
    ).

    IF lt_return IS NOT INITIAL.
      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX 1.
      MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number
          WITH <fs_return>-message_v1 <fs_return>-message_v2 <fs_return>-message_v3 <fs_return>-message_v4 INTO ev_status_text.

      ev_msgtype = <fs_return>-type.
    ENDIF.

  ENDMETHOD.

  METHOD valida_peso.
    CONSTANTS: lc_form_04 TYPE rvari_vnam VALUE 'Z_PP_FORM1_04'.

    DATA: lt_tvarv      TYPE tt_rsdsselopt,
          lv_tvatv_name TYPE rvari_vnam,
          lv_min        TYPE dec_16_08_s,
          lv_max        TYPE dec_16_08_s.

    IF iv_quantidade IS INITIAL.
        MESSAGE e010(ZPP_SHOP_FLOOR) INTO ev_status_text.
        ev_msgtype = 'E'.
        RETURN.
    ENDIF.

    DATA(lo_tvarv) = NEW zcl_tvarv_util(  ).

    lv_tvatv_name = |ZPP_{ iv_dadosqr-_material ALPHA = OUT }|.   "tvarv vai existir um parametro pra cada material material

    lo_tvarv->get_values(
      EXPORTING
        input  = lv_tvatv_name
        type   = 'S'
      IMPORTING
        output = lt_tvarv
    ).

    READ TABLE lt_tvarv ASSIGNING FIELD-SYMBOL(<ls_tvarv>) INDEX 1.
    IF sy-subrc IS INITIAL.
      lv_min = <ls_tvarv>-low. lv_max = <ls_tvarv>-high.

      IF iv_quantidade < lv_min OR iv_quantidade > lv_max.
        MESSAGE e005(ZPP_SHOP_FLOOR) WITH lt_tvarv[ 1 ]-low lt_tvarv[ 1 ]-high INTO ev_status_text.
        ev_msgtype = 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD send.
    CONSTANTS: lc_destination      TYPE char20 VALUE 'NONE',             ##NO_TEXT
               lc_content          TYPE string VALUE 'Content-Type',     ##NO_TEXT
               lc_contentval       TYPE string VALUE 'application/json', ##NO_TEXT
               lc_x_requested_with TYPE string VALUE 'X-Requested-With'. ##NO_TEXT

    DATA: lo_client TYPE REF TO if_http_client.

    FREE: et_return, ev_code, ev_reason, ev_request, ev_result. ", es_interface.
    "//////////////////////////////// Cria a conexão HTTP /////////////////////////////////////////////////////////////////////
    cl_http_client=>create_by_destination( EXPORTING  destination              = lc_destination "tabela de parametro
                                           IMPORTING  client                   = lo_client
                                           EXCEPTIONS argument_not_found       = 1
                                                      destination_not_found    = 2
                                                      destination_no_authority = 3
                                                      plugin_not_active        = 4
                                                      internal_error           = 5
                                                      OTHERS                   = 6 ).

    cl_http_utility=>set_request_uri( EXPORTING request = lo_client->request
                                                uri     = iv_uri ).
    "/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    IF sy-subrc IS NOT INITIAL.
      et_return = me->get_messagem( sy ).
      RETURN.
    ENDIF.
    "////////////////////////////////  Cabeçalo da mensagem /////////////////////////////////////////////////////////////////////
    lo_client->request->set_method( if_http_entity=>co_request_method_post ). "Seta para o método POST
    lo_client->request->set_content_type( EXPORTING content_type = if_rest_media_type=>gc_appl_json ).  " 'content_type' : application/json'
    lo_client->request->set_header_field( EXPORTING name  = lc_x_requested_with  value = 'X' )." 'X-Requested-With' : 'X'
    "/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ev_request = conv_data_to_json( iv_data = is_structure ). "converte a estrutura em JSON
    IF ev_request IS INITIAL.
      et_return = VALUE #( BASE et_return ( type = 'A' id = 'ZPP_SHOP_FLOOR' number = '004' ) ).
      RETURN.
    ENDIF.

    lo_client->request->set_cdata( ev_request ). "Adiciona o json no body da mensagem

    lo_client->send( EXCEPTIONS http_communication_failure = 1 OTHERS = 99 ). "Envia os dados para a API
    IF sy-subrc IS NOT INITIAL.
      et_return = me->get_messagem( sy ).
      RETURN.
    ENDIF.
    "////////////////////////////////  Recebe a resposta /////////////////////////////////////////////////////////////////////
    lo_client->receive( EXCEPTIONS http_communication_failure = 1 OTHERS = 4 ). "recebe a resposta

    DATA(lv_response) = lo_client->response->get_data( ).

    lo_client->response->get_status( IMPORTING code = ev_code  reason = ev_reason ).

    ev_result = cl_bcs_convert=>xstring_to_string( iv_cp = cl_sx_mime_singlepart=>get_sx_node_codepage( )  iv_xstr = lv_response ).
   "/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    IF ev_code NE '200' AND ev_code NE '201'.
      "Falha na chamada do Serviço. Código  &1. Motivo: &2
      et_return = me->get_messagem( VALUE sy( msgno = '001'
                                              msgv1 = |{ CONV symsgv( ev_code ) ALPHA = OUT }|
                                              msgv2 = CONV #( ev_reason )
                                            ) ).
     RETURN.
    ENDIF.

   "ev_code: 200 = ok  | ev_code: 201 Criado com sucesso
    et_return = VALUE #( BASE et_return ( type = 'S' id = 'ZPP_SHOP_FLOOR' number = ev_code ) ).

  ENDMETHOD.

  METHOD get_messagem.
    DATA(ls_sy) = is_sy.
    IF ls_sy-msgty IS INITIAL.
      ls_sy-msgty = 'E'.
      ls_sy-msgid = 'ZPP_SHOP_FLOOR'.
    ENDIF.

    rt_return = VALUE #( BASE rt_return (
                                  type       = ls_sy-msgty
                                  id         = ls_sy-msgid
                                  number     = ls_sy-msgno
                                  message_v1 = ls_sy-msgv1
                                  message_v2 = ls_sy-msgv2
                                  message_v3 = ls_sy-msgv3
                                  message_v4 = ls_sy-msgv4
                             )
                       ).

  ENDMETHOD.

  METHOD conv_data_to_json.

    IF iv_data IS INITIAL.
      RETURN.
    ENDIF.

    /ui2/cl_json=>serialize( EXPORTING data        = iv_data
                                       compress    = iv_compress
                                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                             RECEIVING r_json      = rv_json ).

  ENDMETHOD.

  METHOD conv_json_to_data.
    IF iv_json IS INITIAL.
      RETURN.
    ENDIF.
    " Estrutura
    /ui2/cl_json=>deserialize( EXPORTING json           = iv_json
                               CHANGING  data           = es_structure ).

    " Tabela
    /ui2/cl_json=>deserialize( EXPORTING json           = iv_json
                               CHANGING  data           = et_table ).
  ENDMETHOD.

ENDCLASS.








