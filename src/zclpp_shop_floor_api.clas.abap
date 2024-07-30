CLASS zclpp_shop_floor_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS send
      IMPORTING
        !iv_uri       TYPE string
        !iv_metodo    TYPE char6 OPTIONAL
        !it_table     TYPE ANY TABLE OPTIONAL
        !is_structure TYPE any OPTIONAL
      EXPORTING
        !ev_request   TYPE string
        !ev_result    TYPE string
        !ev_code      TYPE i
        !ev_reason    TYPE string
        !et_return    TYPE bapiret2_t.

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
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_messagem
      IMPORTING
        is_sy            TYPE sy OPTIONAL
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t.
ENDCLASS.



CLASS zclpp_shop_floor_api IMPLEMENTATION.

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
                                              msgv3 = ev_result ) ).
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

ENDCLASS.








