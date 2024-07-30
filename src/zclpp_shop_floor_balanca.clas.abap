CLASS zclpp_shop_floor_balanca DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_rap_query_provider,
                if_apc_wsp_event_handler.

    DATA: m_message TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Implementa um TCP socket paraa busca o peso da balança
    "! @parameter ev_result | Resultado (Peso da balança)
    "! @parameter ev_msgtype | Tipo da mensagem Erro, Aviso...
    "! @parameter ev_reason | Mensagem de erro
    METHODS get_peso
      EXPORTING
        !ev_result  TYPE string
        !ev_msgtype TYPE char1
        !ev_reason  TYPE string.

ENDCLASS.



CLASS zclpp_shop_floor_balanca IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    DATA: lo_cl_somu_form_services TYPE REF TO cl_somu_form_services.

    DATA: lt_tab         TYPE TABLE OF zi_pp_shop_floor_balanca,
          lt_return      TYPE bapiret2_t,
          lt_master_keys TYPE cl_somu_form_services=>ty_gt_key,
          lt_keys        TYPE cl_somu_form_services=>ty_gt_key..

    DATA: lv_result  TYPE string,
          lv_msgtype TYPE char1,
          lv_message TYPE string.

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
              me->get_peso(
                IMPORTING
                  ev_result    = lv_result
                  ev_msgtype   = lv_msgtype
                  ev_reason    = lv_message
              ).

              APPEND VALUE #(
                peso  = lv_result
                msgty  = lv_msgtype
                message = lv_message
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

  METHOD get_peso.
    CONSTANTS: lc_tvarv_balanca TYPE rvari_vnam VALUE 'Z_PP_SHOP_FLOOR_IP_BALANCA',
               lc_frame_t       TYPE string     VALUE '0D', "frame terminator bytes, e.g. line feed 0A
               lc_msgid         TYPE sy-msgid   VALUE 'ZPP_SHOP_FLOOR',
               lc_erro          TYPE sy-msgty   VALUE 'E',
               lc_007           TYPE sy-msgno   VALUE '007',
               lc_008           TYPE sy-msgno   VALUE '008',
               lc_009           TYPE sy-msgno   VALUE '009'.

    DATA: lo_client          TYPE REF TO if_apc_wsp_client,
          lo_event_handler   TYPE REF TO zclpp_shop_floor_balanca,
          lo_message_manager TYPE REF TO if_apc_wsp_message_manager,
          lo_message         TYPE REF TO if_apc_wsp_message.

    DATA: ls_frame TYPE apc_tcp_frame,
          lv_host_tvarv TYPE string.

    DATA(lo_tvarv) = NEW zcl_tvarv_util( ).

    lv_host_tvarv = lo_tvarv->get_single_value( i_param = lc_tvarv_balanca ). "Busca o IP da balança exp: 192.168.5.152:8000
    IF lv_host_tvarv IS INITIAL.
        MESSAGE ID lc_msgid TYPE lc_erro NUMBER lc_008 WITH lc_tvarv_balanca INTO ev_reason.
        ev_msgtype = lc_erro.
        RETURN.
    ENDIF.


    SPLIT lv_host_tvarv AT ':' INTO: DATA(lv_host) DATA(lv_port). "separa o IP e a Porta

    TRY.
        " create the event handler object,  the interface IF_APC_WSP_EVENT_HANDLER is implemented in local class lcl_apc_handler
        CREATE OBJECT lo_event_handler.
        " specification of TCP frame
        ls_frame-frame_type = if_apc_tcp_frame_types=>co_frame_type_terminator. "frames are terminated with specific bytes
        ls_frame-terminator = lc_frame_t. "frame termination bytes
        lo_client = cl_apc_tcp_client_manager=>create( i_host = lv_host
                                                       i_port = lv_port
                                                       i_frame = ls_frame
                                                       i_event_handler = lo_event_handler ).

        " initiate the connection setup, successful connect leads to execution of ON_OPEN
        lo_client->connect( ).

        " wait for the a message from peer
        CLEAR: lo_event_handler->m_message.
        WAIT FOR PUSH CHANNELS UNTIL lo_event_handler->m_message IS NOT INITIAL UP TO 10 SECONDS. "busca o peso da balança
        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID lc_msgid TYPE lc_erro NUMBER lc_007 INTO ev_reason.
          ev_msgtype = lc_erro.
          lo_client->close( i_reason = 'application closed connection!' ).
          RETURN.
        ENDIF.
       "/////////////      Farmatar o valor do peso //////////////////////////
        DATA(lv_received_message) = lo_event_handler->m_message.
        DATA(lv_vl_peso) = CONV numeric12( lv_received_message+4(12) ).
        ev_result = lv_vl_peso / 100000000.
        ev_reason = lv_received_message+2(1).
       "//////////////////////////////////////////////////////////////////////

        lo_client->close( i_reason = 'closed!' ).

      CATCH cx_apc_error INTO DATA(lx_apc_error).
        MESSAGE ID lc_msgid TYPE lc_erro NUMBER lc_009 WITH lv_host_tvarv INTO DATA(lv_msg).
        ev_msgtype = lc_erro.  ev_reason = |{ lv_msg } { lx_apc_error->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_open.
    RETURN.
  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_message.
    TRY.
        m_message = i_message->get_text( ).
      CATCH cx_apc_error INTO DATA(lx_apc_error).
        m_message = lx_apc_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_close.
    RETURN.
  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_error.
    RETURN.
  ENDMETHOD.

ENDCLASS.

