CLASS zclpp_shop_floor_ce DEFINITION
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

    METHODS http_req
      IMPORTING
        iv_dadosqr     TYPE ty_dados
        iv_quantidade  TYPE string
      EXPORTING
        ev_msgtype     TYPE char1
        ev_http_status TYPE i
        ev_status_text TYPE string.

    METHODS valida_peso
      IMPORTING
        iv_dadosqr     TYPE ty_dados
        iv_quantidade  TYPE dec_16_08_s
      EXPORTING
        ev_msgtype     TYPE char1
        ev_status_text TYPE string.


ENDCLASS.



CLASS zclpp_shop_floor_ce IMPLEMENTATION.

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
                  ev_http_status = DATA(lv_http_status)
                  ev_status_text = DATA(lv_status_text)
              ).

              APPEND VALUE #(
                msgty  = lv_msgtype
                message = lv_status_text
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

    DATA(lo_api) = NEW zclpp_shop_floor_api( ).
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

    lo_api->send(
      EXPORTING
        iv_uri       = lv_url
        is_structure = ls_data
      IMPORTING
        ev_request   = lv_request
        ev_result    = lv_result
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

ENDCLASS.
