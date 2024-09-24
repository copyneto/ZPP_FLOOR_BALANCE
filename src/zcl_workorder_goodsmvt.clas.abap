class ZCL_WORKORDER_GOODSMVT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_WORKORDER_GOODSMVT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WORKORDER_GOODSMVT IMPLEMENTATION.


  method IF_EX_WORKORDER_GOODSMVT~BACKFLUSH.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~COGI_AUTHORITY_CHECK.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~COGI_POST.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~COMPLETE_GOODSMOVEMENT.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~GM_SCREEN_LINE_CHECK.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~GM_SCREEN_OKCODE_CHECK.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~GM_WIPBATCH_CHECK.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~GM_WIPBATCH_PROPOSE.
  endmethod.


  METHOD if_ex_workorder_goodsmvt~goods_receipt.
    CONSTANTS lc_z_pp_form2_tara_kg TYPE rvari_vnam VALUE 'Z_PP_FORM2_TARA_KG_'.
    DATA: lv_tara TYPE ru_gmnga.
    DATA(lo_tvarv) = new zcl_tvarv_util( ).

    CHECK ct_goods_receipt IS NOT INITIAL.

    SELECT isbatchmanagementrequired
      FROM i_material
      INTO TABLE @DATA(lt_required)
      FOR ALL ENTRIES IN @ct_goods_receipt
      WHERE material = @ct_goods_receipt-matnr
        AND isbatchmanagementrequired = @abap_true.

    IF lt_required IS NOT INITIAL.
      LOOP AT ct_goods_receipt ASSIGNING FIELD-SYMBOL(<fs_goods>).
        DATA(lv_param_name_tvarv) = CONV rvari_vnam( |{ lc_z_pp_form2_tara_kg }{ <fs_goods>-matnr ALPHA = OUT }| ).
        lv_tara = lo_tvarv->get_single_value( i_param = lv_param_name_tvarv ).
        <fs_goods>-erfmg -= lv_tara.
        <fs_goods>-hsdat = sy-datum.
      ENDLOOP.
    ELSE.
      RETURN.
    ENDIF.


  ENDMETHOD.


  method IF_EX_WORKORDER_GOODSMVT~IM_CALLED.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~MANUAL_GOODS_RECEIPT.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~PICKLIST.
  endmethod.
ENDCLASS.
