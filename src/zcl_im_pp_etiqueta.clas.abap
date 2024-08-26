class ZCL_IM_PP_ETIQUETA definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_WORKORDER_CONFIRM .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_PP_ETIQUETA IMPLEMENTATION.


  method IF_EX_WORKORDER_CONFIRM~AT_CANCEL_CHECK.
  endmethod.


  METHOD if_ex_workorder_confirm~at_save.

    CONSTANTS lc_form TYPE tdsfname VALUE 'ZPP_ETIQUETA_SHOPFLOOR'.
    CONSTANTS lc_z_pp_form2_tara_kg TYPE rvari_vnam VALUE 'Z_PP_FORM2_TARA_KG_'.
    CONSTANTS lc_printter_id TYPE rvari_vnam VALUE 'Z_PP_FORM2_IMPRESSORA'.

    DATA: lo_tvarv TYPE REF TO zcl_tvarv_util.
    DATA: lt_operation TYPE TABLE OF bapi_order_operation1.
    DATA: ls_order     TYPE bapi_pp_order_objects,
          ls_structure TYPE zspp_etiqueta_shop,
          ls_compop    TYPE ssfcompop,
          ls_ctrlop    TYPE ssfctrlop,
          ls_otf       TYPE ssfcrescl.

    DATA: lv_function   TYPE rs38l_fnam,
          lv_printer_id TYPE rspopname,
          lv_material   TYPE i_manufacturingorder-Material,
          lv_batch      TYPE i_manufacturingorder-Batch.

    CREATE OBJECT lo_tvarv.

    CHECK is_confirmation-stzhl IS INITIAL.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME' "#EC CI_SUBRC
      EXPORTING
        formname           = lc_form
      IMPORTING
        fm_name            = lv_function
      EXCEPTIONS ##FM_SUBRC_OK
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    SELECT SINGLE product, Batch
      FROM i_manufacturingorder
      INTO ( @lv_material, @lv_batch )
      WHERE manufacturingorder = @is_confirmation-aufnr.

    SELECT SINGLE materialname
      FROM i_materialtext
      INTO @ls_structure-descricao
      WHERE material = @lv_material
        AND language = @sy-langu. "#EC CI_NOORDER

    SELECT SINGLE shelflifeexpirationdate
      FROM i_batch
      INTO @ls_structure-validade
      WHERE plant    = @is_confirmation-werks
        AND material = @lv_material
        AND batch    = @lv_batch. "#EC CI_NOORDER

    SELECT SINGLE ean11
      FROM mean
      INTO @ls_structure-barcode02
      WHERE matnr = @lv_material
        AND eantp = 'HE'. "#EC CI_NOORDER

    DATA(lv_param_name_tvarv) = CONV rvari_vnam( |{ lc_z_pp_form2_tara_kg }{ lv_material ALPHA = OUT }| ).
    ls_structure-tara = lo_tvarv->get_single_value( i_param = lv_param_name_tvarv ).

    ls_structure-codigo       = lv_material.
    ls_structure-peso         = is_confirmation-gmnga.
    ls_structure-peso_liquido = ( is_confirmation-gmnga - ls_structure-tara ).
    ls_structure-ordem        = is_confirmation-aufnr.
    ls_structure-producao     = is_confirmation-ersda.
    ls_structure-lote         = lv_batch. "is_confirmation-uccha.
    ls_structure-usuario      = sy-uname.
    ls_structure-impressao    = |{ sy-datum }{ sy-uzeit }|.

    ls_structure-barcode01 = |{ ls_structure-codigo }{ is_confirmation-gmnga }{ ls_structure-tara }{ ls_structure-peso_liquido }{ ls_structure-producao }{ ls_structure-lote }|.
    REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN ls_structure-barcode01 WITH ''.

    ls_order-operations = abap_true.

    lv_printer_id = lo_tvarv->get_single_value( i_param = lc_printter_id ).

    ls_compop = VALUE ssfcompop( tddest  = lv_printer_id
                                 tdnoprev = abap_true
                                 tdimmed = abap_true ).

    ls_compop-tdnoprev  = 'X'.
    ls_ctrlop-langu = sy-langu.
    ls_ctrlop-getotf = abap_false.
    ls_ctrlop-preview = abap_false.
*   ls_compop-tddest    = lv_printer_id.
    ls_compop-tdnewid   = 'X'.

    CALL FUNCTION lv_function
      EXPORTING
        control_parameters = ls_ctrlop
        output_options     = ls_compop
        user_settings      = ' '
        is_struture        = ls_structure
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc = 0.
                                                          "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  method IF_EX_WORKORDER_CONFIRM~BEFORE_UPDATE.
  endmethod.


  method IF_EX_WORKORDER_CONFIRM~INDIVIDUAL_CAPACITY.
  endmethod.


  method IF_EX_WORKORDER_CONFIRM~IN_UPDATE.
  endmethod.
ENDCLASS.
