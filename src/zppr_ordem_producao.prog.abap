***********************************************************************
***                          © COCATREL                             ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: Programa auxiliar SMARTFORMS - ZSFPP_ORDEM_PRODUCAO    *
*** Impressão de Ordem Produção                                       *
*** AUTOR: Jeniffer Araujo – META                                     *
*** FUNCIONAL: Antonio  – Meta                                        *
*** DATA: 10/07/2023                                                  *
***********************************************************************
*** HISTÓRICO DAS MODIFICAÇÕES                                        *
*** DATA  | AUTOR | DESCRIÇÃO                                         *
***	      |	      |	                                                  *
***********************************************************************
REPORT zppr_ordem_producao.

DATA caufvd_p_tab LIKE SORTED TABLE OF caufvd_p WITH UNIQUE KEY
                  aufnr                         WITH HEADER LINE.

DATA: ls_dados      TYPE zpps_ordem_producao,
      lv_pdf_file   TYPE xstring,
      lo_adobeforms TYPE REF TO zclpp_shop_flo_adobe.

DATA: lv_form04   TYPE tt_rsdsselopt,
      lv_descarte TYPE string,
      lv_codigo   TYPE string,
      lv_sif      TYPE string.

CONSTANTS: lc_form_04 TYPE rvari_vnam VALUE 'Z_PP_FORM1_04',
           lc_form_05 TYPE rvari_vnam VALUE 'Z_PP_FORM1_05',
           lc_form_06 TYPE rvari_vnam VALUE 'Z_PP_FORM1_06',
           lc_pp03    TYPE char4 VALUE 'PP03'.

* general declarations
INCLUDE ppcoincl.
INCLUDE codrgt10.
INCLUDE lcodrinc.

READ TABLE caufvd_tab[] ASSIGNING FIELD-SYMBOL(<fs_caufvd_p_tab>) INDEX 1.

DATA(lo_tvarv) = NEW zcl_tvarv_util(  ).

DATA(lv_codigoint) = lo_tvarv->get_single_value( i_param = lc_form_04 ).
SPLIT lv_codigoint AT ':' INTO lv_descarte lv_codigo.
ls_dados-codigo = lv_codigo.

DATA(lv_sifint) = lo_tvarv->get_single_value( i_param = lc_form_05 ).
SPLIT lv_sifint AT ':' INTO lv_descarte lv_sif.
ls_dados-sif = lv_sif.

ls_dados-revisao = lo_tvarv->get_single_value( i_param = lc_form_06 ).

CHECK <fs_caufvd_p_tab> IS ASSIGNED.
ls_dados-ordem = |{ <fs_caufvd_p_tab>-aufnr ALPHA = OUT }|.
ls_dados-data = |{ sy-datum+4(2) }/{ sy-datum(4) }|.
ls_dados-data_liberacao = <fs_caufvd_p_tab>-ftrmi .
ls_dados-matnr = <fs_caufvd_p_tab>-matnr.
ls_dados-descricao = <fs_caufvd_p_tab>-matxt.
ls_dados-data_fabricacao = <fs_caufvd_p_tab>-ftrmi.
ls_dados-qnt_total = <fs_caufvd_p_tab>-gamng.

SELECT SINGLE operationconfirmation,
              manufacturingordersequence,
              manufacturingorderoperation
  FROM i_mfgorderoperation
  INTO @DATA(ls_operationconfirmation)
  WHERE manufacturingorder = @<fs_caufvd_p_tab>-aufnr
    AND operationcontrolprofile = @lc_pp03.

ls_dados-qrcode = |{ <fs_caufvd_p_tab>-aufnr }{ <fs_caufvd_p_tab>-matnr }{ <fs_caufvd_p_tab>-werks }{ <fs_caufvd_p_tab>-gmein }{ ls_operationconfirmation-operationconfirmation ALPHA = OUT }{ ls_operationconfirmation-manufacturingordersequence }{
ls_operationconfirmation-ManufacturingOrderOperation }|.
REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN ls_dados-qrcode WITH ''.

CREATE OBJECT lo_adobeforms.
lv_pdf_file = lo_adobeforms->get_adobe(
  EXPORTING
    is_header = ls_dados
).
