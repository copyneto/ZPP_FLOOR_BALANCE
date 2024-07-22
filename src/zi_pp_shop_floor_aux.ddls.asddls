@AbapCatalog.sqlViewName: 'ZVPPSHOPFAUX'
@EndUserText.label: 'CDS Auxiliar- Shop Floor'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog.preserveKey: true
define root view ZI_PP_SHOP_FLOOR_AUX
  as select from    I_MfgOrderMaterialDocumentItem as _MfgOrderMaterialDocumentItem

    left outer join mean on  mean.matnr = _MfgOrderMaterialDocumentItem.MaterialDocumentItem
                         and mean.eantp = 'UC'
   association [1..1] to I_MaterialText  as _MaterialText   on  _MaterialText.Material = $projection.Material 
                                                            and _MaterialText.Language = $session.system_language

{
  key _MfgOrderMaterialDocumentItem.MaterialDocument,
  key _MfgOrderMaterialDocumentItem.MaterialDocumentItem,
  key _MfgOrderMaterialDocumentItem.MaterialDocumentYear,
  key mean.matnr,
  key mean.meinh,
  key mean.lfnum,
      mean.ean11,
      _MfgOrderMaterialDocumentItem.Material,
      _MaterialText.MaterialName,
      _MfgOrderMaterialDocumentItem.Batch,
      _MfgOrderMaterialDocumentItem.PostingDate
}
