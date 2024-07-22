@AbapCatalog.sqlViewName: 'ZVPPSHOPFLOOR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interface - Shop Floor'
define root view ZI_PP_SHOP_FLOOR
  as select from I_MfgOrderConfirmation as _MfgOrderConfirmation
  association [1..*] to ZI_PP_SHOP_FLOOR_AUX as _MfgOrderMaterialDocumentItem on  _MfgOrderConfirmation.MaterialDocument             = _MfgOrderMaterialDocumentItem.MaterialDocument
                                                                              and _MfgOrderMaterialDocumentItem.MaterialDocumentItem = '0001'
  association [1..1] to I_ManufacturingOrder as _ManufacturingOrder           on  _ManufacturingOrder.ManufacturingOrder = _MfgOrderConfirmation.ManufacturingOrder
  association [1..1] to I_Plant         as _Plant                     on  $projection.Plant = _Plant.Plant
{
  key _MfgOrderConfirmation.MfgOrderConfirmationGroup,
  key _MfgOrderConfirmation.MfgOrderConfirmation,
      _MfgOrderConfirmation.Plant,
      _Plant.PlantName,
      _ManufacturingOrder.ManufacturingOrder,
      @Semantics.quantity.unitOfMeasure: 'ConfirmationUnit'
      _MfgOrderConfirmation.ConfirmationYieldQuantity,
      _MfgOrderConfirmation.ConfirmationUnit,
      _MfgOrderMaterialDocumentItem.Material,
      _MfgOrderMaterialDocumentItem.MaterialName,
      _MfgOrderMaterialDocumentItem.Batch,
      _MfgOrderMaterialDocumentItem.ean11,
      _MfgOrderMaterialDocumentItem.MaterialDocument,
      _MfgOrderConfirmation._PostingDate.CalendarDate as PostingDate,
      _MfgOrderConfirmation.MfgOrderConfirmationEntryDate,
      _MfgOrderConfirmation.MfgOrderConfirmationEntryTime,
      _MfgOrderConfirmation.EnteredByUser,
      case _MfgOrderConfirmation.IsReversed 
        when 'X' then 'Cacel'
        else 'Ok'
        end as Status,
      case _MfgOrderConfirmation.IsReversed 
        when 'X' then 2
        else 3
        end as StatusCriticality,
        
       cast(' ' as char60 ) as QRCode
        
}
