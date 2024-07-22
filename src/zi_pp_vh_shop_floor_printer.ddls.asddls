@AbapCatalog.sqlViewName: 'ZVPPSFLOORPRINT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help - Impressora'
@Search.searchable: true
define view ZI_PP_VH_SHOP_FLOOR_PRINTER 
  as select from tsp03d
{
      @Search.defaultSearchElement: true
  key name       as PrinterName, 
      @Search.defaultSearchElement: true
      padest     as PrinterId,
      @Search.defaultSearchElement: true
      patype     as PrinterType,
      //      @Search.defaultSearchElement: true>
      pastandort as PrinterLocation
}
