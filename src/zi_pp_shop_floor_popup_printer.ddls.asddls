@EndUserText.label: 'CDS Abstrata - POPUP de impressão'
define abstract entity ZI_PP_SHOP_FLOOR_POPUP_PRINTER 
{
  @Consumption.valueHelpDefinition: [ { entity:  { name:    'ZI_PP_VH_SHOP_FLOOR_PRINTER',
                                                   element: 'PrinterId' } }]
  @EndUserText.label: 'Impressora'
  Printer : rspopshort ;

}
