@EndUserText.label: 'Custon Entity - Busca Peso'
@ObjectModel.query.implementedBy: 'ABAP:ZCLPP_SHOP_FLOOR_BALANCA'
define custom entity ZI_PP_SHOP_FLOOR_BALANCA  
{
  key peso : char12;
  key Msgty   : syst_msgty;
  key Message : char1024_cs;
}
