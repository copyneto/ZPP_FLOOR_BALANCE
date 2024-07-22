@EndUserText.label: 'Custon Entity - Criar'
@ObjectModel.query.implementedBy: 'ABAP:ZCLPP_SHOP_FLOOR_CE'
define custom entity ZI_PP_SHOP_FLOOR_CE 
 with parameters 
    P_TAG : char60,
    P_Quantidade : char11
 
{
  key Msgty   : syst_msgty;
  key Message : char100;
}
