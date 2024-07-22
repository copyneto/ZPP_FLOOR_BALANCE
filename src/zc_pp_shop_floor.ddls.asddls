@EndUserText.label: 'CDS Consumo - Shop Floor'
@Metadata.allowExtensions: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZC_PP_SHOP_FLOOR
  as projection on ZI_PP_SHOP_FLOOR
{
      @EndUserText.label: 'ID de confirmações da ordem'
  key MfgOrderConfirmationGroup,
      @EndUserText.label: 'Identificador de confirmação'
  key MfgOrderConfirmation,
      @EndUserText.label: 'ID da ordem de produção'
      ManufacturingOrder,
      @EndUserText.label: 'Quantidade boa a ser confirmada'
      ConfirmationYieldQuantity,
      @EndUserText.label: 'Unidade de medida de confirmação'
      ConfirmationUnit,
      @EndUserText.label: 'Nº material para ordem'
      @ObjectModel.text.element: ['MaterialName'] -- descricao da chave
      @Consumption.valueHelpDefinition: [{ entity:{ name: 'I_MaterialText', element: 'Material' } }]
      Material,
      MaterialName,
      @EndUserText.label: 'Número do lote'
      Batch,
      @EndUserText.label: 'Nº europeu do artigo (EAN)'
      ean11,
      @EndUserText.label: 'Número do documento do material'
      MaterialDocument,
      @EndUserText.label: 'Data de lançamento'
      PostingDate,
      @EndUserText.label: 'Data de entrada da confirmação'
      MfgOrderConfirmationEntryDate,
      @EndUserText.label: 'Hora de entrada da confirmação'
      MfgOrderConfirmationEntryTime,
      @EndUserText.label: 'Usuário que inseriu a confirmação'
      EnteredByUser,
      @ObjectModel.text.element: ['PlantName'] -- descricao da chave
      @Consumption.valueHelpDefinition: [{ entity:{ name: 'I_Plant', element: 'Plant' } }]
      Plant,
      PlantName,
      @EndUserText.label: 'Status'
      Status,
      StatusCriticality,
      QRCode
}
