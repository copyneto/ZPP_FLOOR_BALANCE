CLASS lhc_floor DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_msg,
             msg_type TYPE sy-msgty,
             message  TYPE char40,
           END OF ty_msg.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE floor.

    METHODS read FOR READ
      IMPORTING keys FOR READ floor RESULT result.

*    METHODS criar FOR MODIFY
*      IMPORTING keys FOR ACTION Floor~criar.

    METHODS imprimir FOR MODIFY
      IMPORTING keys FOR ACTION floor~imprimir.

    METHODS get_features FOR FEATURES
      IMPORTING keys REQUEST requested_features FOR floor RESULT result.

ENDCLASS.

CLASS lhc_floor IMPLEMENTATION.

  METHOD create.
    DATA: lt_msg TYPE TABLE OF ty_msg,
          lv_msg TYPE ty_msg-message.

    IF lt_msg IS INITIAL.
      MESSAGE ID 'ZPP_SHOP_FLOOR' TYPE 'E' NUMBER '003' INTO lv_msg.
      APPEND VALUE #( msg_type = 'E' message = lv_msg ) TO lt_msg.
    ENDIF.

    reported-floor = VALUE #(
          FOR ls_messages IN lt_msg (
             %msg = new_message_with_text(
                  severity = COND #(
                                       WHEN ls_messages-msg_type EQ 'E' THEN if_abap_behv_message=>severity-error
                                       WHEN ls_messages-msg_type EQ 'S' THEN if_abap_behv_message=>severity-success
                                       WHEN ls_messages-msg_type EQ 'A' THEN if_abap_behv_message=>severity-warning
                                       ELSE if_abap_behv_message=>severity-none
                                    )
                  text = ls_messages-message
             )
          )
    ).
  ENDMETHOD.

  METHOD read.
    RETURN.
  ENDMETHOD.

*  METHOD criar.
*    RETURN.
*  ENDMETHOD.

  METHOD imprimir.

    CONSTANTS lc_form TYPE tdsfname VALUE 'ZPP_ETIQUETA_SHOPFLOOR'.
    CONSTANTS lc_z_pp_form2_tara_kg TYPE rvari_vnam VALUE 'Z_PP_FORM2_TARA_KG'.
    CONSTANTS lc_printter_id TYPE rvari_vnam VALUE 'Z_PP_FORM2_IMPRESSORA'.

    DATA: lv_function   TYPE rs38l_fnam.
    DATA: ls_structure TYPE zppsetiquetashop,
          ls_compop    TYPE ssfcompop.
    DATA: ls_ctrlop TYPE ssfctrlop,
          ls_otf    TYPE ssfcrescl.

    DATA ls_order TYPE bapi_pp_order_objects.
    DATA lt_operation TYPE TABLE OF bapi_order_operation1.
    DATA: lo_tvarv TYPE REF TO zcl_tvarv_util.
    DATA: lt_table TYPE TABLE OF zi_pp_shop_floor.
    DATA: lt_msg        TYPE TABLE OF ty_msg,
          lv_msg        TYPE ty_msg-message,
          lv_printer_id TYPE rspopshort.
    lo_tvarv = NEW zcl_tvarv_util( ).


    lv_printer_id = lo_tvarv->get_single_value( i_param = lc_printter_id ). "Busca se tem impressora cadastrada na stvarv
    IF lv_printer_id IS INITIAL.
        READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.
        IF sy-subrc IS INITIAL.
          lv_printer_id = <fs_key>-%param-printer.
        ENDIF.
    ENDIF.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME' "#EC CI_SUBRC
      EXPORTING
        formname           = lc_form
      IMPORTING
        fm_name            = lv_function
      EXCEPTIONS ##FM_SUBRC_OK
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
      APPEND VALUE #( msg_type = sy-msgty  message = lv_msg ) TO lt_msg. "#EC CI_SUBRC
    ENDIF.
    IF keys IS NOT INITIAL.

      SELECT mfgorderconfirmationgroup,
             mfgorderconfirmation,
             manufacturingorder,
             confirmationyieldquantity,
             confirmationyieldquantity,
             confirmationunit,
             material,
             materialname,
             batch,
             ean11,
             postingdate,
             mfgorderconfirmationentrydate,
             mfgorderconfirmationentrytime,
             enteredbyuser
      FROM zi_pp_shop_floor
      INTO CORRESPONDING FIELDS OF TABLE @lt_table
      FOR ALL ENTRIES IN @keys
      WHERE mfgorderconfirmationgroup = @keys-mfgorderconfirmationgroup
        AND mfgorderconfirmation = @keys-mfgorderconfirmation.

      CHECK lt_table IS NOT INITIAL.

      SELECT plant,
             material,
             batch,
             shelflifeexpirationdate
        FROM i_batch
        INTO TABLE @DATA(lt_batch)
        FOR ALL ENTRIES IN @lt_table
        WHERE plant    = @lt_table-plant
          AND material = @lt_table-material
          AND batch    = @lt_table-batch. "#EC CI_NO_TRANSFORM

    ENDIF.

    DATA(lv_tara) = lo_tvarv->get_single_value( i_param = lc_z_pp_form2_tara_kg ).
    REPLACE ',' IN lv_tara WITH '.'.

    LOOP AT lt_table INTO DATA(ls_table).

      ls_structure-tara = lv_tara.
      ls_structure-codigo       = ls_table-material.
      ls_structure-descricao    = ls_table-materialname.
*     ls_structure-peso         = lt_table--gmnga.
*     ls_structure-peso_liquido = |{ is_confirmation-gmnga }-{  ls_structure-tara }|
      ls_structure-ordem        = ls_table-mfgorderconfirmationgroup.
      ls_structure-producao     = ls_table-manufacturingorder.
      ls_structure-lote         = ls_table-batch.
      ls_structure-usuario      = sy-uname.
      ls_structure-impressao    = |{ sy-datum }{ sy-uzeit }|.
      ls_structure-barcode02    = ls_table-ean11.
      ls_structure-barcode01 = | { ls_structure-codigo }{ ls_structure-peso }{ ls_structure-tara }{  ls_structure-peso_liquido }{ ls_structure-producao }{ ls_structure-lote }|.

      TRY.
          ls_structure-validade     = lt_batch[ plant = ls_table-plant material = ls_table-material batch = ls_table-batch ]-shelflifeexpirationdate.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      ls_compop = VALUE ssfcompop( tddest  = lv_printer_id
                                   tdnoprev = abap_true
                                   tdimmed = abap_true ).

      ls_compop-tdnoprev  = 'X'.
      ls_ctrlop-langu = sy-langu.
      ls_ctrlop-getotf = abap_false.
      ls_ctrlop-preview = abap_false.
      ls_compop-tddest    = lv_printer_id.
      ls_compop-tdnewid   = 'X'.

      ls_ctrlop-no_dialog = abap_true.

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

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
        APPEND VALUE #( msg_type = sy-msgty  message = lv_msg ) TO lt_msg. "#EC CI_SUBRC
      ENDIF.

      CLEAR: ls_structure.
    ENDLOOP.
    IF lt_msg IS INITIAL.
      MESSAGE ID 'ZPP_SHOP_FLOOR' TYPE 'S' NUMBER '003' INTO lv_msg.
      APPEND VALUE #( msg_type = 'S' message = lv_msg ) TO lt_msg.
    ENDIF.
    reported-floor = VALUE #(
          FOR ls_messages IN lt_msg (
             %msg = new_message_with_text(
                  severity = COND #(
                                       WHEN ls_messages-msg_type EQ 'E' THEN if_abap_behv_message=>severity-error
                                       WHEN ls_messages-msg_type EQ 'S' THEN if_abap_behv_message=>severity-success
                                       WHEN ls_messages-msg_type EQ 'A' THEN if_abap_behv_message=>severity-warning
                                       ELSE if_abap_behv_message=>severity-none
                                    )
                  text = ls_messages-message
             )
          )
    ).

  ENDMETHOD.

  METHOD get_features.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_zi_pp_shop_floor DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS check_before_save REDEFINITION.

    METHODS finalize          REDEFINITION.

    METHODS save              REDEFINITION.

ENDCLASS.

CLASS lsc_zi_pp_shop_floor IMPLEMENTATION.

  METHOD check_before_save.
    RETURN.
  ENDMETHOD.

  METHOD finalize.
    RETURN.
  ENDMETHOD.

  METHOD save.
    RETURN.
  ENDMETHOD.

ENDCLASS.
