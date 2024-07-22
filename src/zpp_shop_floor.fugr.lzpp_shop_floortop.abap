FUNCTION-POOL zfgpp_spool MESSAGE-ID spads.

INCLUDE fp_spool_constants.                      " Include from SADF
INCLUDE rspoopt.


DATA: BEGIN OF lt_spool_handles OCCURS 3,
        handle(20) TYPE x,
        rqid       LIKE tsp01-rqident,
        ptype      LIKE tsp03-patype,
        comp,
        active,
        pages      TYPE i,
        last_title LIKE rspotoce-title,
        toc        LIKE rspotoce OCCURS 20,
      END OF lt_spool_handles.

CONSTANTS gc_buffer_not_filled TYPE tspoptions-value VALUE '@@@@@@@'.
DATA: gv_ads_pre_pdl  TYPE tspoptions-value VALUE gc_buffer_not_filled,
      gv_ads_swin_pdl TYPE tspoptions-value VALUE gc_buffer_not_filled,
      gv_ads_rdif_pdl TYPE tspoptions-value VALUE gc_buffer_not_filled.

TABLES: tsp01, tsp02a, tsp09.                               "#EC *

* Spoolrequest attributes
CONSTANTS: gc_sattr_contents LIKE tsp02a-param
                             VALUE 'S_CONTENTS'.    " Table of contents

* XDC for Printer stored in TSP03A
CONSTANTS: gc_tsp03akey_xdcinfo TYPE rspoparam VALUE 'S_XDCNAME'.

* Buffer spool patch version
DATA: gv_spool_patchlevel TYPE i.

* Buffer to store print options
CONSTANTS gc_max_opt TYPE i VALUE 99.
DATA: BEGIN OF gs_print_options_buffer,
        options_used        TYPE c,
        options(gc_max_opt) TYPE x,
        usrstring09         TYPE rspoposs_optstring,
        usrstring16         TYPE rspoposs_optstring,
        usrstring17         TYPE rspoposs_optstring,
        usrstring18         TYPE rspoposs_optstring,
      END OF gs_print_options_buffer.

* Flags to buffer kernel checks
CONSTANTS gc_unknown TYPE c VALUE 'U'.
DATA: gv_kernel_supports_prn_files  TYPE c VALUE gc_unknown,
      gv_kernel_file_ext_from_tsp0b TYPE c VALUE gc_unknown.

TYPES: BEGIN OF ty_prt,
         prt           TYPE tsp03-padest,
         name          TYPE tsp03d-name,
         devtype       TYPE tsp03-patype,
         driver        TYPE tsp0a-driver,
         xdc_available TYPE sap_bool,
         tsp0b         TYPE tsp0b,
       END OF ty_prt.
TYPES: ty_prt_tab TYPE SORTED TABLE OF ty_prt WITH UNIQUE KEY prt.
DATA gt_prt_buffer_tab TYPE ty_prt_tab.

TYPES: BEGIN OF ty_devtype,
         devtype       TYPE tsp03-patype,
         driver        TYPE tsp0a-driver,
         xdc_available TYPE sap_bool,
         tsp0b         TYPE tsp0b,
       END OF ty_devtype.
TYPES: ty_devtype_tab TYPE SORTED TABLE OF ty_devtype WITH UNIQUE KEY devtype.
DATA gt_devtype_buffer_tab TYPE ty_devtype_tab.
