*&---------------------------------------------------------------------*
*& Report:      ZCM_DUAL_CREDIT_OUTB
*& Description: Report for KHEAA on student data/GPA/hours (Financial Aid)
*&---------------------------------------------------------------------*
*&* M O D I F I C A T I O N   L O G                                    *
*----------------------------------------------------------------------*
*  Date Programmer    Request #       Description                      *
*----------------------------------------------------------------------*
*& 02/13/2018 MWW     ERDK931208      Initial program
*&
*&---------------------------------------------------------------------*


REPORT  zcm_dual_credit_outb.

CONSTANTS true        TYPE  c VALUE 'X'.
CONSTANTS false       TYPE  c VALUE space.
CONSTANTS c_school(6) TYPE  n VALUE 927500.

TYPES: BEGIN OF selected_s,
         objid TYPE piqst_objid,
       END OF selected_s.

TYPES: selected_t TYPE TABLE OF selected_s.


TYPES:
  BEGIN OF data_s
    ,persidno(9)            TYPE  n   "student's ssn
    ,nachn                  TYPE  pad_nachn   "last name
    ,vorna                  TYPE  pad_vorna   "first name
    ,mid(1)                 TYPE  c   "middle initial
    ,gbdat                  TYPE  piq_gbdat   "dob
    ,ayear                  TYPE  dats    "academic year begin
    ,semester(1)            TYPE  c   "semester F=fall S=spring
    ,course_abev(4)         TYPE  c   "course abbreviation.
    ,course_numb(4)         TYPE  c   "course number
    ,course_credit_hours(2) TYPE  n   "course credit hours
    ,cost_per_hour(3)       TYPE  n   "cost per credit hour
    ,yearlong_course(1)     TYPE  c   "year long course space=no Y=yes
    ,lab_i(1)               TYPE  c   "lab indicator Y/N
    ,lab_course_abev(4)     TYPE  c   "lab course abbreviation
    ,lab_course_numb(4)     TYPE  c   "lab course number
    ,lab_credit_hours(2)    TYPE  n   "lab credit_hours
    ,grade                  TYPE  piqgradesym
    ,END OF data_s.

TYPES: BEGIN OF otjid_s,
         otjid TYPE otjid,
       END OF   otjid_s.

TYPES: otjid_t TYPE TABLE OF otjid_s WITH NON-UNIQUE DEFAULT KEY.

TYPES:  BEGIN OF student_s
          ,adatanr    TYPE  hradatanr
          ,id         TYPE  piqmodreg_id
          ,se_objid   TYPE  hrobjid
          ,st_otjid   TYPE  varyf
          ,st_objid   TYPE  hrobjid
          ,cs_otjid   TYPE  otjid
          ,sc_otjid   TYPE  otjid
          ,sm_otjid   TYPE  otjid
          ,peryr      TYPE piqperyr
          ,perid      TYPE piqperid
          ,course_credit_hours TYPE  piqcpearned "course credit hours
          ,gradesym   TYPE  piqgradesym
          ,grade      TYPE  piqscale_norm_val
          ,gradescale TYPE  piqscale_id
          ,agrid      TYPE  piqagrid
       ,END OF   student_s.

TYPES:  student_t   TYPE  STANDARD TABLE OF student_s WITH DEFAULT KEY.

TYPES: student_id_t TYPE TABLE OF student_s.

TYPES: BEGIN OF academic_work_s,
         aw_type      TYPE otype,
         aw_objid     TYPE realo,
         st_objid     TYPE piqst_objid, "sobid,
         student12    TYPE piqstudent12,
         modreg_id    TYPE piqmodreg_id,
         se_objid     TYPE hrobjid,
         peryr        TYPE piqperyr,
         perid        TYPE piqperid,
         smstatus     TYPE piqsmstatus,
         smrating     TYPE piqsmrating,
         storreason   TYPE piqstorreason,
         stordate     TYPE piqstordate,
         cpattemp     TYPE piqcpattemp,
         cpearned     TYPE piqcpearned,
         cpgraded     TYPE piqcpgraded,
         cpunit       TYPE piqcpunit,
         bookdate     TYPE piqdate,
         bookreason   TYPE piqbookreason,
         annulment    TYPE piqmodreg_annulment,
         gradesym     TYPE piqgradesym,
         grade        TYPE piqscale_norm_val,
         gradescale   TYPE piqscale_id,
         agrid        TYPE piqagrid,
         agrdate      TYPE piqagrdate,
         agrcompleted TYPE piqagrcompleted,
         agrtype      TYPE piqagrtype,
         agrstat      TYPE piqagrstat,
         agrnotrated  TYPE piqagrnotrated,
         begda        TYPE begda,
         endda        TYPE endda,
         short        TYPE short_d,
         persidno     TYPE piq_prdni,   "student's ssn
         nachn        TYPE  pad_nachn,   "last name
         vorna        TYPE  pad_vorna,   "first name
         mid(1)       TYPE  c,   "middle initial
         gbdat        TYPE  piq_gbdat,    "dob

       END OF academic_work_s .
TYPES:
  academic_work_t TYPE TABLE OF academic_work_s.

TYPES: BEGIN OF event_s,
         se_objid TYPE objid,
         otjid    TYPE otjid,
         begda    TYPE begda,
         endda    TYPE endda,
         kapz1    TYPE kapz1,
         kapz3    TYPE kapz3,
         category TYPE piqcategory,
       END OF event_s.

TYPES: event_t TYPE TABLE OF event_s WITH NON-UNIQUE DEFAULT KEY.



CLASS lcl_interface DEFINITION.
  PUBLIC  SECTION.
    METHODS constructor
      IMPORTING
        iv_file              TYPE  c
        iv_nofile            TYPE  c
        iv_enroll            TYPE  c
        iv_eoterm            TYPE  c
        iv_local             TYPE  c
        iv_server            TYPE  c
        iv_fpath_desk        TYPE  localfile
        iv_fpath_serv        TYPE  localfile
        iv_acayear           TYPE  piqperyr
        iv_acaperid          TYPE  piqperid
        it_selected_students TYPE selected_t
      .

    METHODS build_data.
    METHODS outbound_data.
    METHODS outbound_data_eot.
    METHODS get_personal
      IMPORTING iv_stuobjid TYPE  otjid
      CHANGING  cs_data     TYPE data_s.



  PRIVATE SECTION.

    METHODS get_current_students CHANGING ct_student TYPE student_t.
    METHODS get_academic_work.
    METHODS merge_data IMPORTING it_students TYPE student_t.

    DATA: mv_file              TYPE   c,
          mv_nofile            TYPE   c,
          mv_enroll            TYPE   c,
          mv_eoterm            TYPE   c,
          mv_local             TYPE    c,
          mv_server            TYPE   c,
          mv_fpath_desk        TYPE   localfile,
          mv_fpath_serv        TYPE   localfile,
          mv_select_dt         TYPE   datum,
          mv_acayear           TYPE   num4,
          mv_session           TYPE   num2,
          mv_peryr             TYPE   piqperyr,
          mv_perid             TYPE   piqperid,
          mv_begda             TYPE   begda,
          mv_endda             TYPE   endda,
          mv_aidyr1            TYPE   string,
          mv_aidyr2            TYPE   string,
          mv_plvar             TYPE   plvar,
          mt_selected_students TYPE   selected_t,
          mt_timelimits        TYPE   piqtimelimits_tab,
          mt_curr_students     TYPE   student_id_t,
          mt_academic_work     TYPE   academic_work_t,
          mt_events            TYPE   event_t,
          mt_data              TYPE   STANDARD TABLE OF data_s WITH DEFAULT KEY
          .

ENDCLASS.


TABLES cmacbpst.


DATA  lo_interface        TYPE REF TO lcl_interface.
DATA  gt_student_objects  TYPE selected_t.
DATA  pathlen TYPE i.


SELECTION-SCREEN BEGIN OF BLOCK input WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_stu   FOR cmacbpst-student12.
PARAMETER: p_fyr   TYPE piqperyr AS LISTBOX VISIBLE LENGTH 40 MODIF ID gr1.
PARAMETER: p_perid TYPE piqperid AS LISTBOX VISIBLE LENGTH 40 MODIF ID gr1.

PARAMETER: p_path1 TYPE string MODIF ID gr2.
SELECTION-SCREEN END  OF BLOCK input.

SELECTION-SCREEN BEGIN OF BLOCK input1 WITH FRAME TITLE text-002.
PARAMETERS p_nofile RADIOBUTTON GROUP s5.
PARAMETERS p_file RADIOBUTTON GROUP s5 DEFAULT 'X' .

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 4.
PARAMETERS p_enroll RADIOBUTTON GROUP s1 MODIF ID fs DEFAULT 'X' USER-COMMAND ch1.
SELECTION-SCREEN COMMENT (25) text-p05 FOR FIELD p_enroll MODIF ID fs.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 4.
PARAMETER: p_eoterm  RADIOBUTTON GROUP s1 MODIF ID fs.
SELECTION-SCREEN COMMENT (25) text-p06 FOR FIELD p_eoterm MODIF ID fs.
SELECTION-SCREEN END OF LINE.


PARAMETER: p_local  RADIOBUTTON GROUP s2 DEFAULT 'X' MODIF ID fs USER-COMMAND ch2.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 4.
SELECTION-SCREEN COMMENT (15) text-p07 FOR FIELD p_path2 MODIF ID fsl.
PARAMETERS p_path2  TYPE localfile DEFAULT 'C:\Documents and Settings\<USERID>\My Documents\' MODIF ID fsl.
SELECTION-SCREEN END OF LINE.

PARAMETER: p_server RADIOBUTTON GROUP s2 MODIF ID fs..

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 4.
SELECTION-SCREEN COMMENT (16) text-p08 FOR FIELD p_path3 MODIF ID fss.
PARAMETER: p_path3  TYPE localfile DEFAULT '/data/<SYSID>/conv/cm/' MODIF ID fss.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK input1.

*----------------------------------------------------------------------*
*   INITIALIZATION SECTION                                             *
*----------------------------------------------------------------------*
INITIALIZATION.



  REPLACE '<SYSID>' WITH sy-sysid(3) INTO p_path3.
  REPLACE '<USERID>' WITH sy-uname INTO p_path2.
  CONDENSE p_path2 NO-GAPS.

*----------------------------------------------------------------------*
* AT SELECTION SCREEN OUTPUT                                           *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF screen-group1 EQ 'FS'.
      IF p_nofile EQ 'X'.
        screen-active = 0.
      ELSEIF p_file EQ 'X'.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    "...hide/show local file path
    IF screen-group1 EQ 'FSL'.
      IF p_file EQ 'X'.
        IF p_local EQ 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    "...hide/show server file path
    IF screen-group1 EQ 'FSS'.
      IF p_file EQ 'X'.
        IF p_server EQ 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.


*----------------------------------------------------------------------*
*   AT SELECTION SCREEN ON VALUE REQUEST                               *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.
  p_path1 = zxx_cl_outbound_interface=>desktop_folder_dialog( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.
  p_path2 = zxx_cl_outbound_interface=>desktop_folder_dialog( ).

*----------------------------------------------------------------------*
*   START-OF-SELECTION SECTION                                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  pathlen = strlen( p_path2 ).
  IF pathlen > 1.
    pathlen = pathlen - 1.
    IF p_path2+pathlen NE '\'.
      p_path2 = p_path2 && '\'.
    ENDIF.
  ENDIF.
  pathlen = strlen( p_path3 ).
  IF pathlen > 1.
     pathlen = pathlen - 1.
    IF p_path3+pathlen NE '/'.
      p_path3 = p_path3 && '/'.
    ENDIF.
  ENDIF.

  IF p_stu IS NOT INITIAL.
    SELECT stobjid FROM cmacbpst INTO TABLE gt_student_objects
     WHERE student12 IN p_stu.
  ENDIF.

  CREATE OBJECT lo_interface
    EXPORTING
      iv_file              = p_file
      iv_nofile            = p_nofile
      iv_enroll            = p_enroll
      iv_eoterm            = p_eoterm
      iv_local             = p_local
      iv_server            = p_server
      iv_fpath_desk        = p_path2
      iv_fpath_serv        = p_path3
      iv_acayear           = p_fyr
      iv_acaperid          = p_perid
      it_selected_students = gt_student_objects.

  CALL METHOD lo_interface->build_data( ).


END-OF-SELECTION.

  IF p_enroll EQ true.
    lo_interface->outbound_data( ).
  ELSEIF p_eoterm EQ true.
    lo_interface->outbound_data_eot( ).
  ENDIF.

CLASS lcl_interface IMPLEMENTATION.
  METHOD constructor.
    DATA: ls_object   TYPE hrobject,
          lt_searchtl TYPE piqsearchtl_tab.


    mv_file = iv_file.
    mv_nofile = iv_nofile.
    mv_enroll = iv_enroll.
    mv_eoterm = iv_eoterm.
    mv_local  = iv_local.
    mv_server = iv_server.
    mv_fpath_desk = iv_fpath_desk.
    mv_fpath_serv = iv_fpath_serv.
    mv_peryr = iv_acayear.
    mv_perid = iv_acaperid.
    mt_selected_students = it_selected_students.

*    IF iv_select_dt IS INITIAL.
*      mv_select_dt = sy-datum.
*    ELSE.
*      mv_select_dt  = iv_select_dt.
*    ENDIF.
*
*    "...get first date of session
    SELECT SINGLE peryr perid begda endda INTO ( mv_peryr, mv_perid, mv_begda, mv_endda ) FROM hrt1750
     WHERE timelimit = '0100'
      AND peryr = mv_peryr
      AND perid = mv_perid.

    "...get current plvar
    CALL FUNCTION 'HRIQ_GET_ACTIVE_WF_PLVAR'
      IMPORTING
        act_plvar = mv_plvar.

    "...read time limits
    CALL FUNCTION 'HRIQ_ACAD_ORG_ROOT_GET'
      IMPORTING
        ev_acad_root_objid = ls_object-objid
      EXCEPTIONS
        no_valid_acad_root = 1
        OTHERS             = 99.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ls_object-plvar = mv_plvar.
    ls_object-otype = 'O'.

    "...Read main timelimit to get the begin date
    APPEND '0100' TO lt_searchtl.
    APPEND '0690' TO lt_searchtl.
    APPEND 'OLAI' TO lt_searchtl.
    APPEND 'OLAA' TO lt_searchtl.
    APPEND '0300' TO lt_searchtl.
    APPEND '0510' TO lt_searchtl.
    APPEND '0575' TO lt_searchtl.
    APPEND '0595' TO lt_searchtl.
    APPEND 'YBIL' TO lt_searchtl.


    CALL FUNCTION 'HRIQ_ACAD_READ_TIMELIMITS'
      EXPORTING
        is_object                = ls_object
        it_searchtl              = lt_searchtl
      IMPORTING
        et_timelimits            = mt_timelimits
      EXCEPTIONS
        no_start_object_imported = 1
        customizing_incomplete   = 2
        invalid_ca_object        = 3
        no_eval_path             = 4
        relative_timelimit_error = 5
        no_data_found            = 6
        internal_error           = 7
        OTHERS                   = 8.

*<TASK>why year 2018
    LOOP AT mt_timelimits TRANSPORTING NO FIELDS WHERE ca_peryr  < '2018'.
      DELETE mt_timelimits.
    ENDLOOP.

    LOOP AT mt_timelimits TRANSPORTING NO FIELDS WHERE ca_timelimit = '0300' AND
                                                       ca_window NE 'SPRI'.
      DELETE mt_timelimits.
    ENDLOOP.

*  set invoice subterm
*    set_invoice_subterm( ).

* set admission header
*    ms_admission-header-university = '157447'.
*    ms_admission-header-sissystem = 'SAP'.
*    ms_admission-header-sissystemversion = 'SAP ERP 6.0'.
*    ms_admission-header-applicationsystem = 'SAP'.
*    ms_admission-header-applicationsystemversion = 'EHP 7 on SAP NetWeaver 7.4'.
*
*
*    ms_invoice-header-university = '157447'.
*    ms_invoice-header-sissystem = 'SAP'.
*    ms_invoice-header-sissystemversion = 'SAP ERP 6.0'.
*
*    ms_invoice-term-termcode = get_term_desc( iv_year = mv_peryr iv_period =  mv_perid ).
*    ms_invoice-term-subtermcode = mv_invoice_subterm.
*
*    ms_registration-header-university = '157447'.
*    ms_registration-header-sissystem = 'SAP'.
*    ms_registration-header-sissystemversion = 'SAP ERP 6.0'..
*
*    create_interface: 'NKU-ADMISSIONS'    mr_admission.
*    create_interface: 'NKU-INVOICE'       mr_invoice.
*    create_interface: 'NKU-REGISTRATION'  mr_registration.

  ENDMETHOD.


  METHOD build_data.

    DATA: lt_students     TYPE  TABLE OF student_s.

    FIELD-SYMBOLS <fs_data>   TYPE  academic_work_s.

    get_current_students( CHANGING ct_student = lt_students ).

*    get_academic_work( ).

    merge_data( it_students = lt_students ).


  ENDMETHOD.



  METHOD get_current_students.

    DATA  ls_object   TYPE  hrobject.
    DATA  lv_objid    TYPE  objid.
    DATA  ls_curr_student  TYPE  student_s.
    DATA  lt_searchtl TYPE piqsearchtl_tab.
    DATA  lt_student  TYPE  student_t.

    FIELD-SYMBOLS   <fs_student>  TYPE  student_s.
    FIELD-SYMBOLS   <fs_event>    TYPE  event_s.

    SELECT  DISTINCT p506~adatanr,
      p506~id,
      p506~packnumber AS se_objid,
      cs~varyf AS st_otjid,
      cs~otjid AS cs_otjid,
      sc~otjid AS sc_otjid,
      sm~otjid AS sm_otjid,
      foll~cpearnedfu AS course_credit_hours,
      gen~gradesym,
      gen~grade,
      gen~gradescale,
      gen~agrid
      INTO CORRESPONDING FIELDS OF TABLE @ct_student

    "...select PAD506 for the requested year and sesion to get current BOOKINGS and EVENT PACKAGE
    FROM hrpad506 AS p506
    "...join current BOOKINGS to get STUDENT
    INNER JOIN hrp1001 AS st ON st~adatanr = p506~adatanr
    "...join current BOOKINGS to get MODULE
    INNER JOIN hrp1001 AS sm ON sm~adatanr = p506~adatanr AND sm~otype = 'SM'
    "...with STUDENT get STUDY
    INNER JOIN hrp1001 AS cs ON cs~otype = 'CS'
      AND cs~sclas = 'ST'
      AND cs~sobid = st~objid
    "...with STUDY get PROGRAM OF STUDY
    INNER JOIN hrp1001 AS sc ON sc~otype = 'SC'
      AND sc~sclas = 'CS'
      AND sc~sobid = cs~objid
    "...with MODULE BOOKING ID get grades
    LEFT OUTER JOIN ( piqdbagr_assignm AS assignm
        INNER JOIN piqdbagr_appr    AS appr     ON appr~agrid = assignm~agrid AND ( appr~agrtype = '10' OR appr~agrtype = '0' )
        INNER JOIN piqdbagr_gen     AS gen      ON gen~agrid =  assignm~agrid
        INNER JOIN piqdbagr_foll_up AS foll     ON foll~agrid = assignm~agrid ) ON assignm~modreg_id = p506~id

    WHERE peryr = @mv_peryr
      AND perid = @mv_perid
      AND st~otype = 'ST'
      AND st~plvar = '01'
      AND st~istat = '1'
      AND cs~plvar = '01'
      AND cs~istat = '1'
      AND cs~rsign = 'B'
      AND cs~relat = '517'
      AND sc~plvar = '01'
      AND sc~istat = '1'
      AND sc~rsign = 'B'
      AND sc~relat = '514'
      AND ( sc~objid = '70790088' OR sc~objid = '70000694' )
      AND sm~plvar = '01'
      AND sm~istat = '1'
      .

    " Only keep one record per student
    SORT ct_student.
    DELETE ADJACENT DUPLICATES FROM ct_student.
    LOOP AT ct_student ASSIGNING <fs_student>.
      <fs_student>-st_objid = <fs_student>-st_otjid+2.
    ENDLOOP.

    CLEAR ls_object.
    ls_object-plvar = mv_plvar.
    ls_object-otype = 'O'.

* Read main timelimit to get the begin date
    APPEND '0100' TO lt_searchtl.

    CALL FUNCTION 'HRIQ_ACAD_READ_TIMELIMITS'
      EXPORTING
        is_object                = ls_object
        it_searchtl              = lt_searchtl
      IMPORTING
        et_timelimits            = mt_timelimits
      EXCEPTIONS
        no_start_object_imported = 1
        customizing_incomplete   = 2
        invalid_ca_object        = 3
        no_eval_path             = 4
        relative_timelimit_error = 5
        no_data_found            = 6
        internal_error           = 7
        OTHERS                   = 8.


    SORT ct_student BY peryr perid sm_otjid se_objid.

    IF ct_student IS NOT INITIAL.
      lt_student = ct_student.
      DELETE ADJACENT DUPLICATES FROM lt_student COMPARING se_objid.
      "...get event dates for all EVENTS for requested STUDENTS
      SELECT a~objid a~varyf a~begda a~endda b~kapz1 b~kapz3
        FROM hrp1001 AS a
        INNER JOIN hrp1024 AS b ON b~otjid = a~varyf AND b~endda >= mv_begda AND b~begda <= mv_endda
        INTO TABLE mt_events
        FOR ALL ENTRIES IN lt_student
        WHERE a~objid = lt_student-se_objid
        AND   a~plvar = mv_plvar
        AND   a~otype = 'SE'
        AND   a~rsign = 'A'
        AND   a~relat = '512'
        AND   a~begda <= mv_endda
        AND   a~endda >= mv_begda
        AND  ( a~sclas = 'E'  OR
               a~sclas = 'EL' ).

      SORT mt_events BY se_objid.
      "...get all catergory codes for EVENTS
      LOOP AT mt_events ASSIGNING <fs_event>.
        SELECT b~category  INTO <fs_event>-category
          FROM hrp1001 AS a
        INNER JOIN hrp1731 AS b ON b~otjid = a~varyf
               AND b~endda >= <fs_event>-begda
               AND b~begda <= <fs_event>-endda
        UP TO 1 ROWS
        WHERE a~otjid = <fs_event>-otjid
          AND   a~plvar = mv_plvar
          AND   b~plvar = mv_plvar
          AND   a~subty = 'A020'
          AND   a~endda >= <fs_event>-begda
          AND   a~begda <= <fs_event>-endda.
        ENDSELECT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
*<TASK>70000694 sleect
*<TASK>output grades file
*<TASK>output students ALV


  METHOD get_academic_work.



    DATA: lv_begda_aw      TYPE begda,
          lv_endda_aw      TYPE endda,
          lt_academic_work TYPE academic_work_t.

    FIELD-SYMBOLS: <fs_curr_student> TYPE student_s.
*    FIELD-SYMBOLS: <fs_subterms> TYPE subterm_s.
*    FIELD-SYMBOLS: <fs_timelimits> TYPE piqtimelimits.
    FIELD-SYMBOLS: <fs_academic_work> TYPE academic_work_s.

    CLEAR: mt_academic_work, mt_academic_work[].
    CLEAR: mt_events, mt_events[].

**** need the begda of the first subterm and endda of the last one

*    READ TABLE mt_300_subterms ASSIGNING <fs_subterms> INDEX 1.
*
*    LOOP AT mt_timelimits ASSIGNING <fs_timelimits> WHERE ca_peryr = <fs_subterms>-peryr
*                                                    AND   ca_perid = <fs_subterms>-0100_perid
*                                                    AND  ca_timelimit = '0100'.
*      lv_begda_aw = <fs_timelimits>-ca_lbegda.
*
*    ENDLOOP.
*
*
*    LOOP AT mt_300_subterms ASSIGNING <fs_subterms>.
*    ENDLOOP.
*
*    LOOP AT mt_timelimits ASSIGNING <fs_timelimits> WHERE ca_peryr = <fs_subterms>-peryr
*                                                   AND  ca_perid = <fs_subterms>-0100_perid
*                                                   AND  ca_timelimit = '0100'.
*
*      lv_endda_aw = <fs_timelimits>-ca_lendda.
*
*    ENDLOOP.

**** select courses

    LOOP AT mt_curr_students ASSIGNING <fs_curr_student>.

      SELECT
                   p1001~sclas,
                   p1001~sobid,
                   p1001~objid,
                   cmac~student12,
                   pad506~id,
                   pad506~packnumber,
                   pad506~peryr,
                   pad506~perid,
                   pad506~smstatus,
                   pad506~smrating,
                   pad506~storreason,
                   pad506~stordate,
                   pad506~cpattemp,
                   foll~cpearnedfu,
                   foll~cpgradedfu,
                   foll~cpunitfu,
                   pad506~bookdate,
                   pad506~bookreason,
                   pad506~annulment,
                   gen~gradesym,
                   gen~grade,
                   gen~gradescale,
                   gen~agrid,
                   gen~agrdate,
                   gen~agrcompleted,
                   appr~agrtype,
                   appr~agrstat,
                   gen~agrnotrated,
                   p1001~begda,
                   p1001~endda,
                   sm~short

             FROM  hrp1001 AS p1001
                     INNER JOIN hrpad506 AS pad506 ON pad506~adatanr = p1001~adatanr
                     INNER JOIN cmacbpst AS cmac ON cmac~stobjid = p1001~objid
                     INNER JOIN hrp1000 AS sm ON sm~otype = p1001~sclas AND sm~objid = p1001~sobid
             LEFT OUTER JOIN ( piqdbagr_assignm AS assignm
                               INNER JOIN piqdbagr_appr    AS appr     ON appr~agrid = assignm~agrid AND ( appr~agrtype = '10' OR appr~agrtype = '0' )
                               INNER JOIN piqdbagr_gen     AS gen      ON gen~agrid =  assignm~agrid
                               INNER JOIN piqdbagr_foll_up AS foll     ON foll~agrid = assignm~agrid ) ON assignm~modreg_id = pad506~id

             APPENDING TABLE @mt_academic_work
             WHERE p1001~otjid = @<fs_curr_student>-st_otjid
               AND p1001~plvar = @mv_plvar
               AND p1001~rsign = 'A'
               AND p1001~relat = '506'
               AND pad506~transferflag NE 'X'
               AND p1001~begda >= '20160701'
               AND p1001~begda <= '20161207'.

    ENDLOOP.


    SORT mt_academic_work BY peryr perid aw_objid se_objid stordate.

    lt_academic_work = mt_academic_work.
    DELETE ADJACENT DUPLICATES FROM lt_academic_work COMPARING se_objid.

    IF lt_academic_work IS NOT INITIAL.

      SELECT a~objid a~varyf a~begda a~endda b~kapz1 b~kapz3
        FROM hrp1001 AS a
        INNER JOIN hrp1024 AS b ON b~otjid = a~varyf AND b~endda >= lv_begda_aw AND b~begda <= lv_endda_aw
        INTO TABLE mt_events
        FOR ALL ENTRIES IN lt_academic_work
        WHERE a~objid = lt_academic_work-se_objid
        AND   a~plvar = mv_plvar
        AND   a~otype = 'SE'
        AND   a~rsign = 'A'
        AND   a~relat = '512'
        AND   a~begda <= lv_endda_aw
        AND   a~endda >= lv_begda_aw
        AND  ( a~sclas = 'E'  OR
               a~sclas = 'EL' ).

    ENDIF.

  ENDMETHOD.



  METHOD merge_data.

    DATA  ls_data   TYPE  data_s.
    DATA  lv_short TYPE   hrp1000-short.
    FIELD-SYMBOLS   <fs_student>  TYPE  student_s.
    FIELD-SYMBOLS   <fs_event>    TYPE  event_s.

    LOOP AT it_students ASSIGNING <fs_student>.
      CLEAR ls_data.

      get_personal(
        EXPORTING iv_stuobjid = <fs_student>-st_otjid
        CHANGING cs_data = ls_data ).

      SELECT SINGLE short FROM hrp1000
        INTO lv_short
        WHERE otype = 'SM'
          AND objid = <fs_student>-sm_otjid+2
        .
*      ls_data-persidno
*      ls_data-nachn
*      ls_data-vorna
*      ls_data-mid
*      ls_data-gbdat
      ls_data-ayear = mv_peryr.
      IF mv_perid = '010'.
        ls_data-semester =  'F'.
      ELSEIF mv_perid EQ '030'.
        ls_data-semester = 'S'.
      ELSE.
        ls_data-semester = '?'.
      ENDIF.
      ls_data-course_abev = lv_short(4).
      ls_data-course_numb   = lv_short+4(4).
      ls_data-course_credit_hours = <fs_student>-course_credit_hours.
      ls_data-cost_per_hour = 333.
      "ls_data-yearlong_course
      LOOP AT mt_events ASSIGNING <fs_event>
        WHERE se_objid = <fs_student>-se_objid.
      ENDLOOP.
      IF sy-subrc EQ 0 AND <fs_event>-category EQ 'LAB'.
        ls_data-lab_i =   'Y'.
        ls_data-lab_course_abev   = lv_short(4).
        ls_data-lab_course_numb   = lv_short+4(4).
        ls_data-lab_credit_hours  = <fs_student>-course_credit_hours.
      ELSE.
        ls_data-lab_i =   'N'.
      ENDIF.
      ls_data-grade = <fs_student>-gradesym.
      APPEND ls_data TO mt_data.
    ENDLOOP.
  ENDMETHOD.



  METHOD get_personal.

    DATA  lv_objid                TYPE  hrobjid.
    DATA lv_studentnumber          TYPE bapistudent_head-studentnumber.
    DATA lv_studentpersonaldata    TYPE bapistudent_personal.
    DATA lv_studentpersonaldatat   TYPE bapistudent_personalt.

    DATA lv_studentadditionaldatat TYPE bapistudent_addpersonalt.
    DATA lv_studentresidencydata   TYPE bapistudent_residency.

    lv_objid = iv_stuobjid.
    CALL FUNCTION 'BAPI_STUDENT_GETDETAIL3'
      EXPORTING
        objectid               = lv_objid
        planversion            = mv_plvar
        keydate                = sy-datum
        read_texts             = 'X'
      IMPORTING
        studentnumber          = lv_studentnumber
        studentpersonaldata    = lv_studentpersonaldata
        studentpersonaldatat   = lv_studentpersonaldatat
        studentadditionaldatat = lv_studentadditionaldatat
        studentresidencydata   = lv_studentresidencydata.

*            ms_curr_person-studentid = lv_studentnumber.
*            ms_curr_person-legacysystemid = ''.
    cs_data-persidno = lv_studentpersonaldata-persidno.
    cs_data-vorna = lv_studentpersonaldata-first_name.
    cs_data-mid = lv_studentpersonaldata-middle_name.
    cs_data-nachn = lv_studentpersonaldata-last_name.
    cs_data-gbdat = lv_studentpersonaldata-date_birth.


  ENDMETHOD.



  METHOD outbound_data.

    TYPES:
      BEGIN OF enroll_s
        ,line(125)    TYPE    c   "record
      ,END OF enroll_s.

    TYPES:
      BEGIN OF enroll_hdr_s
        ,htext(9)       TYPE    c   "header record indicator
        ,hschool(8)     TYPE    n   "client number
        ,hfil_01(108)   TYPE    c   "filler
      ,END OF enroll_hdr_s.

    TYPES:
      BEGIN OF enroll_dtl_s
        ,persidno(9)            TYPE    n   "student's ssn
        ,vorna(35)              TYPE    c   "last name
        ,chn(12)                TYPE    c   "first name
        ,mid(1)                 TYPE    c   "middle initial
        ,gbdat                  TYPE    piq_gbdat    "dob
        ,dschool(8)             TYPE    n   "school code
        ,ayear                TYPE    dats    "academic year begin
        ,semester(1)            TYPE    c   "semester F=fall S=spring
        ,course_abev(4)         TYPE    c   "course abbreviation.
        ,course_numb(4)         TYPE    c   "course number
        ,course_credit_hours(2) TYPE    n   "course credit hours
        ,cost_per_hour(3)       TYPE    n   "cost per credit hour
        ,yearlong_course(1)     TYPE    c   "year long course space=no Y=yes
        ,lab(1)                 TYPE    c   "lab indicator Y/N
        ,lab_course_abev(4)     TYPE    c   "lab course abbreviation
        ,lab_course_numb(4)     TYPE    c   "lab course number
        ,lab_credit_hours(2)    TYPE    n   "lab credit_hours
        ,dfil_01(18)            TYPE    c   "filler
      ,END OF enroll_dtl_s.

    TYPES:
      BEGIN OF enroll_ftr_s
        ,ftext(9)       TYPE    c   "footer record indicator
        ,fschool(8)     TYPE    n   "client number
        ,rec_cnt(6)     TYPE    n   "record count
        ,ffil_01(102)   TYPE    c   "filler
      ,END OF enroll_ftr_s.

    DATA    ls_enroll_hdr     TYPE    enroll_hdr_s.
    DATA    ls_enroll_dtl     TYPE    enroll_dtl_s.
    DATA    ls_enroll_ftr     TYPE    enroll_ftr_s.
    DATA    ls_enroll         TYPE    enroll_s.
    DATA    lt_enroll         TYPE    STANDARD TABLE OF enroll_s WITH DEFAULT KEY.

    DATA: lv_guifile    TYPE string,
          lv_serverfile TYPE localfile.
    DATA: lv_file TYPE string,
          lv_len  TYPE i VALUE '125'.

    FIELD-SYMBOLS <fs_data>  TYPE  data_s.
    FIELD-SYMBOLS <fs_line>   TYPE  enroll_s.


    CLEAR ls_enroll_hdr.
    ls_enroll_hdr-htext = 'DCSENRH01'.
    ls_enroll_hdr-hschool = c_school.
    APPEND ls_enroll_hdr TO lt_enroll.

    LOOP AT mt_data ASSIGNING <fs_data>.

      CLEAR ls_enroll_dtl.
      ls_enroll_dtl-persidno = <fs_data>-persidno.
      ls_enroll_dtl-vorna = <fs_data>-vorna.
      ls_enroll_dtl-chn = <fs_data>-nachn.
      ls_enroll_dtl-mid = <fs_data>-mid.
      ls_enroll_dtl-gbdat = <fs_data>-gbdat.
      ls_enroll_dtl-dschool = c_school.
      ls_enroll_dtl-ayear = <fs_data>-ayear.
      ls_enroll_dtl-semester = <fs_data>-semester.
      ls_enroll_dtl-course_abev = <fs_data>-course_abev.
      ls_enroll_dtl-course_numb = <fs_data>-course_numb.
      ls_enroll_dtl-course_credit_hours   = <fs_data>-course_credit_hours.
      ls_enroll_dtl-yearlong_course = 'Y'.
      ls_enroll_dtl-lab = <fs_data>-lab_i.
      ls_enroll_dtl-lab_course_abev = <fs_data>-lab_course_abev.
      ls_enroll_dtl-lab_course_numb = <fs_data>-lab_course_numb.
      ls_enroll_dtl-lab_credit_hours = <fs_data>-lab_credit_hours.
      APPEND ls_enroll_dtl TO lt_enroll.
    ENDLOOP.

    CLEAR   ls_enroll_ftr.
    ls_enroll_ftr-ftext = 'DCSENRT01'.
    ls_enroll_ftr-fschool = c_school.
    ls_enroll_ftr-rec_cnt = 5.
    APPEND ls_enroll_ftr TO lt_enroll.

    lv_file = 'DUALCREDIT_ENROLL'.
    IF mv_local EQ true.
      CONCATENATE mv_fpath_desk lv_file '.txt' INTO lv_guifile.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                  = lv_guifile
          trunc_trailing_blanks_eol = ''
        CHANGING
          data_tab                  = lt_enroll   "gt_output
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          not_supported_by_gui      = 22
          error_no_gui              = 23
          OTHERS                    = 24.
      IF sy-subrc <> 0.      ENDIF.

    ELSE.
      CONCATENATE mv_fpath_serv lv_file '.txt' INTO lv_serverfile.
      "...write file to server
      OPEN DATASET lv_serverfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc <> 0.
        MESSAGE e007(zsys) WITH lv_serverfile.
      ENDIF.
      LOOP AT lt_enroll ASSIGNING <fs_line>.
        TRANSFER <fs_line>  TO lv_serverfile LENGTH lv_len.
      ENDLOOP.
      CLOSE DATASET lv_serverfile.
    ENDIF.

  ENDMETHOD.                    "save_file



  METHOD outbound_data_eot.


    TYPES:
      BEGIN OF endofterm_s
        ,line(125)       TYPE    c   "record
      ,END OF endofterm_s.

    TYPES:
      BEGIN OF endofterm_hdr_s
        ,htext(9)       TYPE    c   "header record indicator
        ,hschool(8)     TYPE    n   "client number
        ,hfil_01(83)    TYPE    c   "filler
      ,END OF endofterm_hdr_s.

    TYPES:
      BEGIN OF endofterm_dtl_s
        ,persidno(9)            TYPE    n   "student's ssn
        ,vorna(35)              TYPE    c   "last name
        ,chn(12)                TYPE    c   "first name
        ,mid(1)                 TYPE    c   "middle initial
        ,gbdat                 TYPE    piq_gbdat    "dob
        ,dschool(8)             TYPE    n   "school code
        ,ayear                  TYPE    dats    "academic year begin
        ,semester(1)            TYPE    c   "semester F=fall S=spring
        ,course_abev(4)         TYPE    c   "course abbreviation.
        ,course_numb(4)         TYPE    c   "course number
        ,grade(2)               TYPE    c   "letter grade
        ,dfil_01(8)             TYPE    c   "filler
      ,END OF endofterm_dtl_s.

    TYPES:
      BEGIN OF endofterm_ftr_s
        ,ftext(9)       TYPE    c   "footer record indicator
        ,fschool(8)     TYPE    n   "client number
        ,rec_cnt(6)     TYPE    n   "record count
        ,ffil_01(77)   TYPE    c   "filler
      ,END OF endofterm_ftr_s.


    DATA    ls_endofterm_hdr     TYPE    endofterm_hdr_s.
    DATA    ls_endofterm_dtl     TYPE    endofterm_dtl_s.
    DATA    ls_endofterm_ftr     TYPE    endofterm_ftr_s.
    DATA    ls_endofterm         TYPE    endofterm_s.
    DATA    lt_endofterm         TYPE    STANDARD TABLE OF endofterm_s WITH DEFAULT KEY.

    DATA: lv_guifile    TYPE string,
          lv_serverfile TYPE localfile.
    DATA: lv_file TYPE string,
          lv_len  TYPE i VALUE '100'.

    FIELD-SYMBOLS <fs_data>  TYPE  data_s.
    FIELD-SYMBOLS <fs_line>  TYPE  endofterm_s.


    CLEAR ls_endofterm_hdr.
    ls_endofterm_hdr-htext = 'DCSG02H01'.
    ls_endofterm_hdr-hschool = c_school.
    APPEND ls_endofterm_hdr TO lt_endofterm.

    LOOP AT mt_data ASSIGNING <fs_data>.

      CLEAR ls_endofterm_dtl.
      ls_endofterm_dtl-persidno = <fs_data>-persidno.
      ls_endofterm_dtl-vorna = <fs_data>-vorna.
      ls_endofterm_dtl-chn = <fs_data>-nachn.
      ls_endofterm_dtl-mid = <fs_data>-mid.
      ls_endofterm_dtl-gbdat = <fs_data>-gbdat.
      ls_endofterm_dtl-dschool = c_school.
      ls_endofterm_dtl-ayear = <fs_data>-ayear.
      ls_endofterm_dtl-semester = <fs_data>-semester.
      ls_endofterm_dtl-course_abev = <fs_data>-course_abev.
      ls_endofterm_dtl-course_numb = <fs_data>-course_numb.
      ls_endofterm_dtl-grade   = <fs_data>-grade.
      APPEND ls_endofterm_dtl TO lt_endofterm.
    ENDLOOP.
    APPEND ls_endofterm_dtl TO lt_endofterm.

    CLEAR   ls_endofterm_ftr.
    ls_endofterm_ftr-ftext = 'DCSG02T01'.
    ls_endofterm_ftr-fschool = c_school.
*<TASK>couint recrods
    ls_endofterm_ftr-rec_cnt = 5.
    APPEND ls_endofterm_ftr TO lt_endofterm.

    lv_file = 'DUALCREDIT_ENDOFTERM'.
    IF mv_local EQ true.
      CONCATENATE mv_fpath_desk lv_file '.txt' INTO lv_guifile.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                  = lv_guifile
          trunc_trailing_blanks_eol = ''
        CHANGING
          data_tab                  = lt_endofterm
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          not_supported_by_gui      = 22
          error_no_gui              = 23
          OTHERS                    = 24.
      IF sy-subrc <> 0.      ENDIF.

    ELSE.
      CONCATENATE mv_fpath_serv lv_file '.txt' INTO lv_serverfile.
      "...write file to server
      OPEN DATASET lv_serverfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc <> 0.
        MESSAGE e007(zsys) WITH lv_serverfile.
      ENDIF.
      LOOP AT lt_endofterm ASSIGNING <fs_line>.
        TRANSFER <fs_line>  TO lv_serverfile LENGTH lv_len.
      ENDLOOP.
      CLOSE DATASET lv_serverfile.
    ENDIF.

  ENDMETHOD.                    "save_file

ENDCLASS.