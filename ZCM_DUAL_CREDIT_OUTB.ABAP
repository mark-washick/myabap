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

TYPES:  modgrp_rt TYPE RANGE OF hrobjid.

TYPES:  BEGIN OF objid_s
          ,varyf    TYPE  varyf
        ,END OF objid_s.
TYPES objid_t TYPE   STANDARD TABLE OF objid_s WITH EMPTY KEY.


TYPES:
  BEGIN OF data_s
    ,student12              TYPE  piqstudent12
    ,persidno(9)            TYPE  n           "student's ssn
    ,vorna                  TYPE  pad_vorna   "last name
    ,nachn                  TYPE  pad_nachn   "first name
    ,mid(1)                 TYPE  c           "middle initial
    ,gbdat                  TYPE  piq_gbdat   "dob
    ,ayear                  TYPE  dats        "academic year begin
    ,semester(1)            TYPE  c           "semester F=fall S=spring
    ,course_abev(4)         TYPE  c           "course abbreviation.
    ,course_numb(4)         TYPE  c           "course number
    ,course_credit_hours(2) TYPE  n           "course credit hours
    ,cost_per_hour(3)       TYPE  n           "cost per credit hour
    ,yearlong_course(1)     TYPE  c           "year long course space=no Y=yes
    ,lab_i(1)               TYPE  c           "lab indicator Y/N
    ,lab_course_abev(4)     TYPE  c           "lab course abbreviation
    ,lab_course_numb(4)     TYPE  c           "lab course number
    ,lab_credit_hours(2)    TYPE  n           "lab credit_hours
    ,grade                  TYPE  piqgradesym "grade
    ,se_objid               TYPE  hrobjid
    ,evtcatg                TYPE  piqcategory
    ,other_modgrps          TYPE  c            "other module groups present
    ,END OF data_s.
TYPES   data_t  TYPE  STANDARD TABLE OF data_s WITH DEFAULT KEY.

TYPES: BEGIN OF otjid_s,
         otjid TYPE otjid,
       END OF   otjid_s.

TYPES: otjid_t TYPE TABLE OF otjid_s WITH NON-UNIQUE DEFAULT KEY.

TYPES:  BEGIN OF student_s
          ,student12  TYPE  piqstudent12
          ,adatanr    TYPE  hradatanr
          ,id         TYPE  piqmodreg_id
          ,se_objid   TYPE  hrobjid
          ,st_otjid   TYPE  varyf
          ,st_objid   TYPE  hrobjid
          ,cs_otjid   TYPE  otjid
          ,sc_otjid   TYPE  otjid
          ,sm_otjid   TYPE  otjid
          ,cg_otjid   TYPE  otjid
          ,peryr      TYPE piqperyr
          ,perid      TYPE piqperid
          ,course_credit_hours TYPE  piqcpattemp "course credit hours
          ,gradesym   TYPE  piqgradesym
          ,grade      TYPE  piqscale_norm_val
          ,gradescale TYPE  piqscale_id
          ,agrid      TYPE  piqagrid
    ,other_modgrps          TYPE  c            "other module groups present
       ,END OF   student_s.

TYPES:  student_t   TYPE  STANDARD TABLE OF student_s WITH DEFAULT KEY.

TYPES: student_id_t TYPE TABLE OF student_s.

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
        iv_cpchr             TYPE  n
        it_selected_students TYPE  modgrp_rt
      .
    METHODS build_data.
    METHODS outbound_data.
    METHODS outbound_data_eot.
    METHODS get_personal
      IMPORTING iv_stuobjid TYPE  otjid
      CHANGING  cs_data     TYPE data_s.
    METHODS build_alv.

    DATA: mv_file   TYPE   c,
          mv_nofile TYPE   c.


  PRIVATE SECTION.

    METHODS get_current_students CHANGING ct_student TYPE student_t.

    METHODS merge_data IMPORTING it_students TYPE student_t.

    METHODS collapse_labs.

    DATA: mv_enroll        TYPE   c,
          mv_eoterm        TYPE   c,
          mv_local         TYPE    c,
          mv_server        TYPE   c,
          mv_fpath_desk    TYPE   localfile,
          mv_fpath_serv    TYPE   localfile,
          mv_select_dt     TYPE   datum,
          mv_acayear       TYPE   num4,
          mv_session       TYPE   num2,
          mv_peryr         TYPE   piqperyr,
          mv_perid         TYPE   piqperid,
          mv_creditcost(3) TYPE  n,
          mv_begda         TYPE   begda,
          mv_endda         TYPE   endda,
          mv_year_outb(8)  TYPE   n,
          mv_aidyr1        TYPE   string,
          mv_aidyr2        TYPE   string,
          mv_plvar         TYPE   plvar,
          mt_timelimits    TYPE   piqtimelimits_tab,
          mt_curr_students TYPE   student_id_t,
          mt_modgrps       TYPE   modgrp_rt,
          mt_events        TYPE   event_t,
          mt_data          TYPE   STANDARD TABLE OF data_s WITH DEFAULT KEY
          .


ENDCLASS.


CLASS lcl_alv DEFINITION.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF data_alv_s
        ,recordno               TYPE  i           "recrod number
        ,persidno(9)            TYPE  n           "student's ssn
        ,vorna                  TYPE  pad_vorna   "last name
        ,nachn                  TYPE  pad_nachn   "first name
        ,ayear(4)               TYPE  c        "academic year begin
        ,semester(2)            TYPE  c           "semester F=fall S=spring
        ,course_abev(4)         TYPE  c           "course abbreviation.
        ,course_numb(4)         TYPE  c           "course number
        ,course_credit_hours(2) TYPE  n           "course credit hours
        ,lab_i(1)               TYPE  c           "lab indicator Y/N
        ,lab_course_abev(4)     TYPE  c           "lab course abbreviation
        ,lab_course_numb(4)     TYPE  c           "lab course number
        ,lab_credit_hours(2)    TYPE  n           "lab credit_hours
        ,grade(2)               TYPE  c           "grade
        ,se_objid               TYPE  hrobjid    "se objectid
        ,evtcatg                TYPE  piqcategory "event category
        ,other_modgrps(16)      TYPE  c           "other module groups present
      ,END OF data_alv_s.

    TYPES  data_alv_t  TYPE STANDARD TABLE OF data_alv_s WITH DEFAULT KEY.

    METHODS constructor IMPORTING it_data TYPE data_t.

    METHODS display.

  PROTECTED SECTION.
    DATA: lo_table   TYPE REF TO cl_salv_table,
          lo_columns TYPE REF TO cl_salv_columns_table.
    METHODS set_alv_columns IMPORTING colhdr TYPE data_alv_s.
    METHODS set_alv_footer.
    DATA    mt_alv  TYPE    data_alv_t.
ENDCLASS.                    "lcl_alv DEFINITION




CLASS lcl_local DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS update_year.

    CLASS-METHODS update_perid.

ENDCLASS.                    "lcl_adm_update_xml DEFINITION
TABLES  hrp1001.
TABLES  cmacbpst.

DATA  lo_interface        TYPE REF TO lcl_interface.
***DATA  gt_student_objects  TYPE hrobjects_t.
DATA  pathlen TYPE i.
DATA lt_modgrp  TYPE  modgrp_rt.
DATA ls_modgrp  LIKE LINE OF lt_modgrp.

SELECTION-SCREEN BEGIN OF BLOCK input WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_modgrp FOR hrp1001-objid.
PARAMETER: p_fyr   TYPE piqperyr AS LISTBOX VISIBLE LENGTH 40 MODIF ID gr1.
PARAMETER: p_perid TYPE piqperid AS LISTBOX VISIBLE LENGTH 40 MODIF ID gr1.
PARAMETERS  p_cpchr TYPE  n LENGTH 3.
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

AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
* AT SELECTION SCREEN OUTPUT                                           *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  CALL METHOD lcl_local=>update_year.
  CALL METHOD lcl_local=>update_perid.

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
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.
*  p_path1 = zxx_cl_outbound_interface=>desktop_folder_dialog( ).

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

  LOOP AT p_modgrp.
    ls_modgrp-sign = 'I'.
    ls_modgrp-option = 'EQ'.
    ls_modgrp-low = p_modgrp-low.
    APPEND ls_modgrp TO lt_modgrp.
  ENDLOOP.

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
      iv_cpchr             = p_cpchr
      it_selected_students = lt_modgrp.

  CALL METHOD lo_interface->build_data( ).


END-OF-SELECTION.

  IF lo_interface->mv_file EQ 'X'.
    IF p_enroll EQ true.
      lo_interface->outbound_data( ).
    ELSEIF p_eoterm EQ true.
      lo_interface->outbound_data_eot( ).
    ENDIF.
  ENDIF.

  lo_interface->build_alv( ).



CLASS lcl_interface IMPLEMENTATION.
  METHOD constructor.

    DATA: ls_object   TYPE hrobject,
          lt_searchtl TYPE piqsearchtl_tab.

    FIELD-SYMBOLS <fs_ss> TYPE  selected_s.


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
    mv_creditcost = iv_cpchr.
    mt_modgrps = it_selected_students.

    mv_year_outb(4) = mv_peryr - 1.
    mv_year_outb+4(4) = mv_peryr.


    "...get first date of session
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

  ENDMETHOD.



  METHOD build_data.

    DATA: lt_students     TYPE  TABLE OF student_s.

    get_current_students( CHANGING ct_student = lt_students ).

    merge_data( it_students = lt_students ).

    collapse_labs( ).

  ENDMETHOD.



  METHOD get_current_students.

    DATA  ls_object   TYPE  hrobject.
    DATA  lv_objid    TYPE  objid.
    DATA  ls_curr_student  TYPE  student_s.
    DATA  lt_searchtl TYPE piqsearchtl_tab.
    DATA  lt_student  TYPE  student_t.
    DATA  lv_single   TYPE  piqstudent12 VALUE space.

*    ranges: lt_sel_student for hrobjid.
    DATA  lt_sel_student TYPE RANGE OF hrobjid.
    DATA  ls_sel_student LIKE LINE OF lt_sel_student.
    DATA  lv_last_stuobid TYPE  hrobjid.
    DATA  lt_other_modgrps  TYPE  objid_t.


    FIELD-SYMBOLS   <fs_student>  TYPE  student_s.
    FIELD-SYMBOLS   <fs_event>    TYPE  event_s.
    FIELD-SYMBOLS   <fs_ss>       TYPE  hrobject.


    SELECT  DISTINCT
          cmac~student12,
          p506~adatanr,
          p506~id,
          p506~packnumber AS se_objid,
          cs~varyf AS st_otjid,
          cs~otjid AS cs_otjid,
          sc~otjid AS sc_otjid,
          sm~otjid AS sm_otjid,
          cg~otjid AS cg_otjid,
          p506~cpattemp AS course_credit_hours,
          gen~gradesym,
          gen~grade,
          gen~gradescale,
          gen~agrid
    INTO CORRESPONDING FIELDS OF TABLE @ct_student

    "...select PAD506 for the requested year and sesion to get current BOOKINGS and EVENT PACKAGE
    FROM hrpad506 AS p506
    "...join current BOOKINGS to get STUDENT
    INNER JOIN hrp1001 AS st ON st~adatanr = p506~adatanr

    INNER JOIN cmacbpst AS cmac ON cmac~stobjid = st~objid
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
    "...with STUDENT STUDY get MODULE GROUP
    INNER JOIN hrp1001 AS cg ON cg~otype = 'CG'
      AND cg~sclas = 'CS'
      AND cg~sobid = cs~objid
    "...with MODULE BOOKING ID get grades
    LEFT OUTER JOIN ( piqdbagr_assignm AS assignm
      INNER JOIN piqdbagr_appr    AS appr     ON appr~agrid = assignm~agrid AND ( appr~agrtype = '10' OR appr~agrtype = '0' )
      INNER JOIN piqdbagr_gen     AS gen      ON gen~agrid =  assignm~agrid
      INNER JOIN piqdbagr_foll_up AS foll     ON foll~agrid = assignm~agrid ) ON assignm~modreg_id = p506~id
    FOR ALL ENTRIES IN @mt_modgrps
    WHERE cg~objid = @mt_modgrps-low
                                      "and cmac~student12 = '100247832'
    AND p506~peryr = @mv_peryr
    AND p506~perid = @mv_perid
    AND p506~transferflag = ' '
    AND ( p506~smstatus = '1' OR p506~smstatus = '2' OR p506~smstatus = '3' )
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
    AND cg~plvar = '01'
    AND cg~istat = '1'
    AND cg~rsign = 'B'
    AND cg~relat = '516'
    AND cg~begda LE @mv_endda
    AND cg~endda GE @mv_begda
    AND sm~plvar = '01'
    AND sm~istat = '1'
    .

    " Only keep one record per student
    SORT ct_student.
    DELETE ADJACENT DUPLICATES FROM ct_student.

    IF lv_single IS NOT INITIAL.
      DELETE ct_student WHERE student12 NE lv_single.
    ENDIF.

    DATA lv_flag TYPE c.
    LOOP AT ct_student ASSIGNING <fs_student>.
      <fs_student>-st_objid = <fs_student>-st_otjid+2.
      IF <fs_student>-st_objid NE lv_last_stuobid.
        CLEAR   lt_other_modgrps.
        SELECT cs~varyf AS varyf
        INTO CORRESPONDING FIELDS OF TABLE @lt_other_modgrps
        FROM hrp1001 AS st
        INNER JOIN hrp1001 AS cs ON cs~otjid = st~varyf
        WHERE st~otype = 'ST'
          AND st~objid = @<fs_student>-st_objid
          AND st~rsign = 'A'
          AND st~relat = '517'
          AND cs~rsign = 'A'
          AND cs~relat = '516'
         AND cs~begda LE @mv_endda
         AND cs~endda GE @mv_begda.
        IF lines( lt_other_modgrps ) GT 1.
          lv_flag = 'X'.
        ELSE.
          CLEAR lv_flag.
        ENDIF.
      ENDIF.
      lv_last_stuobid = <fs_student>-st_objid.
      <fs_student>-other_modgrps = lv_flag.

    ENDLOOP.

    CLEAR ls_object.
    ls_object-plvar = mv_plvar.
    ls_object-otype = 'O'.

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



  METHOD merge_data.

    DATA  ls_data   TYPE  data_s.
    DATA  lv_short TYPE   hrp1000-short.
    DATA  lv_count  TYPE  i.
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
          AND begda LE mv_endda
          AND endda GE mv_begda.

      ls_data-student12 = <fs_student>-student12.
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
      lv_count = 0.
      UNASSIGN <fs_event>.
      LOOP AT mt_events ASSIGNING <fs_event>
        WHERE se_objid = <fs_student>-se_objid.
        lv_count = lv_count + 1.
      ENDLOOP.
      IF lv_count EQ 1 AND sy-subrc EQ 0 AND
        ( <fs_event>-category EQ 'LAB' OR <fs_event>-category EQ 'LLB' ).
        ls_data-lab_i =   'Y'.
        ls_data-lab_course_abev   = lv_short(4).
        ls_data-lab_course_numb   = lv_short+4(4).
        ls_data-lab_credit_hours  = <fs_student>-course_credit_hours.

      ELSE.
        ls_data-lab_i =   'N'.
      ENDIF.
      IF <fs_event> IS ASSIGNED.
        ls_data-evtcatg = <fs_event>-category.
      ENDIF.

      ls_data-grade = <fs_student>-gradesym.
      ls_data-se_objid = <fs_student>-se_objid.
      ls_data-other_modgrps = <fs_student>-other_modgrps.
      APPEND ls_data TO mt_data.

    ENDLOOP.

    SORT mt_data BY vorna nachn student12 course_abev course_numb.

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
    cs_data-nachn = lv_studentpersonaldata-first_name.
    cs_data-mid = lv_studentpersonaldata-middle_name.
    cs_data-vorna = lv_studentpersonaldata-last_name.
    cs_data-gbdat = lv_studentpersonaldata-date_birth.


  ENDMETHOD.



*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
*& this method will collapse the lab onto the course row               *
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
  METHOD collapse_labs.

    DATA  lt_collapsed   TYPE  data_t.

    FIELD-SYMBOLS <data>        TYPE  data_s.
    FIELD-SYMBOLS <collapsed>   TYPE  data_s.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

    LOOP AT mt_data ASSIGNING <data>.

      IF <data>-lab_i NE 'Y'.
        APPEND <data> TO lt_collapsed ASSIGNING <collapsed>.
        CONTINUE.
      ENDIF.



      IF <collapsed> IS ASSIGNED.

        IF <data>-course_numb+3(1) = 'L' AND
           <collapsed>-course_abev EQ <data>-course_abev AND
           <collapsed>-course_numb(3) EQ <data>-course_numb(3).

          <collapsed>-lab_i = <data>-lab_i.

          IF <data>-lab_credit_hours IS INITIAL.

            CLEAR <collapsed>-lab_course_abev.
            CLEAR <collapsed>-lab_course_numb.
            CLEAR <collapsed>-lab_credit_hours.

          ELSE.

            <collapsed>-lab_course_abev = <data>-lab_course_abev.
            <collapsed>-lab_course_numb = <data>-lab_course_numb.
            <collapsed>-lab_credit_hours = <data>-lab_credit_hours.

          ENDIF.

          "...don't write lab line
          CONTINUE.

        ENDIF.
      ENDIF.

      "...are course and lab the same?
      "...if true collapse to one line with no lab info
      IF <data>-course_abev EQ  <data>-lab_course_abev AND
         <data>-course_numb EQ <data>-lab_course_numb AND
         <data>-lab_credit_hours EQ <data>-lab_credit_hours.
        CLEAR <data>-lab_course_abev.
        CLEAR <data>-lab_course_numb.
        CLEAR <data>-lab_credit_hours.
        <data>-lab_i = 'N'.
        APPEND <data> TO lt_collapsed ASSIGNING <collapsed>.
        CONTINUE.
      ENDIF.

      APPEND <data> TO lt_collapsed ASSIGNING <collapsed>.
      CONTINUE.

    ENDLOOP.

    mt_data = lt_collapsed.

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
    ,ayear                  TYPE    dats  "academic year begin
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
      IF sy-sysid(3) = 'ERD'.
        ls_enroll_dtl-persidno = <fs_data>-student12.
      ELSE.
        ls_enroll_dtl-persidno = <fs_data>-persidno.
      ENDIF.
      ls_enroll_dtl-vorna = <fs_data>-vorna.
      ls_enroll_dtl-chn = <fs_data>-nachn.
      ls_enroll_dtl-mid = <fs_data>-mid.
      ls_enroll_dtl-gbdat = <fs_data>-gbdat.
      ls_enroll_dtl-dschool = c_school.
      ls_enroll_dtl-ayear = mv_year_outb.
      ls_enroll_dtl-semester = <fs_data>-semester.
      ls_enroll_dtl-course_abev = <fs_data>-course_abev.
      ls_enroll_dtl-course_numb = <fs_data>-course_numb.
      ls_enroll_dtl-course_credit_hours   = <fs_data>-course_credit_hours.
      ls_enroll_dtl-cost_per_hour   = mv_creditcost.
      CLEAR ls_enroll_dtl-yearlong_course.
      ls_enroll_dtl-lab = <fs_data>-lab_i.
      ls_enroll_dtl-lab_course_abev = <fs_data>-lab_course_abev.
      ls_enroll_dtl-lab_course_numb = <fs_data>-lab_course_numb.
      ls_enroll_dtl-lab_credit_hours = <fs_data>-lab_credit_hours.
      IF <fs_data>-other_modgrps EQ 'X'.
        ls_enroll_dtl-dfil_01 = 'CHECK'.
      ENDIF.
      APPEND ls_enroll_dtl TO lt_enroll.
    ENDLOOP.

    CLEAR   ls_enroll_ftr.
    ls_enroll_ftr-ftext = 'DCSENRT01'.
    ls_enroll_ftr-fschool = c_school.
    ls_enroll_ftr-rec_cnt = lines( lt_enroll ) - 1.   "don't count header & trailer
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
    ,line(100)       TYPE    c   "record
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
    ,ffil_01(77)    TYPE    c   "filler
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
      IF sy-sysid(3) = 'ERD'.
        ls_endofterm_dtl-persidno = <fs_data>-student12.
      ELSE.
        ls_endofterm_dtl-persidno = <fs_data>-persidno.
      ENDIF.
      ls_endofterm_dtl-vorna = <fs_data>-vorna.
      ls_endofterm_dtl-chn = <fs_data>-nachn.
      ls_endofterm_dtl-mid = <fs_data>-mid.
      ls_endofterm_dtl-gbdat = <fs_data>-gbdat.
      ls_endofterm_dtl-dschool = c_school.
      ls_endofterm_dtl-ayear = mv_year_outb.
      ls_endofterm_dtl-semester = <fs_data>-semester.
      ls_endofterm_dtl-course_abev = <fs_data>-course_abev.
      ls_endofterm_dtl-course_numb = <fs_data>-course_numb.
      ls_endofterm_dtl-grade   = <fs_data>-grade.
      IF <fs_data>-other_modgrps EQ 'X'.
        ls_endofterm_dtl-dfil_01 = 'CHECK'.
      ENDIF.
      APPEND ls_endofterm_dtl TO lt_endofterm.
    ENDLOOP.


    CLEAR   ls_endofterm_ftr.
    ls_endofterm_ftr-ftext = 'DCSG02T01'.
    ls_endofterm_ftr-fschool = c_school.
    ls_endofterm_ftr-rec_cnt = lines( lt_endofterm ) - 1.   "don't count header & trailer
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



  METHOD build_alv.
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
*& display output data in alv grid
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*

    DATA lo_alv TYPE REF TO lcl_alv.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

    CREATE OBJECT lo_alv EXPORTING it_data = mt_data .

    CALL METHOD lo_alv->display( ).

  ENDMETHOD.
ENDCLASS.



CLASS lcl_alv IMPLEMENTATION.

  METHOD constructor.

    DATA  ls_alv    TYPE  data_alv_s.
    DATA  lv_tabix  TYPE  i.

    FIELD-SYMBOLS <data>    TYPE data_s.


    LOOP AT it_data ASSIGNING <data>.
      lv_tabix = sy-tabix.
      CLEAR   ls_alv.
      ls_alv-recordno = lv_tabix.
      MOVE-CORRESPONDING <data> TO ls_alv.
      ls_alv-persidno = <data>-student12.
      ls_alv-ayear  = <data>-ayear.
      IF <data>-semester EQ 'F'.
        ls_alv-semester  = '10'.
      ELSE.
        ls_alv-semester  = '30'.
      ENDIF.
      ls_alv-course_abev  = <data>-course_abev.
      ls_alv-course_numb  = <data>-course_numb.
      ls_alv-course_credit_hours  = <data>-course_credit_hours.
      ls_alv-lab_i  = <data>-lab_i.
      ls_alv-lab_course_abev  = <data>-lab_course_abev.
      ls_alv-lab_course_numb  = <data>-lab_course_numb.
      ls_alv-lab_credit_hours = <data>-lab_credit_hours.
      ls_alv-grade            = <data>-grade.
      ls_alv-se_objid         = <data>-se_objid.
      ls_alv-evtcatg          = <data>-evtcatg.
      IF <data>-other_modgrps EQ 'X'.
        ls_alv-other_modgrps    = 'Check Student'.
      ELSE.
        CLEAR ls_alv-other_modgrps.
      ENDIF.
      APPEND ls_alv TO mt_alv.
    ENDLOOP.

  ENDMETHOD.

  METHOD display.

    DATA  ls_alv LIKE LINE OF mt_alv.
    DATA  lr_salv_functions    TYPE REF TO cl_salv_functions_list.

    TRY.
        cl_salv_table=>factory(
        IMPORTING
        r_salv_table = lo_table
        CHANGING
        t_table      = mt_alv ).
      CATCH cx_salv_msg.
    ENDTRY.

*****    o_columns = o_table->get_columns( ).
*****    o_columns->set_optimize( abap_true ).
*   set the alv columns
    me->set_alv_columns( EXPORTING colhdr = ls_alv ).

    lr_salv_functions ?= lo_table->get_functions( ).
    lr_salv_functions->set_all( true ).

*   set the alv footer
*        me->set_alv_footer( ).
    lo_table->display( ).

  ENDMETHOD.                    "display



  METHOD  set_alv_columns.
    DATA: lr_column  TYPE REF TO cl_salv_column_table,
          lr_columns TYPE REF TO cl_salv_columns_table.

    lr_columns = lo_table->get_columns( ).
    lr_columns->set_optimize( abap_true ).

*    TRY.
*        lr_column ?= lr_columns->get_column( 'MPERNR' ).
*        lr_column->set_short_text( 'MPERNR' ).
*        lr_column->set_medium_text( 'MPERNR' ).
*        lr_column->set_long_text( 'MPERNR' ).
*        lr_column->set_key( if_salv_c_bool_sap=>true ).
*        lr_column->set_visible( false ).
*      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
*    ENDTRY.
    TRY.
        lr_column ?= lr_columns->get_column( 'RECORDNO' ).
        lr_column->set_short_text( 'Rec' ).
        lr_column->set_medium_text( 'Record' ).
        lr_column->set_long_text( 'Record' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'PERSIDNO' ).
        lr_column->set_short_text( 'Student' ).
        lr_column->set_medium_text( 'Student' ).
        lr_column->set_long_text( 'Student' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'AYEAR' ).
        lr_column->set_short_text( 'Year' ).
        lr_column->set_medium_text( 'Year' ).
        lr_column->set_long_text( 'Year' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        lr_column ?= lr_columns->get_column( 'SEMESTER' ).
        lr_column->set_short_text( 'Sem' ).
        lr_column->set_medium_text( 'Sem' ).
        lr_column->set_long_text( 'Sem' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'COURSE_ABEV' ).
        lr_column->set_short_text( 'C_ABREV' ).
        lr_column->set_medium_text( 'C_ABREV' ).
        lr_column->set_long_text( 'C_ABREV' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'COURSE_NUMB' ).
        lr_column->set_short_text( 'C_NUMB' ).
        lr_column->set_medium_text( 'C_NUMB' ).
        lr_column->set_long_text( 'C_NUMB' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'COURSE_CREDIT_HOURS' ).
        lr_column->set_short_text( 'Hours' ).
        lr_column->set_medium_text( 'Hours' ).
        lr_column->set_long_text( 'hours' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'LAB_I' ).
        lr_column->set_short_text( 'Lab' ).
        lr_column->set_medium_text( 'Lab' ).
        lr_column->set_long_text( 'Lab' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        lr_column ?= lr_columns->get_column( 'LAB_COURSE_ABEV' ).
        lr_column->set_short_text( 'L_ABREV' ).
        lr_column->set_medium_text( 'L_ABREV' ).
        lr_column->set_long_text( 'L_ABREV' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'LAB_COURSE_NUMB' ).
        lr_column->set_short_text( 'L_NUMB' ).
        lr_column->set_medium_text( 'L_NUMB' ).
        lr_column->set_long_text( 'L_NUMB' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'LAB_CREDIT_HOURS' ).
        lr_column->set_short_text( 'Hours' ).
        lr_column->set_medium_text( 'Hours' ).
        lr_column->set_long_text( 'Hours' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'GRADE' ).
        lr_column->set_short_text( 'Grade' ).
        lr_column->set_medium_text( 'Grade' ).
        lr_column->set_long_text( 'Grade' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_column ?= lr_columns->get_column( 'SE_OBJID' ).
        lr_column->set_short_text( 'SE id' ).
        lr_column->set_medium_text( 'SE id' ).
        lr_column->set_long_text( 'SE id' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.

  METHOD  set_alv_footer.
  ENDMETHOD.
ENDCLASS.



*----------------------------------------------------------------------*
*       CLASS lcl_local IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_local IMPLEMENTATION.

  METHOD update_year.
    DATA: low_year_value TYPE piqperyr,
          it_valuetab    TYPE vrm_values.

    low_year_value = sy-datum(4) - 25.

    SELECT peryr peryt FROM t7piqyeart INTO TABLE it_valuetab
    WHERE peryr GE low_year_value
    AND spras = sy-langu.

    CHECK sy-subrc = 0.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'P_FYR'
        values          = it_valuetab
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "update_p_year

  METHOD update_perid
  .
    DATA: acad_sess   TYPE TABLE OF t7piqperassign,
          it_valuetab TYPE vrm_values.

    SELECT * FROM t7piqperassign INTO TABLE acad_sess
    WHERE pergroup = '001'.

    IF sy-subrc = 0.
      SELECT perid perit FROM t7piqperiodt INTO TABLE it_valuetab
      FOR ALL ENTRIES IN acad_sess
      WHERE perid = acad_sess-perid.

      CHECK sy-subrc = 0.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id              = 'P_PERID'
          values          = it_valuetab
        EXCEPTIONS
          id_illegal_name = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.
  ENDMETHOD.                    "update_p_perid
ENDCLASS.
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
*& method description                                                  *
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*