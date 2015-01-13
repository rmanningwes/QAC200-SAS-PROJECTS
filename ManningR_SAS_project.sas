/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 1:33:39 PM
PROJECT: ManningR_SAS_project_January13
PROJECT PATH: P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp
---------------------------------------- */

/* Library assignment for Local.DATA1 */
Libname DATA1 V9 'P:\QAC\qac200\students\rmanning' ;
/* Library assignment for Local.DATA1 */
Libname DATA1 V9 'P:\QAC\qac200\students\rmanning' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (DATA1)   */
%LET _CLIENTTASKLABEL='Assign Project Library (DATA1)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
LIBNAME DATA1  "P:\QAC\qac200\students\rmanning" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SUBSET   */
%LET _CLIENTTASKLABEL='SUBSET';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(DATA1.SUBSET_MEPS_FULLYR_2012_SAS7);

PROC SQL;
   CREATE TABLE DATA1.SUBSET_MEPS_FULLYR_2012_SAS7(label="SUBSET_MEPS_FULLYR_2012_SAS7") AS 
   SELECT t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42
      FROM EC100002.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS program code   */
%LET SYSLAST=DATA1.SUBSET_MEPS_FULLYR_2012_SAS7;
%LET _CLIENTTASKLABEL='SAS program code';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';
%LET _SASPROGRAMFILE='C:\Users\rmanning\SAS Program Code\SAS program code.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 9:39:53 AM
   By task: Data Set Attributes2

   Input Data: Local:DATA1.SUBSET_MEPS_FULLYR_2012_SAS7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTCONTENTSFORSUBSET_MEPS__0000);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";


PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=DATA1.SUBSET_MEPS_FULLYR_2012_SAS7 ;

RUN;





GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for 2012 MEPS Full Year Subset    */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 MEPS Full Year Subset ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:32 PM
   By task: One-Way Frequencies for 2012 MEPS Full Year Subset 

   Input Data: Local:DATA1.SUBSET_MEPS_FULLYR_2012_SAS7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:DATA1.SUBSET_MEPS_FULLYR_2012_SAS7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.SEX, T.REGION12, T.RACETHX, T.MARRY12X, T.EDUYRDEG, T.EVRWRK, T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPM42, T.ADCMPD42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42, T.ADEXPL42
		     , T.ADEZUN42, T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHECR42, T.ADHOPE42, T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINTR42, T.ADINST42, T.ADLANG42, T.ADLIST42, T.ADMALS42, T.ADMWLM42, T.ADNDCR42, T.ADNERV42
		     , T.ADNSMK42, T.ADRISK42, T.ADNRGY42, T.ADOVER42, T.ADREST42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42, T.ADPRX42, T.ADPWLM42, T.ADRESP42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42, T.ADSPEC42, T.ADSPRF42, T.ADTLHW42
		     , T.AMCHIR12, T.AMTHER12, T.ASTHDX, T.BUSNP12X, T.CANCERDX, T.CHOLDX, T.DIABDX, T.PREGNT31, T.MIDX, T.WLKLIM53, T.MINORP42, T.ADWRTH42, T.K6SUM42, T.MCS42, T.PCS42, T.PHQ242, T.SAQELIG, T.SFFLAG42
	FROM DATA1.SUBSET_MEPS_FULLYR_2012_SAS7(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 MEPS Full Year Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES EVRWRK / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES AMCHIR12 / MISSPRINT  SCORES=TABLE;
	TABLES AMTHER12 / MISSPRINT  SCORES=TABLE;
	TABLES ASTHDX / MISSPRINT  SCORES=TABLE;
	TABLES BUSNP12X / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES CHOLDX / MISSPRINT  SCORES=TABLE;
	TABLES DIABDX / MISSPRINT  SCORES=TABLE;
	TABLES PREGNT31 / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES WLKLIM53 / MISSPRINT  SCORES=TABLE;
	TABLES MINORP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK."%STR(_SUBSET_MEPS_FULLYR_2012 MANAGED)"n);

PROC SQL;
   CREATE TABLE WORK."_SUBSET_MEPS_FULLYR_2012 MANAGED"n AS 
   SELECT t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          /* GENERAL_HEALTH */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health in general (recoded missing)" AS GENERAL_HEALTH, 
          /* HEALTH_LIMITS */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="Health limits modifying activities " AS HEALTH_LIMITS, 
          /* LIMTS_CLMBSTAIRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Health limits climbing stairs " AS LIMTS_CLMBSTAIRS, 
          /* LESS_PHYSPRBS */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="Accomplished less due to physical problems (within last 4 weeks) " AS LESS_PHYSPRBS, 
          /* WRKLIMT_PHYSPROBS */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
            END) LABEL="Work limit due to physical problems (within last 4 weeks) " AS WRKLIMT_PHYSPROBS, 
          /* ACCMPLESS_MNTLPROBS */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="Accomplished less due to mental problems (last 4 weeks)" AS ACCMPLESS_MNTLPROBS, 
          /* WORKLMT_MNTLPROBS */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN 0
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="Work limited because of mental problems (last 4 weeks)" AS WORKLMT_MNTLPROBS, 
          /* PAINLIMS_WORK */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="Pain limits normal work (last 4 weeks)" AS PAINLIMS_WORK, 
          /* CALM_PEACEFUL */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="Felt calm or peaceful (last 4 weeks)" AS CALM_PEACEFUL, 
          /* HI_ENERGY */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="Had a lot of energy (last 4 weeks) " AS HI_ENERGY, 
          /* DOWN_DEPR */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Felt downhearted or depressed (last 4 weeks) " AS DOWN_DEPR, 
          /* HLTHSTOP_SOCACT */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="Health stopped social activity (last 4 weeks) " AS HLTHSTOP_SOCACT, 
          /* MARRY */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital status as of 12/31/12" AS MARRY, 
          /* EDU */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="Year of education or highest degree" AS EDU, 
          /* EMPLY */
            (CASE 
               WHEN -1 = t1.EVRWRK THEN .
               WHEN -7 = t1.EVRWRK THEN .
               WHEN -8 = t1.EVRWRK THEN .
               WHEN -9 = t1.EVRWRK THEN .
               ELSE t1.EVRWRK
            END) LABEL="Ever worked for pay in life" AS EMPLY, 
          /* VST_MED */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="# visits to medical office (last 12 months)" AS VST_MED, 
          /* LMTS_CLMBSTAIRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Health limits climbing stairs " AS LMTS_CLMBSTAIRS, 
          /* EGET_NEEDEDMED */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="Easy getting needed medical care (last 12 months) " AS EGET_NEEDEDMED, 
          /* INS_NOTWORTH */
            (CASE 
               WHEN -1 = t1.ADINSB42 THEN .
               WHEN -7 = t1.ADINSB42 THEN .
               WHEN -8 = t1.ADINSB42 THEN .
               WHEN -9 = t1.ADINSB42 THEN .
               ELSE t1.ADINSB42
            END) LABEL="Health insurance not worth $" AS INS_NOTWORTH, 
          /* LANG */
            (CASE 
               WHEN -1 = t1.ADLANG42 THEN .
               ELSE t1.ADLANG42
            END) LABEL="Language of SAQ interview " AS LANG, 
          /* DRADV_QTSMK */
            (CASE 
               WHEN -1 = t1.ADNSMK42 THEN .
               WHEN -9 = t1.ADNSMK42 THEN .
               ELSE t1.ADNSMK42
            END) LABEL="Dr. advised to quit smoking (last 12 months)" AS DRADV_QTSMK, 
          /* NO_MEDHELP */
            (CASE 
               WHEN -1 = t1.ADOVER42 THEN .
               WHEN -7 = t1.ADOVER42 THEN .
               WHEN -8 = t1.ADOVER42 THEN .
               WHEN -9 = t1.ADOVER42 THEN .
               ELSE t1.ADOVER42
            END) LABEL="Can overcome ills without medical help " AS NO_MEDHELP, 
          /* RISK */
            (CASE 
               WHEN -1 = t1.ADRISK42 THEN .
               WHEN -7 = t1.ADRISK42 THEN .
               WHEN -8 = t1.ADRISK42 THEN .
               WHEN -9 = t1.ADRISK42 THEN .
               ELSE t1.ADRISK42
            END) LABEL="More likely to take risks " AS RISK, 
          /* SMOKE */
            (CASE 
               WHEN -1 = t1.ADSMOK42 THEN .
               WHEN -9 = t1.ADSMOK42 THEN .
               ELSE t1.ADSMOK42
            END) LABEL="Currently smoke " AS SMOKE, 
          /* SPEC */
            (CASE 
               WHEN -1 = t1.ADSPEC42 THEN .
               WHEN -9 = t1.ADSPEC42 THEN .
               ELSE t1.ADSPEC42
            END) LABEL="Needed to see specialist (last 12 months) " AS SPEC, 
          /* ASTH */
            (CASE 
               WHEN -7 = t1.ASTHDX THEN .
               WHEN -8 = t1.ASTHDX THEN .
               WHEN -9 = t1.ASTHDX THEN .
               ELSE t1.ASTHDX
            END) LABEL="Asthma diagnosis " AS ASTH, 
          /* CANCER */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="Cancer diagnosis " AS CANCER, 
          /* CHOL */
            (CASE 
               WHEN -7 = t1.CHOLDX THEN .
               WHEN -8 = t1.CHOLDX THEN .
               WHEN -9 = t1.CHOLDX THEN .
               ELSE t1.CHOLDX
            END) LABEL="High cholesterol diagnosis " AS CHOL, 
          /* DIABD */
            (CASE 
               WHEN -7 = t1.DIABDX THEN .
               WHEN -8 = t1.DIABDX THEN .
               WHEN -9 = t1.DIABDX THEN .
               ELSE t1.DIABDX
            END) LABEL="Diabetes Diagnosis " AS DIABD, 
          /* PREGNT */
            (CASE 
               WHEN -1 = t1.PREGNT31 THEN .
               WHEN -7 = t1.PREGNT31 THEN .
               WHEN -8 = t1.PREGNT31 THEN .
               WHEN -9 = t1.PREGNT31 THEN .
               ELSE t1.PREGNT31
            END) LABEL="Pregnant during reference period " AS PREGNT, 
          /* WLK_LIM */
            (CASE 
               WHEN -1 = t1.WLKLIM53 THEN .
               WHEN -7 = t1.WLKLIM53 THEN .
               WHEN -8 = t1.WLKLIM53 THEN .
               ELSE t1.WLKLIM53
            END) LABEL="Limitations in physical functioning " AS WLK_LIM
      FROM DATA1.SUBSET_MEPS_FULLYR_2012_SAS7 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:33 PM
   By task: Table Analysis

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.GENERAL_HEALTH, T.ADGENH42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGENH42 * GENERAL_HEALTH /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:33 PM
   By task: Table Analysis1

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_LIMITS, T.ADDAYA42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDAYA42 * HEALTH_LIMITS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis2   */
%LET _CLIENTTASKLABEL='Table Analysis2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:33 PM
   By task: Table Analysis2

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.LIMTS_CLMBSTAIRS, T.ADCLIM42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCLIM42 * LIMTS_CLMBSTAIRS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis3   */
%LET _CLIENTTASKLABEL='Table Analysis3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:33 PM
   By task: Table Analysis3

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.LESS_PHYSPRBS, T.ADPALS42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPALS42 * LESS_PHYSPRBS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis4   */
%LET _CLIENTTASKLABEL='Table Analysis4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:33 PM
   By task: Table Analysis4

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.WRKLIMT_PHYSPROBS, T.ADPWLM42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPWLM42 * WRKLIMT_PHYSPROBS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis5   */
%LET _CLIENTTASKLABEL='Table Analysis5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:33 PM
   By task: Table Analysis5

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ACCMPLESS_MNTLPROBS, T.ADMALS42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADMALS42 * ACCMPLESS_MNTLPROBS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis6   */
%LET _CLIENTTASKLABEL='Table Analysis6';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:34 PM
   By task: Table Analysis6

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.WORKLMT_MNTLPROBS, T.ADMWLM42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADMWLM42 * WORKLMT_MNTLPROBS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis7   */
%LET _CLIENTTASKLABEL='Table Analysis7';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:34 PM
   By task: Table Analysis7

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PAINLIMS_WORK, T.ADPAIN42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPAIN42 * PAINLIMS_WORK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis8   */
%LET _CLIENTTASKLABEL='Table Analysis8';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:34 PM
   By task: Table Analysis8

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CALM_PEACEFUL, T.ADCAPE42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCAPE42 * CALM_PEACEFUL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis9   */
%LET _CLIENTTASKLABEL='Table Analysis9';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:34 PM
   By task: Table Analysis9

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HI_ENERGY, T.ADNRGY42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADNRGY42 * HI_ENERGY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis10   */
%LET _CLIENTTASKLABEL='Table Analysis10';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:34 PM
   By task: Table Analysis10

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DOWN_DEPR, T.ADDOWN42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDOWN42 * DOWN_DEPR /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis11   */
%LET _CLIENTTASKLABEL='Table Analysis11';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:34 PM
   By task: Table Analysis11

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HLTHSTOP_SOCACT, T.ADSOCA42
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSOCA42 * HLTHSTOP_SOCACT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis12   */
%LET _CLIENTTASKLABEL='Table Analysis12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:34 PM
   By task: Table Analysis12

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY, T.MARRY12X
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY12X * MARRY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis13   */
%LET _CLIENTTASKLABEL='Table Analysis13';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:35 PM
   By task: Table Analysis13

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUYRDEG, T.EDU
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDUYRDEG * EDU /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis14   */
%LET _CLIENTTASKLABEL='Table Analysis14';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:35 PM
   By task: Table Analysis14

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EVRWRK, T.EMPLY
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EVRWRK * EMPLY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis15   */
%LET _CLIENTTASKLABEL='Table Analysis15';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:35 PM
   By task: Table Analysis15

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADAPPT42, T.VST_MED
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADAPPT42 * VST_MED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis16   */
%LET _CLIENTTASKLABEL='Table Analysis16';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:35 PM
   By task: Table Analysis16

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADCLIM42, T.LMTS_CLMBSTAIRS
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCLIM42 * LMTS_CLMBSTAIRS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis17   */
%LET _CLIENTTASKLABEL='Table Analysis17';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:35 PM
   By task: Table Analysis17

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADEGMC42, T.EGET_NEEDEDMED
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADEGMC42 * EGET_NEEDEDMED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis18   */
%LET _CLIENTTASKLABEL='Table Analysis18';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:35 PM
   By task: Table Analysis18

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADINSB42, T.INS_NOTWORTH
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADINSB42 * INS_NOTWORTH /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis19   */
%LET _CLIENTTASKLABEL='Table Analysis19';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:35 PM
   By task: Table Analysis19

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADLANG42, T.LANG
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADLANG42 * LANG /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis20   */
%LET _CLIENTTASKLABEL='Table Analysis20';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:36 PM
   By task: Table Analysis20

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADNSMK42, T.DRADV_QTSMK
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADNSMK42 * DRADV_QTSMK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis21   */
%LET _CLIENTTASKLABEL='Table Analysis21';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:36 PM
   By task: Table Analysis21

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADOVER42, T.NO_MEDHELP
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADOVER42 * NO_MEDHELP /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis22   */
%LET _CLIENTTASKLABEL='Table Analysis22';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:36 PM
   By task: Table Analysis22

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADRISK42, T.RISK
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADRISK42 * RISK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis23   */
%LET _CLIENTTASKLABEL='Table Analysis23';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:36 PM
   By task: Table Analysis23

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADSMOK42, T.SMOKE
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSMOK42 * SMOKE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis24   */
%LET _CLIENTTASKLABEL='Table Analysis24';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:36 PM
   By task: Table Analysis24

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADSPEC42, T.SPEC
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSPEC42 * SPEC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis25   */
%LET _CLIENTTASKLABEL='Table Analysis25';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:36 PM
   By task: Table Analysis25

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ASTHDX, T.ASTH
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ASTHDX * ASTH /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis26   */
%LET _CLIENTTASKLABEL='Table Analysis26';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:36 PM
   By task: Table Analysis26

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CANCERDX, T.CANCER
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES CANCERDX * CANCER /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis27   */
%LET _CLIENTTASKLABEL='Table Analysis27';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:37 PM
   By task: Table Analysis27

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CHOLDX, T.CHOL
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES CHOLDX * CHOL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis28   */
%LET _CLIENTTASKLABEL='Table Analysis28';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:37 PM
   By task: Table Analysis28

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DIABDX, T.DIABD
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES DIABDX * DIABD /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis29   */
%LET _CLIENTTASKLABEL='Table Analysis29';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:37 PM
   By task: Table Analysis29

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PREGNT31, T.PREGNT
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES PREGNT31 * PREGNT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis30   */
%LET _CLIENTTASKLABEL='Table Analysis30';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:37 PM
   By task: Table Analysis30

   Input Data: Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK._SUBSET_MEPS_FULLYR_2012 MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.WLKLIM53, T.WLK_LIM
	FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES WLK_LIM * WLKLIM53 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse Code SF-12 Variables    */
%LET _CLIENTTASKLABEL='Reverse Code SF-12 Variables ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SF12_REVCODE);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_SF12_REVCODE"n AS 
   SELECT t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          /* ADGENH_R */
            (6- t1.GENERAL_HEALTH) LABEL="General health reverse coded " AS ADGENH_R, 
          /* ADPAIN_R */
            (6- t1.PAINLIMS_WORK) LABEL="Pain limits normal work reverse coded " AS ADPAIN_R, 
          /* ADCAPE_R */
            (6- t1.CALM_PEACEFUL) LABEL="Felt calm or peaceful within the last 4 weeks reverse coded " AS ADCAPE_R, 
          /* ADNRGY_R */
            (6- t1.HI_ENERGY) LABEL="Had a lot of energy within the last 4 weeks reverse coded " AS ADNRGY_R
      FROM WORK.'_SUBSET_MEPS_FULLYR_2012 MANAGED'n t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis31   */
%LET _CLIENTTASKLABEL='Table Analysis31';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:37 PM
   By task: Table Analysis31

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.GENERAL_HEALTH, T.ADGENH_R
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGENH_R * GENERAL_HEALTH /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis32   */
%LET _CLIENTTASKLABEL='Table Analysis32';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:37 PM
   By task: Table Analysis32

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PAINLIMS_WORK, T.ADPAIN_R
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPAIN_R * PAINLIMS_WORK /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis33   */
%LET _CLIENTTASKLABEL='Table Analysis33';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:38 PM
   By task: Table Analysis33

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CALM_PEACEFUL, T.ADCAPE_R
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCAPE_R * CALM_PEACEFUL /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis34   */
%LET _CLIENTTASKLABEL='Table Analysis34';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:38 PM
   By task: Table Analysis34

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HI_ENERGY, T.ADNRGY_R
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADNRGY_R * HI_ENERGY /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:38 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.HEALTH_LIMITS, T.LIMTS_CLMBSTAIRS, T.LESS_PHYSPRBS, T.WRKLIMT_PHYSPROBS, T.ACCMPLESS_MNTLPROBS, T.WORKLMT_MNTLPROBS, T.DOWN_DEPR, T.HLTHSTOP_SOCACT, T.ADGENH_R, T.ADPAIN_R, T.ADCAPE_R, T.ADNRGY_R
	FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES HEALTH_LIMITS /  SCORES=TABLE;
	TABLES LIMTS_CLMBSTAIRS /  SCORES=TABLE;
	TABLES LESS_PHYSPRBS /  SCORES=TABLE;
	TABLES WRKLIMT_PHYSPROBS /  SCORES=TABLE;
	TABLES ACCMPLESS_MNTLPROBS /  SCORES=TABLE;
	TABLES WORKLMT_MNTLPROBS /  SCORES=TABLE;
	TABLES DOWN_DEPR /  SCORES=TABLE;
	TABLES HLTHSTOP_SOCACT /  SCORES=TABLE;
	TABLES ADGENH_R /  SCORES=TABLE;
	TABLES ADPAIN_R /  SCORES=TABLE;
	TABLES ADCAPE_R /  SCORES=TABLE;
	TABLES ADNRGY_R /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012_SF12_2);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012_SF12_2"n AS 
   SELECT t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          /* Sum_SF12_Variables */
            
            (SUM(t1.ADGENH_R,t1.ADPAIN_R,t1.ADCAPE_R,t1.ADNRGY_R,t1.HEALTH_LIMITS,t1.LIMTS_CLMBSTAIRS,t1.LESS_PHYSPRBS,t1.WORKLMT_MNTLPROBS,t1.WRKLIMT_PHYSPROBS,t1.ACCMPLESS_MNTLPROBS,t1.DOWN_DEPR,t1.HLTHSTOP_SOCACT)) 
            LABEL="Sum of SF12 Variables " AS Sum_SF12_Variables
      FROM WORK.MEPS_FULLYR_2012_SF12_REVCODE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:38 PM
   By task: List Data

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADGENH_R, T.ADPAIN_R, T.ADCAPE_R, T.ADNRGY_R, T.HEALTH_LIMITS, T.LIMTS_CLMBSTAIRS, T.LESS_PHYSPRBS, T.WRKLIMT_PHYSPROBS, T.ACCMPLESS_MNTLPROBS, T.WORKLMT_MNTLPROBS, T.DOWN_DEPR, T.HLTHSTOP_SOCACT, T.Sum_SF12_Variables
	FROM WORK.MEPS_FULLYR_2012_SF12_2 as T
;
QUIT;
TITLE;
TITLE1 "Check aggregate variable coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR ADGENH_R ADPAIN_R ADCAPE_R ADNRGY_R HEALTH_LIMITS LIMTS_CLMBSTAIRS LESS_PHYSPRBS WRKLIMT_PHYSPROBS ACCMPLESS_MNTLPROBS WORKLMT_MNTLPROBS DOWN_DEPR HLTHSTOP_SOCACT Sum_SF12_Variables;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:38 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Sum_SF12_Variables
	FROM WORK.MEPS_FULLYR_2012_SF12_2 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Sum_SF12_Variables /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for Aggregate Mental Health Variable   */
%LET _CLIENTTASKLABEL='Summary Statistics for Aggregate Mental Health Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:38 PM
   By task: Summary Statistics for Aggregate Mental Health Variable

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12_Variables
	FROM WORK.MEPS_FULLYR_2012_SF12_2(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for 2012 MEPS Aggregate Overall Mental Health Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Sum_SF12_Variables;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Aggregate Overall Health Variable    */
%LET _CLIENTTASKLABEL='Distribution Analysis for Aggregate Overall Health Variable ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:38 PM
   By task: Distribution Analysis for Aggregate Overall Health Variable 

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12_Variables
	FROM WORK.MEPS_FULLYR_2012_SF12_2(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: MEPS Full Year 2012 Sum_SF12_Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_SF12_Variables;
	HISTOGRAM   Sum_SF12_Variables / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:39 PM
   By task: Distribution Analysis

   Input Data: Local:WORK.MEPS_FULLYR_2012_SF12_2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012_SF12_2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12_Variables
	FROM WORK.MEPS_FULLYR_2012_SF12_2 as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Sum_SF12_Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_SF12_Variables;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder1   */
%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_ AS 
   SELECT /* Aggregate Health_Categorical  */
            (CASE  
               WHEN t1.Sum_SF12_Variables >=2 and  t1.Sum_SF12_Variables <37
               THEN 1
               WHEN t1.Sum_SF12_Variables >=37 and t1.Sum_SF12_Variables <44
               THEN 2
               WHEN t1.Sum_SF12_Variables >=44 and t1.Sum_SF12_Variables <47
               THEN 3
               WHEN t1.Sum_SF12_Variables >=47
               THEN 4
            END) LABEL="Overall Aggregate Health Variable Categories " AS 'Aggregate Health_Categorical 'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables
      FROM WORK.MEPS_FULLYR_2012_SF12_2 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Overall Aggregate Health    */
%LET _CLIENTTASKLABEL='Overall Aggregate Health ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:39 PM
   By task: Overall Aggregate Health 

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T."Aggregate Health_Categorical"n, T.Sum_SF12_Variables
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_ as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES "Aggregate Health_Categorical"n /  SCORES=TABLE;
	TABLES Sum_SF12_Variables /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder2   */
%LET _CLIENTTASKLABEL='Query Builder2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_FULLYR_2012__S7);

PROC SQL;
   CREATE TABLE WORK."MEPS_FULLYR_2012__S7"n AS 
   SELECT t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          /* Sum_Indicative_Variables */
            (SUM(t1.ASTH,t1.CHOL,t1.DIABD,t1.CANCER)) LABEL="Sum of Indicative Health Variables " AS 
            Sum_Indicative_Variables
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SF12_ t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data1   */
%LET _CLIENTTASKLABEL='List Data1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:39 PM
   By task: List Data1

   Input Data: Local:WORK.MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ASTH, T.CANCER, T.DIABD, T.CHOL, T.Sum_Indicative_Variables
	FROM WORK.MEPS_FULLYR_2012__S7 as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregate Variable Coding I";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR ASTH CANCER DIABD CHOL Sum_Indicative_Variables;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:39 PM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Sum_Indicative_Variables
	FROM WORK.MEPS_FULLYR_2012__S7 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Sum_Indicative_Variables /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for Indicative Variables (Recoded)   */
%LET _CLIENTTASKLABEL='Summary Statistics for Indicative Variables (Recoded)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:39 PM
   By task: Summary Statistics for Indicative Variables (Recoded)

   Input Data: Local:WORK.MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_Indicative_Variables
	FROM WORK.MEPS_FULLYR_2012__S7(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for 2012 MEPS Indicative Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Sum_Indicative_Variables;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Indicative Variables    */
%LET _CLIENTTASKLABEL='Distribution Analysis for Indicative Variables ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:39 PM
   By task: Distribution Analysis for Indicative Variables 

   Input Data: Local:WORK.MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_Indicative_Variables
	FROM WORK.MEPS_FULLYR_2012__S7(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Aggregate variable for indicative variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_Indicative_Variables;
	HISTOGRAM   Sum_Indicative_Variables / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis1   */
%LET _CLIENTTASKLABEL='Distribution Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:39 PM
   By task: Distribution Analysis1

   Input Data: Local:WORK.MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_Indicative_Variables
	FROM WORK.MEPS_FULLYR_2012__S7 as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Sum_Indicative_Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_Indicative_Variables;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder3   */
%LET _CLIENTTASKLABEL='Query Builder3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012__S7);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012__S7 AS 
   SELECT t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          /* I4_Categorical */
            (CASE  
               WHEN t1.Sum_Indicative_Variables >=1 and t1.Sum_Indicative_Variables <5
               THEN 1
               WHEN t1.Sum_Indicative_Variables >=5 and t1.Sum_Indicative_Variables <6
               THEN 2
               WHEN t1.Sum_Indicative_Variables >=6 and t1.Sum_Indicative_Variables <7
               THEN 3
               WHEN t1.Sum_Indicative_Variables >=7 
               THEN 4
               
            END) LABEL="Indicative Variables Categorized " AS I4_Categorical
      FROM WORK.MEPS_FULLYR_2012__S7 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies3   */
%LET _CLIENTTASKLABEL='One-Way Frequencies3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:40 PM
   By task: One-Way Frequencies3

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.I4_Categorical, T.Sum_Indicative_Variables
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__S7 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES I4_Categorical /  SCORES=TABLE;
	TABLES Sum_Indicative_Variables /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for marital status    */
%LET _CLIENTTASKLABEL='Summary Statistics for marital status ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:40 PM
   By task: Summary Statistics for marital status 

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__S7(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for Marital status";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR MARRY;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis Marital Status    */
%LET _CLIENTTASKLABEL='Distribution Analysis Marital Status ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:40 PM
   By task: Distribution Analysis Marital Status 

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__S7(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Marital Status";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR MARRY;
	HISTOGRAM   MARRY / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder4   */
%LET _CLIENTTASKLABEL='Query Builder4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012__0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012__0001 AS 
   SELECT t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical, 
          /* Marry_Categorical */
            (CASE  
               WHEN t1.MARRY =1
               THEN 1
               WHEN t1.MARRY >1 and t1.MARRY <5
               THEN 2
               WHEN t1.MARRY =5
               THEN 3
            
            END) LABEL="Marital Status Categorized " AS Marry_Categorical
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__S7 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MARITAL CATEGORIES   */
%LET _CLIENTTASKLABEL='MARITAL CATEGORIES';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:40 PM
   By task: MARITAL CATEGORIES

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Marry_Categorical, T.MARRY
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__0001 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Marry_Categorical /  SCORES=TABLE;
	TABLES MARRY /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for EDU   */
%LET _CLIENTTASKLABEL='Summary Statistics for EDU';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:40 PM
   By task: Summary Statistics for EDU

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDU
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__S7(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for EDU";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR EDU;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis EDU   */
%LET _CLIENTTASKLABEL='Distribution Analysis EDU';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:40 PM
   By task: Distribution Analysis EDU

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__S7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDU
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__S7(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: EDU";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR EDU;
	HISTOGRAM   EDU / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder5   */
%LET _CLIENTTASKLABEL='Query Builder5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012__0002);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012__0002 AS 
   SELECT t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical, 
          /* EDU_Categorical  */
            (CASE  
               WHEN t1.EDU >=0 and t1.EDU <9
               THEN 1
               WHEN t1.EDU >=9 and t1.EDU <16
               THEN 2
               WHEN t1.EDU >=16 and t1.EDU <19
               THEN 3
               WHEN t1.EDU >=19 and t1.EDU <=21
               THEN 4
            
            END) LABEL="Categories for Education " AS 'EDU_Categorical 'n, 
          /* MARRY_CATEGORICAL */
            (CASE  
               WHEN t1.MARRY =1
               THEN 1
               WHEN t1.MARRY >1 and t1.MARRY <5
               THEN 2
               WHEN t1.MARRY =5
               THEN 3
            
            END) LABEL="Categories for Marital status" AS MARRY_CATEGORICAL
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__S7 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: EDU CATEGORIES   */
%LET _CLIENTTASKLABEL='EDU CATEGORIES';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:41 PM
   By task: EDU CATEGORIES

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0002
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0002
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.EDU_Categorical, T.EDU
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__0002 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES EDU_Categorical /  SCORES=TABLE;
	TABLES EDU /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Subset for Merge    */
%LET _CLIENTTASKLABEL='Subset for Merge ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(DATA1.SUBSET_MEPS_FULLYR_2012_A7);

PROC SQL;
   CREATE TABLE DATA1.SUBSET_MEPS_FULLYR_2012_A7(label="SUBSET_MEPS_FULLYR_2012_A7") AS 
   SELECT t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical, 
          t1.EDU_Categorical, 
          t1.MARRY_CATEGORICAL
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__0002 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder6   */
%LET _CLIENTTASKLABEL='Query Builder6';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_MEPS_FULLYR_201);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_MEPS_FULLYR_201 AS 
   SELECT t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical, 
          t1.EDU_Categorical, 
          t1.MARRY_CATEGORICAL, 
          /* INFULLYR1 */
            (1) LABEL="New variable that flags observations " AS INFULLYR1
      FROM DATA1.SUBSET_MEPS_FULLYR_2012_A7 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data for Temporary   */
%LET _CLIENTTASKLABEL='Data for Temporary';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(DATA1.SUBSET_MEPS_FULLYR_2012_A7);

PROC SQL;
   CREATE TABLE DATA1.SUBSET_MEPS_FULLYR_2012_A7(label="SUBSET_MEPS_FULLYR_2012_A7") AS 
   SELECT t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical, 
          t1.EDU_Categorical, 
          t1.MARRY_CATEGORICAL, 
          t1.INFULLYR1
      FROM WORK.QUERY_FOR_SUBSET_MEPS_FULLYR_201 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder8   */
%LET _CLIENTTASKLABEL='Query Builder8';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_MEPS_FULLY_113);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_MEPS_FULLY_113(label="QUERY_FOR_SUBSET_MEPS_FULLY_113") AS 
   SELECT t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical, 
          t1.EDU_Categorical, 
          t1.MARRY_CATEGORICAL, 
          t1.INFULLYR1
      FROM WORK.QUERY_FOR_SUBSET_MEPS_FULLYR_201 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MY DATA SUBSET   */
%LET _CLIENTTASKLABEL='MY DATA SUBSET';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(DATA1.MEPS_FULLYR_2012__A6);

PROC SQL;
   CREATE TABLE DATA1.MEPS_FULLYR_2012__A6(label="MEPS_FULLYR_2012__A6") AS 
   SELECT t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__S7 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:42 PM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder7   */
LIBNAME EC100015 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='Query Builder7';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* INER1 */
            (1) LABEL="Flags observations in the ER data subset " AS INER1
      FROM EC100015.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Inner Join MEPS and ER   */
%LET _CLIENTTASKLABEL='Inner Join MEPS and ER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(DATA1.MEPS_AND_ER_2012);

PROC SQL;
   CREATE TABLE DATA1.MEPS_AND_ER_2012(label="MEPS_AND_ER_2012") AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t2.'Aggregate Health_Categorical'n, 
          t2.AGE12X, 
          t2.DUPERSID AS DUPERSID1, 
          t2.SEX, 
          t2.REGION12, 
          t2.RACETHX, 
          t2.MARRY12X, 
          t2.EDUYRDEG, 
          t2.EVRWRK, 
          t2.ADAPPT42, 
          t2.ADCAPE42, 
          t2.ADCLIM42, 
          t2.ADCMPM42, 
          t2.ADCMPD42, 
          t2.ADCMPY42, 
          t2.ADDAYA42, 
          t2.ADDOWN42, 
          t2.ADDPRS42, 
          t2.ADDRBP42, 
          t2.ADEFRT42, 
          t2.ADEGMC42, 
          t2.ADEXPL42, 
          t2.ADEZUN42, 
          t2.ADFFRM42, 
          t2.ADFHLP42, 
          t2.ADGENH42, 
          t2.ADHECR42, 
          t2.ADHOPE42, 
          t2.ADILCR42, 
          t2.ADILWW42, 
          t2.ADINSA42, 
          t2.ADINSB42, 
          t2.ADINTR42, 
          t2.ADINST42, 
          t2.ADLANG42, 
          t2.ADLIST42, 
          t2.ADMALS42, 
          t2.ADMWLM42, 
          t2.ADNDCR42, 
          t2.ADNERV42, 
          t2.ADNSMK42, 
          t2.ADRISK42, 
          t2.ADNRGY42, 
          t2.ADOVER42, 
          t2.ADREST42, 
          t2.ADPAIN42, 
          t2.ADPALS42, 
          t2.ADPRTM42, 
          t2.ADPRX42, 
          t2.ADPWLM42, 
          t2.ADRESP42, 
          t2.ADRTCR42, 
          t2.ADRTWW42, 
          t2.ADSAD42, 
          t2.ADSMOK42, 
          t2.ADSOCA42, 
          t2.ADSPEC42, 
          t2.ADSPRF42, 
          t2.ADTLHW42, 
          t2.AMCHIR12, 
          t2.AMTHER12, 
          t2.ASTHDX, 
          t2.BUSNP12X, 
          t2.CANCERDX, 
          t2.CHOLDX, 
          t2.DIABDX, 
          t2.PREGNT31, 
          t2.MIDX, 
          t2.WLKLIM53, 
          t2.MINORP42, 
          t2.ADWRTH42, 
          t2.K6SUM42, 
          t2.MCS42, 
          t2.PCS42, 
          t2.PHQ242, 
          t2.SAQELIG, 
          t2.SFFLAG42, 
          t2.GENERAL_HEALTH, 
          t2.HEALTH_LIMITS, 
          t2.LIMTS_CLMBSTAIRS, 
          t2.LESS_PHYSPRBS, 
          t2.WRKLIMT_PHYSPROBS, 
          t2.ACCMPLESS_MNTLPROBS, 
          t2.WORKLMT_MNTLPROBS, 
          t2.PAINLIMS_WORK, 
          t2.CALM_PEACEFUL, 
          t2.HI_ENERGY, 
          t2.DOWN_DEPR, 
          t2.HLTHSTOP_SOCACT, 
          t2.MARRY, 
          t2.EDU, 
          t2.EMPLY, 
          t2.VST_MED, 
          t2.LMTS_CLMBSTAIRS, 
          t2.EGET_NEEDEDMED, 
          t2.INS_NOTWORTH, 
          t2.LANG, 
          t2.DRADV_QTSMK, 
          t2.NO_MEDHELP, 
          t2.RISK, 
          t2.SMOKE, 
          t2.SPEC, 
          t2.ASTH, 
          t2.CANCER, 
          t2.CHOL, 
          t2.DIABD, 
          t2.PREGNT, 
          t2.WLK_LIM, 
          t2.ADGENH_R, 
          t2.ADPAIN_R, 
          t2.ADCAPE_R, 
          t2.ADNRGY_R, 
          t2.Sum_SF12_Variables, 
          t2.Sum_Indicative_Variables, 
          t2.I4_Categorical, 
          t2.EDU_Categorical, 
          t2.MARRY_CATEGORICAL, 
          t2.INFULLYR1, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER1
      FROM WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT t1
           FULL JOIN WORK.QUERY_FOR_SUBSET_MEPS_FULLY_113 t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t1.INER1 = 1 AND t2.INFULLYR1 = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data2   */
%LET _CLIENTTASKLABEL='List Data2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:43 PM
   By task: List Data2

   Input Data: Local:DATA1.MEPS_AND_ER_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:DATA1.MEPS_AND_ER_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID, T.DUPERSID1, T.INER1
	FROM DATA1.MEPS_AND_ER_2012 as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR DUPERSID DUPERSID1 INER1;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes1   */
%LET _CLIENTTASKLABEL='Data Set Attributes1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:43 PM
   By task: Data Set Attributes1

   Input Data: Local:DATA1.MEPS_AND_ER_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForMEPS_AND_ER_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=DATA1.MEPS_AND_ER_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForMEPS_AND_ER_2012(LABEL="Contents Details for MEPS_AND_ER_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForMEPS_AND_ER_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_AND_ER_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForMEPS_AND_ER_2012 OUT=WORK.CONTContentsForMEPS_AND_ER_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForMEPS_AND_ER_2012
		WHERE memname='MEPS_AND_ER_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder9   */
%LET _CLIENTTASKLABEL='Query Builder9';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK."%STR(MEPS_AND_ER_Managed )"n);

PROC SQL;
   CREATE TABLE WORK."MEPS_AND_ER_Managed "n AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID1, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical, 
          t1.EDU_Categorical, 
          t1.MARRY_CATEGORICAL, 
          t1.INFULLYR1, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER1, 
          /* MRI_R */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 2
               ELSE t1.MRI
            END) LABEL="MRI recoded " AS MRI_R, 
          /* XRAYS_R */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 2
               ELSE t1.XRAYS
            END) LABEL="XRAYS recoded " AS XRAYS_R
      FROM DATA1.MEPS_AND_ER_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies4   */
%LET _CLIENTTASKLABEL='One-Way Frequencies4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:43 PM
   By task: One-Way Frequencies4

   Input Data: Local:WORK.MEPS_AND_ER_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_AND_ER_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAYS, T.XRAYS_R
	FROM WORK.MEPS_AND_ER_MANAGED as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAYS /  SCORES=TABLE;
	TABLES XRAYS_R /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies5   */
%LET _CLIENTTASKLABEL='One-Way Frequencies5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:43 PM
   By task: One-Way Frequencies5

   Input Data: Local:WORK.MEPS_AND_ER_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_AND_ER_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MRI, T.MRI_R
	FROM WORK.MEPS_AND_ER_MANAGED as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI /  SCORES=TABLE;
	TABLES MRI_R /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder10   */
%LET _CLIENTTASKLABEL='Query Builder10';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_AND_ER_4Merge);

PROC SQL;
   CREATE TABLE WORK."QUERY_FOR_MEPS_AND_ER_4Merge"n AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM WORK.MEPS_AND_ER_MANAGED t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder11   */
%LET _CLIENTTASKLABEL='Query Builder11';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(DATA1.MEPS_AND_ER_COUNT_VARIABLE);

PROC SQL;
   CREATE TABLE DATA1.MEPS_AND_ER_COUNT_VARIABLE(label="MEPS_AND_ER_COUNT_VARIABLE") AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID1, 
          t1.SEX, 
          t1.REGION12, 
          t2.DUPERSID AS DUPERSID2, 
          t2.COUNT_of_DUPERSID, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical, 
          t1.EDU_Categorical, 
          t1.MARRY_CATEGORICAL, 
          t1.INFULLYR1, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER1, 
          t1.MRI_R, 
          t1.XRAYS_R
      FROM WORK.MEPS_AND_ER_MANAGED t1
           INNER JOIN WORK.QUERY_FOR_MEPS_AND_ER_4MERGE t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for number of ER visits variable    */
%LET _CLIENTTASKLABEL='Summary Statistics for number of ER visits variable ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:44 PM
   By task: Summary Statistics for number of ER visits variable 

   Input Data: Local:DATA1.MEPS_AND_ER_COUNT_VARIABLE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:DATA1.MEPS_AND_ER_COUNT_VARIABLE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM DATA1.MEPS_AND_ER_COUNT_VARIABLE(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for the number of ER visists variable (Count of DUPERSID)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR COUNT_of_DUPERSID;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for number of ER visits    */
%LET _CLIENTTASKLABEL='Distribution Analysis for number of ER visits ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:44 PM
   By task: Distribution Analysis for number of ER visits 

   Input Data: Local:DATA1.MEPS_AND_ER_COUNT_VARIABLE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:DATA1.MEPS_AND_ER_COUNT_VARIABLE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM DATA1.MEPS_AND_ER_COUNT_VARIABLE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Number of ER visits variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM   COUNT_of_DUPERSID / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies6   */
%LET _CLIENTTASKLABEL='One-Way Frequencies6';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:44 PM
   By task: One-Way Frequencies6

   Input Data: Local:DATA1.MEPS_AND_ER_COUNT_VARIABLE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:DATA1.MEPS_AND_ER_COUNT_VARIABLE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID
	FROM DATA1.MEPS_AND_ER_COUNT_VARIABLE as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for Original number of ER visits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder12   */
%LET _CLIENTTASKLABEL='Query Builder12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_AND_ER_Cat_VARI);

PROC SQL;
   CREATE TABLE WORK."MEPS_AND_ER_Cat_VARI"n AS 
   SELECT /* ER_CATEGORICAL */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID =1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID >=2 and t1.COUNT_of_DUPERSID <4
               THEN 2
               WHEN t1.COUNT_of_DUPERSID >=4 and t1.COUNT_of_DUPERSID <8
               THEN 3
               WHEN t1.COUNT_of_DUPERSID >=8 
               THEN 4
            END) LABEL="Number of ER visits categorized " AS ER_CATEGORICAL, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.'Aggregate Health_Categorical'n, 
          t1.AGE12X, 
          t1.DUPERSID1, 
          t1.SEX, 
          t1.REGION12, 
          t1.DUPERSID2, 
          t1.COUNT_of_DUPERSID, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRWRK, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINTR42, 
          t1.ADINST42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNSMK42, 
          t1.ADRISK42, 
          t1.ADNRGY42, 
          t1.ADOVER42, 
          t1.ADREST42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.AMCHIR12, 
          t1.AMTHER12, 
          t1.ASTHDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHOLDX, 
          t1.DIABDX, 
          t1.PREGNT31, 
          t1.MIDX, 
          t1.WLKLIM53, 
          t1.MINORP42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SFFLAG42, 
          t1.GENERAL_HEALTH, 
          t1.HEALTH_LIMITS, 
          t1.LIMTS_CLMBSTAIRS, 
          t1.LESS_PHYSPRBS, 
          t1.WRKLIMT_PHYSPROBS, 
          t1.ACCMPLESS_MNTLPROBS, 
          t1.WORKLMT_MNTLPROBS, 
          t1.PAINLIMS_WORK, 
          t1.CALM_PEACEFUL, 
          t1.HI_ENERGY, 
          t1.DOWN_DEPR, 
          t1.HLTHSTOP_SOCACT, 
          t1.MARRY, 
          t1.EDU, 
          t1.EMPLY, 
          t1.VST_MED, 
          t1.LMTS_CLMBSTAIRS, 
          t1.EGET_NEEDEDMED, 
          t1.INS_NOTWORTH, 
          t1.LANG, 
          t1.DRADV_QTSMK, 
          t1.NO_MEDHELP, 
          t1.RISK, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.ASTH, 
          t1.CANCER, 
          t1.CHOL, 
          t1.DIABD, 
          t1.PREGNT, 
          t1.WLK_LIM, 
          t1.ADGENH_R, 
          t1.ADPAIN_R, 
          t1.ADCAPE_R, 
          t1.ADNRGY_R, 
          t1.Sum_SF12_Variables, 
          t1.Sum_Indicative_Variables, 
          t1.I4_Categorical, 
          t1.EDU_Categorical, 
          t1.MARRY_CATEGORICAL, 
          t1.INFULLYR1, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER1, 
          t1.MRI_R, 
          t1.XRAYS_R
      FROM DATA1.MEPS_AND_ER_COUNT_VARIABLE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis35   */
%LET _CLIENTTASKLABEL='Table Analysis35';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:45 PM
   By task: Table Analysis35

   Input Data: Local:WORK.MEPS_AND_ER_CAT_VARI
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_AND_ER_CAT_VARI
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID, T.ER_CATEGORICAL
	FROM WORK.MEPS_AND_ER_CAT_VARI as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for Contingency Table Comparing number of ER visits & Categorized ER visits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ER_CATEGORICAL * COUNT_of_DUPERSID /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies7   */
%LET _CLIENTTASKLABEL='One-Way Frequencies7';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January13.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:32:45 PM
   By task: One-Way Frequencies7

   Input Data: Local:WORK.MEPS_AND_ER_CAT_VARI
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_AND_ER_CAT_VARI
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ER_CATEGORICAL
	FROM WORK.MEPS_AND_ER_CAT_VARI as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for Categorical number of ER visits variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Robert Manning";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ER_CATEGORICAL /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
