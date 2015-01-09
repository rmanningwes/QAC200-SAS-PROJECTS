/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 09, 2015     TIME: 10:33:23 AM
PROJECT: ManningR_SAS_project_January9
PROJECT PATH: P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
LIBNAME DATA1  "P:\QAC\qac200\students\rmanning" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SUBSET   */
%LET _CLIENTTASKLABEL='SUBSET';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:00 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:00 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:01 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:01 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:01 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:01 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:01 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:01 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:01 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:02 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:02 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:02 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:02 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:02 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:02 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:02 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:03 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:03 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:03 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:03 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:03 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:03 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:03 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:04 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:04 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:04 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:04 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:04 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:04 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:04 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:05 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:05 AM
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


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rmanning\ManningR_SAS_project_January9.egp';
%LET _CLIENTPROJECTNAME='ManningR_SAS_project_January9.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:32:06 AM
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
