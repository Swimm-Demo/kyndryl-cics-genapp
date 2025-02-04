//COMRISK2 JOB 241901,'UPDATE DB',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
//*
//* Commercial Property Risk Calculation - Step 2
//* Update database with risk scores
//*
//STEP010  EXEC PGM=RISKPROG
//STEPLIB  DD DSN=<LOADLIB>,DISP=SHR
//INFILE   DD DSN=<USRHLQ>.RISK.TEMPDATA,DISP=SHR
//OUTFILE  DD DSN=<USRHLQ>.RISK.RISKOUT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,2)),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//ERRFILE  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//STEP020  EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=<DB2HLQ>.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
   DSN SYSTEM(<DB2SSID>)
   RUN PROGRAM(DSNTIAD) PLAN(<DB2PLAN>)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  UPDATE <DB2DBID>.COMMERCIAL C
  SET (RiskScore, LastRiskAssessment) =
      (SELECT DECIMAL(SUBSTR(R.OUT_RISK_SCORE,1,3) || '.' ||
              SUBSTR(R.OUT_RISK_SCORE,4,2), 5, 2),
              CURRENT DATE
       FROM SYSIBM.SYSDUMMY1,
       <USRHLQ>.RISK.RISKOUT R
       WHERE C.PolicyNumber = INTEGER(R.OUT_POLICY_NUM));

  INSERT INTO <DB2DBID>.RISK_HISTORY
      (PolicyNumber, 
       AssessmentDate, 
       RiskScore, 
       RiskCategory)
  SELECT INTEGER(OUT_POLICY_NUM),
         CURRENT TIMESTAMP,
         DECIMAL(SUBSTR(OUT_RISK_SCORE,1,3) || '.' ||
                SUBSTR(OUT_RISK_SCORE,4,2), 5, 2),
         RTRIM(OUT_RISK_CATEGORY)
  FROM <USRHLQ>.RISK.RISKOUT;
/*
//STEP030  EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=<USRHLQ>.RISK.RISKOUT,DISP=SHR
//SORTOUT  DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133)
//SYSIN    DD *
  SORT FIELDS=(12,10,CH,D)    * Sort by risk score descending
  OUTFIL BUILD=(1:1,10,       * Policy Number
                12:12,5,      * Risk Score
                18:22,10,     * Risk Category
                30:'Report Date: ',
                42:&DATE,
                120:X)        * New line
  HEADER1=('Commercial Policy Risk Assessment Report',
           ' ',
           'Policy     Risk   Risk',
           'Number     Score  Category',
           ' ')
  TRAILER1=(' ',
           'End of Report',
           ' ',
           'Total Policies Processed: ',
           COUNT=(M11,LENGTH=6))
/*
//STEP040  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE <USRHLQ>.RISK.TEMPDATA
  DELETE <USRHLQ>.RISK.RISKOUT
  IF MAXCC=8 THEN SET MAXCC=0
/*
//