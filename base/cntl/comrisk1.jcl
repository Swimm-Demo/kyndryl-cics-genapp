//COMRISK1 JOB 241901,'RISK CALC',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
//*
//* Commercial Property Risk Calculation - Step 1
//* Extract active policies and claims data
//*
//STEP010  EXEC PGM=IDCAMS 
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE <USRHLQ>.RISK.TEMPDATA
  IF MAXCC=8 THEN SET MAXCC=0
/*
//STEP020  EXEC PGM=IEFBR14
//DD1      DD DSN=<USRHLQ>.RISK.TEMPDATA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5)),
//            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)
//*
//STEP030  EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=<DB2HLQ>.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
   DSN SYSTEM(<DB2SSID>)
   RUN PROGRAM(DSNTIAD) PLAN(<DB2PLAN>)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
  UNLOAD DATA FROM TABLE
  (SELECT CAST(DIGITS(C.PolicyNumber) AS CHAR(10)) AS PolicyNumber,
         CAST(C.PropertyType AS CHAR(15)) AS PropertyType,
         CAST(C.Address AS CHAR(255)) AS Address,
         CAST(C.Zipcode AS CHAR(8)) AS Zipcode,
         CAST(DIGITS(C.FirePeril) AS CHAR(2)) AS FirePeril,
         CAST(DIGITS(C.CrimePeril) AS CHAR(2)) AS CrimePeril,
         CAST(DIGITS(C.FloodPeril) AS CHAR(2)) AS FloodPeril,
         CAST(DIGITS(C.WeatherPeril) AS CHAR(2)) AS WeatherPeril,
         CAST(DIGITS(COUNT(CL.ClaimNumber)) AS CHAR(3)) AS ClaimCount,
         CAST(DIGITS(COALESCE(SUM(CL.Value), 0)) AS CHAR(9)) AS TotalClaimValue
    FROM <DB2DBID>.COMMERCIAL C
    LEFT JOIN <DB2DBID>.CLAIM CL 
         ON C.PolicyNumber = CL.PolicyNumber
         AND CL.ClaimDate >= CURRENT DATE - 2 YEARS
    WHERE C.Status = 1
    GROUP BY C.PolicyNumber, C.PropertyType, C.Address,
             C.Zipcode, C.FirePeril, C.CrimePeril,
             C.FloodPeril, C.WeatherPeril)
  INTO <USRHLQ>.RISK.TEMPDATA;
/*
//***********************************************************
//* Submit risk calculation job
//***********************************************************
//STEP040  EXEC PGM=IEBGENER
//SYSUT1   DD *
//COMRISK2 JOB 241901,'UPDATE DB',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
/*
//SYSUT2   DD SYSOUT=(A,INTRDR)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*