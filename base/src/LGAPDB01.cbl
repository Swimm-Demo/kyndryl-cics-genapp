       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGAPDB01.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT.DAT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-IN-STAT.
           
           SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.DAT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-OUT-STAT.

       DATA DIVISION.
       
       FILE SECTION.
       FD  INPUT-FILE.
           COPY INPUTREC.

       FD  OUTPUT-FILE.
           COPY OUTPUTREC.

       WORKING-STORAGE SECTION.
           COPY WORKSTOR.

       PROCEDURE DIVISION.

       P001.
           PERFORM P002
           PERFORM P003
           PERFORM P005
           PERFORM P014
           PERFORM P015
           STOP RUN.

       P002.
           DISPLAY 'Policy Premium Calculator Starting...'
           INITIALIZE WS-REC-CNT
           INITIALIZE WS-ERR-CNT
           INITIALIZE WS-PROC-CNT.

       P003.
           OPEN INPUT INPUT-FILE
           IF NOT INPUT-OK
               DISPLAY 'Error opening input file: ' WS-IN-STAT
               STOP RUN
           END-IF
           
           OPEN OUTPUT OUTPUT-FILE
           IF NOT OUTPUT-OK
               DISPLAY 'Error opening output file: ' WS-OUT-STAT
               STOP RUN
           END-IF
           
           PERFORM P004.

       P004.
           MOVE 'CUSTOMER   ' TO OUT-CUSTOMER-NUM
           MOVE 'PROPERTY-TYPE   ' TO OUT-PROPERTY-TYPE
           MOVE 'POSTCODE' TO OUT-POSTCODE
           MOVE 'RSK' TO OUT-RISK-SCORE
           MOVE 'FIRE-PREM' TO OUT-FIRE-PREMIUM
           MOVE 'CRIME-PREM' TO OUT-CRIME-PREMIUM
           MOVE 'FLOOD-PREM' TO OUT-FLOOD-PREMIUM
           MOVE 'WEATHER-PREM' TO OUT-WEATHER-PREMIUM
           MOVE 'TOTAL-PREMIUM' TO OUT-TOTAL-PREMIUM
           MOVE 'STATUS' TO OUT-STATUS
           MOVE 'REJECTION REASON' TO OUT-REJECT-REASON
           WRITE OUTPUT-RECORD.

       P005.
           PERFORM P006
           PERFORM UNTIL INPUT-EOF
               ADD 1 TO WS-REC-CNT
               PERFORM P007
               PERFORM P006
           END-PERFORM.

       P006.
           READ INPUT-FILE
           END-READ.

       P007.
           IF IN-POLICY-TYPE = 'C'
               PERFORM P009
               ADD 1 TO WS-PROC-CNT
           ELSE
               PERFORM P008
               ADD 1 TO WS-ERR-CNT
           END-IF.

       P008.
           MOVE IN-CUSTOMER-NUM TO OUT-CUSTOMER-NUM
           MOVE IN-PROPERTY-TYPE TO OUT-PROPERTY-TYPE
           MOVE IN-POSTCODE TO OUT-POSTCODE
           MOVE ZERO TO OUT-RISK-SCORE
           MOVE ZERO TO OUT-FIRE-PREMIUM
           MOVE ZERO TO OUT-CRIME-PREMIUM
           MOVE ZERO TO OUT-FLOOD-PREMIUM
           MOVE ZERO TO OUT-WEATHER-PREMIUM
           MOVE ZERO TO OUT-TOTAL-PREMIUM
           MOVE 'UNSUPPORTED' TO OUT-STATUS
           MOVE 'Only Commercial policies supported' TO OUT-REJECT-REASON
           WRITE OUTPUT-RECORD.

       P009.
           PERFORM P010
           PERFORM P011
           PERFORM P012
           PERFORM P013.

       P010.
           CALL 'LGAPDB02' USING IN-PROPERTY-TYPE, IN-POSTCODE, 
                                IN-LATITUDE, IN-LONGITUDE,
                                IN-FIRE-COVERAGE, IN-CRIME-COVERAGE,
                                IN-FLOOD-COVERAGE, IN-WEATHER-COVERAGE,
                                IN-CUSTOMER-HISTORY, WS-RISK-SCR.

       P011.
           CALL 'LGAPDB03' USING WS-RISK-SCR, IN-FIRE-PERIL, IN-CRIME-PERIL,
                                IN-FLOOD-PERIL, IN-WEATHER-PERIL, WS-STAT,
                                WS-STAT-DESC, WS-REJ-RSN, WS-FR-PREM,
                                WS-CR-PREM, WS-FL-PREM, WS-WE-PREM,
                                WS-TOT-PREM, WS-DISC-FACT.

       P012.
           CONTINUE.

       P013.
           MOVE IN-CUSTOMER-NUM TO OUT-CUSTOMER-NUM
           MOVE IN-PROPERTY-TYPE TO OUT-PROPERTY-TYPE
           MOVE IN-POSTCODE TO OUT-POSTCODE
           MOVE WS-RISK-SCR TO OUT-RISK-SCORE
           MOVE WS-FR-PREM TO OUT-FIRE-PREMIUM
           MOVE WS-CR-PREM TO OUT-CRIME-PREMIUM
           MOVE WS-FL-PREM TO OUT-FLOOD-PREMIUM
           MOVE WS-WE-PREM TO OUT-WEATHER-PREMIUM
           MOVE WS-TOT-PREM TO OUT-TOTAL-PREMIUM
           MOVE WS-STAT-DESC TO OUT-STATUS
           MOVE WS-REJ-RSN TO OUT-REJECT-REASON
           WRITE OUTPUT-RECORD.

       P014.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.

       P015.
           DISPLAY 'Processing Complete:'
           DISPLAY 'Total Records Read: ' WS-REC-CNT
           DISPLAY 'Records Processed: ' WS-PROC-CNT
           DISPLAY 'Error Records: ' WS-ERR-CNT. 