       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGRISK01.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RISK-SCORE              PIC 999 VALUE ZERO.
       
       LINKAGE SECTION.
       01  LS-RISK-DATA.
           05 LS-PROP-TYPE            PIC X(15).
           05 LS-POSTCODE             PIC X(8).
           05 LS-RISK-SCORE           PIC 999.
           05 LS-STATUS               PIC 9.
           05 LS-REJECT-REASON        PIC X(50).
      
       PROCEDURE DIVISION USING LS-RISK-DATA.
           
           MOVE 100 TO WS-RISK-SCORE.
      
      * Property type risk evaluation
           EVALUATE LS-PROP-TYPE
             WHEN 'WAREHOUSE'
               ADD 50 TO WS-RISK-SCORE
             WHEN 'FACTORY' 
               ADD 75 TO WS-RISK-SCORE
             WHEN 'OFFICE'
               ADD 25 TO WS-RISK-SCORE
             WHEN 'RETAIL'
               ADD 40 TO WS-RISK-SCORE
           END-EVALUATE.

      * High-risk postcode check
           IF LS-POSTCODE(1:2) = 'FL' OR
              LS-POSTCODE(1:2) = 'CR'
             ADD 30 TO WS-RISK-SCORE
           END-IF.

      * Set status based on calculated risk
           IF WS-RISK-SCORE > 200
             MOVE 2 TO LS-STATUS
             MOVE 'High Risk Score - Manual Review Required' 
               TO LS-REJECT-REASON
           ELSE
             IF WS-RISK-SCORE > 150
               MOVE 1 TO LS-STATUS
               MOVE 'Medium Risk - Pending Review'
                 TO LS-REJECT-REASON
             ELSE
               MOVE 0 TO LS-STATUS
               MOVE SPACES TO LS-REJECT-REASON
             END-IF
           END-IF.

           MOVE WS-RISK-SCORE TO LS-RISK-SCORE.
           
           GOBACK.