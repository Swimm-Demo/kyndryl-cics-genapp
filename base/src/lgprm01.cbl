       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGPREM01.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DISCOUNT-FACTOR         PIC V99 VALUE 1.00.
       
       LINKAGE SECTION.
       01  LS-PREMIUM-DATA.
           05 LS-RISK-SCORE           PIC 999.
           05 LS-FIRE-PERIL           PIC 9(4).
           05 LS-FIRE-PREMIUM         PIC 9(8).
           05 LS-CRIME-PERIL          PIC 9(4).
           05 LS-CRIME-PREMIUM        PIC 9(8).
           05 LS-FLOOD-PERIL          PIC 9(4).
           05 LS-FLOOD-PREMIUM        PIC 9(8).
           05 LS-WEATHER-PERIL        PIC 9(4).
           05 LS-WEATHER-PREMIUM      PIC 9(8).
      
       PROCEDURE DIVISION USING LS-PREMIUM-DATA.
      
      * Check for multi-peril discount
           IF LS-FIRE-PERIL > 0 AND
              LS-CRIME-PERIL > 0 AND
              LS-FLOOD-PERIL > 0 AND
              LS-WEATHER-PERIL > 0
             MOVE 0.90 TO WS-DISCOUNT-FACTOR
           END-IF.

      * Calculate individual peril premiums
           COMPUTE LS-FIRE-PREMIUM =
             ((LS-RISK-SCORE * 0.8) * LS-FIRE-PERIL *
               WS-DISCOUNT-FACTOR).
           
           COMPUTE LS-CRIME-PREMIUM =
             ((LS-RISK-SCORE * 0.6) * LS-CRIME-PERIL *
               WS-DISCOUNT-FACTOR).
           
           COMPUTE LS-FLOOD-PREMIUM =
             ((LS-RISK-SCORE * 1.2) * LS-FLOOD-PERIL *
               WS-DISCOUNT-FACTOR).
           
           COMPUTE LS-WEATHER-PREMIUM =
             ((LS-RISK-SCORE * 0.9) * LS-WEATHER-PERIL *
               WS-DISCOUNT-FACTOR).
           
           GOBACK.
