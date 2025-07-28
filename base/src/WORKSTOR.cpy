       01  WS-IN-STAT                  PIC X(2).
           88 INPUT-OK                 VALUE '00'.
           88 INPUT-EOF                VALUE '10'.
           
       01  WS-OUT-STAT                 PIC X(2).
           88 OUTPUT-OK                VALUE '00'.

       01  WS-REC-CNT                  PIC 9(6) VALUE ZERO.
       01  WS-ERR-CNT                  PIC 9(6) VALUE ZERO.
       01  WS-PROC-CNT                 PIC 9(6) VALUE ZERO.

       01  WS-RISK-SCR                 PIC 999 VALUE ZERO.
       01  WS-DISC-FACT                PIC V99 VALUE 1.00.
       01  WS-STAT                     PIC 9 VALUE 0.
       01  WS-REJ-RSN                  PIC X(50).
       01  WS-STAT-DESC                PIC X(20).

       01  WS-FR-PREM                  PIC 9(8)V99.
       01  WS-CR-PREM                  PIC 9(8)V99.
       01  WS-FL-PREM                  PIC 9(8)V99.
       01  WS-WE-PREM                  PIC 9(8)V99.
       01  WS-TOT-PREM                 PIC 9(9)V99.

       01  WS-ERR-MSG                  PIC X(100). 