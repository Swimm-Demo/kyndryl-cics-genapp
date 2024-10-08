---
title: LGDPOL01 Program Overview
---
This document will cover the <SwmToken path="base/src/lgdpol01.cbl" pos="11:6:6" line-data="       PROGRAM-ID. LGDPOL01.">`LGDPOL01`</SwmToken> program. We'll cover:

1. What the Program Does
2. Program Flow
3. Program Sections

## What the Program Does

The <SwmToken path="base/src/lgdpol01.cbl" pos="11:6:6" line-data="       PROGRAM-ID. LGDPOL01.">`LGDPOL01`</SwmToken> program is designed to handle the deletion of policy records in the IBM Db2 database. It initializes necessary working storage variables, checks the communication area (commarea) for validity, and processes the deletion request based on the request ID provided in the commarea. If the request ID is recognized, it calls another program, <SwmToken path="base/src/lgdpol01.cbl" pos="141:9:9" line-data="           EXEC CICS LINK PROGRAM(LGDPDB01)">`LGDPDB01`</SwmToken>, to perform the actual deletion of the policy record.

## Program Flow

The program flow of <SwmToken path="base/src/lgdpol01.cbl" pos="11:6:6" line-data="       PROGRAM-ID. LGDPOL01.">`LGDPOL01`</SwmToken> is as follows:

1. Initialize working storage variables.
2. Check if the commarea is received and valid.
3. Validate the length of the commarea.
4. Convert the request ID to uppercase.
5. Check if the request ID is recognized.
6. If recognized, call the <SwmToken path="base/src/lgdpol01.cbl" pos="141:9:9" line-data="           EXEC CICS LINK PROGRAM(LGDPDB01)">`LGDPDB01`</SwmToken> program to delete the policy record.
7. Return control to the caller.

```mermaid
graph TD
  A[Start] --> B[Initialize Working Storage]
  B --> C{Commarea Received?}
  C -->|No| D[Issue ABEND]
  C -->|Yes| E[Validate Commarea Length]
  E --> F[Convert Request ID to Uppercase]
  F --> G{Request ID Recognized?}
  G -->|No| H[Set Return Code to 99]
  G -->|Yes| I[Call LGDPDB01 to Delete Policy]
  I --> J[Return Control to Caller]

%% Swimm:
%% graph TD
%%   A[Start] --> B[Initialize Working Storage]
%%   B --> C{Commarea Received?}
%%   C -->|No| D[Issue ABEND]
%%   C -->|Yes| E[Validate Commarea Length]
%%   E --> F[Convert Request ID to Uppercase]
%%   F --> G{Request ID Recognized?}
%%   G -->|No| H[Set Return Code to 99]
%%   G -->|Yes| I[Call <SwmToken path="base/src/lgdpol01.cbl" pos="141:9:9" line-data="           EXEC CICS LINK PROGRAM(LGDPDB01)">`LGDPDB01`</SwmToken> to Delete Policy]
%%   I --> J[Return Control to Caller]
```

<SwmSnippet path="/base/src/lgdpol01.cbl" line="78">

---

### MAINLINE SECTION

First, the MAINLINE SECTION initializes the working storage variables and sets up general variables using values from the EXEC Interface Block (EIB). It then checks if the commarea is received and valid. If not, it issues an ABEND. If the commarea is valid, it initializes the commarea return code to zero and validates the length of the commarea.

```cobol
       MAINLINE SECTION.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
      * initialize working storage variables
           INITIALIZE WS-HEADER.
      * set up general variable
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * Check commarea and obtain required details                     *
      *----------------------------------------------------------------*
      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('LGCA') NODUMP END-EXEC
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgdpol01.cbl" line="139">

---

### <SwmToken path="base/src/lgdpol01.cbl" pos="139:1:7" line-data="       DELETE-POLICY-DB2-INFO.">`DELETE-POLICY-DB2-INFO`</SwmToken>

Now, the <SwmToken path="base/src/lgdpol01.cbl" pos="139:1:7" line-data="       DELETE-POLICY-DB2-INFO.">`DELETE-POLICY-DB2-INFO`</SwmToken> section is executed if the request ID is recognized. This section calls the <SwmToken path="base/src/lgdpol01.cbl" pos="141:9:9" line-data="           EXEC CICS LINK PROGRAM(LGDPDB01)">`LGDPDB01`</SwmToken> program to delete the policy record from the Db2 database using the commarea.

```cobol
       DELETE-POLICY-DB2-INFO.

           EXEC CICS LINK PROGRAM(LGDPDB01)
                Commarea(DFHCOMMAREA)
                LENGTH(32500)
           END-EXEC.

           EXIT.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgdpol01.cbl" line="149">

---

### <SwmToken path="base/src/lgdpol01.cbl" pos="154:1:5" line-data="       WRITE-ERROR-MESSAGE.">`WRITE-ERROR-MESSAGE`</SwmToken>

Then, the <SwmToken path="base/src/lgdpol01.cbl" pos="154:1:5" line-data="       WRITE-ERROR-MESSAGE.">`WRITE-ERROR-MESSAGE`</SwmToken> section is responsible for writing error messages to the TD QUEUE (CSMT). It formats the current date and time, and writes the error message along with the commarea data to the TDQ.

```cobol
      *================================================================*
      * Procedure to write error message to TD QUEUE(CSMT)             *
      *   message will include Date, Time, Program Name, Customer      *
      *   Number, Policy Number and SQLCODE.                           *
      *================================================================*
       WRITE-ERROR-MESSAGE.
      * Save SQLCODE in message
      * Obtain and format current time and date
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME ABSTIME(Ws-ABSTIME)
                     MMDDYYYY(WS-DATE)
                     TIME(WS-TIME)
           END-EXEC
           MOVE WS-DATE TO EM-DATE
           MOVE WS-TIME TO EM-TIME
      * Write output message to TDQ
           EXEC CICS LINK PROGRAM('LGSTSQ')
                     COMMAREA(ERROR-MSG)
                     LENGTH(LENGTH OF ERROR-MSG)
           END-EXEC.
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](/)</sup></SwmMeta>
