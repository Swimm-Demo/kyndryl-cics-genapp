---
title: Deleting Policy Information (LGDPDB01 program)
---
This document will cover the <SwmToken path="base/src/lgdpdb01.cbl" pos="13:6:6" line-data="       PROGRAM-ID. LGDPDB01.">`LGDPDB01`</SwmToken> program. We'll cover:

1. What the Program Does
2. Program Flow
3. Program Sections

## What the Program Does

The <SwmToken path="base/src/lgdpdb01.cbl" pos="13:6:6" line-data="       PROGRAM-ID. LGDPDB01.">`LGDPDB01`</SwmToken> program is designed to delete policy information from the <SwmToken path="base/src/lgdpdb01.cbl" pos="124:5:5" line-data="      * initialize DB2 host variables">`DB2`</SwmToken> database. It removes the appropriate row from the <SwmToken path="base/src/lgdpdb01.cbl" pos="124:5:5" line-data="      * initialize DB2 host variables">`DB2`</SwmToken> Policy table and ensures that the deletion is propagated to the Endowment, House, Motor, or Commercial tables if necessary. The program checks the request ID in the communication area (commarea) and performs the deletion if the request ID is recognized.

## Program Flow

The program follows these high-level steps:

1. Initialize working storage and <SwmToken path="base/src/lgdpdb01.cbl" pos="124:5:5" line-data="      * initialize DB2 host variables">`DB2`</SwmToken> host variables.
2. Check if the commarea is received and is of sufficient length.
3. Convert customer and policy numbers from the commarea to <SwmToken path="base/src/lgdpdb01.cbl" pos="124:5:5" line-data="      * initialize DB2 host variables">`DB2`</SwmToken> integer format.
4. Check the request ID in the commarea.
5. If the request ID is recognized, delete the policy information from the <SwmToken path="base/src/lgdpdb01.cbl" pos="124:5:5" line-data="      * initialize DB2 host variables">`DB2`</SwmToken> database.
6. Call the <SwmToken path="base/src/lgdpdb01.cbl" pos="59:3:3" line-data="       01 LGDPVS01                  PIC x(8) Value &#39;LGDPVS01&#39;.">`LGDPVS01`</SwmToken> program to propagate the deletion.
7. Return to the caller.

```mermaid
graph TD
  A[Start] --> B[Initialize Variables]
  B --> C{Commarea Received?}
  C -->|No| D[Issue ABEND]
  C -->|Yes| E[Check Commarea Length]
  E --> F{Commarea Length Sufficient?}
  F -->|No| G[Set Return Code and Exit]
  F -->|Yes| H[Convert Customer and Policy Numbers]
  H --> I[Check Request ID]
  I -->|Not Recognized| J[Set Return Code and Exit]
  I -->|Recognized| K[Delete Policy from DB2]
  K --> L[Call LGDPVS01 Program]
  L --> M[Return to Caller]

%% Swimm:
%% graph TD
%%   A[Start] --> B[Initialize Variables]
%%   B --> C{Commarea Received?}
%%   C -->|No| D[Issue ABEND]
%%   C -->|Yes| E[Check Commarea Length]
%%   E --> F{Commarea Length Sufficient?}
%%   F -->|No| G[Set Return Code and Exit]
%%   F -->|Yes| H[Convert Customer and Policy Numbers]
%%   H --> I[Check Request ID]
%%   I -->|Not Recognized| J[Set Return Code and Exit]
%%   I -->|Recognized| K[Delete Policy from DB2]
%%   K --> L[Call <SwmToken path="base/src/lgdpdb01.cbl" pos="59:3:3" line-data="       01 LGDPVS01                  PIC x(8) Value &#39;LGDPVS01&#39;.">`LGDPVS01`</SwmToken> Program]
%%   L --> M[Return to Caller]
```

<SwmSnippet path="/base/src/lgdpdb01.cbl" line="111">

---

### MAINLINE SECTION

First, the MAINLINE SECTION initializes working storage variables and <SwmToken path="base/src/lgdpdb01.cbl" pos="124:5:5" line-data="      * initialize DB2 host variables">`DB2`</SwmToken> host variables. It then checks if the commarea is received and is of sufficient length. If the commarea is valid, it converts the customer and policy numbers from the commarea to <SwmToken path="base/src/lgdpdb01.cbl" pos="124:5:5" line-data="      * initialize DB2 host variables">`DB2`</SwmToken> integer format. Finally, it checks the request ID in the commarea and performs the appropriate action based on the request ID.

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

      * initialize DB2 host variables
           INITIALIZE DB2-IN-INTEGERS.

      *----------------------------------------------------------------*
      * Check commarea and obtain required details                     *
      *----------------------------------------------------------------*
      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgdpdb01.cbl" line="186">

---

### <SwmToken path="base/src/lgdpdb01.cbl" pos="186:1:7" line-data="       DELETE-POLICY-DB2-INFO.">`DELETE-POLICY-DB2-INFO`</SwmToken>

Next, the <SwmToken path="base/src/lgdpdb01.cbl" pos="186:1:7" line-data="       DELETE-POLICY-DB2-INFO.">`DELETE-POLICY-DB2-INFO`</SwmToken> section deletes the appropriate row from the <SwmToken path="base/src/lgdpdb01.cbl" pos="186:5:5" line-data="       DELETE-POLICY-DB2-INFO.">`DB2`</SwmToken> Policy table. It constructs and executes the SQL DELETE statement to remove the policy information. If the SQLCODE is not zero, it sets the return code to '90' and performs the <SwmToken path="base/src/lgdpdb01.cbl" pos="200:3:7" line-data="               PERFORM WRITE-ERROR-MESSAGE">`WRITE-ERROR-MESSAGE`</SwmToken> section.

```cobol
       DELETE-POLICY-DB2-INFO.

           MOVE ' DELETE POLICY  ' TO EM-SQLREQ
           EXEC SQL
             DELETE
               FROM POLICY
               WHERE ( CUSTOMERNUMBER = :DB2-CUSTOMERNUM-INT AND
                       POLICYNUMBER  = :DB2-POLICYNUM-INT      )
           END-EXEC

      *    Treat SQLCODE 0 and SQLCODE 100 (record not found) as
      *    successful - end result is record does not exist
           IF SQLCODE NOT EQUAL 0 Then
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-IF.

           EXIT.

```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgdpdb01.cbl" line="212">

---

### <SwmToken path="base/src/lgdpdb01.cbl" pos="212:1:5" line-data="       WRITE-ERROR-MESSAGE.">`WRITE-ERROR-MESSAGE`</SwmToken>

Then, the <SwmToken path="base/src/lgdpdb01.cbl" pos="212:1:5" line-data="       WRITE-ERROR-MESSAGE.">`WRITE-ERROR-MESSAGE`</SwmToken> section writes an error message to the queues. It includes the date, time, program name, customer number, policy number, and SQLCODE in the message. It calls the LGSTSQ program to handle the message and write it to the Transient Data Queue (TDQ) and Temporary Storage Queue (TSQ).

```cobol
       WRITE-ERROR-MESSAGE.
      * Save SQLCODE in message
           MOVE SQLCODE TO EM-SQLRC
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
      * Write 90 bytes or as much as we have of commarea to TDQ
           IF EIBCALEN > 0 THEN
             IF EIBCALEN < 91 THEN
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CA-DATA
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](/)</sup></SwmMeta>
