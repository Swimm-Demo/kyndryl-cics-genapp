---
title: Inquiring Policy (LGIPVS01)
---
The Inquiry Insurance Policy Details (MAINLINE) document describes the process of retrieving a random policy or customer number from the VSAM KSDS Policy file based on the input parameter of policy type. This is achieved by assigning system and program identifiers, clearing residual data, and determining the context in which the program is running.

For example, if the input parameter specifies a particular policy type, the program will return a corresponding random policy or customer number.

# Inquiry Insurance Policy Details (MAINLINE)

<SwmSnippet path="/base/src/lgipvs01.cbl" line="75">

---

### Assigning System and Program Identifiers

Going into the MAINLINE section, the code assigns system and program identifiers. It starts by clearing the <SwmToken path="base/src/lgipvs01.cbl" pos="77:7:9" line-data="           MOVE SPACES TO WS-RECV.">`WS-RECV`</SwmToken> variable to ensure no residual data is present. Then, it assigns the system ID to <SwmToken path="base/src/lgipvs01.cbl" pos="79:9:11" line-data="           EXEC CICS ASSIGN SYSID(WS-SYSID)">`WS-SYSID`</SwmToken>, the start code to <SwmToken path="base/src/lgipvs01.cbl" pos="83:9:11" line-data="           EXEC CICS ASSIGN STARTCODE(WS-STARTCODE)">`WS-STARTCODE`</SwmToken>, and the invoking program name to <SwmToken path="base/src/lgipvs01.cbl" pos="87:9:11" line-data="           EXEC CICS ASSIGN Invokingprog(WS-Invokeprog)">`WS-Invokeprog`</SwmToken>. These assignments are crucial for determining the context in which the program is running and for handling subsequent logic based on these values.

```cobol
       MAINLINE SECTION.
      *
           MOVE SPACES TO WS-RECV.

           EXEC CICS ASSIGN SYSID(WS-SYSID)
                RESP(WS-RESP)
           END-EXEC.

           EXEC CICS ASSIGN STARTCODE(WS-STARTCODE)
                RESP(WS-RESP)
           END-EXEC.

           EXEC CICS ASSIGN Invokingprog(WS-Invokeprog)
                RESP(WS-RESP)
           END-EXEC.
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](/)</sup></SwmMeta>
