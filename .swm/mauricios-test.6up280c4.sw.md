---
title: Mauricio's test
---
# Introduction

This document will walk you through the implementation details of a specific code change. The purpose of this change is to handle a CICS counter retrieval operation within a COBOL program.

We will cover:

1. Why we use the CICS Get Counter command.
2. How the retrieved counter value is utilized in the program.

# Using the CICS Get Counter command

<SwmSnippet path="/base/src/lgacdb01.cbl" line="201">

---

The CICS Get Counter command is used to retrieve a unique counter value from a specified pool. This is crucial for generating unique identifiers or sequence numbers within the application. In this case, the command fetches the last customer number from the <SwmToken path="/base/src/lgacdb01.cbl" pos="202:3:3" line-data="                         Pool(GENApool)">`GENApool`</SwmToken> and stores it in the <SwmToken path="/base/src/lgacdb01.cbl" pos="203:3:3" line-data="                         Value(LastCustNum)">`LastCustNum`</SwmToken> variable. The response is captured in <SwmToken path="/base/src/lgacdb01.cbl" pos="204:3:5" line-data="                         Resp(WS-RESP)">`WS-RESP`</SwmToken> to handle any potential errors or exceptions.

```
           Exec CICS Get Counter(GENAcount)
                         Pool(GENApool)
                         Value(LastCustNum)
                         Resp(WS-RESP)
           End-Exec.
```

---

</SwmSnippet>

This approach ensures that the application can consistently generate unique customer numbers, which is essential for maintaining data integrity and avoiding duplication.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
