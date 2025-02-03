---
title: Commercial Property Policy Management
---
The Commercial Property Policy Management System provides a streamlined process for handling commercial insurance policies. The system implements sophisticated business rules for risk assessment and premium calculations based on property characteristics and selected perils (Fire, Crime, Flood, and Weather). Through its 3270-style interface, users can inquire about existing policies, add new policies, or delete policies.

## Screen preview

The screen looks roughly like this:

<p align="center"><img src="/.swm/images/%7BDBF68077-89A9-433F-AE3C-3DA28EA38A34%7D-2025-1-3-8-11-48-610.png"></p>

## Add Policy Business Logic Flow

```mermaid
flowchart TD
    A[LGAPDB01]:::a4f5fbfea  -->|CICS LINK| B[LGRKS01]:::a261ec6c4
    
    subgraph Risk Assessment - LGRSK01
        B --> C[Base Risk Score: 100]
        C --> D{Property Type}
        D -->|Warehouse| E[+50 Risk]
        D -->|Factory| F[+75 Risk]
        D -->|Office| G[+25 Risk]
        D -->|Retail| H[+40 Risk]

        E & F & G & H --> I{High Risk Postcode?}
        I -->|Yes| J[+30 Risk]
        I -->|No| L
        J --> L[Final Risk Score]
        L --> M{Risk Level?}
        M -->|>200| N[Set Status: Manual Review]
        M -->|151-200| O[Set Status: Pending Review]
        M -->|≤150| P[Set Status: Auto-Approved]
    end

    N & O & P -->|Return Risk Score & Status| Q[LGAPDB01]:::a4f5fbfea 
    Q -->|CICS LINK| R[LGPRM01]:::a0b8c7d0e 

    subgraph Premium Calculation - LGPRM01
        R --> S[Calculate Base Premiums]
        S --> T[Fire: Risk x 0.8]
        S --> U[Crime: Risk x 0.6]
        S --> V[Flood: Risk x 1.2]
        S --> W[Weather: Risk x 0.9]
        T & U & V & W --> X{All Perils Selected?}
        X -->|Yes| Y[Apply 10% Discount]
        X -->|No| Z[No Discount]
    end

    Y & Z -->|Return Premium Values| AA[LGAPDB01]:::a4f5fbfea 
    AA --> AB[Write to DB2]
    AA --> AC[Write to VSAM]


classDef a0b8c7d0e color:#000000,fill:#7CB9F4
classDef a261ec6c4 color:#000000,fill:#00FFAA
classDef a4f5fbfea color:#000000,fill:#00FFF4
classDef a4271316e color:#000000,fill:#FFFF00
classDef a0298c687 color:#000000,fill:#AA7CB9
classDef a911b8d83 color:#000000,fill:#5afa0a
 

%% Swimm:
%% flowchart TD
%%     A[<SwmToken path="/base/src/lgapdb01.cbl" pos="13:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken>]:::a4f5fbfea  -->|CICS LINK| B[<SwmToken path="/base/src/lgapdb01.cbl" pos="141:19:19" line-data="           05 WS-RISK-PROG            PIC X(8) VALUE &#39;LGRKS01&#39;.">`LGRKS01`</SwmToken>]:::a261ec6c4
%%     
%%     subgraph Risk Assessment - LGRSK01
%%         B --> C[Base Risk Score: <SwmToken path="/base/src/lgrsk01.cbl" pos="18:3:3" line-data="           MOVE 100 TO WS-RISK-SCORE.">`100`</SwmToken>]
%%         C --> D{Property Type}
%%         D -->|Warehouse| E[+<SwmToken path="/base/src/lgrsk01.cbl" pos="23:3:3" line-data="               ADD 50 TO WS-RISK-SCORE">`50`</SwmToken> Risk]
%%         D -->|Factory| F[+<SwmToken path="/base/src/lgrsk01.cbl" pos="25:3:3" line-data="               ADD 75 TO WS-RISK-SCORE">`75`</SwmToken> Risk]
%%         D -->|Office| G[+<SwmToken path="/base/src/lgrsk01.cbl" pos="27:3:3" line-data="               ADD 25 TO WS-RISK-SCORE">`25`</SwmToken> Risk]
%%         D -->|Retail| H[+<SwmToken path="/base/src/lgrsk01.cbl" pos="29:3:3" line-data="               ADD 40 TO WS-RISK-SCORE">`40`</SwmToken> Risk]
%% 
%%         E & F & G & H --> I{High Risk Postcode?}
%%         I -->|Yes| J[+30 Risk]
%%         I -->|No| L
%%         J --> L[Final Risk Score]
%%         L --> M{Risk Level?}
%%         M -->|><SwmToken path="/base/src/lgrsk01.cbl" pos="39:11:11" line-data="           IF WS-RISK-SCORE &gt; 200">`200`</SwmToken>| N[Set Status: Manual Review]
%%         M -->|151-<SwmToken path="/base/src/lgrsk01.cbl" pos="39:11:11" line-data="           IF WS-RISK-SCORE &gt; 200">`200`</SwmToken>| O[Set Status: Pending Review]
%%         M -->|≤<SwmToken path="/base/src/lgrsk01.cbl" pos="44:11:11" line-data="             IF WS-RISK-SCORE &gt; 150">`150`</SwmToken>| P[Set Status: Auto-Approved]
%%     end
%% 
%%     N & O & P -->|Return Risk Score & Status| Q[<SwmToken path="/base/src/lgapdb01.cbl" pos="13:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken>]:::a4f5fbfea 
%%     Q -->|CICS LINK| R[<SwmToken path="/base/src/lgprm01.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGPRM01.">`LGPRM01`</SwmToken>]:::a0b8c7d0e 
%% 
%%     subgraph Premium Calculation - <SwmToken path="/base/src/lgprm01.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGPRM01.">`LGPRM01`</SwmToken>
%%         R --> S[Calculate Base Premiums]
%%         S --> T[Fire: Risk x <SwmToken path="/base/src/lgprm01.cbl" pos="32:11:13" line-data="             ((LS-RISK-SCORE * 0.8) * LS-FIRE-PERIL *">`0.8`</SwmToken>]
%%         S --> U[Crime: Risk x <SwmToken path="/base/src/lgprm01.cbl" pos="36:11:13" line-data="             ((LS-RISK-SCORE * 0.6) * LS-CRIME-PERIL *">`0.6`</SwmToken>]
%%         S --> V[Flood: Risk x <SwmToken path="/base/src/lgprm01.cbl" pos="40:11:13" line-data="             ((LS-RISK-SCORE * 1.2) * LS-FLOOD-PERIL *">`1.2`</SwmToken>]
%%         S --> W[Weather: Risk x <SwmToken path="/base/src/lgprm01.cbl" pos="44:11:13" line-data="             ((LS-RISK-SCORE * 0.9) * LS-WEATHER-PERIL *">`0.9`</SwmToken>]
%%         T & U & V & W --> X{All Perils Selected?}
%%         X -->|Yes| Y[Apply 10% Discount]
%%         X -->|No| Z[No Discount]
%%     end
%% 
%%     Y & Z -->|Return Premium Values| AA[<SwmToken path="/base/src/lgapdb01.cbl" pos="13:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken>]:::a4f5fbfea 
%%     AA --> AB[Write to <SwmToken path="/base/src/lgapdb01.cbl" pos="577:3:3" line-data="       INSERT-DB2-RECORD.">`DB2`</SwmToken>]
%%     AA --> AC[Write to VSAM]
%% 
%% 
%% classDef a0b8c7d0e color:#000000,fill:#7CB9F4
%% classDef a261ec6c4 color:#000000,fill:#00FFAA
%% classDef a4f5fbfea color:#000000,fill:#00FFF4
%% classDef a4271316e color:#000000,fill:#FFFF00
%% classDef a0298c687 color:#000000,fill:#AA7CB9
%% classDef a911b8d83 color:#000000,fill:#5afa0a
%%  
```

## Technical Flow Diagram

In high level, these are the main components:

```mermaid
flowchart TD
    A[SSMAPP4 Screen - 3270 Interface] -->|Screen Input| B[LGTESTP4 - Screen Handler]
    B -->|CICS LINK| C[LGAPOL01 - Business Logic Orchestrator]
    C -->|CICS LINK| D[LGAPDB01 - DB2/Business Rules Handler]:::a4f5fbfea 
    
        D -->|CICS LINK| E[LGRSK01 - Risk Assessment Calculator]:::a261ec6c4
        E -->|Return Risk Score| D
        D -->|CICS LINK| F[LGPRM01 - Premium Calculator]:::a0b8c7d0e
        F -->|Return Premiums| D
        D -->|After Calculations| G[DB2 INSERT]
        D -->|CICS LINK| H[LGAPVS01 - VSAM File Handler]
        H -->|Write to VSAM| I[VSAM Storage]
  
    
    G -->|Store in| J[(DB2 Database)]
    classDef a0b8c7d0e color:#000000,fill:#7CB9F4
classDef a261ec6c4 color:#000000,fill:#00FFAA
classDef a4f5fbfea color:#000000,fill:#00FFF4
classDef a4271316e color:#000000,fill:#FFFF00
classDef a0298c687 color:#000000,fill:#AA7CB9
classDef a911b8d83 color:#000000,fill:#5afa0a

%% Swimm:
%% flowchart TD
%%     A[<SwmToken path="/base/src/lgtestp4.cbl" pos="52:11:11" line-data="           EXEC CICS SEND MAP (&#39;SSMAPP4&#39;)">`SSMAPP4`</SwmToken> Screen - 3270 Interface] -->|Screen Input| B[<SwmToken path="/base/src/lgtestp4.cbl" pos="11:6:6" line-data="       PROGRAM-ID. LGTESTP4.">`LGTESTP4`</SwmToken> - Screen Handler]
%%     B -->|CICS LINK| C[<SwmToken path="/base/src/lgtestp4.cbl" pos="178:10:10" line-data="                 EXEC CICS LINK PROGRAM(&#39;LGAPOL01&#39;)">`LGAPOL01`</SwmToken> - Business Logic Orchestrator]
%%     C -->|CICS LINK| D[<SwmToken path="/base/src/lgapdb01.cbl" pos="13:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken> - DB2/Business Rules Handler]:::a4f5fbfea 
%%     
%%         D -->|CICS LINK| E[LGRSK01 - Risk Assessment Calculator]:::a261ec6c4
%%         E -->|Return Risk Score| D
%%         D -->|CICS LINK| F[<SwmToken path="/base/src/lgprm01.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGPRM01.">`LGPRM01`</SwmToken> - Premium Calculator]:::a0b8c7d0e
%%         F -->|Return Premiums| D
%%         D -->|After Calculations| G[<SwmToken path="/base/src/lgapdb01.cbl" pos="577:3:3" line-data="       INSERT-DB2-RECORD.">`DB2`</SwmToken> INSERT]
%%         D -->|CICS LINK| H[LGAPVS01 - VSAM File Handler]
%%         H -->|Write to VSAM| I[VSAM Storage]
%%   
%%     
%%     G -->|Store in| J[(DB2 Database)]
%%     classDef a0b8c7d0e color:#000000,fill:#7CB9F4
%% classDef a261ec6c4 color:#000000,fill:#00FFAA
%% classDef a4f5fbfea color:#000000,fill:#00FFF4
%% classDef a4271316e color:#000000,fill:#FFFF00
%% classDef a0298c687 color:#000000,fill:#AA7CB9
%% classDef a911b8d83 color:#000000,fill:#5afa0a
```

## Technical Flow Code Walkthrough

### Reading Information and Orchestrating

<SwmSnippet path="/base/src/lgtestp4.cbl" line="156">

---

Within the Screen Handler, when the user selects <SwmToken path="/base/src/lgtestp4.cbl" pos="156:4:4" line-data="             WHEN &#39;2&#39;">`2`</SwmToken> (<SwmToken path="/base/src/ssmap.bms" pos="457:7:9" line-data="               INITIAL=&#39;2. Policy Add     &#39;">`Policy Add`</SwmToken>), the fields provided by the user are set:

```cobol
             WHEN '2'
                 Move '01ACOM'             To  CA-REQUEST-ID
                 Move ENP4CNOO             To  CA-CUSTOMER-NUM
                 Move ENP4IDAO             To  CA-ISSUE-DATE
                 Move ENP4EDAO             To  CA-EXPIRY-DATE
                 Move ENP4ADDO             To  CA-B-Address
                 Move ENP4HPCO             To  CA-B-Postcode
                 Move ENP4LATO             To  CA-B-Latitude
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgtestp4.cbl" line="178">

---

Then <SwmToken path="/base/src/lgtestp4.cbl" pos="178:10:10" line-data="                 EXEC CICS LINK PROGRAM(&#39;LGAPOL01&#39;)">`LGAPOL01`</SwmToken> (the Business Logic Orchestrator) is invoked:

```cobol
                 EXEC CICS LINK PROGRAM('LGAPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapol01.cbl" line="121">

---

<SwmToken path="/base/src/lgtestp4.cbl" pos="178:10:10" line-data="                 EXEC CICS LINK PROGRAM(&#39;LGAPOL01&#39;)">`LGAPOL01`</SwmToken> acts as an intermediary between the screen handler (<SwmToken path="/base/src/lgtestp4.cbl" pos="11:6:6" line-data="       PROGRAM-ID. LGTESTP4.">`LGTESTP4`</SwmToken>) and the DB2/business logic handler (<SwmToken path="/base/src/lgapdb01.cbl" pos="13:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken>). It delegates the actual work to <SwmToken path="/base/src/lgapdb01.cbl" pos="13:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken>:

```cobol
           EXEC CICS Link Program(LGAPDB01)
                Commarea(DFHCOMMAREA)
                LENGTH(32500)
           END-EXEC.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb01.cbl" line="512">

---

Here we initialize the business data and then call risk assessment:

```cobol
       INSERT-COMMERCIAL SECTION.
           PERFORM INITIALIZE-BUSINESS-DATA
           PERFORM CALL-RISK-ASSESSMENT
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb01.cbl" line="525">

---

Initializing the data means propogating the information provided from the screen:

```cobol
       INITIALIZE-BUSINESS-DATA.
           MOVE CA-B-PropType  TO WS-PROP-TYPE
           MOVE CA-B-Postcode  TO WS-POSTCODE
           
           MOVE CA-B-FirePeril    TO WS-FIRE-PERIL
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb01.cbl" line="535">

---

And calling the Risk Assessment program:

```cobol
       CALL-RISK-ASSESSMENT.
           EXEC CICS LINK
                PROGRAM(WS-RISK-PROG)
                COMMAREA(WS-RISK-DATA)
                LENGTH(LENGTH OF WS-RISK-DATA)
                RESP(WS-RESP)
                RESP2(WS-RESP2)
           END-EXEC
```

---

</SwmSnippet>

### Risk Assessment

<SwmSnippet path="/base/src/lgrsk01.cbl" line="18">

---

Where the property type is evaluated, and the risk factor is assigned accordingly:

```cobol
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
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgrsk01.cbl" line="32">

---

In case of a high-risk post code, there is an additional risk factor:

```cobol
      * High-risk postcode check
           IF LS-POSTCODE(1:2) = 'FL' OR
              LS-POSTCODE(1:2) = 'CR'
             ADD 30 TO WS-RISK-SCORE
           END-IF.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgrsk01.cbl" line="39">

---

The risk score leads to accepting or rejecting:

```cobol
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
```

---

</SwmSnippet>

### Premium Calculation

<SwmSnippet path="/base/src/lgapdb01.cbl" line="554">

---

After calculating the risk score, it is provided as input to the Premium Calculation program:

```cobol
       CALL-PREMIUM-CALCULATION.
           MOVE WS-RISK-SCORE TO WS-PREMIUM-DATA
      
           EXEC CICS LINK
                PROGRAM(WS-PREMIUM-PROG)
                COMMAREA(WS-PREMIUM-DATA)
                LENGTH(LENGTH OF WS-PREMIUM-DATA)
                RESP(WS-RESP)
                RESP2(WS-RESP2)
           END-EXEC
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgprm01.cbl" line="6">

---

The default discount factor is <SwmToken path="/base/src/lgprm01.cbl" pos="6:15:17" line-data="       01  WS-DISCOUNT-FACTOR         PIC V99 VALUE 1.00.">`1.00`</SwmToken> - that is, no discount.

```cobol
       01  WS-DISCOUNT-FACTOR         PIC V99 VALUE 1.00.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgprm01.cbl" line="23">

---

If all perils are selected - a discount is provided:

```cobol
           IF LS-FIRE-PERIL > 0 AND
              LS-CRIME-PERIL > 0 AND
              LS-FLOOD-PERIL > 0 AND
              LS-WEATHER-PERIL > 0
             MOVE 0.90 TO WS-DISCOUNT-FACTOR
           END-IF.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgprm01.cbl" line="31">

---

The Premium is calculated based on the risk:

```cobol
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
```

---

</SwmSnippet>

### Writing to Storage

<SwmSnippet path="/base/src/lgapdb01.cbl" line="577">

---

The Premium is written to <SwmToken path="/base/src/lgapdb01.cbl" pos="577:3:3" line-data="       INSERT-DB2-RECORD.">`DB2`</SwmToken>:

```cobol
       INSERT-DB2-RECORD.
      * Convert commarea values to DB2 integer format
           MOVE CA-B-FirePeril     TO DB2-B-FirePeril-Int
           MOVE CA-B-FirePremium   TO DB2-B-FirePremium-Int
           MOVE CA-B-CrimePeril    TO DB2-B-CrimePeril-Int
           MOVE CA-B-CrimePremium  TO DB2-B-CrimePremium-Int
           MOVE CA-B-FloodPeril    TO DB2-B-FloodPeril-Int
           MOVE CA-B-FloodPremium  TO DB2-B-FloodPremium-Int
           MOVE CA-B-WeatherPeril  TO DB2-B-WeatherPeril-Int
           MOVE CA-B-WeatherPremium TO DB2-B-WeatherPremium-Int
           MOVE CA-B-Status        TO DB2-B-Status-Int
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb01.cbl" line="269">

---

And to VSAM by calling <SwmToken path="/base/src/lgapdb01.cbl" pos="269:9:9" line-data="             EXEC CICS Link Program(LGAPVS01)">`LGAPVS01`</SwmToken>:

```cobol
             EXEC CICS Link Program(LGAPVS01)
                  Commarea(DFHCOMMAREA)
                LENGTH(32500)
             END-EXEC.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapvs01.cbl" line="110">

---

We set all parameters as calculated by the Risk Assessment and Premium Calculation programs:

```cobol
             When 'C'
               Move CA-B-Postcode     To WF-B-Postcode
               Move CA-B-Status       To WF-B-Status
               Move CA-B-Customer     To WF-B-Customer
               Move WS-RISK-SCORE     To WF-B-Risk-Score
               Move CA-B-FirePremium  To WF-B-Fire-Premium
               Move CA-B-CrimePremium To WF-B-Crime-Premium
               Move CA-B-FloodPremium To WF-B-Flood-Premium
               Move CA-B-WeatherPremium To WF-B-Weather-Premium
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapvs01.cbl" line="145">

---

The information is then written to VSAM (the file <SwmToken path="/base/src/lgapvs01.cbl" pos="145:10:10" line-data="           Exec CICS Write File(&#39;KSDSPOLY&#39;)">`KSDSPOLY`</SwmToken>):

```cobol
           Exec CICS Write File('KSDSPOLY')
                     From(WF-Policy-Info)
                     Length(104)
                     Ridfld(WF-Policy-Key)
                     KeyLength(21)
                     RESP(WS-RESP)
           End-Exec.
```

---

</SwmSnippet>

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
