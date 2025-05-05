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
    A[Start INSERT-COMMERCIAL] --> B[PROCESS-RISK-SCORE]:::a261ec6c4
    label>LGAPDB01 - Combined Business Rules & Storage Handler]:::a4f5fbfea
    subgraph Risk Score Calculation
        B --> C[Set Base Score: 100]
        C --> D{Property Type?}
        D -->|Warehouse| E[+050]
        D -->|Factory| F[+075]
        D -->|Office| G[+025]
        D -->|Retail| H[+040]
        E & F & G & H --> I{High Risk Postcode?}
        I -->|Yes FL/CR| J[+030]
        I -->|No| K[No Addition]
    end

    J & K --> L[DETERMINE-POLICY-STATUS]:::a4271316e 

    subgraph Status Determination
        L --> M{Final Risk Score}
        M -->|x> 200 | N[Status 2: Manual Review]
        M -->|150 < x < 200| O[Status 1: Pending Review]
        M -->|x≤ 150| P[Status 0: Auto-Approved]
    end

    N & O & P --> Q[CALC-PREMIUMS]:::a0b8c7d0e

    subgraph Premium Calculation
        Q --> R{Check All Perils}
        R -->|All Selected| S[Set Discount 0.90]
        R -->|Not All| T[No Discount]
        S & T --> U[Calculate Premiums]
        U --> V1[Fire: Risk x 0.80]
        U --> V2[Crime: Risk x 0.60]
        U --> V3[Flood: Risk x 1.20]
        U --> V4[Weather: Risk x 0.90]
    end

    V1 & V2 & V3 & V4 --> W[INSERT-DB2-RECORD]:::a0298c687
    W --> X[Store in DB2]
    W --> Y[Call LGAPVS01 to write to VSAM]

classDef a0b8c7d0e color:#000000,fill:#7CB9F4
classDef a261ec6c4 color:#000000,fill:#00FFAA
classDef a4f5fbfea color:#000000,fill:#00FFF4
classDef a4271316e color:#000000,fill:#FFFF00
classDef a0298c687 color:#000000,fill:#AA7CB9
classDef a911b8d83 color:#000000,fill:#5afa0a

%% Swimm:
%% flowchart TD
%%     A[Start INSERT-COMMERCIAL] --> B[<SwmToken path="/base/src/lgcomcal.cbl" pos="277:1:5" line-data="       PROCESS-RISK-SCORE.">`PROCESS-RISK-SCORE`</SwmToken>]:::a261ec6c4
%%     label><SwmToken path="/base/src/lgapdb01.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken> - Combined Business Rules & Storage Handler]:::a4f5fbfea
%%     subgraph Risk Score Calculation
%%         B --> C[Set Base Score: <SwmToken path="/base/src/lgcomcal.cbl" pos="115:18:18" line-data="           03 WS-TM-BASE               PIC 9(3) VALUE 100.">`100`</SwmToken>]
%%         C --> D{Property Type?}
%%         D -->|Warehouse| E[+<SwmToken path="/base/src/lgcomdat.cpy" pos="77:20:20" line-data="                   07  RMS-PF-W-VAL     PIC 9(3) VALUE 050.">`050`</SwmToken>]
%%         D -->|Factory| F[+<SwmToken path="/base/src/lgcomdat.cpy" pos="78:20:20" line-data="                   07  RMS-PF-F-VAL     PIC 9(3) VALUE 075.">`075`</SwmToken>]
%%         D -->|Office| G[+<SwmToken path="/base/src/lgcomdat.cpy" pos="79:20:20" line-data="                   07  RMS-PF-O-VAL     PIC 9(3) VALUE 025.">`025`</SwmToken>]
%%         D -->|Retail| H[+<SwmToken path="/base/src/lgcomdat.cpy" pos="80:20:20" line-data="                   07  RMS-PF-R-VAL     PIC 9(3) VALUE 040.">`040`</SwmToken>]
%%         E & F & G & H --> I{High Risk Postcode?}
%%         I -->|Yes FL/CR| J[+<SwmToken path="/base/src/lgcomdat.cpy" pos="86:20:20" line-data="                   07  RMS-GF-FL-VAL    PIC 9(3) VALUE 030.">`030`</SwmToken>]
%%         I -->|No| K[No Addition]
%%     end
%% 
%%     J & K --> L[<SwmToken path="/base/src/lgcomcal.cbl" pos="330:1:5" line-data="       DETERMINE-POLICY-STATUS.">`DETERMINE-POLICY-STATUS`</SwmToken>]:::a4271316e 
%% 
%%     subgraph Status Determination
%%         L --> M{Final Risk Score}
%%         M -->|x> <SwmToken path="/base/src/lgcomdat.cpy" pos="94:20:20" line-data="                   07  RMS-TH-L2-VAL    PIC 9(3) VALUE 200.">`200`</SwmToken> | N[Status 2: Manual Review]
%%         M -->|<SwmToken path="/base/src/lgcomdat.cpy" pos="93:20:20" line-data="                   07  RMS-TH-L1-VAL    PIC 9(3) VALUE 150.">`150`</SwmToken> < x < <SwmToken path="/base/src/lgcomdat.cpy" pos="94:20:20" line-data="                   07  RMS-TH-L2-VAL    PIC 9(3) VALUE 200.">`200`</SwmToken>| O[Status 1: Pending Review]
%%         M -->|x≤ <SwmToken path="/base/src/lgcomdat.cpy" pos="93:20:20" line-data="                   07  RMS-TH-L1-VAL    PIC 9(3) VALUE 150.">`150`</SwmToken>| P[Status 0: Auto-Approved]
%%     end
%% 
%%     N & O & P --> Q[CALC-PREMIUMS]:::a0b8c7d0e
%% 
%%     subgraph Premium Calculation
%%         Q --> R{Check All Perils}
%%         R -->|All Selected| S[Set Discount <SwmToken path="/base/src/lgcomdat.cpy" pos="106:15:17" line-data="           03  RMS-DISCOUNT-FACTOR      PIC V99 VALUE 0.90.">`0.90`</SwmToken>]
%%         R -->|Not All| T[No Discount]
%%         S & T --> U[Calculate Premiums]
%%         U --> V1[Fire: Risk x <SwmToken path="/base/src/lgcomdat.cpy" pos="102:17:19" line-data="                   07  RMS-PERF-F-VAL   PIC V99 VALUE 0.80.">`0.80`</SwmToken>]
%%         U --> V2[Crime: Risk x <SwmToken path="/base/src/lgcomdat.cpy" pos="103:17:19" line-data="                   07  RMS-PERF-C-VAL   PIC V99 VALUE 0.60.">`0.60`</SwmToken>]
%%         U --> V3[Flood: Risk x <SwmToken path="/base/src/lgcomdat.cpy" pos="104:17:19" line-data="                   07  RMS-PERF-FL-VAL  PIC V99 VALUE 1.20.">`1.20`</SwmToken>]
%%         U --> V4[Weather: Risk x <SwmToken path="/base/src/lgcomdat.cpy" pos="105:17:19" line-data="                   07  RMS-PERF-W-VAL   PIC V99 VALUE 0.90.">`0.90`</SwmToken>]
%%     end
%% 
%%     V1 & V2 & V3 & V4 --> W[INSERT-DB2-RECORD]:::a0298c687
%%     W --> X[Store in DB2]
%%     W --> Y[Call LGAPVS01 to write to VSAM]
%% 
%% classDef a0b8c7d0e color:#000000,fill:#7CB9F4
%% classDef a261ec6c4 color:#000000,fill:#00FFAA
%% classDef a4f5fbfea color:#000000,fill:#00FFF4
%% classDef a4271316e color:#000000,fill:#FFFF00
%% classDef a0298c687 color:#000000,fill:#AA7CB9
%% classDef a911b8d83 color:#000000,fill:#5afa0a
```

"

## Technical Flow Diagram

In high level, these are the main components:

```mermaid
flowchart TD
    A[SSMAPP4 Screen - 3270 Interface] -->|Screen Input| B[LGTESTP4 - Screen Handler]
    B -->|CICS LINK| C[LGAPOL01 - Business Logic Orchestrator]
    C -->|CICS LINK| D[LGAPDB01 - Risk Orchestration & Storage Handler]:::a4f5fbfea
    
    subgraph LGAPDB01[Risk Processing System]
        D1[LGAPDB01 - Initialization & Setup]
        D2[LGCOMCAL - Risk Assessment]:::a261ec6c4
    

        D5[DB2 Operations]:::a0298c687
        D6[VSAM Handler Call]
        
        D1 -->|CICS LINK| D2
        D2 -->|Calculation Results| D1


        D1 --> D5
        D5 --> D6
    end

    D6 -->|CICS LINK| E[LGAPVS01 - VSAM File Handler]

    subgraph Storage[Data Storage]
        F[(DB2 Database)]
        G[(VSAM Files)]
    end

    D5 -->|Store in| F
    E -->|Write to| G

classDef a0b8c7d0e color:#000000,fill:#7CB9F4
classDef a261ec6c4 color:#000000,fill:#00FFAA
classDef a4f5fbfea color:#000000,fill:#00FFF4
classDef a4271316e color:#000000,fill:#FFFF00
classDef a0298c687 color:#000000,fill:#AA7CB9
classDef a911b8d83 color:#000000,fill:#5afa0a
```

## Technical Flow Code Walkthrough

### Reading Information and Orchestrating

<SwmSnippet path="/base/src/lgtestp4.cbl" line="146">

---

Within the Screen Handler, when the user selects <SwmToken path="/base/src/lgtestp4.cbl" pos="146:4:4" line-data="             WHEN &#39;2&#39;">`2`</SwmToken> (<SwmToken path="/base/src/ssmap.bms" pos="457:7:9" line-data="               INITIAL=&#39;2. Policy Add     &#39;">`Policy Add`</SwmToken>), the fields provided by the user are set:

```cobol
             WHEN '2'
                 Move '01ACOM'             To  CA-REQUEST-ID
                 Move ENP4CNOO             To  CA-CUSTOMER-NUM
                 Move ENP4IDAO             To  CA-ISSUE-DATE
                 Move ENP4EDAO             To  CA-EXPIRY-DATE
                 Move ENP4ADDO             To  CA-B-Address
                 Move ENP4HPCO             To  CA-B-PST
                 Move ENP4LATO             To  CA-B-Latitude
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgtestp4.cbl" line="168">

---

Then <SwmToken path="/base/src/lgtestp4.cbl" pos="168:10:10" line-data="                 EXEC CICS LINK PROGRAM(&#39;LGAPOL01&#39;)">`LGAPOL01`</SwmToken> (the Business Logic Orchestrator) is invoked:

```cobol
                 EXEC CICS LINK PROGRAM('LGAPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapol01.cbl" line="103">

---

<SwmToken path="/base/src/lgtestp4.cbl" pos="168:10:10" line-data="                 EXEC CICS LINK PROGRAM(&#39;LGAPOL01&#39;)">`LGAPOL01`</SwmToken> acts as an intermediary between the screen handler (<SwmToken path="/base/src/lgtestp4.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGTESTP4.">`LGTESTP4`</SwmToken>) and the DB2/business logic handler (<SwmToken path="/base/src/lgapdb01.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken>). It delegates the actual work to <SwmToken path="/base/src/lgapdb01.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken>:

```cobol
           EXEC CICS Link Program(LGAPDB01)
                Commarea(DFHCOMMAREA)
                LENGTH(32500)
           END-EXEC.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb01.cbl" line="493">

---

Here we initialize the business data and delegate the logic to the risk calculation module:

```
       P500-BIZ SECTION.
           MOVE CA-CUSTOMER-NUM TO WS-XCUSTID
           MOVE CA-POLICY-NUM TO WS-XPOLNUM
           MOVE CA-B-PropType TO WS-XPROPTYPE
           MOVE CA-B-PST TO WS-XPOSTCODE
           MOVE CA-B-FP TO WS-XFP-FACTOR
           MOVE CA-B-CP TO WS-XCP-FACTOR
           MOVE CA-B-FLP TO WS-XFLP-FACTOR
           MOVE CA-B-WP TO WS-XWP-FACTOR
           MOVE CA-B-Address TO WS-XADDRESS
           MOVE CA-B-Latitude TO WS-XLAT
           MOVE CA-B-Longitude TO WS-XLONG
           MOVE CA-B-Customer TO WS-XCUSTNAME
           MOVE CA-ISSUE-DATE TO WS-XISSUE
           MOVE CA-EXPIRY-DATE TO WS-XEXPIRY
           MOVE CA-LASTCHANGED TO WS-XLASTCHG
           
           EXEC CICS LINK PROGRAM('LGCOMCAL')
                COMMAREA(WS-COMM-RISK-AREA)
                LENGTH(LENGTH OF WS-COMM-RISK-AREA)
           END-EXEC
```

---

</SwmSnippet>

### Risk Score Calculation System

```mermaid
graph TD
subgraph Risk Score Calculation
        C[Set Base Score: 100]
        C --> D{Property Type?}
        D -->|Warehouse| E[+050]
        D -->|Factory| F[+075]
        D -->|Office| G[+025]
        D -->|Retail| H[+040]
        E & F & G & H --> I{High Risk Postcode?}
        I -->|Yes FL/CR| J[+030]
        I -->|No| K[No Addition]
    end

%% Swimm:
%% graph TD
%% subgraph Risk Score Calculation
%%         C[Set Base Score: <SwmToken path="/base/src/lgcomcal.cbl" pos="115:18:18" line-data="           03 WS-TM-BASE               PIC 9(3) VALUE 100.">`100`</SwmToken>]
%%         C --> D{Property Type?}
%%         D -->|Warehouse| E[+<SwmToken path="/base/src/lgcomdat.cpy" pos="77:20:20" line-data="                   07  RMS-PF-W-VAL     PIC 9(3) VALUE 050.">`050`</SwmToken>]
%%         D -->|Factory| F[+<SwmToken path="/base/src/lgcomdat.cpy" pos="78:20:20" line-data="                   07  RMS-PF-F-VAL     PIC 9(3) VALUE 075.">`075`</SwmToken>]
%%         D -->|Office| G[+<SwmToken path="/base/src/lgcomdat.cpy" pos="79:20:20" line-data="                   07  RMS-PF-O-VAL     PIC 9(3) VALUE 025.">`025`</SwmToken>]
%%         D -->|Retail| H[+<SwmToken path="/base/src/lgcomdat.cpy" pos="80:20:20" line-data="                   07  RMS-PF-R-VAL     PIC 9(3) VALUE 040.">`040`</SwmToken>]
%%         E & F & G & H --> I{High Risk Postcode?}
%%         I -->|Yes FL/CR| J[+<SwmToken path="/base/src/lgcomdat.cpy" pos="86:20:20" line-data="                   07  RMS-GF-FL-VAL    PIC 9(3) VALUE 030.">`030`</SwmToken>]
%%         I -->|No| K[No Addition]
%%     end
```

The risk scoring logic is distributed across a network of programs for modularity and maintainability.

<SwmSnippet path="/base/src/lgcomcal.cbl" line="277">

---

The main risk processing occurs in <SwmToken path="/base/src/lgcomcal.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGCOMCAL.">`LGCOMCAL`</SwmToken>, which initializes the risk matrix structure.

Notice we actually initialize <SwmToken path="/base/src/lgcomcal.cbl" pos="280:15:21" line-data="           MULTIPLY 2 BY WS-SUB-1 GIVING WS-RC-BASE-VAL.">`WS-RC-BASE-VAL`</SwmToken> to <SwmToken path="/base/src/lgcomcal.cbl" pos="115:18:18" line-data="           03 WS-TM-BASE               PIC 9(3) VALUE 100.">`100`</SwmToken>.

```
       PROCESS-RISK-SCORE.
           MOVE WS-TM-BASE TO WS-TEMP-SCORE.
           DIVIDE 2 INTO WS-TEMP-SCORE GIVING WS-SUB-1.
           MULTIPLY 2 BY WS-SUB-1 GIVING WS-RC-BASE-VAL.
           
           MOVE 0 TO WS-RC-PROP-FACT.
           
           MOVE 'COMMERCIAL' TO RMS-TYPE
           MOVE '1.0.5' TO RMS-VERSION
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgcomcal.cbl" line="287">

---

Property type evaluation is performed through matrix lookups:

```
           EVALUATE CA-XPROPTYPE
               WHEN 'WAREHOUSE'
                   MOVE RMS-PF-W-VAL TO RMS-PF-WAREHOUSE
                   COMPUTE WS-TEMP-CALC = RMS-PF-WAREHOUSE
                   ADD WS-TEMP-CALC TO WS-RC-PROP-FACT
               WHEN 'FACTORY'
                   MOVE RMS-PF-F-VAL TO RMS-PF-FACTORY
                   COMPUTE WS-TEMP-CALC = RMS-PF-FACTORY
                   ADD WS-TEMP-CALC TO WS-RC-PROP-FACT
               WHEN 'OFFICE'
                   MOVE RMS-PF-O-VAL TO RMS-PF-OFFICE
                   COMPUTE WS-TEMP-CALC = RMS-PF-OFFICE
                   ADD WS-TEMP-CALC TO WS-RC-PROP-FACT
               WHEN 'RETAIL'
                   MOVE RMS-PF-R-VAL TO RMS-PF-RETAIL
                   COMPUTE WS-TEMP-CALC = RMS-PF-RETAIL
                   ADD WS-TEMP-CALC TO WS-RC-PROP-FACT
               WHEN OTHER
                   MOVE 0 TO WS-RC-PROP-FACT
           END-EVALUATE.
```

---

</SwmSnippet>

<SwmSnippet path="base/src/lgcomcal.cbl" line="308">

---

Geographic risk factors are processed via a specialized matrix:

```
           MOVE 0 TO WS-RC-GEO-FACT.
           
           MOVE RMS-GF-FL-VAL TO RMS-GF-FL
           MOVE RMS-GF-CR-VAL TO RMS-GF-CR
           
           IF CA-XPOSTCODE(1:2) = 'FL'
              MOVE RMS-GF-FL TO WS-RC-GEO-FACT
           ELSE
              IF CA-XPOSTCODE(1:2) = 'CR'
                 MOVE RMS-GF-CR TO WS-RC-GEO-FACT
              END-IF
           END-IF.
```

---

</SwmSnippet>

### Determine Policy Status

```mermaid
flowchart TD
subgraph Status Determination
M{Final Risk Score}
        M -->|x> 200 | N[Status 2: Manual Review]
        M -->|150 < x < 200| O[Status 1: Pending Review]
        M -->|x≤ 150 | P[Status 0: Auto-Approved]
    end

%% Swimm:
%% flowchart TD
%% subgraph Status Determination
%% M{Final Risk Score}
%%         M -->|x> <SwmToken path="/base/src/lgcomdat.cpy" pos="94:20:20" line-data="                   07  RMS-TH-L2-VAL    PIC 9(3) VALUE 200.">`200`</SwmToken> | N[Status 2: Manual Review]
%%         M -->|<SwmToken path="/base/src/lgcomdat.cpy" pos="93:20:20" line-data="                   07  RMS-TH-L1-VAL    PIC 9(3) VALUE 150.">`150`</SwmToken> < x < <SwmToken path="/base/src/lgcomdat.cpy" pos="94:20:20" line-data="                   07  RMS-TH-L2-VAL    PIC 9(3) VALUE 200.">`200`</SwmToken>| O[Status 1: Pending Review]
%%         M -->|x≤ <SwmToken path="/base/src/lgcomdat.cpy" pos="93:20:20" line-data="                   07  RMS-TH-L1-VAL    PIC 9(3) VALUE 150.">`150`</SwmToken> | P[Status 0: Auto-Approved]
%%     end
```

<SwmSnippet path="base/src/lgcomcal.cbl" line="331">

---

Status determination uses threshold values from the risk matrix structure:

```
           MOVE 0 TO WS-RC-STATUS.
           MOVE SPACES TO WS-RC-REASON.
           
           MOVE RMS-TH-L1-VAL TO RMS-TH-LEVEL-1
           MOVE RMS-TH-L2-VAL TO RMS-TH-LEVEL-2
           
           IF WS-SA-RISK > RMS-TH-LEVEL-2
              MOVE 2 TO WS-RC-STATUS
              MOVE 'High Risk Score - Manual Review Required' 
                TO WS-RC-REASON
              MOVE WS-SA-RISK TO CID-FINAL-SCORE
              MOVE WS-RC-STATUS TO CID-STATUS
              MOVE WS-RC-REASON TO CID-REASON
           ELSE
              IF WS-SA-RISK > RMS-TH-LEVEL-1
                 MOVE 1 TO WS-RC-STATUS
                 MOVE 'Medium Risk - Pending Review'
                   TO WS-RC-REASON
                 MOVE WS-SA-RISK TO CID-FINAL-SCORE
                 MOVE WS-RC-STATUS TO CID-STATUS
                 MOVE WS-RC-REASON TO CID-REASON
              ELSE
                 MOVE 0 TO WS-RC-STATUS
                 MOVE SPACES TO WS-RC-REASON
                 MOVE WS-SA-RISK TO CID-FINAL-SCORE
                 MOVE WS-RC-STATUS TO CID-STATUS
                 MOVE SPACES TO CID-REASON
              END-IF
           END-IF.
```

---

</SwmSnippet>

### Premium Calculation

```mermaid
graph TD;
subgraph Premium Calculation
        R{Check All Perils}
        R -->|All Selected| S[Set Discount 0.90]
        R -->|Not All| T[No Discount]
        S & T --> U[Calculate Premiums]
        U --> V1[Fire: Risk x 0.80]
        U --> V2[Crime: Risk x 0.60]
        U --> V3[Flood: Risk x 1.20]
        U --> V4[Weather: Risk x 0.90]
    end

%% Swimm:
%% graph TD;
%% subgraph Premium Calculation
%%         R{Check All Perils}
%%         R -->|All Selected| S[Set Discount <SwmToken path="/base/src/lgcomdat.cpy" pos="106:15:17" line-data="           03  RMS-DISCOUNT-FACTOR      PIC V99 VALUE 0.90.">`0.90`</SwmToken>]
%%         R -->|Not All| T[No Discount]
%%         S & T --> U[Calculate Premiums]
%%         U --> V1[Fire: Risk x <SwmToken path="/base/src/lgcomdat.cpy" pos="102:17:19" line-data="                   07  RMS-PERF-F-VAL   PIC V99 VALUE 0.80.">`0.80`</SwmToken>]
%%         U --> V2[Crime: Risk x <SwmToken path="/base/src/lgcomdat.cpy" pos="103:17:19" line-data="                   07  RMS-PERF-C-VAL   PIC V99 VALUE 0.60.">`0.60`</SwmToken>]
%%         U --> V3[Flood: Risk x <SwmToken path="/base/src/lgcomdat.cpy" pos="104:17:19" line-data="                   07  RMS-PERF-FL-VAL  PIC V99 VALUE 1.20.">`1.20`</SwmToken>]
%%         U --> V4[Weather: Risk x <SwmToken path="/base/src/lgcomdat.cpy" pos="105:17:19" line-data="                   07  RMS-PERF-W-VAL   PIC V99 VALUE 0.90.">`0.90`</SwmToken>]
%%     end
```

<SwmSnippet path="/base/src/lgcomcal.cbl" line="365">

---

Premium calculation starts by initializing peril factors and discount:

```
       CALCULATE-PREMIUMS.
           MOVE 1.00 TO WS-RC-DISCOUNT.
           
           MOVE RMS-PERF-F-VAL TO RMS-PERF-FIRE
           MOVE RMS-PERF-C-VAL TO RMS-PERF-CRIME
           MOVE RMS-PERF-FL-VAL TO RMS-PERF-FLOOD
           MOVE RMS-PERF-W-VAL TO RMS-PERF-WEATHER
           
           IF CA-XFP-FACTOR > 0 AND
              CA-XCP-FACTOR > 0 AND
              CA-XFLP-FACTOR > 0 AND
              CA-XWP-FACTOR > 0
              MOVE RMS-DISCOUNT-FACTOR TO WS-RC-DISCOUNT
              MOVE RMS-DISCOUNT-FACTOR TO CID-DISCOUNT-PCT
           END-IF.
```

---

</SwmSnippet>

<SwmSnippet path="base/src/lgcomcal.cbl" line="388">

---

Each peril premium is calculated using the appropriate risk factor:

```
           IF CA-XFP-FACTOR > 0
              COMPUTE WS-TEMP-CALC = 
                 WS-SA-RISK * RMS-PERF-FIRE
              COMPUTE WS-RC-PREM-FIRE =
                 (WS-TEMP-CALC * CA-XFP-FACTOR * WS-RC-DISCOUNT)
              MOVE WS-RC-PREM-FIRE TO CID-FIRE-PREMIUM
           ELSE
              MOVE 0 TO WS-RC-PREM-FIRE
              MOVE 0 TO CID-FIRE-PREMIUM
           END-IF.
```

---

</SwmSnippet>

### Writing to Storage

<SwmSnippet path="base/src/lgapdb01.cbl" line="554">

---

After processing by <SwmToken path="/base/src/lgcomcal.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGCOMCAL.">`LGCOMCAL`</SwmToken>, the values are returned to <SwmToken path="/base/src/lgapdb01.cbl" pos="2:6:6" line-data="       PROGRAM-ID. LGAPDB01.">`LGAPDB01`</SwmToken> for DB2 storage:

```
           MOVE CA-B-FP     TO DB2-B-P1-Int
           MOVE CA-B-CA-B-FPR   TO DB2-B-P1A-Int
           MOVE CA-B-CP    TO DB2-B-P2-Int
           MOVE CA-B-CPR  TO DB2-B-P2A-Int
           MOVE CA-B-FLP    TO DB2-B-P3-Int
           MOVE CA-B-FLPR  TO DB2-B-P3A-Int
           MOVE CA-B-WP  TO DB2-B-P4-Int
           MOVE CA-B-WPR TO DB2-B-P4A-Int
           MOVE CA-B-ST        TO DB2-B-Z9-Int
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb01.cbl" line="268">

---

And VSAM persistence is handled by calling <SwmToken path="/base/src/lgapdb01.cbl" pos="268:9:9" line-data="             EXEC CICS Link Program(LGAPVS01)">`LGAPVS01`</SwmToken>:

```cobol
             EXEC CICS Link Program(LGAPVS01)
                  Commarea(DFHCOMMAREA)
                LENGTH(32500)
             END-EXEC.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapvs01.cbl" line="101">

---

<SwmToken path="/base/src/lgapdb01.cbl" pos="268:9:9" line-data="             EXEC CICS Link Program(LGAPVS01)">`LGAPVS01`</SwmToken> handles writing the calculated values to VSAM:

```cobol
             When 'C'
               Move CA-B-PST     To V2-C-PCD
               Move CA-B-ST       To V2-C-Z9
               Move CA-B-Customer     To V2-C-CUST
               Move WS-RISK-SCORE     To V2-C-VAL
               Move CA-B-CA-B-FPR  To V2-C-P1VAL
               Move CA-B-CPR To V2-C-P2VAL
               Move CA-B-FLPR To V2-C-P3VAL
               Move CA-B-WPR To V2-C-P4VAL
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapvs01.cbl" line="136">

---

The final persistence to VSAM completes the process:

```cobol
           Exec CICS Write File('KSDSPOLY')
                     From(V2-RECORD)
                     Length(104)
                     Ridfld(V2-KEY)
                     KeyLength(21)
                     RESP(V1-RCD1)
           End-Exec.
```

---

</SwmSnippet>

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
