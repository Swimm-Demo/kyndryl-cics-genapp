---
title: Commercial Property Policy Management System - Functional Specification
---
## 1\. Executive Summary & Purpose

The Commercial Property Policy Management System provides functionality for managing commercial insurance policies through a terminal-based interface. The system enables users to inquire about existing policies, add new commercial property policies, and delete existing policies. The system implements automated risk assessment and premium calculation based on property characteristics and selected insurance perils.

&nbsp;

## 2\. User Interface Specification

### 2.1 Screen Layout

The system presents a <SwmToken path="/base/src/ssmap.bms" pos="112:7:7" line-data="SSMAPP1 DFHMDI SIZE=(24,80)">`24`</SwmToken>x<SwmToken path="/base/src/ssmap.bms" pos="112:9:9" line-data="SSMAPP1 DFHMDI SIZE=(24,80)">`80`</SwmToken> character terminal interface with the following layout:

&nbsp;

```
SSP4       General Insurance Commercial Policy Menu                                 
                                                                                
                                                                                
       1. Policy Inquiry          Policy Number [0000000000]                  
       2. Policy Add              Cust Number [0000000000]                    
       3. Policy Delete           Start date [          ] (yyyy-mm-dd)        
                                  Expiry date [          ] (yyyy-mm-dd)       
                                  Address [                         ]         
                                  Postcode     [        ]                     
                                  Latitude/Longitude [           ] [           ]
                                  Customer Name[                         ]    
                                  Property Type[                         ]    
                                  Fire Peril/Prem[    ] [        ]            
                                  Crime Peril/Prem[    ] [        ]           
                                  Flood Peril/Prem[    ] [        ]           
                                  Weather Peril/Prem[    ] [        ]         
                                  Status[    ]                                
                                  Reject Reason[                         ]    
                                                                                
                                                                                
                                                                                
       Select Option [_]                                                       
                                                                                
       [Error message area]
```

### 2.2 Screen Sections

- **Header area** (row 1): System identification (<SwmToken path="/base/src/ssmap.bms" pos="450:26:26" line-data="        DFHMDF POS=(1,1),LENGTH=4,ATTRB=(ASKIP,BRT),INITIAL=&#39;SSP4&#39;">`SSP4`</SwmToken>) and title

- **Menu options area** (rows 4-6): Available operations display

- **Data entry area** (rows 4-18): Input fields for policy information

- **Option selection area** (row <SwmToken path="/base/src/ssmap.bms" pos="594:7:7" line-data="ENP4OPT DFHMDF POS=(22,24),LENGTH=1,ATTRB=(NORM,NUM,UNPROT,FSET),      X">`22`</SwmToken>): User operation choice

- **Error message area** (row <SwmToken path="/base/src/ssmap.bms" pos="599:7:7" line-data="ERP4FLD DFHMDF POS=(24,8),LENGTH=40,ATTRB=(BRT,ASKIP,PROT),            X">`24`</SwmToken>, position <SwmToken path="/base/src/ssmap.bms" pos="599:9:9" line-data="ERP4FLD DFHMDF POS=(24,8),LENGTH=40,ATTRB=(BRT,ASKIP,PROT),            X">`8`</SwmToken>): System feedback messages (<SwmToken path="/base/src/ssmap.bms" pos="599:14:14" line-data="ERP4FLD DFHMDF POS=(24,8),LENGTH=40,ATTRB=(BRT,ASKIP,PROT),            X">`40`</SwmToken> characters, bright display)

&nbsp;

### 2.3 Data Entry Fields with BMS Specifications

**Policy Identification Fields:**

- **Policy Number:** <SwmToken path="/base/src/ssmap.bms" pos="465:14:14" line-data="ENP4PNO DFHMDF POS=(04,50),LENGTH=10,ATTRB=(NORM,UNPROT,IC,FSET),      *">`10`</SwmToken>-digit numeric field, right-justified with zero fill, initial cursor position

  - *Initial Value:* '<SwmToken path="/base/src/lgtestp4.cbl" pos="30:4:4" line-data="           MOVE &#39;0000000000&#39;   To ENP4PNOO.">`0000000000`</SwmToken>'

- **Customer Number:** <SwmToken path="/base/src/ssmap.bms" pos="472:14:14" line-data="ENP4CNO DFHMDF POS=(05,50),LENGTH=10,ATTRB=(NORM,UNPROT,IC,FSET),      *">`10`</SwmToken>-digit numeric field, right-justified with zero fill

  - *Initial Value:* '<SwmToken path="/base/src/lgtestp4.cbl" pos="29:4:4" line-data="           MOVE &#39;0000000000&#39;   To ENP4CNOO.">`0000000000`</SwmToken>'&nbsp;

**Policy Date Fields:**

- **Start Date:** <SwmToken path="/base/src/ssmap.bms" pos="479:14:14" line-data="ENP4IDA DFHMDF POS=(06,50),LENGTH=10,ATTRB=(NORM,UNPROT,FSET),         X">`10`</SwmToken>-character field in <SwmToken path="/base/src/ssmap.bms" pos="484:5:9" line-data="               INITIAL=&#39;(yyyy-mm-dd)&#39;">`yyyy-mm-dd`</SwmToken> format

- **Expiry Date:** <SwmToken path="/base/src/ssmap.bms" pos="488:14:14" line-data="ENP4EDA DFHMDF POS=(07,50),LENGTH=10,ATTRB=(NORM,UNPROT,FSET),         X">`10`</SwmToken>-character field in <SwmToken path="/base/src/ssmap.bms" pos="493:5:9" line-data="               INITIAL=&#39;(yyyy-mm-dd)&#39;">`yyyy-mm-dd`</SwmToken> format

**Property Information Fields:**

- **Address:** <SwmToken path="/base/src/ssmap.bms" pos="497:14:14" line-data="ENP4ADD DFHMDF POS=(08,50),LENGTH=25,ATTRB=(NORM,UNPROT,FSET),         X">`25`</SwmToken>-character display field (stores up to 255 characters in database)

- **Postcode:** <SwmToken path="/base/src/ssmap.bms" pos="504:14:14" line-data="ENP4HPC DFHMDF POS=(09,50),LENGTH=08,ATTRB=(NORM,UNPROT,FSET),         X">`08`</SwmToken>-character field

  **Latitude:** <SwmToken path="/base/src/ssmap.bms" pos="511:14:14" line-data="ENP4LAT DFHMDF POS=(10,50),LENGTH=11,ATTRB=(NORM,UNPROT,FSET),         X">`11`</SwmToken>-character numeric field, right-justified with zero fill

- **Longitude:** <SwmToken path="/base/src/ssmap.bms" pos="515:14:14" line-data="ENP4LON DFHMDF POS=(10,64),LENGTH=11,ATTRB=(NORM,UNPROT,FSET),         X">`11`</SwmToken>-character numeric field, right-justified with zero fill

- **Customer Name:** <SwmToken path="/base/src/ssmap.bms" pos="522:14:14" line-data="ENP4CUS DFHMDF POS=(11,50),LENGTH=25,ATTRB=(NORM,UNPROT,FSET),         X">`25`</SwmToken>-character display field

- **Property Type:** <SwmToken path="/base/src/ssmap.bms" pos="529:14:14" line-data="ENP4PTY DFHMDF POS=(12,50),LENGTH=25,ATTRB=(NORM,UNPROT,FSET),         X">`25`</SwmToken>-character display field

**Insurance Coverage Fields (Perils and Premiums):**

- **Fire Peril:** <SwmToken path="/base/src/ssmap.bms" pos="536:14:14" line-data="ENP4FPE DFHMDF POS=(13,50),LENGTH=4,ATTRB=(NORM,UNPROT,FSET),          X">`4`</SwmToken>-digit numeric code, right-justified with zero fill

- **Fire Premium:** <SwmToken path="/base/src/ssmap.bms" pos="540:14:14" line-data="ENP4FPR DFHMDF POS=(13,56),LENGTH=8,ATTRB=(NORM,UNPROT,FSET),          X">`8`</SwmToken>-digit numeric amount, right-justified with zero fill

- **Crime Peril:** <SwmToken path="/base/src/ssmap.bms" pos="547:14:14" line-data="ENP4CPE DFHMDF POS=(14,50),LENGTH=4,ATTRB=(NORM,UNPROT,FSET),          X">`4`</SwmToken>-digit numeric code, right-justified with zero fill

- **Crime Premium:** <SwmToken path="/base/src/ssmap.bms" pos="551:14:14" line-data="ENP4CPR DFHMDF POS=(14,56),LENGTH=8,ATTRB=(NORM,UNPROT,FSET),          X">`8`</SwmToken>-digit numeric amount, right-justified with zero fill

- **Flood Peril:** <SwmToken path="/base/src/ssmap.bms" pos="558:14:14" line-data="ENP4XPE DFHMDF POS=(15,50),LENGTH=4,ATTRB=(NORM,UNPROT,FSET),          X">`4`</SwmToken>-digit numeric code, right-justified with zero fill

- **Flood Premium:** <SwmToken path="/base/src/ssmap.bms" pos="562:14:14" line-data="ENP4XPR DFHMDF POS=(15,56),LENGTH=8,ATTRB=(NORM,UNPROT,FSET),          X">`8`</SwmToken>-digit numeric amount, right-justified with zero fill

- **Weather Peril:** <SwmToken path="/base/src/ssmap.bms" pos="569:14:14" line-data="ENP4WPE DFHMDF POS=(16,50),LENGTH=4,ATTRB=(NORM,UNPROT,FSET),          X">`4`</SwmToken>-digit numeric code, right-justified with zero fill

- **Weather Premium:** <SwmToken path="/base/src/ssmap.bms" pos="573:14:14" line-data="ENP4WPR DFHMDF POS=(16,56),LENGTH=8,ATTRB=(NORM,UNPROT,FSET),          X">`8`</SwmToken>-digit numeric amount, right-justified with zero fill

**Policy Status Fields:**

- **Status:** <SwmToken path="/base/src/ssmap.bms" pos="580:14:14" line-data="ENP4STA DFHMDF POS=(17,50),LENGTH=4,ATTRB=(NORM,UNPROT,FSET),          X">`4`</SwmToken>-digit numeric code, right-justified with zero fill

- **Reject Reason:** <SwmToken path="/base/src/ssmap.bms" pos="587:14:14" line-data="ENP4REJ DFHMDF POS=(18,50),LENGTH=25,ATTRB=(NORM,UNPROT,FSET),         X">`25`</SwmToken>-character display field

**Operation Selection:**

- **Select Option:** Single character field, numeric only, mandatory entry

### 2.4 Screen Behavior

**Cursor Management:**

- Initial cursor position: Policy Number field

  - *Note:* Both Policy Number and Customer Number have IC attribute; Policy Number gets cursor due to screen position

**Field Initialization:**

- Numeric fields initialized with zero-filled values or <SwmToken path="/base/src/lgtestp4.cbl" pos="31:3:5" line-data="           MOVE LOW-VALUES     To ENP4FPEO.">`LOW-VALUES`</SwmToken>

## 3\. Business Operations

### 3.1 Available Operations

The system provides three primary business operations:

#### 3.1.1 Policy Inquiry (Option 1)

- Retrieve and display existing commercial property policy information

- Supports multiple search criteria for flexible policy lookup

#### 3.1.2 Policy Add (Option 2)

- Create new commercial property insurance policies

- Includes automated risk assessment and premium calculation

- Implements business rules for policy approval/rejection

#### 3.1.3 Policy Delete (Option 3)

- Remove existing commercial property policies from the system

- Requires specific policy and customer identification

&nbsp;

## 4\. Business Rules and Logic

### 4.1 Risk Assessment Methodology

**Base Risk Score Calculation:**

- All commercial property policies begin with a base risk score of <SwmToken path="/base/src/lgcomcal.cbl" pos="115:18:18" line-data="           03 WS-TM-BASE               PIC 9(3) VALUE 100.">`100`</SwmToken> points

**Property Type Risk Factors:**

- Warehouse properties: +50 points

  Factory properties: +75 points

- Office properties: +25 points

- Retail properties: +40 points

- Unrecognized property types: +0 points

**Geographic Risk Factors:**

- Properties with postcodes beginning "FL": +30 points

- Properties with postcodes beginning "CR": +30 points

- All other postcodes: +0 points

**Final Risk Score Calculation:**

- Calculated as: Base Score (100) + Property Type Factor + Geographic Factor

### 4.2 Policy Status Determination

**Status Code Assignment:**

- Risk Score ≤ 150: Status 0 (Auto-Approved)

- Risk Score 151-200: Status 1 (Pending Review)

- Risk Score > 200: Status 2 (Manual Review Required)

**Rejection Reason Messages:**

- Status <SwmToken path="/base/src/lgcomcal.cbl" pos="338:3:3" line-data="              MOVE 2 TO WS-RC-STATUS">`2`</SwmToken> policies: "<SwmToken path="/base/src/lgcomcal.cbl" pos="339:4:16" line-data="              MOVE &#39;High Risk Score - Manual Review Required&#39; ">`High Risk Score - Manual Review Required`</SwmToken>"

- Status <SwmToken path="/base/src/lgcomcal.cbl" pos="346:3:3" line-data="                 MOVE 1 TO WS-RC-STATUS">`1`</SwmToken> policies: "<SwmToken path="/base/src/lgcomcal.cbl" pos="347:4:12" line-data="                 MOVE &#39;Medium Risk - Pending Review&#39;">`Medium Risk - Pending Review`</SwmToken>"

- Status <SwmToken path="/base/src/lgcomcal.cbl" pos="353:3:3" line-data="                 MOVE 0 TO WS-RC-STATUS">`0`</SwmToken> policies: No rejection reason (<SwmToken path="/base/src/lgcomcal.cbl" pos="354:3:3" line-data="                 MOVE SPACES TO WS-RC-REASON">`SPACES`</SwmToken>)

### 4.3 Premium Calculation Logic

**Peril Premium Factors:**

- Fire coverage: Risk Score × <SwmToken path="/base/src/lgcomcal.cbl" pos="128:17:19" line-data="           03 WS-SA-FIRE-FACTOR        PIC V99 VALUE 0.80.">`0.80`</SwmToken>

- Crime coverage: Risk Score × <SwmToken path="/base/src/lgcomcal.cbl" pos="129:17:19" line-data="           03 WS-SA-CRIME-FACTOR       PIC V99 VALUE 0.60.">`0.60`</SwmToken>

- Flood coverage: Risk Score × <SwmToken path="/base/src/lgcomcal.cbl" pos="130:17:19" line-data="           03 WS-SA-FLOOD-FACTOR       PIC V99 VALUE 1.20.">`1.20`</SwmToken>

- Weather coverage: Risk Score × <SwmToken path="/base/src/lgcomcal.cbl" pos="131:17:19" line-data="           03 WS-SA-WEATHER-FACTOR     PIC V99 VALUE 0.90.">`0.90`</SwmToken>

**Multi-Peril Discount:**

- When all four perils (Fire, Crime, Flood, Weather) are selected: 10% discount applied (<SwmToken path="/base/src/lgcomcal.cbl" pos="127:15:17" line-data="           03 WS-SA-DISCOUNT           PIC V99 VALUE 0.90.">`0.90`</SwmToken> factor)

- Partial peril selection: No discount applied (<SwmToken path="/base/src/lgcomcal.cbl" pos="154:15:17" line-data="              05 WS-RC-DISCOUNT        PIC V99 VALUE 1.00.">`1.00`</SwmToken> factor)

**Final Premium Calculation Formula:**

- Individual Peril Premium = (Risk Score × Peril Factor) × Peril Code × Discount Factor

- Total Premium = Sum of all selected peril premiums

## 5\. Data Requirements

### 5.1 Mandatory Fields by Operation

**Policy Inquiry (Option 1):**

- At least one of the following combinations:

  - Policy Number + Customer Number (most specific search)

  - Policy Number only

  - Customer Number only (returns all policies for customer)

  - Postcode only (location-based search)

&nbsp;

**Policy Add (Option 2):**

- Customer Number (must reference existing customer)

- Start Date and Expiry Date (required for policy creation)

- Address, Postcode (required for geographic risk assessment)

- Customer Name, Property Type (required for risk assessment)

&nbsp;

**Policy Delete (Option 3):**

- Policy Number AND Customer Number (both required)

&nbsp;

### 5.2 Optional Fields

- <SwmToken path="/base/src/ssmap.bms" pos="510:4:6" line-data="               INITIAL=&#39;Latitude/Longitude&#39;">`Latitude/Longitude`</SwmToken> coordinates

- All peril codes and premiums (system calculates if property data provided)

- Status (system determines automatically)

- Reject Reason (system populates automatically)

&nbsp;

### 5.3 Field Validation Rules

**Numeric Field Behavior:**

- Right-justification with zero padding for numeric fields

**Select Option Validation:**

- Must enter a value (mandatory field)

- Numeric input only, accepts values 1, 2, or 3

**Date Field Format:**

- Expected format: <SwmToken path="/base/src/ssmap.bms" pos="484:5:9" line-data="               INITIAL=&#39;(yyyy-mm-dd)&#39;">`yyyy-mm-dd`</SwmToken> (format hints displayed to user)

&nbsp;

## 6\. Business Process Flows

### 6.1 Policy Inquiry Process

1. User selects Option 1 (Policy Inquiry)

2. User enters search criteria (Policy Number, Customer Number, and/or Postcode)

3. System validates at least one search criterion is provided

4. System calls <SwmToken path="/base/src/lgtestp4.cbl" pos="112:10:10" line-data="                 EXEC CICS LINK PROGRAM(&#39;LGIPOL01&#39;)">`LGIPOL01`</SwmToken> program for data retrieval

5. System displays policy information

6. If no records found: Display "<SwmToken path="/base/src/lgtestp4.cbl" pos="294:4:11" line-data="           Move &#39;No data was returned.&#39;              To  ERP4FLDO">`No data was returned.`</SwmToken>"

&nbsp;

### 6.2 Policy Add Process

 1. User selects Option <SwmToken path="/base/src/lgtestp4.cbl" pos="146:4:4" line-data="             WHEN &#39;2&#39;">`2`</SwmToken> (Policy Add)

 2. User enters mandatory policy information

 3. System validates customer number exists

    - If not, display: <SwmToken path="/base/src/lgtestp4.cbl" pos="278:4:10" line-data="               Move &#39;Customer does not exist&#39;        To  ERP4FLDO">`Customer does not exist`</SwmToken>

 4. System calls business orchestrator (LGAPOL01)

 5. System calls data handler (LGAPDB01)

 6. System calls risk calculation service (LGCOMCAL)

 7. System performs risk assessment calculation:

    - Calculate base risk score (100)

    - Add property type factor

    - Add geographic risk factor

    - Determine final risk score

 8. System determines policy status based on risk thresholds

 9. System calculates premiums for selected perils

10. System applies multi-peril discount if applicable

11. System stores policy in DB2 database

12. System stores policy in VSAM files

13. System displays success message: "<SwmToken path="/base/src/lgtestp4.cbl" pos="179:4:10" line-data="                 Move &#39;New Commercial Policy Inserted&#39;">`New Commercial Policy Inserted`</SwmToken>"

&nbsp;

### 6.3 Policy Delete Process

1. User selects Option 3 (Policy Delete)

2. User enters Policy Number and Customer Number

3. System validates both identifiers are provided

4. System calls LGDPOL01 program for deletion

5. System removes policy record

6. System clears all display fields

7. System displays success message: "<SwmToken path="/base/src/lgtestp4.cbl" pos="218:4:8" line-data="                 Move &#39;Commercial Policy Deleted&#39;">`Commercial Policy Deleted`</SwmToken>"

&nbsp;

## 7\. Error Handling and Business Exceptions

### 7.1 Field Validation Errors

- **Invalid option selection:** " <SwmToken path="/base/src/lgtestp4.cbl" pos="228:4:12" line-data="                 Move &#39;Please enter a valid option&#39;">`Please enter a valid option`</SwmToken>"

### 7.2 Business Logic Errors

**Policy Add Failures:**

- **Invalid customer number:** " <SwmToken path="/base/src/lgtestp4.cbl" pos="278:4:10" line-data="               Move &#39;Customer does not exist&#39;        To  ERP4FLDO">`Customer does not exist`</SwmToken>" (Error Code <SwmToken path="/base/src/lgtestp4.cbl" pos="277:3:3" line-data="             When 70">`70`</SwmToken>)

- **General add failure:** " <SwmToken path="/base/src/lgtestp4.cbl" pos="281:4:10" line-data="               Move &#39;Error Adding Commercial Policy&#39; To  ERP4FLDO">`Error Adding Commercial Policy`</SwmToken>"

**Policy Delete Failures:**

- **Delete operation failure:** " <SwmToken path="/base/src/lgtestp4.cbl" pos="290:4:10" line-data="           Move &#39;Error Deleting Commercial Policy&#39;   To  ERP4FLDO">`Error Deleting Commercial Policy`</SwmToken>"

**Policy Inquiry Failures:**

- **No matching records:** " <SwmToken path="/base/src/lgtestp4.cbl" pos="294:4:11" line-data="           Move &#39;No data was returned.&#39;              To  ERP4FLDO">`No data was returned.`</SwmToken>"

**System-Level Errors:**

- **Error Code** <SwmToken path="/base/src/lgapdb01.cbl" pos="243:4:4" line-data="             MOVE &#39;98&#39; TO CA-RETURN-CODE">`98`</SwmToken> **:** Insufficient commarea length

- **Error Code** <SwmToken path="/base/src/lgapdb01.cbl" pos="264:4:4" line-data="               MOVE &#39;99&#39; TO CA-RETURN-CODE">`99`</SwmToken>**:** Invalid request ID

- **Error Code** <SwmToken path="/base/src/lgapdb01.cbl" pos="610:4:4" line-data="              MOVE &#39;92&#39; TO CA-RETURN-CODE">`92`</SwmToken>**:** Commercial policy insert SQL failure

- **Error Code** <SwmToken path="/base/src/lgapdb01.cbl" pos="320:4:4" line-data="               MOVE &#39;90&#39; TO CA-RETURN-CODE">`90`</SwmToken>**:** General SQL failure

### 7.3 Data Integrity

**Transaction Management:**

- Failed operations trigger automatic transaction rollback

- Database consistency maintained through transaction management

- All changes committed only upon successful completion

- Critical SQL failures trigger system abend for data protection

&nbsp;

## 8\. System Behavior and Navigation

### 8.1 Screen Navigation

- Initial cursor position: Policy Number field

- ENTER key processes current screen input

- CLEAR key resets screen and reinitializes fields

- <SwmToken path="/base/src/lgtestp4.cbl" pos="51:1:1" line-data="                     PF3(D-END) END-EXEC.">`PF3`</SwmToken> key terminates session with " <SwmToken path="/base/src/lgtestp4.cbl" pos="10:2:4" line-data="                                        &#39;Transaction ended      &#39;.">`Transaction ended`</SwmToken>" message

### 8.2 Field Behavior

- Numeric fields automatically right-justify with zero padding

- Text fields accept alphanumeric input up to specified lengths

- Modified fields are tracked for efficient data transmission

### 8.3 Transaction Processing

- Each operation processes as a complete business transaction

- Successful operations display confirmation messages

- Failed operations display specific error messages and retain user input for correction

- System maintains data integrity through proper transaction management

&nbsp;

## 9\. Integration Requirements

### 9.1 Data Storage

- Policy information stored in both DB2 database and VSAM files

- Database operations maintain referential integrity

&nbsp;

### 9.2 Customer Validation

- Customer numbers must reference existing customer records

- Invalid customer references result in policy add failure with error code <SwmToken path="/base/src/lgtestp4.cbl" pos="277:3:3" line-data="             When 70">`70`</SwmToken>

- System validates customer existence before policy creation

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
