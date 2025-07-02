---
title: Motor Policy Menu Screen (SSMAPP1)
---
The General Insurance Motor Policy Menu screen (SSMAPP1) serves as the main interface for managing motor policy transactions. Users can perform various actions such as policy inquiry, addition, deletion, and updates by selecting the corresponding options. The screen captures essential policy details including policy number, customer number, issue and expiry dates, car specifications, and policy premium, facilitating efficient policy management.

## Screen Preview

```
Tran: SSP1                      General Insurance Motor Policy Menu          Date: mm/dd/yy
Prog: LGTESTP1

1. Policy Inquiry
2. Policy Add
3. Policy Delete
4. Policy Update

Policy Number: ___________
Cust Number  : ___________
Issue date   : ___________ (yyyy-mm-dd)
Expiry date  : ___________ (yyyy-mm-dd)
Car Make     : ______________________
Car Model    : ______________________
Car Value    : ______
Registration : _________
Car Colour   : ________
CC           : ______
Manufacture Date: ___________ (yyyy-mm-dd)
No. of Accidents: ______
Policy Premium  : ______

Select Option: _

[Error/Status Message Area]

ENTER=Continue  F3=Back  F4=Clear
```

## Fields

### Policy Number

- Length: 10 characters
- Right justified, zero-filled
- Required field

### Cust Number

- Length: 10 characters
- Right justified, zero-filled
- Required field

### Issue date

- Length: 10 characters
- Format: yyyy-mm-dd
- Required field

### Expiry date

- Length: 10 characters
- Format: yyyy-mm-dd
- Required field

### Car Make

- Length: 20 characters
- Free text input

### Car Model

- Length: 20 characters
- Free text input

### Car Value

- Length: 6 digits
- Right justified, zero-filled
- Required field

### Registration

- Length: 7 characters
- Free text input

### Car Colour

- Length: 8 characters
- Free text input

### CC

- Length: 4 digits
- Right justified, zero-filled
- Required field

### Manufacture Date

- Length: 10 characters
- Format: yyyy-mm-dd
- Required field

### No. of Accidents

- Length: 6 digits
- Right justified, zero-filled
- Required field

### Policy Premium

- Length: 6 digits
- Right justified, zero-filled
- Required field

### Select Option

- Single digit input
- Must enter a valid option
- Required field

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](/)</sup></SwmMeta>
