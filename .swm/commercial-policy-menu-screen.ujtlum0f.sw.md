---
title: Commercial Policy Menu Screen
---
The Commercial Policy Menu screen (SSMAPP4) serves as the interface for managing commercial insurance policies. Users can perform actions such as policy inquiry, addition, and deletion, while entering details like policy number, customer number, and property information.

## Screen Preview

```
SSP4 General Insurance Commercial Policy Menu

 1. Policy Inquiry
 2. Policy Add
 3. Policy Delete

 Policy Number: ____________
 Cust Number: ____________
 Start date: ____________ (yyyy-mm-dd)
 Expiry date: ____________ (yyyy-mm-dd)
 Address: ___________________________
 Postcode: __________
 Latitude/Longitude: ____________ ____________
 Customer Name: ___________________________
 Property Type: ___________________________
 Fire Peril/Prem: ____ ________
 Crime Peril/Prem: ____ ________
 Flood Peril/Prem: ____ ________
 Weather Peril/Prem: ____ ________
 Status: ____
 Reject Reason: ___________________________

 Select Option: _

[Error/Status Message Area]
```

## Fields

### Policy Number

- Length: 10 characters
- Right justified, zero-filled
- Required for inquiry, add, and delete operations

### Cust Number

- Length: 10 characters
- Right justified, zero-filled
- Required for inquiry, add, and delete operations

### Start date

- Format: yyyy-mm-dd
- Required for add operation

### Expiry date

- Format: yyyy-mm-dd
- Required for add operation

### Address

- Length: 25 characters
- Required for add operation

### Postcode

- Length: 8 characters
- Required for add operation

### Latitude/Longitude

- Length: 11 characters each
- Right justified, zero-filled
- Required for add operation

### Customer Name

- Length: 25 characters
- Required for add operation

### Property Type

- Length: 25 characters
- Required for add operation

### Fire Peril/Prem

- Peril Length: 4 characters, Premium Length: 8 characters
- Right justified, zero-filled
- Required for add operation

### Crime Peril/Prem

- Peril Length: 4 characters, Premium Length: 8 characters
- Right justified, zero-filled
- Required for add operation

### Flood Peril/Prem

- Peril Length: 4 characters, Premium Length: 8 characters
- Right justified, zero-filled
- Required for add operation

### Weather Peril/Prem

- Peril Length: 4 characters, Premium Length: 8 characters
- Right justified, zero-filled
- Required for add operation

### Status

- Length: 4 characters
- Right justified, zero-filled
- Required for add operation

### Reject Reason

- Length: 25 characters
- Required for add operation

### Select Option

- Single character input field
- Must be entered
- Valid options are 1, 2, or 3

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](/)</sup></SwmMeta>
