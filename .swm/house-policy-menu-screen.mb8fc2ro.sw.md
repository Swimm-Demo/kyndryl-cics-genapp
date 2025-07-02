---
title: House Policy Menu Screen
---
The House Policy Menu screen (SSMAPP3) serves as the interface for managing house insurance policies within the general insurance application. It allows users to perform various transactions such as policy inquiry, addition, deletion, and updates, providing fields for essential policy details like policy number, customer number, issue and expiry dates, property type, and house value.

## Screen Preview

```
Tran: SSP3                      General Insurance House Policy Menu          Date: mm/dd/yy
Prog: LGTESTP3

1. Policy Inquiry
2. Policy Add
3. Policy Delete
4. Policy Update

     Policy Number: ___________
     Cust Number  : ___________
     Issue date   : ___________ (yyyy-mm-dd)
     Expiry date  : ___________ (yyyy-mm-dd)
     Property Type: ___________
     Bedrooms     : ___
     House Value  : ________
     House Name   : ____________________
     House Number : ____
     Postcode     : ________

Select Option: _

[Error/Status Message Area]

ENTER=Continue  F3=Back  F4=Clear
```

## Fields

### Policy Number

- Length: 10 characters
- Right justified, zero-filled
- Required field for inquiry, add, delete, and update operations

### Cust Number

- Length: 10 characters
- Right justified, zero-filled
- Required field for inquiry, add, delete, and update operations

### Issue Date

- Length: 10 characters
- Format: yyyy-mm-dd
- Required for add and update operations

### Expiry Date

- Length: 10 characters
- Format: yyyy-mm-dd
- Required for add and update operations

### Property Type

- Length: 15 characters
- Required for add and update operations

### Bedrooms

- Length: 3 digits
- Right justified, zero-filled
- Required for add and update operations

### House Value

- Length: 8 digits
- Right justified, zero-filled
- Required for add and update operations

### House Name

- Length: 20 characters
- Required for add and update operations

### House Number

- Length: 4 characters
- Required for add and update operations

### Postcode

- Length: 8 characters
- Required for add and update operations

### Select Option

- Single digit
- Must enter a valid option (1-4)
- Required field

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](/)</sup></SwmMeta>
