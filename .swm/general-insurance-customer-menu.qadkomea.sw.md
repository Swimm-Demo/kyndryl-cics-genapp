---
title: General Insurance Customer Menu
---
The General Insurance Customer Menu screen (SSMAPC1) serves as the main interface for managing customer-related transactions. It allows users to perform customer inquiries, add new customers, and update existing customer details by entering relevant information such as customer number, name, date of birth, and contact details.

## Screen Preview

```
SSC1        General Insurance Customer Menu

    1. Cust Inquiry
    2. Cust Add
    
    4. Cust Update

    Cust Number     ____________
    Cust Name :First ____________
              :Last  ____________________
    DOB           ____________ (yyyy-mm-dd)
    House Name    ____________________
    House Number  ____
    Postcode      ________
    Phone: Home   ____________________
    Phone: Mob    ____________________
    Email  Addr   ___________________________

    Select Option _

[Error/Status Message Area]

ENTER=Continue  F3=Back  F4=Clear
```

## Fields

### Cust Number (ENT1CNO)

- Length: 10 characters
- Right justified, zero-filled
- Input field, must be entered by the user

### Cust Name :First (ENT1FNA)

- Length: 10 characters
- Input field, user can enter first name

### Cust Name :Last (ENT1LNA)

- Length: 20 characters
- Input field, user can enter last name

### DOB (ENT1DOB)

- Length: 10 characters
- Input field, format must be yyyy-mm-dd

### House Name (ENT1HNM)

- Length: 20 characters
- Input field, user can enter house name

### House Number (ENT1HNO)

- Length: 4 characters
- Input field, user can enter house number

### Postcode (ENT1HPC)

- Length: 8 characters
- Input field, user can enter postcode

### Phone: Home (ENT1HP1)

- Length: 20 characters
- Input field, user can enter home phone number

### Phone: Mob (ENT1HP2)

- Length: 20 characters
- Input field, user can enter mobile phone number

### Email Addr (ENT1HMO)

- Length: 27 characters
- Input field, user can enter email address

### Select Option (ENT1OPT)

- Length: 1 character
- Numeric input field
- Must be entered by the user

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1jaWNzLWdlbmFwcCUzQSUzQVN3aW1tLURlbW8=" repo-name="kyndryl-cics-genapp"><sup>Powered by [Swimm](/)</sup></SwmMeta>
