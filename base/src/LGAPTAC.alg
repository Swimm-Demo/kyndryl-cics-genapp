BEGIN
  COMMENT
  Program: LGAPTAC (Transaction Authentication Code Generator)

  
  Purpose:
  Generates a secure transaction authentication code (TAC) for 
  insurance premium quotes.

  The algorithm uses iterative XOR operations to create a unique
  8-character code based on transaction data and a secret key.
  This provides data integrity verification for premium quotes.
  ;

  PROCEDURE GENERATE_TAC(TRANSACTION_DATA, SECRET_KEY, TAC_OUTPUT);
    VALUE TRANSACTION_DATA, SECRET_KEY;
    STRING TRANSACTION_DATA, SECRET_KEY, TAC_OUTPUT;
  BEGIN
    COMMENT
    Main procedure to generate Transaction Authentication Code.
    ;
    
    INTEGER I, J, DATA_LEN, KEY_LEN, CURRENT_VAL, ROTATION;
    INTEGER ARRAY MAC_BLOCK[1:8];
    INTEGER ARRAY TEMP_BLOCK[1:8];

    DATA_LEN := LENGTH(TRANSACTION_DATA);
    KEY_LEN := LENGTH(SECRET_KEY);

    COMMENT Initialize MAC block with secret key;
    FOR I := 1 STEP 1 UNTIL 8 DO
    BEGIN
      IF I <= KEY_LEN THEN
        MAC_BLOCK[I] := RANK(SECRET_KEY[I])
      ELSE
        MAC_BLOCK[I] := 65; COMMENT Default to 'A' if key too short;
    END;

    COMMENT
    Process transaction data - XOR each byte with MAC block.
    Use modulo arithmetic to cycle through the 8-byte block.
    This creates a dependency on all input data.
    ;
    FOR I := 1 STEP 1 UNTIL DATA_LEN DO
    BEGIN
      CURRENT_VAL := RANK(TRANSACTION_DATA[I]);
      J := ((I - 1) MOD 8) + 1;
      
      COMMENT XOR current character with MAC block position;
      MAC_BLOCK[J] := BITXOR(MAC_BLOCK[J], CURRENT_VAL);
      
      COMMENT Additional mixing - rotate with neighbor;
      IF J < 8 THEN
        MAC_BLOCK[J + 1] := BITXOR(MAC_BLOCK[J + 1], MAC_BLOCK[J])
      ELSE
        MAC_BLOCK[1] := BITXOR(MAC_BLOCK[1], MAC_BLOCK[8]);
    END;

    COMMENT
    Final mixing pass - XOR each position with cumulative sum.
    This ensures all positions depend on complete input.
    ;
    ROTATION := 0;
    FOR I := 1 STEP 1 UNTIL 8 DO
    BEGIN
      ROTATION := ROTATION + MAC_BLOCK[I];
      TEMP_BLOCK[I] := BITXOR(MAC_BLOCK[I], (ROTATION MOD 256));
    END;

    FOR I := 1 STEP 1 UNTIL 8 DO
    BEGIN
      CURRENT_VAL := TEMP_BLOCK[I] MOD 36;
      
      IF CURRENT_VAL < 10 THEN
        TAC_OUTPUT[I] := CHR(48 + CURRENT_VAL) COMMENT 0-9;
      ELSE
        TAC_OUTPUT[I] := CHR(55 + CURRENT_VAL); COMMENT A-Z;
    END;

  END;
  
  PROCEDURE VERIFY_TAC(TRANSACTION_DATA, SECRET_KEY, PROVIDED_TAC, 
                       IS_VALID);
    VALUE TRANSACTION_DATA, SECRET_KEY, PROVIDED_TAC;
    STRING TRANSACTION_DATA, SECRET_KEY, PROVIDED_TAC;
    INTEGER IS_VALID;
  BEGIN
    COMMENT
    Verifies that a provided TAC matches the transaction data.
    Returns 1 if valid, 0 if invalid.
    ;
    
    STRING CALCULATED_TAC[1:8];
    INTEGER I;
    BOOLEAN MATCH;
    
    COMMENT Generate TAC from current transaction data;
    GENERATE_TAC(TRANSACTION_DATA, SECRET_KEY, CALCULATED_TAC);
    
    COMMENT Compare with provided TAC;
    MATCH := TRUE;
    FOR I := 1 STEP 1 UNTIL 8 DO
    BEGIN
      IF CALCULATED_TAC[I] <> PROVIDED_TAC[I] THEN
        MATCH := FALSE;
    END;
    
    IF MATCH THEN
      IS_VALID := 1
    ELSE
      IS_VALID := 0;
      
  END; COMMENT End of VERIFY_TAC procedure;

  COMMENT
  Main program block - Entry point for COBOL CALL statement.
  
  When COBOL executes: CALL 'LGAPTAC' USING PARM1, PARM2, PARM3
  Control transfers to this main block, which receives the parameters
  and calls the appropriate procedure.
  
  Parameter convention (matches COBOL linkage):
  - First parameter: Transaction data string (input)
  - Second parameter: Secret key string (input)
  - Third parameter: TAC output string (output)
  ;
  
  STRING COBOL_PARM1[1:100];  COMMENT Transaction data from COBOL;
  STRING COBOL_PARM2[1:8];    COMMENT Secret key from COBOL;
  STRING COBOL_PARM3[1:8];    COMMENT TAC output to COBOL;
  
  GENERATE_TAC(COBOL_PARM1, COBOL_PARM2, COBOL_PARM3);

END;
