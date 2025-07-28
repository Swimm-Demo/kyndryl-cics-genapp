**FREE

// Commercial Property Insurance Application
// Modern RPGLE Implementation with Proper Procedures
// Program: SSMAPP4

ctl-opt option(*nodebugio : *srcstmt) main(main);

// Procedure Prototypes
dcl-pr displayMenu end-pr;
dcl-pr getMenuOption packed(3:0) end-pr;
dcl-pr policyInquiry end-pr;
dcl-pr policyDelete end-pr;
dcl-pr policyAdd end-pr;
dcl-pr getPropertyDetails end-pr;
dcl-pr selectPerils end-pr;
dcl-pr calculateRiskAssessment end-pr;
dcl-pr checkHighRiskLocation end-pr;
dcl-pr calculatePremium end-pr;
dcl-pr checkMultiPerilDiscount end-pr;
dcl-pr determinePolicyStatus end-pr;
dcl-pr storePolicyData end-pr;
dcl-pr displayPolicySummary end-pr;

// Global Data Definitions
dcl-ds globalData template;
  customerNumber char(10);
  propertyType char(20);
  locationDetails char(100);
  postcode char(10);
  selectedPerils char(4);  // FCFW: Fire,Crime,Flood,Weather
  baseRiskScore packed(5:2) inz(100.00);
  propertyRisk packed(5:2) inz(0.00);
  locationRisk packed(5:2) inz(0.00);
  finalRiskScore packed(5:2);
  firePremium packed(9:2) inz(0.00);
  crimePremium packed(9:2) inz(0.00);
  floodPremium packed(9:2) inz(0.00);
  weatherPremium packed(9:2) inz(0.00);
  basePremium packed(9:2) inz(1000.00);
  totalPremium packed(9:2);
  discountApplied ind inz(*off);
  policyStatus char(30);
end-ds;

dcl-ds gData likeds(globalData);

// High-Risk Postcodes Array
dcl-s highRiskPostcodes char(10) dim(10) ctdata;

// File Declarations
dcl-f DB2FILE usage(*output) template;
dcl-f VSAMFILE usage(*output) keyed template;

// Main Program
dcl-proc main;
  dcl-pi main end-pi;
  
  dcl-s menuOption packed(3:0);
  dcl-s continueFlag ind inz(*on);
  
  dow continueFlag;
    displayMenu();
    menuOption = getMenuOption();
    
    select;
      when menuOption = 1;
        policyInquiry();
      when menuOption = 2;
        policyAdd();
      when menuOption = 3;
        policyDelete();
      when menuOption = 0;
        continueFlag = *off;
      other;
        dsply 'Invalid option. Please try again.';
    endsl;
  enddo;
  
  dsply 'Thank you for using SSMAPP4';
end-proc;

// Display Main Menu
dcl-proc displayMenu;
  dcl-pi displayMenu end-pi;
  
  dsply '=== SSMAPP4 - Commercial Property Insurance ===';
  dsply '1. Policy Inquiry';
  dsply '2. Policy Add';
  dsply '3. Policy Delete';
  dsply '0. Exit';
  dsply 'Select option: ';
end-proc;

// Get Menu Selection
dcl-proc getMenuOption;
  dcl-pi getMenuOption packed(3:0) end-pi;
  
  dcl-s option packed(3:0);
  dcl-s input char(10);
  
  dsply 'Enter choice: ' '' input;
  monitor;
    option = %dec(input : 3 : 0);
  on-error;
    option = -1;
  endmon;
  
  return option;
end-proc;

// Policy Inquiry Function
dcl-proc policyInquiry;
  dcl-pi policyInquiry end-pi;
  
  dsply 'Policy Inquiry function - Not implemented in this example';
end-proc;

// Policy Delete Function
dcl-proc policyDelete;
  dcl-pi policyDelete end-pi;
  
  dsply 'Policy Delete function - Not implemented in this example';
end-proc;

// Main Policy Add Function
dcl-proc policyAdd;
  dcl-pi policyAdd end-pi;
  
  dsply '=== Policy Add - Commercial Property ===';
  
  getPropertyDetails();
  calculateRiskAssessment();
  calculatePremium();
  determinePolicyStatus();
  storePolicyData();
  displayPolicySummary();
end-proc;

// Get Commercial Property Details
dcl-proc getPropertyDetails;
  dcl-pi getPropertyDetails end-pi;
  
  dcl-s input char(100);
  
  dsply 'Enter Customer Number: ' '' input;
  gData.customerNumber = %trim(input);
  
  dsply 'Property Type (WAREHOUSE/FACTORY/OFFICE/RETAIL): ' '' input;
  gData.propertyType = %upper(%trim(input));
  
  dsply 'Enter Location Details: ' '' input;
  gData.locationDetails = %trim(input);
  
  dsply 'Enter Postcode: ' '' input;
  gData.postcode = %upper(%trim(input));
  
  selectPerils();
end-proc;

// Select Insurance Perils
dcl-proc selectPerils;
  dcl-pi selectPerils end-pi;
  
  dcl-s perilChoice char(1);
  dcl-s input char(10);
  
  dsply '=== Select Perils to Cover ===';
  gData.selectedPerils = '0000';
  
  dsply 'Include Fire coverage? (Y/N): ' '' input;
  perilChoice = %upper(%subst(input:1:1));
  if perilChoice = 'Y';
    %subst(gData.selectedPerils:1:1) = '1';
  endif;
  
  dsply 'Include Crime coverage? (Y/N): ' '' input;
  perilChoice = %upper(%subst(input:1:1));
  if perilChoice = 'Y';
    %subst(gData.selectedPerils:2:1) = '1';
  endif;
  
  dsply 'Include Flood coverage? (Y/N): ' '' input;
  perilChoice = %upper(%subst(input:1:1));
  if perilChoice = 'Y';
    %subst(gData.selectedPerils:3:1) = '1';
  endif;
  
  dsply 'Include Weather coverage? (Y/N): ' '' input;
  perilChoice = %upper(%subst(input:1:1));
  if perilChoice = 'Y';
    %subst(gData.selectedPerils:4:1) = '1';
  endif;
end-proc;

// Calculate Risk Assessment
dcl-proc calculateRiskAssessment;
  dcl-pi calculateRiskAssessment end-pi;
  
  // Start with base risk score of 100
  gData.finalRiskScore = gData.baseRiskScore;
  
  // Add Property Type Risk
  select;
    when gData.propertyType = 'WAREHOUSE';
      gData.propertyRisk = 50.00;
    when gData.propertyType = 'FACTORY';
      gData.propertyRisk = 75.00;
    when gData.propertyType = 'OFFICE';
      gData.propertyRisk = 25.00;
    when gData.propertyType = 'RETAIL';
      gData.propertyRisk = 40.00;
    other;
      gData.propertyRisk = 30.00; // Default
  endsl;
  
  gData.finalRiskScore += gData.propertyRisk;
  
  // Check for High-Risk Location
  checkHighRiskLocation();
  
  gData.finalRiskScore += gData.locationRisk;
  
  dsply ('Risk Assessment Complete:');
  dsply ('Base Risk Score: ' + %char(gData.baseRiskScore));
  dsply ('Property Type Risk: +' + %char(gData.propertyRisk));
  dsply ('Location Risk: +' + %char(gData.locationRisk));
  dsply ('Final Risk Score: ' + %char(gData.finalRiskScore));
end-proc;

// Check if Location is High-Risk
dcl-proc checkHighRiskLocation;
  dcl-pi checkHighRiskLocation end-pi;
  
  dcl-s i packed(3:0);
  
  gData.locationRisk = 0.00;
  
  for i = 1 to %elem(highRiskPostcodes);
    if gData.postcode = highRiskPostcodes(i);
      gData.locationRisk = 30.00;
      dsply 'High-risk postcode identified: +30 risk';
      return;
    endif;
  endfor;
end-proc;

// Calculate Premium
dcl-proc calculatePremium;
  dcl-pi calculatePremium end-pi;
  
  gData.totalPremium = 0.00;
  
  dsply '=== Premium Calculation ===';
  
  // Calculate individual peril premiums
  if %subst(gData.selectedPerils:1:1) = '1';
    gData.firePremium = gData.basePremium * 0.8;
    gData.totalPremium += gData.firePremium;
    dsply ('Fire Premium: £' + %char(gData.firePremium));
  endif;
  
  if %subst(gData.selectedPerils:2:1) = '1';
    gData.crimePremium = gData.basePremium * 0.6;
    gData.totalPremium += gData.crimePremium;
    dsply ('Crime Premium: £' + %char(gData.crimePremium));
  endif;
  
  if %subst(gData.selectedPerils:3:1) = '1';
    gData.floodPremium = gData.basePremium * 1.2;
    gData.totalPremium += gData.floodPremium;
    dsply ('Flood Premium: £' + %char(gData.floodPremium));
  endif;
  
  if %subst(gData.selectedPerils:4:1) = '1';
    gData.weatherPremium = gData.basePremium * 0.9;
    gData.totalPremium += gData.weatherPremium;
    dsply ('Weather Premium: £' + %char(gData.weatherPremium));
  endif;
  
  // Check for multi-peril discount
  checkMultiPerilDiscount();
  
  dsply ('Total Premium: £' + %char(gData.totalPremium));
end-proc;

// Check Multi-Peril Discount
dcl-proc checkMultiPerilDiscount;
  dcl-pi checkMultiPerilDiscount end-pi;
  
  dcl-s perilCount packed(3:0) inz(0);
  dcl-s i packed(3:0);
  
  for i = 1 to 4;
    if %subst(gData.selectedPerils:i:1) = '1';
      perilCount += 1;
    endif;
  endfor;
  
  if perilCount = 4;
    gData.totalPremium = gData.totalPremium * 0.9; // 10% discount
    gData.discountApplied = *on;
    dsply 'Multi-peril discount applied: 10%';
  endif;
end-proc;

// Determine Policy Status
dcl-proc determinePolicyStatus;
  dcl-pi determinePolicyStatus end-pi;
  
  dsply '=== Policy Status Determination ===';
  
  select;
    when gData.finalRiskScore > 200;
      gData.policyStatus = 'Manual Review Required';
      dsply 'Status: Manual Review Required (Risk > 200)';
    when gData.finalRiskScore >= 151;
      gData.policyStatus = 'Pending Review';
      dsply 'Status: Pending Review (Risk 151-200)';
    other;
      gData.policyStatus = 'Auto-Approved';
      dsply 'Status: Auto-Approved (Risk ≤ 150)';
  endsl;
end-proc;

// Store Policy Data
dcl-proc storePolicyData;
  dcl-pi storePolicyData end-pi;
  
  dsply '=== Storing Policy Data ===';
  
  // Write to DB2
  dsply 'Writing to DB2 database...';
  // In real implementation, would use SQL statements
  
  // Write to VSAM
  dsply 'Writing to VSAM file...';
  // In real implementation, would use VSAM file operations
  
  dsply 'Policy data stored successfully.';
end-proc;

// Display Policy Summary
dcl-proc displayPolicySummary;
  dcl-pi displayPolicySummary end-pi;
  
  dsply '=== POLICY SUMMARY ===';
  dsply ('Customer Number: ' + gData.customerNumber);
  dsply ('Property Type: ' + gData.propertyType);
  dsply ('Location: ' + gData.locationDetails);
  dsply ('Postcode: ' + gData.postcode);
  dsply ('Selected Perils: ' + gData.selectedPerils);
  dsply ('Final Risk Score: ' + %char(gData.finalRiskScore));
  dsply ('Total Premium: £' + %char(gData.totalPremium));
  if gData.discountApplied;
    dsply 'Multi-peril discount: Applied';
  endif;
  dsply ('Policy Status: ' + gData.policyStatus);
  dsply '========================';
end-proc;

**CTDATA highRiskPostcodes
M1 1AA    
B1 1AA    
L1 1AA    
LS1 1AA   
S1 1AA    
NE1 1AA   
CF1 1AA   
G1 1AA    
BT1 1AA   
PL1 1AA