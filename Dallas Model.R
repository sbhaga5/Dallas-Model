library("readxl")
library(ggplot2)
library(tidyverse)
setwd(getwd())
rm(list = ls())
#
#User can change these values
StartYear <- 2019
StartProjectionYear <- 2020
EndProjectionYear <- 2075
FileName <- 'Model Inputs.xlsx'
#
#Reading Input File
user_inputs_numeric <- read_excel(FileName, sheet = 'Numeric Inputs')
user_inputs_character <- read_excel(FileName, sheet = 'Character Inputs')
Historical_Data <- read_excel(FileName, sheet = 'Historical Data')
Scenario_Data <- read_excel(FileName, sheet = 'Inv_Returns')
City_Payroll <- read_excel(FileName, sheet = 'City_Payroll')
BenefitPayments <- read_excel(FileName, sheet = 'Benefit Payments')
DurConvex <- read_excel(FileName, sheet = 'Duration and Convexity')
#
##################################################################################################################################################################
#
#Functions for later use
#Function for Present Value for Amortization
PresentValue = function(rate, nper, pmt) {
  PV = pmt * (1 - (1 + rate) ^ (-nper)) / rate * (1 + rate)
  return(PV)
}

NPV = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }
  
  return(NPV)
}

#Function for calculating amo payments
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
PMT0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  if(nper == 0){
    a <- 0
  }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period. 
PMT <- function(r, g = 0, nper, pv, t = 1) {
  a <- PMT0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}
#
##################################################################################################################################################################
#
#Reading Values from Input input file and assigning values
#Assigning numeric inputs
for(i in 1:nrow(user_inputs_numeric)){
  if(!is.na(user_inputs_numeric[i,2])){
    assign(as.character(user_inputs_numeric[i,2]),as.double(user_inputs_numeric[i,3]))
  }
}
#
#Assigning character inputs
for(i in 1:nrow(user_inputs_character)){
  if(!is.na(user_inputs_character[i,2])){
    assign(as.character(user_inputs_character[i,2]),as.character(user_inputs_character[i,3]))
  }
}

#Create an empty Matrix for the Projection Years
EmptyMatrix <- matrix(0,(EndProjectionYear - StartProjectionYear + 1), 1)
for(j in 1:length(colnames(Historical_Data))){
  TempMatrix <- rbind(as.matrix(Historical_Data[,j]), EmptyMatrix)
  assign(as.character(colnames(Historical_Data)[j]), TempMatrix)
}
#Assign values for Projection Years
FYE <- StartYear:EndProjectionYear
#Get Start Index, since historical data has 3 rows, we want to start at 4
StartIndex <- StartProjectionYear - StartYear + 1
HistoricalIndex <- StartProjectionYear - StartYear

#Initialize the Inflation Adjusted Variables to use later
UAL_AVA_InflAdj <- UAL_AVA
colnames(UAL_AVA_InflAdj) <- 'UAL_AVA_InflAdj'
UAL_MVA_InflAdj <- UAL_MVA
colnames(UAL_MVA_InflAdj) <- 'UAL_MVA_InflAdj'

#Assign values for simulation
if(SimType == 'Assumed'){
  SimReturn <- SimReturnAssumed
} else if(AnalysisType == 'Conservative'){
  SimReturn <- SimReturnConservative
}

#Initialize Amortization and Outstnading Base
RowColCount <- (EndProjectionYear - StartProjectionYear + 1)
OutstandingBase_CurrentHires <- matrix(0,RowColCount, RowColCount + 1)
Amortization_CurrentHires <- matrix(0,RowColCount, RowColCount + 1)
AmoYearsInput_CurrentHires <- read_excel(FileName, sheet = 'Amortization_CurrentHires')

OutstandingBase_NewHires <- matrix(0,RowColCount, RowColCount + 1)
Amortization_NewHires <- matrix(0,RowColCount, RowColCount + 1)
AmoYearsInput_NewHires <- read_excel(FileName, sheet = 'Amortization_NewHires')
#
##################################################################################################################################################################
#
#Offset Matrix. Used later for Amortization calculation
OffsetYears_CurrentHires <- matrix(0,RowColCount, RowColCount)
for(i in 1:nrow(OffsetYears_CurrentHires)){
  RowCount <- i
  ColCount <- 1
  #This is to create the "zig-zag" pattern of amo years. you also want it to become 0 if the amo years is greater than the payments
  #Meaning if its 10 years, then after 10 years, the amo payment is 0
  while(RowCount <= nrow(OffsetYears_CurrentHires) && (ColCount <= as.double(AmoYearsInput_CurrentHires[i,2]))){
    OffsetYears_CurrentHires[RowCount,ColCount] <- as.double(AmoYearsInput_CurrentHires[i,2])
    RowCount <- RowCount + 1
    ColCount <- ColCount + 1
  }
}

for(i in 1:nrow(OffsetYears_CurrentHires)-1){
  for(j in 1:i+1){
    if(OffsetYears_CurrentHires[i+1,j] > 0) {
      OffsetYears_CurrentHires[i+1,j] <- max(OffsetYears_CurrentHires[i+1,j] - j + 1, 1) 
    }
  }
}

OffsetYears_NewHires <- matrix(0,RowColCount, RowColCount)
for(i in 1:nrow(OffsetYears_NewHires)){
  RowCount <- i
  ColCount <- 1
  #This is to create the "zig-zag" pattern of amo years. you also want it to become 0 if the amo years is greater than the payments
  #Meaning if its 10 years, then after 10 years, the amo payment is 0
  while(RowCount <= nrow(OffsetYears_NewHires) && (ColCount <= as.double(AmoYearsInput_NewHires[i,2]))){
    OffsetYears_NewHires[RowCount,ColCount] <- as.double(AmoYearsInput_NewHires[i,2])
    RowCount <- RowCount + 1
    ColCount <- ColCount + 1
  }
}

for(i in 1:nrow(OffsetYears_NewHires)-1){
  for(j in 1:i+1){
    if(OffsetYears_NewHires[i+1,j] > 0) {
      OffsetYears_NewHires[i+1,j] <- max(OffsetYears_NewHires[i+1,j] - j + 1, 1)
    }
  }
}

#Initialize first row of Amortization and Outstanding Base
OutstandingBase_CurrentHires[1,1] <- UAL_AVA_CurrentHires[HistoricalIndex]
rate <- ((1 + NewDR_CurrentHires[HistoricalIndex]) / (1 + AmoBaseInc)) - 1
period <- max(OffsetYears_CurrentHires[1,1], 1)
pmt <- 1
Amortization_CurrentHires[1,1] <- OutstandingBase_CurrentHires[1,1] / (PresentValue(rate,period,pmt) / ((1+NewDR_CurrentHires[HistoricalIndex])^0.5))

OutstandingBase_NewHires[1,1] <- UAL_AVA_NewHires[HistoricalIndex] 
rate <- ((1 + NewDR_NewHires[HistoricalIndex]) / (1 + AmoBaseInc)) - 1
period <- max(OffsetYears_NewHires[1,1], 1)
pmt <- 1
Amortization_NewHires[1,1] <- OutstandingBase_NewHires[1,1] / (PresentValue(rate,period,pmt) / ((1+NewDR_NewHires[HistoricalIndex])^0.5))
#
##################################################################################################################################################################
#
#Supplemental Contribution is 0 in this case because it is only run with actual values in the scenarios at the end. By default its 0
#Its also supposed to be c(0) instead of 0 because matrix + double doesnt work, so it has to be a matrix for the addition to work
SupplContrib <- c(0)
#Statutory Amo is a hard coded number the actuary put in for numbers before 2024
StatutoryAmo <- 26*c(6.224, 6.382, 6.543, 6.312, 6.524)

#This is payroll upto acrrued liability. these values do not change regardless of scenario or other stress testing
#In order to optimize, they are placed outside the function
for(i in StartIndex:length(FYE)){
  #Payroll
  if(FYE[i] <= 2037){
    TotalPayroll[i] <- City_Payroll$Payroll[i]
  } else {
    TotalPayroll[i] <- TotalPayroll[i-1]*(1 + Payroll_growth) 
  }
  PayrollTier1Growth <- (1 - Payroll_growth_T1_Param1) - (Payroll_growth_T1_Param1*Payroll_growth_T1_Param2)*(FYE[i-1] - Payroll_growth_T1_AnchorYear)
  PayrollTier2Growth <- (1 - Payroll_growth_T2_Param1) - (Payroll_growth_T2_Param1*Payroll_growth_T2_Param2)*(FYE[i-1] - Payroll_growth_T2_AnchorYear)
  
  #If its greater than the Tier end year, 0
  if(FYE[i] == 2020){
    Tier2Payroll[i] <- 135.2126024
  } else if(FYE[i] >= Payroll_growth_T2_TierEndYear){
    Tier2Payroll[i] <- 0
  } else {
    Tier2Payroll[i] <- Tier2Payroll[i-1]*PayrollTier2Growth
  }
  
  #If its greater than the Tier end year, 0
  if(FYE[i] >= Payroll_growth_T1_TierEndYear){
    Tier1Payroll[i] <- 0
  } else {
    Tier1Payroll[i] <- Tier1Payroll[i-1]*PayrollTier1Growth
  }
  
  if(NewHirePlan == "DC"){
    NewHiresDBHybridPayroll[i] <- 0
    BenPayments_NewHires[i] <- 0
  } else {
    NewHiresDBHybridPayroll[i] <- TotalPayroll[i] - (Tier1Payroll[i] + Tier2Payroll[i])
    BenPayments_NewHires[i] <- as.double(BenefitPayments$NewHires[i])
  }
  NewHiresDCPayroll[i] <- TotalPayroll[i] - (Tier1Payroll[i] + Tier2Payroll[i] + NewHiresDBHybridPayroll[i])
  #
  #Discount Rate
  OriginalDR_CurrentHires[i] <- dis_r_currentHires
  NewDR_CurrentHires[i] <- dis_r_proj_currentHires
  OriginalDR_NewHires[i] <- dis_r_newHires
  NewDR_NewHires[i] <- dis_r_proj_newHires
  #
  #Benefit Payments, Admin Expenses
  BenPayments_CurrentHires[i] <- as.double(BenefitPayments$CurrentHires[i])
  BenPayments_DROP[i] <- as.double(BenefitPayments$DROP[i])
  
  AcctBal[i] <- as.double(BenefitPayments$AccountBalance[i])
  if(i < nrow(BenefitPayments)){
    PVB[i] <- -1*(1 + DROP_DR)^PayTimeEOY*NPV(DROP_DR,BenefitPayments$DROP[(i+1):nrow(BenefitPayments)]) 
  } else {
    PVB[i] <- 0
  }
  AAL[i] <- PVB[i] - AcctBal[i]
  
  AdminExp_CurrentHires[i] <- -1*max(Admin_Exp_Pct*(Tier1Payroll[i] + Tier2Payroll[i]), Admin_Exp_Max*(Tier1Payroll[i] + Tier2Payroll[i])/(Tier1Payroll[i] + Tier2Payroll[i] + NewHiresDBHybridPayroll[i]))
  AdminExp_NewHires[i] <- -1*max(Admin_Exp_Pct*NewHiresDBHybridPayroll[i], Admin_Exp_Max*NewHiresDBHybridPayroll[i]/(Tier1Payroll[i] + Tier2Payroll[i] + NewHiresDBHybridPayroll[i]))
  #
  #Accrued Liability, MOY NC - Original DR
  MOYNCExistOrigDR[i] <- (Tier1Payroll[i]*NC_Tier1 + Tier2Payroll[i]*NC_Tier2)
  MOYNCNewHiresOrigDR[i] <- (NewHiresDBHybridPayroll[i]*NC_NewHires_Pct)*(1 + NCGrowth)^(FYE[i] - NC_StaryYear + 1)
  AccrLiabOrigDR_CurrentHires_NoDROP[i] <- AccrLiabOrigDR_CurrentHires_NoDROP[i-1]*(1+OriginalDR_CurrentHires[i-1]) + (MOYNCExistOrigDR[i])*(1+OriginalDR_CurrentHires[i-1])^0.5 + (BenPayments_CurrentHires[i])*(1+OriginalDR_CurrentHires[i-1])^PayTimeEOY
  AccrLiabOrigDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i-1]*(1+OriginalDR_NewHires[i-1]) + (MOYNCNewHiresOrigDR[i])*(1+OriginalDR_NewHires[i-1])^0.5 + (BenPayments_NewHires[i])*(1+OriginalDR_NewHires[i-1])^PayTimeEOY
  #
  #Accrued Liability, MOY NC - New DR
  NewHiresAALDur[i] <- max(NCSensDR_NewHires - NewHireSensSteps*(FYE[i] - 2021), LiabSensDR_NewHires)
  DRDifference_CurrentHires <- 100*(OriginalDR_CurrentHires[i] - NewDR_CurrentHires[i])
  DRDifference_NewHires <- 100*(OriginalDR_NewHires[i] - NewDR_NewHires[i])
  MOYNCExistNewDR[i] <- MOYNCExistOrigDR[i]*((1+(NCSensDR/100))^(DRDifference_CurrentHires))
  MOYNCNewHiresNewDR[i] <- MOYNCNewHiresOrigDR[i]*((1+(NCSensDR/100))^(DRDifference_NewHires))
  
  LiabSensDR <- as.double(DurConvex$Current_AAL_Dur[i])
  Convexity <- as.double(DurConvex$Current_AAL_Conv[i])
  AccrLiabNewDR_CurrentHires_NoDROP[i] <- AccrLiabOrigDR_CurrentHires_NoDROP[i]*((1+(LiabSensDR/100))^(DRDifference_NewHires))*((1+(Convexity/100))^((DRDifference_NewHires)^2/2))
  LiabSensDR <- as.double(DurConvex$NewHire_AAL_Dur[i])
  Convexity <- as.double(DurConvex$NewHire_AAL_Conv[i])
  AccrLiabNewDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i]*((1+(LiabSensDR/100))^(DRDifference_NewHires))*((1+(Convexity/100))^((DRDifference_NewHires)^2/2))
}
#
##################################################################################################################################################################
#Running the model from NC onwards. this gets iterated for different scenarios
RunModel <- function(AnalysisType, SimReturn, SimVolatility, ER_Policy, ScenType, SupplContrib, LvDollarorPercent, SegalOrReason){
  #Scenario Index for referencing later based on investment return data
  ScenarioIndex <- which(colnames(Scenario_Data) == as.character(ScenType))
  
  #Default value is Lv% for Amo Base
  #If its Level Perecent, then set to 0
  if(LvDollarorPercent == 'Lv$'){
    AmoBaseInc <- 0
    
    #Recalculate first value for Lv$
    OutstandingBase_CurrentHires[1,1] <- UAL_AVA_CurrentHires[HistoricalIndex]
    rate <- ((1 + NewDR_CurrentHires[HistoricalIndex]) / (1 + AmoBaseInc)) - 1
    period <- max(OffsetYears_CurrentHires[1,1], 1)
    pmt <- 1
    Amortization_CurrentHires[1,1] <- OutstandingBase_CurrentHires[1,1] / (PresentValue(rate,period,pmt) / ((1+NewDR_CurrentHires[HistoricalIndex])^0.5))
    
    OutstandingBase_NewHires[1,1] <- UAL_AVA_NewHires[HistoricalIndex] 
    rate <- ((1 + NewDR_NewHires[HistoricalIndex]) / (1 + AmoBaseInc)) - 1
    period <- max(OffsetYears_NewHires[1,1], 1)
    pmt <- 1
    Amortization_NewHires[1,1] <- OutstandingBase_NewHires[1,1] / (PresentValue(rate,period,pmt) / ((1+NewDR_NewHires[HistoricalIndex])^0.5))
  }
  #intialize this value at 0 for Total ER Contributions
  Total_ER[StartIndex-1] <- 0
  for(i in StartIndex:length(FYE)){
    #DROP values are going to change if its Segal or Reason so total liability needs to be calculated here
    if(SegalOrReason == 'Segal'){
      AccrLiabOrigDR_CurrentHires_DROP[i] <-  AccrLiabOrigDR_CurrentHires_DROP[i-1]*(1+OriginalDR_CurrentHires[i-1]) + as.double(BenefitPayments$DROP[i])*(1+OriginalDR_CurrentHires[i-1])^PayTimeEOY
    } else {
      AccrLiabOrigDR_CurrentHires_DROP[i] <-  AAL[i]
    }
    AccrLiabOrigDR_Total[i] <- AccrLiabOrigDR_CurrentHires_NoDROP[i] + AccrLiabOrigDR_CurrentHires_DROP[i] + AccrLiabOrigDR_NewHires[i]
    AccrLiabNewDR_CurrentHires_DROP[i] <- AccrLiabOrigDR_CurrentHires_DROP[i]
    AccrLiabNewDR_Total[i] <- AccrLiabNewDR_CurrentHires_NoDROP[i] + AccrLiabNewDR_CurrentHires_DROP[i] + AccrLiabNewDR_NewHires[i]
    
    #Contribution Policy, Amortization Policy
    #These values need to be weeded out for dividing by 0. So initialize to 0 and then check later
    Tier12Payroll <- Tier1Payroll[i] + Tier2Payroll[i]
    NC_CurrentHires[i] <- 0
    EmployerNC_CurrentHires[i] <- 0
    AmoRate_CurrentHires[i] <- 0
    
    if(Amo_Type == 'DB-only Payroll'){
      NewHirePayroll <- NewHiresDBHybridPayroll[i]
    } else {
      NewHirePayroll <- NewHiresDBHybridPayroll[i] + NewHiresDCPayroll[i]
    }
    
    #ProjectionCount is used because amortization and return scenarios do not start at the same time as start index
    #Because start index includes historical data and thus might be 3 by the time ProjectionCount is 1
    ProjectionCount <- i - StartIndex + 1
    
    #Calculate the rates which makes it easier for debugging later
    #Check if payroll is 0 before dividing
    if(Tier12Payroll > 0){NC_CurrentHires[i] <- MOYNCExistNewDR[i] / (Tier1Payroll[i] + Tier2Payroll[i]) / (1+NewDR_CurrentHires[i])^0.5}
    NC_NewHires[i] <- MOYNCNewHiresNewDR[i] / NewHiresDBHybridPayroll[i] / (1+NewDR_NewHires[i])^0.5
    EmployeeNC_CurrentHires[i] <- EEContrib_Tier12
    if(CostSharing_NC == "No"){
      EmployeeNC_NewHires[i] <- EEContrib_NewHires
    } else {
      EmployeeNC_NewHires[i] <- (NC_NewHires[i] - (AdminExp_NewHires[i] / NewHiresDBHybridPayroll[i]))*CostSharingPct
    }
    #Check if payroll is 0 before dividing
    if(Tier12Payroll > 0){EmployerNC_CurrentHires[i] <- NC_CurrentHires[i] - EmployeeNC_CurrentHires[i] - (AdminExp_CurrentHires[i] / (Tier1Payroll[i] + Tier2Payroll[i]))}
    EmployerNC_NewHires[i] <- NC_NewHires[i] - EmployeeNC_NewHires[i] - (AdminExp_NewHires[i] / NewHiresDBHybridPayroll[i])
    
    if(ER_Policy == 'ADC'){
      if(Tier12Payroll > 0){AmoRate_CurrentHires[i] <- sum(Amortization_CurrentHires[ProjectionCount,]) / (Tier1Payroll[i] + Tier2Payroll[i])}
      AmoRate_NewHires[i] <- sum(Amortization_NewHires[ProjectionCount,]) / NewHirePayroll
    } else {
      if(FYE[i] <= 2024){
        #This is the amo rate put in by the actuary that is called on by the function
        TempIndex <- FYE[i] - 2020 + 1
        if(Tier12Payroll > 0){AmoRate_CurrentHires[i] <- (StatutoryAmo[TempIndex] / (Tier1Payroll[i] + Tier2Payroll[i] + NewHiresDBHybridPayroll[i])) - EmployerNC_CurrentHires[i]}
        AmoRate_NewHires[i] <- (StatutoryAmo[TempIndex] / (Tier1Payroll[i] + Tier2Payroll[i] + NewHirePayroll)) - EmployerNC_NewHires[i]
      } else {
        AmoRate_CurrentHires[i] <- ERContrib_Tier12 - EmployerNC_CurrentHires[i]
        AmoRate_NewHires[i] <- ERContrib_NewHires - EmployerNC_NewHires[i]
      }
    }
    
    #Use the rates to calculate EE NC, ER NC, Amo, etc.
    EE_NC_CurrentHires[i] <- EmployeeNC_CurrentHires[i]*(Tier1Payroll[i] + Tier2Payroll[i])
    EE_NC_NewHires[i] <- EmployeeNC_NewHires[i]*NewHiresDBHybridPayroll[i]
    
    if(FYE[i] == 2023){
      ER_NC_CurrentHires[i] <- EmployerNC_CurrentHires[i]*(Tier1Payroll[i] + Tier2Payroll[i]) + SupplContrib
      
    } else {
      ER_NC_CurrentHires[i] <- EmployerNC_CurrentHires[i]*(Tier1Payroll[i] + Tier2Payroll[i])
    }
    ER_NC_NewHires[i] <- EmployerNC_NewHires[i]*NewHiresDBHybridPayroll[i]
    
    if(ER_Policy == 'ADC'){
      if(AmoRate_CurrentHires[i] == 0){
        ER_Amo_CurrentHires[i] <- sum(Amortization_CurrentHires[ProjectionCount,]) 
      } else {
        ER_Amo_CurrentHires[i] <- AmoRate_CurrentHires[i]*(Tier1Payroll[i] + Tier2Payroll[i])
      }
    } else {
      ER_Amo_CurrentHires[i] <- AmoRate_CurrentHires[i]*(Tier1Payroll[i] + Tier2Payroll[i])
    }
    
    if(CostSharing_Amo == 'No'){
      EE_Amo_NewHires[i] <- 0
    } else {
      EE_Amo_NewHires[i] <- AmoRate_NewHires[i]*NewHiresDBHybridPayroll[i]*CostSharingPct
    }
    ER_Amo_NewHires[i] <- max(AmoRate_NewHires[i]*NewHirePayroll - EE_Amo_NewHires[i],-(ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i]))
    #
    #Return data based on deterministic or stochastic
    if((AnalysisType == 'Stochastic') && (i >= StartIndex)){
      ROA_MVA[i] <- rnorm(1,SimReturn,SimVolatility)
    } else if(AnalysisType == 'Deterministic'){
      ROA_MVA[i] <- as.double(Scenario_Data[i,ScenarioIndex]) 
    }
    
    #Solvency Contribution
    CashFlows_CurrentHires <- BenPayments_CurrentHires[i] + BenPayments_DROP[i] + AdminExp_CurrentHires[i] +  EE_NC_CurrentHires[i] + ER_NC_CurrentHires[i] + ER_Amo_CurrentHires[i]
    Solv_Contrib_CurrentHires[i] <- as.double(max(-(MVA_CurrentHires[i-1]*(1+ROA_MVA[i]) + CashFlows_CurrentHires*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    
    CashFlows_NewHires <- BenPayments_NewHires[i] + AdminExp_NewHires[i] +  EE_NC_NewHires[i] + ER_NC_NewHires[i] + ER_Amo_NewHires[i]
    Solv_Contrib_NewHires[i] <- as.double(max(-(MVA_NewHires[i-1]*(1+ROA_MVA[i]) + CashFlows_NewHires*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    
    CashFlows_Total <- CashFlows_CurrentHires + CashFlows_NewHires
    Solv_Contrib_Total <- as.double(max(-(MVA[i-1]*(1+ROA_MVA[i]) + CashFlows_Total*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    
    #The solvency contribution is based on the min of the current/new hires and the total so calculate both separately as above
    Solv_Contrib_CurrentHires[i] <- min(Solv_Contrib_CurrentHires[i],Solv_Contrib_Total)
    Solv_Contrib_NewHires[i] <- min(Solv_Contrib_NewHires[i],Solv_Contrib_Total)
    #
    #Net CF, Expected MVA
    NetCF_CurrentHires[i] <- CashFlows_CurrentHires + Solv_Contrib_CurrentHires[i]
    ExpInvInc_CurrentHires[i] <- (MVA_CurrentHires[i-1]*NewDR_CurrentHires[i-1]) + (NetCF_CurrentHires[i]*NewDR_CurrentHires[i-1]*0.5)
    ExpectedMVA_CurrentHires[i] <- MVA_CurrentHires[i-1] + NetCF_CurrentHires[i] + ExpInvInc_CurrentHires[i]
    MVA_CurrentHires[i] <- MVA_CurrentHires[i-1]*(1+ROA_MVA[i]) + NetCF_CurrentHires[i]*(1+ROA_MVA[i])^PayTimeEOY
    
    NetCF_NewHires[i] <- CashFlows_NewHires + Solv_Contrib_NewHires[i]
    ExpInvInc_NewHires[i] <- (MVA_NewHires[i-1]*NewDR_NewHires[i-1]) + (NetCF_NewHires[i]*NewDR_NewHires[i-1]*0.5)
    ExpectedMVA_NewHires[i] <- MVA_NewHires[i-1] + NetCF_NewHires[i] + ExpInvInc_NewHires[i]
    MVA_NewHires[i] <- MVA_NewHires[i-1]*(1+ROA_MVA[i]) + NetCF_NewHires[i]*(1+ROA_MVA[i])^PayTimeEOY
    #
    #Gain Loss, Defered Losses
    GainLoss_CurrentHires[i] <- MVA_CurrentHires[i] - ExpectedMVA_CurrentHires[i] 
    DeferedCurYear_CurrentHires[i] <- GainLoss_CurrentHires[i]*(0.8/1)
    Year1GL_CurrentHires[i] <- DeferedCurYear_CurrentHires[i-1]*(0.6/0.8)
    Year2GL_CurrentHires[i] <- Year1GL_CurrentHires[i-1]*(0.4/0.6)
    Year3GL_CurrentHires[i] <- Year2GL_CurrentHires[i-1]*(0.2/0.4)
    TotalDefered_CurrentHires[i] <- Year1GL_CurrentHires[i] + Year2GL_CurrentHires[i] + Year3GL_CurrentHires[i] + DeferedCurYear_CurrentHires[i]
    
    GainLoss_NewHires[i] <- MVA_NewHires[i] - ExpectedMVA_NewHires[i] 
    DeferedCurYear_NewHires[i] <- GainLoss_NewHires[i]*(0.8/1)
    Year1GL_NewHires[i] <- DeferedCurYear_NewHires[i-1]*(0.6/0.8)
    Year2GL_NewHires[i] <- Year1GL_NewHires[i-1]*(0.4/0.6)
    Year3GL_NewHires[i] <- Year2GL_NewHires[i-1]*(0.2/0.4)
    TotalDefered_NewHires[i] <- Year1GL_NewHires[i] + Year2GL_NewHires[i] + Year3GL_NewHires[i] + DeferedCurYear_NewHires[i]
    #
    #AVA, MVA, UA, FR
    AVA_CurrentHires[i] <- MVA_CurrentHires[i] - TotalDefered_CurrentHires[i]
    UAL_AVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires_NoDROP[i] + AccrLiabNewDR_CurrentHires_DROP[i] - AVA_CurrentHires[i]
    UAL_MVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires_NoDROP[i] + AccrLiabNewDR_CurrentHires_DROP[i] - MVA_CurrentHires[i]
    
    AVA_NewHires[i] <- MVA_NewHires[i] - TotalDefered_NewHires[i]
    UAL_AVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - AVA_NewHires[i]
    UAL_MVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - MVA_NewHires[i]
    
    UAL_AVA[i] <- UAL_AVA_CurrentHires[i] + UAL_AVA_NewHires[i]
    UAL_MVA[i] <- UAL_MVA_CurrentHires[i] + UAL_MVA_NewHires[i]
    AVA[i] <- AVA_CurrentHires[i] + AVA_NewHires[i]
    MVA[i] <- MVA_CurrentHires[i] + MVA_NewHires[i]
    FR_AVA[i] <- AVA[i] / AccrLiabNewDR_Total[i]
    FR_MVA[i] <- MVA[i] / AccrLiabNewDR_Total[i]
    UAL_AVA_InflAdj[i] <- UAL_AVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    UAL_MVA_InflAdj[i] <- UAL_MVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    #
    #Employer Contribution
    Total_Contrib_DB[i] <- ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_NewHires[i] + Solv_Contrib_CurrentHires[i] + Solv_Contrib_NewHires[i]
    if(NewHirePlan == "Hybrid"){
      Total_Contrib_DC[i] <- Hybrid_Contrib*NewHiresDBHybridPayroll[i]
    } else if (NewHirePlan == "DC"){
      Total_Contrib_DC[i] <- DC_Contrib*NewHiresDCPayroll[i]
    }
    Total_Contrib[i] <- max(Total_Contrib_DB[i] + Total_Contrib_DC[i],0)
    
    ER_InflAdj[i] <- Total_Contrib[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    ER_Percentage[i] <- Total_Contrib[i] / TotalPayroll[i]
    #Total ER starts after first year so set the first year to 0.
    if(i < (StartIndex+1)){
      Total_ER[i] <- 0
    } else {
      Total_ER[i] <- Total_ER[i-1] + ER_InflAdj[i] 
    }
    AllInCost[i] <- Total_ER[i] + UAL_MVA_InflAdj[i]
    #
    #Amortization
    #Current Hires
    if(ProjectionCount < nrow(Amortization_CurrentHires)){
      OutstandingBase_CurrentHires[ProjectionCount+1,2:(ProjectionCount + 1)] <- OutstandingBase_CurrentHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR_CurrentHires[i-1]) - (Amortization_CurrentHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR_CurrentHires[i-1])^0.5)
      OutstandingBase_CurrentHires[ProjectionCount+1,1] <- UAL_AVA_CurrentHires[i] - sum(OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)])
      
      #Amo Layers
      Amortization_CurrentHires[ProjectionCount+1,1:(ProjectionCount + 1)] <- PMT(pv = OutstandingBase_CurrentHires[ProjectionCount+1,1:(ProjectionCount + 1)], 
                                                                                  r = NewDR_CurrentHires[i-1], g = AmoBaseInc, t = 0.5,
                                                                                  nper = pmax(OffsetYears_CurrentHires[ProjectionCount+1,1:(ProjectionCount + 1)],1))
    }
    
    #New Hires
    if(ProjectionCount < nrow(Amortization_NewHires)){
      #Oustanding Balance
      OutstandingBase_NewHires[ProjectionCount+1,2:(ProjectionCount + 1)] <- OutstandingBase_NewHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR_NewHires[i-1]) - (Amortization_NewHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR_NewHires[i-1])^0.5)
      OutstandingBase_NewHires[ProjectionCount+1,1] <- UAL_AVA_NewHires[i] - sum(OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)])
      
      #Amo Layers
      Amortization_NewHires[ProjectionCount+1,1:(ProjectionCount + 1)] <- PMT(pv = OutstandingBase_NewHires[ProjectionCount+1,1:(ProjectionCount + 1)],
                                                                              r = NewDR_NewHires[i-1], g = AmoBaseInc, t = 0.5,
                                                                              nper = pmax(OffsetYears_NewHires[ProjectionCount+1,1:(ProjectionCount + 1)],1))
    }
  }
  
  Output <- cbind(FYE,TotalPayroll,Tier1Payroll,Tier2Payroll,NewHiresDBHybridPayroll,NewHiresDCPayroll,PayrollLegacy,PayrollNewTier,
                  OriginalDR_CurrentHires,NewDR_CurrentHires,OriginalDR_NewHires,NewDR_NewHires,NewHiresAALDur,AccrLiabOrigDR_Total,
                  AccrLiabOrigDR_CurrentHires_NoDROP,AccrLiabOrigDR_CurrentHires_DROP,AccrLiabOrigDR_NewHires,MOYNCExistOrigDR,MOYNCNewHiresOrigDR,
                  AccrLiabNewDR_Total,AccrLiabNewDR_CurrentHires_NoDROP,AccrLiabNewDR_CurrentHires_DROP,AccrLiabNewDR_NewHires,
                  MOYNCExistNewDR,MOYNCNewHiresNewDR,AVA,AVA_CurrentHires,AVA_NewHires,MVA,MVA_CurrentHires,MVA_NewHires,
                  ROA_MVA,UAL_AVA,UAL_AVA_InflAdj, UAL_AVA_CurrentHires,UAL_AVA_NewHires,UAL_MVA, UAL_MVA_InflAdj, UAL_MVA_CurrentHires,
                  UAL_MVA_NewHires,FR_AVA,FR_MVA, Impl_FundPeriod,TargetYear100Pct,NC_CurrentHires,NC_NewHires,EmployeeNC_CurrentHires,
                  EmployeeNC_NewHires, EmployerNC_CurrentHires,EmployerNC_NewHires,AmoRate_CurrentHires,AmoRate_NewHires,AcctBal,PVB,AAL,
                  BenPayments_CurrentHires,BenPayments_DROP,BenPayments_NewHires,AdminExp_CurrentHires,AdminExp_NewHires,
                  EE_NC_CurrentHires,EE_NC_NewHires,EE_Amo_NewHires,ER_NC_CurrentHires,ER_NC_NewHires,ER_Amo_CurrentHires,
                  ER_Amo_NewHires,Solv_Contrib_CurrentHires,Solv_Contrib_NewHires,Total_Contrib_DB,Total_Contrib_DC,Total_Contrib,
                  Years_BP,AllInCost, ER_InflAdj, Total_ER, ER_Percentage,NetCF_CurrentHires,ExpInvInc_CurrentHires,ExpectedMVA_CurrentHires,
                  GainLoss_CurrentHires,DeferedCurYear_CurrentHires,Year1GL_CurrentHires,Year2GL_CurrentHires,Year3GL_CurrentHires,
                  TotalDefered_CurrentHires,NetCF_NewHires,ExpInvInc_NewHires,ExpectedMVA_NewHires,GainLoss_NewHires,DeferedCurYear_NewHires,
                  Year1GL_NewHires,Year2GL_NewHires,Year3GL_NewHires,TotalDefered_NewHires)
  
  return(Output)
}
#
# ##################################################################################################################################################################
#
#Scenarios
Scenario_Returns <- as.data.frame(FYE)
Scenario_UAL <- as.data.frame(FYE)
Scenario_FR <- as.data.frame(FYE)
Scenario_ER_Percentage <- as.data.frame(FYE)
Scenario_ER_InflAdj <- as.data.frame(FYE)
Scenario_Total_ER <- as.data.frame(FYE)
Scenario_AllIn_ER <- as.data.frame(FYE)

#There are 3 types of scenarios here - Recessions, Supplemental and Lv$%
#We are trying to run all of them outside of a function because we need the data for UAL, FR, etc.
#If we run them in a function, we can only generate one output
ScenarioRuns <- 'Return Scenarios'
#Initialize Max Length, this will be used at the end
MaxLength <- 0
if(ScenarioRuns == 'Return Scenarios'){
  Scenarios <- c('Assumption','Model','Recession','Recurring Recession')
  for (i in 1:length(Scenarios)){
    NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'Statutory', Scenarios[i], c(0), 'Lv%', 'Reason'))
    Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
    Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
    Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
    Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
    Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
    Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
    Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
  }
  #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
  ScenarioNames <- Scenarios
  
} else if(ScenarioRuns == 'Supplemental Contribution'){
  Scenarios <- c(0,500,1000,1500,2000)
  for (i in 1:length(Scenarios)){
    NewData <- as.data.frame(RunModel('Deterministic', SimReturn, SimVolatility, 'ADC', 'Recurring Recession', Scenarios[i], 'Lv%', 'Reason'))
    Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
    Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
    Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
    Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
    Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
    Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
    Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
  }
  #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
  ScenarioNames <- c('No Supplemental Contribution', '$0.5 Billion', '$1 Billion', '$1.5 Billion', '$2 Billion')
  
} else if(ScenarioRuns == 'Lv$ vs %'){
  Scenarios <- c('Assumption','Lv%','Assumption','Lv$','Recurring Recession','Lv%','Recurring Recession','Lv$')
  MaxLength <- length(Scenarios)/2
  for (i in 1:MaxLength){
    NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'ADC', Scenarios[i*2 - 1], c(0), Scenarios[i*2], 'Reason'))
    Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
    Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
    Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
    Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
    Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
    Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
    Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
  }
  #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
  ScenarioNames <- c('Assumption Lv%', 'Assumption Lv$', 'Recurring Recession Lv%', 'Recurring Recession Lv$')
}

#MaxLength should in theory be the lenght of the scenarios but because of Lv$%, it may not be
#Hence we have to do the max function
if(ScenarioRuns != 'Lv$ vs %'){
  MaxLength <- length(Scenarios)
}

for(i in 1:MaxLength){
  #Start from StartIndex because thats when the projection is
  #Total ER is already inflation adjusted
  TotalERScenario <- sum(Scenario_Total_ER[nrow(Scenario_Total_ER),i+1])/1000
  #inflation adjusted UAL
  EndingUAL <- Scenario_UAL[nrow(Scenario_UAL),i+1]/1000
  AllInER <- Scenario_AllIn_ER[nrow(Scenario_AllIn_ER),i+1]/1000
  
  if(i == 1){
    ERCostTable <- c(TotalERScenario,EndingUAL, AllInER)
  } else {
    ERCostTable <- rbind(ERCostTable, c(TotalERScenario,EndingUAL, AllInER))
  }
}
colnames(ERCostTable) <- c('Total ER Contributions','Ending UAL','All in ER Cost')
rownames(ERCostTable) <- ScenarioNames

colnames(Scenario_Returns) <- c('FYE',ScenarioNames)
colnames(Scenario_UAL) <- c('FYE',ScenarioNames)
colnames(Scenario_FR) <- c('FYE',ScenarioNames)
colnames(Scenario_ER_Percentage) <- c('FYE',ScenarioNames)
colnames(Scenario_ER_InflAdj) <- c('FYE',ScenarioNames)

ScenarioPlot <- function(Data, YAxisLabel){
  ggplot(Data, aes(x = FYE)) +
    geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
    geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
    geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2) +
    geom_line(aes(y = Data[,5]), color = "#CC0000", size = 2) +
    labs(y = YAxisLabel, x = 'Year') + ggtitle(YAxisLabel)
  #scale_linetype_manual(labels = '')
}
ScenarioPlot(Scenario_UAL, 'Unfunded Liabilities (MVA)')
#
##################################################################################################################################################################
#
Simulations
start_time <- Sys.time()
#Set seed insures a consistency when simulations are run multiple times
set.seed((1234))
NumberofSimulations <- 100
#initialize the return simulations based on years and # of simulations
Returns_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
UAL_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
FR_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
ER_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)

#Run the simulations
for (i in 1:NumberofSimulations){
  NewData <- as.data.frame(RunModel('Stochastic', SimReturnAssumed, SimVolatility, 'ADC', '', c(0), 'Lv%', 'Reason'))
  Returns_Sims[,i+1] <- NewData$ROA_MVA
  UAL_Sims[,i+1] <- NewData$UAL_MVA_InflAdj
  FR_Sims[,i+1] <- NewData$FR_MVA
  ER_Sims[,i+1] <- NewData$ER_Percentage
}

Simulations_Returns <- cbind(FYE,FYE,FYE)
Simulations_UAL <- cbind(FYE,FYE,FYE)
Simulations_FR <- cbind(FYE,FYE,FYE)
Simulations_ER <- cbind(FYE,FYE,FYE)

#Get the 25th, 50th, 75th percentile
for(i in 1:length(FYE)){
  Simulations_Returns[i,] <- t(quantile(Returns_Sims[i,2:ncol(Returns_Sims)],c(0.25,0.5,0.75)))
  Simulations_UAL[i,] <- t(quantile(UAL_Sims[i,2:ncol(UAL_Sims)],c(0.25,0.5,0.75)))
  Simulations_FR[i,] <- t(as.data.frame(quantile(FR_Sims[i,2:ncol(FR_Sims)],c(0.25,0.5,0.75))))
  Simulations_ER[i,] <- t(quantile(ER_Sims[i,2:ncol(ER_Sims)],c(0.25,0.5,0.75)))
}

#plot the graphs
SimulationPlot <- function(Data, FYE){
  Data <- (as.data.frame(Data))
  Data <- cbind(FYE, Data)
  colnames(Data) <- c('FYE','25th Percentile', '50th Percentile', '75th Percentile')
  ggplot(Data, aes(x = Data[,1])) +
    geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
    geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
    geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2)
}
SimulationPlot(Simulations_ER, FYE)

#For supplemental contributions
Scenarios <- c(0,500,1000,1500,2000)
ScenarioNames <- c('No Supplemental Contribution', '$0.5 Billion', '$1 Billion', '$1.5 Billion', '$2 Billion')
#Run the simulations for each contribution
for(k in 1:length(Scenarios)){
  for (i in 1:NumberofSimulations){
    NewData <- as.data.frame(RunModel('Stochastic', SimReturnConservative, SimVolatility, 'Statutory', '', Scenarios[k], 'Lv%', 'Reason'))
    FR_Sims[,i+1] <- NewData$FR_MVA
  }

  #Take the simulation runs and get the probability
  #2050 Projections
  Probability80 <- sum(FR_Sims[32,] >= 0.8) / NumberofSimulations
  Probability100 <- sum(FR_Sims[32,] >= 1) / NumberofSimulations

  if(k == 1){
    ProbabilityData2050 <- c(Probability80,Probability100)
  } else {
    ProbabilityData2050 <- rbind(ProbabilityData2050,c(Probability80,Probability100))
  }

  #2075 Projections
  Probability80 <- sum(FR_Sims[nrow(FR_Sims),] >= 0.8) / NumberofSimulations
  Probability100 <- sum(FR_Sims[nrow(FR_Sims),] >= 1) / NumberofSimulations

  if(k == 1){
    ProbabilityData2075 <- c(Probability80,Probability100)
  } else {
    ProbabilityData2075 <- rbind(ProbabilityData2075,c(Probability80,Probability100))
  }
}

colnames(ProbabilityData2050) <- c('80% FR', '100% FR')
rownames(ProbabilityData2050) <- ScenarioNames
colnames(ProbabilityData2075) <- c('80% FR', '100% FR')
rownames(ProbabilityData2075) <- ScenarioNames

# #plot the graph
# barplot(ProbabilityData2075, main="Probability of achieving Funded Status after 30 years",
#         xlab='Supplemental Contributions',
#         legend = ScenarioNames, beside=TRUE)

end_time <- Sys.time()
print(end_time - start_time)