taxNamesInd <- c("Income Tax", "Social Security Tax", "Medicare Tax",
                 "Medicare-for-all Tax", "Family Leave Tax", 
                 "Healthcare Premiums", "Healthcare Expenses")
taxNamesEmp <- c("Employer Healthcare\nContribution",
                 "Employer Payroll Tax", "Corporate Welfare", 
                 "Employer Medicare-for-all Tax")

getBrackets <- function(filingStatus = c("Married/Joint", "Married/Separate",
                                         "Head of Household", "Single"),
                        useCorporateWelfare = F, nKids = 0) {
  filingStatus <- match.arg(filingStatus)
  nilBracket <- data.frame(bottom = 0, cap = Inf, rate = 0, 
                           extra = 0, deduct = 0)
  
  #federal poverty level
  #http://familiesusa.org/product/federal-poverty-guidelines
  houseSize <- ifelse(filingStatus == "Married/Joint", 2, 1) + nKids
  fpl <- switch(pmin(houseSize, 8),
                11770, 15930, 20090, 24250, 28410, 32570, 36730, 40890)
  
  #http://www.bankrate.com/finance/taxes/tax-brackets.aspx
  #https://www.irs.com/articles/2015-federal-tax-rates-personal-exemptions-and-standard-deductions
  #Married/jointly 2015 rate
  incomeBracketsCurrent <- switch(
    filingStatus,
    "Married/Joint" = data.frame(
      bottom = c(0, 18450, 74900, 151200, 230450, 411500, 464850),
      cap = c(18450, 74900, 151200, 230450, 411500, 464850, Inf),
      rate = c(.1, .15, .25, .28, .33, .35, .396),
      extra = 0, deduct = 1),
    "Single" = data.frame(
      bottom = c(0, 9225, 37450, 90750, 189300, 411500, 413200),
      cap = c(9225, 37450, 90750, 189300, 411500, 413200, Inf),
      rate = c(.1, .15, .25, .28, .33, .35, .396),
      extra = 0, deduct = 1),
    "Married/Separate" = data.frame(
      bottom = c(0, 9225, 37450, 75600, 115225, 205750, 232425),
      cap = c(9225, 37450, 75600, 115225, 205750, 232425, Inf),
      rate = c(.1, .15, .25, .28, .33, .35, .396),
      extra = 0, deduct = 1),
    "Head of Household" = data.frame(
      bottom = c(0, 13150, 50200, 129600, 209850, 411500, 439000),
      cap = c(13150, 50200, 129600, 209850, 411500, 439000, Inf),
      rate = c(.1, .15, .25, .28, .33, .35, .396),
      extra = 0, deduct = 1)
  )
  
  # https://berniesanders.com/issues/medicare-for-all 
  # https://berniesanders.com/wp-content/uploads/2016/01/friedman-memo-1.pdf 
  incomeBracketsBernieNew <- data.frame(
    bottom = c(250000, 499999, 2000000, 10000000),
    cap = c(499999, 2000000, 10000000, Inf),
    rate  = c(.37, .43, .48, .52),
    extra = 0, deduct = 1
  )
  # Bernie's plan only changes brackets for incomes over $250K, so lower 
  # brackets are held at current rate. For married/separate filers, the highest
  # bracket is under that 250K threshold (but at a higher rate than Bernie's
  # $250K bracket, so it is replaced with Bernies)
  oldBracketCutoff <- which(incomeBracketsCurrent$bottom > 250000)[1] - 1
  if(is.na(oldBracketCutoff)) {
    oldBracketCutoff <- nrow(incomeBracketsCurrent) - 1
    incomeBracketsBernieNew$bottom[1] <- 
      incomeBracketsCurrent$bottom[nrow(incomeBracketsCurrent)]
  }
  incomeBracketsBernie <- 
    rbind(incomeBracketsCurrent[seq_len(oldBracketCutoff), ],
          incomeBracketsBernieNew)
  incomeBracketsBernie$cap[oldBracketCutoff] <- 
    incomeBracketsBernieNew$bottom[1]
  
  #https://www.irs.gov/publications/p15/ar02.html#en_US_2016_publink1000202402
  # Tax rates and the social security wage base limit.   Social security and
  # Medicare taxes have different rates and only the social security tax has a
  # wage base limit. The wage base limit is the maximum wage subject to the tax
  # for the year. Determine the amount of withholding for social security and
  # Medicare taxes by multiplying each payment by the employee tax rate. There are
  # no withholding allowances for social security and Medicare taxes.
  # 
  # For 2016, the social security tax rate is 6.2% (amount withheld) each for the
  # employer and employee (12.4% total). The social security wage base limit is
  # $118,500. The tax rate for Medicare is 1.45% (amount withheld) each for the
  # employee and employer (2.9% total). There is no wage base limit for Medicare
  # tax; all covered wages are subject to Medicare tax.
  # 
  # Additional Medicare Tax withholding.   In addition to withholding Medicare tax
  # at 1.45%, you must withhold a 0.9% Additional Medicare Tax from wages you pay
  # to an employee in excess of $200,000 in a calendar year. You are required to
  # begin withholding Additional Medicare Tax in the pay period in which you pay
  # wages in excess of $200,000 to an employee and continue to withhold it each
  # pay period until the end of the calendar year. Additional Medicare Tax is only
  # imposed on the employee. There is no employer share of Additional Medicare
  # Tax. All wages that are subject to Medicare tax are subject to Additional
  # Medicare Tax withholding if paid in excess of the $200,000 withholding
  # threshold.
  ssBracketsCurrent <- data.frame(
    bottom = c(0, 118500),
    cap = c(118500, Inf),
    rate = c(.062, 0),
    extra = 0, deduct = 0
  )
  
  medicareBracketsCurrent <- data.frame(
    bottom = c(0, 200000),
    cap = c(200000, Inf),
    rate = c(.0145, .0145 + .009),
    extra = 0, deduct = 0
  )
  
  #https://www.ssa.gov/oact/solvency/BSanders_20150323.pdf
  #earnings in the 118500 - 250000 range are not
  #taxed unless the total earnings are > 250000 
  #($8,153 in taxes for the 118500 - 250000 range)
  #extra field compensates
  # Correction: I was wrong about the gap income being taxed for incomes
  # over 250000, changed extra back to 0
  ssBracketsBernie <- data.frame(
    bottom = c(0, 118500, 250000),  
    cap = c(118500, 250000, Inf),  
    rate = c(.062, 0, .062),
    extra = 0,
    deduct = 0
  )
  
  
  mfaBracketsCurrent <- nilBracket
  
  #https://berniesanders.com/issues/medicare-for-all-2/
  #https://berniesanders.com/wp-content/uploads/2016/01/friedman-memo-1.pdf
  # "The	2.2%	income	tax	applies	to	taxable	income as currently defined"
  mfaBracketsBernie <- data.frame(
    bottom = 0, cap = Inf, rate = .022, extra = 0, deduct = 1
  )
  
  familyLeaveBracketsCurrent <- nilBracket
  
  #http://www.gillibrand.senate.gov/issues/paid-family-medical-leave
  familyLeaveBracketsBernie <- data.frame(
    bottom = c(0, 113700),
    cap = c(113700, Inf),
    rate = c(.002, 0),
    extra = 0, 
    deduct = 0
  )
  

  # Anyone earning above the medicaid threshold should eligible for
  # employer sponsored coverage under Obamacare, so Obamacare exchange plans
  # and tax credits are not included.
  # Using Milliman index for families because estimate of actual out-of-pocket 
  # available. KFF's estimate for family premiums is ~$1,000 less
  # http://www.milliman.com/uploadedFiles/insight/Periodicals/mmi/2015-MMI.pdf 
  # Using KFF for singles because milliman doesn't have info on singles 
  # http://kff.org/health-costs/report/2015-employer-health-benefits-survey/
  healthPremBracketsCurrent <- switch(
    as.character(houseSize > 1), 
    "TRUE" = data.frame(
      bottom = c(0, fpl * 1.33),
      cap = c(fpl * 1.33, Inf),
      rate = c(0, 0),
      extra = c(0, 6408), 
      deduct = 0),
    data.frame(
      bottom = c(0, fpl * 1.33),
      cap = c(fpl * 1.33, Inf),
      rate = c(0, 0),
      extra = c(0, 1071), 
      deduct = 0)
  )
  
  
  #out of pocket expenses set at 2.4% for medicaid recipients, or at the
  #national average of $4,065 for all others 
  #(assumes state with obamacare medicaid expansion)
  #http://www.cbpp.org/research/out-of-pocket-medical-expenses-for-medicaid-beneficiaries-are-substantial-and-growing
  #http://www.milliman.com/uploadedFiles/insight/Periodicals/mmi/2015-MMI.pdf 
  #page 7
  #http://obamacarefacts.com/obamacares-medicaid-expansion/
  # No good out-of-pocket estimate for singles, using avg deductible from KFF
  # http://kff.org/health-costs/report/2015-employer-health-benefits-survey/
  healthPocketBracketsCurrent <- switch(
    as.character(houseSize > 1), 
    "TRUE" = data.frame(
    bottom = c(0, fpl * 1.33),
    cap = c(fpl * 1.33, Inf),
    rate = c(.024, 0),
    extra = c(0, max(4065 - .024 * fpl * 1.33, 0)), 
    deduct = 0),
    data.frame(
      bottom = c(0, fpl * 1.33),
      cap = c(fpl * 1.33, Inf),
      rate = c(.024, 0),
      extra = c(0, max(1318 - .024 * fpl * 1.33, 0)), 
      deduct = 0)
  )
  
  #https://berniesanders.com/issues/medicare-for-all-2/
  #https://berniesanders.com/wp-content/uploads/2016/01/friedman-memo-1.pdf 
  # "It is assumed that 20% of out-of-pocket spending is for activities that
  # would not be covered because they are deemed not medically necessary"
  # Therefore, we keep 20% of the current plan OOP estimate
  healthPremBracketsBernie <- nilBracket
  
  #https://berniesanders.com/issues/medicare-for-all-2/
  healthPocketBracketsBernie <- healthPocketBracketsCurrent
  healthPocketBracketsBernie$rate <- healthPocketBracketsBernie$rate * 0.2
  healthPocketBracketsBernie$extra <- healthPocketBracketsBernie$extra * 0.2
  
  # Using Milliman index for families because estimate of actual out-of-pocket 
  # available. Differences between KFF and milliman for employer contribution
  # are minimal 
  # http://www.milliman.com/uploadedFiles/insight/Periodicals/mmi/2015-MMI.pdf 
  # Using KFF for singles because milliman doesn't have info on singles 
  # http://kff.org/health-costs/report/2015-employer-health-benefits-survey/
  healthEmpBracketsCurrent <- switch(
    as.character(houseSize > 1),
    "TRUE" = data.frame(
      bottom = c(0, fpl * 1.33),
      cap = c(fpl * 1.33, Inf),
      rate = 0,
      extra = c(0, 14198), 
      deduct = 0),
    data.frame(
      bottom = c(0, fpl * 1.33),
      cap = c(fpl * 1.33, Inf),
      rate = 0,
      extra = c(0, 5179), 
      deduct = 0)
  )
  
  # https://berniesanders.com/issues/medicare-for-all-2/
  healthEmpBracketsBernie <- nilBracket
  
  # https://www.irs.gov/publications/p15/ar02.html#en_US_2016_publink1000202402
  # https://www.irs.gov/pub/irs-pdf/f940.pdf
  # For unemployment tax, including the full 6% as discounts apply only when
  # equivalent amounts are paid to state unemployment program
  payrollTaxBracketsCurrent <- data.frame(
    bottom = c(0, 7000, 118500), 
    cap = c(7000, 118500, Inf), 
    #unemployment tax ends at $7,000, soc sec tax ends at $118,500
    rate = c(.062 + .0145 + 0.06, .062 + .0145, .0145), 
    extra = 0, deduct = 0
  )
  
  #https://www.ssa.gov/oact/solvency/BSanders_20150323.pdf
  # only difference is the Soc Sec cap removal - follows the same pattern as 
  # invidiual soc sec tax
  payrollTaxBracketsBernie <- data.frame(
    bottom = c(0, 7000, 118500, 250000), 
    cap = c(7000, 118500, 250000, Inf), 
    #unemployment tax ends at $7,000, 
    #soc sec tax ends at $118,500 and resumes at $250,000 (with backlog)
    #medicare for all tax added to all brackets
    rate = c(.062 + .0145 + 0.06, .062 + .0145, .0145, .0145 + .062), 
    extra = c(0, 0, 0, 8153), 
    deduct = 0
  )
  
  payrollMFABracketsCurrent <- nilBracket
  
  # https://berniesanders.com/issues/medicare-for-all-2/
  payrollMFABracketsBernie <- data.frame(
    bottom = 0, cap = Inf, rate = .062, extra = 0, deduct = 0
  )
  
  # not currently used - represents the what employers save by paying so little
  # that their employees qualify for medicare
  corpWelfareGapCurrent <- data.frame(
    bottom = c(0, fpl * 1.33),
    cap = c(fpl * 1.33, Inf),
    rate = 0,
    extra = c(14198, -14198),
    deduct = 0
  )
  
  corpWelfareGapBernie <- nilBracket
  
  currentIndBrackets <- list(incomeBracketsCurrent,
                             ssBracketsCurrent,
                             medicareBracketsCurrent,
                             mfaBracketsCurrent,
                             familyLeaveBracketsCurrent,
                             healthPremBracketsCurrent,
                             healthPocketBracketsCurrent)
  currentEmpBrackets <- list(healthEmpBracketsCurrent,
                             payrollTaxBracketsCurrent,
                             corpWelfareGapCurrent,
                             payrollMFABracketsCurrent)
  
  bernieIndBrackets <- list(incomeBracketsBernie,
                            ssBracketsBernie,
                            medicareBracketsCurrent,
                            mfaBracketsBernie,
                            familyLeaveBracketsBernie,
                            healthPremBracketsBernie,
                            healthPocketBracketsBernie)
  bernieEmpBrackets <- list(healthEmpBracketsBernie,
                            payrollTaxBracketsBernie,
                            corpWelfareGapBernie,
                            payrollMFABracketsBernie)
  
  names(currentIndBrackets) <- taxNamesInd
  names(bernieIndBrackets) <- taxNamesInd
  names(currentEmpBrackets) <- taxNamesEmp
  names(bernieEmpBrackets) <- taxNamesEmp
  if(!useCorporateWelfare) {
    currentEmpBrackets$`Corporate Welfare` <- NULL
    bernieEmpBrackets$`Corporate Welfare` <- NULL
  }
  list(currentIndBrackets = currentIndBrackets, 
       bernieIndBrackets = bernieIndBrackets,
       currentEmpBrackets = currentEmpBrackets,
       bernieEmpBrackets = bernieEmpBrackets)
}