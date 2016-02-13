##Utility/workshorse functions
#Calculate tax owed in a marginal tax system (different rates apply to portions
#of income in different brackets) for a vector of incomes
tax <- function(brackets, income, deduction) {
  rowSums(do.call(mapply, c(FUN = function(bottom, cap, rate, extra, deduct){
    income <- income - deduction * deduct
    margin <- pmin(income, cap) - bottom
    ifelse(margin <= 0, 0, margin * rate + extra)
  }, brackets)))
}

#Apply tax function for multiple taxes to a vector of incomes
taxes <- function(incomes, bracketsList, deductions = 0) {
  names(incomes) <- as.character(incomes)
  ret <- sapply(bracketsList, tax, income = incomes, deduction = deductions)
  ret <- data.frame(ret, check.names = F)
  ret$income <- incomes
  ret$effectiveIncome <- incomes + 
    rowSums(ret[, na.omit(match(taxNamesEmp, names(ret)))], na.rm = T)
  ret$agi <- pmax(incomes - deductions, 0)
  ret
}

#Apply earned income tax credit and child tax credit to results of taxes
#function. CTC must be calculated after EITC using the modified income tax
applyCredits <- function(calculatedTaxes, filingStatus, nKids) {
  mutate(calculatedTaxes, 
         `Income Tax` = `Income Tax` - eitc(income, agi, filingStatus, nKids),
         `Income Tax` = `Income Tax` - 
           ctc(agi, income, `Income Tax`, filingStatus, nKids))
}

# Read census income file for filer type and compute percentiles
# income distribution data from US census, 2014 family incomes
# https://www.census.gov/hhes/www/cpstables/032015/faminc/toc.htm
# https://www.census.gov/hhes/www/cpstables/032015/perinc/pinc11_000.htm
getCensusIncomes <- function(filingStatus, sex) {
  if(filingStatus == "Married/Joint") {
    f <- "data/finc07.xls"
  } else if(sex == "M") {
    f <- "data/pinc11_1.xls"
  } else {
    f <- "data/pinc11_2.xls"

  }
  t <- read_excel(f, skip = 8)[c(-1, -46), 1:3] %>%
    mutate(prop = Number / sum(Number), centile = cumsum(prop),
           payer = "Population")
  names(t)[3] <- "income"
  tops <- gsub(",", "", t[[1]])
  tops <- as.numeric(regmatches(tops, regexpr("[[:digit:]]+$", tops)))
  t$income <- c(tops, t$income[nrow(t)])
  t
  getIncomeForPercentile <- approxfun(t$centile, t$income)
  getPercentileForIncome <- approxfun(t$income, t$centile)
  # glm modeling allows for smoother interpolation, but becomes inaccurate
  # at extremes. Linear interpolation used instead
  # mod <- glm(centile ~ income, acs, family = "binomial")
  # getPercentileForIncome <- function(x) {
  #   predict(mod, data.frame(income = x), type = "response")
  # }
  
  centileLabeler <- function(breaks) {
    #browser()
    incs <- scales::dollar(round(getIncomeForPercentile(breaks)))
    topBracket <- which(breaks == 1)
    if(length(topBracket)) {
      breaks[topBracket] <- 1 - t$centile[nrow(t) - 1]
      breaks[topBracket] <- round(breaks[topBracket],
                                digits = ifelse(breaks[topBracket] < .01, 3, 2))
    }
    #pmin used to avoid rounding up to "100th percentile"
    pct <- pmin(round(breaks * 100), 99)
    hundos <- which(pct == 99)
    lastDig <- substring(as.character(pct), 
                                    nchar(as.character(pct)), 
                                    nchar(as.character(pct)))
    getSuffix <- function(x) {
      substring(as.character(x), nchar(as.character(x)), 
                nchar(as.character(x))) %>%
        switch("1" = "st", "2" = "nd", "3" = "rd", "th")
    }
    pct <- paste0(pct, sapply(pct, getSuffix),  " Percentile")
    pct[breaks == .5] <- "Median"
    pct[topBracket] <- paste0("Top ", breaks[topBracket] * 100, "% Average")
    pct[hundos] <- paste0(">", pct[hundos])
    paste(incs, pct, sep = "\n")
  }
  list(acs = t, getIncomeForPercentile = getIncomeForPercentile,
       getPercentileForIncome = getPercentileForIncome, 
       centileLabeler = centileLabeler)
}

##Functions to implement tax laws

##Deduction and exemptions
#https://www.irs.com/articles/2015-federal-tax-rates-personal-exemptions-and-standard-deductions
#https://www.irs.gov/publications/p17/ch03.html
getDeduction <- function(incomes, 
                         filingStatus = c("Married/Joint", "Married/Separate",
                                          "Head of Household", "Single"),
                         nKids) {
  filingStatus <- match.arg(filingStatus)
  # https://www.irs.com/articles/2015-federal-tax-rates-personal-exemptions-and-standard-deductions
  standardDeduction <- switch(filingStatus,
                              "Married/Joint" = 12600,
                              "Married/Separate" = 6300,
                              "Head of Household" = 9250,
                              "Single" = 6300)
  # https://www.irs.gov/publications/p17/ch03.html
  exemptions <- (ifelse(filingStatus == "Married/Joint", 2, 1) + nKids) * 4000
  exPhaseOutStart <- switch(filingStatus,
                            "Married/Joint" = 309900,
                            "Married/Separate" = 154950,
                            "Head of Household" = 284050,
                            "Single" = 258250)
  exPhaseOut <- 
    pmin(ceiling(pmax(0, incomes - exPhaseOutStart) / 2500) * .02, 1)
  standardDeduction + exemptions * (1 - exPhaseOut)
  
}

##Earned income tax credit
#https://www.irs.gov/Credits-&-Deductions/Individuals/Earned-Income-Tax-Credit/EITC-Income-Limits-Maximum-Credit-Amounts
#https://www.law.cornell.edu/uscode/text/26/32
eitc <- function(inc, agi, 
                 filingStatus = c("Married/Joint", "Married/Separate",
                                  "Head of Household", "Single"), 
                 nKids) {
  filingStatus <- match.arg(filingStatus)
  kidSwitch <- pmin(nKids, 3) + 1
  max <- switch(kidSwitch, 503, 3359, 5548, 6242)
  if(filingStatus == "Married/Joint") {
    fullPhaseout <- switch(kidSwitch, 20330, 44651, 49974, 53267)
  } else {
    fullPhaseout <- switch(kidSwitch, 14820, 39131, 44454, 47747)
  }
  #fullPhaseout <- 44651
  pPhaseout <- .2106
  pPhasein <- .4
  #calulcate phaseout threshold based on where credit reduces to $0
  startPhaseout <- fullPhaseout - max / pPhaseout
  # credit = 40% of income up to max minus 21.05% percent of agi over phaseout
  # threshold down to a minimum credit of $0
  pmin(inc * pPhasein, max) - 
    pmin(pmax(agi - startPhaseout, 0) * pPhaseout, max)
}

##Child tax credit
#https://www.irs.gov/publications/p972/ar02.html
#https://www.irs.gov/pub/irs-pdf/f1040s8.pdf
ctc <- function(agi, income, incTax, 
                filingStatus = c("Married/Joint", "Married/Separate",
                                 "Head of Household", "Single"), 
                nkids) {
  filingStatus <- match.arg(filingStatus)
  incTax <- pmax(0, incTax)
  ctc <- nkids * 1000
  phaseOut <- switch(filingStatus,
                     "Married/Joint" = 110000,
                     "Married/Separate" = 55000,
                     "Head of Household" = 75000,
                     "Single" = 75000)
  #reduce max ctc by 5% of agi over phaseout threshold rounded up to nearest
  #thousand
  ctc <- pmax(ctc - pmax(ceiling((agi - phaseOut) / 1000) * 1000 * .05, 0), 0)
  #creditable ctc is smaller of remaning ctc (after taxes reduced to 0) or 15%
  #of earned income over $3K
  actc <- pmax(pmin(ctc - incTax, .15 * pmax(income - 3000, 0)), 0)
  #credit is full ctc up to the point that taxes are reduced to zero, plus
  #credtiable portion of the remainder
  pmin(ctc, incTax) + actc
}


taxesByIncomes <- function(incomes, filingStatus, nKids, sex, 
                           employer = c("ignore", "split", "isolate", "pool"),
                           useCorporateWelfare = F) {
  employer = match.arg(employer)
  totalDeduction <- getDeduction(incomes, filingStatus, nKids)
  
  brackets <- getBrackets(filingStatus, useCorporateWelfare, nKids)
  cb <- brackets$currentIndBracket
  bb <- brackets$bernieIndBrackets
  if(employer == "pool") {
    cb <- c(cb, brackets$currentEmpBrackets)
    bb <- c(bb, brackets$bernieEmpBrackets)
  }
  cur <- taxes(incomes, cb, totalDeduction) %>% 
    mutate(set = "Current", payer = "Individual") %>%
    applyCredits(filingStatus, nKids)
  bern <- taxes(incomes, bb, totalDeduction) %>% 
    mutate(set = "Bernie", payer = "Individual") %>%
    applyCredits(filingStatus, nKids)
  
  dat <- gather(rbind(cur, bern), expense, amount, 
                -income, -effectiveIncome, -set, -payer, -agi)
  if(employer %in% c("split", "isolate")) {
    curEmp <- taxes(incomes, brackets$currentEmpBrackets) %>%
      mutate(set = "Current", payer = "Employer")
    bernEmp <- taxes(incomes, brackets$bernieEmpBrackets) %>%
      mutate(set = "Bernie", payer = "Employer")
    emps <- gather(rbind(curEmp, bernEmp), expense, amount, -income, 
                   -effectiveIncome, -set, -payer, -agi)
  }
  switch(employer, 
         "split" = rbind(dat, emps),
         "isolate" = emps,
         dat)
}

netTaxDifferences <- function(dat, acsList) {
  #total effective tax rates
  datSum <- dat %>% group_by(payer, set, income, effectiveIncome, agi) %>% 
    summarize(tTax = sum(amount)) %>% ungroup %>% 
    mutate(eTax = tTax / effectiveIncome,
           percentile = acsList$getPercentileForIncome(income))

  #differences between plans / savings data
  inner_join(filter(datSum, set == "Bernie"), 
             filter(datSum, set == "Current"), 
             by = c("payer", "income", "percentile")) %>%
    rename(eTaxBern = eTax.x, eTaxCur = eTax.y,
           tTaxBern = tTax.x, tTaxCur = tTax.y) %>%
    mutate(increase = eTaxBern > eTaxCur, delta = tTaxCur - tTaxBern,
           lab = paste0(ifelse(increase, "Increase: ", "Savings: "), 
                        scales::dollar(abs(round(delta)))))
  
}