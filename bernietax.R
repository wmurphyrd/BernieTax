library(dplyr); library(tidyr); library(acs); library(ggplot2); library(readxl)
library(gridExtra)

useCorporateWelfare <- F

nilBracket <- data.frame(
  bottom = 0, cap = Inf, rate = 0, extra = 0, deduct = 0
)


# https://berniesanders.com/issues/medicare-for-all
# https://berniesanders.com/wp-content/uploads/2016/01/friedman-memo-1.pdf
incomeBracketsBernie <- data.frame(
  bottom = c(0, 18450, 74900, 151200, 230450, 250000, 
             499999, 2000000, 10000000),
  cap = c(18450, 74900, 151200, 230450, 250000, 
          499999, 2000000, 10000000, Inf),
  rate  = c(.1, .15, .25, .28, .33, .37, .43, .48, .52),
  extra = 0, deduct = 1
)

#http://www.bankrate.com/finance/taxes/tax-brackets.aspx
#Married/jointly 2015 rate
incomeBracketsCurrent <- data.frame(
  bottom = c(0, 18450, 74900, 151200, 230450, 411500, 464850),
  cap = c(18450, 74900, 151200, 230450, 411500, 464850, Inf),
  rate = c(.1, .15, .25, .28, .33, .35, .396),
  extra = 0, deduct = 1
)

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
ssBracketsBernie <- data.frame(
  bottom = c(0, 118500, 250000),  
  cap = c(118500, 250000, Inf),  
  rate = c(.062, 0, .062),
  extra = c(0, 0, 8153),
  deduct = 0
)


mfaBracketsCurrent <- nilBracket

#https://berniesanders.com/issues/medicare-for-all-2/
mfaBracketsBernie <- data.frame(
  bottom = 0, cap = Inf, rate = .022, extra = 0, deduct = 0
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

#federal poverty level for family of 4
#http://familiesusa.org/product/federal-poverty-guidelines
fpl4 <- 24250

# 35 hours per week at minmum wage is 54% of FPL (below medicaid threshold)
# Therefore anyone earning above the medicaid threshold will should eligible for
# employer sponsored coverage under Obamacare, so Obamacare exchange plans
# and tax credits are not included
healthPremBracketsCurrent <- data.frame(
  bottom = c(0, fpl4 * 1.33),
  cap = c(fpl4 * 1.33, Inf),
  rate = c(0, 0),
  extra = c(0, 6408), 
  deduct = 0
)


#out of pocket expenses set at 2.4% for medicaid recipients, or at the
#national average of $4,065 for all others 
#(assumes state with obamacare medicaid expansion)
#http://www.cbpp.org/research/out-of-pocket-medical-expenses-for-medicaid-beneficiaries-are-substantial-and-growing
#http://www.milliman.com/uploadedFiles/insight/Periodicals/mmi/2015-MMI.pdf 
#page 7
#http://obamacarefacts.com/obamacares-medicaid-expansion/
healthPocketBracketsCurrent <- data.frame(
  bottom = c(0, fpl4 * 1.33),
  cap = c(fpl4 * 1.33, Inf),
  rate = c(.024, 0),
  extra = c(0, 4065 - .024 * fpl4 * 1.33), 
  deduct = 0
)

healthPremBracketsBernie <- nilBracket

healthPocketBracketsBernie <- nilBracket

healthEmpBracketsCurrent <- data.frame(
  bottom = c(0, fpl4*1.33),
  cap = c(fpl4*1.33, Inf),
  rate = 0,
  extra = c(0, 14198), 
  deduct = 0
)

healthEmpBracketsBernie <- nilBracket

payrollTaxBracketsCurrent <- data.frame(
  bottom = 0, cap = Inf, rate = .0765, extra = 0, deduct = 0
)

payrollTaxBracketsBernie <- data.frame(
  bottom = 0, cap = Inf, rate = .1385, extra = 0, deduct = 0
)

corpWelfareGapCurrent <- data.frame(
 bottom = c(0, fpl4 * 1.33),
 cap = c(fpl4 * 1.33, Inf),
 rate = 0,
 extra = c(14198, -14198),
 deduct = 0
)

corpWelfareGapBernie <- nilBracket

taxNamesInd <- c("Income Tax", "Social Security Tax", "Medicare Tax",
                 "Medicare-for-all Tax", "Family Leave Tax", 
                 "Healthcare Premiums", "Healthcare Expenses")
taxNamesEmp <- c("Employer Healthcare\nContribution",
                 "Employer Payroll Tax", "Corporate Welfare")

currentIndBrackets <- list(incomeBracketsCurrent,
                           ssBracketsCurrent,
                           medicareBracketsCurrent,
                           mfaBracketsCurrent,
                           familyLeaveBracketsCurrent,
                           healthPremBracketsCurrent,
                           healthPocketBracketsCurrent)
currentEmpBrackets <- list(healthEmpBracketsCurrent,
                           payrollTaxBracketsCurrent,
                           corpWelfareGapCurrent)

bernieIndBrackets <- list(incomeBracketsBernie,
                          ssBracketsBernie,
                          medicareBracketsCurrent,
                          mfaBracketsBernie,
                          familyLeaveBracketsBernie,
                          healthPremBracketsBernie,
                          healthPocketBracketsBernie)
bernieEmpBrackets <- list(healthEmpBracketsBernie,
                          payrollTaxBracketsBernie,
                          corpWelfareGapBernie)

names(currentIndBrackets) <- taxNamesInd
names(bernieIndBrackets) <- taxNamesInd
names(currentEmpBrackets) <- taxNamesEmp
names(bernieEmpBrackets) <- taxNamesEmp
if(!useCorporateWelfare) {
  currentEmpBrackets$`Corporate Welfare` <- NULL
  bernieEmpBrackets$`Corporate Welfare` <- NULL
}

tax <- function(brackets, income, deduction) {
  rowSums(do.call(mapply, c(FUN = function(bottom, cap, rate, extra, deduct){
    income <- income - deduction * deduct
    margin <- pmin(income, cap) - bottom
    ifelse(margin <= 0, 0, margin * rate + extra)
  }, brackets)))
}


taxes <- function(incomes, bracketsList, deductions = 0) {
  names(incomes) <- as.character(incomes)
#   ret <- mapply(function(inc, ded) {
#     sapply(bracketsList, tax, income = inc, deduction = ded)
#   }, inc = incomes, ded = deductions)
  ret <- sapply(bracketsList, tax, income = incomes, deduction = deductions)
  ret <- data.frame(ret, check.names = F)
  #ret <- data.frame(t(ret), check.names = F)
  
  ret$income <- incomes
  ret$effectiveIncome <- incomes + 
    rowSums(ret[, na.omit(match(taxNamesEmp, names(ret)))], na.rm = T)
  ret$agi <- pmax(incomes - deductions, 0)
  ret
}


incomes <- seq(8000, 402000, by = 2000)

##Deductions and credits

#https://www.irs.com/articles/2015-federal-tax-rates-personal-exemptions-and-standard-deductions
standardDeduction <- 12600
#https://www.irs.gov/publications/p17/ch03.html
exemptions <- 4 * 4000
exPhaseOut <- pmin(ceiling(pmax(0, incomes - 309900) / 2500) * .02, 1)
exemptions <- exemptions - exemptions * exPhaseOut
totalDeduction <- standardDeduction + exemptions
#incomes <- seq(20000, 5000000, by = 10000)

##Earned income tax credit
#https://www.irs.gov/Credits-&-Deductions/Individuals/Earned-Income-Tax-Credit/EITC-Income-Limits-Maximum-Credit-Amounts
#https://www.law.cornell.edu/uscode/text/26/32
eitc <- function(inc, agi) {
  max <- 5548
  fullPhaseout <- 44651
  pPhaseout <- .2106
  pPhasein <- .4
  #calulcate phaseout threshold based on where credit reduces to $0
  startPhaseout <- fullPhaseout - max / pPhaseout
  # credit = 40% of income up to max of 5548 minus 21.05% percent of agi
  # over phaseout threshold down to a minimum credit of 0$ at agi of $44,651
  pmin(inc * pPhasein, max) - 
    pmin(pmax(agi - startPhaseout, 0) * pPhaseout, max)
}

##Child tax credit
#https://www.irs.gov/publications/p972/ar02.html
ctc <- function(agi, income, incTax, nkids = 2) {
  incTax <- pmax(0, incTax)
  ctc <- nkids * 1000
  #reduce max ctc by 5% of income over $110,000 rounded up to nearest thousand
  ctc <- pmax(ctc - pmax(ceiling((agi - 110000) / 1000) * 1000 * .05, 0), 0)
  #creditable ctc is smaller of remaning ctc (after taxes reduced to 0) or 15%
  #of earned income over $3K
  actc <- pmax(pmin(ctc - incTax, .15 * pmax(income - 3000, 0)), 0)
  #credit is full ctc up to the point that taxes are reduced to zero, plus
  #credtiable portion of the remainder
  pmin(ctc, incTax) + actc
}

cur <- taxes(incomes, currentIndBrackets, totalDeduction)
cur <- cur %>% mutate(set = "Current", payer = "Individual",
              `Income Tax` = `Income Tax` - eitc(income, agi),
              `Income Tax` = `Income Tax` - ctc(agi, income, `Income Tax`))
bern <- taxes(incomes, bernieIndBrackets, totalDeduction)
bern <- bern %>% mutate(set = "Bernie", payer = "Individual",
                      `Income Tax` = `Income Tax` - eitc(income, agi),
                      `Income Tax` = `Income Tax` - 
                        ctc(agi, income, `Income Tax`))
curEmp <- taxes(incomes, currentEmpBrackets)
curEmp$set <- "Current"
curEmp$payer <- "Employer"
bernEmp <- taxes(incomes, bernieEmpBrackets)
bernEmp$set <- "Bernie"
bernEmp$payer <- "Employer"

dat <- rbind(gather(rbind(cur, bern),
              expense, amount, -income, -effectiveIncome, -set, -payer, -agi),
             gather(rbind(curEmp, bernEmp),
                    expense, amount, -income, -effectiveIncome, -set, -payer, -agi))

#total effective tax rates
datSum <- dat %>% group_by(payer, set, income, effectiveIncome, agi) %>% 
  summarize(tTax = sum(amount)) %>% ungroup %>% 
  mutate(eTax = tTax / effectiveIncome,
         payer = factor(payer, levels = c("Individual", "Employer")))


##income distribution data from US census, 2014 family incomes
#https://www.census.gov/hhes/www/cpstables/032015/faminc/toc.htm
acs <- read_excel("finc07.xls", skip = 8)[c(-1, -46), 1:3]
acs <- acs %>% rename(income = `Mean \n Income (dollars)`) %>% 
  mutate(prop = Number / sum(Number), centile = cumsum(prop),
         payer = "Population")
getIncomeForPercentile <- approxfun(acs$centile, acs$income)
getPercentileForIncome <- approxfun(acs$income, acs$centile)
# glm modeling allows for smoother interpolation, but becomes inaccurate
# at extremes. Linear interpolation used instead
# mod <- glm(centile ~ income, acs, family = "binomial")
# getPercentileForIncome <- function(x) {
#   predict(mod, data.frame(income = x), type = "response")
# }
percentiles <- read_excel("T11-0089.xls", skip = )
percentiles <- data.frame(p = c(seq(.25, .75, by = .25), .95))
percentiles <- mutate(percentiles, income = getIncomeForPercentile(p),
                      labs = paste0(as.character(p * 100), "th Percentile"),
                      xlabs = paste(scales::dollar(income), labs, sep = "\n"))
percentiles$xlabs[percentiles$p == .5] <- 
  sub("50th Percentile", "Median", percentiles$xlabs[percentiles$p == .5])

centileLabeler <- function(breaks) {
  pct <- paste0(round(breaks*100), "th Percentile")
  pct[breaks == .5] <- "Median"
  incs <- scales::dollar(getIncomeForPercentile(breaks))
  paste(incs, pct, sep = "\n")
}

datSum <- mutate(datSum, percentile = getPercentileForIncome(income))

#differences between plans / ribbon data
datDiff <- inner_join(filter(datSum, set == "Bernie"), 
                      filter(datSum, set == "Current"), 
                      by = c("payer", "income", "percentile")) %>%
  rename(eTaxBern = eTax.x, eTaxCur = eTax.y) %>%
  mutate(increase = eTaxBern > eTaxCur,
         iTop = ifelse(increase, eTaxBern, NA),
         iBottom = ifelse(increase, eTaxCur, NA),
         dTop = ifelse(!increase, eTaxCur, NA),
         dBottom = ifelse(!increase, eTaxBern, NA))

png("bernieTax.png", 1024, 768)
source("berniePlot.R", print.eval = T)
dev.off()

savings <- datDiff %>% filter(payer == "Individual") %>%
  mutate(delta = (eTaxCur - eTaxBern) * income)
print("Max savings:")
savings[which.max(savings$delta), c("income", "delta")]
print("Median family savings:")
savings[which.min(abs(savings$percentile - .5)), c("income", "delta")]
