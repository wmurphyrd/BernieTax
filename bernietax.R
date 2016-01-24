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
#http://obamacarefacts.com/insurance-exchange/premium-tax-credits/
#These brackets erroneously apply Obamacre premium credits to employer plans
# healthPremBracketsCurrent <- data.frame(
#   bottom = c(0, fpl4 * 1.33, fpl4 * 1.5, fpl4 * 2, fpl4 * 2.5, fpl4 * 4),
#   cap = c(fpl4 * 1.33, fpl4 * 1.5, fpl4 * 2, fpl4 * 2.5, fpl4 * 4, Inf),
#   #note, 250-400% of FPL has obamacare rate max of 9.66%, but this exceeds
#   #national average, so the flat average is used instead
#   rate = c(0, .047, .0641, .0818, 0, 0),
#   extra = c(0, 0, 0, 0, 6408, 6408)
# )

# 35 hours per week at minmum wage is 54% of FPL (below medicaid threshold)
# Therefore anyone earning above the medicaid threshold will should eligible for
# employer sponsored coverage under Obamacare
healthPremBracketsCurrent <- data.frame(
  bottom = c(0, fpl4 * 1.33),
  cap = c(fpl4 * 1.33, Inf),
  #note, 250-400% of FPL has obamacare rate max of 9.66%, but this exceeds
  #national average, so the flat average is used instead
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

tax <- function(brackets, deduction, x) {
  sum(mapply(function(income, bottom, cap, rate, extra){
    if(income > cap) income <- cap
    margin <- income - bottom
    if(margin <= 0) return(0)
    margin * rate + extra
  }, bottom = brackets$bottom,
  cap = brackets$cap, rate = brackets$rate, extra = brackets$extra,
  MoreArgs = list(income = x))) 
}

taxes <- function(incomes, brackets, deductions = 0) {
  names(incomes) <- as.character(incomes)
  ret <- data.frame(t(sapply(incomes - deductions, function(x) {
    sapply(brackets, tax, x)
  })),
  check.names = F)
  ret$income <- incomes
  ret$effectiveIncome <- incomes + 
    rowSums(ret[, na.omit(match(taxNamesEmp, names(ret)))], na.rm = T)
  ret
}


incomes <- seq(20000, 450000, by = 2000)
#https://www.irs.com/articles/2015-federal-tax-rates-personal-exemptions-and-standard-deductions
standardDeduction <- 12600
#https://www.irs.gov/publications/p17/ch03.html
exemptions <- 4 * 4000
exPhaseOut <- pmin(ceiling(pmax(0, incomes - 309900) / 2500) * .02, 1)
exemptions <- exemptions - exemptions * exPhaseOut
totalDeduction <- standardDeduction + exemptions
#incomes <- seq(20000, 5000000, by = 10000)

cur <- taxes(incomes, currentIndBrackets, totalDeduction)
cur$set <- "Current"
cur$payer <- "Individual"
bern <- taxes(incomes, bernieIndBrackets, totalDeduction)
bern$set <- "Bernie"
bern$payer <- "Individual"
curEmp <- taxes(incomes, currentEmpBrackets)
curEmp$set <- "Current"
curEmp$payer <- "Employer"
bernEmp <- taxes(incomes, bernieEmpBrackets)
bernEmp$set <- "Bernie"
bernEmp$payer <- "Employer"

dat <- rbind(gather(rbind(cur, bern),
              expense, amount, -income, -effectiveIncome, -set, -payer),
             gather(rbind(curEmp, bernEmp),
                    expense, amount, -income, -effectiveIncome, -set, -payer))
# dat <- rbind(gather(cur, expense, amount, -income, -effectiveIncome, -set),
#       gather(bern, expense, amount,  -income, -effectiveIncome, -set))
dat <- mutate(dat, effectiveIncome = 
                effectiveIncome + standardDeduction + exemptions,
              income = income + standardDeduction + exemptions,
              rate = amount / effectiveIncome,
              incomeT = income / 1000)

# minimumWage35HoursBernie <- 15 * 35 * 52
# 
# dat <- filter(dat, 
#               !(income < minimumWage35HoursBernie & set == "Bernie"))



#incomelabs <- c(10, round(minimumWage35HoursBernie/1000), 54, 150, 400)

# #png("bernietax.png", width = 960, height = 960)
# ggplot(dat, aes(x = incomeT, y = rate, fill = expense)) + 
#   geom_area(position = "stack") + facet_grid(set~.) +
#   scale_x_log10(labels = scales::dollar, 
#                 breaks = incomelabs) +
#   theme(text = element_text(size = 24)) +
#   guides(fill = guide_legend("")) +
#   labs(x = "Taxable Income (thousands)", 
#        y = "Effective Tax Rate") +
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, .6, by = .1))
# #dev.off()

datSum <- dat %>% group_by(payer, set, income) %>% 
  summarize(eTax = sum(rate)) %>% ungroup %>%
  mutate(payer = factor(payer, levels = c("Individual", "Employer")))

datDiff <- inner_join(filter(datSum, set == "Bernie"), 
                      filter(datSum, set == "Current"), 
                      by = c("payer", "income")) %>%
  rename(eTaxBern = eTax.x, eTaxCur = eTax.y) %>%
#   mutate(increase = eTaxBern - eTaxCur,
#          bottom = ifelse(increase <= 0, eTaxBern, eTaxCur),
#          dir = ifelse(increase > 0, "Increase", "Decrease")) %>%
  # select(income, payer, top, bottom, dir)
  mutate(increase = eTaxBern > eTaxCur,
         iTop = ifelse(increase, eTaxBern, NA),
         iBottom = ifelse(increase, eTaxCur, NA),
         dTop = ifelse(!increase, eTaxCur, NA),
         dBottom = ifelse(!increase, eTaxBern, NA))

##income distribution data from US census, 2014 family incomes
#https://www.census.gov/hhes/www/cpstables/032015/faminc/toc.htm
acs <- read_excel("finc07.xls", skip = 8)[c(-1, -46), 1:3]
acs <- acs %>% rename(income = `Mean \n Income (dollars)`) %>% 
  mutate(prop = Number / sum(Number), centile = cumsum(prop),
         payer = "Population")
getIncomeForPercentile <- approxfun(acs$centile, acs$income)
percentiles <- read_excel("T11-0089.xls", skip = )
percentiles <- data.frame(p = c(seq(.25, .75, by = .25), .95))
percentiles <- mutate(percentiles, income = getIncomeForPercentile(p),
                      labs = paste0(as.character(p * 100), "th Percentile"),
                      xlabs = paste(scales::dollar(income), labs, sep = "\n"))
percentiles$xlabs[percentiles$p == .5] <- 
  sub("50th Percentile", "Median", percentiles$xlabs[percentiles$p == .5])

stop()
ggplot(filter(datDiff, payer == "Individual"), aes(x = income)) +
  geom_segment(aes(xend = income, yend = 0.51, y = .2), percentiles,
               linetype = 2, color = "grey40") +
  geom_ribbon(aes(ymax = iTop, ymin = iBottom, fill = "Increase")) +
  geom_ribbon(aes(ymax = dTop, ymin = dBottom, fill = "Decrease")) +
#   geom_line(aes(y = eTax - .0025, x = income * 1.03, group = set), datSum, 
#             size = 2, color = "grey20") + 
  geom_line(aes(y = eTaxBern, color = "Bernie"), size = 1.9) +
  geom_line(aes(y = eTaxCur, color = "Current"), size = 1.9) +
  scale_x_log10(breaks = percentiles$income, labels = percentiles$xlabs) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Tax Plan", values = c("#287CBF", "#EB514F")) +
  scale_fill_manual("Change under Bernie's Plans", 
                    values = c("#7db6e3", "#f18b89")) +
  theme_classic() +
  theme(axis.line = element_blank(), leged.position = "bottom") 

  #stat_smooth(aes(y = centile), acs, se = F, method = loess) +
  #coord_cartesian(xlim = c(min(incomes), max(incomes)))


# distrPlot <- ggplot(acs, aes(x = Dollars, y = Number)) +
#   stat_smooth(se = F, method = loess) +
#   coord_cartesian(xlim = c(min(incomes), max(incomes))) +
#   theme_classic() +
#   theme(axis.line = element_blank())
# 
# lay <- rbind(1, 1, 2)
# grid.arrange(taxPlots, distrPlot, layout_matrix = lay)

ggplot(datDiff, aes(x = income)) +
  geom_ribbon(aes(ymax = iTop, ymin = iBottom, fill = "Increase")) +
  geom_ribbon(aes(ymax = dTop, ymin = dBottom, fill = "Decrease")) +
  geom_line(aes(y = eTax - .0025, x = income * 1.03, group = set), datSum, 
            size = 2, color = "grey20") + 
  geom_line(aes(y = eTax, color = set), datSum, size = 1.9) +
  facet_grid(payer ~ ., scales = "free") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Tax Plan", values = c("#287CBF", "#EB514F")) +
  scale_fill_manual("Change under Bernie's Plans", 
                    values = c("#7db6e3", "#f18b89")) +
  theme_classic() +
  theme(axis.line = element_blank()) +
  stat_smooth(aes(y = centile), acs, se = F, method = loess) +
  coord_cartesian(xlim = c(min(incomes), max(incomes)))
