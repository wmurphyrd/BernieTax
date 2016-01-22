
# https://berniesanders.com/issues/medicare-for-all
incomeBracketsBernie <- data.frame(
  bottom = c(0, 18450, 74900, 151200, 230450, 250000, 
             499999, 2000000, 10000000),
  cap = c(18450, 74900, 151200, 230450, 250000, 
          499999, 2000000, 10000000, Inf),
  rate  = c(.1, .15, .25, .28, .33, .37, .43, .48, .52),
  extra = 0
)

#http://www.bankrate.com/finance/taxes/tax-brackets.aspx
#Married/jointly 2015 rate
incomeBracketsCurrent <- data.frame(
  bottom = c(0, 18450, 74900, 151200, 230450, 411500, 464850),
  cap = c(18450, 74900, 151200, 230450, 411500, 464850, Inf),
  rate = c(.1, .15, .25, .28, .33, .35, .396),
  extra = 0
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
  extra = 0
)

medicareBracketsCurrent <- data.frame(
  bottom = c(0, 200000),
  cap = c(200000, Inf),
  rate = c(.0145, .0145 + .009),
  extra = 0
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
  extra = c(0, 0, 8153)
)


mfaBracketsCurrent <- data.frame(
  bottom = 0, cap = Inf, rate = 0, extra = 0
)

mfaBracketsBernie <- data.frame(
  bottom = 0, cap = Inf, rate = .022, extra = 0
)

familyLeaveBracketsCurrent <- data.frame(
  bottom = 0, cap = Inf, rate = 0, extra = 0
)

#http://www.gillibrand.senate.gov/issues/paid-family-medical-leave
familyLeaveBracketsBernie <- data.frame(
  bottom = c(0, 113700),
  cap = c(113700, Inf),
  rate = c(.002, 0),
  extra = 0
)

#fpls for family 4
#http://familiesusa.org/product/federal-poverty-guidelines
fpl4 <- 24250
#http://obamacarefacts.com/insurance-exchange/premium-tax-credits/
healthPremBracketsCurrent <- data.frame(
  bottom = c(0, fpl4 * 1.33, fpl4 * 1.5, fpl4 * 2, fpl4 * 2.5, fpl4 * 4),
  cap = c(fpl4 * 1.33, fpl4 * 1.5, fpl4 * 2, fpl4 * 2.5, fpl4 * 4, Inf),
  #note, 250-400% of FPL has obamacare rate max of 9.66%, but this exceeds
  #national average, so the flat average is used instead
  rate = c(0, .047, .0641, .0818, 0, 0),
  extra = c(0, 0, 0, 0, 6408, 6408)
)

#out of pocket expenses set at $0 for medicaid recipients, or at the
#national average of $4,065 for all others 
#(assumes state with obamacare medicaid expansion)
#http://www.milliman.com/uploadedFiles/insight/Periodicals/mmi/2015-MMI.pdf 
#page 7
#http://obamacarefacts.com/obamacares-medicaid-expansion/
healthPocketBracketsCurrent <- data.frame(
  bottom = c(0, fpl4*1.33),
  cap = c(fpl4*1.33, Inf),
  rate = 0,
  extra = c(0, 4065)
)

healthPremBracketsBernie <- data.frame(
  bottom = 0, cap = 0, rate = 0, extra = 0
)

healthPocketBracketsBernie <- data.frame(
  bottom = 0, cap = 0, rate = 0, extra = 0
)

tax <- function(brackets, x) {
  sum(mapply(function(income, bottom, cap, rate, extra){
    if(income > cap) income <- cap
    margin <- income - bottom
    if(margin < 0) return(0)
    margin * rate + extra
  }, bottom = brackets$bottom,
  cap = brackets$cap, rate = brackets$rate, extra = brackets$extra,
  MoreArgs = list(income = x))) 
}

taxes <- function(incomes, brackets, ssExtra) {
  names(incomes) <- as.character(incomes)
  ret <- data.frame(t(sapply(incomes, function(x) sapply(brackets, tax, x))),
                    check.names = F)
  ret$income <- incomes
  ret
}

taxNames <- c("Income Tax", "Social Security Tax", "Medicare Tax",
                  "Medicare-for-all Tax", "Family Leave Tax", 
              "Healthcare Premiums", "Healthcare Expenses")

currentBrackets <- list(incomeBracketsCurrent,
                        ssBracketsCurrent,
                        medicareBracketsCurrent,
                        mfaBracketsCurrent,
                        familyLeaveBracketsCurrent,
                        healthPremBracketsCurrent,
                        healthPocketBracketsCurrent)
bernieBrackets <- list(incomeBracketsBernie,
                       ssBracketsBernie,
                       medicareBracketsCurrent,
                       mfaBracketsBernie,
                       familyLeaveBracketsBernie,
                       healthPremBracketsBernie,
                       healthPocketBracketsBernie)

names(currentBrackets) <- taxNames
names(bernieBrackets) <- taxNames

incomes <- seq(20000, 400000, by = 2000)
incomelabs <- c(24, 54, 150, 400)
#incomes <- seq(20000, 5000000, by = 10000)

cur <- taxes(incomes, currentBrackets)
#http://www.milliman.com/uploadedFiles/insight/Periodicals/mmi/2015-MMI.pdf 
#page 7
#cur$`Healthcare Expense` <- 10473
cur$set <- "Current"
bern <- taxes(incomes, bernieBrackets)
#bern$`Healthcare Expense` <- 0
bern$set <- "Bernie"

library(tidyr)
dat <- rbind(gather(cur, expense, amount, -income, -set),
      gather(bern, expense, amount,  -income, -set))
dat <- dplyr::mutate(dat, rate = amount / income,
                     incomeT = income / 1000)
library(ggplot2)
png("bernietax.png", width = 960, height = 960)
ggplot(dat, aes(x = incomeT, y = rate, fill = expense)) + 
  geom_area(position = "stack") + facet_grid(set~.) +
  scale_x_log10(labels = scales::dollar, 
                breaks = incomelabs) +
  theme(text = element_text(size = 24)) +
  guides(fill = guide_legend("")) +
  labs(x = "Taxable Income (thousands)", 
       y = "Effective Tax Rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, .5, by = .1))
dev.off()
