#total effective tax rates with employer contributions

library(dplyr); library(tidyr); library(ggplot2); library(readxl)
source("R/functions/bernieTaxBrackets.R")
source("R/functions/bernietaxFunctions.R")

useCorporateWelfare <- F
#filingStatus <- "Single"
#nKids <- 0
filingStatus <- "Married/Joint"
nKids <- 2
sex <- "F"

taxNamesInd <- c("Income Tax", "Social Security Tax", "Medicare Tax",
                 "Medicare-for-all Tax", "Family Leave Tax", 
                 "Healthcare Premiums", "Healthcare Expenses")
taxNamesEmp <- c("Employer Healthcare\nContribution",
                 "Employer Payroll Tax", "Corporate Welfare")

#incomes <- seq(8000, 402000, by = 2000)
incomes <- seq(10000, 50000000, by = 10000)

acsList <- getCensusIncomes(filingStatus, sex)
getIncomeForPercentile <- acsList$getIncomeForPercentile
getPercentileForIncome <- acsList$getPercentileForIncome
acs <- acsList$acs
# glm modeling allows for smoother interpolation, but becomes inaccurate
# at extremes. Linear interpolation used instead
# mod <- glm(centile ~ income, acs, family = "binomial")
# getPercentileForIncome <- function(x) {
#   predict(mod, data.frame(income = x), type = "response")
# }

centileLabeler <- acsList$centileLabeler

incomes <- c(incomes, getIncomeForPercentile(c(.25, .5, .75)))

totalDeduction <- getDeduction(incomes, filingStatus, nKids)

brackets <- getBrackets(filingStatus, useCorporateWelfare, nKids)


pCur <- taxes(incomes, c(brackets$currentIndBrackets, 
                        brackets$currentEmpBrackets), totalDeduction) %>% 
  mutate(set = "Current") %>%
  applyCredits(filingStatus, nKids)
pBern <- taxes(incomes, c(brackets$bernieIndBrackets,
                          brackets$bernieEmpBrackets), totalDeduction) %>% 
  mutate(set = "Bernie") %>%
  applyCredits(filingStatus, nKids)

pDat <- rbind(gather(rbind(pCur, pBern),
                    expense, amount, -income, -effectiveIncome, -set, -agi))

#total effective tax rates
pDatSum <- pDat %>% group_by(set, income, effectiveIncome, agi) %>% 
  summarize(tTax = sum(amount)) %>% ungroup %>% 
  mutate(eTax = tTax / effectiveIncome)

pDatSum <- mutate(pDatSum, percentile = getPercentileForIncome(income))

#differences between plans / ribbon data
pDatDiff <- inner_join(filter(pDatSum, set == "Bernie"), 
                      filter(pDatSum, set == "Current"), 
                      by = c("income", "percentile")) %>%
  rename(eTaxBern = eTax.x, eTaxCur = eTax.y) %>%
  mutate(increase = eTaxBern > eTaxCur,
         iTop = ifelse(increase, eTaxBern, NA),
         iBottom = ifelse(increase, eTaxCur, NA),
         dTop = ifelse(!increase, eTaxCur, NA),
         dBottom = ifelse(!increase, eTaxBern, NA))


png("img/png/bernieBillionsWithEmpTax.png", width = 1024, height = 768)
source("R/plots/berniePooledPlot.R", print.eval = T)
dev.off()
