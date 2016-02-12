library(dplyr); library(tidyr); library(ggplot2); library(readxl)
source("bernieTaxBrackets.R")
source("bernietaxFunctions.R")

useCorporateWelfare <- F
writePlotToDisk <- F
filingStatus <- "Single"
nKids <- 0
#filingStatus <- "Married/Joint"
#nKids <- 2
sex <- "M"

incomes <- seq(8000, 402000, by = 2000)
#incomes <- seq(10000, 50000000, by = 10000)

acsList <- getCensusIncomes(filingStatus, sex)
acs <- acsList$acs
getIncomeForPercentile <- acsList$getIncomeForPercentile
getPercentileForIncome <- acsList$getPercentileForIncome
centileLabeler <- acsList$centileLabeler

incomes <- c(incomes, getIncomeForPercentile(c(.25, .5, .75)))

dat <- taxesByIncomes(incomes, filingStatus, nKids, sex, useCorporateWelfare)

#total effective tax rates
datSum <- dat %>% group_by(payer, set, income, effectiveIncome, agi) %>% 
  summarize(tTax = sum(amount)) %>% ungroup %>% 
  mutate(eTax = tTax / effectiveIncome,
         payer = factor(payer, levels = c("Individual", "Employer")))


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


if(writePlotToDisk) png("bernieTax_color.png", width = 1024, height = 768)

if(filingStatus == "Married/Joint") {
  source("berniePlot.R", print.eval = T)
} else {
  if(sex == "M") {
    source("berniePlotSingle.R", print.eval = T)
  } else {
    source("berniePlotSingleF.R", print.eval = T)
  }
}

if(writePlotToDisk) dev.off()


#data table export (not currently working)
# export <- rbind(cur, bern) %>% merge(datSum)
# export <- rename(export, `Total Tax` = tTax, `Effective Tax Rate` = eTax,
#                  `Income Percentile` = percentile, Income = income) %>%
#   select(-agi, -effectiveIncome, -payer)
# export <- merge(filter(export, set == "Bernie") %>% select(-set),
#       filter(export, set == "Current") %>% select(-set),
#       by = c("Income", "Income Percentile"),
#       suffixes = c(" with Bernie", " at Present")) %>%
#   arrange(Income)
# 
# write.csv(export, "BernieTaxExport.csv", row.names = F)