library(dplyr); library(tidyr); library(ggplot2); library(readxl)
source("bernieTaxBrackets.R")
source("bernietaxFunctions.R")

useCorporateWelfare <- F
filingStatus <- "Single"
nKids <- 0
sex <- "M"

taxNamesInd <- c("Income Tax", "Social Security Tax", "Medicare Tax",
                 "Medicare-for-all Tax", "Family Leave Tax", 
                 "Healthcare Premiums", "Healthcare Expenses")
taxNamesEmp <- c("Employer Healthcare\nContribution",
                 "Employer Payroll Tax", "Corporate Welfare")

incomes <- seq(8000, 402000, by = 2000)
#alternate for billionaires graph
#incomes <- seq(10000, 50000000, by = 10000)


#https://www.irs.com/articles/2015-federal-tax-rates-personal-exemptions-and-standard-deductions
#standardDeduction <- 12600
#https://www.irs.gov/publications/p17/ch03.html
#exemptions <- 4 * 4000
#exPhaseOut <- pmin(ceiling(pmax(0, incomes - 309900) / 2500) * .02, 1)
#exemptions <- exemptions - exemptions * exPhaseOut
#totalDeduction <- standardDeduction + exemptions
totalDeduction <- getDeduction(incomes, filingStatus, nKids)

brackets <- getBrackets(filingStatus, useCorporateWelfare, nKids)
cur <- taxes(incomes, brackets$currentIndBrackets, totalDeduction) %>% 
  mutate(set = "Current", payer = "Individual") %>%
  applyCredits(filingStatus, nKids)
bern <- taxes(incomes, brackets$bernieIndBrackets, totalDeduction) %>% 
  mutate(set = "Bernie", payer = "Individual") %>%
  applyCredits(filingStatus, nKids)

curEmp <- taxes(incomes, brackets$currentEmpBrackets) %>%
  mutate(set = "Current", payer = "Employer")
bernEmp <- taxes(incomes, brackets$bernieEmpBrackets) %>%
  mutate(set = "Bernie", payer = "Employer")

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
#https://www.census.gov/hhes/www/cpstables/032015/perinc/pinc11_000.htm
# acs <- read_excel("finc07.xls", skip = 8)[c(-1, -46), 1:3]
# acs <- acs %>% rename(income = `Mean \n Income (dollars)`) %>% 
#   mutate(prop = Number / sum(Number), centile = cumsum(prop),
#          payer = "Population")
acs <- getCensusIncomes(filingStatus, sex)
getIncomeForPercentile <- approxfun(acs$centile, acs$income)
getPercentileForIncome <- approxfun(acs$income, acs$centile)
# glm modeling allows for smoother interpolation, but becomes inaccurate
# at extremes. Linear interpolation used instead
# mod <- glm(centile ~ income, acs, family = "binomial")
# getPercentileForIncome <- function(x) {
#   predict(mod, data.frame(income = x), type = "response")
# }

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

savings <- datDiff %>% filter(payer == "Individual") %>%
  mutate(delta = (eTaxCur - eTaxBern) * income, 
         mid = (eTaxCur + eTaxBern)/2)
whichSavings <- c(which.min(abs(savings$percentile - .25)),
                  which.min(abs(savings$percentile - .5)),
                  which.min(abs(savings$percentile - .75)),
                  which.max(savings$percentile))
#whichSavings <- c(40000, 80000, 120000, 400000)
savings <- savings[whichSavings,
                   c("income", "delta", "mid", 
                     "eTaxCur", "eTaxBern")]
#savings$percentile <- getPercentileForIncome(whichSavings)
savings$percentile <- c(.25, .5, .75, max(datDiff$percentile))
savings$hjust <- c(-.08, .5, .5, -.05)
savings$fill <- ifelse(savings$delta < 0, "Increase", "Savings")
savings$delta <- abs(savings$delta)
savings$lab <- paste0(savings$fill, ": ", scales::dollar(savings$delta))
savings$mid[1] <- savings$mid[1] + .05
savings$lab[4] <- paste("Top 5% Average", "Income ($400K)", savings$lab[4], sep = "\n")


png("bernieTax_color.png", width = 1024, height = 768)
source("berniePlot.R", print.eval = T)
dev.off()



#data table export
export <- rbind(cur, bern) %>% merge(datSum)
export <- rename(export, `Total Tax` = tTax, `Effective Tax Rate` = eTax,
                 `Income Percentile` = percentile, Income = income) %>%
  select(-agi, -effectiveIncome, -payer)
export <- merge(filter(export, set == "Bernie") %>% select(-set),
      filter(export, set == "Current") %>% select(-set),
      by = c("Income", "Income Percentile"),
      suffixes = c(" with Bernie", " at Present")) %>%
  arrange(Income)

write.csv(export, "BernieTaxExport.csv", row.names = F)