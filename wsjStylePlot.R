library(dplyr); library(tidyr); library(ggplot2); library(readxl)
source("bernieTaxBrackets.R")
source("bernietaxFunctions.R")

filingStatus <- "Married/Separate"
nKids <- 0
#filingStatus <- "Married/Joint"
#nKids <- 2
sex <- "M"

acsList <- getCensusIncomes(filingStatus, sex)
incomes <- c(2500, 16000,acsList$getIncomeForPercentile(c(.5, .75, .95)),
             max(acsList$acs$income))

incomeScaleFun <- approxfun(incomes, seq_along(incomes))
# incomeScaleFunInv <- approxfun(c(.001,seq_along(incomes)),
#                                c(acsList$acs$income[1],incomes))
acsList$acs$scale <- incomeScaleFun(acsList$acs$income)
# denseData <- with(acsList$acs[acsList$acs$scale > 0, ], rep(scale, Number))
denseData <- rep(acsList$acs$income, acsList$acs$Number)
denseFun <- MASS::fitdistr(na.omit(incomeScaleFun(denseData)), "lognormal")

dat <- taxesByIncomes(incomes, filingStatus, nKids, sex)          

dat2 <- filter(dat, payer == "Individual", amount != 0) %>% 
  mutate(percentile = acsList$getPercentileForIncome(income),
         set = factor(set, levels = c("Current", "Bernie")),
         incomeFactor = factor(income, levels = incomes),
         offset = (as.numeric(set) - (length(levels(set)) + 1) / 2) / 
           (length(levels(set)) + .3) + as.numeric(incomeFactor),
         expenseGroup = factor(expense))
levels(dat2$expenseGroup) <- list("Income Tax" = c("Income Tax"), 
                                  "Other Taxes" = 
                                    c("Social Security Tax",
                                      "Medicare Tax",
                                      "Family Leave Tax",
                                      "Employer Payroll Tax"),
                                  "Healthcare Tax" = 
                                    c("Medicare-for-all Tax"),
                                  "Healthcare Costs" = 
                                    c("Healthcare Premiums",
                                      "Healthcare Expenses",
                                      "Employer Healthcare\nContribution"))
dat2 <- group_by(dat2, offset, income, incomeFactor, effectiveIncome, set, 
                 payer, expenseGroup) %>% summarise(amount = sum(amount)) %>%
  group_by(payer, set, income) %>%
  #arrange(income, expenseGroup) %>%
  mutate(eTax  = amount / effectiveIncome, 
         labely = cumsum(pmax(eTax, 0)) - 0.5 * eTax)

centileLabeler <- function(x) {
  acsList$centileLabeler(acsList$getPercentileForIncome(incomes[as.numeric(x)]))
}

cols <- expand.grid(exp = unique(dat2$expenseGroup), set = unique(dat2$set))
cols <- arrange(cols, set, exp)
cols$key <- paste(cols$set, cols$exp)

blues <- colorRampPalette(c("blue4", "lightskyblue1"))
greys <- colorRampPalette(c("grey40", "grey70"))
colorPal <- c(greys(nrow(filter(cols, set == "Current"))),
              blues(nrow(filter(cols, set == "Bernie"))))
names(colorPal) <- cols$key

ggplot(dat2, aes(x = offset, y = eTax, 
                 fill = paste(set, expenseGroup))) +
  stat_function(fun = dlnorm, args = denseFun$estimate,
                geom = "area", fill = "grey20") +
  geom_bar(position = "stack", stat = "identity",
           data = filter(dat2, set == "Bernie"), width = 0.4) +
  geom_bar(position = "stack", stat = "identity",
           data = filter(dat2, set == "Current"), width = 0.4) +
  geom_text(aes(y = labely, label = scales::percent(eTax))) +
  #scale_fill_gradientn(colors = c("blue4", "deepskyblue", "grey27", "grey65")) +
  scale_fill_manual("Expense", values = colorPal) +
  scale_x_reverse(breaks = seq_along(incomes), labels = centileLabeler) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(xlim = c(1.7, 6.5), ylim = c(0, .45))
