

library(dplyr); library(tidyr); library(ggplot2); library(readxl)
source("bernieTaxBrackets.R")
source("bernietaxFunctions.R")

barStylePlot <- function(filingStatus, nKids, sex, 
                         employer = c("ignore", "isolate", "pool"),
                         customIncomes = numeric(0)) {
  employer = match.arg(employer)
  # load census data
  acsList <- getCensusIncomes(filingStatus, sex)
  #if(any(is.na(customIncomes))) {
  #find best incomes to display
  incomeRange <- c(seq(min(acsList$acs$income), 
                       max(acsList$acs$income), by = 1000),
                   max(acsList$acs$income),
                   acsList$getIncomeForPercentile(c(.5, .75, .95)))
  dat <- taxesByIncomes(incomeRange, filingStatus, nKids, sex, employer) 
  savings <- netTaxDifferences(dat, acsList)
  #start with lowest income with no negative income tax (because negative numbers don't stack well)
  startIncome <- min(group_by(dat, income) %>% 
                       filter(all(amount >= 0)) %>% 
                       magrittr::extract2("income"))
  startIncomeLab <- ifelse(employer == "isolate", "", "\n(Income Tax Threshold)")
  #find max savings
  maxSavings <- savings$income[which.max(savings$delta)]
  # Start income and max savings tend to be right next to each other, so just
  # keep one
  if(maxSavings > startIncome) {
    if(acsList$getIncomeForPercentile(.25) > startIncome) {
      startIncome <- acsList$getIncomeForPercentile(.25)
      startIncomeLab <- ""
    } else startIncome <- maxSavings
  }
  #find breakeven point
  savings <- filter(savings, income > startIncome)
  breakeven <- savings$income[which.min(abs(savings$delta)) + 1]
  
  #choose lowest income without negative income tax, breakeven point, common
  #percentiles, max savings, and highest group
  incomes <- sort(unique(c(min(acsList$acs$income), startIncome, breakeven,
                           maxSavings,
                           acsList$getIncomeForPercentile(c(.5, .75, .95)),
                           max(acsList$acs$income),
                           customIncomes)))
  #} else {
#   if(any(!is.na(customIncomes))) {
#     incomes <- customIncomes
#   }
  incomeScaleFun <- approxfun(incomes, seq_along(incomes))
  denseDistr <- rep(acsList$acs$income, acsList$acs$Number)
  denseFun <- MASS::fitdistr(na.omit(incomeScaleFun(denseDistr)), "lognormal")
  
  dat <- taxesByIncomes(incomes, filingStatus, nKids, sex, employer)          
  #dat <- filter(dat, income %in% incomes)
  dat2 <- filter(dat, amount != 0) %>% 
    mutate(percentile = acsList$getPercentileForIncome(income),
           set = factor(set, levels = c("Current", "Bernie")),
           incomeFactor = factor(income, levels = incomes),
           offset = (as.numeric(set) - (length(levels(set)) + 1) / 2) / 
             (length(levels(set)) + .3) + as.numeric(incomeFactor),
           expenseGroup = factor(expense))
  levels(dat2$expenseGroup) <- list("Income Tax" = c("Income Tax"), 
                                    "Payroll Taxes" = 
                                      c("Social Security Tax",
                                        "Medicare Tax",
                                        "Family Leave Tax",
                                        "Employer Payroll Tax"),
                                    "Healthcare Tax" = 
                                      c("Medicare-for-all Tax"),
                                    "Healthcare Premiums" = 
                                      c("Healthcare Premiums",
                                        "Employer Healthcare\nContribution"),
                                    "Healthcare Out-of-Pocket" = 
                                      c("Healthcare Expenses"))
  dat2 <- group_by(dat2, offset, income, incomeFactor, effectiveIncome, set, 
                   payer, expenseGroup) %>% 
    summarise(amount = sum(amount)) %>%
    group_by(payer, set, income) %>%
    mutate(eTax  = amount / effectiveIncome, 
           labely = cumsum(pmax(eTax, 0)) - 0.5 * eTax,
           labely = ifelse(eTax < .015 & grepl("Income Tax", expenseGroup), 0, labely),
           labely = ifelse(eTax < .015 & grepl("Healthcare [TO]", expenseGroup), 
                           cumsum(pmax(eTax, 0)), labely),
           pctLabelJust = ifelse(eTax < .015, .5, NA) + 
             .6 * grepl("Income Tax", expenseGroup) +
             -.7 * grepl("Healthcare [TO]", expenseGroup),
           pctLabelText = ifelse(eTax > .002, 
                                 scales::percent(round(eTax, 3)), ""))
  
  centileLabeler <- function(x) {
    inc <- incomes[as.numeric(x)]
    pct <- acsList$getPercentileForIncome(inc)
    lab <- ifelse(is.na(pct), scales::dollar(inc), 
                  acsList$centileLabeler(pct))
    paste0(lab, ifelse(inc ==  startIncome, startIncomeLab,
                       ifelse(inc == maxSavings, "\n(Maximum Savings)", 
                              ifelse(inc == breakeven, "\n(Cross-over Point)",
                                     ""))))
  }
  
  #Create a parellel ramp palettes in two colors for bar groups
  #cols <- expand.grid(exp = unique(dat2$expenseGroup), set = unique(dat2$set))
  cols <- dat2 %>% ungroup %>% select(exp = expenseGroup, set) %>% unique 
  #cols <- arrange(cols, as.character(set), as.character(exp))
  cols <- arrange(cols, set, exp)
  cols$key <- paste(cols$set, cols$exp)
  #cols$key <- factor(cols$key, levels = unique(cols$key))
  blues <- colorRampPalette(c("dodgerblue4", "slategray2"))
  greys <- colorRampPalette(c("grey40", "grey80"))
  fillPal <- c(greys(nrow(filter(cols, set == "Current"))),
               blues(nrow(filter(cols, set == "Bernie"))))
  names(fillPal) <- cols$key
  
  savings <- netTaxDifferences(dat, acsList)
  savings <- savings %>%
    filter(payer == "Individual") %>%
    mutate(labely = eTaxBern,
           hjust = -.15,
           fillKey = ifelse(increase, 
                            "Current Healthcare Out-of-Pocket",
                            "Bernie Healthcare Tax"))
  
  
  savings <- dat2 %>% 
    ungroup() %>% 
    filter(set == "Bernie") %>% 
    select(income, offset) %>%
    unique %>% 
    inner_join(savings)
  
#   xlims <- c(min(dat2[dat2$income == startIncome, "offset"]),
#              max(dat2$offset) + .2)
#   #expand upper y limit for increase/decrease labels
#   ylims <- c(0, 1.2 * max(
#     (dat2 %>% group_by(set, income) %>% summarise(eTax = sum(eTax)))$eTax))
  
  title <- paste("Bernie Sanders Tax Plan Impact for", ifelse(filingStatus == "Married/Joint", 
                  paste("Family of", nKids + 2), 
                  paste("Single", ifelse(sex == "M", "Male", "Female"),
                        ifelse(nKids > 0, paste0("with ", nKids, " Child",
                                                ifelse(nKids > 1, "ren", "")),
                               ""))))
  
  ggplot(dat2, aes(x = offset, y = eTax, 
                   fill = paste(set, expenseGroup))) +
    stat_function(fun = dlnorm, args = denseFun$estimate,
                  geom = "area", fill = "grey20") +
    geom_bar(position = "stack", stat = "identity",
             data = filter(dat2, set == "Bernie"), width = 0.4) +
    geom_bar(position = "stack", stat = "identity",
             data = filter(dat2, set == "Current"), width = 0.4) +
    geom_text(aes(y = labely, label = pctLabelText, 
                  hjust = pctLabelJust), fontface = "bold") +
    geom_label(aes(y = labely, label = lab, hjust = hjust, fill = fillKey),
               data = savings, size = 4.5) +
    scale_fill_manual("Expense", values = fillPal) +
    scale_x_reverse(breaks = seq_along(incomes), labels = centileLabeler) +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = .1)) +
    #coord_flip(xlim = xlims, ylim = ylims) +
    coord_flip() +
    labs(x = "Income", y = "Effective Tax Rate with Healthcare Burden",
         title = title) +
    theme(legend.position = "bottom", text = element_text(size = 18))
}
