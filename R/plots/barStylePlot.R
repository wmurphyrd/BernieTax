

library(dplyr); library(tidyr); library(ggplot2); library(readxl)
source("R/functions/bernieTaxBrackets.R")
source("R/functions/bernietaxFunctions.R")

barStylePlot <- function(filingStatus, nKids, sex, 
                         employer = c("ignore", "isolate", "split", "pool"),
                         customIncomes = numeric(0)) {
  employer = match.arg(employer)

  acsList <- getCensusIncomes(filingStatus, sex)

  #get wide range of incomes to find a few landmark incomes to display
  incomeRange <- c(seq(min(acsList$acs$income), 
                       max(acsList$acs$income), by = 1000),
                   max(acsList$acs$income))
  dat <- taxesByIncomes(incomeRange, filingStatus, nKids, sex, employer) 
  savings <- netTaxDifferences(dat, acsList)

  # find breakeven point as income level with smallest savings that is greater 
  # than the lowest income level with savings 
  savings <- arrange(savings, income)
  startIncome <- savings$income[min(which(savings$delta > 0))]
  savings <- filter(savings, income > startIncome)
  breakeven <- savings$income[which.max(ifelse(savings$delta > 0, NA, 
                                                 savings$delta))]
  maxSavings <- savings$income[which.max(savings$delta)]
  
  #choose breakeven point, common percentiles, max savings, highest group, and
  #any custom incomes as incomes to display
  incomes <- c(min(acsList$acs$income), maxSavings,
               max(acsList$acs$income), customIncomes)
  if(employer != "split") incomes <- c(incomes, breakeven)
  #avoid ploting two nearby incomes with landmark incomes are near common
  #percentiles
  commonPercentiles <- unlist(sapply(c(.25, .5, .75, .95), function(x) {
    x[!any(abs(x - na.omit(acsList$getPercentileForIncome(incomes))) < .04)]
  }))
  commonPercentiles <- acsList$getIncomeForPercentile(c(.05, commonPercentiles))
  incomes <- sort(unique(c(incomes, commonPercentiles)))

  incomeScaleFun <- approxfun(incomes, seq_along(incomes))
  denseDistr <- rep(acsList$acs$income, acsList$acs$Number)
  denseFun <- MASS::fitdistr(na.omit(incomeScaleFun(denseDistr)), "lognormal")
  
  dat <- taxesByIncomes(incomes, filingStatus, nKids, sex, employer)
  dat$payer = factor(dat$payer)
  levels(dat$payer) <- list("Personal Taxes" = "Individual",
                             "Employer-Paid Taxes" = "Employer")

  dat2 <- filter(dat, amount != 0) %>% 
    mutate(percentile = acsList$getPercentileForIncome(income),
           set = factor(set, levels = c("Current", "Bernie")),
           incomeFactor = factor(income, levels = incomes),
           offset = (as.numeric(set) - (length(levels(set)) + 1) / 2) / 
             (length(levels(set)) + .6) + as.numeric(incomeFactor),
           expenseGroup = factor(expense))

  levels(dat2$expenseGroup) <- list("Income Tax" = c("Income Tax"), 
                                    "Payroll Taxes" = 
                                      c("Social Security Tax",
                                        "Medicare Tax",
                                        "Family Leave Tax",
                                        "Employer Payroll Tax"),
                                    "Healthcare Tax" = 
                                      c("Medicare-for-all Tax",
                                        "Employer Medicare-for-all Tax"),
                                    "Healthcare Premiums" = 
                                      c("Healthcare Premiums",
                                        "Employer Healthcare\nContribution"),
                                    "Healthcare Out-of-Pocket" = 
                                      c("Healthcare Expenses"))
  dat2 <- group_by(dat2, offset, income, incomeFactor, effectiveIncome, set, 
                   payer, expenseGroup) %>% 
    summarise(amount = sum(amount)) %>%
    group_by(payer, set, income) %>%
    # effective income is only a useful concept when pooling employer/employee
    mutate(eTax  = amount / ifelse(employer == "pool", 
                                   effectiveIncome, income), 
           labely = cumsum(eTax) - 0.5 * eTax,
           labely = ifelse(abs(eTax) < .017 & 
                             grepl("Income Tax", expenseGroup), 
                           0, labely),
           labely = ifelse(abs(eTax) < .017 & 
                             grepl("Healthcare [TO]", expenseGroup), 
                           cumsum(eTax), labely),
           pctLabelJust = ifelse(eTax < .017, .5, NA) + 
             .6 * grepl("Income Tax", expenseGroup) +
             -.5 * grepl("Healthcare [TO]", expenseGroup),
           pctLabelText = ifelse((eTax >= .0035 | labely == 0) & 
                                   labely >= 0 & eTax > 0, 
                                 scales::percent(round(eTax, 3)), ""))

  centileLabeler <- function(x) {
    inc <- incomes[as.numeric(x)]
    pct <- acsList$getPercentileForIncome(inc)
    lab <- ifelse(is.na(pct), scales::dollar(inc), 
                  acsList$centileLabeler(pct))
    paste0(lab, ifelse(inc == maxSavings, "\n(Maximum Savings)", 
                       ifelse(inc == breakeven, 
                              "\n(Cross-over Point)","")))
  }
  
  #Create a parellel ramp palettes in two colors for bar groups
  cols <- dat2 %>% ungroup %>% select(exp = expenseGroup, set) %>% unique 
  cols <- arrange(cols, set, exp)
  cols$key <- paste(cols$set, cols$exp)
  blues <- colorRampPalette(c("dodgerblue4", "slategray2"))
  greys <- colorRampPalette(c("grey40", "grey80"))
  fillPal <- c(greys(nrow(filter(cols, set == "Current"))),
               blues(nrow(filter(cols, set == "Bernie"))))
  names(fillPal) <- cols$key
  
  #find labels for lightest color of each palette for the label fill
  lastBernColLab <- names(fillPal)[max(grep("Bernie", names(fillPal)))]
  lastCurColLab <- names(fillPal)[max(grep("Current", names(fillPal)))]
  savings <- netTaxDifferences(dat, acsList)
  savings <- savings %>%
    mutate(labely = pmax(pmin(eTaxBern, eTaxCur), 0),
           set = ifelse(increase, "Current", "Bernie"),
           hjust = -.3,
           fillKey = ifelse(increase, lastCurColLab, lastBernColLab))
  #Need correspnding x-axis offset positions to display savings labels in
  #correct place
  savings <- dat2 %>% 
    ungroup() %>% 
    select(income, set, offset) %>%
    unique %>% 
    inner_join(savings)
  
#   xlims <- c(min(dat2[dat2$income == startIncome, "offset"]),
#              max(dat2$offset) + .2)
#   #expand upper y limit for increase/decrease labels
#   ylims <- c(0, 1.2 * max(
#     (dat2 %>% group_by(set, income) %>% summarise(eTax = sum(eTax)))$eTax))
  
  title <- paste("Bernie Sanders Tax Plan Impact for", 
                 ifelse(filingStatus == "Married/Joint", 
                        paste("Family of", nKids + 2), 
                        paste("Single", ifelse(sex == "M", "Male", "Female"),
                              ifelse(nKids > 0, paste0("with ", nKids, " Child",
                                                       ifelse(nKids > 1, "ren", 
                                                              "")),""))))
  
  plt <- ggplot(dat2, aes(x = offset, y = eTax, 
                          fill = paste(set, expenseGroup))) +
    stat_function(fun = dlnorm, args = denseFun$estimate,
                  geom = "area", fill = "black", alpha = .75) +
    geom_bar(position = "stack", stat = "identity",
             data = filter(dat2, set == "Bernie"), width = 0.35) +
    geom_bar(position = "stack", stat = "identity",
             data = filter(dat2, set == "Current"), width = 0.35) +
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
  if(employer == "split") plt <- plt + facet_grid(~payer)
  plt
}
