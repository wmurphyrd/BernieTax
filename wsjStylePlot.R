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

denseModel <- glm(Number ~ income, acsList$acs, family = gaussian(link = "log"))
denseDistr <- rep(acsList$acs$income, acsList$acs$Number)
denseFun <- MASS::fitdistr(na.omit(incomeScaleFun(denseDistr)), "lognormal")

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
                 payer, expenseGroup) %>% 
  summarise(amount = sum(amount)) %>%
  group_by(payer, set, income) %>%
  mutate(eTax  = amount / effectiveIncome, 
         labely = cumsum(pmax(eTax, 0)) - 0.5 * eTax,
         pctLabelJust = ifelse(eTax < .015, .5, NA) + 
           .7 * grepl("Income Tax", expenseGroup) +
           -.75 * grepl("Healthcare", expenseGroup))

centileLabeler <- function(x) {
  acsList$centileLabeler(acsList$getPercentileForIncome(incomes[as.numeric(x)]))
}

#Create a parellel ramp palettes in two colors for bar groups
cols <- expand.grid(exp = unique(dat2$expenseGroup), set = unique(dat2$set))
#cols <- arrange(cols, as.character(set), as.character(exp))
cols <- arrange(cols, set, exp)
cols$key <- paste(cols$set, cols$exp)
#cols$key <- factor(cols$key, levels = unique(cols$key))
blues <- colorRampPalette(c("dodgerblue4", "lightskyblue1"))
greys <- colorRampPalette(c("grey40", "grey70"))
fillPal <- c(greys(nrow(filter(cols, set == "Current"))),
              blues(nrow(filter(cols, set == "Bernie"))))
names(fillPal) <- cols$key
colorPal <- fillPal[c(floor(length(fillPal) / 4) + floor(length(fillPal) / 2),
                    floor(length(fillPal) / 4))]
names(colorPal) <- c("FALSE", "TRUE")



savings <- netTaxDifferences(dat, acsList)
savings <- savings %>%
  filter(payer == "Individual") %>%
  mutate(labely = eTaxBern,
         hjust = -.15,
         fillKey = ifelse(increase, 
                          "Current Healthcare Costs",
                          "Bernie Healthcare Tax"))


savings <- dat2 %>% 
  ungroup() %>% 
  filter(set == "Bernie") %>% 
  select(income, offset) %>%
  unique %>% 
  inner_join(savings)

ggplot(dat2, aes(x = offset, y = eTax, 
                 fill = paste(set, expenseGroup))) +
  stat_function(fun = dlnorm, args = denseFun$estimate,
                geom = "area", fill = "grey20") +
  geom_bar(position = "stack", stat = "identity",
           data = filter(dat2, set == "Bernie"), width = 0.4) +
  geom_bar(position = "stack", stat = "identity",
           data = filter(dat2, set == "Current"), width = 0.4) +
  geom_text(aes(y = labely, label = scales::percent(eTax), 
                hjust = pctLabelJust), fontface = "bold") +
  geom_label(aes(y = labely, label = lab, hjust = hjust, fill = fillKey),
            data = savings, size = 4.5) +
  scale_fill_manual("Expense", values = fillPal) +
  #scale_color_manual(values = colorPal, guide = F) +
  scale_x_reverse(breaks = seq_along(incomes), labels = centileLabeler) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(xlim = c(1.7, 6.5), ylim = c(0, .5)) +
  labs(x = "Income for Male Single Filer", 
       y = "Effective Rate for Tax & Healthcare Burden") +
  theme(legend.position = "bottom", text = element_text(size = 18))
