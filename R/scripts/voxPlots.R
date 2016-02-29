# Finds marginal rate and ignores "extra" & "deduct"
# Adjusts rates to find true tax rates based on effective income after
# accounting for employer paid compensation that is in addition to income
# These figures are flawed and used for example purposes in an article
getMarginalRateVox <- function(bracketsList, income) {
  bracketsList <- bracketsList[!grepl("Healthcare", names(bracketsList))]
  rates <- sapply(bracketsList, function(brackets){
    brackets$rate[which(brackets$bottom < income & brackets$cap >= income)]
  })
  sum(rates / (1 + sum(rates[grepl("Employer",names(rates))])))
}

#####
# Real tax rate explanation


library(ggplot2); library(dplyr)

inc <- 10000
inc.rate <- .1
prl.rate <- .1

labFunc <- function(x) {
  ef.inc <- inc + inc * prl.rate
  scales::percent(1 - (x / ef.inc))
}
png("img/png/realrates_raw.png", w = 800, h = 200)
data.frame(rate = c(c(1 - inc.rate, inc.rate, prl.rate)),
           cat = factor(1:3, labels = c("Take-Home Pay", "Income Tax", 
                                        "Payroll Tax")),
           lab.x = c(1, 1.23, .78)) %>%
  mutate(amount = rate * inc, 
         lbl = paste(cat, "\n", "Amount:",
                     scales::dollar(amount),
                     "\nPortion of Income:",
                     scales::percent(rate)),
         lbl.pos = cumsum(amount) - amount / 2) %>%
  ggplot(aes(x = 1, y = amount, fill = cat)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(y = lbl.pos, label = lbl, x = lab.x),
            fontface = "bold", size = 4) + 
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 11000, by = 1000), 
                  labels = labFunc) +
  scale_fill_brewer(palette = "BuGn", direction = -1) +
  labs(x = "", y = "Real Tax Rate") +
  theme_classic() + 
  theme(text = element_text(size = 18), 
        legend.position = "none", axis.text.y = element_blank(),
        axis.line = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = grid::unit(c(4,0,0,0), "lines"))
dev.off()


##### 
# Math error table

library(xtable); library(scales)
voxIncomes <- c(0, 18550, 75300, 118500, 151900, 231450, 
                250000, 413350, 466950, 500000, 2000000, 
                10000000)
bracketsList <- getBrackets()
cur <- sapply(voxIncomes + 1, getMarginalRateVox, 
              bracketsList = c(bracketsList$currentIndBrackets,
                               bracketsList$currentEmpBrackets))
bern <- sapply(voxIncomes + 1, getMarginalRateVox, 
              bracketsList = c(bracketsList$bernieIndBrackets,
                               bracketsList$bernieEmpBrackets))
voxTable <- data.frame(income = voxIncomes,
                       set = c(rep("Current", length(cur)), 
                               rep("Bernie", length(bern))),
                       margRate = c(cur, bern),
                       voxRate  = c(.253, .303, .403, .279, .309, .359, .368, 
                                    .388, .434, .434, .434, .434,
                                    .341, .391, .491, .367, .397, .447, .620, 
                                    .620, .620, .68, .73, .77)) %>%
  mutate(voxError = voxRate - margRate) %>% 
  arrange(income, desc(set)) %>%
  mutate_each(funs(percent), margRate, voxRate, voxError) %>%
  mutate(income = dollar(income))
voxTable$income[seq(2, nrow(voxTable), by = 2)]  <- ""
names(voxTable) <- c("Taxable Income Above", "Tax Plan", "Real Marginal Rate",
                     "Matthews's Rate", "Size of Error")


tab <- xtable(voxTable)
print(tab, file = "voxTable.txt", type = "html", include.rownames = F)


#########
# Top Marginal Plot

library(dplyr); library(tidyr); library(ggplot2); library(readxl)
source("R/functions/bernieTaxBrackets.R")
source("R/functions/bernietaxFunctions.R")

nKids <- 0
filingStatus <- "Married/Joint"
sex <- "M"

incomes <- 10^seq(4.6, 10, by = .05)

dat <- taxesByIncomes(incomes, filingStatus, nKids, sex, employer = "pool")

#total effective tax rates
datSum <- dat %>% group_by(set, income, effectiveIncome, agi) %>% 
  summarize(tTax = sum(amount)) %>% ungroup %>% 
  mutate(eTax = tTax / effectiveIncome)


trendColors <- c("#2d8cd8", "#48476f")

crossover <- inner_join(filter(datSum, set == "Bernie"),
                        filter(datSum, set == "Current"),
                        by = "income") %>%
  mutate(adiff = abs(eTax.x - eTax.y)) %>%
  arrange(adiff) %>%
  filter(row_number() ==1) %>%
  magrittr::extract2("eTax.y")

ybrks <- c(min(datSum$eTax), max(datSum$eTax),
           max(filter(datSum, set == "Current", income > 2500000)$eTax),
           max(filter(datSum, income < 250000)$eTax), crossover, .77)

png("img/png/topmarginal_raw.png", w = 800, h = 400)
ggplot(datSum, aes(x = income, y = eTax, color = set)) +
  geom_line(size = 2.75) +
  scale_x_log10(breaks = c(50000, 250000, 10^(6:10)),
                labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent, breaks = ybrks) +
  scale_color_manual("Tax Plan", values = trendColors) +
  coord_cartesian(ylim = ybrks[c(1, length(ybrks))]) +
  theme(axis.line = element_blank(), 
        legend.position = "none", 
        text = element_text(size = 18),
        axis.title.y = element_text(margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = 
          element_line(colour = "grey80", size = 1, linetype = 2),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = grid::unit(c(2, 2, 0.1, 0.1), "lines")) +
  labs(y = "Effective Rate", 
       x = "Income")  
dev.off()
