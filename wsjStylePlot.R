dat2 <- mutate(dat, grp = paste(set, expense))
ggplot(filter(dat2, payer == "Individual", 
              aes(x = income, y = amount / effectiveIncome, fill = expense,
                  group = grp))