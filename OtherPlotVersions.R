
ggplot(filter(datDiff, payer == "Individual"), aes(x = income)) +
  geom_segment(aes(xend = income, yend = 0.51, y = .2), percentiles,
               linetype = 2, color = "grey40") +
  geom_ribbon(aes(ymax = iTop, ymin = iBottom, fill = "Increase")) +
  geom_ribbon(aes(ymax = dTop, ymin = dBottom, fill = "Decrease")) +
#   geom_line(aes(y = eTax - .0025, x = income * 1.03, group = set), datSum, 
#             size = 2, color = "grey20") + 
  geom_line(aes(y = eTaxBern, color = "Bernie"), size = 1.9) +
  geom_line(aes(y = eTaxCur, color = "Current"), size = 1.9) +
  scale_x_log10(breaks = percentiles$income, labels = percentiles$xlabs) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Tax Plan", values = c("#287CBF", "#EB514F")) +
  scale_fill_manual("Change under Bernie's Plans", 
                    values = c("#7db6e3", "#f18b89")) +
  theme_classic() +
  theme(axis.line = element_blank(), leged.position = "bottom") 

  #stat_smooth(aes(y = centile), acs, se = F, method = loess) +
  #coord_cartesian(xlim = c(min(incomes), max(incomes)))


# distrPlot <- ggplot(acs, aes(x = Dollars, y = Number)) +
#   stat_smooth(se = F, method = loess) +
#   coord_cartesian(xlim = c(min(incomes), max(incomes))) +
#   theme_classic() +
#   theme(axis.line = element_blank())
# 
# lay <- rbind(1, 1, 2)
# grid.arrange(taxPlots, distrPlot, layout_matrix = lay)

ggplot(datDiff, aes(x = income)) +
  geom_ribbon(aes(ymax = iTop, ymin = iBottom, fill = "Increase")) +
  geom_ribbon(aes(ymax = dTop, ymin = dBottom, fill = "Decrease")) +
  geom_line(aes(y = eTax - .0025, x = income * 1.03, group = set), datSum, 
            size = 2, color = "grey20") + 
  geom_line(aes(y = eTax, color = set), datSum, size = 1.9) +
  facet_grid(payer ~ ., scales = "free") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Tax Plan", values = c("#287CBF", "#EB514F")) +
  scale_fill_manual("Change under Bernie's Plans", 
                    values = c("#7db6e3", "#f18b89")) +
  theme_classic() +
  theme(axis.line = element_blank()) +
  stat_smooth(aes(y = centile), acs, se = F, method = loess) +
  coord_cartesian(xlim = c(min(incomes), max(incomes)))
