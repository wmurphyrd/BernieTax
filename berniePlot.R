shadowOffsetX <- 1.02
shadowOffsetY <- -.0025
shadowSize <- 1.9
shadowColor <- "grey30"
ggplot(filter(datDiff, payer == "Individual"), aes(x = income)) +
#   geom_segment(aes(xend = income, yend = 0.52, y = .2), percentiles,
#                linetype = 2, color = "grey40") +
  geom_ribbon(aes(ymax = iTop, ymin = iBottom, fill = "Increase")) +
  geom_ribbon(aes(ymax = dTop, ymin = dBottom, fill = "Savings")) +
  geom_line(aes(y = eTaxBern + shadowOffsetY , x = income * shadowOffsetX), 
            size = shadowSize, color = shadowColor) + 
  geom_line(aes(y = eTaxCur + shadowOffsetY, x = income * shadowOffsetX), 
            size = shadowSize, color = shadowColor) + 
  geom_line(aes(y = eTaxBern, color = "Bernie"), size = 1.9) +
  geom_line(aes(y = eTaxCur, color = "Current"), size = 1.9) +
  scale_x_log10(breaks = percentiles$income, labels = percentiles$xlabs) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("Tax Plan", values = c("#287CBF", "#EB514F")) +
  scale_fill_manual("Change under Bernie's Plans", 
                    limits = c("Savings", "Increase"),
                    values = c("#7db6e3", "#f18b89")) +
  guides(color = guide_legend(order = 1)) +
  theme(axis.line = element_blank(), legend.position = "bottom",
        text = element_text(size = 18),
        panel.grid.major.x = 
          element_line(colour = "grey40", size = 1, linetype = 2),
        panel.background = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y = "Effective Tax Rate", x = "")
