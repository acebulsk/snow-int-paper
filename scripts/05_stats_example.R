### mix of one- and two-sided alternatives
warpbreaks.aov <- aov(breaks ~ tension,
                      data = warpbreaks)
### contrasts for `tension'
K <- rbind("L - M" = c( 1, -1, 0),
           "M - L" = c(-1, 1, 0),
           "L - H" = c( 1, 0, -1),
           "M - H" = c( 0, 1, -1),
           "H - L" = c(-1, 0, 1))

warpbreaks.mc <- glht(warpbreaks.aov,
                      linfct = mcp(tension = K),
                      alternative = "greater")

summary(warpbreaks.mc)

ggplot(warpbreaks, aes(tension, breaks)) +
  geom_boxplot() +
  facet_grid(~wool)
