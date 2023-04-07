library(dplyr)
library(ggplot2)

dat <- readRDS("Study2/Data/coords/study2_ef5.rds")

opt.labs <- c("Calibration slope",
              "Calibration intercept",
              "Brier score",
              "Logarithmic loss",
              "AUC",
              "Classification accuracy",
              "Cohen's Kappa",
              "Deviance")
names(opt.labs) <- c("CalSlope", "CalInt", "BrierScore", "LogLoss", "AUC", "Accuracy", "Kappa", "Deviance")
opt.labs <- opt.labs[c(8, 5, 4, 6, 7, 3, 2, 1)]

plot <- dat %>% 
  mutate(metric = factor(metric, levels = names(opt.labs))) %>% 
  ggplot() + 
  geom_line(aes(x = x, y = y, group = start_seed),
            stat = "smooth",
            method = stats::loess, 
            formula = y ~ x,
            se = F, 
            col = "#525252",
            linewidth = 0.25,
            alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.75) +
  xlab("Estimated Probability") +
  ylab("Observed Proportion") +
  theme_classic() +
  xlim(0, 1) +
  ylim(0, 1) +
  facet_wrap(~ metric,
             nrow = 3,
             labeller = labeller(metric = opt.labs)) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    strip.text = element_text(size = 11)
  )

plot_name <- paste0("Study2/Output/plot_EF_", 5, ".pdf")
ggsave(plot_name, plot, width = 8.5, height = 8.5, units = "in")
