library(dplyr)
library(ggplot2)

alg.labs <- c("grid", "random", "mbo")
names(alg.labs) <- c("Grid search", "Random search", "Model-based optimisation")
alg.labs <- alg.labs[c(2, 1, 3)]

for (ef in c("ef1", "ef3", "ef5")) {
  dat <- readRDS(paste0("Study3/Data/coords/study3_", ef, ".rds"))
  
  plot <- dat %>% 
    mutate(algorithm = factor(algorithm, levels = alg.labs, labels = names(alg.labs))) %>% 
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
    facet_wrap(~ algorithm,
               nrow = 2) +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      strip.text = element_text(size = 11)
    )
  
  plot_name <- paste0("Study3/Output/plot_", ef, ".pdf")
  ggsave(plot_name, plot, width = 8.5, height = 8.5, units = "in")
}
