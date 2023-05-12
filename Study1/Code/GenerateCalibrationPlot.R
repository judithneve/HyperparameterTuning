library(dplyr)
library(purrr)
library(ggplot2)

HP.labs <- c('None',
             'mtry + min.node.size',
             'mtry + min.node.size + replace',
             'mtry + min.node.size + splitrule',
             'mtry + min.node.size + replace + splitrule',
             'mtry + min.node.size + sample.fraction',
             'mtry + min.node.size + sample.fraction + replace',
             'mtry + min.node.size + sample.fraction + splitrule',
             'mtry + min.node.size + sample.fraction + replace + splitrule')
HP.labs.lines <- c('None',
                   'mtry + min.node.size',
                   'mtry + min.node.size + replace',
                   'mtry + min.node.size + splitrule',
                   'mtry + min.node.size\n+ replace + splitrule',
                   'mtry + min.node.size\n+ sample.fraction',
                   'mtry + min.node.size\n+ sample.fraction + replace',
                   'mtry + min.node.size\n+ sample.fraction + splitrule',
                   'mtry + min.node.size + replace\n+ sample.fraction + splitrule')
names(HP.labs.lines) <- HP.labs

for (ef in c("ef1", "ef3", "ef5")) {
  dat <- list.files(path = "./Study1/Data/coords/", pattern = ef) %>%
    paste0("./Study1/Data/coords/", .) %>% 
    map(readRDS) %>% 
    bind_rows() %>% 
    mutate(hp_combination = ifelse(hp_combination == "", "None", hp_combination))
  
  plot <- dat %>% 
    mutate(hp_combination = factor(hp_combination, levels = names(HP.labs.lines), labels = HP.labs.lines)) %>% 
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
    facet_wrap(~ hp_combination,
               nrow = 3) +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      strip.text = element_text(size = 11)
    )
  
  plot_name <- paste0("Study1/Output/plot_", ef, ".pdf")
  ggsave(plot_name, plot, width = 8.5, height = 8.5, units = "in")
  
}
