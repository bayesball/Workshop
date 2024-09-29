RE_plot <- function(erm){
  require(ggplot2)
  erm$bases <- factor(erm$bases,
                      levels = c("000", "100", "010", "001",
                                 "110", "101", "011", "111"))
  erm$Bases_Score <- as.numeric(erm$bases) - 1
  erm$outs <- as.character(erm$outs_ct)
  
  ggplot(erm, aes(Bases_Score, mean_run_value,
                  color = outs)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm",
                formula = "y ~ x",
                se = FALSE) +
    ggtitle(paste("Expected Runs")) +
    theme(text = element_text(size = 18),
          plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    xlab("Bases Score") +
    ylab("Runs Expectancy")
}