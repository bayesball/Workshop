RE_plot <- function(erm){
  require(ggplot2)
  # using Tom Tango's bases score
  erm |> 
    mutate(Bases_Score = 1 * (substr(bases, 1, 1) == "1") +
                   2 * (substr(bases, 2, 2) == "1") +
                   4 * (substr(bases, 3, 3) == "1"),
           outs = as.character(outs_ct)) -> erm
  
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