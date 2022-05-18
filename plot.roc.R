plot.roc <- function(glm, lang = c("en", "pt"), line.color = "#DC634E", fill.color = "#E17C69"){
  # Required packages.
  if(!require(pacman)){install.packages("pacman")}
  p_load("tidyverse", "pROC", "scales")
  
  # Get a ROC object.
  df = glm$data             # respective data.frame
  r.glm = glm$y             # actual outcome responses
  p.glm = glm$fitted.values # predicted outcome values
  roc.obj <- roc(formula = r.glm ~ p.glm, data = df) # ROC object
  
  # Compute the CI of sensitivities at given specificities
  ci.obj <- ci.se(roc = roc.obj, specificities = seq(0, 1, length.out = nrow(df)))
  ci.df <- data.frame(sp = as.numeric(rownames(ci.obj)), se.lower = ci.obj[, 1], se.upper = ci.obj[, 3])
  
  # Plot it
  g <- 
    ggroc(data = roc.obj, lwd = 0.7, color = line.color, legacy.axes = TRUE) +
    geom_ribbon(data = ci.df, mapping = aes(x = 1 - sp, ymin = se.lower, ymax = se.upper), fill = fill.color, color = "transparent", alpha = 0.2) +
    geom_abline(linetype = "dashed", color = "darkgray", alpha = 0.7) +
    scale_x_continuous(labels = scales::percent_format(suffix = "", accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(suffix = "", accuracy = 1)) +
    theme_classic() +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))
  if(lang == "en"){
    return(g + labs(x = "\n1 - Specificity (%)", y = "Sensitivity (%)\n"))
  } else{
    return(g + labs(x = "\n1 - Especificidade (%)", y = "Sensibilidade (%)\n"))
  }
}