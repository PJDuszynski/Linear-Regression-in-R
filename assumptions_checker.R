assumptions_checker <- function(x, crit_sd = 3) {
  require(tidyverse)
  require(car)
  require(GGally)
  require(ggthemes)
  
  if (class(x) != "lm") stop("Object Must be class lm")
  
  
  #VIFs for multicollinearity
  vifs <- vif(x)
  
  #Normality of Residuals
  res_students <- x %>% rstudent() %>%
    imap(~tibble(obs = .y, value = .x)) %>% 
    do.call(what = rbind)
  
  qq_plot <- res_students %>% ggplot(aes(sample = value)) + 
    geom_qq() +
    geom_qq_line()                                        ##change the theme?
  
  ##outliers and heteroscedasticity
  
  res_plot <- res_students %>% 
    ggplot(aes(x = obs, y = value, label = obs)) + 
      geom_point(position = "jitter", alpha = 0.5) + 
      geom_abline(slope = 0, intercept = 3, col = "red") +
      geom_abline(slope = 0, intercept = -3, col = "red") +
      geom_text(position = position_dodge(width = 0.1), size = 3) +
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
  
  res_outliers <- which( res_students %>% pull(value) %>% abs() > crit_sd)
  
  ## Checking Leverage and hat values
  hat_vals <- x %>% hatvalues() %>%
    imap(~tibble(obs = as.integer(.y), value = .x)) %>% 
    do.call(what = rbind)
  
  high_hats <- which( hat_vals %>% pull(value) > 2 * ncol(x$model) / nrow(x$model))
  
  hat_plot <- hat_vals %>%
    ggplot(aes(x = obs, y = value, label = obs)) + 
      geom_point() + 
      geom_abline(slope = 0, intercept = 2 * ncol(x$model) / nrow(x$model), col = "red") +
      geom_text(position = position_dodge(width = .2))  ## label the value of the line
  
  ##influential points
  
  cooks_dist <- cooks.distance(x)
  df_fits <- dffits(x)
  
  plot(x, 4) 
  cooks_plot <- recordPlot()
  #gotta redo these manually
  plot(x, 5)
  cooks_lev_plot <- recordPlot()
  
  high_cooks = which(cooks_dist > 4 / nrow(x$model))
  
  asums <- list("vif" = vifs, 
                 "res_students" = res_students,
                 "qq_plot" = qq_plot,
                 "res_plot" = res_plot,
                 "res_outliers" = res_outliers,
                 "hat_vals" = hat_vals,
                 "high_hats" = high_hats,
                 "hat_plot" = hat_plot,
                 "cooks_dist" = cooks_dist,
                 "df_fits" = df_fits,
                 "cooks_plot" = cooks_plot,
                 "cooks_lev_plot" = cooks_lev_plot,
                 "high_cooks" = high_cooks
                 )
  asums
}

## Assumptions checker doesnt work with the  cooks distance plots,
## put can just use the commands plot(lm, 4) and plot(lm, 5)
## R base graphics is Suboptimal
