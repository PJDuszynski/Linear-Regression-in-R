library(tidyverse)
library(tidyr)
library(rsample)
library(Metrics)
library(car)
library(GGally)
library(ggthemes)
library(leaps)
library(dplyr)
library(broom)

#convert Boston into a tibble
boston <- Boston %>% as_tibble()

#exploratory plot matrix

plot_matrix <- boston %>%
  select(-chas) %>%
  ggpairs()

plot_matrix

boston %>% summary()

set.seed(23)
split_dat <- boston %>% initial_split(prop = .75)
test <- testing(split_dat)
train <- training(split_dat)

# Run a full model on the testing data to check assumptions before selecting the best oen

full_model <- lm(medv ~., data = train)
full_assums <- full_model %>%  assumptions_checker()
full_model %>% glance() #r-qaured of .758, definitely something there.

#Check for multicollinearity
full_assums$vif
#significant multicollinearity likely between rad and vif. Do not choose a model with both those variables included

##Check normality of residuals
full_assums$qq_plot
#residuals are monotone, but the model is skewed to the right. A transformation may be useful

##Check outliers and heteroscedasticity 
full_assums$res_plot
full_assums$res_outliers
#285, 286, 287, 315 outliers. NO visible pattern in residuals

##Check leverage
full_assums$hat_plot
full_assums$high_hats
# 294, 309, 214, 370 high leverage

##Check influence
full_assums$cooks_plot
full_assums$high_cooks
##54  83 112 118 122 126 130 152 159 167 175 177 182 209 281 282 284 285 286 287 288 289 315 370 380 

full_assums$cooks_lev_plot
# Point # 285 (large outlier) and 294 (high leverage) are worrying

train %>% slice(285, 294)
##  observation 294 appears to be the highest crime district in the city and also in the upper quartile of taxation.
## because of it's extreme high leverage value, we will remove it from the test dataset, and quickly recheck the assumptions.

train_2 <- train %>% slice(-294)

full_model2 <- lm(medv ~., data = train_2)
full_model2 %>% glance()
full_assums2 <- full_model2 %>% assumptions_checker()
train_2 %>% slice(308)
## no reason to remove 308 from the model. 

## In sum, problems with multicollinearity and normality of residuals. Don't include tax and rad. Consdier box-cox transform. 


