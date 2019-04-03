
##Using normal multiple regression, no box-cox 

subsum <- regsubsets(medv ~., data = train_2, nvmax = 13, nbest = 5) %>% 
  summary()

model_list <- subsum$outmat %>% 
              as_tibble() %>% 
              mutate(cp = subsum$cp, adjr2 = subsum$adjr2, bic = subsum$bic) %>% 
              arrange(desc(adjr2))

model_list %>% slice(28)
#the model with chas, nox, rm, dis, ptratio, black, and lstat has the highest adjr2 without including collinear variables. 

## Now we will do cross validation on the model. 


cv_dat <- vfold_cv(train_2, v = 5) %>% 
            mutate(train = map(splits, ~training(.x)),
                   validate = map(splits, ~testing(.x)))

cv_vals <- cv_dat %>% 
           mutate(model = map(train, ~lm(medv ~ chas + nox + rm + dis + ptratio + black + lstat, data = .x))) %>% 
           mutate(actual = map(train, ~.x$medv)) %>% 
           mutate(predictions = map2(model, validate, ~predict(.x, .y)))

cv_rmse <- cv_vals %>% mutate(rmse = map2_dbl(actual, predictions, ~rmse(.x, .y)))

cv_rmse$rmse %>% mean()

## RMSE is 12.24

#@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@#

##Using Box-Cox Transformation 

precox <- lm(medv ~ chas + nox + rm + dis + ptratio + black + lstat, data = train_2)
MASS::boxcox(precox) %>% as_tibble() %>% slice(which.max(y)) 
#.222 maximizes the log-likelihood. 

bct <- function(x, lambda) {      #boxcox forward
  (x^lambda -1) / lambda
}

bci <- function(x, lambda) {      #boxcox inverse
  (x*lambda + 1)^(1/lambda) 
}

coxsum <- regsubsets(bct(medv, .222) ~., data = train_2, nvmax = 13, nbest = 5) %>% 
  summary()

cox_list <- subsum$outmat %>% 
  as_tibble() %>% 
  mutate(cp = subsum$cp, adjr2 = subsum$adjr2, bic = subsum$bic) %>% 
  arrange(desc(adjr2))

cox_list %>% slice(26)
#crim, nox, rm, dis, rad, tax, ptratio, black, lstat

cox_mod <- lm(bct(medv, .222) ~ crim + nox + rm + dis + rad + tax + ptratio + black + lstat, data = train_2)
cox_assum <- assumptions_checker(cox_mod)
##assumptions look about the same. Do cv. 

cv_valscox <- cv_dat %>% 
  mutate(model = map(train, ~lm(bct(medv, .222) ~ crim + nox + rm + dis + rad + tax + ptratio + black + lstat, data = .x))) %>% 
  mutate(actual = map(train, ~.x$medv)) %>% 
  mutate(predictions = map2(model, validate, ~bci(predict(.x, .y), .222)))

cv_rmsecox <- cv_vals %>% mutate(rmse = map2_dbl(actual, predictions, ~rmse(.x, .y)))

cv_rmsecox$rmse %>% mean()
##boxcox transformation preforms worse with RMSE of 20.7

#@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@#
#@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@#

#Now let's look at the testing data (already had cox-mod but including for clairty)

regular <- lm(medv ~ chas + nox + rm + dis + ptratio + black + lstat, data = train_2)
cox <- lm(bct(medv, .222) ~ crim + nox + rm + dis + rad + tax + ptratio + black + lstat, data = train_2)

reg_preds <- predict(regular, test) 
cox_preds <- predict(cox, test ) %>% bci(.222)

rmse(cox_preds, test$medv) #5.363
rmse(reg_preds, test$medv) #5.279

##simple linear regression model performs slightly better

glance(regular)
tidy(regular)

glance(cox)
tidy(cox)

##even though cox has a higher adjusted r-suqared , the non-boxcox model performs better and the coefficients are easier
# to interpret 

##why is the cv RMSE high than the test validation RMSE??? 