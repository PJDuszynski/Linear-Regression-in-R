library(glmnet)

##Ridge regression using same test and train split

set.seed(23)
split_dat <- boston %>% initial_split(prop = .75)
test <- testing(split_dat)
train <- training(split_dat)

x <- model.matrix(medv ~., train)[,-1] #glmnet requires the data to be input as matrix/vector form
y <- train %>% pull(medv)

testx <- model.matrix(medv~., test)[,-1]
testy <- test %>% pull(medv)

#alpha = 0 specifies ridge regression model
ridge_mod <- cv.glmnet(x, y, alpha = 0)

ridge_mod %>% plot()

ridge_mod$lambda.min ## this is the minimum lambda value
which(ridge_mod$lambda == ridge_mod$lambda.min) ##99 is the best model

tidy(ridge_mod) %>% slice(99)
glance(ridge_mod)
coef(ridge_mod)
#largest coefs are nox, chas, rm, ptratio

ridge_preds <- predict(ridge_mod, s = .711, newx = testx )

rmse(ridge_preds, testy)
##rmse 5.41

#@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@_@#
## LASSSO regression

lasso_model <- cv.glmnet(x, y, alpha = 1) #alpha 1 for LASSO
plot(lasso_model)

best_lamb <- lasso_model$lambda.min
which(lasso_model$lambda == lasso_model$lambda.min)
glance(lasso_model)
coef(lasso_model)

lass_preds <- predict(lasso_model, s = best_lamb, newx = testx)

rmse(lass_preds, testy)
##lasso has rmse of 5.57 age, rad, tax are all sent to zero
## nox, chas, rm, ptratio, and dis are the largest