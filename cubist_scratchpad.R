library(Cubist)
library(mlbench)

# https://cran.r-project.org/web/packages/Cubist/vignettes/cubist.html
# https://topepo.github.io/Cubist/reference/cubist.default.html
# https://www.rulequest.com/cubist-win.html#:~:text=Rule%2Dbased%20models,-When%20Cubist%20is&text=Some%20attribute%20values%20might%20be,the%20most%20frequent%20attribute%20value.
# https://medium.com/@ODSC/balancing-interpretability-and-predictive-power-with-cubist-models-in-r-858d2c936b79

# get boston housing data
data(BostonHousing)
BostonHousing
BostonHousing %>% glimpse()
BostonHousing$chas <- as.numeric(BostonHousing$chas) - 1

# get training data
set.seed(1)
inTrain <- sample(1:nrow(BostonHousing), floor(.8*nrow(BostonHousing)))

train_pred <- BostonHousing[ inTrain, -14]
test_pred  <- BostonHousing[-inTrain, -14]

train_resp <- BostonHousing$medv[ inTrain]
test_resp  <- BostonHousing$medv[-inTrain]

# create model on training data
model_tree <- cubist(x = train_pred, y = train_resp)
model_tree
summary(model_tree)

# inspect the contents of cubist class
class(model_tree)
str(model_tree)

# predict on test data
model_tree_pred <- predict(model_tree, test_pred)

## Test set RMSE
sqrt(mean((model_tree_pred - test_resp)^2))

## Test set R^2
cor(model_tree_pred, test_resp)^2






