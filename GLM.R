library(broom)
library(sigr)

# Call glance() on unemployment_model to see the details in a tidier form
glance(unemployment_model)

# Call wrapFTest() on unemployment_model to see the most relevant details
wrapFTest(unemployment_model)

# Load the package WVPlots
library(WVPlots)

# Plot the Gain Curve
GainCurvePlot(unemployment, "predictions", "female_unemployment", "Unemployment model")


# mpg is in the workspace
summary(mpg)
dim(mpg)

# Use nrow to get the number of rows in mpg (N) and print it
(N <- nrow(mpg))

# Calculate how many rows 75% of N should be and print it
# Hint: use round() to get an integer
(target <- round(N * 0.75))

# Create the vector of N uniform random variables: gp
gp <- runif(N)

# Use gp to create the training set: mpg_train (75% of data) and mpg_test (25% of data)
mpg_train <- mpg[gp < 0.75, ]
mpg_test <- mpg[gp >= 0.75, ]

# mpg_train is in the workspace
summary(mpg_train)

# create a formula to express cty as a function of hwy: fmla and print it.
(fmla <- cty ~ hwy)

# Now use lm() to build a model mpg_model from mpg_train that predicts cty from hwy 
mpg_model <- lm(fmla, data = mpg_train)

# Use summary() to examine the model
summary(mpg_model)


# Examine the objects in the workspace
ls.str()

# predict cty from hwy for the training set
mpg_train$pred <- predict(mpg_model)

# predict cty from hwy for the test set
mpg_test$pred <- predict(mpg_model, newdata = mpg_test)

# Evaluate the rmse on both training and test data and print them
(rmse_train <- rmse(mpg_train$pred, mpg_train$cty))
(rmse_test <- rmse(mpg_test$pred, mpg_test$cty))


# Evaluate the r-squared on both training and test data.and print them
(rsq_train <- r_squared(mpg_train$pred, mpg_train$cty))
(rsq_test <- r_squared(mpg_test$pred, mpg_test$cty))


# Load the package vtreat
library(vtreat)

# mpg is in the workspace
summary(mpg)

# Get the number of rows in mpg
nRows <- nrow(mpg)

# Implement the 3-fold cross-fold plan with vtreat
splitPlan <- kWayCrossValidation(nRows, 3, NULL, NULL)

# Examine the split plan
str(splitPlan)


# Run the 3-fold cross validation plan from splitPlan
k <- 3 # Number of folds
mpg$pred.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(cty ~ hwy, data = mpg[split$train, ])
  mpg$pred.cv[split$app] <- predict(model, newdata = mpg[split$app, ])
}

# Predict from a full model
mpg$pred <- predict(lm(cty ~ hwy, data = mpg))


# Get the rmse of the full model's predictions
rmse(mpg$pred, mpg$cty)

# Get the rmse of the cross-validation predictions
rmse(mpg$pred.cv, mpg$cty)

# Use unique() to see how many possible values Time takes
unique(flowers$Time)

# alcohol is in the workspace
summary(alcohol)

# Create the formula with main effects only
(fmla_add <- Metabol ~ Gastric + Sex)

# Create the formula with interactions
(fmla_interaction <- Metabol ~  Gastric + Gastric:Sex)

# Fit the main effects only model
model_add <- lm(fmla_add, data = alcohol)

# Fit the interaction model
model_interaction <- lm(fmla_interaction, data = alcohol)

# Call summary on both models and compare
summary(model_add)
summary(model_interaction)

# Both the formulae are in the workspace
fmla_add
fmla_interaction

# Create the splitting plan for 3-fold cross validation
set.seed(34245)  # set the seed for reproducibility
splitPlan <- kWayCrossValidation(nrow(alcohol), 3, NULL, NULL)

# Sample code: Get cross-val predictions for main-effects only model
alcohol$pred_add <- 0  # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_add <- lm(fmla_add, data = alcohol[split$train, ])
  alcohol$pred_add[split$app] <- predict(model_add, newdata = alcohol[split$app, ])
}


# Get the cross-val predictions for the model with interactions
alcohol$pred_interaction <- 0 # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_interaction <- lm(fmla_interaction, data = alcohol[split$train, ])
  alcohol$pred_interaction[split$app] <- predict(model_interaction, newdata = alcohol[split$app, ])
}

# Get RMSE
alcohol %>% 
  gather(key = modeltype, value = pred, pred_add, pred_interaction) %>%
  mutate(residuals = Metabol - pred) %>%
  group_by(modeltype) %>%
  
  
# fdata is in the workspace
summary(fdata)

# Examine the data: generate the summaries for the groups large and small:
fdata %>% 
  group_by(label) %>%        # group by small/large purchases
  summarize(min  = min(y),   # min of y
            mean = mean(y),  # mean of y
            max  = max(y))   # max of y

# Fill in the blanks to add error columns
fdata2 <- fdata %>% 
  group_by(label) %>%               # group by label
  mutate(residual = pred - y,     # Residual
         relerr   = residual/y)   # Relative error

# Compare the rmse and rmse.rel of the large and small groups:
fdata2 %>% 
  group_by(label) %>% 
  summarize(rmse     = sqrt(mean(residual^2)),  # RMSE
            rmse.rel = sqrt(mean(relerr^2)))    # Root mean squared relative error

# Plot the predictions for both groups of purchases
ggplot(fdata2, aes(x = pred, y = y, color = label)) + 
  geom_point() + 
  geom_abline() + 
  facet_wrap(~ label, ncol = 1, scales = "free") + 
  ggtitle("Outcome vs prediction")




# Poisson or quasipoisson

# One of the assumptions of Poisson regression to predict counts is that the event 
# you are counting is Poisson distributed: the average count per unit time is the same 
# as the variance of the count. In practice, "the same" means that the mean and the variance 
# should be of a similar order of magnitude.
# 
# When the variance is much larger than the mean, the Poisson assumption doesn't apply, and 
# one solution is to use quasipoisson regression, which does not assume that variance=meanvariance=mean.


# bikesJuly is in the workspace
str(bikesJuly)

# The outcome column
outcome 

# The inputs to use
vars 

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

# Calculate the mean and variance of the outcome
(mean_bikes <- mean(bikesJuly$cnt))
(var_bikes <- var(bikesJuly$cnt))

# Fit the model
bike_model <- glm(fmla, data = bikesJuly, family = quasipoisson)

# Call glance
(perf <- glance(bike_model))

# Calculate pseudo-R-squared
(pseudoR2 <- 1 - perf$deviance/perf$null.deviance)


# bikesAugust is in the workspace
str(bikesAugust)

# bike_model is in the workspace
summary(bike_model)

# Make predictions on August data
bikesAugust$pred  <- ___

# Calculate the RMSE
bikesAugust %>% 
  mutate(residual = ___) %>%
  summarize(rmse  = ___)

# Plot predictions vs cnt (pred on x-axis)
ggplot(bikesAugust, aes(x = ___, y = ___)) +
  geom_point() + 
  geom_abline(color = "darkblue")


# gaussian (default): "regular" regression
# binomial: probabilities
# poisson/quasipoisson: counts



# Load the package mgcv
library(mgcv)

# Create the formula 
(fmla.gam <- weight ~ s(Time) )

# Fit the GAM Model
model.gam <- gam(fmla.gam, data = soybean_train, family = gaussian)

# Call summary() on model.lin and look for R-squared
summary(model.lin)

# Call summary() on model.gam and look for R-squared
summary(model.gam)

# Call plot() on model.gam
plot(model.gam)


# soybean_test is in the workspace
summary(soybean_test)

# Get predictions from linear model
soybean_test$pred.lin <- predict(model.lin, newdata = soybean_test)

# Get predictions from gam model
soybean_test$pred.gam <- as.numeric(predict(model.gam, newdata = soybean_test))

# Gather the predictions into a "long" dataset
soybean_long <- soybean_test %>%
  gather(key = modeltype, value = pred, pred.lin, pred.gam)

# Calculate the rmse
soybean_long %>%
  mutate(residual = weight - pred) %>%     # residuals
  group_by(modeltype) %>%                  # group by modeltype
  summarize(rmse = sqrt(mean(residual^2))) # calculate the RMSE

# Compare the predictions against actual weights on the test data
soybean_long %>%
  ggplot(aes(x = Time)) +                          # the column for the x axis
  geom_point(aes(y = weight)) +                    # the y-column for the scatterplot
  geom_point(aes(y = pred, color = modeltype)) +   # the y-column for the point-and-line plot
  geom_line(aes(y = pred, color = modeltype, linetype = modeltype)) + # the y-column for the point-and-line plot
  scale_color_brewer(palette = "Dark2")





# bikesJuly is in the workspace
str(bikesJuly)

# Random seed to reproduce results
seed

# the outcome column
(outcome <- "cnt")

# The input variables
(vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"))

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

# Load the package ranger
library(ranger)

# Fit and print the random forest model.
(bike_model_rf <- ranger(fmla, 
                         bikesJuly, 
                         num.trees = 500, 
                         respect.unordered.factors = "order", 
                         seed = seed))


# bikesAugust is in the workspace
str(bikesAugust)

# bike_model_rf is in the workspace
bike_model_rf

# Make predictions on the August data
bikesAugust$pred <- predict(bike_model_rf, bikesAugust)$predictions

# Calculate the RMSE of the predictions
bikesAugust %>% 
  mutate(residual = cnt - pred)  %>%        # calculate the residual
  summarize(rmse  = sqrt(mean(residual^2))) # calculate rmse

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(bikesAugust, aes(x = pred, y = cnt)) + 
  geom_point() + 
  geom_abline()


# Print quasipoisson_plot
quasipoisson_plot

# Plot predictions and cnt by date/time
bikesAugust %>% 
  mutate(instant = (instant - min(instant))/24) %>%  # set start to 0, convert unit to days
  gather(key = valuetype, value = value, cnt, pred) %>%
  filter(instant < 14) %>% # first two weeks
  ggplot(aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted August bike rentals, Random Forest plot")




# dframe is in the workspace
dframe

# Create a vector of variable names
(vars <- c("color", "size"))

# Load the package vtreat
library(vtreat)

# Create the treatment plan
treatplan <- designTreatmentsZ(dframe, vars)

# Examine the scoreFrame
(scoreFrame <- treatplan %>%
    use_series(scoreFrame) %>%
    select(varName, origName, code))

# We only want the rows with codes "clean" or "lev"
(newvars <- scoreFrame %>%
    filter(code %in% c("clean", "lev")) %>%
    use_series(varName))

# Create the treated training data
(dframe.treat <- prepare(treatplan, dframe, varRestriction = newvars))



# treatplan is in the workspace
summary(treatplan)

# newvars is in the workspace
newvars

# Print dframe and testframe
dframe
testframe

# Use prepare() to one-hot-encode testframe
(testframe.treat <- prepare(treatplan, testframe, varRestriction = newvars))


# The July data is in the workspace
ls()

# Load the package xgboost
library(xgboost)

# Run xgb.cv
cv <- xgb.cv(data = as.matrix(bikesJuly.treat), 
             label = bikesJuly$cnt,
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0   # silent
)

# Get the evaluation log
elog <- cv$evaluation_log

# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))    # find the index of min(test_rmse_mean)



# Examine the workspace
ls()

# The number of trees to use, as determined by xgb.cv
ntrees

# Run xgboost
bike_model_xgb <- xgboost(data = as.matrix(bikesJuly.treat), # training data as matrix
                          label = bikesJuly$cnt,  # column of outcomes
                          nrounds = ntrees,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
)

# Make predictions
bikesAugust$pred <- predict(bike_model_xgb, as.matrix(bikesAugust.treat))

# Plot predictions vs actual bike rental count
ggplot(bikesAugust, aes(x = pred, y = cnt)) + 
  geom_point() + 
  geom_abline()

# bikesAugust is in the workspace
str(bikesAugust)

# Calculate RMSE
bikesAugust %>%
  mutate(residuals = cnt - pred) %>%
  summarize(rmse = sqrt(mean(residuals^2)))

# Print quasipoisson_plot
quasipoisson_plot

# Print randomforest_plot
randomforest_plot

# Plot predictions and actual bike rentals as a function of time (days)
bikesAugust %>% 
  mutate(instant = (instant - min(instant))/24) %>%  # set start to 0, convert unit to days
  gather(key = valuetype, value = value, cnt, pred) %>%
  filter(instant < 14) %>% # first two weeks
  ggplot(aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted August bike rentals, Gradient Boosting model")
