setwd("C:/Users/bruno/OneDrive/Desktop/rstudy/R-udemy/r_dataScience_bootcamp/sec7_model_selection")
getwd()

library(stringr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(rmarkdown)

# BIAS VARIANCE TRADEOFF --------------------------------------------------
data <- read_xlsx("machine1.xlsx")
data


# EXAMPLE OF HIGH BIAS MODEL ----------------------------------------------

# Visualizing
ggplot(data = data, aes(x = Rotation, y = Production)) +
  geom_point()

# Fitting a Linear Regression
m1 <- lm(data = data, Production ~ Rotation)
summary(m1)
par(mfrow=c(2,2))
plot(m1)

pred <- predict(m1)
pred

ggplot(data = data, aes(x = Rotation, y = Production)) +
  geom_point() +
  geom_line(aes(x = Rotation, y = pred), col = "red", size = 1)

# The Linear Model has high Bias referent to the data. Basically,
# our assumption of a linear relationship is not appropriate for 
# describing the data. It's clear from the plot that some kind of
# non-linear relationship exist in the data. It is even possible
# that a hidden factor could be a predictor in this data.

# Creating a Model Eval. Data
model_eval <- data.frame(data, pred) %>% 
  select(c(Production, pred))
model_eval

model_eval <- model_eval %>% mutate(error = abs(Production - pred))
model_eval

# Error plot
ggplot(data = model_eval, aes(x = Production, y = pred, col = error)) +
  theme_classic() +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red")


# SIMULATED DATA FOR COMPARISON -------------------------------------------

# Let's do all of the steps above on a simulated dataset in which we will
# enforce a true linear relationship

# Notice how y = x + noise, such that the noise is Gaussian
x = rnorm(n = 100, mean = 0, sd = 1)
y = x + rnorm(100, mean = 0, sd = 0.25)

sim_data <- data.frame(y, x)

ggplot(data = sim_data, aes(x = x, y = y))+
  geom_point()

m_sim <- lm(data = sim_data, y ~ x)
summary(m_sim)
par(mfrow=c(2,2))
plot(m_sim)

pred_sim <- predict(m_sim)
sim_data <- data.frame(sim_data, pred_sim)
sim_data <- sim_data %>% mutate(error = abs(y - pred_sim))

sim_data

model_eval_sim <- sim_data %>% 
  select(c(y, pred_sim, error))

model_eval_sim

# Finally, the plot:
# Notice how the errors are evenly distributed
ggplot(data = model_eval_sim, aes(x = y, y = pred_sim, col = error)) +
  theme_classic() +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = F, col = "black")


# HIGH VARIANCE EXAMPLE ---------------------------------------------------

# Fitting a Polynomial Linear Regression
# Notice it's still called linear, since the model is linear with respect
# to the exponents.

# Manually creating the polynomial variables:
# **Important: This method does not use orthogonal polynomials, which
# Introduce a high degree of dependence between the predictors!
data_poly <- data %>% 
  mutate(Rotation2 = Rotation^2) %>% 
  mutate(Rotation3 = Rotation^3)

data_poly

m_poly <- lm(data = data_poly, Production ~ Rotation + Rotation2 + Rotation3)
summary(m_poly)
par(mfrow=c(2,2))
plot(m_poly)

# Extracting fitted values:
pred_poly <- predict(m_poly)
data_poly <- data.frame(data_poly, pred_poly)

ggplot(data = data_poly, aes(x = Rotation, y = Production)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = "Polynomial Regression of Production vs. Rotation") +
  geom_point(col = "darkred") +
  geom_line(aes(x = Rotation, y = pred_poly), size = 1)


# Let's build an error plot as we did for the linear regression
poly_error <- data_poly %>% 
  select(Production, pred_poly)

poly_error <- poly_error %>% 
  mutate(error = abs(pred_poly - Production))

ggplot(data = poly_error, aes(x = Production, y = pred_poly, col = error)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = "Error plot for the Polynomial Model",
       y = "Predicted Values") +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red")

# One important thing to notice is that, by capturing the
# polynomial (curve-like) nature of the data, the errors (Y_hat x Y)
# are now Linear. But, is this the "correct" relationship in the data?
# The problem is, we can always fit a "good" curve on the training data
# by adding enough parameters to the model. But if these only fit to
# the training data, we have "overfitted" the model.
# When applying that model such model on the test data, we are very likely
# to get large error values. This is known as a High Variance model.


# OVERFITTING A MODEL -----------------------------------------------------

# Creating powers up to 11th
data_hpoly <- data

for (i in 2:11){
  data_hpoly <- data.frame(data_hpoly, data_hpoly$Rotation^i)
}

data_hpoly

poly_orders <- str_c("Rotation", 2:11)
colnames(data_hpoly) <- c("Rotation", "Production", poly_orders)

data_hpoly <- data_hpoly %>% select(Production, everything())
View(data_hpoly)

# Fitting an 11th order polynomial
m_hpoly <- lm(data = data_hpoly, Production ~.)
summary(m_hpoly)
par(mfrow = c(2, 2))
plot(m_hpoly)

# Some notes: Though the R-squared has raised considerably since the
# simple linear model, notice the p-values on each order of the predictor.
# All predictors have become non-significant. This is specially due to 
# the dependence between the different powers and overfitting. Overfitting
# is specially likely to be observed as you increase the number of predictors
# is high relative to number of data points. Nevertheless, let's see how
# the curve behaves:

pred_hpoly <- predict(m_hpoly)

meval_hpoly <- data_hpoly %>% 
  select(Production, Rotation) %>% 
  data.frame(pred_hpoly)

meval_hpoly <- meval_hpoly %>% 
  mutate(errors = abs(Production - pred_hpoly))

meval_hpoly

# Curve Plot:
ggplot(data = meval_hpoly, aes(x = Rotation, y = Production)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = "Hyperpolynomial (11th order) Curve Fitting") +
  geom_point(col = "darkred") +
  stat_smooth(method = "lm", formula = y ~ x, se = F, col = "red") +
  geom_line(aes(x = Rotation, y = pred_hpoly), size = 1, alpha = 0.8)

# The red comes from the simple linear model. Notice how the curve actually
# "tries" to reach the individual data points. This is characteristic of 
# overfitting. Had we enough predictors, the curve could pass through every
# single point.

# Error Plot:
ggplot(data = meval_hpoly, aes(x = Production, y = pred_hpoly, col = errors)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Error Plot - 11th Order Hyperpolynomial",
       y = "Predicted")

# The errors are again linearized, and the predicted and real values seem
# ever closer, but, again, make no mistake, this is just overfitting on the
# training set.

# Just out of curiosity, let us see the polynomial applied on a greater
# number of data points
new_data <- data.frame(Rotation = seq(20, 80, by = 0.5))
for(i in 2:11){
  new_data <- data.frame(new_data, new_data$Rotation^i)
}
colnames(new_data) <- c("Rotation", poly_orders)
new_data %>% head()

new_pred <- predict(m_hpoly, newdata = new_data)

new_data <- data.frame(new_pred, new_data)
new_data %>% head()

# Finally, the plot:
ggplot(data = new_data, aes(x = Rotation, y = new_pred)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(size = 1) +
  labs(title = "Hyperpolynomial - Smoothed Curve",
       y = "Predicted Production")


# EVALUATING MODEL ON UNSEEN DATA -----------------------------------------

machine1_new <- read_xlsx("machine1_newdata.xlsx")
machine1_new

p <- ggplot(data = data, aes(x = Rotation, y = Production)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

p + geom_point(col = "black") +
  geom_point(data = machine1_new,
             aes(x = Rotation, y = Production),
             col = "red")

# Predicting on the new data with the previous models
new_pred_lin <- predict(m1, newdata = machine1_new)

machine1_new$pred_lin <- new_pred_lin
machine1_new

# R-squared Calculator Function:
calc_Rsqrd <- function(y, y_pred){
  return(cor(y, y_pred)^2)
}

# Calculating R-squared on new data based on the simple linear model
# trained on the training data
calc_Rsqrd(y = machine1_new$Production, y_pred = machine1_new$pred_lin)

# Notice the substantial R-squared reduction between training and
# test data.

# Let's perform the same on the hyperpolynomial model
# Creating the data to apply predict()
poly_data_new <- machine1_new %>% select(Production, Rotation)

for (i in 2:11){
  poly_data_new <- data.frame(poly_data_new, poly_data_new$Rotation^i)
}
colnames(poly_data_new) <- c("Production", "Rotation", poly_orders)
poly_data_new %>% head()

pred_poly_new <- predict(m_hpoly, newdata = poly_data_new)

meval_poly_new <- poly_data_new %>%
  select(Production, Rotation) %>% 
  data.frame(pred_poly_new)

meval_poly_new %>% head()

# Calculating R-Squared:
calc_Rsqrd(y = meval_poly_new$Production,
           y_pred = meval_poly_new$pred_poly_new)

# Again, another substantial reduction, highlighting the overfitted nature
# of the model on the training set.


# RANDOMIZED TRAINING AND TEST DATA SETS ----------------------------------

full_data <- read_xlsx("machine_full.xlsx")
full_data %>% head()

## Splitting data into training and test set
# Training set -> 80% of the data

full_data <- full_data %>% 
  mutate(row_id = row_number())

# Set seed for reproducibility
set.seed(1234)
training_data <- full_data %>% 
  sample_frac(0.8)

training_data %>% head()

# Extracting test data (20% remainder) with slice
train_rowid <- training_data %>% pull(row_id)
train_rowid
full_data %>% slice(-train_rowid)

# Using ati_join() to get the remaining 20% of the data
# Gives exatcly the same result as above in less code
test_data <- anti_join(full_data, training_data, by = "row_id")



# PERFORMANCE ACROSS TRAINING AND TEST DATA -------------------------------

# Removing the row_id column from both sets
training_data <- training_data %>% select(Production, Rotation)
test_data <- test_data %>% select(Production, Rotation)

# Fitting a model on the training data
m_lin <- lm(data = training_data,
            Production ~ Rotation)

# We get an R-sqrd and adjusted R-sqrd of 0.2815 and 0.2669, respectively
# Notice the weird curve-like pattern of the residual in the
# residuals vs. fitted plot, hinting that an assumption of a linear 
# relationship between our response and predictor is not likely.
summary(m_lin)
par(mfrow=c(2,2))
plot(m_lin)

## Calculating R-sqrd on the Test data
test_pred <- predict(m_lin, newdata = test_data)
test_data$test_pred <- test_pred

# Notice the deterioration in R-squared
# We observe a reduction of ~ 40% between training and test data
testR2 <- calc_Rsqrd(y = test_data$Production,
                     y_pred = test_data$test_pred)

trainR2 <- summary(m_lin)$r.squared

ratioR2 <- testR2/trainR2
1-ratioR2


# FITTING A POLYNOMIAL REGRESSION -----------------------------------------

# Let's visualize the full data
ggplot(data = full_data, aes(x = Rotation, y = Production)) +
  theme_classic() +
  geom_point()

# The pattern in the points resemble that of a 3rd degree polynomial
# Let's now run polynomial regressions. Again, we will not use orthogonal
# polynomials at this point, and non-orthogonal polynomials will always
# cause high degree of correlations between the predictors.

training_data_poly <- training_data
test_data_poly <- test_data %>% select(Production, Rotation)

for(i in 2:4){
  training_data_poly <- data.frame(training_data_poly,
                                   training_data_poly$Rotation^i)
  
  test_data_poly <- data.frame(test_data_poly,
                               test_data_poly$Rotation^i)
}

# Renaming Columns
new_cols <- c("Production", "Rotation", str_c("Rotation", 2:4))
new_cols
colnames(training_data_poly) <- new_cols
colnames(test_data_poly) <- new_cols

training_data_poly %>% head()
test_data_poly %>% head()

## Fitting the polynomial models
# Full model - 4th degree polynomial
m_poly4 <- lm(data = training_data_poly,
              Production ~ .)
summary(m_poly4)

# Third degree polynomial model (our hypothesis)
m_poly3 <- lm(data = training_data_poly,
              Production ~ Rotation + Rotation2 + Rotation3)
summary(m_poly3)

# Notice how the fourth degree polynomial is no better at explaining
# the variance in the training data
anova(m_poly3, m_poly4)

# Also, there's a reduction in the adjusted R-sqrd on the fourth
# degree polynomial model (recall that the adjusted r-squared corrects
# for the number of predictors)
summary(m_poly3)$adj.r.squared > summary(m_poly4)$adj.r.squared

# The reduction may be small, but together with the anova comparison
# between the models being non-significant, we should prefer the simpler
# third-degree model

# How about comparing a third and second degree model
m_poly2 <- lm(data = training_data_poly,
              Production ~ Rotation + Rotation2)
summary(m_poly2)

# Notice that the adjusted r-squared is now more than half that of the
# third degree polynomial model
summary(m_poly2)$adj.r.squared / summary(m_poly3)$adj.r.squared

# There's also a significant difference between the models 
# in the anova test
anova(m_poly3, m_poly2)

# All of these hint at us that the third degree polynomial might indeed
# be the best relationship for describing the data.

# Notice, however, that these have only been analyzed on the training data.
# Let's now evaluate the models on our test data.

# Create some summary vectors
model <- NULL
train_r2 <- NULL
test_r2 <- NULL


# Loping through in order to create summary data
for (i in 1:4){
  m <- lm(data = training_data_poly[, c(1:(i+1))],
          Production ~ .)
  
  model <- append(model, str_c("Degree", i))
  train_r2 <- append(train_r2, summary(m)$r.squared)
  
  pred <- predict(m, newdata = test_data_poly)
  test_r2 <- append(test_r2, calc_Rsqrd(y = test_data_poly$Production,
                                        y_pred = pred))
  
  
}

# Turning to dataframe
R2_summary <- data.frame(
  model = model,
  train_R2 = train_r2,
  test_R2 = test_r2
)

R2_summary

# Notice that on the above we've calculated the R2 and not the adjusted
# R2, which makes it harder to detect the inappropriateness of the 
# fourth degree polynomial model. Regardless, two things are clear:
# 1) The R2 seems to reach an assymptotic behavior from poly3 to poly 4.
# More polynomial terms would make this clearer.
# 2) There seems to be roughly a loss of 50% - 40% in R2 from the training
# to test data.

R2_summ_long <- R2_summary %>% 
  pivot_longer(cols = c("train_R2", "test_R2"),
               names_to = "data_R2", values_to = "R2")

# Visualizing
# Notice the asymptotic behavior from third to fourth degree polynomial
ggplot(data = R2_summ_long, aes(x = model, y = R2, col = data_R2)) +
  theme_classic() +
  geom_point() +
  geom_line(aes(group = data_R2)) +
  labs(title = "R2 Behavior - Train vs. Test Data on Polynomial Models",
       x = "Model") +
  scale_color_manual(name = "Data",
                     labels = c("Test", "Train"),
                     values = c("red", "black"))


# ADDING MORE POLYNOMIAL TERMS --------------------------------------------

# We will now repeat the above process, but adding more polynomial terms
# successively. It will be useful in highlighting the R2 changes between
# training and test data:

# Recreating data - Models up to 12 degree polynomial:
training_data_poly <- training_data
test_data_poly <- test_data %>% select(Production, Rotation)

for(i in 2:12){
  training_data_poly <- data.frame(training_data_poly,
                                   training_data_poly$Rotation^i)
  
  test_data_poly <- data.frame(test_data_poly,
                               test_data_poly$Rotation^i)
}

# Renaming Columns
new_cols <- c("Production", "Rotation", str_c("Rotation", 2:12))
new_cols
colnames(training_data_poly) <- new_cols
colnames(test_data_poly) <- new_cols

training_data_poly %>% head()
test_data_poly %>% head()


model <- NULL
train_r2 <- NULL
test_r2 <- NULL

# Loping through in order to create summary data
for (i in 1:12){
  m <- lm(data = training_data_poly[, c(1:(i+1))],
          Production ~ .)
  
  model <- append(model, str_c("Degree", i))
  train_r2 <- append(train_r2, summary(m)$r.squared)
  
  pred <- predict(m, newdata = test_data_poly)
  test_r2 <- append(test_r2, calc_Rsqrd(y = test_data_poly$Production,
                                        y_pred = pred))
  
  
}

# Turning to dataframe
R2_summary <- data.frame(
  model = model,
  train_R2 = train_r2,
  test_R2 = test_r2
)

R2_summary


R2_summ_long <- R2_summary %>% 
  pivot_longer(cols = c("train_R2", "test_R2"),
               names_to = "data_R2", values_to = "R2")

R2_summ_long <- R2_summ_long %>%
  mutate_at(.vars = "model",
            .funs = factor, levels = model)

glimpse(R2_summ_long)

# Visualizing
# Notice the asymptotic behavior from third to fourth degree polynomial
ggplot(data = R2_summ_long, aes(x = model, y = R2, col = data_R2)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90))+
  geom_point() +
  geom_line(aes(group = data_R2)) +
  labs(title = "R2 Behavior - Train vs. Test Data on Polynomial Models",
       x = "Model") +
  scale_color_manual(name = "Data",
                     labels = c("Test", "Train"),
                     values = c("red", "black"))

# The plot highlights the very interesting concept we've been discussing:
# Though the training R2 is ever raising (asymptotically), the test R2 will
# eventually plunge. That's because the model has started overfitting to
# the training data, capturing features that are particularly to that set
# of data, but which are not a feature of the test data (and not a general
# pattern within the investigated problem).
# This is the Bias vs. Variance trade-off in a nutshell.
