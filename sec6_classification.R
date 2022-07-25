setwd("C:/Users/bruno/OneDrive/Desktop/rstudy/R-udemy/r_dataScience_bootcamp/sec6_classification")
getwd()

library(ggplot2)
library(dplyr)
library(readxl)
library(DHARMa)
library(tidyverse)
library(gridExtra)
library(rmarkdown)

# LOGISTIC REGRESSION EXAMPLE - SPECIES CLASSIFICATION --------------------

df <- iris
df %>% head()

# Let's filter only two species, since logistic regression ideally classifies
# between two classes
df <- df %>% filter(Species == c("setosa", "virginica"))
glimpse(df)

# Create a binary column (target column)
df <- df %>% mutate(bin_species = case_when(Species == "setosa" ~ 1,
                                      TRUE ~ 0))
df %>% head()
# Let's use sepal width to try and predict the species.
m1 <- glm(data = df, bin_species ~ Sepal.Width,
    family = "binomial")

summary(m1)

# Extracting predicted values
pred <- m1 %>% predict(type = "response")
df <- df %>% cbind(pred)

# Creating predicted probabilities column
df <- df %>% mutate(pred.prob = case_when(pred > 0.5 ~ 1,
                        TRUE ~ 0))

df %>%  select(Sepal.Width, Species, pred, pred.prob) %>% head()


# Plotting original data
ggplot(df, aes(x = Sepal.Width, y = bin_species)) +
  theme_classic() +
  labs(title = "Predicting Setosa vs Virginica with Sepal Width",
       x = "Sepal Width", y = "P(Setosa)") +
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none") +
  geom_point(aes(col=bin_species)) +
  geom_smooth(method="glm", method.args = list(family = "binomial"),
              se = F, col = "black")

# Plotting predicted data
ggplot(df, aes(x = Sepal.Width, y = pred.prob)) +
  theme_classic() +
  labs(title = "Predicting Setosa vs Virginica with Sepal Width",
       x = "Sepal Width", y = "P(Setosa)") +
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none") +
  scale_color_hue(l = 40, c = 30) +
  geom_point(aes(col=factor(pred.prob))) +
  geom_line(aes(x = Sepal.Width, y = pred),
            size = 1, col = "steelblue")



# CLASSIFICATION - FRAUDULENT DATA ----------------------------------------

data <- read_xlsx("fraud_data.xlsx")
data %>% glimpse()
View(data)

# Visualizing Data
p_hr_amt <- ggplot(data = data, aes(x = Hour, y = Amount, col = factor(Fraud))) +
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_color_manual(name = "Fraudulent",
                       labels = c("Yes", "No"),
                       values = c("red", "black")) +
  labs(title = "Transaction Amount vs. Transaction Hour")

p_hr_amt

#* It seems we have an association between whether a transaction
#* is fraudulent and transaction hour. Let's visualize it in
#* another plot.

# Hour x Fraudulence
ggplot(data = data, aes(x = factor(Fraud), y = Hour, fill = factor(Fraud))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(col = factor(Fraud)), alpha = 0.5) +
  scale_color_manual(values = c("grey", "red")) +
  scale_fill_manual(values = c("grey", "red")) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Fraudulence vs. Transaction Hour",
       x = "Fraudulent", y = "Hour")

#* The boxplots hint to a strong association between fraudulence and
#* transaction hour. It's worth checking the boxplots for fraudulence vs.
#* Amount:

# Amount x Fraudulence
ggplot(data = data, aes(x = factor(Fraud), y = Amount, fill = factor(Fraud))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(col = factor(Fraud)), alpha = 0.5) +
  scale_color_manual(values = c("grey", "red")) +
  scale_fill_manual(values = c("grey", "red")) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Fraudulence vs. Transaction Amount",
       x = "Fraudulent", y = "Amount")

#* The relationship seems much less pronounced. The median amount for fraudulent
#* transactions seems slightly above that for non-fraudulent transactions.
#* Interestingly, it appears that fraudulent transactions have a much higher
#* spread in transaction amount than the non-fraudulent group.


# ANALYZING THE RELATIONSHIP FORMALLY - LOGISTIC REGRESSION ----------

# We shall first run simple logistic regressions for
# each predictor (Hour, Amount). We will later use both predictors in a single
# model.


# Let's first fit a logistic regression between Hour x Fraudulence:
m_hour <- glm(data = data, Fraud ~ Hour, family = "binomial")
par(mfrow=c(2,2))
plot(m_hour)

# Checking Residuals with DHARma
par(mfrow=c(1,1))
testDispersion(m_hour)

simulateResiduals(m_hour, plot = T)

# Model is validated.
# Let us interpret the results:
summary(m_hour)

#* As hinted by the boxplots, transaction hour is a significant predictor of
#* Fraudulence. Since a GLM's output is logit-wise, and harder to interpret
#* directly, let us use the predict function for better visualization:

# For now, we don't have a training set, so we will use the fitted values
pred_prob_hour <- predict(m_hour, type = "response")
data <- tibble(data, pred_prob_hour)
data <- data %>% mutate(pred_fraud_hour = case_when(pred_prob_hour > 0.5 ~ "Yes",
                                            TRUE ~ "No"))

# Time to visualize
p_hour <- ggplot(data = data, aes(x = Hour, y = pred_prob_hour)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = "Logistic Regression: Fraudulence vs. Transaction Hour",
       x = "Hour", y = "P(Fraud)") +
  geom_line(size = 1) +
  geom_point(aes(x = Hour, y = Fraud, col = pred_fraud_hour)) +
  scale_color_manual(name = "Fraudulent",
                     values = c("darkslategrey", "red"))

p_hour

# The points are the actual fraudulence status in the data. Notice the model
# did a pretty god job at classifying fraudulence status by transaction hour.

# Let's do the same fro vraudulence x amount.
m_amt <- glm(data = data, Fraud ~ Amount, family = "binomial")
par(mfrow=c(2,2))
plot(m_amt)

par(mfrow=c(1,1))
testDispersion(m_amt)
simulateResiduals(m_amt, plot = T)

# Model doesn't behave as well as for the Hour predictor, but it's validated.
summary(m_amt)

# Model interpretation:
# Amount is also a significant predictor, but notice how the p-value is much
# higher than for the Hour predictor, though still significant.
# Model AIC is also much much higher.

# Let's get some predicted values and visualize:
pred_prob_amount <- predict(m_amt, type = "response")
data <- tibble(data, pred_prob_amount)
data <- data %>% mutate(pred_fraud_amount = case_when(pred_prob_amount > 0.5 ~ "Yes",
                                              TRUE ~ "No"))
View(data)

ggplot(data = data, aes(x = Amount, y = pred_prob_amount)) +
  geom_line() + theme_classic() +
  labs(title = "Logistic Regression: Fraudulence vs. Transaction Amount",
       y = "P(Fraudulence)")

# Notice the weird shape of the line. We can get a better line by calling
# predict() on a greater number of amount values:

new_data <- seq(0, 2000, 1) %>% data.frame()
colnames(new_data) <- "Amount"

new_data <- new_data %>% mutate(pred_p_amount = predict(m_amt,
                                            type = "response",
                                            newdata = new_data))

# The greater number of points will make the line smoother.
p_amt <- ggplot(data = new_data, aes(x = Amount, y = pred_p_amount)) +
  geom_line() + theme_classic() +
  labs(title = "Logistic Regression: Fraudulence vs. Transaction Amount",
       y = "P(Fraudulence)") +
  geom_point(data = data, aes(x = Amount, y = Fraud, col = factor(Fraud))) +
  scale_color_manual(name = "Fraudulent", labels = c("No", "Yes"),
                     values = c("red", "darkslategrey"))

p_amt

# As before, the points depict the real fraudulence status per data point.
# The relationship is clearly much less pronounced.

# Lets look at the model summaries again:
summary(m_hour)
summary(m_amt)

# Let's get a deeper interpretation of these models.
# The negative coefficient on Hour tells us that this predictor has an
# inverse relationship to the response, that is, the earlier (late at night)
# on a day a transaction takes place, higher the risk of fraud.
# Moreover, this relationship is rather strong with a very low p-value.
# On the other hand, amount's got a positive slope,
# indicating the higher the transaction amount,
# the higher the probability of fraud. This relationship is not as
# pronounced, however, with a higher p-value associated with this predictor.
# It's significant, nonetheless.
# These characteristics are well seen in both plots.
grid.arrange(p_hour, p_amt, ncol=2)


# MULTIPLE LOGISTIC REGRESSION --------------------------------------------

# Our final step for this data is running a model with both hour and amount
# as predictors:

m_hr_amt <- glm(data = data, Fraud ~ Hour + Amount, family = "binomial")
par(mfrow=c(2,2))
plot(m_hr_amt)

par(mfrow=c(1,1))
testDispersion(m_hr_amt)
simulateResiduals(m_hr_amt, plot = T)

# Model Validated
# Interpreting:
summary(m_hr_amt)

# Not surprisingly, when using both predictors, the effect of Amount
# disappears. It confirms our previous intuition that Amount was a bad
# predictor to begin with (Recall the boxplot and the logistic plot).
# We can even show that dropping Amount will not affect model performance
# more formally

anova(m_hr_amt, m_hour, test="Chisq")

# As we can see, the p-val > 0.05 indicates that there's not a significant
# reduction in explanatory power.
# We can compare the AICs
AIC(m_hour) - AIC(m_hr_amt)

# Though the AIC reduction is small, the anova test and model parsimony
# tells us that we should prefer the simpler model and assume the effect
# of amount on fraudulence is non-significant.
# This also serves the purpose of showing us how isolated predictors may
# seem significant, while together the relationship changes. In our example,
# our previous inspection of the data also hints us to the probable
# non-significance of the amount predictor.

# Finally, notice how Amount and Hour share a degree of correlation
# Pearson's Correlation Coefficient
p_hr_amt
cor(data$Amount, data$Hour, method = "pearson")

# We even get a significant regression between both variable
summary(lm(data = data, Hour ~ Amount))

ggplot(data = data, aes(x = Hour, y = Amount)) +
  theme_classic() +
  geom_point() + geom_smooth(method = "lm", se = F, col = "red")

# This correlation could have been responsible for the significance
# of the Amount predictor when the sole predictor in the model.
