setwd("C:/Users/bruno/OneDrive/Desktop/rstudy/R-udemy/r_dataScience_bootcamp/sec6_classification")
getwd()

library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)


# LOGISTIC REGRESSION - FRAUD DATA EXAMPLE --------------------------------
data <- read_xlsx("fraud_data.xlsx")
data
View(data)

p <- ggplot(data = data, aes(x=Hour, y=Amount, color = factor(Fraud))) +
  geom_point()
p

p2 <- ggplot(data=data, aes(x=Hour, y=factor(Fraud), color = factor(Fraud)))+
  geom_point()
p2


# USING A LINEAR MODEL ----------------------------------------------------


m1 <- lm(data = data, Fraud ~ Hour)
summary(m1)
pred <- predict(m1)

data_lin <- data.frame(data, pred)
ggplot(data = data, aes(x=Hour, y=pred)) +
  geom_line(size=1) +
  geom_point(aes(x=Hour, y=Fraud, color = factor(Fraud)))

# Though the linear model captures a very general pattern, it's clearly
# inappropriate. Namely, it predicts fraudulence lower than 0 and  higher
# than 1, for starters.


# CALCULATING A SIGMOID FUN. AND FITTING A LOGISTIC REGRESSION ------------

# Calculating it manually
b0 <- m1$coefficients[1]
b1 <- m1$coefficients[2]

pred_sig <- 1/(1+exp(-(b0 + data$Hour*b1)))
pred

data_sig <- data.frame(data, pred_sig)
ggplot(data = data_sig, aes(x = Hour, y = pred_sig)) +
  geom_line()

# It's a curve, but notice how it's still very close to a line.
# This is not surprising since we are using the coefficients estimated
# from a Linear Model that used MSE as it's cost function.
# The thing is, the structure of the errors in our problem is not Gaussian.
# We need a method that takes into account this error structure in estimating
# the coefficients. Tht's the core of GLMs. For our response, we expect
# the errors to behave Binomially. To link those errors to a linear
# transform, we use the logit function. The principle here is that we 
# model the errors using a function from the exponential family, linearize
# through the appropriate link function, and estimate the coefficients.
# However, our cost function now is log-likelihood based, instead of the
# Squared errors in classic linear models.


# LOGISTIC REGRESSION WITH GLMs - GENERALIZED LINEAR MODELS ---------------

m_glm <- glm(data = data, Fraud ~ Hour, family = "binomial")
summary(m_glm)

# We must be very carefull with these coefficients. They are the coefficients
# of a "line" (a linear transform). To interpret it as the response, we must
# transform them back. Let's do it manually:

b0 <- m_glm$coefficients[1]
b1 <- m_glm$coefficients[2]

hour <- seq(1:24)
pred_frd_lin <- b0 + hour*b1
pred_frg_sig <- 1/(1+exp(-(b0 + hour*b1)))

data_glm_manual <- data.frame(hour, pred_frd_lin, pred_frg_sig)
data_glm_manual

# Now comes the fun! Let's visualize!
ggplot(data = data_glm_manual,
       aes(x = hour, y = pred_frd_lin)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(size = 1) +
  labs(x = "Hour", y = "Fraud(Linear)",
       title = "Fraud vs Hour - Linear Transform")

# Well, did we fail? Actually, no. The above is the linear transform of the
# sigmoid function, almost (and notice that I said "ALMOST", but not quite)
# as if we are taking the log of an exponential. The exponents behave linearly.
# Now, let's see our manual sigmoid predict, but before, let's add a flag column
# to our predicted data.

# String Flag
data_glm_manual <- data_glm_manual %>%
  mutate(fraud = case_when(pred_frg_sig > 0.5 ~ "Yes",
                           TRUE ~ "No"))

# Binary Flag
data_glm_manual <- data_glm_manual %>%
  mutate(fraud_bin = case_when(pred_frg_sig > 0.5 ~ 1,
                               TRUE ~ 0))

ggplot(data = data_glm_manual,
       aes(x = hour, y = pred_frg_sig)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(size = 1, alpha = 0.7) +
  geom_point(aes(x = hour, y = fraud_bin, color = fraud)) +
  labs(x="Hour", y = "Sim. P(Fraud)", title = "Fraud vs. Hour")

# Now we get the expected behavior. Notice the only thing we did was
# reversing the linear transform. Instead of doing all that manually,
# there are better options in R, namely the predict() function.


# THE LOG-LOSS FUNCTION ---------------------------------------------------

data

# Building the intuition:
# Let's create a set of random b0 and b1 and build sigmoid lines
# based on those:

# Adding random variation around the optimal estimated coefficients
b0 <- 6.11 + rnorm(n=5, mean = 0, sd = 2)
b1 <- -0.85 + rnorm(n=5, mean=0, sd = 0.5)
length(b0) == length(b1)

betas <- tibble(b0, b1)
betas

# Create an hours vector
hour <- data$Hour

# Simulate sigmoid data from the random coefficients:
sig_tibble <- NULL
for (i in 1:nrow(betas)){
  sig_tibble <- cbind(sig_tibble, (1/(1+exp(-(betas$b0[i] + betas$b1[i]*hour)))))
}

sig_tibble <- data.frame(sig_tibble)
colnames(sig_tibble) <- str_c(rep("sig", nrow(betas)), 1:nrow(betas))
sig_tibble <- tibble(hour, sig_tibble, fraud_real = data$Fraud)

# Visualizing Simulations (Red Curve = Optimal Curve)
ggplot(data = sig_tibble, aes(x = hour, y = fraud_real)) +
  theme_classic() +
  scale_color_discrete(name="Fraudulent", labels = c("No", "Yes")) +
  labs(title="Simulated Sigmoid Lines for Random Coefficients",
       x = "Transaction Hour", y = "P(Fraud)") +
  geom_line(aes(x = hour, y = sig1), alpha = 0.5) +
  geom_line(aes(x = hour, y = sig2), alpha = 0.5) +
  geom_line(aes(x = hour, y = sig3), alpha = 0.5) +
  geom_line(aes(x = hour, y = sig4), alpha = 0.5) +
  geom_line(aes(x = hour, y = sig5), alpha = 0.5) +
  geom_smooth(method = "glm",
              method.args= list(family = "binomial"),
              size = 1, col = "red", se=F) +
  geom_point(aes(col = factor(fraud_real)))
