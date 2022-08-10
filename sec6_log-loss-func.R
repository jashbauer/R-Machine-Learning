setwd("C:/Users/bruno/OneDrive/Desktop/rstudy/R-udemy/r_dataScience_bootcamp/sec6_classification")
getwd()

library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)


# THE LOG-LOSS FUNCTION ---------------------------------------------------
data <- read_xlsx("fraud_data.xlsx")
data

# Building the intuition:

# Let's create a set of random b0 and b1 and build sigmoid lines
# based on those. Let's build it around the optimal coefficients from
# the glm output:
m_glm <- glm(data = data, Fraud ~ Hour, family = "binomial")
summary(m_glm)

# Adding random variation around the optimal estimated coefficients:
set.seed(123)
b0 <- 6.11 + rnorm(n=5, mean = 0, sd = 2)
set.seed(123)
b1 <- -0.85 + rnorm(n=5, mean=0, sd = 1)
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


# LOG-LOSS OR BINARY CROSS ENTROPY FUNCTION -------------------------------

# Hp(q) = (-1/N)*SUM[yi*log(p(yi)) + (1-yi)*log(1 - p(yi))])

# It's time to calculate and compare the log-loss function for our
# simulations.

# Create a Log-Loss Function
log_loss <- function(N, y, p_y){
  ll <- (-1/N)*sum(y*log(p_y) + (1-y)*log(1-p_y))
  return(ll)
}

# Testing it with sigmoid 1
log_loss(N = nrow(sig_tibble), y = sig_tibble$fraud_real, p_y = sig_tibble$sig1)

y <- sig_tibble$fraud_real
n <- nrow(sig_tibble)
sigmoids <- sig_tibble %>% select(-c(hour, fraud_real))

# Calculating Log-Loss for each simulation
sig_log_loss <- sigmoids %>% transmute_all(.funs = log_loss, N=n, y=y) %>% 
  distinct()

sig_log_loss
sig_log_loss <- sig_log_loss %>% 
  pivot_longer(everything(),names_to = "sigmoids", values_to = "log_loss")

sig_log_loss

sig_log_loss %>% filter(log_loss == min(log_loss))

# Sigmoid 4 seems to have the lowest value among our random coefficients.
# Let's isolate and compare it visually to the optimal curve.
ggplot(data = sig_tibble, aes(x = hour, y = fraud_real)) +
  theme_classic() +
  scale_color_discrete(name="Fraudulent", labels = c("No", "Yes")) +
  labs(title="Simulated Sigmoid Lines for Random Coefficients",
       x = "Transaction Hour", y = "P(Fraud)") +
  geom_line(aes(x = hour, y = sig4), alpha = 0.5) +
  geom_smooth(method = "glm",
              method.args= list(family = "binomial"),
              size = 1, col = "red", se=F) +
  geom_point(aes(col = factor(fraud_real)))

# Again, the red curve is the one with the optimal coefficients.
# The sig4 curve looks indeed the one that more closely resembles the optimal
# curve.

# Now, let's compare it to the log-loss value of the optimal coefficients:
summary(m_glm)

p_y_optimal <- predict(m_glm, newdata = data.frame(Hour = hour),
                       type = "response")
p_y_optimal

optimal_ll <- log_loss(N = n, y = y, p_y = p_y_optimal)

new_row <- data.frame("optimal", optimal_ll)
names(new_row) <- c("sigmoids", "log_loss")

sig_log_loss <- rbind(new_row, sig_log_loss)

# Finally, let's arrange into a dataframe:
sig_log_loss %>% arrange(log_loss)


# GRADIENT DESCENT INTUITION ----------------------------------------------

# Let us create some more b0s, b1s and their combinations:
b0 <- seq(4, 7, 0.05)
b1 <- seq(-3, 0, 0.05)

# Creates every combination of two vectors as a data frame
betas <- expand.grid(b0, b1)
colnames(betas) <- c("b0","b1")
betas$log_loss <- NaN
betas %>% head()

# Log-Loss Calculator
log_lss_calc <- function(x, y_response, b0, b1){
  pred_y <- 1/(1+exp(-(b0 + b1*x)))
  log_lss <- log_loss(length(y), y = y_response, p_y = pred_y)
  return(log_lss)
}

# Testing
log_lss_calc(x = hour, y_response = y, b0 = 6.11, b1 = -0.84)

# For each of the beta combinations in our betas data, we are going to
# apply our calculator:
for (i in 1:nrow(betas)){
  betas$log_loss[i] <- log_lss_calc(x = hour,
                                    y_response = y,
                                    b0 = betas$b0[i],
                                    b1 = betas$b1[i])
}
betas %>% head()

# Lastly, let us plot a 3-D surface plot of our log-loss landscape.
# Notice the minimum on the curve is none-other than the estimated
# coefficients in the glm model.
fig <- plot_ly(betas,
        x = ~b0,
        y = ~b1,
        z = ~log_loss,
        color = ~log_loss,
        marker = list(size = 2))

fig <- fig %>% add_markers()
fig <- fig %>% layout(title = "Log-Loss Landscape for Multiple Betas",
               scene = list(xaxis = list(title = "Beta 0"),
                            yaxis = list(title = "Beta 1"),
                            zaxis = list(title = "Log Loss")))

fig

# We can confirm it by:
betas %>% filter(log_loss == min(log_loss))

# Thus, we've demonstrated the logic of Gradient Descent using two
# coefficients and the Log-Loss function as a cost function.

# It's important to note that thus far we have used the same data
# to evaluate our models. Ideally, we want to train our model on
# a set of data and apply it to another, independent set,
# to evaluate it (otherwise, our model can be biased towards the
# particular data in which it has been trained).
