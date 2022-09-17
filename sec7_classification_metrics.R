setwd("C:/Users/bruno/OneDrive/Desktop/rstudy/R-udemy/r_dataScience_bootcamp/sec7_model_selection")
getwd()

library(stringr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(DHARMa)
library(ROCR)
library(rmarkdown)


# CLASSIFICATION METRICS --------------------------------------------------

## Importing data
titanic <- read.csv("titanic_dataset.csv")
View(titanic)
glimpse(titanic)

# Selecting Target and Features
data <- titanic %>% select(Survived, Fare, Age, Pclass)
data %>% head()
data %>% glimpse()

## Replacing NA in age with mean age
# Extracting mean age
mean_age <- data$Age %>% mean(na.rm = T)
# Replacing NA with mean age
data$Age[is.na(data$Age)] <- mean_age

data

# Creating a row ID column
data <- data %>% mutate(Id = seq(1, nrow(data)))

## Splitting data:
splitter <- function(data, frac){
  set.seed(1234)
  
  train_data <- sample_frac(data, size = frac)
  test_data <- anti_join(data, train_data, by = "Id")
  
  train_data$Id <- NULL
  test_data$Id <- NULL
  
  return(list(train_data, test_data))
}

data_sets_list <- splitter(data = data, frac = 0.8)

# Extracting data from the list
train_data <- data_sets_list[[1]]
test_data <- data_sets_list[[2]]



# Running the Model - GLM with binomial family errors ---------------------

m <- glm(data = train_data, Survived ~ Fare + Age + Pclass,
         family = "binomial")
summary(m)

# Model assumptions assessment with DHARMa
testDispersion(m)
simulateResiduals(fittedModel = m, plot = T)

## Predicting response on test data
y_pred_prob <- predict(m, newdata = test_data, type = "response")

test_data$y_pred_prob <- y_pred_prob

# Response - Assuming 0.5 Threshold
test_data <- test_data %>%
  mutate(y_pred = case_when(y_pred_prob > 0.5 ~ 1,
                                        TRUE ~ 0))

test_data


# Confusion Matrix --------------------------------------------------------

conf_matrix <- test_data %>%
  group_by(y_pred, Survived) %>% 
  count()

conf_matrix

# Classification Eval Function
class_metrics <- function(c_matrix){
  require(dplyr)
  
  TN <- c_matrix %>%
    filter(y_pred == 0, Survived == 0)%>%
    pull(n)
  
  TP <- c_matrix %>% 
    filter(y_pred == 1, Survived == 1) %>% 
    pull(n)
  
  FN <- c_matrix %>% 
    filter(y_pred == 0, Survived == 1) %>% 
    pull(n)
  
  FP <- c_matrix %>% 
    filter(y_pred == 1, Survived == 0) %>% 
    pull(n)
  
  TOTAL <- TN + TP + FN + FP
  
  metrics <- c(TP, FP, TN, FN, TOTAL)
  names(metrics) <- c("TP", "FP", "TN", "FN", "TOTAL")
  
  return(metrics)
  
  
}

metrics <- class_metrics(c_matrix = conf_matrix)

# Turn into matrix format (What is usually called the Confusion Matrix)
confusion_matrix <- matrix(metrics[1:4], nrow=2, ncol=2)
colnames(confusion_matrix) <- c("True_1", "True_0")
rownames(confusion_matrix) <- c("Pred_1", "Pred_0")

confusion_matrix


# PRECISION, RECALL AND F-SCORE -------------------------------------------

## Accuracy = Positives / Total
# Accuracy is a problematic metric specially in data where the
# target event is rare (Target Unbalanced).
accuracy <- (metrics["TP"] + metrics["TN"])/metrics["TOTAL"]
names(accuracy) <- "Accuracy"
accuracy

## Precision = (TP)/(TP + FP)
# The proportion of True Positives within the total number of Positives
precision <- metrics["TP"]/(metrics["TP"] + metrics["FP"])
names(precision) <- "Precision"
precision

## Recall = (TP)/(TP + FN)
# The proportion of True Positives captured within all those which
# actually have a positive response to the event. In our example, the
# denominator represents all those who actually survived, while the
# numerator represents those who survived and were correctly predicted
# by the model
recall <- metrics["TP"]/(metrics["TP"] + metrics["FN"])
names(recall) <- "Recall"
recall

## F-Score = 2*(precision*recall)/(precision + recall)
# F-Score is like a harmonizing mean between precision and recall.
# It's upper limit is 1, indicating a perfect model.
# We would get an F-Score of 1 when both precision and
# recall are equal to 1 (There are no False Negatives or False Positives)
# in the model prediction.
# It's a great alternative for unbalanced data.
fscore <- 2*(precision*recall)/(precision + recall)
names(fscore) <- "F-Score"
fscore

metrics2 <- c(precision, recall, fscore)
metrics2



# ROC - RECEIVER OPERATING CHARACTERISTIC ---------------------------------

### Building a curve manually

## Calculating a ROC curve
sensitivity <- recall
specificity <- metrics["TN"]/(metrics["TN"] + metrics["FP"])

head(test_data)

## Create a ROC generating function
calc_roc <- function(pred_data){
  require(dplyr)
  
  sensitivity_vals <- c()
  specificity_vals <- c()
  threshold_vals <- c()
  
  
  for (thresh in seq(0, 1, 0.001)){
    
    pred_data$y_pred <- ifelse(pred_data$y_pred_prob > thresh, 1, 0)
    
    c_matrix <- pred_data %>%
      group_by(y_pred, Survived) %>% 
      count()
    
    TN <- c_matrix %>%
      filter(y_pred == 0, Survived == 0)%>%
      pull(n)
    
    TP <- c_matrix %>% 
      filter(y_pred == 1, Survived == 1) %>% 
      pull(n)
    
    FN <- c_matrix %>% 
      filter(y_pred == 0, Survived == 1) %>% 
      pull(n)
    
    FP <- c_matrix %>% 
      filter(y_pred == 1, Survived == 0) %>% 
      pull(n)
    
    TP <- ifelse(length(TP) == 0, 0, TP)
    FP <- ifelse(length(FP) == 0, 0, FP)
    TN <- ifelse(length(TN) == 0, 0, TN)
    FN <- ifelse(length(FN) == 0, 0, FN)
    
    sensitivity <- TP/(TP + FN)
    specificity <- TN/(TN + FP)
    
    
    sensitivity_vals <- c(sensitivity_vals, sensitivity)
    specificity_vals <- c(specificity_vals, specificity)
    threshold_vals <- c(threshold_vals, thresh)
    
  }
  
  
  roc_vals <- data.frame(threshold_vals, sensitivity_vals, specificity_vals)
  roc_vals <- roc_vals %>% mutate(FPR = 1-specificity_vals)
  return(roc_vals)
}

roc_vals <- calc_roc(pred_data = test_data)

## ROC values
roc_vals %>% sample_n(10)

# FPR = False Positive Rate
# FPR = 1-specificity

## Plotting the ROC curve
ggplot(data = roc_vals, aes(x = FPR, y = sensitivity_vals)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = "ROC Curve - Titanic Data",
       y = "Sensitivity = TP/(TP + FN)") +
  geom_line() +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", size = 0.5, col = "red")



# ROC - CALCULATING WITH ROCR PACKAGE -------------------------------------

pred <- prediction(
  test_data$y_pred_prob,
  test_data$Survived
)

perf <- performance(
  pred,
  measure = "tpr",
  x.measure = "fpr"
)

plot(perf)

### CALCULATING THE ROC-AUC - THE AREA UNDER THE ROC CURVE
auc <- performance(
  pred,
  measure = "auc"
)

# Extracting the AUC value from S4 object
auc@y.values[[1]]

## Interpretation:
# AUC = 0.5 -> Imply a random model - that is, defines positives and negatives
# at random.
# AUC > 0.5 -> The model is performing better than a random model.
# AUC < 0.5 -> Model is performing worse than a random model.

# AUC is maxed at 1, meaning the model is capable of correctly classifying
# every positive instance in the data. On the other hand, an AUC = 0 means
# the model is reciprocicating, classifying every positive as a negative
# and vice-versa. An AUC = 0.5 implies the model is just randomly guessing
# the positives and negatives. Therefore, we want AUC to be as close as
# possible from 1. Also, AUC is agnostic with relation to arbitrary
# thresholds, making it ideal when a specific threshold value is not
# theoretically clear.