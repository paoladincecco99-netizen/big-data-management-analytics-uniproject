# Setting working directory
setwd("~/Desktop/BDMA")
 
# Installing and loading packages
install.packages(c("dplyr", "ggplot2","readr", "viridis", "RColorBrewer", "corrplot", "stargazer", "rpart", "rpart.plot"))
library(dplyr) 
library(ggplot2)
library(readr)
library(viridis)
library(RColorBrewer)
library(corrplot)
library(stargazer)
library(rpart)
library(rpart.plot)

# Loading and reading the dataset
insurance <- read_csv("insurance.csv")

glimpse(insurance)
colSums(is.na(insurance)) 
summary(insurance)

# Converting categorical variables into factors for regression and setting reference levels
insurance <- insurance %>%
  mutate(
    gender = relevel(as.factor(gender), ref = "female"),
    smoker = relevel(as.factor(smoker), ref = "no"),
    region = relevel(as.factor(region), ref = "northeast")
  )

glimpse(insurance)

#######################################################################
# Question 1: average charges for smokers vs non-smokers by age group
#######################################################################
# Creating age groups ([18,30), ..., [60,70))
insurance <- insurance %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(18, 30, 40, 50, 60, 70),
      labels = c("18–29","30–39","40–49","50–59","60–69"),
      right = FALSE,
      include.lowest = FALSE 
    )
  )
table(insurance$age_group)

# Computing average charges per group and smoking status
avg_charges <- insurance %>%
  group_by(age_group, smoker) %>%
  summarise(
    mean_charges = mean(charges, na.rm = TRUE)
  ) %>%
  ungroup()

avg_charges

# Creating tables for smokers and non-smokers only
avg_no <- avg_charges %>%
  filter(smoker == "no") %>%
  select(age_group, mean_charges) %>%
  rename(no = mean_charges)

avg_yes <- avg_charges %>%
  filter(smoker == "yes") %>%
  select(age_group, mean_charges) %>%
  rename(yes = mean_charges)

# Joining them together and computing difference
charge_diff <- avg_no %>%
  left_join(avg_yes, by = "age_group") %>%
  mutate(
    diff = yes - no       
  )

charge_diff %>%
  mutate(
    no   = format(round(no, 0), big.mark = ",", scientific = FALSE),
    yes  = format(round(yes, 0), big.mark = ",", scientific = FALSE),
    diff = format(round(diff, 0), big.mark = ",", scientific = FALSE)
  )

# Bar chart
ggplot(avg_charges, aes(x = age_group, y = mean_charges, fill = smoker)) +
  geom_col(position = "dodge") + 
  labs(
    title = "Average Medical Charges by Age Group and Smoking Status",
    x = "Age Groups",
    y = "Average Charges"
  ) + 
  scale_fill_brewer(
    palette = "Paired",
    name = "Smoker Status",
    labels = c("No", "Yes")
  ) +
  theme_minimal()

# Find age group with largest difference between smokers and non-smokers
max_diff_row <- charge_diff %>%
 filter(diff == max(diff))

format(round(max_diff_row$diff, 1), 
       big.mark = ",")

####################################
# Question 2: correlation analysis
####################################
# Selecting only numerical variables
num_vars <- insurance %>%
  select(charges, age, bmi, children)

glimpse(num_vars)

# Computing correlation matrix
cor_matrix <- cor(num_vars)
round(cor_matrix, 2)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         col = colorRampPalette(c("red", "white", "lightblue"))(200),
         addCoef.col = "black",
         tl.col = "darkblue",
         tl.srt = 45,
         cl.ratio = 0.3)

round(cor_matrix["charges",], 2)

######################################
# Question 3: linear regression model
######################################
lm_model1 <- lm(charges ~ age + gender + bmi + children + smoker + region,
                data = insurance)

stargazer(lm_model1, type = "text",
          title = "Linear Regression Results",
          out = "lm_model1_table.html")

#######################################
# Q5a: Plotting histogram of residuals
#######################################

residuals_model1 <- residuals(lm_model1)

hist(residuals_model1,
     breaks = 15,                       
     col = "skyblue",
     border = "white",
     main = "Histogram of Residuals",
     xlab = "Residuals",
     prob = TRUE)                      


# Adding zero reference line
abline(v = 0, col = "red", lwd = 2)

# Adding theoretical normal curve overlay
xfit <- seq(min(residuals_model1), max(residuals_model1), length=100)
yfit <- dnorm(xfit, mean=mean(residuals_model1), sd=sd(residuals_model1))
lines(xfit, yfit, col="darkblue", lwd=2)

#######################################
# Q5b: Plotting residuals vs Predicted
#######################################
# Extracting predicted values
predicted_model1 <- predict(lm_model1)

# Plotting residuals vs predicted charges
plot(predicted_model1, residuals_model1,
     col = "darkblue",
     bg = "white",
     pch = 21,
     main = "Residuals vs Predicted Charges",
     xlab = "Predicted Charges",
     ylab = "Residuals")

# Add horizontal reference line at 0
abline(h = 0, col = "red", lwd = 2)

#################################################
# Q6: Creating log_charges and fitting lm_model2
#################################################
# Creating log-transformed target variable
insurance <- insurance %>%
  mutate(log_charges = log(charges))

# Fitting linear model with log_charges
lm_model2 <- lm(log_charges ~ age + gender + bmi + children + smoker + region,
             data = insurance)

stargazer(lm_model2, type = "text",
          title = "Log Regression Results",
          out = "lm_model2_table.html")

###########################################
# Q6b: Histogram of residuals (log model)
###########################################

residuals_model2 <- residuals(lm_model2)

hist(residuals_model2,
     breaks = 15,
     col = "lightblue",
     border = "white",
     main = "Histogram of Residuals (Log Model)",
     xlab = "Residuals",
     prob = TRUE)

# Adding zero reference line
abline(v = 0, col = "red", lwd = 2)

# Adding theoretical normal curve overlay
xfit <- seq(min(residuals_model2), max(residuals_model2), length=100)
yfit <- dnorm(xfit, mean=mean(residuals_model2), sd=sd(residuals_model2))
lines(xfit, yfit, col="darkblue", lwd=2)

######################################
# Q6c: Residuals vs Predicted (Log Model)
######################################

predicted_model2 <- predict(lm_model2)

plot(predicted_model2, residuals_model2,
     col = "darkblue",
     bg = "white",
     pch = 21,
     main = "Residuals vs Predicted Charges (Log Model)",
     xlab = "Predicted Log-Charges",
     ylab = "Residuals")

abline(h = 0, col = "red", lwd = 2)

#########################################################
# Q7: Reporting R-squared and Adjusted R-squared values
#########################################################
stargazer(lm_model2, type = "text", title = "Log Regression Results")

#########################
# Q8: Model prediction 1
#########################
#Preparing new data
new_obs <- data.frame(
  age = c(25, 45, 32, 54, 29),
  gender = as.factor(c("male", "female", "male", "female", "female")),
  bmi = c(28.0, 35.2, 30.5, 24.7, 22.8),
  children = c(1, 3, 0, 2, 1),
  smoker = as.factor(c("no", "yes", "no", "yes", "yes")),
  region = as.factor(c("northeast", "southeast", "northwest", "southwest", "southeast"))
)

# Aligning factor levels
new_obs$gender <- factor(new_obs$gender, levels = levels(insurance$gender))
new_obs$smoker <- factor(new_obs$smoker, levels = levels(insurance$smoker))
new_obs$region <- factor(new_obs$region, levels = levels(insurance$region))

#Predicting log_charges & converting them back
pred_log <- predict(lm_model2, newdata = new_obs)

pred_charges <- exp(pred_log)

formatted_pred_charges <- formatC(pred_charges,
                                  format = "f",
                                  digits = 2,
                                  big.mark = ",")
formatted_pred_charges

###############################
# Q9: Observations percentages
###############################
# Creating binary charges
mi_charges <- mean(insurance$charges)

insurance <- insurance %>%
  mutate(binary_charges = ifelse(charges <= mi_charges, 0, 1))

# Computing percentage of observations per class
table_counts <- table(insurance$binary_charges)
percentage_classes <- prop.table(table_counts) * 100

formatted_percentage_classes <- format(percentage_classes, 
                                       digits = 4)
formatted_percentage_classes

##################################
# Q10: Logistic Regression Model
##################################
log_model <- glm(binary_charges ~ age + gender + bmi + children + smoker + region,
                 data = insurance,
                 family = binomial)

odds_ratio <- exp(coef(log_model))
odds_ratio

stargazer(log_model, type = "text", 
          title = "Logistic Regression Results",
          out = "log_model_table.html")


##########################
# Q12: Model Prediction 2
##########################
# Predicting probabilities
prob_log <- predict(log_model, newdata = new_obs, type = "response")
formatted_prob_log <- format(prob_log,
                             digits = 2)
formatted_prob_log

# Converting them into binary predictions
bin_log <- ifelse(prob_log > 0.5, 1, 0)
bin_log

####################################
# Q13: Creating multi-class charges
####################################
# Computing quartiles
q1 <- quantile(insurance$charges, 0.25)
q3 <- quantile(insurance$charges, 0.75)

# Creating categorical variable
insurance <- insurance %>%
  mutate(multiclass_charges = case_when(
    charges <= q1 ~ "low",
    charges > q1 & charges <= q3 ~ "medium",
    charges > q3 ~ "high"
  ))

# Getting percentages
insurance$multiclass_charges <- factor(insurance$multiclass_charges,
                                       levels = c("low", "medium", "high"))

table_multi <- table(insurance$multiclass_charges)
percentage_multi <- prop.table(table_multi) * 100

formatted_percentage_multi <- format(percentage_multi,
                                     digits = 4)
formatted_percentage_multi

##################################################
# Creating decision tree for questions 14, 15, 16
##################################################
decision_tree_model <- rpart(multiclass_charges ~ age + gender + bmi + children + smoker + region,
                             data = insurance,
                             method = "class",
                             control = rpart.control(maxdepth = 4))

View(decision_tree_model$frame)

# Q14: Counting number of leaf nodes
num_leaves <- sum(decision_tree_model$frame$var == "<leaf>")
num_leaves

# Q15: Plotting decision tree model
rpart.plot(decision_tree_model,
           type = 4,              
           extra = 104,
           nn = TRUE, 
           under = TRUE,           
           fallen.leaves = TRUE,   
           box.palette = "Blues",  
           shadow.col = "gray",    
           branch.lwd = 2,        
           branch.col = "gray50",
           main = "Decision Tree for Multiclass Charges")

# Q16: Predicting multiclass_charges 
pred_tree <- predict(decision_tree_model,
                     newdata = new_obs,
                     type = "class")

pred_tree

#########################
# End of the Assignment #
#########################
