# Setting working directory
setwd("")

# Installing and loading packages
install.packages(c("dplyr", "ggplot2", "caret", "pROC", "class", "arules", "readr", "stargazer"))
library(dplyr) 
library(ggplot2)
library(caret)
library(pROC)
library(class)
library(arules)
library(readr)
library(stargazer)

# Loading and reading the dataset
insurance_new <- read_csv("insurance_new.csv")

glimpse(insurance_new)
colSums(is.na(insurance_new)) 
summary(insurance_new)

# Creating binary charges
mi_charges <- mean(insurance_new$charges)

insurance_new <- insurance_new %>%
  mutate(binary_charges = ifelse(charges > mi_charges, 1, 0))

# Converting categorical variables into factors
insurance_new <- insurance_new %>%
  mutate(
    gender = as.factor(gender),
    smoker = as.factor(smoker),
    region = as.factor(region),
    binary_charges = as.factor(binary_charges)
  )

#######################################
# Q1: Logistic regression model
#######################################
# Building the model
logistic_model <- glm(binary_charges ~ age + gender + bmi + children + smoker + region,
                 data = insurance_new,
                 family = binomial)

summary(logistic_model)

# Predicting porbabilities
pred_probs <- predict(logistic_model, type = "response")


# a) Cut-off probabilty 0.2
pred_02 <- ifelse(pred_probs > 0.2, 1, 0)
table(Actual = factor(insurance_new$binary_charges, levels = c(1, 0)), Predicted = factor(pred_02, levels = c(1, 0)))


# b) Cut-off probabilty 0.8
pred_08 <- ifelse(pred_probs > 0.8, 1, 0)
table(Actual = factor(insurance_new$binary_charges, levels = c(1, 0)), Predicted = factor(pred_08, levels = c(1, 0)))


# c) ROC curve and AUC
roc_obj <- roc(insurance_new$binary_charges, pred_probs)
plot(roc_obj, col = "blue4", main = "ROC Curve for Logistic Regression")

# AUC value
auc_value <- auc(roc_obj)
auc_value

################################################
# Q2: Logistic regression model, 0.5 threshold
################################################
# Confusion matrix
pred_05 <- ifelse(pred_probs > 0.5, 1, 0)
table(Actual = factor(insurance_new$binary_charges, levels = c(1, 0)), Predicted = factor(pred_05, levels = c(1, 0)))

# b) Accuracy
accuracy <- sum(diag(cm_05)) / sum(cm_05)
accuracy

# c) Precision
precision <- cm_05["1","1"] / (cm_05["1","1"] + cm_05["1","0"])
precision

# d) Sensitivity and f) True positive rate 
sensitivity <- cm_05["1","1"] / (cm_05["1","1"] + cm_05["0","1"])
sensitivity

# e) Specificity
specificity <- cm_05["0","0"] / (cm_05["0","0"] + cm_05["1","0"])
specificity

# g) False positive rate
fpr <- 1 - specificity
fpr

#################
# Q3: Distances
#################
# Loading new dataset
books <- read.csv("books.csv")

# Inspecting structure
str(books)
head(books)
summary(books)

# Selecting only the 5 category columns for the two customers
cust_241 <- books[books$customer_id == 241, c("fiction", "non_fiction", "childrens_book", "self_help", "mystery")]
cust_431 <- books[books$customer_id == 431, c("fiction", "non_fiction", "childrens_book", "self_help", "mystery")]

# a) Euclidean distance
euclidean_241_431 <- sqrt(sum((cust_241 - cust_431)^2))
euclidean_241_431

# b) Manhattan distance
cust_82 <- books[books$customer_id == 82, c("fiction", "non_fiction", "childrens_book", "self_help", "mystery")]
cust_199 <- books[books$customer_id == 199, c("fiction", "non_fiction", "childrens_book", "self_help", "mystery")]

manhattan_82_199 <- sum(abs(cust_82 - cust_199))
manhattan_82_199

# c) Centroid of first 150 customers
first150 <- books[1:150, ]
centroid <- colMeans(first150[, c("fiction",
                                  "non_fiction",
                                  "childrens_book",
                                  "self_help",
                                  "mystery")])
round(centroid, 2)

####################
# Q4: Co-occurence
####################
# Select only the 5 category columns
categories <- books[, c("fiction", "non_fiction", "childrens_book", "self_help", "mystery")]

# Compute co-occurrence matrix
co_occurrence <- t(categories) %*% as.matrix(categories)
co_occurrence

# Convert to data frame of pairs
pairs <- data.frame()
cat_names <- colnames(categories)

for (i in 1:5) {
  for (j in i:5) {
    if (i != j) {
      count <- co_occurrence[i, j]
      pairs <- rbind(pairs, data.frame(
        Category1 = cat_names[i],
        Category2 = cat_names[j],
        Cooccurrence = count
      ))
    }
  }
}

pairs

# a) Highest co-occurrence
max_pair <- pairs[which.max(pairs$Cooccurrence), ]
max_pair
# b) Lowest co-occurrence
min_pair <- pairs[which.min(pairs$Cooccurrence), ]
min_pair

# Checking
sum(books$fiction == 1 & books$mystery == 1)
sum(books$non_fiction == 1 & books$childrens_book == 1)

################
# Q5: Clusters
################
# Computing total books purchased per customer
books$total_books <- rowSums(books[, c("fiction", "non_fiction", "childrens_book", "self_help", "mystery")])

# Ensuring all clusters 0–5 appear
cluster_sizes <- table(factor(books$total_books, levels = 0:5))

# Converting to data frame
cluster_df <- data.frame(
  Total_Books_Purchased = 0:5,
  Number_of_Customers   = as.vector(cluster_sizes)
)

# Printing the table using stargazer (text format)
stargazer(cluster_df, type = "text", summary = FALSE, rownames = FALSE,
          title = "Cluster Sizes Based on Total Books Purchased", out = "cluster_table.html")

###############
# Q6: Support
###############
# Total number of transactions
N <- nrow(books)

# Compute supports
support_fiction <- sum(books$fiction == 1) / N
support_nonfiction <- sum(books$non_fiction == 1) / N
support_fiction_selfhelp <- sum(books$fiction == 1 & books$self_help == 1) / N

# Create data frame for table
support_df <- data.frame(
  Itemset = c("{fiction}", "{non_fiction}", "{fiction, self_help}"),
  Support = c(support_fiction, support_nonfiction, support_fiction_selfhelp)
)

# Show table using stargazer
stargazer(support_df, type = "text", summary = FALSE, rownames = FALSE,
          title = "Support Values", out = "support_table.html")

##################
# Q7: Confidence
##################
# a) {fiction} → {mystery}
conf_fiction_to_mystery <- 
  sum(books$fiction == 1 & books$mystery == 1) /
  sum(books$fiction == 1)

# b) {non_fiction} → {self_help}
conf_nonfiction_to_selfhelp <- 
  sum(books$non_fiction == 1 & books$self_help == 1) /
  sum(books$non_fiction == 1)

# c) {fiction, self_help} → {childrens_books}
conf_fictionself_to_children <- 
  sum(books$fiction == 1 & books$self_help == 1 & books$childrens_book == 1) /
  sum(books$fiction == 1 & books$self_help == 1)

# Build results table
confidence_df <- data.frame(
  Rule = c("{fiction} -> {mystery}",
           "{non_fiction} -> {self_help}",
           "{fiction, self_help} -> {childrens_books}"),
  Confidence = c(conf_fiction_to_mystery,
                 conf_nonfiction_to_selfhelp,
                 conf_fictionself_to_children)
)

# Stargazer output table
stargazer(confidence_df, type = "text", summary = FALSE, rownames = FALSE,
          title = "Confidence of Association Rules", out = "confidence_table.html")

##################
# Q8: Lift
##################
# Support values
supp_fiction <- sum(books$fiction == 1) / N
supp_nonfiction <- sum(books$non_fiction == 1) / N
supp_selfhelp <- sum(books$self_help == 1) / N
supp_children <- sum(books$childrens_book == 1) / N
supp_fiction_selfhelp <- sum(books$fiction == 1 & books$self_help == 1) / N

# Joint supports
supp_fictionself_children <- sum(books$fiction == 1 & books$self_help == 1 & books$childrens_book == 1) / N
supp_fiction_nonfiction <- sum(books$fiction == 1 & books$non_fiction == 1) / N
supp_nonfiction_selfhelp <- sum(books$non_fiction == 1 & books$self_help == 1) / N

# Lift calculations
lift_fictionself_to_children <- supp_fictionself_children / (supp_fiction_selfhelp * supp_children)
lift_fiction_to_nonfiction <- supp_fiction_nonfiction / (supp_fiction * supp_nonfiction)
lift_nonfiction_to_selfhelp <- supp_nonfiction_selfhelp / (supp_nonfiction * supp_selfhelp)

# Table
lift_df <- data.frame(
  Rule = c(
    "{fiction, self_help} -> {childrens_books}",
    "{fiction} -> {non_fiction}",
    "{non_fiction} -> {self_help}"
  ),
  Lift = c(
    lift_fictionself_to_children,
    lift_fiction_to_nonfiction,
    lift_nonfiction_to_selfhelp
  )
)

# Output table with stargazer
stargazer(lift_df,
          type = "text",
          summary = FALSE,
          rownames = FALSE,
          title = "Lift of Association Rules", out = "lift_table.html")

