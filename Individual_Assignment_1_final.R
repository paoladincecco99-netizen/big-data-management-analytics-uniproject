##################################
# Big Data Management & Analytics
# Individual Assignment 1
##################################

# Setting my working directory
getwd()                   # where R reads/writes files by default
setwd("")  # path to personal folder

# Installing packages
install.packages(c("dplyr", "ggplot2", "lubridate", "readr", "plotrix"))

# Loading packages
library(dplyr) # for data manipulation
library(ggplot2) # for data visualization
library(lubridate) # for working with dates 
library(readr) # for reading data 
library(plotrix) # for additional data visualization tools 

# Importing data set and renaming file
orange_data <- read_csv("orange_inc.csv", na = ("-")) # the dataset uses "-" to 
                                                   # indicate missing cells

#############################################
# Question 1: Explore data & missing values
#############################################
# Exploring the data 
str(orange_data)
head(orange_data) 
tail(orange_data) 

# Converting it to data frame
orange_data  <- as.data.frame(orange_data)

# Checking and counting missing values per column
(is.na(orange_data))
sum(is.na(orange_data))
colSums((is.na(orange_data)))

missing_table <- data.frame(
  Missing_Values = colSums(is.na(orange_data[-1]))) # omitting first row (user_id)

print(missing_table)

##########################
# Question 2: Clean data
##########################
complete.cases(orange_data) # checking for complete cases
orange_data_clean <- orange_data[complete.cases(orange_data), ] # keeping only complete cases
complete_obs<- nrow(orange_data_clean) # counting total number of complete cases

print(paste0("Total number of observations: ", format(round(complete_obs, 0), big.mark = ","), "."))

#####################################
# Question 3: Create derived column
#####################################

# Identifying product columns dynamically
product_cols <- c("oPhone", "oWatch", "oPods", "oTV")

# Storing product prices
product_prices <- c(
  oPhone = 1200,
  oWatch = 300,
  oPods = 150,
  oTV = 2500
)

# Adding number of products and ownership flag dynamically
orange_data_clean <- orange_data_clean %>%
  mutate(
    num_products = rowSums(select(., all_of(product_cols))),
    owns_multiple_products = ifelse(num_products >= 2, 1, 0)
  )

# Deriving number of customers owning multiple products
custom_mult_prod <- sum(orange_data_clean$owns_multiple_products)

print(paste0("Customers owning multiple products: ", format(
  round(custom_mult_prod, 0), big.mark = ","), "."))

#############################################
# Question 4, 5, 6: Filter customer groups
#############################################
# creating subsets to answer questions 4 and 5
# 4) Female 18–25 with Visa/Mastercard credit

subset1 <- subset(
  orange_data_clean,
  age_bracket == "18-25" &
    gender == "female" &
    card %in% c("Visa_credit", "Mastercard_credit"))
count_subset1 <- nrow(subset1)

print(paste("There are", format(round(count_subset1, 0), big.mark = ","), 
            "female 18-25 credit card users."))

# 5) Older than 25, debit card, multiple products
subset2 <- subset(
  orange_data_clean,
  age_bracket != "18-25" &
    grepl("debit", card) &
    owns_multiple_products == 1)
percentage_subset2 <- round(100 * nrow(subset2) / nrow(orange_data_clean), 2)

print(
  paste0(
  "The percentage of +25 y.o. customers owning multiple products purchased with a debit card is ", 
  percentage_subset2, "%"))

# 6) Customers with all four products
owns_all_products <- sum(
  rowSums(orange_data_clean[product_cols] == 1) == length(product_cols))

print(paste0("There are ", owns_all_products, " customers who own all four products"))

###############################################
# Question 7: Average products by loyalty tier
###############################################
# Sorting tiers logically
orange_data_clean$tier <- factor(
  orange_data_clean$tier,
  levels = c("silver", "gold", "platinum")
)

# Calculating average products owned by loyalty tier
avg_products_tier <- orange_data_clean %>%
  group_by(tier) %>%
  summarise(Average_Products = mean(num_products))

print(avg_products_tier)

#################################
# Question 8: Compute revenues
#################################
# Calculating each customer's sales revenues dynamically
orange_data_clean <- orange_data_clean %>%
  mutate(
    sales_revenue = as.numeric(as.matrix(select(., all_of(product_cols))) %*% product_prices)
  )

# a) Total revenues
total_revenues <- sum(orange_data_clean$sales_revenue)
cat("Total revenues: ", format(round(total_revenues, 0), big.mark = ","))

# b) Month with highest sales
# Adding month column
orange_data_clean$month_of_purchase <- month(ymd(orange_data_clean$date))

# Calculating total revenue by month
monthly_rev <- orange_data_clean %>%
  group_by(month_of_purchase) %>%
  summarise(total_revenue = sum(sales_revenue))

# Deriving month with highest sales
max_row <- monthly_rev[which.max(monthly_rev$total_revenue), ]

print(paste0("Month ", max_row$month_of_purchase, 
    " has the highest total revenue of €",
    format(round(max_row$total_revenue, 0), big.mark = ",")))

# c) Revenues from each product
# Calculating total revenues from each product
Sales_Revenue <- sapply(product_cols, function(prod) {
  sum(orange_data_clean[[prod]]) * product_prices[prod]
})

# Creating a clean data frame
total_sales_by_product <- data.frame(
  Product = unname(product_cols), 
  Sales_Revenue = unname(Sales_Revenue) #to remove vector names
)

# Formatting the revenue numbers with commas
total_sales_by_product$Sales_Revenue <- format(
  round(total_sales_by_product$Sales_Revenue, 0),
  big.mark = ","
)

print(total_sales_by_product)

######################################
# Question 9 : Charts visualizations
######################################

# a) Bar chart: Monthly Sales Revenue

# Identifying index of the highest revenue month
max_index <- which.max(monthly_rev$total_revenue)

# Defining colors and highlighting the max bar in a different one
bar_colors <- rep("darkorange", nrow(monthly_rev))
bar_colors[max_index] <- "red2"   

# Plotting the bar chart 
par(mar = c(4, 2, 3, 2))
barplot(
  monthly_rev$total_revenue,
  names.arg = monthly_rev$month_of_purchase,
  col = bar_colors,
  main = "Monthly Sales Revenue (€)",
  ylab = "Revenue (€)",
  xlab = "Month",
  cex.axis = 1,
  cex.names = 0.8)

# b) Pie chart

# Computing percentage labels
labels_percent <- paste0(
  round(100 * total_sales_by_product$Sales_Revenue / sum(total_sales_by_product$Sales_Revenue), 1), "%"
)

# Defining colors
slice_colors <- c("brown2", "cornsilk", "darkgoldenrod1", "darkorange1")

# Ploting pie chart with percentages on slices
pie3D(
  total_sales_by_product$Sales_Revenue,
  labels = labels_percent,
  main = "Share of Total Revenue by Product",
  col = slice_colors,
  radius = 1,
  explode = 0.1,
  )

# Adding vertical legend
legend("topright",                      
       legend = total_sales_by_product$Product,
       fill = slice_colors,
       cex = 0.8,
       title = "Products")

#############################################
# Question 10: young oPhone owners insights
#############################################

# Filtering and saving young oPhone owners
young_oPhone_owners <- subset(
  orange_data_clean,
  age_bracket == "18-25" & oPhone == 1
)

#a) Most preferred card type among young_oPhone_owners
preferred_card <- names(sort(table(young_oPhone_owners$card), decreasing = TRUE))[1]
print(preferred_card)

# b) Average number of products purchased
avg_products_young <- round(mean(young_oPhone_owners$num_products), 2)
print(avg_products_young)

print(paste0("Young oPhone owners mostly use ", preferred_card, 
             " card and own on average ", avg_products_young, " products."))

###########################################
# Question 11: Product ownership by gender
###########################################
# 1) Calculating average number of products owned by gender
avg_products_gender <- orange_data_clean %>%
  group_by(gender) %>%
  summarise(avg_products = round(mean(num_products), 3),
            pctg_multi_product = round(mean(owns_multiple_products) * 100, 1))

print(avg_products_gender)

# 2) Investigating preferred card type among oTV owners aged 25+
# Filtering and saving older oTV owners
older_oTV_owners <- subset(
  orange_data_clean,
  age_bracket != "18-25" & oTV == 1
)

# Most preferred card type among older oTV owners
older_preferred_card <- names(sort(table(older_oTV_owners$card), decreasing = TRUE))[1]

print(paste0("oTV owners older than 25 mostly use ", older_preferred_card, 
             " card"))


#############################################################
# Question 12: 10% discount on customers owning 3+ products
#############################################################

# Calculating revenues by segment
revenue_by_segment <- orange_data_clean %>%
  group_by(num_products) %>%
  summarise(
    total_customers = n(),
    avg_revenue = mean(sales_revenue),
    total_revenue = sum(sales_revenue),
    .groups = "drop"
  )

# Adding segment label
revenue_by_segment <- revenue_by_segment %>%
  mutate(
    segment = ifelse(num_products >= 3, "3+ products", paste0(num_products, " product(s)"))
  )

# Total revenue before discount
total_before <- sum(revenue_by_segment$total_revenue)

# Applying 10% discount for existing 3+ product customers
revenue_by_segment <- revenue_by_segment %>%
  mutate(
    total_revenue_discount = ifelse(segment == "3+ products", total_revenue * 0.9, total_revenue)
  )

# Total revenue after discount (before upgrade)
total_after_discount <- sum(revenue_by_segment$total_revenue_discount)

# Simulating upgrade scenario (20% of 2-product customers buy a 3rd product)
upgrade_rate <- 0.2
num_upgrade <- revenue_by_segment$total_customers[revenue_by_segment$num_products == 2] * upgrade_rate
avg_revenue_3 <- revenue_by_segment$avg_revenue[revenue_by_segment$num_products == 3]

# Revenue added from upgrades (applying 10% discount)
added_revenue <- num_upgrade * avg_revenue_3 * 0.9

# Total revenue after discount and upgrade
total_after_upgrade <- total_after_discount + added_revenue

# Printing results
cat("Total revenue before discount: €", format(round(total_before,0), big.mark=","), "\n")
cat("Total revenue after discount (before 20% upgrade of 2-product customers): €", format(round(total_after_discount,0), big.mark=","), "\n")
cat("Total revenue after discount (after 20% upgrade of 2-product customers): €", 
    format(round(total_after_upgrade,0), big.mark=","), "\n")

##################################
# End of the Assignment
##################################
