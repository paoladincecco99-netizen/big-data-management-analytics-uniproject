##################################
# Big Data Management & Analytics
# Session 2: Programming Workshop
##################################



# Your working directory
getwd()                   # where R reads/writes files by default
setwd("/path/to/folder")  # <- adjust on your device


###############################
# 1. Basic operations
###############################

# Arithmetic
1 + 2
5 - 3
4 * 2
7 / 3
8 ^ 2 # 8 ** 2

# Integer division and modulus
8 %% 3      # remainder (modulus)
11 %/% 3    # integer division


# Assignment operator
x = 2  # assigns a value of 2 to x
x <- 3 # replaces the value of x from 2 to 3 (<- preferred over =)


x <- 1
y <- 2
z <- 3

# Logical operators (return TRUE or FALSE)

x < y     # Less than
x <= y    # Less than or equal to
y > z     # Greater than
y >= z    # Greater than or equal to
x == y    # Exactly equal to
x != y    # Not equal to

!(x > y)          # NOT operator – reverses TRUE/FALSE
(x >= 2) | (y >= 2)  # OR operator – TRUE if either condition is TRUE
(x >= 2) & (y >= 2)  # AND operator – TRUE only if both are TRUE

isTRUE(x < y)     # Checks if expression is exactly TRUE

# Floating point caveat: avoid exact equality, use all.equal()
0.1 + 0.2 == 0.3        # likely FALSE due to rounding, so use the function:
all.equal(0.1 + 0.2, 0.3)

# Special values in R: NA, NaN, Inf, -Inf

c(1, NA, 3)       # NA = missing value
mean(c(1, NA, 3))             # NA
mean(c(1, NA, 3), na.rm=TRUE) # ignore NAs

0/0      # NaN = undefined (0 divided by 0)
1/0      # Inf = positive infinity
-1/0     # -Inf = negative infinity

is.na(NA)    # TRUE
is.nan(NaN)  # TRUE
is.infinite(1/0)  # TRUE
is.finite(1/0)    # FALSE

is.na(NA)   # TRUE

###############################
# 2. Packages
###############################

# List attached packages


# Install (only once)
install.packages("ggplot2")
install.packages(c("dplyr", "tidyr")) #install multiple at once


# Loading a package to use at the beginning of each new session
library(ggplot2)

# Easier way to install packages is from the packages tab on
# bottom right window pane


###############################
# 3. Data types
###############################

# Numeric types
x_num <- 13.5       # double (floating point)
x_int <- 10L        # L makes an integer literal
x_cplx <- 1 + 3i    # complex

# Character (string)
day <- "Thursday"
floor_chr <- "9"    # character, not numeric
name <- 'Sameer'    # single quotes also OK

# Logical (boolean)
is_employed <- TRUE  # TRUE/FALSE preferred over T/F 
is_prime <- FALSE

# Type checking & coercion
is.numeric(x_num)
as.numeric(floor_chr)   # coercion; returns 9
?function #explains in help panel what the function does

###############################
# 4. Data structures
###############################

## 4.1 Vectors (1D, same data type)

# Combine elements with c() — short for "combine" or "concatenate"
colors <- c("red", "green", "blue")       # character vector
distances <- c(2.4, 3.0, 8.1, 152.9, 30.5) # numeric vector
print(colors)
print(distances)

# Built-in vectors
letters      # lowercase letters a–z
LETTERS      # uppercase A–Z
1:10         # sequence of integers 1 to 10

# Repetition and sequences
rep(3.5, 5)                 # repeat numeric
rep("data", 3)              # repeat character
seq(from = 2, to = 10, by = 2)         # even numbers
seq(from = 1, by = 2, length.out = 10) # first 10 odd numbers

# Indexing (R starts at 1, python at )
distances[1]          # first element
distances[2:4]        # elements 2 to 4
distances[c(1, 3, 5)] # specific positions
distances[-2]         # all except the 2nd

# Logical indexing (powerful filtering)
distances[distances > 5]
long_distances <- distances > 5  # logical vector (TRUE/FALSE)

# Naming elements
city_ratings <- c(Rotterdam = 4.5, Amsterdam = 4.6, Paris = 3.9)
city_ratings["Amsterdam"]
city_ratings[c("Amsterdam", "Paris")]

# Modify elements
colors[2:3] <- c("pink", "yellow")
distances[distances < 5] <- 0
length(distances)  # number of elements

# Factors (categorical variables): To represent categories or groups (e.g., regions, gender, species)

zip_codes <- c("PA", "PA", "BN", "PA", "LB", "BN", "LB")  
# Character vector of region codes

zip_factored <- factor(zip_codes)  
# Convert character vector into a factor (categorical variable)
# R stores unique values as "levels" and treats them as categories

zip_factored        # Displays each value with its factor levels
levels(zip_factored) # Shows the distinct category levels: "BN", "LB", "PA"

# levels or categories are created alphabetically. You can change that. 


# Ordered factors (have order)
sizes <- factor(c("S","M","L","M"), 
                levels = c("S","M","L"), 
                ordered = TRUE)


## 4.2 Matrices (2D, same data type)

# Create a sales matrix 
sales_matrix <- matrix(1:9, nrow = 3, ncol = 3)
sales_matrix

# Fill by row instead of column
region_matrix <- matrix(1:8, nrow = 4, ncol = 2, byrow = TRUE)

# Name rows/columns
colnames(region_matrix) <- c("Product_A", "Product_B")
rownames(region_matrix) <- c("North", "South", "East", "West")
region_matrix

# Combine vectors (e.g., new sales data)
q1_sales <- c(100, 120, 130)
q2_sales <- c(140, 160, 180)
sales_combined_col <- cbind(q1_sales, q2_sales)  # combine columns
sales_combined_row <- rbind(q1_sales, q2_sales)  # combine rows

# Access
sales_matrix[2, 3]       # row 2, col 3
sales_matrix[1, ]        # full row
sales_matrix[, 2:3]      # columns 2–3
sales_matrix[-1, 2]      # exclude 1st row, select col 2

# Modify values
sales_matrix[1:2, ] <- 1
sales_matrix <- cbind(sales_matrix, c(10, 11, 12))       # add column
sales_matrix <- rbind(sales_matrix, rep(0, ncol(sales_matrix)))  # add row

# Matrix operations
t(sales_matrix)                   # transpose
sales_matrix %*% t(sales_matrix)  # matrix multiplication
diag(1, 4, 4)                     # identity matrix

# Reshape
dim(sales_matrix) <- c(4, 4)
sales_matrix


## 4.3 Arrays (≥3D)

# Create a 3D array of quarterly sales (3 regions × 3 products × 3 quarters)
region_array <- array(1:27, dim = c(3, 3, 3))
region_array[, , 1]  # first quarter data


## 4.4 Lists (heterogeneous containers)

# Store info for one business location
product_colors <- c("blue", "red", "orange")
product_names  <- c("apple", "banana", "mango")
is_active      <- c(TRUE, FALSE, TRUE)
store_ids      <- c(101, 205)

store_list <- list(
   colors = product_colors,
   products = product_names,
   active_status = is_active,
   store_id = store_ids
)
store_list

# Access
store_list$products   # by name
store_list[[1]]       # by position


## 4.5 Data frames (2D, mixed types)

# Create car inventory dataset
car_model    <- c("Merc 240", "Honda Civic", "Cadillac Fleetwood")
horse_power  <- c(62, 52, 205)
carburetors  <- c(1, 1, 0)
is_available <- c(FALSE, TRUE, FALSE)

car_inventory <- data.frame(
   car_model, horse_power, carburetors, is_available,
   stringsAsFactors = FALSE #whichever strings are there, not considered as factors
)
car_inventory

# Access & filter
car_inventory$horse_power
car_inventory[1, ]
car_inventory[, "car_model"]
car_inventory[car_inventory$horse_power > 100, ]

# Modify
car_inventory$new_column <- c("A", "B", "C")  # add a
car_inventory$new_column <- NULL               # remove

# Built-in dataset example: iris
# (Contains measurements of 150 flowers, 3 species)

head(iris)     # first 6 rows
tail(iris)     # last 6 rows
summary(iris)  # basic statistics for each column
dim(iris)      # number of rows and columns
names(iris)    # column names
str(iris)      # structure: shows types and sample data


###############################
# 5. Importing and exporting data
###############################

install.packages(c("readr", "writexl"))
library(readr)     # for CSV import/export
library(writexl)   # for writing Excel files

# read the data first to see

readLines("patient_data.csv", n = 3) #read first three rows

patient_data <- read_csv2("patient_data.csv")

patient_data  <- as.data.frame(patient_data)

# check missing values

(is.na(patient_data))
sum(is.na(patient_data))
colSums((is.na(patient_data)))

complete.cases(patient_data) # find complete cases
patient_data_clean <- patient_data[complete.cases(patient_data), ] # keep only complete rows

# good practice

patient_data <- read_csv2("patient_data.csv", na = c("", "NA", "NULL", "-", "N/A"))

## 5.1 Manipulating data frames


patient_data_clean$RiskScore <- ifelse(patient_data_clean$age > 50, "High", "Low")  # Add a new derived column (e.g., RiskScore)

set.seed(123) # important for reproducibility to set number of elements to generate
patient_data_clean$Gender <- sample(c("Male", "Female"), 
                                    size = nrow(patient_data_clean), 
                                    replace = TRUE) # Randomly assign "Male" or "Female" to each observation


# adding new observations

new_patient <- data.frame(
   id = 101,
   age = 45,
   status = "Recovered",
   RiskScore = "Low",
   Gender = "Female"
)

patient_data_clean <- rbind(patient_data_clean, new_patient)

# remove column

patient_data_clean$RiskScore <- NULL

# subsetting dataframes

subset1 <- patient_data_clean[, c("id", "age", "status")] # Select specific columns

subset2 <- patient_data_clean[1:5, ] # Select specific rows (first 5)

subset3 <- subset(patient_data_clean, age > 40) # patients older than 40

subset4 <- subset(patient_data_clean, status == "Recovered") # recovered patients

subset5 <- subset(patient_data_clean, age > 40 & status == "Recovered") # multiple conditions



###############################
# 6. Control flow & functions
###############################

# if / else example — customer discount
purchase_amount <- 120

if (purchase_amount > 100) {
   discount <- "Eligible for 10% discount"
} else {
   discount <- "No discount"
}
discount


# for loop — calculate total sales for 5 stores
store_sales <- c(1200, 950, 1800, 2200, 1600)
adjusted_sales <- rep(0, 5)

for (i in 1:5) {
   adjusted_sales[i] <- store_sales[i] * 1.05  # 5% growth projection
}
adjusted_sales


# while loop — reaching sales target
total_sales <- 0
month <- 0

while (total_sales < 10000) {
   month <- month + 1
   total_sales <- total_sales + 1800  # average monthly sales
}
list(months_needed = month, total_sales = total_sales)


# Functions
square <- function(x) {
   # input: numeric x
   # output: x squared
   return(x^2)
}
square(4)


###############################
# 7. Dates & times
###############################

# Base R dates
d1 <- as.Date("2025-01-15")
d2 <- as.Date("2025-02-05")
as.numeric(d2 - d1)   # difference in days

# lubridate — easiest way to parse & handle dates/times
install.packages("lubridate")
library(lubridate)

# Parse dates in different formats
ymd("2025-10-29")   # year-month-day
mdy("10/29/2025")   # month-day-year
dmy("29-10-2025")   # day-month-year

# Current date/time
now()               # current date-time 
today()             # date only

# Extract or modify parts
year(now()); month(now()); wday(now(), label = TRUE)

# Round or floor to nearest unit
floor_date(now(), unit = "month")   # start of current month
ceiling_date(now(), unit = "week")  # start of next week


###############################
# 8. Basic statistics
###############################

mean(iris$Petal.Length)
median(iris$Petal.Length)
sd(iris$Petal.Length)
var(iris$Petal.Length)
cor(iris$Sepal.Length, iris$Sepal.Width)
quantile(iris$Sepal.Length, probs = c(0.1, 0.5, 0.9))

###############################
# 9. Plotting in R
###############################

# Sample business data
sales <- c(1500, 2300, 1800, 2100, 2600)
months <- c("Jan", "Feb", "Mar", "Apr", "May")
revenue <- c(12.5, 15.2, 13.8, 17.4, 19.1)
regions <- c("North", "South", "East", "West", "Central")


## 10.1 Simple line plot 

plot(sales, type = "o", col = "blue", pch = 16,
     xlab = "Month", ylab = "Sales ($)",
     main = "Monthly Sales Trend",
     xaxt = "n")
axis(1, at = 1:5, labels = months)


## 10.2 Bar plot

barplot(sales,
        names.arg = months,
        col = "skyblue",
        main = "Monthly Sales",
        ylab = "Sales ($)",
        border = NA)


## 10.3 Histogram 

hist(revenue,
     col = "lightgreen",
     main = "Revenue Distribution",
     xlab = "Revenue ($ thousands)")


## 10.4 Boxplot 

boxplot(revenue,
        main = "Revenue Spread",
        ylab = "Revenue ($ thousands)",
        col = "lightpink")


## 10.5 Pie chart 

region_share <- c(25, 20, 30, 15, 10)
pie(region_share, labels = regions,
    main = "Sales Share by Region",
    col = rainbow(length(regions)))


## 10.6 Scatter plot 

plot(sales, revenue,
     main = "Sales vs Revenue",
     xlab = "Sales ($)",
     ylab = "Revenue ($ thousands)",
     pch = 19, col = "darkorange")

abline(lm(revenue ~ sales), col = "red", lwd = 2)





