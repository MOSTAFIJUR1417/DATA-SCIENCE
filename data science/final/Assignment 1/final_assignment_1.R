

#------------------>>>Data Pre Processing  <<<----------------------

library('dplyr')

file <- read.csv('data/train.csv')


file <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Assignment 1/data/train.csv")


print(file)

summary(file)
colSums(is.na(file))

count(file)

file$Employee.ID <- NULL

length(file)
head(file,10)



# -------------------------- < Chi-Square > -------------------------
# Create a contingency table
table_data <- table(file$Employee.Recognition, file$Attrition)


chi_result <- chisq.test(table_data)
print(chi_result)


unique(file$Number.of.Promotions)



table_data <- table(file$Employee.Recognition, file$Attrition)

chi_result <- chisq.test(table_data)

print(chi_result)

print("Observed:")
print(chi_result$observed)

print("Expected:")
print(chi_result$expected)

print("Chi-square contributions per cell:")
print((chi_result$observed - chi_result$expected)^2 / chi_result$expected)



table_data <- table(file$Attrition, file$Employee.Recognition)  

chi_result <- chisq.test(table_data)
chi_result

chi_result$statistic
chi_result$parameter
chi_result$method
chi_result$p.value
print(chi_result)

print("Observed:")
print(chi_result$observed)

print("Expected:")
print(chi_result$expected)

print("Chi-square contributions per cell:")
print((chi_result$observed - chi_result$expected)^2 / chi_result$expected)


columns <-colnames(file)
for (i in colnames(file)) {
  print(i)
}


# Apply Chi squere



  #---------------------------------------------------------------------------------#
  
  # Identify categorical columns
  categorical_cols <- names(file)[sapply(file, function(col) is.factor(col) || is.character(col))]
  categorical_cols <- setdiff(categorical_cols, "Attrition")  # Exclude the target column
  
  # Initialize an empty results data frame
  chi_results_df <- data.frame(
    Variable = character(),
    Target = character(),
    Chi_Square = numeric(),
    DF = integer(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through and apply Chi-square test
  for (col_name in categorical_cols) {
    table_data <- table(file[[col_name]], file$Attrition)
    chi_result <- chisq.test(table_data)
    
    # Append result to the dataframe
    chi_results_df <- rbind(chi_results_df, data.frame(
      Variable = col_name,
      Target = "Attrition",
      Chi_Square = chi_result$statistic,
      DF = chi_result$parameter,
      P_Value = chi_result$p.value
    ))
  }
  
  file["Attrition"]
  file[["Attrition"]]
  file$Attrition
  # Print results
  print(chi_results_df)
  
  
  
  
  #---------------------------------------------------------------------------------#
  
  
  chatagorical_columns <- names(file)[sapply(file, function(col) is.factor(col) || is.character(col))]
  chatagorical_columns <- setdiff(chatagorical_columns, "Attrition")
  
  chi_results <- data.frame(
    Variable=character(),
    Target = character(),
    Chi_Square = numeric(),
    DF = integer(),
    P_Value = numeric()
  )
  
  for (column in chatagorical_columns)
  {
    tabulate_data <- table(file[[column]], file$Attrition)
    chi_result <- chisq.test(tabulate_data)
    chi_results <- rbind(chi_results, data.frame(
      Variable = column,
      Target = "Attrition",
      Chi_result = chi_result$statistic,
      DF = chi_result$parameter,
      P_Value = round(chi_result$p.value, 8)

      
    ))
  }
  
  
print(chi_results)
  

file$Attrition <- ifelse(file$Attrition =="Stayed", 1,0)
  
numeric_column <- names(file)[sapply(file, function(x) is.numeric(x) || is.integer(x))]
print(numeric_column)
  
pearson_spearman <- data.frame(
  Variable = character(),
  Target = character(),
  Pearson_correlation = numeric(),
  Spearman_correlation = numeric()
)

for( column in numeric_column)
{
  pearson <- cor(file[[column]], file$Attrition, method="pearson")
  spearman <- cor(file[[column]], file$Attrition, method="spearman")
  pearson_spearman <-rbind(pearson_spearman, data.frame(
    Variable = column,
    Target = "Attrition",
    Pearson_correlaton = pearson,
    Spearman_correlation = spearman
  ))
}
  
pearson_spearman
  
  
  
  
  
  
  
  
  
  
  

file$PromotionCategory <- ifelse(file$Number.of.Promotions == 0, "None",
                                 ifelse(file$Number.of.Promotions == 1, "Few",
                                        ifelse(file$Number.of.Promotions == 2, "Some",
                                               ifelse(file$Number.of.Promotions >= 3, "Many", NA))))

unique(file$PromotionCategory)



table_data <- table(file$PromotionCategory, file$Attrition)


chi_result <- chisq.test(table_data)
print(chi_result)



# Convert to factor for categorical handling
file$PromotionCategory <- as.factor(file$PromotionCategory)

# View the unique categories
unique(file$PromotionCategory)

file
#-------------------<< Apply Pearson >>-------------------

unique(file$Attrition)
file$Attrition <- ifelse(file$Attrition == 'Stayed', 1, 0)  # "Stayed" "Left"
unique(file$Attrition)


pearson_cor_age <- cor(file$Age, file$Attrition, method = "pearson")
pearson_cor_year <- cor(file$Years.at.Company, file$Attrition, method = "pearson")
pearson_cor_income <- cor(file$Monthly.Income, file$Attrition, method = "pearson")
pearson_cor_promotions <- cor(file$Number.of.Promotions, file$Attrition, method = "pearson")
pearson_cor_distance <- cor(file$Distance.from.Home, file$Attrition, method = "pearson")
pearson_cor_department <- cor(file$Number.of.Dependents, file$Attrition, method = "pearson")
pearson_cor_tenure <- cor(file$Company.Tenure, file$Attrition, method = "pearson")

cat("Pearson correlation of Age: ", pearson_cor_age, "\n")
cat("Pearson correlation of Years at Company: ", pearson_cor_year, "\n")
cat("Pearson correlation of Monthly Income: ", pearson_cor_income, "\n")
cat("Pearson correlation of Number of Promotions: ", pearson_cor_promotions, "\n")
cat("Pearson correlation of Distance from Home: ", pearson_cor_distance, "\n")
cat("Pearson correlation of Number of Dependents: ", pearson_cor_department, "\n")
cat("Pearson correlation of Company Tenure: ", pearson_cor_tenure, "\n")

#-------------------------<<< Spearman Correlation>>> --------------

spearman_cor_age <- cor(file$Age, file$Attrition, method = "spearman")
spearman_cor_year <- cor(file$Years.at.Company, file$Attrition, method = "spearman")
spearman_cor_income <- cor(file$Monthly.Income, file$Attrition, method = "spearman")
spearman_cor_promotions <- cor(file$Number.of.Promotions, file$Attrition, method = "spearman")
spearman_cor_distance <- cor(file$Distance.from.Home, file$Attrition, method = "spearman")
spearman_cor_department <- cor(file$Number.of.Dependents, file$Attrition, method = "spearman")
spearman_cor_tenure <- cor(file$Company.Tenure, file$Attrition, method = "spearman")

cat("Spearman correlation of Age: ", spearman_cor_age, "\n")
cat("Spearman correlation of Years at Company: ", spearman_cor_year, "\n")
cat("Spearman correlation of Monthly Income: ", spearman_cor_income, "\n")
cat("Spearman correlation of Number of Promotions: ", spearman_cor_promotions, "\n")
cat("Spearman correlation of Distance from Home: ", spearman_cor_distance, "\n")
cat("Spearman correlation of Number of Dependents: ", spearman_cor_department, "\n")
cat("Spearman correlation of Company Tenure: ", spearman_cor_tenure, "\n")

#-------------------< Print Pearson vs Spearman >-----------------

cat("Age correlation: Pearson vs Spearman ->", pearson_cor_age, " vs ", spearman_cor_age, "\n")
cat("Years at Company correlation: Pearson vs Spearman ->", pearson_cor_year, " vs ", spearman_cor_year, "\n")
cat("Monthly Income correlation: Pearson vs Spearman ->", pearson_cor_income, " vs ", spearman_cor_income, "\n")
cat("Promotions correlation: Pearson vs Spearman ->", pearson_cor_promotions, " vs ", spearman_cor_promotions, "\n")
cat("Distance from Home correlation: Pearson vs Spearman ->", pearson_cor_distance, " vs ", spearman_cor_distance, "\n")
cat("Number of Dependents correlation: Pearson vs Spearman ->", pearson_cor_department, " vs ", spearman_cor_department, "\n")
cat("Company Tenure correlation: Pearson vs Spearman ->", pearson_cor_tenure, " vs ", spearman_cor_tenure, "\n")








# ------------------------<<< Before Work >>>------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------



#------------------>>>Data Pre Processing  <<<----------------------

library('dplyr')

file <- read.csv('data/train.csv')


file <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Assignment 1/data/train.csv")


print(file)

summary(file)
colSums(is.na(file))

count(file)

file$Employee.ID <- NULL

length(file)
head(file,10)



# ----------------------------------- < Chi-Square Correlation  > ---------------------------


catagorical_columns <- names(file)[sapply(file, function(col) is.factor(col) || is.character(col))]
catagorical_columns <- setdiff(chatagorical_columns, "Attrition")

chi_results <- data.frame(
  Variable=character(),
  Target = character(),
  Chi_Square = numeric(),
  DF = integer(),
  P_Value = numeric()
)

for (column in chatagorical_columns)
{
  tabulate_data <- table(file[[column]], file$Attrition)
  chi_result <- chisq.test(tabulate_data)
  chi_results <- rbind(chi_results, data.frame(
    Variable = column,
    Target = "Attrition",
    Chi_result = chi_result$statistic,
    DF = chi_result$parameter,
    P_Value = round(chi_result$p.value, 8)
    
    
  ))
}


print(chi_results)



#-----------------------------------------------------------------------------------#


anova_result <- aov( Number.of.Dependents ~ Attrition , data=file)
summary(anova_result)


numeric_column <- names(file)[sapply(file, function(x) is.numeric(x) || is.integer(x))]
print(numeric_column)


anova_results <- data.frame(
  Variable = character(),
  Target = character(),
  F_Value = numeric(),
  Pr(F) = numeric()
)

for (col in numeric_column)
{
  anova_result = aov(col ~ Attrition, data=file)
  anova_results <- rbind(anova_results, data.frame(
    Variable = col,
    Target = "Attrition",
    F_value = anova_result$fitted.values,
    Pr(F) = anova_result$residuals
  ))
}


# Get numeric columns
numeric_columns <- names(file)[sapply(file, is.numeric)]

# Prepare dataframe to store results
anova_results <- data.frame(
  Variable = character(),
  Target = character(),
  F_Value = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Run ANOVA for each numeric column
anova_results <- data.frame(
  Variable = character(),
  Target = character(),
  F_Value = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (col in numeric_columns) {
  formula <- as.formula(paste(col, "~ Attrition"))
  result <- summary(aov(formula, data = file))
  
  f_val <- result[[1]]$`F value`[1]
  p_val <- result[[1]]$`Pr(>F)`[1]
  
  anova_results <- rbind(anova_results, data.frame(
    Variable = col,
    Target = "Attrition",
    F_Value = round(f_val, 4),
    P_Value = round(p_val, 8),
    stringsAsFactors = FALSE
  ))
}



file$Attrition <- ifelse(file$Attrition =="Stayed", 1, 0)

for( col in catagorical_columns)
{
  formula <- as.formula(paste("Attrition ~ ", col))
  result <- summary(aov(formula, data = file))
  
  f_val <- result[[1]]$`F value`[1]
  p_val <- result[[1]]$`Pr(>F)`[1]
  
  anova_results <- rbind(anova_results, data.frame(
    Variable = col,
    Target = "Attrition",
    F_Value = round(f_val, 4),
    P_Value = round(p_val, 8),
    stringsAsFactors = FALSE
  ))
}
anova_results$F_Value <- round(anova_results$F_Value, 4)
anova_results$P_Value <- round(anova_results$P_Value, 8)


# View results sorted by significance
anova_results <- anova_results[order(anova_results$P_Value), ]
print(anova_results)














anova_result$model
anova_result$coefficients
unique(file$Number.of.Dependents)

file$Number.of.Dependents <- ifelse(file$Number.of.Dependents == 0, "none",
                                    ifelse(file$Number.of.Dependents == 1, "one",
                                           ifelse(file$Number.of.Dependents == 2, "two",
                                                  ifelse(file$Number.of.Dependents == 3, "three",
                                                         ifelse(file$Number.of.Dependents == 4, "four",
                                                                ifelse(file$Number.of.Dependents == 5, "five",
                                                                       ifelse(file$Number.of.Dependents == 6, "four",NA)))))))




file$Attrition <- ifelse(file$Attrition =="Stayed", 1,0)

anova_result <- aov( Attrition ~ Number.of.Dependents  , data=file)
summary(anova_result)

file$Attrition



#-----------------------------<< Pearson and Spearman correlation >>--------------------------------------------#



file$Attrition <- ifelse(file$Attrition =="Stayed", 1,0)

numeric_column <- names(file)[sapply(file, function(x) is.numeric(x) || is.integer(x))]
print(numeric_column)

pearson_spearman <- data.frame(
  Variable = character(),
  Target = character(),
  Pearson_correlation = numeric(),
  Spearman_correlation = numeric()
)

for( column in numeric_column)
{
  pearson <- cor(file[[column]], file$Attrition, method="pearson")
  spearman <- cor(file[[column]], file$Attrition, method="spearman")
  pearson_spearman <-rbind(pearson_spearman, data.frame(
    Variable = column,
    Target = "Attrition",
    Pearson_correlaton = pearson,
    Spearman_correlation = spearman
  ))
}

print(pearson_spearman)












#---------------------------------------------------------------------------------#





file$Attrition <- ifelse(file$Attrition =="Stayed", 1,0)

numeric_column <- names(file)[sapply(file, function(x) is.numeric(x) || is.integer(x))]
print(numeric_column)

pearson_spearman <- data.frame(
  Variable = character(),
  Target = character(),
  Pearson_correlation = numeric(),
  Spearman_correlation = numeric()
)

for( column in numeric_column)
{
  pearson <- cor(file[[column]], file$Attrition, method="pearson")
  spearman <- cor(file[[column]], file$Attrition, method="spearman")
  pearson_spearman <-rbind(pearson_spearman, data.frame(
    Variable = column,
    Target = "Attrition",
    Pearson_correlaton = pearson,
    Spearman_correlation = spearman
  ))
}

print(pearson_spearman)



















file$PromotionCategory <- ifelse(file$Number.of.Promotions == 0, "None",
                                 ifelse(file$Number.of.Promotions == 1, "Few",
                                        ifelse(file$Number.of.Promotions == 2, "Some",
                                               ifelse(file$Number.of.Promotions >= 3, "Many", NA))))

unique(file$PromotionCategory)



table_data <- table(file$PromotionCategory, file$Attrition)


chi_result <- chisq.test(table_data)
print(chi_result)



# Convert to factor for categorical handling
file$PromotionCategory <- as.factor(file$PromotionCategory)

# View the unique categories
unique(file$PromotionCategory)

file
#-------------------<< Apply Pearson >>-------------------


#-------------------------<<< Spearman Correlation>>> --------------







# ------------------------<<< Before Work >>>------------------------
unique(file$ChestPainType)
unique(file$RestingBP)
sum(file$RestingBP ==0)
sum(file$Cholesterol > 500)



quartiles <- quantile(file$Cholesterol, probs = c(0.25, 0.75), na.rm = TRUE)
Q1 <- quartiles[1]
Q3 <- quartiles[2]

IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")

cat("Number of rows less than lower boundary:", sum(file$Cholesterol < lower_bound), "\n")
cat("Number of rows more than upper boundary:", sum(file$Cholesterol > upper_bound), "\n")

mean <- mean(file$Cholesterol)
chol_mean <- mean(file$Cholesterol[file$Cholesterol != 0])

file$Cholesterol[file$Cholesterol == 0] <- chol_mean
summary(file$Cholesterol)

mean_bp <- mean(file$RestingBP)
file$RestingBP[file$RestingBP == 0 ] <- mean_bp
summary(file)


sum(file$Oldpeak < 0)
unique(file$Oldpeak[file$Oldpeak < 0])

file$Oldpeak[file$Oldpeak < 0] <- abs(file$Oldpeak[file$Oldpeak < 0])
summary(file$Oldpeak)

summary(file)
colSums(is.na(file))


#------------------>>>  Correlation between Attributes  <<<----------------------

# pearson correlation between Age and Heart Disease
pearson_cor <- cor(file$Years.at.Company, file$HeartDisease, method = "pearson")
pearson_cor

spearman_cor <- cor(file$Age, file$HeartDisease, method ="spearman")
spearman_cor


file$Sex <- ifelse(file$Sex == "M", 0, 1)
kendall_cor <- cor(file$Sex, file$HeartDisease, method ="kendall")
kendall_cor


file$SexNumeric <- ifelse(file$Sex == "M", 1, 0)
kendall_cor <- cor(file$Age, file$HeartDisease, method = "kendall")
kendall_cor



anova_result <- aov(HeartDisease ~ Sex, data = file)
summary(anova_result)



