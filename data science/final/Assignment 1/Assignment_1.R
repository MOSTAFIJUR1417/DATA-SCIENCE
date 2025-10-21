


library('dplyr')

#file <- read.csv('data/train.csv')


file <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Assignment 1/data/train.csv")



print(file)

summary(file)
colSums(is.na(file))

q1 <- quantile(file$Monthly.Income, 0.25)
q3 <- quantile(file$Monthly.Income, 0.75)
iqr <- q3 - q1

lowerboundary <- q1 - 1.5 * iqr
upperboundary <- q3 + 1.5 * iqr
cat("IQR value: ", iqr)
cat("Lower Boundary: ", lowerboundary)
cat("Upper Boundary: ", upperboundary)
cat("Total data: ", length(file$Monthly.Income), "\n")
cat("Count lower than lower bound: ", sum(file$Monthly.Income < lowerboundary), "\n")
cat("Count higher than upper bound: ", sum(file$Monthly.Income > upperboundary), "\n")

file <- file[file$Monthly.Income <= upperboundary, ]
cat("Count higher than upper bound: ", sum(file$Monthly.Income > upperboundary), "\n")

colSums(is.na(file))

count(file)

file$Employee.ID <- NULL

length(file)
head(file,10)






catagorical_columns <- names(file)[sapply(file, function(col) is.factor(col) || is.character(col))]
catagorical_columns <- setdiff(catagorical_columns, "Attrition")

chi_results <- data.frame(
  Variable=character(),
  Target = character(),
  Chi_Square = numeric(),
  DF = integer(),
  P_Value = numeric()
)

for (column in catagorical_columns)
{
  tabulate_data <- table(file[[column]], file$Attrition)
  chi_result <- chisq.test(tabulate_data)
  chi_results <- rbind(chi_results, data.frame(
    Variable = column,
    Target = "Attrition",
    Chi_result = chi_result$statistic,
    DF = chi_result$parameter,
    P_Value = (chi_result$p.value)
    
    
  ))
}


print(chi_results)






numeric_columns <- names(file)[sapply(file, function(x) is.numeric(x) || is.integer(x))]
print(numeric_columns)



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


print(anova_results)



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


print(anova_results)



pearson_spearman <- data.frame(
  Variable = character(),
  Target = character(),
  Pearson_correlation = numeric(),
  Spearman_correlation = numeric()
)


for( column in numeric_columns)
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









