# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# STEP 1: Read the data
data <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Assignment 1/data/train.csv")

# STEP 2: Data Preprocessing

# Convert necessary columns to factors
data$Gender <- as.factor(data$Gender)
data$Attrition <- as.factor(data$Attrition)
data$Job.Role <- as.factor(data$Job.Role)
data$Marital.Status <- as.factor(data$Marital.Status)
data$Education.Level <- as.factor(data$Education.Level)
data$Overtime <- as.factor(data$Overtime)

# Create a Sentiment column (positive for 'Stayed', negative for 'Left')
data <- data %>%
  mutate(Sentiment = ifelse(Attrition == "Stayed", "Positive", "Negative")) %>%
  mutate(Sentiment = as.factor(Sentiment))

# STEP 3: Visualizations

## 1. Histogram for Age (with value labels)
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  stat_bin(binwidth = 5, geom = "text", aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

## 2. Density Plot for Monthly Income by Attrition
ggplot(data, aes(x = Monthly.Income, color = Attrition)) +
  geom_density() +
  theme_minimal() +
  labs(title = "Density of Monthly Income by Attrition", x = "Monthly Income", y = "Density")

## 3. Box Plot for Monthly Income by Job Role
ggplot(data, aes(x = Job.Role, y = Monthly.Income, fill = Job.Role)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Monthly Income by Job Role", x = "Job Role", y = "Monthly Income")

## 4. Bar Plot for Gender (with value labels)
ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "orange", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

## 5. Bar Plot for Marital Status (with value labels)
ggplot(data, aes(x = Marital.Status)) +
  geom_bar(fill = "purple", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Marital Status Distribution", x = "Marital Status", y = "Count")

## 6. Sketch (Sentiment based on Attrition) with value labels
ggplot(data, aes(x = Sentiment, fill = Sentiment)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Sketch of Employee Sentiment", x = "Sentiment", y = "Count")

## 7. Box Plot of Distance from Home by Attrition
ggplot(data, aes(x = Attrition, y = Distance.from.Home, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Distance from Home by Attrition", x = "Attrition", y = "Distance")

## 8. Summary Statistics for Numerical Columns
num_data <- select(data, Age, Years.at.Company, Monthly.Income, Distance.from.Home)
print(summary(num_data))









library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Assignment 1/data/train.csv")

# Sample 50 rows once
data_sample <- sample_n(data, 100)

# Make sure factors are ordered (optional)
data_sample$Work.Life.Balance <- factor(data_sample$Work.Life.Balance, levels = c("Poor", "Fair", "Good", "Excellent"), ordered = TRUE)
data_sample$Job.Satisfaction <- factor(data_sample$Job.Satisfaction, levels = c("Medium","High","Very High","Low"), ordered = TRUE)
data_sample$Marital.Status <- factor(data_sample$Marital.Status, levels = c("Married", "Divorced", "Single"))

# Create separate data frames with jitter to avoid exact overlap
df_wlb <- data_sample %>%
  select(Monthly.Income, Age) %>%
  mutate(Type = "Work.Life.Balance",
         Monthly.Income = Monthly.Income + runif(n(), -500, 500),  # jitter horizontally
         Age = Age + runif(n(), -1, 1))                            # jitter vertically

df_js <- data_sample %>%
  select(Monthly.Income, Age) %>%
  mutate(Type = "Job.Satisfaction",
         Monthly.Income = Monthly.Income + runif(n(), -500, 500),
         Age = Age + runif(n(), -1, 1))

df_ms <- data_sample %>%
  select(Monthly.Income, Age) %>%
  mutate(Type = "Marital.Status",
         Monthly.Income = Monthly.Income + runif(n(), -500, 500),
         Age = Age + runif(n(), -1, 1))

# Combine
plot_data <- bind_rows(df_wlb, df_js, df_ms)

# Colors for each variable type
colors <- c("Work.Life.Balance" = "blue", "Job.Satisfaction" = "red", "Marital.Status" = "green")

# Plot
ggplot(plot_data, aes(x = Monthly.Income, y = Age, color = Type)) +
  geom_point(shape = 20, size = 3, alpha = 0.7) +
  scale_color_manual(values = colors) +
  labs(
    title = "Scatter Plot: Age vs Monthly Income with Three Variables (Jittered)",
    x = "Monthly Income",
    y = "Age",
    color = "Variable"
  ) +
  theme_minimal()


ggplot(data_sample, aes(x = Marital.Status, y = Monthly.Income, fill = Marital.Status)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1) +  # Boxplot inside
  theme_minimal() +
  labs(
    title = "Violin Plot: Attrition Distribution by Marital Status",
    x = "Marital Status",
    y = "Monthly.Income"
  ) +
  theme(legend.position = "none")  # Remove redundant legend


ggplot(data_sample, aes(x = Performance.Rating, y = Monthly.Income, fill = Performance.Rating)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1) +  # Boxplot inside
  theme_minimal() +
  labs(
    title = "Violin Plot: Attrition Distribution by Marital Status",
    x = "Performance.Rating",
    y = "Monthly.Income"
  ) +
  theme(legend.position = "none")  # Remove redundant legend


avg_worklife <- data_sample %>%
  group_by(Years.at.Company) %>%
  summarise(AvgWLBalance = mean(as.numeric(factor(Work.Life.Balance)), na.rm = TRUE))

ggplot(avg_worklife, aes(x = Years.at.Company, y = AvgWLBalance)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "orange", size = 2) +
  theme_minimal() +
  labs(title = "Average Work-Life Balance by Years at Company", x = "Years at Company", y = "Avg Work-Life Balance")











library(tibble)
library(fmsb)  # For radar chart
library(tidyr) # For data reshaping

# Make sure factors are still set (in case you start from here)
data$Gender <- as.factor(data$Gender)
data$Attrition <- as.factor(data$Attrition)
data$Job.Role <- as.factor(data$Job.Role)
data$Marital.Status <- as.factor(data$Marital.Status)
data$Education.Level <- as.factor(data$Education.Level)
data$Overtime <- as.factor(data$Overtime)
data <- data %>%
  mutate(Sentiment = ifelse(Attrition == "Stayed", "Positive", "Negative")) %>%
  mutate(Sentiment = as.factor(Sentiment))




data$Gender <- as.factor(data$Gender)
data$Attrition <- as.factor(data$Attrition)
data$Job.Role <- as.factor(data$Job.Role)
data$Marital.Status <- as.factor(data$Marital.Status)
data$Education.Level <- as.factor(data$Education.Level)
data$Overtime <- as.factor(data$Overtime)
data <- data %>%
  mutate(Sentiment = ifelse(Attrition == "Stayed", "Positive", "Negative")) %>%
  mutate(Sentiment = as.factor(Sentiment))





# ------------------------- 1. Multivariate Scatter Plot -------------------------
ggplot(data, aes(x = Total.Working.Years, y = Monthly.Income, color = Marital.Status, shape = Overtime)) +
  geom_point(alpha = 0.7, size = 3) +
  theme_minimal() +
  labs(title = "Income vs Total Working Years by Marital Status & Overtime", x = "Total Working Years", y = "Monthly Income")

# ------------------------- 2. Violin Plot -------------------------
ggplot(data, aes(x = Marital.Status, y = Age, fill = Marital.Status)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  theme_minimal() +
  labs(title = "Violin Plot: Age Distribution by Marital Status", x = "Marital Status", y = "Age")

# ------------------------- 3. Line Graph -------------------------
avg_worklife <- data %>%
  group_by(Years.at.Company) %>%
  summarise(AvgWLBalance = mean(as.numeric(factor(Work.Life.Balance)), na.rm = TRUE))

ggplot(avg_worklife, aes(x = Years.at.Company, y = AvgWLBalance)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "orange", size = 2) +
  theme_minimal() +
  labs(title = "Average Work-Life Balance by Years at Company", x = "Years at Company", y = "Avg Work-Life Balance")

# ------------------------- 4. Radar Chart (More Variables) -------------------------
radar_data2 <- data %>%
  group_by(Sentiment) %>%
  summarise(
    Age = mean(Age, na.rm = TRUE),
    TotalWorkingYears = mean(Total.Working.Years, na.rm = TRUE),
    EnvironmentSatisfaction = as.numeric(mean(as.numeric(factor(Environment.Satisfaction)), na.rm = TRUE)),
    TrainingTimesLastYear = mean(Training.Times.Last.Year, na.rm = TRUE),
    YearsWithCurrManager = mean(Years.With.Cur.Manager, na.rm = TRUE)
  ) %>%
  column_to_rownames("Sentiment")

radar_data_scaled2 <- rbind(
  apply(radar_data2, 2, max),
  apply(radar_data2, 2, min),
  radar_data2
)

radarchart(
  radar_data_scaled2,
  axistype = 1,
  pcol = c("darkorange", "darkblue"),
  pfcol = adjustcolor(c("darkorange", "darkblue"), alpha.f = 0.3),
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  caxislabels = seq(0, max(radar_data_scaled2), length.out = 5),
  cglwd = 0.8,
  vlcex = 0.9,
  title = "Radar Chart: Avg Metrics by Sentiment (Set 2)"
)

legend("topright", legend = rownames(radar_data2), col = c("darkorange", "darkblue"), pch = 20, pt.cex = 1.5, bty = "n")

# 1. Multivariate Scatter Plot (Monthly Income vs Age by Job Role and Gender)
ggplot(data, aes(x = Age, y = Monthly.Income, color = Job.Role, shape = Gender)) +
  geom_point(alpha = 0.7, size = 3) +
  theme_minimal() +
  labs(title = "Income vs Age by Job Role & Gender", x = "Age", y = "Monthly Income")

# 2. Violin Plot (Monthly Income by Education Level)
ggplot(data, aes(x = Education.Level, y = Monthly.Income, fill = Education.Level)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  theme_minimal() +
  labs(title = "Violin Plot: Monthly Income by Education Level", x = "Education Level", y = "Monthly Income")

# 3. Line Graph (Average Income by Years at Company)
avg_income <- data %>%
  group_by(Years.at.Company) %>%
  summarise(AvgIncome = mean(Monthly.Income, na.rm = TRUE))

ggplot(avg_income, aes(x = Years.at.Company, y = AvgIncome)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Average Monthly Income by Years at Company", x = "Years at Company", y = "Average Monthly Income")




# Step 1: Prepare summarized data
radar_data <- data %>%
  group_by(Sentiment) %>%
  summarise(
    MonthlyIncome = mean(Monthly.Income, na.rm = TRUE),
    JobSatisfaction = as.numeric(mean(as.numeric(factor(Job.Satisfaction)), na.rm = TRUE)),
    WorkLifeBalance = as.numeric(mean(as.numeric(factor(Work.Life.Balance)), na.rm = TRUE)),
    PerformanceRating = as.numeric(mean(as.numeric(factor(Performance.Rating)), na.rm = TRUE)),
    DistanceFromHome = mean(Distance.from.Home, na.rm = TRUE)
  ) %>%
  column_to_rownames("Sentiment")

# Step 2: Normalize the data for radar chart
# fmsb requires the first row to be max and second to be min
radar_data_scaled <- rbind(
  apply(radar_data, 2, max),
  apply(radar_data, 2, min),
  radar_data
)

# Step 3: Plot Radar Chart
colors_border <- c("blue", "red")
colors_in <- adjustcolor(colors_border, alpha.f = 0.3)

radarchart(
  radar_data_scaled,
  axistype = 1,
  pcol = colors_border,
  pfcol = colors_in,
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  caxislabels = seq(0, max(radar_data_scaled), length.out = 5),
  cglwd = 0.8,
  vlcex = 0.9,
  title = "Radar Chart: Average Metrics by Sentiment"
)

legend(
  x = "topright",
  legend = rownames(radar_data),
  bty = "n",
  pch = 20,
  col = colors_border,
  text.col = "black",
  cex = 0.9,
  pt.cex = 1.5
)








# ------------------------- SETUP -------------------------
library(dplyr)
library(ggplot2)
library(fmsb)
library(tibble)

# Set factors and Sentiment
data$Gender <- as.factor(data$Gender)
data$Attrition <- as.factor(data$Attrition)
data$Job.Role <- as.factor(data$Job.Role)
data$Marital.Status <- as.factor(data$Marital.Status)
data$Education.Level <- as.factor(data$Education.Level)
data$Overtime <- as.factor(data$Overtime)
data <- data %>%
  mutate(Sentiment = ifelse(Attrition == "Stayed", "Positive", "Negative")) %>%
  mutate(Sentiment = as.factor(Sentiment))

data

names(data)



ggplot(data, aes(x = Monthly.Income, y = Age)) +
  geom_point(aes(color = factor(Work.Life.Balance), size = Job.Satisfaction, shape = Marital.Status), alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Multivariate Scatter Plot",
    x = "Monthly Income",
    y = "Age",
    color = "Work-Life Balance",
    size = "Job Satisfaction",
    shape = "Marital Status"
  ) +
  theme_minimal()












data_sample <- sample_n(data, 50)


data_sample$Work.Life.Balance <- factor(data_sample$Work.Life.Balance, levels = c("Poor", "Fair", "Good", "Excellent"), ordered = TRUE)

ggplot(data_sample, aes(x = Monthly.Income, y = Age)) +
  geom_point(color = "blue", shape = 20, size = 2, alpha = 0.6) +  # all points same color, shape, size
  labs(
    title = "Scatter Plot: Age vs Monthly Income (Random Sample of 50 Rows)",
    x = "Monthly Income",
    y = "Age"
  ) +
  theme_minimal()

data_sample <- sample_n(data, 50)

data_sample$Job.Satisfaction <- factor(data_sample$Job.Satisfaction, levels = c("Medium","High","Very High","Low"), ordered = TRUE)

ggplot(data_sample, aes(x = Monthly.Income, y = Age)) +
  geom_point(color = "red", shape = 20, size = 2, alpha = 0.6) +  # all points same color, shape, size
  labs(
    title = "Scatter Plot: Job satisfiction based on Age vs Monthly Income ",
    x = "Monthly Income",
    y = "Age"
  ) +
  theme_minimal()

data_sample <- sample_n(data, 50)

data_sample$Work.Life.Balance <- factor(data_sample$Work.Life.Balance, levels = c("Excellent","Poor","Good","Fair"), ordered = TRUE)

ggplot(data_sample, aes(x = Monthly.Income, y = Age)) +
  geom_point(color = "green", shape = 20, size = 2, alpha = 0.6) +  # all points same color, shape, size
  labs(
    title = "Scatter Plot: Job satisfiction based on Age vs Monthly Income ",
    x = "Monthly Income",
    y = "Age"
  ) +
  theme_minimal()








library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Assignment 1/data/train.csv")


# Sample 50 rows once
data_sample <- sample_n(data, 50)

# Make sure factors are ordered (optional)
data_sample$Work.Life.Balance <- factor(data_sample$Work.Life.Balance, levels = c("Poor", "Fair", "Good", "Excellent"), ordered = TRUE)
data_sample$Job.Satisfaction <- factor(data_sample$Job.Satisfaction, levels = c("Medium","High","Very High","Low"), ordered = TRUE)
data_sample$Marital.Status <- factor(data_sample$Marital.Status, levels = c("Married", "Divorced", "Single"))

# Create separate data frames for each variable to plot points separately with different colors
df_wlb <- data_sample %>% select(Monthly.Income, Age) %>% mutate(Type = "Work.Life.Balance")
df_js <- data_sample %>% select(Monthly.Income, Age) %>% mutate(Type = "Job.Satisfaction")
df_ms <- data_sample %>% select(Monthly.Income, Age) %>% mutate(Type = "Marital.Status")

# Combine them into one dataframe
plot_data <- bind_rows(df_wlb, df_js, df_ms)

# Define colors for each variable type
colors <- c("Work.Life.Balance" = "blue", "Job.Satisfaction" = "red", "Marital.Status" = "green")

# Plot
ggplot(plot_data, aes(x = Monthly.Income, y = Age, color = Type)) +
  geom_point(shape = 20, size = 2, alpha = 0.6) +
  scale_color_manual(values = colors) +
  labs(
    title = "Scatter Plot: Age vs Monthly Income with Three Variables",
    x = "Monthly Income",
    y = "Age",
    color = "Variable"
  ) +
  theme_minimal()


names(data)


unique(data$comp)


unique(data$Marital.Status)
unique(data$Job.Satisfaction)
unique(data$Work.Life.Balance)



# ------data.class()# ------------------------- 1. Multivariate Scatter Plot -------------------------
ggplot(data, aes(x = Age, y = Monthly.Income, color = Job.Role, shape = Gender)) +
  geom_point(alpha = 0.7, size = 3) +
  theme_minimal() +
  labs(title = "Income vs Age by Job Role & Gender", x = "Age", y = "Monthly Income")

# ------------------------- 2. Violin Plot -------------------------
ggplot(data, aes(x = Education.Level, y = Monthly.Income, fill = Education.Level)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  theme_minimal() +
  labs(title = "Violin Plot: Monthly Income by Education Level", x = "Education Level", y = "Monthly Income")

# ------------------------- 3. Line Graph -------------------------
avg_income <- data %>%
  group_by(Years.at.Company) %>%
  summarise(AvgIncome = mean(Monthly.Income, na.rm = TRUE))

ggplot(avg_income, aes(x = Years.at.Company, y = AvgIncome)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Average Monthly Income by Years at Company", x = "Years at Company", y = "Average Monthly Income")

# ------------------------- 4. Radar Chart -------------------------
radar_data1 <- data %>%
  group_by(Sentiment) %>%
  summarise(
    MonthlyIncome = mean(Monthly.Income, na.rm = TRUE),
    JobSatisfaction = as.numeric(mean(as.numeric(factor(Job.Satisfaction)), na.rm = TRUE)),
    WorkLifeBalance = as.numeric(mean(as.numeric(factor(Work.Life.Balance)), na.rm = TRUE)),
    PerformanceRating = as.numeric(mean(as.numeric(factor(Performance.Rating)), na.rm = TRUE)),
    DistanceFromHome = mean(Distance.from.Home, na.rm = TRUE)
  ) %>%
  column_to_rownames("Sentiment")

radar_data_scaled1 <- rbind(
  apply(radar_data1, 2, max),
  apply(radar_data1, 2, min),
  radar_data1
)

radarchart(
  radar_data_scaled1,
  axistype = 1,
  pcol = c("blue", "red"),
  pfcol = adjustcolor(c("blue", "red"), alpha.f = 0.3),
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  caxislabels = seq(0, max(radar_data_scaled1), length.out = 5),
  cglwd = 0.8,
  vlcex = 0.9,
  title = "Radar Chart: Avg Metrics by Sentiment (Set 1)"
)

legend("topright", legend = rownames(radar_data1), col = c("blue", "red"), pch = 20, pt.cex = 1.5, bty = "n")

print(colnames(data))

ggplot(data, aes(x = Years.at.Company, y = Monthly.Income, color = Marital.Status, shape = Overtime)) +
  geom_point(alpha = 0.7, size = 3) +
  theme_minimal() +
  labs(title = "Income vs Years at Company by Marital Status & Overtime", x = "Years at Company", y = "Monthly Income")

# ------------------------- 1. Multivariate Scatter Plot -------------------------
ggplot(data, aes(x = Years.at.Company, y = Monthly.Income, color = Marital.Status, shape = Overtime)) +
  geom_point(alpha = 0.7, size = 3) +
  theme_minimal() +
  labs(title = "Income vs Years at Company by Marital Status & Overtime", x = "Years at Company", y = "Monthly Income")

# ------------------------- 2. Violin Plot -------------------------
ggplot(data, aes(x = Marital.Status, y = Age, fill = Marital.Status)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  theme_minimal() +
  labs(title = "Violin Plot: Age Distribution by Marital Status", x = "Marital Status", y = "Age")

# ------------------------- 3. Line Graph -------------------------
avg_worklife <- data %>%
  group_by(Years.at.Company) %>%
  summarise(AvgWLBalance = mean(as.numeric(factor(Work.Life.Balance)), na.rm = TRUE))

ggplot(avg_worklife, aes(x = Years.at.Company, y = AvgWLBalance)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "orange", size = 2) +
  theme_minimal() +
  labs(title = "Average Work-Life Balance by Years at Company", x = "Years at Company", y = "Avg Work-Life Balance")

# ------------------------- 4. Radar Chart (More Variables) -------------------------
radar_data2 <- data %>%
  group_by(Sentiment) %>%
  summarise(
    Age = mean(Age, na.rm = TRUE),
    TotalWorkingYears = mean(Total.Working.Years, na.rm = TRUE),
    EnvironmentSatisfaction = as.numeric(mean(as.numeric(factor(Environment.Satisfaction)), na.rm = TRUE)),
    TrainingTimesLastYear = mean(Training.Times.Last.Year, na.rm = TRUE),
    YearsWithCurrManager = mean(Years.With.Cur.Manager, na.rm = TRUE)
  ) %>%
  column_to_rownames("Sentiment")

radar_data_scaled2 <- rbind(
  apply(radar_data2, 2, max),
  apply(radar_data2, 2, min),
  radar_data2
)

radarchart(
  radar_data_scaled2,
  axistype = 1,
  pcol = c("darkorange", "darkblue"),
  pfcol = adjustcolor(c("darkorange", "darkblue"), alpha.f = 0.3),
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  caxislabels = seq(0, max(radar_data_scaled2), length.out = 5),
  cglwd = 0.8,
  vlcex = 0.9,
  title = "Radar Chart: Avg Metrics by Sentiment (Set 2)"
)

legend("topright", legend = rownames(radar_data2), col = c("darkorange", "darkblue"), pch = 20, pt.cex = 1.5, bty = "n")

