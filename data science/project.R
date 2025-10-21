library(readxl)
library(dplyr)



file <- read_excel("C:/Users/ASUS/Desktop/Data Science/Dataset_MIdterm_sectoin(D).xlsx")

print(file)

#Missing value BAR plot
# Example named vector (you should already have this from your data set)
missing_value <- colSums(is.na(file))
barplot(
  missing_value,
  names.arg = names(missing_value),
  las = 2,  # Rotate x-axis labels
  col = "skyblue",
  main = "Missing Values per Column",
  ylab = "Count of Missing Values"
)





# Remove NOISE from Age

summary(file$Age)
file$Age <- sapply(file$Age, function(x){
  if(!is.na(x) && x< 0){
    abs(x)
  } else {
    x
  }
})
summary(file$Age)

colSums(is.na(file))

# Remove NULL from Age
sum(is.na(file$Age))
mean_age <- ceiling(mean(file$Age, na.rm = TRUE))  
file$Age[is.na(file$Age)] <- mean_age  
sum(is.na(file$Age))

# Remove NULL from Gender
sum(is.na(file$Gender))
unique(file$Gender)
max_gender <- names(which.max(table(file$Gender)))
file$Gender[is.na(file$Gender)] <- max_gender  
sum(is.na(file$Gender))
table(file$Gender)

# Handle Invalid Value of Blood Pressure
sapply(file, class)
unique(file$BloodPressure)
length(file$BloodPressure)
unique(file$BloodPressure)
file$BloodPressure <- gsub("[^0-9]", "", file$BloodPressure)
file$BloodPressure <- as.numeric(file$BloodPressure)
length(file$BloodPressure)


# Remove NULL from Blood Pressure
sum(is.na(file$BloodPressure))
median_BloodPressure <- median(file$BloodPressure, na.rm = TRUE)
file$BloodPressure[is.na(file$BloodPressure)] <- median_BloodPressure 
sum(is.na(file$BloodPressure))


# Remove NULL from Heart_Rate

sum(is.na(file$Heart_Rate))
max_heartRate <- names(which.max(table(file$Heart_Rate)))
file$Heart_Rate[is.na(file$Heart_Rate)] <- max_heartRate
sum(is.na(file$Heart_Rate))
max_heartRate
colSums(is.na(file))
colSums(is.na(file))

# Find Detect outlier in the data set

summary(file$Age)
count(file)
count <- sum(file$Age > 120, na.rm = TRUE)
count
count(file)
file <- file %>% 
  filter(( Age <= 120))
count(file)

summary(file$Age)

# Convert Numerical To Categorical

sapply(file, class)
head(file,10)
file$Gender <- recode(file$Gender,
                      `1` = "Male",
                      `0` = "Female")

# Convert Categorical to Numerical 

sapply(file, class)

file$Heart_Rate <- recode(file$Heart_Rate,
                      `High` = 1,
                      `Low` = 0)
head(file,10)

# Normalize the Value of Blood Pressure 
head(file[, c("Age", "BloodPressure")], 10)

file$BloodPressure
file$BloodPressure  <- (file$BloodPressure  - min(file$BloodPressure)) / 
  (max(file$BloodPressure) - min(file$BloodPressure ))
file$BloodPressure

# Normalize the Value of Age
file$Age
file$Age  <- (file$Age  - min(file$Age)) / 
  (max(file$Age) - min(file$Age ))
file$Age
head(file[, c("Age", "BloodPressure")], 10)

# Remove Duplicate

sum(duplicated(file)) 
count(file)
file <- distinct(file)
count(file)
sum(duplicated(file)) 
file


# Apply Filtering Method
file <- file[file$Gender %in% c("Male", "Female"),]

# Solve Imbalance using Undersampling

minority_n <- nrow(filter(file, HeartDisease == 0))
majority_sample <- file %>%
  filter(HeartDisease == 1) %>%
  sample_n(minority_n)
count(majority_sample)
undersampled_data <- bind_rows(majority_sample, file %>% filter(HeartDisease == 0))

table(file$HeartDisease)
table(undersampled_data$HeartDisease)


# Solve Imbalance using Oversampling


majority_n <- nrow(filter(file, HeartDisease == 1))

minority_sample <- file %>%
  filter(HeartDisease == 0) %>%
  sample_n(majority_n, replace = TRUE)

oversampled_data <- bind_rows(
  file %>% filter(HeartDisease == 1),
  minority_sample
)

table(file$HeartDisease)
table(oversampled_data$HeartDisease)

sum(duplicated(oversampled_data))



# Split train and test set

set.seed(123)

index <- sample(1:nrow(oversampled_data), size = 0.8 * nrow(oversampled_data))

train_data <- oversampled_data[index, ]
test_data  <- oversampled_data[-index, ]

# Check distribution
table(train_data$HeartDisease)
table(test_data$HeartDisease)


# Compare Age across Gender

age_stats_gender <- file %>%
  group_by(Gender) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Mode_Age = as.numeric(names(sort(table(Age), decreasing = TRUE)[1]))
  )

print(age_stats_gender)


#  Compare Age across Heart Rate Groups
age_stats_by_hr <- file %>%
  group_by(Heart_Rate) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Mode_Age = as.numeric(names(sort(table(Age), decreasing = TRUE)[1]))
  )

print(age_stats_by_hr)


# Compare Spread of Age Across Gender Groups
age_spread_gender <- file %>%
  group_by(Gender) %>%
  summarise(
    Min_Age = min(Age, na.rm = TRUE),
    Max_Age = max(Age, na.rm = TRUE),
    Range_Age = Max_Age - Min_Age,
    IQR_Age = IQR(Age, na.rm = TRUE),
    Variance_Age = var(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE)
  )

print(age_spread_gender)


summary(file)
