library(readxl)
library(dplyr)
file <- read_excel("Dataset_MIdterm_sectoin(D).xlsx")

summary(file)

missing_value <- colSums(is.na(file))
barplot(missing_value, names.arg = names(missing_value), las = 2, col = "skyblue", main = "Missing Values per Column", ylab = "Count of Missing Values", xlab = "Columns")
summary(file)

text(x = seq_along(missing_value), y = missing_value, labels = missing_value, pos = 3, cex = 0.8)

file$Age <- sapply(file$Age, function(x) { if (!is.na(x) && x < 0) abs(x) else x })
file <- file %>% filter(is.na(Age) | (Age >= 0 & Age <= 120))
file$Age[is.na(file$Age)] <- ceiling(mean(file$Age, na.rm = TRUE))
summary(file)


max_gender <- names(which.max(table(file$Gender)))
file$Gender[is.na(file$Gender)] <- max_gender

file$BloodPressure <- gsub("[^0-9]", "", as.character(file$BloodPressure))
file$BloodPressure <- as.numeric(file$BloodPressure)
file$BloodPressure[is.na(file$BloodPressure)] <- median(file$BloodPressure, na.rm = TRUE)

max_heartRate <- names(which.max(table(file$Heart_Rate)))
file$Heart_Rate[is.na(file$Heart_Rate)] <- max_heartRate

file$Gender <- recode(file$Gender, `0` = "Male", `1` = "Female")
file$Heart_Rate <- recode(file$Heart_Rate, `High` = 1, `Low` = 0)
file$BloodPressure <- (file$BloodPressure - min(file$BloodPressure)) / (max(file$BloodPressure) - min(file$BloodPressure))
file$Age <- (file$Age - min(file$Age)) / (max(file$Age) - min(file$Age))
file <- distinct(file)

file <- file[file$Gender %in% c("Male", "Female"), ]

majority_n <- nrow(filter(file, HeartDisease == 1))

minority_sample <- file %>% filter(HeartDisease == 0) %>% sample_n(majority_n, replace = TRUE)

oversampled_data <- bind_rows(file %>% filter(HeartDisease == 1), minority_sample)
table(oversampled_data$HeartDisease)
table(oversampled_data)
set.seed(123)
index <- sample(1:nrow(oversampled_data), size = 0.8 * nrow(oversampled_data))

train_data <- oversampled_data[index, ]
test_data  <- oversampled_data[-index, ]



age_stats_gender <- file %>% group_by(Gender) %>% summarise(Mean_Age = mean(Age, na.rm = TRUE), Median_Age = median(Age, na.rm = TRUE), Mode_Age = as.numeric(names(sort(table(Age), decreasing = TRUE)[1])))
print(age_stats_gender)

age_stats_by_hr <- file %>% group_by(Heart_Rate) %>% summarise(Mean_Age = mean(Age, na.rm = TRUE), Median_Age = median(Age, na.rm = TRUE), Mode_Age = as.numeric(names(sort(table(Age), decreasing = TRUE)[1])))
print(age_stats_by_hr)

age_spread_gender <- file %>% group_by(Gender) %>% summarise(Min_Age = min(Age, na.rm = TRUE), Max_Age = max(Age, na.rm = TRUE), Range_Age = Max_Age - Min_Age, IQR_Age = IQR(Age, na.rm = TRUE), Variance_Age = var(Age, na.rm = TRUE), SD_Age = sd(Age, na.rm = TRUE))
print(age_spread_gender)

write.csv(file, "Updated-Dataset.csv", row.names = FALSE)

