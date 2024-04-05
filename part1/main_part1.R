# RomanOFF read dir
getwd()
#setwd('/Users/roman.osadsky/Desktop/ROMAN/FIIT/OZNAL_PROJECT/oznal_project1')
setwd(getwd())

library(dplyr)
library(ggplot2)
library(pROC)
library(GGally)
library(tidyverse)
library(caret)
library(tidyr)
library(e1071)
library(corrplot)
library(MASS)
library(reshape2)
library(gridExtra)

# načítanie datasetu
labor_measurements <- read.csv('./data/measurements.csv', sep = '\t')
labor_stations <- read.csv('./data/stations.csv', sep = '\t')

cat('Zobrazenie prvých riadkov datasetov, ich rozmerov a typov atribútov \n\n')

cat('Merenia: \n')
head(labor_measurements)
cat('Number of records: ', nrow(labor_measurements), '\n')
cat('Number of attributes: ', ncol(labor_measurements), '\n\n')
cat('Types of attributes:\n')
str(labor_measurements)

cat('Stanice: \n')
head(labor_stations)
cat('Number of records: ', nrow(labor_stations), '\n')
cat('Number of attributes: ', ncol(labor_stations), '\n\n')
cat('Types of attributes:\n')
str(labor_stations)


cat('Zjednotenie atribútov:')
# Data frame labor_measurements obsahuje iba číselné údaje float takže problém rozdielnych formátov nie je.
# Ked sa pozrieme na labor_stations tam už to tak nieje preto si skontrolujeme formáty

colnames(labor_stations)

# Kontrola jedinečných hodnôt a ich počtu
qos_count <- table(labor_stations$QoS)
#qos_count
# Nahradenie špecifických hodnôt
labor_stations$QoS[labor_stations$QoS == "acceptable"] <- "accep"
labor_stations$QoS[labor_stations$QoS == "maitennce"] <- "maintenance"
# Opätovná kontrola jedinečných hodnôt a ich počtov po nahradení
qos_count <- table(labor_stations$QoS)
#qos_count

# Kontrola jedinečných hodnôt a ich počtu
revision_count <- table(labor_stations$revision)
#revision_count
# Nahradenie špecifických hodnôt
source("part1/utils.R")
labor_stations$revision <- sapply(labor_stations$revision, parse_dates)
cat('Identifikované rozdielne formáty dátumu sme zjednotili na formát dd mmm yyyy')
# Opätovná kontrola jedinečných hodnôt a ich počtov po nahradení
revision_count <- table(labor_stations$revision)
#revision_count

cat('Nahradenie chýbajúcich hodnôt', '\n')

# Nahradenie prázdnych reťazcov reťazcami NA
labor_measurements[labor_measurements == ""] <- NA
labor_stations[labor_stations == ""] <- NA

missing_data_percentage <- sum(is.na(labor_measurements)) / nrow(labor_measurements) * 100
cat(sprintf("Chýbajúce dáta v labor_measurements tvoria %.5f%% dát", missing_data_percentage))
cat("\n")
missing_data_percentage_stations <- sum(is.na(labor_stations)) / nrow(labor_stations) * 100
cat(sprintf("Chýbajúce dáta v labor_stations tvoria %.5f%% dát", missing_data_percentage_stations))
cat("\n")

# Spočítanie a vypísanie riadkov s hodnotami NA pre labor_measurements
na_count_measurements <- sum(!complete.cases(labor_measurements))
cat('NaN hodnoty pre labor_measurements:', na_count_measurements, '\n')

# Spočítanie a vypísanie riadkov s hodnotami NA pre labor_stations
na_count_stations <- sum(!complete.cases(labor_stations))
cat('NaN hodnoty pre labor_stations:', na_count_stations, '\n')

# Drop rows with any NA values
labor_measurements <- na.omit(labor_measurements)
labor_stations <- na.omit(labor_stations)

# Kontrola duplicitných záznamov
cat('Duplicitné záznamy:', '\n')
if (any(duplicated(labor_measurements))) {
  duplicates_count <- sum(duplicated(labor_measurements))
  cat('labor_measurements pocet duplikatov', duplicates_count, '\n')
} else {
  cat('labor_measurements no duplicates\n')
}
if (any(duplicated(labor_stations))) {
  duplicates_count_stations <- sum(duplicated(labor_stations))
  cat('labor_stations pocet duplikatov', duplicates_count_stations, '\n')
} else {
  cat('labor_stations no duplicates\n')
}

labor_measurements <- unique(labor_measurements)
labor_stations <- unique(labor_stations)

cat('Nan záznamy a duplicitné záznamy boli odstránené', '\n')

# Zlúčenie súborov údajov
cat('Nasleduje zmergovanie datasetov:', '\n')
# Odstránenie nepotrebných stĺpcov
labor_stations <- labor_stations[, -4]
#head(labor_stations)
# Merge
df <- merge(labor_measurements, labor_stations, by = c("latitude", "longitude"), all = FALSE)
# Odstránenie stĺpcov 'latitude' a 'longitude' lebo ich už viac nepotrebujeme
df <- df[, !names(df) %in% c('latitude', 'longitude')]
# reorganizacia stlpcov
df <- df[, c('location', 'warning', 'QoS', 'revision', 'TEMP', 'PRES', 'PM2.5', 'NOx', 'PM10', 'C2H3NO5', 'CH4', 'Pb', 'NH3', 'SO2', 'O3', 'CO', 'PAHs', 'H2CO', 'CFCs')]
head(df)

############## Data Exploration ##############

# Spočítame počet varovaní a vypočítame percento
war_count <- df %>%
  count(warning) %>%
  rename(Warning = warning, Count = n) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Vykreslenie počtu varovaní ako koláčový graf s percentami
ggplot(war_count, aes(x = "", y = Percentage, fill = factor(Warning))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Warning visual ratio", fill = "Warning") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(breaks = c("0", "1"))

# Vytvorenie funkcie na vykreslenie histogramov pre každý stĺpec
plot_histogram <- function(column_name, data) {
  ggplot(data, aes(x = !!sym(column_name))) +
    geom_histogram(bins = 50, fill = "blue", color = "black") +
    labs(title = paste("Histogram of", column_name), x = column_name, y = "Frequency") +
    theme_minimal()
}

# Vygenerovanie zoznamu grafov histogramov pre každý stĺpec v df
histogram_plots <- lapply(colnames(df), function(col) plot_histogram(col, df))

# Výpočet skewness
# Identifikácia riadkov s nečíselnými hodnotami v údajne číselnom stĺpci
df_nums_only <- df %>%
  select_if(is.numeric)

# skontrolujeme každý číselný stĺpec, či sa v ňom nenachádzajú hodnoty, ktoré sa nedajú dať na číslo
for (col in names(df_nums_only)) {
  if (is.factor(df_nums_only[[col]])) {
    df_nums_only[[col]] <- as.character(df_nums_only[[col]])
  }

  if (!all(sapply(df_nums_only[[col]], is.numeric))) {
    warning(paste("Non-numeric values found in column:", col))
    df_nums_only[[col]] <- as.numeric(as.character(df_nums_only[[col]]))
  }
}
sapply(df_nums_only, skewness)

# korelačná matica
df_nums_only_corr <- cor(df_nums_only, use = "complete.obs")  # 'use' parameter handles missing values

# Vytvorenie masky pre hodnoty, ktoré spĺňajú kritériá
# vyfiltrovanie korelácií s abs hodnotou podla:
threshold_high <- 0.39
threshold_low <- -0.39
mask <- abs(df_nums_only_corr) >= threshold_high & df_nums_only_corr != 1

# nahradenie hodnot kt nesplnanu kriteria NaNkom
filtered_corr_matrix <- df_nums_only_corr
filtered_corr_matrix[!mask] <- NA

# vykreslime filtrovanú korelačnú maticu
corrplot(filtered_corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, title = "Filtered Correlation Matrix (|r| >= 0.39)")

# Významnejšie korelácie sa javia byť medzi týmito atribútmi:
# PM2.5 - PM10
# PM10 - NH3
# PM10 - C2H3NO5
cat("# Významnejšie korelácie sa javia byť medzi týmito atribútmi:\n PM2.5 - PM10\n PM10 - NH3\n PM10 - C2H3NO5")

################### Linear regression ##################
#numeric_df <- df[sapply(df, is.numeric)]
#ggpairs(numeric_df,
#        lower = list(continuous = "smooth"),
#        diag = list(continuous = "barDiag"))
# cor(numeric_df, method = "pearson")

# rozdelenie data setu na test a train
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob = c(0.5, 0.3))
train_data <- df[sample,]
test_data <- df[!sample,]

######################## Model ##########################

# PM2.5 – the most harmful pollution
PM25Model <- lm(PM2.5 ~ PM10  + SO2 + CO, data = train_data)
summary(PM25Model)

par(mar = c(1, 1, 1, 1))
plot(PM25Model)
# PM25 = 12.88 - 0.11 * PM10 - 0.23 * SO2 + 0.10 * CO
###################### Predictions ######################

predicted_values <- predict(PM25Model, newdata = test_data)

# Manualny test
cat("Expected: 5.72081 , Predicted: ",predict(PM25Model,data.frame(PM10=5.85838,  SO2=11.00024, CO=7.00959)))

# Overenie správnosti modelu
mean_absolute_error <- mean(abs(predicted_values - test_data$PM2.5))
mean_squared_error <- mean((predicted_values - test_data$PM2.5)^2)
root_mean_squared_error <- sqrt(mean_squared_error)

data.frame(Metric = c("Mean Absolute Error", "Mean Squared Error", "Root Mean Squared Error"), Value = c(mean_absolute_error,mean_squared_error,root_mean_squared_error))

# Plot predicted vs actual
plot(x=predicted_values, y=test_data$PM2.5,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')

# Ako realne vyzerajú dáta
head(data.frame(actual=test_data$PM2.5, predicted=predicted_values))



# klasifikacia
df$warning <- as.factor(df$warning)

# Splitting the dataset
set.seed(123)
index <- sample(1:nrow(df), round(0.7 * nrow(df)))
trainData <- df[index,]
testData <- df[-index,]

# Train Logistic Regression model
logisticModel <- glm(warning ~ ., data = trainData, family = binomial())

# Predict on test set
probabilities <- predict(logisticModel, newdata = testData, type = "response")
predictions <- ifelse(probabilities > 0.5, levels(testData$warning)[2], levels(testData$warning)[1])
predictions <- factor(predictions, levels = levels(testData$warning))

# Evaluate model performance
confMatrix <- table(Predicted = predictions, Actual = testData$warning)
print(confMatrix)

confMat <- confusionMatrix(predictions, testData$warning)
confMatMatrix <- as.matrix(confMat$table)
confMatMelted <- melt(confMatMatrix)
colnames(confMatMelted) <- c('Actual', 'Predicted', 'Value')

p <- ggplot(confMatMelted, aes(x = Actual, y = Predicted, fill = Value)) +
  geom_tile(color = "white") +  # Tiles for each combination of actual and predicted
  geom_text(aes(label = Value), color = "black", size = 5) +  # Add text to each tile
  scale_fill_gradient(low = "white", high = "steelblue") +  # Gradient color for tiles
  theme_minimal() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +  # Adjust x-axis text
  xlab('Predicted') +  # X-axis label
  ylab('Actual') +  # Y-axis label
  ggtitle('Confusion Matrix - Logistic Regression')  # Graph title

print(p)

