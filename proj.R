#R Programming Project 
setwd("D:\\r prog")

df <- read.csv("listings.csv")

#statistical analysis
nrow(df)
ncol(df)
colnames(df)
str(df)
head(df)
tail(df)
colSums(is.na(df))  
summary(df)
# mean(df),min(df),max(df),median(df) for dataset gives NA as most arguments are not numeric or logical

library(dplyr)
df_no_duplicates_rows <- distinct(df)
df_no_duplicates_columns <- df[, !duplicated(names(df))]
nrow(df)
ncol(df)
#indicates no complete duplicate rows or columns


#to print mean, median and std dev of some columns with int data type after removing duplicates 
#1
mean_of_host_listing_count <- mean(df$host_listings_count, na.rm = TRUE)
median_of_host_listing_count <- median(df$host_listings_count, na.rm = TRUE)
sd_of_host_listing_count <- sd(df$host_listings_count, na.rm = TRUE)

cat("Mean of host_listings_count:", mean_of_host_listing_count, "\n")
cat("Median of host_listings_count:", median_of_host_listing_count, "\n")
cat("Standard Deviation of host_listings_count:", sd_of_host_listing_count, "\n")

#2
mean_of_host_total_listing_count <- mean(df$host_total_listings_count, na.rm = TRUE)
median_of_host_total_listing_count <- median(df$host_total_listings_count, na.rm = TRUE)
sd_of_host_total_listing_count <- sd(df$host_total_listings_count, na.rm = TRUE)

cat("Mean of host_total_listing_count:", mean_of_host_total_listing_count, "\n")
cat("Median of host_total_listing_count:", median_of_host_total_listing_count, "\n")
cat("Standard Deviation of host_total_listing_count:", sd_of_host_total_listing_count, "\n")

#3
mean_of_bedrooms <- mean(df$bedrooms, na.rm = TRUE)
median_of_bedrooms <- median(df$bedrooms, na.rm = TRUE)
sd_of_bedrooms <- sd(df$bedrooms, na.rm = TRUE)

cat("Mean of bedrooms:", mean_of_bedrooms, "\n")
cat("Median of bedrooms:", median_of_bedrooms, "\n")
cat("Standard Deviation of bedrooms:", sd_of_bedrooms, "\n")

#4
mean_of_review_scores_checkin <- mean(df$review_scores_checkin , na.rm = TRUE)
median_of_review_scores_checkin  <- median(df$review_scores_checkin , na.rm = TRUE)
sd_of_review_scores_checkin  <- sd(df$review_scores_checkin , na.rm = TRUE)

cat("Mean of bedrooms:", mean_of_review_scores_checkin , "\n")
cat("Median of bedrooms:", median_of_review_scores_checkin , "\n")
cat("Standard Deviation of bedrooms:", sd_of_review_scores_checkin , "\n")

#if all rows with atleast one na value is removed
df_no_na <- na.omit(df)
nrow(df_no_na)
#will be zero as column license column has all na values

#Data cleaning - missing values - ignoring the NA values in one example columns
nrow(df)
colSums(is.na(df))
df1 <- subset(df, !(is.na(df$review_scores_checkin)))
df1$review_scores_checkin
nrow(df1)

mean_of_bedrooms <- mean(df1$bedrooms, na.rm = TRUE)
median_of_bedrooms <- median(df1$bedrooms, na.rm = TRUE)
sd_of_bedrooms <- sd(df1$bedrooms, na.rm = TRUE)

cat("Mean of bedrooms:", mean_of_bedrooms, "\n")
cat("Median of bedrooms:", median_of_bedrooms, "\n")
cat("Standard Deviation of bedrooms:", sd_of_bedrooms, "\n")

#Data cleaning - missing values - Filling in a constant value (replacing NA with 0)
df_new <- read.csv("listings.csv")
df_new$review_scores_checkin <- replace(df$review_scores_checkin, is.na(df$review_scores_checkin), 0)
print(df_new$review_scores_checkin)
nrow(df_new)

mean_of_bedrooms <- mean(df_new$bedrooms, na.rm = TRUE)
median_of_bedrooms <- median(df_new$bedrooms, na.rm = TRUE)
sd_of_bedrooms <- sd(df_new$bedrooms, na.rm = TRUE)

cat("Mean of bedrooms:", mean_of_bedrooms, "\n")
cat("Median of bedrooms:", median_of_bedrooms, "\n")
cat("Standard Deviation of bedrooms:", sd_of_bedrooms, "\n")

#Data cleaning - missing values - Filling in mean value (replacing NA with mean)
df_new1 <- read.csv("listings.csv")
df_new1$review_scores_checkin <- replace(df_new1$review_scores_checkin, is.na(df_new1$review_scores_checkin), mean(df_new1$review_scores_checkin, na.rm = TRUE))
print(df_new1$review_scores_checkin)
nrow(df_new1)

mean_of_bedrooms <- mean(df_new1$bedrooms, na.rm = TRUE)
median_of_bedrooms <- median(df_new1$bedrooms, na.rm = TRUE)
sd_of_bedrooms <- sd(df_new1$bedrooms, na.rm = TRUE)

cat("Mean of bedrooms:", mean_of_bedrooms, "\n")
cat("Median of bedrooms:", median_of_bedrooms, "\n")
cat("Standard Deviation of bedrooms:", sd_of_bedrooms, "\n")

#Outlier detection - less than Q1 – 1.5 IQR and above Q3 + 1.5 IQR 
#IQR: interquartile range 
# If a value is less than Q1 − 1.5 × IQR or greater than Q3 + 1.5 × IQR , it's considered an outlier. 
#outlier present
s <- summary(df$minimum_nights)
s
df_new2 <- read.csv("listings.csv")
df_new2<-filter(df_new2, df_new2$minimum_nights < (s[2] - 1.5 * IQR(df_new2$minimum_nights, na.rm = TRUE)))
print(df_new2$minimum_nights)
nrow(df_new2$minimum_nights)
print(filter(df, df$minimum_nights > (s[5] + 1.5 * IQR(df$minimum_nights, na.rm = TRUE))))
print(df_new3$minimum_nights)
nrow(df_new3)

#outlier not present
s <- summary(df$review_scores_value)
s
df_new4<-filter(df, df$review_scores_value < (s[2] - 1.5 * IQR(df$review_scores_value, na.rm = TRUE)))
print(df_new4$review_scores_value)
nrow(df_new4)
df_new5<-filter(df, df$review_scores_value > (s[5] + 1.5 * IQR(df$review_scores_value, na.rm = TRUE)))
print(df_new5$review_scores_value)
nrow(df_new5)

#Stratified sampling: population is split into groups and a certain number of members from each group are randomly 
#selected to be included in the sample.
StratifiedSample1 <- strata(data = df, size = 5, method = "srswor")
df[StratifiedSample1$ID_unit, ]

StratifiedSample2 <- strata(data = df, size = 5, method = "srswr")
df[StratifiedSample2$ID_unit, ]

##this was givivng only na so we calculate matrix after replacing na values in data with its mean 
cor(df[, c("host_listings_count", "host_total_listings_count", "accommodates", "bathrooms", "bedrooms", "beds", "review_scores_rating")])
# Impute missing values with mean
df_imputed <- df[, c("host_listings_count", "host_total_listings_count", "accommodates", "bathrooms", "bedrooms", "beds", "review_scores_rating")]

for (col in colnames(df_imputed)) {
  df_imputed[, col] <- ifelse(is.na(df_imputed[, col]), mean(df_imputed[, col], na.rm = TRUE), df_imputed[, col])
}

cor_matrix_imputed <- cor(df_imputed)
cor_matrix_imputed


# Example 1: Frequency distribution for neighbourhood
neighbourhood_freq_dplyr <- df %>% count(neighbourhood)
neighbourhood_freq_dplyr

# Example 2: Frequency distribution for property_type
property_type_freq_dplyr <- df %>% count(property_type)
property_type_freq_dplyr

# Example 3: Frequency distribution for room_type
room_type_freq_dplyr <- df %>% count(room_type)
room_type_freq_dplyr

# Example 4: Frequency distribution for neighbourhood_group_cleansed
cleansed_frequency <- df %>% count(neighbourhood_group_cleansed)
cleansed_frequency



# Example 1: Cross-tabulation for neighbourhood_group_cleansed and room_type
cross_tab1 <- table(df$neighbourhood_group_cleansed, df$room_type)
cross_tab1

# Example 2: Cross-tabulation for host_is_superhost and instant_bookable
cross_tab2 <- table(df$host_is_superhost, df$instant_bookable)
cross_tab2

# Example 3: Cross-tabulation for neighbourhood and host_is_superhost
cross_tab3 <- table(df$neighbourhood, df$host_is_superhost)
cross_tab3

# Example 4: Cross-tabulation for neighbourhood_group_cleansed and room_type
cross_tab4 <- table(df$neighbourhood_group_cleansed, df$room_type)
cross_tab4







#Visual analysis
# Create histograms for numerical variables
numeric_vars <- sapply(df, is.numeric)
numeric_df <- df[, numeric_vars]

# Histograms
par(mfrow = c(2,2))
for (i in 1:4) {
  hist(numeric_df[, i], main = names(numeric_df)[i], col = "lightblue", border = "black")
}
par(mfrow = c(2, 2))
for (i in 5:8) {
  hist(numeric_df[, i], main = names(numeric_df)[i], col = "lightblue", border = "black")
}
par(mfrow = c(2, 2))
for (i in 9:12) {
  hist(numeric_df[, i], main = names(numeric_df)[i], col = "lightblue", border = "black")
}
par(mfrow = c(2, 2))
for (i in 13:16) {
  hist(numeric_df[, i], main = names(numeric_df)[i], col = "lightblue", border = "black")
}
par(mfrow = c(2, 2))
for (i in 17:20) {
  hist(numeric_df[, i], main = names(numeric_df)[i], col = "lightblue", border = "black")
}
par(mfrow = c(2, 2))
for (i in 21:24) {
  hist(numeric_df[, i], main = names(numeric_df)[i], col = "lightblue", border = "black")
}
par(mfrow = c(2, 2))
for (i in 25:28) {
  hist(numeric_df[, i], main = names(numeric_df)[i], col = "lightblue", border = "black")
}
par(mfrow = c(2, 2))
for (i in 29:32) {
  hist(numeric_df[, i], main = names(numeric_df)[i], col = "lightblue", border = "black")
}
par(mfrow = c(2, 2))
for (i in 33:36) {
  hist(numeric_df[, i], main = names(numeric_df)[i], col = "lightblue", border = "black")
}

#density plots
# Identify numeric variables
numeric_vars <- sapply(df, is.numeric)
numeric_df <- df[, numeric_vars]

# Remove missing values
numeric_df_no_na <- na.omit(numeric_df)

# Set up a 3x3 grid for density plots
par(mfrow = c(2, 2))

# Create density plots for each numeric variable
for (i in 1:4) {
  plot(density(numeric_df_no_na[, i]), main = names(numeric_df)[i], col = "blue", lwd = 2)
}
for (i in 5:8) {
  plot(density(numeric_df_no_na[, i]), main = names(numeric_df)[i], col = "blue", lwd = 2)
}
for (i in 9:12) {
  plot(density(numeric_df_no_na[, i]), main = names(numeric_df)[i], col = "blue", lwd = 2)
}
for (i in 13:16) {
  plot(density(numeric_df_no_na[, i]), main = names(numeric_df)[i], col = "blue", lwd = 2)
}
for (i in 17:20) {
  plot(density(numeric_df_no_na[, i]), main = names(numeric_df)[i], col = "blue", lwd = 2)
}
for (i in 21:24) {
  plot(density(numeric_df_no_na[, i]), main = names(numeric_df)[i], col = "blue", lwd = 2)
}
for (i in 25:28) {
  plot(density(numeric_df_no_na[, i]), main = names(numeric_df)[i], col = "blue", lwd = 2)
}
for (i in 29:32) {
  plot(density(numeric_df_no_na[, i]), main = names(numeric_df)[i], col = "blue", lwd = 2)
}


library(ggplot2)
ggplot(df, aes(x = neighbourhood_group_cleansed, y = review_scores_rating)) +
  geom_boxplot() +
  labs(title = "Boxplot of Review Scores by Neighbourhood Group")


#Line chart
plot(df$review_scores_checkin, type = "l", col = "green", xlab = "", ylab = "scores", main = "Review_scores_checkin")
plot(df$Temp, type = "o", col = "red", xlab = "Day", ylab = "Temperature", main = "Temperature Trend")
plot(df$review_scores_checkin,type="l", col = "blue",xlab="review_scores_checkin")
plot(df$host_listings_count,type="l", col = "blue",xlab="host_listings_count")
plot(df$host_total_listings_count, type="l",col = "blue",xlab="host_total_listings_count")
plot(df$latitude,type="l", col = "blue",xlab="latitude")
plot(df$longitude,type="l", col = "blue",xlab="longitude")
plot(df$accommodates,type="l", col = "blue",xlab="accommodates")
plot(df$bedrooms, type="l",col = "blue",xlab="bedrooms")
plot(df$beds,type="l", col = "blue",xlab="beds")
plot(df$minimum_nights,type="l", col = "blue",xlab="minimum_nights")
plot(df$maximum_nights,type="l", col = "blue",xlab="maximum_nights")
plot(df$minimum_maximum_nights,type="l", col = "blue",xlab="minimum_maximum_nights")
plot(df$availability_365,type="l", col = "blue",xlab="availability_365")
plot(df$availability_90,type="l", col = "blue",xlab="availability_90")
plot(df$availability_30,type="l", col = "blue",xlab="availability_30")
plot(df$availability_60,type="l", col = "blue",xlab="availability_60")

#Box plot
boxplot(df$review_scores_checkin, col = "blue",xlab="review_scores_checkin")
boxplot(df$host_listings_count, col = "blue",xlab="host_listings_count")
boxplot(df$host_total_listings_count, col = "blue",xlab="host_total_listings_count")
boxplot(df$latitude, col = "blue",xlab="latitude")
boxplot(df$longitude, col = "blue",xlab="longitude")
boxplot(df$accommodates, col = "blue",xlab="accommodates")
boxplot(df$bedrooms, col = "blue",xlab="bedrooms")
boxplot(df$beds, col = "blue",xlab="beds")
boxplot(df$minimum_nights, col = "blue",xlab="minimum_nights")
boxplot(df$maximum_nights, col = "blue",xlab="maximum_nights")
boxplot(df$minimum_maximum_nights, col = "blue",xlab="minimum_maximum_nights")
boxplot(df$availability_365, col = "blue",xlab="availability_365")
boxplot(df$availability_90, col = "blue",xlab="availability_90")
boxplot(df$availability_30, col = "blue",xlab="availability_30")
boxplot(df$availability_60, col = "blue",xlab="availability_60")

