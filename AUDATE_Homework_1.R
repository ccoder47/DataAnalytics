#Clarence Audate Homework Assignment #1 
#June 2, 2025

#Question 1
temp <- sample(beaver2$temp, 30)

#Q1A - Display the vector temp
dput(temp)

# Q1B - Compute the mean and median of your current temp
mean(temp)
median(temp)

# Q1C - Convert temp to integers and find the mode
temp_int <- as.integer(temp)   # truncate decimal values
temp_int                      # show the integer vector

# Create a frequency table and find the mode
freq_table <- table(temp_int)
mode_temp_int <- as.numeric(names(freq_table[freq_table == max(freq_table)]))
mode_temp_int

# Q1D - Compute variance and standard deviation of temp
var_temp <- var(temp)
sd_temp <- sd(temp)

var_temp
sd_temp
# Q1E - Five-number summary, IQR, and outliers
# Five-number summary (Min, Q1, Median, Q3, Max)
summary(temp)          # includes Mean too
five_num <- fivenum(temp)  # strictly Min, Q1, Median, Q3, Max
five_num

#Interquartile Range (IQR = Q3 - Q1)
iqr_val <- IQR(temp)
iqr_val

# Outlier thresholds
Q1 <- five_num[2]
Q3 <- five_num[4]
lower_bound <- Q1 - 1.5 * iqr_val
upper_bound <- Q3 + 1.5 * iqr_val

# Identify outliers
outliers <- temp[temp < lower_bound | temp > upper_bound]
outliers

# Q1F - Compute z-scores of temp
mean_temp <- mean(temp)
sd_temp <- sd(temp)
z_scores <- (temp - mean_temp) / sd_temp
z_scores

# Q1G - Values less than the first quartile (Q1)
Q1 <- quantile(temp, 0.25)  # get the 1st quartile
less_than_Q1 <- temp[temp < Q1] # values less than Q1
Q1
less_than_Q1

# Q1H - Access the first and last elements of temp
first_value <- temp[1]
last_value <- temp[length(temp)]

first_value
last_value

# Q1I - Create a 5x6 matrix from temp (fill row-wise)
temp.matrix <- matrix(temp, nrow = 5, ncol = 6, byrow = TRUE)
temp.matrix

# Q1J - Show the first and last columns (works for any matrix size)
first_col <- temp.matrix[, 1]
last_col <- temp.matrix[, ncol(temp.matrix)]

first_col
last_col

# Q1K - Assign row and column names
rownames(temp.matrix) <- paste0("Row", 1:nrow(temp.matrix))
colnames(temp.matrix) <- paste0("Day", 1:ncol(temp.matrix))

temp.matrix

# Q2. Using function data() to load R data set air quality
data(airquality)        # Load the built-in dataset

# Q2A - How many variables are there? How many observations are there?
ncol(airquality)        # Number of variables
nrow(airquality)        # Number of observations

print(paste("The airquality dataset contains", ncol(airquality), "variables and", nrow(airquality), "observations."))

# Q2B - Are there missing values in the data set? If so, clean up the data by removing those observations with missing values 
sum(is.na(airquality))                   # Total number of missing values
airquality_no_na <- na.omit(airquality) # Remove rows with any NA values
nrow(airquality_no_na)                  # Number of rows after removing NAs
print(paste("The cleaned airquality dataset contains", nrow(airquality_no_na), "observations after removing rows with missing values."))

# Q2C - Show the mean, median, mode, and 1st and 3rd quartiles of variables Temp and Wind in the cleaned data set.
# ----- TEMP -----
mean_temp <- mean(airquality_no_na$Temp)
median_temp <- median(airquality_no_na$Temp)
temp_freq <- table(airquality_no_na$Temp)
mode_temp <- as.numeric(names(temp_freq[temp_freq == max(temp_freq)]))
q_temp <- quantile(airquality_no_na$Temp, c(0.25, 0.75))

print(paste("Temp - Mean:", mean_temp))
print(paste("Temp - Median:", median_temp))
print(paste("Temp - Mode:", paste(mode_temp, collapse = ", ")))
print(paste("Temp - 1st Quartile:", q_temp[1]))
print(paste("Temp - 3rd Quartile:", q_temp[2]))

# ----- WIND -----
mean_wind <- mean(airquality_no_na$Wind)
median_wind <- median(airquality_no_na$Wind)
wind_freq <- table(airquality_no_na$Wind)
mode_wind <- as.numeric(names(wind_freq[wind_freq == max(wind_freq)]))
q_wind <- quantile(airquality_no_na$Wind, c(0.25, 0.75))

print(paste("Wind - Mean:", mean_wind))
print(paste("Wind - Median:", median_wind))
print(paste("Wind - Mode:", paste(mode_wind, collapse = ", ")))
print(paste("Wind - 1st Quartile:", q_wind[1]))
print(paste("Wind - 3rd Quartile:", q_wind[2]))

# Q2D - In the cleaned data set, how many days with a Temp higher than 78, and how many days with a Wind lower than 9?
temp_above_78 <- sum(airquality_no_na$Temp > 78)
wind_below_9 <- sum(airquality_no_na$Wind < 9)

print(paste("Number of days with Temp > 78:", temp_above_78))
print(paste("Number of days with Wind < 9:", wind_below_9))

# Q2E - Add a new column Celsius to the cleaned data set, showing temperature in Celsius.
airquality_no_na$Celsius <- (airquality_no_na$Temp - 32) * 5 / 9

# Show the first few rows to confirm the new column
head(airquality_no_na)

print("The cleaned dataset now includes a new column Celsius (temperature in Celsius).")


