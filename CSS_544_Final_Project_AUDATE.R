# Get and print current working directory
getwd()  # shows where R is looking for files

# Load the plotting and data-manipulation libraries
library(ggplot2)   # for layered, grammar-of-graphics plotting
library(dplyr)     # for data-frame manipulation verbs (filter, mutate, etc.)
library(corrplot)  # for visualizing correlation matrices
options(digits = 4)
# 1) Data Import and Preparation ----------------------------------------------

# Read CSV, treating the first row as column names
data <- read.csv(
  "student_habits_performance.csv",
  header = TRUE,           # first row are headers
  stringsAsFactors = FALSE # keep strings as character, not factors
)

# Inspect data structure and first few records
str(data)                  # prints each column’s type and sample values
sum(is.na(data))
summary(data)
# Convert character columns into factors for proper categorical handling
data$gender <- factor(data$gender)
data$part_time_job <- factor(data$part_time_job)
data$diet_quality <- factor(
  data$diet_quality,
  levels = c("Poor","Fair","Good"),  # define logical order
  ordered = TRUE                      # make it an ordered factor
)
data$parental_education_level <- factor(
  data$parental_education_level,
  levels = c("None","High School","Bachelor","Master"),
  ordered = TRUE
)
data$internet_quality <- factor(
  data$internet_quality,
  levels = c("Poor","Average","Good"),
  ordered = TRUE
)
data$extracurricular_participation <- factor(data$extracurricular_participation)

# Remove the student_id column—doesn’t inform analysis
data$student_id <- NULL

# 2) Univariate Analysis: Diet Quality Counts --------------------------------

diet_counts <- table(data$diet_quality)  # raw counts by diet level
print(diet_counts)

df_diet <- as.data.frame(diet_counts)    # convert to 2-col data.frame
colnames(df_diet) <- c("Diet", "Count")  # give clear column names
print(df_diet)

prop.table(diet_counts)                  # show proportions

# Bar chart of diet distribution
ggplot(data, aes(x = diet_quality)) +
  geom_bar(fill = "skyblue") +           # bar height = count
  labs(
    title = "Diet Quality Distribution",
    x     = "Diet Quality",
    y     = "Number of Students"
  ) +
  theme_minimal()

#  Pie chart of diet distribution (base R)
pie(
  diet_counts,
  labels = paste0(
    names(diet_counts),
    " (",
    round(100 * diet_counts / sum(diet_counts), 1),
    "%)"
  ),
  col = c("#F8766D", "#00BA38", "#619CFF"),
  main = "Diet Quality Distribution"
)

# 3)Exam Score Distribution ----------------------------

summary(data$exam_score)  # min, 1st Q, median, mean, 3rd Q, max
sd(data$exam_score)       # standard deviation

# Histogram of exam scores
ggplot(data, aes(x = exam_score)) +
  geom_histogram(
    binwidth = 5,           # 5-point bins
    fill     = "cadetblue",
    color    = "black"
  ) +
  labs(
    title = "Exam Score Histogram",
    x     = "Exam Score",
    y     = "Frequency"
  ) +
  theme_minimal()

# Boxplot of exam scores
ggplot(data, aes(y = exam_score)) +
  geom_boxplot(fill = "lightskyblue") +
  labs(
    title = "Exam Score Boxplot",
    y     = "Exam Score"
  ) +
  theme_minimal()

# 4) Bivariate Analysis: Correlations ----------------------------------------

# Identify numeric columns and compute correlation matrix
numeric_cols <- sapply(data, is.numeric)
cor_mat <- cor(data[, numeric_cols])

# Visualize correlation matrix
corrplot(
  cor_mat,
  type   = "upper",  # show only upper triangle
  tl.cex = 0.8,      # label size
  title  = "Correlation Matrix"
)

# Scatterplot with linear fit: study hours → exam score
ggplot(data, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Exam Score vs Study Hours",
    x     = "Study Hours per Day",
    y     = "Exam Score"
  ) +
  theme_minimal()

# 5) Central Limit Theorem Demonstration ------------------------------------

set.seed(123)
pop_mean <- mean(data$exam_score)
pop_sd   <- sd(data$exam_score)
sample_sizes <- c(5, 30, 100)

# Panel of histograms of sample means
par(mfrow = c(1, 3))
for (n in sample_sizes) {
  samp_means <- replicate(
    1000,
    mean(sample(data$exam_score, n, replace = TRUE))
  )
  hist(
    samp_means,
    probability = TRUE,
    main        = paste("n =", n),
    xlab        = "Sample Mean"
  )
  curve(
    dnorm(x, pop_mean, pop_sd / sqrt(n)),
    add = TRUE,
    col = "red",
    lwd = 2
  )
  abline(v = pop_mean, col = "blue", lty = 2)
}
par(mfrow = c(1, 1))

#5b
# 1) Simulate sample means for three sample sizes
set.seed(123)
sample_sizes <- c(5, 30, 100)
sim_list <- lapply(sample_sizes, function(n) {
  means <- replicate(1000, mean(sample(data$exam_score, n, replace=TRUE)))
  data.frame(sample_mean = means, n = factor(n))
})
sim_df <- do.call(rbind, sim_list)

# 2) Single density plot showing all three
library(ggplot2)
ggplot(sim_df, aes(x = sample_mean, color = n, fill = n)) +
  geom_density(alpha = 0.3, size = 1) +
  labs(
    title = "Sampling Distribution of the Mean\nfor Different Sample Sizes",
    x = "Sample Mean of Exam Score",
    y = "Density",
    color = "Sample Size (n)",
    fill  = "Sample Size (n)"
  ) +
  theme_minimal()


# 6) Sampling Methods Comparison ---------------------------------------------

set.seed(42)
srs   <- data[sample(nrow(data), 100), ]                                  # simple random sample

strat <- data %>% group_by(gender) %>% sample_frac(0.1) %>% as.data.frame()  # stratified
k     <- 10
start <- sample(1:k, 1)

sys   <- data[seq(start, nrow(data), by = k), ]                          # systematic

# Compare mean exam scores
print(mean(srs$exam_score))
print(mean(strat$exam_score))
print(mean(sys$exam_score))
print(mean(data$exam_score))  # population mean

# 7) Cross-Tab and Proportions: Job vs Extracurricular -----------------------

job_extra <- table(data$part_time_job, data$extracurricular_participation)
print(prop.table(job_extra, 1))  # row-wise proportions

# Convert to data frame for clearer output
library(knitr)
prop_df <- as.data.frame.matrix(round(prop_job_extra * 100, 1))  # convert to % and round
colnames(prop_df) <- c("No Extracurricular (%)", "Yes Extracurricular (%)")
rownames(prop_df) <- c("No Part-Time Job", "Has Part-Time Job")

kable(prop_df, caption = "Proportion of Extracurricular Participation by Part-Time Job Status")

ggplot(data, aes(x = part_time_job, fill = extracurricular_participation)) +
  geom_bar(position = "fill") +
  labs(
    title = "Extracurricular Participation by Job Status",
    x     = "Part-Time Job",
    y     = "Proportion"
  ) +
  theme_minimal()



# 8) Exam Score by Other Factors --------------------------------------------

# Part-time job box plot
ggplot(data, aes(x = part_time_job, y = exam_score)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  stat_summary(
    fun    = mean,
    geom   = "point",
    shape  = 18,
    size   = 3,
    color  = "red"
  ) +
  labs(
    title = "Exam Score by Part-Time Job",
    x     = "Part-Time Job",
    y     = "Exam Score"
  ) +
  theme_minimal()

# Netflix Hours
ggplot(data, aes(x = netflix_hours, y = exam_score)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Exam Score vs Netflix Hours",
    x     = "Netflix Hours per Week",
    y     = "Exam Score"
  ) +
  theme_minimal()

# Internet Quality
ggplot(data, aes(x = internet_quality, y = exam_score)) +
  geom_boxplot(fill = "peachpuff", outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  stat_summary(
    fun    = mean,
    geom   = "point",
    shape  = 18,
    size   = 3,
    color  = "blue"
  ) +
  labs(
    title = "Exam Score by Internet Quality",
    x     = "Internet Quality",
    y     = "Exam Score"
  ) +
  theme_minimal()

# Parental Education
ggplot(data, aes(x = parental_education_level, y = exam_score)) +
  geom_boxplot(fill = "thistle", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  stat_summary(
    fun    = mean,
    geom   = "point",
    shape  = 18,
    size   = 3,
    color  = "darkblue"
  ) +
  labs(
    title = "Exam Score by Parental Education",
    x     = "Parental Education Level",
    y     = "Exam Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# 1) Create numeric versions of the factors
data$internet_quality_num <- as.numeric(data$internet_quality)
data$parent_edu_num       <- as.numeric(data$parental_education_level)

# 2) Scatter + regression: Exam Score vs Internet Quality
ggplot(data, aes(x = internet_quality_num, y = exam_score)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.6, size = 1.5) +    # jitter so points don’t overlap
  geom_smooth(method = "lm", se = TRUE, color = "red", lwd = 1) +     # add linear model fit
  scale_x_continuous(
    breaks = 1:3,
    labels = levels(data$internet_quality)
  ) +
  labs(
    title = "Exam Score vs Internet Quality",
    x     = "Internet Quality",
    y     = "Exam Score"
  ) +
  theme_minimal()

# 3) Scatter + regression: Exam Score vs Parental Education Level
ggplot(data, aes(x = parent_edu_num, y = exam_score)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", lwd = 1) +
  scale_x_continuous( #breaks continuous plots into 4 "Breaks"
    breaks = 1:4,
    labels = levels(data$parental_education_level)
  ) +
  labs(
    title = "Exam Score vs Parental Education Level",
    x     = "Parental Education Level",
    y     = "Exam Score"
  ) +
  theme_minimal() 
