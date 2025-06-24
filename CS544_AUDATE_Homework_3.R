# CS544 Assignment 3

# Load required packages (install packages if not already installed)
library(dplyr)
library(ggplot2)
library(UsingR)

# Part 1: Aids2 data analysis
aids <- read.csv("Aids2.csv")

# a) Count female vs male patients and visualize
sex_counts <- aids %>% count(sex)
print(sex_counts)
ggplot(aids, aes(x=sex, fill=sex)) + 
  geom_bar() +
  labs(title="Number of patients by sex", x="Sex", y="Count")

# b) Percentage of patients alive vs dead (overall), and visualize
status_counts <- aids %>% count(status) %>% 
  mutate(percent = 100 * n / sum(n))
print(status_counts)
ggplot(status_counts, aes(x=status, y=percent, fill=status)) +
  geom_bar(stat="identity") +
  labs(title="Percentage of patients by status", x="Status", y="Percent")

# c) Percentage breakdown of alive/dead by gender
gender_status_prop <- prop.table(table(aids$sex, aids$status), margin=1) * 100
print(gender_status_prop)
# Interpretation: Each row shows the percentage of that gender who are alive (A) or dead (D).

# d) Median age at diagnosis by gender, and histograms by gender
median_age_by_sex <- aids %>%
  group_by(sex) %>%
  summarise(median_age = median(age, na.rm=TRUE))

# convert to a base data.frame
median_age_by_sex_df <- as.data.frame(median_age_by_sex)
print(median_age_by_sex_df)

# Plot histograms of age distribution for each sex
ggplot(aids, aes(x=age, fill=sex)) +
  geom_histogram(binwidth=5, alpha=0.7) +
  facet_wrap(~ sex) +
  labs(title="Age at Diagnosis by Sex", x="Age at Diagnosis", y="Frequency")

# Part 2: Voting data analysis
# a) Create the voting result matrix (Gender vs Vote)
voting_counts <- matrix(c(36, 10, 4,
                          24, 30, 6),
                        nrow = 2, byrow = TRUE)
rownames(voting_counts) <- c("Men", "Women")
colnames(voting_counts) <- c("Yes", "No", "Abstain")
print(voting_counts)

# b) Add dimension variables (Gender and Vote) and show totals
vote_margins <- addmargins(voting_counts)
print(vote_margins)
# The last row and last column of vote_margins show the totals for each gender and each voting outcome.

# c) Proportional data separated by gender and by voting result
prop_by_gender <- prop.table(voting_counts, margin = 1) * 100  # percentages within each gender
prop_by_vote   <- prop.table(voting_counts, margin = 2) * 100  # percentages within each vote category
print(prop_by_gender)
print(prop_by_vote)
# Interpretation:
# Within genders: Men 72% Yes, 20% No, 8% Abstain; Women 40% Yes, 50% No, 10% Abstain.
# This shows men were much more likely to vote Yes, whereas women more often voted No.
# Within vote outcomes: Of all "Yes" votes, 60% came from men vs 40% from women.
# Of No votes, only 25% came from men vs 75% from women.
# About 40% of abstainers were men and 60% women.

# d) Mosaic plot with appropriate colors and legend
mosaicplot(voting_counts, col = c("skyblue", "salmon", "lightgreen"),
           main = "Mosaic Plot: Gender vs Voting Result")
legend("topright", legend = colnames(voting_counts), fill = c("skyblue", "salmon", "lightgreen"), title = "Vote")

# e) Side-by-side barplot for Gender and Vote
barplot(t(voting_counts), beside = TRUE,
        col = c("skyblue", "salmon", "lightgreen"),
        legend.text = TRUE, args.legend = list(x = "topright", inset = 0.02),
        main = "Voting Results by Gender", xlab = "Gender", ylab = "Number of People")

# Part 3: Pairwise plots for midsize dataset
data(midsize)  # load the midsize dataset (UsingR package)
# a) Show pairwise scatterplots for all variables
pairs(midsize, main = "Pairwise Scatterplots for midsize data")
# b) Provide interpretations of the pairwise relationships
# 1. Newer cars have higher prices. Each model's price increases as Year approaches 2004,the new car price point.
# 2. All three car models' prices are strongly positively correlated with each other. When one model is expensive in a year, the others tend to be expensive as well
# 3. The Ford Taurus generally has lower prices than the Honda Accord or Toyota Camry for the same year, indicating it depreciates more.
# 4. The relationship between a car's age since 2004 and its price appears roughly linear for each model, suggesting a steady depreciation rate over time.

# Part 4: 
data(MLBattend)
# a) Extract the wins for teams BAL, BOS, DET, LA, PHI into vectors
BAL_wins <- MLBattend %>% filter(franchise == "BAL") %>% arrange(year) %>% pull(wins)
BOS_wins <- MLBattend %>% filter(franchise == "BOS") %>% arrange(year) %>% pull(wins)
DET_wins <- MLBattend %>% filter(franchise == "DET") %>% arrange(year) %>% pull(wins)
LA_wins  <- MLBattend %>% filter(franchise == "LA")  %>% arrange(year) %>% pull(wins)
PHI_wins <- MLBattend %>% filter(franchise == "PHI") %>% arrange(year) %>% pull(wins)

# b) Create a data frame with these vectors
wins_df <- data.frame(BAL = BAL_wins, BOS = BOS_wins, DET = DET_wins, LA = LA_wins, PHI = PHI_wins)
print(head(wins_df))  # show first few rows of the data frame

# c) Display a boxplot of the wins data for the five teams
boxplot(wins_df, main = "Distribution of Wins (1969-2000) for Selected Teams",
        xlab = "Team", ylab = "Wins per Season", col = "lightgray")

# d) Interpretations of the boxplot:
# 1. The LA Dodgers (LA) have a higher median number of wins compared to the other teams, indicating they were generally more successful during 1969-2000.
# 2. The Detroit Tigers (DET) and Philadelphia Phillies (PHI) show lower median wins, reflecting more losing seasons on average in this period.
# 3. Baltimore (BAL) has a very large range of wins, including some exceptionally high-win seasons (over 100 wins) and some very low-win seasons (around 54 wins).
# 4. Several teams have notable outliers. For example, BAL has a high-win outlier season, and DET has low-win outlier seasons.
# 5. The variability in wins is greater for teams like DET and PHI (wider boxes and longer whiskers), suggesting their performance fluctuated more year-to-year. In contrast, LA appears more consistent. 

