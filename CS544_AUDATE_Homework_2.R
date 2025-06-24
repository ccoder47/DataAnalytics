#PART 1:
#Problem A).
p_D     <- 0.05    # prevalence P(disease)
p_notD  <- 1 - p_D
sens    <- 0.92    # sensitivity  P(positive | disease)
fpr     <- 0.06    # false‐positive rate P(positive | no disease)

# marginals
p_pos  <- sens*p_D + fpr*p_notD
p_neg  <- (1 - sens)*p_D + (1 - fpr)*p_notD

# posteriors
p_notD_given_pos <- (fpr * p_notD) / p_pos
p_D_given_neg   <- ((1 - sens) * p_D) / p_neg

# report
data.frame(
  event       = c("P(no disease | positive)", "P(disease | negative)"),
  probability = c(p_notD_given_pos,  p_D_given_neg)
)


# We are given the probability of a voter belonging to a party and
# the probability that a person from that party answers 'Yes' to the
# universal healthcare question.
# We need to find the probability that a person belongs to a specific party
# GIVEN that they answered 'No'.
# This is a Bayes' Theorem problem.
#
# Let:
# D, R, I be the events that a person is a Democrat, Republican, or Independent.
# Y be the event that a person answers 'Yes'.
# N be the event that a person answers 'No'.
#
# We want to find P(D|N), P(R|N), and P(I|N).
#
# Bayes' Theorem states: P(A|B) = [P(B|A) * P(A)] / P(B)
# For our case: P(Democrat | No) = [P(No | Democrat) * P(Democrat)] / P(No)
#-----------------------------------------------------------------------

#Problem B).
p <- c(D = 0.42,   # P(D)
       R = 0.48,   # P(R)
       I = 0.10)   # P(I)

# P(No | party) = 1 - P(Y | party)
pNo_given <- c(D = 1 - 0.85,  # 0.15
               R = 1 - 0.50,  # 0.50
               I = 1 - 0.60)  # 0.40

#Marginal P(No)
pNo <- sum(p * pNo_given)

#Posterior P(party | No)
p_given_No <- (p * pNo_given) / pNo

#Display results
pNo
#> [1] 0.343

p_given_No
#>        D        R        I 
#> 0.183652 0.699416 0.116932 

# As percentages:
round(100 * p_given_No, 1)
#>    D    R    I 
#> 18.4 69.9 11.7 

# In a drama class, there are 12 females and 10 males.
# A group of 15 is to be chosen at random for a play.

#Problem C).
females <- 12
males <- 10
total_students <- females + males
group_size <- 15

# Calculate the total number of ways to choose 15 students from 22
total_combinations <- choose(total_students, group_size)

cat("Total possible ways to choose 15 students:", total_combinations)


# i) What is the probability that all males were chosen? 
# This means we choose 10 males from 10, and the remaining 5 people from the 12 females.
ways_all_males <- choose(males, 10) * choose(females, group_size - 10)
prob_all_males <- ways_all_males / total_combinations
cat("Ways to choose all 10 males and 5 females:", ways_all_males)
cat("Probability that all males were chosen:", prob_all_males)


# ii) What is the probability that at least 5 males were chosen? ---
# This means the number of males can be 5, 6, 7, 8, 9, or 10.
# We can use the hypergeometric distribution probability function, dhyper.
# dhyper(x, m, n, k) where:
# x = number of successes (males)
# m = total number of items with the desired trait (total males)
# n = total number of items without the trait (total females)
# k = the number of items drawn (group size)
# We sum the probabilities for x = 5 through 10.
prob_at_least_5_males <- sum(dhyper(5:10, m = males, n = females, k = group_size))
cat("Probability that at least 5 males were chosen:", prob_at_least_5_males)


# iii) What is the probability that at most 5 males were chosen?
# We must choose 15 people. If we choose all 12 females, we still need 3 more people,
# So, "at most 5" means the number of males can be 3, 4, or 5.
prob_at_most_5_males <- sum(dhyper(3:5, m = males, n = females, k = group_size))
cat("Probability that at most 5 males were chosen:", prob_at_most_5_males)

# parameters
m <- 10   # number of males in the class
n <- 12   # number of females in the class
k <- 15   # size of the group drawn

# (i) P(all 10 males chosen)
p_all_males   <- dhyper(10, m = m, n = n, k = k)

# (ii) P(at least 5 males chosen) = 1 − P(X ≤ 4)
p_at_least_5  <- 1 - phyper(4, m = m, n = n, k = k)

# (iii) P(at most 5 males chosen) = P(X ≤ 5)
p_at_most_5   <- phyper(5, m = m, n = n, k = k)

data.frame(
  event = c("all 10 males", "≥ 5 males", "≤ 5 males"),
  probability = c(p_all_males, p_at_least_5, p_at_most_5)
)

#PART 2:
# load the air quality data
data(airquality)

# Inspect the first few rows
df <- airquality
# check
head(df)

sum(df$DIFFS > 0)

# diff(df$Temp)
df$DIFFS <- c(0, diff(df$Temp))
# check
head(df)
sum(df$DIFFS > 0)
library(dplyr)

monthly_stats <- df %>%
  group_by(Month) %>%
  summarise(
    avg = mean(Temp,   na.rm = TRUE),
    med = median(Temp, na.rm = TRUE)
  ) %>%
  arrange(Month)

# Print with month names
for(i in seq_len(nrow(monthly_stats))) {
  m   <- monthly_stats$Month[i]
  av  <- monthly_stats$avg[i]
  md  <- monthly_stats$med[i]
  cat(sprintf(
    "The average and median temperature for %s are %.1f and %.1f\n",
    month.name[m], av, md
  ))
}

monthly_extremes <- df %>%
  group_by(Month) %>%
  summarise(
    coldest = Day[which.min(Temp)],
    hottest = Day[which.max(Temp)]
  ) %>%
  arrange(Month)

for(i in seq_len(nrow(monthly_extremes))) {
  m <- monthly_extremes$Month[i]
  c <- monthly_extremes$coldest[i]
  h <- monthly_extremes$hottest[i]
  cat(sprintf(
    "The coldest and hottest days in %s are %d and %d\n",
    month.name[m], c, h
  ))
}


