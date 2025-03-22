# Creating the contingency table
hotel_survey <- matrix(c(128, 199, 186, 88, 33, 66), 
                       nrow = 2, byrow = TRUE,
                       dimnames = list(Answer = c("Yes", "No"), 
                                       Hotel = c("Hotel1", "Hotel2", "Hotel3")))
#Display the table
hotel_survey

# Calculating frequencies and proportions
total_responses <- sum(hotel_survey)
freq_table <- prop.table(hotel_survey)  # Proportions (frequencies)2
freq_table #Proportions
freq_table * 100  # Convert to percentages

# Chi-square test for independence
chisq_test <- chisq.test(hotel_survey)
chisq_test
#The likelihood of a guest choosing the hotel again depends
#on which hotel they stayed in, inclduing differences in satisfaction levels among hotels. 

#HYPOTHESIS
####
# Null Hypothesis(H0): There is no relationship between hotel and guests'decision to stay again. independent of hotel.
# Alternative Hypothesis(H1): There is a significant relationship between hotel and guest's decision to stay again. (depends on hotel)

# Hypothesis:
# H0: There is no relationship between the response (Yes/No) and the hotel.
# H1: There is a significant relationship between the response (Yes/No) and the hotel.


# Load necessary library
library(vcd)

# Create the contingency table
hotel_survey <- matrix(c(128, 199, 186, 88, 33, 66), 
                       nrow = 2, 
                       byrow = TRUE,
                       dimnames = list(Choose_Again = c("Yes", "No"),
                                       Hotel = c("Hotel1", "Hotel2", "Hotel3")))

# Compute association statistics
assoc_stats <- assocstats(hotel_survey)

# Extract Cramér's V
cramer_v <- assoc_stats$cramer
cramer_v

# Compute Contingency Coefficient
contingency_coeff <- sqrt(assoc_stats$chisq / (assoc_stats$chisq + sum(hotel_survey)))
contingency_coeff


# Checking coefficients of association
library(vcd)
assocstats(hotel_survey)

# Interpretation:
# - If the p-value from the chi-square test is less than 0.05, we reject H0 and conclude there is a significant relationship.
# - Cramér’s V and contingency coefficient will show the strength of the relationship.



#Since p-value is much smaller than 0.05 we reject null hypothesis.
#THis means there is a significant relationship between hotel and guests decisionsto choose the hotel again.
# The likelihood of guests returning depends on the hotel they stayed in. 



library(vcd)  # For association measures

# Create the contingency table
hotel_survey <- matrix(c(128, 199, 186, 88, 33, 66), 
                       nrow = 2, 
                       byrow = TRUE,
                       dimnames = list(Choose_Again = c("Yes", "No"),
                                       Hotel = c("Hotel1", "Hotel2", "Hotel3")))

# Compute Cramér's V
cramer_v <- assocstats(hotel_survey)$cramer
cramer_v

# Compute Phi coefficient (only for 2x2, so not applicable here)

# Compute Contingency Coefficient
contingency_coeff <- sqrt(40.228 / (40.228 + sum(hotel_survey)))
contingency_coeff
