head(salary_13_15_1_)
# Load necessary libraries
library(ggplot2)
library(gplots)
library(psych)
library(pastecs)


# Check the structure and summary
head(salary_13_15_1_)
summary(salary_13_15_1_)


library(pastecs)

salary_summary1 <- by(salary_13_15_1_$pablic, salary_13_15_1_$year, stat.desc)
salary_summary1
salary_summary2 <- by(salary_13_15_1_$private, salary_13_15_1_$year, stat.desc)
salary_summary2
salary_summary3 <- by(salary_13_15_1_$total, salary_13_15_1_$year, stat.desc)
salary_summary3


#Tables
library(dplyr)
df_pablic <- do.call(rbind, salary_summary1) %>% as.data.frame()
df_private <- do.call(rbind, salary_summary2) %>% as.data.frame()
df_total <- do.call(rbind, salary_summary3) %>% as.data.frame()

df_pablic$year <- rownames(df_pablic)
df_private$year <- rownames(df_private)
df_total$year <- rownames(df_total)


df_pablic$variable <- "Public"
df_private$variable <- "Private"
df_total$variable <- "Total"


salary_summary_table <- bind_rows(df_pablic, df_private, df_total)


salary_summary_table <- salary_summary_table %>% select(year, variable, everything())

print(salary_summary_table)


salary_summary <- by(salary_13_15_1_[, -which(names(salary_13_15_1_) == "year")], 
                     salary_13_15_1_$year, 
                     stat.desc, basic = FALSE, norm = FALSE)
salary_summary



# Descriptive Statistics by Year
# Compute descriptive statistics using aggregate


salary_summary <- aggregate(. ~ year, data = salary_13_15_1_, 
                            FUN = function(x) c(Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)))
library(pastecs)
stat.desc(salary_13_15_1_)
result = by()
data_sal = data.frame(salary_13_15_1_)
data_sal



# Print the summary
salary_summary

# Boxplots: Salary in Public and Private Sectors by Years
boxplot(salary_13_15_1_$pablic ~ salary_13_15_1_$year, col = c("red", "blue", "green"),
        main = "Public Sector Salary by Year", xlab = "Year", ylab = "Salary (EUR)")

boxplot(salary_13_15_1_$private ~ salary_13_15_1_$year, col = c("red", "blue", "green"),
        main = "Private Sector Salary by Year", xlab = "Year", ylab = "Salary (EUR)")

# Compute Mean and Standard Deviation for Public & Private Sectors by Year
public_means <- by(salary_13_15_1_$pablic, salary_13_15_1_$year, mean)
public_sd <- by(salary_13_15_1_$pablic, salary_13_15_1_$year, sd)

private_means <- by(salary_13_15_1_$private, salary_13_15_1_$year, mean)
private_sd <- by(salary_13_15_1_$private, salary_13_15_1_$year, sd)

# Barplot with Error Bars (Public & Private Sector Salaries)
barplot2(public_means, beside=TRUE, col="YellowGreen", plot.ci=TRUE, 
         ci.u=public_means+public_sd, ci.l=public_means-public_sd,
         main="Public Sector Salaries by Year", xlab="Year", ylab="Salary (EUR)")

barplot2(private_means, beside=TRUE, col="SteelBlue", plot.ci=TRUE, 
         ci.u=private_means+private_sd, ci.l=private_means-private_sd,
         main="Private Sector Salaries by Year", xlab="Year", ylab="Salary (EUR)")


#

# Scatterplot: Public vs Private Sector Salary
plot(salary_13_15_1_$pablic, salary_13_15_1_$private, xlab = "Public Sector Salary", ylab = "Private Sector Salary",
     main = "Scatter Plot: Public vs Private Salary", col = "red", pch = 16)

# Regression Analysis: Public vs Private Salary
reg_model <- lm(private ~ pablic, data = salary_13_15_1_)
summary(reg_model)

# Add Regression Line
abline(reg_model, col = "blue", lwd = 2)

# Compute R-squared
r_squared <- summary(reg_model)$r.squared
r_squared

# Compute Fitted Values and Residuals
fitted_values <- fitted(reg_model)
residuals_values <- residuals(reg_model)

# Normality Check of Residuals
shapiro.test(residuals_values)
hist(residuals_values, main="Residuals Histogram", col="gray", xlab="Residuals")

# Conclusion:
# The regression coefficient (b1) represents the change in private sector salary for each 1 EUR increase in public sector salary.
# The R-squared value indicates how well the public sector salary explains the variation in private sector salary.



library(dplyr)

# Compute summary statistics
salary_summary <- salary_13_15_1_ %>%
  group_by(year) %>%
  summarise(
    total_mean = mean(total),
    total_sd = sd(total),
    public_mean = mean(pablic),
    public_sd = sd(pablic),
    private_mean = mean(private),
    private_sd = sd(private)
  )

# Print table
salary_summary


# Regression model
salary_model <- lm(private ~ pablic, data = salary_13_15_1_)
model_summary <- summary(salary_model)

# Extract coefficients
regression_table <- data.frame(
  Term = names(model_summary$coefficients[,1]),
  Estimate = model_summary$coefficients[,1],
  Std_Error = model_summary$coefficients[,2],
  t_value = model_summary$coefficients[,3],
  p_value = model_summary$coefficients[,4]
)

# Print regression table
regression_table


# Compute correlation
correlation_value <- cor(salary_13_15_1_$pablic, salary_13_15_1_$private)

# Create correlation table
correlation_table <- data.frame(
  Variable1 = "Public Salary",
  Variable2 = "Private Salary",
  Correlation = correlation_value
)

# Print correlation table
correlation_table

#Salaries in Latvia increased steadily from **2013 to 2015**, 
#with public sector wages consistently higher than private sector wages. 
#However, public sector salaries showed greater variability. 
#Regression analysis indicates a **strong positive correlation (0.878)** 
#  between public and private sector salaries, with **77% of
#private sector salary variations explained by public sector salaries**.


library(ggplot2)

# Scatter plot: Public vs. Private Salary
ggplot(salary_13_15_1_, aes(x = pablic, y = private)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot: Public vs Private Salary",
       x = "Public Sector Salary (EUR)",
       y = "Private Sector Salary (EUR)") +
  theme_minimal()



# Calculate correlation coefficient
correlation <- cor(salary_13_15_1_$pablic, salary_13_15_1_$private)

# Print result
correlation



# Fit linear regression model
model <- lm(private ~ pablic, data = salary_13_15_1_)

# Display regression summary
summary(model)


b0 <- coef(model)[1]  # Intercept
b1 <- coef(model)[2]  # Slope

# Print regression equation
cat("Regression Model: Private Salary =", round(b0, 3), "+", round(b1, 3), "* Public Salary\n")

# Conclusions: 
# b0=89.44, when public salary is 0, 
# the predicted private salary would be 89.44
#b1=0.805, for every 1 euro increase in public sect salary
# the private sect salary increase by 0.805 eur on avg.
# the hight t-value(10.706) and ver low p-value indicate,
# that the relationship is significant.
# The +ve slope confirm a strong correlation
# between public and private sect salaries.
# multiple r-squered(0.7712) suggests 77.12% of variation

#The coefficient b1=0.805 means that every 1 eur increase,
# in public set salary, the private sect salary,
# increases by approx 0.805 eur. 
# this indicate a strong relationship between two sectors,
#suggesting that changes in public sect wages have a significant,
# impact on private sect salaries.


#R-Squered = 0.7712, which means 77.12% of the variation in private sector
#salaries can be explained by the public sector salaries. 
#This indicate a strong relationship between two variables.
#However the 22.88% of the variation is due to other factors not included in the model. 


# Compute fitted values
fitted_values <- fitted(model)
plot(model, which = 1)

plot(model, which = 2)
# Compute residuals
residuals_values <- residuals(model)

# Display first few values
head(data.frame(Fitted = fitted_values, Residuals = residuals_values))




# 1. Linearity check: Scatter plot with regression line
plot(salary_13_15_1_$pablic, salary_13_15_1_$private,
     main = "Scatterplot of Public vs Private Salary",
     xlab = "Public Salary", ylab = "Private Salary", pch = 19, col = "blue")
abline(reg_model, col = "red", lwd = 2)

# 2. Independence check: Durbin-Watson test
library(lmtest)
dwtest(model)

# 3. Homoscedasticity check: Residuals vs Fitted plot
plot(fitted_values, residuals_values,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# 4. Normality check: Histogram and Q-Q plot
hist(residuals_values, breaks = 15, col = "gray", main = "Histogram of Residuals")
qqnorm(residuals_values, main = "Q-Q Plot of Residuals")
qqline(residuals_values, col = "red")

# Shapiro-Wilk test for normality
shapiro.test(residuals_values)

#Since p-value > 0.05, we fail to reject null hypothesis,
# meaning the residuals are normally distributed.
#The regression model is statistically valid regarding
#normality, and no transformation of variable is needed.

