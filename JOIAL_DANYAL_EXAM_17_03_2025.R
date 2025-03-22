head(employee_data)
library(dplyr)
library(pastecs)

# Compute descriptive statistics for educ_year by sex
desc_stat <- by(employee_data$educ_year, employee_data$sex, stat.desc)
desc_stat


desc_stat <- employee_data %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    average = mean(educ_year, na.rm = TRUE),
    S = sd(educ_year, na.rm = TRUE),
    V_perc = (S / average)*100
  )

print(desc_stat)


#Conclusion: 
# Females have a higher average education duration compared to males with 12.37 years.
# Median education years of females(15 years) is also greater then for males(12 years).
#The higher SD(2.98 for female) and coef-variation(20.65) suggests that female education years
# are more spread out compared to male. 


#BoxPlots and Histogram
library(ggplot2)


#Histogram
ggplot(employee_data, aes(x = educ_year)) +
  geom_histogram(aes(y = ..density..), fill = "SteelBlue", color = "black", alpha = 0.7) +  
  geom_line(data = data.frame(x = educ_density$x, y = educ_density$y), aes(x = x, y = y), color = "green", linewidth = 1) +  
  labs(title = "Histogram of Education Years", x = "Education Years", y = "Density")

#BarPlot
barplot2(educ_means, beside = TRUE, col = c("YellowGreen","Blue"),
         plot.ci = Tggplot(employee_data, aes(x=educ_year))+
           geom_histogram(bins=20, fill = "SteelBlue", color="black")+
           labs(title="Histogram of Education Years", x = "Education Years", y = "Frequency ")
         RUE, ci.u = educ_means + educ_sd, ci.l = educ_means - educ_sd,
         names.arg = c("Male","Female"),
         main = "Average Education YEars by Sex", xlab = "Sex", ylab = "Education")

##### Ex. 2. 

#define Variables
x_var <- employee_data$job_time
y_var <- employee_data$salary

#Scatter Plot
plot(x_var, y_var, main = "Salary vs Job Time", xlab = "Job Time", ylab = "Salary", pch = 19, col = "blue")

#Correlation Coefficients
cor_coeff <- cor(x_var, y_var, use = "complete.obs")
print(paste("Correlation Coefficients:", cor_coeff))
#Evaluation: The value(0.084) is very close to 0 suggesting a weak relationship, almost non linear.
#As it is +ve it suggests a slight positive correlation.


#Linear Model
model <- lm(y_var, x_var, data = employee_data)
summary(model)

abline(model, col = "green", lwd = 2)

# Evaluation of b0 and b1 and conclusions: b0=22843.32, b1=142.72 and P-Value=0.000365
# It suggests every 1-unit increase in x_var, y is expected to increase by 142.72 units.
# P-value is greater then 0.05 suggesting relationship is not statistical significantly at the 5% level..
#Conclusion:
# The regression model is suggesting a very weak relatinship betweenx_var and y.
# Intercept(b0) is significant, the slope(b1) is not significant.

#b1 - the slope represents the expected change in y for 1-unit increase in x_var.
#142.72 suggests a positive relationship, when x_var increases, y tend to increase.



#Predict y for a given x(lets say x = 50)
x_new <- data.frame(x_var = 50)
predicted_y <- predict(model, x_new)
print(paste("Predicted y for x = 50", predicted_y))


#R-Squared Computation
r_squered<-summary(model)$r.squared
r_squered

#The 0.00071 value suggests the x_var barely explains any variability in y, making this model unreliable for prediction.


shapiro.test(residuals_values)
plot(model$fitted.values, resid(model),
     main = "Residuals vs. Fitted",
     xlab = "Fitted Values", ylab="Residuals")
abline(h=0, col="blue", lwd=2)



#### EX. 3 ########


#Table
sex_position_table<- table(employee_data$sex, employee_data$position)
print(sex_position_table)

#Chi Test
chi_test <- chisq.test(sex_position_table)
print(chi_test)
#Conclsions: Since the P-value is exteremly small, we reject the null hypothesis.
#There is a significant relatinship between sex and position in the data.
#SEx and position are not independant. 

#Coefficients of Associations
library(vcd)
cramers_v <- assocstats(sex_position_table)$cramer
print(paste("Cramer V:", cramer_v))


#Conclusion: Since the value we have got is 0.37, so we conclude tehre is a moderate association between sex and position.
# 0.3-0.5 moderate association 

