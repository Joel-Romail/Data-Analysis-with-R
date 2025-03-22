# Task Food # 2

head(food_consump)
install.packages("ggplot2")
library(ggplot2)

model <- lm(Food_EUR ~ Salary, data = food_consump)
summary(model)

# Regression Equation: Food_EUR = 25.29196 + 0.0730199 * Salary
# Meaning of b1:For Every 1 EUR increase in salary,
# food consumption increases by 0.073EUR.
# The P-value is very significant. 


# Predictions for Salary = 500 and 800
salary_values <- data.frame(Salary = c(500, 800))
predictions <- predict(model, salary_values)
cat("Predicted Food Consumption for Salary 500 EUR:", predictions[1], "EUR\n")
cat("Predicted Food Consumption for Salary 800 EUR:", predictions[2], "EUR\n")


r_squared <- summary(model)$r.squared
cat("R-squared:", r_squared)
# R-Squered Meaning: Since R-squered is close to 1, 
# So the model fits the data well.
# It can be used to predict food consumption based on salary. 

#PLOT
ggplot(food_consump, aes(x = Salary, y = Food_EUR)) +
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "Regression: Salary vs Food Consumption (EUR)", 
       x = "Salary (EUR)", 
       y = "Food Consumption (EUR)")
