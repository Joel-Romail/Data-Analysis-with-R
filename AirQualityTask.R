# Load required libraries
library(ggplot2)
library(GGally)  # For scatter matrix
library(gplots)  # For barplot2

# Load dataset
data("airquality")

### 1. Histogram with Normal Curve for Wind
ggplot(airquality, aes(x = Wind)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "lightblue", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Histogram of Wind with Normal Curve", x = "Wind Speed", y = "Density") +
  theme_minimal()

### 2. Histogram with Normal Curve for Temp
ggplot(airquality, aes(x = Temp)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "lightgreen", color = "black") +
  geom_density(color = "blue", linewidth = 1) +
  labs(title = "Histogram of Temperature with Normal Curve", x = "Temperature (F)", y = "Density") +
  theme_minimal()

### 3. Scatter Plot of Wind vs. Temp
ggplot(airquality, aes(x = Wind, y = Temp)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot: Wind vs. Temperature", x = "Wind Speed", y = "Temperature (F)") +
  theme_minimal()

### 4. Scatter Matrix for Ozone, Solar.R, Wind, and Temp
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], 
      main = "Scatter Matrix of Air Quality Variables", 
      col = "blue", pch = 19)

# Alternatively, using GGally:
ggpairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")]) + 
  labs(title = "Scatter Matrix of Air Quality Variables")

### 5. Bar Plot of Average Monthly Temperature with SD
means_temp = tapply(airquality$Temp, airquality$Month, mean)
SD_temp = tapply(airquality$Temp, airquality$Month, sd)

barplot2(means_temp, beside = TRUE, col = "coral", 
         plot.ci = TRUE, ci.u = means_temp + SD_temp, ci.l = means_temp - SD_temp,
         xlab = "Month", ylab = "Average Temperature (F)", 
         main = "Average Monthly Temperature with SD")

