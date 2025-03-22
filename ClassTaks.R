help(USJudgeRatings)
head(USJudgeRatings)
names(USJudgeRatings)

USJudgeRatings[, 1]
USJudgeRatings[, c(1, 2)]
USJudgeRatings[, -1]

USJudgeRatings[1, ]

USJudgeRatings[1, 1]

USJudgeRatings["PHYS"]

plot(USJudgeRatings$PREP, USJudgeRatings$ORAL)
#Scatter Plot
plot(USJudgeRatings$PREP, USJudgeRatings$ORAL,
     xlab = "PREP", ylab = "ORAL",
     main = "Scatter Plot",
     pch = 10, xlim = c(0,10), ylim = c(0,9),
     col = "red")
#Adding Regression Line
reg_model <- lm(ORAL~PREP, data = USJudgeRatings)
abline(reg_model, col = "blue", lwd = 2)

#Scatter Plot Matrix

pairs(~PREP+FAMI+ORAL+WRIT, data = USJudgeRatings,
      main = "Simple ScatterPlot Matrix")

x = USJudgeRatings[2:11]
y = USJudgeRatings[2:11]
cor(x,y)

#Correlograms
library(corrgram)
corrgram(USJudgeRatings[2:11], order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Correlgram")

#Histogram
hist(USJudgeRatings$ORAL, col = "gray")

#Histrogram = Adding a normal Curve
x = USJudgeRatings$ORAL
h = hist(x, breaks = 10, col = "red",
         xlab = "ORAL",
         main = "Histogram wth Normal Curve")

xfit = seq(min(x), max(x), length=20)
yfit = dnorm(xfit, mean = mean(x), sd=sd(x))
yfit1=yfit*0.5*length(x)
lines(xfit, yfit1, col = "blue", lwd = 2)




#AirQuality

help("airquality")
data("airquality")
head(airquality)
library(ggplot2)
library(gplots)

#BoxPlot
boxplot(airquality$Wind~airquality$Month, col=c("red","blue","grey","orange","green"))
legend("topright", title = "Month", c("May", "Jun", "Jul", "Aug", "Sep"),
       pch = 15, col = c("red", "blue", "grey", "orange", "green"))

#Compute Means and SD for each Month
means_w = by(airquality$Wind, airquality$Month, mean)
SD_w = by(airquality$Wind, airquality$Month, sd)

barplot2(means_w, xlab ="Month", ylab="Wind", col="YellowGreen",
         plot.ci = TRUE, ci.u = means_w+SD_w, ci.l=means_w-SD_w)




library(psych)


salary = c(5.4, 6.6, 6.1, 7.5, 5.6, 7.3, 6.9, 7.5, 6.6, 7.1)
gender = c("f", "f", "f", "f", "m", "m", "m", "f", "m", "f")
EUR_salary = data.frame(salary, gender)
EUR_salary
head(EUR_salary)

mean(EUR_salary$salary)
library(pastecs)
result1 = by(EUR_salary$salary, EUR_salary$gender, stat.desc)
result1

summary_table <- data.frame(
  gender = c("Female(f)", "Male(m)"),
  do.call(cbind, lapply(result1, as.data.frame))
)

