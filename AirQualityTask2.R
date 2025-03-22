help("airquality")
summary(airquality)
library(pastecs)
library(dplyr)

par1=by(airquality, airquality$Month, stat.desc)
par1


summary_table <- airquality %>%
  group_by(Month)%>%
  summarise(
    Ozone_Mean = mean(Ozone,na.rm = TRUE),
    Ozone_SD = sd(Ozone,na.rm = TRUE),
    SolarR_Mean = mean(Solar.R,na.rm = TRUE),
    Wind_Mean = mean(Wind,na.rm = TRUE),
    Temp_Mean = mean(Temp,na.rm = TRUE),
  )

print(summary_table)

#Ozone levels increase from May (23.6 ppb) to a peak in August (60.0 ppb) before dropping in September (31.4 ppb), likely due to seasonal temperature changes and atmospheric conditions. Solar radiation is highest in July (216 W/m²) and decreases in later months, influencing ozone formation. The data suggests that higher temperatures and sunlight contribute to increased ozone levels, while cooler conditions in September lead to a decline.





