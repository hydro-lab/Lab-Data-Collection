library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(latex2exp)

# READ IN AND SORT
x <- read_csv("/Users/davidkahler/Documents/R/Lab-Data-Collection/FisherHall.csv", col_names = FALSE)
x <- x %>%
     select(-X1, -X4, -X7, -X8, -X9, -X10) %>%
     rename(unix_utc = X2,
            time_et = X3,
            Variable = X5,
            Value = X6)

this <- week(Sys.Date())
lastweek <- x %>%
     mutate(w = week(time_et)) %>% 
     filter(w == (this-1)) %>% 
     mutate(d = as_date(time_et)) %>%
     select(time_et,d,Variable,Value)

air_temp <- filter(lastweek, Variable == "AirTC_Avg")
air_temp <- ggplot(air_temp) + 
     geom_line(aes(x=time_et,y=Value)) + 
     scale_x_datetime() +
     xlab("Date") +
     ylab(TeX('Air Temperature $(^o C)$')) +
     #ylab("Air Temperature (Celcius)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 0.3) +
     theme(axis.text = element_text(face = "plain", size = 12), axis.title = element_text(face = "plain", size = 14))
ggsave("FISHERtemp.jpg", plot = air_temp, device = "jpeg", dpi = 72, width = 20, height = 10, units = "cm")

rh <- filter(lastweek, Variable == "RH")
rh <- ggplot(rh) + 
     geom_line(aes(x=time_et,y=Value)) + 
     scale_x_datetime() +
     xlab("Date") +
     ylab("Relative Humidity (%)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 0.3) +
     theme(axis.text = element_text(face = "plain", size = 12), axis.title = element_text(face = "plain", size = 14))
ggsave("FISHERrh.jpg", plot = rh, device = "jpeg", dpi = 72, width = 20, height = 10, units = "cm")

bp <- filter(lastweek, Variable == "BV_BP_Avg")
bp <- ggplot(bp) + 
     geom_line(aes(x=time_et,y=Value)) + 
     scale_x_datetime() +
     xlab("Date") +
     ylab("Barometric Pressure (hPa)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 0.3) +
     theme(axis.text = element_text(face = "plain", size = 12), axis.title = element_text(face = "plain", size = 14))
ggsave("FISHERpres.jpg", plot = bp, device = "jpeg", dpi = 72, width = 20, height = 10, units = "cm")

precip <- lastweek %>%
     filter(Variable == "Rain_mm_Tot") %>%
     group_by(d) %>%
     summarize(Value = sum(Value), dt = mean(time_et))
precip <- ggplot(precip) + 
     geom_col(aes(x=dt,y=Value)) + 
     scale_x_datetime() +
     xlab("Date") +
     ylab("Daily Precipitation (mm)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 0.3) +
     theme(axis.text = element_text(face = "plain", size = 12), axis.title = element_text(face = "plain", size = 14))
ggsave("FISHERprecip.jpg", plot = precip, device = "jpeg", dpi = 72, width = 20, height = 10, units = "cm")



