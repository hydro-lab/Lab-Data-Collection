library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(latex2exp)

# READ IN AND SORT

x <- read_csv("Fisher711_FisherMet.dat", skip = 4, col_names = FALSE)
t <- read_csv("FisherHall_download.csv") # all in UTC
previousDownload <- t$RECORD[nrow(t)]
download <- data.frame(Sys.Date(), x$X1[nrow(x)], x$X2[nrow(x)])
write_csv(download, "FisherHall_download.csv", append = TRUE)

y <- x %>%
     rename(TIMESTAMP = X1, 
            RECORD = X2, 
            PTemp_C_Avg = X3, 
            AirTC_Avg = X4, 
            AirTC_Std = X5, 
            RH = X6, 
            BV_BP_Avg = X7, 
            SlrW_Avg = X8, 
            CS320_Angle_Avg = X9, 
            Rain_mm_Tot = X10) %>%
     filter(RECORD > previousDownload) %>% # this should filter out previous download, record starts at 0, 
     mutate(time_utc = ymd_hms(TIMESTAMP), 
            unix_utc = as.numeric(time_utc), 
            time_et = with_tz(time_utc, tz = "US/Eastern"), 
            utc_offset = (as.numeric(force_tz(time_et, tz = "UTC")) - unix_utc)/3600 ) %>%
     select(-TIMESTAMP, -RECORD, -CS320_Angle_Avg, -PTemp_C_Avg)

# PIVOT TO LONGER FORMAT, ADD SITE, SOURCE, and QC flag
# LEVEL 0 is raw data

z <- pivot_longer(y, cols = c(AirTC_Avg,AirTC_Std,RH,BV_BP_Avg,SlrW_Avg,Rain_mm_Tot),
                  names_to = "Variable",
                  values_to = "Value")
z$Site <- "Duquesne_FisherHall"
z$Source <- "DuqSciEng"
z$QC <- "Level0"

z$Method <- NA
n <- nrow(z)
for (i in 1:n) {
     if (z$Variable[i]=="AirTC_Avg") {z$Method[i] = "Thermometer_hygrometer"}
     else if (z$Variable[i]=="AirTC_Std") {z$Method[i] = "Thermometer_hygrometer"}
     else if (z$Variable[i]=="RH") {z$Method[i] = "Thermometer_hygrometer"}
     else if (z$Variable[i]=="SlrW_Avg") {z$Method[i] = "Thermopile_pyranometer"}
     else if (z$Variable[i]=="SlrW_Std") {z$Method[i] = "Thermopile_pyranometer"}
     else if (z$Variable[i]=="CS320_Temp_Avg") {z$Method[i] = "Thermopile_pyranometer"}
     else if (z$Variable[i]=="CS320_Angle_Avg") {z$Method[i] = "Thermopile_pyranometer"}
     else if (z$Variable[i]=="Rain_mm_Tot") {z$Method[i] = "Heated_tipping_bucket"}
}

level0 <- z %>%
     mutate(time_utc = as.character(time_utc)) %>% 
     mutate(time_et = as.character(time_et))
write_csv(level0, "/Users/davidkahler/Documents/R/Lab-Data-Collection/FisherHall.csv", append = TRUE)

# PERFORM QC ON SENSORS: LIMITS, SPIKES, 
# LEVEL 1 is minimal QC

n <- nrow(y)
for (i in 1:n) {
     if ( (is.na(y$AirTC_Avg[i])) | (is.na(y$AirTC_Std[i])) ) {
          y$AirTC_Avg[i] <- -9999
          y$AirTC_Std[i] <- -9999
     } else if (y$AirTC_Avg[i] < -40) {
          y$AirTC_Avg[i] <- -9999
          print(paste0("Temperature below valid range.  Check UTC: ", y$time_utc[i]))
     } else if (y$AirTC_Avg[i] > 70) {
          y$AirTC_Avg[i] <- -9999
          print(paste0("Temperature above valid range.  Check UTC: ", y$time_utc[i]))
     } else if ((i > 4) & (i < (n-4))) {
          s <- 0 # standard deviation
          for (j in (j-4):(j+4)) {
               s <- max(c(max(y$AirTC_Std[(i-4):(i-1)], na.rm = TRUE), max(y$AirTC_Std[(i+1):(i+4)], na.rm = TRUE)))
               if ( ( y$AirTC_Avg[i] > t+(8*s) ) | ( y$AirTC_Avg[i] < t-(8*s) ) ) {
                    print(paste0("Spike detected; manual intervention needed at UTC: ", z$time_utc[i]))
               }
          }
     }
}

z <- y %>%
     select(AirTC_Avg, time_utc, unix_utc, time_et, utc_offset) %>%
     pivot_longer(cols = AirTC_Avg,
                  names_to = "Variable",
                  values_to = "Value")
z$Site <- "Duquesne_FisherHall"
z$Source <- "DuqSciEng"
z$QC <- "Level1"
z$Method <- "Thermometer_hygrometer"

level1 <- z
write_csv(level1, "/Users/davidkahler/Documents/R/Lab-Data-Collection/FisherHall.csv", append = TRUE)

# NOTE:
# This code will not produce plots for the website, only the data for upload.

