library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(latex2exp)

# READ IN AND SORT

x <- read_csv("/Volumes/D/Fisher711_Table1.dat", skip = 4, col_names = FALSE)

y <- x %>%
     rename(TIMESTAMP = X1, 
            RECORD = X2, 
            PTemp_C_Avg = X3, 
            Rain_mm_Tot = X4, 
            AirTC_Avg = X5, 
            AirTC_Std = X6, 
            RH = X7, 
            SlrW_Avg = X8, 
            SlrW_Std = X9, 
            CS320_Temp_Avg = X10, 
            CS320_Angle_Avg = X11) %>%
     mutate(time_utc = ymd_hms(TIMESTAMP), 
            unix_utc = as.numeric(time_utc), 
            time_et = with_tz(time_utc, tz = "US/Eastern"), 
            utc_offset = (as.numeric(force_tz(time_et, tz = "UTC")) - unix_utc)/3600 ) %>%
     select(-TIMESTAMP, -RECORD, -SlrW_Std, -CS320_Temp_Avg, -CS320_Angle_Avg, -PTemp_C_Avg)

# PIVOT TO LONGER FORMAT, ADD SITE, SOURCE, and QC flag
# LEVEL 0 is raw data

z <- pivot_longer(y, cols = c(AirTC_Avg,AirTC_Std,RH,SlrW_Avg,Rain_mm_Tot),
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
     else if (z$Variable[i]=="SlrW_Avg") {z$Method[i] = "ThermopilePyranometer"}
     else if (z$Variable[i]=="SlrW_Std") {z$Method[i] = "ThermopilePyranometer"}
     else if (z$Variable[i]=="CS320_Temp_Avg") {z$Method[i] = "ThermopilePyranometer"}
     else if (z$Variable[i]=="CS320_Angle_Avg") {z$Method[i] = "ThermopilePyranometer"}
     else if (z$Variable[i]=="Rain_mm_Tot") {z$Method[i] = "HeatedTippingBucket"}
}

level0 <- z
write_csv(level0, "/Users/davidkahler/Documents/R/Lab-Data-Collection/FisherHall.csv", append = TRUE)

# PERFORM QC ON SENSORS: LIMITS, SPIKES, 
# LEVEL 1 is minimum QC

n <- nrow(y)
for (i in 1:n) {
     if ( (is.na(y$AirTC_Avg[i])) | (is.na(y$AirTC_Std[i])) ) {
          y$AirTC_Avg[i] <- -9999
          y$AirTC_Std[i] <- -9999
     } else if (y$AirTC_Avg[i] < -40) {
          y$AirTC_Avg[i] <- -9999
          print(paste0("temperature below valid range ", y$time_utc[i]))
     } else if (y$AirTC_Avg[i] > 70) {
          y$AirTC_Avg[i] <- -9999
          print(paste0("temperature above valid range ", y$time_utc[i]))
     } else if ((i > 4) & (i < (n-4))) {
          c <- 0 # count
          t <- 0 # temperature
          s <- 0 # standard deviation
          for (j in (j-4):(j+4)) {
               if 
          }
          s <- max(c(max(y$AirTC_Std[(i-4):(i-1)], na.rm = TRUE), max(y$AirTC_Std[(i+1):(i+4)], na.rm = TRUE)))
          t <- mean(c(mean(y$AirTC_Avg[(i-4):(i-1)], na.rm = TRUE), mean(y$AirTC_Avg[(i+1):(i+4)], na.rm = TRUE)))
          if ( ( y$AirTC_Avg[i] > t+(8*s) ) | ( y$AirTC_Avg[i] < t-(8*s) ) ) {
               print(paste0("spike detected, manual intervention needed ", z$time_utc[i]))
          }
     } 
     if (is.na(x$RHpct_Min[i])) { # NOTE: should also fix to include screening for -9999 or other common error flags
     x$RHpct_Min[i] <- -9999
     x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "m,") #
} else {
     if (x$RHpct_Min[i] < 0) {
          if (x$RHpct_Min[i] < -1) {x$RHpct_Min[i] <- -9999}
          x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "l,")
     }
     if (x$RHpct_Min[i] > 100) {
          if (x$RHpct_Min[i] > 101) {x$RHpct_Min[i] <- -9999}
          x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "h,")
     }
}
     
}

if (is.na(x$RHpct_Min[i])) { # NOTE: should also fix to include screening for -9999 or other common error flags
     x$RHpct_Min[i] <- -9999
     x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "m,") #
} else {
     if (x$RHpct_Min[i] < 0) {
          if (x$RHpct_Min[i] < -1) {x$RHpct_Min[i] <- -9999}
          x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "l,")
     }
     if (x$RHpct_Min[i] > 100) {
          if (x$RHpct_Min[i] > 101) {x$RHpct_Min[i] <- -9999}
          x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "h,")
     }
}
if (is.na(x$RHpct_Max[i])) {
     x$RHpct_Max[i] <- -9999
     x$RHpct_Max_qc[i] <- paste0(x$RHpct_Max_qc[i], "m,") #
} else {
     if (x$RHpct_Max[i] < 0) {
          if (x$RHpct_Max[i] < -1) {x$RHpct_Max[i] <- -9999}
          x$RHpct_Max_qc[i] <- paste0(x$RHpct_Max_qc[i], "l,")
     }
     if (x$RHpct_Max[i] > 100) {
          if (x$RHpct_Max[i] > 101) {x$RHpct_Max[i] <- -9999}
          x$RHpct_Max_qc[i] <- paste0(x$RHpct_Max_qc[i], "h,")
     }
}





z <- pivot_longer(y, cols = c(AirTC_Avg,AirTC_Std,RH,SlrW_Avg,Rain_mm_Tot),
                  names_to = "Variable",
                  values_to = "Value")
z$Site <- "Duquesne_FisherHall"
z$Source <- "DuqSciEng"
z$QC <- "Level1"

z$Method <- NA
n <- nrow(z)
for (i in 1:n) {
     if (z$Variable[i]=="AirTC_Avg") {z$Method[i] = "Thermometer_hygrometer"}
     else if (z$Variable[i]=="AirTC_Std") {z$Method[i] = "Thermometer_hygrometer"}
     else if (z$Variable[i]=="RH") {z$Method[i] = "Thermometer_hygrometer"}
     else if (z$Variable[i]=="SlrW_Avg") {z$Method[i] = "ThermopilePyranometer"}
     else if (z$Variable[i]=="SlrW_Std") {z$Method[i] = "ThermopilePyranometer"}
     else if (z$Variable[i]=="CS320_Temp_Avg") {z$Method[i] = "ThermopilePyranometer"}
     else if (z$Variable[i]=="CS320_Angle_Avg") {z$Method[i] = "ThermopilePyranometer"}
     else if (z$Variable[i]=="Rain_mm_Tot") {z$Method[i] = "HeatedTippingBucket"}
}

level1 <- z
write_csv(level1, "/Users/davidkahler/Documents/R/Lab-Data-Collection/FisherHall.csv", append = TRUE)




