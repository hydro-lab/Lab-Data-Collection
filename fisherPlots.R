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

