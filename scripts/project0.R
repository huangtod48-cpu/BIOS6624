############################################################
# BIOS 6624 - Project 0
# Author: Tao
# Purpose:
#   Q1: Agreement between booklet and MEMS sampling times
#   Q2: Adherence to 30-min and 10-hour sampling windows
#   Q3: Diurnal patterns of cortisol and DHEA
############################################################


library(tidyverse)
library(lubridate)
library(lme4)
library(broom)
library(broom.mixed)

dat_raw <- read_csv("data/Project0_Clean_v2.csv", show_col_types = FALSE)
glimpse(dat_raw)

dat_time <- dat_raw %>%
  select(
    SubjectID,
    CollectionDate = `Collection Date`,
    Sample = `Collection Sample`,
    DAYNUMB,
    wake_time = `Sleep Diary reported wake time`,
    booklet_time = `Booklet: Clock Time`,
    mems_time = `MEMs: Clock Time`
  )

dat_time <- dat_time %>%
  mutate(
    CollectionDate = mdy(CollectionDate),
    wake_datetime = as.POSIXct(paste(CollectionDate, wake_time)),
    booklet_datetime = as.POSIXct(paste(CollectionDate, booklet_time)),
    mems_datetime = as.POSIXct(paste(CollectionDate, mems_time))
  )

dat_time <- dat_time %>%
  mutate(
    booklet_min_since_wake =
      as.numeric(difftime(booklet_datetime, wake_datetime, units = "mins")),
    mems_min_since_wake =
      as.numeric(difftime(mems_datetime, wake_datetime, units = "mins"))
  )

summary(dat_time$booklet_min_since_wake)
summary(dat_time$mems_min_since_wake)


