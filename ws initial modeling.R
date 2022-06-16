# Replication Code for "Warning Shots" by Mikaela Karstens

# Required packages for replication - Please install if needed =================

library(dplyr)
library(survival)
library(stargazer)
library(pscl)
library(broom)
library(ggplot2)
library(survey)

# Settings =====================================================================

options(scipen = 50) # bias against scientific notation for convenience

# Loading data =================================================================

data <- read.csv("Warning-Shots-Data.csv")
names(data)

# Subset datasets ==============================================================

tc_multi <- data[data$event_num >= 2, ]

tc23 <- filter(data, event_num == 2 | event_num == 3)

tc45 <- filter(data, event_num == 4 | event_num == 5)

tc4_more <- filter(data, event_num >= 4)

tc6_more <- filter(data, event_num >= 6)

tc1 <- filter(data, event_num == 1)

tc2 <- filter(data, event_num == 2)

tc3 <- filter(data, event_num == 3)

tc4 <- filter(data, event_num == 4)

tc5 <- filter(data, event_num == 5)

tc6 <- filter(data, event_num == 6)

# Survival objects =============================================================

all_elapse <- Surv(data$alt_start, data$alt_stop, data$new_tc_dummy)
all_gap <- Surv(data$start, data$stop, data$new_tc_dummy)

tc_multi_elapse <- Surv(tc_multi$alt_start, tc_multi$alt_stop,
                        tc_multi$new_tc_dummy)
tc_multi_gap <- Surv(tc_multi$start, tc_multi$stop, tc_multi$new_tc_dummy)

tc1_elapse <- Surv(tc1$alt_start, tc1$alt_stop, tc1$new_tc_dummy)
tc1_gap <- Surv(tc1$start, tc1$stop, tc1$new_tc_dummy)

tc2_elapse <- Surv(tc2$alt_start, tc2$alt_stop, tc2$new_tc_dummy)
tc2_gap <- Surv(tc2$start, tc2$stop, tc2$new_tc_dummy)

tc23_elapse <- Surv(tc23$alt_start, tc23$alt_stop, tc23$new_tc_dummy)
tc23_gap <- Surv(tc23$start, tc23$stop, tc23$new_tc_dummy)

tc3_elapse <- Surv(tc3$alt_start, tc3$alt_stop, tc3$new_tc_dummy)
tc3_gap <- Surv(tc3$start, tc3$stop, tc3$new_tc_dummy)

tc4_elapse <- Surv(tc4$alt_start, tc4$alt_stop, tc4$new_tc_dummy)
tc4_gap <- Surv(tc4$start, tc4$stop, tc4$new_tc_dummy)

tc45_elapse <- Surv(tc45$alt_start, tc45$alt_stop, tc45$new_tc_dummy)
tc45_gap <- Surv(tc45$start, tc45$stop, tc45$new_tc_dummy)

tc5_elapse <- Surv(tc5$alt_start, tc5$alt_stop, tc5$new_tc_dummy)
tc5_gap <- Surv(tc5$start, tc5$stop, tc5$new_tc_dummy)

tc6_elapse <- Surv(tc6$alt_start, tc6$alt_stop, tc6$new_tc_dummy)
tc6_gap <- Surv(tc6$start, tc6$stop, tc6$new_tc_dummy)

tc4_more_elapse <- Surv(tc4_more$alt_start, tc4_more$alt_stop,
                        tc4_more$new_tc_dummy)
tc4_more_gap <- Surv(tc4_more$start, tc4_more$stop, tc4_more$new_tc_dummy)

tc6_more_elapse <- Surv(tc6_more$alt_start, tc6_more$alt_stop,
                        tc6_more$new_tc_dummy)
tc6_more_gap <- Surv(tc6_more$start, tc6_more$stop, tc6_more$new_tc_dummy)

# Simple Cox Model

basic <- coxph(tc1_gap ~ hrp_mean + polity2 + hs_capacity +
                  area_1000_log + lmtnest + elf +
                  cluster(ccode), data = tc1, method = "efron")

summary(basic)
