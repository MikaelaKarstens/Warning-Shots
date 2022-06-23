# Replication Code for "Warning Shots" by Mikaela Karstens

# Required packages for replication - Please install if needed =================

library(dplyr)
library(survival)
library(stargazer)
library(pscl)
library(broom)
library(ggplot2)
library(survey)
library(plm)
library(matrixStats)

# Settings =====================================================================

options(scipen = 50) # bias against scientific notation for convenience

# Loading data =================================================================

data <- read.csv("WS-Data.csv")
names(data)

# Subset datasets ==============================================================

tc_multi <- data[data$event_num >= 2, ]

tc23 <- filter(data, event_num == 2 | event_num == 3)

tc45 <- filter(data, event_num == 4 | event_num == 5)

tc4_more <- filter(data, event_num >= 4)

tc5_more <- filter(data, event_num >= 5)

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

tc5_more_elapse <- Surv(tc5_more$alt_start, tc5_more$alt_stop,
                        tc5_more$new_tc_dummy)
tc5_more_gap <- Surv(tc5_more$start, tc5_more$stop, tc5_more$new_tc_dummy)

tc6_more_elapse <- Surv(tc6_more$alt_start, tc6_more$alt_stop,
                        tc6_more$new_tc_dummy)
tc6_more_gap <- Surv(tc6_more$start, tc6_more$stop, tc6_more$new_tc_dummy)

# Simple Cox Model =============================================================

basic <- coxph(tc1_gap ~ hrp_mean + v2x_polyarchy + hs_capacity +
                  area_1000_log + lmtnest + elf + acd_inter_ongoing +
                 acd_intra_ongoing + 
                  cluster(ccode), data = tc1, method = "efron")

summary(basic)

stargazer(basic)

basic <- coxph(tc1_gap ~ hrp_mean + v2x_polyarchy + hs_capacity +
                 area_1000_log + lmtnest + elf + acd_inter_ongoing +
                 acd_intra_ongoing + 
                 cluster(ccode), data = tc1, method = "efron")

summary(basic)

stargazer(basic)

tc_pwp <- coxph(all_gap ~ hrp_mean + v2x_polyarchy + hs_capacity +
                  area_1000_log + lmtnest + elf + acd_inter_ongoing +
                  acd_intra_ongoing + strata(event_num) +
                  cluster(ccode), data = data, method = "efron")

summary(tc_pwp)
stargazer(tc_pwp)

tc_pwp <- coxph(all_gap ~ hrp_mean + v2x_polyarchy + hs_capacity +
                  area_1000_log + lmtnest + elf + acd_inter_ongoing +
                  acd_intra_ongoing + strata(event_num) +
                  cluster(ccode), data = data, method = "efron")

tc1_pwp <- coxph(tc1_gap ~ hrp_mean + v2x_polyarchy + hs_capacity +
                   area_1000_log + lmtnest + elf + acd_inter_ongoing +
                   acd_intra_ongoing + strata(event_num) +
                   cluster(ccode), data = tc1, method = "efron")


tc23_pwp <- coxph(tc23_gap ~ hrp_mean + v2x_polyarchy + hs_capacity +
                    area_1000_log + lmtnest + elf + acd_inter_ongoing +
                    acd_intra_ongoing + strata(event_num) +
                    cluster(ccode), data = tc23, method = "efron")




tc4_more_pwp <- coxph(tc4_more_gap ~ hrp_mean + v2x_polyarchy + hs_capacity +
                   area_1000_log + lmtnest + elf + acd_inter_ongoing +
                   acd_intra_ongoing + strata(event_num) +
                   cluster(ccode), data = tc4_more, method = "efron")




stargazer(tc_pwp, tc1_pwp, tc23_pwp, tc4_more_pwp,
          type = "latex",
          title = "PWP Gap Time Model Results",
          model.numbers = F,
          column.labels = c("All TCs",
                            "First TC",
                            " TC 2 or 3",
                            "TC 4+"),
          dep.var.labels = c("(1)", "(2)", "(3)", "(4)"),
          covariate.labels = c("Human Rights Protection",
                               "Democracy (V-Dem)",
                               "State Capacity (HS)",
                               "State Area (logged)",
                               "Mountains",
                               "ELF",
                               "Interstate War",
                               "Intrastate War",
                               "ELF",
                               "TC Tally"),
          keep.stat = c("n"))

# hrp_mean - human rights protections scores 
# polity2 - polity
# v2x_polyarchy - vdem democracy
# v2x_libdem - vdem liberal democracy
# v2x_partipdem - vdem participatory democ
# v2x_egaldem - vdem egalitarian democ
# acd_inter_ongoing - acd ongoing interstate war
# acd_intra_ongoing - acd ongoing intrastate war
# hs_capacity - hansen and sigman capacity
# elf - ethnic linguistic fractionalization
# wbpopest - population

# Interrupted TS ===============================================================


sub_dat <- filter(data,
                  ccode != 90,  # Guatemala
                  ccode != 100, # Colombia
                  ccode != 372, # Georgia
                  ccode != 432, # Mali
                  ccode != 451, # Sierra Leone
                  ccode != 475, # Nigeria
                  ccode != 483, # Chad
                  ccode != 490, # DRC
                  ccode != 500, # Uganda
                  ccode != 501, # Kenya
                  ccode != 520, # Somalia
                  ccode != 530, # Ethiopia
                  ccode != 560, # S. Africa
                  ccode != 565, # Namibia
                  ccode != 645, # Iraq
                  ccode != 660, # Lebanon
                  ccode != 750, # India
                  ccode != 770, # Pakistan
                  ccode != 775, # Burma
                  ccode != 811, # Cambodia
                  ccode != 817, # S. Vietnam
                  ccode != 840) # Philippines

main <- plm(
  hrp_mean ~ num_tc + v2x_polyarchy + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data
)

main1 <- plm(
  hrp_mean ~ num_tc + v2x_polyarchy + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp1,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data
)

main2 <- plm(
  hrp_mean ~ num_tc + v2x_polyarchy + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp2,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data
)

main3 <- plm(
  hrp_mean ~ num_tc + v2x_polyarchy + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp3,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data
)

main4 <- plm(
  hrp_mean ~ num_tc + v2x_polyarchy + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp4,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data
)

main5 <- plm(
  hrp_mean ~ num_tc + v2x_polyarchy + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp5,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data
)

coefs <- as.data.frame(cbind(main1$coefficients, main2$coefficients, main3$coefficients,
               main4$coefficients, main5$coefficients))

coefs$average <- (coefs$V1 + coefs$V2 + coefs$V3 + coefs$V4 + coefs$V5)/5

ses <- as.data.frame(cbind(summary(main1)$coefficients[,2],
                           summary(main2)$coefficients[,2],
                           summary(main3)$coefficients[,2],
                           summary(main4)$coefficients[,2],
                           summary(main5)$coefficients[,2]))

ses$average_sqse <- ((ses$V1)^2 + (ses$V2)^2 + (ses$V3)^2 + (ses$V4)^2 +
                     (ses$V5)^2)/5

ses <- ses %>% mutate(coef_var = rowVars(as.matrix(coefs[,c(1,2,3,4,5)])))

ses$se <- sqrt(ses$average_sqse + 1.2*ses$coef_var)

results_m1 <- as.data.frame(cbind(coefs$average, ses$se))
colnames(results_m1) <- c("Coef", "SE") 
rownames(results_m1) <- c("num_tc", "vdem", "hs_capacity", "area_1000_log",
                          "acd_intra_ongoing", "acd_inter_ongoing", "lag_hrp")
results_m1$Z <- results_m1$Coef/results_m1$SE
results_m1$pvalue <- exp()

P = exp(−0.717×z − 0.416×z2).

main.sub <- plm(
  hrp_mean ~ num_tc + v2x_polyarchy + hs_capacity +
    area_1000_log
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)


summary(main)
summary(main.sub)

stargazer(
  main,
  main.sub,
  title = "Model Results with Two-Way Fixed Effects",
  model.numbers = F,
  column.labels = c("All States", "Subset"),
  dep.var.labels = c("Human Rights Protection"),
  covariate.labels = c(
    "Number of TCs",
    "Democracy (V-Dem)",
    "Capacity",
    "Area (logged)",
    "Civil War",
    "Interstate War",
    "Human Rights Protection (t-1)"
  ),
  keep.stat = c("n")
)


