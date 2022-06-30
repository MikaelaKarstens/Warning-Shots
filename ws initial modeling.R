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
library(Amelia)

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

# Modeling First Territorial Contender =========================================

first <- coxph(tc1_gap ~ hrp_mean + polity2 + hs_capacity +
                 area_1000_log + lmtnest + elf + acd_inter_ongoing +
                 acd_intra_ongoing + 
                 cluster(ccode), data = tc1, method = "efron")

summary(first)

stargazer(first,
          type = "latex",
          title = "Cox Model of First TC Emergence",
          model.numbers = F,
          dep.var.labels = "First TC",
          covariate.labels = c("Human Rights Protection",
                               "Polity2",
                               "State Capacity",
                               "Area (logged)",
                               "Mountainous",
                               "ELF",
                               "Ongoing Interstate War",
                               "Ongoing Intrastate War"),
          keep.stat = c("n"))

Haz <- exp(first$coefficients)

# Figure 1 - Coef Plot for First TC ============================================

coefs <- tidy(first, conf.int = TRUE, exponentiate = F)
coefs$hazard_ratio <- exp(coefs$estimate)
coefs$variable <- c("Human Rights Protection",
                    "Polity2",
                    "State Capacity",
                    "Area (logged)",
                    "Mountainous",
                    "ELF",
                    "Ongoing Interstate War",
                    "Ongoing Intrastate War")


tc1_pwp <- data.frame(variable = coefs$variable,
                       hazard_ratio = coefs$hazard_ratio,
                       beta = coefs$estimate,
                       se = summary(first)$coef[, 4],
                       tc = "First TC")
tc1_pwp$variable <- factor(tc1_pwp$variable,
                            levels = unique(as.character(tc1_pwp$variable)))


interval1 <- -qnorm((1 - 0.90) / 2)  # 90 % multiplier
interval2 <- -qnorm((1 - 0.95) / 2)  # 95 % multiplier


first_fig <- ggplot(tc1_pwp)
first_fig <- first_fig + geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2)
first_fig <- first_fig + geom_linerange(aes(x = variable,
                                        ymin = beta - se * interval1,
                                        ymax = beta + se * interval1),
                                    lwd = 1.5,
                                    position = position_dodge(width = 1 / 2))
first_fig <- first_fig + geom_pointrange(aes(x = variable, y = beta,
                                         ymin = beta - se * interval2,
                                         ymax = beta + se * interval2,
                                         size = 2),
                                     lwd = 1, shape = 18,
                                     position = position_dodge(width = 1 / 2))

first_fig <- first_fig + xlim(rev(levels(tc1_pwp$variable))) +
  coord_flip() + theme_minimal()
first_fig <- first_fig + labs(y = "Coefficient Estimate")
first_fig <- first_fig + theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(family = "serif", size = 14),
  axis.text.y = element_text(family = "serif", size = 14),
  legend.position = c(.9, .3),
  legend.title = element_text(family = "serif", size = 16),
  legend.text = element_text(family = "serif", size = 14)
)

print(first_fig)

# Modeling all TCs =============================================================


tc_pwp <- coxph(all_gap ~ hrp_mean + polity2 + hs_capacity +
                  area_1000_log + lmtnest + elf + acd_inter_ongoing +
                  acd_intra_ongoing + strata(event_num) +
                  cluster(ccode), data = data, method = "efron")

tc1_pwp <- coxph(tc1_gap ~ hrp_mean + polity2 + hs_capacity +
                   area_1000_log + lmtnest + elf + acd_inter_ongoing +
                   acd_intra_ongoing + strata(event_num) +
                   cluster(ccode), data = tc1, method = "efron")


tc23_pwp <- coxph(tc23_gap ~ hrp_mean + polity2 + hs_capacity +
                    area_1000_log + lmtnest + elf + acd_inter_ongoing +
                    acd_intra_ongoing + strata(event_num) +
                    cluster(ccode), data = tc23, method = "efron")


tc4_more_pwp <- coxph(tc4_more_gap ~ hrp_mean + polity2 + hs_capacity +
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
                               "Polity2",
                               "State Capacity",
                               "Area (logged)",
                               "Mountainous",
                               "ELF",
                               "Ongoing Interstate War",
                               "Ongoing Intrastate War"),
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

Hazall <- exp(tc_pwp$coefficients)
Haz1 <- exp(tc1_pwp$coefficients)
Haz23 <- exp(tc23_pwp$coefficients)
Haz4plus <- exp(tc4_more_pwp$coefficients)

# Figure 2 - Coef Plot of All TCs ==============================================


coefs_all <- tidy(tc_pwp, conf.int = TRUE, exponentiate = F)
coefs_all$hazard_ratio <- exp(coefs_all$estimate)
coefs_all$variable <- c("Human Rights Protection",
                        "Polity2",
                        "State Capacity",
                        "Area (logged)",
                        "Mountainous",
                        "ELF",
                        "Ongoing Interstate War",
                        "Ongoing Intrastate War")

coefs1 <- tidy(tc1_pwp, conf.int = TRUE, exponentiate = F)
coefs1$hazard_ratio <- exp(coefs1$estimate)
coefs1$variable <- c("Human Rights Protection",
                     "Polity2",
                     "State Capacity",
                     "Area (logged)",
                     "Mountainous",
                     "ELF",
                     "Ongoing Interstate War",
                     "Ongoing Intrastate War")

coefs23 <- tidy(tc23_pwp, conf.int = TRUE, exponentiate = F)
coefs23$hazard_ratio <- exp(coefs23$estimate)
coefs23$variable <- c("Human Rights Protection",
                      "Polity2",
                      "State Capacity",
                      "Area (logged)",
                      "Mountainous",
                      "ELF",
                      "Ongoing Interstate War",
                      "Ongoing Intrastate War")

coefs4more <- tidy(tc4_more_pwp, conf.int = TRUE, exponentiate = F)
coefs4more$hazard_ratio <- exp(coefs4more$estimate)
coefs4more$variable <- c("Human Rights Protection",
                      "Polity2",
                      "State Capacity",
                      "Area (logged)",
                      "Mountainous",
                      "ELF",
                      "Ongoing Interstate War",
                      "Ongoing Intrastate War")


combined <- data.frame(variable = coefs_all$variable,
                       hazard_ratio = coefs_all$hazard_ratio,
                       beta = coefs_all$estimate,
                       se = summary(tc_pwp)$coef[, 4],
                       tc = "All TCs")
combined$variable <- factor(combined$variable,
                            levels = unique(as.character(combined$variable)))

first <- data.frame(variable = coefs1$variable,
                    hazard_ratio = coefs1$hazard_ratio,
                    beta = coefs1$estimate,
                    se = summary(tc1_pwp)$coef[, 4],
                    tc = "First TC")

mod23 <- data.frame(variable = coefs23$variable,
                    hazard_ratio = coefs23$hazard_ratio,
                    beta = coefs23$estimate,
                    se = summary(tc23_pwp)$coef[, 4],
                    tc = "2-3")

mod4plus <- data.frame(variable = coefs4more$variable,
                    hazard_ratio = coefs4more$hazard_ratio,
                    beta = coefs4more$estimate,
                    se = summary(tc4_more_pwp)$coef[, 4],
                    tc = "4+")



allmod <- data.frame(rbind(combined, first, mod23, mod4plus))

allmod$variable <- factor(allmod$variable,
                          levels = unique(as.character(allmod$variable)))


interval1 <- -qnorm((1 - 0.90) / 2)  # 90 % multiplier
interval2 <- -qnorm((1 - 0.95) / 2)  # 95 % multiplier

allmod$tc <- factor(allmod$tc,
                    levels = c("4+", "2-3", "First TC", "All TCs"))

pwp_fig <- ggplot(allmod, aes(colour = tc))
pwp_fig <- pwp_fig + scale_color_viridis_d(option = "D",
                                           breaks = c("All TCs", "First TC",
                                                      "2-3", "4+"),
                                           name = "TC Number")
pwp_fig <- pwp_fig + geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2)
pwp_fig <- pwp_fig + geom_linerange(aes(x = variable,
                                        ymin = beta - se * interval1,
                                        ymax = beta + se * interval1),
                                    lwd = 1.5,
                                    position = position_dodge(width = 1 / 2))
pwp_fig <- pwp_fig + geom_pointrange(aes(x = variable, y = beta,
                                         ymin = beta - se * interval2,
                                         ymax = beta + se * interval2,
                                         size = 2),
                                     lwd = 1, shape = 18,
                                     position = position_dodge(width = 1 / 2))

pwp_fig <- pwp_fig + xlim(rev(levels(allmod$variable))) +
  coord_flip() + theme_minimal()
pwp_fig <- pwp_fig + labs(y = "Coefficient Estimate")
pwp_fig <- pwp_fig + theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(family = "serif", size = 14),
  axis.text.y = element_text(family = "serif", size = 14),
  legend.position = c(.8, .5),
  legend.title = element_text(family = "serif", size = 16),
  legend.text = element_text(family = "serif", size = 14)
)

print(pwp_fig)

# TC Survival ==================================================================

tcdat <- read.csv("TC_Years_warning_shots.csv")
merg <- select(data, unique_id, hrp_mean, lag_hrp, lag_hrp1, lag_hrp2, lag_hrp3,
               lag_hrp4, lag_hrp5, state_name, lmtnest, elf, acd_inter_ongoing,
               acd_intra_ongoing, area_1000_log, hs_capacity, Devel, polity2,
               tc_tally)


tcdat <- left_join(tcdat, merg)

tc_surv <- Surv(tcdat$start, tcdat$stop, tcdat$Death)

tc_survive <- coxph(tc_surv ~ hrp_mean + polity2 + hs_capacity +
                 area_1000_log + lmtnest + acd_inter_ongoing +
                 acd_intra_ongoing + tc_tally , data = tcdat, method = "efron")

summary(tc_survive)

stargazer(tc_survive,
          type = "latex",
          title = "Cox Model of TC Survival",
          model.numbers = F,
          dep.var.labels = "TC Survival",
          covariate.labels = c("Human Rights Protection",
                               "Polity2",
                               "State Capacity",
                               "Area (logged)",
                               "Mountainous",
                               "Ongoing Interstate War",
                               "Ongoing Intrastate War",
                               "TC Tally"),
          keep.stat = c("n"))

Haz <- exp(tc_survive$coefficients)

sd(data$hrp_mean, na.rm = T)

# Coef plot of TC Survival =====================================================

coefs <- tidy(tc_survive, conf.int = TRUE, exponentiate = F)
coefs$hazard_ratio <- exp(coefs$estimate)
coefs$variable <- c("Human Rights Protection",
                    "Polity2",
                    "State Capacity",
                    "Area (logged)",
                    "Mountainous",
                    "Ongoing Interstate War",
                    "Ongoing Intrastate War",
                    "TC Tally")


tcsurv_cox <- data.frame(variable = coefs$variable,
                      hazard_ratio = coefs$hazard_ratio,
                      beta = coefs$estimate,
                      se = summary(tc_survive)$coef[, 4])

tcsurv_cox$variable <- factor(tcsurv_cox$variable,
                           levels = unique(as.character(tcsurv_cox$variable)))


interval1 <- -qnorm((1 - 0.90) / 2)  # 90 % multiplier
interval2 <- -qnorm((1 - 0.95) / 2)  # 95 % multiplier


tc_surv_fig <- ggplot(tcsurv_cox)
tc_surv_fig <- tc_surv_fig + geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2)
tc_surv_fig <- tc_surv_fig + geom_linerange(aes(x = variable,
                                            ymin = beta - se * interval1,
                                            ymax = beta + se * interval1),
                                        lwd = 1.5,
                                        position = position_dodge(width = 1 / 2))
tc_surv_fig <- tc_surv_fig + geom_pointrange(aes(x = variable, y = beta,
                                             ymin = beta - se * interval2,
                                             ymax = beta + se * interval2,
                                             size = 2),
                                         lwd = 1, shape = 18,
                                         position = position_dodge(width = 1 / 2))

tc_surv_fig <- tc_surv_fig + xlim(rev(levels(tcsurv_cox$variable))) +
  coord_flip() + theme_minimal()
tc_surv_fig <- tc_surv_fig + labs(y = "Coefficient Estimate")
tc_surv_fig <- tc_surv_fig + theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(family = "serif", size = 14),
  axis.text.y = element_text(family = "serif", size = 14),
  legend.position = c(.9, .3),
  legend.title = element_text(family = "serif", size = 16),
  legend.text = element_text(family = "serif", size = 14)
)

print(tc_surv_fig)

# Interrupted TS ===============================================================

data2 <- select(data, num_tc, polity2, hs_capacity, area_1000_log, 
               acd_intra_ongoing, acd_inter_ongoing, lag_hrp, state_name, year, 
               hrp_mean, lag_hrp1, lag_hrp2, lag_hrp3, lag_hrp4, lag_hrp5, ccode)

data22 <- data2 %>% na.omit
data2 <- data22

sub_dat <- filter(data2,
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
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data2
)

main1 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp1,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data2
)

main2 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp2,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data2
)

main3 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp3,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data2
)

main4 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp4,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data2
)

main5 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log 
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp5,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data2
)

coefs <- as.data.frame(cbind(main1$coefficients, main2$coefficients, main3$coefficients,
               main4$coefficients, main5$coefficients))



ses <- as.data.frame(cbind(summary(main1)$coefficients[,2],
                           summary(main2)$coefficients[,2],
                           summary(main3)$coefficients[,2],
                           summary(main4)$coefficients[,2],
                           summary(main5)$coefficients[,2]))

melded <- mi.meld(coefs, ses, byrow = FALSE)

results_m1 <- as.data.frame(t(do.call(rbind.data.frame, melded)))


colnames(results_m1) <- c("Coef", "SE") 
results_m1$Z <- results_m1$Coef/results_m1$SE
results_m1$p <- 2*pnorm(-abs(results_m1$Z))
  
mfx <- marginaleffects(main1)

mfx <- Effect("num_tc", main1)


main.sub <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)

main.sub1 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp1,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)

main.sub2 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp2,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)

main.sub3 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp3,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)

main.sub4 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp4,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)

main.sub5 <- plm(
  hrp_mean ~ num_tc + polity2 + hs_capacity +
    area_1000_log
  + acd_intra_ongoing + acd_inter_ongoing + lag_hrp5,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)


coefs <- as.data.frame(cbind(main.sub1$coefficients, main.sub2$coefficients, main.sub3$coefficients,
                             main.sub4$coefficients, main.sub5$coefficients))




ses <- as.data.frame(cbind(summary(main.sub1)$coefficients[,2],
                           summary(main.sub2)$coefficients[,2],
                           summary(main.sub3)$coefficients[,2],
                           summary(main.sub4)$coefficients[,2],
                           summary(main.sub5)$coefficients[,2]))

melded <- mi.meld(coefs, ses, byrow = FALSE)

results_m1sub <- as.data.frame(t(do.call(rbind.data.frame, melded)))


colnames(results_m1sub) <- c("Coef", "SE") 
results_m1sub$Z <- results_m1sub$Coef/results_m1sub$SE
results_m1sub$p <- 2*pnorm(-abs(results_m1sub$Z))

# Coef plot of ITS =============================================================

results_m1

results_m1sub

results_m1$variable <- c("Number of TCs",
                         "Polity2",
                         "Capacity",
                         "Area",
                         "Civil War",
                         "Interstate War",
                         "Human Rights Protection (t-1)")

results_m1sub$variable <- c("Number of TCs",
                       "Polity2",
                       "Capacity",
                       "Area",
                       "Civil War",
                       "Interstate War",
                       "Human Rights Protection (t-1)")


all_plot <- data.frame(variable = results_m1$variable,
                       beta = results_m1$Coef,
                       se = results_m1$SE,
                       tc = "All Cases")

all_plot$variable <- factor(all_plot$variable,
                            levels = unique(as.character(all_plot$variable)))

sub_plot <- data.frame(variable = results_m1sub$variable,
                       beta = results_m1sub$Coef,
                       se = results_m1sub$SE,
                       tc = "Subset")


allmod <- data.frame(rbind(all_plot, sub_plot))

allmod$variable <- factor(allmod$variable,
                          levels = unique(as.character(allmod$variable)))

submod <- filter(allmod, variable != "Human Rights Protection (t-1)")

submod$variable <- factor(submod$variable,
                          levels = unique(as.character(submod$variable)))

interval1 <- -qnorm((1 - 0.90) / 2)  # 90 % multiplier
interval2 <- -qnorm((1 - 0.95) / 2)  # 95 % multiplier


allmod$tc <- factor(allmod$tc,
                    levels = c("All Cases", "Subset"))

main_coef <- ggplot(allmod, aes(colour = tc))
main_coef <- main_coef + scale_color_viridis_d(option = "D", begin = 0.2, end = .8,
                                               breaks = c("All Cases", "Subset"),
                                               name = "")
main_coef <- main_coef + geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2)
main_coef <- main_coef + geom_linerange(aes(x = variable,
                                            ymin = beta - se * interval1,
                                            ymax = beta + se * interval1),
                                        lwd = 1.5,
                                        position = position_dodge(width = 1 / 2))
main_coef <- main_coef + geom_pointrange(aes(x = variable, y = beta,
                                             ymin = beta - se * interval2,
                                             ymax = beta + se * interval2,
                                             size = 2),
                                         lwd = 1, shape = 18,
                                         position = position_dodge(width = 1 / 2))

main_coef <- main_coef + xlim(rev(levels(allmod$variable))) +
  coord_flip() + theme_minimal()
main_coef <- main_coef + labs(y = "Coefficient Estimate")
main_coef <- main_coef + theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(family = "serif", size = 14),
  axis.text.y = element_text(family = "serif", size = 14),
  legend.position = c(.8, .4),
  legend.title = element_text(family = "serif", size = 16),
  legend.text = element_text(family = "serif", size = 14)
)

print(main_coef)



submod$tc <- factor(submod$tc,
                    levels = c("All Cases", "Subset"))

sub_coef <- ggplot(submod, aes(colour = tc))
sub_coef <- sub_coef + scale_color_viridis_d(option = "D", begin = 0.2, end = .8,
                                             breaks = c("All Cases", "Subset"),
                                             name = "")
sub_coef <- sub_coef + geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2)
sub_coef <- sub_coef + geom_linerange(aes(x = variable,
                                          ymin = beta - se * interval1,
                                          ymax = beta + se * interval1),
                                      lwd = 1.5,
                                      position = position_dodge(width = 1 / 2))
sub_coef <- sub_coef + geom_pointrange(aes(x = variable, y = beta,
                                           ymin = beta - se * interval2,
                                           ymax = beta + se * interval2,
                                           size = 2),
                                       lwd = 1, shape = 18,
                                       position = position_dodge(width = 1 / 2))

sub_coef <- sub_coef + xlim(rev(levels(submod$variable))) +
  coord_flip() + theme_minimal()
sub_coef <- sub_coef + labs(y = "Coefficient Estimate")
sub_coef <- sub_coef + theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(family = "serif", size = 14),
  axis.text.y = element_text(family = "serif", size = 14),
  legend.position = c(.8, .3),
  legend.title = element_text(family = "serif", size = 16),
  legend.text = element_text(family = "serif", size = 14)
)

print(sub_coef)

