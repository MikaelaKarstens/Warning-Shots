# Dataset Creation

main <- read.csv("Warning-Shots-Data.csv")

library(dplyr)

# Human Rights Protection Scores Fariss ----

fariss <- read.csv("HumanRightsProtectionScores_v4.01.csv")

fariss$unique_id <- fariss$COW + (fariss$YEAR/10000)

fariss$hrp_mean  <- fariss$theta_mean
fariss$hrp_sd <- fariss$theta_sd
fariss$kills_mean <- fariss$killing_estimate_mean
fariss$kills_median <- fariss$killing_estimate_median
fariss$ciri_disap <- fariss$DISAP
fariss$ciri_kill <- fariss$KILL
fariss$ciri_polpris <- fariss$POLPRIS
fariss$ciri_tort <- fariss$TORT
fariss$pts_amnesty <- fariss$Amnesty
fariss$pts_state <- fariss$State
fariss$pts_hrw <- fariss$HRW
fariss$hath_tort <- fariss$hathaway
fariss$ucdp_low <- fariss$killing_low
fariss$ucdp_best <- fariss$killing_best
fariss$ucdp_high <- fariss$killing_high

merg <- select(fariss, unique_id, hrp_mean, hrp_sd, kills_mean, kills_median,
               ciri_disap, ciri_kill, ciri_polpris, ciri_tort, pts_amnesty,
               pts_state, pts_hrw, hath_tort, ucdp_low, ucdp_best, ucdp_high)

data <- left_join(main, merg)

# Polity ----

polity <- read.csv("polity5.csv")

polity$unique_id <- polity$ccode + (polity$year/10000)
polity$polity_democ <- polity$democ
polity$polity_autoc <- polity$autoc

merg <- select(polity, unique_id, polity, polity2, polity_democ, polity_autoc)

main <- left_join(data, merg)


write.csv(main, "Warning-Shots-Data.csv")
