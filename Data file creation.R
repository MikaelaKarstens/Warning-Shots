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

#Vdem ----

vdem <- read.csv("V-Dem-CY-Core-v10.csv")
names(vdem)
vdem$unique_id <- vdem$COWcode + (vdem$year/10000)

vdem <- select(vdem, unique_id, v2x_polyarchy,
               v2x_libdem, v2x_partipdem, v2x_egaldem)

data <- left_join(main, vdem)

#War ----
library(peacesciencer)
war <- create_stateyears()
war$unique_id <- war$ccode + (war$year/10000)
war <- add_gwcode_to_cow(war)

war <- add_ucdp_acd(war, type = "interstate", only_wars = T)
war$acd_inter_ongoing <- war$ucdpongoing
war$acd_inter_onset <- war$ucdponset
war$ucdpongoing <- NULL
war$ucdponset <- NULL
war$maxintensity <- NULL
war$conflict_ids <- NULL

war <- add_ucdp_acd(war, type = "intrastate", only_wars = T)
war$acd_intra_ongoing <- war$ucdpongoing
war$acd_intra_onset <- war$ucdponset
war$ucdpongoing <- NULL
war$ucdponset <- NULL
war$maxintensity <- NULL
war$conflict_ids <- NULL

war <- add_ucdp_acd(war, type = c("intrastate", "interstate"),
                    only_wars = T)
war$acd_any_ongoing <- war$ucdpongoing
war$acd_any_onset <- war$ucdponset
war$ucdpongoing <- NULL
war$ucdponset <- NULL
war$maxintensity <- NULL
war$conflict_ids <- NULL
war$gwcode <- NULL
war$statenme <- NULL
war$ccode <- NULL
war$year <- NULL

names(war)

main <- left_join(data, war)

# Pop estimates and gdp from Anders, Fariss, and Markowitz 2020----

dem <- create_stateyears()
dem$unique_id <- dem$ccode + (dem$year/10000)
dem <- add_gwcode_to_cow(dem)
dem <- add_sdp_gdp(dem)
names(dem)
merg <- select(dem, unique_id, wbgdp2011est, wbgdppc2011est,
               wbpopest, sdpest)
main <- left_join(data, merg)

data <- read.csv("Warning-Shots-Data.csv")

# Write Data ----
write.csv(main, "Warning-Shots-Data.csv")
