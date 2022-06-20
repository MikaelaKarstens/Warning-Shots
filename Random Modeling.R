# Tests for Warning Shots

data <- read.csv("WS-Data.csv")
names(data)

cor(data$v2x_polyarchy, data$polity2, use = "complete.obs")
# Correlate at 0.827

# CIRI Tests ----

ciri <- filter(data, !is.na(ciri_kill))
table(ciri$year)


# MI for accounting for uncertainty in hrp scores ----

# made in Excel. Random draw from normal distribution with mean of hrp_mean
# and sd of hrp_sd. Saved and then lagged below. 

new_dat <- data %>%
  group_by(ccode) %>%
  dplyr::mutate(lag_hrp1 = lag(hrp1, n = 1),
                lag_hrp2 = lag(hrp2, n = 1),
                lag_hrp3 = lag(hrp3, n = 1),
                lag_hrp4 = lag(hrp4, n = 1),
                lag_hrp5 = lag(hrp5, n = 1),
                )

write.csv(new_dat, "WS-Data.csv")
