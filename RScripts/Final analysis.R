
  library(tidyverse)
  library(lme4)
  library(binom)

# read in animal data
  dat <- read.csv("data\\Animal temperature data.csv")
  glimpse(dat)
  
# julian dates and times have origin = 2012-01-01
  
  dat <- dat %>%
         mutate(hour = floor((jul_time - jul_day) * 24),
                min = floor(((jul_time - jul_day) * 24 - hour) * 60),
                whole_temp = floor(itemp),
                temp_dif = animal_temp - itemp,
                aj = factor(paste(animal_id, jul_day)),
                lj = factor(paste(location, jul_day)),
                alj = factor(paste(animal_id, location, jul_day)))
                
################################################################################
# summary of data

# number of animals
  length(table(dat$animal_id))
# number of days
  length(table(dat$jul_day))
# number of animals by days
  length(table(dat$aj))
# total number of observations
  dim(dat)
# number of observations in daylight hours
  dim(filter(dat, daylight == 1))

################################################################################
################################################################################
################################################################################
# read in and plot habitat temperatures

  all.temp <- read.csv("data\\Habitat temperature data.csv")
  
# summary of data
# number of measurements
  a <- c(all.temp$temp_sun, all.temp$temp_shade, all.temp$temp_bur)
  length(a[!is.na(a)])
  
# number of days
  length(table(all.temp$jul_day))


# plot shaded ground, burrow and sun temps
  a <- 0.3
  s <- 1

  y.title <- expression(paste("Burrow (", italic(T[burrow]), ") and open ground (", italic(T[sun]), ") temperature (˚C)"), sep = "")
  x.title <- expression(paste("Shaded ground (", italic(T[shade]), ") temperature (˚C)"), sep = "")

  ggplot(all.temp, aes(y = temp_bur, x = temp_shade)) +
    geom_jitter(colour = "blue", alpha = a, size = 1) +
    geom_jitter(aes(y = temp_sun), colour = "red", alpha = a, size = 1) +
    geom_abline(aes(intercept = 0, slope = 1), lwd = 1.1) +
    labs(y = y.title, x = x.title) +
    scale_x_continuous(limits=c(-1,60), breaks=seq(0,60,10))+
    scale_y_continuous(limits=c(-1,70), breaks=seq(0,70,10))+
    theme_classic(14) +
    theme(axis.line.x = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
          axis.text=element_text(size=14),
          axis.title=element_text(size=18))
  
  
################################################################################
# plot animal temp versus shaded ground

# calculate mean animal temp for each degree of shaded ground temp
  mr <- group_by(dat, whole_temp) %>%
        summarise(animal_temp = mean(animal_temp),
                  n = n())

# plot the animal data
  y.title <- expression(paste("Transmitter (", italic(T[trans]), ") temperature (˚C)"), sep = "")
  x.title <- expression(paste("Shaded ground (", italic(T[shade]), ") temperature (˚C)"), sep = "")

  ggplot(dat, aes(y = animal_temp, x = itemp)) +
    geom_point(size = 1, alpha = 0.2, colour = "grey") +
    geom_point(data = subset(mr, n > 5), aes(y = animal_temp, x = whole_temp), size = 2.5) +
    geom_abline(aes(intercept = 0, slope = 1), lwd = 1.1) +
    geom_hline(yintercept = c(12.1, 38.9)) +
    annotate("text", x = 57, y = 40, parse = T, label = "CT[max]", parse = T, size = 6) +
    annotate("text", x = 57, y = 13.5, parse = T, label = "CT[min]", parse = T, size = 6) +
    labs(y = y.title, x = x.title) +
    scale_x_continuous(limits=c(-1,60), breaks=seq(0,60,10))+
    scale_y_continuous(limits=c(-1,70), breaks=seq(0,70,10))+
    theme_classic(14) +
    theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.text=element_text(size=14),
          axis.title=element_text(size=18))
  

###############################################################################
# Density plots of animal and habitat temperatures in 5 degree groupings

  dat <- mutate(dat, five_temp = floor(whole_temp / 5))
  all.temp <- mutate(all.temp, five_temp = floor(whole_temp / 5))

# limit to between 5 and 50 degrees
  sub.dat <- filter(dat, five_temp >=1 & five_temp <= 9)
  sub.all <- filter(all.temp, five_temp >=1 & five_temp <= 9)
  
  sub.all <- mutate(sub.all, five_temp_label = paste((five_temp * 5), "-", (five_temp * 5 + 4), sep = ""),
                    mean_ground_shade = ((five_temp * 5) + (five_temp * 5 + 4)) / 2)
  
# choose nice colours
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  n = 3
  cols = gg_color_hue(n)

  ggplot(sub.all, aes(temp_bur)) +
    geom_density(colour = "blue", bw = 3, lwd = 1.05) +
    geom_density(aes(temp_sun), colour = "red", bw = 3, lwd = 1.05) +
    geom_density(data = sub.dat, aes(animal_temp), fill = "grey", bw = 3, alpha = 0.4, lwd = 1.05) +
    geom_vline(aes(xintercept = mean_ground_shade), linetype="solid", size=1.2) +
    geom_text(aes(x = 60, y = 0.09, label = five_temp_label), size=5) +
    facet_wrap(~ five_temp, scale = "free_y", ncol = 3) +
    scale_y_continuous(breaks = NULL) +
    ylab("Relative frequency\n") +
    xlab("\nTemperature (\u00B0C)") +
    xlim(-2, 72) +
    theme_classic(12) +
    theme(strip.background = element_blank(),
       strip.text.x = element_blank(),
      axis.text.x=element_text(size=13.5),
      axis.title.x=element_text(size=18),
      axis.title.y=element_text(size=18)
    )
    
################################################################################
################################################################################
# ACTIVITY
# use temperature data to identify the habitat temp closest to animal temp
# and use this to define activity. 

# read in Canberra BOM weather data
  can <- read.csv("data\\daily max temp.csv")
    can$date <- paste(can$day, "/", can$month, "/", can$year, sep = "")
  can$jul_day <- floor(as.numeric(julian(strptime(can$date, "%d/%m/%Y", tz = "Australia/Sydney"), origin = as.POSIXct("2012-01-01"))))
  can <- dplyr:::select(can, jul_day, bom_max)
  
  avt <- left_join(dat, can, by = "jul_day") %>%
    mutate(bom_max = floor(bom_max))
  
# days with sun temps
  st <- group_by(all.temp, jul_day) %>%
        summarise(sun = length(temp_sun[is.na(temp_sun) == F]))
  table(st$sun)
# 28 of 144 days without sun temps

################################################################################
# function to compute a running sd
  rollapply <- function(vec, width, FUN)  
    sapply(seq_along(vec), 
            function(i) if (i < width) NA else FUN(vec[(i - width):(i + width)]))

################################################################################
# function to calculate activity

pl <- function(dd, aa, ss, width.inc = 1, sd.inc = 1) {
  a <- filter(avt, animal_id == aa & jul_day == dd) %>%
       arrange(jul_time) %>%
       mutate(time = hour + min/60) %>%
       dplyr:::select(animal_id, date, jul_day, jul_time, daylight, bom_max, time, animal_temp, itemp, bur_temp)
       
  b <- filter(all.temp, jul_day == dd)
  
  sun <- filter(b, site1 == ss) %>%
         group_by(jul_time) %>%
         summarise(temp_sun = mean(temp_sun))
  
  bur <- filter(b, site1 == ss) %>%
         group_by(jul_time) %>%
         summarise(temp_bur = mean(temp_bur))

# if no sun for that site use data from other sites for that day
  if(all(is.na(sun$temp_sun) == T)) {
  sun <- mutate(b, jul_time = round(jul_time, 2)) %>%
         group_by(jul_time) %>%
         summarise(temp_sun = mean(temp_sun, na.rm = T))
  }
  
# if no bur for that site use data from other sites for that day
  if(all(is.na(bur$temp_bur) == T)) {
  bur <- mutate(b, jul_time = round(jul_time, 2)) %>%
         group_by(jul_time) %>%
         summarise(temp_bur = mean(temp_bur, na.rm = T))
  }

# plot sun or not
  plot.sun <- 0
  if(all(is.na(sun$temp_sun) == F)) {
    plot.sun <- 1
    c <- spline(x = sun$jul_time, y = sun$temp_sun, xout = a$jul_time)
    a$sun_temp <- c$y
  }

# burrow temps
  if(all(is.na(bur$temp_bur) == T)) a$temp_bur <- a$bur_temp else {
    c <- spline(x = bur$jul_time, y = bur$temp_bur, xout = a$jul_time)
    a$temp_bur <- c$y
  }

  ymin <- 0
  ymax <- max(c(a$animal_temp, a$temp_bur, a$itemp, a$sun_temp), na.rm = T)
  
# for each observation identify which habitat it is closest to in temp
  dif <- matrix(nrow = nrow(a), ncol = 3)
  dif[, 1] <- abs(a$temp_bur - a$animal_temp)
  dif[, 2] <- abs(a$itemp - a$animal_temp)
  if(plot.sun == 1) dif[, 3] <- abs(a$sun_temp - a$animal_temp)
  
  a$hab <- apply(dif, 1, function(x) which(x == min(x, na.rm = T)))

################################
#criteria for activity

# if temp > shade and > burrow then active
  a$active <- ifelse(a$animal_temp > a$itemp & a$animal_temp > a$temp_bur, 1, 0)
  
# if temp diff between observations is very low then inactive
  a <- arrange(a, jul_time)
  a$temp_dif1 <- abs(a$animal_temp - lag(a$animal_temp))
  a$temp_dif2 <- abs(a$animal_temp - lead(a$animal_temp))
  a$active <- ifelse(!is.na(a$temp_dif1) & !is.na(a$temp_dif2) & a$temp_dif1 < 0.5 & a$temp_dif2 < 0.5, 0, a$active)

# remove single and double non-actives between actives
  n.obs <- length(a$active)
  for(i in 3:(n.obs - 2)) {
    a$active[i] <- ifelse(a$active[i] == 0 & a$active[i - 1] == 1 & a$active[i + 1] == 0 & a$active[i + 2] == 1, 1, a$active[i])
  }
  for(i in 2:(n.obs - 1)) {
    a$active[i] <- ifelse(a$active[i - 1] == 1 & a$active[i + 1] == 1, 1, a$active[i])
  }
  
# filter a to include only daylight hours
  a <- filter(a, daylight == 1)
  
################################
# graph
  plot(animal_temp ~ time, type = "n", data = a, ylim = c(ymin, ymax),
       bty = "l", main = paste(a$animal_id[1], a$jul_day[1], a$bom_max[1], sep = ";   "),
       xlab = "Time (hours)", ylab = "Temperature C")
    lines(a$itemp ~ a$time, col = "green", lwd = 2)
    lines(a$temp_bur ~ a$time, col = "blue", lwd = 2)
    if(plot.sun == 1) lines(a$sun_temp ~ a$time, col = "red", lwd = 2)

    point.fill <- ifelse(a$active == 1, "black", "white")
    lines(a$animal_temp ~ a$time)
    points(a$animal_temp ~ a$time, pch = 21, bg = point.fill, lwd = 2)

  return(a)
}
################################################################################

# example plot
  pl(dd = 720, aa = "QB 13-4", ss = "QB")

  
# get a list of all animal_id by jul_day
  am <- group_by(avt, animal_id, jul_day, location) %>%
        summarise(n = n())
  
# plot trace for each animal and save activity data
  out <- pl(dd = am$jul_day[1], aa = am$animal_id[1], ss = substr(am$location[1], 1, 2))
        
  for(j in 2:nrow(am)) {
    temp <- pl(dd = am$jul_day[j], aa = am$animal_id[j], ss = substr(am$location[j], 1, 2))
    out <- bind_rows(out, temp)
  }
  

################################################################################
# plot proportion active by temp groups
  out <- mutate(out, five_temp = floor(bom_max / 5))

  out$tg <- "40+"
  out$tg <- ifelse(out$bom_max < 40, "35-39", out$tg)
  out$tg <- ifelse(out$bom_max < 35, "30-34", out$tg)
  out$tg <- ifelse(out$bom_max < 30, "25-29", out$tg)
  out$tg <- ifelse(out$bom_max < 25, "20-24", out$tg)
  out$tg <- ifelse(out$bom_max < 20, "15-19", out$tg)
  out$tg <- ifelse(out$bom_max < 15, "10-14", out$tg)

# number of lizard days per group
  nd <- group_by(out, animal_id, jul_day, tg) %>%
        summarise(n = n()) %>%
        group_by(tg) %>%
        summarise(n = n())  
  nd
  
# labels for banner
  out$ban.lab <- ifelse(out$tg == "40+", "40+  (n = 3)", "35-39   (n = 27)")
  out$ban.lab <- ifelse(out$tg == "30-34", "30-34   (n = 52)", out$ban.lab)
  out$ban.lab <- ifelse(out$tg == "25-29", "25-29   (n = 83)", out$ban.lab)
  out$ban.lab <- ifelse(out$tg == "20-24", "20-24   (n = 84)", out$ban.lab)
  out$ban.lab <- ifelse(out$tg == "15-19", "15-19   (n = 27)", out$ban.lab)
  out$ban.lab <- ifelse(out$tg == "10-14", "10-14   (n = 2)", out$ban.lab)

  sum.tg <- mutate(out, floor.hour = floor(time)) %>%
             filter(bom_max >= 15) %>%
             group_by(ban.lab, floor.hour) %>%
             summarise(n = n(),
                       active = sum(active),
                       prop.active = active / n)  
  
# compute binomial confidence intervals
  out.binom <- binom.confint(sum.tg$active, sum.tg$n, method = "wilson")
  sum.tg$lcl <- out.binom$lower
  sum.tg$ucl <- out.binom$upper

  ggplot(sum.tg, aes(y = prop.active, x = floor.hour)) +
    geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1) +
    geom_line(lwd = 1.1) +
    geom_vline(xintercept = 12, lty = 2) +
    facet_wrap(~ ban.lab) +
    ylab("Proportion of time active\n") +
    xlab("\nTime of day (hour)") +
    theme_bw(12) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 18),
          axis.text=element_text(size=14),
          axis.title=element_text(size=18))

################################################################################
# now get means to plot 
  sum.out <- mutate(out, floor.hour = floor(time)) %>%
             group_by(bom_max, floor.hour) %>%
             summarise(n = n(),
                       n.liz = length(unique(paste(animal_id, jul_day))),
                       active = sum(active),
                       prop.active = active / n,
                       animal_temp = mean(animal_temp))


  sum.mean <- group_by(sum.out, bom_max) %>%
              summarise(n.obs = n(),
                        n = max(n.liz),
                        prop = mean(prop.active)) 
  
# fit quadratic regression to these data
  m1 <- lm(prop ~ bom_max + I(bom_max^2), data = sum.mean)
  summary(m1)

# dataframe to plot quadratic regression
  xx <- seq(11, 40, 0.1)
  yy <- coef(m1)[1] + coef(m1)[2] * xx + coef(m1)[3] * xx^2
  bp.dat <- data.frame(xx = xx, yy = yy)
  
  range(xx[yy >= 0.5])


  ggplot(sum.mean, aes(y = prop, x = bom_max)) +
    geom_point(aes(size = n)) +
    geom_line(data = bp.dat, aes(y = yy, x = xx), lwd = 1.2) +
    ylab("Proportion of daylight hours active\n") +
    xlab("\nDaily maximum air temperature (?C)") +
    geom_hline(yintercept = 0.5, lty = 2) +
#    ylim(0.4, 0.8) +
    theme_classic(14) +
    theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.text=element_text(size=14),
          legend.position = "bottom")
    
################################################################################
# metaboilic requirments per day based on temperature
# read in data
  dat <- read.csv("data\\metabolic rates summary_november.csv")
  names(dat) <- c("ged", "sex", "state", "temp.set", "temp", "mr", "vo2", "body_mass", "run")
  dat$ged <- factor(dat$ged)
  glimpse(dat)

# drop missing values
  dat <- filter(dat, is.na(mr) == F)
  
# convert metabolic rate to KJ g-1 h-1
  dat <- mutate(dat, mrkj = mr * 0.02)
  
# get mean mr per individual at each temp
  mmr <- dat %>%
         group_by(ged, temp.set, sex) %>%
         summarise(mrkj = mean(mrkj),
                   mr = mean(mr),
                   temp = mean(temp), 
                   n = n())
  
################################################################################
# panel figure
  
  par(mfrow = c(2, 2), mar = c(5, 5, 2, 1))
  
# activity times
  plot(prop ~ bom_max, data = sum.mean, type = "n", bty = "l",
       ylab = "Proportion of daylight hours active",
       xlab = "", cex.lab = 1.4, cex.axis = 1.2,
       xlim = c(10, 40))
  symbols(sum.mean$bom_max, sum.mean$prop, circles = sqrt(sum.mean$n / pi),
          inches = 1/12, add = T, bg = "black")
  lines(yy ~ xx, data = bp.dat, lwd = 2, col = "red")
  mtext("A", side = 3, line = 0.5, adj = 0, cex = 1.5)
  
  
# plot metabolic rates    
# fit model taking into account repeated measures on individuals
  m0 <- lmer(log(mr) ~ log(temp) + (1|ged), data = mmr)
  summary(m0)
  
# test for sex differences
  m0s <- lmer(log(mr) ~ log(temp) + sex + (1|ged), data = mmr)
  summary(m0s)
# extract coefficients
  coefs <- data.frame(coef(summary(m0s)))
# use normal distribution to approximate p-value
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
  coefs
  
# line to plot
  xx <- seq(min(mmr$temp), max(mmr$temp), 0.1)
  yy <- exp(fixef(m0)[1] + fixef(m0)[2] * log(xx))
  
  plot(mr ~ temp, data = mmr, bty = "l", pch = 19, cex = 1.5,
       xlab = "Temperature (\u00B0C)", cex.lab = 1.4, cex.axis = 1.2,
       ylab = expression(paste("Metabolic rate (ml ",O[2], g^-1, h^-1, ")")))
  lines(yy ~ xx, lwd = 2, col = "red")
  mtext("B", side = 3, line = 0.5, adj = 0, cex = 1.5)
  
# daily metabolic requirements and proporiton of time active
  day.out <- sum.out %>%
    mutate(met.rate = exp(fixef(m0)[1] + fixef(m0)[2] * log(animal_temp))) %>%
    group_by(bom_max) %>%
    summarise(n = max(n.liz),
              met.rate = mean(met.rate),
              prop = mean(prop.active),
              mp = met.rate / prop) %>%
    filter(mp != "Inf")
  
# mean metabolic rate
  m3 <- loess(met.rate ~ bom_max, control = loess.control(surface = "direct"), data = day.out)
  
  nd <- data.frame(bom_max = seq(14, 40, 0.01))
  yy <- predict(m3, newdata = nd)
  
  plot(met.rate ~ bom_max, type = "n", data = day.out, bty = "l",
       ylab = expression(paste("Mean metabolic rate (ml", O[2], g^-1, h^-1, ")")),
       xlab = "\nDaily maximum air temperature (\u00B0C)", cex.lab = 1.4, cex.axis = 1.2,
       xlim = c(10, 40))
  symbols(day.out$bom_max, day.out$met.rate, circles = sqrt(day.out$n / pi),
          inches = 1/12, add = T, bg = "black")
  lines(yy ~ nd$bom_max, lwd = 2, col = "red")
  mtext("C", side = 3, line = 0.5, adj = 0, cex = 1.5)
  
# mean metabolic cost
  m3.1 <- loess(mp ~ bom_max, control = loess.control(surface = "direct"), data = day.out)
  
  yy <- predict(m3.1, newdata = nd)
  
  plot(mp ~ bom_max, type = "n", data = day.out, bty = "l",
       ylab = expression(paste("Mean energetic cost (kJ ", g^-1, h^-1, ")")),
       xlab = "\nDaily maximum air temperature (\u00B0C)", cex.lab = 1.4, cex.axis = 1.2,
       xlim = c(10, 40))
  symbols(day.out$bom_max, day.out$mp, circles = sqrt(day.out$n / pi),
          inches = 1/12, add = T, bg = "black")
  lines(yy ~ nd$bom_max, lwd = 2, col = "red")
  mtext("D", side = 3, line = 0.5, adj = 0, cex = 1.5)

################################################################################
################################################################################
# read in Canberra BOM weather data
  cd <- read.csv("data\\daily max temp.csv")
  glimpse(cd)
  
# select summer months
  cd <- filter(cd, month %in% c(12, 1, 2)) %>%
        mutate(year = ifelse(month %in% c(10, 11, 12), year + 1, year)) %>%
        filter(year > 1940 & year < 2021)
        
  head(cd)
  tail(cd)
  
# estimate daily activity
  cd$da <- coef(m1)[1] + coef(m1)[2] * cd$bom_max + coef(m1)[3] * cd$bom_max^2
  
# estimate metabolic gain required per hour of activity
  cd$mg <- predict(m3.1, newdata = cd)
  
# sum by summers
  sum.y <- group_by(cd, year) %>%
           summarise(mean_bom = mean(bom_max),
                     sd_bom = sd(bom_max),
                     mean.act = mean(da),
                     mean.mg = mean(mg))
  
  anom.yr <- 1960:1990
  
  overall.mean.act <-  mean(sum.y$mean.act[sum.y$year %in% anom.yr])
  overall.mean.mg <- mean(sum.y$mean.mg[sum.y$year %in% anom.yr])
  overall.mean.act
  overall.mean.mg

  par(mfrow = c(2, 2), mar = c(5, 5, 3, 1))
  
  m4 <- loess(scale(mean_bom, center = mean(mean_bom[year %in% anom.yr]), scale = F) ~ year, data = sum.y, span = 0.3)

  plot(scale(mean_bom, center = mean(mean_bom[year %in% anom.yr]), scale = F) ~ year, type = "h", data = sum.y,
       col = ifelse(scale(mean_bom, center = mean(mean_bom[year %in% anom.yr]), scale = F) >= 0, "red", "blue"), lwd = 2,
       ylab = "Temperature anomaly (\u00B0C)", xlab = "", bty = "l", cex.lab = 1.4, cex.axis = 1.2)
  polygon(x = c(2006, 2010, 2010, 2006), y = c(-3, -3, 5, 5), col = rgb(0, 0, 0, 0.1), border = NA)
  lines(predict(m4) ~ sum.y$year, lwd = 2)
  mtext("A", side = 3, line = 0.5, adj = 0, cex = 1.5)

  m5 <- loess(scale(mean.act, center = mean(mean.act[year %in% anom.yr]), scale = F) / overall.mean.act ~ year, data = sum.y, span = 0.3)

  plot(scale(mean.act, center = mean(mean.act[year %in% anom.yr]), scale = F) / overall.mean.act ~ year, type = "h", data = sum.y,
       col = ifelse(scale(mean.act, center = mean(mean.act[year %in% anom.yr]), scale = F) >= 0, "blue", "red"), lwd = 2,
       ylab = "Proportional change in daily activity", xlab = "Year", bty = "l", cex.lab = 1.4, cex.axis = 1.2)
  polygon(x = c(2006, 2010, 2010, 2006), y = c(-3, -3, 5, 5), col = rgb(0, 0, 0, 0.1), border = NA)
  lines(predict(m5) ~ sum.y$year, lwd = 2)
  mtext("B", side = 3, line = 0.5, adj = 0, cex = 1.5)

  m6 <- loess(scale(mean.mg, center = mean(mean.mg[year %in% anom.yr]), scale = F) / overall.mean.mg ~ year, data = sum.y, span = 0.3)

  plot(scale(mean.mg, center = mean(mean.mg[year %in% anom.yr]), scale = F) / overall.mean.mg ~ year, type = "h", data = sum.y,
       col = ifelse(scale(mean.mg, center = mean(mean.mg[year %in% anom.yr]), scale = F) >= 0, "red", "blue"), lwd = 2,
       ylab = "Proportional change in energetic cost", xlab = "Year", bty = "l", cex.lab = 1.4, cex.axis = 1.2)
  polygon(x = c(2006, 2010, 2010, 2006), y = c(-3, -3, 5, 5), col = rgb(0, 0, 0, 0.1), border = NA)
  lines(predict(m6) ~ sum.y$year, lwd = 2)
  mtext("C", side = 3, line = 0.5, adj = 0, cex = 1.5)


################################################################################
################################################################################
# function to plot a publication graph

pl.pub <- function(dd, aa, ss, width.inc = 1, sd.inc = 0.3) {
  a <- filter(avt, animal_id == aa & jul_day == dd) %>%
       arrange(jul_time) %>%
       mutate(time = hour + min/60) %>%
       dplyr:::select(animal_id, jul_day, jul_time, daylight, bom_max, time, animal_temp, itemp, bur_temp)

  b <- filter(all.temp, jul_day == dd)

  sun <- filter(b, site1 == ss) %>%
         group_by(jul_time) %>%
         summarise(temp_sun = mean(temp_sun))

  bur <- filter(b, site1 == ss) %>%
         group_by(jul_time) %>%
         summarise(temp_bur = mean(temp_bur))

# if no sun for that site use data from other sites for that day
  if(all(is.na(sun$temp_sun) == T)) {
  sun <- mutate(b, jul_time = round(jul_time, 2)) %>%
         group_by(jul_time) %>%
         summarise(temp_sun = mean(temp_sun, na.rm = T))
  }

# if no bur for that site use data from other sites for that day
  if(all(is.na(bur$temp_bur) == T)) {
  bur <- mutate(b, jul_time = round(jul_time, 2)) %>%
         group_by(jul_time) %>%
         summarise(temp_bur = mean(temp_bur, na.rm = T))
  }

# plot sun or not
  plot.sun <- 0
  if(all(is.na(sun$temp_sun) == F)) {
    plot.sun <- 1
    c <- spline(x = sun$jul_time, y = sun$temp_sun, xout = a$jul_time)
    a$sun_temp <- c$y
  }

# burrow temps
  if(all(is.na(bur$temp_bur) == T)) a$temp_bur <- a$bur_temp else {
    c <- spline(x = bur$jul_time, y = bur$temp_bur, xout = a$jul_time)
    a$temp_bur <- c$y
  }

  ymin <- min(c(a$animal_temp, a$temp_bur, a$itemp, a$sun_temp), na.rm = T)
  ymax <- max(c(a$animal_temp, a$temp_bur, a$itemp, a$sun_temp), na.rm = T)

# filter a to include only daylight hours
#  a <- filter(a, daylight == 1)

# for each observation identify which habitat it is closest to
  dif <- matrix(nrow = nrow(a), ncol = 3)
  dif[, 1] <- abs(a$temp_bur - a$animal_temp)
  dif[, 2] <- abs(a$itemp - a$animal_temp)
  if(plot.sun == 1) dif[, 3] <- abs(a$sun_temp - a$animal_temp)

  a$hab <- apply(dif, 1, function(x) which(x == min(x, na.rm = T)))

################################
#criteria for activity
# if temp > shade and > burrow then active
  a$active <- ifelse(a$animal_temp > a$itemp & a$animal_temp > a$temp_bur, 1, 0)

# if temp diff between observations is very low then inactive
  a <- arrange(a, jul_time)
  a$temp_dif1 <- abs(a$animal_temp - lag(a$animal_temp))
  a$temp_dif2 <- abs(a$animal_temp - lead(a$animal_temp))
  a$active <- ifelse(!is.na(a$temp_dif1) & !is.na(a$temp_dif2) & a$temp_dif1 < 0.5 & a$temp_dif2 < 0.5, 0, a$active)

# remove single and double non-actives between actives
  n.obs <- length(a$active)
  for(i in 3:(n.obs - 2)) {
    a$active[i] <- ifelse(a$active[i] == 0 & a$active[i - 1] == 1 & a$active[i + 1] == 0 & a$active[i + 2] == 1, 1, a$active[i])
  }
  for(i in 2:(n.obs - 1)) {
    a$active[i] <- ifelse(a$active[i - 1] == 1 & a$active[i + 1] == 1, 1, a$active[i])
  }

# filter a to include only daylight hours
#  a <- filter(a, daylight == 1)

  return(list(a, plot.sun))
}

  out1 <- pl.pub(dd = 449, aa = "CK 23", ss = "CK")
  out2 <- pl.pub(dd = 720, aa = "QB 13-4", ss = "QB")


  a <- out2[[1]]
  plot.sun <- out2[[2]]

  sun.rise <- a$time[which(a$daylight == 1)[1]]
  sun.set <- a$time[max(which(a$daylight == 1))]

  a <- a[-99, ]

  x11()
  plot(animal_temp ~ time, type = "n", data = a, ylim = c(15, 53), xlim = c(5, 21),
       bty = "l",
       xlab = "Time (hours)", ylab = "Temperature (?C)", cex.lab = 1.4, cex.axis = 1.2)
    lines(a$itemp ~ a$time, col = "orange", lwd = 3)
    lines(a$temp_bur ~ a$time, col = "blue", lwd = 3)
    if(plot.sun == 1) lines(a$sun_temp ~ a$time, col = "red", lwd = 3)

    point.fill <- ifelse(a$active == 1, "black", "white")
    lines(a$animal_temp ~ a$time, lwd = 2)
    points(a$animal_temp ~ a$time, pch = 21, bg = point.fill, cex = 1.2, lwd = 2)

    abline(v = sun.rise, lty = 2)
    text(sun.rise, 56, "Sunrise", xpd = NA)
    abline(v = sun.set, lty = 2)
    text(sun.set, 56, "Sunset", xpd = NA)

    text(14, 52, expression(italic(T[sun])))
    text(14, 40, expression(italic(T[shade])))
    text(14, 28.5, expression(italic(T[burrow])))



