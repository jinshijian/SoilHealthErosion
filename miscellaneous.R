
#*****************************************************************************************************************
# Basic
#*****************************************************************************************************************


# summary function

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=T,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     median = median   (xx[[col]], na.rm=na.rm),
                     #do.call("rbind", tapply(xx[[col]], measurevar, quantile, c(0.25, 0.5, 0.75)))
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#*****************************************************************************************************************
# Not used
#*****************************************************************************************************************
# plot Figure normal distribution

plot_normal <- function () {
  
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.6, 0.7, 0.0, 0.1)  # by inches, inner margin
       , omi = c(0.0, 0.1, 0.4, 0.1)  # by inches, outer margin
       , mgp = c(0.5, 0.5, 0) # set distance of axis
       , tcl = 0.4
       , cex.axis = 1.0
       , mfrow=c(2,2) )
  
  # Subset
  sub_erosion <- erosion_meta[!is.na(erosion_meta$Erosion_C), ]
  sub_erosion$yi <- log(sub_erosion$Erosion_T) - log(sub_erosion$Erosion_C)
  sub_runoff <- erosion_meta[!is.na(erosion_meta$Runoff_C),]
  sub_runoff$yi <- log(sub_runoff$Runoff_T) - log(sub_runoff$Runoff_C)
  
  # hist gram and QQ plot for erosion *************************
  list_histo <- hist(sub_erosion$yi, col='gray',breaks=10
                     , las = 1
                     , cex = 1
                     , main = ""
                     , xlab = ""
                     , ylab = "" )
  # lines(density(Shannon_ANOVA$residuals), col="blue", lwd=2)
  box()
  mtext(side = 1, text = paste0("Erosion"), line = 2.0, cex=1, outer = F)
  mtext(side = 2, text = expression("Frequency (n)"), line = 2.0, cex=1.0, outer = F)
  
  # check for kurtosis
  print(summary(kurtosis(sub_erosion$yi)))
  
  # shapiro.test
  # shapiro <- shapiro.test(sub_erosion$yi)
  # shapiro_p <- shapiro$p.value
  # text(min(sub_erosion$yi)*0.95, max(list_histo$counts)*0.95
  #      , paste("(a) shapiro(p) = ",round(shapiro_p, 2)), cex = 1, adj = 0)
  
  text(-4.25, 3.65, paste0("(a)"), cex = 1, adj = 0)
  
  # QQ plot
  qqPlot(sub_erosion$yi
         , las = 1
         , cex = 1
         , pch = 1
         , main = ""
         , xlab = ""
         , ylab = ""
         , col.lines  = 'red')
  
  mtext(side = 1, text = expression("Normal data quantile"), line = 2.0, cex=1, outer = F)
  mtext(side = 2, text = expression("Normal theoretical quantile"), line = 2.0, cex=1.0, outer = F)
  text(-1.75, 0, paste0("(b)"), cex = 1, adj = 0)
  
  # hist gram and QQ plot for runoff *************************
  list_histo <- hist(sub_runoff$yi, col='gray',breaks=10
                     , las = 1
                     , cex = 1
                     , main = ""
                     , xlab = ""
                     , ylab = "" )
  # lines(density(Shannon_ANOVA$residuals), col="blue", lwd=2)
  box()
  mtext(side = 1, text = paste0("Runoff"), line = 2.5, cex=1, outer = F)
  mtext(side = 2, text = expression("Frequency (n)"), line = 2.0, cex=1.0, outer = F)
  text(-1.75, 6.25, paste0("(c)"), cex = 1, adj = 0)
  
  # check for kurtosis
  print(summary(kurtosis(sub_runoff$yi)))
  
  # shapiro.test
  # shapiro <- shapiro.test(sub_runoff$yi)
  # shapiro_p <- shapiro$p.value
  # text(min(sub_runoff$yi)*0.95, max(list_histo$counts)*0.95
  #      , paste("(c) shapiro(p) = ",round(shapiro_p, 2)), cex = 1, adj = 0)
  
  # QQ plot
  qqPlot(sub_runoff$yi
         , las = 1
         , cex = 1
         , pch = 1
         , main = ""
         , xlab = ""
         , ylab = ""
         , col.lines  = 'red')
  
  mtext(side = 1, text = expression("Normal data quantile"), line = 2.5, cex=1, outer = F)
  mtext(side = 2, text = expression("Normal theoretical quantile"), line = 2.0, cex=1.0, outer = F)
  text(-1.75, 0, paste0("(d)"), cex = 1, adj = 0)
  
}


# meta analysis **********************************************************************************************

meta_erosion <- function () {
  
  subdata <- erosion_meta[!is.na(erosion_meta$Erosion_C), ] 
  
  subdata$sd_C <- ifelse(is.na(subdata$sd_C), subdata$Erosion_C*0.1, subdata$sd_C) 
  subdata$sd_T <- ifelse(is.na(subdata$sd_T), subdata$Erosion_T*0.1, subdata$sd_T) 
  # mean(subdata$NC, na.rm=T)
  subdata$NC <- ifelse(is.na(subdata$NC), 4, subdata$NC)
  subdata$NT <- ifelse(is.na(subdata$NT), 4, subdata$NT)
  
  subdata$yi <- log(subdata$Erosion_T) - log(subdata$Erosion_C)
  subdata$vi <- subdata$sd_T^2/(subdata$NT*subdata$Erosion_T) + subdata$sd_C^2/(subdata$NC*subdata$Erosion_C)
  
  res1 <- rma(yi, vi, weighted=TRUE, data = subdata)
  
  forest(res1, slab = paste("Experiment ", 1:19, ": ", subdata$FamilyName, ifelse(subdata$FirstName!="",", ","")
                            , subdata$FirstName, " (", subdata$Year,")", sep = "")
  )
  
  op <- par(cex = 1, font = 2)
  text(-13.2,                21, "Experiment ID: First author (Year)",     pos = 4)
  text(8,                 21, "Log ratio [95% CI]", pos = 2)
  par(op)
  
  # Supplemental
  plot(res1)
  
  res2 <- trimfill(res1, comb.fixed=TRUE)
  summary(trimfill(res2, comb.fixed=TRUE))
  plot(trimfill(res2, comb.fixed=TRUE))
}

meta_erosion()


# Meta analysis of runoff
meta_runoff <- function () {
  
  subdata <- erosion_meta[!is.na(erosion_meta$Runoff_C), ] 
  
  subdata$sd_C <- ifelse(is.na(subdata$sd_C), subdata$Runoff_C*0.1, subdata$sd_C) 
  subdata$sd_T <- ifelse(is.na(subdata$sd_T), subdata$Runoff_T*0.1, subdata$sd_T) 
  # mean(subdata$NC, na.rm=T)
  subdata$NC <- ifelse(is.na(subdata$NC), 4, subdata$NC)
  subdata$NT <- ifelse(is.na(subdata$NT), 4, subdata$NT)
  
  subdata$yi <- log(subdata$Runoff_T) - log(subdata$Runoff_C)
  subdata$vi <- subdata$sd_T^2/(subdata$NT*subdata$Runoff_T) + subdata$sd_C^2/(subdata$NC*subdata$Runoff_C)
  
  res1 <- rma(yi, vi, weighted=TRUE, data = subdata)
  # forest(res1)
  
  forest(res1, slab = paste("Experiment ",1:20, ": ", subdata$FamilyName, ifelse(subdata$FirstName!="",", ","")
                            , subdata$FirstName, " (", subdata$Year,")", sep = "")
  )
  
  op <- par(cex = 1, font = 2)
  text(-16.2,                22, "Experiment ID: First author (Year)",     pos = 4)
  text(16,                 22, "Log ratio [95% CI]", pos = 2)
  par(op)
  
  # supplemental
  plot(res1)
}

meta_runoff()

### Meta-analysis by group
# Runoff meta-analysis by group
erosion_meta$sd_C <- ifelse(is.na(erosion_meta$sd_C), erosion_meta$Erosion_C*0.1, erosion_meta$sd_C) 
erosion_meta$sd_T <- ifelse(is.na(erosion_meta$sd_T), erosion_meta$Erosion_T*0.1, erosion_meta$sd_T) 
# mean(erosion_meta$NC, na.rm=T)
erosion_meta$NC <- ifelse(is.na(erosion_meta$NC), 4, erosion_meta$NC)
erosion_meta$NT <- ifelse(is.na(erosion_meta$NT), 4, erosion_meta$NT)

erosion_meta$yi <- log(erosion_meta$Erosion_T) - log(erosion_meta$Erosion_C)
erosion_meta$vi <- erosion_meta$sd_T^2/(erosion_meta$NT*erosion_meta$Erosion_T) + erosion_meta$sd_C^2/(erosion_meta$NC*erosion_meta$Erosion_C)

unique(erosion_meta$Conservation_type)
subdata <- subset(erosion_meta, Conservation_type == 'CC' & !is.na(Erosion_C))

res1 <- rma(yi, vi, weighted=TRUE, data = subdata)
plot(res1)
summary(res1)

res2 <- trimfill(res1, comb.fixed=TRUE)
summary(trimfill(res2, comb.fixed=TRUE))
plot(trimfill(res2, comb.fixed=TRUE))

# Runoff meta-analysis by group
erosion_meta$sd_C <- ifelse(is.na(erosion_meta$sd_C), erosion_meta$Runoff_C*0.1, erosion_meta$sd_C) 
erosion_meta$sd_T <- ifelse(is.na(erosion_meta$sd_T), erosion_meta$Runoff_T*0.1, erosion_meta$sd_T) 
# mean(erosion_meta$NC, na.rm=T)
erosion_meta$NC <- ifelse(is.na(erosion_meta$NC), 4, erosion_meta$NC)
erosion_meta$NT <- ifelse(is.na(erosion_meta$NT), 4, erosion_meta$NT)

erosion_meta$yi <- log(erosion_meta$Runoff_T) - log(erosion_meta$Runoff_C)
erosion_meta$vi <- erosion_meta$sd_T^2/(erosion_meta$NT*erosion_meta$Runoff_T) + erosion_meta$sd_C^2/(erosion_meta$NC*erosion_meta$Runoff_C)

unique(erosion_meta$Conservation_type)
subdata <- subset(erosion_meta, Conservation_type == 'CC' & !is.na(Runoff_C))

res1 <- rma(yi, vi, weighted=TRUE, data = subdata)
plot(res1)
summary(res1)

res2 <- trimfill(res1, comb.fixed=TRUE)
summary(trimfill(res2, comb.fixed=TRUE))
plot(trimfill(res2, comb.fixed=TRUE))


#*****************************************************************************************************************
# test code
#*****************************************************************************************************************


