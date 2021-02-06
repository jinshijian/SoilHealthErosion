
#*****************************************************************************************************************
# Basic functions
#*****************************************************************************************************************
# plot site distribution

plot_sites <- function (sdata) {
  # Subset
  # sdata <- SoilHealthDB
  sub_erosion <- sdata %>% filter(!is.na(Erosion_C))
  sub_runoff <- sdata[!is.na(sdata$Runoff_C),]
  
  # aggregate for sub_erosion
  siteInfor_erosion <- sub_erosion %>% select(YearPublication, Latitude, Longitude) %>% 
    group_by(Latitude, Longitude) %>% summarise(N = n())
  siteInfor_erosion <- siteInfor_erosion %>% na.omit()

  sort(siteInfor_erosion$N)
  siteInfor_erosion[siteInfor_erosion$N >= 75,3]
  
  siteInfor_erosion$var_size <- mean(siteInfor_erosion$N)*0.15 + (siteInfor_erosion$N)*0.05
  
  sort(unique(siteInfor_erosion$var_size))
  
  # aggregate for runoff
  siteInfor_runoff <- sub_runoff %>% select(YearPublication, Latitude, Longitude) %>% 
    group_by(Latitude, Longitude) %>% summarise(N = n())
  siteInfor_runoff <- siteInfor_runoff %>% na.omit()
  siteInfor_runoff[siteInfor_runoff$N >= 75,3]
  siteInfor_runoff$var_size <- mean(siteInfor_runoff$N)*0.15 + (siteInfor_runoff$N)*0.05
  
  # create labels information
  # label_erosion <- paste0("Erosion (n=", nrow(siteInfor_erosion),")")
  # label_runoff <- paste0("Runoff (n=", nrow(siteInfor_runoff),")")
  label_erosion <- paste0("Erosion (n=", nrow(siteInfor_erosion),")")
  label_runoff <- paste0("Runoff (n=", nrow(siteInfor_runoff),")")
  
  
  # Step 2: Plot
  # global map
  counties <- map_data("world", region = ".", exact = FALSE)
  # sort(unique(counties$region))
  
  globMap <- ggplot(data = counties) + 
    geom_polygon(aes(x = long, y = lat , group = group), color = "white", fill = 'gray') + 
    guides(fill=FALSE)+
    theme(legend.position="none")
  # ylim(-57, 90)
  globMap
  
  sitemap1 <- globMap + 
    # erosion
    geom_point(data = siteInfor_erosion, aes(x=Longitude, y=Latitude)
               , shape=1, col="black", size = siteInfor_erosion$var_size*1.5
               , show.legend = TRUE) +
    # runoff
    geom_point(data = siteInfor_runoff, aes(x=Longitude, y=Latitude)
               , shape=3, col = "red" 
               , size = siteInfor_runoff$var_size*1.5, alpha = 7/10
               , show.legend = TRUE)+
    
    theme(axis.text.y   = element_text(size=12),
          axis.text.x   = element_text(size=12),
          legend.position = "bottom",
          
          axis.title.y   = element_text(size=15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x   = element_text(size=15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
          
          # axis.title.y  = element_blank(),
          # axis.title.x  = element_blank(),
          
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
    scale_x_continuous(name="Longitude", breaks=seq(-180,180, 30),labels = seq(-180,180, 30))+
    scale_y_continuous(limits = c(-60, 90),name="Latitude", breaks=seq(-60,90,20),labels = seq(-60,90,20))+
    annotate("text", x = -150, y = 0, label = "Legend", size = 4, adj = 0)+
    annotate("text", x = -150, y = -15, label = label_erosion, size = 4, adj = 0)+
    annotate("text", x = -150, y = -30, label = label_runoff, size = 4, adj = 0)+
    
    # Add legend sign
    geom_point( aes(x=-170, y=-15)
                , shape=1, col="black", size = 3
                , show.legend = TRUE) +
    geom_point( aes(x=-170, y=-30)
                , shape=3, col="red", size = 3
                , show.legend = TRUE) 
  
  # ?scale_y_continuous()
  
  # ggsave("outputs/Figure 1. Sites distribution.png", width = 8, height = 4.5, dpi = 300, units = "in" )
  
  print(sitemap1) 
}




#************************************************************
# plot normal distribution using SoilHealthDB

plot_normal2 <- function (sdata1, sdata2) {
  
  tiff("outputs/Figure 2. All data.tiff", width = 8, height = 6, pointsize = 1/300, units = 'in', res = 300)
  
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.6, 0.7, 0.0, 0.1)  # by inches, inner margin
       , omi = c(0.0, 0.1, 0.4, 0.1)  # by inches, outer margin
       , mgp = c(0.5, 0.5, 0) # set distance of axis
       , tcl = 0.4
       , cex.axis = 1.0
       , mfrow=c(2, 3) )
  
  # hist gram and QQ plot for erosion *************************
  list_histo <- hist(sdata1$yi, col='gray',breaks=20
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
  print(summary(kurtosis(sdata1$yi)))
  
  # shapiro.test
  # shapiro <- shapiro.test(sdata1$yi)
  # shapiro_p <- shapiro$p.value
  # text(min(sdata1$yi)*0.95, max(list_histo$counts)*0.95
  #      , paste("(a) shapiro(p) = ",round(shapiro_p, 2)), cex = 1, adj = 0)
  
  text(-6, 105, paste0("( a )"), cex = 1.25, adj = 0)
  
  # QQ plot
  qqPlot(sdata1$yi
         , las = 1
         , cex = 1
         , pch = 1
         , main = ""
         , xlab = ""
         , ylab = ""
         , col.lines  = 'red')
  
  mtext(side = 1, text = expression("Normal data quantile"), line = 2.0, cex=1, outer = F)
  mtext(side = 2, text = expression("Normal theoretical quantile"), line = 2.0, cex=1.0, outer = F)
  text(-2.5, 2.5, paste0("( b )"), cex = 1.25, adj = 0)
  
  # boots
  k =50000
  # n = 10
  mysamples = replicate(k, sample(sdata1$yi, replace=T))
  mymeans = apply(mysamples, 2, mean)
  
  list_histo <- hist(mymeans, col='gray',breaks=20
                     , las = 1
                     , cex = 1
                     , main = ""
                     , xlab = ""
                     , ylab = "" )
  

  text(-2.0, 9000, paste0("( c )"), cex = 1.25, adj = 0)
  
  box()
  mtext(side = 1, text = paste0("Erosion (boots)"), line = 2.0, cex=1, outer = F)
  mtext(side = 2, text = expression("Frequency (n)"), line = 3.5, cex=1.0, outer = F)
  
  
  # unique(sub_runoff$yi) %>% sort()
  # sdata2 <- sub_runoff
  # hist gram and QQ plot for runoff *************************
  list_histo <- hist(sdata2$yi, col='gray',breaks=25
                     , las = 1
                     , cex = 1
                     , main = ""
                     , xlab = ""
                     , ylab = "" )
  # lines(density(Shannon_ANOVA$residuals), col="blue", lwd=2)
  box()
  mtext(side = 1, text = paste0("Runoff"), line = 2.0, cex=1, outer = F)
  mtext(side = 2, text = expression("Frequency (n)"), line = 2.0, cex=1.0, outer = F)
  text(-5.5, 150, paste0("( d )"), cex = 1.25, adj = 0)
  
  # check for kurtosis
  print(summary(kurtosis(sdata2$yi)))
  
  # shapiro.test
  # shapiro <- shapiro.test(sdata2$yi)
  # shapiro_p <- shapiro$p.value
  # text(min(sdata2$yi)*0.95, max(list_histo$counts)*0.95
  #      , paste("(c) shapiro(p) = ",round(shapiro_p, 2)), cex = 1, adj = 0)
  
  # QQ plot
  qqPlot(sdata2$yi
         , las = 1
         , cex = 1
         , pch = 1
         , main = ""
         , xlab = ""
         , ylab = ""
         , col.lines  = 'red')
  
  mtext(side = 1, text = expression("Normal data quantile"), line = 2.5, cex=1, outer = F)
  mtext(side = 2, text = expression("Normal theoretical quantile"), line = 2.0, cex=1.0, outer = F)
  text(-2.5, 3.25, paste0("( e )"), cex = 1.25, adj = 0)
  
  # boots
  
  mysamples = replicate(k, sample(sdata2$yi, replace=T))
  mymeans = apply(mysamples, 2, mean)
  
  list_histo <- hist(mymeans, col='gray',breaks=20
                     , las = 1
                     , cex = 1
                     , main = ""
                     , xlab = ""
                     , ylab = "" )
  
  
  text(-1.375, 10800, paste0("( f )"), cex = 1.25, adj = 0)
  
  box()
  mtext(side = 1, text = paste0("Runoff (boots)"), line = 2.0, cex=1, outer = F)
  mtext(side = 2, text = expression("Frequency (n)"), line = 3.0, cex=1.0, outer = F)
  
  dev.off()
}


#************************************************************
# function for boost sampling

boosting_erorun <- function (sdata, term, treatment) {
  k = n_rep
  mysamples = replicate(k, sample(sdata$yi, replace=T))
  mymeans = apply(mysamples, 2, mean)
  
  # quantile
  BootsTest <- quantile(mymeans,c(0.025, 0.50, 0.975))
  Boots_outputs <- tibble(term, treatment, (exp(BootsTest[[1]])-1)*100, (exp(BootsTest[[2]])-1)*100
                          , (exp(BootsTest[[3]])-1)*100, nrow(sdata), unique(sdata$StudyID) %>% length )
  colnames(Boots_outputs) <- c("Term", "Treatment", "Low", "Mean", "High", "obs", "n_study")
  
  return(Boots_outputs)
}



raw_boots_compare <- function (sdata) {
  # histgram for raw data
  
  raw_plot <- tibble(Raw = sdata$yi) %>%
    ggplot(., aes(Raw)) + geom_histogram(color="black", fill="gray", bins = 30) +
    theme(axis.title.x = element_blank())
  
  # histgram for boosting results
  k = n_rep
  mysamples = replicate(k, sample(sdata$yi, replace=T))
  mymeans = apply(mysamples, 2, mean)
  
  boots_plot <- tibble(Boots = mymeans) %>%
    ggplot(., aes(Boots)) + geom_histogram(color="black", fill="gray", bins = 30) +
    theme(axis.title.x = element_blank())
  
  plot_grid(raw_plot, boots_plot)
}




#*****************************************************************************************************************
# SLR functions
#*****************************************************************************************************************

SLR_erosion <- function () {
  # test question 1: whether Ts is good surrogate for Ta?
  pdf( 'outputs/SLR_erosion.pdf', width=4, height=3)
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.6, 0.7, 0.0, 0.1)  # by inches, inner margin
       , omi = c(0.0, 0.1, 0.4, 0.1)  # by inches, outer margin
       , mgp = c(0.5, 0.5, 0) # set distance of axis
       , tcl = 0.4
       , cex.axis = 1.0
       # , las = 1
       , mfrow=c(1,1) )
  
  erocol <- which(colnames(SoilHealthDB) == 'Erosion_C')
  indcator_col <- which(colnames(SoilHealthDB) == 'Runoff_C')
  respcol <- c(seq(which(colnames(SoilHealthDB) == 'BiomassCash_C'),which(colnames(SoilHealthDB) == 'OC_C'),5)
               ,seq(which(colnames(SoilHealthDB) == 'N_C'),which(colnames(SoilHealthDB) == 'MBN_C'),5)) # all response columns
  
  erosion_results <- data.frame()
  
  for (i in 1: length(respcol)) {
    subdata <- SoilHealthDB[, c(which(colnames(SoilHealthDB) == 'StudyID'|colnames(SoilHealthDB) == 'ExperimentID')
                                , erocol, erocol + 1, indcator_col, indcator_col + 1, respcol[i], respcol[i]+1 )]
    
    indicator <- colnames(subdata)[7] 
    indicator <- str_sub(indicator, 1, str_length(indicator)-2)
    
    colnames(subdata) <- c("StudyID", "ExperimentID",  "Erosion_C", "Erosion_T", "Runoff_C", "Runoff_T", "Y_C", "Y_T")
    
    subdata <- subdata %>% filter(!is.na(Erosion_C) & !is.na(Y_C))
    # min(subdata$Erosion_C)
    # min(subdata$Erosion_T)
    subdata$Erosion_C <- ifelse(subdata$Erosion_C == 0, 0.001, subdata$Erosion_C)
    subdata$Erosion_T <- ifelse(subdata$Erosion_T == 0, 0.001, subdata$Erosion_T)
    
    subdata$Y_C <- ifelse(subdata$Y_C == 0, 0.001, subdata$Y_C)
    subdata$Y_T <- ifelse(subdata$Y_T == 0, 0.001, subdata$Y_T)
    
    n_total <- nrow(subdata)
    n_study <- length(unique(subdata$StudyID))
    
    if (n_total > 5) {
      subdata %>% mutate(y = log(Y_T)-log(Y_C), x = log(Erosion_T) - log(Erosion_C)) -> subdata
      
      
      # plot
      plot (subdata$y ~ subdata$x
            , xlab = ""
            , ylab = ""
            , pch = c(19) 
            , main = ""
            , col = c("gray")
            , data = subdata
            , las = 1)
      mtext(side = 1, text = "Erosion (RR)", line = 1.5, cex=1.25, outer = F)
      mtext(side = 2, text = paste0(indicator, " (RR)"), line = 2.5, cex=1.25, outer = F)
      
      # SLR model
      SLR <- lm(y ~ x, data = subdata)
      erosion_a <- summary(SLR)$coefficients[1,1] %>% round(6)
      erosion_b <- summary(SLR)$coefficients[2,1] %>% round(6)
      p_erosion_b <- summary(SLR)$coefficients[2,4]%>% round(6)
      erosion_R2 <- summary(SLR)$r.squared %>% round(6)
      
      # Pearson correlation
      pearspn_cor <- cor(x = subdata$x, y = subdata$y, method = "pearson")
      spearman_cor <- cor(x = subdata$x, y = subdata$y, method = "spearman")
      rcorr_result <- rcorr(as.matrix(tibble(x = subdata$x, y = subdata$y)))
      
      model_prems <- data.frame(indicator, n_total, n_study, erosion_a, erosion_b, p_erosion_b, erosion_R2,
                                pearspn_cor, spearman_cor, rcorr_result$r[1,2], rcorr_result$P[1,2] %>% round(2) )
      erosion_results <- rbind(erosion_results, model_prems)
      
      # add regression line
      if(p_erosion_b<0.05) {curve(erosion_a + erosion_b * x, min(subdata$x), max(subdata$x), col = "black", lwd = 2, add = T)} else{next}
    }
  }
  dev.off()
  return(erosion_results)
}





#******************************************************************************************************
SLR_Runoff <- function () {
  pdf( 'outputs/SLR_Runoff.pdf', width=4, height=3)
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.6, 0.7, 0.0, 0.1)  # by inches, inner margin
       , omi = c(0.0, 0.1, 0.4, 0.1)  # by inches, outer margin
       , mgp = c(0.5, 0.5, 0) # set distance of axis
       , tcl = 0.4
       , cex.axis = 1.0
       # , las = 1
       , mfrow=c(1,1) )
  
  erocol <- which(colnames(SoilHealthDB) == 'Erosion_C')
  indcator_col <- which(colnames(SoilHealthDB) == 'Runoff_C')
  respcol <- c(seq(which(colnames(SoilHealthDB) == 'BiomassCash_C'),which(colnames(SoilHealthDB) == 'OC_C'),5)
               ,seq(which(colnames(SoilHealthDB) == 'N_C'),which(colnames(SoilHealthDB) == 'MBN_C'),5)) # all response columns
  
  runoff_results <- data.frame()
  
  for (i in 1: length(respcol)) {
    subdata <- SoilHealthDB[, c(which(colnames(SoilHealthDB) == 'StudyID'|colnames(SoilHealthDB) == 'ExperimentID')
                                , erocol, erocol + 1, indcator_col, indcator_col + 1, respcol[i], respcol[i]+1 )]
    
    indicator <- colnames(subdata)[7] 
    indicator <- str_sub(indicator, 1, str_length(indicator)-2)
    
    colnames(subdata) <- c("StudyID", "ExperimentID",  "Erosion_C", "Erosion_T", "Runoff_C", "Runoff_T", "Y_C", "Y_T")
    
    subdata <- subdata %>% filter(!is.na(Erosion_C) & !is.na(Y_C))
    # min(subdata$Erosion_C)
    # min(subdata$Erosion_T)
    subdata$Erosion_C <- ifelse(subdata$Erosion_C == 0, 0.001, subdata$Erosion_C)
    subdata$Erosion_T <- ifelse(subdata$Erosion_T == 0, 0.001, subdata$Erosion_T)
    
    subdata$Y_C <- ifelse(subdata$Y_C == 0, 0.001, subdata$Y_C)
    subdata$Y_T <- ifelse(subdata$Y_T == 0, 0.001, subdata$Y_T)
    
    n_total <- nrow(subdata)
    n_study <- length(unique(subdata$StudyID))
    
    subdata %>% mutate(y = log(Y_T)-log(Y_C), x = log(Erosion_T) - log(Erosion_C)) -> subdata
    
    if(n_total > 5) {
      subdata <- SoilHealthDB[, c(which(colnames(SoilHealthDB) == 'StudyID'|colnames(SoilHealthDB) == 'ExperimentID')
                                  , erocol, erocol + 1, indcator_col, indcator_col + 1, respcol[i], respcol[i]+1 )]
      
      indicator <- colnames(subdata)[7] 
      indicator <- str_sub(indicator, 1, str_length(indicator)-2)
      
      colnames(subdata) <- c("StudyID", "ExperimentID",  "Erosion_C", "Erosion_T", "Runoff_C", "Runoff_T", "Y_C", "Y_T")
      
      subdata <- subdata %>% filter(!is.na(Runoff_C) & !is.na(Y_C))
      min(subdata$Runoff_C)
      min(subdata$Runoff_T)
      subdata$Runoff_C <- ifelse(subdata$Runoff_C == 0, 0.001, subdata$Runoff_C)
      subdata$Runoff_T <- ifelse(subdata$Runoff_T == 0, 0.001, subdata$Runoff_T)
      
      subdata$Y_C <- ifelse(subdata$Y_C == 0, 0.001, subdata$Y_C)
      subdata$Y_T <- ifelse(subdata$Y_T == 0, 0.001, subdata$Y_T)
      
      n_total <- nrow(subdata)
      n_study <- length(unique(subdata$StudyID))
      
      subdata %>% mutate(y = log(Y_T)-log(Y_C), x = log(Runoff_T) - log(Runoff_C)) -> subdata
      
      # plot
      plot (subdata$y ~ subdata$x
            , xlab = ""
            , ylab = ""
            , pch = c(19) 
            , main = ""
            , col = c("gray")
            , data = subdata
            , las = 1)
      mtext(side = 1, text = "Runoff (RR)"
            , line = 1.5, cex=1.25, outer = F)
      mtext(side = 2, text = paste0(indicator, " (RR)"), line = 2.5, cex=1.25, outer = F)
      
      # SLR model
      SLR <- lm(y ~ x, data = subdata)
      runoff_a <- summary(SLR)$coefficients[1,1] %>% round(6)
      runoff_b <- summary(SLR)$coefficients[2,1] %>% round(6)
      p_runoff_b <- summary(SLR)$coefficients[2,4]%>% round(6)
      runoff_R2 <- summary(SLR)$r.squared %>% round(6)
      
      # Pearson correlation
      pearspn_cor <- cor(x = subdata$x, y = subdata$y, method = "pearson")
      spearman_cor <- cor(x = subdata$x, y = subdata$y, method = "spearman")
      rcorr_result <- rcorr(as.matrix(tibble(x = subdata$x, y = subdata$y)))
     
      
      model_prems <- data.frame(indicator, n_total, n_study, runoff_a, runoff_b, p_runoff_b, runoff_R2,
                                pearspn_cor, spearman_cor, rcorr_result$r[1,2], rcorr_result$P[1,2])
      runoff_results <- rbind(runoff_results, model_prems)
      
      # add regression line
      if(p_runoff_b<0.05) {curve(runoff_a + runoff_b * x, min(subdata$x), max(subdata$x), col = "black", lwd = 2, add = T)} else {next}
      
      print(i)
    }
  }
  
  dev.off()
  return (runoff_results)
}


# i = 1
#******************************************************************************************************
SLR_leaching <- function () {
  pdf( 'outputs/SLR_leaching.pdf', width=4, height=3)
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.6, 0.7, 0.0, 0.1)  # by inches, inner margin
       , omi = c(0.0, 0.1, 0.4, 0.1)  # by inches, outer margin
       , mgp = c(0.5, 0.5, 0) # set distance of axis
       , tcl = 0.4
       , cex.axis = 1.0
       # , las = 1
       , mfrow=c(1,1) )
  
  erocol <- which(colnames(SoilHealthDB) == 'Erosion_C')
  indcator_col <- which(colnames(SoilHealthDB) == 'Leaching_C')
  respcol <- c(seq(which(colnames(SoilHealthDB) == 'BiomassCash_C'),which(colnames(SoilHealthDB) == 'OC_C'),5)
               ,seq(which(colnames(SoilHealthDB) == 'N_C'),which(colnames(SoilHealthDB) == 'MBN_C'),5)) # all response columns
  
  runoff_results <- data.frame()
  
  for (i in 1: length(respcol)) {
    subdata <- SoilHealthDB[, c(which(colnames(SoilHealthDB) == 'StudyID'|colnames(SoilHealthDB) == 'ExperimentID')
                                , erocol, erocol + 1, indcator_col, indcator_col + 1, respcol[i], respcol[i]+1 )]
    
    indicator <- colnames(subdata)[7] 
    indicator <- str_sub(indicator, 1, str_length(indicator)-2)
    
    colnames(subdata) <- c("StudyID", "ExperimentID",  "Erosion_C", "Erosion_T", "Leaching_C", "Leaching_T", "Y_C", "Y_T")
    
    subdata <- subdata %>% filter(!is.na(Leaching_C) & !is.na(Y_C))
    # min(subdata$Leaching_C)
    # min(subdata$Leaching_T)
    subdata$Leaching_C <- ifelse(subdata$Leaching_C == 0, 0.001, subdata$Leaching_C)
    subdata$Leaching_T <- ifelse(subdata$Leaching_T == 0, 0.001, subdata$Leaching_T)
    
    subdata$Y_C <- ifelse(subdata$Y_C == 0, 0.001, subdata$Y_C)
    subdata$Y_T <- ifelse(subdata$Y_T == 0, 0.001, subdata$Y_T)
    
    n_total <- nrow(subdata)
    n_study <- length(unique(subdata$StudyID))
    
    subdata %>% mutate(y = log(Y_T)-log(Y_C), x = log(Leaching_T) - log(Leaching_C)) -> subdata
    
    if(n_total > 5) {
      subdata <- SoilHealthDB[, c(which(colnames(SoilHealthDB) == 'StudyID'|colnames(SoilHealthDB) == 'ExperimentID')
                                  , erocol, erocol + 1, indcator_col, indcator_col + 1, respcol[i], respcol[i]+1 )]
      
      indicator <- colnames(subdata)[7] 
      indicator <- str_sub(indicator, 1, str_length(indicator)-2)
      
      colnames(subdata) <- c("StudyID", "ExperimentID",  "Erosion_C", "Erosion_T", "Leaching_C", "Leaching_T", "Y_C", "Y_T")
      
      subdata <- subdata %>% filter(!is.na(Leaching_C) & !is.na(Y_C))
      
      subdata$Leaching_C <- ifelse(subdata$Leaching_C == 0, 0.001, subdata$Leaching_C)
      subdata$Leaching_T <- ifelse(subdata$Leaching_T == 0, 0.001, subdata$Leaching_T)
      
      subdata$Y_C <- ifelse(subdata$Y_C == 0, 0.001, subdata$Y_C)
      subdata$Y_T <- ifelse(subdata$Y_T == 0, 0.001, subdata$Y_T)
      
      n_total <- nrow(subdata)
      n_study <- length(unique(subdata$StudyID))
      
      subdata %>% mutate(y = log(Y_T)-log(Y_C), x = log(Leaching_T) - log(Leaching_C)) -> subdata
      
      # plot
      plot (subdata$y ~ subdata$x
            , xlab = ""
            , ylab = ""
            , pch = c(19) 
            , main = ""
            , col = c("gray")
            , data = subdata
            , las = 1)
      mtext(side = 1, text = "Leaching (RR)"
            , line = 1.5, cex=1.25, outer = F)
      mtext(side = 2, text = paste0(indicator, " (RR)"), line = 2.5, cex=1.25, outer = F)
      
      # SLR model
      SLR <- lm(y ~ x, data = subdata)
      runoff_a <- summary(SLR)$coefficients[1,1] %>% round(6)
      runoff_b <- summary(SLR)$coefficients[2,1] %>% round(6)
      p_runoff_b <- summary(SLR)$coefficients[2,4]%>% round(6)
      runoff_R2 <- summary(SLR)$r.squared %>% round(6)
      
      model_prems <- data.frame(indicator, n_total, n_study, runoff_a, runoff_b, p_runoff_b, runoff_R2)
      runoff_results <- rbind(runoff_results, model_prems)
      
      # add regression line
      if(p_runoff_b<0.05) {curve(runoff_a + runoff_b * x, min(subdata$x), max(subdata$x), col = "black", lwd = 2, add = T)} else {next}
      
    }
  }
  
  dev.off()
  return (runoff_results)
}

# SLR_leaching()


#******************************************************************************************************
# SLR_depth()

# i = 1
groud_water_slr <- function(){
  
  pdf( 'outputs/SLR_water_depth.pdf', width=4, height=3)
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.6, 0.7, 0.0, 0.1)  # by inches, inner margin
       , omi = c(0.0, 0.1, 0.4, 0.1)  # by inches, outer margin
       , mgp = c(0.5, 0.5, 0) # set distance of axis
       , tcl = 0.4
       , cex.axis = 1.0
       # , las = 1
       , mfrow=c(1,1) )
  
  # get subdata
  
  indcator_col <- which(colnames(SoilHealthDB) == 'water_depth')
  respcol <- c(seq(which(colnames(SoilHealthDB) == 'BiomassCash_C'),which(colnames(SoilHealthDB) == 'OC_C'),5)
               ,seq(which(colnames(SoilHealthDB) == 'N_C'),which(colnames(SoilHealthDB) == 'MBN_C'),5)) # all response columns
  
  # resp_results <- data.frame()
  
  for (i in c(2:4,8,17,18,34,35)){
    
    subdata <- SoilHealthDB[, c(which(colnames(SoilHealthDB) == 'StudyID'|colnames(SoilHealthDB) == 'ExperimentID')
                                , indcator_col, respcol[i], respcol[i]+1 )]
    
    indicator <- colnames(subdata)[4] 
    # indicator <- str_sub(indicator, 1, str_length(indicator)-2)
    
    colnames(subdata) <- c("StudyID", "ExperimentID", "Water_depth", "Y_C", "Y_T")
    
    subdata <- subdata %>% filter(!is.na(Water_depth) & !is.na(Y_C) & Water_depth < 255)
    # subdata %>% mutate(y = log(Y_T)-log(Y_C)) -> subdata
    
    # SLR model
    SLR <- lm(Y_C ~ Water_depth, data = subdata)
    runoff_a <- try(summary(SLR)$coefficients[1,1] %>% round(6))
    runoff_b <- try(summary(SLR)$coefficients[2,1] %>% round(6))
    p_runoff_b <- try(summary(SLR)$coefficients[2,4]%>% round(6))
    
    n_total <- nrow(subdata)
    n_study <- length(unique(subdata$StudyID))
    
    
    # plot
    plot (subdata$Y_C ~ subdata$Water_depth
          , xlab = ""
          , ylab = ""
          , pch = c(19) 
          , main = ""
          , col = c("gray")
          , data = subdata
          , las = 1)
    mtext(side = 1, text = "Water_depth(m)"
          , line = 1.75, cex=1.25, outer = F)
    mtext(side = 2, text = paste0(indicator), line = 2.5, cex=1.25, outer = F)
    
    if(p_runoff_b<0.05) {curve(runoff_a + runoff_b * x, 0, 50, col = "black", lwd = 2, add = T)} 
    else {next}
    
    # print for loop of ith run
    print(i)
    
  }
  
  dev.off()
}

# groud_water_slr()
# i = 24
groud_water_slr_RR <- function(){
  
  pdf( 'outputs/SLR_water_depth_RR.pdf', width=4, height=3)
  par( mar=c(2, 0.2, 0.2, 0.2)
       , mai=c(0.6, 0.7, 0.0, 0.1)  # by inches, inner margin
       , omi = c(0.0, 0.1, 0.4, 0.1)  # by inches, outer margin
       , mgp = c(0.5, 0.5, 0) # set distance of axis
       , tcl = 0.4
       , cex.axis = 1.0
       # , las = 1
       , mfrow=c(1,1) )
  
  # get subdata
  
  indcator_col <- which(colnames(SoilHealthDB) == 'water_depth')
  respcol <- c(seq(which(colnames(SoilHealthDB) == 'BiomassCash_C'),which(colnames(SoilHealthDB) == 'OC_C'),5)
               ,seq(which(colnames(SoilHealthDB) == 'N_C'),which(colnames(SoilHealthDB) == 'MBN_C'),5)) # all response columns
  
  # resp_results <- data.frame()
  
  for (i in c(2:30,34,37,38)){
    
    subdata <- SoilHealthDB[, c(which(colnames(SoilHealthDB) == 'StudyID'|colnames(SoilHealthDB) == 'ExperimentID')
                                , indcator_col, respcol[i], respcol[i]+1 )]
    
    indicator <- colnames(subdata)[4] 
    indicator <- str_sub(indicator, 1, str_length(indicator)-2)
    
    colnames(subdata) <- c("StudyID", "ExperimentID", "Water_depth", "Y_C", "Y_T")
    
    subdata <- subdata %>% filter(!is.na(Water_depth) & !is.na(Y_C) & Water_depth < 255)
    
    subdata$Y_C <- ifelse(subdata$Y_C == 0, 0.001, subdata$Y_C)
    subdata$Y_T <- ifelse(subdata$Y_T == 0, 0.001, subdata$Y_T)
    subdata %>% mutate(y = log(Y_T)-log(Y_C)) -> subdata
    
    # SLR model
    SLR <- lm(y ~ Water_depth, data = subdata)
    runoff_a <- try(summary(SLR)$coefficients[1,1] %>% round(6))
    runoff_b <- try(summary(SLR)$coefficients[2,1] %>% round(6))
    p_runoff_b <- try(summary(SLR)$coefficients[2,4]%>% round(6))
    
    n_total <- nrow(subdata)
    n_study <- length(unique(subdata$StudyID))
    
    
    # plot
    plot (subdata$y ~ subdata$Water_depth
          , xlab = ""
          , ylab = ""
          , pch = c(19) 
          , main = ""
          , col = c("gray")
          , data = subdata
          , las = 1)
    mtext(side = 1, text = "Water_depth(m)"
          , line = 1.75, cex=1.25, outer = F)
    mtext(side = 2, text = paste0(indicator, " (RR)"), line = 2.5, cex=1.25, outer = F)
    
    if(p_runoff_b<0.05) {curve(runoff_a + runoff_b * x, 0, 50, col = "black", lwd = 2, add = T)}
    else {next}
    
    # print for loop of ith run
    print(i)
    
  }
  
  dev.off()
}

# groud_water_slr_RR()

#*****************************************************************************************************************
# Summary function for indicator sample size
#*****************************************************************************************************************

sum_indicator <- function () {
  
  # summary for erosion and indicators
  
  erocol <- which(colnames(SoilHealthDB) == 'Erosion_C')
  runoffcol <- which(colnames(SoilHealthDB) == 'Runoff_C')
  respcol <- c(seq(which(colnames(SoilHealthDB) == 'BiomassCash_C'),which(colnames(SoilHealthDB) == 'OC_C'),5)
               ,seq(which(colnames(SoilHealthDB) == 'N_C'),which(colnames(SoilHealthDB) == 'MBN_C'),5)) # all response columns
  
  sum_results <- data.frame()
  
  for (i in 1: length(respcol)) {
    subdata <- SoilHealthDB[, c(which(colnames(SoilHealthDB) == 'StudyID'|colnames(SoilHealthDB) == 'ExperimentID')
                                , erocol, erocol + 1, runoffcol, runoffcol + 1, respcol[i], respcol[i]+1 )]
    
    indicator <- colnames(subdata)[7] 
    indicator <- str_sub(indicator, 1, str_length(indicator)-2)
    
    colnames(subdata) <- c("StudyID", "ExperimentID",  "Erosion_C", "Erosion_T", "Runoff_C", "Runoff_T", "Y_C", "Y_T")
    
    subdata <- subdata %>% filter(!is.na(Erosion_C) & Erosion_C > 0 & !is.na(Y_C))

    n_total <- nrow(subdata)
    n_study <- length(unique(subdata$StudyID))
    resp <- "Erosion"
    
    if (n_total > 0) {
      
      erosion_indicator <- data.frame(indicator, n_total, n_study, resp)
      sum_results <- rbind(sum_results, erosion_indicator)
     
    }
    else {print(indicator)}
  }
  
  # summary for runoff and indicators
  runoffcol <- which(colnames(SoilHealthDB) == 'Runoff_C')
  respcol <- c(seq(which(colnames(SoilHealthDB) == 'BiomassCash_C'),which(colnames(SoilHealthDB) == 'OC_C'),5)
               ,seq(which(colnames(SoilHealthDB) == 'N_C'),which(colnames(SoilHealthDB) == 'MBN_C'),5)) # all response columns
  
  for (i in 1: length(respcol)) {
    subdata <- SoilHealthDB[, c(which(colnames(SoilHealthDB) == 'StudyID'|colnames(SoilHealthDB) == 'ExperimentID')
                                , erocol, erocol + 1, runoffcol, runoffcol + 1, respcol[i], respcol[i]+1 )]
    
    indicator <- colnames(subdata)[7] 
    indicator <- str_sub(indicator, 1, str_length(indicator)-2)
    
    colnames(subdata) <- c("StudyID", "ExperimentID",  "Erosion_C", "Erosion_T", "Runoff_C", "Runoff_T", "Y_C", "Y_T")
    
    subdata <- subdata %>% filter(!is.na(Runoff_C) & Runoff_C > 0 & !is.na(Y_C))
    
    n_total <- nrow(subdata)
    n_study <- length(unique(subdata$StudyID))
    resp <- "Runoff"
    
    if (n_total > 0) {
      
      runoff_indicator <- data.frame(indicator, n_total, n_study, resp)
      sum_results <- rbind(sum_results, runoff_indicator)
      
    }
  }

  return(sum_results)
}



