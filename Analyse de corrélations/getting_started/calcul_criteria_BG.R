
# Chargement des packages

library(tidyverse)
library(reshape2)
library(cowplot)
library(magrittr)
library(ggplot2)
library(corrplot)
library(slider)
library(nlme)

#Fonctions ####

stability <- function(df, nperf){
  # df <- data
  for(i in 3:nperf){
    # i=5
    name <- colnames(df)[i]
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(agroecosyst)%>%
      mutate(detrend = if(all(is.na(perf))) NA else  residuals(lm(perf ~ Year, na.action = na.exclude)))%>%# de-trended values (residuals of linear regression)
      mutate(min = min(perf, na.rm = T), max = max(perf, na.rm = T), Range = max - min)%>%  # min, max, yield range
      #mutate(min = ifelse(min == Inf, NA, min), max = ifelse(max == -Inf, NA, max), range = ifelse(max == Inf, NA, range))%>%
      mutate(VAR = var(perf, na.rm = T))%>% # variance
      mutate(RSD = abs(sd(perf, na.rm = T)/mean(perf, na.rm = T))*100)%>% # relative standard deviation 
      arrange(df$agroecosyst,df$Year)%>%
      mutate(MVS = mean(abs(perf- slide_dbl(perf, mean, .before = 1, .after = 1)), na.rm = T))%>%
      ungroup()
    
    
    df <- df %>%
      group_by(Year)%>%
      mutate(EI = mean(detrend, na.rm = T))%>% # environmental index
      ungroup()
    
    df <- df %>%
      group_by(agroecosyst)%>%
      mutate(slopeFW = if(all(is.na(perf))) NA else abs(lm(detrend ~ EI, na.action = na.exclude)$coefficients[[2]]-1))%>% # slope of finlay-wilkinson regression
      mutate(RESf = if(all(is.na(perf))) NA else residuals(lm(perf ~ Year, na.action = na.exclude)))%>% # residuals of linear regression
      ungroup()
    
    colnames(df)[(ncol(df)-9):ncol(df)] <-c(paste0(name,".detrend"),paste0(name,".min"),
                                            paste0(name,".max"),paste0(name,".Range"),
                                            paste0(name,".VAR"), paste0(name, ".RSD"),
                                            paste0(name, ".MSV"),paste0(name,".EI"), paste0(name, ".slopeFW"),
                                            paste0(name,".RESf"))              
    
    dir.create(file.path("../correlations_mockdata/Correlations/Criteria correlation plot"), recursive = TRUE)
    jpeg(paste0("Criteria correlation plot/",name,".stability_cor.jpg"), width = 17, height = 17, units = "cm", res = 300)
    corrplot(cor(df[c((ncol(df)-8): (ncol(df) - 3),(ncol(df) - 1): ncol(df))], use = "pairwise.complete.obs"))
    dev.off()
    
    colnames(df)[i] <- name
    
  }
  return(df)
}

resistance <- function(df, nperf){
  #df <- data
  for(i in 3:nperf){
    #i=3
    name <- colnames(df)[i]
    colnames(df)[i] <- "perf"
    colnames(df)[colnames(df) == paste0(name,".detrend")] <- "detrend"
    
    perclow <- quantile(df$perf, c(.10), na.rm = T)
    perchigh <- quantile(df$perf, c(.80), na.rm = T)
    
    df <- df%>%
      mutate(RESe = if(length(perf[Year == 2015]) != 0) (mean(c(perf[Year == 2015])) - mean(c(mean(c(perf[Year == 2012])),mean(c(perf[Year == 2013])),mean(c(perf[Year == 2014]))), na.rm = T))
             else NA)%>% # Resistance to drought (2015)
      group_by(agroecosyst)%>%
      mutate(probalow = length(na.omit(perf[perf < perclow]))/length(na.omit(perf)))%>%# probability of low performance level Li et al 2019
      mutate(probalow10 = dnorm((perclow - mean(perf, na.rm = T))/sd(perf, na.rm = T))*100)%>%# probability of low performance level Macholdt et al 2020
      mutate(probahigh = length(na.omit(perf[perf > perchigh]))/length(na.omit(perf)))%>%# probability of high performance level
      mutate(pi = if(all(is.na(perf)) | length(!is.na(perf)[!is.na(perf) == T]) == 1) NA else (fitted(loess(perf ~ Year, span = 0.85, na.action = na.exclude))))%>%
      mutate(loessdetrend = if(all(is.na(perf) | identical(round(perf,3),round(pi,3)))) NA else ((perf-pi)/pi))%>% # de trending with loess regression (y - fitted)/fitted
      mutate(probafail = 1/var(loessdetrend, na.rm = T))%>%# probability of failure/crash
      mutate(NED = length(na.omit(perf[perf < 0.75*slide_dbl(perf, mean, na.action = na.omit, .before = 2)])))%>% # number of economic disruption
      mutate(RECOV = recovery(perf))%>%# time to recover (in years) of economic disruption
      ungroup()

    df <- df[,-c(ncol(df)-5)]
    
    #dir.create(file.path("Criteria correlation plot"), recursive = TRUE)  
    jpeg(paste0("Criteria correlation plot/",name,".resist_cor.jpg"), width = 17, height = 17, units = "cm", res = 300)
    corrplot(cor(df[c((ncol(df)-7): (ncol(df)-5),(ncol(df)-3): ncol(df))], use = "pairwise.complete.obs"))
    #print(cor.test(pull(df[(ncol(df)-5)]), pull(df[ncol(df)-3])))
    dev.off()
    
    colnames(df)[c((ncol(df)-7): ncol(df))] <- c(paste0(name,".probalow"),paste0(name,".probalow10"),
                                                 paste0(name,".probahigh"),
                                                 paste0(name, ".loessdetrend"),
                                                 paste0(name, ".probafail"), paste0(name, ".RESe"),
                                                 paste0(name, ".NED"), paste0(name, ".RECOV"))
    colnames(df)[i] <- name
    colnames(df)[colnames(df) == "detrend"] <- paste0(name,".detrend")
  }
  return(df)
}

leveltrend <- function(df, nperf){
  for(i in 3:nperf){
    # df <- data
    name <- colnames(df)[i]
    
    colnames(df)[i] <- "perf"
    
    df <- df%>%
      group_by(agroecosyst)%>%
      mutate(level = mean(perf, na.rm = T))%>%# mean level
      mutate(RD = mean(indicators$GM_ha[data$Year %in% Year] - perf[Year %in% indicators$Year] ,na.rm = T))%>%# mean relative distance to regional GM
      mutate(INTERf = if(all(is.na(perf))) NA else lm(perf ~ Year, na.action = na.exclude)$coefficients[[1]])%>% # intercept of linear regression
      mutate(PREDf = if(all(is.na(perf))) NA else mean(fitted.values(lm(perf ~ Year, na.action = na.exclude)), na.rm = T))%>% # mean of predicted values of linear regression
      mutate(TRENDf = if(all(is.na(perf))) NA else lm(perf ~ Year, na.action = na.exclude)$coefficients[[2]])%>% # slope of linear regression
      ungroup()
      
    #dir.create(file.path("Criteria correlation plot"), recursive = TRUE)  
    jpeg(paste0("Criteria correlation plot/",name,".trend_cor.jpg"), width = 17, height = 17, units = "cm", res = 300)
    corrplot(cor(df[c((ncol(df)-4): ncol(df))], use = "pairwise.complete.obs"))
    dev.off()
    
    # dir.create(file.path("Criteria fixed regression plot"), recursive = TRUE)  
    # jpeg(paste0("Criteria fixed regression plot/",name,".fixed_intercept_slope.jpg"), width = 17, height = 17, units = "cm", res = 300)
    # ggplot(df, aes(x = intercept, y = trend, label = agroecosyst)) + geom_text() + ggtitle(paste("Regression on ",name)) + xlab("Intercept") +
    #   ylab("Slope")
    # dev.off()
    
    colnames(df)[c((ncol(df)-4): ncol(df))] <- c(paste0(name,".level"),paste0(name, ".RD"),paste0(name, ".INTERf"),
                                                 paste0(name,".PREDf"), paste0(name,".TRENDf"))
    colnames(df)[i] <- name
    
    df <- df %>%
      mutate_at(vars(1:ncol(df)), ~ifelse(. == "NaN", NA, .))
  }
  return(df)
}

recovery <- function(perf){
  # i=3
  # df <- data[data$agroecosyst == "21",]
  # colnames(df)[i] <- "perf"
  # attach(df)
  
  dis <- which(perf < 0.75*slide_dbl(perf, mean, na.action = na.omit, .before = 2))  # Number of economic disruption
  n <- c()
  for(i in dis){ # pour chaque disruption
    n <- c(n,
           if(length(which(perf[(i+1):length(perf)] < mean(c(lag(perf)[i], lag(perf)[i-1]), na.rm = T))) == length(perf)-i | length(perf)-i == 0) NA # if never recovery or disruption the last year of records -> NA
           else length(which(perf[(i+1):length(perf)] < mean(c(lag(perf)[i], lag(perf)[i-1]), na.rm = T)))+1)  # else count the nb of year since recovery
  } # n = 1 means that the year after, recovery as been done
  # perf
  # dis
  # n
  return(if (is.null(n)|is.na(all(n))) NA else (mean(n, na.rm =T)))
}

randomreg <- function(df, nperf){
  for(i in 3:nperf){
    # i=3
    # df <- data_trend
    slope = c()
    interc = c()
    name <- colnames(df)[i]
    colnames(df)[i] <- "perf"
    
    # Calcul of random residuals and slope of the individuals in the linear mixed regression   
    t = df$Year
    y = df$perf
    indiv = df$agroecosyst
    rand.reg = lme(y ~ t, random = ~ 1 + t |indiv, na.action=na.exclude,
                   control=list(msMaxIter=5e8,opt="optim",msVerbose=TRUE)) # corAR1(), random = list(indiv = pdDiag(~ 1 + t))
    
    #Check hypothesis
    # plot(rand.reg) # homog�n�it� variance
    # print(qqnorm(rand.reg, ~ranef(.))) # normalit� des r�sidus
    # print(dwtest(resid(rand.reg) ~ t)) # not correlated
    
    for (j in unique(indiv)){
      slope[indiv==j] = random.effects(rand.reg)[j,2]
      interc[indiv ==j] = random.effects(rand.reg)[j,1]
    } 
    
    df <- df %>%
      mutate(RESr = if(all(is.na(perf))) NA else as.vector(residuals(rand.reg)))%>% # residuals of mixed regression
      mutate(pred = as.vector(fitted.values(rand.reg)))%>% # fitted values of mixed regression
      mutate(TRENDr = slope)%>% # slope of mixed regression
      mutate(INTERr = interc)%>% # intercept of mixed regression
      group_by(agroecosyst)%>%
      mutate(PREDr = if(all(is.na(perf))) NA else mean(pred, na.rm = T))%>%
      ungroup()
    
    df <- df[, -(ncol(df)-3)] # remove pred column
    
    colnames(df)[c((ncol(df)-3): ncol(df))] <- c(paste0(name,".RESr"),paste0(name, ".TRENDr"),paste0(name, ".INTERr"),
                                                 paste0(name,".PREDr"))
    colnames(df)[i] <- name
    
    dir.create(file.path("Criteria mixed regression plot"), recursive = TRUE)
    jpeg(paste0("Criteria mixed regression plot/",name,".fixed_intercept_slope.jpg"), width = 19, height = 13, units = "cm", res = 300)
    plot(random.effects(rand.reg)[,1], random.effects(rand.reg)[,2], type="n", xlab="Random intersect", ylab="Random slope", main=paste("Random regression on ",name))
    text(random.effects(rand.reg)[,1], random.effects(rand.reg)[,2], rownames(random.effects(rand.reg)))
    dev.off()
    
  }
  
  return(df)
  
}


criteriadynamics <- function(agroecosyst, Year, Y){
  
  data_temp <- cbind(agroecosyst, Year, Y)
  nperf <- ncol(Y) + 2
  
  # Criteria calculation ####
  
  data_stab <- stability(data_temp, nperf)
  data_resist <- resistance(data_stab, nperf)
  data_trend <- leveltrend(data_resist, nperf)
  data_randomreg <- randomreg(data_trend, nperf) 
  names(data_randomreg)
  
  #Save file
  write_excel_csv(data_randomreg,paste("Criteria_Data.csv", sep=""), delim = ";", na = "")
  
  return(data_randomreg)
}
