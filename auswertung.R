library(ggplot2, plyr)

(name <-'"Pinecone"')

timeData <- processingTime(replaceZeroWithMean(raw$vPAGETIME44),
                           replaceZeroWithMean(raw$vPAGETIME45),
                           replaceZeroWithMean(raw$vPAGETIME46),
                           replaceZeroWithMean(raw$vPAGETIME47),
                           replaceZeroWithMean(raw$vPAGETIME48),
                           name
                           )


categorizationData <- categorization(replaceNaWithMean(raw$V201),
                                     replaceNaWithMean(raw$V206),
                                     replaceNaWithMean(raw$V211),
                                     replaceNaWithMean(raw$V216),
                                     replaceNaWithMean(raw$V221),
                                     name
                                     )

ratingData <- rating(na.omit(raw$V197.C41),
                     na.omit(raw$V202.C42),
                     na.omit(raw$V207.C43),
                     na.omit(raw$V212.C44),
                     na.omit(raw$V217.C45),
                     
                     na.omit(raw$V198.C41),
                     na.omit(raw$V203.C42),
                     na.omit(raw$V208.C43),
                     na.omit(raw$V213.C44),
                     na.omit(raw$V218.C45),
                     
                     na.omit(raw$V199.C41),
                     na.omit(raw$V204.C42),
                     na.omit(raw$V209.C43),
                     na.omit(raw$V214.C44),
                     na.omit(raw$V219.C45),
                     
                     na.omit(raw$V200.C41),
                     na.omit(raw$V205.C42),
                     na.omit(raw$V210.C43),
                     na.omit(raw$V215.C44),
                     na.omit(raw$V220.C45),
                     name
                     )

#Creates final plot with standard deviation error bars
finalPlotSD <- ratingData$SD + 
  coord_fixed(0.8) + 
  theme_classic() +
  labs(title = paste('Results for',name, 'including standard deviation'), x = 'Level of CGI') + 
  scale_y_continuous('Categorization/Rating', breaks = seq(-1,6, by=1), sec.axis = sec_axis(~.*10, name = 'Processing Time in s', breaks = seq(0,60,by=10))) +
  geom_line(data = timeData, aes(x=levelOfCgi, y=times/10, group=1), size=1, linetype='dashed', color = 'darkorange') +
    geom_point(data = timeData, aes(x=levelOfCgi, y=times/10, group=1), size=1.5, color='darkorange') +
    geom_errorbar(data = timeData, aes(x=levelOfCgi, y=times/10, ymin=times/10-sd/10, ymax=times/10+sd/10, group=1), width=.3, color='darkorange', size=1) +
  geom_line(data = categorizationData, aes(x=levelOfCgi, y=category, group=1), size=1, linetype='dashed', color='gray50') +
    geom_point(data = categorizationData, aes(x=levelOfCgi, y=category, group=1), size=1.5, color='gray50') +
    geom_errorbar(data = categorizationData,  aes(x=levelOfCgi, y=category, group=1, ymin=category-sd, ymax=category+sd), width=.3, size=1, color='gray50') 

print(finalPlotSD)

#Creates final plot with confidence interval error bars
finalPlotCI <- ratingData$CI +
  coord_fixed(0.9) +
  theme_classic() +
  labs(title = paste('Results',name, 'including confidence intervals'), x = 'Level of CGI') +
  scale_y_continuous('Categorization/Rating', breaks = seq(-1,6, by=1), sec.axis = sec_axis(~.*10, name = 'Processing Time in s', breaks = seq(0,60,by=10))) +
  geom_line(data = timeData, aes(x=levelOfCgi, y=times/10, group=1), size=1, linetype='dashed', color = 'darkorange') +
    geom_point(data = timeData, aes(x=levelOfCgi, y=times/10, group=1), size=1.5, color='darkorange') +
    geom_errorbar(data = timeData, aes(x=levelOfCgi, y=times/10, ymin=times/10-ci/10, ymax=times/10+ci/10, group=1), width=.3, color='darkorange', size=1) +
  geom_line(data = categorizationData, aes(x=levelOfCgi, y=category, group=1), size=1, color='gray50', linetype='dashed') +
    geom_point(data = categorizationData, aes(x=levelOfCgi, y=category, group=1), size=1.5, color='gray50') +
    geom_errorbar(data = categorizationData,  aes(x=levelOfCgi, y=category, group=1, ymin=category-ci, ymax=category+ci), width=.3, size=1, color='gray50')

print(finalPlotCI)

# Varianz

varianceMeans(raw$V67.C19,raw$V92.C16, raw$V77.C17, raw$V82.C18, raw$V87.C20)

# Functions

# Returns Data Frame containing Processing times
processingTime <- function(pagetime1, pagetime2, pagetime3, pagetime4, pagetime5) {
  vCgi0 <- rep('0', length(pagetime1))
  vCgi1 <- rep('1', length(pagetime2))
  vCgi2 <- rep('2', length(pagetime3))
  vCgi3 <- rep('3', length(pagetime4))
  vCgi4 <- rep('4', length(pagetime5))
  
  levelOfCgi <- c(vCgi0, vCgi1, vCgi2, vCgi3, vCgi4)
  times <- c(pagetime1, pagetime2, pagetime3, pagetime4, pagetime5)
  
  df <- data.frame(levelOfCgi, times)

  df2 <- summarySE(df, measurevar='times', groupvars = 'levelOfCgi')

  return(df2)
}

#Returns Data Frame Containing Categorization Results
categorization <- function(cgi0, cgi1, cgi2, cgi3, cgi4) {
  vCgi0 <- rep('0', length(cgi0))
  vCgi1 <- rep('1', length(cgi1))
  vCgi2 <- rep('2', length(cgi2))
  vCgi3 <- rep('3', length(cgi3))
  vCgi4 <- rep('4', length(cgi4))
  
  levelOfCgi <- c(vCgi0, vCgi1, vCgi2, vCgi3, vCgi4)
  category <- c(cgi0, cgi1, cgi2, cgi3, cgi4)
  
  df <- data.frame(levelOfCgi, category)
  
  df2 <- summarySE(df, measurevar = 'category', groupvars = 'levelOfCgi')
  
  return(df2)
}

#Returns to Results for Rating, one as Plot with standard deviation error bars, one with confidence interval error bars
rating <- function(w1, w2, w3, w4, w5, 
                   f1, f2, f3, f4, f5,
                   e1, e2, e3, e4, e5,
                   r1, r2, r3, r4, r5,
                   name) {
  
  
  warmth1 <- rep('0', length(w1))
  warmth2 <- rep('1', length(w2))
  warmth3 <- rep('2', length(w3))
  warmth4 <- rep('3', length(w4))
  warmth5 <- rep('4', length(w5))
  
  levelOfCgiWarmth <- c(warmth1, warmth2, warmth3, warmth4, warmth5)
  warmth <- c(w1, w2, w3, w4,w5)
  dfw <- data.frame(levelOfCgiWarmth, warmth)
  df2w <- summarySE(dfw, measurevar = 'warmth', groupvars = 'levelOfCgiWarmth')
  
  familiarity1 <- rep('0', length(f1))
  familiarity2 <- rep('1', length(f2))
  familiarity3 <- rep('2', length(f3))
  familiarity4 <- rep('3', length(f4))
  familiarity5 <- rep('4', length(f5))
  
  levelOfCgiFamiliarity <- c(familiarity1, familiarity2, familiarity3, familiarity4, familiarity5)
  familiarity <- c(f1, f2, f3, f4, f5)
  dff <- data.frame(levelOfCgiFamiliarity, familiarity)
  df2f <- summarySE(dff, measurevar = 'familiarity', groupvars = 'levelOfCgiFamiliarity')

  eeriness1 <- rep('0', length(e1))
  eeriness2 <- rep('1', length(e2))
  eeriness3 <- rep('2', length(e3))
  eeriness4 <- rep('3', length(e4))
  eeriness5 <- rep('4', length(e5))
  
  levelOfCgiEeriness <- c(eeriness1, eeriness2, eeriness3, eeriness4, eeriness5)
  eeriness <- c(e5, e4, e3, e2, e1)
  dfe <- data.frame(levelOfCgiEeriness, eeriness)
  df2e <- summarySE(dfe, measurevar = 'eeriness', groupvars = 'levelOfCgiEeriness')
  
  realism1 <- rep('0', length(r1))
  realism2 <- rep('1', length(r2))
  realism3 <- rep('2', length(r3))
  realism4 <- rep('3', length(r4))
  realism5 <- rep('4', length(r5))
  
  levelOfCgiRealism <- c(realism1, realism2, realism3, realism4, realism5)
  realism <- c(r1, r2, r3, r4, r5)
  dfr <- data.frame(levelOfCgiRealism, realism)
  df2r <- summarySE(dfr, measurevar = 'realism', groupvars = 'levelOfCgiRealism')
  
  ratingPlotSD <- ggplot() + 
    
    theme_classic() +
    
    coord_fixed(1) +
    
    geom_line(data = df2w, aes(x=levelOfCgiWarmth, y=warmth, group=1), size=1, color='blue') +
    geom_point(data = df2w, aes(x=levelOfCgiWarmth, y=warmth, group=1), size=1.5, color='blue') +
    geom_errorbar(data = df2w, aes(x=levelOfCgiWarmth, y=warmth, group=1, ymin=warmth-sd, ymax=warmth+sd), width=.3, color='blue', size=1)+


    geom_line(data = df2r,aes(x=levelOfCgiRealism, y=realism, group=1), size=1, color='green3') +
    geom_point(data = df2r,aes(x=levelOfCgiRealism, y=realism, group=1), size=1.5, color='green3') +
    geom_errorbar(data = df2r,aes(x=levelOfCgiRealism, y=realism, group=1, ymin=realism-sd, ymax=realism+sd), width=.3, size=1, color='green3') +

    geom_line(data = df2f, aes(x=levelOfCgiFamiliarity, y=familiarity, group=1), size=1, color='red') +
    geom_point(data = df2f, aes(x=levelOfCgiFamiliarity, y=familiarity, group=1), size=1.5, color='red') +
    geom_errorbar(data = df2f, aes(x=levelOfCgiFamiliarity, y=familiarity, group=1, ymin=familiarity-sd, ymax=familiarity+sd), width=.3, color='red', size=1) + 

    geom_line(data = df2e, aes(x=levelOfCgiEeriness, y=eeriness, group=1), size=1) +
    geom_point(data = df2e, aes(x=levelOfCgiEeriness, y=eeriness, group=1), size=1.5) +
    geom_errorbar(data = df2e, aes(x=levelOfCgiEeriness, y=eeriness, group=1, ymin=eeriness-sd, ymax=eeriness+sd), width=.3, size=1) 
    
    print(ratingPlotSD)
    
  ratingPlotCI <- ggplot() +

    theme_classic() +

    coord_fixed(1) +

    geom_line(data = df2w, aes(x=levelOfCgiWarmth, y=warmth, group=1), size=1, color='blue') +
    geom_point(data = df2w, aes(x=levelOfCgiWarmth, y=warmth, group=1), size=1.5, color='blue') +
    geom_errorbar(data = df2w, aes(x=levelOfCgiWarmth, y=warmth, group=1, ymin=warmth-ci, ymax=warmth+ci), width=.3, color='blue', size=1)+

    

    geom_line(data = df2r,aes(x=levelOfCgiRealism, y=realism, group=1), size=1, color='green3') +
    geom_point(data = df2r,aes(x=levelOfCgiRealism, y=realism, group=1), size=1.5, color='green3') +
    geom_errorbar(data = df2r,aes(x=levelOfCgiRealism, y=realism, group=1, ymin=realism-ci, ymax=realism+ci), width=.3, size=1, color='green3') +
      
    geom_line(data = df2f, aes(x=levelOfCgiFamiliarity, y=familiarity, group=1), size=1, color='red') +
    geom_point(data = df2f, aes(x=levelOfCgiFamiliarity, y=familiarity, group=1), size=1.5, color='red') +
    geom_errorbar(data = df2f, aes(x=levelOfCgiFamiliarity, y=familiarity, group=1, ymin=familiarity-ci, ymax=familiarity+ci), width=.3, color='red', size=1)+

    geom_line(data = df2e, aes(x=levelOfCgiEeriness, y=eeriness, group=1), size=1) +
    geom_point(data = df2e, aes(x=levelOfCgiEeriness, y=eeriness, group=1), size=1.5) +
    geom_errorbar(data = df2e, aes(x=levelOfCgiEeriness, y=eeriness, group=1, ymin=eeriness-ci, ymax=eeriness+ci), width=.3, size=1)
    print(ratingPlotCI)

  data_list <- list('SD' = ratingPlotSD, 'CI' = ratingPlotCI)

  return(data_list)
    
}

#Replaces '0' in a vector with its mean 
replaceZeroWithMean <- function(vector) {
  m <- mean(vector[vector != 0])
  vector <- replace(vector, vector==0, m)
  return(vector)
}

#Replaces 'na' in a vector with its mean 
replaceNaWithMean <- function(vector) {
  m <- mean(na.omit(vector))
  (vector[is.na(vector)] <- m)
  return(vector)
}

#returns analyzes variance for a row of results and returns mean results
varianceMeans <- function(value1, value2, value3, value4, value5) {
  a <- t.test(value1, value2, paired = T)
  b <- t.test(value2, value3, paired = T)
  c <- t.test(value3, value4, paired = T)
  d <- t.test(value4, value5, paired = T)
  
  t <- c(a$statistic, b$statistic, c$statistic, d$statistic)
  df <- c(a$parameter, b$parameter, c$parameter, d$parameter)
  p <- c(a$p.value, b$p.value, c$p.value, d$p.value)
  mod <- c(a$estimate, b$estimate, c$estimate, d$estimate)
  
  return(c(mean(t), mean(df), mean(p), mean(mod)))
}
