##Uncertainty quantification analyses
##4/22/19

rm(list=ls())

######################################################################################
##Libraries##
######################################################################################

library(dplyr)

######################################################################################
##Inputs##
######################################################################################

##Filepath for saving figures
figure_filepath <- "/Users/emhuang/Documents/data/ACSGA/paper/plotting/uncertainty_quant"

##Subject numbers
subjects <- c("subject1","subject2","subject3","subject4")

##Filepath with all the subjects' prediction results from implementing movelet method
##with tri-axial data and L2 distance (see movelet_triaxial_L2.R code in movelet folder)
filepath <- "~/Documents/data/ACSGA/analysis/movelet/results/standard"

##Data types
dataTypes <- c("acc_front", "acc_back", "gyro_front", "gyro_back")

##Cutoff
cutoff <- 0.5
cutoffs <- seq(from = 0, to = 1, by = 0.01)
ncutoffs <- length(cutoffs)

##list of activities in the training and test data
activityList <- c("walk",
                  "stand",
                  "stairUp",
                  "stairDown",
                  "standToSit",
                  "sit",
                  "sitToStand",
                  "revolving door",
                  "normal",
                  "fast",
                  "slow")

##the color for each activity to be used in plots
activityCols <- c("green2",
                  "black",
                  "red",
                  "blue",
                  "orange",
                  "yellow",
                  "blueviolet",
                  "deeppink",
                  "green2",
                  "green3",
                  "green4")

##Subject colors
subject_cols <- list()
subject_cols[["subject1"]] <- "#ca0020"
subject_cols[["subject2"]] <- "#f4a582"
subject_cols[["subject3"]] <- "#92c5de"
subject_cols[["subject4"]] <- "#0571b0"

######################################################################################
##Functions##
######################################################################################

##Function that creates two new columns in the data frame with predicted and true labels
##M = whether the predicted label matches the true label
##prop_predicted = the proportion of votes (among the neighbor movelets) for the predicted label
##It assumes the only activities in the dictionary are "stand", "sit", "walk", "standToSit", "sitToStand"
compute_M_and_voteProp <- function(result) {
  ##Indicator of whether the prediction was correct
  result$M = result$label.predict == result$label
  
  ##Proportion of votes that went to the predicted activity label
  result$prop_predicted <- NA
  for (i in 1:nrow(result)){
    if (!is.na(result$label.predict[i])){
      if (result$label.predict[i] == "stand"){
        result$prop_predicted[i] <- result$stand[i]
      } else if (result$label.predict[i] == "sit"){
        result$prop_predicted[i] <- result$sit[i]
      } else if (result$label.predict[i] == "walk"){
        result$prop_predicted[i] <- result$walk[i]
      } else if (result$label.predict[i] == "standToSit"){
        result$prop_predicted[i] <- result$standToSit[i]
      } else if (result$label.predict[i] == "sitToStand"){
        result$prop_predicted[i] <- result$sitToStand[i]
      } else if (result$label.predict[i] == "stairUp"){
        result$prop_predicted[i] <- result$stairUp[i]
      } else if (result$label.predict[i] == "stairDown"){
        result$prop_predicted[i] <- result$stairDown[i]
      } else {
      }
    }
  }
  
  return(result)
}

##Function that plots activity labels over time
##(1) The top panel is the true activity label
##(2) The middle panel is the predicted activity label
##(3) The bottom panel is the predicted activity label, after excluding predictions where the
##predicted probability that M = 1 is less than or equal to the cutoff
plotActivityPrediction_cutoff <- function(result, xRange, activityList, activityCols){
  numActivities <- length(activityList)
  
  if (is.na(xRange[1])){
    xRange <- range(result$timeElapsed)
  }
  
  par(mfrow = c(3,1))
  
  for (i in 1:numActivities){
    plot(result$timeElapsed[!is.na(result$label) & result$label == activityList[i]], 
         rep(1, sum(result$label == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "Time elapsed (minutes)", ylab = "", ylim = c(0,1),
         main = "Truth",
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)  
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  for (i in 1:numActivities){
    plot(result$timeElapsed[!is.na(result$label.predict) & result$label.predict == activityList[i]], 
         rep(1, sum(result$label.predict == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "Time elapsed (minutes)", ylab = "", ylim = c(0,1),
         main = "Prediction",
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  for (i in 1:numActivities){
    plot(result$timeElapsed[!is.na(result$label.predict.cutoff) & result$label.predict.cutoff == activityList[i]], 
         rep(1, sum(result$label.predict.cutoff == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "Time elapsed (minutes)", ylab = "", ylim = c(0,1),
         main = "Prediction (applying cutoff)",
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
}


######################################################################################
##Plot predicted labels over time for a given subject, with and without omissions##
######################################################################################

##Subject number
subject <- "subject3" ##Update with desired subject

##Training
load(paste(filepath,subject,"lap_around_quad.Rdata",sep = "/"))
training <- pred_ALL

##Test
load(paste(filepath,subject,"appleDown_faceAgainstLeg.Rdata",sep = "/"))
test <- pred_ALL

for (dataType in dataTypes){
  print(subject)
  print(dataType)
  
  ##Get training data set, compute M and proportion of votes that went to predicted label
  trainingData <- training[[dataType]]
  trainingData <- compute_M_and_voteProp(trainingData)
  
  ##Fit logistic regression model
  fit <- glm(M ~ prop_predicted, data = trainingData, family = "binomial")
  
  
  
  ##Get test data set, compute M and proportion of votes that went to predicted label
  newData <- test[[dataType]]
  
  ##For true fast, slow, and normal walking, change those all to walking
  indices <- which(newData$label %in% c("slow", "fast", "normal"))
  if (length(indices) > 0){
    newData$label[indices] <- "walk"
  }
  
  newData <- compute_M_and_voteProp(newData)
  

  
  ##Do predictions on test data using fitted model
  newData$probM1 <- predict(fit, newdata = newData, type = "response") ##probM1 is the predicted probability that M = 1
  
  M1 <- subset(newData, M == TRUE)
  M0 <- subset(newData, M == FALSE)
  
  ##Look at the predicted probability that M = 1
  par(mfrow = c(1,1))
  boxplot(M1$probM1, M0$probM1, 
          main = paste("Predicted probability that M = 1 \n", subject, dataType, sep = " "),
          names = c("M = 1", "M = 0"), ylim = c(0,1))
  
  ##Look at the proportion of votes that went to the predicted activity label
  #boxplot(M1$prop_predicted, M0$prop_predicted, 
  #        main = paste("Proportion of Votes for Predicted Label \n", subject, dataType, sep = " "),
  #        names = c("M = 1", "M = 0"), ylim = c(0,1))
  
  ##Exclude (i.e., make NA) the predicted labels that have probM1 lower than the cutoff 
  newData_exc <- newData %>% mutate(label.predict.cutoff = ifelse(probM1 > cutoff, yes = label.predict, no = NA)) %>% data.frame
  
  ##Plot the prediction results that exclude predicted labels that don't meet the cutoff
  plotActivityPrediction_cutoff(newData_exc, xRange = NA, activityList, activityCols)
}


###################################################################################################################
##Plot proportion of predicted labels that get excluded as a function of threshold
###################################################################################################################

data_type <- "gyro_front"

##This list is to store the participant-specific curves for proportion of predicted labels that are excluded 
##as a function of threshold
prop_excluded <- list()

##This list is to store the participant-specific ROC curves
roc_curves <- list()

for (subject in subjects){
  print(subject)
  
  ##Subject's training data set for all data types
  load(paste(filepath,subject,"lap_around_quad.Rdata",sep = "/"))
  training <- pred_ALL
  
  ##Subject's test data set for all data types
  load(paste(filepath,subject,"appleDown_faceAgainstLeg.Rdata",sep = "/"))
  test <- pred_ALL
  
  ##Get training data set for data_type, compute M (indicator of whether the predicted label matches the true label)
  ##and the proportion of votes that went to predicted label
  trainingData <- training[[data_type]]
  trainingData <- compute_M_and_voteProp(trainingData)
  
  ##Fit logistic regression model
  fit <- glm(M ~ prop_predicted, data = trainingData, family = "binomial")
  
  ##Get test data set for data_type, compute M and proportion of votes that went to predicted label
  newData <- test[[data_type]]
  
  ##For true fast, slow, and normal walking, change those all to walking
  indices <- which(newData$label %in% c("slow", "fast", "normal"))
  if (length(indices) > 0){
    newData$label[indices] <- "walk"
  }
  
  newData <- compute_M_and_voteProp(newData)
  
  ##Get predicted probability that predicted label is correct using fitted logistic regression model
  newData$probM1 <- predict(fit, newdata = newData, type = "response") ##probM1 is the predicted probability that M = 1
  
  ##Remove timepoints without a prediction
  newData <- subset(newData, !is.na(label.predict))
  
  ##Compute proportion of votes that get excluded as a function of the cutoff p
  vec <- rep(NA, ncutoffs)
  for (i in 1:ncutoffs){
    vec[i] <- mean(newData$probM1 <= cutoffs[i]) 
  }
  prop_excluded[[subject]] <- vec
  
  ##Compute sensitivity & 1 - specificity 
  ##sensitivity = Pr(remove|mismatch)
  ##specificity = Pr(keep|not mismatch)
  roc <- data.frame(cutoffs, sens = NA, spec = NA)
  for (i in 1:ncutoffs){
    temp <- newData
    
    ##Indicator of whether the prediction is excluded
    temp$exc <- temp$probM1 <= cutoffs[i] 
    
    ##Indicator of whether the prediction matches the truth
    temp$mismatch <- temp$label != temp$label.predict 
    
    ##Subsets with mismatch = TRUE (to compute sensitivity) and
    ##mismatch = FALSE (to compute specificity)
    mismatch_sub <- subset(temp, mismatch == TRUE)
    match_sub <- subset(temp, mismatch == FALSE)
    
    ##Compute sensitivity & specificity
    roc$sens[i] <- mean(mismatch_sub$exc == 1)
    roc$spec[i] <- mean(match_sub$exc == 0)
  }
  
  roc_curves[[subject]] <- roc
}

##Plot ROC curves
pdf(paste(figure_filepath,"/roc.pdf",sep = ""))
plot(NA, xlim = c(0,1), ylim = c(0,1), 
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     main = "Panel B",
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
for (subject in subjects){
  roc <- roc_curves[[subject]]
  lines(1-roc$spec, roc$sens, lwd = 2, col = subject_cols[[subject]])
  points(1-roc$spec[roc$cutoffs == 0.5], roc$sens[roc$cutoffs == 0.5], col = subject_cols[[subject]], cex = 1.5)
}
legend("bottomright", 
       c("Subject 1", "Subject 2", "Subject 3", "Subject 4"),
       col = as.character(unlist(subject_cols)),
       lwd = 2)
dev.off()

##Plot proportion of predicted labels that get retained as a function of threshold
pdf(paste(figure_filepath,"/prop_retained.pdf",sep = ""))
plot(NA, xlim = c(0,1), ylim = c(0,1), 
     xlab = "Threshold", ylab = "Proportion of predicted labels retained",
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
for (subject in subjects){
  vec <- prop_excluded[[subject]]
  lines(cutoffs, 1-vec, lwd = 2, col = subject_cols[[subject]])
}
legend("bottomleft", 
       c("Subject 1", "Subject 2", "Subject 3", "Subject 4"),
       col = as.character(unlist(subject_cols)),
       lwd = 2)
dev.off()

##3/10/20
##Plot proportion of predicted labels that get excluded as a function of threshold
pdf(paste(figure_filepath,"/prop_excluded.pdf",sep = ""))
plot(NA, xlim = c(0,1), ylim = c(0,1), 
     xlab = "Threshold", ylab = "Proportion of predicted labels excluded",
     main = "Panel A",
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
for (subject in subjects){
  vec <- prop_excluded[[subject]]
  lines(cutoffs, vec, lwd = 2, col = subject_cols[[subject]])
}
legend("topleft", 
       c("Subject 1", "Subject 2", "Subject 3", "Subject 4"),
       col = as.character(unlist(subject_cols)),
       lwd = 2)
dev.off()




##Make a two-panel figure with plot of ROC curves and plot of predicted labels that
##get excluded

pdf(paste(figure_filepath,"/unc_quant_results.pdf",sep = ""),
    width = 10,
    height = 6)

par(mfrow=c(1,2))


plot(NA, xlim = c(0,1), ylim = c(0,1), 
     xlab = "Threshold", ylab = "Proportion of predicted labels excluded",
     main = "(a)",
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
for (subject in subjects){
  vec <- prop_excluded[[subject]]
  lines(cutoffs, vec, lwd = 2, col = subject_cols[[subject]])
}
legend("topleft", 
       c("Subject 1", "Subject 2", "Subject 3", "Subject 4"),
       col = as.character(unlist(subject_cols)),
       lwd = 2)

plot(NA, xlim = c(0,1), ylim = c(0,1), 
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     main = "(b)",
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
for (subject in subjects){
  roc <- roc_curves[[subject]]
  lines(1-roc$spec, roc$sens, lwd = 2, col = subject_cols[[subject]])
  points(1-roc$spec[roc$cutoffs == 0.5], roc$sens[roc$cutoffs == 0.5], col = subject_cols[[subject]], cex = 1.5)
}
dev.off()


