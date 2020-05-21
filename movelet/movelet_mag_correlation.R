##8/30/18

##Apply movelet method with magnitude data and correlation distance metric

##To use the code, update the:
##(1) subject numbers, i.e., "subject_train" (the subject whose data is used to build the dictionary) and 
##"subject" (the subject that you are doing activity predictions for)
##(2) filepath for plots, i.e., "filepath_plots"

rm(list=ls())

####################################################################################
##Inputs##
####################################################################################

##Training subject number
subject_train <- "subject4"

##Test subject number
subject <- "subject4"

##Whether to use magnitude data, or triaxial data
useMag <- TRUE

##Interpolation
interpolation <- FALSE
interpolation_type <- NA

##number of samples per second in raw data or upsampled/downsampled data (if doing interpolation)
frequency <- 10

##Filtering out the thin lines
filtering <- TRUE ##on = TRUE, off = FALSE

##Filepath for plots
filepath_plots <- paste("~/Documents/data/ACSGA/analysis/movelet/results/magnitude_correlation/", subject, sep = "")

####################################################################################
##Filepaths##
####################################################################################

##Filepath for labelled phone training data
filepath_labelledTrainingData <- paste("~/Documents/data/ACSGA/data/", subject_train, "/labelled_data/training/phones/training.Rdata", sep = "") 

##Filepath for labelled phone test data
filepath_labelledTestData <- paste("~/Documents/data/ACSGA/data/", subject, "/labelled_data/test/phones", sep = "") 

###############################################################################
##Load functions##
###############################################################################

setwd("~/Dropbox/Onnela_postdoc/code/functions")

source("functions_ACSGA.R")

###############################################################################
##Settings##
###############################################################################

options(digits = 12)


##length of movelet in seconds
moveletLength <- 1
##measure of distance for movelet method
distOption <- "correlation" 

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

##the color for each activity
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

##the number of seconds of training data to be used (for non-instantaneous activities)
trainingData_length <- 4

##for short activities, just use all the data you have

##activities in order of the ones that we would not want to miss to the ones
##that we don't mind missing
trainingActivities <- c("walk",        
                        "stairUp",    
                        "stairDown", 
                        "stand",
                        "sit1",
                        "sit2",   
                        "standToSit1",
                        "standToSit2", 
                        "sitToStand1",    
                        "sitToStand2")

##activities with redundancies collapsed, in order of the ones that we would not want
##to miss to the ones that we don't mind missing
trainingActivities_collapsed <- c("walk",
                                  "stairUp",
                                  "stairDown",
                                  "stand",
                                  "sit",
                                  "standToSit",
                                  "sitToStand")

###############################################################################
##Load training data##
###############################################################################

##Load phone training data
load(filepath_labelledTrainingData)

###############################################################################
##For non-instantaneous activities, keep only the middle portion (trainingData_length seconds)
##of training data##

##These activities include stand, walk, sit, stairUp, stairDown
###############################################################################

##Vector of activities that are non-instantaneous
activities.long <- c("stand", "walk", "stairUp", "stairDown", "sit1", "sit2")

##For these activities, keep only the middle trainingData_length seconds
for (activity in activities.long){
  ##Time range for the activity
  timeRange <- range(c(acc_back$timeElapsed[acc_back$label == activity],
                       acc_front$timeElapsed[acc_front$label == activity],
                       gyro_front$timeElapsed[gyro_front$label == activity],
                       gyro_back$timeElapsed[gyro_back$label == activity]))
  
  ##Middle trainingData_length seconds of that range
  diff <- (timeRange[2] - timeRange[1] - trainingData_length)/2
  midRange <- timeRange + c(diff, -diff)
  
  ##Remove rows where timeElapsed is outside the range
  index <- which(acc_back$label == activity & (acc_back$timeElapsed < midRange[1] | acc_back$timeElapsed > midRange[2]))
  acc_back <- acc_back[-index,]
  index <- which(gyro_back$label == activity & (gyro_back$timeElapsed < midRange[1] | gyro_back$timeElapsed > midRange[2]))
  gyro_back <- gyro_back[-index,]
  index <- which(acc_front$label == activity & (acc_front$timeElapsed < midRange[1] | acc_front$timeElapsed > midRange[2]))
  acc_front <- acc_front[-index,]
  index <- which(gyro_front$label == activity & (gyro_front$timeElapsed < midRange[1] | gyro_front$timeElapsed > midRange[2]))
  gyro_front <- gyro_front[-index,]
}

rm(activities.long, index, midRange)

##For now, we remove the "turn a corner" data
acc_front <- subset(acc_front, label != "turn a corner")
acc_back <- subset(acc_back, label != "turn a corner")
gyro_front <- subset(gyro_front, label != "turn a corner")
gyro_back <- subset(gyro_back, label != "turn a corner")

##Put training data for the various devices into one list
training <- list(acc_front = acc_front,
                 acc_back = acc_back, 
                 gyro_front = gyro_front,
                 gyro_back = gyro_back)

rm(acc_front, acc_back, gyro_front, gyro_back)


###############################################################################
##Apply movelet method to test data##
###############################################################################

##Set working directory to where the plots should be saved
setwd(filepath_plots)

########################################################
##Lap around quad##
########################################################

##Name of session 
session <- "lap_around_quad"

##Load data from session
load(paste(filepath_labelledTestData, "/", session, ".Rdata", sep = ""))

##Put test data from different sensors in a single list
test <- list(acc_front = acc_front,
             acc_back = acc_back,
             gyro_front = gyro_front,
             gyro_back = gyro_back)

##Remove rows for which label is NA
test <- lapply(test, function(x){x[!is.na(x$label),]})

##Apply movelet method and save plots

pdf(paste(session,".pdf", sep = ""))

applyMoveletMethod(test, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols, 
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()


########################################################
##Sitting##
########################################################

##Name of session
session <- "sitting"

##Load data from session
load(paste(filepath_labelledTestData, "/", session, ".Rdata", sep = ""))

##Put test data from different sensors in a single list
test <- list(acc_front = acc_front,
             acc_back = acc_back,
             gyro_front = gyro_front,
             gyro_back = gyro_back)

##Remove rows for which label is NA
test <- lapply(test, function(x){x[!is.na(x$label),]})

##Apply movelet method and save plots

pdf(paste(session,".pdf", sep = ""))

applyMoveletMethod(test, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()

########################################################
##Walking at different speeds##
########################################################

##Name of session
session <- "walking_varySpeed"

##Load data from session
load(paste(filepath_labelledTestData, "/", session, ".Rdata", sep = ""))

##Put test data from different sensors in a single list
test <- list(acc_front = acc_front,
             acc_back = acc_back,
             gyro_front = gyro_front,
             gyro_back = gyro_back)

##Remove rows for which label is NA
test <- lapply(test, function(x){x[!is.na(x$label),]})

##Separate to normal, fast, slow
normal <- lapply(test, function(x){x[x$label == "normal",]})
fast <- lapply(test, function(x){x[x$label == "fast",]})
slow <- lapply(test, function(x){x[x$label == "slow",]})


##Apply movelet method and save plots


session <- "walking_varySpeed-normal"

pdf(paste(session, ".pdf", sep = ""))

applyMoveletMethod(normal, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()


session <- "walking_varySpeed-slow"

pdf(paste(session, ".pdf", sep = ""))

applyMoveletMethod(slow, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()


session <- "walking_varySpeed-fast"

pdf(paste(session, ".pdf", sep = ""))

applyMoveletMethod(fast, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()


########################################################
##Lap around quad (apple down, face against leg)##
########################################################

##Name of session
session <- "appleDown_faceAgainstLeg"

##Load data from session
load(paste(filepath_labelledTestData, "/", session, ".Rdata", sep = ""))

##Put test data from different sensors in a single list
test <- list(acc_front = acc_front,
             acc_back = acc_back,
             gyro_front = gyro_front,
             gyro_back = gyro_back)

##Remove rows for which label is NA
test <- lapply(test, function(x){x[!is.na(x$label),]})

##Apply movelet method and save plots

pdf(paste(session,".pdf", sep = ""))

applyMoveletMethod(test, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()

pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()


########################################################
##Lap around quad (apple down, face opposite leg)##
########################################################

##Name of session
session <- "appleDown_faceOppositeLeg"

##Load data from session
load(paste(filepath_labelledTestData, "/", session, ".Rdata", sep = ""))

##Put test data from different sensors in a single list
test <- list(acc_front = acc_front,
             acc_back = acc_back,
             gyro_front = gyro_front,
             gyro_back = gyro_back)

##Remove rows for which label is NA
test <- lapply(test, function(x){x[!is.na(x$label),]})

##Apply movelet method and save plots

pdf(paste(session,".pdf", sep = ""))

applyMoveletMethod(test, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()


########################################################
##Lap around quad (apple up, face against leg)##
########################################################

##Name of session
session <- "appleUp_faceAgainstLeg"

##Load data from session
load(paste(filepath_labelledTestData, "/", session, ".Rdata", sep = ""))

##Put test data from different sensors in a single list
test <- list(acc_front = acc_front,
             acc_back = acc_back,
             gyro_front = gyro_front,
             gyro_back = gyro_back)

##Remove rows for which label is NA
test <- lapply(test, function(x){x[!is.na(x$label),]})

##Apply movelet method and save plots

pdf(paste(session,".pdf", sep = ""))

applyMoveletMethod(test, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()


########################################################
##Lap around quad (apple up, face opposite leg)##
########################################################

##Name of session
session <- "appleUp_faceOppositeLeg"

##Load data from session
load(paste(filepath_labelledTestData, "/", session, ".Rdata", sep = ""))

##Put test data from different sensors in a single list
test <- list(acc_front = acc_front,
             acc_back = acc_back,
             gyro_front = gyro_front,
             gyro_back = gyro_back)

##Remove rows for which label is NA
test <- lapply(test, function(x){x[!is.na(x$label),]})

##Apply movelet method and save plots

pdf(paste(session,".pdf", sep = ""))

applyMoveletMethod(test, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()

pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()


########################################################
##Going to building##
########################################################

##Name of session
session <- "going_to_building"

##Load data from session
load(paste(filepath_labelledTestData, "/", session, ".Rdata", sep = ""))

##Put test data from different sensors in a single list
test <- list(acc_front = acc_front,
             acc_back = acc_back,
             gyro_front = gyro_front,
             gyro_back = gyro_back)

##Remove rows for which label is NA
test <- lapply(test, function(x){x[!is.na(x$label),]})

##Apply movelet method and save plots

pdf(paste(session,".pdf", sep = ""))

applyMoveletMethod(test, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()


########################################################
##Stairs in building##
########################################################

##Name of session
session <- "stairs_in_building"

##Load data from session
load(paste(filepath_labelledTestData, "/", session, ".Rdata", sep = ""))

##Put test data from different sensors in a single list
test <- list(acc_front = acc_front,
             acc_back = acc_back,
             gyro_front = gyro_front,
             gyro_back = gyro_back)

##Remove rows for which label is NA
test <- lapply(test, function(x){x[!is.na(x$label),]})

##Separate to stair up vs down
upStairs <- lapply(test, function(x){x[x$label == "stairUp",]})
downStairs <- lapply(test, function(x){x[x$label == "stairDown",]})


##Apply movelet method and save plots

session <- "stairs_in_building-upStairs"

pdf(paste(session, ".pdf", sep = ""))

applyMoveletMethod(upStairs, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()




session <- "stairs_in_building-downStairs"

pdf(paste(session, ".pdf", sep = ""))

applyMoveletMethod(downStairs, training, 
                   frequency, moveletLength, distOption, 
                   trainingActivities, trainingActivities_collapsed,
                   activityList, activityCols,
                   useMag = useMag, filtering = filtering,
                   interpolation, interpolation_type)
dev.off()


pdf(paste(session,"_majority_vote.pdf", sep = ""))

load(paste(session,".Rdata", sep = ""))
plotVoteProportions(pred_ALL, xRange = NA, activityList, activityCols)

dev.off()

