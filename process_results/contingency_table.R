##9/24/18

##Plot contingency table of predicted activity label versus true activity label
##(used to make tables in Supplement)

rm(list=ls())

##########################################################################################
##Libraries##
##########################################################################################

library(xtable)

##########################################################################################
##Inputs##
##########################################################################################

nSubjects <- 4

data_types <- c("acc_front",
                "acc_back",
                "gyro_front",
                "gyro_back")

nDataTypes <- length(data_types)

training_activities <- c("stand",
                         "walk",
                         "stairUp",
                         "stairDown",
                         "standToSit",
                         "sit",
                         "sitToStand")

nTrainingActivities <- length(training_activities)

##########################################################################################
##Functions##
##########################################################################################

##Get distribution of predicted activity labels (We assume that pred does not have NA's 
##in the labels or predicted labels)
freq <- function(pred){
  vec <- rep(0, nTrainingActivities)
  for (k in 1:nTrainingActivities){
    vec[k] <- mean(pred$label.predict == training_activities[k])
  }
  return(vec)
}

##Process results for walking at different speeds
walking_speeds <- function(filepath,nSubjects,nTrainingActivities,nDataTypes){
  for (subject_id in 1:nSubjects){
    subject <- paste("subject", subject_id, sep = "")
    print(subject)
    
    ##Where the results will be saved
    mat <- matrix(NA, nrow = nTrainingActivities, ncol = nDataTypes * 3) ##3 comes from the 3 speeds
    
    ##Initialize at column 1 of table
    col <- 1
    
    load(paste(filepath, "/", subject, "/walking_varySpeed-slow.Rdata", sep = ""))
    
    for (data_type in data_types){
      result <- pred_ALL[[data_type]]
      result <- subset(result, !is.na(label.predict) & !is.na(label))
      mat[,col] <- freq(result)
      col <- col + 1
    }
    
    load(paste(filepath, "/", subject, "/walking_varySpeed-normal.Rdata", sep = ""))
    
    for (data_type in data_types){
      result <- pred_ALL[[data_type]]
      result <- subset(result, !is.na(label.predict) & !is.na(label))
      mat[,col] <- freq(result)
      col <- col + 1
    }
    
    load(paste(filepath, "/", subject, "/walking_varySpeed-fast.Rdata", sep = ""))
    
    for (data_type in data_types){
      result <- pred_ALL[[data_type]]
      result <- subset(result, !is.na(label.predict) & !is.na(label))
      mat[,col] <- freq(result)
      col <- col + 1
    }
    
    ##Average sensitivity for each sensor-placement 
    print(c(mean(mat[2,c(1,5,9)]), mean(mat[2,c(2,6,10)]), mean(mat[2,c(3,7,11)]), mean(mat[2,c(4,8,12)])))
    
    
    print(xtable(mat, digits = 2))
    
    
    invisible(readline(prompt="Press [enter] to continue"))
  }
}

##########################################################################################
##Test data collection (excluding different walking speeds and varying phone orientation##

##Raw triaxial data##
##########################################################################################

##UPDATE data_type
data_type <- "gyro_front"

test_activities <- c("stand",
                     "walk",
                     "stairUp",
                     "stairDown",
                     "standToSit",
                     "sit",
                     "sitToStand",
                     "revolving door")

nTestActivities <- length(test_activities)

segments <- c("going_to_building",
              "lap_around_quad",
              "stairs_in_building-upStairs",
              "stairs_in_building-downStairs",
              "sitting")

##Function for computing distribution of predicted activity labels
dist_prediction <- function(subject, data_type, filepath, segments, 
                            test_activities, nTrainingActivities, nTestActivities){
  result <- list()
  
  for (segment in segments){
    load(paste(filepath, "/", subject, "/", segment, ".Rdata", sep = ""))
    result[[segment]] <- pred_ALL[[data_type]]
  }
  
  result <- do.call(rbind, result)
  result <- subset(result, !is.na(label) & !is.na(label.predict))
  
  mat <- matrix(NA, nrow = nTrainingActivities, ncol = nTestActivities)
  
  for (i in 1:nTestActivities){
    testActivity <- test_activities[i]
    sub_data <- subset(result, label == testActivity)
    mat[,i] <- freq(sub_data)
  }
  return(mat)
}


##Do this for L2 distance metric
for (subject_id in 1:nSubjects){
  
  subject <- paste("subject", subject_id, sep = "")
  print(subject)
  
  ##First get triaxial results
  
  filepath <- "~/Documents/data/ACSGA/analysis/movelet/results/standard"
  
  mat_TRI <- dist_prediction(subject, data_type, filepath, segments, 
                                test_activities, nTrainingActivities, nTestActivities)
  
  print(mean(diag(mat_TRI))) ##average sensitivity for the 7 training activities
  
  ##Next get magnitude results
  
  filepath <- "~/Documents/data/ACSGA/analysis/movelet/results/magnitude"
  
  mat_MAG <- dist_prediction(subject, data_type, filepath, segments, 
                                test_activities, nTrainingActivities, nTestActivities)
  
  print(mean(diag(mat_MAG))) ##average sensitivity for the 7 training activities
  
  ##Get results in a single data frame (alternate between tri-axial and magnitude results)
  mat <- matrix(NA, nrow = nTrainingActivities, ncol = nTestActivities * 2)
  mat[,c(1,3,5,7,9,11,13,15)] <- mat_TRI
  mat[,c(2,4,6,8,10,12,14,16)] <- mat_MAG
  
  ##Get LaTeX table
  print(xtable(mat, digits = 2))
  
  invisible(readline(prompt="Press [enter] to continue"))
}  



##Do this for correlation distance metric
for (subject_id in 1:nSubjects){
  
  subject <- paste("subject", subject_id, sep = "")
  print(subject)
  
  ##First get triaxial results
  
  filepath <- "~/Documents/data/ACSGA/analysis/movelet/results/triaxial_correlation"
  
  mat_TRI <- dist_prediction(subject, data_type, filepath, segments, 
                                test_activities, nTrainingActivities, nTestActivities)
  
  print(mean(diag(mat_TRI))) ##average sensitivity for the 7 training activities
  
  ##Next get magnitude results
  
  filepath <- "~/Documents/data/ACSGA/analysis/movelet/results/magnitude_correlation"
  
  mat_MAG <- dist_prediction(subject, data_type, filepath, segments, 
                                test_activities, nTrainingActivities, nTestActivities)
  
  print(mean(diag(mat_MAG))) ##average sensitivity for the 7 training activities
  
  ##Get results in a single data frame (alternate between tri-axial and magnitude results)
  mat <- matrix(NA, nrow = nTrainingActivities, ncol = nTestActivities * 2)
  mat[,c(1,3,5,7,9,11,13,15)] <- mat_TRI
  mat[,c(2,4,6,8,10,12,14,16)] <- mat_MAG
  
  ##Get LaTeX table
  print(xtable(mat, digits = 2))
  
  
  invisible(readline(prompt="Press [enter] to continue"))
}  

##########################################################################################
##Walking at different speeds##
##########################################################################################

filepath <- "~/Documents/data/ACSGA/analysis/movelet/results/standard"
walking_speeds(filepath,nSubjects,nTrainingActivities,nDataTypes)

filepath <- "~/Documents/data/ACSGA/analysis/movelet/results/magnitude"
walking_speeds(filepath,nSubjects,nTrainingActivities,nDataTypes)

filepath <- "~/Documents/data/ACSGA/analysis/movelet/results/triaxial_correlation"
walking_speeds(filepath,nSubjects,nTrainingActivities,nDataTypes)

filepath <- "~/Documents/data/ACSGA/analysis/movelet/results/magnitude_correlation"
walking_speeds(filepath,nSubjects,nTrainingActivities,nDataTypes)

##########################################################################################
##Phone orientation -- raw triaxial data and magnitude##
##########################################################################################

test_activities <- c("stand",
                     "walk",
                     "stairUp",
                     "stairDown")

nTestActivities <- length(test_activities)

orientations <- c(##"appleDown_faceAgainstLeg",
                  "appleDown_faceOppositeLeg",
                  "appleUp_faceAgainstLeg",
                  "appleUp_faceOppositeLeg")

##Update data_type
data_type <- "acc_front"

process_vary_orientation <- function(filepath,subject,orientation,data_type,
                                     nTrainingActivities, nTestActivities){
  load(paste(filepath, "/", subject, "/", orientation, ".Rdata", sep = ""))
  result <- pred_ALL[[data_type]]
  result <- subset(result, !is.na(label) & !is.na(label.predict))
  
  mat <- matrix(NA, nrow = nTrainingActivities, ncol = nTestActivities)
  for (i in 1:nTestActivities){
    testActivity <- test_activities[i]
    sub_data <- subset(result, label == testActivity)
    mat[,i] <- freq(sub_data)
  }
  return(mat)
}

##UPDATE orientation
orientation <- orientations[3]

print(orientation)


for (subject_id in 1:nSubjects){
  
  subject <- paste("subject", subject_id, sep = "")
  print(subject)
  
  ##Uncomment if considering L2 distance
  #filepathTRI <- "~/Documents/data/ACSGA/analysis/movelet/results/standard"
  #filepathMAG <- "~/Documents/data/ACSGA/analysis/movelet/results/magnitude"
  
  ##Uncomment if considering correlation distance
  filepathTRI <- "~/Documents/data/ACSGA/analysis/movelet/results/triaxial_correlation"
  filepathMAG <- "~/Documents/data/ACSGA/analysis/movelet/results/magnitude_correlation"
  
  ##First get triaxial results
  mat_TRI <- process_vary_orientation(filepathTRI,subject,orientation,data_type,
                                      nTrainingActivities, nTestActivities)
  
  ##Next get magnitude results
  mat_MAG <- process_vary_orientation(filepathMAG,subject,orientation,data_type,
                                      nTrainingActivities, nTestActivities)
  
  
  print(c(mean(diag(mat_TRI)), mean(diag(mat_MAG)))) ##average sensitivity
  
  ##Get results in a single data frame (alternate between tri-axial and magnitude results)
  mat <- matrix(NA, nrow = nTrainingActivities, ncol = nTestActivities * 2)
  mat[,c(1,3,5,7)] <- mat_TRI
  mat[,c(2,4,6,8)] <- mat_MAG
  
  ##Get LaTeX table
  print(xtable(mat, digits = 2))
  
  rm(mat, mat_TRI, mat_MAG)
  
  invisible(readline(prompt="Press [enter] to continue"))
}  


