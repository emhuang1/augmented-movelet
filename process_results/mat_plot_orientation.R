##12/28/19

##Plotting summarized results (average across participants) for the test collection phase
##where we looked at the effect of changing the orientation of the front pocket phone

rm(list=ls())

##########################################################################################
##Inputs##
##########################################################################################

nSubjects <- 4

data_types <- c("acc_front",
                "gyro_front")

nDataTypes <- length(data_types)

training_activities <- c("stand",
                         "walk",
                         "stairUp",
                         "stairDown",
                         "standToSit",
                         "sit",
                         "sitToStand")

nTrainingActivities <- length(training_activities)


test_activities <- c("stand",
                     "walk",
                     "stairUp",
                     "stairDown")

nTestActivities <- length(test_activities)

orientations <- c(
  "appleDown_faceAgainstLeg",
  "appleDown_faceOppositeLeg",
  "appleUp_faceAgainstLeg",
  "appleUp_faceOppositeLeg")

##########################################################################################
##Libraries##
##########################################################################################

library(fields)
library(dplyr)

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

##########################################################################################
##Plot front pocket matrix for different orientations##
##########################################################################################

pdf(paste("~/Documents/data/ACSGA/paper/method_performance/mat_plot_orientation",".pdf",sep = ""),
    width = 10, height = 7
    )

filepaths <- c("~/Documents/data/ACSGA/analysis/movelet/results/standard",
               "~/Documents/data/ACSGA/analysis/movelet/results/magnitude",
               "~/Documents/data/ACSGA/analysis/movelet/results/triaxial_correlation",
               "~/Documents/data/ACSGA/analysis/movelet/results/magnitude_correlation")

nFilepaths <- length(filepaths)

data_level <- c("tri", "mag", "tri", "mag")

metric <- c("L2", "L2", "cor", "cor")

pos <- "front" ##We are focusing on the front pocket phone

par(mai =c(.5,.1,.5,.1), oma = c(3,5.25,1,3))

set.panel(2,2)

for (orientation in orientations){
  print(orientation)
  
  accuracy_allFilepaths <- list()
  
  for (i in 1:nFilepaths){
    filepath <- filepaths[i]
    
    ##For the given data level and metric, store average accuracy (correctly classify as walking) where the average is taken across subjects
    ##We do this separately for each unique pair of data type and walking speed
    accuracy <- data.frame(data_level = data_level[i], metric = metric[i], data_type = data_types, stand = NA, walk = NA, stairUp = NA, stairDown = NA)
    
    
    for (data_type in data_types){
      
      print(data_type)
      
      matrix_all_subjects <- matrix(NA, nrow = nSubjects, ncol = 4)
      
      for (subject_id in 1:nSubjects){
        subject <- paste("subject", subject_id, sep = "")
        print(subject)
        
        matrix_single_subject <- process_vary_orientation(filepath,subject,orientation,data_type,
                                                          nTrainingActivities, nTestActivities)
        
        
        matrix_all_subjects[subject_id,] <- diag(matrix_single_subject)
        
      }
      
      accuracy[accuracy$data_type == data_type, c("stand","walk","stairUp","stairDown")] <- colMeans(matrix_all_subjects)
      
    }
    
    accuracy_allFilepaths[[i]] <- accuracy
  }
  
  temp <- do.call(rbind, accuracy_allFilepaths)
  
  
  gyro_cor_mag <- subset(temp, metric == "cor" & data_level == "mag" & data_type == paste("gyro",pos,sep = "_")) %>% select(stand,walk,stairUp,stairDown) %>% as.numeric %>% unname
  gyro_cor_tri <- subset(temp, metric == "cor" & data_level == "tri" & data_type == paste("gyro",pos,sep = "_")) %>% select(stand,walk,stairUp,stairDown) %>% as.numeric %>% unname
  gyro_L2_mag <- subset(temp, metric == "L2" & data_level == "mag" & data_type == paste("gyro",pos,sep = "_")) %>% select(stand,walk,stairUp,stairDown) %>% as.numeric %>% unname
  gyro_L2_tri <- subset(temp, metric == "L2" & data_level == "tri" & data_type == paste("gyro",pos,sep = "_")) %>% select(stand,walk,stairUp,stairDown) %>% as.numeric %>% unname
  acc_cor_mag <- subset(temp, metric == "cor" & data_level == "mag" & data_type == paste("acc",pos,sep = "_")) %>% select(stand,walk,stairUp,stairDown) %>% as.numeric %>% unname
  acc_cor_tri <- subset(temp, metric == "cor" & data_level == "tri" & data_type == paste("acc",pos,sep = "_")) %>% select(stand,walk,stairUp,stairDown) %>% as.numeric %>% unname
  acc_L2_mag <- subset(temp, metric == "L2" & data_level == "mag" & data_type == paste("acc",pos,sep = "_")) %>% select(stand,walk,stairUp,stairDown) %>% as.numeric %>% unname
  acc_L2_tri <- subset(temp, metric == "L2" & data_level == "tri" & data_type == paste("acc",pos,sep = "_")) %>% select(stand,walk,stairUp,stairDown) %>% as.numeric %>% unname
  
  dat <- data.frame(activity = test_activities,
                          gyro_cor_mag,
                          gyro_cor_tri, 
                          gyro_L2_mag, 
                          gyro_L2_tri, 
                          acc_cor_mag, 
                          acc_cor_tri, 
                          acc_L2_mag, 
                          acc_L2_tri)
  
  mat <- as.matrix(dat[,-1])
  
  ##Title of the heatmap based on the phone orientation
  if (orientation == "appleDown_faceAgainstLeg"){
    title <- "(a)"
  } else if (orientation == "appleDown_faceOppositeLeg"){
    title <- "(b)"
  } else if (orientation == "appleUp_faceAgainstLeg"){
    title <- "(c)"
  } else {
    title <- "(d)"
  }
  
  

  
  ##Make the heatmap of the matrix (each cell is the average accuracy where the
  ##average is taken across subjects)
  image(x = 1:4, 
             y = 1:8, 
             z = mat,
             col = paste("gray",seq(99,1,by = -1),sep=""),
             zlim = c(0,1),               
             yaxt = "n",
             xaxt = "n",
             xlab = NA,
             ylab = NA,
             main = title,
             cex.main = 1.3)
  
  if (orientation == orientations[1] | orientation == orientations[3]){
    axis(2, at=1:8, labels = FALSE)
    text(x = 0.35,
         y = 1:8,
         labels = gsub("_","/",names(dat)[-1]), las = 1, 
         cex = 1.2,
         xpd = NA,
         srt = 30,
         adj = 0.96)  
  }
  
  axis(1, at=1:4, labels = FALSE)
  
  text(x = 1:4,
       y = par("usr")[3] - 0.7,
       labels = dat$activity, 
       cex = 1.2,
       xpd = NA,
       srt = 25,
       adj = 0.96)
  
  
}

par(oma=c( 3,0,3,.2))
image.plot(legend.only = TRUE, zlim = c(0,1), col = paste("gray",seq(99,1,by = -1),sep=""))


dev.off()

