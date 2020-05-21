##12/31/19

##Plotting summarized results (average across participants) for the primary test data collection phase
##(i.e., all test data collected except changing walking speeds and changing orientation)

rm(list=ls())

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

test_activities <- c("stand",
                     "walk",
                     "stairUp",
                     "stairDown",
                     "standToSit",
                     "sit",
                     "sitToStand")

nTestActivities <- length(test_activities)

segments <- c("going_to_building",
              "lap_around_quad",
              "stairs_in_building-upStairs",
              "stairs_in_building-downStairs",
              "sitting")

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

##########################################################################################
##Plot front pocket matrix and back pocket matrix##
##########################################################################################

filepaths <- c("~/Documents/data/ACSGA/analysis/movelet/results/standard",
               "~/Documents/data/ACSGA/analysis/movelet/results/magnitude",
               "~/Documents/data/ACSGA/analysis/movelet/results/triaxial_correlation",
               "~/Documents/data/ACSGA/analysis/movelet/results/magnitude_correlation")

nFilepaths <- length(filepaths)

data_level <- c("tri", "mag", "tri", "mag")

metric <- c("L2", "L2", "cor", "cor")

accuracy_allFilepaths <- list()

for (filepath_index in 1:nFilepaths){
  filepath <- filepaths[filepath_index]
  
  ##For the given data level and metric, store average accuracy (correctly classify as walking) where the average is taken across subjects
  ##We do this separately for each unique pair of data type and walking speed
  accuracy <- data.frame(data_level = data_level[filepath_index], metric = metric[filepath_index], data_type = data_types, 
                         stand = NA, walk = NA, stairUp = NA, stairDown = NA,
                         standToSit = NA, sit = NA, sitToStand = NA)
  
  for (data_type in data_types){
    
    print(data_type)
    
    matrix_all_subjects <- matrix(NA, nrow = nSubjects, ncol = nTestActivities)
    
    for (subject_id in 1:nSubjects){
      subject <- paste("subject", subject_id, sep = "")
      print(subject)
      
      
      matrix_single_subject <- dist_prediction(subject, data_type, filepath, segments, test_activities, nTrainingActivities, nTestActivities)
      
      matrix_all_subjects[subject_id,] <- diag(matrix_single_subject)
      
    }
    
    accuracy[accuracy$data_type == data_type, test_activities] <- colMeans(matrix_all_subjects)
    
  }
  
  accuracy_allFilepaths[[filepath_index]] <- accuracy
}

temp <- do.call(rbind, accuracy_allFilepaths)

##Make separate plots for front and back pockets##

titles <- c("(a)", "(b)") ##titles <- c("Front Pocket","Back Pocket")
positions <- c("front","back")
npos <- length(positions)

pdf(paste("~/Documents/data/ACSGA/paper/method_performance/mat_plot",".pdf",sep=""),
    width = 10, height = 7)

par(mai =c(.5,.1,.5,.1), oma = c(3,5.25,1,3))

set.panel(1,2)

for (i in 1:npos){
  title <- titles[i]
  pos <- positions[i]
  
  gyro_cor_mag <- subset(temp, metric == "cor" & data_level == "mag" & data_type == paste("gyro",pos,sep = "_")) %>% select(test_activities) %>% as.numeric %>% unname
  gyro_cor_tri <- subset(temp, metric == "cor" & data_level == "tri" & data_type == paste("gyro",pos,sep = "_")) %>% select(test_activities) %>% as.numeric %>% unname
  gyro_L2_mag <- subset(temp, metric == "L2" & data_level == "mag" & data_type == paste("gyro",pos,sep = "_")) %>% select(test_activities) %>% as.numeric %>% unname
  gyro_L2_tri <- subset(temp, metric == "L2" & data_level == "tri" & data_type == paste("gyro",pos,sep = "_")) %>% select(test_activities) %>% as.numeric %>% unname
  acc_cor_mag <- subset(temp, metric == "cor" & data_level == "mag" & data_type == paste("acc",pos,sep = "_")) %>% select(test_activities) %>% as.numeric %>% unname
  acc_cor_tri <- subset(temp, metric == "cor" & data_level == "tri" & data_type == paste("acc",pos,sep = "_")) %>% select(test_activities) %>% as.numeric %>% unname
  acc_L2_mag <- subset(temp, metric == "L2" & data_level == "mag" & data_type == paste("acc",pos,sep = "_")) %>% select(test_activities) %>% as.numeric %>% unname
  acc_L2_tri <- subset(temp, metric == "L2" & data_level == "tri" & data_type == paste("acc",pos,sep = "_")) %>% select(test_activities) %>% as.numeric %>% unname
  
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
  
  
  
  image(x = 1:nTestActivities, 
        y = 1:8, 
        z = mat,
        col = paste("gray",seq(99,1,by = -1),sep=""),
        zlim = c(0,1),               
        yaxt = "n",
        xaxt = "n",
        xlab = NA,
        ylab = NA,
        main = title,
        cex.main = 2)
  
  if (i == 1){
    axis(2, at=1:8, labels = FALSE)
    text(x = 0.25,
         y = 1:8,
         labels = gsub("_","/",names(dat)[-1]), las = 1, 
         cex = 1.2,
         xpd = NA,
         srt = 30,
         adj = 0.96)  
  }
  
  axis(1, at=1:nTestActivities, labels = FALSE)
  
  text(x = 1:nTestActivities,
       y = par("usr")[3] - 0.45,
       labels = dat$activity, 
       cex = 1.2,
       xpd = NA,
       srt = 25,
       adj = 0.96)
  
  
  
}

par(oma=c( 0,0,0,.2))
image.plot(legend.only = TRUE, zlim = c(0,1), col = paste("gray",seq(99,1,by = -1),sep=""))

dev.off()



