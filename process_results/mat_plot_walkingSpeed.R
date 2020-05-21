##12/28/19

##Plotting summarized results (average across participants) for the test collection phase
##where subject walked at different speeds

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


##########################################################################################
##Libraries##
##########################################################################################

library(fields)
library(dplyr)

##########################################################################################
##Functions##
##########################################################################################


##########################################################################################
##Plot front pocket matrix and back pocket matrix for different walking speeds##
##########################################################################################

filepaths <- c("~/Documents/data/ACSGA/analysis/movelet/results/standard",
               "~/Documents/data/ACSGA/analysis/movelet/results/magnitude",
               "~/Documents/data/ACSGA/analysis/movelet/results/triaxial_correlation",
               "~/Documents/data/ACSGA/analysis/movelet/results/magnitude_correlation")

nFilepaths <- length(filepaths)

data_level <- c("tri", "mag", "tri", "mag")

metric <- c("L2", "L2", "cor", "cor")

accuracy_allFilepaths <- list()

for (i in 1:nFilepaths){
  filepath <- filepaths[i]
  
  ##For the given data level and metric, store average accuracy (correctly classify as walking) where the average is taken across subjects
  ##We do this separately for each unique pair of data type and walking speed
  accuracy <- data.frame(data_level = data_level[i], metric = metric[i], data_type = data_types, slow = NA, normal = NA, fast = NA)
  
  for (data_type in data_types){
    
    print(data_type)
    
    matrix_all_subjects <- matrix(NA, nrow = nSubjects, ncol = 3)
    
    for (subject_id in 1:nSubjects){
      subject <- paste("subject", subject_id, sep = "")
      print(subject)
      
      ##Where the subject-specific results will be saved
      vec <- rep(NA, 3) ##3 comes from the 3 speeds (slow, normal, fast)
      
      ##Initialize at index 1 of vec
      index <- 1
      
      load(paste(filepath, "/", subject, "/walking_varySpeed-slow.Rdata", sep = ""))
      
      result <- pred_ALL[[data_type]]
      result <- subset(result, !is.na(label.predict) & !is.na(label))
      vec[index] <- mean(result$label.predict == "walk")
      index <- index + 1
      
      
      load(paste(filepath, "/", subject, "/walking_varySpeed-normal.Rdata", sep = ""))
      
      result <- pred_ALL[[data_type]]
      result <- subset(result, !is.na(label.predict) & !is.na(label))
      vec[index] <- mean(result$label.predict == "walk")
      index <- index + 1
      
      
      load(paste(filepath, "/", subject, "/walking_varySpeed-fast.Rdata", sep = ""))
      
      result <- pred_ALL[[data_type]]
      result <- subset(result, !is.na(label.predict) & !is.na(label))
      vec[index] <- mean(result$label.predict == "walk")
      
      matrix_all_subjects[subject_id,] <- vec 
      
    }
    
    accuracy[accuracy$data_type == data_type, c("slow","normal","fast")] <- colMeans(matrix_all_subjects)
    
  }
  
  accuracy_allFilepaths[[i]] <- accuracy
}

temp <- do.call(rbind, accuracy_allFilepaths)

##Make separate plots for front and back pockets##

titles <- c("(a)", "(b)") ##titles <- c("Front Pocket","Back Pocket")
positions <- c("front","back")
npos <- length(positions)

pdf(paste("~/Documents/data/ACSGA/paper/method_performance/mat_plot_walkingSpeed",".pdf",sep=""),
    width = 10, height = 7)

par(mai =c(.5,.1,.5,.1), oma = c(3,5.25,1,3))

set.panel(1,2)

for (i in 1:npos){
  title <- titles[i]
  pos <- positions[i]
  
  gyro_cor_mag <- subset(temp, metric == "cor" & data_level == "mag" & data_type == paste("gyro",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
  gyro_cor_tri <- subset(temp, metric == "cor" & data_level == "tri" & data_type == paste("gyro",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
  gyro_L2_mag <- subset(temp, metric == "L2" & data_level == "mag" & data_type == paste("gyro",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
  gyro_L2_tri <- subset(temp, metric == "L2" & data_level == "tri" & data_type == paste("gyro",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
  acc_cor_mag <- subset(temp, metric == "cor" & data_level == "mag" & data_type == paste("acc",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
  acc_cor_tri <- subset(temp, metric == "cor" & data_level == "tri" & data_type == paste("acc",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
  acc_L2_mag <- subset(temp, metric == "L2" & data_level == "mag" & data_type == paste("acc",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
  acc_L2_tri <- subset(temp, metric == "L2" & data_level == "tri" & data_type == paste("acc",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
  
  front_dat <- data.frame(activity = c("slow","normal","fast"),
                          gyro_cor_mag,
                          gyro_cor_tri, 
                          gyro_L2_mag, 
                          gyro_L2_tri, 
                          acc_cor_mag, 
                          acc_cor_tri, 
                          acc_L2_mag, 
                          acc_L2_tri)
  
  mat <- as.matrix(front_dat[,-1])
  
  
  
  image(x = 1:3, 
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
    text(x = 0.425,
         y = 1:8,
         labels = gsub("_","/",names(front_dat)[-1]), las = 1, 
         cex = 1.2,
         xpd = NA,
         srt = 30,
         adj = 0.99)  
  }
  

  axis(1, at=1:3, labels = FALSE)
  
  text(x = 1:3,
       y = par("usr")[3] - 0.3,
       labels = front_dat$activity, 
       cex = 1.2,
       xpd = NA,
       srt = 25,
       adj = 0.99)
  
}

par(oma=c( 0,0,0,.2))
image.plot(legend.only = TRUE, zlim = c(0,1), col = paste("gray",seq(99,1,by = -1),sep=""))

dev.off()




##OLD VERSION WHERE THE PLOTS ARE PLOTTED IN SEPARATE WINDOWS
# ##12/28/19
# 
# ##WALKING SPEEDS
# 
# rm(list=ls())
# 
# ##########################################################################################
# ##Inputs##
# ##########################################################################################
# 
# nSubjects <- 4
# 
# data_types <- c("acc_front",
#                 "acc_back",
#                 "gyro_front",
#                 "gyro_back")
# 
# nDataTypes <- length(data_types)
# 
# training_activities <- c("stand",
#                          "walk",
#                          "stairUp",
#                          "stairDown",
#                          "standToSit",
#                          "sit",
#                          "sitToStand")
# 
# nTrainingActivities <- length(training_activities)
# 
# 
# ##########################################################################################
# ##Libraries##
# ##########################################################################################
# 
# library(fields)
# library(dplyr)
# 
# ##########################################################################################
# ##Functions##
# ##########################################################################################
# 
# 
# ##########################################################################################
# ##Plot front pocket matrix and back pocket matrix for different walking speeds##
# ##########################################################################################
# 
# filepaths <- c("~/Documents/data/ACSGA/analysis/movelet/results/standard",
#                "~/Documents/data/ACSGA/analysis/movelet/results/magnitude",
#                "~/Documents/data/ACSGA/analysis/movelet/results/triaxial_correlation",
#                "~/Documents/data/ACSGA/analysis/movelet/results/magnitude_correlation")
# 
# nFilepaths <- length(filepaths)
# 
# data_level <- c("tri", "mag", "tri", "mag")
# 
# metric <- c("L2", "L2", "cor", "cor")
# 
# accuracy_allFilepaths <- list()
# 
# for (i in 1:nFilepaths){
#   filepath <- filepaths[i]
#   
#   ##For the given data level and metric, store average accuracy (correctly classify as walking) where the average is taken across subjects
#   ##We do this separately for each unique pair of data type and walking speed
#   accuracy <- data.frame(data_level = data_level[i], metric = metric[i], data_type = data_types, slow = NA, normal = NA, fast = NA)
#   
#   for (data_type in data_types){
#     
#     print(data_type)
#     
#     matrix_all_subjects <- matrix(NA, nrow = nSubjects, ncol = 3)
#     
#     for (subject_id in 1:nSubjects){
#       subject <- paste("subject", subject_id, sep = "")
#       print(subject)
#       
#       ##Where the subject-specific results will be saved
#       vec <- rep(NA, 3) ##3 comes from the 3 speeds (slow, normal, fast)
#       
#       ##Initialize at index 1 of vec
#       index <- 1
#       
#       load(paste(filepath, "/", subject, "/walking_varySpeed-slow.Rdata", sep = ""))
#       
#       result <- pred_ALL[[data_type]]
#       result <- subset(result, !is.na(label.predict) & !is.na(label))
#       vec[index] <- mean(result$label.predict == "walk")
#       index <- index + 1
#       
#       
#       load(paste(filepath, "/", subject, "/walking_varySpeed-normal.Rdata", sep = ""))
#       
#       result <- pred_ALL[[data_type]]
#       result <- subset(result, !is.na(label.predict) & !is.na(label))
#       vec[index] <- mean(result$label.predict == "walk")
#       index <- index + 1
#       
#       
#       load(paste(filepath, "/", subject, "/walking_varySpeed-fast.Rdata", sep = ""))
#       
#       result <- pred_ALL[[data_type]]
#       result <- subset(result, !is.na(label.predict) & !is.na(label))
#       vec[index] <- mean(result$label.predict == "walk")
#       
#       matrix_all_subjects[subject_id,] <- vec 
#       
#     }
#     
#     accuracy[accuracy$data_type == data_type, c("slow","normal","fast")] <- colMeans(matrix_all_subjects)
#     
#   }
#   
#   accuracy_allFilepaths[[i]] <- accuracy
# }
# 
# temp <- do.call(rbind, accuracy_allFilepaths)
# 
# ##Make separate plots for front and back pockets##
# 
# titles <- c("Front Pocket","Back Pocket")
# positions <- c("front","back")
# npos <- length(positions)
# 
# for (i in 1:npos){
#   title <- titles[i]
#   pos <- positions[i]
#   
#   gyro_cor_mag <- subset(temp, metric == "cor" & data_level == "mag" & data_type == paste("gyro",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
#   gyro_cor_tri <- subset(temp, metric == "cor" & data_level == "tri" & data_type == paste("gyro",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
#   gyro_L2_mag <- subset(temp, metric == "L2" & data_level == "mag" & data_type == paste("gyro",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
#   gyro_L2_tri <- subset(temp, metric == "L2" & data_level == "tri" & data_type == paste("gyro",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
#   acc_cor_mag <- subset(temp, metric == "cor" & data_level == "mag" & data_type == paste("acc",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
#   acc_cor_tri <- subset(temp, metric == "cor" & data_level == "tri" & data_type == paste("acc",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
#   acc_L2_mag <- subset(temp, metric == "L2" & data_level == "mag" & data_type == paste("acc",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
#   acc_L2_tri <- subset(temp, metric == "L2" & data_level == "tri" & data_type == paste("acc",pos,sep = "_")) %>% select(slow,normal,fast) %>% as.numeric %>% unname
#   
#   front_dat <- data.frame(activity = c("slow","normal","fast"),
#                           gyro_cor_mag,
#                           gyro_cor_tri, 
#                           gyro_L2_mag, 
#                           gyro_L2_tri, 
#                           acc_cor_mag, 
#                           acc_cor_tri, 
#                           acc_L2_mag, 
#                           acc_L2_tri)
#   
#   mat <- as.matrix(front_dat[,-1])
#   
#   pdf(paste("~/Documents/data/ACSGA/paper/method_performance/mat_plot_walkingSpeed_",pos,".pdf",sep=""))
#   par(mar=c(4,7,4,2)+0.1)
#   image.plot(x = 1:3, 
#              y = 1:8, 
#              z = mat,
#              col = paste("gray",seq(99,1,by = -1),sep=""),
#              zlim = c(0,1),               
#              yaxt = "n",
#              xaxt = "n",
#              xlab = NA,
#              ylab = NA,
#              main = title, 
#              cex.main = 2)
#   
#   axis(1, at=1:3, labels = front_dat$activity, cex.axis = 1.2)
#   axis(2, at=1:8, labels = gsub("_","/",names(front_dat)[-1]), las = 1, cex.axis = 1.2)
#   dev.off()
# }












