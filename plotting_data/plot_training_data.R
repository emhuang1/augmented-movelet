##9/18/18

rm(list=ls())

##In this code, we plot the training data used to build the participant's dictionaries.
##We show both the tri-axial data and the magnitude data.

######################################################################################
##Filepath for saving plots##
######################################################################################

filepath_plots <- "~/Documents/data/ACSGA/paper/plotting/output"

####################################################################################
##Settings##
####################################################################################

##number of participants
nSubjects <- 4

##data types
data_types <- c("acc_front",
                "acc_back",
                "gyro_front",
                "gyro_back")

##activity names
training_activities <- c("walk",
                        "stairUp",
                        "stairDown",
                        "stand",
                        "standToSit1",
                        "sit1",
                        "sitToStand1")

##number of activities
nActivities <- length(training_activities)

##ranges for plotting raw accelerometer and gyro data
yRange_acc <- c(-2,3)
yRange_gyro <- c(-pi,pi) 

##ranges for plotting magnitude data
yRange_gyro_MAG <- c(0,2*pi)
yRange_acc_MAG <- c(0,3)

xRange <- c(0,4) ##maximum number of seconds for each activity

######################################################################################
##Load functions and libraries##
######################################################################################

setwd("~/Dropbox/Onnela_postdoc/code/functions")

source("functions_ACSGA.R")

library(ggplot2)
library(reshape)

######################################################################################
##Format data for plotting##
######################################################################################

##For each data type, get everybody's data into a single data frame

formatted_data <- list()

for (data_type in data_types){ 
  formatted_data_temp <- list()
  i <- 1
  for (subjectID in 1:nSubjects){
    ##Subject name
    subject <- paste("subject", subjectID, sep = "")
    
    ##Filepath for labelled training data
    filepath_labelledData <- paste("~/Documents/data/ACSGA/data/", subject, "/labelled_data/training", sep = "") 
    
    ##Load data
    load(paste(filepath_labelledData,"/phones/training.Rdata", sep = ""))
    
    training <- list(acc_front = acc_front,
                     acc_back = acc_back,
                     gyro_front = gyro_front,
                     gyro_back = gyro_back)
    
    data <- training[[data_type]]
    
    for (activity in training_activities){
      sub <- subset(data, label == activity)
      sub$timeElapsed <- sub$timeElapsed - min(sub$timeElapsed)
      sub$participant <- subjectID
      formatted_data_temp[[i]] <- sub
      i <- i + 1
    }
  }  
  formatted_data[[data_type]] <- do.call(rbind, formatted_data_temp)
}

##Convert the data to a long format (x, y, z, mag are no longer columns;
##x, y, z, mag are given by "axis"
##value is given by "val"

data <- lapply(formatted_data, function(x){melt(x, id=c("participant","timeElapsed","label"))})


######################################################################################
##Plot data##
######################################################################################

for (data_type in data_types){
  
  ##y label for plots, which will depend on whether we are looking at gyro or accelerometer
  if (data_type == "acc_front" | data_type == "acc_back"){
    ylab_tri <- "Acceleration (g)"
    ylab_mag <- "Magnitude (g)"
  } else {
    ylab_tri <- "Angular velocity (radians/second)"
    ylab_mag <- "Magnitude (radians/second)"
  }
  
  ##Look at data from the specific data type (e.g., acc_front)
  sub_data <- data[[data_type]]
  
  ##Remove 1 from sit, standToSit, and sitToStand
  sub_data$label[sub_data$label == "sit1"] <- "sit"
  sub_data$label[sub_data$label == "sitToStand1"] <- "sitToStand"
  sub_data$label[sub_data$label == "standToSit1"] <- "standToSit"
  
  ##Reorder the activities, right now they are in alphabetical order
  sub_data$label <- factor(sub_data$label, levels=c("walk", "stairUp", "stairDown", "stand", "standToSit", "sit", "sitToStand"))
  
  ##Separate the data into triaxial data or magnitude data
  sub_data_MAG <- subset(sub_data, variable == "mag")
  sub_data_TRI <- subset(sub_data, variable != "mag")
  
  ##Rename variable to axis in sub_data_TRI
  sub_data_TRI$Axis <- sub_data_TRI$variable
  
  ##Plot triaxial data
  sp <- ggplot(sub_data_TRI, aes(x = timeElapsed, y = value, color = Axis, group = Axis)) + geom_line() + coord_cartesian(xlim=c(0, 10))
  
  sp + facet_grid(label ~ participant) + labs(x = "Time elapsed (seconds)", y = ylab_tri) + scale_x_continuous(breaks = c(0,2,4,6,8,10)) + theme(text = element_text(size=20)) #+ theme(strip.text.y = element_text(margin = margin(.1, 0, .1, 0, "cm")))

  
  ggsave(paste(filepath_plots, "/Training_Plots_TRI_", data_type, ".pdf", sep = ""))
  
  ##Plot magnitude data
  sp <- ggplot(sub_data_MAG, aes(x = timeElapsed, y = value)) + geom_line() + coord_cartesian(xlim=c(0, 10))
  
  sp + facet_grid(label ~ participant) + labs(x = "Time elapsed (seconds)", y = ylab_mag) + scale_x_continuous(breaks = c(0,2,4,6,8,10)) + theme(text = element_text(size=20))#+ theme(strip.text.y = element_text(margin = margin(.1, 0, .1, 0, "cm")))
  
  ggsave(paste(filepath_plots, "/Training_Plots_MAG_", data_type, ".pdf", sep = ""))
}
