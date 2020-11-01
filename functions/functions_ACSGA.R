##6/23/18##
##########################################################################################
##Plot triaxial data##
##########################################################################################

##INPUTS##

##data: the data frame that has a time elapsed column
##xcol, ycol, zcol: the desired colors for the axes
##line_type: the type of line used for the plot (e.g., "o", "l")
##xRange: the range for the x-axis
##yRange: the range for the y-axis
##ylab: the y-axis label
##xlab: the x-axis label
##plot_title: the plot title
##timepoints: timepoints where vertical lines should be drawn

plotRawTriData <- function(data, 
                           xRange = NULL, yRange,
                           xcol = "red", ycol = "blue", zcol = "green", 
                           line_type = "l", 
                           plot_title = NA,
                           xlab = "Time elapsed (seconds)",
                           ylab = NA, 
                           timepoints = NA){
  
  if (is.null(xRange)){
    xRange <- range(data$timeElapsed)
  }
  
  plot(data$timeElapsed, data$x,
       xlim = xRange,
       ylim = yRange,
       type = line_type,
       main = plot_title,
       xlab = xlab,
       ylab = ylab,
       col = xcol)
  par(new = TRUE)
  plot(data$timeElapsed, data$y,
       xlim = xRange,
       ylim = yRange,
       type = line_type,
       xlab = NA,
       ylab = NA,
       col = ycol)
  par(new = TRUE)
  plot(data$timeElapsed, data$z,
       xlim = xRange,
       ylim = yRange,
       type = line_type,
       xlab = NA,
       ylab = NA,
       col = zcol)
  
  abline(v = timepoints, col = "black", lty = 2)
}


##8/24/18##
##########################################################################################
##Plot magnitude data##
##########################################################################################

##INPUTS##

##data: the data frame that has a time elapsed, x, y, z columns
##line_type: the type of line used for the plot (e.g., "o", "l")
##xRange: the range for the x-axis
##yRange: the range for the y-axis
##ylab: the y-axis label
##xlab: the x-axis label
##plot_title: the plot title
##timepoints: timepoints where vertical lines should be drawn

plotMagData <- function(data, 
                        xRange = NULL, yRange,
                        line_type = "l", 
                        plot_title = NA,
                        xlab = "Time elapsed (seconds)",
                        ylab = NA, 
                        timepoints = NA){
  
  if (is.null(xRange)){
    xRange <- range(data$timeElapsed)
  }
  
  data$mag <- sqrt(data$x^2 + data$y^2 + data$z^2)
  
  plot(data$timeElapsed, data$mag,
       xlim = xRange,
       ylim = yRange,
       type = line_type,
       main = plot_title,
       xlab = xlab,
       ylab = ylab)
  
  abline(v = timepoints, col = "black", lty = 2)
}




##7/6/18
###################################################################################################################
##Apply the movelet method and plot the results
##Data is assumed to be coming in at a constant frequency with no off-periods
###################################################################################################################

##INPUT##

##test: a list with the test data
##training: a list with the training data (names should be the same as in test)
##frequency: frequency of data collection, in samples per second
##moveletLength: length of movelet, in seconds
##distOption: distance metric ("L2" or "correlation")
##trainingActivities: vector of activities in training data, in order of the one you wouldn't want to miss the most to the one you don't mind missing
##activityList: vector of all activities in either training or test data
##activityCols: color for each activity in activityList
##interpolation: whether interpolation is desired
##interpolation_type: the type of interpolation (e.g., cubic spline)

##OUTPUT##

##Pictures of the true sequence of activities versus the predicted sequence of activities

applyMoveletMethod <- function(test, training, 
                               frequency, moveletLength, distOption, 
                               trainingActivities, trainingActivities_collapsed, activityList, activityCols,
                               useMag = FALSE, filtering = FALSE, interpolation = FALSE,
                               interpolation_type = NA){
  
  
  ##Names of data types
  dataTypes <- names(training)
  
  ##Get first timestamp in the test data (will be used as the origin when computing time elapsed)
  firstTime <- min(unlist(lapply(test, function(x){min(x$timestamp)})))
  
  ##This list will be used to save the predictions
  pred_ALL <- list()
  
  ##This list will be used to save each movelet's minimum discrepancy from the dictionary activity entries 
  mat.minDiscrep_ALL <- list()
  
  ##Apply movelet method separately to each sensor (acc_front, acc_back, gyro_front, gyro_back)
  for (i in 1:length(dataTypes)){
    
    test_dataset <- test[[dataTypes[i]]]
    training_dataset <- training[[dataTypes[i]]]
    
    ##Perform interpolation, if desired
    if (interpolation){
      if (interpolation_type == "cubic spline"){
        ##Test data set
        timeElapsed <- seq(from = min(test_dataset$timeElapsed), to = max(test_dataset$timeElapsed), by = 1/frequency)
        mag <- spline(test_dataset$timeElapsed, test_dataset$mag, xout = timeElapsed)$y
        x <- spline(test_dataset$timeElapsed, test_dataset$x, xout = timeElapsed)$y
        y <- spline(test_dataset$timeElapsed, test_dataset$y, xout = timeElapsed)$y
        z <- spline(test_dataset$timeElapsed, test_dataset$z, xout = timeElapsed)$y
        timestamp <- approx(x = test_dataset$timeElapsed, y = test_dataset$timestamp, xout = timeElapsed, method="linear")$y
        
        activityChange <- rep(0, nrow(test_dataset))
        
        for (r in 1:nrow(test_dataset)){
          if (r != 1){
            if (test_dataset$label[r] != test_dataset$label[r-1]){
              activityChange[r] <- 1
            }
          } else {
            activityChange[r] <- 1
          }
        }
        
        actRecord <- data.frame(timeElapsed = test_dataset$timeElapsed, label = test_dataset$label)
        actRecord <- actRecord[activityChange == 1,]
        actRecord$label <- as.character(actRecord$label)
        
        test_dataset <- data.frame(timestamp, timeElapsed,x,y,z,mag)
        
        test_dataset$label <- NA
        
        for (j in 1:nrow(actRecord)){
          test_dataset$label[test_dataset$timeElapsed >= actRecord$timeElapsed[j]] <- actRecord$label[j]
        }
        
        
        ##Training data set
        
        data_list <- list()
        
        for (activity in trainingActivities){
          
          data_sub <- subset(training_dataset, label == activity)
          
          timeElapsed <- seq(from = min(data_sub$timeElapsed), to = max(data_sub$timeElapsed), by = 1/frequency)
          mag <- spline(data_sub$timeElapsed, data_sub$mag, xout = timeElapsed)$y
          x <- spline(data_sub$timeElapsed, data_sub$x, xout = timeElapsed)$y
          y <- spline(data_sub$timeElapsed, data_sub$y, xout = timeElapsed)$y
          z <- spline(data_sub$timeElapsed, data_sub$z, xout = timeElapsed)$y
          
          data_sub <- data.frame(timeElapsed, mag, x, y, z, label = activity)
          
          data_list[[activity]] <- data_sub
        }
        
        training_dataset <- do.call(rbind, data_list)
        
      } else {
        print("Interpolation type not supported")
      } 
    }
    
    ##Apply movelet method
    result <- movelet_Bai2012_singleSensor_general(test_dataset, 
                                                   training_dataset,
                                                   frequency, 
                                                   moveletLength, 
                                                   distOption, 
                                                   trainingActivities,
                                                   trainingActivities_collapsed,
                                                   useMag)
    
    ##Get tallies
    tally <- result[["tally"]]
    tally <- data.frame(tally)
    names(tally) <- trainingActivities_collapsed
    
    ##Get mat.minDiscrep
    mat.minDiscrep <- result[["mat.minDiscrep"]]
    mat.minDiscrep <- data.frame(mat.minDiscrep)
    names(mat.minDiscrep) <- trainingActivities
    
    ##Get predictions
    pred <- result[["data"]]
    
    ##Compute time elapsed (minutes) since firstTime
    pred$timeElapsed <- (pred$timestamp-firstTime)/1000/60
    
    ##Don't differentiate between sit1 vs sit2, sitToStand1 vs sitToStand2, standToSit1 vs standToSit2
    ##Moved this to be before the majority vote on 4/20/19
    # pred$label.predict[pred$label.predict == "sit1" | pred$label.predict == "sit2"] <- "sit"
    # pred$label.predict[pred$label.predict == "sitToStand1" | pred$label.predict == "sitToStand2"] <- "sitToStand"
    # pred$label.predict[pred$label.predict == "standToSit1" | pred$label.predict == "standToSit2"] <- "standToSit"
    
    ##Filter out thin lines (for a given timepoint, if the predicted activities at the adjacent timepoints match one
    ##another but are different from the predicted activity at the timepoint, update the predicted activity
    ##to be the same as the neighbors')
    if (filtering){
      
      npred <- sum(!is.na(pred$label.predict))
      
      for (j in 2:(npred-1)){
        ##Double check that the timepoints are close (within a second) to one another
        timeDiff <- pred$timeElapsed[j + 1] - pred$timeElapsed[j - 1]
        
        if (timeDiff <= 1/60){
          a <- pred$label.predict[j - 1]
          b <- pred$label.predict[j]
          c <- pred$label.predict[j + 1]
          
          if (a == c & b != c){
            pred$label.predict[j] <- c
          }  
        }
      }
      
    }
    
    pred_ALL[[dataTypes[i]]] <- cbind(pred[,c("timeElapsed", "label.predict", "label")],tally)
    mat.minDiscrep_ALL[[dataTypes[i]]] <- mat.minDiscrep
    
    ##Plot prediction compared to truth
    plotActivityPrediction_general(pred, xRange = NA, activityList, activityCols)
  }
  
  ##Save the predicted activity labels compared to the true activity labels
  save(pred_ALL, mat.minDiscrep_ALL, file = paste(session,".Rdata", sep = ""))
  
}



##7/6/18
###################################################################################################################
##This is a supporting function to applyMoveletMethod().

##In this function, the movelet method is implemented with the selected distance metric (L2
##or correlation) and data type (triaxial or magnitude).

###################################################################################################################

##Data is assumed to be continuous (i.e., no off-periods)

##INPUT##

##A test data frame with the following columns:  
##timestamp, 
##x-axis, y-axis, z-axis, (axes could be from gyroscope or accelerometer)
##labelled activity

##A training data frame with the following columns:  
##x-axis, y-axis, z-axis, (axes could be from gyroscope or accelerometer)
##labelled activity

##Frequency of data collection, in samples per second

##Length of movelet, in seconds

##Specify the distance metric as "L2" or "correlation" in distOption

##A vector of the activities in the dictionary, in order from the one you care
##about the most to the one you care about the least

##useMag is TRUE (if using magnitude) or FALSE (if using triaxial data)

##trainingActivites_collapsed
##vector of the training activities with redundancies removed
##e.g., "sit1" and "sit2" collapsed to "sit"

##OUTPUT##

##The test data frame with two new columns:
##predicted activity at timestamp
##predicted activity for movelet beginning at the timestamp

##A separate matrix that has, for each timestamp,
##the minimum discrepancy of new movelet to training movelet for each activity

movelet_Bai2012_singleSensor_general <- function(data, training, frequency, moveletLength, distOption, trainingActivities, trainingActivities_collapsed, useMag){
  
  moveletPoints <- frequency * moveletLength ##number of points in a movelet
  
  data.length <- nrow(data) ##number of points in the whole data set
  
  timeDiff <- data$timeElapsed[2:data.length] - data$timeElapsed[1:(data.length - 1)]
  ##the time difference between adjacent points in seconds
  
  print("Separation between data points in seconds varies from")
  print(sort(unique(timeDiff)))
  
  print(paste("Separation should be", 1/frequency, "seconds"))
  
  ##############################################################################
  ##Discrepancy between new movelet with dictionary movelets##
  ##############################################################################
  ##the activity predicted for the movelet
  data$movelet.label <- NA 
  
  numActivities <- length(trainingActivities)
  
  ##mat will be the matrix storing the minimum discrepancy of new movelet to training movelets
  ##the first column corresponds to first activity, etc.
  mat.minDiscrep <- matrix(NA, nrow = data.length, ncol = numActivities)
  
  for (i in 1:(data.length-moveletPoints+1)){
    
    ##The movelet beginning at the i-th timestamp in data
    
    newMovelet <- data[i:(i+moveletPoints-1),]
    
    ##Compute the minimum discrepancy between the new unlabelled movelet
    ##and the movelets for each activity in the dictionary
    
    for (c in 1:numActivities){
      mat.minDiscrep[i,c] <- minDiscrep(newMovelet, 
                                        subset(training, label == trainingActivities[c]), 
                                        moveletPoints, 
                                        distOption,
                                        useMag)  
    }
    
    
    ##The discrep vector gives the minimum discrepancy for each activity
    discrep <- mat.minDiscrep[i,]
    
    ##The activity (or activities) that attain the minimum discrepancy
    
    if (distOption == "L2") {
      winner <- which(discrep == min(discrep)) ##minimize L2 distance
    } else {
      winner <- which(discrep == max(discrep)) ##maximize correlation
    }
    
    
    ##Notification if there is a tie
    
    if(length(winner) > 1){
      print(paste("Ties for movelet", i))
    }
    
    ##Predicted activity for movelet beginning at i-th timestamp
    ##If there was a tie, choose the activity that was earliest in the
    ##activities vector.
    
    data$movelet.label[i] <- trainingActivities[min(winner)]
  }
  
  ##Merge "sit1" and "sit2" to "sit"
  data$movelet.label[data$movelet.label == "sit1" | data$movelet.label == "sit2"] <- "sit"
  
  ##Merge "sitToStand1" and "sitToStand2" to "sitToStand"
  data$movelet.label[data$movelet.label == "sitToStand1" | data$movelet.label == "sitToStand2"] <- "sitToStand"
  
  ##Merge "standToSit1" and "standToSit2" to "standToSit"
  data$movelet.label[data$movelet.label == "standToSit1" | data$movelet.label == "standToSit2"] <- "standToSit"
  
  ##############################################################################
  ##Predict activity for each timestamp##
  ##############################################################################
  data$label.predict <- NA
  
  numActivities_coll <- length(trainingActivities_collapsed)
  
  ##Count the number of votes for each activity  
  tally <- matrix(NA, nrow = data.length, ncol = numActivities_coll)
  
  for (i in 1:(data.length-moveletPoints)){
    
    ##Predictions for the movelet at timestamp i up to the movelet at 
    ##timestamp i + moveletPoints 
    moveletPredictions <- data$movelet.label[i:(i + moveletPoints)]
    
    for (j in 1:numActivities_coll){
      tally[i,j] <- sum(moveletPredictions == trainingActivities_collapsed[j])
    }
    
    ##Choose the activity with the most votes
    ##If there is a tie, choose the activity that occurs earlier in trainingActivities
    ##vector.
    
    winner <- which(tally[i,] == max(tally[i,]))
    
    data$label.predict[i] <- trainingActivities_collapsed[min(winner)]
  }
  
  tally <- tally/(moveletPoints + 1)
  
  return(list(data = data, mat.minDiscrep = mat.minDiscrep, tally = tally))
}


##7/6/18
###################################################################################################################
##This is a supporting function to the function movelet_Bai2012_singleSensor_general().

##In this function, depending on the distance metric, we find either the minimum L2 distance 
##between a new movelet and the dictionary movelets or the maximum correlation distance 
##between a new movelet and the dictionary movelets.

###################################################################################################################

minDiscrep <- function(newMovelet, activity.training, moveletPoints, distOption, useMag){
  if (nrow(activity.training) >= moveletPoints){
    activity.numMovelets <- nrow(activity.training) - moveletPoints + 1
    activity.discrep <- rep(NA, activity.numMovelets)
    if (distOption == "L2"){
      for (j in 1:activity.numMovelets){
        Movelet <- activity.training[j:(j+moveletPoints-1),]
        if (useMag == 0){
          x.discrep <- sqrt(sum((newMovelet$x - Movelet$x)^2))
          y.discrep <- sqrt(sum((newMovelet$y - Movelet$y)^2))
          z.discrep <- sqrt(sum((newMovelet$z - Movelet$z)^2))
          activity.discrep[j] <- mean(c(x.discrep, y.discrep, z.discrep))  
        } else {
          activity.discrep[j] <- sqrt(sum((newMovelet$mag - Movelet$mag)^2))
        }
      }
      metric <- min(activity.discrep) ##minimum L2 distance
    } else if (distOption == "correlation"){
      for (j in 1:activity.numMovelets){
        if (useMag == 0){
          Movelet <- activity.training[j:(j+moveletPoints-1),]
          x.discrep <- cor(newMovelet$x, Movelet$x)
          y.discrep <- cor(newMovelet$y, Movelet$y)
          z.discrep <- cor(newMovelet$z, Movelet$z)
          activity.discrep[j] <- mean(c(x.discrep, y.discrep, z.discrep))  
        } else {
          Movelet <- activity.training[j:(j+moveletPoints-1),]
          activity.discrep[j] <- cor(newMovelet$mag, Movelet$mag)
        }
      }
      metric <- max(activity.discrep) ##maximum correlation
    } else {
      stop("Error: unsupported distance metric")
    }  
  } else {##The training activity is shorter than the movelet length, compare training window (length n points)
          ##with first n points of new movelet
    n <- nrow(activity.training)
    Movelet <- activity.training
    if (distOption == "L2"){
      if (useMag == 0){
        x.discrep <- sqrt(sum((newMovelet$x[1:n] - Movelet$x)^2))
        y.discrep <- sqrt(sum((newMovelet$y[1:n] - Movelet$y)^2))
        z.discrep <- sqrt(sum((newMovelet$z[1:n] - Movelet$z)^2))
        activity.discrep <- mean(c(x.discrep, y.discrep, z.discrep))  
      } else {
        activity.discrep <- sqrt(sum((newMovelet$mag[1:n] - Movelet$mag)^2))
      }
      metric <- activity.discrep ##minimum L2 distance
    } else if (distOption == "correlation"){
      if (useMag == 0){
        x.discrep <- cor(newMovelet$x[1:n], Movelet$x)
        y.discrep <- cor(newMovelet$y[1:n], Movelet$y)
        z.discrep <- cor(newMovelet$z[1:n], Movelet$z)
        activity.discrep <- mean(c(x.discrep, y.discrep, z.discrep))  
      } else {
        activity.discrep <- cor(newMovelet$mag[1:n], Movelet$mag)
      }
      metric <- activity.discrep ##maximum correlation
    } else {
      stop("Error: unsupported distance metric")
    }    
  }
  
  return(metric)
}



##7/6/18
##########################################################################################
##A supporting function for the function applyMoveletMethod()##

##This supporting function makes a two-panel plot, showing the true activity labels over 
##time in the top panel and the predicted activity labels over time in the bottom panel.
##########################################################################################

##INPUT##

##result is a data frame with
##label = ground truth activity label
##timeElapsed = time elapsed in minutes
##label.predict = predicted activity label

##activityList = the list of all activities

##activityCols = the colors corresponding to the activities

##xRange is a time elapsed range 
##if NA, we let that be the min and max of timeElapsed in the result data frame


##OUTPUT##

##A two-panel plot:
##Panel 1: true activity labels (label) versus time (timeElapsed)
##Panel 2: predicted activity labels (label.predict) versus time (timeElapsed)

plotActivityPrediction_general <- function(result, xRange, activityList, activityCols){
  numActivities <- length(activityList)
  
  if (is.na(xRange[1])){
    xRange <- range(result$timeElapsed)
  }
  
  par(mfrow = c(2,1))
  
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
}


##4/21/19
##########################################################################################
##This function plots, at each timepoint, the proportion of votes that went to the
##predicted activity label.
##########################################################################################

##INPUT##

##pred_ALL is a list with elements that have

##label = ground truth activity label
##timeElapsed = time elapsed in minutes
##label.predict = predicted activity label
##walk = proportion of votes that went to "walk"
##stairUp = proportion of votes that went to "stairUp"
##stairDown = proportion of votes that went to "stairDown"
##stand = proportion of votes that went to "stand"
##sit = proportion of votes that went to "sit"
##standToSit = proportion of votes that went to "standToSit"
##sitToStand = proportion of votes that went to "sitToStand"

##activityList = the list of all activities

##activityCols = the colors corresponding to the activities

##xRange is a time elapsed range 
##if NA, we let that be the min and max of timeElapsed in the result data frame


##OUTPUT##

##A plot:
##Top row: true activity labels (label) versus time (timeElapsed)
##Bottom row: predicted activity labels (label.predict) versus time (timeElapsed)
##In the bottom row

plotVoteProportions <- function(pred_ALL, xRange, activityList, activityCols){
  
  ##Names of data types
  dataTypes <- names(pred_ALL)
  
  ##Number of activities
  numActivities <- length(activityList)
  
  for (dataType in dataTypes){
    result <- pred_ALL[[dataType]]
    
    if (is.na(xRange[1])){
      xRange <- range(result$timeElapsed)
    }
    
    ##Compute the proportion of votes for the predicted activity
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
    
    ##Plot true activity
    for (i in 1:numActivities){
      plot(result$timeElapsed[!is.na(result$label) & result$label == activityList[i]], 
           rep(2, sum(result$label == activityList[i], na.rm = TRUE)), 
           col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
           xlab = "Time elapsed (minutes)", ylab = "", ylim = c(0,2),
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)  
      if (i != numActivities){
        par(new = TRUE)
      }
    }
    
    par(new = TRUE)
    
    ##Plot predicted activity
    for (i in 1:numActivities){
      plot(result$timeElapsed[!is.na(result$label.predict) & result$label.predict == activityList[i]], 
           rep(1, sum(result$label.predict == activityList[i], na.rm = TRUE)), 
           col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
           xlab = "Time elapsed (minutes)", ylab = "", ylim = c(0,2),
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      if (i != numActivities){
        par(new = TRUE)
      }
    }
    
    par(new = TRUE)
    
    ##Plot the proportion of votes for the predicted activity 
    plot(result[,"timeElapsed"], result[,"prop_predicted"], type = "l",
         col = "cyan", lwd = 1.5,
         xlab = "Time elapsed (minutes)", ylab = "", xlim = xRange, ylim = c(0,2),
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    
  }
}




