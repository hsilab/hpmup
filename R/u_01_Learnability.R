#' CalcLearnability
#'
#' @param config
#' @param theta
#' @param cq
#' @param resil_p
#' @param resil_m
#' @param wl_p
#' @param wl_m
#' @param thre_min
#' @param thre_max
#' @param tct_cycle
#' @param mem_chunk
#' @param Att
#'
#' @return
#' @export
#'
#' @examples
CalcLearnability <- function(config, thre_min, thre_max, theta, cq, resil_p, resil_m, wl_p, wl_m, tct_cycle, mem_chunk, Att) {

  A <- CalcA(theta, Att, cq)
  Index_X <- CalcSlopeCurve(resil_p, resil_m, wl_p, wl_m, cq)
  Learnability <- FindX(A, Index_X, thre_max)

  print("1. Learnability: ")
  print(Learnability)

  ErrorRate <- CalcError(Learnability)
  print("2. Error Rate: ")
  print(ErrorRate)

  perf_expert <- 120/tct_cycle # this 120 should also be customized as an input
  Efficiency <- CalcEfficiency(ErrorRate, perf_expert)
  print("3. Efficiency: ")
  print(Efficiency)

  print("4. Memory chunk: ")
  print(mem_chunk)

  satisfaction <- CalcSatisfaction(Learnability, Efficiency, thre_min, thre_max, Att)
  print("5. Satisfaction: ")
  print(satisfaction)

  CalcedUSA <- list(Learnability, ErrorRate, Efficiency, mem_chunk, satisfaction)
  return(CalcedUSA)
}

#' FindX
#'
#' @param A
#' @param Index_X
#' @param thre_max
#'
#' @return the trial number which is lower that the thre_max (moving average of three trials)
#' @export
#'
#' @examples
FindX <- function(A, Index_X, thre_max) {

  flag <- 3
  time_each_trial <- c()
  moving_average <- c()

  # have time for each trial
  for (i in 1:30) {
    L <- A*i^Index_X
    time_each_trial[i] <- L
  }
  # get the moving average
  for (i in 1:28) {
    moving_average[i] <- mean(time_each_trial[i], time_each_trial[i+1], time_each_trial[i+2])
  }
  # get the trial number when it becomes lower than the thre max
  for (i in 1:28) {
    if(moving_average[i] > thre_max) {
      flag <- flag+1
    }
  }

  return(flag)
}

#' CalcSlopeCurve
#'
#' @param resil_p
#' @param resil_m
#' @param wl_p
#' @param wl_m
#' @param cq
#'
#' @return
#' @export
#'
#' @examples
CalcSlopeCurve <- function(resil_p, resil_m, wl_p, wl_m, cq) {

  # interpolated_slope <- -0.2
  # interpolated_interceptor <- 0.95

  # weight of the slope of the linear equation in the numerator of the index of X
  interpolated_slope <- 0
  if (resil_p > 0 & resil_m > 0) {
    interpolated_slope <- -0.2
  } else if (resil_p > 0 & resil_m < 0) {
    interpolated_slope <- -0.3
  } else if (resil_p < 0 & resil_m > 0) {
    interpolated_slope <- -0.35
  } else {
    interpolated_slope <- -0.35
  }
  print("interpolated slope: ")
  print(interpolated_slope)

  # weight of interceptor of the linear equation in the numerator of the index of X
  interpolated_interceptor <- 0
  if (wl_p > 0 & wl_m > 0) { # both physical and mental workload are extremely high
    interpolated_interceptor <- 0.92
  } else if (wl_p > 0 & wl_m < 0) { # both physical and mental workload are extremely low
    interpolated_interceptor <- 0.96
  } else if (wl_p == 0 & wl_m == 0) { # both physical and mental workload are in normal
    interpolated_interceptor <- 0.925
  } else if (wl_p > 0 & wl_m == 0) { # only physical workload is extremely high
    interpolated_interceptor <- 1
  } else if (wl_p < 0 & wl_m == 0) { # only physical workload is extremely low
    interpolated_interceptor <- 1
  } else if (wl_p == 0 & wl_m > 0) { # only mental workload is extremely high
    interpolated_interceptor <- 0.8
  } else {# wl_p == 0 & wl_m < 0 # only mental workload is extremely low
    interpolated_interceptor <- 0.93
  }
  print("interpolated interceptor: ")
  print(interpolated_interceptor)

  # index of X
  print("slope final: ")
  print(interpolated_slope * cq + interpolated_interceptor)
  index_X <- log(interpolated_slope * cq + interpolated_interceptor) / log(2)

  # print(index_X)
  return(index_X)
}


#' CalcA
#'
#' @param theta
#' @param Att
#' @param lot
#'
#' @return
#' @export
#'
#' @examples
CalcA <- function(theta, Att, cq) {

  lot <- Att

  # the t_final is just for A (different from the logic in Satisfaction)
  if (Att < 0.5) {
    lot <- 1-(0.5-Att)*2
  } else if (Att > 0.75) {

    lot <- (Att - 0.5)*2

  } else {
    lot <- Att - 0.5 + 0.65
  }

  A <- theta * lot / cq
  print("A: ")
  print(A)
  return(A)

}




###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
########################      #############################################################
######################## OLD  #############################################################
########################      #############################################################
###########################################################################################
###########################################################################################
###########################################################################################

#' GetLearnability()
#'
#' @description Calculate the number of required trials to be trained
#' @param raw_trn Training trials raw data
#' @param hook_calib_qt Case 1, 2, or 3
#' @param DC_mode The number of modes in DC
#' @param DC_gesture The number of gestures in DC
#' @param PR_mode The number of modes in PR
#' @param PR_gesture The number of gestures in PR
#' @param slope_in Slope value
#'
#' @return The number of required trials to be trained
#' @export
#'
#' @examples
#' GetLearnability(raw_trn, hook_calib_qt, DC_mode, DC_gesture, PR_mode, PR_gesture, slope)
GetLearnability <- function(raw_trn, hook_calib_qt, DC_mode, DC_gesture, PR_mode, PR_gesture, slope_in) {
  # get moving average
  movAvg <- GetMovAvg(raw_trn)
  movAvg
  # get coefficients to calculate A for a new device
  coef <- GetCoef(raw_trn)
  coef
  # get A for DC and PR
  A <- GetA(raw_trn)
  A
  # Update A in case of the new device
  if (curr_Mode == "DC") {
    A <- A[[1]]
  } else if (curr_Mode == "PR") {
    A <- A[[2]]
  } else if (curr_Mode == "CC" | curr_Mode == "Custom") {
    A <- coef[1,1] * dev_Mode + coef[2,1] * dev_Gesture
  }
  A
  # slope
  b <- slope
  b
  # run three cases of learnability
  if ( (hook_calib_qt == "good") && (learning_Skill == "good") ) {
    return(3)
  } else {
    curveData <- GetCurve(A, b)
    curveData
    return(curveData)
  }
}

#' GetMovAvg()
#'
#' @description Calculate averages of three sequential trials
#' @param raw_trn Training trials raw data
#'
#' @return averaged TCT for training trials
#' @export
#'
#' @examples
#' GetMovAvg(raw_trn)
GetMovAvg <- function (raw_trn) {

  # clear the table
  temp_trn <- raw_trn
  temp_trn
  for (i in 1:(nrow(temp_trn)-2)) {
    for (j in 1:(ncol(temp_trn)-3)) {
      temp_trn[i+2, j+3] <- 0
    }
  }
  temp_trn

  # moving average
  for (i in 1:(nrow(temp_trn)-2)) {
    for (j in 1:(ncol(temp_trn)-7)) {
      if ( is.na((raw_trn[i+2, j+3] > 0) && (raw_trn[i+2, j+4] > 0) && (raw_trn[i+2, j+5] > 0)) == FALSE )
        temp_trn[i+2, j+5] <- (raw_trn[i+2, j+3] + raw_trn[i+2, j+4] + raw_trn[i+2, j+5])/3
    }
  }
  temp_trn
  return(temp_trn)
}

#' GetCoef()
#'
#' @description Calculate coefficients in the simulataneous equations to have initial values
#' @return Coefficients
#' @export
#'
#' @examples
#' GetCoef()
GetCoef <- function(raw_trn) {
  # RHS
  A_unit <- GetA(raw_trn)
  A_unit
  # 2by2 matrix
  mat_2by2 <- matrix(c(DC_mode, DC_gesture, PR_mode, PR_gesture), ncol=2, byrow=T)
  mat_2by2
  # rhs matrix
  mat_rhs <- matrix(c(A_unit[[1]], A_unit[[2]]), ncol=1)
  mat_rhs
  # coefs
  coefs <- solve(mat_2by2, mat_rhs)
  coefs
  return(coefs)
}

#' GetA()
#'
#' @description Calculate initial TCT in training session
#' @param raw_trn Training trials raw data
#'
#' @return A pair TCT for the first training trial
#' @export
#'
#' @examples
#' GetA(DC_mode, DC_gesture, PR_mode, PR_gesture)
GetA <- function(raw_trn) {
  # RHS (constants)
  # These two are used as references for future configurations (not for the over-fitting)

  # Get a list of t1 of each configuration
  DC_t1 <- c()
  DC_rhs_counter <- 0
  PR_t1 <- c()
  PR_rhs_counter <- 0
  CC_t1 <- c()
  CC_rhs_counter <- 0
  for (i in 1:nrow(raw_trn)) {
    if(raw_trn[i,2] == "DC") {
      DC_t1 <- c(DC_t1, raw_trn[i,4])
      DC_rhs_counter <- DC_rhs_counter + 1
    } else if (raw_trn[i,2] == "PR") {
      PR_t1 <- c(PR_t1, raw_trn[i,4])
      PR_rhs_counter <- PR_rhs_counter + 1
    } else if (raw_trn[i,2] == "CC")  {
      CC_t1 <- c(CC_t1, raw_trn[i,4])
      CC_rhs_counter <- CC_rhs_counter + 1
    }
  }
  DC_t1
  PR_t1
  CC_t1

  # Get A based on hook calibration quality and configuration.
  # It was shown that there is positive correlation between hook calibration quality and task performance (e.g., r = 0.88 with deviation)
  # if ( (hook_calib_qt == "good") && (curr_Mode == "DC") ) {
  #   DC_rhs <- quantile(DC_t1, 0.30)
  # } else if ( (hook_calib_qt == "good") && (curr_Mode == "PR") ) {
  #   PR_rhs <- quantile(PR_t1, 0.30)
  # } else if ( (hook_calib_qt == "good") && (curr_Mode == "CC") ) {
  #   CC_rhs <- quantile(CC_t1, 0.30)
  # } else if ( (hook_calib_qt == "moderate") && (curr_Mode == "DC") ) {
  #   DC_rhs <- quantile(DC_t1, 0.50)
  # } else if ( (hook_calib_qt == "moderate") && (curr_Mode == "PR") ) {
  #   PR_rhs <- quantile(PR_t1, 0.50)
  # } else if ( (hook_calib_qt == "moderate") && (curr_Mode == "CC") ) {
  #   CC_rhs <- quantile(CC_t1, 0.50)
  # } else if ( (hook_calib_qt == "bad") && (curr_Mode == "DC") ) {
  #   DC_rhs <- quantile(DC_t1, 0.90)
  # } else if ( (hook_calib_qt == "bad") && (curr_Mode == "PR") ) {
  #   PR_rhs <- quantile(PR_t1, 0.90)
  # } else if ( (hook_calib_qt == "bad") && (curr_Mode == "CC") ) {
  #   CC_rhs <- quantile(CC_t1, 0.90)
  # }

  if (curr_Mode == "DC" | curr_Mode == "PR") {
    DC_rhs_qt_good <- quantile(DC_t1, 0.2)
    DC_rhs_qt_moderate <- quantile(DC_t1, 0.8)
    DC_rhs_qt_bad<- quantile(DC_t1, 0.9)
    PR_rhs_qt_good <- quantile(PR_t1, 0.2)
    PR_rhs_qt_moderate <- quantile(PR_t1, 0.65)
    PR_rhs_qt_bad<- quantile(PR_t1, 0.93)
  } else if (curr_Mode == "CC") {
    CC_rhs_qt_good <- quantile(CC_t1, 0.2)
    CC_rhs_qt_moderate <- quantile(CC_t1, 0.68)
    CC_rhs_qt_bad<- quantile(CC_t1, 0.76)
  }

  # Return a pair of rhs based on the interest (configuration)
  # In default, DC and PR will be returned.
  # However, if the case of CC, CC_rhs will be used to calculate coefficients
  # DC will be used always. Only CC and PR will be switched
  if ( (curr_Mode == "CC") && (hook_calib_qt == "good") ) {
    rhs_1 <- DC_rhs_qt_good
    rhs_2 <- CC_rhs_qt_good
  } else if ( (curr_Mode == "CC") && (hook_calib_qt == "moderate") ) {
    rhs_1 <- DC_rhs_qt_moderate
    rhs_2 <- CC_rhs_qt_moderate
  } else if ( (curr_Mode == "CC") && (hook_calib_qt == "bad") ) {
    rhs_1 <- DC_rhs_qt_bad
    rhs_2 <- CC_rhs_qt_bad
  } else if ( hook_calib_qt == "good" ) {
    rhs_1 <- DC_rhs_qt_good
    rhs_2 <- PR_rhs_qt_good
  } else if ( hook_calib_qt == "moderate" ) {
    rhs_1 <- DC_rhs_qt_moderate
    rhs_2 <- PR_rhs_qt_moderate
  } else if ( hook_calib_qt == "bad" ) {
    rhs_1 <- DC_rhs_qt_bad
    rhs_2 <- PR_rhs_qt_bad
  }

  rhs_1
  rhs_2
  rhs_1 <- as.numeric(rhs_1)
  rhs_2 <- as.numeric(rhs_2)
  rhs_1
  rhs_2

  A_list <- list(rhs_1, rhs_2)
  return(A_list)
}

#' GetCurve()
#'
#' @description Calculate the number of required trials to be trained based on configuration and cases
#' @param A Initial TCT of the training session
#' @param slope_in Slope value
#'
#' @return The number of required trials to be trained
#' @export
#'
#' @examples
#' GetCurve(A, slope value)
GetCurve <- function(A, slope_in) {
  # slope calculation
  # A <- 61
  # slope_in <- 0.8
  slope_use <- log(slope_in)/log(2)
  slope_use
  # get TCT
  curve_TCT <- c()

  # Apply Case 2 (Normal learning curve)
  for (i in 1:30)
    curve_TCT[i] <- A*i^slope_use
  curve_TCT

  # three slots for Satisfaction
  Three_slots <- c()

  # to return the trial number
  trial_num <- 0
  if (curr_Mode == "DC") {
    for (i in 1:(length(curve_TCT)-2)) {
      if (isFALSE( (curve_TCT[i]+curve_TCT[i+1]+curve_TCT[i+2])/3 > DC_max) == FALSE)
        trial_num <- trial_num + 1
    }
  } else if (curr_Mode == "PR") {
    for (i in 1:length(curve_TCT)) {
      if (isFALSE( (curve_TCT[i]+curve_TCT[i+1]+curve_TCT[i+2])/3 > PR_max) == FALSE)
        trial_num <- trial_num + 1
    }
  } else if (curr_Mode == "CC") {
    for (i in 1:length(curve_TCT)) {
      if (isFALSE( (curve_TCT[i]+curve_TCT[i+1]+curve_TCT[i+2])/3 > CC_max) == FALSE)
        trial_num <- trial_num + 1
    }
  } else if (curr_Mode == "Custom") {
    for (i in 1:length(curve_TCT)) {
      if (curve_TCT[i] > FD_max)
        trial_num <- trial_num + 1
    }
  }

  # For Satisfaction???
  # Three_slots[1] <- curve_TCT[trial_num-1]
  # Three_slots[2] <- curve_TCT[trial_num]
  # Three_slots[3] <- curve_TCT[trial_num+1]

  # For Learnability
  trial_num <- trial_num + 1 # because the for-loop stops at the index before the index when TCT becomes smaller than threshold.
  trial_num

  # Need to add 2 because the trial number in curve_TCT is moving average trial number
  # ex) if the trial number is 1, it means it conducted three trials and calculated one moving average
  trial_pkg <- c()

  if ( (learning_Skill == "good") && (hook_calib_qt == "moderate") ) {
    return(trial_num)
  } else if ( (learning_Skill == "moderate" && hook_calib_qt == "good") ) {
    return(trial_num)
  } else {
    trial_uptick <- GetUptick(curve_TCT)
    trial_uptick
    return(trial_uptick)
  }
}

#' GetUptick()
#'
#' @description Calculate upticks
#' @param curve_TCT
#'
#' @return The number of required trials to be trained in Case 3
#' @export
#'
#' @examples
#' GetUptick(curve_TCT)
GetUptick <- function(curve_TCT) {
  # uptick trial
  ut <- 0
  ut <- sample(3:4, size=1) # need to adjust "3" to "2"
  ut

  # update the curve with random upticks
  curve_uptick <- c()
  i <- 1
  j <- 1
  while(i < 100) {
    if (i < ut)
      curve_uptick[i] <- curve_TCT[j]
    else if (i == ut) {
      curve_uptick[i] <- (curve_TCT[j-2] + curve_TCT[j-1])/2
      curve_uptick[i+1] <- (curve_uptick[i] + curve_TCT[i-1])/2
      curve_uptick[i+2] <- curve_TCT[j]
      i <- i + 2
      ut <- ut + sample(3:5, size=1) # This sampling generation has an impact on the number of trials
    }
    i <- i + 1
    j <- j + 1
  }
  curve_uptick

  # remove NAs
  curve_uptick_NET <- c()
  for (i in 1:length(curve_uptick)) {
    if (!is.na(curve_uptick[i]))
      curve_uptick_NET <- c(curve_uptick_NET, curve_uptick[i])
  }
  curve_uptick_NET

  # to return the trial number
  trial_num <- 0
  if (curr_Mode == "DC") {
    for (i in 1:length(curve_uptick_NET)) {
      if (curve_uptick_NET[i] > DC_max)
        trial_num <- trial_num + 1
    }
  } else if (curr_Mode == "PR") {
    for (i in 1:length(curve_uptick_NET)) {
      if (curve_uptick_NET[i] > PR_max)
        trial_num <- trial_num + 1
    }
  } else if (curr_Mode == "CC") {
    for (i in 1:length(curve_uptick_NET)) {
      if (curve_uptick_NET[i] > CC_max)
        trial_num <- trial_num + 1
    }
  } else if (curr_Mode == "Custom") {
    for (i in 1:length(curve_uptick_NET)) {
      if (curve_uptick_NET[i] > Custom_max)
        trial_num <- trial_num + 1
    }
  }
  trial_num

  return(trial_num/2)
}
