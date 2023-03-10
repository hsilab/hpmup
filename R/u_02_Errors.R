#' CalcError
#'
#' @param L
#'
#' @return
#' @export
#'
#' @examples
CalcError <- function (L) {
  norm_L <- 1-L/20 # this '20' is a very assumption that the number of maximum training trials is 20
  ErrorRate <- max( (exp(norm_L)-exp(1))/(1-exp(1)), 0)

  return(ErrorRate)
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


#' GetErrSimple()
#'
#' @description Return error using distribution and probability
#' @param oper_name Name of the operator
#'
#' @return Error information in a list format: err_Time, err_Case, err_chunk, motor_err
#' @export
#'
#' @examples
#' GetErrSimple(oper_name)
GetErrSimple <- function(oper_name) {

  # error information on each type of configuration and gesture
  if (curr_Mode == "DC") {
    err_prob_Flexion <- 0.45
    err_prob_Extension <- 0.45
    err_prob_Power_grip <- 0.35

    err_dura_Flexion <- rnorm(n=1, mean=750, sd=250)
    err_dura_Extension <- rnorm(n=1, mean=650, sd=250)
    err_dura_Power_grip <- rnorm(n=1, mean=550, sd=250)
  } else if (curr_Mode == "PR") {
    err_prob_Sup_Pro_PR <- 0.35
    err_prob_Open_PR <- 0.45
    err_prob_Close_PR <- 0.5

    err_dura_Sup_Pro_PR <- rnorm(n=1, mean=650, sd=250)
    err_dura_Open_PR <- rnorm(n=1, mean=750, sd=150)
    err_dura_Close_PR <- rnorm(n=1, mean=800, sd=150)
  } else if (curr_Mode == "CC") {
    err_prob_Sup_Pro_CC <- 0.55
    err_prob_Open_CC <- 0.45
    err_prob_Close_CC <- 0.55

    err_dura_Sup_Pro_CC <- rnorm(n=1, mean=950, sd=100)
    err_dura_Open_CC <- rnorm(n=1, mean=950, sd=150)
    err_dura_Close_CC <- rnorm(n=1, mean=850, sd=150)
  }

  # choose a specific error information for input operator
  err_prob <- 0
  err_dura <- 0
  if (oper_name == "Flexion") {
    err_prob <- err_prob_Flexion
    err_dura <- err_dura_Flexion
  } else if (oper_name == "Extension") {
    err_prob <- err_prob_Extension
    err_dura <- err_dura_Extension
  } else if ( (oper_name == "Grasp") && (curr_Mode == "DC") ) {
    err_prob <- err_prob_Power_grip
    err_dura <- err_dura_Power_grip
  } else if ( (oper_name == "Turn_MTM") && (curr_Mode == "PR") ) {
    err_prob <- err_prob_Sup_Pro_PR
    err_dura <- err_dura_Sup_Pro_PR
  } else if ( (oper_name == "Open_MTM") && (curr_Mode == "PR") ) {
    err_prob <- err_prob_Open_PR
    err_dura <- err_dura_Open_PR
  } else if ( (oper_name == "Grasp") && (curr_Mode == "PR") ) {
    err_prob <- err_prob_Close_PR
    err_dura <- err_dura_Close_PR
  } else if ( (oper_name == "Turn_MTM") && (curr_Mode == "CC") ) {
    err_prob <- err_prob_Sup_Pro_CC
    err_dura <- err_dura_Sup_Pro_CC
  } else if ( (oper_name == "Open_MTM") && (curr_Mode == "CC") ) {
    err_prob <- err_prob_Open_CC
    err_dura <- err_dura_Open_CC
  } else if ( (oper_name == "Grasp") && (curr_Mode == "CC") ) {
    err_prob <- err_prob_Close_CC
    err_dura <- err_dura_Close_CC
  }

  err_thre_1 <- sample(0:9, size=1)/10
  err_thre_2 <- sample(0:9, size=1)/100
  err_thre_3 <- sample(0:9, size=1)/1000
  err_thre <- err_thre_1 + err_thre_2 + err_thre_3
  err_thre

  err_Perp <- 0.1
  err_Motor <- 1 - err_Perp

  err_oper_1 <- sample(0:9, size=1)/10
  err_oper_2 <- sample(0:9, size=1)/100
  err_oper_3 <- sample(0:9, size=1)/1000
  err_oper <- err_oper_1 + err_oper_2 + err_oper_3
  err_oper

  if ( (err_thre < err_prob) && (err_oper < err_Perp) ) {
    err_dura <- 3500
    err_Case <- 2  # err_perceptual + err_motor = 2
    err_Chunk <- "Vision error"
    motor_Err <- 1
  } else if (err_thre < err_prob) {
    err_Case <- 1  # only motor error
    err_Chunk <- "Motor error"
    motor_Err <- 1
  }

  if (err_thre < err_prob) {
    err_List <- list(err_dura, err_Case, err_Chunk, motor_Err)
    return(err_List)
  } else {
    err_List <- list(0, 0, 0, 0)
    return(err_List)
  }
}

#' GetErr()
#'
#' @description Calculate errors
#' @param oper_name Name of the operator
#'
#' @return Error information in a list format: err_Time, err_Case, err_chunk, motor_err
#' @export
#'
#' @examples
#' GetErr(oper_name)
GetErr <- function(oper_name) {

  err_chunk <- 0
  gesture_counter <- 0
  # make a data frame for a specific gesture
  for (i in 1:nrow(raw_error)) {
    if (raw_error[i, 4] == "Flexion") {
      gesture_table <- rbind(gesture_table, raw_error[i,])
      gesture_counter <- gesture_counter + 1
    }
  }
  gesture_table

  # p : calculate error probability
  p1 <- 0 # perception - correct
  p2 <- 0 # perception - incorrect
  for (i in 1:nrow(gesture_table)) {
    if (gesture_table[i, 5] == 1)
      p1 <- p1 + 1
    else
      p2 <- p2 + 1
  }
  p1
  p2
  prob_p1 <- p1/(p1+p2)
  prob_p1
  prob_p2 <- 1 - prob_p1
  prob_p2

  # p : criteria
  p_rc_1 <- sample(1:9, size=1)/10
  p_rc_2 <- sample(1:9, size=1)/100
  p_rc_3 <- sample(1:9, size=1)/1000
  p_rc <- p_rc_1 + p_rc_2 + p_rc_3
  p_rc

  # p : decision
  op_P <- 0 # a variable for P operator
  if (prob_p1 > p_rc) {
    op_P <- 1
  } else {
    op_P <- 2
  }
  op_P

  # c : calculate error probability
  c1 <- 0 # cognition - intended - not confused
  c2 <- 0 # cognition - intended - confused
  c3 <- 0 # cognition - unintended
  # calculate each c case based on p status (op_P)
  for (i in 1:nrow(gesture_table)) {
    if (gesture_table[i, 5] == op_P && gesture_table[i, 6] == 1)
      c1 <- c1 + 1
    else if (gesture_table[i, 5] == op_P && gesture_table[i, 6] == 2)
      c2 <- c2 + 1
    else if (gesture_table[i, 5] == op_P && gesture_table[i, 6] == 3)
      c3 <- c3 + 1
  }
  c1
  c2
  c3
  prob_c1 <- c1/(c1+c2+c3)
  prob_c1
  prob_c2 <- c2/(c1+c2+c3)
  prob_c2
  prob_c3 <- 1 - prob_c1 - prob_c2
  prob_c3

  # c : criteria
  c_rc_1 <- sample(1:9, size=1)/10
  c_rc_2 <- sample(1:9, size=1)/100
  c_rc_3 <- sample(1:9, size=1)/1000
  c_rc <- c_rc_1 + c_rc_2 + c_rc_3
  c_rc

  # c : decision
  op_C <- 0 # a variable for C operator
  if (prob_c1 > c_rc) {
    op_C <- 1
  } else if (prob_c1 + prob_c2 > c_rc) {
    op_C <- 2
  } else {
    op_C <- 3
  }
  op_C

  # m : calculate error probability
  m1 <- 0 # motor - type ok & intensity ok
  m2 <- 0 # motor - type ok & intensity wrong
  m3 <- 0 # motor - type wrong & intensity ok
  m4 <- 0 # motor - type wrong & intensity wrong
  # calculate each m case based on p and m status (op_P and op_C)
  for (i in 1:nrow(gesture_table)) {
    if (gesture_table[i, 5] == op_P && gesture_table[i, 6] == op_C && gesture_table[i, 7] == 1)
      m1 <- m1 + 1
    else if (gesture_table[i, 5] == op_P && gesture_table[i, 6] == op_C && gesture_table[i, 7] == 2)
      m2 <- m2 + 1
    else if (gesture_table[i, 5] == op_P && gesture_table[i, 6] == op_C && gesture_table[i, 7] == 3)
      m3 <- m3 + 1
    else if (gesture_table[i, 5] == op_P && gesture_table[i, 6] == op_C && gesture_table[i, 7] == 4)
      m4 <- m4
  }
  m1
  m2
  m3
  m4
  prob_m1 <- m1/(m1+m2+m3+m4)
  prob_m1
  prob_m2 <- m2/(m1+m2+m3+m4)
  prob_m2
  prob_m3 <- m3/(m1+m2+m3+m4)
  prob_m3
  prob_m4 <- 1 - prob_m1 - prob_m2 - prob_m3
  prob_m4

  # m : criteria
  m_rc_1 <- sample(1:9, size=1)/10
  m_rc_2 <- sample(1:9, size=1)/100
  m_rc_3 <- sample(1:9, size=1)/1000
  m_rc <- m_rc_1 + m_rc_2 + m_rc_3
  m_rc

  # m : decision
  op_M <- 0 # a variable for M operator
  if (prob_m1 > m_rc) {
    op_M <- 1
  } else if (prob_m1 + prob_m2 > m_rc) {
    op_M <- 2
  } else if (prob_m1 + prob_m2 + prob_m3 > m_rc) {
    op_M <- 3
  } else {
    op_M <- 4
  }
  op_M

  # Get Error time duration
  err_List <- c()
  err_Time <- 0
  motor_err <- 0 # to count the number of motor operators
  err_Case <- op_P + op_C + op_M

  # differentiate the type of error chunk based on the error type
  if (op_C > 1)
    err_chunk <- "<current mode>"
  else if (op_M == 2)
    err_chunk <- "<current intensity>"
  else if (op_M == 3)
    err_chunk <- "<current type>"
  else if (op_M == 4)
    err_chunk <- "<current type and intensity>"

  # Decision for motor error
  if (op_M > 1)
    motor_err <- 1

  if (op_P + op_C + op_M > 3) {
    err_Time <- GetErrTimeDur(op_P, op_C, op_M, gesture_table)
    err_Time
    err_List <- list(err_Time, err_Case, err_chunk, motor_err)
    return(err_List) # return the error time only if the summation of all three operators is bigger than 3
  } else {
    err_List <- list(0, 0, 0, 0)
    return(err_List) # if the combination is (1,1,1), then return 0. This will be used to treat next lines
  }
}

#' GetErrTimeDur()
#'
#' @description Calculate the duration of an error
#' @param op_P type of perceptual error
#' @param op_C type of cognitive error
#' @param op_M type of motor error
#' @param gesture_table A data frame for a specific type of gesture
#'
#' @return Error duration
#' @export
#'
#' @examples
#' GetErrTimeDur(op_P, op_C, op_M, gesture_table)
GetErrTimeDur <- function(op_P, op_C, op_M, gesture_table) {
  time_Sum <- 0
  time_Count <- 0
  for (i in 1:nrow(gesture_table)) {
    if (gesture_table[i, 5] == op_P && gesture_table[i, 6] == op_C && gesture_table[i, 7] == op_M) {
      time_Sum <- time_Sum + gesture_table[i, 12]
      time_Count <- time_Count + 1
    }
  }
  # error time for perception error = dropping or missing the pin
  if (op_P > 1)
    err_Time <- 3500
  else
    err_Time <- time_Sum / time_Count
  return(err_Time)
}
