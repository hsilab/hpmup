# pr <- read.csv(file="./data/PR-CRT.csv", header=FALSE)
# dc <- read.csv(file="./data/DC-CRT.csv", header=FALSE)


############## General
# duration of the task
exp_time <- 120000 # 2 minutes

##### <User input : current mode>
# curr_Mode <- "CC"
# curr_Mode <- "PR"
 curr_Mode <- "DC"
# curr_Mode <- "Custom" # This FD is for user defined device

##### <User input : Custom>
cus_Mode <- 3
cus_Gesture <- 5
# DC and PR will be used as default.
DC_mode <- 2
DC_gesture <- 3
PR_mode <- 1
PR_gesture <- 4

##### <User input : for getting A (initial TCT)>
if (curr_Mode == "DC") {
  dev_Mode <- 2
  dev_Gesture <- 3
} else if (curr_Mode == "PR") {
  dev_Mode <- 1
  dev_Gesture <- 4
} else if (curr_Mode == "CC") {
  dev_Mode <- 1
  dev_Gesture <- 4
} else if (curr_Mode == "Custom") {
  dev_Mode <- cus_Mode
  dev_Gesture <- cus_Gesture
}

##### <User input : calib qt, Learning skill, fatigue>
# <Hook calibration quality>
# hook_calib_qt <- "good" # Case 1
 hook_calib_qt <- "moderate" # Case 2
# hook_calib_qt <- "bad" # Case 3

# <Learning skill>
# learning_Skill <- "good"
 learning_Skill <- "moderate"
# learning_Skill <- "bad"

# <Fatigue>
fatigue <- "low"
# fatigue <- "moderate"
# fatigue <- "high"

##### <User input : b (choose slope)>
# slope <- 0.7
# slope <- 0.75
slope <- 0.8
# slope <- 0.85
# slope <- 0.9
# slope <- 0.95

if (learning_Skill == "good") {
  slope <- 0.75
} else if (learning_Skill == "moderate") {
  slope <- 0.8
} else if (learning_Skill == "bad") {
  slope <- 0.85
}

# equation: Y=A*x^b
# Y = the cost of unit x (dependent variable)
# A = the theoretical cost of unit 1 (a.k.a. T1)
# x = the unit number (independent variable)
# b = a constant representing the slope (slope = 2b)

# Regarding b
# 70% = entirely manual operations
# 80% = 75% manual + 25% automated
# 85% = 50% manual + 50% automated
# 90% = 25% manual + 75% automated
# example
# 85%	aircraft industry
# 80~85%	shipbuilding
# 75~85%	Electrical
# 90~95%	Electronics
# 90~95%	Machining
# 88~92%	Welding

##### <User input : criteria>
DC_max <- 35
PR_max <- 25
CC_max <- 23
Custom_max <- 38

##### <User input : Satisfaction weights>
wts_perf <- 0.25
wts_desire <- 1 - wts_perf

##### <User input : Satisfaction Desire>
##### 1 = good, 0 = bad
stf_dim <- 1
stf_wgt <- 1
stf_wear <- 1
stf_safe <- 1
stf_dura <- 1
stf_control <- 1
stf_comfort <- 0


###### HPM-UP
# PRE-DEFINED OPERATORS
oper_set <- read.csv(file="./data/cpmr_test.csv", header=FALSE)

# BEST TRAINING TRIALS - ERROR
if (curr_Mode == "DC") {
  raw_error <- read.csv(file="./data/Err_DC.csv", header=TRUE)
} else if (curr_Mode == "PR") {
  raw_error <- read.csv(file="./data/Err_PR.csv", header=TRUE)
} else if (curr_Mode == "CC") {
  raw_error <- read.csv(file="./data/Err_CC.csv", header=TRUE)
}
# TRAINING TRIAL INFORMATION - LEARNABILITY & SATISFACTION
raw_trn <- read.csv(file="./data/Training_overview.csv", header=TRUE)
# DATASET for CRT (Machien)
norm_CRT <- read.csv(file="H:/Study/2_NSF/Phase_1/9. Actual expr/norm_CRT.csv", header=TRUE)

# data from for working memory
a<-c()
b<-c()
c<-c()
d<-c()
e<-c()
f<-c()
g<-c()
h<-c()

wm_Box <- data.frame(a, b, c, d, e, f, g, h)

wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))

colnames(wm_Box)[1]<-"chunk_num"
colnames(wm_Box)[2]<-"chunk_name"
colnames(wm_Box)[3]<-"stack_depth"
colnames(wm_Box)[4]<-"pushed_time(Global)"
colnames(wm_Box)[5]<-"elapsed_time(Local)"
colnames(wm_Box)[6]<-"rehearsal"
colnames(wm_Box)[7]<-"activation"
colnames(wm_Box)[8]<-"prob_recall"
wm_Box

# data from for chunk lifecyle
cl_1 <- c()
cl_2 <- c()
cl_3 <- c()
cl_4 <- c()
cl_5 <- c()

chunk_Lifecyle <- data.frame(cl_1, cl_2, cl_3, cl_4, cl_5)
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))

colnames(chunk_Lifecyle)[1]<-"Chunk_name"
colnames(chunk_Lifecyle)[2]<-"Time_pushed"
colnames(chunk_Lifecyle)[3]<-"Time_decayed"
colnames(chunk_Lifecyle)[4]<-"Time_end"
colnames(chunk_Lifecyle)[5]<-"Time_total"
chunk_Lifecyle

# data frame for a specific type of gesture
gesture_table <- data.frame()
a<-c()
b<-c()
c<-c()
d<-c()
e<-c()
f<-c()
g<-c()
h<-c()
i<-c()
j<-c()
k<-c()
l<-c()
m<-c()
n<-c()

gesture_table
gesture_table <- data.frame(a, b, c, d, e, f, g, h, i, j, k, l, m, n)
gesture_table <- rbind(gesture_table, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))
colnames(gesture_table) <- colnames(raw_error)

# data frame: number of operators
num_Oper <- c()
no_1 <- c()
no_2 <- c()
no_3 <- c()

num_Oper <- data.frame(no_1, no_2, no_3)
num_Oper <- rbind(num_Oper, c(0,0,0))

colnames(num_Oper)[1]<-"Perceptual"
colnames(num_Oper)[2]<-"Congitive"
colnames(num_Oper)[3]<-"Motor"

#' ResetAll()
#'
#' @description Reset all variables
#' @return Empty variables
#' @export
#'
#' @examples
#' ResetAll()
ResetAll <- function() {
  # reset wm_Box
  for (i in 1:7) {
    for (j in 1:8) {
      wm_Box[i,j] <- 0
    }
    wm_Box[i,1] <- i # assigning chunk number in each row
    wm_Box[i,6] <- 3 # all rehearsals are reset as 3
  }
  # reset chunk_Lifecyle
  for (i in 1:nrow(chunk_Lifecyle)) {
    for (j in 1:ncol(chunk_Lifecyle)) {
      chunk_Lifecyle[i,j] <- 0
    }
  }
  acc_Time_1 <- 0
  acc_Time_2 <- 0

  reseted <- list(wm_Box, chunk_Lifecyle, acc_Time_1, acc_Time_2)
  return(reseted)
}

#' ResetNumOp()
#'
#' @description Reset number of operators
#' @return Empty data frame for number of operators
#' @export
#'
#' @examples
#' ResetNumOp()
ResetNumOp <- function() {
  num_Oper[1,1] <- 0
  num_Oper[1,2] <- 0
  num_Oper[1,3] <- 0
  return (num_Oper)
}
