# pr <- read.csv(file="./data/PR-CRT.csv", header=FALSE)
# dc <- read.csv(file="./data/DC-CRT.csv", header=FALSE)

############## General
# cw GROUPS
# cw_group <- 3
cw_group <- 4

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
 learning_Skill <- "good"
# learning_Skill <- "moderate"
# learning_Skill <- "bad"

# <Fatigue>
# fatigue <- "low"
 fatigue <- "moderate"
# fatigue <- "high"

##### <User input : b (choose slope)>
# slope <- 0.7
# slope <- 0.75
# slope <- 0.8
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
# oper_set <- read.csv(file="./data/cpmr_test.csv", header=FALSE)

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
# DATASET for CRT (Machine Learning)
norm_CRT <- read.csv(file="D:/JP_project_test/hpmup/data/norm_CRT24.csv", header=TRUE)
stan_CRT <- read.csv(file="D:/JP_project_test/hpmup/data/stan_CRT24.csv", header=TRUE)
CRT_24 <- read.csv(file="D:/JP_project_test/CRT_ML.csv", header=TRUE)


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



###############################################################################################################
# SAME WITH N-CPM FROM HERE #####################################################################################
###############################################################################################################

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


# Glossary
a<-c()
b<-c()
c<-c()
d<-c()

glossaryGUI <- data.frame(a, b, c, d)

glossaryGUI <- rbind(glossaryGUI, c(0, 0, 0, 0))

colnames(glossaryGUI)[1]<-"Name"
colnames(glossaryGUI)[2]<-"Definition"
colnames(glossaryGUI)[3]<-"Reference"
colnames(glossaryGUI)[4]<-"Task completion time (ms)"

glossaryGUI

glossaryGUI <- rbind(glossaryGUI, c("Look","Look at an item at a known position","Kieras, 1997; John & Gray, 1995; Estes, 2017",550))
glossaryGUI <- rbind(glossaryGUI, c("Read","Time to read a single word","Kieras, 1997; Estes, 2017",260))
glossaryGUI <- rbind(glossaryGUI, c("Search","Search for an item at an unknown position","Kieras, 1997; Estes, 2017",1250))
glossaryGUI <- rbind(glossaryGUI, c("Saccade","A single rapid eye movement","Card et al., 1986",230))
glossaryGUI <- rbind(glossaryGUI, c("Hear","Listen to someone speaking. Label should be the text of the speech","Kieras, 1997; John & Gray, 1995; Estes, 2017",400))
glossaryGUI <- rbind(glossaryGUI, c("Attend","Shifting of attention to stimuli","John & Gray, 1995; Estes, 2017",50))
glossaryGUI <- rbind(glossaryGUI, c("Initiate","Initiate motor process","John & Gray, 1995; Estes, 2017",50))
glossaryGUI <- rbind(glossaryGUI, c("Ignore","Removes item from working memory","Kieras, 1997; John & Gray, 1995; Estes, 2017",50))
glossaryGUI <- rbind(glossaryGUI, c("Mental","Generic operator for thinking","Card et al., 1980",1350))
glossaryGUI <- rbind(glossaryGUI, c("Recall","Retreive information from long term memory or working memory","Kieras, 1997; John & Gray, 1995; Estes, 2017",550))
glossaryGUI <- rbind(glossaryGUI, c("Store","Place item in working memory","Kieras, 1997; John & Gray, 1995; Estes, 2017",50))
glossaryGUI <- rbind(glossaryGUI, c("Think","Generic operator for thinking","Kieras, 1997; John & Gray, 1995; Estes, 2017",1250))
glossaryGUI <- rbind(glossaryGUI, c("Verify","Generic operator for thinking","Kieras, 1997; John & Gray, 1995; Estes, 2017",1250))
glossaryGUI <- rbind(glossaryGUI, c("Click","Press of a mouse button","Kieras, 1997; John & Gray, 1995; Estes, 2017",320))
glossaryGUI <- rbind(glossaryGUI, c("Drag","Drag an item across a screen, associated with touchscreen devices","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",546))
glossaryGUI <- rbind(glossaryGUI, c("Grasp","Act of reaching with the hand and grasping an object","Kieras, 1997; Estes, 2017",750))
glossaryGUI <- rbind(glossaryGUI, c("Hands","Move hands to position","Kieras, 1997; John & Gray, 1995; Estes, 2017",450))
glossaryGUI <- rbind(glossaryGUI, c("Keystroke","Press a single keyboard key","Kieras, 1997; John & Gray, 1995; Estes, 2017",280))
glossaryGUI <- rbind(glossaryGUI, c("Point","Move cursor via mouse","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",340))
glossaryGUI <- rbind(glossaryGUI, c("Swipe","One swipe gesture","Kieras, 1997; Estes, 2017",170))
glossaryGUI <- rbind(glossaryGUI, c("Tap","Touch a series of virtual buttons","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",179))
glossaryGUI <- rbind(glossaryGUI, c("Touch","Press a virtual button","Kieras, 1997; Estes, 2017",490))
glossaryGUI <- rbind(glossaryGUI, c("Turn","One turn of a knob or dial","Kieras, 1997; Estes, 2017",800))
glossaryGUI <- rbind(glossaryGUI, c("Type","Press a series of keyboad keys","Kieras, 1997; John & Gray, 1995; Estes, 2017",280))
glossaryGUI <- rbind(glossaryGUI, c("Write","Time to write a single word (handwriting)","Kieras, 1997; Estes, 2017",2000))
glossaryGUI <- rbind(glossaryGUI, c("Say","Speech. Label should be the text of speech","Kieras, 1997; John & Gray, 1995; Estes, 2017",400))
glossaryGUI <- rbind(glossaryGUI, c("Wait","User waiting for system. Modify time by adding x seconds at end of line","Kieras, 1997; John & Gray, 1995; Estes, 2017",1000))
glossaryGUI <- rbind(glossaryGUI, c("Reach","Move a hand to a display","Maynard et al., 1948",234))
glossaryGUI <- rbind(glossaryGUI, c("Flick","Flick a screen","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",298.5))
glossaryGUI <- rbind(glossaryGUI, c("double_Tap","Double tap a screen","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",358))
glossaryGUI <- rbind(glossaryGUI, c("Zoom in","Zoom in on the screen","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",506))
glossaryGUI <- rbind(glossaryGUI, c("Zoom out","Zoom out from the screen","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",506))
glossaryGUI <- rbind(glossaryGUI, c("Point_Finger","Point finger at the screen","None",340))
glossaryGUI <- glossaryGUI[-1,]

head(glossaryGUI)


# operator set
a<-c()
b<-c()
c<-c()

oper_set <- data.frame(a,b,c)

oper_set <- rbind(oper_set, c(0, 0, 0))

colnames(oper_set)[1]<-"Category_1"
colnames(oper_set)[2]<-"Category_2"
colnames(oper_set)[3]<-"TCT"

head(oper_set)

oper_set <- rbind(oper_set , c("see","Look",as.numeric(550)))
oper_set <- rbind(oper_set , c("see","Perceptual_processor",as.numeric(100)))
oper_set <- rbind(oper_set , c("see","Proofread",as.numeric(330)))
oper_set <- rbind(oper_set , c("see","Read",as.numeric(260)))
oper_set <- rbind(oper_set , c("see","Search",as.numeric(1250)))
oper_set <- rbind(oper_set , c("see","Saccade",as.numeric(30)))
oper_set <- rbind(oper_set , c("see","Hear",as.numeric(400)))
oper_set <- rbind(oper_set , c("cognitive","Attend",as.numeric(50)))
oper_set <- rbind(oper_set , c("cognitive","Cognitive_processor",as.numeric(70)))
oper_set <- rbind(oper_set , c("cognitive","Initiate",as.numeric(50)))
oper_set <- rbind(oper_set , c("cognitive","Ignore",as.numeric(50)))
oper_set <- rbind(oper_set , c("cognitive","Mental",as.numeric(1250)))
oper_set <- rbind(oper_set , c("cognitive","Recall",as.numeric(550)))
oper_set <- rbind(oper_set , c("cognitive","Store",as.numeric(50)))
oper_set <- rbind(oper_set , c("cognitive","Think",as.numeric(1250)))
oper_set <- rbind(oper_set , c("cognitive","Verify",as.numeric(1250)))
oper_set <- rbind(oper_set , c("cognitive","Decide",as.numeric(50)))
oper_set <- rbind(oper_set , c("hands","Click",as.numeric(320)))
oper_set <- rbind(oper_set , c("hands","Drag",as.numeric(230)))
oper_set <- rbind(oper_set , c("hands","Grasp",as.numeric(750)))
oper_set <- rbind(oper_set , c("hands","Hands",as.numeric(450)))
oper_set <- rbind(oper_set , c("hands","Keystroke",as.numeric(280)))
oper_set <- rbind(oper_set , c("hands","Motor_processor",as.numeric(70)))
oper_set <- rbind(oper_set , c("hands","Point",as.numeric(950)))
oper_set <- rbind(oper_set , c("hands","Swipe",as.numeric(170)))
oper_set <- rbind(oper_set , c("hands","Tap",as.numeric(450)))
oper_set <- rbind(oper_set , c("hands","Touch",as.numeric(490)))
oper_set <- rbind(oper_set , c("hands","Turn",as.numeric(800)))
oper_set <- rbind(oper_set , c("hands","Type",as.numeric(280)))
oper_set <- rbind(oper_set , c("hands","Write",as.numeric(2000)))
oper_set <- rbind(oper_set , c("hands","Say",as.numeric(400)))
oper_set <- rbind(oper_set , c("system","Wait",as.numeric(1000)))
oper_set <- rbind(oper_set , c("hands","Reach",as.numeric(795.6)))
oper_set <- rbind(oper_set , c("hands","Flexion",as.numeric(209.5)))
oper_set <- rbind(oper_set , c("hands","Extension",as.numeric(201.4)))
oper_set <- rbind(oper_set , c("hands","Turn_MTM",as.numeric(306)))
oper_set <- rbind(oper_set , c("hands","Move_MTM",as.numeric(795.6)))
oper_set <- rbind(oper_set , c("hands","Open_MTM",as.numeric(520)))
oper_set <- rbind(oper_set , c("hands","GLM_drag",as.numeric(546)))
oper_set <- rbind(oper_set , c("hands","Flick",as.numeric(298.5)))
oper_set <- rbind(oper_set , c("hands","GLM_Tap",as.numeric(179)))
oper_set <- rbind(oper_set , c("hands","double_tap",as.numeric(358)))
oper_set <- rbind(oper_set , c("hands","Zoomin",as.numeric(506)))
oper_set <- rbind(oper_set , c("hands","Zoomout",as.numeric(506)))
oper_set <- rbind(oper_set , c("hands","GLM_point",as.numeric(340)))
oper_set <- rbind(oper_set , c("hands","Tap_dup",as.numeric(450)))
oper_set <- rbind(oper_set , c("Foot","Kick",as.numeric(400)))
oper_set <- rbind(oper_set , c("hands","Raise",as.numeric(1223)))
oper_set <- rbind(oper_set , c("hands","Return",as.numeric(795.6)))
oper_set <- rbind(oper_set , c("hands","Release",as.numeric(750)))
oper_set <- oper_set[-1,]

head(oper_set)
