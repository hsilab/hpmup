
#' CalcEfficiency
#'
#' @param E
#' @param perf_expert
#'
#' @return
#' @export
#'
#' @examples
CalcEfficiency <- function (E, perf_expert) {

  return ( max(ceiling(perf_expert*(1-E)), 0) )

}


###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
########################                        ###########################################
######################## For the Main function  ###########################################
########################                        ###########################################
###########################################################################################
###########################################################################################
###########################################################################################


#' ExtOper()
#'
#' @description Return a pure operator name
#' @param line_of_input
#'
#' @return operator name
#' @export
#'
#' @examples
#' ExtOper(line_of_input)
ExtOper <- function(line_of_input) {
  oper_raw<-strsplit(line_of_input, " ")
  mat <- matrix(unlist(oper_raw), ncol=1, byrow=TRUE)

  # count the number of dots
  dot_counter <- 0
  for (i in 1:nchar(mat[1,1])) {
    c <- substr(mat[1,1], i, i)
    if (c == ".")
      dot_counter <- dot_counter + 1
  }
  dot_counter

  oper<-substr(mat[1,1], dot_counter+1, nchar(mat[1,1]))
  return(oper)
}
# ExtOper <- function(line_of_input) {
#   oper_raw<-strsplit(line_of_input, " ")
#   mat <- matrix(unlist(oper_raw), ncol=1, byrow=TRUE)
#   oper<-substr(mat[1,1], 2, nchar(mat[1,1]))
#   return(oper)
# }

#' RetrOpTime_Motor()
#'
#' @description Retrieving the operator time (for error)
#' @param oper operator name
#' @param k line number
#'
#' @return error time for a motor operator
#' @export
#'
#' @examples
#' RetrOpTime_Motor(operator name, line number)
RetrOpTime_Motor <- function (oper, k) {

  # get the time of hand gesture
  err_Time <- 0
  # err_Time <- GetErr(oper)
  err_Time <- GetErrSimple(oper)
  return(err_Time)
}

#' RetrievingOperTime()
#'
#' @description Retrieving the operator time
#' @param oper operator name
#' @param k line number
#'
#' @return operator time for an operator
#' @export
#'
#' @examples
#' RetrievingOperTime(operator name, line number, scenario, oper_set)
RetrievingOperTime <- function (oper, k) {
  oper_set_line <- nrow(oper_set)

  matched_Time <- 0
  for (i in 1:oper_set_line) {
    if (oper == oper_set[i, 2])
      matched_Time <- oper_set[i, 3]
  }

  # calculation time for hearing and saying : each word is 400 ms
  if (oper == "Hear" | oper == "Say") {
    num_Words <- strsplit(task_1[k, 1], " ")
    num_Words <- length(num_Words[[1]]) - 1
    matched_Time <- num_Words * 400
  }
  return(matched_Time)
}
# OLD RetrievingOperTime
# RetrievingOperTime <- function (oper, k, scenario, oper_set) {
#   oper_set_line <- nrow(oper_set)
#
#   # bring operator time from the database
#   matched_Time <- 0
#   for (i in 1:oper_set_line) {
#     if (oper == oper_set[i, 2])
#       matched_Time <- oper_set[i, 3]
#   }
#
#   # # N-CPM : Novice Vision
#   # if (oper == "Look" | oper == "Search") {
#   #   repetition <- sample(1:3, size=1) # Novice look & searching pattern
#   #   for (i in 1:oper_set_line) {
#   #     if (oper == oper_set[i, 2])
#   #       matched_Time <- oper_set[i, 3]
#   #   }
#   #   matched_Time <- repetition * matched_Time
#   # }
#
#   # # N-CPM : Novice Motor (including experts)
#   # if (oper == "Touch") {
#   #   RT <- 0 # Hick-Hyman law (Hick, 1952)
#   #   MT <- 0 # Fitts' law (Fitts, 1954)
#   #   nov_Chunk <- 1.9 # (Chase & Simon, 1973)
#   #   exp_Chunk <- 2.5 # (Chase & Simon, 1973)
#   #   nov_dat_display <- 10 # number of items in the display for novices (all items)
#   #   exp_dat_display <- 2 # number of items in the display for experts (only essential information
#   #   icon_width <- 3
#   #   icon_distance <- 30
#   #
#   #   RT_nov <- 1/nov_Chunk * log2(nov_dat_display)
#   #   RT_exp <- 1/exp_Chunk * log2(exp_dat_display)
#   #
#   #   MT_nov <- 1/nov_Chunk * log2(2*nov_dat_display/icon_width) * 1000 # millisecond
#   #   MT_exp <- 1/exp_Chunk * log2(2*nov_dat_display/icon_width) * 1000 # millisecond
#   #
#   #   matched_Time <- RT_nov + MT_nov # for novices
#   #   # matched_Time <- RT_exp + MT_exp # for experts
#   # }
#
#   # # calculation time for hearing and saying : each word is 400 ms
#   # if (oper == "Hear" | oper == "Say") {
#   #   num_Words <- strsplit(scenario[k, 1], " ")
#   #   num_Words <- length(num_Words[[1]]) - 1
#   #   matched_Time <- num_Words * 400
#   # }
#   return(matched_Time)
# }

#' Shiny_Parameter_Test()
#'
#' @description testing parameter calculation between R and Shiny
#' @param test
#'
#' @return add + 5
#' @export
#'
#' @examples
Shiny_Parameter_Test <- function(test) {
  return(test + 5)
}

