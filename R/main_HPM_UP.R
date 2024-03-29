#' Title HPM-UP main function
#'
#' @param scenario : is a scenario file (csv format)
#'
#' @return All results
#' @export
#'
#' @examples
#' RunHPMUP(scenario)
RunHPMUP <- function(scenario, time_library) {

  # pin_Num <- 0 # the number of pins moved
  # cycle_counter <- 0 # a counter for each cycle

  # Check reset is correct
  Reseted <- ResetAll()
  WM_A <- Reseted[[1]] # WM_A = Working memory of A-CPM
  chk_Life_A <- Reseted[[2]] # chk_Life_A = Chunk life of A-CPM
  Time_current <- Reseted[[3]] # acc_Time_1
  Time_past <- Reseted[[4]] # acc_Time_2
  num_Oper <- ResetNumOp()
  Motor_err <- 0

  # New Model (Actually, the model in May 2021)
  for (i in 2:nrow(scenario)) {
    oper_Name <- ExtOper(scenario[i, 1])
    oper_Time <- RetrievingOperTime(oper_Name, i)
    Time_current <- Time_current + as.numeric(oper_Time)

    num_of_chunks <- MultipleChunks(oper_Time, oper_Name, scenario[i, 1])
    chunk_process <- ExtChunk(scenario[i, 1], oper_Name, Time_current, Time_past, WM_A, oper_Time, num_of_chunks, chk_Life_A)
    WM_A <- chunk_process[[1]]
    chk_Life_A <- chunk_process[[2]]
    Time_past <- Time_current

    num_Oper <- GetNumOper(oper_Name, num_Oper, oper_Time)
  }

  # # Task completion time
  # cat(sprintf("%.2f", Time_current/1000), " seconds")
  #
  # # Memory chunk
  # MC <- GetMemoryChunk(chk_Life_A, Time_current)
  # cat(sprintf("%.2f", MC[[2]]), "chunks")
  #
  # # number of operators
  # num_Oper


  # old Model
  # while(Time_current < exp_time) {
  #
  #   # initiate reading the line
  #   i <- i + 1
  #
  #   op_Name <- ExtOper(scenario[i, 1])
  #
  #   # This "if" is for error process
  #   if (op_Name != "Goal") {
  #
  #     # operator time for motor gestures to deal with errors
  #     # if(op_Name == "Flexion" | op_Name == "Extension" | op_Name == "Grasp" && Err_inclusion == 1) {
  #     if(op_Name == "Flexion" | op_Name == "Extension" | op_Name == "Grasp"
  #        | op_Name == "Open_MTM" | op_Name == "Turn_MTM" | op_Name == "Move_MTM") {
  #
  #       err_List <- RetrOpTime_Motor(op_Name, i)
  #
  #       # if there was an error (err_List[[2]] > 0), do all model run (update the chunk information)
  #       while(err_List[[2]] > 0) {
  #
  #         op_Time <- err_List[[1]]
  #         Time_current <- Time_current + as.numeric(op_Time)
  #
  #         # chunk process for the operator
  #         num_of_chks <- MultipleChunks(op_Time, op_Name, scenario[i, 1])
  #         chk_process_op <- ExtChunk(scenario[i, 1], op_Name, Time_current, Time_past, WM_A, op_Time, num_of_chks, chk_Life_A)
  #         WM_A <- chk_process_op[[1]]
  #         chk_Life_A <- chk_process_op[[2]]
  #         # chunk process for error chunk
  #         err_line <- GetLOI(op_Name, err_List[3])
  #         num_of_chks <- MultipleChunks(op_Time, op_Name, err_line)
  #         chk_process_err <- ExtChunk(err_line, op_Name, Time_current, Time_past, WM_A, op_Time, num_of_chks, chk_Life_A)
  #         WM_A <- chk_process_err[[1]]
  #         chk_Life_A <- chk_process_err[[2]]
  #
  #         # count the number of motor operators that caused errors
  #         if (err_List[[4]] == 1)
  #           Motor_err <- Motor_err + 1
  #
  #         # time update
  #         Time_past <- Time_current
  #
  #         # count and update the number of operator
  #         num_Oper <- GetNumOper(op_Name, num_Oper, oper_Time)
  #
  #         # attempt to the next operator
  #         err_List <- RetrOpTime_Motor(op_Name, 2)
  #       }
  #     }
  #
  #     op_Time <- RetrievingOperTime(op_Name, i, scenario, time_library)
  #     Time_current <- Time_current + as.numeric(op_Time)
  #
  #     num_of_chks <- MultipleChunks(op_Time, op_Name, scenario[i, 1])
  #     chk_process <- ExtChunk(scenario[i, 1], op_Name, Time_current, Time_past, WM_A, op_Time, num_of_chks, chk_Life_A)
  #     WM_A <- chk_process[[1]]
  #     chk_Life_A <- chk_process[[2]]
  #     Time_past <- Time_current
  #
  #     num_Oper <- GetNumOper(op_Name, num_Oper, op_Time)
  #   }
  #
  #   # add time to adjust pins in a good position
  #   if( (i == nrow(scenario)) && (cycle_counter %% 3 == 0) ) {
  #     Time_current <- Time_current + 5000
  #   }
  #   # Reset the reading line number to 0 if one cycle ends with "plus 1" in pin_Num
  #   if(i == nrow(scenario)) {
  #     i <- 0
  #     pin_Num <- pin_Num + 1
  #     cycle_counter <- cycle_counter + 1
  #   }
  # }



  ################### model outcome ########################
  # Working memory chunk information
  WM_A

  # Task completion time - for one cycle
  # cat(sprintf("%.2f", Time_current/1000), " seconds")
  TCT <- Time_current/1000


  # L <- CalcLearnability(control_scheme,
  #                       training_threshold,
  #                       theta,
  #                       CQ,
  #                       LoT,
  #                       slope_P,
  #                       slope_M,
  #                       intercept_P,
  #                       intercept_M)



  # Error



  # Number of pins moved WITH errors
  # print(pin_Num)

  # Chunk lifecyle
  # MC_A[[1]]

  # number of total operators
  num_Oper

  ###################  Usability - Memorability ##############
  MC_A <- GetMemoryChunk(chk_Life_A, Time_current) # MC_A = Memory chunks of A-CPM
  # cat("Memorability: ", sprintf("%.2f", MC_A[[2]]), "chunks")

  ###################  Usability - Efficiency ###############
  # One_cycle <- Time_current/1000/pin_Num
  # cat("Efficiency :", sprintf("%.2f", One_cycle), " seconds for one cycle (moving one pin from one bar to another bar)")

  ###################  Usability - Errors ###################
  # cat("Errors :", sprintf("%.2f", Motor_err), " errors happened")

  # ###################  Usability - Learnability #############
  # LB <- GetLearnability(raw_trn, hook_calib_qt, DC_mode, DC_gesture, PR_mode, PR_gesture, slope)
  # # cat("Learnability :", sprintf("%.2f", LB[[1]]), " trials are required")
  #
  # ###################  Usability - Satisfaction #############
  # STF_Des <- GetDesire(stf_dim, stf_wgt, stf_wear, stf_safe, stf_dura, stf_control, stf_comfort)
  # # STF_Des
  # STF_Perf <- GetPcvdPerf(LB, pin_Num)
  # # STF_Perf
  # STF <- GetSatisfaction(STF_Perf, STF_Des, wts_perf, wts_desire)
  # # STF

  ###################  Usability - Cognitive workload #############
  # CW_R <- LassoMain()

  final_R <- list(Working_memory = WM_A
                  , TCT = TCT
                  , The_number_of_pins = 120/TCT
                  , Memory_chunks_structure = MC_A[[1]]
                  , The_number_of_operators = num_Oper
                  , Memory_chunks = round(MC_A[[2]], 2)
                  ) #, CW_R
  return(final_R)
}


#' test_Shiny_Inputs
#'
#' @param config
#' @param thre
#' @param theta
#' @param cq
#' @param lot
#' @param resil_p
#' @param resil_m
#' @param wl_p
#' @param wl_m
#'
#' @return
#' @export
#'
#' @examples
test_Shiny_Inputs <- function(config, thre, theta, cq, lot, resil_p, resil_m, wl_p, wl_m) {
  print(config)
  print(thre)
  print(theta)
  print(cq)
  print(lot)
  print(resil_p)
  print(resil_m)
  print(wl_p)
  print(wl_m)
}

