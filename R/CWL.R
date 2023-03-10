#' Title GetPredict
#'
#' @param pcps pupil change of percentage size
#' @param blink blink rate
#' @param perf task performance
#' @param tct_cycle one cycle task performance
#' @param nC number of cognitive operators
#' @param nP number of perceptual operators
#' @param nM number of motor operators
#' @param tct_CPM TCT from CPM
#' @param mem_load memory load
#' @param training number of required training trials
#' @param task_type CRT or SHAP
#' @param config DC, PR, or CC
#'
#' @return
#' @export
#'
#' @examples skipped
GetPredict <- function(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config, task_type) {

  # (config_DC, config_PR) -> 1, -1 -> DC
  # (config_DC, config_PR) -> -1, 1 -> PR
  # (config_DC, config_PR) -> -1, -1 -> CC

  if (config == 1) {
    config_DC <- 1
    config_PR <- -1
  } else if (config == 2) {
    config_DC <- -1
    config_PR <- 1
  } else {
    config_DC <- -1
    config_PR <- -1
  }

  if (task_type == 1)
    returnedPred <- GetPredCRT(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR)
  else
    returnedPred <- GetPredSHAP(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR)

  return (returnedPred)
}

#' GetPredCRT
#'
#' @param pcps pcps
#' @param blink blink
#' @param perf perf
#' @param tct_cycle tct_cycle
#' @param nC nC
#' @param nP nP
#' @param nM n<
#' @param tct_CPM tct_CPM
#' @param mem_load mem_load
#' @param training training
#' @param config_DC config_DC
#' @param config_PR config_PR
#'
#' @return
#' @export
#'
#' @examples
GetPredCRT <- function (pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR) {

  ####### cluster 2
  testmodelling_c2 <- readRDS("data/CRT_RF_cluster_2.rds") #replace with your file path
  print("Cluster 2 rds file load was successful")

  data_c2 <- data.frame(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR)
  print("Cluster 2 data frame was successful")

  names(data_c2) <- c("pcps", "blink", "perf", "tct_cycle", "nC", "nP", "nM", "tct_CPM", "mem_load", "training", "config_DC", "config_PR")
  #print(a)

  rf_c2 = testmodelling_c2 %>% predict(data_c2) # %>% bind_cols(data_c2 %>% dplyr::select(target))
  new_rf_pred_c2 = testmodelling_c2 %>% predict(data_c2, type = "prob") # %>% bind_cols(data_c2 %>% dplyr::select(target))
  #print(new_rf_pred)

  print("Cluster 2 prediction was successful")

  # in cluster_2, 1 is high and 2 is low
  # new_rf_pred_wo <- new_rf_pred_c2 %>% select(c(-target))
  # classified_c2_index <- which(new_rf_pred_wo == max(new_rf_pred_wo))
  classified_c2_index <- which(new_rf_pred_c2 == max(new_rf_pred_c2))
  classified_c2_class <- 0

  print("Cluster 2 Indexing was successful")

  if (classified_c2_index == 1)
    classified_c2_class <- "High"
  else
    classified_c2_class <- "Low"

  print("Cluseter 2 is successful")

  # c2_cluster average
  c2_cluster_1 <- 68.01
  c2_cluster_2 <- 45.08
  c2_wtd_avg <- new_rf_pred_c2[[1]][1] * c2_cluster_1 + new_rf_pred_c2[[2]][1] * c2_cluster_2


  ####### cluster 3
  testmodelling_c3 <- readRDS("data/CRT_RF_cluster_3.rds") #replace with your file path
  print("Cluster 3 rds file load was successful")

  data_c3 <- data.frame(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR)
  names(data_c3) <- c("pcps", "blink", "perf", "tct_cycle", "nC", "nP", "nM", "tct_CPM", "mem_load", "training", "config_DC", "config_PR")
  #print(a)

  rf_c3 = testmodelling_c3 %>% predict(data_c3) # %>% bind_cols(data_c3 %>% dplyr::select(target))
  new_rf_pred_c3 = testmodelling_c3 %>% predict(data_c3, type = "prob") # %>% bind_cols(data_c3 %>% dplyr::select(target))
  #print(new_rf_pred)

  # in cluster_3, 1 is Low, 2 is High, and 3 is Moderate
  # new_rf_pred_wo <- new_rf_pred_c3 %>% select(c(-target))
  classified_c3_index <- which(new_rf_pred_c3 == max(new_rf_pred_c3))
  classified_c3_class <- 0

  if (classified_c3_index == 1)
    classified_c3_class <- "Low"
  else if (classified_c3_index == 2)
    classified_c3_class <- "High"
  else
    classified_c3_class <- "Moderate"

  print("Cluseter 3 is successful")

  # c3_cluster average
  c3_cluster_1 <- 20.83
  c3_cluster_2 <- 68.82
  c3_cluster_3 <- 49.19
  c3_wtd_avg <- new_rf_pred_c3[[1]][1] * c3_cluster_1 + new_rf_pred_c3[[2]][1] * c3_cluster_2 + new_rf_pred_c3[[3]][1] * c3_cluster_3


  ####### cluster 4
  testmodelling_c4 <- readRDS("data/CRT_RF_cluster_4.rds") #replace with your file path
  print("Cluster 4 rds file load was successful")

  data_c4 <- data.frame(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR)
  names(data_c4) <- c("pcps", "blink", "perf", "tct_cycle", "nC", "nP", "nM", "tct_CPM", "mem_load", "training", "config_DC", "config_PR")
  #print(a)

  rf_c4 = testmodelling_c4 %>% predict(data_c4) # %>% bind_cols(data_c4 %>% dplyr::select(target))
  new_rf_pred_c4 = testmodelling_c4 %>% predict(data_c4, type = "prob") # %>% bind_cols(data_c4 %>% dplyr::select(target))
  #print(new_rf_pred)

  # in cluster_4, 1 is Low, 2 is Very high, 3 is High, and 4 is Moderate
  # new_rf_pred_wo <- new_rf_pred_c4 %>% select(c(-target))
  classified_c4_index <- which(new_rf_pred_c4 == max(new_rf_pred_c4))
  classified_c4_class <- 0

  if (classified_c4_index == 1)
    classified_c4_class <- "Low"
  else if (classified_c4_index == 2)
    classified_c4_class <- "Very high"
  else if (classified_c4_index == 3)
    classified_c4_class <- "High"
  else
    classified_c4_class <- "Moderate"

  print("Cluseter 4 is successful")

  # c4_cluster average
  c4_cluster_1 <- 38.68
  c4_cluster_2 <- 80.15
  c4_cluster_3 <- 70.30
  c4_cluster_4 <- 57.80
  c4_wtd_avg <- new_rf_pred_c4[[1]][1] * c4_cluster_1 + new_rf_pred_c4[[2]][1] * c4_cluster_2 + new_rf_pred_c4[[3]][1] * c4_cluster_3 + new_rf_pred_c4[[4]][1] * c4_cluster_4


  ####### hist 4
  testmodelling_hist4 <- readRDS("data/CRT_RF_hist_4.rds") #replace with your file path
  print("Hist 4 rds file load was successful")

  data_hist4 <- data.frame(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR)
  names(data_hist4) <- c("pcps", "blink", "perf", "tct_cycle", "nC", "nP", "nM", "tct_CPM", "mem_load", "training", "config_DC", "config_PR")
  #print(a)

  rf_hist4 = testmodelling_hist4 %>% predict(data_hist4) # %>% bind_cols(data_hist4 %>% dplyr::select(target))
  new_rf_pred_hist4 = testmodelling_hist4 %>% predict(data_hist4, type = "prob") # %>% bind_cols(data_hist4 %>% dplyr::select(target))
  #print(new_rf_pred)

  # in hist_4, 1 is Low, 2 is Moderate, 3 is High, and 4 is Very High
  # new_rf_pred_wo <- new_rf_pred_hist4 %>% select(c(-target))
  classified_hist4_index <- which(new_rf_pred_hist4 == max(new_rf_pred_hist4))
  classified_hist4_class <- 0

  if (classified_hist4_index == 1)
    classified_hist4_class <- "Low"
  else if (classified_hist4_index == 2)
    classified_hist4_class <- "Moderate"
  else if (classified_hist4_index == 3)
    classified_hist4_class <- "High"
  else
    classified_hist4_class <- "Very high"

  print("Hist 4 is successful")

  # hist4_cluster average
  hist4_cluster_1 <- 16.83
  hist4_cluster_2 <- 45.42
  hist4_cluster_3 <- 64.16
  hist4_cluster_4 <- 80.60
  hist4_wtd_avg <- new_rf_pred_hist4[[1]][1] * hist4_cluster_1 + new_rf_pred_hist4[[2]][1] * hist4_cluster_2 + new_rf_pred_hist4[[3]][1] * hist4_cluster_3 + new_rf_pred_hist4[[4]][1] * hist4_cluster_4


  ####### Final class and avg
  class_final <- paste(classified_c2_class, ", ", classified_c3_class, ", ", classified_c4_class, ", ", classified_hist4_class)
  reg_final <- mean(c2_wtd_avg, c3_wtd_avg, c4_wtd_avg, hist4_wtd_avg)
  return_final <- paste(class_final, ", ", reg_final)

  return(return_final)

}


#' GetPredSHAP
#'
#' @param pcps pcps
#' @param blink blink
#' @param perf perf
#' @param tct_cycle tct_cycle
#' @param nC nC
#' @param nP nP
#' @param nM nM
#' @param tct_CPM tct_CPM
#' @param mem_load mem_load
#' @param training training
#' @param config_DC config_DC
#' @param config_PR config_PR
#'
#' @return
#' @export
#'
#' @examples
GetPredSHAP <- function (pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR) {

  ####### cluster 2
  testmodelling_c2 <- readRDS("data/SHAP_RF_cluster_2.rds") #replace with your file path
  print("Cluster 2 rds file load was successful")

  data_c2 <- data.frame(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR)
  print("Cluster 2 data frame was successful")

  names(data_c2) <- c("pcps", "blink", "perf", "tct_cycle", "nC", "nP", "nM", "tct_CPM", "mem_load", "training", "config_DC", "config_PR")
  #print(a)

  rf_c2 = testmodelling_c2 %>% predict(data_c2) # %>% bind_cols(data_c2 %>% dplyr::select(target))
  new_rf_pred_c2 = testmodelling_c2 %>% predict(data_c2, type = "prob") # %>% bind_cols(data_c2 %>% dplyr::select(target))
  #print(new_rf_pred)

  print("Cluster 2 prediction was successful")

  # in cluster_2, 1 is high and 2 is low
  # new_rf_pred_wo <- new_rf_pred_c2 %>% select(c(-target))
  # classified_c2_index <- which(new_rf_pred_wo == max(new_rf_pred_wo))
  classified_c2_index <- which(new_rf_pred_c2 == max(new_rf_pred_c2))
  classified_c2_class <- 0

  print("Cluster 2 Indexing was successful")

  if (classified_c2_index == 1)
    classified_c2_class <- "High"
  else
    classified_c2_class <- "Low"

  print("Cluseter 2 is successful")

  # c2_cluster average
  c2_cluster_1 <- 64.39
  c2_cluster_2 <- 42.70
  c2_wtd_avg <- new_rf_pred_c2[[1]][1] * c2_cluster_1 + new_rf_pred_c2[[2]][1] * c2_cluster_2


  ####### cluster 3
  testmodelling_c3 <- readRDS("data/SHAP_RF_cluster_3.rds") #replace with your file path
  print("Cluster 3 rds file load was successful")

  data_c3 <- data.frame(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR)
  names(data_c3) <- c("pcps", "blink", "perf", "tct_cycle", "nC", "nP", "nM", "tct_CPM", "mem_load", "training", "config_DC", "config_PR")
  #print(a)

  rf_c3 = testmodelling_c3 %>% predict(data_c3) # %>% bind_cols(data_c3 %>% dplyr::select(target))
  new_rf_pred_c3 = testmodelling_c3 %>% predict(data_c3, type = "prob") # %>% bind_cols(data_c3 %>% dplyr::select(target))
  #print(new_rf_pred)

  # in cluster_3, 1 is Low, 2 is High, and 3 is Moderate
  # new_rf_pred_wo <- new_rf_pred_c3 %>% select(c(-target))
  classified_c3_index <- which(new_rf_pred_c3 == max(new_rf_pred_c3))
  classified_c3_class <- 0

  if (classified_c3_index == 1)
    classified_c3_class <- "High"
  else if (classified_c3_index == 2)
    classified_c3_class <- "Moderate"
  else
    classified_c3_class <- "Low"

  print("Cluseter 3 is successful")

  # c3_cluster average
  c3_cluster_1 <- 69.17
  c3_cluster_2 <- 52.21
  c3_cluster_3 <- 33.39
  c3_wtd_avg <- new_rf_pred_c3[[1]][1] * c3_cluster_1 + new_rf_pred_c3[[2]][1] * c3_cluster_2 + new_rf_pred_c3[[3]][1] * c3_cluster_3


  ####### cluster 4
  testmodelling_c4 <- readRDS("data/SHAP_RF_cluster_4.rds") #replace with your file path
  print("Cluster 4 rds file load was successful")

  data_c4 <- data.frame(pcps, blink, perf, tct_cycle, nC, nP, nM, tct_CPM, mem_load, training, config_DC, config_PR)
  names(data_c4) <- c("pcps", "blink", "perf", "tct_cycle", "nC", "nP", "nM", "tct_CPM", "mem_load", "training", "config_DC", "config_PR")
  #print(a)

  rf_c4 = testmodelling_c4 %>% predict(data_c4) # %>% bind_cols(data_c4 %>% dplyr::select(target))
  new_rf_pred_c4 = testmodelling_c4 %>% predict(data_c4, type = "prob") # %>% bind_cols(data_c4 %>% dplyr::select(target))
  #print(new_rf_pred)

  # in cluster_4, 1 is Low, 2 is Very high, 3 is High, and 4 is Moderate
  # new_rf_pred_wo <- new_rf_pred_c4 %>% select(c(-target))
  classified_c4_index <- which(new_rf_pred_c4 == max(new_rf_pred_c4))
  classified_c4_class <- 0

  if (classified_c4_index == 1)
    classified_c4_class <- "High"
  else if (classified_c4_index == 2)
    classified_c4_class <- "Very high"
  else if (classified_c4_index == 3)
    classified_c4_class <- "Low"
  else
    classified_c4_class <- "Moderate"

  print("Cluseter 4 is successful")

  # c4_cluster average
  c4_cluster_1 <- 64.37
  c4_cluster_2 <- 80.03
  c4_cluster_3 <- 30.06
  c4_cluster_4 <- 49.68
  c4_wtd_avg <- new_rf_pred_c4[[1]][1] * c4_cluster_1 + new_rf_pred_c4[[2]][1] * c4_cluster_2 + new_rf_pred_c4[[3]][1] * c4_cluster_3 + new_rf_pred_c4[[4]][1] * c4_cluster_4


  ####### Final class and avg
  class_final <- paste(classified_c2_class, ", ", classified_c3_class, ", ", classified_c4_class)
  reg_final <- mean(c2_wtd_avg, c3_wtd_avg, c4_wtd_avg)
  return_final <- paste(class_final, ", ", reg_final)

  return(return_final)

}
