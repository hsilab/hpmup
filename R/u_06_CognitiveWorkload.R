#' @import glmnet caret dplyr e1071
NULL

#' LassoMain()
#'
#' @description Run LASSO regression
#' @return Probability of each class
#' @export
#'
#' @examples
#' LassoMain()
LassoMain <- function() {

  # Split - data.frame
  t_index <- sample(1:nrow(norm_CRT), size=nrow(norm_CRT)*0.7)
  dat_train <- norm_CRT[t_index, ]
  dat_test <- norm_CRT[-t_index, ]

  # Build the model
  lambda <- 10^seq(-3, 3, length = 100)

  lasso <- train(
    factor(CW_G4) ~ PCPS + BR + perf_CRT + TCT_cycle + nC + nP + nM + TCT_CPM + ML
    , data = dat_train
    , method = "glmnet"
    , trControl = trainControl("cv", number = 10)
    , tuneGrid = expand.grid(alpha = 1, lambda = lambda)
  )

  coef(lasso$finalModel, lasso$bestTune$lambda)

  predictions <- lasso %>% predict(dat_test[,5:13])

  # Accuracy
  matched_case <- 0
  for(i in 1:length(predictions)) {
    if(predictions[i] == dat_test$CW_G4[i])
      matched_case <- matched_case + 1
  }
  acc_lasso <- matched_case/length(predictions)*100
  acc_lasso

  # Equation
  lss_PCPS <- 0.73
  lss_BR <- 0.48
  lss_perf_CRT <- 0.47
  lss_TCT_cycle <- 0.49
  lss_nC <- 0.31
  lss_nP <- 0.44
  lss_nM <- 0.42
  lss_TCT_CPM <- 0.23
  lss_ML <- 0

  lasso_coef <- coef(lasso$finalModel, lasso$bestTune$lambda)

  for (i in 1:4) {
    if (i == 1) { # high
      exp_High <- exp(lasso_coef[[i]][1]
                      + lasso_coef[[i]][2] * lss_PCPS
                      + lasso_coef[[i]][3] * lss_BR
                      + lasso_coef[[i]][4] * lss_perf_CRT
                      + lasso_coef[[i]][5] * lss_TCT_cycle
                      + lasso_coef[[i]][6] * lss_nC
                      + lasso_coef[[i]][7] * lss_nP
                      + lasso_coef[[i]][8] * lss_nM
                      + lasso_coef[[i]][9] * lss_TCT_CPM
                      + lasso_coef[[i]][10] * lss_ML)
    } else if (i == 2 ) { # low
      exp_Low <- exp(lasso_coef[[i]][1]
                     + lasso_coef[[i]][2] * lss_PCPS
                     + lasso_coef[[i]][3] * lss_BR
                     + lasso_coef[[i]][4] * lss_perf_CRT
                     + lasso_coef[[i]][5] * lss_TCT_cycle
                     + lasso_coef[[i]][6] * lss_nC
                     + lasso_coef[[i]][7] * lss_nP
                     + lasso_coef[[i]][8] * lss_nM
                     + lasso_coef[[i]][9] * lss_TCT_CPM
                     + lasso_coef[[i]][10] * lss_ML)
    } else if (i == 3 ) { # moderate
      exp_Moderate <- exp(lasso_coef[[i]][1]
                          + lasso_coef[[i]][2] * lss_PCPS
                          + lasso_coef[[i]][3] * lss_BR
                          + lasso_coef[[i]][4] * lss_perf_CRT
                          + lasso_coef[[i]][5] * lss_TCT_cycle
                          + lasso_coef[[i]][6] * lss_nC
                          + lasso_coef[[i]][7] * lss_nP
                          + lasso_coef[[i]][8] * lss_nM
                          + lasso_coef[[i]][9] * lss_TCT_CPM
                          + lasso_coef[[i]][10] * lss_ML)
    } else if (i == 4 ) { # very low
      exp_Very_low <- exp(lasso_coef[[i]][1]
                          + lasso_coef[[i]][2] * lss_PCPS
                          + lasso_coef[[i]][3] * lss_BR
                          + lasso_coef[[i]][4] * lss_perf_CRT
                          + lasso_coef[[i]][5] * lss_TCT_cycle
                          + lasso_coef[[i]][6] * lss_nC
                          + lasso_coef[[i]][7] * lss_nP
                          + lasso_coef[[i]][8] * lss_nM
                          + lasso_coef[[i]][9] * lss_TCT_CPM
                          + lasso_coef[[i]][10] * lss_ML)
    }
  }

  pr_High <- exp_High/(1 + exp_Low + exp_Moderate + exp_Very_low)
  pr_Low <- exp_Low/(1 + exp_Low + exp_Moderate + exp_Very_low)
  pr_Moderate <- exp_Moderate/(1 + exp_Low + exp_Moderate + exp_Very_low)
  pr_Very_low <- exp_Very_low/(1 + exp_Low + exp_Moderate + exp_Very_low)

  lasso_summ <- lasso_Summary()
  lasso_summ
  lasso_Prob <- lasso_prob(pr_High, pr_Low, pr_Moderate, pr_Very_low, lasso_summ)
  lasso_Prob
  lasso_Pred <- lasso_pred_Result(lasso_Prob)
  lasso_Pred

  return(lasso_Pred)
}

#' lasso_Summary()
#'
#' @description Create a data table for LASSO summary
#' @return An empty data frame
#' @export
#'
#' @examples
#' lasso_Summary()
lasso_Summary <- function() {
  lss_summ <- c()
  High <- c()
  Moderate <- c()
  Low <- c()
  Very_low <- c()

  lss_summ <- data.frame(High, Moderate, Low, Very_low)
  lss_summ <- rbind(lss_summ, c(0,0,0, 0))

  colnames(lss_summ)[1]<-"Pr(High)"
  colnames(lss_summ)[2]<-"Pr(Moderate)"
  colnames(lss_summ)[3]<-"Pr(Low)"
  colnames(lss_summ)[4]<-"Pr(Very Low)"
  return(lss_summ)
}

#' lasso_prob()
#'
#' @description Calculate the probability of each class
#' @param pr_High Probability of "High"
#' @param pr_Low Probability of "Low"
#' @param pr_Moderate Probability of "Moderate"
#' @param pr_Very_low Probability of "Very low"
#' @param lss_summ Summary table
#'
#' @return Prabability table for each class
#' @export
#'
#' @examples
#' lasso_prob(pr_High, pr_Low, pr_Moderate, pr_Very_low, lss_summ)
lasso_prob <- function (pr_High, pr_Low, pr_Moderate, pr_Very_low, lss_summ) {
  lss_summ[1]<-pr_High
  lss_summ[2]<-pr_Moderate
  lss_summ[3]<-pr_Low
  lss_summ[4]<-pr_Very_low
  return(lss_summ)
}

#' lasso_pred_Result()
#'
#' @description Extract the highest probability
#' @param lasso_prob_table Summary table
#'
#' @return The class with highest probability
#' @export
#'
#' @examples
#' lasso_pred_Result(lasso_prob_table)
lasso_pred_Result <- function (lasso_prob_table) {
  for (i in 1:4) {
    if (max(lasso_prob_table) == lasso_prob_table[i])
      return(colnames(lasso_prob_table[i]))
  }
}
