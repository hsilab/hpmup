#' CalcSatisfaction
#'
#' @param L
#' @param Effi
#' @param thre_min
#' @param thre_max
#' @param Att
#'
#' @return
#' @export
#'
#' @examples
CalcSatisfaction <- function(L, Effi, thre_min, thre_max, Att) {

  if (Att < 0.5) {

    return ( 1-(0.5-Att)*2 )

  } else if (Att >= 0.75) {

    Att <- (Att - 0.5)*2
    real <- Effi
    expectation <- 120/(thre_min + (thre_max-thre_min)/(2*17)*L)
    return ( (Att + 0.65) * real / expectation )

  } else {

    return (Att - 0.5 + 0.65)

  }

  # if (Att < 0) {
  #   return ( 1-Att )
  #
  # } else if (Att > 0.5) {
  #
  #   real <- Effi
  #   expectation <- 120/(thre_min + (thre_max-thre_min)/(2*17)*L)
  #   return ( (Att + 0.65)*real/expectation )
  #
  # } else {
  #
  #   return ( Att + 0.65 )
  #
  # }
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

#' GetPcvdPerf()
#'
#' @description Expectation on performance. Calculate the expected performance in actual trial
#' @param Learn Not used
#' @param num_of_pins Number of moved pins
#'
#' @return Expected number of moved pins in the actual trial
#' @export
#'
#' @examples
#' GetPcvdPerf(LB, pin_Num)
GetPcvdPerf <- function (Learn, num_of_pins) {

  # calculate the range of performance based on three trials that passed training criteria
  max_expected_pins <- (exp_time/1000)/(Learn[3]/3) # maximum number of pins depends on the last trial from the best three training trials
  min_expected_pins <- (exp_time/1000)/(Learn[1]/3) # minimum number of pins depends on the first trial from the best three training trials

  expectation <- 0
  # if the actually moved pins are greater than the minimum expectation, then return the maximum "Perceived performance" which is 1.
  if (num_of_pins >= min_expected_pins)
    return(1)
  # if not, return the ratio of actual pins and min expected pins
  else if (num_of_pins < min_expected_pins)
    return(num_of_pins/min_expected_pins)
}

#' GetDesire()
#'
#' @description Return the average of desire
#' @param dim Dimension: size, height, length, width
#' @param wgt Weight
#' @param wear Ease in adjusting (fixing or fastening) the parts of the assistive device
#' @param safe Safety or security
#' @param dura Durability (endurance or resistance to wear)
#' @param control Ease of use
#' @param comfort Comfortability
#'
#' @return The average of desire
#' @export
#'
#' @examples
#' GetDesire(dim, wgt, wear, safe, dura, control, comfort)
GetDesire <- function (dim, wgt, wear, safe, dura, control, comfort) {
  avg_stf <- (dim + wgt + wear + safe + dura + control + comfort)/7
  return (avg_stf)
}

#' GetSatisfaction()
#'
#' @description Calculate the weighted average of satisfaction
#' @param per_perf Expected number of moved pins in the actual trial
#' @param desire The average of desire
#' @param wts_perf Weight of perceived performance
#' @param wts_desire Weight of desire
#'
#' @return
#' @export
#'
#' @examples
#' GetSatisfaction(per_perf, desire, wts_perf, wts_desire)
GetSatisfaction <- function (per_perf, desire, wts_perf, wts_desire) {
  scaled_STF <- (per_perf*wts_perf + desire*wts_desire)*100
  return (scaled_STF)
}
