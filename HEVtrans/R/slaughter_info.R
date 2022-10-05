#' Get the time points when slaughter happens
#'
#' @description \code{slaughter_info}  Get the time points when slaughter happens
#'
#' @usage slaughter_info(events, day_intro)
#'
#' @param events data.frame of events
#' @param day_intro day of exposed animal introduction
#'
#' @return Two numbers vector
#' @examples tspan <- slaughter_info(events, day_intro)
#'
#' @export

slaughter_info <- function(events, day_intro){
  slaughter        <- events[which(events$event=="exit" & events$type %in% c("fi2sl", "renew_out")),][,c("time", "node")]
  slaughter_time   <- as.numeric(unique(slaughter[,"time"]))
  simulation_time  <- max(events[,"time"])
  tspan            <- sort(unique(c(1, seq(day_intro+1, simulation_time, by = 100), slaughter_time)))
  tspan            <- tspan[which(tspan > (day_intro-1))]
  list(slaughter=slaughter,tspan=c(1, tspan))
}
