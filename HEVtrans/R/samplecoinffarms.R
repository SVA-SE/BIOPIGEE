#' Sample co-infected farms
#'
#' @description \code{samplecoinffarms} Create a subset of farms that are co-infected
#'
#' @usage samplecoinffarms(farmlist, free_prob, id, farmtype, sampledtyp)
#'
#' @param farmlist data.frame including type and id columns
#' @param id column name indicating site id in the farmlist dataframe
#' @param farmtype column name indicating farm type in the farmlist dataframe
#' @param sampledtype type of farms amoung with coinfected farms will be sampled
#' @param free_prob probability of FF farms to be IMV-free
#'
#' @return Vector containing the sampled farms id
#' @examples free_FF <- samplecoinffarms(farmlist, free_prob)
#'
#' @export

samplecoinffarms <- function(farmlist, free_prob, id = "siteID", farmtype = "type", sampledtype ="FF") {

  sample(x    = farmlist[ which(farmlist[,farmtype] %in% sampledtype), id],
         size = free_prob * length(farmlist[ which(farmlist[,farmtype] %in% sampledtype), id])
         )

}
