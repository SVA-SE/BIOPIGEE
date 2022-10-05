##' Sum shipped animals
##'
##' @description \code{network_nodes} sum animals shipped from a given
##'     sector of a given farm to a given sector of a given farm
##'
##' @usage network_nodes(data)
##'
##' @param dataset data.frame including transported animal number and
##'     origin and destination ids, sectors and farming types columns
##'
##' @return data.frame
##' @examples result_nAnim <- network_nodes(data)
##'
##' @export
network_nodes <- function(dataset) {
    sumAnim <- aggregate(nAnim ~ from_id + to_id + sector_from + sector_to + from_type + to_type, data = dataset, sum)

    animSum <- aggregate(data = sumAnim, nAnim ~ from_id + sector_from + sector_to, sum)
    colnames(animSum)[4] <- "nAnimFrom"

    merge(sumAnim, animSum)
}
