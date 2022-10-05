#'  Create gdata
#'
#' @description \code{create_gdata}  Define general parameters that are identicals for all farms
#'
#' @usage create_gdata(lambda,    decRate,    SFRate,    cleanRate,    qIng,    mu,    qFeces_sow,    qFeces_pig)
#'
#' @param lambda Duration of maternal immunity (/days)
#' @param decRate HEV decay rate in the environment (/day)
#' @param SFRate Faeces elimination rate through slatted floor (/day)
#' @param cleanRate Faeces removal rate by cleaning
#' @param qIng Average quantity of faeces ingested by a pig (g/day)
#' @param mu Duration of active immunity (/days)
#' @param qFeces_sow Average amount of faeces shed by a sow (g/day)
#' @param qFeces_pig Average amount of faeces shed by a pig (g/day)
#'
#' @return Vector containing general parameters required by the model
#' @examples gdata <- create_gdata(lambda = 45.8, decRate=0.08, SFRate=0.70, cleanRate = 0.98, qIng = 25, mu = 1/30, qFeces_sow = 2000, qFeces_pig = 550)
#'
#' @noRd

create_gdata <- function(lambda  = 1/45.8,
                         decRate = 0.08,
                         SFRate  = 0.70,
                         # cleanRate = 0, # FIX ME - remove from gdata - use ldata
                         qIng    = 25,
                         mu      = 1/30,
                         qFeces_sow = 2000,
                         qFeces_pig = 550) {

  c(lambda  = lambda,
    decRate = decRate,
    SFRate  = SFRate,
    # cleanRate = cleanRate,
    qIng = qIng,
    mu = mu,
    qFeces_sow = qFeces_sow, # Murai, K. et al - 2018. Mathematical modeling of porcine epidemic diarrhea virus dynamics within a farrow-to-finish swine farm to investigate the effects of control measures. Prev. Vet. Med. 149, 115–124.
    qFeces_pig = qFeces_pig)
}
