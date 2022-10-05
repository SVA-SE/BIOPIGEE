##' Class \code{HEV}
##'
##' Class to handle the \code{HEV} \code{SimInf_model}.
##' @export
setClass("HEV", contains = "SimInf_model")

##' Create a model for the SimInf framework
##'
##' Create a model to be used by the SimInf framework.
##' @param farms List of farms.
##' @param tspan A vector of increasing time points
##'     where the state of each node is to be returned.
##' @param days the number of days to generate events.
##' @import SimInf
##' @importFrom methods as
##' @importFrom HEVevents create_events
##' @export
HEV <- function(farms, tspan = NULL, days = NULL,
                lambda  = 1/45.8,
                decRate = 0.08,
                SFRate  = 0.70,
                # cleanRate = 0, # FIX ME - remove from gdata - use ldata
                qIng    = 25,
                mu      = 1/30,
                qFeces_sow = 2000,
                qFeces_pig = 550) {
    G <- structure(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 1), .Dim = c(20L, 20L), .Dimnames = list(c("S1 -> (S1*(beta*(I1+I2)+betaEWP*phi*qIng+betaEBAP*qIng*(Ql+Qr)))/(S1+E1+I1+R11+R12+R13+R14+R15+R16+S2+E2+I2+R21+R22+R23+R24+R25+R26) -> E1",
    "E1 -> lat*E1 -> I1", "I1 -> gamma*I1 -> R11", "R11 -> mu*R11 -> R12",
    "R12 -> mu*R12 -> R13", "R13 -> mu*R13 -> R14", "R14 -> mu*R14 -> R15",
    "R15 -> mu*R15 -> R16", "R16 -> mu*R16 -> S1", "S2 -> (R11+R12+R13+R14+R15+R16)>0?1000*S2:0 -> M2",
    "M2 -> lambda*M2 -> S2", "S2 -> (S2*(beta*(I1+I2)+betaEWP*phi*qIng+betaEBAP*qIng*(Ql+Qr)))/(S1+E1+I1+R11+R12+R13+R14+R15+R16+S2+E2+I2+R21+R22+R23+R24+R25+R26) -> E2",
    "E2 -> lat*E2 -> I2", "I2 -> gamma*I2 -> R21", "R21 -> mu*R21 -> R22",
    "R22 -> mu*R22 -> R23", "R23 -> mu*R23 -> R24", "R24 -> mu*R24 -> R25",
    "R25 -> mu*R25 -> R26", "R26 -> mu*R26 -> S2"), c("1", "2", "3",
    "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
    "16", "17", "18", "19", "20")))

    S <- structure(c(-1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 0, -1), .Dim = 19:20, .Dimnames = list(c("S1", "E1", "I1",
    "R11", "R12", "R13", "R14", "R15", "R16", "M2", "S2", "E2", "I2",
    "R21", "R22", "R23", "R24", "R25", "R26"), c("1", "2", "3", "4",
    "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
    "16", "17", "18", "19", "20")))

    E <- structure(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
    1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1), .Dim = c(19L, 12L), .Dimnames = list(c("S1", "E1", "I1",
    "R11", "R12", "R13", "R14", "R15", "R16", "M2", "S2", "E2", "I2",
    "R21", "R22", "R23", "R24", "R25", "R26"), c("1", "2", "3", "4",
    "5", "6", "7", "8", "9", "10", "11", "12")))

    N <- structure(c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, -9L, -10L, -10L,
    -10L, -10L, -10L, -10L, -10L, -10L, -10L, 1L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), .Dim = c(19L,
    2L), .Dimnames = list(c("S1", "E1", "I1", "R11", "R12", "R13",
    "R14", "R15", "R16", "M2", "S2", "E2", "I2", "R21", "R22", "R23",
    "R24", "R25", "R26"), c("1", "2")))

    as(SimInf_model(
        ldata  = create_ldata(farms),
        gdata  = create_gdata(lambda,
                              decRate,
                              SFRate,
                              # cleanRate, # FIX ME - remove from gdata - use ldata
                              qIng,
                              mu,
                              qFeces_sow,
                              qFeces_pig),
        v0     = create_v0(farms),
        G      = G,
        S      = S,
        E      = E,
        N      = N,
        tspan  = tspan,
        events = create_events(farms, days, FALSE),
        u0     = create_u0(farms)),
       "HEV")
}

##' Run the model
##'
##' @rdname run-methods
##' @param model The model to run.
##' @param solver Which numerical solver to utilize. Default is 'ssm'.
##' @param ... Additional arguments.
##' @return A model with a single stochastic solution trajectory attached to it.
##' @export
##' @import methods
##' @useDynLib HEVtrans, .registration=TRUE
setMethod(
    "run",
    signature(model = "HEV"),
    function(model, solver = c("ssm", "aem"), ...) {
        solver <- match.arg(solver)
        validObject(model)
        .Call(HEVtrans_run, model, solver)
    }
)
