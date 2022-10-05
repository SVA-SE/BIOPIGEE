##' Extract the between farm movements from a HEV model
##'
##' @param model the HEV model to extract the network from.
##' @param keep_internal keep internal movements.
##' @return a \code{data.table} with the columns \code{time} (the time
##'     for when the movement occured), \code{from} (the farm the
##'     animals are moved from), \code{to} (the farm the animals are
##'     moved to), and \code{n} (the number of animals moved).
##' @importFrom data.table data.table
setGeneric(
    "network",
    signature = "model",
    function(model, keep_internal = FALSE) {
        standardGeneric("network")
    }
)

##' @rdname network
##' @include model.R
##' @export
setMethod(
    "network",
    signature(model = "HEV"),
    function(model, keep_internal) {
        
        ## First, determine which events are external transfer events.
        i <- which(model@events@event == 3)

        if (!isTRUE(keep_internal)) { 
            ## Then determine which external transfer events that are
            ## between different farms.
            j <- which(model@ldata["farm", model@events@node[i]] !=
                       model@ldata["farm", model@events@dest[i]])
    
            ## Determine the valid indices
            i <- i[j]
        }
        
        ## Create a data.table with the network. Note that it might
        ## contain multiple movements per time-point since it's based
        ## on nodes.
        result <- data.table(time = model@events@time[i],
                             from = model@ldata["farm", model@events@node[i]],
                             to = model@ldata["farm", model@events@dest[i]],
                             n = model@events@n[i])

        ## Aggregate the result by farm.
        result[, .(n = sum(n)), by = c("time", "from", "to")]
    }
)
