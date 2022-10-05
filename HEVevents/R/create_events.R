##' Create events for the HEV model
##'
##' @param farms list with farm data.
##' @param days the number of days to generate events.
##' @param named_events use named events ("exit", "enter", "intTrans",
##'     "extTrans") instead of integers (0, 1, 2, 3). Default is to
##'     use named events.
##' @return a \code{data.frame} with events.
##' @useDynLib HEVevents HEVevents
##' @export
create_events <- function(farms = NULL, days = NULL, named_events = TRUE) {
    events <- .Call(HEVevents, farms, as.integer(days))

    if (isTRUE(named_events)) {
        event_names <- c("exit", "enter", "intTrans", "extTrans")
        events$event <- event_names[events$event + 1]
    }

    events
}

##' @export
format.farm <- function(x, ...) {
    duration <- c(ges       = x$duration_ges,
                  pre_birth = x$duration_pre_birth,
                  fa        = x$duration_fa,
                  pw        = x$duration_pw,
                  fi        = x$duration_fi)

    size <- matrix(c(x$size_ges_sector, x$size_ges_room, x$size_ges_pen,
                     x$size_fa_sector,  x$size_fa_room,  x$size_fa_pen,
                     x$size_pw_sector,  x$size_pw_room,  x$size_pw_pen,
                     x$size_fi_sector,  x$size_fi_room,  x$size_fi_pen),
                   nrow = 3, ncol = 4,
                   dimnames = list(
                       c("sector", "room", "pen"),
                       c("ges", "fa", "pw", "fi")))

    c(sprintf("Id:\t%i\n", x$farm_id),
      sprintf("Type:\t%s\n", x$farm_type),
      "\n",
      "Size\n",
      "----\n",
      paste0(capture.output(size), collapse = "\n"),
      "\n\n",
      "Duration\n",
      "--------\n",
      paste0(capture.output(duration), collapse = "\n"),
      "\n\n",
      "Interval\n",
      "--------\n",
      paste0(x$interval, collapse = " "))
}

##' @export
print.farm <- function(x, ...) {
    cat(format(x, ...), "\n", sep = "")
    invisible(x)
}

##' @export
format.slaughterhouse <- function(x, ...) {
    c(sprintf("Id:\t%i\n", x$farm_id),
      sprintf("Type:\t%s\n", x$farm_type))
}

##' @export
print.slaughterhouse <- function(x, ...) {
    cat(format(x, ...), "\n", sep = "")
    invisible(x)
}
