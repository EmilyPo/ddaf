#' @title Netsim Size Reduction
#'
#' @description Take objects and remove the cel.complete and misc variables I don't currently need
#'
#' @param x is the netsim output
#' @export
#'
make_small_netsim <- function(x, keep_epi=NULL, keep_attr=NULL){
  out <- list()
  out$param <- x$param
  out$control <- x$control
  out$nwparam <- x$nwparam
  out$el <- x$el
  out$cel.temp <- x$cel.temp

  if (is.null(keep_epi)){
    if("effectivedebutprop" %in% ls(x$epi)){
    keep_epi <- c("meanDegMarcoh", "meanDegOther", "marcoh", "other", "other.new", "other.end",
                  "marcoh.new", "marcoh.end", "effectivedebutprop")
    } else {
      keep_epi <- c("meanDegMarcoh", "meanDegOther", "marcoh", "other", "other.new", "other.end",
                    "marcoh.new", "marcoh.end")
    }
  } else {
    keep_epi <- keep_epi
    }

  out$epi <- x$epi[keep_epi]

  if (is.null(keep_attr)){
    if ("effectiveDebut" %in% ls(x$attr[[1]])){
      keep_attr <- c("age", "agecat", "debut", "effectiveDebut", "male",
                     "olderpartnerM", "olderpartnerO")
    } else {
      keep_attr <- c("age", "agecat", "debut", "male",
                     "olderpartnerM", "olderpartnerO")
    }

  } else {
    keep_attr <- keep_attr
    }

  for (i in 1:x$control$nsims){
    x$attr[[i]] <- x$attr[[i]][keep_attr]
  }
  out$attr <- x$attr

  class(out) <- "netsim"

  return(out)
}

#' @title Netsim Summary Stats
#'
#' @description Take objects generate the summary stats I use in graphs etc
#'
#' @param x is the netsim output
#' @export
#'
make_summary_stats <- function(x, maxage, simname){
  out <- list()
  out$name <- simname
  out$nwparam[[1]] <- x$nwparam[[1]][c("coef.form", "coef.diss")]
  out$nwparam[[2]] <- x$nwparam[[2]][c("coef.form", "coef.diss")]

  out$marcoh <- as.matrix(marcoh_dist(x, maxage)[[2]])
  out$casual <- as.matrix(casual_dist(x, maxage)[[2]])
  out$meandegs <- calc_meandeg(x, maxage)
  out$meandurs <- as.matrix(duration_calcs(x, maxage))

  if ("effectivedebutprop" %in% ls(x$epi)){
  out$effdebut <- apply(x$epi$effectivedebutprop, 1, mean)
  }

  out$debut <- apply(x$epi$debutprop, 1, mean)

  return(out)
}


