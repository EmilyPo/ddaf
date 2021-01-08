#' @title Netsim Summary Stats
#'
#' @description Take objects generate the summary stats I use in graphs etc
#'
#' @param x is the netsim output
#' @export

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
