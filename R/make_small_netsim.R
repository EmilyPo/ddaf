#' @title Netsim Size Reduction
#'
#' @description Take objects and remove the cel.complete and misc variables I don't currently need
#'
#' @param x is the netsim output
#' @export
#'
make_small_netsim <- function(x){
  out <- list()
  out$param <- x$param
  out$control <- x$control
  out$nwparam <- x$nwparam
  keep <- c("num", "num.male", "num.feml", "meanAge", "propMale", "b.flow", "ds.flow", "marcoh.departed", "other.departed", "debutprop",
            "coef.form.marcoh", "coef.form.casual", "meanDegMarcoh", "meanDegOther", "marcoh", "other", "other.new", "other.end",
            "marcoh.new", "marcoh.end", "effectivedebutprop")
  out$epi <- x$epi[keep]
  out$el <- x$el

  for (i in 1:x$control$nsims){
    x$attr[[i]] <- x$attr[[i]][1:11]
  }

  out$attr <- x$attr
  out$cel.temp <- x$cel.temp
  class(out) <- "netsim"

  return(out)
}



