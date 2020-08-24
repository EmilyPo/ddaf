#' @title Sample Relationships with NSFG Design
#'
#' @description This function takes the output from simulation cel.temp and cel.complete,
#' removes relationships that ended more than 12 months from last time step,
#' adds exact duration flag and limits to 3 relationships per ID.
#'
#' @param x is the netsim output
#' @export
#'

as_nsfg_rels <- function(x){

  nsteps <- x$control$nsteps
  time.step <- x$param$time.unit
  nsims <- x$control$nsims

  allRelsComplete <- NULL

  for (i in 1:nsims){
    # rels that ended
    rels <- x$cel.complete[[i]]

    # rels that are ongoing
    relscur <- x$cel.temp[[i]]

    # full
    allRels <- rbind(rels, relscur)

    # make all end == NA 100 and create censored flag (1 if exact, 0 if ongoing)
    # (previous NA, newly 100 are all ongoing)
    allRels[,"exact"] <- ifelse(is.na(allRels[,"end"]), 0, 1)
    allRels[,"end"] <- ifelse(is.na(allRels[,"end"]), nsteps, allRels[,"end"])
    allRels[,"len"] <- allRels[,"end"]-allRels[,"start"]

    # limit to those that ended in last 12 months or are ongoing
    cap <- nsteps - floor(365/time.step)

    current <- which(allRels[,"end"] >= cap)

    allRels <- allRels[current,]

    # limit that to 3 rels per UID
    tabP1 <- table(allRels$p1)
    tabP2 <- table(allRels$p2)

    relsp1 <- NULL
    relsp2 <- NULL

    if (length(which(tabP1>3)) > 0) {
      #grab ids that have more than 3 partners in last year
      ids <- as.numeric(names(which(tabP1>3)))

      # drop all rels associated with that id from main dat and make separate dataframe
      relsToLimit <- allRels[which(allRels$p1 %in% ids),]
      Rels <- allRels[-which(allRels$p1 %in% ids),]

      # for each id grab 3 most recent rels
      for (i in length(ids)) {
        z <- relsToLimit[relsToLimit$p1 %in% ids[i],]
        mostRecent <- nrow(z)
        third <- mostRecent-2
        z <- z[third:mostRecent,]
        relsp1 <- rbind(relsp1, z)
      }
    }

    if (length(which(tabP2>3)) > 0) {
      #grab ids that have more than 3 partners in last year
      ids <- as.numeric(names(which(tabP2>3)))

      # drop all rels associated with that id from main dat and make separate dataframe
      relsToLimit <- allRels[which(allRels$p2 %in% ids),]
      Rels <- allRels[-which(allRels$p2 %in% ids),]

      # for each id grab 3 most recent rels
      for (i in length(ids)) {
        z <- relsToLimit[relsToLimit$p2 %in% ids[i],]
        mostRecent <- nrow(z)
        third <- mostRecent-2
        z <- z[third:mostRecent,]
        relsp2 <- rbind(relsp2, z)
      }
    }

    relsLimited <- rbind(relsp1, relsp2)

    allRelsThisSim <- rbind(allRels, relsLimited)

    allRelsComplete <- rbind(allRelsComplete, allRelsThisSim)

  }

  return(allRelsComplete)

}

#' @title Compile Active Relationships
#'
#' @description This function takes the output from simulation cel.temp and splits
#' the data by network type
#'
#' @param x is the netsim output
#' @export
#'
active_rels <- function(x){

  nsteps <- x$control$nsteps
  time.step <- x$param$time.unit
  nsims <- x$control$nsims

  m <- NULL
  o <- NULL

  for (i in 1:nsims){

    rels <- x$cel.temp[[i]]

    rels$end <- rep(nsteps, nrow(rels))
    rels$len <- rels$end-rels$start


    marcoh <- rels[rels$type=="marcoh",]
    other <- rels[rels$type=="other",]

    m <- rbind(m, marcoh)
    o <- rbind(o, other)

  }

  list <- list(m, o)

  return(list)
}

#' @title Compile Active Relationships
#'
#' @description This function takes the output from simulation cel.complete,
#' keeps all relationships that begin after the simulation began, and splits
#' the data by network type
#'
#' @param x is the netsim output
#' @export
#'
complete_rels <- function(x){

  nsteps <- x$control$nsteps
  time.step <- x$param$time.unit
  nsims <- x$control$nsims

  allRelsComplete <- NULL

  for (i in 1:nsims){
    # rels that ended
    rels <- x$cel.complete[[i]]

    # limit to those that started after simulation began (time step 3+)
    rels <- rels[rels$start > 2,]

    # calculate length
    rels$len <- rels$end-rels$start

    # bind
    allRelsComplete <- rbind(allRelsComplete, rels)

  }

  m <- allRelsComplete[allRelsComplete$type=="marcoh",]
  c <- allRelsComplete[allRelsComplete$type=="other",]

  dat <- list(m,c)

  return(dat)

}

#' @title Get Active Agedist
#'
#' @description This function takes final age attribute and generates a dist of counts
#' by age, averaged across all simulation runs
#'
#' @param x is the netsim output
#' @export
#'
get_agedist <- function(x){
  nsims <- x$control$nsims
  ages <- NULL

  for (i in 1:nsims){
    a <- table(floor(x$attr[[i]]$age))
    ages <- cbind(ages, a)
  }

  means <- round(apply(ages, 1, mean))
}
