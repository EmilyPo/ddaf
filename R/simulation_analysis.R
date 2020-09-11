#' @title Distribution of Mean Degree by Age in Marriage/Cohab Network
#'
#' @description This function takes the list of relationships active on the last time step of
#' the simulation (averaged over each run) and calculates the mean degree by age. It
#' then takes that distribution and plots it under the expected distribution based on
#' the egodata. The exported object includes both the ggplot object and the raw data.
#'
#' @param x network simulation object
#' @param maxage age at which egos depart model - either 45 or 65
#' @param marcoh_ages_restricted dist of expected mean degree using age-restricted alter set
#' @param marcoh_ages_all dist of exp mean degree using all reported alters
#' @param categorical default = FALSE, whether or not to return dist by age or agecat
#'
#' @import ggplot2
#'
#' @export

marcoh_dist <- function(x, maxage, categorical=FALSE){
  if (categorical==TRUE){
    nsims <- x$control$nsims
    rels <- active_rels(x)
    open_pop_counts <- get_agedist(x, categorical=TRUE)
    egodat <- egodata_meandegs(categorical=T)

    m <- rels[[1]]
    ages <- c(m$age1, m$age2)


    if (maxage==45){
      ages <- cut(ages, c(15, 20, 25, 30, 35, 40, 45), right=F)
    }

    if (maxage==65){
      ages <- ages[ages<45]
      ages <- cut(ages, c(15, 20, 25, 30, 35, 40, 45), right=F)
    }

    tab <- table(ages)/nsims
    allages <- c(15, 20, 25, 30, 35, 40)
    ma <- egodat[[1]][,2][[1]]
    ma2 <- egodat[[2]][,2][[1]]


    open_marcoh_meandeg <- tab/open_pop_counts

    open_marcoh <- as.data.frame(cbind(allages, ma, ma2, open_marcoh_meandeg))
    colnames(open_marcoh) <- c("Ego Age", "Weighted Egodata - Restricted", "Weighted Egodata", "Final State of Network")

    open_plot <- ggplot() +
      geom_bar(data = open_marcoh, aes(x=`Ego Age`, y=`Final State of Network`, fill="Final State of Network"), stat = "identity",
               fill="#999999") +
      geom_point(open_marcoh, mapping=aes(x=`Ego Age`, y=`Weighted Egodata - Restricted`, color="Weighted Egodata - Restricted"))  +
      geom_point(open_marcoh, mapping=aes(x=`Ego Age`, y=`Weighted Egodata`, color="Weighted Egodata")) +
      xlab("Ego Age") +
      ylab("Mean Degree") +
      ggtitle("Mean Degree of Marriage/Cohabs by Ego Age") +
      scale_color_manual(name = "Egodata",
                         labels = c("Weighted Egodata", "Weighted Egodata, All Alters"),
                         values = c("darkblue", "darkred"))
  } else {
  nsims <- x$control$nsims
  rels <- active_rels(x)
  open_pop_counts <- get_agedist(x)
  egodat <- egodata_meandegs()

  m <- rels[[1]]

  ages <- c(m$age1, m$age2)

  if (maxage==65){
    ages <- ages[ages<45]
    open_pop_counts <- open_pop_counts[which(as.numeric(names(open_pop_counts))<45)]
  }

  tab <- table(ages)

  if(length(tab) < length(open_pop_counts)){
      missing <- setdiff(as.numeric(names(open_pop_counts)), as.numeric(names(tab)))
      if (missing==15){
        tab <- c(0,tab)
      }
  }


  tab <- tab/nsims
  allages <- 15:44
  ma <- egodat[[1]][,2][[1]]
  ma2 <- egodat[[2]][,2][[1]]

  open_marcoh_meandeg <- tab/open_pop_counts

  open_marcoh <- as.data.frame(cbind(allages, ma, ma2, open_marcoh_meandeg))
  colnames(open_marcoh) <- c("Ego Age", "Weighted Egodata - Restricted", "Weighted Egodata", "Final State of Network")

  open_plot <- ggplot() +
    geom_bar(data = open_marcoh, aes(x=`Ego Age`, y=`Final State of Network`, fill="Final State of Network"), stat = "identity",
             fill="#999999") +
    geom_point(open_marcoh, mapping=aes(x=`Ego Age`, y=`Weighted Egodata - Restricted`, color="Weighted Egodata - Restricted"))  +
    geom_point(open_marcoh, mapping=aes(x=`Ego Age`, y=`Weighted Egodata`, color="Weighted Egodata"))+
    xlab("Ego Age") +
    ylab("Mean Degree") +
    ggtitle("Mean Degree of Marriage/Cohabs by Ego Age") +
    scale_color_manual(name = "Egodata",
                       labels = c("Weighted Egodata", "Weighted Egodata, All Alters"),
                       values = c("darkblue", "darkred"))

  }
  dat <- list(open_plot, open_marcoh)

  return(dat)

}

#' @title Distribution of Mean Degree by Age in Casual Network
#'
#' @description This function takes the list of relationships active on the last time step of
#' the simulation (averaged over each run) and calculates the mean degree by age. It
#' then takes that distribution and plots it under the expected distribution based on
#' the egodata. The exported object includes both the ggplot object and the raw data.
#'
#' @param x network simulation object
#' @param maxage age at which egos depart model - either 45 or 65
#' @param casual_ages_restricted dist of expected mean degree using age-restricted alter set
#' @param casual_ages_all dist of exp mean degree using all reported alters
#'
#' @import ggplot2
#' @export
casual_dist <- function(x, maxage, categorical=FALSE){
  if (categorical==TRUE){
    nsims <- x$control$nsims
    rels <- active_rels(x)
    open_pop_counts <- get_agedist(x, categorical=TRUE)
    egodat <- egodata_meandegs(categorical = T)

    m <- rels[[2]]
    ages <- c(m$age1, m$age2)


    if (maxage==45){
      ages <- cut(ages, c(15, 20, 25, 30, 35, 40, 45), right=F)
    }

    if (maxage==65){
      ages <- ages[ages<45]
      ages <- cut(ages, c(15, 20, 25, 30, 35, 40, 45), right=F)
    }

    tab <- table(ages)/nsims
    allages <- c(15, 20, 25, 30, 35, 40)
    ma <- egodat[[3]][,2][[1]]
    ma2 <- egodat[[4]][,2][[1]]


    open_marcoh_meandeg <- tab/open_pop_counts

    open_casual <- as.data.frame(cbind(allages, ma, ma2, open_marcoh_meandeg))
    colnames(open_casual) <- c("Ego Age", "Weighted Egodata - Restricted", "Weighted Egodata", "Final State of Network")

    open_plot <- ggplot() +
      geom_bar(data = open_casual, aes(x=`Ego Age`, y=`Final State of Network`, fill="Final State of Network"), stat = "identity",
               fill="#999999") +
      geom_point(open_casual, mapping=aes(x=`Ego Age`, y=`Weighted Egodata - Restricted`, color="Weighted Egodata - Restricted"))  +
      geom_point(open_casual, mapping=aes(x=`Ego Age`, y=`Weighted Egodata`, color="Weighted Egodata")) +
      xlab("Ego Age") +
      ylab("Mean Degree") +
      ggtitle("Mean Degree of Casuals by Ego Age") +
      scale_color_manual(name = "Egodata",
                         labels = c("Weighted Egodata", "Weighted Egodata, All Alters"),
                         values = c("darkblue", "darkred"))
  } else {
    nsims <- x$control$nsims
    rels <- active_rels(x)
    open_pop_counts <- get_agedist(x)
    egodat <- egodata_meandegs()

    m <- rels[[2]]

    ages <- c(m$age1, m$age2)

    if (maxage==65){
      ages <- ages[ages<45]
      open_pop_counts <- open_pop_counts[which(as.numeric(names(open_pop_counts))<45)]
    }

    tab <- table(ages)

    if(length(tab) < length(open_pop_counts)){
      missing <- setdiff(as.numeric(names(open_pop_counts)), as.numeric(names(tab)))
      if (missing==15){
        tab <- c(0,tab)
      }
    }


    tab <- tab/nsims
    allages <- 15:44
    ma <- egodat[[3]][,2][[1]]
    ma2 <- egodat[[4]][,2][[1]]

    open_marcoh_meandeg <- tab/open_pop_counts

    open_casual <- as.data.frame(cbind(allages, ma, ma2, open_marcoh_meandeg))
    colnames(open_casual) <- c("Ego Age", "Weighted Egodata - Restricted", "Weighted Egodata", "Final State of Network")

    open_plot <- ggplot() +
      geom_bar(data = open_casual, aes(x=`Ego Age`, y=`Final State of Network`, fill="Final State of Network"), stat = "identity",
               fill="#999999") +
      geom_point(open_casual, mapping=aes(x=`Ego Age`, y=`Weighted Egodata - Restricted`, color="Weighted Egodata - Restricted"))  +
      geom_point(open_casual, mapping=aes(x=`Ego Age`, y=`Weighted Egodata`, color="Weighted Egodata"))+
      xlab("Ego Age") +
      ylab("Mean Degree") +
      ggtitle("Mean Degree of Casuals by Ego Age") +
      scale_color_manual(name = "Egodata",
                         labels = c("Weighted Egodata", "Weighted Egodata, All Alters"),
                         values = c("darkblue", "darkred"))

  }
  dat <- list(open_plot, open_casual)

  return(dat)

}

#' @title Final Mean Degree Distribution of Simulation
#'
#' @description This function calculates the target mean degree and the mean degree at the end of
#' the simulation (averaged across each run) for each network.
#'
#' @param x network simulation object
#' @param maxage age at which egos depart model - either 45 or 65
#'
#' @export
calc_meandeg <- function(x, maxage){
  nsteps <- x$control$nsteps
  nsims <- x$control$nsims

  if (maxage==45){
    marcohs <- 1:nsims
    for (i in 1:nsims) marcohs[i] <- (nrow(x$el[[i]][[1]])*2)/x$epi$num[[i]][nsteps]
    meanMarcoh <- round(mean(marcohs),3)
    targetm <- round((x$nwparam[[1]]$target.stats[[1]]/50000)*2,3)

    others <- 1:nsims
    for (i in 1:nsims) others[i] <- (nrow(x$el[[i]][[2]])*2)/x$epi$num[[i]][nsteps]
    meanOther <- round(mean(others),3)
    targeto <- round((x$nwparam[[2]]$target.stats[[1]]/50000)*2,3)

    dat <- matrix(c(targetm, meanMarcoh, targeto, meanOther), nrow=2, byrow = T)
  }

  if (maxage==65){
    marcohs <- 1:nsims

    for (i in 1:nsims){
      rels <- sum(table(floor(x$attr[[i]]$age[x$el[[i]][[1]]]))[1:30])
      marcohs[i] <- rels/sum(x$attr[[i]]$age<45)
    }
    meanMarcoh <- round(mean(marcohs),3)
    targetm <- round((x$nwparam[[1]]$target.stats[[1]]/50000)*2,3)

    others <- 1:nsims
    for (i in 1:nsims){
      rels <- sum(table(floor(x$attr[[i]]$age[x$el[[i]][[2]]]))[1:30])
      others[i] <- rels/sum(x$attr[[i]]$age<45)
    }
    meanOther <- round(mean(others),3)
    targeto <- round((x$nwparam[[2]]$target.stats[[1]]/50000)*2,3)

    dat <- matrix(c(targetm, meanMarcoh, targeto, meanOther), nrow=2, byrow = T)
  }

  return(dat)
}

#' @title Calculate Active and Completed Mean Relationship Length
#'
#' @description This function calculates the mean relationship length of active rels (averaged
#' across each run) in each network, the mean length of completed rels, and includes
#' the target length in the exported object.
#'
#' @param x network simulation object
#' @param maxage age at which egos depart model - either 45 or 65
#'
#' @export
#'
duration_calcs <- function(x, maxage){
  active <- ddaf::active_rels(x)

  target_m <- round(x$nwparam[[1]]$coef.diss[[2]])
  target_c <- round(x$nwparam[[2]]$coef.diss[[2]])

  if (maxage==45){
    mean_m_active <- round(mean(active[[1]]$len))
    mean_c_active <- round(mean(active[[2]]$len))
  }

  if (maxage==65){
    m <- active[[1]]
    m_young <- m[which(
      (m$age1 >= 45 & m$age2 <45) |
        (m$age1 < 45 & m$age2 >= 45) |
        (m$age1 < 45 & m$age2 < 45)),]

    c <- active[[2]]
    c_young <- c[which(
      (c$age1 >= 45 & c$age2 <45) |
        (c$age1 < 45 & c$age2 >= 45) |
        (c$age1 < 45 & c$age2 < 45)),]

    mean_m_active <- round(mean(m_young$len))
    mean_c_active <- round(mean(c_young$len))

  }

  dat <- as.data.frame(matrix(c(target_m, mean_m_active, target_c, mean_c_active), nrow=2, byrow = T))
  row.names(dat) <- c("Marriage/Cohab", "Casual")
  colnames(dat) <- c("Target", "Simulation")
  dat$`Pct Off` <- round(((dat$Simulation-dat$Target)/dat$Target)*100, 2)

  return(dat)
}
