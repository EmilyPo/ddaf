#' @title Mean Degree Distributions in Egodata
#'
#' @description This function takes the egodata (stored in system data) and summarizes the expected mean degree
#' distribution by age.
#' @import dplyr
#' @import srvyr
#' @export
#' @examples
#' egodata_meandegs()

egodata_meandegs <- function(categorical=FALSE){
  egosvy <- as_survey(egodat_marcoh$egos, weights="weight")
  egosvy2 <- as_survey(egodat_marcoh_allalters$egos, weights="weight")

  if(categorical==TRUE){
    marcoh_ages <- egosvy %>% group_by(agecat) %>% summarise(mean=survey_mean(deg.marcoh.binary))
    marcoh_ages_all <- egosvy2 %>% group_by(agecat) %>% summarise(mean=survey_mean(deg.marcoh.binary))

    casual_ages <- egosvy %>% group_by(agecat) %>% summarise(mean=survey_mean(deg.other))
    casual_ages_all <- egosvy2 %>% group_by(agecat) %>% summarise(mean=survey_mean(deg.other))

    dat <- list(marcoh_ages, marcoh_ages_all, casual_ages, casual_ages_all)

  } else {
  marcoh_ages <- egosvy %>% group_by(age) %>% summarise(mean=survey_mean(deg.marcoh.binary))
  marcoh_ages_all <- egosvy2 %>% group_by(age) %>% summarise(mean=survey_mean(deg.marcoh.binary))

  casual_ages <- egosvy %>% group_by(age) %>% summarise(mean=survey_mean(deg.other))
  casual_ages_all <- egosvy2 %>% group_by(age) %>% summarise(mean=survey_mean(deg.other))

  dat <- list(marcoh_ages, marcoh_ages_all, casual_ages, casual_ages_all)
  }
  return(dat)
}

#' @title Mean Degree Distributions in Egodata
#'
#' @description This function takes the egodata (stored in system data) and summarizes the expected mean degree
#' distribution by age. Targets expanded for EPT project with three dyamiic networks and sex differences in mean degree
#' does not include egodata for alters beyond age 45
#'
#' this will NOT work locally unless you insert the path
#' @import dplyr
#' @import srvyr
#' @export
#' @examples
#' egodata_meandegs()

egodata_meandegs_ept <- function(path=NULL){
  if (is.null(path)) {
    fit <- readRDS("~/Dissertation/ExpeditedPartnerTherapy/fits/fit.marriage.rds")
  } else {
      fit <- path
    }

  egos <- fit$egodata$egos
  egosvy <- as_survey(egos, weights="weight")

  # marriage
  mar_f <- egosvy %>% filter(male==0) %>% group_by(age) %>% summarise(mean=survey_mean(deg.mar))
  mar_m <- egosvy %>% filter(male==1) %>%group_by(age) %>% summarise(mean=survey_mean(deg.mar))

  # cohab
  cohab_f <- egosvy %>% filter(male==0) %>% group_by(age) %>% summarise(mean=survey_mean(deg.cohab))
  cohab_m <- egosvy %>% filter(male==1) %>%group_by(age) %>% summarise(mean=survey_mean(deg.cohab))

  # casual
  casual_f <- egosvy %>% filter(male==0) %>% group_by(age) %>% summarise(mean=survey_mean(deg.other))
  casual_m <- egosvy %>% filter(male==1) %>% group_by(age) %>% summarise(mean=survey_mean(deg.other))

  dat <- list(mar_f, mar_m, cohab_f, cohab_m, casual_f, casual_m)

  return(dat)
}
