#' @title Mean Degree Distributions in Egodata
#'
#' @description This function takes the egodata (stored in system data) and summarizes the expected mean degree
#' distribution by age.
#' @import dplyr
#' @import srvyr
#' @export
#' @examples
#' egodata_meandegs()

egodata_meandegs <- function(){
  egosvy <- as_survey(egodat_marcoh$egos, weights="weight")
  egosvy2 <- as_survey(egodat_marcoh_allalters$egos, weights="weight")

  marcoh_ages <- egosvy %>% group_by(age) %>% summarise(mean=survey_mean(deg.marcoh.binary))
  marcoh_ages_all <- egosvy2 %>% group_by(age) %>% summarise(mean=survey_mean(deg.marcoh.binary))

  casual_ages <- egosvy %>% group_by(age) %>% summarise(mean=survey_mean(deg.other))
  casual_ages_all <- egosvy2 %>% group_by(age) %>% summarise(mean=survey_mean(deg.other))

  return(list(marcoh_ages, marcoh_ages_all, casual_ages, casual_ages_all))
}
