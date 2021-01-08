#### 10 sims #####
# in "july2020" "partner corrections" "nocross"
sim_10_5200_offset_nocross_eligible <- readRDS("~/Dissertation/Duration_Simulations/for_dissertation/10sims/sim_10_5200_nocross_offset_eligible_effectivedebutracking.rds")
nocrossterms_yesoffset_no65_eligible_nocorrections <- make_small_netsim(sim_10_5200_offset_nocross_eligible)
saveRDS(nocrossterms_yesoffset_no65_eligible_nocorrections, here("for_dissertation", "10sims", "nocrossterms_yesoffset_no65_eligible_nocorrections10.rds"))

# debut sims
#sim_10_5200_debut_cross <- readRDS("~/Dissertation/Duration_Simulations/for_dissertation/10sims/sim_10_5200_debut_cross.rds")
#yescrossterms_nooffset_no65_debut_nocorrections10 <- make_small_netsim(sim_10_5200_debut_cross)
#saveRDS(yescrossterms_nooffset_no65_debut_nocorrections10, here("for_dissertation", "10sims", "yescrossterms_nooffset_no65_debut_nocorrections10.rds"))

#sim_10_5200_nocross_debut_nooffset <- readRDS("~/Dissertation/Duration_Simulations/for_dissertation/10sims/sim_10_5200_nocross_debut_nooffset.rds")
#nocrossterms_nooffset_no65_debut_nocorrections10 <- make_small_netsim(sim_10_5200_nocross_debut_nooffset)
#saveRDS(nocrossterms_nooffset_no65_debut_nocorrections10, here("for_dissertation", "10sims", "nocrossterms_nooffset_no65_debut_nocorrections10.rds"))

#sim_10_5200_nocross_offset_debut <- readRDS("~/Dissertation/Duration_Simulations/for_dissertation/10sims/sim_10_5200_nocross_offset_debut.rds")
#nocrossterms_yesoffset_no65_debut_nocorrections10 <- make_small_netsim(sim_10_5200_nocross_offset_debut)
#saveRDS(nocrossterms_yesoffset_no65_debut_nocorrections10, here("for_dissertation", "10sims", "nocrossterms_yesoffset_no65_debut_nocorrections10.rds"))

#sim_10_5200_nocross_offset_65_debut <- readRDS("~/Dissertation/Duration_Simulations/for_dissertation/10sims/sim_10_5200_nocross_offset_65_debut.rds")
#nocrossterms_yesoffset_yes65_debut_nocorrections <- make_small_netsim(sim_10_5200_nocross_offset_65_debut)
#saveRDS(nocrossterms_yesoffset_yes65_debut_nocorrections, here("for_dissertation", "10sims", "nocrossterms_yesoffset_yes65_debut_nocorrections10.rds"))

#sim_10_5200_nocross_offset_edapprox_debut <- readRDS("~/Dissertation/Duration_Simulations/for_dissertation/10sims/sim_10_5200_nocross_offset_edapprox_debut.rds")
#nocrossterms_yesoffset_no65_debut_edapprox <- make_small_netsim(sim_10_5200_nocross_offset_edapprox_debut)
#saveRDS(nocrossterms_yesoffset_no65_debut_edapprox, here("for_dissertation", "10sims", "nocrossterms_yesoffset_no65_debut_edapprox10.rds"))


#sim_10_5200_nocross_offset_edapprox_debut_mort <- readRDS(here("sims", "July2020", "debut", "sim_10_5200_nocross_offset_edapprox_debut_mort.rds"))
#nocrossterms_yesoffset_no65_debut_edapprox_mort <- make_small_netsim(sim_10_5200_nocross_offset_edapprox_debut_mort)
#saveRDS(nocrossterms_yesoffset_no65_debut_edapprox_mort, here("for_dissertation", "10sims", "nocrossterms_yesoffset_no65_debut_edapprox_mort10.rds"))

#sim_10_5200_nocross_offset_extraeligible<- readRDS(here("sims", "July2020", "partner_corrections",  "sim_10_5200_offest_eligible_nocross.rds"))
#nocrossterms_yesoffset_no65_debut_extraeligible <- make_small_netsim(sim_10_5200_nocross_offset_extraeligible)
#saveRDS(nocrossterms_yesoffset_no65_debut_extraeligible, here("for_dissertation", "10sims", "nocrossterms_yesoffset_no65_debut_extraeligible.rds"))

##### 10 sims for debut and eligibility but with edapprox and mortality corrections ##########

# some of these files say 10_1560 but it's really 10_3120 so correcting here
# eligible
sim_10_3120_nocross_offset_eligible_effectivedebutracking <- readRDS("~/Dissertation/Duration_Simulations/sims/July2020/debut/sim_10_1560_nocross_offset_eligible_effectivedebutracking.rds")
nocrossterms_yesoffset_no65_eligible_withcorrections<- make_small_netsim(sim_10_3120_nocross_offset_eligible_effectivedebutracking)
saveRDS(nocrossterms_yesoffset_no65_eligible_withcorrections, here("for_dissertation", "10sims", "nocrossterms_yesoffset_no65_eligible_withcorrections.rds"))

# eligible - calibrated
sim_10_3120_nocross_offset_calibratedeligible_effectivedebutracking <- readRDS("~/Dissertation/Duration_Simulations/sims/July2020/debut/sim_10_1560_nocross_offset_calibratedeligible_effectivedebutracking.rds")
nocrossterms_yesoffset_no65_calibratedeligible_withcorrections <- make_small_netsim(sim_10_3120_nocross_offset_calibratedeligible_effectivedebutracking)
saveRDS(nocrossterms_yesoffset_no65_calibratedeligible_withcorrections, here("for_dissertation", "10sims", "nocrossterms_yesoffset_no65_calibratedeligible_withcorrections.rds"))

# debut with nodefactor
sim_10_3120_nocross_offset_debut_youngboost_effectivedebutracking <- readRDS("~/Dissertation/Duration_Simulations/sims/July2020/debut/sim_10_1560_nocross_offset_nodefactor_effectivedebutracking.rds")
nocrossterms_yesoffset_no65_debut_nodefactor_withcorrections <- make_small_netsim(sim_10_3120_nocross_offset_debut_youngboost_effectivedebutracking)
saveRDS(nocrossterms_yesoffset_no65_debut_nodefactor_withcorrections, here("for_dissertation", "10sims", "nocrossterms_yesoffset_no65_debut_nodefactor_withcorrections.rds"))




#### 5 sims #####
# in "july2020" "partner corrections"
#yescrossterms_yesoffset_no65_eligible_nocorrections <- make_small_netsim(sim_5_5200_offest)
#usethis::use_data(yescrossterms_yesoffset_no65_eligible_nocorrections)

#readRDS("~/Dissertation/Duration_Simulations/sims/July2020/partner_corrections/sim_5_5200_offest_65.rds")
#yescrossterms_yesoffset_yes65_eligible_nocorrections <- make_small_netsim(sim_5_5200_offest_65)
#usethis::use_data(yescrossterms_yesoffset_yes65_eligible_nocorrections)

# in "july2020" "partner corrections" "nocross"
#sim_5_5200_offest_nocross <- readRDS("~/Dissertation/Duration_Simulations/sims/July2020/partner_corrections/nocross/sim_5_5200_offest_nocross.rds")
#nocrossterms_yesoffset_no65_eligible_nocorrections <- make_small_netsim(sim_5_5200_offest_nocross)
#usethis::use_data(nocrossterms_yesoffset_no65_eligible_nocorrections)

# in "july2020" "debut"
#sim_5_5200_og <- readRDS("~/Dissertation/Duration_Simulations/sims/July2020/debut/sim_5_5200_og.rds")
#yescrossterms_nooffset_no65_debut_nocorrections <- make_small_netsim(sim_5_5200_og)
#usethis::use_data(yescrossterms_nooffset_no65_debut_nocorrections)

#sim_5_5200_nocross <- readRDS("~/Dissertation/Duration_Simulations/sims/July2020/debut/sim_5_5200_nocross.rds")
#nocrossterms_nooffset_no65_debut_nocorrections <- make_small_netsim(sim_5_5200_nocross)
#usethis::use_data(nocrossterms_nooffset_no65_debut_nocorrections)

#sim_5_5200_nocross_offset <- readRDS("~/Dissertation/Duration_Simulations/sims/July2020/debut/sim_5_5200_nocross_offset.rds")
#nocrossterms_yesoffset_no65_debut_nocorrections <- make_small_netsim(sim_5_5200_nocross_offset)
#usethis::use_data(nocrossterms_yesoffset_no65_debut_nocorrections)

#sim_5_5200_nocross_offset_edapprox <- readRDS("~/Dissertation/Duration_Simulations/sims/July2020/debut/sim_5_5200_nocross_offset_edapprox.rds")
#nocrossterms_yesoffset_no65_debut_edapprox <- make_small_netsim(sim_5_5200_nocross_offset_edapprox)
#usethis::use_data(nocrossterms_yesoffset_no65_debut_edapprox)

#sim_5_5200_nocross_offset_65 <- readRDS("~/Dissertation/Duration_Simulations/sims/July2020/debut/sim_5_5200_nocross_offset_65.rds")
#nocrossterms_yesoffset_yes65_debut_nocorrections <- make_small_netsim(sim_5_5200_nocross_offset_65)
#usethis::use_data(nocrossterms_yesoffset_yes65_debut_nocorrections)
