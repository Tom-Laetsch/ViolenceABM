"
Coding to run simulations removing X% of the hostile population compared to no removal. 
The removal is taken place targetted (by removing those agents with the largest violence factor)
as well as random, to see which effects are caused via targetting vs random population decrease.
"

source( 'FunctFiles/INITIALIZE.R' )
source('FunctFiles/ITERATION.R')

RemoveHostilesByViolenceFac <- function( abm, per_rem ){
     hostiles <- abm$VARIABLES$HOSTILES
     n_remove <- as.integer( per_rem * nrow( hostiles ) / 100 )
     hdf <- hostiles[ order(hostiles$violence_fac, decreasing = T), ]
     to_remove <- hdf$uid[ 1:n_remove ]
     hostiles$out_of_commission[ hostiles$uid %in% to_remove ] <- Inf
     abm$VARIABLES$HOSTILES <- hostiles
     abm
}

RemoveHostilesRandomly <- function( abm, per_rem ){
     hostiles <- abm$VARIABLES$HOSTILES
     n_remove <- as.integer( per_rem * nrow( hostiles ) / 100 )
     to_remove <- sample( x = hostiles$uid, size = n_remove, replace = F )
     hostiles$out_of_commission[ hostiles$uid %in% to_remove ] <- Inf
     abm$VARIABLES$HOSTILES <- hostiles
     abm
}


### FILE IO STUFF

HostileRemoval_OutputFilepaths <- function( version_num, output_dir ){
     fp_prefix <- pre_intervention_fp <- gsub( pattern = '//+', 
                                               replacement = '/', 
                                               x = sprintf("%s/V%s", output_dir, version_num ) )
     
     pre_intv_fp <- sprintf( "%s_pre_intv.RDS", fp_prefix )
     
     no_intv_fp <- sprintf(  "%s_no_intv.RDS", fp_prefix )
     
     rand_5per_intv_fp <- sprintf( "%s_5per_rand_removal.RDS", fp_prefix )
     
     targ_5per_intv_fp <- sprintf( "%s_5per_targ_removal.RDS", fp_prefix )
     
     rand_10per_intv_fp <- sprintf( "%s_10per_rand_removal.RDS", fp_prefix )
     
     targ_10per_intv_fp <- sprintf( "%s_10per_targ_removal.RDS", fp_prefix )
     
     rand_15per_intv_fp <- sprintf( "%s_15per_rand_removal.RDS", fp_prefix )
     
     targ_15per_intv_fp <- sprintf( "%s_15per_targ_removal.RDS", fp_prefix )
     
     list(
          pre_intv = pre_intv_fp,
          no_intv= no_intv_fp,
          rand_5per = rand_5per_intv_fp,
          targ_5per = targ_5per_intv_fp,
          rand_10per = rand_10per_intv_fp,
          targ_10per = targ_10per_intv_fp,
          rand_15per = rand_15per_intv_fp,
          targ_15per = targ_15per_intv_fp
     )
     
}


RunSingleVersion <- function( output_fp_list,
                              INTRODUCE_INTERVENTION_TIMESTEP = 500,
                              INTERVENTION_TIMESTEPS = 500 ){
     
     ### Create an instance of the ABM
     abm <- InitializeABM()
     
     ### Run the abm through a certain number of pre-intervention timesteps\
     for( timestep in 1:(INTRODUCE_INTERVENTION_TIMESTEP-1) ){ 
          abm <- IterateABM( abm = abm, verbose = F ) 
     }
     
     # pre-intervention file-io stuff 
     saveRDS( object = abm, file = output_fp_list$pre_intv )
     
     ### Run Interventions:
     # - 1: removing top x% based on vf
     # - 2: removing x% randomly
     # - 3: no removal
     
     # clone the pre-intervention abm to copies which will have different interventions
     abm_no_intv <- abm
     abm_remove_rand5 <- RemoveHostilesRandomly( abm, per_rem = 5 )
     abm_remove_targ5 <- RemoveHostilesByViolenceFac( abm, per_rem = 5 )
     abm_remove_rand10 <- RemoveHostilesRandomly( abm, per_rem = 10 )
     abm_remove_targ10 <- RemoveHostilesByViolenceFac( abm, per_rem = 10 )
     abm_remove_rand15 <- RemoveHostilesRandomly( abm, per_rem = 15 )
     abm_remove_targ15 <- RemoveHostilesByViolenceFac( abm, per_rem = 15 )
     
     for( timestep in INTRODUCE_INTERVENTION_TIMESTEP:(INTRODUCE_INTERVENTION_TIMESTEP + INTERVENTION_TIMESTEPS) ){
          # no removal 
          abm_no_intv <- IterateABM( abm_no_intv )
          # 5% removal
          abm_remove_rand5 <- IterateABM( abm_remove_rand5 )
          abm_remove_targ5 <- IterateABM( abm_remove_targ5 )
          # 10% removal
          abm_remove_rand10 <- IterateABM( abm_remove_rand10 )
          abm_remove_targ10 <- IterateABM( abm_remove_targ10 )
          # 15% removal
          abm_remove_rand15 <- IterateABM( abm_remove_rand15 )
          abm_remove_targ15 <- IterateABM( abm_remove_targ15 )
     }
     
     # output
     saveRDS( object = abm_no_intv, file = output_fp_list$no_intv )
     saveRDS( object = abm_remove_targ5, file = output_fp_list$targ_5per )
     saveRDS( object = abm_remove_rand5, file = output_fp_list$rand_5per )
     saveRDS( object = abm_remove_targ10, file = output_fp_list$targ_10per )
     saveRDS( object = abm_remove_rand10, file = output_fp_list$rand_10per )
     saveRDS( object = abm_remove_targ15, file = output_fp_list$targ_15per )
     saveRDS( object = abm_remove_rand15, file = output_fp_list$rand_15per )
     
}

RunInterventionVersions <- function( num_versions = 100, output_dir = './output/Experiment_5Aug2019' ){
     
     for( version_num in 1:num_versions ){
          start.time <- Sys.time()
          output_fp_list <- HostileRemoval_OutputFilepaths( version_num = version_num, output_dir = output_dir )
          cat(sprintf('Beginning Version #%s...', version_num))
          RunSingleVersion( output_fp_list = output_fp_list )
          end.time <- Sys.time()
          cat( sprintf('Done. Duration: %.2f minutes.\n', as.numeric(end.time - start.time, units = 'mins' ) ) )
     }
     
}



Get_Removal_SETUP_info <- function( abm ){
     list( n_desigs = abm$SETUP$PLAYGROUND_SETTINGS$NUM_DESIG,
           n_hostiles = sum(abm$SETUP$PLAYGROUND$n_agents),
           t_min = abm$SETUP$TENSION_SETTINGS$TENS_MAT_ENTRY_MIN,
           t_max = abm$SETUP$TENSION_SETTINGS$TENS_MAT_ENTRY_MAX,
           t_counter = abm$SETUP$TENSION_SETTINGS$TENS_COUNT$NAME,
           vf_on_inter = abm$SETUP$VIOLENCE_FAC_SETTINGS$VIOLENCE_FAC_ON_INTERACTION$NAME,
           ih_wt = abm$SETUP$IMPART_HOSTILITY_SETTINGS$WEIGHT, 
           ih_bal = abm$SETUP$IMPART_HOSTILITY_SETTINGS$BALANCE )
}



Get_Removal_Row_Information <- function( version_num, intervention_timestep = 500, rds_dir = 'output/Experiment_5Aug2019/'  ){
     prefix_fp <- gsub(
          pattern = '//+', 
          replace = '/',
          x = sprintf( '%s/V%s_', rds_dir, version_num )
     )
     
     noi_fp <- sprintf( '%s%s', prefix_fp, 'no_intv.RDS' )
     tr5_fp <- sprintf( '%s%s', prefix_fp, '5per_targ_removal.RDS')
     tr10_fp <- sprintf( '%s%s', prefix_fp, '10per_targ_removal.RDS') 
     tr15_fp <- sprintf( '%s%s', prefix_fp, '15per_targ_removal.RDS')
     rr5_fp <- sprintf( '%s%s', prefix_fp, '5per_rand_removal.RDS')
     rr10_fp <- sprintf( '%s%s', prefix_fp, '10per_rand_removal.RDS')
     rr15_fp <- sprintf( '%s%s', prefix_fp, '15per_rand_removal.RDS')
     
     abm_noi <- readRDS( noi_fp )
     abm_tr5 <- readRDS( tr5_fp )
     abm_tr10 <- readRDS( tr10_fp )
     abm_tr15 <- readRDS( tr15_fp )
     abm_rr5 <- readRDS( rr5_fp )
     abm_rr10 <- readRDS( rr10_fp )
     abm_rr15 <- readRDS( rr15_fp )
     
     nh_prei <- nrow( abm_noi$VARIABLES$HOSTILITY_HIST[abm_noi$VARIABLES$HOSTILITY_HIST$timestep < intervention_timestep, ] )
     nh_noi <- nrow( abm_noi$VARIABLES$HOSTILITY_HIST )
     nh_tr5 <- nrow( abm_tr5$VARIABLES$HOSTILITY_HIST )
     nh_tr10 <- nrow( abm_tr10$VARIABLES$HOSTILITY_HIST )
     nh_tr15 <- nrow( abm_tr15$VARIABLES$HOSTILITY_HIST )
     nh_rr5 <- nrow( abm_rr5$VARIABLES$HOSTILITY_HIST )
     nh_rr10 <- nrow( abm_rr10$VARIABLES$HOSTILITY_HIST )
     nh_rr15 <- nrow( abm_rr15$VARIABLES$HOSTILITY_HIST )
     
     row <- data.frame( Get_Removal_SETUP_info( abm_noi ), stringsAsFactors = F )
     row$total_time <- abm_noi$VARIABLES$TIMESTEP - 1
     
     row$version_num <- version_num
     
     row$intv_start <- intervention_timestep
     row$nhost_pre_intv <- nh_prei
     row$nhost_no_rem_delt <- nh_noi - nh_prei
     row$nhost_targ5_rem_delt <- nh_tr5 - nh_prei
     row$nhost_rand5_rem_delt <- nh_rr5 - nh_prei
     row$nhost_targ10_rem_delt <- nh_tr10 - nh_prei
     row$nhost_rand10_rem_delt <- nh_rr10 - nh_prei
     row$nhost_targ15_rem_delt <- nh_tr15 - nh_prei
     row$nhost_rand15_rem_delt <- nh_rr15 - nh_prei
     
     row[ c('version_num',
            'intv_start',
            'total_time',
            'n_desigs',
            'n_hostiles',
            't_min',
            't_max',
            't_counter',
            'vf_on_inter',
            'ih_wt', 
            'ih_bal',
            'nhost_pre_intv',
            'nhost_no_rem_delt',
            'nhost_targ5_rem_delt',
            'nhost_rand5_rem_delt',
            'nhost_targ10_rem_delt',
            'nhost_rand10_rem_delt',
            'nhost_targ15_rem_delt',
            'nhost_rand15_rem_delt') ]
}

Create_Removal_Summary_DF <- function( version_nums = 1:100 ){
     ret_df <- NULL
     for( vn in version_nums ){
          ret_df <- rbind( ret_df, Get_Removal_Row_Information(vn) )
     }
     ret_df
}