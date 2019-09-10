DrawTiles <- function( abm ){
     
     # partition the hostiles by nbhd 
     
     # get the hostile df for those hostiles still active
     hdf <- abm$VARIABLES$HOSTILES
     hdf <- hdf[ hdf$out_of_commission == 0, ]
     
     # give an easy nickname for the movement probability matrix
     mpm <- abm$VARIABLES$MOVEMENT_PROB_MATRIX
     
     # give an easy nickname for the tile data frame
     tdf <- abm$SETUP$TILES
     
     # give an easy nickname for the nbhds and designations
     nbhds <- abm$TOOLS$GetNbhds()
     desigs <- abm$TOOLS$GetDesigs()
     
     # decide which nbhd to move to
     hdf$move_to_nbhd <- '' # initialize column
     for( nbhd in nbhds ){
          probs <- mpm[nbhd, ]
          bn <- (hdf$base_nbhd == nbhd)
          hdf[ bn, ]$move_to_nbhd <- sample( x = names(probs), size = sum(bn), replace = T, prob = probs )
     }
     
     hdf$move_to_tile <- '' # initialize column
     for( nbhd in nbhds ){
          mtn <- (hdf$move_to_nbhd == nbhd)
          hdf[mtn, ]$move_to_tile <- sample( tdf[ tdf$nbhd == nbhd, ]$tile, size = sum(mtn), replace = T )
     }
     
     # calculate interactions between enemy agents
     n.desigs <- length( desigs )
     ret_df <- NULL
     for( i in 1:(n.desigs-1) ){
          for( j in (i+1):n.desigs ){
               hdf1 <- hdf[ hdf$designation == desigs[i], ]
               hdf2 <- hdf[ hdf$designation == desigs[j], ]
               ret_df <- rbind( ret_df, merge( hdf1, hdf2, by = 'move_to_tile' ) )
          }
     }
     
     data.frame( agent_1 = ret_df$uid.x, 
                 agent_2 = ret_df$uid.y, 
                 stringsAsFactors = F )
}

GetHostilities <- function( abm, interaction_df ){
     
     # give easy nicknames to the objects we use here
     idf <- interaction_df
     hdf <- abm$VARIABLES$HOSTILES
     ih <- abm$SETUP$IMPART_HOSTILITY
     voi <- abm$SETUP$VIOLENCE_FAC_ON_INTERACTION
     tm <- abm$VARIABLES$TENSION_MATRIX
     toi <- abm$SETUP$TENSION_ON_INTERACTION
     
     # initialize the df we return 
     ret_df <- data.frame( perp_id = character(), 
                           perp_nbhd = character(),
                           vict_id = character(), 
                           vict_nbhd = character(),
                           stringsAsFactors = F )
     
     n.row <- nrow( idf )
     if( n.row == 0 ) return( ret_df )
     idf$inum <- 1:n.row
     a1df <- merge( y = idf, x = hdf, by.y = c('agent_1'), by.x = c('uid') )
     a2df <- merge( y = idf, x = hdf, by.y = c('agent_2'), by.x = c('uid') )
     a1df <- a1df[ order(a1df$inum), ]
     a2df <- a2df[ order(a2df$inum), ]
     
     for( row in 1:n.row ){
          a1 <- a1df[row,]
          a2 <- a2df[row,]
          
          if( ih(perp_agent = a1, 
                 vict_agent = a2, 
                 tension_matrix = tm, 
                 violence_fac_calculator = voi, 
                 tension_calculator = toi) ){ 
               ret_df <- rbind( ret_df, data.frame( perp_id = a1$uid, 
                                                    perp_nbhd = a1$base_nbhd,
                                                    vict_id = a2$uid, 
                                                    vict_nbhd = a2$base_nbhd,
                                                    stringsAsFactors = F ) )
          }
          if( ih(perp_agent = a2, 
                 vict_agent = a1, 
                 tension_matrix = tm, 
                 violence_fac_calculator = voi, 
                 tension_calculator = toi) ){ 
               ret_df <- rbind( ret_df, 
                                data.frame( perp_id = a2$uid, 
                                            perp_nbhd = a2$base_nbhd,
                                            vict_id = a1$uid, 
                                            vict_nbhd = a1$base_nbhd,
                                            stringsAsFactors = F ) )
          }
     }
     ret_df
}

UpdateTimestep <- function( abm, new_hostilities ){
     require( dplyr )
     
     variables <- abm$VARIABLES
     ts <- variables$TIMESTEP
     hdf <- variables$HOSTILES 
     tm <- variables$TENSION_MATRIX
     hh <- variables$HOSTILITY_HIST
     
     injd <- abm$SETUP$INJURY_DURATION
     injd_overlap <- abm$SETUP$INJURY_DURATION_ON_OVERLAP
     
     new_tm <- abm$SETUP$UPDATE_TENSION_MATRIX( tension_matrix = tm,
                                                new_hostilities = new_hostilities,
                                                tools = abm$TOOLS )
     
     hdf$out_of_commission <- sapply( hdf$out_of_commission, FUN = function(ooc) max( 0, ooc - 1) )
     
     n.hty <- nrow( new_hostilities )
     if( n.hty > 0 ){
          new_hostilities$injury_duration <- injd( n.hty )
          injury_df <- new_hostilities %>% group_by(vict_id) %>% summarise( injury_duration = injd_overlap( injury_duration ) )
          
          for( i in 1:nrow(injury_df) ){
               row <- injury_df[i, ]
               vid <- (hdf$uid == row$vict_id)
               hdf[ vid, ]$out_of_commission <- hdf[ vid, ]$out_of_commission + row$injury_duration
          }
          
          new_hostilities$timestep <- ts
          
          hh <- rbind( hh, new_hostilities )
     }
     
     
     variables$HOSTILES <- hdf
     variables$HOSTILITY_HIST <- hh 
     variables$TENSION_MATRIX <- new_tm
     variables$TIMESTEP <- ts + 1
     
     abm$VARIABLES <- variables
     abm
}

IterateABM <- function( abm, verbose = F ){
     
     # give a nickname to the timestep for ease
     if( verbose ){ 
          ts <- abm$VARIABLES$TIMESTEP
          cat( sprintf('Starting timestep %d:\n', ts) )
     }
     
     # decide which tile each hostile agent is moving to and find interactions
     if( verbose ) cat( '\t-Selecting which agents interact... ' )
     idf <- DrawTiles( abm = abm ) 
     if( verbose ) cat('Done\n')
     # decide which hostilities take place during interactions
     if( verbose ) cat( '\t-Selecting which interacting agents exchange hostilities... ' )
     hostilities <- GetHostilities( abm = abm, interaction_df = idf )
     if( verbose ) cat('Done\n')
     # update the timestep by calculating all things involved in new hostilities:
     # - injury durations
     # - tension increase/decrease
     if( verbose ) cat( sprintf('\t-Updating variables with %d new hostilities... ', nrow(hostilities)) )
     updated_abm <- UpdateTimestep( abm = abm, new_hostilities = hostilities )
     if( verbose ) cat('Done\n\n')
     
     # return
     updated_abm
}