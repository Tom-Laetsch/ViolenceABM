###################################################
### SETUP BY SELECTING SETTINGS (TUNING PARAMETERS)

.PerformSetup <- function( ){
     # from PLAYGROUND.R
     source( 'FunctFiles/PLAYGROUND.R' )
     .pg_stngs <- ChoosePlaygroundSettings()
     .PLAYGROUND <- SETUP_PLAYGROUND( .pg_stngs )
     
     .CreateTiles <- function( playground ){
          nbhd <- rep( playground$nbhd, playground$n_tiles )
          tile <- paste0('tile',1:length(nbhd))
          data.frame( nbhd = nbhd, tile = tile, stringsAsFactors = F)
     }
     .TILES <- .CreateTiles( .PLAYGROUND )
     
     # from VIOLENCE_FAC.R
     source( 'FunctFiles/VIOLENCE_FAC.R' )
     .vf_stngs <- ChooseViolenceFacSettings()
     .VIOLENCE_FAC <- SETUP_VIOLENCE_FAC( .vf_stngs )
     .VIOLENCE_FAC_ON_INTERACTION <- SETUP_VIOLENCE_FAC_ON_INTERACTION( .vf_stngs )
     
     # from TENSION.R
     source( 'FunctFiles/TENSION.R' )
     .tens_stngs <- ChooseTensionSettings()
     .UPDATE_TENSION_MATRIX <- SETUP_UPDATE_TENSION_MATRIX( .tens_stngs )
     .TENSION_ON_INTERACTION <- SETUP_TENSION_ON_INTERACTION( .tens_stngs )
     
     # from IMPART_HOSTILITY.R
     source( 'FunctFiles/IMPART_HOSTILITY.R' )
     .ih_stngs <- ChooseHostilitySettings()
     .IMPART_HOSTILITY <- SETUP_IMPART_HOSTILITY( .ih_stngs )
     
     # from MOVEMENT.R
     source( 'FunctFiles/MOVEMENT.R' )
     .mv_stngs <- ChooseMovementSettings()
     .STAYING_PROBS_CREATOR <- SETUP_STAYING_PROBS_CREATOR( .mv_stngs )
     .DISTANCE_MATRIX_CREATOR <- SETUP_DISTANCE_MATRIX_CREATOR( .mv_stngs )
     
     source( 'FunctFiles/INJURY.R' )
     .injury_stngs <- ChooseInjurySettings()
     .INJURY_DURATION <- SETUP_INJURY_DURATION( .injury_stngs )
     .INJURY_DURATION_ON_OVERLAP <- SETUP_INJURY_DURATION_ON_OVERLAP( .injury_stngs )
     
     # TOOLS
     .NEW_NBHD_MATRIX <- function(){
          nbhds <- .PLAYGROUND$nbhd
          d <- length( nbhds )
          ret_mat <- matrix( 0, d, d )
          colnames(ret_mat) <- nbhds
          rownames(ret_mat) <- nbhds
          ret_mat
     }
     
     ##### STORE THE SETUP IN A CONVENIENT LIST
     SETUP <- list(
          # PG stuff
          PLAYGROUND_SETTINGS = .pg_stngs,
          PLAYGROUND = .PLAYGROUND,
          TILES = .TILES,
          # VF stuff
          VIOLENCE_FAC_SETTINGS = .vf_stngs,
          VIOLENCE_FAC = .VIOLENCE_FAC,
          VIOLENCE_FAC_ON_INTERACTION = .VIOLENCE_FAC_ON_INTERACTION,
          # TENS stuff
          TENSION_SETTINGS = .tens_stngs,
          UPDATE_TENSION_MATRIX = .UPDATE_TENSION_MATRIX,
          TENSION_ON_INTERACTION = .TENSION_ON_INTERACTION,
          # IH stuff
          IMPART_HOSTILITY_SETTINGS = .ih_stngs,
          IMPART_HOSTILITY = .IMPART_HOSTILITY,
          # INJ stuff
          INJURY_DURATION = .INJURY_DURATION,
          INJURY_DURATION_ON_OVERLAP = .INJURY_DURATION_ON_OVERLAP,
          # MV STUFF
          MOVEMENT_SETTINGS = .mv_stngs,
          STAYING_PROBS_CREATOR = .STAYING_PROBS_CREATOR,
          DISTANCE_MATRIX_CREATOR = .DISTANCE_MATRIX_CREATOR
     )
     
     SETUP
}

########################################
##### INITIALIZE THE NECESSARY VARIABLES

# helper functions in initialization
NamedMatrixInitializer<- function( rc_names ){
     # rc_names: row and col names for the matrix
     d <- length( rc_names )
     ret_mat <- matrix( 0, d, d )
     colnames(ret_mat) <- rc_names
     rownames(ret_mat) <- rc_names
     ret_mat
}

.HostilesInitializer <- function( playground, violence_fac_dist ){
     desigs <- rep( playground$designation, playground$n_agents )
     fams <- rep( playground$family, playground$n_agents )
     nbhds <- rep( playground$nbhd, playground$n_agents )
     
     hdf <- data.frame( designation = desigs,
                        family = fams,
                        base_nbhd = nbhds,
                        stringsAsFactors = F )
     n_hostiles <- nrow(hdf)
     
     hdf$violence_fac <- violence_fac_dist( n_hostiles )
     hdf$out_of_commission <- 0
     hdf$uid <- paste0( 'hostile', 1:n_hostiles )
     hdf
}

.HostilityHistInitializer <- function( ){
     data.frame(
          timestep = integer(),
          perp_uid = character(),
          perp_desig = character(),
          perp_family = character(),
          perp_base_nbhd = character(),
          vict_uid = character(),
          vict_desig = character(),
          vict_family = character(),
          vict_base_nbhd = character(),
          stringsAsFactors = F
     )
}

.TensionMatrixInitializer <- function( nbhds, min_tens ){
     n.nbhds <- length( nbhds )
     tm <- matrix( 0, n.nbhds, n.nbhds )
     rownames(tm) <- nbhds
     colnames(tm) <- nbhds
     
     tm[ upper.tri(tm) ] <- min_tens 
     tm[ lower.tri(tm) ] <- min_tens
     
     tm
}

.StayingProbsInitializer <- function( playground, staying_probs_creator, ... ){
     staying_probs_creator( playground$nbhd, ... )
}

.DistanceMatrixInitializer <- function( playground, distance_matrix_creator, ... ){
     distance_matrix <- NamedMatrixInitializer( playground$nbhd )
     distance_matrix_creator( distance_matrix )
}


.CreateMovingProbMatrix <- function( playground, staying_probs, distance_matrix, ... ){
     index_set <- playground$nbhd
     n.indices <- length(index_set)
     ret_mat <- matrix( 0, n.indices, n.indices )
     rownames(ret_mat) <- index_set
     colnames(ret_mat) <- index_set 
     for( idx1 in index_set ){
          p_stay <- staying_probs[idx1]
          p_out <- 1.0 - p_stay
          probs <- numeric( )
          probs[idx1] <- p_stay
          
          for( idx2 in index_set ){
               if( identical(idx1, idx2) ) next
               probs[idx2] <- 1.0 / distance_matrix[idx1,idx2]
          }
          
          p2 <- probs[ names(probs) != idx1 ]
          probs[ names(probs) != idx1 ] <- p_out * ( p2 / sum( p2 ) )
          ret_mat[ idx1, names(probs) ] <- probs
     }
     ret_mat
}

.InitializeVariables <- function( SETUP ){
     # initialize the variables 
     HOSTILES <- .HostilesInitializer( playground = SETUP$PLAYGROUND, violence_fac_dist = SETUP$VIOLENCE_FAC )
     HOSTILITY_HIST <- .HostilityHistInitializer()
     TENSION_MATRIX <- .TensionMatrixInitializer( nbhds = SETUP$PLAYGROUND$nbhd, min_tens = SETUP$TENSION_SETTINGS$TENS_MAT_ENTRY_MIN )
     STAYING_PROBS <- .StayingProbsInitializer( playground = SETUP$PLAYGROUND, 
                                                staying_probs_creator = SETUP$STAYING_PROBS_CREATOR )
     DISTANCE_MATRIX <- .DistanceMatrixInitializer( playground = SETUP$PLAYGROUND, 
                                                                 distance_matrix_creator = SETUP$DISTANCE_MATRIX_CREATOR )
     MOVEMENT_PROB_MATRIX <- .CreateMovingProbMatrix( playground = SETUP$PLAYGROUND,
                                                      staying_probs = STAYING_PROBS,
                                                      distance_matrix = DISTANCE_MATRIX )
     TIMESTEP <- 1
     VARIABLES <- list( 
          HOSTILES = HOSTILES,
          HOSTILITY_HIST = HOSTILITY_HIST,
          TENSION_MATRIX = TENSION_MATRIX,
          STAYING_PROBS = STAYING_PROBS,
          DISTANCE_MATRIX = DISTANCE_MATRIX,
          MOVEMENT_PROB_MATRIX = MOVEMENT_PROB_MATRIX,
          TIMESTEP = TIMESTEP 
     )
     
     VARIABLES
}


.CreateTools <- function( SETUP ){
     nbhds <- unique( SETUP$PLAYGROUND$nbhd )
     desigs <- unique( SETUP$PLAYGROUND$designation )
     families <- unique( SETUP$PLAYGROUND$family )
     
     CreateIndexedMatrix <- function( idxs ){
          n.idxs <- length( idxs )
          mat <- matrix( 0, n.idxs, n.idxs )
          rownames( mat ) <- idxs
          colnames( mat ) <- idxs
          mat
     }
     
     TOOLS <- list(
          GetNbhds = function(...) nbhds,
          GetDesigs = function( ... ) desigs,
          GetFamilies = function( ... ) families,
          TakeSnapshot = function( abm, output_fp, ... ) saveRDS( object = abm, file = output_fp, ... ),
          OpenSnapshot = function( ss_fp, ... ) readRDS( ss_fp, ... ),
          CreateIndexedMatrix = function( idxs = nbhds ) CreateIndexedMatrix( idxs ),
          NbhdsByDesig = function( desig ) SETUP$PLAYGROUND$nbhd[ SETUP$PLAYGROUND$designation == desig ],
          FamsByDesig = function( desig ) SETUP$PLAYGROUND$family[ SETUP$PLAYGROUND$designation == desig ],
          NbhdsByFam = function( fam ) SETUP$PLAYGROUND$nbhd[ SETUP$PLAYGROUND$family == fam ]
     )
     
     TOOLS
     
}
#################################################
##### CREATE THE MAIN LIST USED IN NEWLY RUN ABM

InitializeABM <- function( ){
     SETUP <- .PerformSetup()
     VARIABLES <- .InitializeVariables( SETUP )
     TOOLS <- .CreateTools( SETUP )
     list(
          SETUP = SETUP,
          TOOLS = TOOLS,
          VARIABLES = VARIABLES
          )
}