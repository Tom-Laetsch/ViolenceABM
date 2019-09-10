"
The 'PLAYGROUND' is the basic structure behind the ABM, keeping track of the summary:
- designations, family, and nbhd: the hierarchy of potential gang assignements (we consider
designation to be the alliances formed, family to be a specific gang, and neighborhood as
a single physical neighborhood occupied by a gang. The same gang family may spread over 
several nbhds, just as a single designation may spread over several families. The reason for 
this hierarchy is that tension is considered a nbhd-level factor, whereas dominance is considered
a family-level factor. Two agents are enemies when they come from different designations.)
- n_agents and n_tiles: the number of agents in a given nbhd, and the number of tiles assigned to those
agents. The larger the number of tiles, the physically 'larger' we consider the nhbd size is considered; 
e.g., with a 
large nbhd (many tiles) and a small number of agents, interactions within that nbhd are less than 
the opposite scenario.
"
#####################################################
# choices
NUM_DESIG_CHOICES <- 2:3
# selector
NumDesig_Selector <- function( ){
     sample( x = NUM_DESIG_CHOICES, size = 1 )
}

# choices
NUM_FAM_PER_DESIG_CHOICES <- list(
     Uniform = list(
          PARAMETERS = list(
               minNum = 1:2,
               spread = 0:1
          ),
          NUM_FAM = function( params ){
               minNum <- params['minNum']
               maxNum <- minNum + params['spread']
               function( n ){
                    sample( x = minNum:maxNum, size = n, replace = T )
               }
          },
          WEIGHT = 1
     )
)
# selector
NumFamPerDesig_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( NUM_FAM_PER_DESIG_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, NUM_FAM_PER_DESIG_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- NUM_FAM_PER_DESIG_CHOICES[[ choice ]]
     
     # Select corresponding parameters
     param_names <- names( fam$PARAMETERS )
     params <- c()
     for( nm in param_names ){
          params <- c( params, sample(fam$PARAMETERS[[nm]])[1] )
     }
     names(params) <- param_names
     
     # Create return list
     ChosenOne<-list(  
          NAME = choice,
          CHOSEN_PARAMETERS = params,
          CHOOSER = fam$NUM_FAM( params )
     )
     
     ChosenOne
}

# choices
NUM_NBHD_PER_FAM_CHOICES <- list( 
     Uniform = list(
          PARAMETERS = list(
               minNum = 1:2,
               spread = 0:1
          ),
          NUM_NBHD = function( params ){
               minNum <- params['minNum']
               maxNum <- minNum + params['spread']
               function( n ){
                    sample( x = minNum:maxNum, size = n, replace = T )
               }
          },
          WEIGHT = 1
     )
)
# selector
NumNbhdPerFam_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( NUM_NBHD_PER_FAM_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, NUM_NBHD_PER_FAM_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- NUM_NBHD_PER_FAM_CHOICES[[ choice ]]
     
     # Select corresponding parameters
     param_names <- names( fam$PARAMETERS )
     params <- c()
     for( nm in param_names ){
          params <- c( params, sample(fam$PARAMETERS[[nm]])[1] )
     }
     names(params) <- param_names
     
     # Create return list
     ChosenOne<-list(  
          NAME = choice,
          CHOSEN_PARAMETERS = params,
          CHOOSER = fam$NUM_NBHD( params )
     )
     
     ChosenOne
}

# choices
NUM_AGENTS_PER_NBHD_CHOICES <- list(
     Uniform = list(
          PARAMETERS = list(
               minNum = seq(1000,5000,1000),
               spread = seq(1000,10000,1000)
          ),
          NUM_AGENTS = function( params ){
               minNum <- params['minNum']
               maxNum <- minNum + params['spread']
               function( n ){
                    sample( x = minNum:maxNum, size = n, replace = T )
               }
          },
          WEIGHT = 1
     )
)
# selector
NumAgentsPerNbhd_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( NUM_AGENTS_PER_NBHD_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, NUM_AGENTS_PER_NBHD_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- NUM_AGENTS_PER_NBHD_CHOICES[[ choice ]]
     
     # Select corresponding parameters
     param_names <- names( fam$PARAMETERS )
     params <- c()
     for( nm in param_names ){
          params <- c( params, sample(fam$PARAMETERS[[nm]])[1] )
     }
     names(params) <- param_names
     
     # Create return list
     ChosenOne<-list(  
          NAME = choice,
          CHOSEN_PARAMETERS = params,
          CHOOSER = fam$NUM_AGENTS( params )
     )
     
     ChosenOne
}



# choices
NUM_TILES_PER_NBHD_CHOICES <- list(
     DynMultFac = list(
          PARAMETERS = NULL,
          NUM_TILES = function( ... ){
               function( populations ){
                    n = length( populations )
                    mfs = sample( x = seq(1, 3, 0.5), size = n, replace = T )
                    round(mfs * populations)
               }
          }, 
          WEIGHT = 1
     ),
     StatMultFac = list(
          PARAMETERS = list(
               multfac = seq(1, 3, 0.5)
          ),
          NUM_TILES = function( params ){
               mf = params['multfac']
               function( populations ) round(mf * populations)
          },
          WEIGHT = 1
     )
)
# selector
NumTilesPerNbhd_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( NUM_TILES_PER_NBHD_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, NUM_TILES_PER_NBHD_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     
     choice <- sample( families, size = 1, prob = pdf )
     fam <- NUM_TILES_PER_NBHD_CHOICES[[ choice ]]
     
     # Select corresponding parameters
     param_names <- names( fam$PARAMETERS )
     params <- c()
     for( nm in param_names ){
          params <- c( params, sample(fam$PARAMETERS[[nm]])[1] )
     }
     names(params) <- param_names
     
     # Create return list
     ChosenOne<-list(  
          NAME = choice,
          CHOSEN_PARAMETERS = params,
          CHOOSER = fam$NUM_TILES( params )
     )
     
     ChosenOne
}

ChoosePlaygroundSettings <- function( ){
     PLAYGROUND_SETTINGS <- list()
     # choose the number of designations
     PLAYGROUND_SETTINGS$NUM_DESIG <- NumDesig_Selector()
     # choose the number of families each designation has
     PLAYGROUND_SETTINGS$NUM_FAM_PER_DESIG <- NumFamPerDesig_Selector()
     # choose the number of nbhds each family has
     PLAYGROUND_SETTINGS$NUM_NBHD_PER_FAM <- NumNbhdPerFam_Selector()
     # choose the number of agents living in each nbhd
     PLAYGROUND_SETTINGS$NUM_AGENTS_PER_NBHD <- NumAgentsPerNbhd_Selector()
     # choose the number of tiles per nbhd
     PLAYGROUND_SETTINGS$NUM_TILES_PER_NBHD <- NumTilesPerNbhd_Selector()
     # return
     PLAYGROUND_SETTINGS
}


SETUP_PLAYGROUND <- function( playground_stngs ){
     
     namer <- function( pref, nums ){
          ret <- c()
          for( n in nums ){
               ret <- c( ret, paste0(pref, 1:n) )
          }
          ret
     }
     
     n_desigs <- playground_stngs$NUM_DESIG
     n_fam_per_desig <- playground_stngs$NUM_FAM_PER_DESIG$CHOOSER( n = n_desigs )
     n_tot_fam <- sum( n_fam_per_desig )
     n_nbhd_per_fam <- playground_stngs$NUM_NBHD_PER_FAM$CHOOSER( n = n_tot_fam )
     n_tot_nbhd <- sum( n_nbhd_per_fam )
     
     # start by naming designations as d1, d2, etc
     desigs <- paste0( 'd', 1:n_desigs)
     # repeat desigs to match number of fams; e.g., d1, d1, d2, d3, d3, d3, etc
     # when n_fam_per_desig = c(2,1,3,...)
     desigs <- rep( desigs, n_fam_per_desig )
     # name the families, including designation for disambiguation: d1f1, d1f2, d2f1, etc
     fams <- paste0( desigs, namer( pref = 'f', nums = n_fam_per_desig ) )
     
     # continue like above now using nbhds
     desigs <- rep( desigs, n_nbhd_per_fam )
     fams <- rep( fams, n_nbhd_per_fam )
     nbhds <- paste0( fams, namer(pref = 'n', nums = n_nbhd_per_fam) )
     
     # get the number of agents per nbhd
     n_agents_per_nbhd <- playground_stngs$NUM_AGENTS_PER_NBHD$CHOOSER( n = n_tot_nbhd )
     
     # get the number of tiles per nbhd
     n_tiles_per_nbhd <- playground_stngs$NUM_TILES_PER_NBHD$CHOOSER( populations = n_agents_per_nbhd )
     
     # return PLAYGROND data.frame
     data.frame(
          designation = desigs,
          family = fams,
          nbhd = nbhds,
          n_agents = n_agents_per_nbhd,
          n_tiles = n_tiles_per_nbhd,
          stringsAsFactors = F
     )
     
}
