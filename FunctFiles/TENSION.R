"
For the purposes of the ABM, tension is a dynamic, non-negative number assigned
between enemy gangs based on past hostilities. The higher the tension directed 
from gang A towards gang B, the more likely an agent from A will enact a hostility
on an agent from B during an interaction. 
"

############################## TENSION TUNING PARAMETERS ###############################


### SECTION: Min and Max Avg Tension
# choices
TENS_MAT_ENTRY_MIN_CHOICES <- seq(0, 1, 0.2 )
# selector
TensMatEntryMin_Selector <- function( ){
     sample( x = TENS_MAT_ENTRY_MIN_CHOICES, size = 1 )
}
# choices
TENS_MAT_ENTRY_MAX_CHOICES <- seq( 1.5, 5, 0.5 )
# selector
TensMatEntryMax_Selector <- function( ){
     sample( x = TENS_MAT_ENTRY_MAX_CHOICES, size = 1 )
}

### SECTION: HOW MUCH TO INCREASE THE MEAN BASED ON NUMBER OF HOSTILITIES
# choices
TENS_MAT_ENTRY_INCREASE_CHOICES <- list(
     Linear = list(
          PARAMETERS = list(
               slope = seq( 0.05, 0.25, 0.05 )
          ),
          INCREASE = function( params ){
               inc = params['slope']
               function( prev_tens, tens_count, minTens, maxTens ) min( maxTens, max(minTens, prev_tens) + (tens_count*inc) )
          },
          WEIGHT = 1
     )
)
# selector
TensMatEntryIncrease_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( TENS_MAT_ENTRY_INCREASE_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, TENS_MAT_ENTRY_INCREASE_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- TENS_MAT_ENTRY_INCREASE_CHOICES[[ choice ]]
     
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
          INCREASE = fam$INCREASE( params )
     )
     
     ChosenOne
}

### SECTION: HOW MUCH TO DECREASE THE MEAN BASED ON TIME PASSED SINCE LAST HOSTILITY
# choices
TENS_MAT_ENTRY_DECREASE_CHOICES <- list(
     FixedIncrement = list(
          PARAMETERS = list(
               increment = seq( 0, 0.05, 0.005 )
          ),
          DECREASE = function( params ){ 
               inc = params['increment']
               function( prev_tens, minTens, maxTens ) max( minTens, min(maxTens, prev_tens) - inc )
          },
          WEIGHT = 1
     )
)
# selector
TensMatEntryDecrease_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( TENS_MAT_ENTRY_DECREASE_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, TENS_MAT_ENTRY_DECREASE_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- TENS_MAT_ENTRY_DECREASE_CHOICES[[ choice ]]
     
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
          DECREASE = fam$DECREASE( params )
     )
     
     ChosenOne
}


### SECTION: HOW TENSION COUNTS HOSTILITIES BETWEEN NEIGHBORHOODS
# choices
TENS_COUNT_CHOICES <- list(
     # symmetric choices
     Max = list(
          TENSION_COUNT = max,
          WEIGHT = 1
     ),
     Min = list(
          TENSION_COUNT = min,
          WEIGHT = 0
     ),
     Sum = list(
          TENSION_COUNT = sum,
          WEIGHT = 1
     ),
     Avg = list(
          TENSION_COUNT = function( outward, inward ) mean( c(outward,inward) ),
          WEIGHT = 1
     ),
     # asymmetric choices
     Outward = list(
          TENSION_COUNT = function( outward, inward ) outward,
          WEIGHT = 1
     ),
     Inward = list(
          TENSION_COUNT = function( outward, inward ) inward,
          WEIGHT = 1
     )
)
# selector 
TensCount_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( TENS_COUNT_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, TENS_COUNT_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- TENS_COUNT_CHOICES[[ choice ]]
     
     # Create return list
     ChosenOne<-list(  
          NAME = choice,
          EFFECTIVE_HOSTILITY_COUNT = fam$TENSION_COUNT
     )
     
     ChosenOne
}

### SECTION: DURING AN INTERACTION, WHAT IS THE MEAN TENSION FELT BETWEEN THE NBHDS
# choices
EFFECTIVE_MEAN_TENS_CHOICES <- list( 
     MaxOverall = list(
          EFFECTIVE_MEAN_TENS = function( from_nbhd_tension_row, ... ) max( from_nbhd_tension_row ),
          WEIGHT = 1
     ),
     NbhdSpecific = list( 
          EFFECTIVE_MEAN_TENS = function( from_nbhd_tension_row, towards_nbhd_name ) from_nbhd_tension_row[ towards_nbhd_name ],
          WEIGHT = 1
     )
)
# selector
EffectiveMeanTens_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( EFFECTIVE_MEAN_TENS_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, EFFECTIVE_MEAN_TENS_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- EFFECTIVE_MEAN_TENS_CHOICES[[ choice ]]
     
     # Create return list
     ChosenOne<-list(  
          NAME = choice,
          CALCULATOR = fam$EFFECTIVE_MEAN_TENS
     )
     
     ChosenOne
}

### SECTION: BASED ON THE MEAN, WHAT IS THE ACTUAL TENSION FELT DURING INTERACTION
# choices
TENS_ON_INTERACTION_CHOICES <- list(
     # random
     TrunNormal = list(
          PARAMETERS = list(
               sig2 = seq( 0.05, 0.25, 0.05 )
          ),
          TENSION = function( params ){
               sig2 <- params['sig2']
               function( tens_avg, minTens = 0, maxTens = Inf ){
                    max( minTens, min( maxTens, rnorm( n = 1, mean = tens_avg, sd = sqrt(sig2) ) ) )
               }
          },
          WEIGHT = 1
     ),
     # fixed
     Fixed = list(
          TENSION = function( ... ){ function( tens_avg, ... ) tens_avg },
          WEIGHT = 1
     )
)
# selector
TensOnInteraction_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( TENS_ON_INTERACTION_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, TENS_ON_INTERACTION_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- TENS_ON_INTERACTION_CHOICES[[ choice ]]
     
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
          TENSION = fam$TENSION( params )
     )
     
     ChosenOne
}


############################# OVERALL PARAMETER SELECTION ####################################

ChooseTensionSettings <- function(){
     TENSION_SETTINGS <- list()
     ### Part 1: choose a min/max avg tension
     TENSION_SETTINGS$TENS_MAT_ENTRY_MIN <- TensMatEntryMin_Selector()
     TENSION_SETTINGS$TENS_MAT_ENTRY_MAX <- TensMatEntryMax_Selector()
     ### Part 2: choose how to inc/dec tension average
     TENSION_SETTINGS$TENS_MAT_ENTRY_INCREASE <- TensMatEntryIncrease_Selector()
     TENSION_SETTINGS$TENS_MAT_ENTRY_DECREASE <- TensMatEntryDecrease_Selector()
     ### Part 3: choose how to count hostilities in calculating tens avg
     TENSION_SETTINGS$TENS_COUNT <- TensCount_Selector()
     ### Part 4: choose the effective mean tension
     TENSION_SETTINGS$EFFECTIVE_MEAN_TENS <- EffectiveMeanTens_Selector()
     ### Part 5: choose how to calculate tension felt during interaction
     TENSION_SETTINGS$TENS_ON_INTERACTION <- TensOnInteraction_Selector()
     
     # return 
     TENSION_SETTINGS
}



############################## TENSION SIMULATION FUNCTIONALITY ###############################

# returns the function to update the tension matrix after each time step
SETUP_UPDATE_TENSION_MATRIX <- function( tension_stngs ){
     tens_count_calculator <- tension_stngs$TENS_COUNT$EFFECTIVE_HOSTILITY_COUNT
     increase_func <- tension_stngs$TENS_MAT_ENTRY_INCREASE$INCREASE
     decrease_func <- tension_stngs$TENS_MAT_ENTRY_DECREASE$DECREASE
     minTens <- tension_stngs$TENS_MAT_ENTRY_MIN
     maxTens <- tension_stngs$TENS_MAT_ENTRY_MAX
     ## using the tuning parameters to setup up how to calculate tension matrix entries
     tens_mat_entry_calculator <- function( prev_tens, outward, inward, ... ){
          tens_count <- tens_count_calculator( outward = outward, inward = inward )
          if( tens_count == 0 ){
               dec_value <- decrease_func(prev_tens = prev_tens, minTens = minTens, maxTens = maxTens, ...)
               return( dec_value )
               
          } else {
               return( increase_func(prev_tens = prev_tens, tens_count = tens_count, minTens = minTens, maxTens = maxTens, ...) )
          }
     }
     ## use the matrix entry calculator to update the matrix
     function( tension_matrix, new_hostilities, tools ){
          require( dplyr )
          # initialize the return tension matrix
          new_tm <- tension_matrix
          htysum <- new_hostilities %>% group_by( perp_nbhd, vict_nbhd ) %>% summarise( n = n() )
          
          # create the current hostility summarized matrix 
          chsm <- tools$CreateIndexedMatrix()
          n.htysum <- nrow( htysum )
          if(n.htysum > 0 ){
               for( i in 1:n.htysum ){
                    pn <- htysum$perp_nbhd[i]
                    vn <- htysum$vict_nbhd[i]
                    chsm[pn, vn] <- htysum$n[i]
               }
          }
          
          nbhds <- tools$GetNbhds()
          n.nbhds <- length( nbhds )
          for( i in 1:(n.nbhds - 1) ){
               nbhd1 <- nbhds[i]
               for( j in (1+1):n.nbhds ){
                    nbhd2 <- nbhds[j]
                    # nbhd1 -> nbhd2
                    prev_tens <- tension_matrix[nbhd1,nbhd2]
                    outward <- chsm[nbhd1, nbhd2]
                    inward <- chsm[nbhd2, nbhd1]
                    mat_entry <- tens_mat_entry_calculator( prev_tens = prev_tens, outward = outward, inward = inward )
                    new_tm[nbhd1, nbhd2] <- mat_entry
                    
                    # nbhd2 -> nbhd1
                    prev_tens <- tension_matrix[nbhd2,nbhd1]
                    outward <- chsm[nbhd2, nbhd1]
                    inward <- chsm[nbhd1, nbhd2]
                    mat_entry <- tens_mat_entry_calculator( prev_tens = prev_tens, outward = outward, inward = inward )
                    new_tm[nbhd2, nbhd1] <- mat_entry
               }
          }
          
          new_tm
     }
}

# returns the function to calculate the tension interaction after each timestep
SETUP_TENSION_ON_INTERACTION <- function( tension_stngs ){
     effective_mean_tens_calculator <- tension_stngs$EFFECTIVE_MEAN_TENS$CALCULATOR
     tens_on_interaction_calculator <- tension_stngs$TENS_ON_INTERACTION$TENSION
     function( from_nbhd, towards_nbhd, tension_matrix, ... ){
          tens_avg <- effective_mean_tens_calculator( tension_matrix[from_nbhd, ], towards_nbhd )
          tens_on_interaction_calculator( tens_avg, ... )
     }
}

