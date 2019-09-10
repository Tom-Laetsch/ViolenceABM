########################################################################
##################################### DECIDING WHO TO (OR NOT TO) ATTACK

ALLOW_ATTACKS <- F


TO_ATTACK_AUX_FUN_SPACE <- list(
     ViaGainingDominance = list(
          PARAMETERS = list(
               dom_thres = c( seq(from = 5, to = 15, by = 5), Inf ),
               allow_multiple_attacks = c(T,F)
          ),
          WHO_TO_ATTACK = function( params ){
               
               dom_thres <- max( params['dom_thres'], 0 )
               
               allow_multiple_attacks <- params['allow_multiple_attacks']
               
               attack_yes_no <- function( dom_val, zero_offset = 0.01 ){
                    p <- min( abs(dom_val), dom_thres ) / ( dom_thres + zero_offset )
                    return( sample(c(T,F), size = 1, prob = c(p, 1-p)) )
               }
               
               function( hostile_fam ){
                    # requires DOMINANCE MATRIX
                    if( !exists('DOMINANCE_MATRIX') ) return(NULL)
                    if( nrow(DOMINANCE_MATRIX) == 0 ) return(NULL)
                    
                    dom_vec <- DOMINANCE_MATRIX[ hostile_fam, ]
                    min.dom <- min( dom_vec )
                    if( min.dom >= 0 ) return(NULL)
                    
                    # negative dominance means the hostile_fam is "lower" on the dominance than the other family
                    dom_vec <- dom_vec[ dom_vec < 0 ]
                    
                    to_attack <- sapply( dom_vec, FUN = attack_yes_no )
                    if( !any( to_attack ) ) return( NULL )
                    to_attack <- names( to_attack[ to_attack ] )
                    if( ! allow_multiple_attacks ) to_attack <- sample( to_attack, size = 1 )
                    return( to_attack )
               }
          },
          WEIGHT = 1
     ),
     ViaRelievingTension = list(
          PARAMETERS = list(
               
          ),
          WHO_TO_ATTACK = function( params ){
               
          },
          WEIGHT = 0
     )
)

##### HERE IS AN EXAMPLE OF HOW WE MIGHT RANDOMLY CHOOSE THE DISTRIBUTION FROM THE ABOVE LIST
ToAttack_Selector <- function( ){
     
     if( !ALLOW_ATTACKS ){
          return( 
               list(
                    NAME = 'OFF',
                    CHOSEN_PARAMETERS = NULL,
                    FUN = function( ... ) NULL
               )
          )
     }
     
     # Select which distribution family (based on weights)
     vias <- names( TO_ATTACK_AUX_FUN_SPACE )
     x <- numeric()
     for( nm in vias ){
          x <- c( x, TO_ATTACK_AUX_FUN_SPACE[[nm]]$WEIGHT )
     }
     names(x) <- vias
     pdf <- x / sum( x )
     choice <- sample( vias, size = 1, prob = pdf )
     via <- TO_ATTACK_AUX_FUN_SPACE[[ choice ]]
     
     # Select corresponding parameters
     param_names <- names( via$PARAMETERS )
     params <- c()
     for( nm in param_names ){
          params <- c( params, sample(via$PARAMETERS[[nm]])[1] )
     }
     names(params) <- param_names
     
     # Create return list
     ChosenOne<-list(  
          NAME = choice,
          CHOSEN_PARAMETERS = params,
          FUN = via$WHO_TO_ATTACK( params )
     )
     
     ChosenOne
}

########################################################################
########################################### GIVEN AN ATTACK, HOW SEVERE? 

ATTACK_SEVERITY_AUX_FUN_SPACE <- list(
     Uniform = list(
          PARAMETERS = list(
               min_val = 1:3,
               spread = 1:3
          ),
          DIST = function( params ){
               min_val <- params['min_val']
               max_val <- min_val + params['spread']
               function( n ){
                    sample( x = min_val:max_val, size = 1 )
               }
          },
          WEIGHT = 1
     )
)

##### HERE IS AN EXAMPLE OF HOW WE MIGHT RANDOMLY CHOOSE THE DISTRIBUTION FROM THE ABOVE LIST
AttackSeverity_Selector <- function( ){
     
     if( !ALLOW_ATTACKS ){
          return(
               list(
                    NAME = 'OFF',
                    CHOSEN_PARAMETERS = NULL,
                    FUN = function(...) NULL
               )
          )
     }
     
     # Select which distribution family (based on weights)
     families <- names( ATTACK_SEVERITY_AUX_FUN_SPACE )
     x <- numeric()
     for( nm in families ){
          x <- c( x, ATTACK_SEVERITY_AUX_FUN_SPACE[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- ATTACK_SEVERITY_AUX_FUN_SPACE[[ choice ]]
     
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
          FUN = fam$DIST( params )
     )
     
     ChosenOne
}