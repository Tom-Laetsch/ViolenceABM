
# choices
STAYING_PROBS_CHOICES <- list(
     Uniform = list(
          PARAMETERS = list(
               minVal = seq(0.99, 0.999, 0.001),
               maxVal = seq(0.9999, 0.99999, 0.00001)
          ),
          DIST = function( params, ... ){
               minVal = params['minVal']
               maxVal = params['maxVal']
               function(n){
                    runif( n = n, min = minVal, max = maxVal )
               }
          },
          WEIGHT = 1
     )
)
# selector
StayingProbs_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( STAYING_PROBS_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, STAYING_PROBS_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- STAYING_PROBS_CHOICES[[ choice ]]
     
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
          DIST = fam$DIST( params )
     )
     
     ChosenOne
}

# choices
DISTANCE_MATRIX_ENTRY_CHOICES <- list(
     Uniform = list(
          PARAMETERS = list(
               minVal = 1,
               maxVal = 25
          ),
          DIST = function( params ){
               minVal = params['minVal']
               maxVal = params['maxVal']
               function( n ){
                    runif( n = n, min = minVal, max = maxVal )
               }
          },
          WEIGHT = 1
     )
)
# selector
DistanceMatrixEntry_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( DISTANCE_MATRIX_ENTRY_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, DISTANCE_MATRIX_ENTRY_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- DISTANCE_MATRIX_ENTRY_CHOICES[[ choice ]]
     
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
          DIST = fam$DIST( params )
     )
     
     ChosenOne
}

# choices
DISTANCE_TO_WEIGHT_CHOICES <- list(
     Inverse = list(
          PARAMETERS = list(
               pow = c(1,2,3)
          ),
          CALCULATOR = function( params ){
               p = params['pow']
               function( d ){
                    1.0 / (d^p)
               }
          },
          WEIGHT = 1
     )
)
# selector
DistanceToWeight_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( DISTANCE_TO_WEIGHT_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, DISTANCE_TO_WEIGHT_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- DISTANCE_TO_WEIGHT_CHOICES[[ choice ]]
     
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
          CALCULATOR = fam$CALCULATOR( params )
     )
     
     ChosenOne
}



############################# OVERALL PARAMETER SELECTION ####################################

ChooseMovementSettings <- function(){
     MOVEMENT_SETTINGS <- list()
     MOVEMENT_SETTINGS$STAYING_PROBS <- StayingProbs_Selector()
     MOVEMENT_SETTINGS$DISTANCE_MATRIX_ENTRY <- DistanceMatrixEntry_Selector()
     MOVEMENT_SETTINGS$DISTANCE_TO_WEIGHT <- DistanceToWeight_Selector()
     # return 
     MOVEMENT_SETTINGS
}

SETUP_STAYING_PROBS_CREATOR <- function( mv_stngs ){
     f <- mv_stngs$STAYING_PROBS$DIST
     function( nbhd_names ){
          n <- length(nbhd_names)
          staying_probs <- f( n )
          names( staying_probs ) <- nbhd_names
          staying_probs
     }
}


SETUP_DISTANCE_MATRIX_CREATOR <- function( mv_stngs ){
     distance_distribution <- mv_stngs$DISTANCE_MATRIX_ENTRY$DIST
     function( init_matrix ){
          ret_mat <- init_matrix
          ut <- upper.tri(ret_mat)
          n.entr <- sum( ut )
          ret_mat[ upper.tri(ret_mat) ] <- distance_distribution( n.entr )
          ret_mat[ lower.tri(ret_mat) ] <- t(ret_mat)[ lower.tri(ret_mat) ]
          ret_mat
     }
}


SETUP_DISTANCE_WEIGHT_MATRIX_CREATOR <- function( mv_stngs ){
     distance_distribution <- mv_stngs$DISTANCE_MATRIX_ENTRY$DIST
     dist_to_wgt <- mv_stngs$DISTANCE_TO_WEIGHT$CALCULATOR
     dw_entry_calc <- function(  ){
          d <- distance_distribution( 1 )
          dist_to_wgt( d )
     }
     function( initialized_nbhd_matrix ){
          dw_matrix <- initialized_nbhd_matrix
          nbhds <- rownames( initialized_nbhd_matrix )
          n.nbhds <- length( nbhds )
          for( i in 1:(n.nbhds-1) ){
               for(j in (i+1):n.nbhds ){
                    n1 <- nbhds[i]
                    n2 <- nbhds[j]
                    dw_matrix[n1,n2] <- dw_entry_calc()
                    dw_matrix[n2,n1] <- dw_matrix[n1,n2]
               }
          }
          
          dw_matrix
     }
}

