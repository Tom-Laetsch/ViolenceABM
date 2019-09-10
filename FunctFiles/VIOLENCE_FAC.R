"
At the beginning of a simulation, each hostile agent is assigned a violence factor,
which is interpreted as their individual propensity for violence. The violence factor
is then used by an individual agent A to adjust the probability that on interaction with an 
enemy agent B, agent A will enact a hostility on B. The convention is that the higher the 
violence factor, the more likely the agent will be hostile during interactions. 

Presently, we have the voilence factor distributions having mean 1, with a scaling factor
chosen in HOSTILITY.R which can adjust the mean. This gives us a more easily interpretable
balancing between the individual violence factor and the tension between gangs. 
"

############################################################

### SECTION: CHOICES OF DISTRIBUTION TO CREATE VIOLENCE FACTORS
# choices
VIOLENCE_FAC_CHOICES <- list(
     TruncExp =  list(
          PARAMETERS = list(
               mu = 1,
               minVal = seq( 0, 0.25, 0.01 ),
               maxVal = c( 2:10, Inf)
          ),
          DIST = function( params ){
               mu <- params['mu']
               minVal <- params['minVal']
               maxVal <- params['maxVal']
               function( n ){
                    ret <- rexp( n = n, rate = 1/mu )
                    ret[ ret < minVal ] <- minVal
                    ret[ ret > maxVal ] <- maxVal
                    ret
               }
          },
          WEIGHT = 1
     ),
     TruncGamma = list( 
          PARAMETERS = list(
               mu = 1,
               shape = 1:15,
               minVal = seq( 0, 0.25, 0.01 ),
               maxVal = c( 2:10, Inf)
          ),
          DIST = function( params ){
               mu = params['mu']
               shape = params['shape']
               rate = shape / mu
               minVal = params['minVal']
               maxVal = params['maxVal']
               function( n ){
                    ret <- rgamma( n = n, shape = shape, rate = rate )
                    ret[ ret < minVal ] <- minVal
                    ret[ ret > maxVal ] <- maxVal
                    ret
               }
          },
          WEIGHT = 1
     )
)
# selector
ViolenceFac_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( VIOLENCE_FAC_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, VIOLENCE_FAC_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- VIOLENCE_FAC_CHOICES[[ choice ]]
     
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
          DISTRIBUTION = fam$DIST( params )
     )
     
     ChosenOne
}


### SECTION: HOW VIOLENCE FAC IS FIGURED BETWEEN ENEMIES DURING INTERACTION
# choices
VIOLENCE_FAC_ON_INTERACTION_CHOICES <- list(
     # symmetric choices
     Max = list(
          PARAMETERS = NULL,
          CALCULATOR = function( ... ) max,
          WEIGHT = 1
     ),
     Min = list(
          PARAMETERS = NULL,
          CALCULATOR = function( ... ) min,
          WEIGHT = 0
     ),
     Sum = list(
          PARAMETERS = NULL,
          CALCULATOR = function( ... ) sum,
          WEIGHT = 0
     ),
     Avg = list(
          PARAMETERS = NULL,
          CALCULATOR = function( ... ) { function( perp_vf, vict_vf ) mean( c(perp_vf, vict_vf) ) },
          WEIGHT = 1
     ),
     # asymmetric choices
     Identity = list(
          PARAMETERS = NULL,
          CALCULATOR = function( ... ) { function( perp_vf, vict_vf ) perp_vf },
          WEIGHT = 1
     ),
     RandNorm = list(
          PARAMETERS = list(
               sig2 = seq(0.05,0.1,0.01)
          ),
          CALCULATOR = function( params ){
               sig2 = params['sig2']
               function( perp_vf, vict_vf ){
                    max(0, rnorm( n = 1, mean = perp_vf, sd = sqrt(sig2) ) )
               }
          },
          WEIGHT = 1
     )
)
# selector 
ViolenceFacOnInteraction_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( VIOLENCE_FAC_ON_INTERACTION_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, VIOLENCE_FAC_ON_INTERACTION_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- VIOLENCE_FAC_ON_INTERACTION_CHOICES[[ choice ]]
     
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
          CALCULATOR = fam$CALCULATOR( params = params )
     )
     
     ChosenOne
}


############################# OVERALL PARAMETER SELECTION ####################################

ChooseViolenceFacSettings <- function( ){
     VIOLENCE_FAC_SETTINGS <- list()
     # Select the distribution from which violence factors are drawn
     VIOLENCE_FAC_SETTINGS$VIOLENCE_FAC_DISTR <- ViolenceFac_Selector()
     # Select the calculator for determining the violence factor used on interaction between enemies
     VIOLENCE_FAC_SETTINGS$VIOLENCE_FAC_ON_INTERACTION <- ViolenceFacOnInteraction_Selector()
     # return
     VIOLENCE_FAC_SETTINGS
}


############################## VIOLENCE FAC SIMULATION FUNCTIONALITY ###############################

## Return the distribution function from which the violence factor is drawn
SETUP_VIOLENCE_FAC <- function( vf_stngs ){
     distr <- vf_stngs$VIOLENCE_FAC_DISTR$DISTRIBUTION
     # we could just return distr, but this makes clear the arguments
     function( n_agents, ... ){
          distr( n_agents, ... )
     }
}

## Return the violence factor felt on interaction
SETUP_VIOLENCE_FAC_ON_INTERACTION <- function( vf_stngs ){
     vfi_calc <- vf_stngs$VIOLENCE_FAC_ON_INTERACTION$CALCULATOR
     # we could just return vfi_calc, but this makes clear the arguments
     function( perp_vf, vict_vf, ... ){
          vfi_calc( perp_vf, vict_vf, ... )
     }
}