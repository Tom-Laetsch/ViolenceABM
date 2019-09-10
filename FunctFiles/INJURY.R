"
An injury can occur during an interaction between two enemy agents. The duration of the injury is randomly assigned in the code below.
Death is also a possibility, considered as an injury with an infinite duration.
The injury duration (if any) will be summed to the agent's 'out_of_commission' feature, and remove them from the ABM
for that number of timesteps.
"
############################################################
.quantileApproximator <- function( quant_p, distr, n_trials = 1000, ...  ){
     x <- distr( n = n_trials, ... )
     quantile( x, quant_p )
}

INJURY_DURATION_CHOICES <- list(
     Exp =  list(
          PARAMETERS = list(
               p_inj = seq(0.25, 0.75, 0.05),
               mu = seq(100,300,25),
               p_death_if_inj = seq( 0.1, 0.3, 0.05 ),
               minInjDuration = seq( 5, 20, 5 )
          ),
          DIST = function( params ){
               # pull out the parameters to set up the distribution
               p_inj <- params['p_inj']
               mu <- params['mu']
               minInjDuration <- params['minInjDuration']
               #calculate the threshold for death based on quantile percentage
               p_dii <- params['p_death_if_inj']
               deathAfter <- .quantileApproximator( quant_p = 1-p_dii, distr = rexp, rate = 1/mu )
               function( n ){
                    # sample Bernoulli to decide if injury occurs
                    inj <- rbinom( n = n, size = 1, prob = p_inj )
                    # for those injured, calculate injury length
                    n_inj <- length(inj[inj == 1])
                    if( n_inj > 0 ){
                         inj_durs <- rexp( n = n_inj, rate = 1/mu )
                         inj_durs <- sapply( X = inj_durs, FUN = function(x) ifelse( x >= deathAfter, Inf, max( minInjDuration, round(x) ) ) )
                         inj[ inj == 1 ] <- inj_durs
                    }
                    inj
               }
          },
          WEIGHT = 1
     ),
     Norm =  list(
          PARAMETERS = list(
               p_inj = seq(0.25, 0.75, 0.05),
               mu = seq(50,150,25),
               sig2 = seq(400,1600,25),
               p_death_if_inj = seq( 0.1, 0.3, 0.05 ),
               minInjDuration = seq(5,20,5)
          ),
          DIST = function( params ){
               # pull out the parameters to set up the distribution
               p_inj <- params['p_inj']
               mu <- params['mu']
               sig2 <- params['sig2']
               minInjDuration <- params['minInjDuration']
               #calculate the threshold for death based on quantile percentage
               p_dii <- params['p_death_if_inj']
               deathAfter <- .quantileApproximator( quant_p = 1-p_dii, distr = rnorm, mean = mu, sd = sqrt(sig2) )
               function( n ){
                    # sample Bernoulli to decide if injury occurs
                    inj <- rbinom( n = n, size = 1, prob = p_inj )
                    # for those injured, calculate injury length
                    n_inj <- length(inj[inj == 1])
                    if( n_inj > 0 ){
                         inj_durs <- rnorm( n = n_inj, mean = mu, sd = sqrt(sig2) )
                         inj_durs <- sapply( X = inj_durs, FUN = function(x) ifelse( x >= deathAfter, Inf, max( minInjDuration, round(x) ) ) )
                         inj[ inj == 1 ] <- inj_durs
                    }
                    inj
               }
          },
          WEIGHT = 1
     )
)


##### HERE IS AN EXAMPLE OF HOW WE MIGHT RANDOMLY CHOOSE THE DISTRIBUTION FROM THE ABOVE LIST
InjuryDuration_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( INJURY_DURATION_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, INJURY_DURATION_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- INJURY_DURATION_CHOICES[[ choice ]]
     
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
OVERLAPPING_INJURY_HANDLER_CHOICES <- list(
     Max = list(
          INJURY_DURATION = max,
          WEIGHT = 1
     ),
     Sum = list(
          INJURY_DURATION = sum,
          WEIGHT = 1
     )
)
# selector 
OverlappingInjuryHandler_Selector <- function( ){
     
     # Select which distribution family (based on weights)
     families <- names( OVERLAPPING_INJURY_HANDLER_CHOICES )
     x <- numeric()
     for( nm in families ){
          x <- c( x, OVERLAPPING_INJURY_HANDLER_CHOICES[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- OVERLAPPING_INJURY_HANDLER_CHOICES[[ choice ]]
     
     # Create return list
     ChosenOne<-list(  
          NAME = choice,
          CALCULATOR = fam$INJURY_DURATION
     )
     
     ChosenOne
}


ChooseInjurySettings <- function(){
     INJURY_SETTINGS <- list()
     INJURY_SETTINGS$INJURY_DURATION <- InjuryDuration_Selector()
     INJURY_SETTINGS$INJURY_DURATION_ON_OVERLAP <- OverlappingInjuryHandler_Selector()
     
     # return 
     INJURY_SETTINGS
}

SETUP_INJURY_DURATION <- function( injury_stngs ) injury_stngs$INJURY_DURATION$DIST

SETUP_INJURY_DURATION_ON_OVERLAP <- function( injury_stngs ) injury_stngs$INJURY_DURATION_ON_OVERLAP$CALCULATOR