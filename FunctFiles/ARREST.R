###########################################################
quantileApproximator <- function( quant_p, distr, n_trials = 10000, ...  ){
     x <- distr( n = n_trials, ... )
     quantile( x, quant_p )
}

ARREST_AUX_FUN_SPACE <- list(
     Exp =  list(
          PARAMETERS = list(
               p_arrest = seq(0.25, 0.75, 0.05),
               mu = seq(100,300,25),
               p_life_if_arrest = seq( 0.1, 0.3, 0.05 ),
               minArrestDuration = seq( 5, 20, 5 )
          ),
          DIST = function( params ){
               # pull out the parameters to set up the distribution
               p_arrest <- params['p_arrest']
               mu <- params['mu']
               minArrestDuration <- params['minArrestDuration']
               # calculate the threshold for life in prison based on quantile percentage
               p_lia <- params['p_life_if_arrest']
               lifeAfter <- quantileApproximator( quant_p = 1-p_lia, distr = rexp, rate = 1/mu )
               function( n ){
                    # sample Bernoulli to decide if arrest occurs
                    arrest <- rbinom( n = n, size = 1, prob = p_arrest )
                    # for those arrested, calculate arrest duration
                    n_arrest <- length(arrest[arrest == 1])
                    if( n_arrest > 0 ){
                         # use exponential distribution to select arrest duration
                         arrest_durs <- rexp( n = n_arrest, rate = 1/mu )
                         # is it a life sentence?
                         arrest_durs <- sapply( X = arrest_durs, FUN = function(x) ifelse( x >= lifeAfter, Inf, max( minArrestDuration, round(x) ) ) )
                         arrest[ arrest == 1 ] <- arrest_durs
                    }
                    arrest
               }
          },
          WEIGHT = 1
     ),
     Norm =  list(
          PARAMETERS = list(
               p_arrest = seq(0.25, 0.75, 0.05),
               mu = seq(50,150,25),
               sig2 = seq(400,1600,25),
               p_life_if_arrest = seq( 0.1, 0.3, 0.05 ),
               minArrestDuration = seq(0,20,5)
          ),
          DIST = function( params ){
               # pull out the parameters to set up the distribution
               p_arrest <- params['p_arrest']
               mu <- params['mu']
               sig2 <- params['sig2']
               minArrestDuration <- params['minArrestDuration']
               # calculate the threshold for life in prison based on quantile percentage
               p_lia <- params['p_life_if_arrest']
               lifeAfter <- quantileApproximator( quant_p = 1-p_lia, distr = rnorm, mean = mu, sd = sqrt(sig2) )
               function( n ){
                    # sample Bernoulli to decide if injury occurs
                    arrest <- rbinom( n = n, size = 1, prob = p_arrest )
                    # for those arrested, calculate arrest duration
                    n_arrest <- length(arrest[arrest == 1])
                    if( n_arrest > 0 ){
                         # use normal distribution to select arrest duration
                         arrest_durs <- rnorm( n = n_arrest, mean = mu, sd = sqrt(sig2) )
                         # is it a life sentence?
                         arrest_durs <- sapply( X = arrest_durs, FUN = function(x) ifelse( x >= lifeAfter, Inf, max( minArrestDuration, round(x) ) ) )
                         arrest[ arrest == 1 ] <- arrest_durs
                    }
                    arrest
               }
          },
          WEIGHT = 1
     )
)


##### HERE IS AN EXAMPLE OF HOW WE MIGHT RANDOMLY CHOOSE THE DISTRIBUTION FROM THE ABOVE LIST
Arrest_Selector <- function( ){     
     
     # Select which distribution family (based on weights)
     families <- names( ARREST_AUX_FUN_SPACE )
     x <- numeric()
     for( nm in families ){
          x <- c( x, ARREST_AUX_FUN_SPACE[[nm]]$WEIGHT )
     }
     names(x) <- families
     pdf <- x / sum( x )
     choice <- sample( families, size = 1, prob = pdf )
     fam <- ARREST_AUX_FUN_SPACE[[ choice ]]
     
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
############################################################
#' Static: ARREST DURATION CALCULATOR
#' 
#' This function updates a hostile agent data frame with individual violence 
#' factors (propensities) sampled from the a selected random distribution.
#' 
#' @param host_agents a data frame of hostile agents that encounter authority
#' @param distr the distribution from where the arrest durations are drawn, input is number of values to return
#' @return Returns the host_agents data frame with out_of_commission feature updated
#' @export
ARREST <- function( host_agents, distr, ... ){
     n_agents <- nrow( host_agents )
     if( n_agents > 0 ) host_agents$out_of_commission <- host_agents$out_of_commission + distr( n_agents )
     host_agents
}