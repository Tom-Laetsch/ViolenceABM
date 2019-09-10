"
When two enemy hostile agents interact, there is a possibility of 
a (directional) hostility between the two. The probability that 
agent A enacts a hostility against agent B depends on the violence
factor of A and on the tension between the gangs of 
A and B. 

If VF is the violence factor of A and T is the tension A feels 
on interaction with an enemy B, then the probability that 
A will enact a hostility on B will be: 
p = 1 - exp(- wt*( bal*VF + (1-bal)*T ) )
where wt (hostility weight) and bal (hostility balance) are chosen 
randomly here below. 

We have that VF and T are distributions with mean ~1;
the larger choice of wt will increase the probability
of a hostility as p -> 1 with hwt -> Inf; for bal ~ 1/2, 
the VF and T will roughly carry the same weight in the
decision, whereas bal -> 1 implies VF makes the decision, 
and bal -> 0 implies T makes the decision. 
"

############################## HOSTILITY TUNING PARAMETERS ###############################

# choices
IMPART_HOSTILITY_WEIGHT_CHOICES <- seq( 0.001, # ~ 1 in 1000 results in hostility
                                        0.1, # ~ 1 in 10 results in hostility
                                        0.003 )
# selector
ImpartHostilityWeight_Selector <- function(){
     sample( x = IMPART_HOSTILITY_WEIGHT_CHOICES, size = 1 )
}

# choices
IMPART_HOSTILITY_BALANCE_CHOICES <- seq( 0.1, 0.9, 0.2 )
# selector
ImpartHostilityBalance_Selector <- function(){
     sample( x = IMPART_HOSTILITY_BALANCE_CHOICES, size = 1 )
}


############################# OVERALL PARAMETER SELECTION ####################################

ChooseHostilitySettings <- function(){
     IMPART_HOSTILITY_SETTINGS <- list()
     # choose the hostility weight
     IMPART_HOSTILITY_SETTINGS$WEIGHT = ImpartHostilityWeight_Selector()
     # choose the balance between VF and T
     IMPART_HOSTILITY_SETTINGS$BALANCE = ImpartHostilityBalance_Selector()
     # return
     IMPART_HOSTILITY_SETTINGS
}


############################## IMPART HOSTILITY SIMULATION FUNCTIONALITY ###############################

SETUP_IMPART_HOSTILITY <- function( host_stngs ){
     wt <- host_stngs$WEIGHT
     bal <- host_stngs$BALANCE
     
     function( perp_agent, vict_agent, tension_matrix, violence_fac_calculator, tension_calculator ){
          
          # check that the perp and vict are enemies
          # enemies have different designations
          if( identical(perp_agent$designation, vict_agent$designation) ){
               # agents of the same designation aren't enemies
               return( 0 )
          }
          
          # calculate the effective violence factor
          perp_vf <- perp_agent$violence_fac
          vict_vf <- vict_agent$violence_fac
          vf <- violence_fac_calculator( perp_vf = perp_vf, vict_vf = vict_vf )
          
          # calculate the effective tension
          from_nbhd <- perp_agent$base_nbhd
          towards_nbhd <- vict_agent$base_nbhd
          tens <- tension_calculator( from_nbhd = from_nbhd, towards_nbhd = towards_nbhd, tension_matrix = tension_matrix )
          
          # decide whether or not a hostility is given from perp towards vict
          p <- 1.0 - exp( - wt*( bal*vf + (1-bal)*tens ) )
          
          sample( c(T,F), size = 1, prob = c(p, 1-p) )
     }
}