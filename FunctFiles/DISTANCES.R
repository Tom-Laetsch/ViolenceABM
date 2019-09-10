METRIC_AUX_FUN_SPACE <- list(
     TaxiNodeNodeDist = list(
          METRIC = function( node1, node2 ) return( abs(node1[1]-node2[1]) + abs(node1[2] - node2[2]) ),
          WEIGHT = 1
     ),
     EucNodeNodeDist = list(
          METRIC = function( node1, node2 ) return( sqrt( (node1[1]-node2[1])^2 + (node1[2] - node2[2])^2 ) ),
          WEIGHT = 1
     ),
     LinfNodeNodeDist = list(
          METRIC = function( node1, node2 ) return( max( abs(node1[1]-node2[1]), abs(node1[2] - node2[2]) ) ),
          WEIGHT = 0
     )
)


Metric_Selector <- function(){
     
     # Select the metric
     metric_names <- names( METRIC_AUX_FUN_SPACE )
     x <- numeric()
     for( nm in metric_names ){
          x <- c( x, METRIC_AUX_FUN_SPACE[[nm]]$WEIGHT )
     }
     names(x) <- metric_names
     pdf <- x / sum( x )
     choice <- sample( metric_names, size = 1, prob = pdf )
     
     # Create the return list
     ChosenOne <- list(
          NAME = choice,
          CHOSEN_PARAMETERS = NULL,
          FUN  = METRIC_AUX_FUN_SPACE[[choice]]$METRIC 
     )
     
     ChosenOne
}


# distance between a node and a nbhd
NodeNbhdDist <- function( node, nbhd_coords ){
     return( min( apply( X=nbhd_coords, FUN=function(x){ METRIC( x, node ) }, MARGIN=1 ) ) )
}

# distance between two nbhds
NbhdNbhdDist <- function( nbhd_coords1, nbhd_coords2 ){
     return( min( apply( nbhd_coords1, MARGIN=1, FUN=function(x){ NodeNbhdDist( x, nbhd_coords2 ) } ) ) )
}

#'
#' @param dist the distance scalar that is to be weighted
#' @param coef a strictly positive scalar by which to multiply distance
#' @param min_val a positive scalar that acts as an extra safety measure for
#'   when the user wishes to divide by zero
#' @return A weighted distance scalar
#' @export
dist3 <- function( dist, coef, min_val = 0 ){
     return( coef * max( dist, min_val )^3 )
}
############################################################
#' Progressive distance-based weighting function
#'
#' This distance-based weighting function produces a scalar which becomes
#' progressively larger as an agent moves farther away from their base.
#' Distances are weighted differently depending on if they belong in the
#' category 'close', 'mid' or 'far'.
#'
#' @param dist the distance scalar that is to be weighted
#' @param close_coef strictly postive scalar that acts as a weighting factor for
#'   'close' distances. Also partially determines coefficients for 'mid' and
#'   'far' distances
#' @param close_cutoff the upper bound on 'close' distances, a positive scalar
#' @param mid_cutoff the upper bound on 'mid' distances, a positive scalar
#' @param  min_val a positive scalar that acts as an extra safety measure for
#'   when the user wishes to divide by zero
#' @return A weighted distance scalar
#' @export
distGraded <- function( dist, 
                        close_coef,
                        close_cutoff, 
                        mid_cutoff, 
                        min_val = 1 ){
     # coef chosen to create smooth cutoff boundaries
     mid_coef <- close_coef / close_cutoff
     far_coef <- mid_coef / mid_cutoff
     if( dist <= close_cutoff ){
          return( close_coef * dist )
     } else if( close_cutoff< dist & dist <= mid_cutoff ) {
          return( mid_coef * dist^2 )
     }
     return( far_coef * dist^3 )
}

