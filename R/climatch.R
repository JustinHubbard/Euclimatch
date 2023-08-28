
#' Percent Euclidean climatch score
#'
#' A summarized climatch score within the recipient region to a source region. Provides the percentage of climate data points in the recipient region equal to or above a specified score (default is 6), or the mean climatch score across the whole recipient region. Note no floor function is used as in Crombie et al. (2008)
#'
#' @param recipient A data.frame of climatic variables for the recipient region
#' @param source A data.frame of climatic variables for the source region
#' @param globvar A vector of the global variance of each climate variable
#' @param type "perc" specifies a percent climatch score above or equal to a given value and "mean" calculates the mean climatch score within the recipient region
#' @param score The score to use in calculating the percentage match (default is 6)
#'
#' @return A numeric value of the percentage of climatch scores within recipient >= a specified value, or the mean climatch score
#'
#' @references Crombie, J., Brown, L., Lizzio, J., & Hood, G. (2008). Climatch user manual. Australian Government, Bureau of Rural Sciences.
#'
#' @examples
#' i <- as.data.frame(matrix(runif(n=180, min=1, max=20), nrow=60)) # Fake source climate data
#' j <- as.data.frame(matrix(runif(n=300, min=10, max=40), nrow=100)) # Fake recipient data
#' variance <- c(600, 800, 450) # Fake global variance
#'
#' climatch(recipient = j, source = i, globvar = variance)
#' @export

climatch <- function(recipient, source, globvar, type = "perc", score = 6){

  # Run the climatch algorithm for the vector of climatch scores
  match.vec <- climatch_vec(recipient = recipient, source = source, globvar = globvar)

  # Summarize the climatch vector as a percent match above the 'score' threshold
  if(type == "perc"){
    score <- (sum(match.vec >= score) / nrow(recipient)) * 100
  }

  # Summarize the climatch vector as the mean score
  if(type == "mean"){
    score <- mean(match.vec)
  }

  return(score)
}
