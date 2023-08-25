#' Run climatch in parallel
#'
#' @param recipient List of dataframes of the recipient regions
#' @param source List of dataframes of the source regions
#' @param biovar Vector of the columns (climate variables) to use, default all columns
#' @param globvar Vector of the global variance of each variable
#' @param ncores The number of cores to use in parallel
#' @param type Choose between 'climatch_perc' with 'perc' and 'climatch_vec' withe 'vec'. Default is 'perc'.
#' @return 'perc' returns dataframe of climatch within recipients (rows) to each source represented in columns. 'vec' returns dataframe of climatch of a recipient (each column corresponds to grid cell), to sources (corresponding to rows).
#'
#' @importFrom foreach %:% %dopar%
#' @import doParallel
#' @import RcppParallel
#'
#' @examples
#' # example code
#' i1 <- as.data.frame(matrix(runif(n=180, min=1, max=20), nrow=60)) # Fake source climate data.
#' i2 <- as.data.frame(matrix(runif(n=180, min=20, max=40), nrow=60))
#' i <- list(i1, i2) # list the source dataframes
#' j1 <- as.data.frame(matrix(runif(n=300, min=10, max=40), nrow=100)) # Fake recipient climate data.
#' j2 <- as.data.frame(matrix(runif(n=300, min=20, max=50), nrow=100))
#' j <- list(j1, j2) # list the recipient dataframes
#' variance <- c(60, 800, 450) # Fake global variance
#'
#' climatch_par(recipient = j, source = i, biovar = 1:3, globvar = variance, ncores = 2, type = 'perc')
#' climatch_par(recipient = j1, source = i, biovar = 1:3, globvar = variance, ncores = 2, type = 'vec')
#'
#' @export
climatch_par <- function(recipient, source, globvar, biovar = 1:length(globvar), ncores, type = 'perc') {

  recipient.n <- length(recipient)
  source.n <- length(source)
  climatch.pairwise <- data.frame(nrow = recipient.n, ncol = source.n)

  i <- NULL
  j <- NULL

  # Set up parallel processing
  nc <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(nc)

  if(type == 'perc'){
    # Parallel foreach loop - each row will represent recipient regions and columns for sources
    climatch.pairwise <- foreach::foreach(i = 1:source.n, .combine = "cbind", .inorder = TRUE, .packages = c("Euclimatch")) %:%
     foreach::foreach(j = 1:recipient.n, .combine = "c", .inorder = TRUE, .packages = c("Euclimatch")) %dopar% {
        Euclimatch::climatch_perc(recipient = recipient[[j]][, biovar, drop = FALSE], source = source[[i]][, biovar, drop = FALSE], globvar = globvar)
      }
  }

  if(type == 'vec'){
    recipient.n <- nrow(recipient)
    source.n <- length(source)
    climatch.pairwise <- data.frame(nrow = source.n, ncol = source.n)

    climatch.pairwise <- foreach::foreach(i = 1:source.n, .combine = "rbind", .inorder = TRUE, .packages = c("Euclimatch")) %dopar% {
      Euclimatch::climatch_vec(recipient = recipient[, biovar, drop = FALSE], source = source[[i]][, biovar, drop = FALSE], globvar = globvar)
    }
  }

  parallel::stopCluster(nc)  # Stop cluster
  return(climatch.pairwise)
}

