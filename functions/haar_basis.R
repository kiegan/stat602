haar_basis <- function(x,m)
{
  ## define the endpoints of the interval on which we desire the basis
  a <- min(x)
  b <- max(x)
  
  ## transform the x-values to be on (0,1)
  xtrans <- (x - a)/(b - a)
  
  ## create father function
  father <- function(x)
  {
    return(1 * ( (0 <= x) & (x <= 1) ) )
  }
  
  ## create mother function
  mother <- function(x)
  {
    return(father(2*x) - father(2*x - 1))
  }
  
  ## psi is a list where each element is a matrix corresponding to the basis functions corresponding to a specific m
  ## X is the design matrix
  psi <- list()
  X <- cbind(father(xtrans), mother(xtrans))
  for(i in 1:m)
  {
    psi[[i]] <- matrix(nrow = length(x), ncol = length(0:(2^i - 1)))
    for(j in 0:(2^i - 1))
    {
      psi[[i]][,j + 1] <- sqrt(2^i) * mother(2^i * (xtrans - j/(2^i)))
    }
    X <- cbind(X, psi[[i]])
  }
  return(X)
}
X <- haar_basis(x = d3.4$x, m = 3)
head(X)
