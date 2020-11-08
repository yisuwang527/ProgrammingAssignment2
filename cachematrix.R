## the two functions can create a matrix, and calculate its inverse,
## as well as cache the inverse of the matrix 


## This function creates a matrix and it can calculate the inverse of the input
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function calculates the inverse matrix of the above function however
## first  check if the inverse has already be set, if it has
## it gets the inverse from the cache and skips the computation.
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
   
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
   
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv 
}
