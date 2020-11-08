## the two function can cache the inverse of a matrix 



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


## function that calculates the inverse matrix of the above function however
## first  check if the inverse already exists, if it does
## it does not calculate the inverse again, just prints it
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
