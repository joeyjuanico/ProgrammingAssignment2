## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

# function parameter is a matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize cached to null
  cache <- NULL
  
  # store a matrix referred to as x
  setMatrix <- function(newValue) {
    x <<- newValue
    # new value is stored so empty the cache
    cache <<- NULL
  }
  
  # return the stored value referred to as x
  getMatrix <- function() {
    x
  }
  
  # save the given argument to cache
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. each of these are functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## getting a matrix that is the inverse of x
  inv <- x$getInverse()
  
  ## returns if the inverse has already been calculated; inv is not null
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse has not yet been calculated
  ## get the matrix from our object
  data <- x$get()
  
  ## calculate the inverse by using matrix multiplication
  m <- solve(data) %*% data
  
  ## store the inverse to the object to future usage
  x$setInverse(m)
  
  ## return a matrix that is the inverse of x
  m 
}
