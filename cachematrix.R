## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

# function parameter is a matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize cached to null
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## getting a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ################################################
  ## If the inverse wasn't yet been calculated ##
  ################################################
  
  ## getting the matrix from our object
  data <- x$get()
  
  ## calculating the inverse by using matrix multiplication
  m <- solve(data) %*% data
  
  ## storing the inverse to the object to future usage
  x$setInverse(m)
  
  ## returning a matrix that is the inverse of 'x'
  m 
}
