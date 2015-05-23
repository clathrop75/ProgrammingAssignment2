## Put comments here that give an overall description of what your
## functions do

## This function creates a list and provides a way to set and get the
## value of a matrix as well as get and set the value of the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  
  ## Set a new matrix
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  
  ## Get the stored matrix
  get <- function() x
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) matrixInverse <<- inverse
  
  ## Get the stored inverse of the matrix
  getInverse <- function() matrixInverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse of a matrix.
## If the inverse has already been calculated then the prior
## cached value is returned. Otherwise, the inverse is
## calculated, cached for future use and then returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Get the stored value of the matrix from the makeCacheMatrix list
  matrixInverse <- x$getInverse()
  
  ## If the stored value of the matrix is not null
  ## then return the cached matrix
  if(!is.null(matrixInverse)) {
    message("retrieving cached data")
    return(matrixInverse)
  }
  ## If the stored value of the matrix is null
  ## then calculate the inverse and store it
  else {
    message("calculating inverse")
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
  }
  
}
