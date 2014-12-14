## A matrix inversion library with result caching
##
## Description
##   Because matrix inversion is a costly computation therefore in certain situations there
##   may be some benefit to caching the result to prevent needless re-computation of the 
##   inverse on the same result
##
## Usage
##   Creating a cache object:
##     cacheMatrixObject <- makeCacheMatrix(userMatrix)
##   Computing or retrieving the inverse
##     matrixInverse <- cacheSolve(cacheMatrixObject)
##
## Submitted in partial fulfilment of the Coursera R Programming course (Dec. 2014) for hsub

# makeCacheMatrix(x) - Creates matrix caching object used by cacheSolve()
# Parameters:
#     x - A square invertible matrix
#         NOTE: Per the specification, the matrix is assumed to be square
#               and invertible, therefore no validation is performed
# Returns: A matrix cache object
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    cachedInverse <<- NULL
  }
  
  getMatrix <- function() { 
    x 
  }
  
  setInverse <- function(newInverse) {
    cachedInverse <<- newInverse 
  }
  
  getInverse <- function() {
    cachedInverse
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve(x[, ...]) - solve() wrapper for matrix inversion with result caching 
# Parameters:
#   x   - A matrix cache object returned by the makeCacheMatrix() function
#   ... - [OPTIONAL] additional optional solve() parameters (see base:solve() documentation)
# Returns: The inverse of the matrix in x 
cacheSolve <- function(x, ...) {
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  
  targetMatrix <- x$getMatrix()
  targetMatrixInverse <- solve(targetMatrix, ...)
  x$setInverse(targetMatrixInverse)
  
  targetMatrixInverse
}