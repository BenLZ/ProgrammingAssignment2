## These functions are used to compute the inverse of a matrix, then cache it. If the same matrix is input, the function will retrieve that
## inverted matrix from the cache.

## makeCacheMatrix is used to create an object that will serve as the cache for the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(inverse) matrixInverse <<- inverse
  getInverseMatrix <- function() matrixInverse
  list(set = set, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## This function determines if the inverse has already been computed, then either returns that inverse or computes the inverse initially and returns it.

cacheSolve <- function(x, ...) {
  InverseMatrix <- x$getInverseMatrix()
  if(!is.null(matrixInverse)) {
    message("getting cached matrix")
    return(matrixInverse)
  }
  data <- x$getMatrix()
  matrixInverse <- solve(data)
  x$setInverseMatrix(matrixInverse)
  matrixInverse
}
