## Programming Assignment 2


makeCacheMatrix <- function(x = matrix())  {
  # initialize to NULL
 matrixcache <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    matrixcache <<- NULL
  }
  
  get <- function() x
  setMatrix <- function(inverse) matrixcache <<- inverse
  getInverse <- function() matrixcache
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixcache <- x$getInverse()
  if (!is.null(matrixcache)) {
    message("getting cached data")
    return(matrixcache)
  }
  mat <- x$get()
  matrixcache<- solve(matrixcache, ...)
  x$setInverse(matrixcache)
  matrixcache
}
