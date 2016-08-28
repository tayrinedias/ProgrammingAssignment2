## My function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix cache potentially time-consuming computations.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvertedmatrix <- function(invertedmatrix) m <<- solve(x)
  getinvertedmatrix <- function() m
  list(set = set, get = get,
       setinvertedmatrix = setinvertedmatrix,
       getinvertedmatrix = getinvertedmatrix)
}


## cacheSolve looks up the result of the calculation of an inverted matrix in the cache, to save the time that would be spent calculating it every time.

cacheSolve <- function(x, ...) {
    m <- x$getinvertedmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvertedmatrix(m)
    m
}
