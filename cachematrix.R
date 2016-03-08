## makeCacheMatrix creates a special "matrix" object that can cache its inverse with the 
## appropriate set and get function.

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) xinv <<- solve
  getsolve <- function() xinv
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  xinv <- x$getsolve()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setsolve(xinv)
  xinv
}