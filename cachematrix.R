## Put comments here that give an overall description of what your
## functions do

## This function inizialize a Cache Matrix, with set function that store matrix and clear inverse, get method return the matrix
## setsolve and getsolve store and return the inverse of x 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    ##set and clear inverse of x on change
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function check if the cache matrix have already colculate inverse, if not then calculate and set by setsolve metodh.
## And return the cached or calculated result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  ##if cached return value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
