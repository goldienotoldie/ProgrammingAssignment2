## This R script has two functions that enable caching of
## the inverse of a given matrix.


## This function takes a matrix argument and
## constructs an R object that can store and
## return its inverse.
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  # sets the matrix inverse
  setinverse <- function(matinv) minv <<- matinv
  # returns the matrix inverse
  getinverse <- function() minv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a cached matrix object returned
## by calling makeCacheMatrix on the matrix and returns
## the inverse if found in cache otherwise calculates the
## inverse and caches it before returning the same.
cacheSolve <- function(x, ...) {
  # check if the matrix inverse is cached
  minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  # if inverse is not cached calculate it
  matinv <- solve(data)
  # save the inverse value in cache
  x$setinverse(matinv)
  matinv
}
