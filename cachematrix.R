## This script creates two objects for use in caching the inverse of
## a matrix
## makeCacheMatrix - object that stores a matrix and its inverse
## cacheSolve - using a makeCacheMatrix object, returns the inverse
##              of a matrix.

## makeCacheMatrix
## Object that stores a passed matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## inv stores the inverse of the input matrix
  inv <- NULL
  
  ## set method - constructer
  set <- function(y) {
            x <<- y
            inv <<- NULL
  }
  
  ## get method - retrieves current matrix
  get <- function() x
  
  ## setinverse method - stores the matrix inverse
  setinverse <- function(solve) inv <<- solve
  
  ## getinverse method - returns the stored matrix inverse
  getinverse <- function() inv
  
  ## list method - lists the methods associated with the object
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## Given a makeCacheMatrix object, either returns the cached inverse
## or creates the inverse, caches it, and returns it
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()      ## get the currently cached inverse
  if(!is.null(inv)) {        ## if inverse exists, return it
    message("getting cached data")
    return(inv)
  }
  data <- x$get()           ## inverse didn't exit - get the matrix
  inv <- solve(data, ...)   ## invert the matrix
  x$setinverse(inv)         ## cache the inverse
  inv                       ## return the inverse
}
