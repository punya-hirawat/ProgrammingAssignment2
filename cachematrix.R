## The two functions initialise the matrix set in the function as a global
## variable and allow it to be dealt with in multiple ways and find its inverse
## and store that as a global variable, respectively

## this function can be used to intialise a matrix and cache it, and also
## contains functions to be able to change the contents of the matrix, retrieve
## the matrix, retrieve the inverted matrix and also set the matrix as an 
## inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## this function solves the cached matrix, i.e, calculates its inverse and sets
## that to the global variable inv

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  temp <- x$get()
  inv <- solve(temp, ...)
  x$setinverse(inv)
  inv
}
