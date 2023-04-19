
# The first function "makeCacheMatrix" produces a custom matrix with a list of functions for setting the value of the matrix, 
# getting the value of the matrix, setting the value of the inverse, and also getting the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The special matrix provided by the above makeCacheMatrix is inversed by the below code.
#If the matrix has not changed and the inverse has already been calculated, cacheSolve should get the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
