#function provides the ability to get and set values related to a cached matrix
#returns a list object containing set, get, setInverse, and getInverse functions
makeCacheMatrix <- function(x) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#function returns the inverse of a matrix. It is assumed that the passed in matrix is invertible.
#if it exists, it pulls the inverse from a cached variable instead of re-computing the inverse.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}