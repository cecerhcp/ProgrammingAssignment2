## These two functions together make it possible for the user to use
## a matrix with built-in cache for compute its inverse.
## One creates this special kind of matrix and the other does the actual
## computation.

##  This function returns a special "matrix" that is actually a list that
##  contains the matrix itself and functions to be able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      k <- NULL
      set <- function(y) 
      {
            x <<- y
            k <<- NULL
      }
      get <- function() x
      setinv <- function(inv) k <<- inv
      getinv <- function() k
      list(set = set, get = get,
      setinv = setinv,
      getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above, returning the already there cached value or actually
## computing its inverse with the solve function.

cacheSolve <- function(x, ...) {
      k <- x$getinv()
      if(!is.null(k)) {
            message("getting cached data")
            return(k)
      }
      data <- x$get()
      k <- solve(data)
      x$setinv(k)
      k
}
