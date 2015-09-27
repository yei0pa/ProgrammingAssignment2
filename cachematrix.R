# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a matrix that will
# 1 - set the value of the matrix
# 2 - get the value of the matrix
# 3 - set the value of inverse of the matrix
# 4 - get the value of inverse of the matrix

# The matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                       # initalize
  set <- function(y) {              # set the value of the matrix
    x <<- y                       
    inv <<- NULL
  }
  get <- function() x               # get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse   # set the value of inv of the matrix
  getinverse <- function() inv      # get the value of inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")  # msg if inversed computer before
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

