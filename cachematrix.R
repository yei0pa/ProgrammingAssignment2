

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

