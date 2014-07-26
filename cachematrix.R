##  Because finding the inverses of matrices may take a long time, it is
##      useful to store solutions in advance so that they may be returned
##      efficiently.  makeCacheMatrix calculates and stores inverses of 
##      matrices, and cacheSolve either retrieves the finished calculation,
##      or (if inverse not found in the cache) calculates it.


##  makeCacheMatrix runs the solve function on a matrix to calculate its
##  inverse.  It uses s <<- solve so that the variable s will be 
##  pre-defined for use by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
          s <- NULL
          set <- function(y) {
              x <<- y
              s <<- NULL
          }
          get <- function () x
          setinverse <- function(solve) s<<- solve
          getinverse <- function() s
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}
          

##  The first part of cacheSolve looks for the inverse of the matrix in the
##  cache established by makeCacheMatrix. If it is there, cacheSolve returns
##  the message "getting cached data" and the inverse matrix.  If it is not
##  there, cacheSolve runs the solve function and calculates and returns
##  the inverse.

cacheSolve <- function(x, ...) {
          s <- x$getinverse()
          if(!is.null(s)) {
                message("getting cached data")
                return (s)
          }
          data <- x$get()
          s <- solve(data, ...)
          x$setinverse(s)
          s
}
