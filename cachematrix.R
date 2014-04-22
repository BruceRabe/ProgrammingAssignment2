## These two functions wrap a matrix with a cached value of the matrix inverse, 
##   so that it only needs to be calculated once and then retrieved later

## makeCacheMatrix wraps a matrix x with functions to get and set the value, 
##  and get and set the inverse.
##  Example usage:
##     cm = makeCacheMatrix(matrix(c(4,7,2,6),2,2))
##     cm$get()
##     cacheSolve(cm)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # cached value of the inverse of the matrix
  
  ## set the matrix value
  set <- function(y) {
    x <<- y
    inverse <<- NULL  # reset the cached inverse
  }
  
  ## get the value
  get <- function() x
  
  ## set the inverse
  setinverse <- function(i)  inverse <<- i
  
  ## get the inverse
  getinverse <- function() inverse
  
  ## return a list of the functions for the cacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x', using the cache if possible
cacheSolve <- function(x, ...) {
   ## first check to see if the inverse has already been calculated
   i <- x$getinverse()
   if(!is.null(i)) {
      # already calculated - just return the cached value
      message("getting cached data")
      return(i)
   }
   
   # else it has not been calculated yet so we need to solve() for the inverse of the matrix value
   data <- x$get() # get the actual matrix value, not the wrapper list of functions
   i <- solve(data)

   # now set the inverse in the cache so we don't have to recalculate again
   x$setinverse(i)
   
   #return the inverse
   i
}
