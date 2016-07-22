## Paco´s code solution

## this function is actually a list with elements to be called by the next function in order
##to calculate the inverse matrix if not cached already

makeCacheMatrix <- function(x = matrix()) {
  
      m <- NULL
      
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(inverse) m <<- inverse
      
      getinverse <- function() m
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## Write a short comment describing this function

##this function is the actual procces that calls the previous list. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  
  
}
