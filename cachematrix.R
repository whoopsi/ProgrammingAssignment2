## The first function, makeCacheMatrix creates a "matrix" that stores the cache value
##get the value of the matrix
##set the value of the matrix
##get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
       inverse<- NULL
       set <- function(y) {
         x <<- y
         inverse <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) inverse <<- solve
       getinverse <- function() inverse
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## if the mean is calculated before cachesolve gets the data from makecachematrix, 
##else it calculates the inverse of the matrix

cacheSolve <- function(x= matrix(), ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
   
}
