## The below code calculates the inverse of matrix and stores it in cache so it doesnt have to calulate the inverse everytime
##The next time if pass the same matrix the result is cached instead of calculating inverse again 

## The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     # defines variable m
     set <- function(y) {
          x <<- y        ## Assigns the input matrix to variable x
          m <<- NULL     ## Assigns NULL to the M to Null in parent environment
     }
     
     
     get <- function() x   ## returns matrix x
     setinverse <- function(inverse) m <<- inverse  ## It sets m as the inverse of matrix in the cache
     getinverse <- function() m                     ## It gets the cached inverse of a matrix
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     ##  it Calls the getinverse function to check if the inverse of the given matrix is already present in Cache
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     ## If the inverse is present it print the result from cache
     ## Otherwise it calculates  the inverse using solve function
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
