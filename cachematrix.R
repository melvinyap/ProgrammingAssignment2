## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### Function creates a cached matrix with 
### relevant methods to store the values of
### both the matrix and its inverse matrix
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
### Function determines whether the inverse of the matrix
### has been calculated before and returns the value of
### the cached inverse matrix. Otherwise, it will proceed
### to derive the inverse matrix using the solve() function.
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
