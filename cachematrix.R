## This script contains 2 functions that are aimed at computing the inverse of a matrix. We create a cache to store the inverse of a matrix once computed, so it can be used later when the inverse for the same matrix is requested again. 


## Function 1 - makeCacheMatrix creates a special matrix which is a list of functions to set the matrix, get the matrix, set the inverse of the matrix and get the inverse of the matrix from the cache. 

makeCacheMatrix <- function(x = matrix()) {
                m = NULL
                set <- function(y) {
                     x <<- y
                     m <<- NULL
                }
                get <- function() x
                setinv <- function(solve) m <<- solve
                getinv <- function() m
                list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Function 2 - cacheSolve checks the cache to see if the inverse of a matrix has already been computed, and if yes, returns the inverse from the cache. If the inverse is not available in the cache, this function computes the inverse and stores it in the cache for later use. 

cacheSolve <- function(x, ...) {
           ## Return a matrix that is the inverse of 'x'
           m <- x$getinv()
           if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
           }
           data <- x$get()
           m <- solve(data, ...)
           x$setinv(m)
           m
}
