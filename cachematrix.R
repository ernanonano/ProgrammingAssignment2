###############################################################################
## Programming Assignment 2 for R Programming on Coursera
## 
## There are two functions:
##
## * makeCacheMatrix: Creates a special "matrix" object that can cache its 
##                    inverse.
##
## * cacheSolve: This function computes the inverse of the special "matrix" 
##               returned by makeCacheMatrix above. If the inverse has already
##               been calculated (and the matrix has not changed), then 
##               cacheSolve should retrieve the inverse from the cache.
##
###############################################################################


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse_ <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
#     {
#         return x
#     }
#     
    setinverse <- function (inverse) {
        inverse_ <<- inverse
    }
    
    getinverse <- function() inverse_
#     {
#         return inverse_
#     }

    list(set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix or gets its cached value if possible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse        
}
