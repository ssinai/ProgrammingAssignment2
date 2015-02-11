## Create a special "matrix" object that allows a user to store and retrieve
## a matrix and its inverse. If the inverse doesn't yet exist, create it and
## cache it so that it doesn't need to be recreated the next time it's
## retrieved.

## create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
    inverse_m <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        m <<- y
        inverse_m <<- NULL
    }
    
    # get the value of the matrix
    get <- function() m
    
    # set the value of the inverse matrix
    set_inverse <- function(inverse) inverse_m <<- inverse 
    
    # get the value of the inverse matrix
    get_inverse <- function() inverse_m
    
    # create and return a list containing the four
    # previously assigned methods.
    list(set = set, get = get, 
         set_inverse = set_inverse, 
         get_inverse = get_inverse)   
}


## compute the inverse of the special "matrix" retured by
## makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the
## cachesolve should retieve the inverse from the cache.

cacheSolve <- function(data, ...) {
     
    inverse_m <- data$get_inverse()
    
    if (!is.null(inverse_m)){
        message("getting cached inverse matrix")
        return(inverse_m)
    }   
  
    m <- data$get() 
    
    inverse_m <- solve(m, ...)
    
    data$set_inverse(inverse_m)   
    
    inverse_m   
}
