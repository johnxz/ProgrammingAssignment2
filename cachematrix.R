## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## 'makeCacheMatrix', which contains a list of nested functions, will take a matrix object as input, 
## and create a special "matrix" object which will store the input matrix, and cache its inverse by 
## assign a value computed outside of this function

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        get <- function() x
        setinverse <- function(inverse) inv_x <<- inverse
        getinverse <- function() inv_x
        list(get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## `cacheSolve` computes the inverse with the solve function in R for the invertible matrix returned 
## by `makeCacheMatrix`, if it can't retrieve it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinverse(inv_x)
        inv_x
}
