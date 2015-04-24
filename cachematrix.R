## Jonathan Hsu - R Programming Assignment 2
## This programming assignment consists of 2 functions to cache the inverse of a matrix.

## Function 1
## This function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## Set the value of the matrix         
        cache_Matrix <- NULL
        set <- function(y) {
                x <<- y
                cache_Matrix <<- NULL 
        }
        
        ## Ge the value of the matrix
        get <- function() x
        
        ## Set the value of the inverse
        set_inv <- function(inverse) cache_Matrix <<- inverse
        
        ## Get the value of the inverse
        get_inv <- function() cache_Matrix
        list(set = set, get = get, 
             set_inv = set_inv, 
             get_inv = get_inv)
}

## Function 2
## This function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        cache_Matrix <- x$get_inv()
        if(!is.null(cache_Matrix)) {
                message ("getting cached data")
                return(cache_Matrix)
        }
        
        ## Calculate inverse of matrix and return result
        data <- x$get()
        cache_Matrix <- solve(data, ...)
        x$set_inv(cache_Matrix)
        cache_Matrix                
}



