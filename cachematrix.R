## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here).

## Your assignment is to write a pair of functions that cache the
## inverse of a matrix.

## Write the following functions:

## 'makeCacheMatrix': This function creates a special "matrix" 
## object that can cache its inverse.

## Computing the inverse of a square matrix can be done with the 
## solve function in R. For example, if X is a square invertible 
## matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always
## invertible.


## The first function, 'makeCacheMatrix' creates a list of matrix
## functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m_inv <- NULL
        
        set <- function(y) {
                
                x <<- y
                m_inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inv) m_inv <<- inv
        getinv <- function() m_inv
        
        list(set = set, get = get, setinv = setinv, 
             getinv = getinv)
}


## 'cacheSolve': This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m_inv <- x$getinv()
        
        if(!is.null(m_inv)) {
                
                ## if there is data in 'm_inv', return the 
                ## inverse matrix from cached data
                message("Getting cached data.")
                return(m_inv)
        }
        
        ## If theres no cached data, do the following:
        data <- x$get()
        
        m_inv <- solve(data, ...)
        
        x$setinv(m_inv)
        
        ## Return a matrix that is the inverse of 'x'
        return(m_inv)
}