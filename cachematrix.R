## makeCacheMatrix and cacheSolve, helper functions to compute and cache the
## inverse of a matrix

## How to use them:
## > source("cachematrix.R")
## > m = matrix(c(1, -1/4, -1/4, 1), 2, 2)
## > cachedm = makeCacheMatrix(m)
## > invm = cacheSolve(cachedm)
## > invm = cacheSolve(cachedm)
##  getting cached data
## > m %*% invm
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > dim(invm) == dim(m)
## [1] TRUE TRUE

## makeCacheMatrix, function (x = matrix())
## returns a special "matrix" object that can cache its inverse.
## The following functions are added as parts of the matrix x:
## set : set the value of the matrix
## get : get the value of the matrix
## setinverse : set the value of the inverse of the matrix x
## getinverse  : get the value of the inverse of the matrix x


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
             
}


## cacheSolve, function (x, ...)
## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above at first call,
## returns the cached inverse of the matrix at next calls.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}

