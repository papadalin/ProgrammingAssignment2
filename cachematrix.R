## A pair of functions that cache the inverse of a matrix.
## This function creates a specia "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x<<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function computes the inverse of the specia "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated(and the
## matrix has not changed),then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        m
}
