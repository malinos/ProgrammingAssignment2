## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 
## This is the ProgrammingAssignment2 with the pair of function: 
## 1.makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix 

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #If the <<- were <- then we would be creating
        #a local variable m and not updating the m that is
        #in the parent environment, and it would subsequently
        #never be cached
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x  ##returns X
        setsolve <- function(solve) m <<- mean
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## 2.cacheSolve: computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
