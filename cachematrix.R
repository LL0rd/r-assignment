## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## cache initialisation
    m <- NULL
    
    ## set the value of the matrix
    set <- function(y) { 
        x <<- y # set the actual data
        m <<- NULL # clear some old stored values
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## save the stored solved value
    setsolve <- function(solve) m <<- solve
    
    ## get the stored value
    getsolve <- function() m
    
    ## return a list with get/set functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## try to get the stored Value from Cache
    m <- x$getsolve()
    if(!is.null(m)) { ## if the Value was not found in Cache, m is NULL
        message("getting cached data")
        return(m) ## return the cached data, in case it was found, or else continue
    }
    data <- x$get() ## get the matrix data / values
    m <- solve(data, ...) ## actually solve the matrix
    x$setsolve(m) ## save inversed / solved matrix in cache
    m ## return the solved matrix
}
