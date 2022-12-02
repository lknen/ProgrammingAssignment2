## Below are two functions that are used to create 
## a special object that stores a matrix and cache's its inverse

## makeCacheMatrix creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmat <- function(invmat) m <<- invmat
        getinvmat <- function() m
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)        
}


## cacheSolve computes the inverse of the special "matrix". 
## If the inverse has already been calculated, then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmat(m)
        m
}