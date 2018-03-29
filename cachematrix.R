## The functions below due to the following:
##		1st Function - Create a special "matrix" object that can
##					   cache its inverse.
##		2nd Function - Computes the inverse of the special "matrix" 
##					   returned by makeCacheMatrix above or retrieve 
##					   the inverse from the cache. 

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSpecial <- function(inverse) m <<- inverse
        getSpecial <- function() m
        list(set = set, get = get,
             setSpecial = setSpecial,
             getSpecial = getSpecial)
}

##  This function computes the inverse of the special "matrix" 
##	created by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSpecial()
        if (!is.null(m)) {
        		message("getting cached data")
        		return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSpecial(m)
        m
}
