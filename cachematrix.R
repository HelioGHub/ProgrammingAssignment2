## Matrix inversion is usually a costly computation, therefore
## this function will cache the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get = function() x
	setinversion <- function(solve) m <<- solve
	getinversion <- function() m
	list(set = set, get = get,
		setinversion = setinversion,
		getinversion - getinversion)
}


## The following function calculates the inversion of a matrix 
## created with the above function. If the inversion of the 
## matrix is already in cache, it will use the cache value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinversion()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversion(m)
        m

}
