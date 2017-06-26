makeCacheMatrix <- function(x = matrix()) {
	# Creates a matrix object that can cache its inverse.
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        # Returns a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
