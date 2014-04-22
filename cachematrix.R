## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of functions caches the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
        inverse <- NULL

        # sets the content of the matrix, which clears the cached inverse
        set <- function(new_matrix) {
                mat <<- new_matrix
                inverse <<- NULL
        }

        #returns the matrix
        get <- function() mat

        #sets the inverse of the matrix in the cache
        setInverse <- function(inv) inverse <<- inv

        #returns the cached inverse of the matrix
        getInverse <- function() inverse

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
					## if there is a cached inverse, returns it directly
					message("getting cached data")
					return(i)
        } else {
					## if the inverse is not yet cached, calculates it, caches it, and returns it
					data <- x$get()
					i <- solve(data, ...)
					x$setInverse(i)
					return(i)
  			}
}
