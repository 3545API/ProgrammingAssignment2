## Caching the Inverse of a Matrix

## makeCacheMatrix creats a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(Solve) m <<- Solve
        getSolve <- function() m
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix or gets the inverse if the calculation is done

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
