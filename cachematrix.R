## Introduces special matrice that cache its inverse

## makeCacheMatrix creates a list with 4 functions: set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## returns a cached inverse of a matrix if precomputed, computes otherwise

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (is.null(inv)) {
            inv <- solve(x$get(), ...)
            x$setinv(inv)
        } else {
            message("getting cached data")
        }
        inv
}

