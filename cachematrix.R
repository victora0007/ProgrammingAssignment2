## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The following functions
## work together to create a square invertible matrix and make the inverse of the matrix
## available in the cache environment.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cmi <- NULL
        set <- function(y) {
                x <<- y
                cmi <<- NULL
        }
        get <- function() x
        setMatrixInv <- function(cminv) cmi <<- cminv
        getMatrixInv <- function() cmi
        list(set = set, get = get, setMatrixInv = setMatrixInv, getMatrixInv = getMatrixInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cmi <- x$getMatrixInv()
        if(!is.null(cmi)) {
                message("Getting cached data.")
                return(cmi)
        }
        data <- x$get()
        cmi <- solve(data)
        x$setMatrixInv(cmi)
        cmi
}
