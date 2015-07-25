## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##

makeCacheMatrix <- function(x = matrix()) {
        cmi <- NULL
        
        set <- function(y) {
                x <<- y
                cmi <<- NULL
        }
        
        get <- function() x
        
        setMatrixInv <- function(cminv) cmi <<- cminv
        
        getMatrixInv <- function() cmi
        
        list(set = set, get = get,
             setMatrixInv = setMatrixInv,
             getMatrixInv = getMatrixInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cmi <- x$getMatrixInv
        
        if (!is.null(cmi)) {
                message("Getting cached data")
                return(cmi)
        }
        
        data <- x$get()
        cmi <- solve(data, ...)
        x$setMatrixInv(cmi)
        cmi
}
