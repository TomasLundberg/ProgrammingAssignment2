## cachematrix.R
##
## Functions for calculating matrix inverses.  The inverse is
## stored in a cache which makes subsequent calls for the
## matrix inverse very fast (if the matrix data is unchanged).
##
## A special "matrix object" is used for both storing both the
## actual matrix itself and its inverse.
##
## NOTE: No error checking is done. It is implicity assumed that
## matrix supplied is always invertible.

## makeCacheMatrix
##
## Creates a "matrix object" used in subsequent matrix inversions.
##
## Usage:
##
##    x <- makeCacheMatrix()      creates matrix object 'x'
##    x$set(data)                 set the matrix values
##    x$get()                     get the matrix values
##    x$getinv()                  get the matrix inverse or NULL
##                                if the inverse hasn't been
##                                calculated yet
##    x$setinv()                  set the matrix inverse; used
##                                by 'cacheSolve'
##
makeCacheMatrix <- function(x = matrix()) {
    # Default value for the cached inverse
    inv <- NULL
    set <- function(y = matrix()) {
        # Store matrix values in matrix object
        x <<- y
        # When we set the matrix value, theinverse must be 
        # (re-)calculated so set it to NULL.
        inv <<- NULL
    }
    get <- function() {
        # Return the stored matrix
        x
    }
    setinv <- function(inverse = matrix()) {
        # Set the inverse in the cache
        inv <<- inverse
    }
    getinv <- function() {
        # Return the cached inverse
        inv
    }
    # Return list of "methods"
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## cacheSolve
##
## Returns the matrix inverse, using the inverse
## stored in the cache if available.  If no cached
## inverse is available, the inverse is calculated
## using 'solve', and the inverse is stored in the
## cache.
##
## Usage:
## 
##   y <- cacheSolve(x)
##
## Input:
##      x  "matrix object" created by 'makeCacheMatrix'
##
## Output:
##      y  inverse of 'x'
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
            # Inverse was cached
            message("Getting chached data")
            return(inv)
        }
        # No inverse chached; calculate inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        # Store inverse in cache
        x$setinv(inv)
        # Return inverse
        inv
}
