## These two functions work together to store a matrix, checks for a cached inverse, and either 
## retreives the cached inverse or calculates the inverse of the matrix (if not cached).

## The first part of the function creates a list of functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse (m1) and get the value of m1.


makeCacheMatrix <- function(x = matrix()) {
    m1 <- NULL
    set <- function(y) {
        x <<- y
        m1 <<- NULL
    }
    get <- function() x
    setm1 <- function(inverse) m1 <<- inverse
    getm1 <- function() m1
    list(set = set, get = get, setm1 = setm1, getm1 = getm1)
}


## This function checks if the inverse (m1) has already been calculated.
## If it has been calculated, it gets the inverse from the cache and 
## does not re-calculate. If it has not been calculated, it does the calculation
## and sets the value of the inverse via the 'setinv' function.
## Finally, it prints the inverse of the matrix.

cacheSolve <- function(x, ...) {
    m1 <- x$getm1()
    if(!is.null(m1)) {
        message("Getting cached data.")
        return(m1)
    }
    data <- x$get()
    m1 <- solve(data)
    x$setm1(m1)
    m1
}
