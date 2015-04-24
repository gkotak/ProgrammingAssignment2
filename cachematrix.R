## These functions are used in conjunction with each other
## The makeCacheMatrix returns a list of 4 purpose-built functions - get, set,
## getSolve (inverse) and setSolve (inverse) instead of exposing the matrix
## itself. Furthermore, intead of calculating the inverse (.solve) each time
## getSolve() is called it relies on a cached value. This cache value
## is populated only after cacheSolve function is called which accepts the
## return value of makeCacheMatrix function. We do this since .solve() could be an 
## expensive (performance-wise) and could take a long time / impact CPU especially if 
## called in a loop. The typical usage of is to first called makeCacheMatrix and cacheSolve 
## only one then call the get, set, setSolve, getSolve as many times as required in your logic


## This function creates accepts a matrix and return a list of 4 functions  get, set,
## getSolve (inverse) and setSolve (inverse) on this matrix
## If you just call this function and try getSolve
## without first calling CacheSolve, then getSolve
## will return NULL, instead of trying to call the .solve
## function for performance reasons. This functionl makes use of the <<- operator
## to assign a value to an object in an environment that is different from the current environment
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      ## its important this is set to NULL as out of scope reference. Else if someone sets 
      ## with new matrix, even if cacehSolve is called, it will just return the cache which
      ## is based on old matrix and needs to be refreshed.
      inv <<- NULL
    }
    get <- function() x
    setSolve <- function(inversed) inv <<- inversed
    getSolve <- function() inv
    ## return a list of the functions.
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function set the accepts an object returned by makeCacheMatrix,
## which is a special kind of matrix with 4 functions mentioned above.
## It's objective is to cache the Inverse by calling the .solve function
## which was deliberately omitted in the makeCacheMatrix function for
## performance reasons. But before doing so, it does check to see if Inverse
## already exists. 
cacheSolve <- function(x, ...) {
    inv <- x$getSolve()
    #if already cached just return instead of computing again
    if(!is.null(inv)) {
      message("getting cached inverse data")
    }
    else {
      matrix_data <- x$get()
      inv <- solve(matrix_data, ...)
      x$setSolve(inv)
    }
    inv
}
