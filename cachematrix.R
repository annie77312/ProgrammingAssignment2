## Using makeCacheMatrix to cache the result of the inverse of a matrix
## and can get the cached result by cacheSolve function

## Make a cache matrix with methods
## set,get,setInvers,getInverse for cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) r <<- inv
    getInverse <- function() {r}
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## input parameters x type as makeCacheMatrix
## will return the inverse of matrix
## and cache the data in x's cache

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
