
##makeCacheMatrix and cacheSolve produce the inverse of a matrix and either
##return the cached inverse, or, if there is no cached value, create the
##inverse matrix and return it.


##The function makeCacheMatrix takes a matrix object and enables this matrix's
##inverse to be cached (returned without running the solve() function again).
##It returns a list of the functions created.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve returns the inversed matrix from the cache-if there is no cached
##value, it calculates and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}