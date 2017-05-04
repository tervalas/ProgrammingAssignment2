## cachematrix.R
## contains functions that cache matrix inverse results
## for simpler retrieval later

## makeCacheMatrix: creates functions to cache matrix inverse results
## usage: makeCacheMatrix(matrix_name)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: Solves matrix inversion, using cached results if available
## Usage: cacheSolve(makeCacheMatrix(matrix_name))

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
