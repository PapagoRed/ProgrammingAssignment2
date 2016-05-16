


## makeCacheMatrix is a function that creates a matrix object that 
## is able to cache the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve is a function that calculates the inverse of the 
## matrix created from the makeCacheMatrix function
## if already solved it find the chached matrix


cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    y <- x$get()
    m <- solve(y, ...)
    x$setinv(m)
    m
}