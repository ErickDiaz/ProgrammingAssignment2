## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix creates a matrix, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse matrix
## get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) 
}


## Calculates the inverse matrix, checks to see if the inverse matrix
## has already been calculated. If so, it gets the inverse matrix from the cache
## otherwise, it calculates the inverse matrix and sets the value of the inverse matrix
## to the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ## Verifies if de inverse matrix exists 
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    ## get the matrix value
    data <- x$get()
    ## calculates the inverse matrix
    m <- solve(data, ...)
    ## set the value of the inverse matrix in cache
    x$setInverse(m)
    m
}
