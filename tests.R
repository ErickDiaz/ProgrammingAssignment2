makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

createRandomMatrix <- function() {
  x <- matrix(runif(1000000, min=0, max=100), 1000, 1000)
  x
}

createM <- function(){
  matrix = matrix(c(4,2,7,6),nrow =2, ncol = 2)
}

makeMatrix <- function( x ){
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

cacheInverse <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

compare <- function() {
  matrix <- createRandomMatrix()

  
  ## Resultado de la funcion en cache
  a <- makeMatrix(matrix)  
  time<- system.time( cacheInverse(a) )
  print(time)
  
  ## Resultado de Solve  
  time<- system.time( solve(matrix) )
  print(time)

}
