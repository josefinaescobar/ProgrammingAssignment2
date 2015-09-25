##This function creates a special "matrix" stored in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,setinverse = setinverse, 
       getinverse = getinverse)
}

##This function returned a inverse matrix retrieve from the cache
#send a message when a matix stored in cache
cacheSolve <- function(x, ...) {
m <- x$getinverse()
if(!is.null(m)) {
  message("data stored in cache")
  return(m)
  }
data <- x$get()
m <- solve(data, ...)
x$setinverse(m)
m
}

