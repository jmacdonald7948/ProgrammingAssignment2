## These functions are used to cache an inverse of a matrix.
##This helps us save time, by being able to cache the data from the vector allowing it to be looked up easily, rather then recomputing the data values.

## The below function "makeCacheMatrix" creates vector that consists of a list of functions to set the value of the vector, get th value of th vector, set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  stock <- NULL
  set <- function(y){
    x <<- y
    stock <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse){stock <<- inverse}
  getinverse <- function() {stock}
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## The function below calculates the inverse of the vector created from the previous function.
## It first checks to see if the inverse was already calculated. If it does it gets the data/values from the cache an skips computing it.
## If it's not already calculated it will calculate the inverse and will cache the value of the inverse through the "setinverse" function.
cacheSolve <- function(x, ...) {
  stock <- x$getinverse()
  if(!is.null(stock)){ 
    message("get cached data")
    return(stock)
  }
  data <- x$get()
  stock <- solve(data, ...)
  x$setinverse(stock)
  stock
}
