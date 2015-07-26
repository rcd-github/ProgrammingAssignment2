## The overall objective of the combination of both functions is to return the inverse of a 
# matrix that is provided by the user as argument x

# Function makeCacheMatrix returns a list composed of four elements, each one of which is a 
# function: set, get, setinverse, and getinverse. setinverse sends the value of the inverse of
# matrix x to the cache. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function cacheSolve calculates the inverse matrix of matrix x (which was provided as an 
# argument to Function makeCacheMatrix. If the inverse is already in the cache, because it
# was calculated before, given that the matrix x is the same, this function instead of 
# calculating the inverse matrix again, fetches it from the cache directly and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
