## The overall objective of the combination of both functions is to return the inverse of a 
# matrix that is provided by the user as argument x

# Function makeCacheMatrix returns a list composed of four elements, each one of which is a 
# function: set, get, setinverse, and getinverse. setinverse sends the value of the inverse of
# matrix x to the cache. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # set ORIGINAL value of the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setinverse <- function(solve) inv <<- solve # assigns the value of the inverse to cache
  getinverse <- function() inv  # checks to see whether the inverse has a value NULL or not
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

# The code provided by forking the data was correct. I could have changed the variable names,
# but I decided to leave it as it is, as it added no functionality
# to make the code work, for example, we define a matrix z, so that
# z <- matrix(c(1,2,3,4), nrow=2)
# then, we create the various functions using makeCacheMatrix as follows:
# test <- makeCacheMatrix(z)
# After that, we call
# cacheSolve(test)
# we can check that the solution is correct by using solve(z)
# the first time, it calculates the inverse of z with solve and stores the variable in
# inv. If we call cacheSolve(test) again, it returns the cache value
