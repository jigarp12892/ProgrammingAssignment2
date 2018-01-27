#The first function will create a list which contains the getter and setter of square matrices and it's inverse

makeCacheMatrixMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function()m
  list(set = set, get =get,setsolve = setsolve,getsolve = getsolve)
}

# The second function checks if the inverse of an input already is in the cache, and if not, it calculates the inverse.
 
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}