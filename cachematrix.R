## The following codes first, creates a "Cache matrix" which means a matirx##
##whose inverse value will cached if required or else computed in the second function##

### A function in which a matrix and an empty inverse matrix linked to the matrix are created## 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL              ##createsinverse of the matrix#
  set <- function(y) { 
    x <<- y 
    inv <<- NULL
  }                        ##sets a matrix##
  get <- function() x      ##gets the matrix set in the above function##
  setinv <- function(inverse) inv <- inverse   ##sets the value of the inverse##
  getinv <- function() inv                     ##gets the value of the inverse##
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)        ##returns the list##
}

##This function caches the value of the matrix created in the above function.##
##If the matrix is different, it will calculate the inverse using solve##

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)            ##caches the inverse value if already caculated##
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)            ##If not calculates the inverse value##
  inv                     ##return inverse of the matrix##
}
