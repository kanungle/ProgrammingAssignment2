
## This function takes in a matrix for the cache and creates a list for get, set, getInv, and getInv.
## This cache can then be retrieved in subsequent function calls so R does not have to re-invert 
## the matrix, thus saving resources.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I<<- NULL
  }
  get <- function() x
  setInv <- function(solve) I <<- solve
  getInv <- function() I
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## This is the solving function that determines if the inverse of a matrix already exists. If it
## does, then R simply retrieves that inverted matrix from the Cache. If it does not, R then
## calculates the inverted matrix, stores it in the cache, and also returns it to the user.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInv(I)
  I
}
