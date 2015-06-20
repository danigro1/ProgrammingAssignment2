## We're going to create a function that will store
## a Metrix and it's Inverse in Cache.  Then we're
## to create another funtion that will invert a matrix
## but only after it inspects if the matrix has been
## inverted and stored in cache already.

## This function creates a special "vector", 
## which is really a list containing a function to
## set the value of the x matrix
## get the value of the x matrix
## set the value of the i inverted matrix
## get the value of the i inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) i <<- Inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function will check to see if the matrix arg matches one that
## has been cached and if available will return the cached inverted
## version.  Otherwise, it will invert the matrix, store it in cache
## and return the inverted matrix
cacheSolve <- function(x, ...) {
  ## need a way to compare two matrixes
  matrixtest <- function(m1,m2){
    is.matrix(m1) && is.matrix(m2) && dim(m1) == dim(m2) && all(m1 == m2)
  }
  data <- x$get()
  ## Return a matrix that is the inverse of 'x'
  ## is the current matrix already solved???
  ## if so, use the cache version
  ## this checks to see that it's the same matrix
  ## note always true on first pass
  if (matrixtest(data,x$get())) {
    ## this checks to see if the inverse is already cached
    ## Note that this code would need to be repeated in another process
    ## to ever execute the "getting cached data" code, it will not fall
    ## into this code on the initial run.    
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
  }
  ## otherwise, invert it, cache it, return it
  i <- solve(data, ...)
  x$setInverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}

