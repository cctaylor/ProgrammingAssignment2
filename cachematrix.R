## Store a matrix in a class. Once the inverse has been calculated once, cache it so 
## the inverse never has to be recalcalculated.

## Create a matrix class with set and get functions, and the matrix inverse with get and
## set functions

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- matrix(0, nrow = nrow(x), ncol = ncol(x))
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverted) inv <<- inverted
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Return the inverse of the matrix. First check if previously calculated and cached
## if so, return cached version. If not, calculate and cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!matequal(matrix(0, nrow=nrow(x$get()), ncol=ncol(x$get())),inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}

matequal <- function(x, y) {
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}