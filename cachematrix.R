## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix is a function that recieves a matrix and builds another 
## functions in a list to set the value of the matrix, retrieve its value, 
## set the value of the inverse matrix and retrieve the inverse matrix value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## cacheSolve is a function that recieves a function as argument, checks
## if the inverse matrix was calculated, if so, returns the inverse matrix 
## value, if null, it then retrieves the matrix and runs the solve function
## to set (cache) and return the value 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
