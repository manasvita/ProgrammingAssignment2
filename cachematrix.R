## The two function declared below are meant to create a special "matrix" object
## and store the value of its inverse in a way that it can be accessed later from 
## the memory instead of recomputing the inverse of the same matrix. This process of
## carrying out a certain operation on a very long vector and storing its value in
## memory that can be looked up as needed instead of re-calculating is called caching
## and saves lot of computation time when very long vectors are involved.

## The function below accepts a matrix as an argument and creates special "matrix" 
## object that can cache its inverse. It sets the matrix, gets the value of the matrix,
## sets the value of its inverse and gets the value of its inverse. Finally, it makes 
## a list of all these functions to carry out the entire operation.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(setvar = set, getvar = get,
       setsolvevar = setsolve,
       getsolvevar = getsolve)
}


## The function below calculates the inverse of the special "matrix" created using the 
## above function. It checks to see if the inverse already exists and if it does it 
## prints out the appropriate message and returns the value of the inverse stored in 
## the cache. If the inverse doesn't exist, it calculates it, returns the freshly 
## computed value, and sets the new value in the cache using the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolvevar()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$getvar()
  s <- solve(data, ...)
  x$setsolvevar(s)
  s
}

