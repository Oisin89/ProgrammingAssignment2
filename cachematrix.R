## These functions will compute the inverse of a matrix. They will also create a cache memory, so that if the  
## output inverse matrix is required again, it can be retrieved from cache memory, rather than having to be 
## re-computed multiple times.

## This first function, makeCacheMatrix creates a special 'matrix' object, which is a list containing 
## functions to:
##
##  1. - set the value of the matrix
##  2. - get the value of the matrix
##  3. - set the value of the inverse matrix
##  4. - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL   ## begins by setting the inverse matrix solve output to NULL as a placeholder.
  set <- function(y) {   ## A function to assign a new matrix y to x before resetting the
    x <<- y              ## inverse matrix solve output to NULL.
    s <<- NULL
  }
  get <- function() x    ## A function to return matrix x.
  setsolve <- function(solve) s <<- solve   ## A function to set the inverse matrix, s, to solve
  getsolve <- function() s   ## A function to return the inverse matrix.
  list(set = set, get = get,   ## Returns the 'special vector' contianing all of the previously 
       setsolve = setsolve,    ## defined functions.
       getsolve = getsolve)
}

## This second function calculates the inverse matrix using the special "matrix" of functions created 
## with the makeCacheMatrix function. However, it first checks to see if the inverse matrix has  
## already been calculated. If so, it gets the inverse matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverese matrix of the data and sets the value of 
## the inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  
  s <- x$getsolve()   ## Assigns to s the value in cache memory (NULL if nothing in there)
  if(!is.null(s)) {   ## If there is some value in cached memory, return that value...
    message("getting cached data")
    return(s)
  }
  data <- x$get()     ## ...otherwise set 'data' equal to our matrix x
  s <- solve(data, ...)  ## solve to find the inverse matrix of x
  x$setsolve(s)          ## set the cache memory value to the result, to be accessed in future
  s                      ## return the result
}
