## These functions make it more efficient to calculate
## the inverse of square matrices by only calculating
## a given matrix's inverse once and caching the result

## This function caches the results of solve(x),
## the inverse of x, on any invertible square matrix 

 makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   x <- x
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) m <<- solve
   getsolve <- function() m
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
   m
}

## if a new matrix is entered, this function
## calculates the inverse,
## else it returns the cached solution to a
## previously-solved matrix

cacheSolve <- function(x = matrix()) {{
  m <- x[,]
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}}
