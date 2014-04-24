## Below two functions work "in companion" to set up a cache-environment and then to calculate the inverse of an - inversible - matrix, if the inverse does not already exist in cahce.
## After the two functions there is a test section, where:
## 1. a test with an inversible matrix is done.
## 2. a test with a non-inversible matrix returns an error-message.

## A "special matrix" is created in an environment, different from the current.
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
  
}


## The inverse matrix of the "special matrix" is calculated, unless it is already stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


# TESTING
### Testing with an invertible matrix.
a <- makeCacheMatrix(matrix(rnorm(9),3,3))
class(a)
class(a$get)
a$get()
a$set(matrix(rnorm(9),3,3))
a$get()
b <- cacheSolve(a)
b
b <- cacheSolve(a)


### Testing with a NONinvertible - singular - matrix.
a <- makeCacheMatrix(matrix(1:9,3,3))
a$get()
b <- cacheSolve(a)
