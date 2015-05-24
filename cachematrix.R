## Put comments here that give an overall description of what your
## functions do

 ## Write a short comment describing this function
 # Creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) { # a square invertible matrix
  im <- NULL
  set <- function(y) { # set the matrix
    x <<- y # assign a value to an object in an environment different
    im <<- NULL
  }
  get <- function() x # get the matrix
  setinverse <- function(solve) im <<- solve # set the inverse
  getinverse <- function() im # get the inverse
  list(set = set, get = get, # list of the input to cacheSolve()
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# computes the inverse of the "matrix" returned by makeCacheMatrix()
cacheSolve <- function(x, ...) { # output of makeCacheMatrix()
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) { # if the inverse has already been calculated get it from the cache and skips the computation.
    message("getting cached data")
    return(im)
  } # if not, calculates the inverse
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im) # sets the value of the inverse in the cache via the setinv function
  im
}