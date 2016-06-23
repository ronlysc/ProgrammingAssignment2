## Creates a special "matrix" object that can cache its inverse

## Creates a special object that contain a list of functions to
## 1. set the content/value of the matrix
## 2. get the content/value of the matrix
## 3. set the content/value of the inverse of that matrix
## 4. get the content/value of the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                     x <<- y
                     inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## calculates the inverse of the special matrix created with the above function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       ## checks to see if the inverse of the matrix has already been calculated
       inv <- x$getinv()
  if(!is.null(inv)) {
           ## if so, get the inverse from the cache and skip computation
           message("getting cached data")
           return(inv)
  }
## otherwise, calculated the inverse and store the result in the cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
