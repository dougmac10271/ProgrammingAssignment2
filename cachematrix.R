## Function makeCacheMatrix takes an inversible matrix and: 
## 1. creates an inverse
## 2. caches it

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve takes a matrix and: 
## 1. checks if its inverse was previously cached, 
## 2. if the inverse is cached it returns the already inversed matrix,
## 3. if the inverse is not cached it inverts the matrix and returns that result

cacheSolve <- function(x, ...) {
  ## Tests whether the inverse of matrix 'x' was previously cached
  ## and, if so, returns the cached matrix
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Creates an inverse of matrix 'x'
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
