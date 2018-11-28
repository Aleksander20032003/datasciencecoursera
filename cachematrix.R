# R Programming Project- Week 3 - Caching the Inverse of a Matrix

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
# A square invertible matrix with functions that set and get the matrix, then set and get its inverse 
makeCacheMatrix <- function(x = matrix()) {   
  m <- NULL
  set <- function(y) {
    x <<- y  # "<<-" because we assign assign a value to an object from another environment
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {               # if the inverse matrix is already calculated 
    message("getting cached data") # it takes it from the cache and skips this step
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
