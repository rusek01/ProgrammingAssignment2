## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function below was desingned along the lines of makeVector example function
## i only changet names  from mean to inv, however becouse programers need to be lazy i didnt even change m variable :)
## so now this function has 4 lets call them methods 1 - to set matrix (and wipe cache), 2 - to return that matrix
## 3 - to set cache with inverted matrix, and 4 - to get inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  ## becouse of how makeCacheMatrix works, if !is.null(m) is true we
  ## know that original matrix didn't change, becouse if we used $set method from makeCacheMatrix
  ## and thats only way to change original matrix, we set value of m to null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  d <- x$get() ## code to get matrix
  m <- solve(d)  ## code to invert matrix
  x$setinv(m)
  m
}

