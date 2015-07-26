## Following functions can be used to offload demanding task of calculating
## matrix inversion

## function below creates list object with four methods which can be used
## to store matrix and its inversion. storing matrix clears inversion, it
## also provides mean of reading stored values

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


## this function checks if matrix has its inversion already, if so it returns
## it, if now it reads original matrix and sets its inversion

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

