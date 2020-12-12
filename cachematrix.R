## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(m){
    x <<- m
    i <<- NULL
  }
  get <- function() x
  seti <- function(invertedm) {
    i <<- invertedm
  }
  geti <- function() {
    i
  }
  list(set = set, get = get, seti = seti, geti = geti)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$geti()
  if (!is.null(i)){
    print("cached data exist")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$seti(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
