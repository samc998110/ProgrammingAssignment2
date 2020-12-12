## The makeCacheMatrix function creates a list of function which are anchored in an environment 
## (the environment during execution of makeCacheMatrix). When these functions are later called 
## during the execution of cacheSolve function, they are executed in a manner that
## relate symbols, with priority, to objects in the said environment. 
## Access to that environment is possible because when these functions are saved, 
## the environment is also saved as supporting info.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(m){
    x <<- m
    i <<- NULL
  }
  get <- function() x
  seti <- function(invertedm) i <<- invertedm
  geti <- function() i
  list(set = set, get = get, seti = seti, geti = geti)
  
}


## access the saved environment via the functions(strapped to said environment) 
## on list to retrieve the value of the cached "i" in that environment. If no 
## prior data saved to "i", then solve the input matrix and save the solution to 
## "i" in that environment.


cacheSolve <- function(x, ...) {
  i <- x$geti()
  if (!is.null(i)){
    message("cached data exist")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$seti(i)
  i
}
