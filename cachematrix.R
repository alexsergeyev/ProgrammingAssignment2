## Extends matrix with cached inverse value of it
## Return list containing a function to
## set the value of the matrix (set)
## get the value of the matrix (get)
## set the value of the inverse matrix (setcache)
## get the value of the inverse matrix (getcache)

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setcache <- function(cache_value) cache <<- cache_value
  getcache <- function() cache
  list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## The following function calculates the inverse matrix of the special "matrix" 
## created with the makeCacheMatrix. 
## 1. It first checks to see if the inverse has already been calculated. 
## 2. If so, it gets the value from cache and skips the computation. 
## 3. Otherwise, it calculates the inverse matrix and sets the value of the cache

cacheSolve <- function(x, ...) {
  cached_data <- x$getcache()
  if(!is.null(cached_data)) {
    message("getting cached data")
    return(cached_data)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setcache(cache)
  cache
}
