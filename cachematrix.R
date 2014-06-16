## function that caches a matrix, first setting the value then getting the value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## check to see if the inverse of the matrix has already been cached
## if it has, used cached value, otherwise calculate the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
  
   
}
