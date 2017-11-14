# Two functions for taking in a matrix then computing and caching its inverse

# Creates a matrix object with functions to get, set, getInverse, and setInverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) i <<- inv
	getInverse <- function() i 
	list(set = set, get = get, 
		setInverse = setInverse, getInverse = getInverse)
}


# Returns a matrix that is the inverse of 'x' from makeCacheMatrix
cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)){
		message('getting cached data')
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
}
