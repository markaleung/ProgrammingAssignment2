## Here are two functions for creating a wrapper consisting of a matrix and its inverse, and for accessing the inverse (and calculating and setting it if it is not calculated)

## This function creates a wrapper for a matrix, and provides methods to change and access the matrix, as well as a variable to store and access a cached value for the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse)i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes a wrapper created by makeCacheMatrix, and either returns the existing inverse value if set, or if not set, calculates the inverse value from the wrapper's matrix, assigns it to the wrapper's cached inverse value, and returns it. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}