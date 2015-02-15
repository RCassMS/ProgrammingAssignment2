
makeCacheMatrix <- function(x = matrix()) {

#	This function creates a special matrix object that 
#	will cache its inverse. It returns a list of functions to
#	set and get the value of the matrix and to set and get the
#	value of the cached inverse matrix.
	
	m <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() {
		x
	}

	setsolve <- function(solve) {
		m <<- solve
	}

	getsolve <- function() {
		m
	}

	list(set = set, get = get, 
	setsolve = setsolve, getsolve = getsolve)
}


cacheSolve <- function(x, ...) {

#	This function computes the inverse of the special matrix 
#	returned by makeCacheMatrix() above. 
#	If the inverse has already been calculated (and the matrix 
#	has not changed), then cacheSolve() should retrieve the 
#	inverse from the cache.
	
	m <- x$getsolve()

	if(!is.null(m)) {
		message("Getting cached data...")
		return(m)
	}

	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}