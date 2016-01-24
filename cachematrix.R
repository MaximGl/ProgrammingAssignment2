## makeCacheMatrix create "special" matrix that can be cached by using cacheSolve
## Usage example:
## m <- makeCacheMatrix(matrix(rnorm(9), 3, 3)) # create new matrix 3x3
## m1 <- cacheSolve(m) # find inverted matrix
## identical(m1, cacheSolve(m)) # get matrix from cache and compare with forst call
## m$get() %*% cacheSolve(m) # result must be Identity matrix

## Create "special" matrix that can be cached

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(m) {
		x <<- m
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverted) inv <<- inverted
	getinv <- function() inv
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}

## Return inverted matrix from cache or compute it by method solve.
## Matrix must be created by function makeCacheMatrix.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
