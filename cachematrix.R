## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  1. Set the value of the matrix
##  2. Get the value of the matrix
##  3. Set the value of the inverse of the matrix
##  4. Get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	##  Set the value of the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	##  Get the value of the matrix
	get <- function() x
	##  Set the value of the inverse of the matrix
	setinverse <- function(inverse) m <<- inverse
	##  Get the value of the inverse of the matrix
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculate the inverse of the special
## "matrix" returned by `makeCacheMatrix` above.  
## However, it first checks to see if the inverse has already been calculated. 
## If so, it get the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse using the 'solve' function in R.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	## checks to see if the inverse has already been calculated. 
	if(!is.null(m)) {
		## get the inverse from the cache and skips the computation. 
		message("getting cached data")
		return(m)
	}
	## calculates the inverse using the 'solve' function in R.
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
