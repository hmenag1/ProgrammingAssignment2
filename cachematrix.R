## This pair of functions will cache the inverse of a matrix, assuming it is
## invertible, to avoid time-consuming computations. These functions will take
## advantage of the R language scoping rules to preserve state in an R object.

## This function will create a list containing a function to:
## 1. set the values within the matrix
## 2. get the matrix back
## 3. set the values within the inverted matrix
## 4. get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function (solve) m <<- solve
	getsolve <- function () m
	list(set=set, get=get,
		setsolve=setsolve,
		getsolve=getsolve)
}


## The following function will calculated the inverted matrix from the list
## created by the function above. But before it does that, it will check to
## see if the inverted matrix has already been calculated. If not, it will
## invert the matrix and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m	

}
