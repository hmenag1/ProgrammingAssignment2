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
  ## Set the matrix object
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
  ## Get the matrix object
	get <- function() x
  ## Set the inverted matrix object
	setsolve <- function (solve) m <<- solve
  ## Get the inverted matrix object
	getsolve <- function () m
  ## Creates a special list that will be returned
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
  ## Checks if the inverted matrix object has already been
  ## created. If not, create it.
	if(!is.null(m)) {
    ## The inverted matrix object has already been cached
		message("getting cached data")
		return(m)
	}
  ## The inverted matrix object does not exist. So,
  ## create it
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m	

}
