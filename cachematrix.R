## This R function is able to cache potentially time-consuming computations.
## A very long vector may take too lopng to compute the mean, especially if
## it has to be computed repeatedly. If the contents of a vector are not changing, 
## it will cache the value of the mean so that it can be looked up in the cache
## rather than recomputed. This script will take advantage to the scoping rules 
## of the R lanaguage and how they can be manipulated to preserve state inside of 
## an R object.

## makeCacheMatrix creates a special "matrix", which is a list containing a function to
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse of the matrix
## 4 - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	#sets i to null for initialization
	i <- NULL
	
	#store the new cache
	set <- function(y) {
		x <<- y 
		i <<- NULL
	}

	#return the stored matrix
	get <- function () x
	
	#set the inverse 
	setinverse <- function(inv) i <<- inv
	
	#get the inverse
	getinverse <- function() i

	#return the list of elements
	list (set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse
	)
	
}


## cacheSolve calculates the inverse of the special "matrix" created in the above function
## checkes first to see if the incerse has already been calculated.
## if so, it get the inverse from cache and skips the computations
## otherwise it calculated the inverse and set the value via set inverse function

cacheSolve <- function(x, ...) {
      #get the cache
	m <- x$getinverse()
	
	#check if cache is null, if not return cache
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	##get the matrix, compute the inverse, and set inverse
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)

	##return
	m 
}
