## These functions create a function environment and return an inverse of a matrix
##  - The inverse of the matrix is calculated if necessary

## makeCacheMatix creates functions and returns them in a list

makeCacheMatrix <- function(specialMatrix = numeric()){
	set <- function(specialInput){
		specialMatrix <<- specialInput
		specialInverse <<- NULL
	}
	get <- function() specialMatrix
	setInverse <- function(newInverse) specialInverse <<- newInverse
	getInverse <- function() specialInverse
	list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}

## cacheSolve calculates a matrix inverse if it has not

cacheSolve <- function(inverseEnvironment){
	specialInverse <- inverseEnvironment$getsolve()
	if (!is.null(specialInverse)){
		message("getting cached data")
		return(specialInverse)
	}
	tempMatrix <- inverseEnvironment$get()
	specialInverse <- solve(tempMatrix)
	inverseEnvironment$setInverse(specialInverse)
	specialInverse
}
