## Assigneent2:
 
## The following two functions allow to set/get a matrix and its inverse 
## (if invertible) and store (and then retrieve) them in (from) the cache
## 
## Usage example:
## > mat44<-matrix(rnorm(16),4,4)
## > mat44
##           [,1]        [,2]       [,3]       [,4]
## [1,]  0.2307788 -1.81980183 -1.0252312  0.4263521
## [2,]  2.2994369 -1.52020221  2.1121725 -0.9382014
## [3,]  1.0797694 -2.06983367  0.6502715 -1.8972151
## [4,] -1.0557448  0.02750255 -0.9763753 -1.2649795
## > test<-makeCacheMatrix(mat44)
##
## > cacheSolve(test)
##            [,1]      [,2]      [,3]      [,4]
## [1,]  2.6238184  6.427805 -6.941078  6.527231
## [2,]  0.9951161  3.212447 -3.671476  3.459287
## [3,] -2.3109931 -4.888131  5.549184 -5.476169
## [4,] -0.3844472 -1.521860  1.430020 -1.936122 
##
## > cacheSolve(test)
## Cached result
##           [,1]      [,2]      [,3]      [,4]
## [1,]  2.6238184  6.427805 -6.941078  6.527231
## [2,]  0.9951161  3.212447 -3.671476  3.459287
## [3,] -2.3109931 -4.888131  5.549184 -5.476169
## [4,] -0.3844472 -1.521860  1.430020 -1.936122
 




## This function creates a special list of functions that, for a given matrix, 
## makes possible to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) calculate and set the inverse of the matrix 
## 4) get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

	## Init the m_inverse to NULL
        x_inv <- NULL                     
  
	## set the matrix to cache
  	set_x <- function(y) {                      
    	x <<- y
    	x_inv <<- NULL              
  	}
  
 	## get the cached matrix
  	get_x <- function() x                           
  
  	##set the inverse using solve()
  	set_xinverse <- function(solve) x_inv <<- solve
  
  	## get the inverse     
  	get_xinverse <- function() x_inv        
  
  	## create the list of functions for makeCacheMatrix        
  	list(
  		set = set_x, 
  		get = get_x,                    
    	setinverse = set_xinverse,
    	getinverse = get_xinverse
    )
}


## This function returns the cached result if the inverse of the matrix x has been already calculated.
## Otherwise, it calculates, sets and returns the inverse of the matrix x.

cacheSolve <- function(x, ...) {
    
        ## try to get from cache
        x_inv <- x$getinverse()
  
  	## return the cached result if the inverse for matrix x exists
  	if(!is.null(x_inv)) {                 
    	message("Cached result")
    	return(x_inv)
  	}
  	## set and return the inverse for matrix x if it does not exist:
  	else{
  		nx <- x$get()                               
  		nx_inv <- solve(nx, ...)
  		x$setinverse(nx_inv)
  		return(nx_inv)
  	}
}
