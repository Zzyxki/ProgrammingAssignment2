## CacheMatrix Class V_0.1
## 

## Class: CacheMatrix
## Methods: 
##	set: Assign a matrix x to the object, resets the value of inv
##	get: Returns the matrix x
##	setinv: Sets the cached inverse matrix inv mostly used in cacheSolve
##	getinv: Returns the inverse matrix inv
##	cacheSolve: calculates the inverse matrix inv of x
##	fcacheSolve: force recalculation of inv

makeCacheMatrix <- function(x = matrix()) {
## Object creation function
	set<-function(y){
	## Set the value of the matrix & reset inverse
	## as it would be invalid otherwise
		x<<-y
		inv<<-NULL
	}
	get<-function() x

	## gives the original matrix as opposed to the object

	setinv<-function(inverse)inv<<-inverse

	## insert the cached value of the inverse

	getinv<-function()inv

	## similar to get, returns a parameter as opposed to the object

	cacheSolve<-function(){
		inv<-x$getinv()
`		if(!is.null(inv)){
			message("Inverse matrix already cached.")
		}
		matrix<-x$get()
		inv<-solve(matrix)
		x$setinv(inv)
	}
	
	## This was supposed to be a seperate function but I feel it works
	## better as a method.

	fcacheSolve<-function(){
		matrix<-x$get()
		inv<-solve(matrix)
		x$setinv(inv)
	}
	
	## I included this method in case the wrong matrix was stored in inv
	
	list(set=set,get=get,setinv=setinv,getinv=getinv,cacheSolve=cacheSolve) 
}


## The seperate function of cacheSolve is detailed below in case the method
## doesnt work as intended.

##cacheSolve <- function(x) {
##	inv<-x$getinv()
##`		if(!is.null(inv)){
##			message("Inverse matrix already cached.")
##			return(inv)
##		}
##		matrix<-x$get()
##		inverse<-solve(matrix)
##		x$setinv(inverse)
##		inv
##	}

