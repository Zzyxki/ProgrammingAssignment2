## CacheMatrix Class V_1.0
## "Seeming"
## Class: CacheMatrix
## Methods
##	set: Assign a matrix x to the object, resets the value of inv
##	get: Returns the matrix x
##	setinv: Sets the cached inverse matrix inv mostly used in cacheSolve
##	getinv: Returns the inverse matrix inv
##	cacheSolve: calculates the inverse matrix inv of x
##	fcacheSolve: force recalculation of inv

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}

	get<-function() {x}

	setinv<-function(inverse)inv<<-inverse

	getinv<-function(){inv}

	cacheSolve<-function(){
		inv<-getinv()
		if(!(is.null(inv))){
			message("Inverse matrix already cached")
		}
		matrix<-get()
		inv<-solve(matrix)
		setinv(inv)
	}
	
	## This was supposed to be a seperate function but I feel it works
	## better as a method.

	fcacheSolve<-function(){
		matrix<-get()
		inv<-solve(matrix)
		setinv(inv)
	}
	
	## I included this method in case the wrong matrix was stored in inv
	
	list(set = set,get = get,
		setinv = setinv,
		getinv = getinv,
		cacheSolve = cacheSolve,
		fcacheSolve = fcacheSolve) 
 }
