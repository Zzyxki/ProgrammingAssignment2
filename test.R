makeCacheMatrix <- function(x = matrix()) {
## Object creation function
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}

	## Set the value of the matrix & reset inverse
	## as it would be invalid otherwise

	get<-function() {x}

	## gives the original matrix as opposed to the object

	setinv<-function(inverse)inv<<-inverse

	## insert the cached value of the inverse

	getinv<-function(){inv}

	## similar to get, returns a parameter as opposed to the object

	cacheSolve<-function(){
		inv<-getinv()
`		if(!is.null(inv)){
			message("Inverse matrix already cached")
		}
		matrix<-x$get()
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
