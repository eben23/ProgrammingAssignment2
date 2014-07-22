#source("c:/assignment/cacheMatrix.R")
## Put comments here that give an overall description of what your
## functions do

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	
	get<-function() x
	
	setInverse<-function(solve) m<<-solve
	
	getInverse<-function() {m}
	
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


#This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        
        data<-x$get()
        m<-solve(data, ...)
        x$setInverse(m)
        m
}

#Test function for calculating the inverse of a 1000*1000 matrix
#with and withuot caching
#The times taken with both the styles are also displayed
doTest<-function() {
	cacheFunc<-makeCacheMatrix()
	
	mat<-matrix(rnorm(1000*1000), 1000, 1000)
	cacheFunc$set(mat)
	
	t1<-Sys.time()
	output1<-cacheSolve(cacheFunc)
	t2<-Sys.time()
	
	
	output2<-cacheSolve(cacheFunc)
	t3<-Sys.time()
	
	print("Time Taken WITHOUT caching - FIRST TIME SOLVE")
	print(difftime(t2,t1))
	print("Time Taken WITH caching")
	print(difftime(t3,t2))
	
}
	
doTest()