## MakeCaches stores the matrix data and cacheSolve calculates the inverse 
##if not calculates already


## makeCacheMatrix makes a list that caches the data ( the matrix and its invers)
## along with functions (set, get , setinverse, getinverse)


makeCacheMatrix <- function(x = matrix()) {

  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(inverse) inv<-inverse
  getinverse <- function() inv
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## calculates the inverse of an object x (constructed by makeCacheMAtrix)
## if  already calculated and returns it else it calculates it first

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getinverse()
  
  if(!is.null(m) )
  {
    print("getting cached inverse")
    return (m)
  }
  
  m<-solve(x$get())
  x$setinverse(m) 
  m
  
}
