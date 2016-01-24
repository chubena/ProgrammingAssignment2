## These two functions create a special matrix object that can allow for caching 
##of the inverse of a matrix, provided the matrix is a square matrix.

## This function creates a special matrix object that can be cached with the 
## of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL #introduces the null inverse matrix
  #this section of code assigns a new matrix y to the matrix x,nulls inverse
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x  #get the matrix x
  setinv<- function(rz) inv<<-rz  #set the inverse of the matrix with this function
  getinv<-function() inv #call the inverse of the matrix 
  list(set=set,get=get,setinv=setinv,getinv=getinv) #store the defined functions

}


##This function returns the inverse of a square matrix
##If the inverse was calculated and the matrix is unchanged, it returns the same inverse
##Else, the inverse is calculated and cached and also returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  temp<-x$get()
  inv<-solve(temp)
  x$setinv(inv)
  inv
}

