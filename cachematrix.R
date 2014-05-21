## Programming Assignment 2: Lexical Scoping
## These two functions will cache a matrix and its inverse

##This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL #create a NULL variable inv which will be the matrix inverse
        
        set<-function(y){
                x<<-y #reset x to the new input y
                inv<<-NULL #cache inv as NULL to before solving for new inverse
        }
        get<-function() x #get the input
        
        setinv<-function(mat) inv<<-mat #cache the inverse matrix into the inv variable
        getinv<-function() inv #retrieve the inverse matrix, inv, from the cache
        
        list(set=set,get=get,setinv=setinv,getinv=getinv) #create a list containing
        #the function values; for use in indexing in cacheSolve function
        
}


##This functions solves for the inverse of the matrix object or retrieves and 
##returns the existing inverse from the cache

cacheSolve <- function(x, ...) {
        
        if(!is.null(x$getinv())){ #check for a cached value for "inv"
                print("getting cached data")
                mat<-x$getinv()
                return(mat)
        }
        
        x0<-x$get() #get the original matrix input "x"
        mat<-solve(x0,...) #solve for the inverse of "x"
        x$setinv(mat) #cache the solution into "inv"
        mat #output inverse matrix
        
}
