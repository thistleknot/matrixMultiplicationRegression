size1=4096
size2=4096
size3=4096

A <- matrix(rnorm(size1*size2),size1,size2)
B <- matrix(rnorm(size2*size3),size2,size3)

l=nrow(A)*ncol(B)

C <- double(length=l)

if(ncol(A)==nrow(B))
{ 
   m=nrow(A)
   k=nrow(B)
   n=ncol(B)
}


#dyn.load("/home/rstudio/libboost_matrix.so")
dyn.load("/home/rstudio/libcublas_sgemm.so")

#.C("main")
#m <- matrix(rnorm(100),10,10)  

#n <- length(m)

ptm <- proc.time()
D <- matrix(.C("return_Matrix",
   as.double(A),
   as.double(B),
   as.double(C),
   as.integer(m),
   as.integer(k),
   as.integer(n)
)[[3]],m,n)
timed <- proc.time() - ptm
print(timed)
dyn.unload("/home/rstudio/libcublas_sgemm.so")



#.C("main")
ptm <- proc.time()
D2 <- A %*% B
#C
timed <- proc.time() - ptm
print(timed)



