# Problem Set 1
# Count inversions
# Input: array of n numbers
# Output: number of inversions
#
mmerge<-function(a,b) {
  invcount = 0
  r<-numeric(length(a)+length(b))
  ai<-1; bi<-1; j<-1;
  for(j in 1:length(r)) {
    if((ai<=length(a) && a[ai]<b[bi]) || bi>length(b)) {
      r[j] <- a[ai]
      ai <- ai+1
    } else {
      r[j] <- b[bi]
      bi <- bi+1
      invcount = invcount + (length(a) - ai + 1)
    }
  }
# r
#  output<-list(r,invcount)
 list(icount=invcount,r=r)
}

mmergesort<-function(A) {
  invcount2 =0
  if(length(A)>1) {
    q <- ceiling(length(A)/2)
    a <- mmergesort(A[1:q])
    b <- mmergesort(A[(q+1):length(A)])
#    mmerge(a,b)
#    r
    sorted=mmerge(a,b)
    sorted$r
 # invcount2 = invcount2 + sorted$icount
   print(sorted$icount)
  } else {
    A
  }
#  invcount2
}
IntegerArray <- read.table("~/Courses/Coursera/DAAlgo/IntegerArray.txt", quote="\"")
head(IntegerArray)
x<-c(18, 16, 8, 7, 6, 3, 11, 9, 15, 1)

x<-c(4, 1, 2, 3)
mmergesort(x)


mmergesort(IntegerArray)

inversionNumber <- function(x){
  mergeSort <- function(x){
    if(length(x) == 1){
      inv <- 0
    } else {
      n <- length(x)
      n1 <- ceiling(n/2)
      n2 <- n-n1
      y1 <- mergeSort(x[1:n1])
      y2 <- mergeSort(x[n1+1:n2])
      inv <- y1$inversions + y2$inversions
      x1 <- y1$sortedVector
      x2 <- y2$sortedVector
      i1 <- 1
      i2 <- 1
      while(i1+i2 <= n1+n2+1){
        if(i2 > n2 || i1 <= n1 && x1[i1] <= x2[i2]){
          x[i1+i2-1] <- x1[i1]
          i1 <- i1 + 1
        } else {
          inv <- inv + n1 + 1 - i1
          x[i1+i2-1] <- x2[i2]
          i2 <- i2 + 1
        }
      }
    }
    return (list(inversions=inv,sortedVector=x))
  }
  r <- mergeSort(x)
  return (r$inversions)
}

inversionNumber(x)
inversionNumber(IntegerArray$V1)

