
rn2w<-function(r,n){
  if (any(abs(r)>1)) {
    print("rn2w exception")
    r[r>1]<-1
  }
  r<-abs(r)
  z<-atanh(r)

  pw<-(r+n)*0
    pw1=pnorm(qnorm(alpha/2)+z*sqrt(n-3));
    pw2=pnorm(qnorm(alpha/2)-z*sqrt(n-3));
    pw<-pw1+pw2;
    # pw[n<3]<-0
    pw  
}

rw2n<-function(r,w){
  if (any(abs(r)>1)) {
    print("rw2n exception")
    r[r>1]<-1
  }
  r<-abs(r)
  z<-atanh(r);
  nnear<-((qnorm(1-alpha/2)+qnorm(w))/z)^2+3;
  round(nnear)  
}
