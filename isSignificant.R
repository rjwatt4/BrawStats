isSignificant<-function(method="NHST",p,r,n,evidence) {
  switch (method,
          "NHST"={
            sig<-p<alpha
            },
          "sLLR"={
            s<-r2llr(r,n,"sLLR",evidence$llr,evidence$prior)
            sig<-s>alphaLLR
            },
          "dLLR"={
            if (any(abs(r)>1)) {
              browser()
            }
            d<-r2llr(r,n,"dLLR",evidence$llr,evidence$prior)
            sig<-d>alphaLLR
            }
  )
  return(sig)
}
