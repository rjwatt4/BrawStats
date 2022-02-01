drawInference<-function(IV,IV2,DV,effect,design,result,disp){
  
  r<-effect$rIV
  if (!is.null(IV2)){
    r<-c(r,effect$rIV2,effect$rIVIV2DV)
  }

  switch (disp,
          "p"={g<-p_plot(result,IV,IV2,DV,r,design$sN)},
          "r"={g<-r_plot(result,IV,IV2,DV,r,design$sN)},
          "w"={g<-w_plot(result,IV,IV2,DV,r,design$sN)},
          "nw"={g<-nw_plot(result,IV,IV2,DV,r,design$sN)},
          "e1"={g<-e1_plot(result,IV,IV2,DV,r,design$sN)},
          "e2"={g<-e2_plot(result,IV,IV2,DV,r,design$sN)},
          "ci1"={g<-ci1_plot(result,IV,IV2,DV,r,design$sN)},
          "ci2"={g<-ci2_plot(result,IV,IV2,DV,r,design$sN)}
  )
  
  g+ggtitle(result$an_name)
}
