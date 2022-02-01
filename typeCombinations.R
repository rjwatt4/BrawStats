get2combination<-function(keypress) {
  
  IV<-variables[1,]
  IV2<-variables[2,]
  DV<-variables[3,]
  switch (keypress-48,
          { #print(1)
            IV$type<-"Interval"
            DV$type<-"Interval"
            IV2$name<-"none"
          },
          { #print(2)
            IV$type<-"Categorical"
            IV$ncats<-3
            IV$cases<-"C1,C2,C3"
            IV$proportions<-"1,1,1"
            DV$type<-"Interval"
            IV2$name<-"none"
          },
          { #print(3)
            IV$type<-"Interval"
            DV$type<-"Ordinal"
            IV2$name<-"none"
          },
          { #print(4)
            IV$type<-"Categorical"
            IV$ncats<-3
            IV$cases<-"C1,C2,C3"
            IV$proportions<-"1,1,1"
            DV$type<-"Ordinal"
            IV2$name<-"none"
          },
          { #print(5)
            IV$type<-"Interval"
            DV$type<-"Categorical"
            IV2$name<-"none"
          },
          { #print(6)
            IV$type<-"Categorical"
            IV$ncats<-3
            IV$cases<-"C1,C2,C3"
            IV$proportions<-"1,1,1"
            DV$type<-"Categorical"
            IV2$name<-"none"
          },
  )
  result<-list(IV=IV, IV2=IV2, DV=DV)
}


get3combination<-function(keypress) {

    IV<-variables[1,]
    IV2<-variables[2,]
    IV2$name<-"IV2"
    DV<-variables[3,]
    switch (keypress-48,
            { #print(11)
              IV$type<-"Interval"
              IV2$type<-"Interval"
              DV$type<-"Interval"
            },
            { #print(12)
              IV$type<-"Categorical"
              IV$ncats<-3
              IV$cases<-"C1,C2,C3"
              IV$proportions<-"1,1,1"
              IV2$type<-"Interval"
              DV$type<-"Interval"
            },
            { #print(13)
              IV$type<-"Interval"
              IV2$type<-"Categorical"
              IV2$ncats<-3
              IV2$cases<-"D1,D2,D3"
              IV2$proportions<-"1,1,1"
              DV$type<-"Interval"
            },
            { #print(14)
              IV$type<-"Categorical"
              IV$ncats<-3
              IV$cases<-"C1,C2,C3"
              IV$proportions<-"1,1,1"
              IV2$type<-"Categorical"
              IV2$ncats<-3
              IV2$cases<-"D1,D2,D3"
              IV2$proportions<-"1,1,1"
              DV$type<-"Interval"
            },
            { #print(15)
              IV$type<-"Interval"
              IV2$type<-"Interval"
              DV$type<-"Categorical"
              DV$ncats<-2
              DV$cases<-"E1,E2"
              DV$proportions<-"1,1"
            },
            { #print(16)
              IV$type<-"Categorical"
              IV$ncats<-3
              IV$cases<-"C1,C2,C3"
              IV$proportions<-"1,1,1"
              IV2$type<-"Interval"
              DV$type<-"Categorical"
              DV$ncats<-2
              DV$cases<-"E1,E2"
              DV$proportions<-"1,1"
            },
            { #print(17)
              IV$type<-"Interval"
              IV2$type<-"Categorical"
              IV2$ncats<-3
              IV2$cases<-"C1,C2,C3"
              IV2$proportions<-"1,1,1"
              DV$type<-"Categorical"
              DV$ncats<-2
              DV$cases<-"E1,E2"
              DV$proportions<-"1,1"
            },
            { #print(18)
              IV$type<-"Categorical"
              IV$ncats<-3
              IV$cases<-"C1,C2,C3"
              IV$proportions<-"1,1,1"
              IV2$type<-"Categorical"
              IV2$ncats<-3
              IV2$cases<-"C1,C2,C3"
              IV2$proportions<-"1,1,1"
              DV$type<-"Categorical"
              DV$ncats<-2
              DV$cases<-"E1,E2"
              DV$proportions<-"1,1"
            },
    )
    result<-list(IV=IV, IV2=IV2, DV=DV)
}
