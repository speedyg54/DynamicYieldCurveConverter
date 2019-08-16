rm(list=ls())

"
The logic within will be converting between the spot, par, and forward curves.
There will be 6 functions in total please pay attention to the notes at the
beginning of each section
"

##A) Spot to Forward
forward_from_spot<-function( R0t1 , nbr_parm){
  Frd_Curve = c(0,0,0,0)
  for (i in 1:length(R0t1) )  {
    if (i == 1) {
      #1 year forward rate is equal to the spot rate
      Frd_Curve[i] = R0t1[i]
    }
    else if (i <= nbr_parm ) {
    Frd_Curve[i] = (  ( (1+R0t1[i])^(i) ) / ( (1+R0t1[i-1])^(i-1) ) )^1/(i-(i-1)) -1
    }
  }
  cat("The Forward Curve is: ",Frd_Curve, "\n")
}

#Note this will only calculate 1 year interval forward curve
forward_from_spot( c(0.02,0.03,0.04,0.05), 4 )



##B) Forward to Spot
spot_from_forward<-function( F0t1) {
  Spt_Curve = c(0,0,0,0)
  Spt_Curve[1] <- F0t1[1]
  Spt_Curve[2] <- sqrt((1+F0t1[1])*(1+F0t1[2]))-1
  Spt_Curve[3] <- ( (1+F0t1[1])*(1+F0t1[2])*(1+F0t1[3]) )^(1/3) - 1
  Spt_Curve[4] <- ( (1+F0t1[1])*(1+F0t1[2])*(1+F0t1[3])*(1+F0t1[4]) )^(1/4) - 1
  
  cat("The Spot CUrve is: ", Spt_Curve, "\n")
  }

spot_from_forward( c(0.03,0.04,0.05,0.06))


##C) Spot to Par

par_from_spot<- function( S0t1) {
  Par_Curve = c(0,0,0,0)
  tm1 <- S0t1[1] 
  tm2 <- S0t1[2]
  tm3 <- S0t1[3]
  tm4 <- S0t1[4]
  Par_Curve[1] = 100*(1+tm1) - 100
  Par_Curve[2] = ( ( (1+tm1)*(1+tm2)^2*100) - (100*(1+tm1)) )/ ((1+tm1)+(1+tm2)^2)
  Par_Curve[3] = ( (1+tm1)*((1+tm2)^2)*((1+tm3)^3)*100 
              - (100*(1+tm1)*(1+tm2)^2) ) /
    (  ((1+tm1)*(1+tm2)^2)+((1+tm2)^2)*((1+tm3)^3)+((1+tm1)*(1+tm3)^3) )
  
  Par_Curve[4] = ( (1+tm1)*((1+tm2)^2)*((1+tm3)^3)*((1+tm4)^4)*100
              - 100*(1+tm1)*(1+tm2)^2*(1+tm3)^3 )/
      (  (1+tm1)*((1+tm2)^2)*((1+tm3)^3)+
         (1+tm1)*((1+tm2)^2)*((1+tm4)^4)+
         ((1+tm2)^2)*((1+tm3)^3)*((1+tm4)^4)+
         (1+tm1)*((1+tm3)^3)*((1+tm4)^4) )
  cat("The Par Curve is: ", Par_Curve, "\n")
}

par_from_spot( c(0.05263,0.05616,0.06359,0.07008) )

##D)Par to Spot

spot_from_par<- function ( P0t1) {
  Spot_Curve <- c(0,0,0,0)
  tm1 <- P0t1[1] 
  cpn1 <- (tm1*100) #to arrive at annualized coupon
  tm2 <- P0t1[2]
  cpn2 <- (tm2*100)
  tm3 <- P0t1[3]
  cpn3 <- (tm3*100)
  tm4 <- P0t1[4]
  cpn4 <- (tm4*100)
  Spot_Curve[1] <- tm1
  Spot_Curve[2] <-( (1/(100-cpn2/(1+Spot_Curve[1]) ) ) * (100+cpn2) )^ (1/2) - 1
  Spot_Curve[3] <-( (1/(100-( (cpn3/(1+Spot_Curve[1]))+(cpn3/((1+Spot_Curve[2])^2)))) * (100+cpn3) ) )^ (1/3) - 1
  Spot_Curve[4] <-( (1/(100-( (cpn4/(1+Spot_Curve[1]))+(cpn4/((1+Spot_Curve[2])^2))+(cpn4/((1+Spot_Curve[3])^3)))) * (100+cpn4) ) ) ^ (1/4) - 1   
  cat("The Spot Curve is: ", Spot_Curve, "\n")
}
spot_from_par( c(0.03,0.04,0.045,0.06) ) 

##E) Forward to Par
par_from_forward<- function ( F0t1) {
  Par_Curve<- c(0,0,0,0)
  #Calculate spot curve from Forward
  Spt_Curve = c(0,0,0,0)
  Spt_Curve[1] <- F0t1[1]
  Spt_Curve[2] <- sqrt((1+F0t1[1])*(1+F0t1[2]))-1
  Spt_Curve[3] <- ( (1+F0t1[1])*(1+F0t1[2])*(1+F0t1[3]) )^(1/3) - 1
  Spt_Curve[4] <- ( (1+F0t1[1])*(1+F0t1[2])*(1+F0t1[3])*(1+F0t1[4]) )^(1/4) - 1
  cat("The Spot Curve is: ", Spt_Curve,"\n")
  #For cleanerish code
  tm1 <- Spt_Curve[1] 
  tm2 <- Spt_Curve[2]
  tm3 <- Spt_Curve[3]
  tm4 <- Spt_Curve[4]
  #Calculate Par curve from Spot curve
  Par_Curve[1] = 100*(1+tm1) - 100
  Par_Curve[2] = ( ( (1+tm1)*(1+tm2)^2*100) - (100*(1+tm1)) )/ ((1+tm1)+(1+tm2)^2)
  Par_Curve[3] = ( (1+tm1)*((1+tm2)^2)*((1+tm3)^3)*100 
                   - (100*(1+tm1)*(1+tm2)^2) ) /
    (  ((1+tm1)*(1+tm2)^2)+((1+tm2)^2)*((1+tm3)^3)+((1+tm1)*(1+tm3)^3) )
  
  Par_Curve[4] = ( (1+tm1)*((1+tm2)^2)*((1+tm3)^3)*((1+tm4)^4)*100
                   - 100*(1+tm1)*(1+tm2)^2*(1+tm3)^3 )/
    (  (1+tm1)*((1+tm2)^2)*((1+tm3)^3)+
         (1+tm1)*((1+tm2)^2)*((1+tm4)^4)+
         ((1+tm2)^2)*((1+tm3)^3)*((1+tm4)^4)+
         (1+tm1)*((1+tm3)^3)*((1+tm4)^4) )
  cat("The Par Curve is: ", Par_Curve, "\n")
}
par_from_forward( c(0.03,0.04,0.05,0.06) )

##F) Par to Forward
forward_from_par <- function ( P0t1, nbr_parm) {
  #calculate the Par to Spot first
  Spot_Curve <- c(0,0,0,0,0)
  tm1 <- P0t1[1] 
  cpn1 <- (tm1*100) #to arrive at annualized coupon
  tm2 <- P0t1[2]
  cpn2 <- (tm2*100)
  tm3 <- P0t1[3]
  cpn3 <- (tm3*100)
  tm4 <- P0t1[4]
  cpn4 <- (tm4*100)
  tm5 <- P0t1[5]
  cpn5 <- (tm5*100)
  Spot_Curve[1] <- tm1
  Spot_Curve[2] <-( (1/(100-cpn2/(1+Spot_Curve[1]) ) ) * (100+cpn2) )^ (1/2) - 1
  Spot_Curve[3] <-( (1/(100-( (cpn3/(1+Spot_Curve[1]))+(cpn3/((1+Spot_Curve[2])^2)))) * (100+cpn3) ) )^ (1/3) - 1
  Spot_Curve[4] <-( (1/(100-( (cpn4/(1+Spot_Curve[1]))+(cpn4/((1+Spot_Curve[2])^2))+(cpn4/((1+Spot_Curve[3])^3)))) * (100+cpn4) ) ) ^ (1/4) - 1   
  Spot_Curve[5] <-( (1/(100-( (cpn5/(1+Spot_Curve[1]))+(cpn5/((1+Spot_Curve[2])^2))+(cpn5/((1+Spot_Curve[3])^3))+(cpn5/((1+Spot_Curve[4])^4)))) * (100+cpn5) ) ) ^ (1/5) - 1   
  
    cat("The Spot Curve is: ", Spot_Curve, "\n")
 
  Frd_Curve = c(0,0,0,0,0)
  for (i in 1:length(Spot_Curve) )  {
    if (i == 1) {
      #1 year forward rate is equal to the spot rate
      Frd_Curve[i] = Spot_Curve[i]
    }
    else if (i <= nbr_parm ) {
      Frd_Curve[i] = (  ( (1+Spot_Curve[i])^(i) ) / ( (1+Spot_Curve[i-1])^(i-1) ) )^1/(i-(i-1)) -1
    }
  }
  cat("The Forward Curve is", Frd_Curve, "\n")
} 

#Enter the Par Curve and number of years below
forward_from_par( c(0.02,0.03,0.04,0.05,0.06), 5 )

