# This finds the optimal steady state harvest point for each individual species
# It includes a minimum viable biomass

FishOpt_GS<-function(bvec,K,g,p,cost,beta,disc,tol=.1, t_end = 300){
  # K: carrying capacity
  # bvec: biomass vector
  # g: 
  # p: price
  # beta: power indicating the fisheries is more difficult
  # cost: cost
  # disc: discount rate
  # t_end: # of periods or years
  # tol: tolerance check for the bellman equation convergence
  
  # set discount factor
  delta=1/(1+disc)
  #Optim objective function
  Objective<-function(f,b,bvec,K,gar,p,cost,beta,delta,V){
    r<-gar #intrinsic growth
    bnext = max(min(bvec), b + r*b*(1-b/K) - f)
    bnext = min(max(bvec), bnext) #Make sure we don't go below the minimum biomass
    profit=p*f-cost*f^beta
    Vnext = spline(x=bvec,y=V,xout=bnext) #interpolation of the value function
    negout = -(profit + delta*Vnext$y) # bellman equation
    return(negout) # Negative profits
  }
  #Set up storage vectors and initial loop parameters
  tolcheck=10*tol
  t=0 #time vector
  Vstar=rep(0,length.out=length(bvec)) #optimal value 
  f=rep(1,length.out=length(bvec))
  #While iterate until convergence
  while(t<4|tolcheck>tol){ #
    t=t+1 #accumulate time
    V=Vstar #keep vstar
    pastf=f
    #Loop over states
    for(i in 1:length(bvec)){
      b=bvec[i]
      if(i==1){
        guess=0
      }else{
        guess=f[i-1]
      }
      low=0.00001 ## If you get error in optim finite fn convergence issues, changes this to something positive but REALLY small 0.00001
      high=(b + g*b*(1-b/K)) #Max possible harvest is the available fish plus the growth
      Opt=optim(par=guess,fn=Objective,lower=low,upper=high,method = "L-BFGS-B",b=b,bvec=bvec,gar=g,cost=cost,K=K,p=p,V=V,beta=beta,delta=delta)
      f[i]=Opt$par
      Vstar[i]=-Opt$value
    }
    #Check tolerance in each time period, this is policy function convergence, no check for value function convergence
    tolcheck=sum(abs(f-pastf))
    if(t>t_end){
      print("Exceed Maximum time steps with tolerance remaining:")
      print(tolcheck)
      break
    }
    converge=c(t,tolcheck)
  }
  return(list(Policy=f,Value=Vstar,converge=converge))
}