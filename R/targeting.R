# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu) 
# or professor Ram Rajagopal (ramr@stanford.edu)

## solve QP problem => after giving sol, it will select largest N guys
## given lambda, solve arg min -lambda*t(mu)%*%x + t(x)%*%cov%*%x s.t sum of x <= N, x_i = 0 or 1
## => relax the integer constraint to 0~1
## solve.QP: min -db+1/2*bDb s.t. Ab >= b0
solve.subQP=function(lambda,mu,covar,N){
  require(quadprog)
  Dmat = 2*covar
  dvec = lambda*mu
  Amat = t(rbind(rep(-1,length(mu)),diag(length(mu)),-diag(length(mu))))
  #Amat = t(rbind(diag(length(mu)),-diag(length(mu))))
  bvec = c(-N,rep(0,length(mu)),rep(-1,length(mu)))
  #bvec = c(rep(0,length(mu)),rep(-1,length(mu)))
  sol = solve.QP(Dmat, dvec,Amat,bvec)
  sol
}

## solve the algorithm:mixed with second problem minimizing the penalty
solve.gre=function(mu,cov,Tes,N,mode=1,obj=1){
  ## mode=1 :assume the covariance matrix is diagonal
  ## mode=2 :non diagonal covariance matrix
  ## obj=1 : solve a basic SKP (maximizing the reliability to achieve the target energy saving)
  ## obj=2 : solve the second problem (minimizing the penalty when it fails in achieving the target energy saving)
  sigma = diag(cov)

  ## greedy solution
  grx = matrix(0,1,length(mu))
  Tes2=Tes
  tmpmu = mu
  if (Tes<=sum(sort(mu,decreasing=TRUE)[1:N])){ ## feasible solution
    for (i in 1:N){
      tmpmu = mu
      tmpmu[mu<Tes2/(N+1-i) | grx==1]=0
      idx = which.max(tmpmu/sqrt(sigma))
      grx[idx] = 1
      Tes2 = Tes2 - mu[idx]
    }

    if (mode==1) {
      if (obj==1) grv = (Tes - sum(mu*grx))/sqrt(sum(sigma*grx))
      else {
        tmp3 = -(Tes - sum(mu*grx))/sqrt(sum(sigma*grx))
        grv = sqrt(sum(sigma*grx))*(1/sqrt(2*pi)*exp(-1/2*tmp3^2)+tmp3*(pnorm(tmp3)-1))
      }
    } else { ## cov case
      if (obj==1) grv = (Tes - sum(mu*grx))/sqrt(matrix(grx,nrow=1)%*%cov%*%matrix(grx))
      else {
        tmp3 = -(Tes - sum(mu*grx))/sqrt(matrix(grx,nrow=1)%*%cov%*%matrix(grx))
        grv = sqrt(matrix(grx,nrow=1)%*%cov%*%matrix(grx))*(1/sqrt(2*pi)*exp(-1/2*tmp3^2)+tmp3*(pnorm(tmp3)-1))
      }
    }
  } else { ## infeasible case=> just solve with diagonal assumption
    grx[order(mu/sqrt(sigma),decreasing=TRUE)[1:N]]=1

    if (mode==1) {
      if (obj==1) grv = (Tes - sum(mu*grx))/sqrt(sum(sigma*grx))
      else {
        tmp3 = -(Tes - sum(mu*grx))/sqrt(sum(sigma*grx))
        grv = sqrt(sum(sigma*grx))*(1/sqrt(2*pi)*exp(-1/2*tmp3^2)+tmp3*(pnorm(tmp3)-1))
      }
    } else { ## cov case
      if (obj==1) grv = (Tes - sum(mu*grx))/sqrt(matrix(grx,nrow=1)%*%cov%*%matrix(grx))
      else {
        tmp3 = -(Tes - sum(mu*grx))/sqrt(matrix(grx,nrow=1)%*%cov%*%matrix(grx))
        grv = sqrt(matrix(grx,nrow=1)%*%cov%*%matrix(grx))*(1/sqrt(2*pi)*exp(-1/2*tmp3^2)+tmp3*(pnorm(tmp3)-1))
      }
    }
  }
  return(list(grx = grx, grv=grv))
}

## solve the algorithm:mixed with second problem minimizing the penalty
solve.alg2=function(mu,cov,Tes,N,mode=1,M=20,obj=1,greedy=1){
  ## mode=1 :assume the covariance matrix is diagonal
  ## mode=2 :solve subQP
  ## obj=1 : solve a basic SKP (maximizing the reliability to achieve the target energy saving)
  ## obj=2 : solve the second problem (minimizing the penalty when it fails in achieving the target energy saving)
  ## greedy=1: solve greedy algorithm
  tmpsol = matrix(0,M+1,length(mu))
  a = tan((0:M)*pi/2/M) ## slope to test
  sigma = diag(cov)

  ## greedy solution
  if (greedy==1)  gr = solve.gre(mu,cov,Tes,N,mode=mode,obj=obj)

  if (Tes<=sum(sort(mu,decreasing=TRUE)[1:N])){ ## feasible solution
    opti = 1e+8
    optx = 0; opta=0

    if (mode==1){ ## diagonal case
      for (i in 1:(1+M)){
        tmp = a[i]*mu - sigma
        n = sum(tmp>0)
        if (N<=n) tmpsol[i,order(tmp,decreasing=TRUE)[1:N]] = 1
        else tmpsol[i,which(tmp>0)] = 1

        if (obj==1){
          tmp2 = (Tes - sum(mu*tmpsol[i,]))/sqrt(sum(sigma*tmpsol[i,])) ## objective function
        } else {
          tmp3 = -(Tes - sum(mu*tmpsol[i,]))/sqrt(sum(sigma*tmpsol[i,]))
          tmp2 = sqrt(sum(sigma*tmpsol[i,]))*(1/sqrt(2*pi)*exp(-1/2*tmp3^2)+tmp3*(pnorm(tmp3)-1))
        }

        if (tmp2<opti){
          opti = tmp2
          optx = tmpsol[i,]
          opta = a[i]
        }
      }
    } else { ## mode==2
      for (i in 1:(M+1)){
        test = solve.subQP(a[i],mu,cov,N)
        tmpsol[i,order(test$solution,decreasing=TRUE)[1:N]] = 1

        if (obj==1){
          tmp2 = (Tes - sum(mu*tmpsol[i,]))/sqrt(matrix(tmpsol[i,],nrow=1)%*%cov%*%matrix(tmpsol[i,]))
        } else {
          tmp3 = -(Tes - sum(mu*tmpsol[i,]))/sqrt(matrix(tmpsol[i,],nrow=1)%*%cov%*%matrix(tmpsol[i,]))
          tmp2 = sqrt(matrix(tmpsol[i,],nrow=1)%*%cov%*%matrix(tmpsol[i,]))*(1/sqrt(2*pi)*exp(-1/2*tmp3^2)+tmp3*(pnorm(tmp3)-1))
        }

        if (tmp2<opti){
          opti = tmp2
          optx = tmpsol[i,]
          opta = a[i]
        }
      }
    }
  } else { ## infeasible case=> just solve with diagonal assumption

    opti = 1e+8
    optx = 0; opta=0

    for (i in 1:(M+1)){
      tmp = a[i]*mu + sigma
      tmpsol[i,order(tmp,decreasing=TRUE)[1:N]] = 1
      if (mode==1){
        if (obj==1){
          tmp2 = (Tes - sum(mu*tmpsol[i,]))/sqrt(sum(sigma*tmpsol[i,])) ## objective function
        } else {
          tmp3 = -(Tes - sum(mu*tmpsol[i,]))/sqrt(sum(sigma*tmpsol[i,]))
          tmp2 = sqrt(sum(sigma*tmpsol[i,]))*(1/sqrt(2*pi)*exp(-1/2*tmp3^2)+tmp3*(pnorm(tmp3)-1))
        }
      } else {
        if (obj==1){
          tmp2 = (Tes - sum(mu*tmpsol[i,]))/sqrt(matrix(tmpsol[i,],nrow=1)%*%cov%*%matrix(tmpsol[i,]))
        } else {
          tmp3 = -(Tes - sum(mu*tmpsol[i,]))/sqrt(matrix(tmpsol[i,],nrow=1)%*%cov%*%matrix(tmpsol[i,]))
          tmp2 = sqrt(matrix(tmpsol[i,],nrow=1)%*%cov%*%matrix(tmpsol[i,]))*(1/sqrt(2*pi)*exp(-1/2*tmp3^2)+tmp3*(pnorm(tmp3)-1))
        }
      }

      if (tmp2<opti){
        opti = tmp2
        optx = tmpsol[i,]
        opta = a[i]
      }
    }
  }
  out = list(opti = opti, optx = optx, opta = opta)
  if (greedy==1) out = c(out, grx = gr$grx, grv=gr$grv)
  return(out)
}

#' @export
target = function(M = 20, N = 1000, Tes = 1000, target.obj=1, target.hour = 18, target.mode=1, target.greedy=0,
                      find.N.for.Tes = 0, p = 0.95, get.prob.Curve = 0, ss=2, mu=NULL, sigma=NULL){
  ## for the time being, let's use pre-processed temperature data from tempinfo_for_1houraligned.RData
  ## if the date or zipcode is not covered by this, we may need to get temperature info by another function

  ## M: number of iteration to run the heuristic algorithm to select customers
  ## N: number of customer enrollment limit
  ## Tes: Targeted energy saving
  ## target.obj: targeting objective, transferred to solve.alg2 as obj
  ## target.hour: targeting hour, 18 means 5~6PM
  ## target.mode: targeting mode, transferred to solve.alg2 as mode
  ## target.greedy: solve targeting by greedy algorithm, transferred to solve.alg2 as greedy
    ## mode=1 :assume the covariance matrix is diagonal
    ## mode=2 :solve subQP
    ## obj=1 : solve a basic SKP (maximizing the reliability to achieve the target energy saving)
    ## obj=2 : solve the second problem (minimizing the penalty when it fails in achieving the target energy saving)
  ## find.N.for.Tes: find N to achieve Tes with probability > p
  ## p: probability to achieve Tes
  ## get.prob.Curve: find the probability curve increasing with increasing N (from prob 0.01 to 0.99)
  ## ss: step size
  ## mu: response mean vector
  ## sigma: reponse standard deviation vector

  ## select customers and return the solved result
  ## find.N.for.Tes: find N to achieve Tes with probability > p
  ## p: probability to achieve Tes
  ## get.prob.Curve: find the probability curve increasing with increasing N

  print('Targeting started')
  target.res = solve.alg2(mu = mu, cov = sigma^2,
                      Tes=Tes, N=N, mode=target.mode, M=M, obj=target.obj, greedy = target.greedy)
  print('Targeting done')

  Ns = c()
  alg.sol = c(); alg.sol.value = c()
  gre.sol = c(); gre.sol.value = c()

  if (get.prob.Curve) { ## only for obj 1
    print('Get probability curve')

    NT = min(which(cumsum(sort(mu,decreasing=TRUE))>Tes))
    i = NT
    while (i<=length(mu)){
      Ns = c(Ns, i)
      tmp = solve.alg2(mu,sigma^2,Tes=Tes, N=i, mode=target.mode, M=M, obj=target.obj, greedy = target.greedy)

      alg.sol = rbind(alg.sol,tmp$optx)
      alg.sol.value = c(alg.sol.value,1 - pnorm(tmp$opti))

      if (target.greedy==1) {
        gre.sol = rbind(gre.sol,tmp$grx)
        gre.sol.value = c(gre.sol.value,1 - pnorm(tmp$grv))
      }

      if (pnorm(tmp$opti)<0.01) break
      i = i+ss
    }
    i = NT - ss
    while (i>0){
      Ns = c(i, Ns)
      tmp = solve.alg2(mu,sigma^2,Tes=Tes, N=i, mode=target.mode, M=M, obj=target.obj, greedy = target.greedy)

      alg.sol = rbind(tmp$optx,alg.sol)
      alg.sol.value = c(1 - pnorm(tmp$opti),alg.sol.value)

      if (target.greedy==1) {
        gre.sol = rbind(gre.sol,tmp$grx)
        gre.sol.value = c(gre.sol.value,1 - pnorm(tmp$grv))
      }

      if (pnorm(tmp$opti)>0.99) break
      i = i-ss
    }
    print('Get probability curve done')
  }

  N.for.Tes = 0
  Cus.for.Tes.alg = c()
  Cus.for.Tes.gre = c()

  if (find.N.for.Tes) { ## assume p > 0.5 because it doesn't make sense if it's <0.5
    print('Start finding N for Tes ')
    if (get.prob.Curve) {
      idx = which(alg.sol.value>p)
      N.for.Tes = Ns[idx]
      Cus.for.Tes.alg = alg.sol[idx,]
      if (target.greedy==1) Cus.for.Tes.gre = gre.sol[idx,]
    } else {
      NT = min(which(cumsum(sort(mu,decreasing=TRUE))>Tes))
      i = NT
      while (i<=length(mu)){
        tmp = solve.alg2(mu,sigma^2,Tes=Tes, N=i, mode=target.mode, M=M, obj=target.obj, greedy = target.greedy)

        if (pnorm(tmp$opti)<1-p) break
        i = i+ss
      }
      N.for.Tes = i
      Cus.for.Tes.alg = tmp$optx
      if (target.greedy==1) Cus.for.Tes.gre = tmp$grx
    }
    print('Finding N for Tes done')
  }

  out = list(  N.for.Tes         = N.for.Tes,
               Cus.for.Tes.alg   = Cus.for.Tes.alg,
               Ns                = Ns,
               alg.sol           = alg.sol,
               alg.sol.value     = alg.sol.value,
               target.prob       = 1 - pnorm(target.res$opti),
               target.cus.alg    = target.res$optx )

  if (target.greedy) {
    out = c(out, Cus.for.Tes.gre = Cus.for.Tes.gre,
                 gre.sol         = gre.sol,
                 gre.sol.value   = gre.sol.value,
                 target.cus.gre  = target.res$grx )
  }
  if (!response.provide) {
    out = c(out, candidates      = candidates,
                 temp.sense.coef = temp.sense.coef,
                 temp.sense.std  = temp.sense.std )
  }
  return(out)
}
