require(lmom)
require(ismev)
require(evd)


# from ks_all function... wrap in function?
if (sum(is.na(data))>0) {
  params<-c(NA,NA,NA) 
  # for dealing with missing data in obs. in this case 'data' is 'data[,,index]'
} else {
  # use lmom package to calculate L-moments (samlmu) and then fit GEV distribution (pelgev)
  #params <- pelgev(samlmu(data,nmom=4,sort.data=TRUE))*c(1,1,-1)
  params <- pelgev(samlmu(data,nmom=4,sort.data=TRUE))
}

# Code from Tony Ladson's github gist:
# https://gist.github.com/TonyLadson/9ebe8b311cc927f5cdde1b48c3b580bd
# Referenced from his blog:
# https://tonyladson.wordpress.com/2017/07/04/converting-between-ey-aep-and-ari/
AEP2ARI <- function(AEP){
  1/-log(1-AEP)
}

ARI2AEP <- function(ARI){
  (exp(1/ARI) - 1)/exp(1/ARI)
}

# Function to calculate GEV
gevFunc <- function(fit=c('lmom','pfit','mle'),data,times,start,end){
  year_list<-unique(strftime(times,"%Y"))
  index <- which(year_list >= start & year_list <= end)
  if (length(index) < 20) {stop('Execution halted: Less than 20 years available for GEV analysis period')}
  # if (sum(is.na(data[,,index])>0)) {stop('Execution halted: Missing variable data')} moved into ks_all to deal with Obs
  dstat<-apply(data[,,index],1:2,ks_all,match.arg(fit))
  # invert the location parameter if minima (tnn)
  if ( (length(grep('tnn', var)) == 1) | 
       (length(grep('txn', var)) == 1) ) {
    dstat <- apply(dstat,2:3,y<-function(x) x*c(-1,1,1))
  }
  return(list(params=dstat[1:3,,]))
}

