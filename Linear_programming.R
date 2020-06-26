#Linear programming
library(lpSolve)
simple.lp=lp(objective.in=c(617, 238), 
             const.mat=matrix(c(1,1,0,1,0,1), 3),
             const.rhs=c(166, 100, 150), 
             const.dir=c("<=","<=","<="),
             direction="max")
simple.lp$solution

int.lp=lp(objective.in=c(617, 238), 
             const.mat=matrix(c(1,1,0,1,0,1), 3),
             const.rhs=c(166, 100, 150), 
             const.dir=c("<=","<=","<="),
             direction="max",
             int.vec=c(1,2))
int.lp$solution
int.lp$objval

int.lp=lp(objective.in=c(617, 238), 
          const.mat=matrix(c(1,1,0,1,0,1), 3),
          const.rhs=c(166, 125, 150), 
          const.dir=c("<=","<=","<="),
          direction="max",
          int.vec=c(1,2))
int.lp$solution
int.lp$objval


int.lp=lp(objective.in=c(617, 238), 
          const.mat=matrix(c(1,1,0,1,0,1), 3),
          const.rhs=c(218, 100, 150), 
          const.dir=c("<=","<=","<="),
          direction="max",
          int.vec=c(1,2))
int.lp$solution
int.lp$objval



less.sign=rep(paste("<="),8)
int.lp=lp(objective.in=c(428, 190, 642, 224, 512, 190), 
          const.mat=rbind(c(1,1,1,1,0,0),
                          c(1,1,0,0,1,1),
                          diag(1,6,6)),
          const.rhs=c(166, 166, 80, 120, 75, 100, 60, 110), 
          const.dir=dput(less.sign),
          direction="max",
          int.vec=c(1,2))
int.lp$solution
int.lp$objval



#Google's problem
less.sign=rep(paste("<="),6)
Google.lp=lp(objective.in=c(0.5,0.5, 1.6, 1, 0.75, 2, 0.5, 4, 5), 
          const.mat=rbind(c(0.5,0.5, 1.6,rep(0,6)),
                          c(0,0,0, 1, 0.75, 2, 0, 0, 0),
                          c(rep(0,6), 0.5, 4 ,5 ),
                          c(1,0,0,1,0,0,1,0,0),
                          c(0,1,0,0,1,0,0,1,0),
                          c(0,0,1,0,0,1,0,0,1)),
          const.rhs=c(170, 100, 160, 140, 80,80), 
          const.dir=dput(less.sign),
          direction="max",
          int.vec=seq(1,9))
Google.lp$solution
Google.lp$objval




