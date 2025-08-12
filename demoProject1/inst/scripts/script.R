set.seed(7)
d <- read.csv("data.csv")
d <- d[!is.na(d$x1)&d$x1>0,]
d$g <- ifelse(d$trt==1,1,0)
d$y <- with(d, (x1*0.3+x2*0.1+g*0.5) + rnorm(nrow(d),0,1))
res <- tapply(d$y,d$g,mean)
zz <- res[2]-res[1]
S <- replicate(1000,{
    jj <- sample(nrow(d), nrow(d), replace=TRUE)
    tt <- tapply(d$y[jj], d$g[jj], mean)
    tt[2]-tt[1]
})
ci <- quantile(S, c(.025,.975))
cat(zz>0, ci[1], ci[2])
