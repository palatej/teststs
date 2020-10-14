library(rjdoutliers)

a<-lapply(retail, function(z){rjdoutliers::stsoutliers(log(z), seasonal = "HarrisonStevens", X.td = c(1,1,1,1,1,2,0), so=T)})
print(lapply(a, function(q){q$outliers}))

b<-lapply(retail, function(z){return (rjdoutliers::tramooutliers(log(z), X.td = c(1,1,1,1,1,2,0), so=T))})
print(lapply(a, function(q){q$outliers}))

c<-lapply(retail, function(z){return (rjdoutliers::regarimaoutliers(log(z), X.td = c(1,1,1,1,1,2,0), so=T))})
print(lapply(c, function(q){q$outliers}))
