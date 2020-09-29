library(rjdsts)

a<-stsoutliers(log(retail$BookStores), seasonal = "HarrisonStevens", X.td = c(1,1,1,1,1,2,0))
print(a$model$variables)
print(a$model$b)
print(a$model$b/sqrt(diag(a$model$bcov)))

b<-stsoutliers(log(retail$JewelryStores), seasonal = "HarrisonStevens", X.td = c(1,2,3,4,5,6,0))
print(b$model$variables)
print(b$model$b)
print(b$model$b/sqrt(diag(b$model$bcov)))

c<-stsoutliers(log(retail$MotorVehicleAndPartsDealers), seasonal = "HarrisonStevens", X.td = c(1,1,1,1,1,2,0))
print(c$model$variables)
print(c$model$b)
print(c$model$b/sqrt(diag(c$model$bcov)))

d<-stsoutliers(log(retail$BuildingMatAndSuppliesDealers), seasonal = "HarrisonStevens", X.td = c(1,1,1,1,1,2,0))
print(d$model$variables)
print(d$model$b)
print(d$model$b/sqrt(diag(d$model$bcov)))
