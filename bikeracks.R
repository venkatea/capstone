bkoutput <- read.csv("spssoutput.csv")
bkoutputspss <-  c("longitude_bikeracks","latitude_bikeracks")
bkoutput <- -c("longitude_bikeracks","latitude_bikeracks")
delete <- c("longitude_bikeracks","latitude_bikeracks")
bkoutput <- bkoutput[, !(colnames(bkoutput) %in% delete), drop=FALSE]
bkoutput1 <- sample(nrow(bkoutput), floor(nrow(bkoutput) * 0.4))
bkmodtraining <- bkoutput[bkoutput1, ]
bkmodtraintest <- bkoutput[-bkoutput1, ]
bkoutput2 <- sample(nrow(bkmodtraintest), floor(nrow(bkmodtraintest) * 0.5))
#three datasets bkmodtraining bkmodtesttest1,bkmodtesttest2
#mid split test bkmodtraintest
bkmodtesttest1 <- bkmodtraintest[bkoutput2, ]
bkmodtesttest2 <- bkmodtraintest[-bkoutput2, ]

bkmodtraininglatlon <- bkmodtraining[bkoutputspss]
bkmodtest1latlon <- bkmodtesttest1[bkoutputspss]
bkmodtest2latlon <- bkmodtesttest2[bkoutputspss]

bkmodtraining <- bkmodtraining[, !(colnames(bkmodtraining) %in% delete), drop=FALSE]
bkmodtesttest1 <- bkmodtesttest1[, !(colnames(bkmodtesttest1) %in% delete), drop=FALSE]
bkmodtesttest2 <- bkmodtesttest2[, !(colnames(bkmodtesttest2) %in% delete), drop=FALSE]

bkmodtraining <- data.frame(bkmodtraining, bkmodtraininglatlon$latitude_bikeracks)
bkmodtesttest1 <- data.frame(bkmodtesttest1, bkmodtest1latlon$latitude_bikeracks) 
is.na()
bkmodtesttest1[bkmodtesttest1cond] <- which(bkmodtesttest1cond)
bkmodel <- depmix(bkmodtraining$`bkmodtraininglatlon$latitude_bikeracks`~bkmodtraining$NoRacks+bkmodtraining$White+bkmodtraining$Hispanic.Origin..of.any.race.+bkmodtraining$Number.of.returns, data = bkmodtraining, nstates = 10, ntimes = NULL)
colnames(bkmodtraining)[colnames(bkmodtraining)=="bkmodtraininglatlon.latitude_bikeracks"] <- "latitude"
colnames(bkmodtesttest1)[colnames(bkmodtesttest1)=="bkmodtest1latlon.latitude_bikeracks"] <- "latitude"
bkmodel2 <- nnet(latitude~NoRacks+White+Hispanic.Origin..of.any.race.+Number.of.returns,size = 3, data = bkmodtraining)

bkpredict <- predict(bkmodel2,bkmodtesttest1)
bkmodtesttest1$predictlatitude <- bkpredict
summary(bkmodtesttest1)
install.packages("depmixS4")
library("depmixS4")

