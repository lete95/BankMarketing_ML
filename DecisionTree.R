## DECISION TREE

library(rpart.plot)
set.seed(243)

##Select Train and Test sets

boundary <- round(nrow(data)*2/3)
train_data <- data[1:boundary,]
test_data <- data[-(1:boundary),]

## Decision tree to predict subscribe variable

library(rpart)
p2 <- rpart(subscription ~ ., data=train_data,
            control=rpart.control(cp=0.001, xval=10))
printcp(p2)
plotcp(p2)
p2$cptable <- as.data.frame(p2$cptable)
ind <- which.min(p2$cptable$xerror)

## Minimum xerror found 

xerr <- p2$cptable$xerror[ind]
xstd <- p2$cptable$xstd[ind]
i = which(p2$cptable$xerror < xerr+xstd)[1]

## Cutoff value

alfa = p2$cptable$CP[i]
plot(p2$cptable[,2],p2$cptable[,3],type="l", xlab = "Size of the tree", ylab = "Relative impurity")
lines(p2$cptable[,2],p2$cptable[,4],col="blue")
legend("topright",c("R(T)training","R(T)cv"),col=c("black","blue"),lty=1)
abline(v=i, col="red", lty = 2)

## Prune the tree

p1 <- prune(p2,cp=alfa)
rpart.plot(p1)

## Plot importance of variables

p2$variable.importance
barplot(p2$variable.importance[1:8],las=TRUE,cex.names=0.65,
        main = "Importance of each variable to get the decision tree",
        col=ifelse(p2$variable.importance[1:7]>30,"orange","#ffc080"))

rm(list=setdiff(ls(), c("data","mca")))

   