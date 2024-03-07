samples<-list()
means<-c()


for (i in 1:100){
  samples[[i]]<-rnorm(20, 3, 0.5)
  means[i]<-mean(samples[[i]])
}


p<-lapply(samples, t.test, conf.level=0.95)

lower<-c()
upper<-c()

for (i in 1:100){
  lower[i]<-p[[i]]$conf.int[1]
  upper[i]<-p[[i]]$conf.int[2]
 
  
}

df<-data.frame(lower, upper, means)
df$x<-seq(1:100)


library(ggplot2)

ggplot(df, aes(x=x, y=means))+geom_point()+geom_errorbar(aes(ymax=upper, ymin=lower))+geom_hline(yintercept = 3, color="red")+
  theme_bw()+ylab("Confidence Interval")+xlab("Sample Number")
