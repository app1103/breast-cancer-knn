wbcd<-read.csv("https://raw.githubusercontent.com/shifteight/R/master/MLwR/wisc_bc_data.csv")
wbs<-wbcd[-1]
table(wbs$diagnosis)
wbs$diagnosis<-as.factor(wbs$diagnosis,levels = c("B","M"),labels = c("Benign","Malignant")
round(prop.table(table(wbs$diagnosis))*100,digits = 1)
summary(wbs)
summary(wbs[c("radius_mean","area_mean","smoothness_mean")])
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wbs_n<-as.data.frame(lapply(wbs[2:31],normalize))
summary(wbs_n$area_mean)
wbs_train<-wbs_n[1:469,]
wbs_test<-wbs_n[470:569,]
wbs_train_labels<-wbs[1:469,1]
wbs_test_labels<-wbs[470:569,1]
wbs_pred<-knn(train = wbs_train,test=wbs_test,cl=wbs_train_labels,k=21)
CrossTable(x= wbs_test_labels,y = wbs_pred,prop.chisq = FALSE)

