CODE 
#####################################################################
###################   GBDT JASON  ################################
#####################################################################

library(haven)
GBDT <- read.csv("D:/R programming/Park frequency/20200417 GBDT den open.csv",sep=",")


#GBDT$gendernew<- as.factor(GBDT$male)
#GBDT$edunew<- as.factor(GBDT$education)
#GBDT$incomenew<- as.factor(GBDT$family_income_year)
#GBDT2 <- subset(GBDT, select = -c(male))
names(GBDT)<- c('y1','dislw','ldenp','ldenr','lden1','lden2','lei',
                'wdenp','wdenr','wden1','wden2','wei','age','edu',
                'gender','income','fsize','child','car','lsit',
                'wsit','pres','hukou')

library(gbm)
set.seed(2017)
gbm_H12.2 = gbm(y1 ~ dislw+ldenp+ldenr+lden1+lden2+lei+wdenp+wdenr+wden1+wden2+wei+age+edu+gender+income+fsize+child+car+lsit+wsit+pres+hukou,
                distribution = 'bernoulli',  # see the help for other choices  
                data = GBDT,
                var.monotone = c(rep(0, length = 22)),  # -1: monotone decrease, +1: monotone increase,  
                #  0: no monotone restrictions  
                n.trees=1300,                     # number of trees  
                shrinkage=0.001,                   # shrinkage or learning rate, 0.001 to 0.1 usually work  
                interaction.depth=5,             # 1: additive model, 2: two-way interactions, etc.  
                bag.fraction = 0.5,              # subsampling fraction, 0.5 is probably best  
                train.fraction = 0.5,           # fraction of data for training, first train.fraction*N used for training  
                n.minobsinnode = 10,             # minimum total weight needed in each node  
                cv.folds = 5,                     # do 3-fold cross-validation  
                keep.data=TRUE,                  # keep a copy of the dataset with the object  
                verbose=FALSE,                   # don't print out progress  
                n.cores=4)                        # use only a single core (detecting #cores is error-prone, so avoided here) 





# check performance using 5-fold cross-validation
par(mfrow=c(1,1))
best.gbm_H12.2 <- gbm.perf(gbm_H12.2,method="cv")  
print(best.gbm_H12.2)   #[1] XXX 
print(gbm_H12.2$cv.error[best.gbm_H12.2])

# plot the performance
# plot variable influence  
sum.H12.2 <- summary(gbm_H12.2,n.trees=best.gbm_H12.2) # based on the estimated best number of trees  
str(gbm_H12.2)
print(gbm_H12.2)
print(sum.H12.2)

#try to smooth those lines

library(stats)
a = 0.6
par(mfrow = c(1,1),mar=(c(6,8,5,3)))
x <- data.frame('ID' = 1:22, 'var' = c('dislw','ldenp','ldenr','lden1','lden2','lei',
                                      'wdenp','wdenr','wden1','wden2','wei','age','edu',
                                       'gender','income','fsize','child','car','lsit','wsit','pres',
                                       'hukou'), 
                'desc' = c('Commute Distance','Home Population Density','Home Road Density',
                          'Home Transit Density','Home Open Space Density','Home Mixed Land Use',
                          'Workplace Population Density',
                           'Workplace Road Density','Workplace Transit Density',
                           'Workplace Open Space Density','Workplace Mixed Land Use','Age','Education',
                           'Sex','Household Income','Household Size','Children','Car Ownership',
                          'Sitting hours at home','Sitting hours at work','Exercise perference','Hukou'))
y <- sum.H12.2[1:2]
z <- merge(x[,c('var','ID','desc')], y[,c('var','rel.inf')])
w <- z[order(z$rel.inf, decreasing = T),]


for (i in 1:22){
    y <- plot(gbm_H12.2,w[i,2],best.gbm_H12.2,return.grid = TRUE,type='response')
    plot(y,col = "blue", type = 'l',lwd=3, xlab = paste(w[i,3],'(',round(w[i,4], 1),'%)'), ylab = 'P(go to open space more than once per week)' 
         ,cex.lab=1,cex.axis=1)
  }
       
for (i in 1:22){
  y <- plot(gbm_H12.2,w[i,2],best.gbm_H12.2,return.grid = TRUE,type='response')
  s <- smooth.spline(y, spar = a)
  plot(s,col = "blue", type = 'l',lwd=3, xlab = paste(w[i,3],'(',round(w[i,4], 1),'%)'), ylab = 'P(go to open space more than once per week)', 
       cex.lab=1,cex.axis=1)
}
      
 
   

