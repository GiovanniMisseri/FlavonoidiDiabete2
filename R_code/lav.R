
#-----AGRUMI--------------------------------------------------------------------------------------
require(sas7bdat)
diet=read.sas7bdat(file = "/home/giovanni/Dropbox/gianni/uni/III anno/epidemiologia/tesina/diet.sas7bdat")
qq=which(diet[,2]=="Citrus fruits")
citro=diet[c(qq),]
colnames(dm2p)=c("X","Country" , "size",     "national", "Male",     "Female" , 
                 "X20.39"   ,"X40.59" ,  "X60.79")
totc=merge(citro,dm2p,by="Country")
attach(totc)
plot(somma,national,main = "Citrus Fruits",xlab = "Mean Citrus Fruits intake",ylab = "Diabet Prevalence",pch=20,col=X,cex=1.5)
cor(national,somma)
lmc=lm(national~somma)
abline(a=lmc$coefficients[1],b=lmc$coefficients[2])
text(somma,national,labels = Country ,pos=1)
#identify(totc[,3],totc[,5],pos = T)

#------TEA------------------------------------------------------------------------------------------------

t=which(diet[,2]=="Tea (Infusion)")
tea=diet[c(t),]
tott=merge(tea,dm2p,by="Country")
attach(tott)
cor(tott[,3],tott[,5])
plot(somma,national,main = "Tea",xlab = "Mean Tea intake",ylab = "Diabet Prevalence",pch=20,col=X,cex=1.5)
text(somma,national,labels = Country ,pos=1)

lmt=lm(national~somma)


abline(a=lmt$coefficients[1],b=lmt$coefficients[2])
#identify(tott[,3],tott[,5],pos = T)

#-----POMODORI----------------------------------------------------------------------------------------------------------------

p=which(diet[,2]=="Fruiting vegetables")
pom=diet[c(p),]
totp=merge(pom,dm2p,by="Country")
attach(totp)
cor(totp[,3],totp[,5])
plot(somma,national,main = "Tomatoes",xlab = "Mean Tomatoes intake",ylab = "Diabet Prevalence",pch=20,col=X,cex=1.5)

lmp=lm(national~somma)
abline(a=lmp$coefficients[1],b=lmp$coefficients[2])
text(somma,national,labels = Country ,pos=1)

#----------MODELLO LINEARE--------------------------------------------------------------------------------------------------------------

p1=merge(dm2p,citro,by="Country")
p2=merge(p1,pom,by="Country")
ori=merge(p2,tea,by="Country")
colnames(ori)=c("Country" , "size",     "national", "Male",     "Female" , 
                          "X20.39"   ,"X40.59" ,  "X60.79","citro","pomodori","tea")


ori=ori[,-c(13,11,9)]
attach(ori)
plot(density(national))
curve(dnorm(x,mean = mean(national),sd =sd(national) ),add=T,col="blue")
#####curve(dt(x,df = 30),add=T,col="red")
lm=lm(log(national)~#Male+Female+ X20.39 + X40.59 + X60.79 +
        (ori[,10]) + (pomodori) + (ori[,12]))
summary(lm)
plot(lm)
write.csv(x = ori,file = "ori.csv")

#---------PROVA GAM---------------------------------------------------------------------------------------------------------------------------------------

#polinmi frazionari   roiston


gam=gam(national~#Male+Female+ X20.39 + X40.59 + X60.79 +
      ori[,10] + pomodori + ori[,12])

summary(gam)

plot(gam)

#--------------CONSUMATORI--------------------------------------------------------------------------------------------------------------------------------------------------------------


attach(datacons)

#-----------TEA----------------------------------------------------------------------------------------------------------------------------

plot(consu.tea,national,main = "Tea",xlab = "Consumer Proportion",ylab = "Diabet Prevalence",pch=20,col=X,cex=1.5)
cor(national,consu.tea)
lmt=lm(national~consu.tea)
abline(lmt$coefficients[1],lmt$coefficients[2])
text(consu.tea,national,labels = Country ,pos=1)


#---------AGRUMI-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

plot(consu.agr,national,main = "Citrus Fruits",xlab = "Consumer Proportion",ylab = "Diabet Prevalence",pch=20,col=X,cex=1.5)
cor(consu.agr,national)
lmca=lm(national~consu.agr)
abline(a = lmca$coefficients[1],b = lmca$coefficients[2])
text(consu.agr,national,labels = Country ,pos=2)


#--------POMODORI-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

plot(consu.pom,national,main = "Tomatoes",xlab = "Consumer proportion",ylab = "Diabet Prevalence",pch=20,col=X,cex=1.5)
cor(consu.pom,national)
lmpo=lm(national~consu.pom)
abline(lmpo$coefficients[1],lmpo$coefficients[2])

text(consu.pom,national,labels = Country ,pos=1)


#---------MODELLO LINEARE %CONSUMATORI--------------------------------------------------------------------------------------------------------------------------------------------------


attach(datacons)
plot(density(national),main = "Diabetes Density vs Normal Distribution")

curve(dnorm(x,mean = mean(national),sd =sd(national) ),add=T,col="blue")
legend(0.11,30, c("Normal","Diabetes"),lty=c(1,1), lwd=c(2,2),col=c("blue","black"))
#####curve(dt(x,df = 30),add=T,col="red")
lm=lm((national)~ consu.pom+consu.agr+consu.tea)
summary(lm)

plot(lm)



#----------DESCRITTIVI--------------------------------------------------------------------------------------------------------------------------------------------------


plot(national,type = "h", frame.plot = T,xlab = "",main = "Diabetes Prevalence", ylab = "Prevalence",xaxt="n")
axis(1,at =Country,labels = Country,las=2)

attach(dm2p)
#-------------FASCIE DI ETA---------------------------------------------------------------------------------------------------
hist(X40.59,col='skyblue',border=F,add=F,xlim = c(0,0.08),main = "Diabetes prevalence in different ages",xlab = "Diabetes prevalence")
hist(X20.39,add=T,col=scales::alpha('red',.5),border=F)
hist(X60.79,add=T,col=scales::alpha('green',.5),border=F)
legend(0.063,6, c("20-39 years","40-59 years","60-79 years"),lty=c(1,1), lwd=c(2,2),col=c("red","skyblue","green"))

#--------SESSO----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

hist(Male,col='skyblue',border=F,add=F,xlim = c(0,0.08),main = "sex",xlab = "Diabetes prevalence")
hist(Female,add=T,col=scales::alpha('red',.5),border=F)
legend(0.063,6, c("Male","Female"),lty=c(1,1), lwd=c(2,2),col=c("skyblue","red"))



#-----------PROVA MAPPA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
attach(datacons)
worldMap <- getMap()
# Member States of the European Union
europeanUnion <- Country
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)

europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)


europeanUnionTable <- data.frame(country = europeanUnion, value = 1/national)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

P <- P + scale_fill_gradient(name = "Growth Rate", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")


P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))




