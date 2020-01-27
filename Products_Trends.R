
#import dataset
Scatter <- read.csv('C:/Users/fmang/Desktop/SampleTickets.txt',header =TRUE, sep ='\t')

#View (Scatter)

Scatter$Month.Year..Ticket.Created.= factor(Scatter$Month.Year..Ticket.Created.,levels(Scatter$Month.Year..Ticket.Created.)[c(4,3,2,1,6,5)]) #Reset Levels Ordering

Scatter<-Scatter[is.na(Scatter$Month.Year..Ticket.Created.)==0,] #ensure there are no blanks for that column under Month.Year..Created.

#Scatter<-Scatter[Scatter$Product.!="-",] #Filtered out empty entries
#Scatter<-Scatter[Scatter$Product.!="",]


temp<-unique(as.character(Scatter$Product.)) #Get unique list of product names

t<-unique(as.character(Scatter$Month.Year..Ticket.Created.))    #Set up unique months


c<- list()

for(i in 1:length(t))   # start of a for loop
{
  c<-c(c,rep(t[i],length(temp)))
  i<-i+1
}

z<-sapply(c, function(x){as.character(x[1])})     #Transpose the data

s<-as.data.frame(cbind(rep(temp,length(t)),z))      #Coerce into a data frame
names(s)[1]<-"Product"                              #Set names to columns
names(s)[2]<-"Month"

s$link<- paste(s$Product,s$Month)
View(s)

Scatter$link<-paste(Scatter$Product.,as.character(Scatter$Month.Year..Ticket.Created.))
unique(s$link)
unique(Scatter$link)
final<- merge(s,Scatter,by.x="link",by.y="link",all.x=TRUE)
#View(final)
final[1,]
names(final)
f<-cbind(final[2:3],final[8])

names(f)



#f$Product = factor (f$Product, levels(f$Product)[c(2:5,8:10,1,14:15,6:7,20,11:13,16:19)]) #factoring of products
names(f)[3] <- "Total"
#names(f)[1]<-"Product"
#names(f)[2]<-"Month"

f<-aggregate(f$Total,by=list(f$Product,f$Month),FUN = "sum")                   #Aggregating the data set
names(f)[3] <- "Total"
names(f)[1]<-"Product"
names(f)[2]<-"Month"

unique(f$Product)

for (i in 1:length(f$Total))
{
  if(is.na(f$Total[i])==1)
    f$Total[i]<-0
  i<-i+1
}


f$Month <- factor(f$Month,levels(f$Month)[c(4,3,2,1,6,5)])           #factoring of months



a<- list()                                  #Set empty lists as buckets to store and differentiate good and bad trends
b<- list()

for (i in 1:length(temp))                   #Iteration thru each product to get the best fit line
{
  h<-f[f$Product==temp[i],]
  plot(h$Month,h$Total)
  if(mean(h$Total)< 1)          #Average less than 0.5 CIRTs is considered good, move on to next item
  {
    a <- c(a,temp[i])
    next
  }
  h$Month <- as.numeric (h$Month)
  abline(c<-lm(h$"Total" ~h$Month,na.action=na.omit))           #determine the sign of the slope
  print (coef(c)[2])                                            #retrieve it
  if(coef(c)[2] < 0)                                            #different signs of the slope will be mapped to different buckets
  {
    a<-c(a,temp[i])}
  else{
    b<-c (b,temp[i])
  }
}

v<-sapply(a, function(x){as.character(x[1])})                         #transpose the data
u<-sapply(b, function(x){as.character(x[1])})

#plot
xyplot(f$"Total"~f$Month|f$Product, data = f,
       panel = function (x,y,subscripts){ panel.xyplot(x,y, pch =21, fill ="blue", col = "black")
         colors <- c(rep("green",length(a)),rep("red",length(b)))
         product <- c(v,u)
         cur.product <- f$Product[subscripts][1]
         panel.lmline(x,y,lwd =2, col = colors[product == cur.product])
       },
       main="CIRT Distribution by Product and Month",
       ylab="CIRT Count", xlab="Month of Occurrence", scales=list(x = list(rel="free",rot = 45),y=list(at = seq (from =0, to =50,by= 5)))
)


