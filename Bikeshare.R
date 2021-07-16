#I used the large data set provided in the resource, file.choose() let you open the csv files from a window directly in your local computer.
ny = read.csv('new_york.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

library(ggplot2)
library(dplyr)
library(lubridate)
  ## Question 01 ##
   #Tabels#
#We create a new column that fuses the names of Start and End Station, then use the table() function to have the count.
#Finally, we order the table in descending order.
ny$Full_Trip=paste(ny$Start.Station,"-",ny$End.Station)
tb=table(ny$Full_Trip)
mx=as.data.frame(tb)
head(mx%>%arrange(desc(Freq)),5)

#We create a subset of data that contains the rows only for the top 5 full trips
Sub_data=ny[ny$Full_Trip=="Pershing Square North - W 33 St & 7 Ave"|
        ny$Full_Trip=="E 7 St & Avenue A - Cooper Square & E 7 St"|
        ny$Full_Trip=="Central Park S & 6 Ave - Central Park S & 6 Ave"|
        ny$Full_Trip=="Central Park S & 6 Ave - 5 Ave & E 88 St"|
        ny$Full_Trip=="Grand Army Plaza & Central Park S - Grand Army Plaza & Central Park S",]

#A contingency table, we know there is a user type named "", so we use {exclude = ""}.
Contingency_table=table(Sub_data$Full_Trip,Sub_data$User.Type,exclude = "")
Contingency_table

#Finally We perform a Khi test, an addition to test if the two variables are dependents
chisq.test(Contingency_table)

   #Graphics#
#To create the Barplot we construct each layer separately and store it in a name
p=ggplot(Sub_data[Sub_data$User.Type=="Subscriber"|Sub_data$User.Type=="Customer",], aes(x=User.Type, fill=Full_Trip))

#'..count../tapply(..count.., ..x.. ,sum)[..x..]' is used to creat a percentage of customers.
gm_bar=geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge")

gm_text=geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..],
		label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
		stat="count", position=position_dodge(0.9), vjust=-0.5,size=2.5)

gm_legend=theme(legend.text = element_text(colour="black", size=6,face="bold"),
		legend.background = element_rect(size=0.05),
		legend.position = "right",legend.key.size = unit(0.1, "cm"))

gm_labs=labs(title="Percentage of customers and subscribers in the top 5 full trips",y="Percentage of Users",x="User Type")

#We combine the layers to make the finall Barplot
p+gm_bar+gm_text+gm_legend+gm_labs+scale_y_continuous(labels = scales::percent)

  ## Question 02 ##
   #Tabels#
#We use ifelse() function to create a new column for the trip duration class, divided into two levels.
wash$Trip_Dur_Class=ifelse(wash$Trip.Duration/86400<=1,"Day","Over_Day")

#We convert the start time to a by using as.POSIXLT for better manipulation of the date format.
wash$Time=as.POSIXlt(wash$Start.Time,tryFormats= c("%Y-%m-%d %H:%M"))

#We create a sub-data containing the rows where only Time class is "Day".
wash_sub1=wash[wash$Trip_Dur_Class=="Day",]

#prop.table(,1) gives us the percentage of count summed by rows.
prop.table(table(wash_sub1$User.Type,wday(wash_sub1$Time,label=TRUE)),1)

#We used this graphic to look at the overall evolution of rental count by day from January to June.
labs1=labs(title="Distribution of day of the month count across six months periode",y="Day of month count",x="Time")
wash %>% ggplot(aes(as.POSIXct(Time)))+geom_freqpoly(binwidth = 86400)+theme(aspect.ratio=5/10)+labs1

   #Graphics#
#To create the Barplot we construct each layer separately.
q=ggplot(wash_sub1,aes(x=User.Type,fill=wday(Time,label=TRUE)))
bar=geom_bar(aes( y =..count..), position="dodge")
txt=geom_text(stat = "count", aes(label = ..count.., y = ..count..),position=position_dodge(0.9),vjust=-0.7,size=2.75)
labs2=labs(title="DOW count by user type for a day or less of trip duration",y="DOW count",x="User Type",fill="Day of Week (DOW)")
q+bar+txt+labs2

  ## Question 03 ##
   #Tabels#
#We created two sub-data depending on the missing values of "Birth Year", one where rows have no missing values(chi_sub) and one where there is the only row of missing values(chi_na).
chi_sub=subset(chi,!is.na(Birth.Year))
chi_na=subset(chi,is.na(Birth.Year))

#In the chi_sub data we removed entire rows of outliers found in the "Year.Birth", it's stored in "x".
outliers=boxplot(chi_sub$Birth.Year,plot=FALSE)$out
x=chi_sub
x=x[-which(x$Birth.Year %in% outliers),]

summary(chi_na[,c(4,7)])

   #Graphics#
#The scatter plot is created like those before by layers.
s=ggplot(aes(x=Birth.Year, y=Trip.Duration), data=x)
s.point=geom_point(alpha=2/10, position = position_jitter(h=0), color='green')
s.line=geom_line(stat = 'summary', fun.y=mean)
s.line.9=geom_line(stat = 'summary', fun.y = quantile,linetype=2,fun.args = list(probs =0.9),color="blue")
s.line.1=geom_line(stat = 'summary', fun.y = quantile,linetype=2,fun.args = list(probs =0.1),color="blue")
s+s.point+s.line+s.line.9+s.line.1+coord_trans(y='sqrt')+labs(title="Variation of time duration by year of birth",x="Year of Birth",y="Time Duration")

#Spearman correlation test is an addition to confirm the results, we used Spearman because of the discrete variable "Birth.Year" and for not checking the test's assumptions.
cor.test(x$Birth.Year,x$Trip.Duration)
