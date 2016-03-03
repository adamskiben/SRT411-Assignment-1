# SRT411-Assignment-1
Code for Assignment 1
This contains all the code used for all 4 parts of the assignment.

Part 1
library(devtools)
install_github("verisr", "jayjacobs")
library(verisr)
jsondir <- 'data/vcdb2/'
vcdb<- json2veris(jsondir)


Chart 1: Frequency of Attack Actors

actors <- getenum(vcdb, "actor")
actors <- getenum(vcdb, "actor", add.freq=TRUE)
names(actors)[1]<-paste("Attack Actor")
names(actors)[2]<-paste("Frequency of Attacks")

ggplot(actors,aes(x=Actors,y=Attack_Frequency)) + 
ggtitle("Frequency of Attack Actors") +
geom_bar(stat = "identity") +
geom_text(aes(label = sprintf("%.2f%%", Attack_Frequency/sum(Attack_Frequency)*100)),vjust = -.5)


Chart 2: Type of External Actors
external <- getenum(vcdb, "actor.external.variety", add.freq=TRUE)
barchart(enum~x, data=externalAr, main="Types of External Actors",ylab = "External Actor", xlab="Occurances", auto.key=TRUE)


Chart 3: Different types of Attack Actions
action <- getenum(vcdb, "action", add.freq=TRUE)
barchart(enum~x,data=action, main="Differnt Types of Attacks",ylab = "Action", xlab="Occurances")


Chart 4: Physical types of Attacks
phyvar <- getenum(vcdb, "action.physical.variety")
barchart(enum~x,data=phyvar, main="Types of Physical Attacks",ylab = "Physical Attacks", xlab="Occurancess")



Chart 5: Types of Targeted Assets
assets <- getenum(vcdb, "asset.assets")
barchart(enum~x,data=assets, main="Types of Assets",ylab = "Assets", xlab="Occurancess")


Chart 6: CIA Attributes Attacked
att <- getenum(vcdb, "attribute")
attar <- arrange(att,desc(x))
barchart(enum~x,data=attar, main="CIA Attributes Attacked",ylab = "Attributes", xlab="Occurancess")

Chart 7: Types of Hacking Targets
hck <- getenum(vcdb, "action.hacking.vector")
barchart(enum~x,data=hck, main="Types of Hacking Targets",ylab = "Hacking Types", xlab="Occurancess")

Chart 8: Attacks on Assets
actAss <- getenum(vcdb, enum="action", primary="asset.assets", add.freq=TRUE)

levelplot(x ~ enum*enum1, data = actAss, shrink = c(0.5, 1), main = "Type of Attacks on Assets", xlab="Attacks", ylab="Assets",col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))

Chart 9: Attack on CIA
actCia <- getenum(vcdb, enum="action", primary="attribute", add.freq=TRUE)
levelplot(x ~ enum*enum1, data = actCia, shrink = c(0.5, 1), main = "Type of Attacks on CIA", xlab="Attacks", ylab="CIA",col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))

Part 2
Chart 1: Different Types of Attacks 	
xyplot(Attack.category ~ Number.of.events, data =ev, ylab = "Types of Attacks", xlab = "Number of events", geom = "boxplot", xlim=range(0:5000))

Chart 2: Different types of attacks
qplot(Attack.category,data = ip, main = "Different types of Attacks")

Chart 3: Different types of attacks and Source IPs
qplot(Attack.category,data = ip, fill = Source.IP, main = "Different types of Attacks w sIP")  	

Chart 4: Different types of attacks and Destination IP’s
qplot(Attack.category, data = ip,  fill = Destination.IP,  main = "Different types Attacks w dIP")       

Chart 5: Source  IP Counts
qplot(Source.IP,data = ip, main = "Different types of Attacks")

Chart 6: Focus on single IP address 175.45.176.1
SingleSipFilt = filter(ip, Source.IP == '175.45.176.1')			# focus on a single IP
SingleSipGbyAttck = groupby(SingleSipFilt, Attack.category)	       # set it up for summarize
SingleSipSum = summarize(SingleSipGbyAttck, cont=n())	     # Attcks and Counts    
SingleSipArr = arrange(SingleSipSum, desc(cont), Attack.category)      #high to low
barchart(cont~Attack.category, data=SingleSipArr)		#attack occurrences  for single IP
or
SingleSipFilt = filter(ip, Source.IP == '175.45.176.1')
qplot(Attack.category,data = SingleSipFilt, main = "Attacks from SIP 175.45.176.1")

Chart 7: Table with 3 columns of counts of sIP, Attacks, Freq, in 13 attack boxes
1. tbip=table(ip$Source.IP, ip$Attack.category)
2. tbip=as.data.frame(table(ip$Source.IP, ip$Attack.category))
3. xyplot(Freq ~ sIP | Attcks, data = tbip, main = "Number of attack from Source IP", xlab="Source IP", ylab="Occurances", pch=19 )
or
4. levelplot(Freq ~ Var1*Var2, data = tbip, shrink = c(0.5, 1), main = "Number of attack from Source IP", xlab="Source IP", ylab="Occurances",col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))



Chart 8: Destination Port Numbers being used by Attacks
dstip = as.data.frame(table(ip$Destination.IP, ip$Destination.Port) 
topPor2 = dstip[dstip$Freq > 5,]                                      
xyplot(ports ~  dIP , data = topPor2, main = "Destination Port Numbers being used by Attacks", xlab="Destination IP", ylab="Destination Port", grid=TRUE )

Chart 9: Top 25 Attacked Destination Ports  	    
1.    port = as.data.frame(table(ip$Destination.Port) 		#count all different ports
2.    top = port[port$Freq > 100,] 				#only counts of ports above 100
3.    xyplot(Port.Num ~ Freq, data = top,  ylab = "Destination Port Number", xlab = "Number of attacks",main = "Top 25 Attacked Destination Ports" , geom = "boxplot", xlim=range(100:20000))

Chart 10: Destination port 80 attacks (subset)     
1. http = subset(ip, Destination.Port == "80")  
2. qplot(Attack.category,data = http, fill = Source.IP, main = "Different types of HTTP Attacks")


Chart 11: Top Protocols used in Attacks
smpr=as.data.frame(table(ip$Protocol))
topprot = smpr[smpr$Freq >390]
barchart(Var1 ~  Freq , data = topprot, main = "Top 11 Protocols used in attacks", xlab="Number of Times used", ylab="Protocol", grid=TRUE )
barchart(Var1 ~  Freq , data = topprot, main = "Top 11 Protocols used in attacks", sub="(Displaying Detail of Smaller Value Range)", xlab="Number of Times used", ylab="Protocol", grid=TRUE, xlim=range(0:10000)
1. Chart 1: Ping Sweep Frequency and Destination IP
icmp = subset(dos, Protocol == "ICMP")
dt = as.data.frame(table(icmp$Destination))
top = dt[dt$Freq>0,]
xyplot(Var1 ~ Freq, data =top, ylab="Destination IP",xlab="Frequency", type = "b", main = "Ping Sweep Occurances on Destination IP")

2. Chart 2: Locate sadmind daemon

sadmin = subset(dos, Protocol == 'SADMIND')
xyplot(Source ~ Time, data = sadmin, ylab = "IP Address", xlab = "Time", main = "Locate hosts running SADMIND protocol")

3. Chart 3: Intrusion through sadmind vulnerability

tel = subset(dos,Protocol == "TELNET")
tbip = as.data.frame(table(tel$Source,tel$Destination))
toptb = tbip[tbip$Freq>0,]
levelplot(Freq ~ Var1*Var2, data = toptb, shrink = c(0.5, 1), main = "Number of attack from Source IP", xlab="Source IP", ylab="Occurances",col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))


4. Chart 4: Trojan DDoS host Installation

ds2 = subset(dos, Destination %in% c('172.16.115.20','202.77.162.213','172.16.112.10','172.16.112.50'))
ds3 = subset(ds2, Source %in% c('172.16.115.20','202.77.162.213','172.16.112.10','172.16.112.50'))
ds4 = subset(ds3, Protocol %in% c('TELNET','RSH','TCP'))
ds4$No. <- NULL, ds4$Length<-NULL, ds4$Info <- NULL
p <- ggparcoord(data = ds4, columns = 1:3, title = "DDos Attack Graph with Specific Protocols (TCP/TELNET/RSH)  Focusing on Compromised Hosts", showPoints = TRUE)
print(p)


5. Chart 5  DDoS  Attack Graph with Multiple Protocols High Activity Compromised Hosts 

ds2 = subset(dos, Destination %in% c('172.16.115.20','202.77.162.213','172.16.112.10','172.16.112.50'))
ds3 = subset(ds2, Source %in% c('172.16.115.20','202.77.162.213','172.16.112.10','172.16.112.50'))
p <- ggparcoord(data = ds3, title = "DDos Attack Graph with Multiple Protocols Focusing on High Activity Compromised Hosts")
 print(p)

1. Graph 1- Threat occurrences in Last 24 hours
cy <- read.csv("D:/srt411(dataAnalysis)/Data Sets/cymon/cy.csv")
barchart(Freq~Threat, data=cym, xlab='Threat Category', ylab="Frequency", main="Threats Occurances in Last 24 hours")

2. Graph 2 – Threat event occurancs for buderotic.com
bud = subset(cy,Domain == "buderotic.com")
qplot(Threat,data = bud, main = "Threa Event Occurances for buderotic.com (188.127.239.142)" )

3. Graph 3- Threat occurances for buderotic.com
bud$Date <- as.Date(as.character(bud$Date),format="%Y%m%d")
xyplot(Threat ~ Date, data = bud, ylab = "Threat", xlab = "Months (2015-2016)", type = "b",main= "Threat Occurances for buderotic.com", scales = list(x=list(at = as.numeric(bud$Date),labels=format(bud$Date,"%m"))))

4. Graph 4 – Alias IP for buderotic.com
Dom = as.data.frame(table(cy$Source,cy$Domain,cy$Threat))
topDom = Dom[Dom$Freq>0,]
topDom2 = subset(topDom, Var1 != "188.127.239.142" )
levelplot(Freq ~ Var1*Var3, data = topDom2, shrink = c(0.5, 1), main = "Alias IP's for buderotic.com", xlab="Alias IP", ylab="Threats Reported",col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))

5. Graph 4 – Alias Domains for buderotic.com
levelplot(Freq ~ Var2*Var3, data = topDom2, shrink = c(0.5, 1), main = "Alias Domain Names for buderotic.com", xlab="Alias Domain", ylab="Threats Reported",col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))


colnames(topDom2)[2] <- "Domains"
qplot(Var3,data = topDom2, fill = Domains, main = "Alias Domains for buderotic.com ", xlab = "Threats Reported")









      







