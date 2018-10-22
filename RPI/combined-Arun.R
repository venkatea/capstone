Campaign.Data <- read.csv("Campaign Data for RPI.csv", na.strings = c(""," ", "  ", "NA"))
LeadData <- read.csv("Lead Data for RPI project.csv")
oldLeadData <- read.csv("RPI Project Data - Leads.csv")

NewCamp=read.csv("Campaign Data for RPI.csv",na.strings = c(""," ", "  ", "NA"))
NewOpp=read.csv("RPI Project Data - Opportunities.csv",na.strings = c(""," ", "  ", "NA"))
Oppdata
summary(Campaign.Data)
summary(LeadData)
summary(Campaign.Data$Campaign.Name)
summary(Campaign.Data$Campaign.ID)
plot(Campaign.Data$Campaign.Name)
plot(Campaign.Data$Campaign.ID)
summary(Campaign.Data$Member.Status)
datamerge=merge(NewCamp,LeadData, by="Lead.ID")
summary(datamerge$Won)

Won <- LeadData[ which(LeadData$Won=="1"), ]
Closed<- LeadData[ which(LeadData$Closed=="1"), ]
mergewon <-merge(NewCamp,Won, by="Lead.ID")
mergeclosed <- merge(NewCamp,Closed, by="Lead.ID")

opportunitydata <- merge(LeadData,NewOpp, by="Opportunity.ID")
oldOppdata <- merge(oldLeadData,NewOpp,by="Opportunity.ID")
mergedopp <- merge(opportunitydata,oldOppdata,by="Opportunity.ID")
summary(Won)

plot(datamerge$Stage)
Campaign.Data<- LeadData[ which(LeadData$Won=="1"), ]

plot(datamerge$Member.Status)

datamerge$responsedfactor=as.factor(datamerge$Responded)
summary(datamerge$responsedfactor)

opp=merge(RPI.Project.Data...Opportunities,LeadData, by="Opportunity.ID")