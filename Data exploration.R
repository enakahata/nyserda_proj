#setup
library(tidyverse)
library(ggplot2)
library(psych)
NYSERDA <- read.csv("NYSERDA.csv")
#head(NYSERDA)
#summary(NYSERDA)
NYSERDA_tib <- as_tibble(NYSERDA)
NYSERDA_tib

#check dates
#NYSERDA$Award.Date <- as.Date(NYSERDA$Award.Date,"%m/%d/%y")
#NYSERDA$Award.Date
#plot(NYSERDA$Award.Date)

#cleanup
tib1 <- select(NYSERDA_tib, Application.ID:Project.Title, Award.Date, Award.Amount..US.Dollars.)
tib1$Contractor.Type <- as.factor(tib1$Contractor.Type)
tib1$Project.Type <- as.factor(tib1$Project.Type)
colnames(tib1) <- c("AppID", "ContractorName", "ContractorType", "ProjectType", "Tech1", "Tech2", "Tech3", "ProjectTitle", "AwardDate", "AwardAmtUSD")
tib2 <- separate(tib1, AwardDate, c("Month", "Day", "Year"), "/")
tib2$Year <- as.numeric(tib2$Year)
tib2$Month <- as.numeric(tib2$Month)
tib2$Day <- as.numeric(tib2$Day)
head(tib2)
summary(tib2)

##Research Study Descriptions
RnD<-filter(tib2, ProjectType == "Research Study") 
select(RnD, ContractorType, ProjectTitle)

#Scatter plot of award amount and project type over time
p <- ggplot(data=tib2, aes(x=Year, y=AwardAmtUSD)) +
     geom_point(aes(color=ProjectType)) +
     scale_y_continuous(labels = scales::comma) +
     labs(title = "NYSERDA Awards over time 1991-2020", 
        subtitle = "Project funding by type",
        caption = "Source: NYSERDA", 
        x = "Year", 
        y = "Award Amount (USD)") +
     theme_minimal()
p
#not super useful because of outliers

#Bar plot of award amount and project type per year
p2 <- ggplot(tib2, aes(fill=ProjectType, x=Year, y=AwardAmtUSD)) +
  geom_bar(position="stack", stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "NYSERDA Awards over time 1991-2020", 
       subtitle = "Project funding by type",
       caption = "Source: NYSERDA", 
       x = "Year", 
       y = "Award Amount (USD)") +
  theme_minimal()
p2

#Scatter plot of award amount and contractor type over time
p3 <- ggplot(data=tib2, aes(x=Year, y=AwardAmtUSD)) +
  geom_point(aes(color=ContractorType)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "NYSERDA Awards over time 1991-2020", 
       subtitle = "Project funding by contractor type",
       caption = "Source: NYSERDA", 
       x = "Year", 
       y = "Award Amount (USD)") +
  theme_minimal()
p3
#not super useful because of outliers

#Bar plot of award amount and project type per year
p4 <- ggplot(tib2, aes(fill=ContractorType, x=Year, y=AwardAmtUSD)) +
  geom_bar(position="stack", stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "NYSERDA Awards over time 1991-2020", 
       subtitle = "Project funding by contractor type",
       caption = "Source: NYSERDA", 
       x = "Year", 
       y = "Award Amount (USD)") +
  theme_minimal()
p4

#Histogram of projects per year
p6 <- ggplot(data = sub1, aes(x=Year, fill=ProjectType)) +
        geom_histogram(binwidth = 1)+
        facet_wrap(~ProjectType)+
  labs(title = "NYSERDA Awards 1991-2020", 
       subtitle = "Number of projects funded per year",
       caption = "Source: NYSERDA", 
       x = "Year", 
       y = "Number of Projects Funded") +
        theme_minimal()
p6


#outliers
outliers <- filter(tib2, AwardAmtUSD >= 20000000)
outliers
outliers$ProjectType
outliers$ProjectTitle
outliers$ContractorName

##WITHOUT OUTLIERS
tib3 <- filter(tib2, AwardAmtUSD<=20000000)
tib3
#scatterplot
p5 <- ggplot(data=tib3, aes(x=Year, y=AwardAmtUSD)) +
  geom_point(aes(color=ProjectType)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "NYSERDA Awards over time 1991-2020", 
       subtitle = "Project funding by type",
       caption = "Source: NYSERDA", 
       x = "Year", 
       y = "Award Amount (USD)") +
  theme_minimal()
p5

#histogram w/ all award amounts
h1 <- ggplot(data = tib2, aes(x=AwardAmtUSD, fill=ProjectType)) +
  geom_histogram(binwidth=1000000) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "NYSERDA Award Size", 
       subtitle = "Project funding by type",
       caption = "Source: NYSERDA", 
       x = "Award Amount (USD)", 
       y = "# of projects funded") +
  theme_minimal() +
  facet_wrap(~ProjectType)
h1
h2 <- ggplot(data = tib2, aes(x=AwardAmtUSD)) +
  geom_histogram(binwidth=100000) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "NYSERDA Award Size", 
       #subtitle = "Project funding by type",
       caption = "Source: NYSERDA", 
       x = "Award Amount (USD)", 
       y = "# of projects funded") +
  theme_minimal()
h2

#histogram under $5,000,000
under5m<- filter(tib2, AwardAmtUSD <=5000000)
h3 <- ggplot(data = under5m, aes(x=AwardAmtUSD, fill=ProjectType)) +
  geom_histogram(binwidth=5000) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "NYSERDA Award Size", 
       subtitle = "Project funding by type",
       caption = "Source: NYSERDA", 
       x = "Award Amount (USD)", 
       y = "# of projects funded") +
  theme_minimal() +
  facet_wrap(~ProjectType)
h3

h4 <- ggplot(data = under5m, aes(x=AwardAmtUSD)) +
  geom_histogram(binwidth=50000) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "NYSERDA Award Size", 
       #subtitle = "Project funding by type",
       caption = "Source: NYSERDA", 
       x = "Award Amount (USD)", 
       y = "# of projects funded") +
  theme_minimal()
h4

#SUMMARY STATS
BizSupport <- filter(tib2, ProjectType == "Business Support")
BizSupport
mean(BizSupport$AwardAmtUSD)

describeBy(tib2$AwardAmtUSD, group=tib2$ProjectType, mat = TRUE)
describeBy(tib2$AwardAmtUSD, group=tib2$ContractorType, mat = TRUE)
StatsYear <- as.tibble(describeBy(tib2$AwardAmtUSD, group=tib2$Year, mat = TRUE))
StatsYear <- select(StatsYear, group1, n, mean, sd, median, min, max, se)
StatsYear
#lost the years somewhere in here. gotta fix that
annualmed <- ggplot(StatsYear, aes(x=group1, y=median)) +
                      geom_point() +
                      scale_y_continuous(labels = scales::comma) +
                      labs(title = "Median NYSERDA Awards over time 1991-2020", 
                      caption = "Source: NYSERDA", 
                      x = "Year") +
                      theme_minimal()
annualmed

annualavg <- ggplot(StatsYear, aes(x=group1, y=n)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Number of NYSERDA Awards per year 1991-2020", 
       caption = "Source: NYSERDA", 
       x = "Year") +
  theme_minimal()
annualavg 