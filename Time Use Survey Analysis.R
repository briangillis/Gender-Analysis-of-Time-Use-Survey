library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)


# still need to account for the category/sub-category

#############################  Cleaning ##########################################################
### choose the population group (pg) you want
a <- list("Overall",1,2,3,4)
b <- list("Participation Rates",1,5,6,7)
c <- list("Of Participants", 1,8,9,10) 
pg <- list(a, b, c)


# Loop through the sheets and load the particular data




for (j in pg)  {
  
  sheets <- c("2004","2005", "2006","2007", "2008","2009", "2010", "2011","2012","2013","2014","2015")
  t <- data.frame()
  
  for (i in sheets) {
    tus <- read_excel("D:/Google Drive/Wittenberg/ANLT 550/Time Use Surveys.xlsx",
                          sheet = i)
    # get pg as indexes (pgi)
    pgi <- as.numeric(unlist(j[2:5]))
    # pull out the columns we need and we want them characters (not factors) for now
    tus <- tus[, pgi]
    names(tus) <- c("activity","total", "men", "women")
  
    # add in the year
    year <- rep(i, length(tus$activity))
    tus$year <- year
    
    # combine with t
    t <- rbind(t, tus)
  }
  
  # some more cleanup
  
  t$men <- round(as.numeric(t$men),2) # we're rounding because some years are rounded in the data
  t$women <- round(as.numeric(t$women),2)
  t$total <- round(as.numeric(t$total),2)
  
  # get rid of the totals and blank rows
  t <- subset(t, activity != "Total, all activities" & is.na(activity)==F)
  
  # some of these activities are subsets of the larger group (i.e., housework activities is a broader category which 
  # includes housework, lawn care, etc.). Those distinctions are indicated by indenting so we want to count the 
  # whitespace in front of an activity.
  
  trim.leading <- function (x)  sub("^\\s+", "", x)
  count.leading.whitespaces <- function (x) nchar(x) - nchar(trim.leading(x))
  
  t$leading.whitespace <- NA
  t$leading.whitespace <- count.leading.whitespaces(t$activity)
  
  # now we need to create the necessary columns that indicate different categories
  # how many indentation levels
  table(t$leading.whitespace)
  # so there are four levels and so four columns
  
  sc <- vector()
  for (i in seq(1:(length(t$activity)-1))) {
    sc <- c(sc, 
      ifelse(t$leading.whitespace[[i]]<t$leading.whitespace[[i+1]], "Yes", "No"))
  }
  # I should probably fix the programming but here is a quick solution to the index out of bounds problem
  sc <- c(sc,"No")
  
  t$sub.coming <- sc
    
  # so now we can just consider the elements that don't have a subcategory immediately following (sub.coming=="No")
  
  # any nas left should be 0s
  #   t$hours.total[is.na(t$hours.total)] <- 0
  #   t$hours.men[is.na(t$hours.men)] <- 0
  #   t$hours.women[is.na(t$hours.women)] <- 0
  
  t <- na.omit(t)
  # now we need to make sure that we are looking at the same activities in each year and get rid of unncessary rows
  t <- t %>%
    group_by(activity) %>%
    summarize(
      min.year = min(year),
      max.year = max(year),
      freq = n()
    ) %>%
    filter(
      min.year == "2004",
      max.year == "2015",
      freq == 12
    ) %>%
    inner_join(t) %>%
    filter(
      sub.coming=="No"
      
      ) %>%
    select(
      activity,
      year,
      total,
      women,
      men
    )
  
  # test 
  t %>%
    group_by(year) %>%
    summarize(
      n=n(),
      men = sum(men),
      women = sum(women),
      total = sum(total)
    )
  
  ###### pivot out the data  #################
  
  tl <- melt(t, value.name = "hours")
  table(tl$variable)
  
  # getting tripped up with factors, switch to character
  tl$variable <- as.character(tl$variable)
  
  # now we want to add in some reference points
  ##### 2005 same gender
  
  tl <- tl %>%
    filter(year=="2005") %>%
    right_join(tl, by=c("activity"="activity", "variable" ="variable")) %>%
    select(
      activity,
      year.y,
      variable,
      hours.y,
      hours.x
    )
  names(tl) <- c("activity","year","variable","hours","hours.2005.same")
  
  ##### Now we should add in the 2005 numbers for the oppisite gender
  
  tlo <- subset(tl, year == "2005")
  # swtich genders
  tlo <- tlo %>%
    mutate(variable = ifelse(variable=="men","women",
            ifelse(variable=="women","men",variable)))
           
  # test (run it after each step to see if it switches)
  tlo %>%
    group_by(variable)%>%
    summarise(
      sum(hours)
    )
  
  # join back to tl
  tl <- tl %>%
    left_join(tlo, by=c("activity"="activity", "variable" ="variable")) %>%
    select(
      activity,
      year.x,
      variable,
      hours.x,
      hours.2005.same.x,
      hours.y
    )
  names(tl) <- c("activity","year","variable","hours","hours.2005.same","hours.2005.opp")
  
  #housecleaning
    rm(tlo)
    rm(t)
    rm(tus)
  # so we have most of the hours
  
  #  Remove the "other" category - it looks to have drastic changes which might reflect different categories
  
  tl <- subset(tl, activity != "Other activities, not elsewhere classified")
  
  # add some other metrics to look at
  
  tl$diff <- tl$hours - tl$hours.2005.opp
  tl$diff.squared <- tl$diff^2
  
  # now we can add it back in
  r <- tl %>%
    group_by(year, variable) %>%
    summarize(
      n= n(),
      hours = sum(hours),
      Edist = sqrt(sum(diff.squared)),
      sim = (1 /(1+Edist))
    ) %>%
    ungroup()
  
  
  r <- r %>%
    filter(variable=="total") %>%
    select(year, hours)%>%
    right_join(r, by=c("year"="year")) %>%
    filter(variable!="total")
  
  names(r) <- c("year","hours.total","variable", "n", "hours", "Edist", "Sim")
  
  # write r to a folder with a generated name
  file.name = paste("Gender Analysis Results ", j[[1]], ".csv",sep="")
  
  setwd("D:/Google Drive/Wittenberg/ANLT 550")
  file.remove(file.name)
  write.csv(r,file.name, row.names=F)
  

} 
# End of Broader Loop

#################################################  Read the tables back in ######################
a <- read.csv("Gender Analysis Results Overall.csv")
a$pop.group <- "Overall"
b <- read.csv("Gender Analysis Results Participation Rates.csv")
b$pop.group <- "Participation Rates"
c <- read.csv("Gender Analysis Results Of Participants.csv")
c$pop.group <- "Of Participants"


d <- rbind(a, b, c)
#rm(a)
#rm(b)
#rm(c)

d$adj_Edist <- (d$Edist/d$hours.total)*24

write.csv(d,"Gender Analysis Combined Results.csv", row.names =F)


##################################################  Viz ##########################################

#### Is the difference between men and women shrinking?

dw <- dcast(d,variable + pop.group ~  year , value.var = "Sim")
names(dw) <- c("gender", "pop.group", "dist.2004","dist.2005","dist.2006","dist.2007","dist.2008",
      "dist.2009","dist.2010","dist.2011","dist.2012","dist.2013","dist.2014","dist.2015")

dw$change <- (dw$dist.2015 - dw$dist.2005) / dw$dist.2005


ggplot(data = dw, aes(x=paste(pop.group, gender), y=change)) +
  geom_bar(stat="identity") +
  coord_flip()

write.csv(dw,"Gender Analysis Combined Results Wide Similarity.csv", row.names =F)

# now I want gender as columns and pop.groups as rows



# Bar Chart is not as clear
#  ggplot(r, aes(x=year, y=dif, fill=variable)) + 
#    geom_bar(stat="identity", position ="dodge")

ggplot(data=subset(d,variable=="men"), aes(x=year, y=(Edist / hours), group= pop.group)) + 
  geom_line(aes(color=pop.group),linetype = "dotdash") +
  geom_line(data=subset(d,variable=="women"), aes(x=year, y=(Edist / hours.total), group= pop.group, color=pop.group))


# now let's get a slope chart
d_slope <- subset(d, year %in% c("2005","2015"))

ggplot(data=subset(d_slope,variable=="men"), aes(x=year, y=sim, group= pop.group)) + 
  geom_line(aes(color=pop.group),linetype = "dashed", size=2) +
  geom_line(data=subset(d_slope,variable=="women"), aes(x=year, y=sim,
                                                        group= pop.group, color=pop.group), size= 2)


#########################################



