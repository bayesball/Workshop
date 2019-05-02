#Install packages

library(Lahman)
library(dplyr)
library(ggplot2)


#Select------------------------------------------------------------------------------

bdat <- Batting %>% select(playerID,yearID,teamID,HR)

#Filter------------------------------------------------------------------------------

bdat <- Batting %>%
          filter(HR >= 50) %>%
          select(playerID,yearID,teamID,HR)


#Arrange-----------------------------------------------------------------------------

bdat <- Batting %>%
          filter(HR >= 50) %>%
          select(playerID,yearID,teamID,HR) %>%
          arrange(HR)


bdat <- Batting %>%
          filter(HR >= 50) %>%
          select(playerID,yearID,teamID,HR) %>%
          arrange(desc(HR))

#Grouping-----------------------------------------------------------------------------

bdat <- Batting %>%
          filter(playerID == "ruthba01") %>%
          group_by(playerID) %>%
          summarize(careerHR = sum(HR))

#Players with 500 or more career homeruns---------------------------------------------

bdat <- Batting %>%
          group_by(playerID) %>%
          summarize(careerHR = sum(HR)) %>%
          filter(careerHR >= 500) %>%
          arrange(desc(careerHR))

#Inner Join---------------------------------------------------------------------------

HR_df <- Batting %>%
          filter(HR >= 50) %>%
          select(playerID,yearID,teamID,HR)

playerNames_df <- Master %>%
              select(playerID,nameFirst,nameLast)

result <- inner_join(playerNames_df,HR_df,by = c("playerID"="playerID")) %>%
        select(nameFirst,nameLast,yearID,teamID,HR) %>%
        arrange(desc(HR))

teamNames_df <- Teams %>%
                  select(teamID,yearID,name)


result2 <- inner_join(teamNames_df,result,by=c("teamID"="teamID","yearID"="yearID")) %>%
      select(nameFirst,nameLast,name,yearID,HR) %>%
      arrange(desc(HR))


#Players with 500 or more career homeruns (with names)----------------------------------

HR_df <- Batting %>%
          group_by(playerID) %>%
          summarize(careerHR = sum(HR)) %>%
          filter(careerHR >= 500)
          
playerNames_df <- Master %>%
              select(playerID,nameFirst,nameLast)

result <- inner_join(playerNames_df,HR_df,by=c("playerID"="playerID")) %>%
          select(nameFirst,nameLast,careerHR) %>%
          arrange(desc(careerHR))


#ggplot--------------------------------------------------------------------------------

ruth<-Batting %>%
        filter(playerID == "ruthba01") %>%
        select(yearID,HR)

ggplot() +
  geom_line(data=ruth,aes(x=yearID,y=HR),color="blue") +
  geom_point(data=ruth,aes(x=yearID,y=HR),color="blue",fill="orange",size=3,shape=21) +
  theme_minimal()

#ggplot grouping-----------------------------------------------------------------------

ruthGehrig<-Batting %>%
        filter(playerID == "ruthba01" | playerID == "gehrilo01") %>%
        select(playerID,yearID,HR)

ggplot() +
  geom_line(data = ruthGehrig,aes(x=yearID,y=HR,group=playerID,color=playerID)) +
  geom_point(data = ruthGehrig,aes(x=yearID,y=HR,group=playerID,color=playerID)) +
  ggtitle("Ruth and Gehrig HR Totals") +
  xlab("Year") +
  ylab("Season HR Totals") +
  scale_color_manual(values=c("#003f5c","#bc5090")) +
  theme_minimal()

#facets------------------------------------------------------------------------------

hitters<-Batting %>%
  group_by(playerID) %>%
  summarize(careerHR = sum(HR)) %>%
  top_n(10)

HR_df <-Batting %>%
  select(playerID,yearID,HR)

topHitters <- inner_join(hitters,HR_df,by=c("playerID")) %>%
                  select(playerID,yearID,HR)

ggplot() +
  geom_line(data=topHitters,aes(x=yearID,y=HR,group=playerID,color=playerID)) +
  facet_grid(playerID~.) +
  theme_minimal()

ggplot() +
  geom_line(data=topHitters,aes(x=yearID,y=HR,group=playerID,color=playerID)) +
  facet_wrap(~playerID) +
  theme_minimal()

#barplots--------------------------------------------------------------------------

phils<-Batting %>%
        filter(yearID >= 1970 & yearID <= 1979) %>%
        filter(teamID == "PHI") %>%
        group_by(playerID) %>%
        summarize(decadeHR = sum(HR),decadeAB = sum(AB)) %>%
        filter(decadeAB >= 1500) %>%
        arrange(desc(decadeHR))

phils$playerID <- factor(phils$playerID,levels=phils$playerID)

ggplot() +
  geom_bar(data=phils,aes(x=playerID,y=decadeHR),stat=("identity"),color="red",
           fill="lightskyblue") +
  theme_minimal() +
  ggtitle("Phils Homeruns '70's") +
  xlab("Player") +
  ylab("Homerun Total") +
  coord_flip()

#Putting all together-----------------------------------------------------------

phils<-Batting %>%
        filter(yearID >= 1970 & yearID <= 1979) %>%
        filter(teamID == "PHI") %>%
        group_by(playerID) %>%
        summarize(decadeHR = sum(HR),decadeAB = sum(AB)) %>%
        mutate(playerTeam = paste(playerID,"PHI",sep=""),team="phillies") %>%
        select(playerTeam,decadeHR,decadeAB,team) %>%
        filter(decadeAB >= 1500)

pirates<-Batting %>%
        filter(yearID >= 1970 & yearID <= 1979) %>%
        filter(teamID == "PIT") %>%
        group_by(playerID) %>%
        summarize(decadeHR = sum(HR),decadeAB = sum(AB)) %>%
        mutate(playerTeam = paste(playerID,"PIT",sep=""),team="pirates") %>%
        select(playerTeam,decadeHR,decadeAB,team) %>%
        filter(decadeAB >= 1500) 

philsPirates <- rbind(phils,pirates) %>%
                  arrange(decadeHR)

philsPirates$playerTeam <- factor(philsPirates$playerTeam,levels=philsPirates$playerTeam)

ggplot()+
  geom_bar(data=philsPirates,aes(x=playerTeam,y=decadeHR,color=team,fill=team),
           stat=("identity")) +
  scale_color_manual(values=c("red","black")) +
  scale_fill_manual(values=c("lightskyblue","gold")) +
  theme_minimal() +
  coord_flip()

ggplot()+
  geom_bar(data=philsPirates,aes(x=playerTeam,y=decadeHR,color=team,fill=team),
           stat=("identity")) +
  scale_color_manual(values=c("red","black")) +
  scale_fill_manual(values=c("lightskyblue","gold")) +
  theme_minimal() +
  coord_flip() +
  facet_grid(.~team)

