library(tidyverse)
library(ggthemes)
library(cluster)
library(factoextra)
library(plm)


####upload performance data for teams
A <- readr::read_csv("/Users/joeysoeder/Desktop/FUQUAMQM/FALL1/EMPIRICALECON/TeamProject/understat_per_game.csv")

### remove russian league games from dataset
A <- A %>% 
  filter(league !="RFPL")

###change class of date to date for ease
A$date <- as.Date(A$date)

### create new variable for season
A <- A %>% 
  mutate(season = case_when(date > '2014-07-01' & date < '2015-07-01'~ "14-15",
                            date > '2015-07-01' & date < '2016-07-01'~ "15-16",
                            date > '2016-07-01' & date < '2017-07-01'~ "16-17",
                            date > '2017-07-01' & date < '2018-07-01'~ "17-18",
                            date > '2018-07-01' & date < '2019-07-01'~ "18-19",
                            date > '2019-07-01' ~ "19-20"))

###change class of season to factor for grouping
A$season <- as.factor(A$season)
#  set counter for cumsum for matchday
A$Matchday = 1
### create new variable matchday        
 A <- A %>% 
  group_by(team, season) %>% 
 arrange(team, season, date) %>% 
  mutate(Matchday = cumsum(Matchday))  


### read in coaching changes data
B <- readr::read_csv(("/Users/joeysoeder/Desktop/FUQUAMQM/FALL1/EMPIRICALECON/TeamProject/SoccerCoachingChanges1.csv"))


 ####change classes of columns to be appropriate
 B$Matchday <- as.numeric(B$Matchday)
 B$Rankpre <- as.numeric(B$Rankpre)
 B$rankfinal <- as.numeric(B$rankfinal)
 B$preppg <- as.numeric(B$preppg)
 B$postppg <- as.numeric(B$postppg)
 B$leaving_date <- as.Date(B$leaving_date, '%d-%b-%y')
 
 
 
 #####Create new variable season
 C <- B %>% 
   mutate(season = case_when(leaving_date > '2014-07-01' & leaving_date < '2015-07-01'~ "14-15",
                             leaving_date > '2015-07-01' & leaving_date < '2016-07-01'~ "15-16",
                             leaving_date > '2016-07-01' & leaving_date < '2017-07-01'~ "16-17",
                             leaving_date > '2017-07-01' & leaving_date < '2018-07-01'~ "17-18",
                             leaving_date > '2018-07-01' & leaving_date < '2019-07-01'~ "18-19",
                             leaving_date > '2019-07-01' ~ "19-20"))
 
 ###filter out tenure less than 14 matchday greater
 C <- C %>% 
   filter(tenure > 14)
 

 # filter out games that dont give 10 game window - not using
 # C10 <- C %>% 
 #   filter(Matchday < 29,
 #          Matchday > 9)
 
 # filter out games that dont give 5 game window
 C5 <-  C %>% 
    filter(Matchday < 34,
           Matchday > 4)
 ### creating extra save of C5 due to splitting here and conducting diff analysis w code
 C5i <- C5
 
 
 C5 <- C5 %>% 
   select(Club, Matchday, season)
 
 # not using
 # C10 <- C10 %>% 
 #    select(Club, Matchday, season)
 
 
#create new data frame with only the information needed
D <- A %>% 
  select(Matchday, league, team, season, xG, xGA)


# mutate dataframe to define target variable xGd
D <- D %>% 
   mutate(xGd = xG - xGA) %>% 
   select(Matchday, league, team, season, xGd)


#rename team variable for even join
D$Club <- D$team

#inner join D and C5
df <- inner_join(D, C5, by = 'Club')  

#create new variable to see if coach changed during season
df <- df %>% 
   mutate(change = ifelse(season.x == season.y, 1, 0))

#take all season data in which coach changed
df1 <- df %>% 
   filter(change == 1)

# take all data from seasons where coaches didnt change - data super messy everything is triplicated
df0 <- df %>% 
   filter(change == 0)

#drop variables that cause triplicated records to be unique
df0 <- df0 %>% 
   select(-Matchday.y, - season.y, -team)


#drop duplicates
df0 <- df0[!duplicated(df0),]


########################################################################################################
########################################################################################################

### calculate avg exp. goal difference for each team for each season
df0 <- df0 %>% 
   group_by(team, season.x) %>% 
   mutate(avgxGd = mean(xGd))   

### drop variables that make duplicates unique and remove duplicates
df0 <- df0 %>% 
   select(-xGd, -Matchday.x)

 df0 <- df0[!duplicated(df0),]

# ########################################################################################################
# ########################################################################################################
# graph a boxplot of avgxgd
 ggplot(data = df0)+
    geom_boxplot(mapping = aes(x = avgxGd))+
    theme_economist()
# frequency poly of avgxgd
 ggplot(data = df0)+
    geom_freqpoly(mapping = aes(x = avgxGd))+
    theme_economist()

 ## histogram showing firings by matchday
 hist(C5$Matchday, main="Firings by Matchday", xlab="Matchday", col="blue")
 axis(1, tck=-0.015, col.ticks="black")
 axis(2, tck=1, col.ticks="light gray", lwd.ticks="1")
 axis(2, tck=-0.015)
 minor.tick(nx=5, ny=2, tick.ratio=0.5)
 box()

 
 #### ggplot showing change in ranks pre and post firing
 ggplot(C, aes(x=Rankpre, y=rankfinal)) + geom_point() + geom_smooth() +   xlab("Rank Pre-Firing") +
   ylab("Final Rank") + ggtitle(" Pre-Firing v/s Final Ranks")
########################################################################################################
########################################################################################################

 ## create a new dataset for a visual, grouping by club, whether or not they changed coaches, and the season
 dfvis <- df %>% 
   select(-Matchday.y, -season.y)
 dfvis <- dfvis[!duplicated(dfvis),]

 
 dfvis <- dfvis %>% 
   group_by(Club, change, season.x) %>% 
   mutate(AvgxGd = mean(xGd)) 
 
 ## remove unneeded variables and duplicates
dfvis <- select(dfvis, -xGd, -Matchday.x)
 dfvis <- dfvis[!duplicated(dfvis),]

## get rid of na values
dfvis <- na.omit(dfvis)

## create freqpoly comparing change and not change distribution of points 
ggplot(data = dfvis, mapping = aes(x = AvgxGd)) +
  geom_freqpoly() + 
  facet_wrap(~ change)+
  theme_economist()

########################################################################################################
########################################################################################################
 # create variable change that is 1 if the game is before the day of the coach getting sacked and 0 otherwise
df1.1 <- df1 %>% 
   mutate(change = ifelse (Matchday.x > Matchday.y, 1, 0))

#create avgxgd
df1.1 <- df1.1 %>% 
  group_by(Club, season.x, change) %>% 
  mutate(avgxGd = mean(xGd))

##drop variables and duplicates
df1.1 <- select(df1.1, -Matchday.x, -xGd)
df1.1 <- df1.1[!duplicated(df1.1),]

### add treated variable for diffindiff
df1.2 <- df1.1 %>% 
  select(-Matchday.y, -season.y) %>% 
  mutate(treated = 1)

##add treated variable for diffindiff
df0 <- df0 %>% 
  mutate(treated = 0)

### create new column for visual
df0.1 <- df0 %>% 
  mutate(change = 1)

## rbind treated and untreated to make final df used in analysis
final <- rbind(df0, df1.2)
final <- rbind(final, df0.1)

########################################################################################################
########################################################################################################

### plots for did
ggplot(final, aes(x = factor(treated), y = avgxGd)) +
  geom_point(size = 0.5, alpha = 0.2) +
  stat_summary(geom = "point", fun.y = "mean", size = 5, color = "red") +
  facet_wrap(~ change)+
  theme_economist()

ggplot(final, aes(x = factor(treated), y = avgxGd)) +
  #geom_point(size = 0.5, alpha = 0.2)+
  stat_summary(geom = "pointrange", size = 1, color = "red",
               fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  facet_wrap(~ change)+
  theme_economist()

### create plott data for final vis
plot_data <- final %>% 
  mutate(change = factor(change),
         treated = factor(treated)) %>% 
  group_by(change, treated) %>% 
  summarize(mean_xGd = mean(avgxGd),
            se_xGd = sd(avgxGd) / sqrt(n()),
            upper = mean_xGd + (-1.96 * se_xGd),
            lower = mean_xGd + (1.96 * se_xGd)) 


###final vis plots
ggplot(plot_data, aes(x = treated, y = mean_xGd)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  color = "darkblue", size = 1, ) +
  facet_wrap(~ change)+
  theme_economist()

### with line

ggplot(plot_data, aes(x = change, y = mean_xGd, color = treated)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) + 
  geom_line(aes(group = treated))+
  theme_economist()

########################################################################################################
########################################################################################################


# create Pooled OLS model
model <- lm(avgxGd ~ treated + change + (change*treated), data = final)
summary(model)

#######################################RESULTS##########################################
# Call:
#   lm(formula = avgxGd ~ treated + change + (change * treated), 
#      data = final)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.27799 -0.34888 -0.07629  0.25136  2.31704 
# 
# Coefficients:
#               Estimate Std.  Error  t value  Pr(>|t|)    
# (Intercept)     3.848e-02  2.646e-02   1.454    0.146    
# treated        -4.111e-01  4.384e-02  -9.379   <2e-16 ***
# change          9.852e-16  3.742e-02   0.000    1.000    
# treated:change  1.331e-01  6.199e-02   2.147    0.032 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5312 on 1264 degrees of freedom
# Multiple R-squared:  0.0938,	Adjusted R-squared:  0.09165 
# F-statistic: 43.61 on 3 and 1264 DF,  p-value: < 2.2e-16
#######################################RESULTS##########################################
########################################################################################

df2 <- left_join(D, C5i, by = c('Club','season'))
names(df2)[1] <- "Matchday"
names(df2)[8] <- "SackCoachMD"

#drop duplicated rows
df3 <- df2[!duplicated(df2[,c(1,3,4)]), ]

#create Treatment and Post dummy variable
df3$Treatment <- ifelse(is.na(df3$SackCoachMD),0,1)
df3$Post <- ifelse(df3$Matchday>df3$SackCoachMD & df3$Treatment == 1,1,0)
df3$Time <- paste(df3$season,df3$Matchday,sep = " ")

# Run LSDV regression
LSDV_lm <- lm(xGd ~ Treatment + factor(team) + factor(season) + factor(Matchday), data=df3)
summary(LSDV_lm)

#######################################RESULTS##########################################
########################################################################################
# Call:
#   lm(formula = xGd ~ Treatment + factor(team) + factor(season) + 
#        factor(Matchday), data = df3)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.8537 -0.7560  0.0244  0.7812  5.6522 
# 
# Coefficients:
#                                      Estimate  Std. Error t value Pr(>|t|)    
# (Intercept)                          0.3773904  0.0973921   3.875 0.000107 ***
#   Treatment                           -0.2294769  0.0215016 -10.673  < 2e-16 ***
# ........many many lines......
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.222 on 21514 degrees of freedom
# Multiple R-squared:  0.1631,	Adjusted R-squared:  0.1559 
# F-statistic: 22.66 on 185 and 21514 DF,  p-value: < 2.2e-16
#######################################RESULTS##########################################
########################################################################################

# # Run Fixed Effect panel regression
# pooling_model <- plm(xGd~Treatment,data=df3, index = c("Time","team"), model="within",effect="twoways")
# summary(pooling_model) 

#######################################RESULTS##########################################
########################################################################################
# Twoways effects Within Model
# 
# Call:
#   plm(formula = xGd ~ Treatment, data = df3, effect = "twoways", 
#       model = "within", index = c("Time", "team"))
# 
# Unbalanced Panel: n = 228, T = 50-98, N = 21700
# 
# Residuals:
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -5.863280 -0.757807  0.022936  0.778059  5.730295 
# 
# Coefficients:
#   Estimate Std. Error t-value  Pr(>|t|)    
# Treatment -0.229673   0.021591 -10.638 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    32248
# Residual Sum of Squares: 32077
# R-Squared:      0.0052774
# Adj. R-Squared: -0.011978
# F-statistic: 113.158 on 1 and 21329 DF, p-value: < 2.22e-16
#######################################RESULTS##########################################
########################################################################################

# Relaxing common trend assumptioin
LSDV_rlm <- lm(xGd ~ Treatment + factor(Treatment*Matchday) + factor(team) + factor(season) + factor(Matchday), data=df3)
summary(LSDV_rlm)




# # Cross-checking EDA, not relevant to panel regression
# mean(df3$xGd[df3$Treatment == 1]) 
# mean(df3$xGd[df3$Treatment == 0]) 
