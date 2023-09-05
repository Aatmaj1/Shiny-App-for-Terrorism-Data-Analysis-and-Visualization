library(dplyr)
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)
library(leaflet)
library(shinyalert)
library(threejs)
library(caret)
library (klaR)
library(shinycssloaders)
df <- read.csv( "globalterrorismdb_0718dist.csv" )
df <- df %>% mutate( decade = 
                       ifelse( year < 1980, '70s', 
                               ifelse( year < 1990, '80s', 
                                       ifelse( year < 2000, '90s', 
                                               ifelse( year < 2010, '2000s', '2010s')))))
df$decade <- factor( df$decade, levels = c( "70s", "80s", "90s", "2000s", "2010s" ))
df500 <- df %>% dplyr::select( decade, latitude, longitude, gname ) %>% group_by( decade ) %>%
  slice( 1:10000 )

df500 <- df500 %>% group_by( gname ) %>% filter( n() >= 300 & gname != "Unknown" )

df500 <- df500[complete.cases( df500 ), ]

fullpop <- read.csv( "C:/Users/JanardaA/Desktop/cluster/UNpopfile.csv" )

top10 <- df %>% filter( df$gname %in% c( "Taliban", "Shining Path (SL)", "Islamic State of Iraq and the Levant (ISIL)", 
                                         "Farabundo Marti National Liberation Front (FMLN)", "Al-Shabaab", 
                                         "Irish Republican Army (IRA)", "Revolutionary Armed Forces of Colombia (FARC)", 
                                         "New People's Army (NPA)", "Kurdistan Workers' Party (PKK)", "Boko Haram" )) %>%  
  dplyr::select( year, gname ) %>% group_by( year, gname ) %>% summarise( nr_of_attacks = n() )%>%
  arrange( desc( nr_of_attacks )) %>% top_n( n=10, wt=nr_of_attacks )
top10 <- top10[complete.cases(top10), ]
pop <- fullpop %>% dplyr::select( -MidPeriod, -PopMale, -PopFemale, -VarID )
pop <- pop %>% filter( year > 1969 & Variant == 'Medium' & year < 2017 ) %>%
  dplyr::select( -Variant, -LocID )
futurepop <- fullpop %>% filter( year >2016 & Variant == 'Medium' ) %>% 
  dplyr::select( -Variant, -LocID, -MidPeriod, -PopMale, -PopFemale, -VarID )
popworld <- pop %>% filter( Location == "World" ) %>% dplyr::select( -Location )
df2 <- inner_join( df, popworld,by= c( "year" = "year" ) )
gra1<-df %>% dplyr::select( year ) %>% group_by( year ) %>% summarise( tr=n() ) 
a <- df2 %>% dplyr::select( year,PopTotal,gname ) %>% group_by( year,PopTotal,gname ) %>% 
  summarise( ta=n() )
b<-df %>% filter( nkill != 'Unknown',nkill !=0 ) %>% dplyr::select( year,nkill ) %>% 
  group_by( year ) %>% summarise( c = sum( nkill ) )
df3 <- df2 %>% group_by( year ) %>% summarise( terrorist_attacks_count = n() )
df3 <- inner_join( df3, popworld, by = c("year" = "year") )
df3 <- df3 %>% mutate( decade = ifelse( year<1980, '70s', ifelse( year < 1990, '80s', 
                                                                  ifelse( year < 2000, '90s', ifelse( year < 2010, '2000s', '2010s')))))
df3$decade <- factor( df3$decade, levels=c( "70s", "80s", "90s", "2000s", "2010s" ) )
m1 <- lm( data=df3, terrorist_attacks_count ~ PopTotal )
m2 <- lm( data=df3, terrorist_attacks_count ~ PopTotal + I(PopTotal^2) )
m3 <- lm( data=df3, terrorist_attacks_count ~ PopTotal + I(PopTotal^2) + I(PopTotal^3) )
futurepopworld <- futurepop %>% filter( Location == "World" ) %>% dplyr::select( -Location )
futurepopworld$terrorist_attacks_count <- NA
fpopworld1 <- futurepopworld
fpopworld2 <- futurepopworld
fpopworld3 <- futurepopworld
fpopworld1$terrorist_attacks_count <- predict( object = m1, newdata = fpopworld1 )

fpopworld2$terrorist_attacks_count <- predict( object = m2, newdata = fpopworld2 )

fpopworld3$terrorist_attacks_count <- predict( object = m3, newdata = fpopworld3)

#filter until 2040
fpopworld1 <- filter( fpopworld1, year < 2041 )
fpopworld2 <- filter( fpopworld2, year < 2041 )
fpopworld3 <- filter(fpopworld3, year < 2041 )
isi <- df %>% filter( year == 2014 ) %>% dplyr::select( targtype1 ) %>% group_by( targtype1 ) %>% 
  summarise( targcount = n() )
isimap <- df %>% filter( year == 2014 ) %>% dplyr::select( latitude,longitude,gname )
#--------cluster---------------------------------
# gl <- df[ ,c(2,9,11,30,36,42,59,85,100)]
# gl <- gl %>% filter(df$gname != "Unknown")
# gf <- subset(gl, year >= 2014)
# clusters <- kmodes (gf, 12, iter.max = 10, weighted = FALSE, fast = TRUE)
# clusterOutput <- cbind(gf, clusters$cluster)
# arr<-c(1:12)
# out<-cbind(arr,clusters$modes)
#-------globe-------------------
glob <- df %>% dplyr::select(latitude,longitude,gname)
glob <- glob %>% dplyr::group_by(latitude,longitude,gname) %>% dplyr::summarise(total=n()) %>% slice(1:10000)
glo  <- subset(glob, select = -c(gname))
#------------ML-----------
region.a <- df %>% dplyr::select(region)
region.a <- unique(region.a)
country.a <- df %>% dplyr::select(country)
country.a <- unique(country.a)
attack.a <- df %>% dplyr::select(attacktype1) 
attack.a <- unique(attack.a)
dtree <- df %>% dplyr::select('region', 'country', 'attacktype1', 'targtype1')
dtree[complete.cases(dtree), ]
inTrain <- createDataPartition(dtree$targtype1, p=0.7, list=FALSE)
training <- dtree[inTrain, ]
testing <- dtree[-inTrain, ]
#fancyRpartPlot(modFit$finalModel)
modFit<-train(targtype1 ~region+country+attacktype1, method="rpart", data=training)
