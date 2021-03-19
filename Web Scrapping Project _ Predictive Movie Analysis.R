##########################################################################################################################################################################
# Luis Treviño                                                                                                                                                            #
# Professor Matthew Martinez                                                                                                                                              #
# R - Programming Web Scrapping Project                                                                                                                                   #
# March 19, 2021                                                                                                                                                          #
##########################################################################################################################################################################


#Bring In Libraries#----
library(rvest)
library(plyr)
library(dplyr)
library(ggplot2)
library(devtools)
#############################   ROTTEN TOMATOES   ###############################----

#Website to Scrape for WORST MOVIES
URL_worst <- html("https://editorial.rottentomatoes.com/guide/worst-movies-of-all-time/")

#Website to Scrape for BEST MOVIES
URL_best <- html("https://editorial.rottentomatoes.com/guide/100-club-certified-fresh-movies/")


#Attributes imported as Lists
#Movie Title
listWT <- list(URL_worst%>%
                 html_nodes(".article_movie_title a") %>%
                 html_text())
listBT <- list(URL_best%>%
                 html_nodes(".article_movie_title a") %>%
                 html_text())
#Release Year
listWY <- list(URL_worst%>%
                 html_nodes(".start-year") %>%
                 html_text())
listBY <- list(URL_best%>%
                 html_nodes(".start-year") %>%
                 html_text())

# Critic Scores
listWS <- list(URL_worst%>%
                 html_nodes(".tMeterScore") %>%
                 html_text())
listBS <- list(URL_best%>%
                 html_nodes(".tMeterScore") %>%
                 html_text())

# Director
WD <- URL_worst%>%
                 html_nodes(".director a:nth-child(2)") %>%
                 html_text()

WD2 <- append(WD, "Stewart Hendler")

WD2 <- WD2[c(1:66, 100, 67:99)]

listWD <- list(WD2)

listBD <- list(URL_best%>%
                 html_nodes(".director a:nth-child(2)") %>%
                 html_text())

#Main Stars (Actors)
listWA1 <- list(URL_worst%>%
                  html_nodes(".cast a:nth-child(2)") %>%
                  html_text())
listWA2 <- list(URL_worst%>%
                  html_nodes(".cast a:nth-child(3)") %>%
                  html_text())
listWA3 <- list(URL_worst%>%
                  html_nodes(".cast a:nth-child(4)") %>%
                  html_text())
listWA4 <- list(URL_worst%>%
                  html_nodes(".cast a:nth-child(5)") %>%
                  html_text())

listBA1 <- list(URL_best%>%
                  html_nodes(".cast a:nth-child(2)") %>%
                  html_text())
listBA2 <- list(URL_best%>%
                  html_nodes(".cast a:nth-child(3)") %>%
                  html_text())
listBA3 <- list(URL_best%>%
                  html_nodes(".cast a:nth-child(4)") %>%
                  html_text())
listBA4 <- list(URL_best%>%
                  html_nodes(".cast a:nth-child(5)") %>%
                  html_text())


#Data Frame of WORST MOVIES
df_wrt <- do.call(rbind, Map(data.frame, Title=listWT, Year=listWY, Score=listWS, Director=listWD, Actor1=listWA1, Actor2=listWA2, Actor3=listWA3, Actor4=listWA4))


#*NOTE: Number of elements between lists for BEST MOVIES differ;
lengths(listBT)
lengths(listBA1)
#Therefore edits are made in order to be able to combine lists in Data Frame:

#1 Original lists turned into individual data frames
df_BT <- do.call(rbind, Map(data.frame, Titles=listBT))
df_BY <- do.call(rbind, Map(data.frame, Year=listBY))
df_BS <- do.call(rbind, Map(data.frame, Score=listBS))
df_BD <- do.call(rbind, Map(data.frame, Director=listBD))

#2 Rows in data frames reordered so that 3 blank entries are now at the end
df_BT <- df_BT$Titles[c(1:15, 17:23, 25:69, 16, 24, 70)]
df_BY <- df_BY$Year[c(1:15, 17:23, 25:69, 16, 24, 70)]
df_BS <- df_BS$Score[c(1:15, 17:23, 25:69, 16, 24, 70)]
df_BD <- df_BD$Director[c(1:15, 17:23, 25:69, 16, 24, 70)]

#3 Individual Data Frames converted back into Lists
listBT_new <- list(df_BT)
listBY_new <- list(df_BY)
listBS_new <- list(df_BS)
listBD_new <- list(df_BD)

#New data frame with new (ordered) lists

df_bstA <- do.call(rbind, Map(data.frame, Title=listBT_new, Year=listBY_new, Score=listBS_new, Director=listBD_new))

# Data frame of Actor Lists

df_bstB <- do.call(rbind, Map(data.frame, Actor1=listBA1, Actor2=listBA2, Actor3=listBA3, Actor4=listBA4))

# Merged and final Data Frame for BEST MOVIES

df_bst <- merge(data.frame(df_bstA, row.names=NULL), data.frame(df_bstB, row.names=NULL), 
                by = 0, all = TRUE)[-1]

####################################################################################################

####################################################################################################

##################################   IMDB   ##################################----

#Websites to scrape - WORST MOVIES

URL_bottom <- html("https://www.imdb.com/search/title/?groups=bottom_250&lists=!watchlist&sort=user_rating,asc")
URL_bottom151 <- html("https://www.imdb.com/search/title/?groups=bottom_250&sort=user_rating,asc&start=151&ref_=adv_nxt")

#Websites to scrape - BEST MOVIES 

URL_top <- html("https://www.imdb.com/search/title/?groups=top_250&sort=user_rating")
URL_top2 <- html("https://www.imdb.com/search/title/?groups=top_250&sort=user_rating,desc&start=101&ref_=adv_nxt")


#Attributes imported as Lists

#Movie Title
list01M <- list(URL_bottom%>%
                  html_nodes(".lister-item-header a") %>%
                  html_text())
list04M <- list(URL_bottom151%>%
                  html_nodes(".lister-item-header a") %>%
                  html_text())

list02M <- list(URL_top%>%
                  html_nodes(".lister-item-header a") %>%
                  html_text())
list03M <- list(URL_top2%>%
                  html_nodes(".lister-item-header a") %>%
                  html_text())


#Release Year
list01Y <- list(URL_bottom%>%
                  html_nodes(".text-muted.unbold") %>%
                  html_text())
list04Y <- list(URL_bottom151%>%
                  html_nodes(".text-muted.unbold") %>%
                  html_text())

list02Y <- list(URL_top%>%
                  html_nodes(".text-muted.unbold") %>%
                  html_text())
list03Y <- list(URL_top2%>%
                  html_nodes(".text-muted.unbold") %>%
                  html_text())

#Critic Score
list01S <- list(URL_bottom%>%
                  html_nodes(".ratings-imdb-rating strong") %>%
                  html_text())
list04S <- list(URL_bottom151%>%
                  html_nodes(".ratings-imdb-rating strong") %>%
                  html_text())

list02S <- list(URL_top%>%
                  html_nodes(".ratings-imdb-rating strong") %>%
                  html_text())
list03S <- list(URL_top2%>%
                  html_nodes(".ratings-imdb-rating strong") %>%
                  html_text())


#Runtime
list01R <- list(URL_bottom%>%
                  html_nodes(".runtime") %>%
                  html_text())
list04R <- list(URL_bottom151%>%
                  html_nodes(".runtime") %>%
                  html_text())

list02R <- list(URL_top%>%
                  html_nodes(".runtime") %>%
                  html_text())
list03R <- list(URL_top2%>%
                  html_nodes(".runtime") %>%
                  html_text())

#Votes
list01V <- list(URL_bottom%>%
                  html_nodes(".sort-num_votes-visible span:nth-child(2)") %>%
                  html_text())
list04V <- list(URL_bottom151%>%
                  html_nodes(".sort-num_votes-visible span:nth-child(2)") %>%
                  html_text())

list02V <- list(URL_top%>%
                  html_nodes(".sort-num_votes-visible span:nth-child(2)") %>%
                  html_text())
list03V <- list(URL_top2%>%
                  html_nodes(".sort-num_votes-visible span:nth-child(2)") %>%
                  html_text())

#Director
list01D <- list(URL_bottom%>%
                  html_nodes("p:nth-child(5) a:nth-child(1)") %>%
                  html_text())
list04D <- list(URL_bottom151%>%
                  html_nodes("p:nth-child(5) a:nth-child(1)") %>%
                  html_text())

list02D <- list(URL_top%>%
                  html_nodes("p:nth-child(5) a:nth-child(1)") %>%
                  html_text())
list03D <- list(URL_top2%>%
                  html_nodes("p:nth-child(5) a:nth-child(1)") %>%
                  html_text())


# Cleaning of white space and extra phrases

#Vector Assignments - Genre
G <- URL_bottom%>%
  html_nodes(".genre") %>%
  html_text()
G_4 <- URL_bottom151%>%
  html_nodes(".genre") %>%
  html_text()

H <- URL_top%>%
  html_nodes(".genre") %>%
  html_text()
H_2 <- URL_top2%>%
  html_nodes(".genre") %>%
  html_text()

#Trim function which uses regular expressions to clean white space
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)}

G1 <- trim(G)
G4 <- trim(G_4)
H1 <- trim(H)
H2 <- trim(H_2)

Actors 
#Main 1
AM1 <- URL_bottom%>%
  html_nodes(":nth-child(48) a:nth-child(4) , :nth-child(34) a:nth-child(4), :nth-child(33) a:nth-child(4), :nth-child(23) a:nth-child(4), 
             :nth-child(21) a:nth-child(4), #main :nth-child(10) a:nth-child(4), #main :nth-child(5) .lister-item-content a:nth-child(4), 
             :nth-child(50) a:nth-child(4), .lister-item-content a:nth-child(3)") %>%
  html_text()
AM1D <- URL_bottom151%>%
  html_nodes(":nth-child(48) a:nth-child(4) , :nth-child(34) a:nth-child(4), :nth-child(33) a:nth-child(4), :nth-child(23) a:nth-child(4), 
             :nth-child(21) a:nth-child(4), #main :nth-child(10) a:nth-child(4), #main :nth-child(5) .lister-item-content a:nth-child(4), 
             :nth-child(50) a:nth-child(4), .lister-item-content a:nth-child(3)") %>%
  html_text()

AM1X <- URL_top%>%
  html_nodes(":nth-child(48) a:nth-child(4) , :nth-child(34) a:nth-child(4), :nth-child(33) a:nth-child(4), :nth-child(23) a:nth-child(4), 
             :nth-child(21) a:nth-child(4), #main :nth-child(10) a:nth-child(4), #main :nth-child(5) .lister-item-content a:nth-child(4), 
             :nth-child(50) a:nth-child(4), .lister-item-content a:nth-child(3)") %>%
  html_text()
AM1Y <- URL_top2%>%
  html_nodes(":nth-child(48) a:nth-child(4) , :nth-child(34) a:nth-child(4), :nth-child(33) a:nth-child(4), :nth-child(23) a:nth-child(4), 
             :nth-child(21) a:nth-child(4), #main :nth-child(10) a:nth-child(4), #main :nth-child(5) .lister-item-content a:nth-child(4), 
             :nth-child(50) a:nth-child(4), .lister-item-content a:nth-child(3)") %>%
  html_text()

#Main2
AM2 <- URL_bottom%>%
  html_nodes("a:nth-child(4)") %>%
  html_text()
AM2D <- URL_bottom151%>%
  html_nodes("a:nth-child(4)") %>%
  html_text()

AM2X <- URL_top%>%
  html_nodes("a:nth-child(4)") %>%
  html_text()
AM2Y <- URL_top2%>%
  html_nodes("a:nth-child(4)") %>%
  html_text()

#Main3
AM3 <- URL_bottom%>%
  html_nodes(".lister-item-content a:nth-child(5)") %>%
  html_text()
AM3D <- URL_bottom151%>%
  html_nodes(".lister-item-content a:nth-child(5)") %>%
  html_text()

AM3X <- URL_top%>%
  html_nodes(".lister-item-content a:nth-child(5)") %>%
  html_text()
AM3Y <- URL_top2%>%
  html_nodes(".lister-item-content a:nth-child(5)") %>%
  html_text()

#Main4
AM4 <- URL_bottom%>%
  html_nodes("a:nth-child(6)") %>%
  html_text()
AM4D <- URL_bottom151%>%
  html_nodes("a:nth-child(6)") %>%
  html_text()

AM4X <- URL_top%>%
  html_nodes("a:nth-child(6)") %>%
  html_text()
AM4Y <- URL_top2%>%
  html_nodes("a:nth-child(6)") %>%
  html_text()

# Cleaning of extra names and unnecessary number entries
# vec_new <- vec[! vec %in% c("A", "C")]        # Removing multiple values

AM1 <- AM1[!AM1 %in% c("3", "4")]
AM1D <- AM1D[!AM1D %in% c("3", "4", "Gülsen Özbakan" ,"Bruce Willis", "Todd Lincoln", "David Morrissey", "Brandon T. Jackson")]

AM2 <- AM2[!AM2 %in% c("4", "Most Popular Movies", "Most Popular Shows", "IMDb Originals", "Golden Globes", "Celebrity News",
                "Celebs")]
AM2D <- AM2D[!AM2D %in% c("4","Most Popular Movies", "Most Popular Shows", "IMDb Originals", "Golden Globes", "Celebrity News",
                          "Celebs")]

AM3 <- AM3[!AM3 %in% c("5")]
AM3D <- AM3D[!AM3D %in% c("5")]

AM4 <- AM4[!AM4 %in% c("6","Top Box Office","TV News","IMDb Podcasts" ,"Women's History Month","Keywords")]
AM4D <- AM4D[!AM4D %in% c("6","Top Box Office","TV News","IMDb Podcasts" ,"Women's History Month","Keywords")]


AM1X <- AM1X[!AM1X %in% c("3", "4", "Lee J. Cobb","Ingrid Bergman","Charles Bronson")]
AM1X <- AM1X[-11]

AM1Y <- AM1Y[!AM1Y %in% c("3", "4","Gary Lockwood", "Alfred Abel","Bora Todorovic","Paul Dano")]
AM1Y <- AM1Y[-6]



AM2X <- AM2X [!AM2X %in% c("4","Most Popular Movies", "Most Popular Shows", "IMDb Originals", "Golden Globes", "Celebrity News",
                           "Celebs")]
AM2Y <- AM2Y[!AM2Y %in% c("4","Most Popular Movies", "Most Popular Shows", "IMDb Originals", "Golden Globes", "Celebrity News",
                          "Celebs")]

AM3X <- AM3X[!AM3X %in% c("5")]
AM3Y <- AM3Y[!AM3Y %in% c("5")]

AM4X <- AM4X[!AM4X %in% c("6","Top Box Office","TV News","IMDb Podcasts" ,"Women's History Month","Keywords")]
AM4Y <- AM4Y[!AM4Y %in% c("6","Top Box Office","TV News","IMDb Podcasts" ,"Women's History Month","Keywords")]

#Cleaned Vectors Converted into Lists

# Genre Lists
list01G <- list(G1)
list04G <- list(G4)
list01H <- list(H1)
list04H <- list(H2)

#Worst Movies Actors Lists
list01AM1 <- list(AM1)
list04AM1 <- list(AM1D)

list01AM2 <- list(AM2)
list04AM2 <- list(AM2D)

list01AM3 <- list(AM3)
list04AM3 <- list(AM3D)

list01AM4 <- list(AM4)
list04AM4 <- list(AM4D)

#Best Movies Actors Lists
list02AM1 <- list(AM1X)
list03AM1 <- list(AM1Y)

list02AM2 <- list(AM2X)
list03AM2 <- list(AM2Y)

list02AM3 <- list(AM3X)
list03AM3 <- list(AM3Y)

list02AM4 <- list(AM4X)
list03AM4 <- list(AM4Y)


# Data Frames of IMDB Movies

#Data Frames for WORST MOVIES IMDB
df_bottom <- do.call(rbind, Map(data.frame, Title=list01M, Year=list01Y, Score=list01S, Director=list01D, Actor1=list01AM1, Actor2=list01AM2, Actor3=list01AM3, 
                                Actor4=list01AM4, Runtime=list01R, Genre=list01G, Votes=list01V))
df_bottom151 <- do.call(rbind, Map(data.frame, Title=list04M, Year=list04Y, Score=list04S, Director=list04D, Actor1=list04AM1, Actor2=list04AM2, Actor3=list04AM3, 
                                   Actor4=list04AM4,  Runtime=list04R, Genre=list04G, Votes=list04V))

#Final Merged Data Frame for WORST MOVIES

df_wrt_imbd <- rbind(df_bottom, df_bottom151)


#Data Frames for BEST MOVIES IMDB
df_top <- do.call(rbind, Map(data.frame, Title=list02M, Year=list02Y, Score=list02S, Director=list02D, Actor1=list02AM1, Actor2=list02AM2, Actor3=list02AM3, 
                             Actor4=list02AM4, Runtime=list02R, Genre=list01H, Votes=list02V))
df_top151 <- do.call(rbind, Map(data.frame, Title=list03M, Year=list03Y, Score=list03S, Director=list03D, Actor1=list03AM1, Actor2=list03AM2, Actor3=list03AM3, 
                                Actor4=list03AM4,  Runtime=list04R, Genre=list04H, Votes=list04V))

#Final Merged Data Frame for BEST MOVIES
df_bst_imbd <- rbind(df_top, df_top151)


############################################################Summary of Obtained Data Frames#######################################################----

# Data Frames of Best Rated Movies Ever

#B1
df_bst  #70 Movies from Rotten Tomatoes
#B2
df_bst_imbd #100 Movies from IMDB


# Data Frames of Worst Rated Movies Ever

#W1
df_wrt #100 Movies from Rotten Tomatoes
#w2
df_wrt_imbd  #100 Movies from IMDB


#########################################################################################################################################################

#Plots/Visuals from Data Frames ----

#Visuals of Variables: Runtime, Score, Votes

#Vector Assign,ents
R_w2 <-  df_wrt_imbd$Runtime
V_w2 <- df_wrt_imbd$Votes

R_b2 <-  df_bst_imbd$Runtime
V_b2 <- df_bst_imbd$Votes


#Pattern to remove selected phrases
df_wrt_imbd$Runtime <- as.numeric(gsub("min", "",R_w2))
df_wrt_imbd$Votes <- as.numeric(gsub(",","",V_w2))

df_bst_imbd$Runtime <- as.numeric(gsub("min", "",R_b2))
df_bst_imbd$Votes <- as.numeric(gsub(",","",V_b2))


# Column conversion from character to numeric
df_wrt_imbd$Runtime <- as.numeric(df_wrt_imbd$Runtime)
df_wrt_imbd$Score <- as.numeric(df_wrt_imbd$Score)


df_bst_imbd$Runtime <- as.numeric(df_bst_imbd$Runtime)
df_bst_imbd$Score <- as.numeric(df_bst_imbd$Score)


#Average number of Votes
Avg_vw <- mean(V_w2)
Avg_vb <- mean(V_b2)

L <- as.vector(rbind(Avg_vw,Avg_vb)[,1])


barplot(L/1000, ylim = c(0,600),col=c("red","lightblue"), xaxt="n", main ="Total Average Votes Submitted",ylab = "(Thousands)")
axis(1, at = c(.75,1.95), labels = c("Worst Movies","Best Movies") , tick=FALSE, cex=0.9)

# Averages Scores for movies
Avg_sb <- mean(df_bst_imbd$Score)
Avg_sw <- mean(df_wrt_imbd$Score)

M <- as.vector(rbind(Avg_sw,Avg_sb)[,1])

barplot(M, ylim = c(0,10),col=c("red","lightblue"), xaxt="n", main ="Total Average Scores on 1-10 Scale")
axis(1, at = c(.75,1.95), labels = c("Worst Movies = 3.39","Best Movies = 8.45") , tick=FALSE, cex=.9)


#Runtime_Scatterplots

plot(df_bst_imbd$Runtime, main="Best Movies Runtimes", xlab="Index", ylab="Minutes", col = "blue", cex = 1.2, pch = 19)
plot(df_wrt_imbd$Runtime, main="Worst Movies Runtimes", xlab="Index", ylab="Minutes", col = "red", cex = 1.2, pch = 19)


#Director Scatter Plots
u <- table(df_wrt_imbd$Director)
i <- table(df_bst_imbd$Director)

dfu<- as.data.frame(u)
dfi<- as.data.frame(i)

plot(dfu$Freq, main = "Frequency of Director Name in Worst Movies", ylab = "Count", col = "red", cex = 1.2, pch = 18)
plot(dfi$Freq, main = "Frequency of Director Name in Best Movies",  ylab = "Count", col = "blue", cex = 1.2, pch = 18)






