install.packages("tidyverse", type = "binary")
install.packages("ggrepel", type = "binary")
install.packages("ggimage", type = "binary")
install.packages("nflfastR", type = "binary")
install.packages("nflreadr", type = "binary")
install.packages("ggplot2")
install.packages("readr")


library(tidyverse)
library(ggrepel)
library(ggimage)
library(ggplot2)
library(dplyr)
library(nflfastR)
library(nflreadr)
library(readr)
library(nflfastR)
library(dplyr)

"
This code will create an expected fantasy points or fantasy usage model
First we will find the td% of each target depending on the yardline of the target.
This is based on the past 20 seasons of data.
Then we will find the td% of each rushing attempt depending on the yardline of the rush.
We will do the same exercise for interceptions with all passing attempts, irrelevant of yardline.
We will do the same exercise for fumbles with all rushing and passing plays, irrelevant of yardline.
We will do the same exercise for 2pat, breaking out success of run and pass plays
"

#load last 20 seasons of nfl data
pbp<-load_pbp(2001:2021)
#add a target line variable taking the starting line and the air yards
pbp<-pbp%>%
  mutate(
    target_yrdline=(yardline_100-air_yards))
#change xyac average mean and cp NA to 0
pbp$xyac_mean_yardage[is.na(pbp$xyac_mean_yardage)]<-0
pbp$cp[is.na(pbp$cp)]<-0
pbp$target_yrdline[is.na(pbp$target_yrdline)]<-0
pbp$air_yards[is.na(pbp$air_yards)]<-0

#create a df that takes the % that a target to a specific yard line turns into a td
passingtddf<-pbp%>%
  filter(!is.na(receiver)&complete_pass==1&!is.na(air_yards))%>%
  group_by(target_yrdline)%>%
  summarize(totptds=sum(pass_touchdown),attempts=n())
passingtddf<-passingtddf%>%
  mutate(rectdpercent=(totptds/attempts))%>%
  select(target_yrdline,rectdpercent)


#create a df that takes the % that a carry from a specific snap yard line turns into a td
rushingtddf<-pbp%>%
  filter(play_type_nfl=="RUSH")%>%
  group_by(yardline_100)%>%
  summarize(totrtds=sum(rush_touchdown),attempts=n())
rushingtddf<-rushingtddf%>%
  mutate(rushtdpercent=(totrtds/attempts))%>%
  select(yardline_100,rushtdpercent)

#create a df that takes the % that a pass attempt from anywhere on the field will turn into an interception
passingintdf<-pbp%>%
  filter(play_type_nfl=="PASS")%>%
  group_by(play_type_nfl)%>%
  summarize(totints=sum(interception),attempts=n())
passingintdf<-passingintdf%>%
  mutate(intpercent=(totints/attempts))%>%
  select(play_type_nfl,intpercent)

#create a df that takes the % that a play attempt from anywhere on the field will turn into a fumble  
fumbledf<-pbp%>%
  filter((play_type_nfl=="PASS"|play_type_nfl=="RUSH")&special_teams_play==0)%>%
  group_by(special_teams_play)%>%
  summarize(totfums=sum(fumble_lost),attempts=n())
fumbledf<-fumbledf%>%
  mutate(fumpercent=(totfums/attempts))%>%
  select(special_teams_play,fumpercent)

#create a df that takes the % that a carry from a specific snap yard line turns into an average gain
rushingyarddf<-pbp%>%
  filter(play_type_nfl=="RUSH")%>%
  group_by(yardline_100)%>%
  summarize(totryds=sum(rushing_yards),attempts=n())
rushingyarddf<-rushingyarddf%>%
  mutate(rushyard_mean=(totryds/attempts))%>%
  select(yardline_100,rushyard_mean)

"#create a df that takes the % that a punt or kick return turns into a td
returntddf<-pbp%>%
  filter((!is.na(punt_returner_player_name)|!is.na(kickoff_returner_player_name))&fumble_lost==0&)%>%
  group_by(play_type_nfl)%>%
  summarize(totrettds=sum(return_touchdown),attempts=n())
returntddf<-returntddf%>%
  mutate(returntd_mean=totrettds/attempts)%>%
  select(play_type_nfl,returntd_mean)
pbp%>%
  filter((play_type_nfl=='KICK_OFF'|play_type_nfl=='PUNT')&fumble_lost==0)%>%
  select(desc,punt_returner_player_name,kickoff_returner_player_name,return_touchdown)%>%
  view()"
  
#create 2pat passing df
pbp%>%
  filter(two_point_attempt==1)%>%
  group_by(play_type,success)%>%
  summarize(numbofplays=n())
#PAT2 run conversion = 56.0% and pass conversion= 44.6%
  
pbp<-pbp%>%
  left_join(passingtddf, by =c("target_yrdline"))%>%
  left_join(rushingtddf,by =c("yardline_100"))%>%
  left_join(rushingyarddf, by =c("yardline_100"))%>%
  left_join(passingintdf,by =c("play_type_nfl"))%>%
  left_join(fumbledf,by =c("special_teams_play"))
pbp$fumpercent[is.na(pbp$fumpercent)]<-0.006022111
pbp%>%
  select(fumble_lost,special_teams_play,play_type)%>%
  filter(special_teams_play==0)%>%
  view()
#load 2021 Data

pbp2021<-pbp%>%
  filter(season==2021&season_type=="REG")

#load roster data
roster2021<-load_rosters(2021)
roster2020<-load_rosters(2020)
#adding henry ruggs data back into 2021 df
ruggsdf<-roster2020%>%
  filter(gsis_id=="00-0036357")
rosternew2021<-rbind(roster2021,ruggsdf)
rosternew2021[4840,1]<-2021 #column1 is season
rosternew2021[4840,24]<-2 #column2 is years_exp
#adding Antonio Brown team, Tyrell Williams team,
rosternew2021[4654,2]<-"DET"
rosternew2021[4679,2]<-"TB"
#changing David Johnson, Michael Thomas, AJ Green,Lamar Jackson,Michael Carter,Josh Allen,David Moore duplicates
rosternew2021[4363,7]<-"David Johnson2"
rosternew2021[513,7]<-"Michael Thomas2"
rosternew2021[593,7]<-"A.J. Green2"
rosternew2021[421,7]<-"Lamar Jackson2"
rosternew2021[2036,7]<-"Michael Carter2"
rosternew2021[3367,7]<-"Josh Allen2"
rosternew2021[1199,7]<-"Josh Allen3"
rosternew2021[617,7]<-"David Moore2"
rosternew2021[2508,7]<-"Ryan Griffin2"
roster2021<-rosternew2021 #change back to roster2021 name

#select relevant columns, make sure we filter out any players with NA for gsis_id
roster2021short<-roster2021%>%
  filter(!is.na(gsis_id))%>%
  select(position,full_name,gsis_id)
passpbp2021<-pbp2021
rushrecpbp2021<-pbp2021
#create passing data frame of passing fantasy points 
passpbp2021<-passpbp2021%>%
  filter(play_type_nfl=="PASS"&play==1)%>%
  mutate(
    pat2_pass_conv=0.446,
    expfanpassyrds=(cp*(air_yards+xyac_mean_yardage)*.04)*pass_attempt,
    expfanpasstds=(cp*rectdpercent*4)*pass_attempt,
    expfanpassto=((intpercent+fumpercent)*pass_attempt)*-1,
    expfanpass2pat=(two_point_attempt*pat2_pass_conv)*2,
    expassfp=expfanpasstds+expfanpassyrds+expfanpass2pat+expfanpassto)

#create receiving/rushing df
rushrecpbp2021<-rushrecpbp2021%>%
  filter((play_type_nfl=="PASS"|play_type_nfl=="RUSH")&play==1)%>%
  mutate(
    pat2_pass_conv=0.446,
    pat2_rush_conv=0.560,
    expfanrecep_half=(cp*pass_attempt*.5),
    expfanrecyrds=(cp*(air_yards+xyac_mean_yardage)*0.1),
    expfanrectds=(cp*rectdpercent*6)*pass_attempt,
    expfanrushyrds=((rushyard_mean)*0.1)*rush_attempt,
    expfanrushtds=(rushtdpercent*6)*rush_attempt,
    expfanrecfum=((fumpercent)*pass_attempt)*-1,
    expfanrushfum=((fumpercent)*rush_attempt)*-2,
    expfanrec2pat=(two_point_attempt*pat2_pass_conv)*2,
    expfanrush2pat=(two_point_attempt*pat2_rush_conv)*2,
    exrushrecfp=(expfanrecyrds+expfanrectds+expfanrushyrds+expfanrushtds+
      expfanrecfum+expfanrushfum+expfanrec2pat+expfanrush2pat+expfanrecep_half))
#create a summary of weekly expected passing fantasy points
passfpsumm<-passpbp2021%>%
  group_by(passer_player_id,passer,week)%>%
  summarize(expassfptot=sum(expassfp))%>%
  mutate(gsis_id=passer_player_id)%>%
  ungroup()

#create a summary of weekly expected rushing/receiving fantasy points
rushrecfpsumm<-rushrecpbp2021%>%
  group_by(fantasy_player_name,fantasy_player_id,week)%>%
  summarize(exrushrecfptot=sum(exrushrecfp))%>%
  filter(!is.na(fantasy_player_name))%>%
  mutate(gsis_id=fantasy_player_id)%>%
  ungroup()
#combine the two dfs together
totalfpsumm<-passfpsumm%>%
  full_join(rushrecfpsumm,by =c('passer'='fantasy_player_name','gsis_id','week'))
#Set NA = 0
totalfpsumm$expassfptot[is.na(totalfpsumm$expassfptot)]<-0
totalfpsumm$exrushrecfptot[is.na(totalfpsumm$exrushrecfptot)]<-0

totalfpsumm<-totalfpsumm%>%
  mutate(
    total_combined_fp=expassfptot+exrushrecfptot,
    player_name=passer)%>%
  select(total_combined_fp,player_name,gsis_id,week)
  
pgfpsumm<-totalfpsumm%>%
  group_by(gsis_id,player_name)%>%
  summarize(season_totalfp=sum(total_combined_fp),games=n())
  
pgfpsumm<-pgfpsumm %>% 
mutate(expected_fantasy_ppg=season_totalfp/games)%>%
  view()

totalfpsumm1316<-totalfpsumm%>%
  filter(week>12&week<17)%>%
  select(total_combined_fp,player_name,gsis_id,week)

pgfpsumm1316<-totalfpsumm1316%>%
  group_by(gsis_id,player_name)%>%
  summarize(season_totalfp=sum(total_combined_fp),games=n())

pgfpsumm1316<-pgfpsumm1316%>% 
  mutate(expected_fantasy_ppg=season_totalfp/games)%>%
  view()
  



    














