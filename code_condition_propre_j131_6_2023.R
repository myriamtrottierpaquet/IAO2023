####CHOIX POUR PASTILLE
rm(list=ls())
#package
library(tidyverse)
library(ggrepel)
library(cowplot)

#working directory
setwd("C:/Users/Frederic/OneDrive - Université Laval/Pellet pairing 2022")
#setwd("C:/Users/Frederic/OneDrive/pellet_pairing")
setwd("C:/Users/myria/Documents/Maitrise/IAO2023/Pellet pairing 2022")

### correction PCA

#charger les donnees de condition de toutes les annees
data <- get(load("cond_IAO2.RData")) ####################### VERFIER QUE CEST LE BON FICHIER (devrait jamais changer) #########
nrow(data)
sort(unique(data$year))

#selection de colonnes
data <- data %>% 
  dplyr::select(day, year, weight, tarse, culmen, collier = collar) %>% 
  mutate(ID = NA, catch=NA)

# meme ordre que lautre fichier
data <- data %>% 
  dplyr::select(day, year, weight, tarse, culmen, ID, catch, collier) 

# retrait d'une valeur de tarse aberrante (micro tarse)
data <- data %>% filter(tarse>60)


#lecture fichier de donnees capture aujourdhui
data_ajd <- read.csv("cond_IAO_aujourdhui_2023_131_6.csv",header = TRUE, sep= ";") ############ VERFIER QUE CEST LE BON FICHIER #######


#selection des colonnes
data_ajd <- data_ajd %>% 
  dplyr::select(day, year, weight, tarse, culmen, ID, catch, collier) %>% 
  mutate(tarse = as.numeric(str_replace(tarse, pattern = ",", replacement =  ".")),
         culmen = as.numeric(str_replace(culmen, pattern = ",", replacement = ".")))

data_ajd <- data_ajd %>% na.omit()

#verification noms des colonnes
names(data_ajd)
names(data)

#fusion des deux jeux de donnees
data_full <- rbind(data, data_ajd )

#verification de la fusion
identical((nrow(data) + nrow(data_ajd)), nrow(data_full))
head(data_full)[c(1:3),]


# CORRECTION
corr_fun <- function(data){
  pp_tarsus_culmen<-princomp(data[,c('tarse','culmen')])
  ACP1<-pp_tarsus_culmen$scores
  PC1<-ACP1[,1]
  PC2<-ACP1[,2]
  data$PC1<-PC1
  
  #Compute relative mass
  reg_PCA_weight<-lm(weight~PC1, data=data)
  
  #add residuals to mean weight
  data$weight_size <- as.vector(mean(data$weight) + residuals(reg_PCA_weight))
  
  catch <- data$catch
  tarse <- data$tarse
  culmen <- data$culmen
  ID <- data$ID 
  day <- data$day
  year <- data$year
  org_weight <- data$weight
  weight_size <- data$weight_size
  collier <- data$collier
  
  return(data.frame(ID, collier, day, year, tarse, culmen, PC1, weight_size, org_weight, catch))
  
}


cond_cap <- corr_fun(data_full)
#write.csv(cond_cap, file = "C:/Users/pileg51/OneDrive/Maitrise_Canada/IAO/google_drive/acc_TRT/condition_corporelle.csv")

ggplot(aes(x=PC1, y=org_weight), data=cond_cap)+ 
  geom_point() + geom_smooth(method='lm')+
  geom_point(aes(x=PC1, y=org_weight), data=cond_cap[1,], col='red', size=4)+
  geom_point(aes(x=PC1, y=org_weight), data=cond_cap[4,], col='green', size=4)


ggplot(aes(x=PC1, y=weight_size), data=cond_cap)+ 
  geom_point() + geom_smooth(method='lm')+
  geom_point(aes(x=PC1, y=weight_size), data=cond_cap[1,], col='red', size=4)+
  geom_point(aes(x=PC1, y=weight_size), data=cond_cap[4,], col='green', size=4)



############################   Graphiques pour choisir

### Changer 1 fois le capture_day et deux fois le catch

capture_day <- 131 ################################################# VERFIER QUE CEST LE BON JOUR DE CAPTURE #######
catch_no <- 6       ####################### CHANGER AUSSI LE CATCH NUMBER
current_year <- 2023

mod <- lm(weight_size~day, data=filter(cond_cap, year<2021))
pred_df <- data.frame(day = capture_day)
pred_df$fit <- predict(object = mod, newdata = pred_df)
pred_df$fit.high <- pred_df$fit+25 
pred_df$fit.low <- pred_df$fit-25


pred_line <- data.frame(day= filter(cond_cap, year<2021) %>% dplyr::select(day))
pred_line$fit <- predict(mod, data=pred_line)

p1 <- ggplot()+
  geom_smooth(data=filter(cond_cap, year<2024), aes(x=day, y=weight_size), method = lm, se = T)+
  geom_point(data=filter(cond_cap, year<2024), aes(x=day, y=weight_size), col = 'grey')+
  geom_rect(data=pred_df, aes(xmin=capture_day-2, xmax=capture_day+2, ymax = fit.high, ymin=fit.low), fill='green', alpha=0.15)+
  geom_point(data=cond_cap %>%  filter(year==current_year, day==capture_day, catch == catch_no), aes(x=day, y=weight_size)) + 
  coord_cartesian(ylim=c(2400,3200))
p1

dat_graph <- cond_cap %>% filter(year==current_year,
                                 day==capture_day,
                                 catch==catch_no,
                                 weight_size < pred_df$fit+150,
                                 weight_size > pred_df$fit-75)

temp = cond_cap %>% filter(year==current_year,
                    day==capture_day,
                    catch==catch_no)


p2 <- ggplot(data=dat_graph, aes(y=PC1, x=org_weight))+
  geom_point(pch=17, cex=2, col='blue', alpha=0.7) +
  geom_point(data=dat_graph, aes(y=PC1, x=weight_size), cex=2)+
  geom_linerange(aes(xmin=weight_size, xmax=org_weight, y=PC1))+
  geom_vline(xintercept = pred_df$fit, col= 'blue', size=1, lty=1)+
  geom_vline(xintercept = pred_df$fit+25, col= 'green', size=1, lty=2)+ # 25g au dessus de la masse moyenne corrigée
  geom_vline(xintercept = pred_df$fit-25, col= 'green', size=1, lty=2)+ # 25g en dessous de la masse moyenne corrigée
  geom_vline(xintercept = pred_df$fit+50, col= 'orange', size=1, lty=3)+ # 50g au dessus de la masse moyenne corrigée
  geom_vline(xintercept = pred_df$fit+75, col= 'red', size=1, lty=3)+ # 75g au dessus de la masse moyenne corrigée
  
  geom_text_repel(data=dat_graph, aes(x=org_weight, y=PC1, label=ID), col='blue')+
  
  xlab('Point noir = Masse corrigee /n Triangle bleu = Masse reelle')

p2



# le triangle bleu (masse relle) doit etre au dessus du seuil minimum pour qu'une oie puisse porter un collier GPS
# le point noir est idealement legerement en dessous de la ligne pleine bleu (condition corrigee un peu en dessous de la moyenne pour que l'oie ait encore de la place pour gagner en condition)
# les deux points noir d'une meme paire devraient etre le plus proche possible tant sur x (masse) que sur y (taille squelettique), mais la priorite va à la proximité sur l'axe des x (masse)

# 45/0.015; 45/0.02
# 45g collar = 1.5% body mass d'une oie de 3000g
# 45g collar = 2% body mass d'une oie de 2250g


#hist(cond_cap$org_weight, breaks = 20)


# Final graph

chosen_ones <- data.frame(ring_ID = c('2127-44235', '2127-44238', #1
                                      '2127-44234', '2127-44200', #2
                                      '2127-44247', '2127-44241'),#3
                          pair_no = c(1, 1, 2, 2,3,3))                ############################ A MODIFIER SELON LES OIES CHOISIES ######
chosen_ones$pair_no <- as.factor(chosen_ones$pair_no)                  



# check up


ggplot()+
  geom_smooth(data=filter(cond_cap, year<2021), aes(x=day, y=weight_size), method = lm, se = T)+
  geom_point(data=filter(cond_cap, year<2021), aes(x=day, y=weight_size), col = 'grey')+
  geom_rect(data=pred_df, aes(xmin=capture_day-2, xmax=capture_day+2, ymax = fit.high, ymin=fit.low), fill='green', alpha=0.15)+
  geom_point(data=filter(cond_cap, year==current_year & day==capture_day & catch ==catch_no), aes(x=day, y=weight_size)) +
  
  geom_point(data=filter(cond_cap, year==current_year & day==capture_day & ID %in% chosen_ones$ring_ID), aes(x=day, y=weight_size), col="red") +
  #geom_point(aes(x=123, y=2835.445), col='green', size=8)+
  
  coord_cartesian(ylim=c(2200,3500))
  #coord_cartesian(ylim=c(pred_df$fit.low-100,pred_df$fit.high+100))

chosen_dat_graph <- merge(chosen_ones, cond_cap, by.x="ring_ID", by.y = "ID", all.x=T) %>% arrange(pair_no)


p3 <- ggplot(data=chosen_dat_graph, aes(y=PC1, x=org_weight, col=pair_no))+
  geom_point(pch=17, cex=2, alpha=0.7) +
  geom_point(data=chosen_dat_graph, aes(y=PC1, x=weight_size, col=pair_no), cex=2)+
  geom_linerange(aes(xmin=weight_size, xmax=org_weight, y=PC1))+
  geom_vline(xintercept = pred_df$fit, col= 'blue', size=1, lty=1)+
  geom_vline(xintercept = pred_df$fit+25, col= 'green', size=1, lty=2)+ # 25g au dessus de la masse moyenne corrigée
  geom_vline(xintercept = pred_df$fit-25, col= 'green', size=1, lty=2)+ # 25g en dessous de la masse moyenne corrigée
  geom_vline(xintercept = pred_df$fit+50, col= 'orange', size=1, lty=3)+ # 50g au dessus de la masse moyenne corrigée
  geom_vline(xintercept = pred_df$fit+75, col= 'red', size=1, lty=3)+ # 75g au dessus de la masse moyenne corrigée
  geom_text_repel(data=chosen_dat_graph, aes(x=org_weight, y=PC1, label=ring_ID, col= pair_no))+
  
  xlab('Point noir = Masse corrigee /n Triangle bleu = Masse reelle')

p3


##### Lidocaine
LIDO <- cond_cap%>%
          mutate(lido= (2*(org_weight/1000))/20,
                 met= (1.5*(org_weight/1000))/20)

chosen_to_save <- merge(chosen_ones, LIDO, by.x="ring_ID", by.y = "ID", all.x=T) %>% arrange(pair_no)

write.table(chosen_to_save, file= paste0("chosen_ones_day_",capture_day, "capture_no_", "6", ".xls"))


#calcul date de depart
date_depart <- read.csv("SNGO_date_depart.csv",header = TRUE, sep= ";")

date_depart %>% 
  group_by(Year) %>%
  dplyr::select(Departure) %>%
  filter(Departure>120) %>% 
  na.omit() %>% 
  summarize(Mean_departure = mean(Departure, na.rm=TRUE),
            min_departure = min(Departure, na.rm=T))
