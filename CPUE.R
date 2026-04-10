# cpue 

data_PNMGL <- read_csv("data/pro/data_PNMGL.csv")

data_PNMGL<- data_PNMGL%>%
  mutate(Nb_de_prises = if_else (Nb_de_prises>150 , 1, Nb_de_prises))%>%
  mutate(Nb_de_prises = if_else (NomVernaculaire != "Maquereau commun" & NomVernaculaire != "Sardine commune" & NomVernaculaire != "Maquereau espagnol" & NomVernaculaire!="Girelle" & NomVernaculaire !="Girelle paon"  & Nb_de_prises >30 , 1, Nb_de_prises)) %>%
  mutate(Nb_de_pêcheurs = if_else (Nb_de_pêcheurs <50 | Nb_de_pêcheurs >= 0, 1, Nb_de_pêcheurs)) 


#engin -> min 30 sortie avec 
data_PNMGL <- data_PNMGL%>%
  group_by(Engin)%>%
  filter(n() >= 30) %>%
  ungroup()

# CPUE = (nb/pecheur/engin/sortie)/duree sortie) *60 

data_PNMGL<- data_PNMGL%>%
  mutate(effort = Durée_de_la_session / Nb_de_pêcheurs) # pour gerer les sorties a plusieurs pecheurs 
  

cpue_NB<- data_PNMGL%>%
  group_by(Engin, NomVernaculaire)%>%
  summarise(
    somme_Nki = sum(Nb_de_prises, na.rm = TRUE),
    somme_Tki = sum(effort, na.rm = TRUE),
    CPUE = (somme_Nki / somme_Tki) * 60
  )
  


cpue_NB<- data_PNMGL%>%
  group_by(Engin, NomVernaculaire)%>%
  summarise(
    somme_Bki = sum(Nb_de_prises, na.rm = TRUE),
    somme_Tki = sum(effort, na.rm = TRUE),
    CPUE = (somme_Bki / somme_Tki) * 60
  )


ggplot(cpue, aes(x = Engin, y = CPUE, fill = NomVernaculaire)) +
  geom_col(position = "dodge") +
  labs(
    title = "CPUE par engin et par espèce",
    x = "Engin",
    y = "CPUE (g/h)"
  ) +
  theme_minimal()



# pour les espèces top 10 
espece_top10 <- data_PNMGL %>%
  group_by(NomScientifique) %>%
  summarise(total= sum(Nb_de_prises, na.rm=T), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  slice_head(n=10)

PNMGL_10 <- data_PNMGL %>%
  filter(NomScientifique %in% espece_top10$NomScientifique)


cpue_NB_10<- PNMGL_10%>%
  group_by(Engin, NomVernaculaire)%>%
  summarise(
    somme_Bki = sum(Nb_de_prises, na.rm = TRUE),
    somme_Tki = sum(effort, na.rm = TRUE),
    CPUE = (somme_Bki / somme_Tki) * 60
  )

ggplot(cpue_NB_10, aes(x = Engin, y = CPUE, fill = NomVernaculaire)) +
  geom_col(position = "dodge") +
  labs(
    title = "CPUE par engin et par espèce",
    x = "Engin",
    y = "CPUE (nb/h)"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
