# quantifier les erreurs 


# on prend raw data : separation pour chaque AMP 

data_Banyuls <- read_csv("data/pro/data_Banyuls.csv")
data_Bonifacio <- read_csv("data/pro/data_bonifacio.csv")
data_Calanque <- read_csv("data/pro/data_Calanque.csv")
data_CapCorse <- read_csv("data/pro/data_cap_corse.csv")
data_PNMGL <- read_csv("data/pro/data_PNMGL.csv")
data_hors_amp <- read_csv("data/pro/hors_amp.csv")


# puis tri erreur nb de pecheur : compter le nb de lignes erreur , premier gros tri pour enlever les nb aberrants, mais des fois concours de pêche donc garder un certain nombre


erreur_banyuls_pecheur <- data_Banyuls%>%
  filter(Nb_de_pêcheurs > 50 | Nb_de_pêcheurs <= 0)

erreur_PNMGL_pecheur <- data_PNMGL%>%
  filter(Nb_de_pêcheurs > 50 | Nb_de_pêcheurs <= 0)

erreur_capcorse_pecheur <- data_CapCorse%>%
  filter(Nb_de_pêcheurs > 50 | Nb_de_pêcheurs <= 0)

erreur_calanques_pecheur<- data_Calanque%>%
  filter(Nb_de_pêcheurs > 50 | Nb_de_pêcheurs <= 0)

erreur_horsAMP_pecheur <- data_hors_amp%>%
  filter(Nb_de_pêcheurs > 50 | Nb_de_pêcheurs <= 0)




# erreur nb de prises : compter le nb d'erreur : on se dit que si ce n'est pas du petit pelagique, c'est peu probable d'en prendre + que 30 (basé sur ce que gestionnaires m'ont dit) : ici l'objectif est d'nlever ce qui est vraiment aberrant, donc on peut viser large 

#PNMGL 
erreur_PNMGL_prise <- data_PNMGL %>%
  filter((NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 100) |
           (!NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 30))

#Banyuls
erreur_banyuls_prise <- data_Banyuls %>%
  filter((NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 100) |
           (!NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 30))



#Bonifacio
erreur_bonifacio_prise <- data_Bonifacio%>%
  filter((NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 100) |
           (!NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 30))


#calanques
erreur_calanques_prise <- data_Calanque %>%
  filter((NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 100) |
           (!NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 30))


#capcorse
erreur_capcorse_prise <- data_CapCorse %>%
  filter((NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 100) |
           (!NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 30))

#horsAMP
erreur_horsAMP_prise <- data_hors_amp %>%
  filter((NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 100) |
           (!NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol") & Nb_de_prises > 30))


tableau_erreurs <- data.frame(
  AMP = c("PNMGL", "Banyuls", "Bonifacio", "Calanques", "CapCorse", "HorsAMP"),
  Nb_erreurs_prises = c(nrow(erreur_PNMGL_prise),
                 nrow(erreur_banyuls_prise),
                 nrow(erreur_bonifacio_prise),
                 nrow(erreur_calanques_prise),
                 nrow(erreur_capcorse_prise),
                 nrow(erreur_horsAMP_prise)))




# automatisation de la detection d'erreur pour que ça soit plus simple 

# fonction qui detecte les erreurs 

erreurs_nb <- function(data, nom_amp) {
  list(
    prises = data %>%
      filter((NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol", "Girelle","Girelle paon") & Nb_de_prises > 100) |
               (!NomVernaculaire %in% c("Maquereau commun", "Sardine commune", "Maquereau espagnol", "Girelle","Girelle paon") & Nb_de_prises > 30)),
    
    pecheurs = data %>%
      filter(Nb_de_pêcheurs < 0 | Nb_de_pêcheurs > 50)
  )
}


erreurs_PNMGL <- erreurs_nb(data_PNMGL, "PNMGL")
erreurs_Banyuls <- erreurs_nb(data_Banyuls, "Banyuls")
erreurs_Bonifacio <- erreurs_nb(data_Bonifacio, "Bonifacio")
erreurs_Calanques <- erreurs_nb(data_Calanque, "Calanques")
erreurs_CapCorse <- erreurs_nb(data_CapCorse, "CapCorse")
erreurs_HorsAMP <- erreurs_nb(data_hors_amp, "HorsAMP")




# erreur taille/poids : nb d'erreurs 

# on regarde que pour les prises uniques car sinon on peut pas savoir 


# taille max 
erreurs_taille_max <- function(data, nom_amp) {
  list(
    anomalies = data %>%
      filter(Nb_de_prises == 1) %>%
      filter(
        (Taille_MM > Taille_max_mm * 1.1 & !is.na(Taille_max_mm)) |
          (Poids_g > Poids_max_g * 1.1 & !is.na(Poids_max_g))
      )
  )
}

erreurs_taille_PNMGL <- erreurs_taille_max(data_PNMGL, "PNMGL")
erreurs_taille_Banyuls <- erreurs_taille_max(data_Banyuls, "Banyuls")
erreurs_taille_Bonifacio <- erreurs_taille_max(data_Bonifacio, "Bonifacio")
erreurs_taille_Calanques <- erreurs_taille_max(data_Calanque, "Calanques")
erreurs_taille_CapCorse <- erreurs_taille_max(data_CapCorse, "CapCorse")
erreurs_taille_HorsAMP <- erreurs_taille_max(data_hors_amp, "HorsAMP")


#taille min 

taille_banyuls <- read_delim("data/raw/taille_banyuls.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE,  locale = locale(encoding = "latin1"))
taille_calanque <- read_excel("data/raw/taille_calanque.xlsx")
taille_cap_corse <- read_excel("data/raw/taille_cap_corse.xlsx")
taille_pmngl <- read_delim("data/raw/taille_pmngl.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "latin1"))
#taille_bonifacio a faire !!!!!


erreur_inf_banyuls <- data_Banyuls %>%
  filter(Nb_de_prises==1)%>%

  left_join(taille_banyuls %>% select( NomScientifique, taille_min_MM),
            by = "NomScientifique") %>%
  filter(
    !is.na(taille_min_MM),
    Taille_MM < taille_min_MM * 0.9,
    Taille_MM > 0,
    Nokill !=1)



erreur_inf_PNMGL <- data_PNMGL %>%
  filter(Nb_de_prises==1)%>%
  
  left_join(taille_pmngl %>% select( NomScientifique, taille_min_MM),
            by = "NomScientifique") %>%
  filter(
    !is.na(taille_min_MM),
    Taille_MM < taille_min_MM * 0.9,
    Taille_MM > 0,
    Nokill !=1)



erreur_inf_calanque <- data_Calanque %>%
  filter(Nb_de_prises==1)%>%
  
  left_join(taille_calanque %>% select( NomScientifique, taille_min_MM),
            by = "NomScientifique") %>%
  filter(
    !is.na(taille_min_MM),
    Taille_MM < taille_min_MM * 0.9,
    Taille_MM > 0,
    Nokill !=1)


erreur_inf_capcorse <- data_CapCorse %>%
  filter(Nb_de_prises==1)%>%
  
  left_join(taille_cap_corse %>% select( NomScientifique, taille_min_MM),
            by = "NomScientifique") %>%
  filter(
    !is.na(taille_min_MM),
    Taille_MM < taille_min_MM * 0.9,
    Taille_MM > 0,
    Nokill !=1)


erreur_inf_bonifacio<- data.frame()

erreur_inf_horsAMP <- data.frame() # on peut rien mettre car pas de reglementation 


# erreur d'engin / technique 





# tableau erreur 

tableau_erreurs <- data.frame(
  AMP = c("PNMGL", "Banyuls", "Bonifacio", "Calanques", "CapCorse", "HorsAMP"),
  Erreurs_prises = c(
    nrow(erreurs_PNMGL$prises),
    nrow(erreurs_Banyuls$prises),
    nrow(erreurs_Bonifacio$prises),
    nrow(erreurs_Calanques$prises),
    nrow(erreurs_CapCorse$prises),
    nrow(erreurs_HorsAMP$prises)
  ),
  Erreurs_pecheurs = c(
    nrow(erreurs_PNMGL$pecheurs),
    nrow(erreurs_Banyuls$pecheurs),
    nrow(erreurs_Bonifacio$pecheurs),
    nrow(erreurs_Calanques$pecheurs),
    nrow(erreurs_CapCorse$pecheurs),
    nrow(erreurs_HorsAMP$pecheurs)
  ), 
  Erreurs_taille_max = c(
    nrow(erreurs_taille_PNMGL$anomalies),
    nrow(erreurs_taille_Banyuls$anomalies),
    nrow(erreurs_taille_Bonifacio$anomalies),
    nrow(erreurs_taille_Calanques$anomalies),
    nrow(erreurs_taille_CapCorse$anomalies),
    nrow(erreurs_taille_HorsAMP$anomalies)
  ), 
  Erreurs_taille_min = c(
    nrow(erreur_inf_PNMGL), 
    nrow(erreur_inf_banyuls), 
    nrow(erreur_inf_bonifacio), 
    nrow(erreur_inf_calanque), 
    nrow(erreur_inf_capcorse), 
    nrow(erreur_inf_horsAMP)
  )
) %>%
  mutate(
    Total = Erreurs_prises + Erreurs_pecheurs + Erreurs_taille_max + Erreurs_taille_min
  ) %>%
  arrange(desc(Total))


print(tableau_erreurs)



# correlation niveau reglementation 

reglementation <- read_excel("data/pro/reglementation.xlsx")



tableau_erreurs <- tableau_erreurs%>%
  left_join(reglementation)


table_anova <- tableau_erreurs%>%
  select(AMP, Total, Reglementation)


anova <- aov(Total ~ Reglementation, data = table_anova)
summary(anova)




