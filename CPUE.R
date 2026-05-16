# cpue 

pro_data <- read_csv("data/pro/pro_data.csv")

############################essai brouillon ###########

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










# data prise multiple 

pnmgl_multiple<-data_PNMGL%>%
  select(Id_Prise,Nb_de_prises, Taille_MM, Poids_g, NomVernaculaire, NomScientifique)

# correction_poids<- pro_data%>%
#   mutate(Poids_theorique_SIH = NA)%>%
#   mutate(Poids_theorique_FB = NA)%>%
#   mutate(Poids_theorique_ObsBio = NA)%>%
#   mutate(type_poids_theorique_choisi = NA)%>%
#   mutate(Diagnostic_poids = NA)%>%
#   mutate(Poids_final = NA)
  
# 
# correction_poids<- correction_poids%>%
#   mutate(Diagnostic_poids = ifelse(Poids_g==0, "Poids absent", Diagnostic_poids))


SIH_poids_theo <- data_coherente_SIH_clean%>%
  dplyr::select(NomVernaculaire, NomScientifique, RTP_COEF_A_CM, RTP_COEF_B, origine)%>%
  distinct(NomVernaculaire, .keep_all = T)%>%
  rename(a_SIH=RTP_COEF_A_CM)%>%
  rename(b_SIH =RTP_COEF_B)%>%
  rename(origine_SIH = origine)

FB_poids_theo <- data_coherente_FB_clean%>%
  dplyr::select(NomVernaculaire, NomScientifique, a, b, origine)%>%
  distinct(NomVernaculaire, .keep_all = T)%>%
  rename(a_FB=a)%>%
  rename(b_FB = b)%>%
  rename(origine_FB = origine)


Obsbio_poids_theo <- data_coherente_obsbio_clean%>%
  mutate(origine = "med")%>%
  dplyr::select(NomVernaculaire, NomScientifique, a, b, origine)%>%
  distinct(NomVernaculaire, .keep_all = T)%>%
  rename(a_obs = a) %>%
  rename(b_obs = b)%>%
  rename(origine_obs = origine)



#Poids_RTP=a*Taille_CM^b

correction_poids<- pro_data%>%
  left_join(Obsbio_poids_theo)%>%
  left_join(SIH_poids_theo)%>%
  left_join(FB_poids_theo)
  

correction_poids<- correction_poids%>%
  mutate(Poids_theorique_SIH = a_SIH*Taille_MM ^b_SIH)%>%
  mutate(Poids_theorique_FB= a_FB * (Taille_MM/10)^b_FB)%>%
  mutate(Poids_theorique_ObsBio = a_obs* (Taille_MM/10)^b_obs)
  
  
  

  
df_test1 <- pnmgl_multiple %>%
  rowwise() %>%
  mutate(
    nb_non_na = sum(!is.na(c(Poids_theorique_SIH, Poids_theorique_FB, Poids_theorique_ObsBio))),
    
    Poids_corrige = ifelse(nb_non_na == 1,
                           sum(c(Poids_theorique_SIH, Poids_theorique_FB, Poids_theorique_ObsBio), na.rm = TRUE),
                           NA),
    
    Type_correction_poids = ifelse(nb_non_na == 1,
                  case_when(
                    !is.na(Poids_theorique_SIH) ~ "poids théo SIH",
                    !is.na(Poids_theorique_FB) ~ "poids théo FB",
                    !is.na(Poids_theorique_ObsBio) ~ "poids théo Obsbio"
                  ),
                  NA)
  ) %>%
  ungroup()




mes colonnes : poids_obs ; Poids_theorique_SIH, Poids_theorique_FB, Poids_theorique_ObsBio; 3 colonnes origine des poids: origine_obs, origine_sih, origine_fb ; type_poids_theorique_choisi, diag_poids ; poids_final 


df_test<- df%>%
  
  Si il y a 2 Na (donc on a une seule valeur remplie) ET poids_obs==0 ,
  mettre dans la colonne type_poids_theorique_choisi "source 'a remplir selon la source' poids théo",
  mettre dans diag_poids :  "poids absent",
  mettre le poids theorique dans la colonne poids_final
  

Si il y a 2 Na (donc on a une seule valeur remplie) ET poids_obs < ou > (poids théo+50%)  ,
  mettre dans la colonne type_poids_theorique_choisi "source 'a remplir selon la source' poids théo",
  mettre dans diag_poids :  "poids aberrant",
  mettre le poids theorique dans la colonne poids_final


Si il y a 2 Na (donc on a une seule valeur remplie) ET poids_obs = (poids théo+50%)  ,
  mettre dans la colonne type_poids_theorique_choisi "poids obs",
  mettre dans diag_poids :  "poids ok",
  mettre le poids_obs dans la colonne poids_final
  

 Si on a <2 NA des poids theorique ET poids_obs==0 ET que 1 des origines des poids = "med" ,
  mettre dans la colonne type_poids_theorique_choisi "source X poids théo",
  mettre dans diag_poids :  "poids absent", 
  mettre le poids theorique dans la colonne poids_final

  if somme NA des colonnes 3 poids theorique >1 ET poids_obs < ou > (poids théo+50%)  ET que 1 des origines des poids = "med" ,
  mettre dans la colonne type_poids_theorique_choisi "source X poids théo",
  mettre dans diag_poids :  "poids aberrant", 
  mettre le poids theorique dans la colonne poids_final

  if somme NA des colonnes 3 poids theorique >1 ET poids_obs = (poids théo+50%)  ET que 1 des origines des poids = "med" ,
  mettre dans la colonne type_poids_theorique_choisi "poids obs",
  mettre dans diag_poids :  "poids ok", 
  mettre le poids_obs dans la colonne poids_final

  if somme NA des colonnes 3 poids theorique >1 ET poids_obs < ou > (poids théo+50%)  ET que >1 des origines des poids = "med" ,
  a voir plus tard 



  
  






# cas ou il y a un seul poids théorique disponible : 

correction_poids <- correction_poids%>%
  
  mutate(
    
    # nombre de NA dans les poids théoriques
    nb_na_theo = rowSums(is.na(across(c(Poids_theorique_SIH, Poids_theorique_FB, Poids_theorique_ObsBio)))),

    # poids théorique disponible (donc le seul non NA si nb_na_theo == 2)
    poids_theo_un_NA = case_when(
      nb_na_theo == 2 ~ coalesce(
        Poids_theorique_SIH,
        Poids_theorique_FB,
        Poids_theorique_ObsBio)),
   
    
    # quelle source est choisie
    source_theo_poids_choisie = case_when( ! is.na(Poids_theorique_SIH) ~"source SIH",
                                      ! is.na(Poids_theorique_FB) ~"source FB",
                                      ! is.na(Poids_theorique_ObsBio) ~"source obsbio"),
    
    # CAS 1 : 1 seul poids théorique dispo + poids observé = 0
    
    type_poids_theorique_choisi = case_when(
      Taille_MM ==0 ~" poids obs",
      
    # Prise unique d'individu : 
  
    nb_na_theo == 2 & Nb_de_prises == 1 & Poids_g == 0 ~
      paste(source_theo_poids_choisie, "poids théo"),
    
    nb_na_theo == 2 & Nb_de_prises == 1 & (Poids_g < 0.5 * poids_theo_un_NA | Poids_g > 1.5 * poids_theo_un_NA) ~
      paste(source_theo_poids_choisie, "poids théo"),
    
    nb_na_theo == 2 & Nb_de_prises == 1 & Poids_g >= 0.5 * poids_theo_un_NA & Poids_g <= 1.5 * poids_theo_un_NA ~
      "poids obs",
    
    # prise multiple d'individus
    
    nb_na_theo == 2 & Nb_de_prises > 1 & Poids_g == 0 ~
      paste(source_theo_poids_choisie, "poids théo"),
    
    # Poids total : poids_g = somme de tous les individus
    nb_na_theo == 2 & Nb_de_prises > 1 &
      (Poids_g / Nb_de_prises) >= 0.5 * poids_theo_un_NA &
      (Poids_g / Nb_de_prises) <= 1.5 * poids_theo_un_NA ~
      "poids observé est la somme",
    
    # Poids moyen : poids_g = poids d'un seul individu
    nb_na_theo == 2 & Nb_de_prises > 1 &
      Poids_g >= 0.5 * poids_theo_un_NA &
      Poids_g <= 1.5 * poids_theo_un_NA ~
      "poids observé est une moyenne",
    
    # Aberrant même ramené à l'individu (poids/N aberrant)
    nb_na_theo == 2 & Nb_de_prises > 1 &
      (Poids_g / Nb_de_prises) < 0.5 * poids_theo_un_NA |
      (Poids_g / Nb_de_prises) > 1.5 * poids_theo_un_NA ~
      paste(source_theo_poids_choisie, "poids théo - aberrant même en somme"),
    
    # Aberrant même comme poids moyen
    nb_na_theo == 2 & Nb_de_prises > 1 &
      Poids_g < 0.5 * poids_theo_un_NA |
      Poids_g > 1.5 * poids_theo_un_NA ~
      paste(source_theo_poids_choisie, "poids théo - aberrant même en moyenne"),
    
    TRUE ~ type_poids_theorique_choisi
    ),
    
    
    
    # diagnostique du poids : absent, observé, nouveau poids

    diag_poids = case_when(
      Taille_MM ==0 ~"pas de taille, poids obs gardé",
      
      # prise unique 

      nb_na_theo == 2 & Nb_de_prises == 1 & Poids_g == 0 ~ "poids absent",

      nb_na_theo == 2 & Nb_de_prises == 1 & (Poids_g < 0.5 * poids_theo_un_NA | Poids_g > 1.5 * poids_theo_un_NA) ~ "poids aberrant",

      nb_na_theo == 2 & Nb_de_prises == 1 & Poids_g >= 0.5 * poids_theo_un_NA & Poids_g <= 1.5 * poids_theo_un_NA ~ "poids ok",
      
      # prise multiple 
      
      nb_na_theo == 2 & Nb_de_prises > 1 & Poids_g == 0 ~ "poids absent",
      
      nb_na_theo == 2 & Nb_de_prises > 1 & (Poids_g / Nb_de_prises) >= 0.5 * poids_theo_un_NA & (Poids_g / Nb_de_prises) <= 1.5 * poids_theo_un_NA ~
        "poids donné total des prises",
      
      nb_na_theo == 2 & Nb_de_prises > 1 & Poids_g >= 0.5 * poids_theo_un_NA & Poids_g <= 1.5 * poids_theo_un_NA ~
        "poids donné moyen d'un individu",
      
      nb_na_theo == 2 & Nb_de_prises > 1 &  (Poids_g / Nb_de_prises) < 0.5 * poids_theo_un_NA | (Poids_g / Nb_de_prises) > 1.5 * poids_theo_un_NA ~
        "poids aberrant",
      
      nb_na_theo == 2 & Nb_de_prises > 1 & Poids_g < 0.5 * poids_theo_un_NA | Poids_g > 1.5 * poids_theo_un_NA ~
        "poids aberrant",
      
      TRUE ~ diag_poids
      

  
    ),

    poids_final = case_when(
      Taille_MM ==0~Poids_g, 
      
      # prise unique 

      nb_na_theo == 2 & Nb_de_prises == 1 & Poids_g == 0 ~ poids_theo_un_NA,
      
      nb_na_theo == 2 & Nb_de_prises == 1 & (Poids_g < 0.5 * poids_theo_un_NA | Poids_g > 1.5 * poids_theo_un_NA) ~ poids_theo_un_NA,
      
      nb_na_theo == 2 & Nb_de_prises == 1 & Poids_g >= 0.5 * poids_theo_un_NA & Poids_g <= 1.5 * poids_theo_un_NA ~ Poids_g,
      
      # prise multiple 
      
      # pas de poids -> poids théorique 
      
      nb_na_theo == 2 & Nb_de_prises > 1 & Poids_g == 0 ~ poids_theo_un_NA,
      
      # Poids total -> on ramène à l'individu
      nb_na_theo == 2 & Nb_de_prises > 1 &
        (Poids_g / Nb_de_prises) >= 0.5 * poids_theo_un_NA &
        (Poids_g / Nb_de_prises) <= 1.5 * poids_theo_un_NA ~
        Poids_g / Nb_de_prises,
      
      # Poids moyen -> on garde tel quel
      nb_na_theo == 2 & Nb_de_prises > 1 &
        Poids_g >= 0.5 * poids_theo_un_NA &
        Poids_g <= 1.5 * poids_theo_un_NA ~
        Poids_g,
      
      # Aberrant somme -> poids théorique
      nb_na_theo == 2 & Nb_de_prises > 1 &
        (Poids_g / Nb_de_prises) < 0.5 * poids_theo_un_NA |
        (Poids_g / Nb_de_prises) > 1.5 * poids_theo_un_NA ~
        poids_theo_un_NA,
      
      # Aberrant moyenne → poids théorique
      nb_na_theo == 2 & Nb_de_prises > 1 &
        Poids_g < 0.5 * poids_theo_un_NA |
        Poids_g > 1.5 * poids_theo_un_NA ~
        poids_theo_un_NA,
      
      
      TRUE ~ NA_real_
    )
  )

# cas ou il y a plusieurs poids théoriques 


# groupé 

correction_poids <- correction_poids %>%
  
  mutate(
    
    # --- compter les NA dans les poids théoriques, le nb de poids d'origine med
    
    # nb de NA dans les poids théoriques : si 2 NA -> alors 1 seul poids théorique, facile on le garde pour les comparaisons 
    nb_theo_non_na = rowSums(!is.na(across(c(Poids_theorique_SIH, Poids_theorique_FB, Poids_theorique_ObsBio)))),
    
    # nb de poids théoriques qui ont comme origine la Med : si 1 seul alors facile on le garde pour les comparaisons 
    nb_med = rowSums(across(c(origine_SIH, origine_fb, origine_obs)) == "med", na.rm = TRUE),
    
    # permet de faciliter quand un seul poids ou >1 mais 1 seul med 
    cas_equivalent_unique = nb_theo_non_na == 1 | (nb_theo_non_na > 1 & nb_med == 1),
    
    # --- Choix du poids théorique ---
    poids_theo_ref = case_when(
      
      # on a 1 ou + poids, mais 1 seul vient de la Med, alors on le garde : 
      cas_equivalent_unique & origine_SIH == "med" ~ Poids_theorique_SIH,
      cas_equivalent_unique & origine_fb == "med" ~ Poids_theorique_FB,
      cas_equivalent_unique & origine_obs == "med" ~ Poids_theorique_ObsBio,
      
      # on a 1 seul poids, qu'il vienne de la Med ou pas, on garde 
      cas_equivalent_unique & !is.na(Poids_theorique_SIH) ~ Poids_theorique_SIH,
      cas_equivalent_unique & !is.na(Poids_theorique_FB) ~ Poids_theorique_FB,
      cas_equivalent_unique & !is.na(Poids_theorique_ObsBio) ~ Poids_theorique_ObsBio
      
    ),
    
    # --- On écrit la source du poids théorique --- 
    
    source_theo = case_when(
      
      cas_equivalent_unique & origine_SIH == "med" ~ "SIH",
      cas_equivalent_unique & origine_fb == "med" ~ "FB",
      cas_equivalent_unique & origine_obs == "med" ~ "ObsBio",
      
      
      cas_equivalent_unique & !is.na(Poids_theorique_SIH) ~ "SIH",
      cas_equivalent_unique & !is.na(Poids_theorique_FB) ~ "FB",
      cas_equivalent_unique & !is.na(Poids_theorique_ObsBio) ~ "ObsBio"
    ),
    
    
    # --- On corrige ou non les poids : comparaison du poids observé (= renseigné dans l'appli) avec le théorique gardé ---
    
    poids_indiv = case_when(
      Nb_de_prises > 1 ~ Poids_g / Nb_de_prises,
      TRUE ~ Poids_g
    ),
    
    ratio = poids_indiv / poids_theo_ref,
    
    # --- 4. Diagnostic ---
    diag_poids = case_when(
      !cas_equivalent_unique ~ NA_character_,
      
      Poids_g == 0 ~ "absent",
      
      ratio >= 0.5 & ratio <= 1.5 ~ case_when(
        Nb_de_prises > 1 & Poids_g != poids_indiv ~ "total",
        TRUE ~ "ok"
      ),
      
      TRUE ~ "aberrant"
    ),
    
    # --- 5. Type ---
    type_poids = case_when(
      !cas_equivalent_unique ~ NA_character_,
      
      Poids_g == 0 ~ paste(source_theo, "théo"),
      
      ratio >= 0.5 & ratio <= 1.5 ~ case_when(
        Nb_de_prises > 1 & Poids_g != poids_indiv ~ "poids observé est la somme",
        Nb_de_prises > 1 ~ "poids observé est une moyenne",
        TRUE ~ "poids obs"
      ),
      
      TRUE ~ paste(source_theo, "théo - aberrant")
    ),
    
    # --- 6. Poids final ---
    poids_final = case_when(
      !cas_equivalent_unique ~ NA_real_,
      
      Poids_g == 0 ~ poids_theo_ref,
      
      ratio >= 0.5 & ratio <= 1.5 ~ case_when(
        Nb_de_prises > 1 & Poids_g != poids_indiv ~ poids_indiv,
        TRUE ~ Poids_g
      ),
      
      TRUE ~ poids_theo_ref
    )
    
  )

#######################################################################
# ca c'est super mais faut d'abord regarder les tailles complètement zinzin 

# code propre !!!!!
pro_data <- read_csv("data/pro/pro_data.csv")


SIH_poids_theo <- data_coherente_SIH_clean%>%
  dplyr::select(NomVernaculaire, NomScientifique, RTP_COEF_A_CM, RTP_COEF_B, origine)%>%
  distinct(NomVernaculaire, .keep_all = T)%>%
  rename(a_SIH=RTP_COEF_A_CM)%>%
  rename(b_SIH =RTP_COEF_B)%>%
  rename(origine_SIH = origine)

FB_poids_theo <- data_coherente_FB_clean%>%
  dplyr::select(NomVernaculaire, NomScientifique, a, b, origine)%>%
  distinct(NomVernaculaire, .keep_all = T)%>%
  rename(a_FB=a)%>%
  rename(b_FB = b)%>%
  rename(origine_FB = origine)


Obsbio_poids_theo <- data_coherente_obsbio_clean%>%
  mutate(origine = "med")%>%
  dplyr::select(NomVernaculaire, NomScientifique, a, b, origine)%>%
  distinct(NomVernaculaire, .keep_all = T)%>%
  rename(a_obs = a) %>%
  rename(b_obs = b)%>%
  rename(origine_obs = origine)



#Poids_RTP=a*Taille_CM^b

correction_poids<- pro_data%>%
  left_join(Obsbio_poids_theo)%>%
  left_join(SIH_poids_theo)%>%
  left_join(FB_poids_theo)


correction_poids<- correction_poids%>%
  mutate(Poids_theorique_SIH = a_SIH*Taille_MM ^b_SIH)%>%
  mutate(Poids_theorique_FB= a_FB * (Taille_MM/10)^b_FB)%>%
  mutate(Poids_theorique_ObsBio = a_obs* (Taille_MM/10)^b_obs)

correction_poids<- correction_poids%>%
  dplyr::select(Id_Prise ,Nb_de_prises, NomScientifique, Taille_MM, Taille_max_mm , Poids_g, a_obs, b_obs, origine_obs, a_SIH, b_SIH, origine_SIH, a_FB, b_FB, origine_FB, Poids_theorique_SIH, Poids_theorique_FB, Poids_theorique_ObsBio)

###################### 1 : regarder si obsbio est une ref pour les ratios

obsBio_reference <- RTP_obsbio %>%
  group_by(NomScientifique) %>%
  summarise(
    # on récupere la taille minimale et maximale etudiée pour les ratio
    Taille_min_ratio = min(LENGTH_TOTAL_VALUE),
    Taille_max_ratio = max(LENGTH_TOTAL_VALUE), 
    
    # quel ecart entre les min et max
    ecart_taille = Taille_max_ratio - Taille_min_ratio, 
    # si min = 10 et max = 20, alors 10 classes d'age, et donc il faut MINIMUM 3*10 individus pour que ça soit robuste 
    
    n_individus_requis = 3 * round(ecart_taille),
    n_individus_obs = n(), 
    
    # est ce que le groupe est suffisamment robuste 
    ratio_robuste = n_individus_obs >=n_individus_requis
    
    
  )



correction_poids <- correction_poids %>%
  left_join(
    obsBio_reference %>% dplyr::select(NomScientifique, Taille_min_ratio, Taille_max_ratio, ratio_robuste),
    by = "NomScientifique"
  )

##################### 2 : faire les corrections de poids si necessaire 
options(scipen = 999)

correction_poids <- correction_poids%>%
  
  mutate(
    
    # --- compter les NA dans les poids théoriques, le nb de poids d'origine med
    
    # nb de NA dans les poids théoriques : si 2 NA -> alors 1 seul poids théorique, facile on le garde pour les comparaisons 
    nb_theo_non_na = rowSums(!is.na(across(c(Poids_theorique_SIH, Poids_theorique_FB, Poids_theorique_ObsBio)))),
    
    # nb de poids théoriques qui ont comme origine la Med : si 1 seul alors facile on le garde pour les comparaisons 
    nb_med = rowSums(across(c(origine_SIH, origine_FB, origine_obs)) == "med", na.rm = TRUE),
    
    # permet de faciliter quand un seul poids ou >1 mais 1 seul med 
    cas_equivalent_unique = nb_theo_non_na == 1 | (nb_theo_non_na > 1 & nb_med == 1),
    
    # --- Choix du poids théorique ---
    poids_theo_ref = case_when(
      
      # on a 1 ou + poids, mais 1 seul vient de la Med, alors on le garde : 
      cas_equivalent_unique & origine_SIH == "med" ~ Poids_theorique_SIH,
      cas_equivalent_unique & origine_FB == "med" ~ Poids_theorique_FB,
      cas_equivalent_unique & origine_obs == "med" ~ Poids_theorique_ObsBio,
      
      # on a 1 seul poids, qu'il vienne de la Med ou pas, on garde 
      cas_equivalent_unique & !is.na(Poids_theorique_ObsBio) ~ Poids_theorique_ObsBio, 
      cas_equivalent_unique & !is.na(Poids_theorique_FB) ~ Poids_theorique_FB,
      cas_equivalent_unique & !is.na(Poids_theorique_SIH) ~ Poids_theorique_SIH,
      
      # on a plusieurs poids d'origine Med : priorité ObsBio > FB > SIH
      
      ## cas ou la taille observée est dans l'echelle d'etude de obsbio, et il y a assez de données pour que ça soit robuste. 
      !cas_equivalent_unique & origine_obs== "med" & 
        Taille_MM >= Taille_min_ratio & Taille_MM <= Taille_max_ratio & ratio_robuste==TRUE ~Poids_theorique_ObsBio, 
      
      ## cas ou obsbio pas retenu (hors tailles dispo ou pas robuste) : on prend FB
      !cas_equivalent_unique & !is.na(Poids_theorique_FB) ~Poids_theorique_FB, 
      
      ## si rien de tout ça, on prend SIH
      !cas_equivalent_unique & !is.na(Poids_theorique_SIH) ~Poids_theorique_SIH
      
    ),
    
    # --- On écrit la source du poids théorique --- 
    
    source_theo = case_when(
      
      cas_equivalent_unique & origine_SIH == "med" ~ "SIH",
      cas_equivalent_unique & origine_FB == "med" ~ "FB",
      cas_equivalent_unique & origine_obs == "med" ~ "ObsBio",
      
      cas_equivalent_unique & !is.na(Poids_theorique_ObsBio) ~ "ObsBio",
      cas_equivalent_unique & !is.na(Poids_theorique_FB) ~ "FB",
      cas_equivalent_unique & !is.na(Poids_theorique_SIH) ~ "SIH",
      
      
      !cas_equivalent_unique & origine_obs== "med" & 
        Taille_MM >= Taille_min_ratio & Taille_MM <= Taille_max_ratio & ratio_robuste==TRUE ~"ObsBio", 
  
      !cas_equivalent_unique & !is.na(Poids_theorique_FB) ~"FB", 
      
      !cas_equivalent_unique & !is.na(Poids_theorique_SIH) ~ "SIH"
    ),
    
    
    # --- On corrige ou non les poids : comparaison du poids observé (= renseigné dans l'appli) avec le théorique gardé ---
    # --- ratio testé dans les deux hypothèses ---
    ratio_si_total   = (Poids_g / Nb_de_prises) / poids_theo_ref,  # hypothèse : poids_g = total
    ratio_si_moyenne = Poids_g / poids_theo_ref,                    # hypothèse : poids_g = individu
    
    # --- Diagnostic ---
    diag_poids = case_when(
      
      # Taille aberrante : on ne fait aucun calcul
      (Taille_MM > Taille_max_mm * 1.1 & !is.na(Taille_max_mm)) ~"taille aberrante, poids obs gardé",
      
      # si taille a peut pres ok, alors on regarde en détail
      Nb_de_prises==0 ~"pas de capture",
      Taille_MM == 0   ~ "pas de taille, poids obs gardé",
      Poids_g == 0     ~ "poids absent",
      Nb_de_prises == 1 & ratio_si_moyenne >= 0.5 & ratio_si_moyenne <= 1.5 ~ "poids obs ok",
      Nb_de_prises == 1 & ratio_si_moyenne <0.5  | ratio_si_moyenne>1.5 ~ "poids obs aberrant",
      
      # Nb_de_prises > 1 : on teste d'abord l'hypothèse "total"
      ratio_si_total   >= 0.5 & ratio_si_total   <= 1.5 ~ "poids donné total des prises",
      ratio_si_moyenne >= 0.5 & ratio_si_moyenne <= 1.5 ~ "poids donné moyen d'un individu",
      TRUE ~ "poids aberrant"
    ),
    
    # --- Poids final ---
    poids_final = case_when(
      (Taille_MM > Taille_max_mm * 1.1 & !is.na(Taille_max_mm)) ~Poids_g,
      
      Nb_de_prises==0 ~ NA_real_,
      Taille_MM == 0   ~ Poids_g,
      Poids_g == 0     ~ poids_theo_ref,
      Nb_de_prises == 1 & ratio_si_moyenne >= 0.5 & ratio_si_moyenne <= 1.5 ~ Poids_g,
      Nb_de_prises == 1                                                       ~ poids_theo_ref,
      
      ratio_si_total   >= 0.5 & ratio_si_total   <= 1.5 ~ Poids_g / Nb_de_prises, # on ramène à l'individu
      ratio_si_moyenne >= 0.5 & ratio_si_moyenne <= 1.5 ~ Poids_g,                # déjà un individu
      TRUE ~ poids_theo_ref
    ))
    
    


data_poids_corrige<- pro_data%>%
  left_join(correction_poids %>% dplyr::select(Id_Prise, poids_final, diag_poids, poids_theo_ref, source_theo), by= "Id_Prise")

write_csv(pro_data,"data/pro/data_poids_corrige.csv")






    



########### cpue calcul : ###############


data_poids_corrige<- read.csv("data/pro/data_poids_corrige.csv")

data_poids_corrige<- data_poids_corrige%>%
  mutate(Heure_debut_session = as.POSIXct(Heure_debut_session, 
                                          format = "%Y-%m-%dT%H:%M:%SZ", 
                                          tz = "UTC"))

# on regarde les captures qui parraissent apartenir a la même sortie
data_poids_corrige <- data_poids_corrige %>%
  mutate(
    ID_sortie_corrige = case_when(
      Id_Sortie != 0 ~ as.character(Id_Sortie),
      TRUE ~ paste(Id_Abonné, Heure_debut_session, Mode_peche, Nom_ZoneDePeche, sep = "_")
    )
  )

# test<-data_poids_corrige %>%
#   group_by(ID_sortie_corrige) %>%
#        summarise(
#              nb_lignes       = n(),
#              especes         = paste(unique(NomScientifique), collapse = ", "),
#              pecheur         = first(Id_Abonné),
#              date            = first(Heure_debut_session),
#              zone            = first(Nom_ZoneDePeche),
#              mode            = first(Mode_peche),
#              duree_heures    = first(Durée_de_la_session),
#              .groups = "drop"
#          ) %>%
#        arrange(ID_sortie_corrige)

# une espèce donnée capturée lors d’une sortie donnée avec une technique donnée

cpue_sp <- data_poids_corrige %>%
  group_by(ID_sortie_corrige, NomScientifique, Mode_peche) %>%
  summarise(
    nb_captures    = sum(Nb_de_prises,  na.rm = TRUE),
    poids_captures = sum(poids_final,   na.rm = TRUE),
    duree_heures   = first(Durée_de_la_session)/60 , # car duree en minute et on veut en heures
    nb_pecheurs    = first(Nb_de_pêcheurs),
    .groups = "drop"
  ) %>%
  mutate(
    effort     = duree_heures * nb_pecheurs,
    cpue_nb    = nb_captures    / effort,
    cpue_poids = poids_captures / effort
  )



# CPUE globales : par sortie et par mode de pêche. Comme ça on prend en compte les sorties sans captures. on peut pas faire par sp et prendre les non captures en compte car pas d'sp cible
  
  cpue_tot_duree <- data_poids_corrige %>%
    group_by(ID_sortie_corrige, Mode_peche) %>%
    summarise(
      nb_captures    = sum(Nb_de_prises,  na.rm = TRUE),  # les 0 sont inclus 
      poids_captures = sum(poids_final,   na.rm = TRUE), # en gramme 
      duree_heures   = first(Durée_de_la_session)/60 , # car duree en minute et on veut en heures
      nb_pecheurs    = first(Nb_de_pêcheurs),
      zone           = first(Nom_ZoneDePeche),
      date           = first(Heure_debut_session),
      .groups = "drop"
    ) %>%
    mutate(
      effort     = duree_heures * nb_pecheurs,
      cpue_nb    = nb_captures    / effort,
      cpue_poids = poids_captures / effort # gramme 
    )

#CPUE par session de pêche (on prend pas en compte la durée car peut être zinz)
  cpue_tot_sortie <- data_poids_corrige %>%
    group_by(ID_sortie_corrige, Mode_peche) %>%
    summarise(
      nb_captures    = sum(Nb_de_prises,  na.rm = TRUE),
      poids_captures = sum(poids_final,   na.rm = TRUE),
      nb_pecheurs    = first(Nb_de_pêcheurs),
      zone           = first(Nom_ZoneDePeche),
      date           = first(Heure_debut_session),
      mois           = first(mois),
      .groups = "drop"
    ) %>%
    mutate(
      # effort = nb de pêcheurs uniquement (1 sortie = 1 unité d'effort)
      cpue_sortie_nb    = nb_captures    / nb_pecheurs,  # ind/pêcheur/sortie
      cpue_sortie_poids = poids_captures / nb_pecheurs,  # g/pêcheur/sortie
    )
  
  

# on a aucun moyen de verifier les durées de sorties, donc on peut regarder si des extremes, sinon il faudra faire les CPUE par jour et non par heures. 
cpue_tot %>% 
  arrange(desc(cpue_poids)) %>% 
  select(ID_sortie_corrige, Mode_peche, cpue_poids, nb_captures, effort) %>% 
  head(10)
# comparaison des CPUE : 


  # ── 1. STATISTIQUES DESCRIPTIVES ───────────────────────────────────────────
  
  stats_mode <- cpue_tot %>%
    group_by(Mode_peche) %>%
    summarise(
      n_sorties      = n(),
      cpue_nb_moy    = mean(cpue_nb,    na.rm = TRUE),
      cpue_nb_med    = median(cpue_nb,  na.rm = TRUE),
      cpue_nb_sd     = sd(cpue_nb,      na.rm = TRUE),
      cpue_poids_moy = mean(cpue_poids, na.rm = TRUE),
      cpue_poids_med = median(cpue_poids, na.rm = TRUE),
      cpue_poids_sd  = sd(cpue_poids,   na.rm = TRUE),
      .groups = "drop"
    )
  
  stats_zone <- cpue_tot %>%
    group_by(zone) %>%
    summarise(
      n_sorties      = n(),
      cpue_nb_moy    = mean(cpue_nb,    na.rm = TRUE),
      cpue_nb_med    = median(cpue_nb,  na.rm = TRUE),
      cpue_nb_sd     = sd(cpue_nb,      na.rm = TRUE),
      cpue_poids_moy = mean(cpue_poids, na.rm = TRUE),
      .groups = "drop"
    )
  
  stats_temps <- cpue_tot %>%
    group_by(annee = year(date), mois = month(date)) %>%
    summarise(
      cpue_nb_moy    = mean(cpue_nb,    na.rm = TRUE),
      cpue_poids_moy = mean(cpue_poids, na.rm = TRUE),
      .groups = "drop"http://127.0.0.1:8163/graphics/plot_zoom_png?width=1154&height=785
    )
  
  # ── 2. GRAPHIQUES ──────────────────────────────────────────────────────────
  
  # CPUE (nb) par mode de pêche : echelle log, toutes AMP confondues 
  
  ggplot(cpue_tot_sortie, aes(x = Mode_peche, y = cpue_sortie_nb +1, fill = Mode_peche)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.3) +
    scale_y_log10() +    
    labs(title = "CPUE (nb individus) par mode de pêche (log)",
         x = "Mode de pêche", y = "CPUE log(nb+1) (ind/pêcheur/sortie)") +
    theme_bw() +
    theme(legend.position = "none")
  
  
  # CPUE (poids) par mode de pêche je peux faire 
  
  ggplot(cpue_tot_sortie, aes(x = Mode_peche, y = cpue_sortie_poids + 1, fill = Mode_peche)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.3) +
    scale_y_log10() +                   
    labs(title = "CPUE (g/pêcheur/sortie) selon mode de pêche (échelle log)",
         x = "Mode de pêche", y = "CPUE log(poids+1) (g/pêcheur/sortie)") +
    theme_bw() +
    theme(legend.position = "none")
  
  
  # CPUE par zone
  ggplot(cpue_tot_sortie, aes(x = zone, y = cpue_nb, fill = zone)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.3) +
    labs(title = "CPUE (nb individus) par zone de pêche",
         x = "Zone", y = "CPUE (ind/pêcheur.heure)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  # zone graph mieux 
  
  ggplot(cpue_tot, aes(x = zone, y = cpue_nb, fill = zone)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.3) +
    labs(title = "CPUE (nb individus) par zone de pêche",
         x = "Zone", y = "CPUE (ind/pêcheur.heure)") +
    
    scale_x_discrete(labels = \(x) str_wrap(x, width = 12)) +
    
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "none"
    )
  
  # Evolution temporelle
  ggplot(stats_temps, aes(x = as.Date(paste(annee, mois, "01", sep = "-")), 
                          y = cpue_nb_moy)) +
    geom_line() +
    geom_point() +
    labs(title = "Evolution temporelle de la CPUE moyenne",
         x = "Date", y = "CPUE moyenne (ind/pêcheur.heure)") +
    theme_bw()
  
  
  # ── 3. TESTS STATISTIQUES ──────────────────────────────────────────────────
  

    
    # Kruskal-Wallis (non paramétrique)
    test_mode <- kruskal.test(cpue_sortie_poids ~ Mode_peche, data = cpue_tot_sortie)
    print(test_mode)
    
    # post-hoc
    library(dunn.test)
    dunn.test(cpue_tot_sortie$cpue_sortie_poids, cpue_tot_sortie$Mode_peche, method = "bonferroni")

  
  # Test entre zones
  kruskal.test(cpue_sortie_poids ~ zone, data = cpue_tot_sortie)
  
  
  
  
  
# CPUE nb/pêcheur/ session ~mode  
  
  ggplot(cpue_tot_sortie, aes(x = Mode_peche, y = cpue_sortie_nb +1, fill = Mode_peche)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.3) +
    scale_y_log10() +    
    labs(title = "CPUE (nb individus) par mode de pêche (log)",
         x = "Mode de pêche", y = "CPUE log(nb+1) (ind/pêcheur/sortie)") +
    theme_bw() +
    theme(legend.position = "none")
  
  
  
  test_mode <- kruskal.test(cpue_sortie_nb ~ Mode_peche, data = cpue_tot_sortie)
  print(test_mode)
  
  # post-hoc
  library(dunn.test)
  dunn.test(cpue_tot_sortie$cpue_sortie_nb, cpue_tot_sortie$Mode_peche, method = "bonferroni")
  
  
  
  
# CPUE g/pecheur/session ~mode  
  
  ggplot(cpue_tot_sortie, aes(x = Mode_peche, y = cpue_sortie_poids + 1, fill = Mode_peche)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.3) +
    scale_y_log10() +                   
    labs(title = "CPUE (g/pêcheur/sortie) selon mode de pêche (échelle log)",
         x = "Mode de pêche", y = "CPUE log(poids+1) (g/pêcheur/sortie)") +
    theme_bw() +
    theme(legend.position = "none")
  
  # Kruskal-Wallis (non paramétrique)
  test_mode <- kruskal.test(cpue_sortie_poids ~ Mode_peche, data = cpue_tot_sortie)
  print(test_mode)
  
  # post-hoc
 
  dunn.test(cpue_tot_sortie$cpue_sortie_poids, cpue_tot_sortie$Mode_peche, method = "bonferroni")
  
  
  
  
# CPUE nb/pecheur/session ~AMP 

  ggplot(cpue_tot_sortie, aes(x = zone, y = cpue_sortie_nb +1 , fill = zone)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.3) +
    scale_y_log10()+
    labs(title = "CPUE (nb individus) par zone de pêche",
         x = "Zone", y = "CPUE (ind/pêcheur/session)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")

  # Kruskal-Wallis (non paramétrique)
  test_zone_nb<- kruskal.test(cpue_sortie_nb ~ zone, data = cpue_tot_sortie)
  print(test_zone_nb)
  
  # post-hoc
  result_zone <- dunn.test(cpue_tot_sortie$cpue_sortie_nb, cpue_tot_sortie$zone, method = "bonferroni")
  
  # tableau propre avec noms complets
tab <-  data.frame(
    comparaison = result_zone$comparisons,
    p_value     = round(result_zone$P.adjusted, 4),
    significatif = result_zone$P.adjusted < 0.05
  ) %>% arrange(p_value)
  


# CPUE poids/pecheur/session ~AMP 
ggplot(cpue_tot_sortie, aes(x = zone, y = cpue_sortie_poids +1 , fill = zone)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_y_log10()+
  labs(title = "CPUE (poids en gramme) par zone de pêche",
       x = "Zone", y = "CPUE log(poids+1) (g/pêcheur/sortie)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Kruskal-Wallis (non paramétrique)
test_zone_pd<- kruskal.test(cpue_sortie_poids ~ zone, data = cpue_tot_sortie)
print(test_zone_pd)

# post-hoc
result_zone <- dunn.test(cpue_tot_sortie$cpue_sortie_poids, cpue_tot_sortie$zone, method = "bonferroni")

# tableau propre avec noms complets
tab <-  data.frame(
  comparaison = result_zone$comparisons,
  p_value     = round(result_zone$P.adjusted, 4),
  significatif = result_zone$P.adjusted < 0.05
) %>% arrange(p_value)





# CPUE g/pêcheurs/session pêche ~ AMP et Mode de pêche 
 

# modèle linéaire avec interaction zone x mode
# (sur les données log-transformées car non normales)
cpue_tot_sortie <- cpue_tot_sortie %>%
  mutate(log_cpue_poids = log(cpue_sortie_poids + 1))

# test sans interaction parce qu'on a pas tous les modes dans toutes les zones (pas de sous marin dans banyuls)
library(car)
model <- lm(log_cpue_poids ~ zone + Mode_peche, data = cpue_tot_sortie)
Anova(model, type = "III")

library(emmeans)
emmeans(model, pairwise ~ zone,       adjust = "bonferroni")
emmeans(model, pairwise ~ Mode_peche, adjust = "bonferroni")



# evolution temporelle des CPUE : 

# Agrégation mensuelle toutes espèces et modes confondus
effort_mensuel <- cpue_tot_sortie %>%
  group_by(mois) %>%
  summarise(
    nb_sorties    = n_distinct(ID_sortie_corrige),          # nombre de sessions
    nb_pecheurs   = sum(nb_pecheurs, na.rm = TRUE), # effort total
    cpue_moy      = mean(cpue_sortie_nb, na.rm = TRUE),
    cpue_median   = median(cpue_sortie_nb, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(mois = factor(mois, levels = ordre_mois))

# Graphique : double axe — CPUE moyenne + nombre de sorties
ggplot(effort_mensuel, aes(x = mois)) +
  geom_col(aes(y = nb_sorties), fill = "#90CAF9", alpha = 0.6) +        # barres = effort
  geom_line(aes(y = cpue_moy * max(nb_sorties) / max(cpue_moy),         # CPUE rescalée
                group = 1), color = "#E53935", linewidth = 1.2) +
  geom_point(aes(y = cpue_moy * max(nb_sorties) / max(cpue_moy)),
             color = "#E53935", size = 3) +
  scale_y_continuous(
    name = "Nombre de sorties",
    sec.axis = sec_axis(
      ~ . * max(effort_mensuel$cpue_moy) / max(effort_mensuel$nb_sorties),
      name = "CPUE moyenne (ind/pêcheur/sortie)"
    ),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(title = "Évolution mensuelle de l'effort de pêche - 2025",
       subtitle = "Toutes espèces et modes confondus",
       x = NULL) +
  theme_minimal() +
  theme(plot.title  = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.y.left  = element_text(color = "#1565C0"),
        axis.title.y.right = element_text(color = "#E53935"))
