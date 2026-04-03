Ref <- read_excel("data/raw/Regles_techniques_engins_referentiels_Validee.xlsx")
pro_data <- read_csv("data/pro/pro_data.csv")

Ref <- Ref%>%
  mutate(`Engins associés` = str_split(`Engins associés`, " ; "))%>%
  unnest(`Engins associés`)


technique <- pro_data%>%
  distinct(Technique)%>%
  pull(Technique)


engin <- pro_data%>%
  distinct(Engin)%>%
  pull(Engin)

combinaison_tech_engin <- pro_data%>%
  distinct(Technique, Engin) %>%
  arrange(Technique, Engin)


# on fait les tri accents et espaces 

combinaison_tech_engin <- combinaison_tech_engin%>%
  mutate(Engin = stri_trans_general(Engin, "Latin-ASCII") , 
         Technique = stri_trans_general(Technique, "Latin-ASCII"))

Ref <- Ref%>%
  mutate(`Engins associés` = stri_trans_general(`Engins associés`, "Latin-ASCII"), 
         Technique = stri_trans_general(Technique, "Latin-ASCII"))


# on reconnait les NA -> on remplace les NA par des "AUCUN"
combinaison_tech_engin <-combinaison_tech_engin%>%
  mutate(Engin = replace_na(Engin, "AUCUN"))

Ref<- Ref %>%
  mutate(`Engins associés`= replace_na(`Engins associés`, "AUCUN"))%>%
  mutate(`Engins associés`= replace(`Engins associés`, `Engins associés`=="NA", "AUCUN"))

# lignes dans le tableau combinaison 



combinaison_tech_engin <- combinaison_tech_engin %>%
  left_join(Ref %>%
            select( Technique, `Engins associés`, ENGF_COD_associe), 
            by = c("Technique", "Engin"="Engins associés" ))

write.csv2(combinaison_tech_engin, "data/pro/combinaison_tech_engin.csv", fileEncoding = "Latin1")


df <- as.data.frame(matrix(nrow = 0, ncol = length(technique)))
colnames(df) <- make.names(technique, unique = TRUE)

df


write.csv2(df, "data/pro/df.csv", fileEncoding = "Latin1")



combinaison_tech_engin_match <- fuzzy_left_join(
  combinaison_tech_engin,
  Regles_techniques_engins_referentiels_Validee_2,
  by = c("Technique" = "Technique",
         "Engin" = "Engin"),
  match_fun = list(`==`, function(x, y) str_detect(x, fixed(y)))
)











































