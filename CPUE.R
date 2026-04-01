# cpue 

data_PNMGL <- read_csv("data/pro/data_PNMGL.csv")

data_PNMGL<- data_PNMGL%>%
  mutate(Nb_de_prises = if_else (Nb_de_prises>100 , 1, Nb_de_prises))%>%
  mutate(Nb_de_prises = if_else (NomVernaculaire != "Maquereau commun" & NomVernaculaire != "Sardine commune" & NomVernaculaire != "Maquereau espagnol"  & Nb_de_prises >30 , 1, Nb_de_prises)) %>%
  mutate(Nb_de_pêcheurs = if_else (Nb_de_pêcheurs <50 | Nb_de_pêcheurs >= 0, 1, Nb_de_pêcheurs)) %>%
  mutate(Durée_de_la_session= replace(Durée_de_la_session, Durée_de_la_session==0, NA))




cpue <- data_PNMGL %>%
  mutate(CPUE = Poids_g / Durée_de_la_session) %>%
  group_by(mois) %>%
  summarise(
    CPUE_moyenne = mean(CPUE, na.rm = TRUE),
    .groups = "drop"
  )

cpue_data <- landings_data %>% 
  # Add colomn for kilograms by dividing gram column by 1000
  mutate(Weight_kg = Weight_g / 1000) %>%
  # Group by year and Trip ID so that you can calculate CPUE for every trip in every year
  group_by(Year,Trip_ID) %>% 
  # For each year and trip ID, calculate the CPUE for each trip by dividing the sum of the catch, converted from grams to kilograms, by the trip by the number of fishing hours
  summarize(Trip_CPUE = sum(Weight_kg) / mean(Effort_Hours)) %>% 
  # Next, just group by year so we can calculate median CPUE for each year across all trips in the year
  group_by(Year) %>% 
  # Calculate median CPUE for each year
  summarize(Median_CPUE_kg_hour = median(Trip_CPUE))


