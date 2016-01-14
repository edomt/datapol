library(dplyr)

deputes <- read.csv('http://www.nosdeputes.fr/synthese/data/csv', sep = ";", encoding = "UTF-8", stringsAsFactors = F)
deputes$num_deptmt <- as.integer(deputes$num_deptmt)

# Retrait des députés outre-mer et sélection des variables
deputes <- filter(deputes, num_deptmt <= 95) %>%
    select(nom, sexe, date_naissance, num_deptmt, parti_ratt_financier, nb_mandats, semaines_presence, commission_presences, commission_interventions, hemicycle_interventions, hemicycle_interventions_courtes, amendements_signes, amendements_adoptes, rapports, propositions_ecrites, propositions_signees, questions_ecrites, questions_orales)

# Conversion en variables centrées réduites
deputes$semaines_presence_S <- scale(deputes$semaines_presence)[,1]
deputes$commission_presences_S <- scale(deputes$commission_presences)[,1]
deputes$commission_interventions_S <- scale(deputes$commission_interventions)[,1]
deputes$hemicycle_interventions_S <- scale(deputes$hemicycle_interventions)[,1]
deputes$hemicycle_interventions_courtes_S <- scale(deputes$hemicycle_interventions_courtes)[,1]
deputes$amendements_signes_S <- scale(deputes$amendements_signes)[,1]
deputes$amendements_adoptes_S <- scale(deputes$amendements_adoptes)[,1]
deputes$rapports_S <- scale(deputes$rapports)[,1]
deputes$propositions_ecrites_S <- scale(deputes$propositions_ecrites)[,1]
deputes$propositions_signees_S <- scale(deputes$propositions_signees)[,1]
deputes$questions_ecrites_S <- scale(deputes$questions_ecrites)[,1]
deputes$questions_orales_S <- scale(deputes$questions_orales)[,1]

# Calcul du score
deputes <- mutate(deputes, score = semaines_presence_S +
                      commission_presences_S +
                      commission_interventions_S +
                      hemicycle_interventions_S +
                      hemicycle_interventions_courtes_S +
                      amendements_signes_S +
                      amendements_adoptes_S +
                      rapports_S +
                      propositions_ecrites_S +
                      propositions_signees_S +
                      questions_ecrites_S +
                      questions_orales_S)

# Score positif pour tout le monde, puis de 0 à 100
deputes$score <- deputes$score + abs(min(deputes$score))
deputes$score <- deputes$score * 100 / max(deputes$score)

# Regroupement des partis politiques minoritaires
deputes[deputes$parti_ratt_financier == "Forces de Gauche", "parti_ratt_financier"] <- "Parti communiste Français"
deputes[deputes$parti_ratt_financier %in% c("Union des Radicaux, centristes, indépendants et démocrates", "Association PSLE - Nouveau Centre"), "parti_ratt_financier"] <- "Union des démocrates et indépendants"
deputes[deputes$parti_ratt_financier == "Debout la France (Debout la République)", "parti_ratt_financier"] <- "Debout la France"

# Ensemble des scores
scores <- deputes %>% arrange(desc(score))
write.csv(scores, "scores.csv", row.names = F, fileEncoding = "latin1")

# Score médian par parti
partis <- deputes %>% filter(!parti_ratt_financier %in% c("Non rattachée", "Non rattaché", "Le Centre pour la France")) %>% group_by(parti_ratt_financier) %>% summarise(score = median(score)) %>% arrange(desc(score)) %>% mutate(score = round(score, 1)) 
write.csv(partis, "partis.csv", row.names = F, fileEncoding = "latin1")

# Top 15
top <- deputes %>% select(nom, parti_ratt_financier, score) %>% arrange(desc(score)) %>% mutate(score = round(score, 1)) %>% head(15)
write.csv(top, "top.csv", row.names = F, fileEncoding = "latin1")

# Bottom 15
bottom <- deputes %>% select(nom, parti_ratt_financier, score) %>% arrange(score) %>% mutate(score = round(score, 1)) %>% head(15)
write.csv(bottom, "bottom.csv", row.names = F, fileEncoding = "latin1")
