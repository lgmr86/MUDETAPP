%>% arrange(as.numeric(substr(annee_n,3,4)))
%>% arrange(as.numeric(substring(annee_n,3)))

2:(length(df_pt)-3)

2:(length(df_pt)-3)

tx_pass <- function(df_pt,tx){
  na_df_pt <- as.data.frame(which(is.na(df_pt), arr.ind = TRUE)) %>% mutate(colone_cible= as.numeric(col)-1)
  for (row in 1:nrow(na_df_pt)){na_df_pt$'taux_moyen'[row]<- ifelse(is.na(t_moy(row,df_pt,na_df_pt,tx)),na_df_pt$taux_moyen[row-1],t_moy(row,df_pt,na_df_pt,tx))}
  for (row in 1:nrow(na_df_pt)){na_df_pt$'value'[row]<- ifelse( is.na(df_pt[as.numeric(na_df_pt$row[row]-1),na_df_pt$col[row]])
                                                                ,as.numeric(na_df_pt$value[row-1])*na_df_pt$taux_moyen[row],
                                                                as.numeric(df_pt[as.numeric(na_df_pt$row[row]-1),na_df_pt$col[row]])*na_df_pt$taux_moyen[row])}
  for (row in 1:nrow(na_df_pt)){na_df_pt$'nom_colonne'[row]<- colnames(df_pt)[na_df_pt$col[row]]}
  for (row in 1:nrow(na_df_pt)){na_df_pt$'annee_n'[row]<- df_pt[na_df_pt$row[row],1] }
  tx_list <- na_df_pt %>% summarise(taux_moyen) %>% distinct()
  tx_list <- as.data.frame(t(append(as.vector(tx_list$taux_moyen),c("Taux de Passage Moyen",1),after = 0)))
  i <- list(2:length(tx_list))[[1]]
  tx_list[,i] <- apply(tx_list[,i], 2, function(x) as.numeric(as.character(x)))
  na_df_pt <- na_df_pt %>% select(value,nom_colonne,annee_n) %>% spread( nom_colonne, value) %>% arrange(as.numeric(substring(annee_n,3))) %>% replace(is.na(.),0)
}

select("Exercice"=exercice,"Vue"=annee_n,"Nom"=nom, "Raison"=raison, "Type"=type, "CP"=cp, "Département"=dépsinistré, "Cause"=cause, "Numéro de Sinistre" = numero, "Numéro de dossier"=dossier,"Date de survenance"=dtsurv, "Payé"=paye, "Réserve"=provisions,"Total"=total)