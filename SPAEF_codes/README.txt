Working directory = workspace Obelix

1. Run optimisation_poids_crossvalidation.R

Script qui permet de lancer le grid search.

models_list.rdata + load_tas_spaef.rdata : permettent de charger dans l'environnement les températures des modèles utilisés pour le graph cut.

Après l'algo de l'AB-swap (l.111), création de la matrice de labels (mat_gco_label_world), ainsi que de la matrice des températures (tas_GC)

Fonction SPAEF (l. 134) :
	- mise en place d'une dataframe pour les histogrames
	- création des 3 arguments alpha, beta et gamma
	- liste spaef comprenant le spaef + les 3 arguments

Liste sauvegardée (weight_spaef) comprenant la liste SPAEF + matrice des labels pour chaque test


2. Run spaef_test.R

Script permettant de générer les cartes et les plots SPAEF + MSE

spaef_env.rdata : l'environnement du script précédent

weight_mat_p0_2038.rdata : résultat du grid search

bias_tas_gc.rdata & bias_tas_mme.rdata : matrices de biais de temp et de temp pour le graph cut et le MME (ensemble multi-modèle) --> réalisé avec le script bias_list_mme.R (idem pour graph cut)

