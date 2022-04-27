## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE
)

## ----setup, warning=FALSE-----------------------------------------------------
library("oottest")
library("xtable")

## ---- eval = FALSE------------------------------------------------------------
#  ?data_two_action_games

## -----------------------------------------------------------------------------
vuong_table_hdg <- vuong_matrix(data_two_action_games[,1:10], predictions_two_action_games[,1:10,])
vuong_table_mp1 <- vuong_matrix(data_two_action_games[,11:15], predictions_two_action_games[,11:15,])
vuong_table_mp2 <- vuong_matrix(data_two_action_games[,16:20], predictions_two_action_games[,16:20,])
vuong_table_ac <- vuong_matrix(data_three_action_games[,1:10], predictions_three_action_games[,1:10,])
vuong_table_rsp1 <- vuong_matrix(data_three_action_games[,11:15], predictions_three_action_games[,11:15,])
vuong_table_rsp2 <- vuong_matrix(data_three_action_games[,16:20], predictions_three_action_games[,16:20,])

## ---- echo = FALSE, results = 'asis'------------------------------------------
knitr::kable(round(vuong_table_hdg,2), caption="Hawk-Dove Games")

## ---- echo = FALSE, results = 'hide'------------------------------------------
oottest:::color_vuong_table(vuong_table_hdg)
oottest:::color_vuong_table(vuong_table_mp1)
oottest:::color_vuong_table(vuong_table_mp2)
oottest:::color_vuong_table(vuong_table_ac)
oottest:::color_vuong_table(vuong_table_rsp1)
oottest:::color_vuong_table(vuong_table_rsp2)

## ---- results='hide'----------------------------------------------------------
num_theories <- dim(predictions_two_action_games)[3]
result <- matrix(, nrow = num_theories, ncol = num_theories)
for (i in 1:num_theories) {
  for (j in 1:num_theories) {
    llr_2 <- oottest:::get_llr(data_two_action_games, predictions_two_action_games[,,i], predictions_two_action_games[,,j])
    llr_3 <- oottest:::get_llr(data_three_action_games, predictions_three_action_games[,,i], predictions_three_action_games[,,j])
    variance_2 <- oottest:::get_variance_of_llr(data_two_action_games, predictions_two_action_games[,,i], predictions_two_action_games[,,j])
    variance_3 <- oottest:::get_variance_of_llr(data_three_action_games, predictions_three_action_games[,,i], predictions_three_action_games[,,j])
    result[i, j] <- (llr_2 + llr_3) / (variance_2 + variance_3)^(.5)
  }
}
colnames(result) <- colnames(predictions_two_action_games[1,,])
rownames(result) <- colnames(predictions_two_action_games[1,,])
oottest:::color_vuong_table(result)

## -----------------------------------------------------------------------------
llh_table_2 <- oottest:::create_likelihood_table(data_two_action_games, predictions_two_action_games)
llh_table_3 <- oottest:::create_likelihood_table(data_three_action_games, predictions_three_action_games)
llh_table_all <- cbind(llh_table_2,llh_table_3)

## ---- echo = FALSE, results = 'asis'------------------------------------------
knitr::kable(round(llh_table_2,2), caption="Loglikelihoods: two action games")
knitr::kable(round(llh_table_3,2), caption="Loglikelihoods: three action games")

## -----------------------------------------------------------------------------
theories <- c("NE-RA", "CH-RA", "QLK-RA", "QCH-RA")
baseline <- llh_table_all["CH-RA", ]
output <- c()
for (theory in theories) {
  lh_diff <- round(llh_table_all[theory,] - baseline, 12)
  treatment <- 1:40
  output <- cbind(treatment, lh_diff)
  write.csv(output, file= paste0(theory, ".csv"), row.names = FALSE, quote = FALSE)
}

## ---- results='hide'----------------------------------------------------------
vuong_table_2 <- vuong_matrix(data_two_action_games, predictions_two_action_games)
vuong_table_3 <- vuong_matrix(data_three_action_games, predictions_three_action_games)

## ---- results='hide'----------------------------------------------------------
xtable(llh_table_2, type = "latex")
xtable(llh_table_3, type = "latex")

## ---- warning=FALSE, message = FALSE, results='hide'--------------------------
chi_sq_hdg <- oottest:::get_all_chi_sq(data_two_action_games[,1:10], predictions_two_action_games[,1:10,])
chi_sq_mp1 <- oottest:::get_all_chi_sq(data_two_action_games[,11:15], predictions_two_action_games[,11:15,])
chi_sq_mp2 <- oottest:::get_all_chi_sq(data_two_action_games[,16:20], predictions_two_action_games[,16:20,])
chi_sq_2 <- oottest:::get_all_chi_sq(data_two_action_games, predictions_two_action_games)
chi_sq_ac <- oottest:::get_all_chi_sq(data_three_action_games[,1:10], predictions_three_action_games[,1:10,])
chi_sq_rsp1 <- oottest:::get_all_chi_sq(data_three_action_games[,11:15], predictions_three_action_games[,11:15,])
chi_sq_rsp2 <- oottest:::get_all_chi_sq(data_three_action_games[,16:20], predictions_three_action_games[,16:20,])
chi_sq_3 <- oottest:::get_all_chi_sq(data_three_action_games, predictions_three_action_games)

## ---- echo = FALSE, results = 'asis'------------------------------------------
knitr::kable(chi_sq_hdg, caption="Chi-squared test for the hawk-dove games")

## ---- results='hide'----------------------------------------------------------
table_2 <- cbind(chi_sq_hdg, chi_sq_mp1, chi_sq_mp2, chi_sq_2)
table_3 <- cbind(chi_sq_ac, chi_sq_rsp1, chi_sq_rsp2, chi_sq_3)
xtable(table_2, type = "latex", digits=c(0,2,8,2,8,2,8,2,8))
xtable(table_3, type = "latex", digits=c(0,2,8,2,8,2,8,2,8))

## ---- results='hide'----------------------------------------------------------
pred_2 <- round(predictions_two_action_games, 3)
th_names <- names(predictions_two_action_games[1,1,])
output <- c()
for (theory in th_names) {
  output <- rbind(output, paste0("(", pred_2[1,,theory], ", ", pred_2[2,,theory], ")"))
}
colnames(output) <- names(predictions_two_action_games[1,,1])
rownames(output) <- th_names
xtable(output, type = "latex")

pred_3 <- round(predictions_three_action_games, 3)
output <- c()
for (theory in th_names) {
  output <- rbind(output, paste0("(", pred_3[1,,theory], ", ", pred_3[2,,theory], ", ", pred_3[3,,theory],  ")"))
}
colnames(output) <- names(predictions_two_action_games[1,,1])
rownames(output) <- th_names
xtable(output, type = "latex")

## ---- results='hide'----------------------------------------------------------
xtable(data_two_action_games, type = "latex", digits=0)
xtable(data_three_action_games, type = "latex", digits=0)

