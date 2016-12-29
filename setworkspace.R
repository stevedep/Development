save.image("../Dropbox/workspace.RData")
load ("../Dropbox/workspace.RData")


save(top50p_fivegram_new_labels,file="top50p_fivegram_new_labels.Rda")
save(top50p_fourgram_new_labels,file="top50p_fourgram_new_labels.Rda")
save(top50p_threegram_new_labels,file="top50p_threegram_new_labels.Rda")
save(top50p_twogram_new_labels,file="top50p_twogram_new_labels.Rda")
