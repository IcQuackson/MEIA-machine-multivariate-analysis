source("scripts/load_machines_subset.R")
X <- load_machines_subset()

#introduzir outlier
i1 <- which(rownames(X) == "hp-3000/64")
X[i1, ] <- c(75, 2000, 0.8, 80000, 300, 24, 62, 47)


source("scripts/pca_original_scale.R")
compute_pca(X, "plots/plots_pca_original_outlier", std = FALSE)
