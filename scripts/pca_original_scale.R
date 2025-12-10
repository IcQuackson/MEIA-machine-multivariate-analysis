source("scripts/load_machines_subset.R")

compute_pca <- function(X = load_machines_subset(),
                                out_dir = "plots/plots_pca_original", std = FALSE) {

  pca <- prcomp(X, center = TRUE, scale. = std)
  pve  <- (pca$sdev^2) / sum(pca$sdev^2)
  cpve <- cumsum(pve)
  k <- which(cpve >= 0.95)[1]

  cat("\n==================== RESULTS ====================\n")
  cat("Number of variables (p):", ncol(X), "\n")
  cat("Number of observations (n):", nrow(X), "\n\n")

  cat("PVE (proportion variance explained) per PC:\n")
  print(pve)

  cat("\nCPVE (cumulative PVE):\n")
  print(cpve)

  cat("\nMinimum k for CPVE >= 0.95:\n")
  cat("k =", k, "\n")
  cat("CPVE[k] =", cpve[k], "\n")


  cat("Loadings (rotation) for retained PCs (1..k):\n")
  print(pca$rotation[, 1:k, drop = FALSE])

  abs_load <- abs(pca$rotation)
  top_pc1 <- sort(abs_load[, 1], decreasing = TRUE)
  cat("\nTop contributors to PC1 (absolute loading):\n")
  print(top_pc1)

  if (ncol(X) >= 2) {
    top_pc2 <- sort(abs_load[, 2], decreasing = TRUE)
    cat("\nTop contributors to PC2 (absolute loading):\n")
    print(top_pc2)
  }


  if (!dir.exists(out_dir))
    dir.create(out_dir)

  # -------------------------------
  # PLOT 1: Scree plot (PVE)
  # -------------------------------
  png(
    filename = file.path(out_dir, "01_scree_pve.png"),
    width = 1200,
    height = 800,
    res = 150
  )
  barplot(
    pve,
    names.arg = paste0("PC", seq_along(pve)),
    las = 2,
    xlab = "Principal components",
    ylab = "Proportion of variance explained"
  )
  dev.off()

  # -------------------------------
  # PLOT 2: Cumulative variance + 95%
  # -------------------------------
  png(
    filename = file.path(out_dir, "02_cumulative_cpve.png"),
    width = 1200,
    height = 800,
    res = 150
  )
  plot(
    cpve,
    type = "b",
    pch = 19,
    ylim = c(0, 1),
    xlab = "Number of PCs",
    ylab = "Cumulative proportion of variance explained"
  )
  abline(h = 0.95, lty = 2)
  abline(v = k, lty = 3)
  text(
    k,
    cpve[k],
    labels = paste0("k = ", k, " (", round(100 * cpve[k], 2), "%)"),
    pos = 4,
    cex = 0.9
  )
  dev.off()

  # -------------------------------
  # PLOT 3: Scores plot (PC1 vs PC2)
  # -------------------------------
  scores <- pca$x
  png(
    filename = file.path(out_dir, "03_scores_pc1_pc2.png"),
    width = 1200,
    height = 800,
    res = 150
  )
  plot(
    scores[, 1],
    scores[, 2],
    pch = 19,
    main = "Scores plot (PC1 vs PC2)",
    xlab = "PC1 score",
    ylab = "PC2 score"
  )
  abline(h = 0, v = 0, lty = 2)
  dev.off()

  # -------------------------------
  # PLOT 4: Loadings barplots (PC1 and PC2)
  # -------------------------------
  png(
    filename = file.path(out_dir, "04_loadings_pc1_pc2.png"),
    width = 1400,
    height = 800,
    res = 150
  )
  par(mfrow = c(1, 2), mar = c(7, 4, 4, 1))

  barplot(pca$rotation[, 1],
          las = 2,
          main = "Loadings - PC1",
          ylab = "Loading")
  abline(h = 0, lty = 2)

  barplot(pca$rotation[, 2],
          las = 2,
          main = "Loadings - PC2",
          ylab = "Loading")
  abline(h = 0, lty = 2)

  par(mfrow = c(1, 1))
  dev.off()

  # -------------------------------
  # PLOT 5: Biplot (PC1 vs PC2)
  # -------------------------------
  png(
    filename = file.path(out_dir, "05_biplot_pc1_pc2.png"),
    width = 1200,
    height = 900,
    res = 150
  )
  biplot(pca,
        choices = c(1, 2),
        cex = 0.8,
        main = "")
  title("Biplot (PC1 vs PC2) - scores + loadings", line = 2)
  dev.off()

  cat("Saved plots in folder:", out_dir, "\n")
  cat("Files:\n")
  cat("  01_scree_pve.png\n")
  cat("  02_cumulative_cpve.png\n")
  cat("  03_scores_pc1_pc2.png\n")
  cat("  04_loadings_pc1_pc2.png\n")
  cat("  05_biplot_pc1_pc2.png\n")
}

compute_classic_pca()
