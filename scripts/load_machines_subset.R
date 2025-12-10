load_machines_subset <- function() {
  if (!requireNamespace("rrcov", quietly = TRUE)) {
    install.packages("rrcov")
  }
  library(rrcov)
  data("machines")


  i1 <- which(rownames(machines) == "hp-3000/64")
  i2 <- which(rownames(machines) == "ibm-4331-2")

  if (is.null(rownames(machines)) || any(rownames(machines) == "")) {
    rownames(X) <- paste0("Obs", seq_len(nrow(machines)))
  }

  machines[i1:i2, ]
}
