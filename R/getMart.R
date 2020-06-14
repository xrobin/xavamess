#' Get the Mart corresponding the Ensembl version and species
#' @param version the ensembl version (number) or \dQuote{current}
#' @param species the species, currently either \dQuote{human} or \dQuote{mouse} in English, or the species part of the dataset name (without the \dQuote{_gene_ensembl} part, as listed by \code{\link[biomaRt]{listDatasets}}, for instance \dQuote{rnorvegicus} or \dQuote{scerevisiae}).
#' @examples
#' if(require("biomaRt")) {
#'   ensembl.proteins <- c("ENSP00000361930", "ENSP00000309503")
#'   mart <- getMart(88, "human")
#'   getBM(attributes = c("ensembl_peptide_id", "uniprotswissprot", "refseq_peptide", "hgnc_symbol"),
#'         filters = "ensembl_peptide_id", values = ensembl.proteins, mart = mart)
#'
#'   # Mouse example
#'   ensembl.proteins <- c("ENSMUSP00000004055", "ENSMUSP00000002708")
#'   mart <- getMart(88, "mmusculus")
#'   getBM(attributes = c("ensembl_peptide_id", "uniprotswissprot", "refseq_peptide", "mgi_symbol"),
#'         filters = "ensembl_peptide_id", values = ensembl.proteins, mart = mart)
#' }
#' @export
getMart <- function(version, species = "human") {
  if (! requireNamespace("biomaRt", quietly = TRUE)) {
    stop("Please install biomaRt from Bioconductor. See the the installation instructions: <https://doi.org/doi:10.18129/B9.bioc.biomaRt>")
  }

  biomart <- "ENSEMBL_MART_ENSEMBL"

  if (species == "human") {
    dataset <- "hsapiens_gene_ensembl"
  }
  else if (species == "mouse") {
    dataset <- "mmusculus_gene_ensembl"
  }
  else {
    dataset <- paste0(species, "_gene_ensembl")
  }

  mart <- biomaRt::useEnsembl("ensembl", dataset = dataset, version = version)
  # Check we're using the right version
  mart.list <- biomaRt::listMarts(mart)
  if (mart.list$version[mart.list$biomart == biomart] != paste0("Ensembl Genes ", version)) {
    stop("Got the wrong Ensembl version")
  }

  return(mart)

}
