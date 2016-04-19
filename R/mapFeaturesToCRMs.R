#.crmFeatureDfToGranges <- function(crm.feature.tab, genome = TRUE) {
#  mat <-
#    matrix(unlist(strsplit(rownames(crm.feature.tab), "_")), nrow = dim(crm.feature.tab)[1], byrow =
#             TRUE)
#  if (genome) {
#    colnames(mat) <- c("genome", "seqnames", "start", "end", "strand")
#  } else {
#    colnames(mat) <- c("seqnames", "start", "end", "strand")
#  }
#  df <- as.data.frame(mat)
#  df[,"start"] <- as.numeric(as.character(df[,"start"]))
#  df[,"end"] <- as.numeric(as.character(df[,"end"]))
#  crm.feature.gr <- GenomicRanges::makeGRangesFromDataFrame(df)
#  GenomicRanges::mcols(crm.feature.gr) <- crm.feature.tab
#  #genome(crm.feature.gr)=mat[1]
#  return(crm.feature.gr)
#}

#' R interface to bed_to_matrix REST in server
#'
#' The \code{mapFeaturesToCRMs} function allows the user to create a training set matrix to build a predictive model. The training set is composed of positive regions (known to be involved in the pathway of interest) and negative regions (randomly picked or known to not be involved in the pathway of interest) that will be described (scored) by features. Three types of features file format are accepted: Position specific scoring matrices modeling motifs recognised by transcription factors, bed files containing region coordinates for any discrete feature (NGS peaks, conservation blocks) and wig/bigWig files containing signal data. This script has been tested with version 0.99 of the online server. Go here to see current version of the server http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php
#'
#' @param URL URL of the server REST target
#' @param positive.bed Positive bed file path. Compulsory
#' @param genome Genome code, eg. dm3 for Drosophila Melanogaster. Compulsory
#' @param negative.bed Negative bed file path.
#' @param shuffling Integer with number of time shuffle background sequences (background.seqs). If negative.bed is NULL and shuffling is set at 0, the feature matrix does not contain negative sequences. It is useful to produce a test set matrix.
#' @param background.seqs Background sequences used for shuffling. If shuffling = 0, set this parameter at 0.
#' @param genome.info File require for shuffling bed. If shuffling = 0, set this parameter at 0.
#' @param pssm Position specific scoring matrices
#' @param background.freqs Background frequencies of nucleotides in genome
#' @param ngs NGS (bed and wig) files
#' @param bed.overlap Minimal overlap as a fraction of query sequence with NGS bed peak. Equivalent with intersectBed -f argument. Default 1bp.
#' @param my.values Bed file where fourth column are values to append to the SVM matrix
#' @param feature.ranking File with ranked features (Output of rankFeatures). It is used for scoring a query bed file
#' @param feature.nb Integer with feature.nb
#' @param crm.feature.file  Path to feature matrix file
#' @param stdout.log.file  Path to standard output log
#' @param stderr.log.file  Path to error log
#' @return A list
#' \item{feature.matrix}{a data frame where each row is a region and each column a feature, each cell carry a score, the first column is the response vector}
#' \item{stdout.log}{Standard output log of mapFeaturesToCRMs script in server}
#' \item{stderr.log}{Standard error log of mapFeaturesToCRMs script in server}
#' @examples
#' \dontrun{
#'  dirPath <- system.file("extdata", package="LedPred")
#'  file.list <-   list.files(dirPath, full.names=TRUE)
#'  background.freqs <- file.list[grep("freq", file.list)]
#'  positive.regions <-  file.list[grep("positive", file.list)]
#'  negative.regions <-  file.list[grep("negative", file.list)]
#'  TF.matrices <-  file.list[grep("tf", file.list)]
#'  ngs.path <- system.file("extdata/ngs", package="LedPred")
#'  ngs.files=list.files(ngs.path, full.names=TRUE)
#'  crm.features.list <- mapFeaturesToCRMs(positive.bed=positive.regions,
#'      negative.bed=negative.regions,  background.freqs=background.freqs,
#'      pssm=TF.matrices, genome="dm3", ngs=ngs.files,
#'      crm.feature.file = "crm.features.tab",
#'      stderr.log.file = "stderr.log", stdout.log.file = "stdout.log")
#'  names(crm.features.list)
#'  class(crm.features.list$crm.features)
#'  crm.features.list$stdout.log
#'  crm.features.list$stderr.log
#'}

mapFeaturesToCRMs <-
  function(URL = 'http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php', positive.bed =
             NULL, genome = NULL, negative.bed = NULL, shuffling = NULL, background.seqs =
             NULL, genome.info = NULL, pssm = NULL, background.freqs = NULL, ngs = NULL, bed.overlap =
             NULL, my.values =
             NULL, feature.ranking = NULL, feature.nb = NULL, crm.feature.file = NULL, stderr.log.file =
             NULL, stdout.log.file = NULL) {
message("mapFeaturesToCRMs is running ...")
    postForm.cmd = "RCurl::postForm("
    if (!is.null(URL)) {
      postForm.cmd = paste(postForm.cmd,"uri='",URL,"'", sep = "")
    } else {
      stop("Server URL is missing...")
    }
    
    if (!RCurl::url.exists(URL)) {
      stop('This URL does not exist.')
    }
    
    if (!is.null(positive.bed)) {
      postForm.cmd = paste(
        postForm.cmd,", positive_bed=RCurl::fileUpload(filename = '",positive.bed,"')", sep =
          ""
      )
    } else {
      stop("positive.bed is missing...")
    }
    
    if (!is.null(genome)) {
      postForm.cmd = paste(postForm.cmd,", genome='",genome,"'", sep = "")
    } else {
      #Genome (dm3,mm9...)
      stop("Genome is missing...")
    }
    
    if (!is.null(negative.bed)) {
      postForm.cmd = paste(
        postForm.cmd,", negative_bed=RCurl::fileUpload(filename = '",negative.bed,"')", sep =
          ""
      )
    } else if (!is.null(shuffling)) {
      if (!is.null(shuffling) &&
          !is.null(background.seqs) && !is.null(genome.info)) {
        postForm.cmd = paste(postForm.cmd,", shuffling=",shuffling, sep = "")
        postForm.cmd = paste(
          postForm.cmd,", background_seqs=RCurl::fileUpload(filename = '",background.seqs,"')", sep =
            ""
        )
        postForm.cmd = paste(
          postForm.cmd,", genome_info=RCurl::fileUpload(filename = '",genome.info,"')", sep =
            ""
        )
      } else {
        stop("negative.bed or shuffling/background.seqs/genome_info are missing...")
      }
    } else {
      # no negative -> prediction
      empty.file = tempfile(pattern = "file", tmpdir = tempdir())
      empty.file = gsub("\\\\", "/", empty.file)
      file.create(empty.file)
      postForm.cmd = paste(
        postForm.cmd,", negative_bed=RCurl::fileUpload(filename = '",empty.file,"')", sep =
          ""
      )
    }
    
    if ((!is.null(pssm) &&
         is.null(background.freqs)) &&
        (is.null(pssm) && !is.null(background.freqs))) {
      stop("Both pssm and background.freqs are needed...")
    } else if (!is.null(pssm) && !is.null(background.freqs)) {
      postForm.cmd = paste(postForm.cmd,", pssm=RCurl::fileUpload(filename = '",pssm,"')", sep =
                             "")
      postForm.cmd = paste(
        postForm.cmd,", background_freqs=RCurl::fileUpload(filename = '",background.freqs,"')", sep =
          ""
      )
    }
    
    if (!is.null(ngs)) {
      postForm.cmd.ngs = lapply(ngs, function(x) {
        sprintf("'ngs[]'=RCurl::fileUpload(filename = '%s')", x)
      })
      postForm.cmd.ngs = paste0(unlist(postForm.cmd.ngs), collapse = ",")
      postForm.cmd = paste(postForm.cmd, postForm.cmd.ngs, sep = ",")
      if (!is.null(bed.overlap)) {
        postForm.cmd = paste(postForm.cmd,", f='",bed.overlap,"'", sep = "")
      }
    }
    
    if (!is.null(my.values)) {
      postForm.cmd.my.values = lapply(my.values, function(x) {
        sprintf("'my_values[]'=RCurl::fileUpload(filename = '%s')", x)
      })
      postForm.cmd.my.values = paste0(unlist(postForm.cmd.my.values), collapse =
                                        ",")
      postForm.cmd = paste(postForm.cmd, postForm.cmd.my.values, sep = ",")
    }
    
    if (!is.null(feature.ranking) && !is.null(feature.nb)) {
      feature.ranking.file = tempfile(pattern = "file", tmpdir = tempdir())
      feature.ranking.file = gsub("\\\\", "/", feature.ranking.file)
      write.table(
        feature.ranking, file = feature.ranking.file, quote = FALSE, row.names = FALSE
      )
      postForm.cmd = paste(
        postForm.cmd,", feature_rank=RCurl::fileUpload(filename = '",feature.ranking.file,"')", sep =
          ""
      )
      postForm.cmd = paste(postForm.cmd,", feature_nb=",feature.nb, sep =
                             "")
    }
    
    postForm.cmd = paste(postForm.cmd,", .encoding='utf-8')", sep = "")
    json = eval(parse(text = postForm.cmd))
    json.decoded <- jsonlite::fromJSON(json)
	while (!RCurl::url.exists(json.decoded[['stdoutLog']])) { Sys.sleep(5) }
	match=FALSE
	while (!match){ Sys.sleep(5); data=readLines(json.decoded[['stdoutLog']]); match=any(grepl('Finished', data)) };
    crm.feature.tab <-
      read.table(json.decoded$crmFeatures, fill = TRUE)
#    crm.feature.gr <- .crmFeatureDfToGranges(crm.feature.tab)
    stdout.log = RCurl::getURL(json.decoded$stdoutLog)
    stderr.log = RCurl::getURL(json.decoded$stderrLog)
    if (!is.null(crm.feature.file)) {
      write.table(crm.feature.tab, file = crm.feature.file, quote = FALSE, col.names=NA, sep="\t")
    }
    if (!is.null(stdout.log.file)) {
      download.file(json.decoded$stdoutLog, destfile = stdout.log.file)
    }
    if (!is.null(stderr.log.file)) {
      download.file(json.decoded$stderrLog, destfile = stderr.log.file)
    }
    return(
      list(
        crm.features = crm.feature.tab, stdout.log = stdout.log, stderr.log = stderr.log
      )
    )
  }

