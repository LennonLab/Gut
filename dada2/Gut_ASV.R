r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

#install.packages("devtools")
#library("devtools")
#devtools::install_github("benjjneb/dada2", ref="v1.16")
library("dada2")

path <- "/N/project/Lennon_Sequences/Guts/raw/"

fnFs <- sort(list.files(path, pattern = "_R1_001.fastq.gz", full.names = TRUE))
fnRs <- sort(list.files(path, pattern = "_R2_001.fastq.gz", full.names = TRUE))
sample.names <- sapply(strsplit(basename(fnFs), "_R"), `[`, 1)


#png(filename="fnFs.png")
#plotQualityProfile(fnFs[1:3])
#dev.off()

#png(filename="fnRs.png")
#plotQualityProfile(fnRs[1:3])
#dev.off()

filtFs <- file.path(path, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(path, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))
names(filtFs) <- sample.names
names(filtRs) <- sample.names

#out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=c(240,170), maxN=0, maxEE=c(2,2), truncQ=2, rm.phix = TRUE, compress=TRUE, multithread=FALSE)
#saveRDS(out, file="/N/u/emamuell/Quartz/github/Gut/dada2/out.rds")
out <- readRDS("/N/u/emamuell/Quartz/github/Gut/dada2/out.rds")
head(out)

#errF <- learnErrors(filtFs, multithread = FALSE)
#errR <- learnErrors(filtRs, multithread = FALSE)
#saveRDS(errF, file= "/N/u/emamuell/Quartz/github/Gut/dada2/errF.rds")
#saveRDS(errR, file= "/N/u/emamuell/Quartz/github/Gut/dada2/errR.rds")
errF <- readRDS("/N/u/emamuell/Quartz/github/Gut/dada2/errF.rds")
errR <- readRDS("/N/u/emamuell/Quartz/github/Gut/dada2/errR.rds")

#png(filename="errF.png")
#plot(errF, nominalQ = TRUE)
#dev.off()

#dadaFs <- dada(filtFs, err=errF, multithread = FALSE)
#saveRDS(dadaFs, file="/N/u/emamuell/Quartz/github/Gut/dada2/dadaFs.rds")
dadaFs <- readRDS("/N/u/emamuell/Quartz/github/Gut/dada2/dadaFs.rds")

#dadaRs <- dada(filtRs, err=errR, multithread = FALSE)
#saveRDS(dadaRs, file="/N/u/emamuell/Quartz/github/Gut/dada2/dadaRs.rds")
dadaRs <- readRDS("/N/u/emamuell/Quartz/github/Gut/dada2/dadaRs.rds")

head(dadaFs[[1]])

#mergers <- mergePairs(dadaFs, filtFs, dadaRs, filtRs, verbose = TRUE)
#saveRDS(mergers, file="/N/u/emamuell/Quartz/github/Gut/dada2/mergers.rds")
mergers <- readRDS("/N/u/emamuell/Quartz/github/Gut/dada2/mergers.rds")

head(mergers[[1]])

seqtab <- makeSequenceTable(mergers)
dim(seqtab)
table(nchar(getSequences(seqtab)))
#seqtab2 <- seqtab[,nchar(colnames(seqtab)) %in% 407:411]

#seqtab.nochim <- removeBimeraDenovo(seqtab2, method = "consensus", multithread = FALSE, verbose = TRUE)
seqtab.nochim <- removeBimeraDenovo(seqtab, method = "consensus", multithread = FALSE, verbose = TRUE)

table(nchar(getSequences(seqtab.nochim)))

dim(seqtab.nochim)
#sum(seqtab.nochim)/sum(seqtab2)
sum(seqtab.nochim)/sum(seqtab)

getN <- function(x) sum(getUniques(x))
track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))

colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nochim")
rownames(track) <- sample.names
head(track)

taxa <- assignTaxonomy(seqtab.nochim, "/N/project/Lennon_Sequences/Guts/silva_nr99_v138.2_toGenus_trainset.fa")
taxa_species <- assignSpecies(taxa, "/N/project/Lennon_Sequences/Guts/silva_v138.2_assignSpecies.fa")
taxa.print <- taxa
rownames(taxa.print) <- NULL
head(taxa.print)

write.csv(taxa, "/N/u/emamuell/Quartz/github/Gut/dada2/Gut_taxa_rawseq.csv", row.names = TRUE)
write.csv(seqtab.nochim, "/N/u/emamuell/Quartz/github/Gut/dada2/Gut_ASV_rawseq.csv", row.names = TRUE)

taxa_ASVs <- taxa
rownames(taxa_ASVs) <- paste0("ASV", seq(nrow(taxa_ASVs)))
head(taxa_ASVs)

seqtab_ASVs <- seqtab.nochim
colnames(seqtab_ASVs) <- paste0("ASV", seq(ncol(seqtab_ASVs)))
head(seqtab_ASVs[,1:10])

write.csv(taxa_ASVs, "/N/u/emamuell/Quartz/github/Gut/dada2/Gut_ASV_taxa.csv", row.names=TRUE)
write.csv(seqtab_ASVs, "/N/u/emamuell/Quartz/github/Gut/dada2/Gut_ASV.csv", row.names=TRUE)
