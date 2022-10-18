##################################################################################
# Alternative method for generating logos as used in Ray et al. 2022
# Required packages: ggseqlogo, ggplot2
# This script takes two arguments:
#     Rscript ggseqlogo_Logo_Generator.R OUTPUT_FILE_PREFIX PFM_INPUT_FILE
# 
# For example if the following code is executed:
#     Rscript ggseqlogo_Logo_Generator.R RBP_293_logo PFM_files/RBP_293_PFM.txt
# A PNG file called RBP_293_logo.png will be saved in the present working
# directory and will contain the logo for the PFM contained in 
# PFM_files/RBP_293_PFM.txt
# 
# The PFM file must be in the following format (tab-delimted):
# Pos	A	C	G	U
# 1	0.0117199809184647	0.101030785363511	  0.784695022596887	0.102554209522865
# 2	0.0117199809184647	0.0979839370448033	0.878576099519995	0.0117199809184647
# 3	0.0117199809184647	0.10549224183019	  0.871067794734607	0.0117199809184647
# 4	0.0117199809184647	0.11131389843915	  0.865246138125649	0.0117199809184647
# 5	0.0117199809184647	0.117081147042418	  0.85947888952238	0.0117199809184647
# 6	0.0117199809184647	0.194966270292311	  0.781593766272489	0.0117199809184647
# 7	0.101466049409041	  0.109137578211501	  0.77767638986272	0.0117199809184647
##################################################################################

library(ggseqlogo)
library(ggplot2)

options(warn=-1)

# Define alphabets
alphabetletters <- list(
  "RNA" = c("A", "C", "G", "U")
)

# Define colour schemes
colorschemes <- list(
  "RNA" = make_col_scheme(chars = c("A", "C", "G", "U"), 
                          cols=c("#00CC00", "#0000CC", "#FFB302", "#CC0001"), 
                          name="RNAseq")
)

# Fuction that converts PFM to logo
pfm_2_logo <- function(pfm) {
  colscheme <- colorschemes[["RNA"]]
  plot <- ggplot() + 
    geom_logo(pfm, 
              namespace = colscheme$letter, 
              col_scheme = colscheme, 
              font="roboto_bold",
              method="bits") + 
    ylim(c(0, 2)) + 
    theme_logo() + theme(legend.position = "none", axis.title.x = element_blank()) + 
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_blank()) + 
    theme(axis.title.y = element_blank())

  return(plot)
}

args = commandArgs(TRUE)
outprefix = args[1]
filename = args[2]

# Read in file containing PFM
file <- read.table(filename, sep = "\t", stringsAsFactors = FALSE, header=TRUE)

# Reformat file into PFM
file <- as.data.frame(file[, 2:ncol(file)], row.names=file[, 1])
pfm <- t(as.matrix(file)) 

# Generate logo
logo <- pfm_2_logo(pfm)

# Save logo to file
png(paste0(outprefix, ".png"), units="in", height=2, width=3.72, res=300)
logo
dev.off()
