setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# >>> Claude 2026-09-25: file name case fixed (enadid_lib.r fails on Linux)
source("enadid_lib.R")
# <<< Claude 2026-09-25
#Spain 2006
path_CIS2006 <- file.path(otherRoot, "enq fec españa/CIS Enq Fec 2006/CIS2006/MD2639/DA2639")

cis2006 <- read_file(path_CIS2006)
