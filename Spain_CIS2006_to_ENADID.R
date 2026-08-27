setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source ("enadid_lib.r")
#Spain 2006
path_CIS2006 <- file.path(otherRoot, "enq fec españa/CIS Enq Fec 2006/CIS2006/MD2639/DA2639")

cis2006 <- read_file(path_CIS2006)
