library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

clinic1 <- read.delim("clinical_aaa.txt", skip = 4)
clinic2 <- read.delim("clinical_afaff.txt", skip = 4)
clinic3 <- read.delim("clinical456.txt", skip = 4)
clinic4 <- read.delim("clinucal_qwerty.txt")
mut1 <- read.delim("mut2.txt")
mut4 <- read.delim("mutations_1.txt")
mut2 <- read.delim("muts_1234.txt")
mut3 <- read.delim("muts.txt")


cancer_genes <- function(table_clinic, table_mut) {
  
  table_mut <- table_mut %>% filter(Hugo_Symbol %in% c("TP53", "TTN", "PTEN")) %>%
    group_by(Tumor_Sample_Barcode) %>%
    summarize(Count = n())
  
  table_mut$Tumor_Sample_Barcode <- str_sub(table_mut$Tumor_Sample_Barcode, start = 1, end = 12)
  
  cancer <- table_clinic %>% select(PATIENT_ID, OS_STATUS) %>%
    right_join(table_mut, by = c("PATIENT_ID" = "Tumor_Sample_Barcode"))
  
  cancer
}

cancer1 <- cancer_genes(clinic1, mut1)

cancer2 <- cancer_genes(clinic2, mut2)

cancer3 <- cancer_genes(clinic3, mut3)

cancer1 <- cancer1 %>% mutate(Study = 1)
cancer2 <- cancer2 %>% mutate(Study = 2)
cancer3 <- cancer3 %>% mutate(Study = 3)

cancer <- bind_rows(cancer1, cancer2, cancer3)

plot1 <- ggplot(cancer, aes(x = OS_STATUS, y = Count)) + geom_boxplot() +
  facet_grid(~Study)

ggsave("plot.png", plot1, width = 20, height = 40, units = "cm")

# cancer1 - Рак молочных желёз
# cancer2 - Рак кишечника
# cancer3 - Рак почек