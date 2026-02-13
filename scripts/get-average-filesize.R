dat <- read.table("../data/cell-line-filesizes.txt", sep = "\t", header = F)
dat <- dat  %>% mutate(
  value = readr::parse_number(V1),
  
unit_raw = str_extract(V1, "[a-zA-Z]+"),
unit     = str_to_upper(unit_raw), # Standardize to GB/MB

# Calculate GB based on the unit
# Nextflow uses binary memory (1024 MB = 1 GB)
filesize_gb = case_when(
  unit == "G" ~ value,
  unit == "M" ~ value / 1024,
  unit == "K" ~ value / (1024^2), # Just in case you have small processes
  TRUE ~ NA_real_
),
sampleid = str_split_fixed(V2, pattern = "\\_", n = 3)[,1]
) 
total_sizes <- dat %>% group_by(sampleid) %>% summarise(total_fastq_size = sum(filesize_gb))
mean(total_sizes$total_fastq_size)
