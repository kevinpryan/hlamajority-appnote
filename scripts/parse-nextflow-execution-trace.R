library(dplyr)
library(stringr)
library(tidyr)
dat <- read.table("../data/execution_trace.txt", sep = "\t", header = T)
dat <- dat %>% mutate(name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1])
# make 
dat_for_summary <- dat %>% dplyr::select(name_clean,
                                         realtime,
                                         peak_rss
                                         ) %>% 
                           #mutate(peak_rss = as.numeric(str_split_fixed(peak_rss, pattern = " ", n = 2)[,1])) %>% 
  mutate(
    # Extract hours, minutes, seconds, and milliseconds using Regex lookaheads
    h  = as.numeric(str_extract(realtime, "\\d+(?=h)")),
    #m  = as.numeric(str_extract(realtime, "\\d+(?=m)")),
    m  = as.numeric(str_extract(realtime, "\\d+(?=m(?!s))")),
    
    # For seconds, we ensure it's 's' not preceded by 'm' (to avoid matching 'ms')
    s  = as.numeric(str_extract(realtime, "\\d+(?=s(?!$))|\\d+(?=s$)")), 
    #ms = as.numeric(str_extract(realtime, "\\d+(?=ms)")),
    ms_raw = as.numeric(str_extract(realtime, "\\d+(?=ms)")),
    
    # Replace NAs (missing units) with 0
    h  = replace_na(h, 0),
    m  = replace_na(m, 0),
    s  = replace_na(s, 0),
    ms = replace_na(ms_raw, 0),
    
    # Calculate total minutes
    minutes_numeric = (h * 60) + m + (s / 60) + (ms / 60000)
  ) %>% 
  mutate(
    # Extract the numeric part (digits and decimals)
    # value = as.numeric(str_extract(peak_rss, "[0-9.]+")),
    # 
    # # Extract the text unit (GB, MB, etc)
    # unit = str_extract(peak_rss, "[A-Za-z]+"),
    value = readr::parse_number(peak_rss),
    
    # 3. EXTRACT UNIT (Clean whitespace and handle lower/uppercase)
    # This regex finds the first letter sequence and trims extra spaces
    unit_raw = str_extract(peak_rss, "[a-zA-Z]+"),
    unit     = str_to_upper(unit_raw), # Standardize to GB/MB
    
    # Calculate GB based on the unit
    # Nextflow uses binary memory (1024 MB = 1 GB)
    rss_gb = case_when(
      unit == "GB" ~ value,
      unit == "MB" ~ value / 1024,
      unit == "KB" ~ value / (1024^2), # Just in case you have small processes
      TRUE ~ NA_real_
    )
  )
dat_for_summary
summary_table <- dat_for_summary %>% 
  group_by(name_clean) %>% 
  summarise(Max_Peak_RSS = round(max(rss_gb), 1),
            average_time = round(mean(minutes_numeric), 1),
            sd_time = round(sd(minutes_numeric),1)
            )
#write.csv(summary_table, file = "../data/summary-table-before-adding-cpus.csv", row.names = F)
# add cpu information and calculate CPU hours
add_cpu <- read.csv("../data/summary-table-cpus.csv") %>% full_join(summary_table) %>% mutate(cpu_hours = (average_time/60)*CPU_Cores)
# read in table with cpus automatically added
dat_for_summary %>% dplyr::filter(name_clean == "HLATYPING:alt_align:bwa_mem_align_alt_postalt")

plot_data <- dat_for_summary %>%
  mutate(process_short = case_when(
    grepl("FASTP", name_clean) ~ "Fastp",
    grepl("bwa_mem", name_clean) ~ "BWA-MEM",
    grepl("samtools_sort", name_clean) ~ "Samtools Sort",
    TRUE ~ name_clean
  )) %>% 
  mutate(process_short = gsub("^HLATYPING\\:", "", process_short))
