library(dplyr)
library(stringr)
library(tidyr)
setwd("/hlamajority-paper/scripts/app_note/")
#dat <- read.table("../../data/raw/1000-genomes/benchmark-1000genomes-nfhlamajority-local-update-db-exclude-trim-majority-all-samples/benchmark-1000genomes-nfhlamajority-all-20260309-majority-handle-error-kourami-hlala/pipeline_info/execution_trace_2026-03-09_11-40-44.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")
dat <- read.table("../../data/raw/1000-genomes/benchmark-1000genomes-nfhlamajority-majority-20-samples-cpu-run/benchmark-1000genomes-nfhlamajority-majority-20-samples-cpu-run/pipeline_info/execution_trace_2026-03-27_12-23-24.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")
dat <- dat %>% mutate(name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1],
                      sampleid = str_split_fixed(name, pattern = "\\(", n = 2)[,2]) %>% 
              mutate(sampleid = gsub(")", "", sampleid))

dat_include_fail <- read.table("../../data/raw/1000-genomes/benchmark-1000genomes-nfhlamajority-local-update-db-exclude-trim-majority-all-samples/benchmark-1000genomes-nfhlamajority-all-20260309-majority-handle-error-kourami-hlala/pipeline_info/execution_trace_2026-03-09_11-40-44.txt", sep = "\t", header = T)
# make 
dat_for_summary <- dat %>% dplyr::select(name_clean,
                                         realtime,
                                         peak_rss,
                                         sampleid
                                         ) %>% 
                           #mutate(peak_rss = as.numeric(str_split_fixed(peak_rss, pattern = " ", n = 2)[,1])) %>% 
  # remove lines of kourami or hlala failure 
  #dplyr::filter(!(name_clean == "HLATYPING:KOURAMI:RUN_KOURAMI_JAR" & peak_rss == "-")) %>% 
  mutate(
    # Extract hours, minutes, seconds, and milliseconds using Regex lookaheads
    h  = as.numeric(str_extract(realtime, "\\d+(?=h)")),
    m  = as.numeric(str_extract(realtime, "\\d+(?=m(?!s))")),
    
    # For seconds, we ensure it's 's' not preceded by 'm' (to avoid matching 'ms')
    s  = as.numeric(str_extract(realtime, "\\d+(?=s(?!$))|\\d+(?=s$)")), 
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

summary_table <- dat_for_summary %>% 
  group_by(name_clean) %>% 
  summarise(Max_Peak_RSS = round(max(rss_gb), 1),
            Mean_Peak_RSS = round(mean(rss_gb), 1),
            max_time = round(max(minutes_numeric), 1),
            average_time = round(mean(minutes_numeric), 1),
            sd_time = round(sd(minutes_numeric),1)
            ) %>% 
  arrange(desc(Max_Peak_RSS))
#write.csv(summary_table, file = "../data/summary-table-before-adding-cpus.csv", row.names = F)
# add cpu information and calculate CPU hours
add_cpu <- read.csv("../../data/summary-table-cpus-update-process-names.csv") %>% full_join(summary_table) %>% mutate(cpu_hours = (average_time/60)*CPU_Cores)
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
# repeat for cell line data
dat <- read.table("../../data/raw/cell-lines/benchmark-cell-lines-all-kourami-3-63-0-majority-vote/pipeline_info/execution_trace_2026-02-23_20-49-36.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")
dat <- dat %>% mutate(name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1],
                      sampleid = str_split_fixed(name, pattern = "\\(", n = 2)[,2]) %>% 
  mutate(sampleid = gsub(")", "", sampleid))

dat_include_fail <- read.table("../../data/raw/cell-lines/benchmark-cell-lines-all-kourami-3-63-0-majority-vote/pipeline_info/execution_trace_2026-02-23_20-49-36.txt", sep = "\t", header = T)
# make 
dat_for_summary <- dat %>% dplyr::select(name_clean,
                                         realtime,
                                         peak_rss,
                                         sampleid
) %>% 
  #mutate(peak_rss = as.numeric(str_split_fixed(peak_rss, pattern = " ", n = 2)[,1])) %>% 
  # remove lines of kourami or hlala failure 
  #dplyr::filter(!(name_clean == "HLATYPING:KOURAMI:RUN_KOURAMI_JAR" & peak_rss == "-")) %>% 
  mutate(
    # Extract hours, minutes, seconds, and milliseconds using Regex lookaheads
    h  = as.numeric(str_extract(realtime, "\\d+(?=h)")),
    m  = as.numeric(str_extract(realtime, "\\d+(?=m(?!s))")),
    
    # For seconds, we ensure it's 's' not preceded by 'm' (to avoid matching 'ms')
    s  = as.numeric(str_extract(realtime, "\\d+(?=s(?!$))|\\d+(?=s$)")), 
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

add_cpu <- read.csv("../../data/summary-table-cpus-update-process-names.csv") %>% full_join(summary_table) %>% mutate(cpu_hours = (average_time/60)*CPU_Cores)

summary_table <- dat_for_summary %>% 
  group_by(name_clean) %>% 
  summarise(Max_Peak_RSS = round(max(rss_gb), 1),
            Mean_Peak_RSS = round(mean(rss_gb), 1),
            max_time = round(max(minutes_numeric), 1),
            average_time = round(mean(minutes_numeric), 1),
            sd_time = round(sd(minutes_numeric),1)
  ) %>% 
  arrange(desc(Max_Peak_RSS))

# repeat for reference building
dat <- read.table("../../data/raw/build-references/reference_build_logs/pipeline_info/execution_trace_2026-03-27_12-58-47.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")
dat <- dat %>% mutate(name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1])
dat_for_summary <- dat %>% dplyr::select(name_clean,
                                         realtime,
                                         peak_rss
                                         
) %>% 
  #mutate(peak_rss = as.numeric(str_split_fixed(peak_rss, pattern = " ", n = 2)[,1])) %>% 
  # remove lines of kourami or hlala failure 
  #dplyr::filter(!(name_clean == "HLATYPING:KOURAMI:RUN_KOURAMI_JAR" & peak_rss == "-")) %>% 
  mutate(
    # Extract hours, minutes, seconds, and milliseconds using Regex lookaheads
    h  = as.numeric(str_extract(realtime, "\\d+(?=h)")),
    m  = as.numeric(str_extract(realtime, "\\d+(?=m(?!s))")),
    
    # For seconds, we ensure it's 's' not preceded by 'm' (to avoid matching 'ms')
    s  = as.numeric(str_extract(realtime, "\\d+(?=s(?!$))|\\d+(?=s$)")), 
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
add_cpu <- read.csv("../../data/summary-table-cpus-reference-workflow.csv") %>% full_join(dat_for_summary) %>% mutate(cpu_hours = (minutes_numeric/60)*CPU_Cores)
sum(add_cpu$cpu_hours)
