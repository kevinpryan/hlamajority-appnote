library(dplyr)
library(stringr)
library(tidyr)
library(readr)

setwd("/hlamajority-paper/scripts/app_note/")
# read in trace file for 20-sample cpu run
dat <- read.table("../../data/raw/1000-genomes/majority/subset_20/pipeline_info/execution_trace_2026-04-01_13-07-30.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")

# Function to parse Nextflow's time format (e.g., '1h 25m 30.1s') into total hours
parse_nf_time_to_hours <- function(time_str) {
  # Handle cases where the time is missing or '0'
  if (is.na(time_str) || time_str == "0" || time_str == "-") {
    return(0)
  }
  
  # Initialize hours, minutes, seconds
  h <- 0; m <- 0; s <- 0
  
  # Extract components using stringr
  if (str_detect(time_str, "h")) {
    h <- as.numeric(str_extract(time_str, "\\d+(\\.\\d+)?(?=h)"))
  }
  if (str_detect(time_str, "m")) {
    m <- as.numeric(str_extract(time_str, "\\d+(\\.\\d+)?(?=m)"))
  }
  if (str_detect(time_str, "s")) {
    s <- as.numeric(str_extract(time_str, "\\d+(\\.\\d+)?(?=s)"))
  }
  
  # Convert everything to hours and sum up
  total_hours <- h + (m / 60) + (s / 3600)
  return(total_hours)
}

# Apply this function to the 'cpu' column
dat_for_summary <- dat %>%
  mutate(
    name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1],
    sampleid = gsub(")", "", str_split_fixed(name, pattern = "\\(", n = 2)[,2]),
    
    # Use the 'cpu' column for actual CPU hours
    realtime_hours = sapply(realtime, parse_nf_time_to_hours),
    cpu_percent = as.numeric(gsub("%", "", X.cpu)),
    actual_cpu_hours = realtime_hours * (cpu_percent / 100)
  )

# Get total CPU hours per sample
cpu_hours_per_sample <- dat_for_summary %>%
  filter(!is.na(actual_cpu_hours) & !(sampleid %in% c("1","2","3","4"))) %>% # Make sure sampleid is treated as character if needed
  group_by(sampleid) %>%
  summarise(total_actual_cpu_hours = sum(actual_cpu_hours))
cpu_hours_per_sample_before_change <- cpu_hours_per_sample
# Calculate the new mean
mean_cpu_hours <- mean(cpu_hours_per_sample$total_actual_cpu_hours)
print(paste("Average CPU hours per sample:", mean_cpu_hours))
# [1] "Average CPU hours per sample: 4.07056201527778"
# Calculate proportion of CPU hours per process
proportion_cpu_hours_per_process <- dat_for_summary %>%
  filter(!is.na(actual_cpu_hours) & !(sampleid %in% c("1","2","3","4"))) %>%
  group_by(sampleid) %>%
  mutate(total_sample_cpu = sum(actual_cpu_hours)) %>%
  ungroup() %>%
  group_by(sampleid, name_clean) %>%
  summarise(
    process_cpu = sum(actual_cpu_hours),
    total_sample_cpu = first(total_sample_cpu)
  ) %>%
  mutate(prop = process_cpu / total_sample_cpu)
proportion_cpu_hours_per_process_before_polysolver_change <- proportion_cpu_hours_per_process
mean_proportion_per_process <- proportion_cpu_hours_per_process %>%
  group_by(name_clean) %>%
  summarise(Mean_Proportion_CPU_Hours = mean(prop)) %>%
  arrange(desc(Mean_Proportion_CPU_Hours))

print("mean proportion of CPU hours consumed by each process...")
print(mean_proportion_per_process)
mean_proportion_per_process_before_polysolver_change <- mean_proportion_per_process
print("CPU hours consumed by BAM_TO_FASTQ processes...")
mean_proportion_per_process %>% dplyr::filter(grepl("BAM_TO_FASTQ", name_clean)) %>% summarise(bam_to_fastq_proportion_cpu_hours = sum(Mean_Proportion_CPU_Hours)) %>% pull()
# [1] 0.310869

# repeat subset run after polysolver change
dat <- read.table("../../data/raw/1000-genomes/majority/subset_20_after_polysolver_change/pipeline_info/execution_trace_2026-04-29_15-33-00.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")

# Apply this function to the 'cpu' column
dat_for_summary <- dat %>%
  mutate(
    name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1],
    sampleid = gsub(")", "", str_split_fixed(name, pattern = "\\(", n = 2)[,2]),
    
    # Use the 'cpu' column for actual CPU hours
    realtime_hours = sapply(realtime, parse_nf_time_to_hours),
    cpu_percent = as.numeric(gsub("%", "", X.cpu)),
    actual_cpu_hours = realtime_hours * (cpu_percent / 100)
  )

# Get total CPU hours per sample
cpu_hours_per_sample <- dat_for_summary %>%
  filter(!is.na(actual_cpu_hours) & !(sampleid %in% c("1","2","3","4"))) %>% # Make sure sampleid is treated as character if needed
  group_by(sampleid) %>%
  summarise(total_actual_cpu_hours = sum(actual_cpu_hours))
cpu_hours_per_sample_after_polysolver_change <- cpu_hours_per_sample
# Calculate the new mean
mean_cpu_hours <- mean(cpu_hours_per_sample$total_actual_cpu_hours)
print(paste("Average CPU hours per sample:", mean_cpu_hours))
# [1] "Average CPU hours per sample: 4.91529447083333"
# Calculate proportion of CPU hours per process
proportion_cpu_hours_per_process <- dat_for_summary %>%
  filter(!is.na(actual_cpu_hours) & !(sampleid %in% c("1","2","3","4"))) %>%
  group_by(sampleid) %>%
  mutate(total_sample_cpu = sum(actual_cpu_hours)) %>%
  ungroup() %>%
  group_by(sampleid, name_clean) %>%
  summarise(
    process_cpu = sum(actual_cpu_hours),
    total_sample_cpu = first(total_sample_cpu)
  ) %>%
  mutate(prop = process_cpu / total_sample_cpu)
proportion_cpu_hours_per_process_after_polysolver_change <- proportion_cpu_hours_per_process

mean_proportion_per_process <- proportion_cpu_hours_per_process %>%
  group_by(name_clean) %>%
  summarise(Mean_Proportion_CPU_Hours = mean(prop)) %>%
  arrange(desc(Mean_Proportion_CPU_Hours))
mean_proportion_per_process_after_polysolver_change <- mean_proportion_per_process

print("mean proportion of CPU hours consumed by each process...")
print(mean_proportion_per_process)
print("CPU hours consumed by BAM_TO_FASTQ processes...")
mean_proportion_per_process %>% dplyr::filter(grepl("BAM_TO_FASTQ", name_clean)) %>% summarise(bam_to_fastq_proportion_cpu_hours = sum(Mean_Proportion_CPU_Hours)) %>% pull()
#[1] 0.2566165
# full 1000 genomes run for full information
dat <- read.table("../../data/raw/1000-genomes/majority/all_samples/pipeline_info/execution_trace_2026-03-09_11-40-44.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")
dat <- dat %>% mutate(name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1],
                      sampleid = str_split_fixed(name, pattern = "\\(", n = 2)[,2]) %>% 
               mutate(sampleid = gsub(")", "", sampleid))
dat_for_summary <- dat %>% dplyr::select(name_clean,
                                         realtime,
                                         peak_rss,
                                         sampleid
) %>% 
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
    value = readr::parse_number(peak_rss),
    
    # EXTRACT UNIT (Clean whitespace and handle lower/uppercase)
    unit_raw = str_extract(peak_rss, "[a-zA-Z]+"),
    unit     = str_to_upper(unit_raw), # Standardize to GB/MB
    
    # Calculate GB based on the unit
    # Nextflow uses binary memory (1024 MB = 1 GB)
    rss_gb = case_when(
      unit == "GB" ~ value,
      unit == "MB" ~ value / 1024,
      unit == "KB" ~ value / (1024^2), 
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

# repeat for reference building
dat <- read.table("../../data/raw/references/build_logs/pipeline_info/execution_trace_2026-04-01_10-32-16.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")
dat <- dat %>% mutate(name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1])
dat_for_summary <- dat %>%
  mutate(
    name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1],
    sampleid = gsub(")", "", str_split_fixed(name, pattern = "\\(", n = 2)[,2]),
    
    # Use the 'cpu' column for actual CPU hours
    realtime_hours = sapply(realtime, parse_nf_time_to_hours),
    cpu_percent = as.numeric(gsub("%", "", X.cpu)),
    actual_cpu_hours = realtime_hours * (cpu_percent / 100)
  ) %>% 
  arrange(desc(actual_cpu_hours))
print("total CPU hours consumed by reference building workflow...")
print(sum(dat_for_summary$actual_cpu_hours))
#[1] 4.653669
print("Before Polysolver update: total CPU hours consumed by reference building workflow...")
print(dat_for_summary)
proportion_cpu_hours_per_process <- dat_for_summary %>% 
          mutate(proportion_cpu_hours = actual_cpu_hours / sum(actual_cpu_hours)) 

proportion_cpu_hours_per_process_bwa_index <- proportion_cpu_hours_per_process %>% dplyr::filter(grepl("BWA_INDEX", name)) %>% summarise(bwa_index_cpu_hours = sum(proportion_cpu_hours)) %>% pull()
print("proporion of cpu hours consumed by BWA Indexing processes...")
print(proportion_cpu_hours_per_process_bwa_index)
# [1] 0.5784558
head(dat_for_summary, n = 1)
#   task_id      hash native_id                                name    status exit                  submit   duration   realtime X.cpu peak_rss peak_vmem   rchar   wchar name_clean sampleid realtime_hours cpu_percent actual_cpu_hours

#1       1 d2/237dc4  11059789 REFERENCES:HLA_LA_REFERENCE_PREPARE COMPLETED    0 2026-04-01 10:32:20.790 1h 45m 57s 1h 45m 42s 96.4%  33.4 GB   34.1 GB 20.4 GB 37.5 GB REFERENCES:HLA_LA_REFERENCE_PREPARE                1.761667        96.4         1.698247

proportion_cpu_hours_hla_references <- proportion_cpu_hours_per_process %>% dplyr::filter(name_clean == "REFERENCES:HLA_LA_REFERENCE_PREPARE") %>% dplyr::select(proportion_cpu_hours) %>% pull()
print("Before Polysolver update: proportion of cpu hours consumed by HLA_LA_REFERENCE_PREPARE processes...")
print(proportion_cpu_hours_hla_references)
#1h 45m 42s 
# 0.3649264 

# repeat for reference building for Polysolver reference update
#dat <- read.table("../../data/raw/references/build_logs_update_polysolver_ref_20260429/pipeline_info/execution_trace_2026-04-29_15-01-20.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")
dat <- read.table("../../data/raw/references/build_logs_update_polysolver_ref_20260429/pipeline_info/execution_trace_2026-04-29_15-01-20.txt", sep = "\t", header = T) %>% dplyr::filter(status != "FAILED")
dat <- dat %>% mutate(name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1])
dat_for_summary <- dat %>%
  mutate(
    name_clean = str_split_fixed(name, pattern = " \\(", n = 2)[,1],
    sampleid = gsub(")", "", str_split_fixed(name, pattern = "\\(", n = 2)[,2]),

    # Use the 'cpu' column for actual CPU hours
    realtime_hours = sapply(realtime, parse_nf_time_to_hours),
    cpu_percent = as.numeric(gsub("%", "", X.cpu)),
    actual_cpu_hours = realtime_hours * (cpu_percent / 100)
  ) %>%
  arrange(desc(actual_cpu_hours))
print("Polysolver update: total CPU hours consumed by reference building workflow...")
print(sum(dat_for_summary$actual_cpu_hours))
# [1] 5.717773
print("Polysolver update: dat_for_summary...")
print(dat_for_summary)
proportion_cpu_hours_per_process <- dat_for_summary %>%
          mutate(proportion_cpu_hours = actual_cpu_hours / sum(actual_cpu_hours))

proportion_cpu_hours_per_process_bwa_index <- proportion_cpu_hours_per_process %>% dplyr::filter(grepl("BWA_INDEX", name)) %>% summarise(bwa_index_cpu_hours = sum(proportion_cpu_hours)) %>% pull()
print("Polysolver update: proporion of cpu hours consumed by BWA Indexing processes...")
print(proportion_cpu_hours_per_process_bwa_index)
# [1] 0.6788074
head(dat_for_summary, n = 1)
#task_id      hash native_id                                name    status
#1      11 b1/d076b7  11075477 REFERENCES:HLA_LA_REFERENCE_PREPARE COMPLETED
#  exit                  submit   duration   realtime X.cpu peak_rss peak_vmem
#1    0 2026-04-29 15:08:05.316 1h 39m 45s 1h 38m 15s 94.3%  33.4 GB   34.1 GB
#    rchar   wchar                          name_clean sampleid realtime_hours
#1 20.4 GB 37.5 GB REFERENCES:HLA_LA_REFERENCE_PREPARE                  1.6375
#  cpu_percent actual_cpu_hours
#1        94.3         1.544162

proportion_cpu_hours_hla_references <- proportion_cpu_hours_per_process %>% dplyr::filter(name_clean == "REFERENCES:HLA_LA_REFERENCE_PREPARE") %>% dplyr::select(proportion_cpu_hours) %>% pull()
print("Polysolver update: proportion of cpu hours consumed by HLA_LA_REFERENCE_PREPARE processes...")
print(proportion_cpu_hours_hla_references)
#1h 38m 15s 
# 0.2700636 
