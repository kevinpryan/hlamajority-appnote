data_in <- read.csv("../data/reduced-sanantonio-benchmark-nci60-per-tool.csv")
data_in %>% dplyr::group_by(tool) %>% 
            summarise(calls_for_accuracy_total = sum(total_calls_for_accuracy),
                                                matching_calls_pcr_total = sum(matching_calls_pcr)) %>% 
       
            dplyr::mutate(accuracy = matching_calls_pcr_total/calls_for_accuracy_total)
