// main.nf

/*
 * ========================================================================================
 *  Process Definitions
 * ========================================================================================
 */

// take in gene and mutation, and generate the mutated peptide sequences
process GENERATE_MUTATIONS {
    label 'rstudio_container'

    // This process only needs the gene and mutation to work
    input:
    tuple val(gene), val(mutation)

    // It outputs the gene/mutation identifiers along with the path to the peptide file
    output:
    tuple val(gene), val(mutation), path("${gene}_${mutation}_peptides.txt"), emit: peptides

    script:
    """
    generate-peptides.R placeholder_name ${gene} ${mutation} ${gene}_${mutation}_peptides.txt
    """
}

// hla_true_uniq="$(echo '${hla_true}' | tr ',' '\n' | sort | uniq | tr '\n' ',' | sed 's/,$//'\)"
//    hla_mistyped_uniq="$(echo '${hla_mistyped}' | tr ',' '\n' | sort | uniq | tr '\n' ',' | sed 's/,$//')"

// Run NetMHCpan for a given peptide file and a specific HLA scenario
process RUN_NETMHCPAN {
    label 'netmhcpan_container'

    // Input is a peptide file for a specific (gene, mutation) pair, and one full scenario
    input:
    tuple val(gene), val(mutation), path(pep_file), val(scenario)

    // Output passes along the identifiers and all the generated .out files
    output:
    tuple val(gene), val(mutation), val(scenario), path("*.out"), path(pep_file), emit: predictions

    script:
    // Using a map for the scenario makes the script much cleaner and less error-prone
    def sample        = scenario.Sample
    def hla_true      = scenario.HLA_true
    def hla_mistyped  = scenario.HLA_mistyped
    def tool          = scenario.Tool
    // Define a base name for the output files
    def base_name = "${sample}_${gene}_${mutation}_HLA-${scenario.HLA_Gene}_${tool}"

    """
    # split WT and MUT peptides
    cut -f4 ${pep_file} | tail -n +2 | sort | uniq > wt_peptides.txt
    cut -f5 ${pep_file} | tail -n +2 | sort | uniq > mut_peptides.txt
    hla_true_uniq="\$(echo '${hla_true}' | tr ',' '\n' | sort -u | paste -sd, -)"
    hla_mistyped_uniq="\$(echo '${hla_mistyped}' | tr ',' '\n' | sort -u | paste -sd, -)"

    echo "HLA_TRUE_UNIQ=\${hla_true_uniq}"
    echo "HLA_MISTYPED_UNIQ=\${hla_mistyped_uniq}"

    # --- Run TRUE HLA ---
    /netMHCpan-4.1/Linux_x86_64/bin/netMHCpan -p wt_peptides.txt -a "\${hla_true_uniq}" -BA -s -t -99 > "${base_name}_WT_TRUE.out.tmp"
    echo "TYPE=WT_TRUE SAMPLE=${sample} GENE=${gene} MUT=${mutation} TOOL=${tool}" | cat - "${base_name}_WT_TRUE.out.tmp" > "${base_name}_WT_TRUE.out"

    /netMHCpan-4.1/Linux_x86_64/bin/netMHCpan -p mut_peptides.txt -a "\${hla_true_uniq}" -BA -s -t -99 > "${base_name}_MUT_TRUE.out.tmp"
    echo "TYPE=MUT_TRUE SAMPLE=${sample} GENE=${gene} MUT=${mutation} TOOL=${tool}" | cat - "${base_name}_MUT_TRUE.out.tmp" > "${base_name}_MUT_TRUE.out"

    # --- Run MISTYPED HLA ---
    /netMHCpan-4.1/Linux_x86_64/bin/netMHCpan -p wt_peptides.txt -a "\${hla_mistyped_uniq}" -BA -s -t -99 > "${base_name}_WT_MIS.out.tmp"
    echo "TYPE=WT_MIS SAMPLE=${sample} GENE=${gene} MUT=${mutation} TOOL=${tool}" | cat - "${base_name}_WT_MIS.out.tmp" > "${base_name}_WT_MIS.out"

    /netMHCpan-4.1/Linux_x86_64/bin/netMHCpan -p mut_peptides.txt -a "\${hla_mistyped_uniq}" -BA -s -t -99 > "${base_name}_MUT_MIS.out.tmp"
    echo "TYPE=MUT_MIS SAMPLE=${sample} GENE=${gene} MUT=${mutation} TOOL=${tool}" | cat - "${base_name}_MUT_MIS.out.tmp" > "${base_name}_MUT_MIS.out"
    """
}

// A549-ATCC, NRAS, Q61K
// Combine all the NetMHCpan results for a given mutation/scenario into a single file
process COMBINE_RESULTS {
    label 'rstudio_container'
    publishDir "results", mode: 'copy'


    // Input needs the prediction files, the original peptides file, and metadata
    input:
    //tuple val(gene), val(mutation), val(scenario), path(prediction_files), path(peptides_file)
    tuple val(sample), val(mutation_gene), val(mutation), path(prediction_files), path(peptides_file)
    path scenarios_file // The full scenarios file for context

    output:
    //path "${scenario.Sample}_${scenario.HLA_Gene}_${gene}_${mutation}_results_joined.csv"
    path "${sample}_${mutation_gene}_${mutation}_results_joined.csv"
    path "${sample}_${mutation_gene}_${mutation}_results_summary.csv", emit: summary
    path "${sample}_${mutation_gene}_${mutation}_results_stats_all.csv", emit: stats
    shell:
    '''

    parse-outputs-netmhcpan.R \\
        "!{sample}" \\
        "!{mutation_gene}" \\
        "!{mutation}" \\
        !{prediction_files} \\
        "!{scenarios_file}" \\
        "!{peptides_file}"
    '''
}

process PUBLISH_SUMMARY {
    publishDir "results/combined", mode: 'copy'

    input:
    path f

    output:
    path "summaries_combined_out.csv"

    script:
    """
    cp ${f} summaries_combined_out.csv
    """
}

process PUBLISH_STATS_COMBINED {
    publishDir "results/combined", mode: 'copy'
    
    input:
    path f
    
    output:
    path "combined_stats_out.csv"
    
    script:
    """
    cp ${f} combined_stats_out.csv
    """
}



/*
parse-outputs-netmhcpan.R \\
        "!{scenario.Sample}" \\
        "!{scenario.HLA_Gene}" \\
        "!{gene}" \\
        "!{mutation}" \\
        !{prediction_files} \\
        "!{scenarios_file}" \\
        "!{peptides_file}"
*/

/*
 * ========================================================================================
 *  Workflow Definition
 * ========================================================================================
 */

workflow {

    // --- 1. Create initial channels ---

    // Channel of mutations: [ gene, mutation ]
    ch_mutations = Channel
        .fromPath("../../../data/processed/neoantigen-prediction/mutations.csv")
        .splitCsv(header:true)
        .map { row -> tuple(row.gene, row.mutation) }
     /*
    ch_scenarios = Channel
        .fromPath("../../../data/processed/neoantigen-prediction/scenarios.txt")
        .splitCsv(header:true, sep:"\t")
        .map { row -> row } // Creates a channel of maps
     */
    Channel
        .fromPath("../../../data/processed/neoantigen-prediction/scenarios.txt")
        .splitCsv(header:true, sep:"\t")
        .set{ ch_scenarios }
    ch_scenarios.view()
    // --- 2. Generate peptide sequences for each mutation ---
    
    // This process runs once for each item in ch_mutations
    GENERATE_MUTATIONS(ch_mutations)
    
    // --- 3. Run NetMHCpan for every combination of peptide file and scenario ---

    // The 'combine' operator creates a cartesian product.
    // For each peptide file from GENERATE_MUTATIONS.out.peptides, it pairs it with
    // every scenario from ch_scenarios.
    GENERATE_MUTATIONS.out.peptides
        .combine(ch_scenarios)
        .set { ch_for_netmhcpan }

    // ch_for_netmhcpan now emits: [ gene, mutation, path_to_peptides, scenario_map ]
    // This perfectly matches the input of RUN_NETMHCPAN
    RUN_NETMHCPAN(ch_for_netmhcpan)

    // --- 4. Combine the results ---

    // The output of RUN_NETMHCPAN is: [ gene, mutation, scenario_map, path_to_results ]
    // We need to add the original peptide file back in to pass to COMBINE_RESULTS.
    // We use the 'join' operator, which joins two channels on a common key.
    // By default, it uses the first element(s) as the key. Here, (gene, mutation) is our key.

    ch_predictions = RUN_NETMHCPAN.out.predictions
    ch_peptides_for_join = GENERATE_MUTATIONS.out.peptides

    // The joined channel will emit:
    // [ gene, mutation, scenario_map, path_to_results, path_to_peptides ]

    //ch_peptides_for_join.view()
    //ch_predictions.view()
    //ch_predictions
    //    .join(ch_peptides_for_join)
    //    .set { ch_for_combine }
    //ch_predictions
    //    .join(ch_peptides_for_join, by: [0, 1])
    //    .set { ch_for_combine }

     ch_predictions
        // Step 4a: Reshape data to [sample, gene, mut, files, pep_file] for grouping
        .map { gene, mut, scenario, files, pep_file ->
            tuple(scenario.Sample, gene, mut, files, pep_file)
        }
        // Step 4b: Group by the first 3 elements (sample, gene, mut)
      .groupTuple(by: [0, 1, 2])
    // Step 4c: Clean up the grouped output
    .map { sample, gene, mut, files_list, pep_files_list ->
        tuple(sample, gene, mut, files_list.flatten(), pep_files_list[0])
    }
    .set { ch_for_combine }
 
    // We also need to pass the original scenarios.txt file as a static value input
    scenarios_file = file("../../../data/processed/neoantigen-prediction/scenarios.txt")
    ch_for_combine.view() 
    
    COMBINE_RESULTS(ch_for_combine, scenarios_file)
   
    COMBINE_RESULTS.out.summary.collectFile(name: 'summaries_combined.csv', keepHeader: true, skip: 1)
                               .set{ ch_summary_combined }

    COMBINE_RESULTS.out.stats.collectFile(name: 'stats_combined.csv', keepHeader: true, skip: 1)
                               .set{ ch_stats_combined }

    PUBLISH_SUMMARY(ch_summary_combined)
    PUBLISH_STATS_COMBINED(ch_stats_combined)
}
