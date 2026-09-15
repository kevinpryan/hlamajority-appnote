#!/usr/bin/env Rscript

# ==============================================================================
# Re-run nf-hlamajority majority voting without PolySolver
#
# This script:
#   1. Reads existing HLA-LA, OptiType, Kourami and mosdepth outputs
#   2. Replaces PolySolver calls with an NA placeholder
#   3. Runs the existing nf-hlamajority majority voting functions
#   4. Writes per-sample and combined output files
#
# The original tool outputs are not modified.
# ==============================================================================


# ------------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
    library(optparse)
    library(dplyr)
    library(stringr)
    library(tibble)
    library(tidyr)
    library(vroom)
})


# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------

# The script is located in:
#
# scripts/nf-hlamajority-without-polysolver/
#
# and the lib directory is alongside it.
#
# Use the script location rather than assuming the current working directory.

args <- commandArgs(trailingOnly = FALSE)

file_arg <- "--file="
script_file <- args[startsWith(args, file_arg)]

if (length(script_file) == 1) {
    script_path <- normalizePath(
        sub(file_arg, "", script_file),
        mustWork = TRUE
    )
    script_dir <- dirname(script_path)
} else {
    # Fallback if running interactively
    script_dir <- getwd()
}

lib_dir <- file.path(script_dir, "lib")


# ------------------------------------------------------------------------------
# Load nf-hlamajority functions
# ------------------------------------------------------------------------------

source(file.path(lib_dir, "HLA_LA_conversion.R"))
source(file.path(lib_dir, "toolOutputToR.HLA_LA.R"))
source(file.path(lib_dir, "Optitype_conversion.R"))
source(file.path(lib_dir, "kourami_conversion.R"))

source(file.path(lib_dir, "majority_voting.R"))
source(file.path(lib_dir, "df_to_list.R"))
source(file.path(lib_dir, "are_vectors_identical.R"))
source(file.path(lib_dir, "extract_tools_contributing_to_vote.R"))
source(file.path(lib_dir, "extract_na_tools.R"))
source(file.path(lib_dir, "weighted_vote.R"))


# ------------------------------------------------------------------------------
# Command-line arguments
# ------------------------------------------------------------------------------

option_list <- list(

    make_option(
        c("-i", "--input"),
        type = "character",
        default = NULL,
        help = "Path to all_samples directory",
        metavar = "character"
    ),

    make_option(
        c("-o", "--output"),
        type = "character",
        default = NULL,
        help = "Output directory",
        metavar = "character"
    ),

    make_option(
        c("-w", "--weights"),
        type = "character",
        default = NULL,
        help = "Path to nf-hlamajority weights CSV",
        metavar = "character"
    )
)


opt_parser <- OptionParser(
    option_list = option_list
)

opt <- parse_args(opt_parser)


# ------------------------------------------------------------------------------
# Check arguments
# ------------------------------------------------------------------------------

if (is.null(opt$input)) {
    print_help(opt_parser)
    stop("--input must be supplied", call. = FALSE)
}

if (is.null(opt$output)) {
    print_help(opt_parser)
    stop("--output must be supplied", call. = FALSE)
}

if (is.null(opt$weights)) {
    print_help(opt_parser)
    stop("--weights must be supplied", call. = FALSE)
}


# ------------------------------------------------------------------------------
# Normalise paths
# ------------------------------------------------------------------------------

base_dir <- normalizePath(
    opt$input,
    mustWork = TRUE
)

output_dir <- normalizePath(
    opt$output,
    mustWork = FALSE
)

weights_file <- normalizePath(
    opt$weights,
    mustWork = TRUE
)


# ------------------------------------------------------------------------------
# Check input directories
# ------------------------------------------------------------------------------

required_dirs <- c(
    "hlala_calls",
    "kourami",
    "optitype_calls",
    "mosdepth"
)

missing_dirs <- required_dirs[
    !dir.exists(file.path(base_dir, required_dirs))
]

if (length(missing_dirs) > 0) {
    stop(
        "The following required directories are missing:\n",
        paste(missing_dirs, collapse = "\n"),
        call. = FALSE
    )
}


# ------------------------------------------------------------------------------
# Create output directories
# ------------------------------------------------------------------------------

dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
)

per_sample_dir <- file.path(output_dir, "per_sample")

dir.create(
    per_sample_dir,
    recursive = TRUE,
    showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# Find samples
# ------------------------------------------------------------------------------

sample_dir <- file.path(
    base_dir,
    "optitype_calls"
)

samples <- list.dirs(
    sample_dir,
    recursive = FALSE,
    full.names = FALSE
)

samples <- samples[nzchar(samples)]
#samples <- samples[
#    samples %in% "HG00096"
#]

if (length(samples) == 0) {
    stop(
        "No samples found in: ",
        sample_dir,
        call. = FALSE
    )
}

cat(
    "\nFound ",
    length(samples),
    " samples.\n\n",
    sep = ""
)


# ------------------------------------------------------------------------------
# Read weights
# ------------------------------------------------------------------------------

weights <- read.csv(
    weights_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
)


# ------------------------------------------------------------------------------
# Function: create PolySolver NA placeholder
# ------------------------------------------------------------------------------

make_polysolver_placeholder <- function() 
    data.frame(
        matrix(
            NA_character_,
            nrow = 1,
            ncol = 6,
            dimnames = list(
                NULL,
                c("A", "A", "B", "B", "C", "C")
            )
        ),
        check.names = FALSE
)

make_na_calls <- function() {
    data.frame(
        matrix(
            NA_character_,
            nrow = 1,
            ncol = 6,
            dimnames = list(
                NULL,
                c("A", "A", "B", "B", "C", "C")
            )
        ),
        check.names = FALSE
    )
}

# ------------------------------------------------------------------------------
# Function: process one sample
# ------------------------------------------------------------------------------

process_sample <- function(samplename) {

    cat(
        "[",
        samplename,
        "] Processing...",
        sep = ""
    )

    # --------------------------------------------------------------------------
    # Input paths
    # --------------------------------------------------------------------------

    optitype_dir <- file.path(
    base_dir,
    "optitype_calls",
    samplename,
    "optitype_calls"
    )

    hlala_dir <- file.path(
    base_dir,
    "hlala_calls",
    samplename,
    "hlala_calls"
    )

    kourami_dir <- file.path(
    base_dir,
    "kourami",
    samplename,
    "kourami_calls"
    )

    depth_file <- file.path(
        base_dir,
        "mosdepth",
        samplename,
        paste0(
            samplename,
            "-mean-depth-hla-classI-exons-2-3.tsv"
            )
    )


    # --------------------------------------------------------------------------
    # Check input files
    # --------------------------------------------------------------------------

    required_paths <- c(
        optitype_dir,
        hlala_dir,
        kourami_dir,
        depth_file
    )

    missing_paths <- required_paths[
        !file.exists(required_paths)
    ]

    if (length(missing_paths) > 0) {
        stop(
            "\n[",
            samplename,
            "] Missing required directory/file(s):\n",
            paste(missing_paths, collapse = "\n")
        )
    }

    # --------------------------------------------------------------------------
    # Convert HLA-LA
    # --------------------------------------------------------------------------

    #hlala <- toolOutputToR.HLA_LA(
    #    hlala_dir,
    #    mhci_only = TRUE,
    #    trim = TRUE
    #)
    
    hlala <- tryCatch(
    {
        toolOutputToR.HLA_LA(
            hlala_dir,
            mhci_only = TRUE,
            trim = TRUE
        )
    },
    error = function(e) {
        cat(
            "\n[", samplename,
            "] HLA-LA conversion failed: ",
            conditionMessage(e),
            " -- using NA calls\n",
            sep = ""
        )

        make_na_calls()
    }
)
    # --------------------------------------------------------------------------
    # Convert OptiType
    # --------------------------------------------------------------------------

    #optitype <- toolOutputToR.Optitype(
    #    optitype_dir
    #)
optitype <- tryCatch(
    {
        toolOutputToR.Optitype(
            optitype_dir
        )
    },
    error = function(e) {
        cat(
            "\n[", samplename,
            "] OptiType conversion failed: ",
            conditionMessage(e),
            " -- using NA calls\n",
            sep = ""
        )

        make_na_calls()
    }
)

    # --------------------------------------------------------------------------
    # PolySolver placeholder
    #
    # Deliberately do NOT read the existing PolySolver output.
    #
    # This preserves the original four-tool structure while ensuring that
    # PolySolver contributes no genotype calls to the vote.
    # --------------------------------------------------------------------------

    polysolver <- make_polysolver_placeholder()

    # --------------------------------------------------------------------------
    # Convert Kourami
    # --------------------------------------------------------------------------

    #kourami <- toolOutputToR.kourami(
    #    kourami_dir,
    #    mhci_only = TRUE,
    #    trim = TRUE
    #)
kourami <- tryCatch(
    {
        toolOutputToR.kourami(
            kourami_dir,
            mhci_only = TRUE,
            trim = TRUE
        )
    },
    error = function(e) {
        cat(
            "\n[", samplename,
            "] Kourami conversion failed: ",
            conditionMessage(e),
            " -- using NA calls\n",
            sep = ""
        )

        make_na_calls()
    }
)
   
    # --------------------------------------------------------------------------
    # Read depth
    # --------------------------------------------------------------------------

    depth_df <- read.delim(
        depth_file,
        stringsAsFactors = FALSE,
        check.names = FALSE
    )


    # --------------------------------------------------------------------------
    # Combine tool calls
    # --------------------------------------------------------------------------

    combined <- rbind(
        hlala,
        optitype,
        polysolver,
        kourami
    )

    rownames(combined) <- c(
        "hlala",
        "optitype",
        "polysolver",
        "kourami"
    )

    combined$tool <- rownames(combined)

    combined$sample <- rep(
        samplename,
        nrow(combined)
    )

    colnames(combined) <- c(
        "A1",
        "A2",
        "B1",
        "B2",
        "C1",
        "C2",
        "tool",
        "sample"
    )


    # --------------------------------------------------------------------------
    # Convert genotype calls to lists
    # --------------------------------------------------------------------------

    A_list <- df_to_list(
        combined,
        cols = c("A1", "A2")
    )

    B_list <- df_to_list(
        combined,
        cols = c("B1", "B2")
    )

    C_list <- df_to_list(
        combined,
        cols = c("C1", "C2")
    )


    # --------------------------------------------------------------------------
    # Remove NA tool calls from voting
    # --------------------------------------------------------------------------

    A_list_notna <- A_list[
        not_na(A_list)
    ]

    B_list_notna <- B_list[
        not_na(B_list)
    ]

    C_list_notna <- C_list[
        not_na(C_list)
    ]


    # --------------------------------------------------------------------------
    # Rank tools exactly as in original parser
    # --------------------------------------------------------------------------

    weights_vote <- weights

    weights_vote$A <- rev(
        rank(weights_vote$A)
    )

    weights_vote$B <- rev(
        rank(weights_vote$B)
    )

    weights_vote$C <- rev(
        rank(weights_vote$C)
    )


    # ==========================================================================
    # HLA-A
    # ==========================================================================

    if (length(A_list_notna) == 0) {

        A_vote <- c(NA, NA)
        matching_tools_A <- NA_character_
        n_tools_called_A <- 0
        n_tools_support_A <- 0
        support_A <- NA_real_

    } else {

        A_identical <- outer(
            A_list_notna,
            A_list_notna,
            FUN = are_vectors_identical_vectorised
        )

        A_vote <- majority_vote_comparison(
            A_identical,
            A_list,
            weights_vote,
            "A"
        )

        matching_tools_A <- extract_tools_contributing_to_vote(
            genotype_list = A_list,
            genotype_call_vector = A_vote
        )

        n_tools_called_A <- length(
            A_list_notna
        )

        n_tools_support_A <- length(
            strsplit(
                matching_tools_A,
                ","
            )[[1]]
        )

        support_A <- round(
            n_tools_support_A /
                n_tools_called_A,
            2
        )
    }


    A_df <- data.frame(
        sample = samplename,
        gene = "HLA-A",
        allele1 = A_vote[[1]],
        allele2 = A_vote[[2]],
        support = support_A,
        matching_tools = matching_tools_A,
        method = "majority_vote",
        weight_winner = n_tools_support_A,
        total_weight = n_tools_called_A,
        n_tools_support = n_tools_support_A,
        n_tools_called = n_tools_called_A,
        stringsAsFactors = FALSE
    )


    # ==========================================================================
    # HLA-B
    # ==========================================================================

    if (length(B_list_notna) == 0) {

        B_vote <- c(NA, NA)
        matching_tools_B <- NA_character_
        n_tools_called_B <- 0
        n_tools_support_B <- 0
        support_B <- NA_real_

    } else {

        B_identical <- outer(
            B_list_notna,
            B_list_notna,
            FUN = are_vectors_identical_vectorised
        )

        B_vote <- majority_vote_comparison(
            B_identical,
            B_list,
            weights_vote,
            "B"
        )

        matching_tools_B <- extract_tools_contributing_to_vote(
            genotype_list = B_list,
            genotype_call_vector = B_vote
        )

        n_tools_called_B <- length(
            B_list_notna
        )

        n_tools_support_B <- length(
            strsplit(
                matching_tools_B,
                ","
            )[[1]]
        )

        support_B <- round(
            n_tools_support_B /
                n_tools_called_B,
            2
        )
    }


    B_df <- data.frame(
        sample = samplename,
        gene = "HLA-B",
        allele1 = B_vote[[1]],
        allele2 = B_vote[[2]],
        support = support_B,
        matching_tools = matching_tools_B,
        method = "majority_vote_no_polysolver",
        weight_winner = n_tools_support_B,
        total_weight = n_tools_called_B,
        n_tools_support = n_tools_support_B,
        n_tools_called = n_tools_called_B,
        stringsAsFactors = FALSE
    )


    # ==========================================================================
    # HLA-C
    # ==========================================================================

    if (length(C_list_notna) == 0) {

        C_vote <- c(NA, NA)
        matching_tools_C <- NA_character_
        n_tools_called_C <- 0
        n_tools_support_C <- 0
        support_C <- NA_real_

    } else {

        C_identical <- outer(
            C_list_notna,
            C_list_notna,
            FUN = are_vectors_identical_vectorised
        )

        C_vote <- majority_vote_comparison(
            C_identical,
            C_list,
            weights_vote,
            "C"
        )

        matching_tools_C <- extract_tools_contributing_to_vote(
            genotype_list = C_list,
            genotype_call_vector = C_vote
        )

        n_tools_called_C <- length(
            C_list_notna
        )

        n_tools_support_C <- length(
            strsplit(
                matching_tools_C,
                ","
            )[[1]]
        )

        support_C <- round(
            n_tools_support_C /
                n_tools_called_C,
            2
        )
    }


    C_df <- data.frame(
        sample = samplename,
        gene = "HLA-C",
        allele1 = C_vote[[1]],
        allele2 = C_vote[[2]],
        support = support_C,
        matching_tools = matching_tools_C,
        method = "majority_vote_no_polysolver",
        weight_winner = n_tools_support_C,
        total_weight = n_tools_called_C,
        n_tools_support = n_tools_support_C,
        n_tools_called = n_tools_called_C,
        stringsAsFactors = FALSE
    )


    # --------------------------------------------------------------------------
    # Combine votes
    # --------------------------------------------------------------------------

    output <- rbind(
        A_df,
        B_df,
        C_df
    )


    # --------------------------------------------------------------------------
    # Join mean depth
    # --------------------------------------------------------------------------

    output <- output %>%
        left_join(
            depth_df,
            by = c("sample", "gene")
        )


    # --------------------------------------------------------------------------
    # Create clean output
    # --------------------------------------------------------------------------

    output_clean <- output %>%
        dplyr::select(
            sample,
            gene,
            allele1,
            allele2,
            matching_tools,
            method,
            support,
            mean_depth_hla_exons_2_3_gene
        )


    # --------------------------------------------------------------------------
    # All-calls output
    #
    # Keep the PolySolver row, but it contains only NA.
    # --------------------------------------------------------------------------

    full_output <- combined %>%
        relocate(
            sample,
            .before = A1
        ) %>%
        relocate(
            tool,
            .before = A1
        )


    # --------------------------------------------------------------------------
    # Write per-sample files
    # --------------------------------------------------------------------------

    write.table(
        full_output,
        file = file.path(
            per_sample_dir,
            paste0(
                samplename,
                "_all_calls_mhci.tsv"
            )
        ),
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
    )


    write.table(
        output,
        file = file.path(
            per_sample_dir,
            paste0(
                samplename,
                "_votes_mhci_stats.tsv"
            )
        ),
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
    )


    write.table(
        output_clean,
        file = file.path(
            per_sample_dir,
            paste0(
                samplename,
                "_votes_mhci.tsv"
            )
        ),
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
    )


    cat(
        " done\n"
    )


    # Return results for combining later

    list(
        votes = output_clean,
        stats = output,
        all_calls = full_output
    )
}


# ------------------------------------------------------------------------------
# Process all samples
# ------------------------------------------------------------------------------

results <- vector(
    mode = "list",
    length = length(samples)
)

names(results) <- samples


failed_samples <- character()


for (i in seq_along(samples)) {

    samplename <- samples[i]

    result <- tryCatch(

        process_sample(samplename),

        error = function(e) {

            cat(
                "\n[",
                samplename,
                "] ERROR: ",
                conditionMessage(e),
                "\n",
                sep = ""
            )

            failed_samples <<- c(
                failed_samples,
                samplename
            )

            NULL
        }
    )

    results[[samplename]] <- result
}


# ------------------------------------------------------------------------------
# Remove failed samples
# ------------------------------------------------------------------------------

successful_results <- results[
    !vapply(
        results,
        is.null,
        logical(1)
    )
]


if (length(successful_results) == 0) {

    stop(
        "\nNo samples completed successfully.",
        call. = FALSE
    )
}


# ------------------------------------------------------------------------------
# Combine results across samples
# ------------------------------------------------------------------------------

votes_combined <- bind_rows(
    lapply(
        successful_results,
        `[[`,
        "votes"
    )
)

stats_combined <- bind_rows(
    lapply(
        successful_results,
        `[[`,
        "stats"
    )
)

all_calls_combined <- bind_rows(
    lapply(
        successful_results,
        `[[`,
        "all_calls"
    )
)


# ------------------------------------------------------------------------------
# Sort combined results
# ------------------------------------------------------------------------------

votes_combined <- votes_combined %>%
    arrange(sample, gene)

stats_combined <- stats_combined %>%
    arrange(sample, gene)

all_calls_combined <- all_calls_combined %>%
    arrange(
        sample,
        factor(
            tool,
            levels = c(
                "hlala",
                "optitype",
                "polysolver",
                "kourami"
            )
        )
    )


# ------------------------------------------------------------------------------
# Write combined outputs
# ------------------------------------------------------------------------------

write.table(
    votes_combined,
    file = file.path(
        output_dir,
        "nf_hlamajority_votes_combined.tsv"
    ),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
)


write.table(
    stats_combined,
    file = file.path(
        output_dir,
        "nf_hlamajority_stats_combined.tsv"
    ),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
)


write.table(
    all_calls_combined,
    file = file.path(
        output_dir,
        "nf_hlamajority_all_calls.tsv"
    ),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
)


# ------------------------------------------------------------------------------
# Write a simple analysis manifest
# ------------------------------------------------------------------------------

manifest <- data.frame(
    analysis = "majority_vote_no_polysolver",
    n_samples_found = length(samples),
    n_samples_successful = length(successful_results),
    n_samples_failed = length(failed_samples),
    polysolver = "excluded_replaced_with_NA",
    tools_used = "HLA-LA,OptiType,Kourami",
    stringsAsFactors = FALSE
)

write.table(
    manifest,
    file = file.path(
        output_dir,
        "analysis_manifest.tsv"
    ),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
)


# ------------------------------------------------------------------------------
# Report failures
# ------------------------------------------------------------------------------

if (length(failed_samples) > 0) {

    writeLines(
        failed_samples,
        con = file.path(
            output_dir,
            "failed_samples.txt"
        )
    )

    cat(
        "\nWARNING: ",
        length(failed_samples),
        " sample(s) failed:\n",
        paste(failed_samples, collapse = "\n"),
        "\n",
        sep = ""
    )
}


# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

cat(
    "\n========================================\n",
    "Analysis complete\n",
    "========================================\n",
    "Samples found:       ", length(samples), "\n",
    "Samples successful:  ", length(successful_results), "\n",
    "Samples failed:      ", length(failed_samples), "\n",
    "Output directory:    ", output_dir, "\n",
    "PolySolver:          excluded / replaced with NA\n",
    "========================================\n\n",
    sep = ""
)
