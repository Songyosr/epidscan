# PRM I/O System
# Complete parameter management for SaTScan PRM files
# Replaces reliance on rsatscan::ss.options() internals

# -----------------------------------------------------------------------------
# prm_parse: Parse PRM text/file into structured R list
# -----------------------------------------------------------------------------

#' Parse a SaTScan PRM Source
#'
#' Converts a PRM file or character vector into a structured R list.
#' The original text is preserved as the "skeleton" attribute for later writing.
#'
#' @param source Either a file path (character of length 1) or a character vector
#'   of PRM lines (e.g., from `readLines()` or `ssenv$.ss.params.defaults`).
#' @return A named list of parameter values. Attributes:
#'   - `skeleton`: Original text vector
#'   - `sections`: Named character vector mapping param names to section names
#'   - `line_map`: Named integer vector mapping param names to line numbers
#' @examples
#' \dontrun{
#' prm <- prm_parse("path/to/file.prm")
#' prm$CaseFile
#' # [1] "epid.cas"
#' }
#' @export
prm_parse <- function(source) {
    # Determine if source is a file path or already a character vector
    if (length(source) == 1 && file.exists(source)) {
        lines <- readLines(source, warn = FALSE)
    } else {
        lines <- source
    }

    # Vectorized parsing
    trimmed <- trimws(lines)

    # Identify line types
    is_empty <- !nzchar(trimmed)
    is_comment <- startsWith(trimmed, ";")
    is_section <- grepl("^\\[.+\\]$", trimmed)

    # Find Key=Value lines (has "=" and not empty/comment/section)
    eq_pos <- regexpr("=", lines, fixed = TRUE)
    is_kv <- eq_pos > 0 & !is_empty & !is_comment & !is_section

    # Extract section names and propagate forward
    section_names <- gsub("^\\[|\\]$", "", trimmed)
    section_names[!is_section] <- NA
    # Fill forward: each line gets the most recent section header
    section_idx <- cumsum(is_section)
    section_idx[section_idx == 0] <- NA
    section_lookup <- section_names[is_section]
    current_sections <- section_lookup[section_idx]
    current_sections[is.na(current_sections)] <- "Unknown"

    # Extract keys and values for K=V lines
    kv_indices <- which(is_kv)
    kv_lines <- lines[kv_indices]
    kv_eq_pos <- eq_pos[kv_indices]

    keys <- trimws(substr(kv_lines, 1, kv_eq_pos - 1))
    values <- trimws(substr(kv_lines, kv_eq_pos + 1, nchar(kv_lines)))

    # Build result list
    result <- setNames(as.list(values), keys)

    # Build section and line_map named vectors
    sections <- setNames(current_sections[kv_indices], keys)
    line_map <- setNames(kv_indices, keys)

    # Extract version from [System] section if present
    version <- NULL
    if ("Version" %in% keys) {
        version <- result[["Version"]]
    }

    # Attach metadata as attributes
    attr(result, "skeleton") <- lines
    attr(result, "sections") <- sections
    attr(result, "line_map") <- line_map
    attr(result, "version") <- version

    class(result) <- c("prm_list", "list")
    result
}

# -----------------------------------------------------------------------------
# prm_set: Modify existing parameters
# -----------------------------------------------------------------------------

#' Modify PRM Parameters
#'
#' Updates existing parameters in a prm_list. By default, errors if a parameter
#' is not found (strict mode).
#'
#' @param prm A `prm_list` object from `prm_parse()`.
#' @param ... Named parameters to modify (e.g., `CaseFile = "test.cas"`).
#' @param .strict If `TRUE` (default), errors on unknown keys. If `FALSE`, warns.
#' @return Modified `prm_list`.
#' @examples
#' \dontrun{
#' prm <- prm_defaults()
#' prm <- prm_set(prm, CaseFile = "my_cases.cas", AnalysisType = 3)
#' }
#' @export
prm_set <- function(prm, ..., .strict = TRUE) {
    updates <- list(...)

    if (length(updates) == 0) {
        return(prm)
    }

    # Vectorized validation: find unknown keys
    update_keys <- names(updates)
    existing_keys <- names(prm)
    unknown_keys <- setdiff(update_keys, existing_keys)

    if (length(unknown_keys) > 0) {
        msg <- sprintf(
            "Parameter%s '%s' not found in prm_list.",
            if (length(unknown_keys) > 1) "s" else "",
            paste(unknown_keys, collapse = "', '")
        )
        if (.strict) {
            stop(msg, call. = FALSE)
        } else {
            warning(msg, call. = FALSE)
            # Filter out unknown keys
            updates <- updates[update_keys %in% existing_keys]
        }
    }

    # Vectorized assignment: convert all values to character
    updates <- lapply(updates, as.character)

    # Use modifyList for efficient bulk update (preserves attributes)
    prm[names(updates)] <- updates

    prm
}

# -----------------------------------------------------------------------------
# prm_add: Add new parameters or sections
# -----------------------------------------------------------------------------

#' Add a New Parameter to PRM
#'
#' Adds a new parameter to a prm_list. Creates the section if it doesn't exist.
#' Errors if the parameter already exists (use `prm_set` for that).
#'
#' @param prm A `prm_list` object.
#' @param key Parameter name (character).
#' @param value Parameter value.
#' @param section Section name without brackets (e.g., "Custom Section").
#' @param info Optional comment line (without leading semicolon).
#' @return Expanded `prm_list` with updated skeleton.
#' @export
prm_add <- function(prm, key, value, section, info = NULL) {
    if (key %in% names(prm)) {
        stop(sprintf("Parameter '%s' already exists. Use prm_set() to modify.", key),
            call. = FALSE
        )
    }

    # Get current skeleton
    skeleton <- attr(prm, "skeleton")
    sections_map <- attr(prm, "sections")
    line_map <- attr(prm, "line_map")

    # Check if section exists
    section_header <- paste0("[", section, "]")
    section_idx <- which(skeleton == section_header)

    if (length(section_idx) == 0) {
        # Section doesn't exist - append it at the end (before [System] if present)
        system_idx <- which(skeleton == "[System]")
        if (length(system_idx) > 0) {
            insert_at <- system_idx[1] - 1
        } else {
            insert_at <- length(skeleton)
        }

        # Build new lines
        new_lines <- c("", section_header)
        if (!is.null(info)) {
            new_lines <- c(new_lines, paste0(";", info))
        }
        new_lines <- c(new_lines, paste0(key, "=", value))

        # Insert
        skeleton <- append(skeleton, new_lines, after = insert_at)
        new_line_num <- insert_at + length(new_lines)
    } else {
        # Section exists - find the end of this section (next section header or EOF)
        next_section_idx <- which(grepl("^\\[", skeleton) & seq_along(skeleton) > section_idx[1])
        if (length(next_section_idx) > 0) {
            insert_at <- next_section_idx[1] - 1
        } else {
            insert_at <- length(skeleton)
        }

        # Build new lines
        new_lines <- character()
        if (!is.null(info)) {
            new_lines <- c(new_lines, paste0(";", info))
        }
        new_lines <- c(new_lines, paste0(key, "=", value))

        # Insert
        skeleton <- append(skeleton, new_lines, after = insert_at)
        new_line_num <- insert_at + length(new_lines)
    }

    # Update prm_list
    prm[[key]] <- as.character(value)
    sections_map[key] <- section
    line_map[key] <- new_line_num

    attr(prm, "skeleton") <- skeleton
    attr(prm, "sections") <- sections_map
    attr(prm, "line_map") <- line_map

    prm
}

# -----------------------------------------------------------------------------
# prm_write: Write prm_list to file using skeleton injection
# -----------------------------------------------------------------------------

#' Write PRM List to File
#'
#' Serializes a prm_list back to a PRM file using skeleton injection.
#' The original structure (comments, sections, order) is preserved.
#'
#' @param prm A `prm_list` object.
#' @param path Output file path.
#' @return Invisibly returns the path.
#' @export
prm_write <- function(prm, path) {
    skeleton <- attr(prm, "skeleton")

    if (is.null(skeleton)) {
        stop("prm_list has no skeleton attribute. Cannot write.", call. = FALSE)
    }

    line_map <- attr(prm, "line_map")
    output <- skeleton

    if (!is.null(line_map) && length(line_map) > 0) {
        # Vectorized: use line_map for direct index assignment
        keys <- names(prm)
        # Only process keys that exist in line_map
        common_keys <- intersect(keys, names(line_map))

        if (length(common_keys) > 0) {
            indices <- line_map[common_keys]
            # Filter valid indices
            valid <- indices > 0 & indices <= length(output)
            valid_keys <- common_keys[valid]
            valid_indices <- indices[valid]

            # Vectorized line construction
            new_lines <- paste0(valid_keys, "=", vapply(prm[valid_keys], as.character, character(1)))

            # Single vectorized assignment
            output[valid_indices] <- new_lines
        }
    } else {
        # Fallback: match keys in skeleton lines vectorized
        keys <- names(prm)
        eq_pos <- regexpr("=", output, fixed = TRUE)
        has_eq <- eq_pos > 0

        # Extract key from each line with "="
        line_keys <- character(length(output))
        line_keys[has_eq] <- trimws(substr(output[has_eq], 1, eq_pos[has_eq] - 1))

        # Find which lines match our keys
        match_idx <- match(line_keys, keys)
        lines_to_update <- !is.na(match_idx)

        if (any(lines_to_update)) {
            matched_keys <- keys[match_idx[lines_to_update]]
            new_lines <- paste0(matched_keys, "=", vapply(prm[matched_keys], as.character, character(1)))
            output[lines_to_update] <- new_lines
        }
    }

    writeLines(output, path)
    invisible(path)
}

# -----------------------------------------------------------------------------
# prm_defaults: Load rsatscan's internal defaults
# -----------------------------------------------------------------------------

#' Load Default PRM Parameters
#'
#' Loads rsatscan's internal default parameters as a prm_list.
#'
#' @param version Optional SaTScan version (e.g., "10.3"). If NULL, uses latest (10.3).
#' @return A `prm_list` parsed from bundled template files.
#' @export
prm_defaults <- function(version = NULL) {
    # Available versions (bundled in inst/extdata/prm_template/)
    available_versions <- c(
        "9.2", "9.3", "9.4", "9.5", "9.6", "9.7",
        "10.0", "10.1", "10.2", "10.3"
    )
    default_version <- "10.3"

    # Use default if not specified
    if (is.null(version)) {
        version <- default_version
    }

    # Validate version
    if (!version %in% available_versions) {
        warning(
            sprintf(
                "Version '%s' not found. Available: %s. Using %s.",
                version, paste(available_versions, collapse = ", "), default_version
            ),
            call. = FALSE
        )
        version <- default_version
    }

    # Construct filename (e.g., "10.3" -> "v10_3.prm")
    filename <- paste0("v", gsub("\\.", "_", version), ".prm")

    # Load from bundled template
    template_path <- system.file("extdata", "prm_template", filename,
        package = "epidscan", mustWork = FALSE
    )

    if (template_path == "") {
        stop(sprintf("Template file '%s' not found in package. This is a bug in epidscan.", filename),
            call. = FALSE
        )
    }

    prm_parse(template_path)
}

# -----------------------------------------------------------------------------
# prm_validate: Validate PRM against a reference template
# -----------------------------------------------------------------------------

#' Validate PRM Parameters
#'
#' Validates a parsed PRM against a known version template. Useful for
#' checking external PRM files before use.
#'
#' @param prm A `prm_list` object (from prm_parse).
#' @param version Version to compare against (e.g., "10.3"). If NULL, uses
#'   the prm's version attribute or defaults to "10.3".
#' @return A list with:
#'   \itemize{
#'     \item \code{valid}: TRUE if no critical issues found
#'     \item \code{missing}: Character vector of keys in reference but not in prm
#'     \item \code{extra}: Character vector of keys in prm but not in reference
#'     \item \code{prm_version}: Version found in prm (or NULL)
#'     \item \code{ref_version}: Reference version used for comparison
#'   }
#' @export
prm_validate <- function(prm, version = NULL) {
    # Determine reference version
    prm_version <- attr(prm, "version")

    if (is.null(version)) {
        # Use prm's version if available, otherwise default
        if (!is.null(prm_version)) {
            # Normalize version: "10.3.0" -> "10.3"
            version <- sub("^(\\d+\\.\\d+).*", "\\1", prm_version)
        } else {
            version <- "10.3" # Default to latest
        }
    }

    # Get reference template
    ref <- tryCatch(
        prm_defaults(version),
        error = function(e) {
            warning("Could not load reference template for version ", version,
                ". Using 10.3.",
                call. = FALSE
            )
            prm_defaults("10.3")
        }
    )

    # Compare keys
    prm_keys <- names(prm)
    ref_keys <- names(ref)

    missing_keys <- setdiff(ref_keys, prm_keys)
    extra_keys <- setdiff(prm_keys, ref_keys)

    # Determine validity (no critical missing keys = valid)
    # For now, valid if no missing keys
    valid <- length(missing_keys) == 0

    list(
        valid = valid,
        missing = missing_keys,
        extra = extra_keys,
        prm_version = prm_version,
        ref_version = version
    )
}

# -----------------------------------------------------------------------------
# Print method for prm_list
# -----------------------------------------------------------------------------

#' @export
print.prm_list <- function(x, ...) {
    n_params <- length(x)
    sections <- unique(attr(x, "sections"))
    n_sections <- length(sections)

    cat("SaTScan PRM List\n")
    cat("================\n")
    cat(sprintf("Parameters: %d\n", n_params))
    cat(sprintf("Sections: %d (%s)\n", n_sections, paste(head(sections, 5), collapse = ", ")))
    if (n_sections > 5) cat("  ...\n")

    cat("\nSample values:\n")
    sample_keys <- c("CaseFile", "AnalysisType", "ModelType", "MonteCarloReps")
    for (key in sample_keys) {
        if (key %in% names(x)) {
            cat(sprintf("  %s = %s\n", key, x[[key]]))
        }
    }
    invisible(x)
}
