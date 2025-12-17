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

    # Initialize
    result <- list()
    sections <- character()
    line_map <- integer()
    current_section <- "Unknown"

    for (i in seq_along(lines)) {
        line <- lines[i]
        trimmed <- trimws(line)

        # Skip empty lines
        if (!nzchar(trimmed)) next

        # Skip comment lines
        if (startsWith(trimmed, ";")) next

        # Section header
        if (grepl("^\\[.+\\]$", trimmed)) {
            current_section <- gsub("^\\[|\\]$", "", trimmed)
            next
        }

        # Key=Value line
        eq_pos <- regexpr("=", line, fixed = TRUE)
        if (eq_pos > 0) {
            key <- trimws(substr(line, 1, eq_pos - 1))
            value <- trimws(substr(line, eq_pos + 1, nchar(line)))

            if (nzchar(key)) {
                result[[key]] <- value
                sections[key] <- current_section
                line_map[key] <- i
            }
        }
    }

    # Attach metadata as attributes
    attr(result, "skeleton") <- lines
    attr(result, "sections") <- sections
    attr(result, "line_map") <- line_map

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

    # Use line_map for O(1) lookup instead of O(n) grep per key
    line_map <- attr(prm, "line_map")
    output <- skeleton

    if (!is.null(line_map) && length(line_map) > 0) {
        # Fast path: use pre-computed line numbers
        for (key in names(prm)) {
            if (key %in% names(line_map)) {
                idx <- line_map[[key]]
                if (idx > 0 && idx <= length(output)) {
                    output[idx] <- paste0(key, "=", prm[[key]])
                }
            }
        }
    } else {
        # Fallback: build replacement patterns vectorized
        # Create a lookup: key -> new_line
        keys <- names(prm)
        new_lines <- paste0(keys, "=", vapply(prm, as.character, character(1)))
        names(new_lines) <- keys

        # For each line, check if it's a Key= line and replace
        for (i in seq_along(output)) {
            line <- output[i]
            # Quick check: skip non-assignment lines
            eq_pos <- regexpr("=", line, fixed = TRUE)
            if (eq_pos > 0) {
                key <- trimws(substr(line, 1, eq_pos - 1))
                if (key %in% keys) {
                    output[i] <- new_lines[[key]]
                }
            }
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
