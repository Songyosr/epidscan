# epidscan 0.2.0

## Major API Overhaul: The Stateless Era
This release introduces a fundamental re-architecture of the package, moving away from global state management to a pure, functional design.

### New Features
*   **`ss_tbl` System**: New S3 classes for robust input handling.
    *   `as_satscan_case()`: Create typesafe Case files.
    *   `as_satscan_population()`: Create typesafe Population files.
    *   `as_satscan_coordinates()`: Native support for `sf` objects and auto-detection of Coordinate systems.
*   **`satscanr()`**: The new core engine. It is stateless (no `ssenv`), accepting `ss_tbl` inputs and returning parsed results directly.
*   **Parameter Management**:
    *   `prm_parse()` / `prm_write()`: New robust parser that preserves comments and structure of SaTScan parameter files.
    *   `prm_validate()`: Validate your settings against official SaTScan templates before execution.

### Improvements
*   **Date Inference**: Automatically detects the time range from your data if `StartDate`/`EndDate` are missing.
*   **Output Management**: Results are now generated in a `tempdir()` by default to keep your workspace clean. Use `output_dir` to save them explicitly.

### Breaking Changes
*   **Replaced**: `epid_satscan()` is superseded by the `as_satscan_*` + `satscanr` workflow.
*   **Superseded**: `prep_cas`, `prep_geo` functions now point to the new `as_satscan_*` equivalents.

# epidscan 0.1.0

* Initial Release
