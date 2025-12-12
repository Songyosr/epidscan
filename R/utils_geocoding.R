# Geocoding Functions
#
# This module provides functions to construct Thai addresses from administrative
# codes and geocode them using the tidygeocoder package.

#' @importFrom dplyr mutate select distinct left_join rename case_when row_number %>%
#' @importFrom stringr str_pad str_squish
#' @importFrom sf st_drop_geometry
#' @importFrom tidygeocoder geocode
#' @export
#'
#' Combines house number, moo, and administrative names into a full address string.
#' Requires a dataframe with columns: address, moo, tmb_code, amp_code, chw_code.
#' Also requires a reference dataframe (admin_ref) with Thai names for admin units.
#'
#' @param data Dataframe containing case data
#' @param admin_ref Dataframe/SF object with columns: ADM3_PCODE, ADM3_TH, ADM2_TH, ADM1_TH
#' @return Dataframe with new 'full_address' column
construct_address <- function(data, admin_ref) {
    # Prepare admin reference lookup
    # ADM3_PCODE is the key (e.g., "TH900101")
    # We need to match it with constructed code from data

    # Ensure admin_ref is a regular dataframe (drop geometry if sf)
    if (inherits(admin_ref, "sf")) {
        admin_ref <- st_drop_geometry(admin_ref)
    }

    # Create lookup table
    admin_lookup <- admin_ref %>%
        select(ADM3_PCODE, ADM3_TH, ADM2_TH, ADM1_TH) %>%
        distinct()

    # Process data
    data_processed <- data %>%
        mutate(
            # Construct Tambon Code to match ADM3_PCODE (e.g., "TH900101")
            # Assuming chw_code, amp_code, tmb_code are present
            # Pad with zeros if necessary (assuming input might be numeric or string)
            chw_pad = str_pad(chw_code, 2, pad = "0"),
            amp_pad = str_pad(amp_code, 2, pad = "0"),
            tmb_pad = str_pad(tmb_code, 2, pad = "0"),
            match_code = paste0("TH", chw_pad, amp_pad, tmb_pad)
        ) %>%
        left_join(admin_lookup, by = c("match_code" = "ADM3_PCODE")) %>%
        mutate(
            # Construct Address String
            # Format: House No, Moo X, Road [Name], Tambon [Name], Amphoe [Name], Province [Name], Thailand
            full_address = paste0(
                ifelse(is.na(address), "", paste0(address, " ")),
                ifelse(is.na(moo) | moo == "-" | moo == "", "", paste0("หมู่ ", moo, " ")),
                ifelse(is.na(road) | road == "-" | road == "", "", paste0("ถ.", road, " ")),
                ifelse(is.na(ADM3_TH), "", paste0("ต.", ADM3_TH, " ")),
                ifelse(is.na(ADM2_TH), "", paste0("อ.", ADM2_TH, " ")),
                ifelse(is.na(ADM1_TH), "", paste0("จ.", ADM1_TH, " ")),
                "Thailand"
            )
        ) %>%
        # Clean up double spaces
        mutate(full_address = str_squish(full_address))

    return(data_processed)
}

#' Geocode Addresses with Fallback
#'
#' Uses tidygeocoder to geocode a vector of addresses.
#' Implements a fallback strategy:
#' 1. Full Address (House No + Moo + Admin)
#' 2. Moo + Admin (if Moo exists)
#' 3. Admin only (Tambon + Amphoe + Province)
#'
#' @param data Dataframe with address components
#' @param method Geocoding method ('arcgis')
#' @return Dataframe with 'lat', 'long' columns added
#' @export
geocode_cases <- function(data, method = "arcgis") {
    message("Starting geocoding using method: ", method)

    # Prepare fallback address columns
    # Note: Nominatim works best with comma separation or clear hierarchy
    data_prep <- data %>%
        mutate(
            # Level 1: Full Address
            addr_l1 = full_address,

            # Level 2: Moo + Road + Admin
            # Try: Moo X, Road Y, Tambon Z, Amphoe W, Province V
            addr_l2 = paste0(
                ifelse(is.na(moo) | moo == "-" | moo == "", "", paste0("หมู่ ", moo, ", ")),
                ifelse(is.na(road) | road == "-" | road == "", "", paste0("ถ.", road, ", ")),
                ifelse(is.na(ADM3_TH), "", paste0("ต.", ADM3_TH, ", ")),
                ifelse(is.na(ADM2_TH), "", paste0("อ.", ADM2_TH, ", ")),
                ifelse(is.na(ADM1_TH), "", paste0("จ.", ADM1_TH))
            ),

            # Level 3: Admin Only (Tambon Centroid)
            # Try: Tambon Y, Amphoe Z, Province W
            addr_l3 = paste0(
                ifelse(is.na(ADM3_TH), "", paste0("ต.", ADM3_TH, ", ")),
                ifelse(is.na(ADM2_TH), "", paste0("อ.", ADM2_TH, ", ")),
                ifelse(is.na(ADM1_TH), "", paste0("จ.", ADM1_TH))
            )
        )

    # RE-IMPLEMENTATION WITH ROW ID FOR SAFETY
    data_w_id <- data_prep %>% mutate(geo_id = row_number())

    # 1. Level 1
    message("Attempting Level 1: Full Address...")
    r1 <- data_w_id %>% geocode(address = addr_l1, method = method, lat = lat, long = long)

    # 2. Level 2 (Moo)
    missing_1 <- r1 %>% filter(is.na(lat))
    if (nrow(missing_1) > 0) {
        message("Attempting Level 2 (Moo) for ", nrow(missing_1), " cases...")
        r2 <- missing_1 %>%
            select(-lat, -long) %>%
            geocode(address = addr_l2, method = method, lat = lat, long = long)

        # Update main results manually to avoid rows_update issues with NAs
        # Join r2 back to r1
        r1 <- r1 %>%
            left_join(r2 %>% select(geo_id, lat2 = lat, long2 = long), by = "geo_id") %>%
            mutate(
                lat = ifelse(is.na(lat), lat2, lat),
                long = ifelse(is.na(long), long2, long)
            ) %>%
            select(-lat2, -long2)
    }

    # 3. Level 3 (Admin)
    missing_2 <- r1 %>% filter(is.na(lat))
    if (nrow(missing_2) > 0) {
        message("Attempting Level 3 (Admin) for ", nrow(missing_2), " cases...")
        r3 <- missing_2 %>%
            select(-lat, -long) %>%
            geocode(address = addr_l3, method = method, lat = lat, long = long)

        # Update main results
        r1 <- r1 %>%
            left_join(r3 %>% select(geo_id, lat3 = lat, long3 = long), by = "geo_id") %>%
            mutate(
                lat = ifelse(is.na(lat), lat3, lat),
                long = ifelse(is.na(long), long3, long)
            ) %>%
            select(-lat3, -long3)
    }

    # Add metadata about precision
    final_res <- r1 %>%
        mutate(
            geo_precision = case_when(
                is.na(lat) ~ "failed",
                geo_id %in% (missing_2$geo_id) ~ "tambon",
                geo_id %in% (missing_1$geo_id) ~ "moo",
                TRUE ~ "exact"
            )
        ) %>%
        rename(lat_geo = lat, long_geo = long)

    return(final_res)
}

#' Validate Coordinates
#'
#' Checks if coordinates fall within the expected bounding box (Songkhla).
#'
#' @param data Dataframe with lat/long columns
#' @param lat_col Name of latitude column
#' @param long_col Name of longitude column
#' @return Dataframe with 'valid_geo' boolean column
#' @export
validate_coordinates <- function(data, lat_col = "lat", long_col = "long") {
    # Songkhla approximate bounding box
    # Lat: 6.2 - 7.9
    # Long: 100.0 - 101.2

    data %>%
        mutate(
            valid_geo = case_when(
                is.na(.data[[lat_col]]) | is.na(.data[[long_col]]) ~ FALSE,
                .data[[lat_col]] >= 6.0 & .data[[lat_col]] <= 8.0 &
                    .data[[long_col]] >= 99.5 & .data[[long_col]] <= 101.5 ~ TRUE,
                TRUE ~ FALSE
            )
        )
}
