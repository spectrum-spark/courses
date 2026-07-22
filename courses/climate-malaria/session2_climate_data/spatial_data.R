library(sf)
library(terra)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(tidyr)

# Province (ADM1) boundaries ----------------------------------------------
shape_file <- st_read(
  file.path(data_dir, "shape_files",
            "tha_admbnda_adm1_rtsd_20190221",
            "tha_admbnda_adm1_rtsd_20190221.shp"),
  quiet = TRUE
)
region_names <- shape_file$ADM1_EN

# Preview what we have loaded ---------------------------------------------
shape_file |>
  slice_head(n = 5)     # the first 5 rows as a clean table in quarto

glue::glue("Number of provinces in shapefile: {length(region_names)}")

missing <- setdiff(selected_provinces, region_names)
glue::glue("Provinces not matched in shapefile and dropped: {paste(missing, collapse = ", ")}")

selected_provinces <- intersect(selected_provinces, region_names)
glue::glue("Provinces carried forward (matched to boundaries): 
    {length(selected_provinces)}")

# One netCDF file per month; sorting the file paths puts them in time order.
tas_files <- sort(list.files(
  file.path(data_dir, "reanalysis", "monthly-averaged", "2t"),
  pattern = "\\.nc$", recursive = TRUE, full.names = TRUE))
pr_files <- sort(list.files(
  file.path(data_dir, "reanalysis", "monthly-averaged", "tp"),
  pattern = "\\.nc$", recursive = TRUE, full.names = TRUE))

tas_r <- rast(tas_files)   # temperature, one layer per month (Kelvin)
pr_r  <- rast(pr_files)    # total precipitation, one layer per month (metres)

# Restrict to the Thailand region 
region_ext <- ext(97, 108, 5, 22)
tas_r <- crop(tas_r, region_ext)
pr_r  <- crop(pr_r,  region_ext)

# Rainfall time base. ERA5 tp and CMIP flux are per-day rates; multiplying by the days
# in a month reports monthly totals. This ONE constant is the whole monthly-vs-daily
# switch: set it to 1 for daily rates. It is reused by the CMIP conversion in Part 4.
DAYS_PER_MONTH <- 30
pr_r  <- pr_r * DAYS_PER_MONTH   # tp: mean daily rate (m/day) -> monthly total (m/month)

# Recover the month for each layer from the filename (YYYYMMDD-...)
month_from_file <- function(f)
  floor_date(as.Date(sub(".*_(\\d{8})-\\d{8}\\.nc$", "\\1", basename(f)),
                     format = "%Y%m%d"), "month")
clim_months <- month_from_file(tas_files)
glue::glue("Reanalysis covers {length(clim_months)} months: 
    {format(min(clim_months))} to {format(max(clim_months))}")

# Map of the time-mean temperature and rainfall over the region -----------
tas_mean_c <- mean(tas_r) - 273.15          # Kelvin -> Celsius for display
pr_mean    <- mean(pr_r)

map_df_t <- as.data.frame(tas_mean_c, xy = TRUE) |>
  rename(value = last_col())
map_df_r <- as.data.frame(pr_mean, xy = TRUE) |>
  rename(value = last_col())

m_t <- ggplot(map_df_t, aes(x, y, fill = value)) +
  geom_raster() +
  geom_sf(data = shape_file, fill = NA, colour = "grey30",
          linewidth = 0.2, inherit.aes = FALSE) +
  scale_fill_steps2(low = "#2166ac", mid = "white", high = "#b2182b",
                    midpoint = 25, limits = c(20, 30), breaks = seq(20, 30, 1),
                    name = "°C", oob = scales::squish) +
  coord_sf(expand = FALSE) +
  labs(title = "Mean temperature", x = "lon", y = "lat") +
  theme_bw()

m_r <- ggplot(map_df_r, aes(x, y, fill = value)) +
  geom_raster() +
  geom_sf(data = shape_file, fill = NA, colour = "grey30",
          linewidth = 0.2, inherit.aes = FALSE) +
  scale_fill_viridis_b(name = "m/month", option = "mako", n.breaks = 8) +
  coord_sf(expand = FALSE) +
  labs(title = "Mean rainfall", x = "lon", y = "lat") +
  theme_bw()

m_t + m_r



# Assign each grid cell to the province whose polygon contains its *centre*.
grid_xy  <- terra::xyFromCell(tas_r, seq_len(terra::ncell(tas_r)))
grid_pts <- sf::st_as_sf(as.data.frame(grid_xy), coords = c("x", "y"),
                         crs = sf::st_crs(shape_file))
within   <- sf::st_within(grid_pts, shape_file)          # cell centre in polygon?
cell_region <- vapply(within,
                      function(h) if (length(h)) h[1] else NA_integer_,
                      integer(1))

# Check 1: colour each grid cell by the province it landed in.
assign_df <- tibble::tibble(
  x = grid_xy[, 1],
  y = grid_xy[, 2],
  province_id = cell_region
)
p_assign <- assign_df |>
  filter(!is.na(province_id)) |>
  ggplot(aes(x, y, fill = province_id)) +
  geom_raster() +
  geom_sf(data = shape_file, fill = NA, colour = "grey20",
          linewidth = 0.2, inherit.aes = FALSE) +
  scale_fill_viridis_c(guide = "none") +
  coord_sf(expand = FALSE) +
  labs(title = "Grid cells coloured by assigned province", x = "lon", y = "lat") +
  theme_bw()

# Check 2: which provinces are carried into the model (selected in Part 2)?
modelled_sf <- shape_file |>
  mutate(
    modelled = if_else(
      ADM1_EN %in% selected_provinces,
      "modelled",
      "not modelled"
    )
  )
p_modelled <- ggplot(modelled_sf) +
  geom_sf(aes(fill = modelled), colour = "grey40", linewidth = 0.2) +
  scale_fill_manual(values = c("modelled" = "#08519c", "not modelled" = "grey88"),
                    name = NULL) +
  coord_sf(expand = FALSE) +
  labs(title = "Provinces carried into the model", x = "lon", y = "lat") +
  theme_bw()

p_assign | p_modelled   # side by side (patchwork)

# Provincial means, area-weighted by cos(latitude); see technical note.
# The assignment and weights are reused for the CMIP data in Part 4.
cell_w <- cos(grid_xy[, 2] * pi / 180)                    # per-cell area weight (cos latitude)
region_means <- function(r) {
  vals <- terra::values(r)                                # ncell x nlayer
  out  <- matrix(NA_real_, length(region_names), ncol(vals),
                 dimnames = list(region_names, NULL))
  for (k in seq_along(region_names)) {
    sel <- which(cell_region == k)
    if (!length(sel)) next
    w    <- cell_w[sel]
    vsel <- vals[sel, , drop = FALSE]
    # Weighted mean over the NON-NA cells: normalise by the weights actually used, so a
    # layer that is all NA gives NA (not 0). Matches xarray's weighted().mean(skipna);
    # it matters for CMIP, where some models are missing (NA) in the final months.
    out[k, ] <- colSums(vsel * w, na.rm = TRUE) / colSums((!is.na(vsel)) * w)
  }
  out
}

tas_prov <- region_means(tas_r)
pr_prov  <- region_means(pr_r)

# Tidy into long tables, keeping only the selected provinces
to_long <- function(mat, value_name) {
  m <- as.matrix(mat)
  rownames(m) <- region_names
  colnames(m) <- as.character(clim_months)
  as.data.frame(m) |>
    tibble::rownames_to_column("province_en") |>
    pivot_longer(-province_en, names_to = "month", values_to = value_name) |>
    mutate(month = as.Date(month))
}

clim_long <- to_long(tas_prov, "temp_k") |>
  left_join(to_long(pr_prov, "rain_m"), by = c("province_en", "month")) |>
  mutate(temp_c = temp_k - 273.15) |>
  filter(province_en %in% selected_provinces)

clim_long |>
  slice_head(n = 6)


ggplot(clim_long, aes(month, temp_c)) +
  geom_line() + geom_point(size = 0.5) +
  facet_wrap(~ province_en, scales = "free_y") +
  labs(x = "date", y = "monthly mean temperature (°C)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggplot(clim_long, aes(month, rain_m)) +
  geom_line() + geom_point(size = 0.5) +
  facet_wrap(~ province_en, scales = "free_y") +
  labs(x = "date", y = "monthly rainfall (m/month)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


### Part 4

library(ncdf4)

# get common models across all scenarios and all variables
cmip_dir <- file.path(data_dir, "cmip6", "monthly-averaged")
cmip_files <- list(
  ssp126_tas = "cmip6_regrided_model_ensemble.Thai_region.ssp126.tas.nc",
  ssp585_tas = "cmip6_regrided_model_ensemble.Thai_region.ssp585.tas.nc",
  ssp126_pr  = "cmip6_regrided_model_ensemble.Thai_region.ssp126.pr.nc",
  ssp585_pr  = "cmip6_regrided_model_ensemble.Thai_region.ssp585.pr.nc"
)

# Read one CMIP variable; dimensions are read by name so storage order does not matter.
read_cmip_var <- function(path, var, from_date = NULL) {
  nc  <- nc_open(path)
  on.exit(nc_close(nc))
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  tim <- ncvar_get(nc, "time")
  models <- ncvar_get(nc, "model")
  # time units -> dates  S
  tunits <- ncatt_get(nc, "time", "units")$value
  origin <- as.Date(sub(".*since ", "", tunits))
  dates  <- floor_date(origin + tim, "month")
  arr <- ncvar_get(nc, var)                       # dims in file order
  dn <- vapply(nc$var[[var]]$dim, function(d) d$name, character(1))
  # reorder to lon, lat, time, model
  perm <- match(c("lon", "lat", "time", "model"), dn)
  arr <- aperm(arr, perm)
  # Trim to the reanalysis start so the fit and projection share a start month.
  if (!is.null(from_date)) {
    keep <- dates >= from_date
    arr  <- arr[, , keep, , drop = FALSE]
    dates <- dates[keep]
  }
  list(arr = arr, lon = lon, lat = lat, dates = dates, models = models)
}

cmip_start <- min(clim_months)   # reanalysis start month
tas126 <- read_cmip_var(file.path(cmip_dir, cmip_files$ssp126_tas), "tas", cmip_start)
glue::glue("CMIP6 grid: {length(tas126$lon)}, lon x {length(tas126$lat)}, lat;
    {length(tas126$models)}, models; {length(tas126$dates)} months")


# Precipitation flux (kg m^-2 s^-1) -> monthly-total depth (m): seconds per day (86400)
# x days per month (DAYS_PER_MONTH, defined in Part 3) / water density (1000 kg m^-3).
# The monthly-vs-daily switch lives entirely in DAYS_PER_MONTH; this line just uses it.
PR_FLUX_TO_M <- 86400 * DAYS_PER_MONTH / 1000

ref_grid <- tas_r[[1]]        # reanalysis grid (already cropped to region)

# Build a north-up, multi-layer SpatRaster from one model's [lon, lat, time] slice
# (lon/lat treated as cell centres, so the extent is padded half a cell each side).
slice_to_rast <- function(slc, lon, lat, ref) {
  lon_o <- order(lon)
  lat_o <- order(lat)                               # index ascending
  slc <- slc[lon_o, lat_o, , drop = FALSE]          # lon asc, lat asc
  a <- aperm(slc, c(2, 1, 3))                        # -> lat x lon x time
  a <- a[dim(a)[1]:1, , , drop = FALSE]              # flip: north (max lat) = row 1
  r <- rast(a)                                       # 3rd dim -> layers
  lo <- sort(lon)
  la <- sort(lat)
  dx <- mean(diff(lo))
  dy <- mean(diff(la))
  ext(r) <- c(min(lo) - dx/2, max(lo) + dx/2,
              min(la) - dy/2, max(la) + dy/2)
  crs(r) <- crs(ref)                                 # match the reanalysis grid
  r
}

# Every model -> provincial means, keeping the per-model detail (province x month x
# model) that the Part 6 projection needs for the spread.
cmip_provincial_bymodel <- function(cv, var, ref, provinces) {
  nm <- length(cv$models)
  # CMIP_MODEL_CAP env var caps the number of models (a faster/debug run); 0 = all.
  cap <- suppressWarnings(as.integer(Sys.getenv("CMIP_MODEL_CAP", "")))
  if (!is.na(cap) && cap > 0) nm <- min(nm, cap)
  out <- vector("list", nm)
  for (m in seq_len(nm)) {
    slc <- cv$arr[, , , m]                     # lon x lat x time
    r <- slice_to_rast(slc, cv$lon, cv$lat, ref)
    names(r) <- as.character(cv$dates)
    if (var == "pr") r <- r * PR_FLUX_TO_M     # flux -> depth (m); see PR_FLUX_TO_M above
    else             r <- r - 273.15           # Kelvin -> Celsius
    r <- resample(r, ref, method = "bilinear") # interpolate onto reanalysis grid
    pm <- region_means(r)                       # strict cell-centre assignment (as Part 3)
    out[[m]] <- pm[provinces, , drop = FALSE]
  }
  arr3 <- simplify2array(out)                  # province x month x model
  list(arr = arr3, dates = cv$dates, provinces = provinces)
}

# Ensemble summary (mean + interquartile band) from a per-model array.
summarise_cmip <- function(bm, scenario, var) {
  arr3 <- bm$arr
  nt <- length(bm$dates)
  qf <- function(p) as.vector(apply(arr3, c(1, 2), quantile, probs = p, na.rm = TRUE))
  tibble::tibble(
    province_en = rep(bm$provinces, times = nt),
    month       = rep(bm$dates, each = length(bm$provinces)),
    scenario = scenario, variable = var,
    mean = as.vector(apply(arr3, c(1, 2), mean, na.rm = TRUE)),
    q05 = qf(0.05), q25 = qf(0.25), q75 = qf(0.75), q95 = qf(0.95)
  )
}


tas585 <- read_cmip_var(file.path(cmip_dir, cmip_files$ssp585_tas), "tas", cmip_start)
pr126  <- read_cmip_var(file.path(cmip_dir, cmip_files$ssp126_pr),  "pr",  cmip_start)
pr585  <- read_cmip_var(file.path(cmip_dir, cmip_files$ssp585_pr),  "pr",  cmip_start)

# Restrict all four files to their common models, in the same order (the projection
# pairs temperature and rainfall model-by-model).
common_models <- sort(Reduce(intersect,
  list(tas126$models, tas585$models, pr126$models, pr585$models)))
align_models <- function(cv) {
  idx <- match(common_models, cv$models)
  cv$arr    <- cv$arr[, , , idx, drop = FALSE]
  cv$models <- common_models
  cv
}
tas126 <- align_models(tas126)
tas585 <- align_models(tas585)
pr126 <- align_models(pr126)
pr585 <- align_models(pr585)
glue::glue("CMIP models in common across all four files: {length(common_models)}")

# Per-model provincial climate (kept for the Part 6 projection)
cmip_tas <- list(
  "SSP1-2.6" = cmip_provincial_bymodel(tas126, "tas", ref_grid, selected_provinces),
  "SSP5-8.5" = cmip_provincial_bymodel(tas585, "tas", ref_grid, selected_provinces))
cmip_pr <- list(
  "SSP1-2.6" = cmip_provincial_bymodel(pr126, "pr", ref_grid, selected_provinces),
  "SSP5-8.5" = cmip_provincial_bymodel(pr585, "pr", ref_grid, selected_provinces))

# Ensemble summary for plotting
cmip_summary <- bind_rows(
  summarise_cmip(cmip_tas[["SSP1-2.6"]], "SSP1-2.6", "tas"),
  summarise_cmip(cmip_tas[["SSP5-8.5"]], "SSP5-8.5", "tas"),
  summarise_cmip(cmip_pr[["SSP1-2.6"]],  "SSP1-2.6", "pr"),
  summarise_cmip(cmip_pr[["SSP5-8.5"]],  "SSP5-8.5", "pr")
)


# Model-and-time mean CMIP field on the reanalysis grid, for one scenario/variable.
cmip_mean_grid <- function(cv, var) {
  fld <- apply(cv$arr, c(1, 2), mean, na.rm = TRUE)   # lon x lat: mean over time and model
  r <- slice_to_rast(array(fld, c(dim(fld), 1)), cv$lon, cv$lat, ref_grid)
  if (var == "pr") r <- r * PR_FLUX_TO_M else r <- r - 273.15
  resample(r, ref_grid, method = "bilinear")
}

cmip_map <- function(r, title, fill_scale) {
  d <- as.data.frame(r, xy = TRUE) |>
    rename(value = last_col())
  ggplot(d, aes(x, y, fill = value)) +
    geom_raster() +
    geom_sf(data = shape_file, fill = NA, colour = "grey30",
            linewidth = 0.2, inherit.aes = FALSE) +
    fill_scale +
    coord_sf(expand = FALSE) +
    labs(title = title, x = "lon", y = "lat") +
    theme_bw()
}

# Banded scales: blue-white-red for temperature,mako for rainfall. Rain limits 
# are the Part 4 daily values x DAYS_PER_MONTH (monthly totals).
temp_scale <- function() scale_fill_steps2(low = "#2166ac", mid = "white",
  high = "#b2182b", midpoint = 25, limits = c(20, 30), breaks = seq(20, 30, 1),
  name = "°C", oob = scales::squish)
rain_scale <- function() scale_fill_viridis_b(name = "m/month", option = "mako",
  limits = c(0.0036, 0.0084) * DAYS_PER_MONTH,
  breaks = seq(0.0036, 0.0084, 0.0006) * DAYS_PER_MONTH, oob = scales::squish)

t126 <- cmip_map(cmip_mean_grid(tas126, "tas"), "Mean temperature (SSP1-2.6)", temp_scale())
t585 <- cmip_map(cmip_mean_grid(tas585, "tas"), "Mean temperature (SSP5-8.5)", temp_scale())
r126 <- cmip_map(cmip_mean_grid(pr126, "pr"),  "Mean rainfall (SSP1-2.6)",   rain_scale())
r585 <- cmip_map(cmip_mean_grid(pr585, "pr"),  "Mean rainfall (SSP5-8.5)",   rain_scale())

(t126 + t585) / (r126 + r585)

example_province <- selected_provinces[1]
prov_idx  <- match(example_province, selected_provinces)
one_model <- min(21L, dim(cmip_tas[["SSP1-2.6"]]$arr)[3])   # one member, to contrast with the ensemble

lab  <- c(tas = "temperature (°C)", pr = "total precipitation (m/month)")
cols <- c("SSP1-2.6" = "#2166ac", "SSP5-8.5" = "#b2182b")

# Monthly model-mean, from the ensemble summary
monthly <- cmip_summary |>
  filter(province_en == example_province)

ggplot(monthly, aes(month, mean, colour = scenario)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y", labeller = labeller(variable = lab)) +
  scale_colour_manual(values = cols) +
  labs(x = "date", y = NULL,
       title = paste0("CMIP6 monthly model-mean for ", example_province)) +
  theme_bw()

# Annual: ensemble mean, one member, and interquartile band across models
annual <- monthly |>
  mutate(year = year(month)) |>
  group_by(scenario, variable, year) |>
  summarise(mean = mean(mean), q25 = mean(q25), q75 = mean(q75), .groups = "drop")

# one member's annual series for this province (dips into the per-model arrays)
one_annual <- bind_rows(
  lapply(names(cmip_tas), function(sc) tibble::tibble(
    year = year(cmip_tas[[sc]]$dates), scenario = sc, variable = "tas",
    value = cmip_tas[[sc]]$arr[prov_idx, , one_model])),
  lapply(names(cmip_pr), function(sc) tibble::tibble(
    year = year(cmip_pr[[sc]]$dates), scenario = sc, variable = "pr",
    value = cmip_pr[[sc]]$arr[prov_idx, , one_model]))
) |>
  group_by(scenario, variable, year) |>
  summarise(value = mean(value), .groups = "drop")

annual_lines <- bind_rows(
  transmute(annual, year, scenario, variable, value = mean, series = "model mean"),
  transmute(one_annual, year, scenario, variable, value, series = "one model")
)

ggplot() +
  geom_ribbon(data = annual, aes(year, ymin = q25, ymax = q75, fill = scenario), alpha = 0.2) +
  geom_line(data = annual_lines,
            aes(year, value, colour = scenario, linetype = series), linewidth = 0.7) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y", labeller = labeller(variable = lab)) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_linetype_manual(name = "series", values = c("model mean" = "solid", "one model" = "dashed")) +
  labs(x = "year", y = NULL,
       title = paste0("CMIP6 annual projections for ", example_province),
       subtitle = "Solid = model mean, dashed = one model, band = interquartile range (only the middle 50% of models)") +
  theme_bw()

