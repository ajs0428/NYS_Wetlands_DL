#### Reclassify and combine ADK wetland data

library(sf)
library(terra)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyterra)
library(future)
library(future.apply)
library(purrr)

set.seed(11)


########################################################################################

# args <- c(
#     64,
#     "Data/Training_Data/02_Done_Reviewed_NWI_Data/"
# )
# 
# args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here
# 
# cat("these are the arguments: \n", 
#     "1) Cluster number for HUC groups:", args[1], "\n", 
#     "2) path the reviewed training data :", args[2]
# )


setGDALconfig("GDAL_PAM_ENABLED", "FALSE") # does not create aux.xml files
########################################################################################

l_chm <- list.files("Data/CHMs/HUC_CHMs/", pattern = ".tif", full.names = TRUE) 
l_chm_huc_nums <- str_extract(l_chm, "(?<=huc_)\\d+(?=_)") |> unique() # each HUC present in the CHMs 

l_naip <- list.files("Data/NAIP/HUC_NAIP_Processed/", pattern = ".tif", full.names = TRUE) 

########################################################################################

clusters <- st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", quiet = TRUE)

adk_files <- list.files("Data/ADK", pattern = ".shp$", full.names = TRUE) |> 
    keep( \(x) !str_detect(x, "RegWetlandAreasParkPromulgated"))
adk_read_filter <- function(filename){
    v <- st_read(filename, quiet = TRUE) |> 
        filter(!st_is_empty(geometry)) |> 
        filter(st_is_valid(geometry)) |> 
        filter(!is.na(NWILABEL)) |> 
        mutate(SYSTEM = as.character(SYSTEM),
               MOD_CLASS = case_when(
                   str_detect(SYSTEM, "^U$") ~ "UPL",
                   str_detect(CLASS1, "OW|AB|US|UB|SB|RB") ~ "OWW",
                   str_detect(CLASS1, "SS") ~ "SSW",
                   str_detect(CLASS1, "EM") ~ "EMW",
                   str_detect(CLASS1, "FO") ~ "FSW",
                   .default = "REVIEW"
               )) |> 
        select(SYSTEM, CLASS1, CLASS2, MOD_CLASS, geometry) 
    if(st_crs(v) != st_crs("EPSG:6347")){
        vt <- st_transform(v, "EPSG:6347")
    } else {
        vt <- v
    }
    return(vt)
}

adk_wetlands <- lapply(adk_files, adk_read_filter) |> 
    bind_rows()

hucs_with_adk <- clusters[rowSums(st_intersects(clusters, adk_wetlands, sparse = FALSE)) != 0, ]
huc_nums_with_adk <- hucs_with_adk[["huc12"]] |> keep(\(x) !str_detect(x, "043001081604"))

########################################################################################
#### Regulated wetlands are different
    ### There are no classification labels but some of them identify wetland not in the other ADK
    ### Use the Laba_wetland_chm_extract_classify to assign classes
regulated_adk <- st_read("Data/ADK/RegWetlandAreasParkPromulgated_UTM83.shp", quiet = TRUE) |> 
    st_transform("EPSG:6347") |> 
    filter(!st_is_empty(geometry)) |>
    filter(st_is_valid(geometry))

hucs_with_regulated_adk <- clusters[rowSums(st_intersects(clusters, regulated_adk, sparse = FALSE)) != 0, ]
huc_nums_with_regulated_adk <- hucs_with_regulated_adk[["huc12"]] |> keep(\(x) !str_detect(x, "043001081604")) #Remove this one huge huc

regadk_wetland_chm_extract_classify <- function(huc_num){
    
    tryCatch({
        if(sum(length(l_naip[str_detect(l_naip , huc_num)]), 
               length(l_chm[str_detect(l_chm , huc_num)])) > 1 ){
            print("Files Exist")
            r_chm <- rast(l_chm[str_detect(l_chm , huc_num)])
            r_naip <- rast(l_naip[str_detect(l_naip , huc_num)])
            stack <- c(r_chm, r_naip)
            huc <- clusters[grepl(pattern = huc_num, x = clusters$huc12), ]
            cluster_num <-  clusters[grepl(pattern = huc_num, x = clusters$huc12), ][["cluster"]]
            
            filename <- paste0("Data/Training_Data/ADK_HUC_Processed/ADK_regulated_cluster_", cluster_num, "_huc_", huc_num, ".gpkg")
            
            if(!file.exists(filename)){
                message(paste0("Creating New ADK Regulated Reclass File: ", filename))
                adk_reg_huc <- st_intersection(regulated_adk, huc)  |> vect()
                adk_reg_wet_chm <- terra::extract(stack, adk_reg_huc, "mean", bind = TRUE) |>
                    tidyterra::mutate(
                        MOD_CLASS = dplyr::case_when(
                            CHM <= 1.0 & ndwi > 0.2 ~ "OWW",
                            CHM > 1.0 & CHM <= 3.5 & ndwi > 0.2 ~ "EMW",
                            CHM >= 1.0 & CHM <= 5.0 & ndwi < 0.2~ "SSW",
                            CHM > 5.0 ~ "FSW",
                            CHM <= 3.5 ~ "EMW",
                            CHM > 3.5 & CHM <= 5.0 ~ "SSW",
                            .default = "REVIEW"
                        ))
                
                print(adk_reg_wet_chm)
                writeVector(adk_reg_wet_chm, filename = filename, overwrite = TRUE)
                rm(adk_reg_wet_chm)
                #return(wet_chm)
            } else {
                message(paste0("NWI Reclass File Aleady Exists: ", filename))
            }
        } else {
            print("No Files")
        }
        
    }, error = function(e){
        message(e$message)
        return(NA)
    })
    return(invisible(NULL))
    gc()
}

lapply(huc_nums_with_regulated_adk, regadk_wetland_chm_extract_classify)


########################################################################################

# Extract all ADK wetlands in a HUC 

adk_singlehuc_fun <- function(huc_num){
    cluster_num <- clusters[grepl(pattern = huc_num, x = clusters$huc12), ][["cluster"]]
    huc <- clusters[grepl(pattern = huc_num, x = clusters$huc12), ]
    fn <- paste0("Data/Training_Data/ADK_HUC_Processed/ADK_WCT_cluster_", cluster_num, "_huc_", huc_num, ".gpkg")
    
    if(!file.exists(fn)){
        
        adk_huc <- st_intersection(adk_wetlands, huc) |> #adk within test huc
            select(MOD_CLASS, huc12, cluster)
        # adk$MOD_CLASS <- factor(adk_huc$MOD_CLASS, levels = c("EMW", "FSW", "OWW", "SSW", "UPL"))
        if(nrow(adk_huc) > 0){
            st_write(adk_huc, dsn = fn,
                     append = FALSE)   
        } else {
            Message("Zero adk Features")
        }
    } else {
        message(paste0("File for huc: ", huc_num, " already exists"))
    }
    
    return(NULL)
}

########################################################################################



#### Single core/sequential 
# 
# lapply(nwi_chm_rcl_huc_list[1], adk_nwi_cmb_fun)

lapply(huc_nums_with_adk, adk_singlehuc_fun)

#### Parallel 
if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
print(corenum)
options(future.globals.maxSize= 32.0 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr)

future_lapply(nwi_chm_rcl_huc_list, adk_nwi_cmb_fun, future.seed = TRUE, 
              future.packages = c("terra", "sf", "dplyr", "tidyr", "stringr"),
              future.globals = TRUE)
