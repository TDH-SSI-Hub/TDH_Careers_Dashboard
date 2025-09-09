# Author: TDH Analytics ----
# Contact info: TDH.Analytics@tn.gov ----

# Project: Queries data for Job Dashboard ----
# Date Created: 2024-12-13

# Purpose of this file: ----
# Pulls, formats, transforms, and outputs data for the HR Career Dashboard



# Step 1: Set Up R Environment  ----
# Load necessary packages
library(httr)
library(xml2)
library(tidyverse)
library(rvest)
library(openxlsx)
library(rlang)


# Step 2: Connect to the Data ----

#In this project we actually connect to the data using an api, however, that api cannot be shared.
#The code below shows how we connect to the data, the step below walks you through how to load a text file example of the data
#skip to step 3

# Define the API endpoint (the url)
#api_url <- "insert the api url"

# Send GET request to fetch the XML data
#response <- GET(api_url)

# Check if the request was successful, success results in a status code 200
#if (status_code(response) == 200) {
#  save <- "yes" } else {
#    save <- "no"
#  }

# Parse the content as XML
#xml_content <- content(response, as = "text")
#xml_parsed <- read_xml(xml_content)

# Extract relevant data using XML structure (adjust XPath as needed)
# Example assumes each data item is in a <record> tag
#records <- xml_parsed %>% xml_find_all("//job")

# Extract specific fields from each record and convert to a dataframe
#df <- records %>%
#  map_df(~ {
#    data.frame(
#      sponsored = xml_find_first(.x, "./sponsored") %>% xml_text(trim = TRUE),
#      job_title = xml_find_first(.x, "./title") %>% xml_text(trim = TRUE),
#      date_posted = xml_find_first(.x, "./date") %>% xml_text(trim = TRUE),
#      reference_number = xml_find_first(.x, "./referencenumber") %>% xml_text(trim = TRUE),
#      group_id = xml_find_first(.x, "./group_id") %>% xml_text(trim = TRUE),
#      url_to_apply = xml_find_first(.x, "./url") %>% xml_text(trim = TRUE),
#     company = xml_find_first(.x, "./company") %>% xml_text(trim = TRUE),
#      location_city = xml_find_first(.x, "./city") %>% xml_text(trim = TRUE),
#      location_state = xml_find_first(.x, "./state") %>% xml_text(trim = TRUE),
#      location_country = xml_find_first(.x, "./country") %>% xml_text(trim = TRUE),
#      location_zip = xml_find_first(.x, "./postalcode") %>% xml_text(trim = TRUE),
#      description = xml_find_first(.x, "./description") %>% xml_text(trim = TRUE),
#      street_address = xml_find_first(.x, "./streetaddress") %>% xml_text(trim = TRUE),
#      salary = xml_find_first(.x, "./salary") %>% xml_text(trim = TRUE),
#      job_type = xml_find_first(.x, "./jobtype") %>% xml_text(trim = TRUE),
#      category = xml_find_first(.x, "./category") %>% xml_text(trim = TRUE),
#      experience = xml_find_first(.x, "./experience") %>% xml_text(trim = TRUE),
#      remotework = xml_find_first(.x, "./remotework") %>% xml_text(trim = TRUE),
#      county_number = xml_find_first(.x, ".//field[@name='rec_location']") %>% xml_text(trim = TRUE), #extracted from nested custom fields
#      job_family = xml_find_first(.x, ".//field[@name='jobfamily']") %>% xml_text(trim = TRUE), #extracted from nested custom fields
#      service_type = xml_find_first(.x, ".//field[@name='employmenttype']") %>% xml_text(trim = TRUE), #extracted from nested custom fields
#      stringsAsFactors = FALSE
#    )
#  })

# Step 3: Load the text file from github  ----
#download using URL, which sometimes can fail if you have a state computer/ZScaler
library(RCurl)
df <- getURL('https://raw.githubusercontent.com/TDH-SSI-Hub/TDH_Careers_Dashboard/refs/heads/main/HR_data.csv')
df2 <- read.csv(text = df)

#if you are having trouble connecting directly, download csv file and do the following
df <- read_csv("C:/file/path/to/your/downloads/HR_data.csv")

#pull todays date
today <- Sys.Date()


# Step 4: Clean the data ----

#value adds based on the current fields

df <- df %>%
  mutate(job_title_clean = trimws(gsub("\\*","",sub("-.*","",job_title))), #cleans the job title name to make it more normal
         job_title_clean_star = trimws(gsub("-\\s*\\d+", "", job_title)), #leave in flex indicator, remove digits after dash
         job_title_clean_star = trimws(sub("-"," ",job_title_clean_star)), #remove any remaining hyphens
         job_title_proper = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(job_title_clean_star), perl=TRUE), #proper case for readability in dash
         flex_role = case_when(str_detect(job_title_clean_star, "\\*") ~ "yes", TRUE ~ "no"), #identify flex indicator and create new column
         days_from_posted = abs(as.numeric(round(difftime(date_posted, today, units= "days"),digits = 0))), #calculates number of days since being posted
         new_job_posting = if_else(days_from_posted <= 7, "New", "Old"), #lets us know if the posting is recent to the last week
         job_type = ifelse(job_type=="", "fulltime", job_type),
         row_num = row_number(),
         county_number = str_remove(county_number, "SHARE-")) 

#Assign county names to county numbers 
df <- df %>%
  mutate(county_name = case_when(
    county_number == "001" ~ "Anderson",
    county_number == "002" ~ "Bedford",
    county_number == "003" ~ "Benton",
    county_number == "004" ~ "Bledsoe",
    county_number == "005" ~ "Blount",
    county_number == "006" ~ "Bradley",
    county_number == "007" ~ "Campbell",
    county_number == "008" ~ "Cannon",
    county_number == "009" ~ "Carroll",
    county_number == "010" ~ "Carter",
    county_number == "011" ~ "Cheatham",
    county_number == "012" ~ "Chester",
    county_number == "013" ~ "Claiborne",
    county_number == "014" ~ "Clay",
    county_number == "015" ~ "Cocke",
    county_number == "016" ~ "Coffee",
    county_number == "017" ~ "Crockett",
    county_number == "018" ~ "Cumberland",
    county_number == "019" ~ "Davidson",
    county_number == "020" ~ "Decatur",
    county_number == "021" ~ "DeKalb",
    county_number == "022" ~ "Dickson",
    county_number == "023" ~ "Dyer",
    county_number == "024" ~ "Fayette",
    county_number == "025" ~ "Fentress",
    county_number == "026" ~ "Franklin",
    county_number == "027" ~ "Gibson",
    county_number == "028" ~ "Giles",
    county_number == "029" ~ "Grainger",
    county_number == "030" ~ "Greene",
    county_number == "031" ~ "Grundy",
    county_number == "032" ~ "Hamblen",
    county_number == "033" ~ "Hamilton",
    county_number == "034" ~ "Hancock",
    county_number == "035" ~ "Hardeman",
    county_number == "036" ~ "Hardin",
    county_number == "037" ~ "Hawkins",
    county_number == "038" ~ "Haywood",
    county_number == "039" ~ "Henderson",
    county_number == "040" ~ "Henry",
    county_number == "041" ~ "Hickman",
    county_number == "042" ~ "Houston",
    county_number == "043" ~ "Humphreys",
    county_number == "044" ~ "Jackson",
    county_number == "045" ~ "Jefferson",
    county_number == "046" ~ "Johnson",
    county_number == "047" ~ "Knox",
    county_number == "048" ~ "Lake",
    county_number == "049" ~ "Lauderdale",
    county_number == "050" ~ "Lawrence",
    county_number == "051" ~ "Lewis",
    county_number == "052" ~ "Lincoln",
    county_number == "053" ~ "Loudon",
    county_number == "054" ~ "McMinn",
    county_number == "055" ~ "McNairy",
    county_number == "056" ~ "Macon",
    county_number == "057" ~ "Madison",
    county_number == "058" ~ "Marion",
    county_number == "059" ~ "Marshall",
    county_number == "060" ~ "Maury",
    county_number == "061" ~ "Meigs",
    county_number == "062" ~ "Monroe",
    county_number == "063" ~ "Montgomery",
    county_number == "064" ~ "Moore",
    county_number == "065" ~ "Morgan",
    county_number == "066" ~ "Obion",
    county_number == "067" ~ "Overton",
    county_number == "068" ~ "Perry",
    county_number == "069" ~ "Pickett",
    county_number == "070" ~ "Polk",
    county_number == "071" ~ "Putnam",
    county_number == "072" ~ "Rhea",
    county_number == "073" ~ "Roane",
    county_number == "074" ~ "Robertson",
    county_number == "075" ~ "Rutherford",
    county_number == "076" ~ "Scott",
    county_number == "077" ~ "Sequatchie",
    county_number == "078" ~ "Sevier",
    county_number == "079" ~ "Shelby",
    county_number == "080" ~ "Smith",
    county_number == "081" ~ "Stewart",
    county_number == "082" ~ "Sullivan",
    county_number == "083" ~ "Sumner",
    county_number == "084" ~ "Tipton",
    county_number == "085" ~ "Trousdale",
    county_number == "086" ~ "Unicoi",
    county_number == "087" ~ "Union",
    county_number == "088" ~ "Van Buren",
    county_number == "089" ~ "Warren",
    county_number == "090" ~ "Washington",
    county_number == "091" ~ "Wayne",
    county_number == "092" ~ "Weakley",
    county_number == "093" ~ "White",
    county_number == "094" ~ "Williamson",
    county_number == "095" ~ "Wilson"
  ))

 #assigns a state region (east, middle, west) to the job location based on county
df <- df %>%
  mutate(tn_grand_division = case_when(
    county_number == "001" ~ "East",
    county_number == "002" ~ "Middle",
    county_number == "003" ~ "West",
    county_number == "004" ~ "East",
    county_number == "005" ~ "East",
    county_number == "006" ~ "East",
    county_number == "007" ~ "East",
    county_number == "008" ~ "Middle",
    county_number == "009" ~ "West",
    county_number == "010" ~ "East",
    county_number == "011" ~ "Middle",
    county_number == "012" ~ "West",
    county_number == "013" ~ "East",
    county_number == "014" ~ "Middle",
    county_number == "015" ~ "East",
    county_number == "016" ~ "Middle",
    county_number == "017" ~ "West",
    county_number == "018" ~ "East",
    county_number == "019" ~ "Middle",
    county_number == "020" ~ "West",
    county_number == "021" ~ "Middle",
    county_number == "022" ~ "Middle",
    county_number == "023" ~ "West",
    county_number == "024" ~ "West",
    county_number == "025" ~ "Middle",
    county_number == "026" ~ "Middle",
    county_number == "027" ~ "West",
    county_number == "028" ~ "Middle",
    county_number == "029" ~ "East",
    county_number == "030" ~ "East",
    county_number == "031" ~ "Middle",
    county_number == "032" ~ "East",
    county_number == "033" ~ "East",
    county_number == "034" ~ "East",
    county_number == "035" ~ "West",
    county_number == "036" ~ "West",
    county_number == "037" ~ "East",
    county_number == "038" ~ "West",
    county_number == "039" ~ "West",
    county_number == "040" ~ "West",
    county_number == "041" ~ "Middle",
    county_number == "042" ~ "Middle",
    county_number == "043" ~ "Middle",
    county_number == "044" ~ "Middle",
    county_number == "045" ~ "East",
    county_number == "046" ~ "East",
    county_number == "047" ~ "East",
    county_number == "048" ~ "West",
    county_number == "049" ~ "West",
    county_number == "050" ~ "Middle",
    county_number == "051" ~ "Middle",
    county_number == "052" ~ "Middle",
    county_number == "053" ~ "East",
    county_number == "054" ~ "East",
    county_number == "055" ~ "West",
    county_number == "056" ~ "Middle",
    county_number == "057" ~ "West",
    county_number == "058" ~ "East",
    county_number == "059" ~ "Middle",
    county_number == "060" ~ "Middle",
    county_number == "061" ~ "East",
    county_number == "062" ~ "East",
    county_number == "063" ~ "Middle",
    county_number == "064" ~ "Middle",
    county_number == "065" ~ "East",
    county_number == "066" ~ "West",
    county_number == "067" ~ "Middle",
    county_number == "068" ~ "Middle",
    county_number == "069" ~ "Middle",
    county_number == "070" ~ "East",
    county_number == "071" ~ "Middle",
    county_number == "072" ~ "East",
    county_number == "073" ~ "East",
    county_number == "074" ~ "Middle",
    county_number == "075" ~ "Middle",
    county_number == "076" ~ "East",
    county_number == "077" ~ "Middle",
    county_number == "078" ~ "East",
    county_number == "079" ~ "West",
    county_number == "080" ~ "Middle",
    county_number == "081" ~ "Middle",
    county_number == "082" ~ "East",
    county_number == "083" ~ "Middle",
    county_number == "084" ~ "West",
    county_number == "085" ~ "Middle",
    county_number == "086" ~ "East",
    county_number == "087" ~ "East",
    county_number == "088" ~ "Middle",
    county_number == "089" ~ "Middle",
    county_number == "090" ~ "East",
    county_number == "091" ~ "Middle",
    county_number == "092" ~ "West",
    county_number == "093" ~ "Middle",
    county_number == "094" ~ "Middle",
    county_number == "095" ~ "Middle"
  ))


# Depending on the type of job (executive or preferred) the job description is stored differently
# Below are cleaning steps for the description column based on each job type (service type field)


df_DESCRIPTION <- df %>% 
  select(service_type, description) %>%
  mutate(service_type = case_when(grepl("Executive", service_type) ~ "Executive",
                                  TRUE ~ "Preferred")) %>% # splits into exec vs preferred which will be changed later by another field
  mutate(row_num = row_number())



#create the fields for the exec positions
df_desc_exec <- df_DESCRIPTION %>%
  filter(service_type == "Executive") %>%
  mutate(
    salary_monthly = str_extract(description, "Salary:.*?Monthly"),
    salary_monthly = ifelse(is.na(salary_monthly), str_extract(description, "(?<=Salary Range: ).*?(?= monthly)"), salary_monthly),
    salary_monthly = ifelse(is.na(salary_monthly), str_extract(description, "(?<=Salary Range: ).*?(?= per month)"), salary_monthly),
    salary_monthly = ifelse(is.na(salary_monthly), str_extract(description, "(?<=Salary: ).*?(?= per Month)"), salary_monthly),
    salary_monthly = ifelse(is.na(salary_monthly), str_extract(description, "(?<=starting monthly salary of ).*?(?=</)"), salary_monthly),
    salary_monthly = ifelse(is.na(salary_monthly), str_extract(description, "(?<=Salary: ).*?(?=<)"), salary_monthly),
    closing_date = as.Date(str_extract(description, "(?<=Closing Date: ).*?(?=<)"), format= "%m/%d/%Y"),
    days_to_close = abs(as.numeric(round(difftime(today,closing_date, units = "days"),digits = 0))),
    closing_soon = if_else(days_to_close <= 5, "Closing Soon", "Not soon"),
    job_overview = str_extract(description, "(?<=Job Overview:</strong><o:p></o:p></p><p class=\"MsoNormal\">)(.*?)(?=<o:p></o:p></p><p class=\"MsoNormal\">)"),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary</strong>).*?(?=<br>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Job Overview:</strong></span><br><span style=\"font-family:Arial, Helvetica, sans-serif;\">).*?(?=</span>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Job Overview:</strong></span></span><o:p></o:p></p><p class=\"MsoNormal\" style=\"line-height:normal;mso-margin-bottom-alt:auto;mso-margin-top-alt:auto;text-align:justify;\"><span style=\"color:black;font-family:Arial, Helvetica, sans-serif;\"><span style=\"font-family:&quot;Arial&quot;,sans-serif;font-size:10.5pt;mso-fareast-font-family:&quot;Times New Roman&quot;;mso-font-kerning:0pt;mso-ligatures:none;\">).*?(?=</span>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Job Overview:</strong></span></span><o:p></o:p></p><p class=\"MsoNormal\" style=\"line-height:normal;mso-margin-bottom-alt:auto;mso-margin-top-alt:auto;text-align:justify;\"><span style=\"color:black;\"><span style=\"font-family:&quot;Arial&quot;,sans-serif;font-size:10.5pt;mso-fareast-font-family:&quot;Times New Roman&quot;;mso-font-kerning:0pt;mso-ligatures:none;\">).*?(?=</span>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary:</strong>).*?(?=</p>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>How you make a difference in this role:</strong></span></span><o:p></o:p></p><p class=\"MsoNormal\" style=\"line-height:normal;mso-margin-bottom-alt:auto;mso-margin-top-alt:auto;text-align:justify;\"><span style=\"color:black;\"><span style=\"font-family:&quot;Arial&quot;,sans-serif;font-size:10.5pt;mso-fareast-font-family:&quot;Times New Roman&quot;;mso-font-kerning:0pt;mso-ligatures:none;\">).*?(?=</span>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Job Overview:</strong><o:p></o:p></p><p class=\"MsoNormal\" style=\"text-align:justify;\">).*?(?=</p>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>How you make a difference in this role:</strong></span></span><o:p></o:p></p><p class=\"MsoNormal\" style=\"line-height:normal;mso-margin-bottom-alt:auto;mso-margin-top-alt:auto;text-align:justify;\"><span style=\"color:black;\"><span style=\"font-family:&quot;Arial&quot;,sans-serif;font-size:10.5pt;mso-fareast-font-family:&quot;Times New Roman&quot;;mso-font-kerning:0pt;mso-ligatures:none;\">).*?(?=</span>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Job Overview:</strong></span><o:p></o:p></p><p class=\"MsoNormal\" style=\"text-align:justify;\"><span style=\"font-family:Arial, Helvetica, sans-serif;\">).*?(?=</span>)"), job_overview)
    
 ) %>%
  select(-description) %>%
  #, -service_type) 
  
  mutate(
    salary_annual = NA_character_,
    num_positions = 1,
    distinguishing_features = NA_character_
  )

df_desc_preferred <- df_DESCRIPTION %>%
  filter(service_type=="Preferred") %>%
  mutate(
    description_parsed = map(description, ~ {
      # Convert HTML content to a rvest-readable object
      html <- tryCatch(read_html(.x), error = function(e) NULL)
      
      if (!is.null(html)) {
        tables <- html %>% html_table()
        if (length(tables) > 0) {
          tables[[1]] %>% pivot_wider(names_from = 1, values_from = 2)
        } else {
          NULL
        }
      } else {
        NULL
      }
    }),
    # Extract specific columns from the parsed description
    closing_dt_build = str_extract(map_chr(description_parsed, ~ .[["Closing Date/Time"]] %||% NA_character_),"^[^ ]+"),
    closingdt_string = str_length(closing_dt_build),
    closing_dt_correction = if_else(closingdt_string <= 10, paste0(str_sub(closing_dt_build,1,6), "20", str_sub(closing_dt_build, 7)),""),
    closing_dt_final = as.Date(if_else(closingdt_string == 10, closing_dt_build, closing_dt_correction), format="%m/%d/%Y"),
    closing_dt = map_chr(description_parsed, ~ .[["Closing Date/Time"]] %||% NA_character_),
    salary_monthly = map_chr(description_parsed, ~ .[["Salary (Monthly)"]] %||% NA_character_),
    salary_annual = map_chr(description_parsed, ~ .[["Salary (Annually)"]] %||% NA_character_)) %>%
  mutate(
    num_positions = str_remove_all(str_extract(description, "\\((\\d+)\\)"), "[()]"),
    days_to_close = abs(as.numeric(round(difftime(today,closing_dt_final, units = "days"),digits = 0))),
    closing_soon = if_else(days_to_close <= 5, "Closing Soon", "Not soon"),
    job_overview = str_extract(description, "(?<=<strong>Summary: </strong>).*?(?=<br>)"),
   # job_overview_2 = str_extract(description, "(?<=>Summary:).*?(?=.</)"), experiment to remove all html tags
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary</strong> <span>).*?(?=</span>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary</strong>).*?(?=<br>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary:</strong>).*?(?=</p>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary:  </strong>).*?(?=</p>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary: </strong>).*?(?=</div>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary:&nbsp;</strong>).*?(?=</div>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary:&nbsp;</strong>).*?(?=</p>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong> Summary:</strong>).*?(?=</div>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong> Summary: </strong>).*?(?=</div>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<b>Summary:</b>).*?(?=</p>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=<strong>Summary:&nbsp; </strong>).*?(?=<br>)"), job_overview),
    job_overview = if_else(is.na(job_overview), str_extract(description, "(?<=</p><p><b>Overview</b> </p> <p>).*?(?=</p>)"), job_overview), 
    job_overview = trimws(str_remove_all(job_overview, ":")),
    distinguishing_features = str_extract(description, "(?<=<strong>Distinguishing Features:</strong>).*?(?=<br>)"),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong>Distinguishing Features: </strong>).*?(?=<br>)"), distinguishing_features),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong>Distinguishing Features:</strong>).*?(?=</div>)"), distinguishing_features),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong>Distinguishing Features: </strong>).*?(?=</div>)"), distinguishing_features),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong> Distinguishing Features: </strong>).*?(?=</div>)"), distinguishing_features),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong> Distinguishing Features:</strong>).*?(?=</div>)"), distinguishing_features),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong>Distinguishing Features:&nbsp;</strong>).*?(?=</div>)"), distinguishing_features),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong>Distinguishing Features:&nbsp;</strong>).*?(?=</p>)"), distinguishing_features),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<b>Distinguishing Features:</b>).*?(?=</div>)"), distinguishing_features),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong>Distinguishing Features:&nbsp; </strong>).*?(?=</div>)"), distinguishing_features),
    distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong>Distinguishing Features:  </strong>).*?(?=<br>)"), distinguishing_features),
   distinguishing_features = if_else(is.na(distinguishing_features),str_extract(description, "(?<=<strong>Distinguishing Features:</strong> <span>).*?(?=</span>)"), distinguishing_features),
   distinguishing_features = trimws(str_remove_all(distinguishing_features, ":")),
    distinguishing_features = trimws(str_remove_all(distinguishing_features, "</div>")),
    distinguishing_features = trimws(str_remove_all(distinguishing_features, "&nbsp;"))
  ) %>%
  select(-description, -description_parsed, -closing_dt_build, -closingdt_string, -closing_dt_correction, -closing_dt) %>%
  # -service_type,
  rename(closing_date = closing_dt_final)

#puts the different job types together again into one dataframe for the details pulled from the description
record_details <- rbind(df_desc_exec, df_desc_preferred)

#removes service type as one of the duplicate columns between record_details and the original DF
df <- df %>%
  select(-service_type)

#joins the df and record_details so that the description data is with the original data from DF
records_combined <- df %>%
  left_join(record_details, by="row_num") %>%
  select(-row_num)

#quick fix: remove hanging html cues and everything after them (probably want to refine this later to incl. any responsibilities etc)
nm1 <- names((records_combined)[,c(37, 38)])
records_combined <- records_combined %>%
  mutate(across(all_of(nm1), ~ str_remove(.x, pattern = "\\<.*")))
  ## above line replaces following deprecated line: mutate(across(all_of(nm1), str_remove, pattern = "\\<.*"))


# Final step: Create an output if the data are good

if (save == "yes") {
  #set working directory
  setwd("Y:/file path/for output file/")
  write.xlsx(records_combined, file = "job_dashboard.xlsx", overwrite = TRUE)
} else {
  
  #print a message to let yourself know something failed in the process 
  print("The data pull for the HR dashboard failed and needs to be checked.")
  

}
