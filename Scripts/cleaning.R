library(readxl)
library(tidyverse)

fields <- c(
  "id",
  "role",
  "career_switch",
  "salary",
  "industry",
  "language",
  "rating_salary",
  "rating_worklife_balance",
  "rating_coworkers",
  "rating_management",
  "rating_upward_mobility",
  "rating_learning",
  "rating_breakin",
  "important_factors",
  "sex",
  "age",
  "country",
  "education",
  "ethnicity"
)

clean_role <- function(df) {
  cleaned <- df %>%
    mutate(
      role = str_replace(role, "Other \\(Please Specify\\):", ""),
      role = str_replace(role, "^[:space:]", ""),
      role = str_to_lower(role),
      role = str_replace(role, "business (intelligence) ", "bi "),
      role = str_replace(role, "analys$|analystt|analystss|analytics", "analyst"),
      role = str_replace(role, "(.)*(?<!bi )analyst(.)*", "data analyst"),
      role = str_replace(role, "(.)*software engineer(.)*", "software engineer"),
      role = str_replace(role, "(.)*(?<!software )engineer$", "data engineer"),
      role = str_replace(role, "(.)*scientist(.)*", "data scientist"),
      role = str_replace(role, "(jr\\.|sr\\.|junior|senior)", ""),
      role = str_replace(role, "develop$", "developer"),
      role = str_replace(role, "(.)*bi (?!developer)(.)*", "bi analyst"),
      role = str_replace(role, "(.)*(?<!software )developer$", "bi developer"),
      role = str_replace(
        role,
        "data(base)* (steward|integrity|coordinator|manager)",
        "database admin"
      ),
      role = fct_lump_min(role, 4),
      role = str_to_title(role),
      role = str_replace(role, "^Bi", "BI")
    )
  return(cleaned)
}
clean_industry <- function(df) {
  cleaned <- df %>%
    mutate(
      industry = str_replace(industry, "^(.)*:[:space:]*", ""),
      industry = str_to_lower(industry),
      industry = str_replace(industry, "(.)*food(.)*", "food"),
      industry = str_replace(industry, "(.)*manuf(.)*", "manufacturing"),
      industry = str_replace(industry, "(.)*automo(.)*", "automotive"),
      industry = str_replace(industry, "(.)*(energy|oil|gas)(.)*", "energy"),
      industry = str_replace(industry, "(.)*consul(.)*", "consulting"),
      industry = str_replace(industry, "(.)*advertising(.)*", "marketing"),
      industry = str_replace(industry, "(.)*(digital|direct) mar(.)*", "marketing"),
      industry = str_replace(
        industry,
        "(.)*(distribution|logist|transportation|supply chain)(.)*",
        "logistics"
      ),
      industry = str_replace(industry, "(.)*(fmcg|reta|retails)(.)*", "retail"),
      industry = str_replace(
        industry,
        "(.)*(none|working|study|student|bootcamp|job)(.)*",
        "NA"
      ),
      industry = na_if(industry, "NA"),
      industry = str_replace(industry, "(.)*profit(.)*", "non-profit"),
      industry = str_replace(industry, "(.)*(gover|state)(.)*", "government"),
      industry = str_replace(industry, "(.)*(air|avia|arrosp)(.)*", "aerospace"),
      industry = fct_lump_min(industry, 9),
      industry = str_to_title(industry),
      #industry = str_replace(industry, "Na$", "NA"),
    )
  return(cleaned)
}
clean_language <- function(df) {
  cleaned <- df %>%
    mutate(
      language = str_replace(language, "^(.)*:[:space:]*", ""),
      language = str_to_lower(language),
      language = str_replace(language, ".*(sql).*", "sql"),
      language = str_replace(language, "(.)*sas(.)*", "sas"),
      language = str_replace(language, ".*(excel).*", "excel"),
      language = str_replace(language, "(.)*power bi(.)*", "dax"),
      language = str_replace(
        language,
        ".*(na|none|any|unknown|dont|learn|yet|role).*",
        "none"
      ),
      language = fct_lump_min(language , 5),
      language = str_to_title(language),
      language = str_replace(language, "Sql", "SQL"),
      language = str_replace(language, "Dax", "DAX")
    )
  return(cleaned)
}
clean_important <- function(df) {
  cleaned <- df %>%
    mutate(
      important_factors = str_replace(important_factors, "^(.)*:[:space:]*", ""),
      important_factors = str_to_lower(important_factors),
      important_factors = str_replace(
        important_factors,
        ".*(learn|growth|mentor|develop).*",
        "learning new skills"
      ),
      important_factors = str_replace(important_factors, ".*(move).*", "location"),
      important_factors = str_replace(important_factors, ".*(salary).*", "better salary"),
      important_factors = str_replace(important_factors, ".*(remote).*", "remote"),
      important_factors = str_replace(
        important_factors,
        ".*(career|responsib|advanc).*",
        "upward mobility"
      ),
      important_factors = fct_lump_min(important_factors, 10),
      important_factors = str_to_title(important_factors)
    )
  return(cleaned)
}
clean_country <- function(df) {
  cleaned <- df %>%
    mutate(
      country = str_replace(country, "^(.)*:[:space:]*", ""),
      country = str_to_lower(country),
      country = str_replace(country, ".*nigeria.*", "nigeria"),
      country = fct_lump_min(country, 10),
      country = str_to_title(country)
    )
  return(cleaned)
}
clean_ethnicity <- function(df) {
  cleaned <- df %>%
    mutate(
      ethnicity = str_replace(ethnicity, "^(.)*:[:space:]*", ""),
      ethnicity = str_to_lower(ethnicity),
      ethnicity = str_replace(ethnicity, ".*african(?! american).*", "african"),
      ethnicity = str_replace(ethnicity, ".*(niger|morocc).*", "african"),
      ethnicity = fct_lump_min(ethnicity, 10),
      ethnicity = str_to_title(ethnicity)
    )
  return(cleaned)
}
clean_salary <- function(df){
  cleaned <- df %>%
  mutate(
    salary = str_replace(salary, "\\+","\\-225"),
    salary = str_replace_all(salary,"k", ""),
    minsalary = as.integer(str_split_fixed(salary, "\\-", 2)[,1]),
    maxsalary = as.integer(str_split_fixed(salary, "\\-", 2)[,2]),
    avgsalary = 1000*(minsalary + maxsalary)/2
  )
  return(cleaned)
  }

df <- read_xlsx("Data/Survey Results.xlsx") %>%
  select(-c(2:10))
names(df) <- fields
cleaned <- df %>%
  clean_country() %>%
  clean_ethnicity() %>%
  clean_important() %>%
  clean_language() %>%
  clean_industry() %>%
  clean_role() %>%
  clean_salary() %>%
  mutate(
    education = na_if(education, "NA"),
    rating_learning = na_if(rating_learning, NA)
  )


names(processed)
ratings <- cleaned %>%
  select(c(1,7:12)) %>%
  pivot_longer(-1,names_to = "aspect", values_to = "satisfaction") %>%
  mutate(
    aspect = str_replace_all(aspect, "_", " ")
  )

responses <- cleaned %>%
  select(-c(4,7:12,20,21))


write_csv(ratings, "./Data/ratings.csv")
write_csv(responses, "./Data/responses.csv")

