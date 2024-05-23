

library(tidyverse)

data_reporting <- tibble::tribble(
                          ~periodname, ~organisationunitname, ~`rate`,
                    "Oct to Dec 2023",              "Acholi",                                                         82L,
                    "Oct to Dec 2023",              "Ankole",                                                         77L,
                    "Oct to Dec 2023",              "Bugisu",                                                         99L,
                    "Oct to Dec 2023",              "Bukedi",                                                         99L,
                    "Oct to Dec 2023",             "Bunyoro",                                                         62L,
                    "Oct to Dec 2023",              "Busoga",                                                         86L,
                    "Oct to Dec 2023",             "Kampala",                                                         60L,
                    "Oct to Dec 2023",            "Karamoja",                                                         97L,
                    "Oct to Dec 2023",              "Kigezi",                                                         65L,
                    "Oct to Dec 2023",               "Lango",                                                         96L,
                    "Oct to Dec 2023",       "North Central",                                                         53L,
                    "Oct to Dec 2023",       "South Central",                                                         75L,
                    "Oct to Dec 2023",                "Teso",                                                         89L,
                    "Oct to Dec 2023",               "Tooro",                                                         75L,
                    "Oct to Dec 2023",           "West Nile",                                                         93L,
                    "Jan to Mar 2024",              "Acholi",                                                         82L,
                    "Jan to Mar 2024",              "Ankole",                                                         70L,
                    "Jan to Mar 2024",              "Bugisu",                                                         99L,
                    "Jan to Mar 2024",              "Bukedi",                                                         98L,
                    "Jan to Mar 2024",             "Bunyoro",                                                         62L,
                    "Jan to Mar 2024",              "Busoga",                                                         92L,
                    "Jan to Mar 2024",             "Kampala",                                                         33L,
                    "Jan to Mar 2024",            "Karamoja",                                                         84L,
                    "Jan to Mar 2024",              "Kigezi",                                                         74L,
                    "Jan to Mar 2024",               "Lango",                                                         84L,
                    "Jan to Mar 2024",       "North Central",                                                         52L,
                    "Jan to Mar 2024",       "South Central",                                                         43L,
                    "Jan to Mar 2024",                "Teso",                                                         56L,
                    "Jan to Mar 2024",               "Tooro",                                                         69L,
                    "Jan to Mar 2024",           "West Nile",                                                         74L
                    )

data_reporting_c <- data_reporting %>%
  pivot_wider(names_from = periodname , values_from = rate ) %>%
  mutate(dif =  (`Jan to Mar 2024` - `Oct to Dec 2023`)/`Jan to Mar 2024`*100)


write_csv(data_reporting_c,"regional_diff.csv")  
