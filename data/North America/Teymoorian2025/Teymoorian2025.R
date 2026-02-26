

#this script is for organizing the over 700 datapoints from
# the (Teymoorian et al., 2025) paper called
#"A province-wide mapping of per- and polyfluoroalkyl substances
#(PFAS) in surface waters of the St. Lawrence 
# River watershed, Québec, Canada"

#data is seperated by year. Going to bring it all together to a single file
T2018 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/North%20America/Teymoorian2025/Teymoorian_et_al_2025_Quebec_2018data.csv")
T2019=read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/North%20America/Teymoorian2025/Teymoorian_et_al_2025_Quebec_2019data.csv")
T2020=read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/North%20America/Teymoorian2025/Teymoorian_et_al_2025_Quebec_2020data.csv")
T2021 =read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/North%20America/Teymoorian2025/Teymoorian_et_al_2025_Quebec_2021data.csv")

#combine all years to a single dataframe
All_T <- bind_rows(T2018, T2019, T2020, T2021)

#bring in the coords
Tcoords = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/North%20America/Teymoorian2025/Teymoorian_et_al_2025_Quebec_site_coords.csv")

Teymoorian <- left_join(All_T, Tcoords, by = "Site name")

#some didn't come in correctly. NEed to fix original csv files