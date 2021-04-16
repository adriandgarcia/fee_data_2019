Active_Cat_try <- ActiveFunds %>%
  mutate(Type =
           case_when(
             grepl("Equity", US.Category.Group) ~ "Equity",
             grepl("Bond", US.Category.Group) ~ "Bond",
             TRUE ~ "Other"
           )) %>% 
  group_by(Firm.Name.Mod, Type) %>% 
  summarise(
    Asst_Wted_Cat_2018 = sum(Asst_By_Fee_2018, na.rm = TRUE),
    Sum_Assets_Cat_2018 = sum(Net_Asst_2018_AVG, na.rm = TRUE),
    Fee_Wted_Cat_2018 = Asst_Wted_Cat_2018 / Sum_Assets_Cat_2018,
    Asst_Wted_Cat_2019 = sum(Asst_By_Fee_2019, na.rm = TRUE),
    Sum_Assets_Cat_2019 = sum(Net_Asst_2019_AVG, na.rm = TRUE),
    Fee_Wted_Cat_2019 = Asst_Wted_Cat_2019 / Sum_Assets_Cat_2019,
    Asst_Wted_Cat_2020 = sum(Asst_By_Fee_2020, na.rm = TRUE),
    Sum_Assets_Cat_2020 = sum(Net_Asst_2020_AVG, na.rm = TRUE),
    Fee_Wted_Cat_2020 = Asst_Wted_Cat_2020 / Sum_Assets_Cat_2020,
    Cat_Count = n()
  ) %>% 
  group_by(Firm.Name.Mod) %>% 
  mutate(
    ShareClass_Count = sum(Cat_Count, na.rm = TRUE),
    Sum_Assts_Wted_2019 = sum(Asst_Wted_Cat_2019, na.rm = TRUE),
    Sum_Assets_2019 = sum(Sum_Assets_Cat_2019, na.rm = TRUE),
    Fee_Wted_2019 = Sum_Assts_Wted_2019 / Sum_Assets_2019,
    pct_assets_2019 = (Sum_Assets_Cat_2019 / Sum_Assets_2019) * 100,
    Sum_Assts_Wted_2020 = sum(Asst_Wted_Cat_2020, na.rm = TRUE),
    Sum_Assets_2020 = sum(Sum_Assets_Cat_2020, na.rm = TRUE),
    Fee_Wted_2020 = Sum_Assts_Wted_2020 / Sum_Assets_2020,
    pct_assets_2020 = (Sum_Assets_Cat_2020 / Sum_Assets_2020) * 100
  )


Active_Cat_Sum_try <- Active_Cat_try %>%
  group_by(Type) %>%  
  summarise(
    SHARECLASSES = sum(Cat_Count),
    FEE = round((sum(Asst_Wted_Cat_2020)/sum(Sum_Assets_Cat_2020)),2),
    ASSETS = sum(Sum_Assets_Cat_2020)
  ) %>%  
  mutate(
    pct = round(ASSETS / sum(ASSETS)*100,2)
  )

test <- ActiveFunds %>% 
  group_by(Firm.Name.Mod, US.Category.Group) %>% 
  summarise(
    Sum_Assets_Cat_2020 = sum(Net.Assets...share.class..Monthly...2020.12..USD, na.rm = TRUE),
    Cat_Count = n()
  ) %>% 
  group_by(Firm.Name.Mod) %>% 
  mutate(
    Sum_Assets_2020 = sum(Sum_Assets_Cat_2020, na.rm = TRUE),
    pct_assets_2020 = (Sum_Assets_Cat_2020 / Sum_Assets_2020) * 100,
    Type =
      case_when(
        grepl("Equity", US.Category.Group) ~ "Equity",
        grepl("Bond", US.Category.Group) ~ "Bond",
        TRUE ~ "Other"
      )) %>%
  filter(Firm.Name.Mod %in% Highcost_Active_Firms) %>%
  group_by(Firm.Name.Mod, Type) %>% 
  summarise(assets =
                  sum(Sum_Assets_Cat_2020, na.rm =T),
            sum(pct_assets_2020, na.rm = TRUE))

test1 <- ActiveFunds %>% 
  group_by(Firm.Name.Mod, Morningstar.Category) %>% 
  summarise(
    Sum_Assets_Cat_2020 = sum(Net.Assets...share.class..Monthly...2020.12..USD, na.rm = TRUE),
    Cat_Count = n()
  ) %>% 
  group_by(Firm.Name.Mod) %>% 
  mutate(
    Sum_Assets_2020 = sum(Sum_Assets_Cat_2020, na.rm = TRUE),
    pct_assets_2020 = (Sum_Assets_Cat_2020 / Sum_Assets_2020) * 100
      ) %>%
  filter(Firm.Name.Mod %in% Highcost_Active_Firms)

ActiveFunds %>% 
  filter(
    Firm.Name.Mod %in% Active_Fees$Firm.Name.Mod
  ) %>% 
  summarise(sum(Estimated.Share.Class.Net.Flow..Yearly...Year2019..USD, na.rm = T)) %>% 
  view()


ActiveFunds1 <- ActiveFunds %>% 
  mutate(
    Firm.Name.Mod =
    case_when(
    grepl("AllianzGI", Name) ~ "Other",
    TRUE ~ Firm.Name.Mod
  ))
  
  Active_Fees1 <- ActiveFunds1 %>%
    group_by(Firm.Name.Mod) %>%
    summarise(
      Count = n(),
      Sum_Assts_Wted_2018 = sum(Asst_By_Fee_2018, na.rm = TRUE),
      Sum_Assets_2018 = sum(Net_Asst_2018_AVG, na.rm = TRUE),
      Fee_Wted_2018 = Sum_Assts_Wted_2018 / Sum_Assets_2018,
      Sum_Assts_Wted_2019 = sum(Asst_By_Fee_2019, na.rm = TRUE),
      Sum_Assets_2019 = sum(Net_Asst_2019_AVG, na.rm = TRUE),
      Fee_Wted_2019 = Sum_Assts_Wted_2019 / Sum_Assets_2019,
      Sum_Assts_Wted_2020 = sum(Asst_By_Fee_2020, na.rm = TRUE),
      Sum_Assets_2020 = sum(Net_Asst_2020_AVG, na.rm = TRUE),
      Fee_Wted_2020 = Sum_Assts_Wted_2020 / Sum_Assets_2020
    )
  
  ActiveFunds %>% summarise(sum(Estimated.Share.Class.Net.Flow..Yearly...Year2020..USD, na.rm = T))
  
  