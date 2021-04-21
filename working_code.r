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
  ActiveFunds %>% summarise(sum(Estimated.Share.Class.Net.Flow..Yearly...Year2019..USD, na.rm = T))
  
  tk <- ActiveFunds %>% 
    mutate(
      Type =
        case_when(
          grepl("Equity", US.Category.Group) ~ "Equity",
          grepl("Bond", US.Category.Group) ~ "Bond",
          TRUE ~ "Other"
        )) %>% 
    filter(
      Firm.Name.Mod %in% Highcost_Active_Firms,
      Type == "Equity"
    ) %>% 
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
  
  Rowe <- PassiveFunds %>% 
    filter(
      grepl("Rowe", Firm.Name.Mod)
        ) %>%
    group_by(Name) %>% 
    mutate(PCT = (Annual.Report.Adjusted.Expense.Ratio..Year2020 - Annual.Report.Adjusted.Expense.Ratio..Year2019)) %>% 
    select(Name, PCT, everything()) %>% 
    arrange(desc(Net.Assets...share.class..Monthly...2020.12..USD)) %>% 
    group_by(Firm.Name.Mod) %>% 
    summarise(FLW_2020 = sum(Estimated.Share.Class.Net.Flow..Yearly...Year2020..USD, na.rm = T))
  
  
  Hmm <- PassiveFunds %>%  
    filter(Annual.Report.Adjusted.Expense.Ratio..Year2020 == 0)
  
  Hmm <- ActiveFunds %>%  
    filter(Annual.Report.Adjusted.Expense.Ratio..Year2020 == 0)

  Passive_LowFees <- Passive_Fees %>%
    arrange(Fee_Wted_2020) %>%
    top_n(-10, Fee_Wted_2020) %>% 
    mutate(
      Pct_Change = (Fee_Wted_2020-Fee_Wted_2019)/Fee_Wted_2019 *100
    )
  
  Lowcost_Passive <- Passive_LowFees$Firm.Name.Mod
  
  Sum <- PassiveFunds %>% 
    # filter(
    #   Firm.Name.Mod %in% Lowcost_Passive
    #   ) %>% 
    group_by(Firm.Name.Mod) %>% 
    summarise(FLW_2020 = sum(Estimated.Share.Class.Net.Flow..Yearly...Year2020..USD, na.rm = T))
  
  # Adjusted_Data <- Full %>%
  #   rename_all(make.names) %>%
  #   filter(Fund.of..Funds != "Yes",
  #          US.Category.Group != "Money Market"
  #   ) %>%
  #   mutate(
  #     Firm.Name.Mod = case_when(
  #       Firm.Name == "SPDR State Street Global Advisors" ~ "State Street Global Advisors",
  #       Firm.Name == "Calvert Research and Management" ~ "Eaton Vance",
  #       Firm.Name == "Nuveen" ~ "TIAA Investments",
  #       Firm.Name %in% c("Harding Loevner", "Third Avenue", "Tweedy, Browner") ~ "AMG Funds",
  #       Firm.Name %in% "Amundi Pioneer" ~ "Pioneer Investments",
  #       grepl("AllianzGI", Name) ~ "AllianzGI_",
  #       TRUE ~ as.character(Firm.Name)
  #     ),
  #     Annual.Report.Adjusted.Expense.Ratio..Year2020 =
  #       case_when(
  #         Ticker == "FLCOX" ~ .035,
  #         TRUE ~ Annual.Report.Adjusted.Expense.Ratio..Year2020
  #       )
  #   ) %>%
  #   mutate(
  #     Net_Asst_2018_AVG = rowMeans(.[, 47:58], na.rm = TRUE),
  #     Net_Asst_2019_AVG = rowMeans(.[, 59:70], na.rm = TRUE),
  #     Net_Asst_2020_AVG = rowMeans(.[, 71:82], na.rm = TRUE),
  #     Asst_By_Fee_2018 = Net_Asst_2018_AVG * Annual.Report.Adjusted.Expense.Ratio..Year2018,
  #     Asst_By_Fee_2019 = Net_Asst_2019_AVG * Annual.Report.Adjusted.Expense.Ratio..Year2019,
  #     Asst_By_Fee_2020 = Net_Asst_2020_AVG * Annual.Report.Adjusted.Expense.Ratio..Year2020,
  #     Pct_Change_2018_2019 = (
  #       Annual.Report.Adjusted.Expense.Ratio..Year2019 - Annual.Report.Adjusted.Expense.Ratio..Year2018
  #     ) / Annual.Report.Adjusted.Expense.Ratio..Year2018 * 100,
  #     Pct_Change_2019_2020 = (
  #       Annual.Report.Adjusted.Expense.Ratio..Year2020 - Annual.Report.Adjusted.Expense.Ratio..Year2019
  #     ) / Annual.Report.Adjusted.Expense.Ratio..Year2019 * 100
  #   )