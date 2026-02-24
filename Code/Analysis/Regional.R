# =====================================================
# Nobel Region Nomination Analysis Script
# =====================================================

# ===============================
# Load Libraries
# ===============================
library(tidyverse)
library(igraph)

# ===============================
# Load Data
# ===============================
load("~/Desktop/nomineeNominatorDetailedData.Rdata")

# ===============================
# 0. Map Countries to UN Subregions
# ===============================
unique_countries <- detailedData %>%
  select(Nominator_Country, Nominee_Country) %>%
  pivot_longer(everything(), values_to = "Country") %>%
  distinct(Country) %>%
  drop_na() %>%
  arrange(Country)

coded_countries <- unique_countries %>%
  mutate(UN_Subregion = case_when(
    # AFRICA
    str_detect(Country, "ALGERIA|EGYPT|TUNISIA") ~ "Northern Africa",
    str_detect(Country, "CAMEROON|CENTRAL AFRICAN REPUBLIC|CONGO, THE DEMOCRATIC REPUBLIC|ETHIOPIA|MADAGASCAR|NIGERIA|RWANDA|SENEGAL|SIERRA LEONE|SOUTH AFRICA|SUDAN|TOGO|ZAMBIA|MAURITANIA|IVORY COAST|LIBERIA") ~ "Sub-Saharan Africa",
    # AMERICAS
    str_detect(Country, "ARGENTINA|BOLIVIA|BRAZIL|CHILE|COLOMBIA|ECUADOR|PARAGUAY|PERU|URUGUAY|VENEZUELA") ~ "South America",
    str_detect(Country, "COSTA RICA|EL SALVADOR|GUATEMALA|MEXICO|NICARAGUA|PANAMA") ~ "Central America",
    str_detect(Country, "CUBA|DOMINICAN REPUBLIC|HAITI|PUERTO RICO") ~ "Caribbean",
    str_detect(Country, "CANADA|UNITED STATES") ~ "Northern America",
    # ASIA
    str_detect(Country, "AFGHANISTAN|BANGLADESH|INDIA|PAKISTAN|SRI LANKA|CEYLON") ~ "Southern Asia",
    str_detect(Country, "CHINA|HONG KONG|JAPAN|KOREA|SOUTH KOREA|TAIWAN") ~ "Eastern Asia",
    str_detect(Country, "BURMA|MYANMAR|LAOS|MALAYSIA|PHILIPPINES|SINGAPORE|THAILAND|VIETNAM") ~ "South-Eastern Asia",
    str_detect(Country, "IRAN|IRAQ|ISRAEL|JORDAN|KUWAIT|LEBANON|SAUDI ARABIA|SYRIA|TURKEY|UNITED ARAB EMIRATES|PALESTINIAN TERRITORY") ~ "Western Asia",
    str_detect(Country, "MONGOLIA") ~ "Eastern Asia",
    # EUROPE
    str_detect(Country, "AUSTRIA|BELGIUM|FRANCE|GERMANY|LUXEMBOURG|MONACO|NETHERLANDS|SWITZERLAND") ~ "Western Europe",
    str_detect(Country, "DENMARK|FINLAND|ICELAND|IRELAND|NORWAY|SWEDEN|UNITED KINGDOM|FAROE ISLANDS") ~ "Northern Europe",
    str_detect(Country, "BULGARIA|CZECH REPUBLIC|HUNGARY|POLAND|ROMANIA|SLOVAKIA|RUSSIAN FEDERATION|U.S.S.R|LITHUANIA|LATVIA|ESTONIA|BELARUS|UKRAINE|GEORGIA|ARMENIA") ~ "Eastern Europe",
    str_detect(Country, "CROATIA|GREECE|ITALY|MALTA|PORTUGAL|SERBIA|SLOVENIA|SPAIN|YUGOSLAVIA") ~ "Southern Europe",
    # OCEANIA
    str_detect(Country, "AUSTRALIA|NEW ZEALAND") ~ "Australia and New Zealand",
    str_detect(Country, "MAURITIUS") ~ "Sub-Saharan Africa",
    # SPECIAL CASES
    str_detect(Country, "HOLY SEE") ~ "Southern Europe",
    TRUE ~ "Unknown"
  ))

detailedData <- detailedData %>%
  left_join(coded_countries %>% rename(Nominator_Country = Country, Nominator_Region = UN_Subregion),
            by = "Nominator_Country") %>%
  left_join(coded_countries %>% rename(Nominee_Country = Country, Nominee_Region = UN_Subregion),
            by = "Nominee_Country")

# ===============================
# 1. Summary: Region Coding Coverage
# ===============================
detailedData %>%
  select(Nominee_Region, Nominator_Region) %>%
  summarise(
    total_records = n(),
    complete_records = sum(complete.cases(.)),
    proportion_complete = complete_records / total_records
  )

# ===============================
# 2. Generate Region-to-Region Matrix
# ===============================
region_data <- detailedData %>%
  filter(!is.na(Nominee_Region), !is.na(Nominator_Region)) %>%
  count(Nominator_Region, Nominee_Region) %>%
  complete(Nominator_Region, Nominee_Region, fill = list(n = 0))

# ===============================
# 3. Visualize Heatmap of Nominations
# ===============================
region_heatmap <- region_data %>%
  mutate(Nominator_Region = fct_rev(Nominator_Region)) %>%
  ggplot(aes(x = Nominee_Region, y = Nominator_Region, fill = n)) +
  geom_tile(color = NA) +
  geom_text(aes(label = ifelse(n > 0, n, "")), color = "white", size = 3) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", na.value = "white") +
  coord_fixed() +
  labs(
    title = "Nobel Nominations by Region",
    x = "Nominee Region",
    y = "Nominator Region",
    fill = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("~/Desktop/nobel_region_heatmap.pdf", plot = region_heatmap,
       width = 8.5, height = 11, units = "in", device = cairo_pdf)

# ===============================
# 4. Self-Nomination Rate (Proportional)
# ===============================
self_nominations_prop <- region_data %>%
  group_by(Nominator_Region) %>%
  summarise(
    total_nominations = sum(n),
    self_nominations = sum(n[Nominee_Region == Nominator_Region]),
    self_prop = self_nominations / total_nominations
  ) %>%
  arrange(desc(self_prop))

# ===============================
# 5. Top Non-Self Targets by Region (Proportional)
# ===============================
top_nonself_nominations_prop <- region_data %>%
  filter(Nominator_Region != Nominee_Region) %>%
  group_by(Nominator_Region) %>%
  mutate(prop = n / sum(n)) %>%
  slice_max(prop, n = 1, with_ties = FALSE) %>%
  ungroup()

# ===============================
# 6. Percent of Nominations Going Outward
# ===============================
percent_outward <- self_nominations_prop %>%
  mutate(percent_outward = 100 * (1 - self_prop)) %>%
  arrange(desc(percent_outward))

# ===============================
# 7. Regions Most Favored by Others (Internal % of incoming)
# ===============================
favored_by_others_within <- region_data %>%
  mutate(is_self = Nominator_Region == Nominee_Region) %>%
  group_by(Nominee_Region) %>%
  summarise(
    total_nominations = sum(n),
    from_other_regions = sum(n[!is_self]),
    prop_from_others = from_other_regions / total_nominations
  ) %>%
  arrange(desc(prop_from_others))

# ===============================
# 8. Regions Most Favored by Others (Share of all cross-nominations)
# ===============================
favored_by_others_global <- region_data %>%
  filter(Nominator_Region != Nominee_Region) %>%
  group_by(Nominee_Region) %>%
  summarise(
    nominations_from_others = sum(n)
  ) %>%
  mutate(
    prop_among_all_nonself = nominations_from_others / sum(nominations_from_others)
  ) %>%
  arrange(desc(prop_among_all_nonself))

# ===============================
# 9. Strong Bilateral Ties (Normalized Mutuality)
# ===============================
bilateral_unique <- region_data %>%
  rename(Region1 = Nominator_Region, Region2 = Nominee_Region) %>%
  filter(Region1 != Region2) %>%
  mutate(pair_id = map2_chr(Region1, Region2, ~ paste(sort(c(.x, .y)), collapse = " --- "))) %>%
  group_by(pair_id) %>%
  summarise(
    regions = str_split(pair_id[1], " --- "),
    Region_A = regions[[1]][1],
    Region_B = regions[[1]][2],
    mutual_nominations = sum(n),
    .groups = "drop"
  )

region_totals <- region_data %>%
  group_by(Nominator_Region) %>%
  summarise(total_outgoing = sum(n), .groups = "drop")

bilateral_normalized <- bilateral_unique %>%
  left_join(region_totals, by = c("Region_A" = "Nominator_Region")) %>%
  left_join(region_totals, by = c("Region_B" = "Nominator_Region"), suffix = c("_A", "_B")) %>%
  mutate(
    total_outgoing = total_outgoing_A + total_outgoing_B,
    prop_mutual = mutual_nominations / total_outgoing
  ) %>%
  select(-regions, -pair_id) %>%
  arrange(desc(prop_mutual))

# ===============================
# 10. Community Detection via Infomap
# ===============================
region_graph <- region_data %>%
  filter(n > 0) %>%
  graph_from_data_frame(directed = TRUE)

region_cliques <- cluster_infomap(region_graph, e.weights = E(region_graph)$n)

membership_df <- tibble(
  Region = names(membership(region_cliques)),
  Clique = membership(region_cliques)
)

# ===============================
# 11. Nomination Equity (Return on Nomination Effort)
# ===============================
top_nominators <- region_data %>%
  group_by(Nominator_Region) %>%
  summarise(total_made = sum(n), .groups = "drop")

top_nominees <- region_data %>%
  group_by(Nominee_Region) %>%
  summarise(total_received = sum(n), .groups = "drop")

equity_gap_prop <- full_join(top_nominators, top_nominees,
                             by = c("Nominator_Region" = "Nominee_Region")) %>%
  rename(Region = Nominator_Region) %>%
  replace_na(list(total_made = 0, total_received = 0)) %>%
  mutate(
    nomination_gap = total_made - total_received,
    return_ratio = total_received / (total_made + 1e-6),
    percent_return = 100 * return_ratio
  ) %>%
  arrange(desc(percent_return))
