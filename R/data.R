# Internal lookup tables (not exported).

.states <- data.frame(
  abb = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
          "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
          "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
          "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
          "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
           "Colorado", "Connecticut", "Delaware", "District of Columbia",
           "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
           "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
           "Massachusetts", "Michigan", "Minnesota", "Mississippi",
           "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
           "New Jersey", "New Mexico", "New York", "North Carolina",
           "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
           "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
           "Texas", "Utah", "Vermont", "Virginia", "Washington",
           "West Virginia", "Wisconsin", "Wyoming"),
  fips = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13",
           "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
           "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
           "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48",
           "49", "50", "51", "53", "54", "55", "56"),
  stringsAsFactors = FALSE
)

.territories <- data.frame(
  abb  = c("PR", "GU", "VI", "AS", "MP"),
  name = c("Puerto Rico", "Guam", "U.S. Virgin Islands", "American Samoa",
           "Northern Mariana Islands"),
  fips = c("72", "66", "78", "60", "69"),
  stringsAsFactors = FALSE
)

# Extra spellings that are exact (not fuzzy) matches. Keys are already
# normalised (lower case, no punctuation, single spaces).
.aliases <- data.frame(
  alias = c("washington dc", "washington d c", "dist of columbia",
            "district columbia", "wash dc", "us virgin islands",
            "virgin islands", "united states virgin islands",
            "commonwealth of massachusetts", "commonwealth of pennsylvania",
            "commonwealth of virginia", "commonwealth of kentucky",
            # traditional / AP-style abbreviations that are not unique
            # prefixes (ala = Alabama or Alaska, miss = Mississippi or
            # Missouri, ...) or that are not prefixes at all
            "ala", "ariz", "calif", "cal", "colo", "conn", "fla", "ill",
            "ind", "kan", "kans", "mass", "mich", "minn", "miss", "mont",
            "neb", "nebr", "nev", "okla", "oreg", "penn", "penna", "tenn",
            "tex", "wash", "wis", "wisc", "wyo", "ore", "del"),
  abb   = c("DC", "DC", "DC", "DC", "DC", "VI", "VI", "VI",
            "MA", "PA", "VA", "KY",
            "AL", "AZ", "CA", "CA", "CO", "CT", "FL", "IL",
            "IN", "KS", "KS", "MA", "MI", "MN", "MS", "MT",
            "NE", "NE", "NV", "OK", "OR", "PA", "PA", "TN",
            "TX", "WA", "WI", "WI", "WY", "OR", "DE"),
  stringsAsFactors = FALSE
)
