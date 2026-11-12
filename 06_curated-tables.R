# --------------------------------------------------------------------
# 06: Tables 1, 2, and 4 — curated (non-computational) tables
#
# These tables are interpretive summaries of the clustering solutions,
# not computed outputs. They are transcribed cell-by-cell from the
# published version (SF-Final/SOY122 (corrected).pdf, pp. 10-12, 26)
# and written as tidy CSVs.
#
#   output/tables/table1-iml-issues-tactics-targets.csv
#   output/tables/table2-supplementary-variables.csv
#   output/tables/table4-iml-summary.csv
#   output/tables/table4-period-membership.csv
# --------------------------------------------------------------------

if (!file.exists("run-all.R")) stop("Run scripts from the repository root (the folder containing run-all.R).")
OUTT <- file.path(getwd(), "output", "tables")
dir.create(OUTT, showWarnings = FALSE, recursive = TRUE)

periods <- c("1960-1965", "1966-1972", "1973-1987", "1988-1995")

# The published table marks items new since the previous period with a
# superscript "a"; items are stored without the marker, tracked by New.
mk <- function(logic, period, comp, col, items, new = rep(FALSE, length(items)))
  data.frame(Logic = logic, Period = periods[period], Component = comp,
             Col = col, Item = items, New = new,
             stringsAsFactors = FALSE)

t1 <- list()
add <- function(logic, period, comp, col, items, new = rep(FALSE, length(items)))
  t1[[length(t1) + 1]] <<- mk(logic, period, comp, col, items, new)

NPC <- "National political conformist"
LPC <- "Local political crusader"
RA  <- "Racial aggressor"
SO  <- "School obstructionist"
CR  <- "Civil rights"
PL  <- "Pro-Life"

# ---- National political conformist --------------------------------------
add(NPC, 1, "Issues", 1, c("Civil Rts.", "Latino", "Human Rights"))
add(NPC, 1, "Issues", 2, c("Peace", "Pro-war"))
add(NPC, 2, "Issues", 1, c("Human Rights", "Peace"))
add(NPC, 2, "Issues", 2, c("Poverty", "Pro-war"), c(TRUE, FALSE))
add(NPC, 3, "Issues", 1, c("Anti-nuclear", "Economic", "Peace"), c(TRUE, TRUE, FALSE))
add(NPC, 3, "Issues", 2, c("Pro-LGBT", "Pro-life"), c(TRUE, TRUE))
add(NPC, 4, "Issues", 1, c("Animal Rights", "Civil Rts.", "Disabled", "Civil Rts.",
                           "Latino", "Human Rights", "Peace"),
    c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE))
add(NPC, 4, "Issues", 2, c("Pro-Blacks", "Pro-LGBT", "Religion", "Women"),
    c(TRUE, FALSE, TRUE, TRUE))
add(NPC, 1, "Tactics", 1, c("Info. Distribution"))
add(NPC, 1, "Tactics", 2, c("Vigil"))
add(NPC, 2, "Tactics", 1, c("Ceremony", "Info. Distribution", "Letter", "Lobby",
                            "March/Procession"), rep(TRUE, 5))
add(NPC, 2, "Tactics", 2, c("Picket", "Rally/Demonstr.", "Symbolic Display", "Vigil"), rep(TRUE, 4))
add(NPC, 3, "Tactics", 1, c("Civil Disobedience", "Info. Distribution", "Letter-writing", "Lobby"),
    c(TRUE, FALSE, FALSE, FALSE))
add(NPC, 3, "Tactics", 2, c("March/Procession", "Picket", "Rally/Demonstr.", "Vigil"))
add(NPC, 4, "Tactics", 1, c("Boycott", "Civil Disobedience", "Info. Distribution", "Letter-writing"),
    c(TRUE, FALSE, FALSE, FALSE))
add(NPC, 4, "Tactics", 2, c("March/Procession", "Picket", "Rally/Demonstr.", "Vigil"))
add(NPC, 1, "Targets", 1, c("Cuba", "Federal Govt."))
add(NPC, 1, "Targets", 2, c("Foreign Govt.", "USSR"))
add(NPC, 2, "Targets", 1, c("Federal Govt."))
add(NPC, 3, "Targets", 1, c("Federal Govt.", "Medical Instit."), c(FALSE, TRUE))
add(NPC, 3, "Targets", 2, c("Foreign Govt."), TRUE)
add(NPC, 4, "Targets", 1, c("Federal Govt."))
add(NPC, 4, "Targets", 2, c("Religious Group"), TRUE)

# ---- Local political crusader -------------------------------------------
add(LPC, 1, "Issues", 1, c("Education", "First Amendment", "Police Brutality", "Pro-minority"))
add(LPC, 1, "Issues", 2, c("Religion", "State Repression", "Urban"))
add(LPC, 2, "Issues", 1, c("Anti-crime", "Civil Rts. Latino", "Environmental",
                           "First Amendment", "Police Brutality"), c(TRUE, TRUE, TRUE, FALSE, FALSE))
add(LPC, 2, "Issues", 2, c("Pro-Blacks", "Pro-minority", "State Repression", "Urban", "Women"),
    c(TRUE, FALSE, FALSE, FALSE, TRUE))
add(LPC, 3, "Issues", 1, c("Animal Rights", "Anti-crime", "Civil Rts. Latino", "Civil Rts. Native",
                           "Consumer Protect", "Environmental", "First Amendment", "NIMBY",
                           "Police Brutality"),
    c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE))
add(LPC, 3, "Issues", 2, c("Poverty", "Prisoner Rights", "Pro-Blacks", "Pro-choice", "Pro-minority",
                           "Religion", "State Repression", "Urban", "Women"),
    c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
add(LPC, 4, "Issues", 1, c("Anti-crime", "Anti-immigration", "Civil Rts. Native", "Consumer Protect.",
                           "Economic", "Environmental", "First Amendment"),
    c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
add(LPC, 4, "Issues", 2, c("NIMBY", "Police Brutality", "Poverty", "Prisoner Rights",
                           "Pro-choice", "Urban"))
add(LPC, 1, "Tactics", 1, c("Boycott", "Lawsuit", "Letter-writing", "March/Procession"))
add(LPC, 1, "Tactics", 2, c("Media", "Picket", "Rally/Demonstr.", "Riot"))
add(LPC, 2, "Tactics", 1, c("Lawsuit", "Media"))
add(LPC, 2, "Tactics", 2, c("Riot"))
add(LPC, 3, "Tactics", 1, c("Lawsuit", "Media"))
add(LPC, 3, "Tactics", 2, c("Riot"))
add(LPC, 4, "Tactics", 1, c("Lawsuit", "Lobby"), c(FALSE, TRUE))
add(LPC, 4, "Tactics", 2, c("Media", "Riot"))
add(LPC, 1, "Targets", 1, c("Educ. Institution", "Local Govt."))
add(LPC, 1, "Targets", 2, c("State Govt."))
add(LPC, 2, "Targets", 1, c("Local Govt."))
add(LPC, 2, "Targets", 2, c("Private"), TRUE)
add(LPC, 3, "Targets", 1, c("Local Govt.", "Private"))
add(LPC, 3, "Targets", 2, c("Religious Group", "State Govt."), c(TRUE, TRUE))
add(LPC, 4, "Targets", 1, c("Local Govt.", "Private"))
add(LPC, 4, "Targets", 2, c("State Govt."))

# ---- Racial aggressor ----------------------------------------------------
add(RA, 1, "Issues", 1, c("Anti-Blacks")); add(RA, 1, "Issues", 2, c("Racial Grievance"))
add(RA, 2, "Issues", 1, c("Anti-Blacks")); add(RA, 2, "Issues", 2, c("Racial Grievance"))
add(RA, 3, "Issues", 1, c("Anti-Blacks")); add(RA, 3, "Issues", 2, c("Racial Grievance"))
add(RA, 4, "Issues", 1, c("Anti-LGBT"), TRUE); add(RA, 4, "Issues", 2, c("Racial Grievance"))
add(RA, 1, "Tactics", 1, c("Attack/Fight")); add(RA, 1, "Tactics", 2, c("Symbolic Display"))
add(RA, 2, "Tactics", 1, c("Attack/Fight"))
add(RA, 3, "Tactics", 1, c("Attack/Fight")); add(RA, 3, "Tactics", 2, c("Symbolic Display"), TRUE)
add(RA, 4, "Tactics", 1, c("Attack/Fight")); add(RA, 4, "Tactics", 2, c("Symbolic Display"))
add(RA, 1, "Targets", 1, c("Ethnic Group"))
add(RA, 2, "Targets", 1, c("Ethnic Group"))
add(RA, 3, "Targets", 1, c("Ethnic Group"))
add(RA, 4, "Targets", 1, c("Ethnic Group")); add(RA, 4, "Targets", 2, c("Homosexuals"), TRUE)

# ---- School obstructionist (as printed in the published table) ----------
# NOTE: in the published table the Targets row of this panel reads
# "Civil Disobedience | Strike | Sit-in"; transcribed here as printed.
add(SO, 2, "Issues", 1, c("Education"))
add(SO, 3, "Issues", 1, c("Education")); add(SO, 3, "Issues", 2, c("Human Rights"), TRUE)
add(SO, 4, "Issues", 1, c("Education")); add(SO, 4, "Issues", 2, c("Pro-minority"), TRUE)
add(SO, 2, "Tactics", 1, c("Boycott")); add(SO, 2, "Tactics", 2, c("Sit-in"))
add(SO, 3, "Tactics", 1, c("Boycott")); add(SO, 3, "Tactics", 2, c("Strike"))
add(SO, 4, "Tactics", 1, c("Sit-in"))
add(SO, 2, "Targets", 1, c("Civil Disobedience")); add(SO, 2, "Targets", 2, c("Strike"))
add(SO, 3, "Targets", 1, c("Sit-in"))

# ---- Civil rights --------------------------------------------------------
add(CR, 1, "Issues", 1, c("Pro-Blacks"))
add(CR, 1, "Tactics", 1, c("Civil Disobedience")); add(CR, 1, "Tactics", 2, c("Sit-in"))
add(CR, 1, "Targets", 1, c("Private"))

# ---- Pro-Life ------------------------------------------------------------
add(PL, 4, "Issues", 1, c("Pro-life"))
add(PL, 4, "Targets", 1, c("Medical Institution"))

table1 <- do.call(rbind, t1)
write.csv(table1, file.path(OUTT, "table1-iml-issues-tactics-targets.csv"), row.names = FALSE)

# ---- Table 2: supplementary variable descriptions ------------------------
table2 <- data.frame(
  Variable = c("No. of SMOs", "No. of Targets", "Event Size", "Violence",
               "Initiating Group", "Policing", "Region"),
  Description = c(
    "A count of named social movement organizations reported to be participating in an event. SMOs appeared at fewer than half of all events (42%), and among those with SMOs 20% included two or more.",
    "A count of identifiable targets reported for the event. Only 5% of all events had no apparent target, while 10% had two (very few had more). The vast majority (85%) had one target.",
    "The number of participants reported at the event. This variable combines two from the original DCA dataset, one an exact count and the other an estimated count (for 49% and 51% of events respectively, depending on which was reported in the NYT). The exact-count variable has a median of 100 and mean of 3,602. The estimated-count variable contains six categories: 1-Small, few handful (1-9 people); 2-Group, committee (10-49 people); 3-Large, gathering (50-99 people); 4-Hundreds, mass, mob (100-999 people); 5-Thousands (1,000-9,999 people); 6-Tens of thousands (10,000 or more people). The modal category is 2 (10-49 people).",
    "A series of variables that capture whether or not a given event reportedly includes: weapons (rocks, bombs, guns, firebombs, bricks, stones), physical or hand-to-hand violence, other forms of violence, weapons and physical violence, weapons and other forms, physical and other forms, weapons, physical, and other forms, property damage, injuries, or death.",
    "Characteristics of the members of an identifiable social, racial, or ethnic group reported to have initiated an event. Coders were limited to two characteristics (e.g., white + students) for each of three initiating groups per event. The group codes present in this analysis include Black, White, Other Race, Students, Residents, SMOs, Occupational Group, Political Group, Religious Group, and Jewish.",
    "The presence or absence of police, and the type of actions they took. Police were present at about one-quarter of all events and took some form of action at just over three-quarters of those. Four (nested) levels of action could be coded when police are present: no action, action, force, and violence. The data also indicate whether or not they made arrests.",
    "A categorical variable indicating the region of the country-Northeast, Midwest, West, South-in which an event occurred."
  ),
  stringsAsFactors = FALSE
)
write.csv(table2, file.path(OUTT, "table2-supplementary-variables.csv"), row.names = FALSE)

# ---- Table 4: summary of IMLs -------------------------------------------
t4 <- data.frame(
  Attribute = rep(c("Issues", "Targets (Arena)", "Tactical Orientation",
                    "Event Characteristics", "IML Characteristics"), 6),
  Logic = rep(c("Civil rights", "Racial aggressor", "National political conformist",
                "Local political crusader", "School obstructionist", "Pro-Life"), each = 5),
  Value = c(
    "Afr. American civil rights",
    "Private organizations",
    "Disruptive",
    "Initiated by Blacks, use of police force and violence",
    "Narrow niche, low stability",
    "Racial, sexuality",
    "Racial/ethnic groups, expanding to homosexuals",
    "Mix of violent, symbolic",
    "Small, initiated by Whites, presence of weapons, violence, injuries, deaths, property damage",
    "Narrow niche, moderate-high stability",
    "Rights-related, peace",
    "National gov'ts, occasionally nonpolitical",
    "Conventional, with increasing variation and confrontation",
    "Very large, multiple SMOs in the earlier periods, single SMO later, initiated by varied groups",
    "Niche broadens (first tactics, then issues), low stability",
    "Policing, First Amendment, urban, environmental, and others",
    "Local and state gov'ts, expanding to private organizations, occasionally nonpolitical",
    "Mostly conventional, but includes riots, with declining variation",
    "Multiple initiators (formal and identity groups), becomes more coalitional over time and less disruptive",
    "Broad niche, low stability",
    "Education",
    "Educational Institutions",
    "Disruptive, with declining variation",
    "Initiated by students, not SMO related, becomes more severely repressed in period 4",
    "Niche narrows, moderate stability",
    "Antiabortion",
    "Medical Institutions",
    "N/A",
    "Midwest, religious initiating groups, multiple targets, police involvement, counterdemonstrators.",
    "Narrow niche, low stability (a: as a new IML stability is low by definition; because it only appears in the final period we cannot assess its long-term stability)"
  ),
  stringsAsFactors = FALSE
)
write.csv(t4, file.path(OUTT, "table4-iml-summary.csv"), row.names = FALSE)

t4m <- data.frame(
  Period = rep(periods, each = 5),
  Logic = rep(c("Civil rights", "Racial aggressor", "National political conformist",
                "Local political crusader", "School obstructionist"), 4),
  Present = c(
    "X", "X", "X", "X", "",     # period 1
    "",  "X", "X", "X", "X",    # period 2
    "",  "X", "X", "X", "X",    # period 3
    "",  "X", "X", "X", "X"     # period 4 (Pro-Life row added below)
  ),
  stringsAsFactors = FALSE
)
t4m <- rbind(t4m, data.frame(Period = periods[4], Logic = "Pro-Life", Present = "X"))
write.csv(t4m, file.path(OUTT, "table4-period-membership.csv"), row.names = FALSE)

cat("Curated tables written to", OUTT, "\n")
