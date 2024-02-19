
census_preproc <- function(root) {

  load(file.path(root, "data", "census", "raw_data.Rdata"))

  wage = data.frame(weight=as.numeric(as.character(df$PWGTP)))
  wage$state = factor(str_pad(as.character(df$ST), 2, pad='0'))


  wage$sex = c('male', 'female')[1+(df$SEX=='2')]
  wage$sex = factor(wage$sex)

  wage$age = as.numeric(as.character(df$AGEP))

  wage$race = c('white',
                'black',
                'AIAN',
                'AIAN',
                'AIAN',
                'asian',
                'NHOPI',
                'other',
                'mix')[as.numeric(as.character(df$RAC1P))]
  wage$race = factor(wage$race)

  wage$hispanic_origin = factor(c('no', 'yes')[1+(df$HISP!='1')])

  wage$nativity = c('native', 'foreign-born')[as.numeric(as.character(df$NATIVITY))]
  wage$nativity = as.factor(wage$nativity)

  wage$citizenship = as.numeric(as.character(df$CIT))

  wage$marital = c('married',
                   'widowed',
                   'divorced',
                   'separated',
                   'never married')[as.numeric(as.character(df$MAR))]
  wage$marital = factor(wage$marital)

  wage$family_size = as.numeric(as.character(df$NPF))

  wage$children = as.numeric(as.character(df$NOC))

  wage$english_level = as.numeric(as.character(df$ENG)) #4 not at all, 0 native

  wage$education_level = as.numeric(as.character(df$SCHL))
  wage$education_level[wage$education_level == 0] = NA
  wage$education = c('No schooling completed',
                     'Nursery school, preschool',
                     'Kindergarten',
                     'Grade 1',
                     'Grade 2',
                     'Grade 3',
                     'Grade 4',
                     'Grade 5',
                     'Grade 6',
                     'Grade 7',
                     'Grade 8',
                     'Grade 9',
                     'Grade 10',
                     'Grade 11',
                     '12th grade - no diploma',
                     'Regular high school diploma',
                     'GED or alternative credential',
                     'Some college, but less than 1 year',
                     '1 or more years of college credit, no degree',
                     "Associate's degree",
                     "Bachelor's degree",
                     "Master's degree",
                     "Professional degree beyond a bachelor's degree",
                     'Doctorate degree')[wage$education_level]

  ##########################
  wage$earnings = as.numeric(as.character(df$PERNP))

  wage$salary = as.numeric(as.character(df$WAGP))
  wage$salary[wage$salary == -1] = NA

  wage$hours_worked = as.numeric(as.character(df$WKHP))

  wage$weeks_worked = c(52,
                        49,
                        47,
                        39,
                        26,
                        0)[as.numeric(df$WKW)]

  wage$employment_status = c(NA,
                             'employed',
                             'not at work',
                             'unemployed',
                             'employed',
                             'not at work',
                             'not in labor force')[1+as.numeric(as.character(df$ESR))]
  wage$employment_status = factor(wage$employment_status)
  #table(wage$employment_status)

  wage$employer = c(NA,
                    'for-profit company',
                    'non-profit company',
                    'government',
                    'government',
                    'government',
                    'self-employed',
                    'self-employed',
                    'self-employed',
                    NA)[1+as.numeric(as.character(df$COW))]
  wage$employer = factor(wage$employer)

  wage$occupation = str_pad(as.character(df$OCCP), 4, pad='0')
  wage$occupation[wage$occupation=='0009'] = NA

  #convert to SOC code, the one provided in SOCP does not work but gives '*'
  #whenever there is many candidates for one OCCP category
  url = 'https://www2.census.gov/programs-surveys/acs/tech_docs/pums/code_lists/ACSPUMS2018CodeLists.xls'
  sheet_name = 'Occupation'
  GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
  table = read_excel(tf, sheet=sheet_name, col_names=FALSE)

  wage$occupation_description = ''
  for(i in 1:nrow(table)){
    print(i)
    key = as.character(table[i, 1])
    if(is.na(key) || length(key)==0){
      next
    }
    wage$occupation_description[wage$occupation == key] = table[i,3]
    wage$occupation[wage$occupation == key] = as.character(table[i,2])
  }
  wage$occupation = factor(wage$occupation)

  industry = str_pad(as.character(df$INDP), 4, pad='0')
  industry[wage$industry=='0169'] = NA

  url = 'https://www2.census.gov/programs-surveys/acs/tech_docs/pums/code_lists/ACSPUMS2018CodeLists.xls'
  sheet_name = 'Industry'
  GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
  table = read_excel(tf, sheet=sheet_name, col_names=FALSE)

  wage$industry_description = ''
  wage$industry = ''
  for(i in 1:nrow(table)){
    print(i)
    key = as.character(table[i, 3])
    if(is.na(key) || length(key)==0){
      next
    }
    wage$industry_description[industry == key] = table[i,2]
    wage$industry[industry == key] = as.character(table[i,5])
  }
  wage$industry = factor(wage$industry)

  wage$place_of_work = str_pad(as.character(df$POWSP), 3, pad='0')
  wage$place_of_work[wage$place_of_work=='000'] = NA
  wage$place_of_work = factor(wage$place_of_work)

  wage$economic_region = ''
  wage$economic_region[wage$place_of_work %in%
                         c('009', '023', '025', '033', '044', '050')] = 'New England'
  wage$economic_region[wage$place_of_work %in%
                         c('010', '011', '024', '034', '036', '042')] = 'Mideast'
  wage$economic_region[wage$place_of_work %in%
                         c('017', '018', '026', '039', '055')] = 'Great Lakes'
  wage$economic_region[wage$place_of_work %in%
                         c('019', '020', '027', '029', '031', '038', '046')] = 'Plains'
  wage$economic_region[wage$place_of_work %in%
                         c('001', '005', '012', '013', '021', '022', '028',
                           '037', '045', '047', '051', '054')] = 'Southeast'
  wage$economic_region[wage$place_of_work %in%
                         c('004', '035', '040', '048')] = 'Southwest'
  wage$economic_region[wage$place_of_work %in%
                         c('008', '016', '040', '048',
                           '030', '049', '056')] = 'Rocky Mountain'
  wage$economic_region[wage$place_of_work %in%
                         c('002', '006', '015', '032', '041', '053')] = 'Far West'
  wage$economic_region[as.numeric(as.character(wage$place_of_work)) > 56] = 'Abroad'
  wage$economic_region[is.na(wage$place_of_work)] = NA
  wage$economic_region = factor(wage$economic_region)

  ordering = c(
    'sex',
    'age',

    'race',
    'hispanic_origin',
    'citizenship',
    'nativity',

    'marital',
    'family_size',
    'children',
    'state',

    'education',
    'education_level',
    'english_level',

    'salary',
    'hours_worked',
    'weeks_worked',
    'employment_status',

    'occupation',
    'occupation_description',
    'industry',
    'industry_description',
    'employer',
    'place_of_work',
    'economic_region',

    'weight'
  )

  wage = wage[, ordering]
  save(wage, file='./computed_data/wage_data.Rdata')

}

gov_census <- function(root, min_age = 18L, max_age = 65L) {

  load(file.path(root, "data", "census", "wage_data.Rdata"))

  dat <- wage
  dat <- data.table(dat)
  rm.cols <- c("occupation_description", "industry_description", "place_of_work")

  dat <- dat[, -rm.cols, with = F]
  dat <- dat[age > min_age & age <= max_age]
  dat <- dat[employer == "government"]

  # impute 49 weeks
  dat[is.na(weeks_worked), "weeks_worked"] <- 49L
  dat <- dat[complete.cases(dat)]
  # look at A and C dependence

  protect.A <- "sex"
  dmgraph <- c("age", "race", "hispanic_origin", "citizenship", "nativity",
               "economic_region")
  fam <- c("marital", "family_size", "children")
  edu <- c("education_level", "english_level")
  work <- c("hours_worked", "weeks_worked", "occupation", "industry")
  out <- "salary"

  # plt <- list()
  #
  # for (feat in dmgraph) {
  #
  #   if (is.factor(dat[[feat]])) {
  #
  #     plt[[feat]] <- ggplot(dat, aes_string(x = feat, fill = "sex")) +
  #       geom_bar(position = "fill") +
  #       scale_y_continuous(labels = scales::percent) + theme_bw()
  #
  #   } else {
  #
  #     plt[[feat]] <- ggplot(dat, aes_string(x = feat, fill = "sex")) +
  #       geom_density(alpha = 0.4) + theme_bw()
  #
  #   }
  #
  # }
  #
  # ggsave(file.path(root, "tests", "census-data", "AC-dependence.png"),
  #        plot = cowplot::plot_grid(plotlist = plt), height = 8, width = 12)

  nuke.cols <- c("weight", "employment_status", "employer", "education",
                 "state")
  dat[, -nuke.cols, with = F]

}
