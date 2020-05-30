********************************
*Name: Shadi Chamseddine
*Name: Christopher Lee
********************************

*DATA IMPORT

*Global macro for directory
global direc "C:\Users\Chris\Google Drive\University\Graduate Studies\Year 2 (Fall 2019 - Winter 2020)\Term 1 (Fall 2018)\ECON 5029 - Methods of Economic Research\Research Paper\Data & Analysis"

*Call to file
insheet using "$direc\stata_datafile.csv", clear

*Log 
log using "$direc\logfile", text replace

*Variable to determine amount of replications
local boots = 499

*DATA CLEANING AND FILTERING

*Drop records for players that played for 2 or more teams
keep if tm != "TOT"

*Drop records for players that don't have salaries
keep if salaryusd != "#N/A"

*Extract season starting year
gen season = substr(year,1,4)

*Convert string variables to numeric variables
*For some reason (destring var, replace) doesn't work
destring teamid, gen(teamid1)
destring wins, gen(wins1)
destring losses, gen(losses1)
destring winpercentage, gen(winpercentage1)
destring psg, gen(psg1)
destring pag, gen(pag1)
destring srs, gen(srs1)
destring salaryusd, gen(salaryusd1)
destring adjustedsalary2019usd, gen(adjustedsalary2019usd1)
destring season, gen(season1)

drop teamid wins losses winpercentage psg pag srs salaryusd adjustedsalary2019usd season
rename (teamid1 wins1 losses1 winpercentage1 psg1 pag1 srs1 salaryusd1 adjustedsalary2019usd1 season1) (teamid wins losses winpercentage psg pag srs salaryusd adjustedsalary2019usd season)

*Drop records for players on 2-way and 10 day contracts
keep if salaryusd >= minimumsalary

*THEIL INDEX CALCULATION

*Create variable to identify players on the same team in the same year
egen grouping = group(teamid season)
su grouping, meanonly

*Loop to calculate Theil index on salary for each team by year
forval i = 1/`r(max)' {

	*Display where we're at in the processing
	if floor((`i')/10) == (`i')/10 {
		noisily display "Working on `i' out of 450 Theil index processes at $S_TIME"
	}
	*Generate inequality measures
	quiet ineqdeco adjustedsalary2019usd, by (grouping)
	*Extract Theil index for current team and year
	quiet gen float adj_sal_theil`i' = r(ge1_`i') if grouping == `i'
}

*Combine all adjusted salary Theil index columns into 1 column
egen theil_adj_sal = rowtotal(adj_sal_theil*)
drop adj_sal_theil*

*Create log adjusted salary Theil index for each team by year
gen log_theil_adj_sal = log(theil_adj_sal)

*SALARY AND VORP DISPERSION (BOOTSTRAP) CALCULATION

*Create variable to identify players on the same team in the same year
egen group = group(teamid season)
su group, meanonly

*Loop to calculate salary and VORP bootstrap variance for each group
forval j = 1/`r(max)' {

	*Display where we're at in the processing
	if floor((`j')/10) == (`j')/10 {
		noisily display "Working on `j' out of 450 bootstrap processes at $S_TIME"
	}
	
	*Generate adjusted salary bootstrap standard deviation for each group
	quiet reg adjustedsalary2019usd if group == `j', vce(bootstrap, reps(`boots') seed(123))
	*Convert adjusted salary bootstrap standard deviation into adjusted salary bootstrap variance
	gen x`j' = _se[_cons]^2
	*Leave adjusted salary bootstrap variance only for its own group
	quiet gen float boot_adj_sal_var`j' = x`j' if group == `j'
	
	*Generate VORP bootstrap standard deviation for each group
	quiet reg vorp if group == `j', vce(bootstrap, reps(`boots') seed(123))
	*Convert VORP bootstrap standard deviation into VORP bootstrap variance
	gen a`j' = _se[_cons]^2
	*Leave VORP bootstrap variance only for its own group
	quiet gen float boot_vorp_var`j' = a`j' if group == `j'
}

*Combine all adjusted salary bootstrap columns into 1 column
egen bootstrap_adj_sal_var = rowtotal(boot_adj_sal_var*)
drop boot_adj_sal_var*

*Create log adjusted salary bootstrap for each team by year
gen log_bootstrap_adj_sal_var = log(bootstrap_adj_sal_var)

*Combine all VORP bootstrap columns into 1 column
egen bootstrap_vorp_var = rowtotal(boot_vorp_var*)
drop boot_vorp_var*

*Create log VORP bootstrap for each team by year
gen log_bootstrap_vorp_var = log(bootstrap_vorp_var)

*SETUP FOR REGRESSIONS AT THE TEAM YEAR LEVEL

*Aggregate data up to the team and year level
collapse (mean) avg_age = age (count) num_players = age, by(year teamid conference winpercentage season log_theil_adj_sal log_bootstrap_adj_sal_var log_bootstrap_vorp_var)

*Generate dummy variable for conference
quiet tab conference, gen(team_conference)

*REGRESSIONS

*Setup panel data format
xtset teamid season, yearly

*Put control variables into a local variable for easy use
local X team_conference* avg_age 

xtabond winpercentage log_bootstrap_adj_sal_var `X', maxldep(2) vce(robust)
xtabond winpercentage log_theil_adj_sal `X', maxldep(2) vce(robust)
xtabond winpercentage log_bootstrap_vorp_var `X', maxldep(2) vce(robust)

xtset, clear

clear
log close