-- ******************
-- Import data
-- ******************

--policyID,statecode,county,eq_site_limit,hu_site_limit,fl_site_limit,
--fr_site_limit,tiv_2011,tiv_2012,eq_site_deductible,hu_site_deductible,fl_site_deductible,fr_site_deductible,
--point_latitude,point_longitude,line,construction,point_granularity

--119736,FL,CLAY COUNTY,498960,498960,498960,498960,498960,792148.9,0,9979.2,0,0,30.102261,-81.711777,Residential,Masonry,1

.print ' '
.print 'Importing data'
-- First, create the table that the CSV will be stored in

CREATE TABLE "insurance" (
policyID INTEGER,
statecode CHAR,
county CHAR,
eq_site_limit REAL,
hu_site_limit REAL,
fl_site_limit REAL,
fr_site_limit REAL,
tiv_2011 REAL,
tiv_2012 REAL,
eq_site_deductible INTEGER,
hu_site_deductible REAL,
fl_site_deductible INTEGER,
fr_site_deductible INTEGER,
point_latitidu REAL,
point_longitude REAL,
line CHAR,
construction CHAR,
point_granularity INTEGER
);

-- Tell SQL to expect a CSV file by declaring CSV mode

.mode csv

-- Import the CSV follwing the directions on the sqlitetutorial website
.import FL_insurance_sample.csv insurance

--Drop the header row

DELETE FROM insurance WHERE policyID = 'policyID';

-- ******************
-- View first 10 observations
-- ******************

.print ''
.print 'view first 10 observation'

SELECT * FROM insurance LIMIT 10;

-- ******************
-- How many unique values of a certain variable?
-- ******************

.print ''
.print 'unique counties'

SELECT DISTINCT county FROM insurance;

-- ******************
-- average property appreciation from 2011 to 2012
-- ******************
.print ' '
.print 'mean different between property appreciation from 2011 to 2012'
SELECT AVG(tiv_2012 - tiv_2011) FROM insurance;


-- ******************
-- Construction: wood or other
-- ******************
.print ' '
.print 'wood or other'
SELECT construction, COUNT(*) FROM insurance GROUP BY construction;

-- ******************
-- Save as text file
-- ******************

.output insurance.sqlite3
.dump

