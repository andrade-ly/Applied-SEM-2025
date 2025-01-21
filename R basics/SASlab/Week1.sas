* 1) assign the libname where you will create permanent SAS datasets, access SAS datasets, etc;

libname d831"C:\Users\vmiller\Dropbox\D831\2019\Week1 Biostats review\SASlab1";

* 2) import Data sheet from the Excel file used for data entry 
     proc contents shows variables in their order within dataset (varnum option);

proc import datafile="C:\Users\vmiller\Dropbox\D831\2019\Week1 Biostats review\SASlab1\nteeth.xlsx" 
  out=teeth replace
  dbms=xlsx;
run;
proc contents data=teeth varnum; 
  title2 'output from proc contents data=teeth varnum;';
run;

proc print data=teeth (obs=10); 
  title2 "Output from: proc print data=teeth";
run;

* 3) create user-defined formats
     Value statements are pasted from Dictionary tab of Nteeth.xlsx;
proc format;  
value Sexf 	
	1="Male"
	2="Female"
    .="missing";
value Racef 	
	1="White"
	2="Black or African American"
	3="Asian"
	4="Other"
	8="Don't know"
	9="Refused"
    .="missing";
value Ethnicityf 	
	1="Hispanic"
	2="Non-Hispanic"
	8="Don't know"
	9="Refused"
	.="missing";
run;

* 4) Now "attach" formats to variables in a proc print.
     Also, attach descritive labels to variables
     This changes appearance of output, but does not change original variables;
proc print data=teeth(obs=10) label ; 
  title2 "Output from: proc print data=teeth(obs=10) label";
   * variable labels;
   label ID="Subject ID" Visit="Visit sequence" VisitDate="Date of examination"
     Age="Age of study participant in years" Sex="Sex of study participant "
     Race="Race of study participant" Ethnicity="Ethnicity of study participant "
     Nteeth="Number of permanent teeth";
  * apply user-defined formats;
  format sex sexf. race racef. ethnicity ethnicityf.  VisitDate mmddyy10.;
  *mmddyy10 is a SAS defined format SAS has many ways to handle dates;
run;


* 5) Check for duplicate records and create a permanent dataset.
     Labels and formats in this proc sort will be attached to the new datasets by default
     To share datasets that have user-defined format attached, you will give future users a 
       catalog of your user-defined format (see Week 2);
proc sort data=teeth out=d831.teeth nodupkey dupout=dupnteeth;
  by id visit;
   label ID="Subject ID" Visit="Visit sequence" VisitDate="Date of examination"
     Age="Age of study participant in years" Sex="Sex of study participant "
     Race="Race of study participant" Ethnicity="Ethnicity of study participant "
     Nteeth="Number of permanent teeth";
  * apply user-defined formats;
  format sex sexf. race racef. ethnicity ethnicityf.  VisitDate mmddyy10.;
run;
proc contents data=d831.teeth varnum; 
  title2 'proc contents data=d831.teeth varnum; ';
run;

proc print data=dupnteeth label;
  title2 "Default output from proc print data=dupnteeth label;";
  * only the 2nd duplicate is saved to dupneteeth;
run;
* the default formats can be over-ridden within a procedure;
proc print data=dupnteeth label; 
  title2 "Output from: proc print data=dupnteeth with format statement to nullify default formats";
  format sex race ethnicity;
run;


* 6) %include executes proc_codebook.sas program  - source2 option shows its processing in the log window
  The program loads a macro that produces a nice codebook from a SAS dataset
  Source of proc_codebook.sas is Carolina Population Center
  http://www.cpc.unc.edu/research/tools/data_analysis/proc_codebook/proc_codebook.sas/view
  Detailed instructions are in proc_codebook.sas;

%include "C:\Users\vmiller\Dropbox\D831\2019\Week1 Biostats review\SASlab1\proc_codebook.sas"/source2; 

%let organization=One Record per Visit (key code is ID+Visit); 
%proc_codebook(lib=d831,
		file1=teeth, 
		fmtlib=work.formats, 
		pdffile=C:\Users\vmiller\Dropbox\D831\2019\week1\teeth.pdf,
        INCLUDE_WARN=no); 
run;

* 7) Identifying both occurrences of duplicates;
proc sort data=teeth; by id visit; run; * do not remove duplicates;
* data _null_ with put statements are a useful way to "look" inside a data step
  - by default, the put statements appear in the log window;
data _null_;
  set teeth;
  by id visit;
  put id= visit= first.id= last.id= first.visit= last.visit= ;
  * first.<variable> is a system-generated indicator variable created with
    a data step includes a by... statement;
run;
data dupkey; * create a new dataset;
  set teeth;  * read data from existing dataset;
  by id visit; * read data from existing dataset;
  if not (first.visit and last.visit) then output; * specify observations to be output (aka saved);
run;

* report duplicate errors;
options orientation=portrait;
ods rtf file="C:\Users\vmiller\Dropbox\D831\2019\Week1 Biostats review\SASlab1\Teeth-Study-QCReport.rtf";
proc print data=dupkey noobs;
  title2 "List of duplicates in Teeth.xlsx";
run;
* a summary report of missing values for numeric variables;
proc means data=teeth nmiss;
  title2 "Summary of missing values in Teeth.xlsx";
   label ID="Subject ID" Visit="Visit sequence" VisitDate="Date of examination"
     Age="Age of study participant in years" Sex="Sex of study participant "
     Race="Race of study participant" Ethnicity="Ethnicity of study participant "
     Nteeth="Number of permanent teeth";
  format VisitDate mmddyy10.;
run;
* a simple way to list observations with one or more missing values;
data missdata; * create a new dataset;
  set teeth;
  nmissing=nmiss(of id--nteeth);
  * nmissing is a SAS function that counts the number of missing values among variables
    specified in parentheses. 
    (of id--nteeth) tells SAS to list all variables in the sequence between id and nteeth
    where the sequence is as specified in the first proc contents, above;
run;
proc print data=missdata label noobs;
  title2 "Listing of observations with missing values in Teeth.xlsx";
  where (nmissing gt 0);  * specify observations to selected for this proc;
  format VisitDate mmddyy10.;
run;

* 7) Using proc tabulate to produce reports;

proc tabulate data=teeth order=formatted;
  title2 "output from: proc tabulate data=teeth";
  var age nteeth ;
  class sex;
  label ID="Subject ID" 
     Age="Age of study participant in years" Sex="Sex of study participant "
     Race="Race of study participant" Ethnicity="Ethnicity of study participant "
     Nteeth="Number of permanent teeth";
  format sex sexf. race racef. ethnicity enthnicityf.;
  tables (nteeth)*sex (age)*sex,(n*f=4.0 (mean stddev)*f=8.2 
        lclm*f=8.2  uclm*f=8.2)/rts=50;
run;
ods rtf close;

* 8) Proc Univariate for summary statistics;

proc univariate data=teeth;
	var age;
	histogram age/normal endpoints = 18 to 70 by 10;
	title2 "output from proc univariate data=teeth";
run;

* 9) basic plots using ODS Graphics;

ods graphics on;
ods select Plots SSPlots; *without the ODS graphics statement you will get line printer output;
proc univariate data=teeth plot;
	var age;
	histogram age/normal endpoints = 18 to 70 by 10; *the endpoints option allows you to specific the x-axis;
	title2 "output from proc univariate histogram of age data=teeth";
run;

proc sgplot data=teeth;
	histogram age;
	title2 "output from proc sgplot histogram data=teeth";
run;

*to examine the distribution of people by race we can also use sgplot; 
proc sgplot data=teeth;
	vbar sex/group=race groupdisplay=cluster;
	title2 "Distribution of gender by race";
	format race racef. sex sexf.;
run;
* 10) boxplots using proc boxplot;

*first sort by the variable you want to use to group your data;
proc sort data=teeth; by sex; run;

*now the data is in the right order to create a boxplot;
  title2 "output from: proc boxplot data=teeth age by sex";
proc boxplot data=teeth;
	plot age*sex; *remember the first variable is your analysis variable and the second variable is your category variable;
	format sex sexf.; 
run;
*SAS has lots of ways to do the same thing. Let's make boxplots using sgplot;
proc sgplot data=teeth;
	vbox age/ category=sex;
	format sex sexf.;
	title2 "Age by sex";
run;
	

* 11) proc freq is a very useful tool;
proc freq data=teeth;
	tables race*sex;
	format race racef. sex sexf.;
	title2 "output from proc freq of race by sex";
run;


