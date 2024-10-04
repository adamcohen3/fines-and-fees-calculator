# The Hawaii State Judiciary Fines and Fees Calculator

## Purpose

### Access to Justice

This calculator is part of an [Access to Justice initiative](https://www.courts.state.hi.us/services/access_to_justice_initiative_main_page) at the Hawai'i State Judiciary. These initiatives seek to lower barriers to accessing the courts given that most people lack the money, time, and legal expertise to make use of court services. The calculator is designed to help people, especially those experiencing financial hardship, calculate the fines and fees owed on a ticket, and if eligible, request either a reduction in their fines or a conversion to community service. 

Calculating the fines and fees on a ticket is less straightforward than it seems. While many of the fines and fees that populate the calculator are fixed (see the rds file at top level of the repo), some of the most common offenses require a court hearing because the fine and fees vary depending on a number of factors, including the number of prior offenses and the age of the defendant. In addition, for certain offenses, the court has discretion to set the fine between a minimum and maximum penalty. This is where the calculator comes in.

For these non-static charges, the calculator allows the user to set the parameters based on their situation (e.g., 1 prior offense in the last year). The initial fines are then determined dynamically, based on user input and the underlying logic of the statutes. After seeing the initial fine or range of fines, the user can input what they are able to pay (for any fine that is not mandatory).

### Transparency

In addition to increasing access to justice, the calculator is intended to bring transparency to the traffic code. It shows the monetary penalties for common traffic offenses without needing to look up statutes, analyze their logic, or translate legalese into plain language. This calculator can be used by defendants, attorneys, the court, and the general public. 

### RCT

The calculator is currently part of a preregistered randomized control trial studying whether people with access to the calculator are more likely than those without access to a) motion the court for a reduction, b) have their motion granted, c) receive larger fine reductions, d) pay their fines, and e) not recidivate in the future.

 ## Data

The calculator relies on three sources of data for offenses and their monetary penalties:  
1. **Hawaii Revised Statutes**: State laws passed by the Legislative branch ([HRS](https://www.capitol.hawaii.gov/hrsall/))
2. **Hawaii Administrative Rules**: Rules established by the Executive branch ([HAR](https://ltgov.hawaii.gov/the-office/administrative-rules/))
3. **Local Ordinances**: Local laws passed by the City Councils (e.g., [ROH](https://www8.honolulu.gov/ocs/revised-ordinances-of-honolulu/))  

It also relies on two sources for cost of living, which can serve as a reference point in determining a defendant's ability to pay their fines and fees:
1. [2024 Federal poverty guidelines *for Hawaii*](https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines)
2. [2020 ALICE survival budget *for Hawaii*](https://www.unitedforalice.org/household-budgets/hawaii)

As of 2024, the calculator primarily focuses on traffic offenses. Most *fines* can be reduced or converted to community service by the court. Most *fees* cannot.

## How it works

Calculating the fines and fees on a ticket is less straightforward than it seems. Some offenses require a court hearing, where the fine and fees are set by the court. This is where the calculator comes in. While most of the fines and fees that populate the calculator are static (see the rds file at top level of the repo), the remaining fines vary depending on a number of parameters determined by statute, including the number of prior offenses and the age of the defendant. In addition, for certain offenses, the court has discretion to set the fine between a minimum and maximum penalty, the range also being determined by statute.

For these non-static charges, the calculator allows the user to set the parameters based on their situation (e.g., 1 prior offense, minor). They can also see the minimum and maximum range for the fine. Based on user input, the initial fines are determined dynamically, following the underlying logic of the statutes. After seeing the initial fine or range of fines, the user can input what they are able to pay for any fine that is not mandatory.

Following a "Welcome and FAQ" screen, the calculator proceeds in four steps.

### Step 1: Charges
1. The user indicates their role, the date of the offenses, and the name of the offenses.
![step 1](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/calculator_step1_crop2.JPG) 

### Step 2: Ability to Pay Form
2. The user selects whether they want to reduce their fines, convert them to community service, or skip ahead. If requesting a reduction or conversion, the user completes an "ability to pay" form that can be provided to the court to determine if the person is eligible.
![step 2](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/calculator_step2_crop2.JPG) 

### Step 3: Fines and Fees
3. The user indicates what they are able to pay.
![step 3](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/calculator_step3_crop2.JPG) 

### Step 4: Summary
4. The user reviews and prints out a summary of what they can pay, and follows the instructions describing how to use the information provided to make a request to the court for a fine reduction or community service conversion.
![step 4](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/calculator_step4_crop2.JPG) 

## Deployment note

If you attempt to run a copy of the shinyapp locally or remotely, you will need disable code that enable persistent storage on Dropbox (using ctrl-f and "dropbox persistent storage" should locate the relevant sections). Or you can create your own [Dropbox token](https://github.com/karthik/rdrop2) to keep persistent storage.
