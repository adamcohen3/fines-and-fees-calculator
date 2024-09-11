# The Hawaii State Judiciary Fines and Fees Calculator

![hex thumbnail](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/hex_thumbnail_207x240.png) 

[https://adamc3.shinyapps.io/lfo_calculator/](https://adamc3.shinyapps.io/lfo_calculator/)    
<br>
The fines and fees calculator is part of an [Access to Justice initiative](https://www.courts.state.hi.us/services/access_to_justice_initiative_main_page) at the Hawai'i State Judiciary. The calculator is designed to help people calculate the fines and fees owed on a ticket, and if eligible, request either a reduction in their fines or a conversion to community service. This calculator can be used by defendants, attorneys, the court, and the general public.

 ## Data

The calculator relies on six sources of data: 

1. Offenses and their monetary penalties:  
      a. Hawaii Revised Statutes => State laws passed by the Legislative branch ([HRS](https://www.capitol.hawaii.gov/hrsall/))  
      b. Hawaii Administrative Rules => Rules established by the Executive branch ([HAR](https://ltgov.hawaii.gov/the-office/administrative-rules/))  
      c. Local Ordinances => Local laws passed by the City Councils (e.g., [ROH](https://www8.honolulu.gov/ocs/revised-ordinances-of-honolulu/))  
2. [2024 Federal poverty guidelines **for Hawaii**](https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines)
3. [2020 ALICE survival budget](https://www.unitedforalice.org/household-budgets/hawaii)

Most of the static fines and fees that populate the calculator are imported from the rds file in the repo. The remaining fines vary depending on the number of prior offenses, age of the defendant, and other factors, and these fines often have a minimum and maximum penalty, with the court ordering a fine within that range. The logic of the traffic code for these set of fines is reflected in the R code.

Most fines can be reduced or coverted to community service by the court. Most fees cannot.

## How it works

Following a Welcome and FAQ screen, the calculator proceeds in four steps.

### Step 1: Charges
1. The user indicates their role, the date of the offenses, and the name of the offenses.
![step 1](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/calculator_step1.JPG) 

### Step 2: Ability to Pay Form
2. The user selects whether they want to reduce their fines, convert them to community service, or skip ahead. If requesting a reduction or conversion, the user completes an "ability to pay" form that can be provided to the court to determine if the person is eligible.
![step 2](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/calculator_step2.JPG) 

### Step 3: Fines and Fees
3. The user indicates what they are able to pay.
![step 3](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/calculator_step3.JPG) 

### Step 4: Summary
4. The user reviews and prints out a summary of what they can pay, and follows the instructions describing how to use the information provided to make a request to the court for a fine reduction or community service conversion.
![step 4](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/calculator_step4.JPG) 

## Deployment note

If you attempt to run a copy of the shinyapp locally or remotely, you will need disable code that enable persistent storage on Dropbox (using ctrl-f and "dropbox persistent storage" should locate the relevant sections). Or you can create your own [Dropbox token](https://github.com/karthik/rdrop2) to keep persistent storage.
