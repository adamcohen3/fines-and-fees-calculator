# The Hawaii State Judiciary Fines and Fees Calculator

![hex thumbnail](https://raw.githubusercontent.com/adamcohen3/fines-and-fees-calculator/master/misc/hex_thumbnail_202x240.png) 

[https://adamc3.shinyapps.io/fines-and-fees-calculator/](https://adamc3.shinyapps.io/fines-and-fees-calculator/)    
<br>
The fines and fees calculator is part of an [Access to Justice initiative](https://www.courts.state.hi.us/services/access_to_justice_initiative_main_page) at the Hawai'i State Judiciary. The calculator is designed to help people calculate the fines and fees owed on a ticket, and if eligible, request either a reduction in their fines or a conversion to community service. This calculator can be used by defendants, attorneys, the court, and the general public.

 ## Data

The calculator relies on three sources of data for offenses and their monetary penalties:  
1. **Hawaii Revised Statutes**: State laws passed by the Legislative branch ([HRS](https://www.capitol.hawaii.gov/hrsall/))
2. **Hawaii Administrative Rules**: Rules established by the Executive branch ([HAR](https://ltgov.hawaii.gov/the-office/administrative-rules/))
3. **Local Ordinances**: Local laws passed by the City Councils (e.g., [ROH](https://www8.honolulu.gov/ocs/revised-ordinances-of-honolulu/))  

It also relies on two sources for cost of living, which can serve as a reference point in determining a defendant's ability to pay:
1. [2024 Federal poverty guidelines *for Hawaii*](https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines)
2. [2020 ALICE survival budget *for Hawaii*](https://www.unitedforalice.org/household-budgets/hawaii)

Most of the static fines and fees that populate the calculator are stored in the rds file at top level of the repo. The remaining fines vary depending on the number of prior offenses, age of the defendant, and other factors, or the court sets the fine between a minimum and maximum penalty determined by statute. These fines require user input, and their underlying logic is recreated in the R code.

As of 2024, the calculator primarily focuses on traffic offenses. Most fines can be reduced or converted to community service by the court. Most fees cannot.

## How it works

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

## Attribution/Mahalo

Mahalo to Flaticon for making available the image in the hex sticker:  
<a href="https://www.flaticon.com/free-icons/speeding-ticket" title="speeding ticket icons">Speeding ticket icons created by surang - Flaticon</a>
