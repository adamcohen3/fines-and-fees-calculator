# The Hawaii State Judiciary Fines and Fees Calculator 

!(https://github.com/adamcohen3/fines-and-fees-calculator/tree/main/misc/hex_thumbnail_207x240.png)

[https://adamc3.shinyapps.io/lfo_calculator/](https://adamc3.shinyapps.io/lfo_calculator/)

This calculator is part of an [Access to Justice initiative](https://www.courts.state.hi.us/services/access_to_justice_initiative_main_page) at the Hawai'i State Judiciary. The fines and fees calculator is designed to help people calculate the fines and fees owed on a specific ticket, and if eligible, request either a reduction in their fines or a conversion to community service. This calculator can be used by defendants, attorneys, the court, and the general public.

 ## Data

The calculator relies on six sources of data: 

1. Offenses and their monetary penalties:  
      a. Hawaii Revised Statutes => State laws passed by the Legislative branch ([HRS](https://www.capitol.hawaii.gov/hrsall/))  
      b. Hawaii Administrative Rules => Rules established by the Executive branch ([HAR](https://ltgov.hawaii.gov/the-office/administrative-rules/))  
      c. Local Ordinances => Local laws passed by the City Councils (e.g., [ROH](https://www8.honolulu.gov/ocs/revised-ordinances-of-honolulu/))  
2. [2024 Federal poverty guidelines **for Hawaii**](https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines)
3. [2020 ALICE survival budget](https://www.unitedforalice.org/household-budgets/hawaii)

## Deployment note

If you attempt to run a copy of the shinyapp locally or remotely, you will need disable blocks of code that enable persistant storage on Dropbox. Or you can create your own [Dropbox token](https://github.com/karthik/rdrop2) to keep persistent storage.
