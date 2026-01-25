My final goal is the following.
Imagine that:
- I have logged the state of data of all 01 records defined in the WORKING STORAGE and LINKAGE sections at the end of the execution of PARAGRAPH "100-PAR"
- I am interested in knowing if MY-VAR changes value may have changed during the execution of "100-PAR"

To achieve my goal, what I need to know is:
- which are the variables that, if changed, change MY-VAR
- see whether these variables may be changed during the execution of "100-PAR"
- if any of these variables or MY-VAR itself are in the set of variables that may change value during the execution of MY-VAR, identify the level 01 record that describes MY-VAR so that I take the logged data and fill with it the level 01 record definition so that I know the value of MY-VAR at the end of the execution of "100-PAR".

Tell me if there is any weakness in this reasoning and provide with the strategy you suggest to achieve this goal.