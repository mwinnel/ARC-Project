<h1>ARC-Project</h1>


<h3>KEY FILES</h3>
<br>
<br>
The following files are the ones in use by the main R program that runs on a sensor unit.
This program has the main function of collecting data in Real time off the sensor units and processing this data for 
events. It also compresses the data using a Douglas algorithm - stores the data localy - streams live data to the server
- keeps a state system code - <br>

run.r  (main file startup script) <br>
<br>
calls - <br>
data_col.R <br>
start_spot.r<br> 
loop.R<br>

Function Files<br>
#-------------------------------------------------

functions.R<br>
functionsGeneric.r<br>
douglasFunction.R<br>

sendAlarm.R (called from loop.R)<br>

#------------------------------------------------
In testing mode files used are

runevents.R <br>
data_col.R<br>
loop2.R<br>

#---------------------------------

All other files found in this repository/directory are not key files 
but used to test some other component or module in the project.





