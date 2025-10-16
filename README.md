# ACE: ABAP Code Explorer

New Idea. I don't need debug to analyse code flow ). So it moves from [ABAP Script](https://github.com/ysichov/Smart-Debugger/blob/master/src/z_smart_debugger.prog.abap) to the usual program )

[Standalone version is here](https://github.com/ysichov/ACE/blob/main/src/z_ace_standalone.prog.abap)

 [Short video demo](https://www.loom.com/share/250574c07071496c8c9b064062bb44dc?sid=dee80fda-5f6d-4384-8457-c53ab97d09e8)

 [ABAP Mermaid](https://github.com/WegnerDan/abapMermaid) should be installed to draw diagrams 

<img width="892" height="897" alt="image" src="https://github.com/user-attachments/assets/de40d1f4-221d-4a0c-955d-61967522a318" />



Type in program, press enter - new window will be opened.


<img width="1908" height="981" alt="image" src="https://github.com/user-attachments/assets/3a23affd-276c-4886-b3ae-78b38e84563c" />

 
 It is multy windows. So You can open as many program as large your display ))).
 
<img width="1852" height="901" alt="image" src="https://github.com/user-attachments/assets/76270fab-3b75-4e1d-bec5-c525acca761b" />

**Update1:** Ask AI button added to chat with any AI model (Settings on the selection screen )

**Update2:** Programs/forms/functions/classes/methods and its parameters were added for navigation. 
 Double-click on a variable name is acting like set/unset watchpoint for Code Flow Smart Mixer. And for simple cases we don't need a debugger or Smart Debugger at all to catch the value origin of selected variable(s).

Watchpoints without debug!!! In programming theory it is called Data Dependency Analysis or Backward Program Slicing which builds a data dependency graph. Some programming languages have such Tools. And ABAP now also has )

Below on the screenshot variable EV_SAL(1) was choosen and CodeMix button generated code flow:
2 - EV_SAL need lv_income and lv_deduct to calc
3 - ev_deduct calculation
4 - lv_income calculation
5 - income need field 'rate' from the table zempl_rates.

Should resolve any depth and all that internal variables renaming like iv_ to ev_ and vice versa.

<img width="1910" height="918" alt="image" src="https://github.com/user-attachments/assets/1b2b5985-5ed0-49fc-845f-de46dccc2f5e" />





