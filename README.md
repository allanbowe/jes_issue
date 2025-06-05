# JES Issue

This was a SASjs Project created to support / illustrate the issue described in the following post:

https://communities.sas.com/t5/SAS-Viya/Returning-webout-from-JES-API/td-p/966992

To deploy the example, run the following code (using your preferred SAS Drive deployment location in line 1):

```sas
%let apploc=/Public/jes_demo;
filename demo url
 "https://raw.githubusercontent.com/allanbowe/jes_issue/refs/heads/main/viya.sas";
%inc demo;
```

You can now open the last line of the log in a browser, and add "&_debug=2477" to get the log