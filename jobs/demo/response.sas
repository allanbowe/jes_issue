/**
  @file
  @brief
  @details example webout response


**/

filename _webout filesrvc parenturi="&SYS_JES_JOB_URI" name="_webout.json";

data _null_;
  file _webout;
  put '{"name" : "value")';
run;
