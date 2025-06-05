/**
  @file
  @brief calling JES API for response

  <h4> SAS Macros </h4>
  @li mf_getapploc.sas
  @li mf_getplatform.sas
  @li mfv_getpathuri.sas

**/

/* get app location */
%let jobloc=%mf_getapploc(&_program)/jobs/demo/response;

/* get uri of the job */
%put %mfv_getpathuri(&jobloc);

/* prepare the job code */
filename ft15f001 temp;
parmcards4;
{
  "name": "exec-startupservice",
  "description": "Powered by SASjs",
  "jobDefinition": {
    "creationTimeStamp": "2025-06-05T14:39:15.607Z",
    "modifiedTimeStamp": "2025-06-05T14:39:15.607Z",
    "createdBy": "allan@4gl.io",
    "modifiedBy": "allan@4gl.io",
    "version": 2,
    "id": "57477668-288d-4699-8f51-e85483f12851",
    "name": "startupservice",
    "type": "Compute",
    "parameters": [
        {
            "version": 1,
            "name": "_addjesbeginendmacros",
            "defaultValue": "false",
            "type": "CHARACTER",
            "required": false
        }
    ],
    "code": "filename _webout filesrvc parenturi=\"&SYS_JES_JOB_URI\" name=\"_webout.json\";data _null_;file _webout;put '{\"name\" : \"value\")';run;"
  },
  "arguments": {
    "_contextName": "SAS Job Execution Compute context",
    "_program": "$APPLOC",
    "_webin_file_count": 0,
    "_OMITJSONLISTING": false,
    "_OMITJSONLOG": false,
    "_OMITSESSIONRESULTS": false,
    "_OMITTEXTLISTING": false,
    "_OMITTEXTLOG": false,
    "_debug": 2477
  }
}
;;;;


/* swap out with currently deployed job location */
filename body temp;
data _null_;
  infile ft15f001;
  file body;
  input;
  length new $1000;
  if index(_infile_,'$APPLOC') then do;
    new=tranwrd(_infile_,'$APPLOC',"&jobloc");
    put new;
  end;
  else put _infile_;
run;

data _null_;
  infile body;
  input;
  putlog _infile_;
run;


options noquotelenmax;
%let base_uri=%mf_getplatform(VIYARESTAPI);
filename f1 temp;
proc http method='POST' in=body out=f1 oauth_bearer=sas_services
  url="&base_uri/jobExecution/jobs"
  ct="application/json";
  debug level=1;
  headers "Accept"="application/json";
run;

data _null_;
  infile f1;
  input; putlog _infile_;
run;
libname json1 JSON fileref=f1;
data _null_;
  set json1.links;
  putlog (_all_)(=);
  if rel='self' and method='GET' then do;
    call symputx('href',quote(cats("&base_uri",href)),'l');
  end;
run;

/* give the job a chance to finish */
%let rc=%sysfunc(sleep(5,1));

/* GET the results */
filename f2 temp;
proc http method='GET' in=body out=f2 oauth_bearer=sas_services
  url=&href;
  debug level=1;
  headers "Accept"="application/json";
run;
data _null_;
  infile f2;
  input; putlog _infile_;
run;
libname json2 JSON fileref=f2;
data results;
  set json2.results;
  putlog (_all_)(=);
run;


