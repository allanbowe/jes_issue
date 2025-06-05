

%global appLoc;
%let compiled_apploc=/Public/jes_demo;
%let appLoc=%sysfunc(coalescec(&appLoc,&compiled_apploc));




%let sasjs_clickmeservice=clickme;
%let syscc=0;
options ps=max nonotes nosgen nomprint nomlogic nosource2 nosource noquotelenmax;
/* user supplied build vars */


/* user supplied build vars end */
/* system macro dependencies for build process */



%macro mp_abort(mac=mp_abort.sas, type=, msg=, iftrue=%str(1=1)
  , errds=work.mp_abort_errds
  , mode=REGULAR
)/*/STORE SOURCE*/;

%global sysprocessmode sysprocessname sasjs_stpsrv_header_loc sasjsprocessmode;
%local fref fid i;

%if not(%eval(%unquote(&iftrue))) %then %return;

%put NOTE: ///  mp_abort macro executing //;
%if %length(&mac)>0 %then %put NOTE- called by &mac;
%put NOTE - &msg;

%if %symexist(_SYSINCLUDEFILEDEVICE)
/* abort cancel FILE does not restart outside the INCLUDE on Viya 3.5 */
and %superq(SYSPROCESSNAME) ne %str(Compute Server)
%then %do;
  %if "*&_SYSINCLUDEFILEDEVICE*" ne "**" %then %do;
    data &errds;
      iftrue='1=1';
      length mac $100 msg $5000;
      mac=symget('mac');
      msg=symget('msg');
    run;
    data _null_;
      abort cancel FILE;
    run;
    %return;
  %end;
%end;

/* Web App Context */
%if %symexist(_PROGRAM)
  or %superq(SYSPROCESSNAME) = %str(Compute Server)
  or &mode=INCLUDE
%then %do;
  options obs=max replace mprint;
  %if "%substr(&sysver,1,1)" ne "4" and "%substr(&sysver,1,1)" ne "5"
  %then %do;
    options nosyntaxcheck;
  %end;

  %if &mode=INCLUDE %then %do;
    %if %sysfunc(exist(&errds))=1 %then %do;
      data _null_;
        set &errds;
        call symputx('iftrue',iftrue,'l');
        call symputx('mac',mac,'l');
        call symputx('msg',msg,'l');
        putlog (_all_)(=);
      run;
      %if (&iftrue)=0 %then %return;
    %end;
    %else %do;
      %put &sysmacroname: No include errors found;
      %return;
    %end;
  %end;

  /* extract log errs / warns, if exist */
  %local logloc logline;
  %global logmsg; /* capture global messages */
  %if %symexist(SYSPRINTTOLOG) %then %let logloc=&SYSPRINTTOLOG;
  %else %let logloc=%qsysfunc(getoption(LOG));
  proc printto log=log;run;
  %let logline=0;
  %if %length(&logloc)>0 %then %do;
    data _null_;
      infile &logloc lrecl=5000;
      input; putlog _infile_;
      i=1;
      retain logonce 0;
      if (
          _infile_=:"%str(WARN)ING" or _infile_=:"%str(ERR)OR"
        ) and logonce=0 then
      do;
        call symputx('logline',_n_);
        logonce+1;
      end;
    run;
    /* capture log including lines BEFORE the err */
    %if &logline>0 %then %do;
      data _null_;
        infile &logloc lrecl=5000;
        input;
        i=1;
        stoploop=0;
        if _n_ ge &logline-15 and stoploop=0 then do until (i>22);
          call symputx('logmsg',catx('\n',symget('logmsg'),_infile_));
          input;
          i+1;
          stoploop=1;
        end;
        if stoploop=1 then stop;
      run;
    %end;
  %end;

  %if %symexist(SYS_JES_JOB_URI) %then %do;
    /* setup webout for Viya */
    options nobomfile;
    %if "X&SYS_JES_JOB_URI.X"="XX" %then %do;
        filename _webout temp lrecl=999999 mod;
    %end;
    %else %do;
      filename _webout filesrvc parenturi="&SYS_JES_JOB_URI"
        name="_webout.json" lrecl=999999 mod;
    %end;
  %end;
  %else %if %sysfunc(filename(fref,&sasjs_stpsrv_header_loc))=0 %then %do;
    options nobomfile;
    /* set up http header for SASjs Server */
    %let fid=%sysfunc(fopen(&fref,A));
    %if &fid=0 %then %do;
      %put %str(ERR)OR: %sysfunc(sysmsg());
      %return;
    %end;
    %let rc=%sysfunc(fput(&fid,%str(Content-Type: application/json)));
    %let rc=%sysfunc(fwrite(&fid));
    %let rc=%sysfunc(fclose(&fid));
    %let rc=%sysfunc(filename(&fref));
  %end;

  /* send response in SASjs JSON format */
  data _null_;
    file _webout mod lrecl=32000 encoding='utf-8';
    length msg syswarningtext syserrortext $32767 mode $10 ;
    sasdatetime=datetime();
    msg=symget('msg');
  %if &logline>0 %then %do;
    msg=cats(msg,'\n\nLog Extract:\n',symget('logmsg'));
  %end;
    /* escape the escapes */
    msg=tranwrd(msg,'\','\\');
    /* escape the quotes */
    msg=tranwrd(msg,'"','\"');
    /* ditch the CRLFs as chrome complains */
    msg=compress(msg,,'kw');
    /* quote without quoting the quotes (which are escaped instead) */
    msg=cats('"',msg,'"');
    if symexist('_debug') then debug=quote(trim(symget('_debug')));
    else debug='""';
    if symget('sasjsprocessmode')='Stored Program' then mode='SASJS';
    if mode ne 'SASJS' then put '>>weboutBEGIN<<';
    put '{"SYSDATE" : "' "&SYSDATE" '"';
    put ',"SYSTIME" : "' "&SYSTIME" '"';
    put ',"sasjsAbort" : [{';
    put ' "MSG":' msg ;
    put ' ,"MAC": "' "&mac" '"}]';
    put ",""SYSUSERID"" : ""&sysuserid"" ";
    put ',"_DEBUG":' debug ;
    if symexist('_metauser') then do;
      _METAUSER=quote(trim(symget('_METAUSER')));
      put ",""_METAUSER"": " _METAUSER;
      _METAPERSON=quote(trim(symget('_METAPERSON')));
      put ',"_METAPERSON": ' _METAPERSON;
    end;
    if symexist('SYS_JES_JOB_URI') then do;
      SYS_JES_JOB_URI=quote(trim(symget('SYS_JES_JOB_URI')));
      put ',"SYS_JES_JOB_URI": ' SYS_JES_JOB_URI;
    end;
    _PROGRAM=quote(trim(resolve(symget('_PROGRAM'))));
    put ',"_PROGRAM" : ' _PROGRAM ;
    put ",""SYSCC"" : ""&syscc"" ";
    syserrortext=cats(symget('syserrortext'));
    if findc(syserrortext,'"\'!!'0A0D09000E0F010210111A'x) then do;
      syserrortext='"'!!trim(
        prxchange('s/"/\\"/',-1,        /* double quote */
        prxchange('s/\x0A/\n/',-1,      /* new line */
        prxchange('s/\x0D/\r/',-1,      /* carriage return */
        prxchange('s/\x09/\\t/',-1,     /* tab */
        prxchange('s/\x00/\\u0000/',-1, /* NUL */
        prxchange('s/\x0E/\\u000E/',-1, /* SS  */
        prxchange('s/\x0F/\\u000F/',-1, /* SF  */
        prxchange('s/\x01/\\u0001/',-1, /* SOH */
        prxchange('s/\x02/\\u0002/',-1, /* STX */
        prxchange('s/\x10/\\u0010/',-1, /* DLE */
        prxchange('s/\x11/\\u0011/',-1, /* DC1 */
        prxchange('s/\x1A/\\u001A/',-1, /* SUB */
        prxchange('s/\\/\\\\/',-1,syserrortext)
      )))))))))))))!!'"';
    end;
    else syserrortext=cats('"',syserrortext,'"');
    put ',"SYSERRORTEXT" : ' syserrortext;
    put ",""SYSHOSTNAME"" : ""&syshostname"" ";
    put ",""SYSJOBID"" : ""&sysjobid"" ";
    put ",""SYSSCPL"" : ""&sysscpl"" ";
    put ",""SYSSITE"" : ""&syssite"" ";
    sysvlong=quote(trim(symget('sysvlong')));
    put ',"SYSVLONG" : ' sysvlong;
    syswarningtext=cats(symget('syswarningtext'));
    if findc(syswarningtext,'"\'!!'0A0D09000E0F010210111A'x) then do;
      syswarningtext='"'!!trim(
        prxchange('s/"/\\"/',-1,        /* double quote */
        prxchange('s/\x0A/\n/',-1,      /* new line */
        prxchange('s/\x0D/\r/',-1,      /* carriage return */
        prxchange('s/\x09/\\t/',-1,     /* tab */
        prxchange('s/\x00/\\u0000/',-1, /* NUL */
        prxchange('s/\x0E/\\u000E/',-1, /* SS  */
        prxchange('s/\x0F/\\u000F/',-1, /* SF  */
        prxchange('s/\x01/\\u0001/',-1, /* SOH */
        prxchange('s/\x02/\\u0002/',-1, /* STX */
        prxchange('s/\x10/\\u0010/',-1, /* DLE */
        prxchange('s/\x11/\\u0011/',-1, /* DC1 */
        prxchange('s/\x1A/\\u001A/',-1, /* SUB */
        prxchange('s/\\/\\\\/',-1,syswarningtext)
      )))))))))))))!!'"';
    end;
    else syswarningtext=cats('"',syswarningtext,'"');
    put ",""SYSWARNINGTEXT"" : " syswarningtext;
    put ',"END_DTTM" : "' "%sysfunc(datetime(),E8601DT26.6)" '" ';
    put "}" ;
    if mode ne 'SASJS' then put '>>weboutEND<<';
  run;

  %put _all_;

  %if "&sysprocessmode " = "SAS Stored Process Server " %then %do;
    data _null_;
      putlog 'stpsrvset program err and syscc';
      rc=stpsrvset('program error', 0);
      call symputx("syscc",0,"g");
    run;
    %if &sysscp=WIN
    and 1=0 /* deprecating this logic until we figure out a consistent abort */
    and "%substr(%str(&sysvlong         ),1,8)"="9.04.01M"
    and "%substr(%str(&sysvlong         ),9,1)">"5" %then %do;
      /* skip approach (below) does not work in windows m6+ envs */
      endsas;
    %end;
    %else %do;
      /**
        * endsas kills 9.4m3 deployments by orphaning multibridges.
        * Abort variants are ungraceful (non zero return code)
        * This approach lets SAS run silently until the end :-)
        * Caution - fails when called within a %include within a macro
        * Use mp_include() to handle this.
        */
      filename skip temp;
      data _null_;
        file skip;
        put '%macro skip();';
        comment '%mend skip; -> fix lint ';
        put '%macro skippy();';
        comment '%mend skippy; -> fix lint ';
      run;
      %inc skip;
    %end;
  %end;
  %else %if "&sysprocessmode " = "SAS Compute Server " %then %do;
    /* endsas kills the session making it harder to fetch results */
    data _null_;
      syswarningtext=symget('syswarningtext');
      syserrortext=symget('syserrortext');
      abort_msg=symget('msg');
      syscc=symget('syscc');
      sysuserid=symget('sysuserid');
      iftrue=symget('iftrue');
      put (_all_)(/=);
      call symputx('syscc',0);
      abort cancel nolist;
    run;
  %end;
  %else %do;
    %abort cancel;
  %end;
%end;
%else %do;
  %put _all_;
  %abort cancel;
%end;
%mend mp_abort;

/** @endcond */



%macro mf_getuniquefileref(prefix=_,maxtries=1000,lrecl=32767);
  %local rc fname;
  %if &prefix=0 %then %do;
    %let rc=%sysfunc(filename(fname,,temp,lrecl=&lrecl));
    %if &rc %then %put %sysfunc(sysmsg());
    &fname
  %end;
  %else %do;
    %local x len;
    %let len=%eval(8-%length(&prefix));
    %let x=0;
    %do x=0 %to &maxtries;
      %let fname=&prefix%substr(%sysfunc(ranuni(0)),3,&len);
      %if %sysfunc(fileref(&fname)) > 0 %then %do;
        %let rc=%sysfunc(filename(fname,,temp,lrecl=&lrecl));
        %if &rc %then %put %sysfunc(sysmsg());
        &fname
        %return;
      %end;
    %end;
    %put unable to find available fileref after &maxtries attempts;
  %end;
%mend mf_getuniquefileref;


%macro mf_getuniquelibref(prefix=mclib,maxtries=1000);
  %local x;

  %if ( %length(&prefix) gt 7 ) %then %do;
    %put %str(ERR)OR: The prefix parameter cannot exceed 7 characters.;
    0
    %return;
  %end;
  %else %if (%sysfunc(NVALID(&prefix,v7))=0) %then %do;
    %put %str(ERR)OR: Invalid prefix (&prefix);
    0
    %return;
  %end;

  /* Set maxtries equal to '10 to the power of [# unused characters] - 1' */
  %let maxtries=%eval(10**(8-%length(&prefix))-1);

  %do x = 0 %to &maxtries;
    %if %sysfunc(libref(&prefix&x)) ne 0 %then %do;
      &prefix&x
      %return;
    %end;
    %let x = %eval(&x + 1);
  %end;

  %put %str(ERR)OR: No usable libref in range &prefix.0-&maxtries;
  %put %str(ERR)OR- Try reducing the prefix or deleting some libraries!;
  0
%mend mf_getuniquelibref;


%macro mf_isblank(param
)/*/STORE SOURCE*/;

  %sysevalf(%superq(param)=,boolean)

%mend mf_isblank;


%macro mf_abort(mac=mf_abort.sas, msg=, iftrue=%str(1=1)
)/des='ungraceful abort' /*STORE SOURCE*/;

  %if not(%eval(%unquote(&iftrue))) %then %return;

  %put NOTE: ///  mf_abort macro executing //;
  %if %length(&mac)>0 %then %put NOTE- called by &mac;
  %put NOTE - &msg;

  %abort;

%mend mf_abort;

/** @endcond */



%macro mfv_getpathuri(filepath
)/*/STORE SOURCE*/;

  %mf_abort(
    iftrue=(&syscc ne 0),
    msg=Cannot enter &sysmacroname with syscc=&syscc
  )

  %local fref rc path name var /* var is used to avoid delete timing issue */;
  %let fref=%mf_getuniquefileref();
  %let name=%scan(&filepath,-1,/);
  %let path=%substr(&filepath,1,%length(&filepath)-%length(&name)-1);

  %if %sysfunc(filename(fref,,filesrvc,folderPath="&path" filename="&name"))=0
  %then %do;
    %let var=_FILESRVC_&fref._URI;
    %str(&&&var)
    %let rc=%sysfunc(filename(fref));
    %symdel &var;
  %end;
  %else %do;
    %put &sysmacroname: did not find &filepath;
    %let syscc=0;
  %end;

%mend mfv_getpathuri;


%macro mf_mval(var);
  %if %symexist(&var) %then %do;
    %superq(&var)
  %end;
%mend mf_mval;



%macro mf_trimstr(basestr,trimstr);
%local baselen trimlen trimval;

/* return if basestr is shorter than trimstr (or 0) */
%let baselen=%length(%superq(basestr));
%let trimlen=%length(%superq(trimstr));
%if &baselen < &trimlen or &baselen=0 %then %return;

/* obtain the characters from the end of basestr */
%let trimval=%qsubstr(%superq(basestr)
  ,%length(%superq(basestr))-&trimlen+1
  ,&trimlen);

/* compare and if matching, chop it off! */
%if %superq(basestr)=%superq(trimstr) %then %do;
  %return;
%end;
%else %if %superq(trimval)=%superq(trimstr) %then %do;
  %qsubstr(%superq(basestr),1,%length(%superq(basestr))-&trimlen)
%end;
%else %do;
  &basestr
%end;

%mend mf_trimstr;


%macro mf_getplatform(switch
)/*/STORE SOURCE*/;
%local a b c;
%if &switch.NONE=NONE %then %do;
  %if %symexist(sasjsprocessmode) %then %do;
    %if &sasjsprocessmode=Stored Program %then %do;
      SASJS
      %return;
    %end;
  %end;
  %if %symexist(sysprocessmode) %then %do;
    %if "&sysprocessmode"="SAS Object Server"
    or "&sysprocessmode"= "SAS Compute Server" %then %do;
        SASVIYA
    %end;
    %else %if "&sysprocessmode"="SAS Stored Process Server"
      or "&sysprocessmode"="SAS Workspace Server"
    %then %do;
      SASMETA
      %return;
    %end;
    %else %do;
      BASESAS
      %return;
    %end;
  %end;
  %else %if %symexist(_metaport) or %symexist(_metauser) %then %do;
    SASMETA
    %return;
  %end;
  %else %do;
    BASESAS
    %return;
  %end;
%end;
%else %if &switch=SASSTUDIO %then %do;
  /* return the version of SAS Studio else 0 */
  %if %mf_mval(_CLIENTAPP)=%str(SAS Studio) %then %do;
    %let a=%mf_mval(_CLIENTVERSION);
    %let b=%scan(&a,1,.);
    %if %eval(&b >2) %then %do;
      &b
    %end;
    %else 0;
  %end;
  %else 0;
%end;
%else %if &switch=VIYARESTAPI %then %do;
  %mf_trimstr(%sysfunc(getoption(servicesbaseurl)),/)
%end;
%mend mf_getplatform;



%macro mfv_existfolder(path
)/*/STORE SOURCE*/;

  %mf_abort(
    iftrue=(&syscc ne 0),
    msg=Cannot enter mfv_existfolder.sas with syscc=&syscc
  )

  %local fref rc;
  %let fref=%mf_getuniquefileref();

  %if %sysfunc(filename(fref,,filesrvc,folderPath="&path"))=0 %then %do;
    1
    %let rc=%sysfunc(filename(fref));
  %end;
  %else %do;
    0
    %let syscc=0;
  %end;

%mend mfv_existfolder;


%macro mv_createfolder(path=
    ,access_token_var=ACCESS_TOKEN
    ,grant_type=sas_services
    ,mdebug=0
    ,outds=_null_
  );
%local dbg;
%if &mdebug=1 %then %do;
  %put &sysmacroname entry vars:;
  %put _local_;
%end;
%else %let dbg=*;

%if %mfv_existfolder(&path)=1 %then %do;
  %&dbg.put &sysmacroname: &path already exists;
  data &outds;
    self_uri="%mfv_getpathuri(&path)";
    output;
    stop;
  run;
  %return;
%end;

%local oauth_bearer;
%if &grant_type=detect %then %do;
  %if %symexist(&access_token_var) %then %let grant_type=authorization_code;
  %else %let grant_type=sas_services;
%end;
%if &grant_type=sas_services %then %do;
  %let oauth_bearer=oauth_bearer=sas_services;
  %let &access_token_var=;
%end;

%mp_abort(iftrue=(&grant_type ne authorization_code and &grant_type ne password
    and &grant_type ne sas_services
  )
  ,mac=&sysmacroname
  ,msg=%str(Invalid value for grant_type: &grant_type)
)

%mp_abort(iftrue=(%mf_isblank(&path)=1)
  ,mac=&sysmacroname
  ,msg=%str(path value must be provided)
)
%mp_abort(iftrue=(%length(&path)=1)
  ,mac=&sysmacroname
  ,msg=%str(path value must be provided)
)

options noquotelenmax;

%local subfolder_cnt; /* determine the number of subfolders */
%let subfolder_cnt=%sysfunc(countw(&path,/));

%local base_uri; /* location of rest apis */
%let base_uri=%mf_getplatform(VIYARESTAPI);

%local href; /* resource address (none for root) */
%let href="&base_uri/folders/folders?parentFolderUri=/folders/folders/none";

%local x newpath subfolder;
%do x=1 %to &subfolder_cnt;
  %let subfolder=%scan(&path,&x,%str(/));
  %let newpath=&newpath/&subfolder;

  %local fname1;
  %let fname1=%mf_getuniquefileref();

  %put &sysmacroname checking to see if &newpath exists;
  proc http method='GET' out=&fname1 &oauth_bearer
      url="&base_uri/folders/folders/@item?path=&newpath";
  %if &grant_type=authorization_code %then %do;
      headers "Authorization"="Bearer &&&access_token_var";
  %end;
  run;
  %local libref1;
  %let libref1=%mf_getuniquelibref();
  libname &libref1 JSON fileref=&fname1;
  %mp_abort(
    iftrue=(
      &SYS_PROCHTTP_STATUS_CODE ne 200 and &SYS_PROCHTTP_STATUS_CODE ne 404
    )
    ,mac=&sysmacroname
    ,msg=%str(&SYS_PROCHTTP_STATUS_CODE &SYS_PROCHTTP_STATUS_PHRASE)
  )
  %if &mdebug=1 %then %do;
    %put &sysmacroname following check to see if &newpath exists:;
    %put _local_;
    data _null_;
      infile &fname1;
      input;
      putlog _infile_;
    run;
  %end;
  %if &SYS_PROCHTTP_STATUS_CODE=200 %then %do;
    %*put &sysmacroname &newpath exists so grab the follow on link ;
    data _null_;
      set &libref1..links;
      if rel='createChild' then
        call symputx('href',quote(cats("&base_uri",href)),'l');
    run;
  %end;
  %else %if &SYS_PROCHTTP_STATUS_CODE=404 %then %do;
    %put &sysmacroname &newpath not found - creating it now;
    %local fname2;
    %let fname2=%mf_getuniquefileref();
    data _null_;
      length json $1000;
      json=cats("'"
        ,'{"name":'
        ,quote(trim(symget('subfolder')))
        ,',"description":'
        ,quote("&subfolder, created by &sysmacroname")
        ,',"type":"folder"}'
        ,"'"
      );
      call symputx('json',json,'l');
    run;

    proc http method='POST'
        in=&json
        out=&fname2
        &oauth_bearer
        url=%unquote(%superq(href));
        headers
      %if &grant_type=authorization_code %then %do;
                "Authorization"="Bearer &&&access_token_var"
      %end;
                'Content-Type'='application/vnd.sas.content.folder+json'
                'Accept'='application/vnd.sas.content.folder+json';
    run;
    %if &SYS_PROCHTTP_STATUS_CODE ne 200 %then %do;
      %put &=SYS_PROCHTTP_STATUS_CODE &=SYS_PROCHTTP_STATUS_PHRASE;
    %end;
    %mp_abort(iftrue=(&SYS_PROCHTTP_STATUS_CODE ne 201)
      ,mac=&sysmacroname
      ,msg=%str(&SYS_PROCHTTP_STATUS_CODE &SYS_PROCHTTP_STATUS_PHRASE)
    )
    %local libref2;
    %let libref2=%mf_getuniquelibref();
    libname &libref2 JSON fileref=&fname2;
    %put &sysmacroname &newpath now created. Grabbing the follow on link ;
    data &outds;
      set &libref2..links;
      if rel='createChild' then do;
        call symputx('href',quote(cats("&base_uri",href)),'l');
        &dbg put (_all_)(=);
      end;
      if method='GET' and rel='self' then do;
        self_uri=uri;
        output;
      end;
      keep self_uri ;
    run;

    libname &libref2 clear;
    filename &fname2 clear;
  %end;
  filename &fname1 clear;
  libname &libref1 clear;
%end;
%mend mv_createfolder;


%macro mv_deletejes(path=
    ,name=
    ,access_token_var=ACCESS_TOKEN
    ,grant_type=sas_services
  );
%local oauth_bearer;
%if &grant_type=detect %then %do;
  %if %symexist(&access_token_var) %then %let grant_type=authorization_code;
  %else %let grant_type=sas_services;
%end;
%if &grant_type=sas_services %then %do;
    %let oauth_bearer=oauth_bearer=sas_services;
    %let &access_token_var=;
%end;

%mp_abort(iftrue=(&grant_type ne authorization_code and &grant_type ne password
    and &grant_type ne sas_services
  )
  ,mac=&sysmacroname
  ,msg=%str(Invalid value for grant_type: &grant_type)
)
%mp_abort(iftrue=(%mf_isblank(&path)=1)
  ,mac=&sysmacroname
  ,msg=%str(path value must be provided)
)
%mp_abort(iftrue=(%mf_isblank(&name)=1)
  ,mac=&sysmacroname
  ,msg=%str(name value must be provided)
)
%mp_abort(iftrue=(%length(&path)=1)
  ,mac=&sysmacroname
  ,msg=%str(path value must be provided)
)

options noquotelenmax;
%local base_uri; /* location of rest apis */
%let base_uri=%mf_getplatform(VIYARESTAPI);
/* fetch the members of the folder to get the uri */
%local fname1;
%let fname1=%mf_getuniquefileref();
proc http method='GET' out=&fname1 &oauth_bearer
  url="&base_uri/folders/folders/@item?path=&path";
%if &grant_type=authorization_code %then %do;
  headers "Authorization"="Bearer &&&access_token_var";
%end;
run;
%if &SYS_PROCHTTP_STATUS_CODE=404 %then %do;
  %put &sysmacroname: Folder &path NOT FOUND - nothing to delete!;
  %return;
%end;
%else %if &SYS_PROCHTTP_STATUS_CODE ne 200 %then %do;
  /*data _null_;infile &fname1;input;putlog _infile_;run;*/
  %mp_abort(mac=&sysmacroname
    ,msg=%str(&SYS_PROCHTTP_STATUS_CODE &SYS_PROCHTTP_STATUS_PHRASE)
  )
%end;

/* grab the follow on link */
%local libref1;
%let libref1=%mf_getuniquelibref();
libname &libref1 JSON fileref=&fname1;
data _null_;
  set &libref1..links;
  if rel='members' then call symputx('mref',quote("&base_uri"!!trim(href)),'l');
run;

/* get the children */
%local fname1a;
%let fname1a=%mf_getuniquefileref();
proc http method='GET' out=&fname1a &oauth_bearer
  url=%unquote(%superq(mref));
%if &grant_type=authorization_code %then %do;
  headers "Authorization"="Bearer &&&access_token_var";
%end;
run;
%if &SYS_PROCHTTP_STATUS_CODE ne 200 %then %do;
  %put &=sysmacroname &=SYS_PROCHTTP_STATUS_CODE &=SYS_PROCHTTP_STATUS_PHRASE;
%end;
%local libref1a;
%let libref1a=%mf_getuniquelibref();
libname &libref1a JSON fileref=&fname1a;
%local uri found;
%let found=0;
/* %put Getting object uri from &libref1a..items; */
data _null_;
  length contenttype name $1000;
  set &libref1a..items;
  if contenttype='jobDefinition' and upcase(name)="%upcase(&name)" then do;
    call symputx('uri',cats("&base_uri",uri),'l');
    call symputx('found',1,'l');
  end;
run;
%if &found=0 %then %do;
  %put NOTE:;%put NOTE- &sysmacroname: &path/&name NOT FOUND;%put NOTE- ;
  %return;
%end;
proc http method="DELETE" url="&uri" &oauth_bearer;
  headers
%if &grant_type=authorization_code %then %do;
      "Authorization"="Bearer &&&access_token_var"
%end;
      "Accept"="*/*";/**/
run;
%if &SYS_PROCHTTP_STATUS_CODE ne 204 %then %do;
  data _null_; infile &fname2; input; putlog _infile_;run;
  %mp_abort(mac=&sysmacroname
    ,msg=%str(&SYS_PROCHTTP_STATUS_CODE &SYS_PROCHTTP_STATUS_PHRASE)
  )
%end;
%else %put &sysmacroname: &path/&name deleted;

/* clear refs */
filename &fname1 clear;
libname &libref1 clear;
filename &fname1a clear;
libname &libref1a clear;

%mend mv_deletejes;
/* system macro dependencies for build process end*/
/* system macros for build process */


%macro mv_createwebservice(path=
    ,name=
    ,desc=Created by the mv_createwebservice.sas macro
    ,precode=
    ,code=ft15f001
    ,access_token_var=ACCESS_TOKEN
    ,grant_type=sas_services
    ,replace=YES
    ,adapter=sasjs
    ,mdebug=0
    ,contextname=
    ,debug=0 /* @TODO - Deprecate */
  );
%local dbg;
%if &mdebug=1 %then %do;
  %put &sysmacroname entry vars:;
  %put _local_;
%end;
%else %let dbg=*;

%local oauth_bearer;
%if &grant_type=detect %then %do;
  %if %symexist(&access_token_var) %then %let grant_type=authorization_code;
  %else %let grant_type=sas_services;
%end;
%if &grant_type=sas_services %then %do;
    %let oauth_bearer=oauth_bearer=sas_services;
    %let &access_token_var=;
%end;

/* initial validation checking */
%mp_abort(iftrue=(&grant_type ne authorization_code and &grant_type ne password
    and &grant_type ne sas_services
  )
  ,mac=&sysmacroname
  ,msg=%str(Invalid value for grant_type: &grant_type)
)
%mp_abort(iftrue=(%mf_isblank(&path)=1)
  ,mac=&sysmacroname
  ,msg=%str(path value must be provided)
)
%mp_abort(iftrue=(%length(&path)=1)
  ,mac=&sysmacroname
  ,msg=%str(path value must be provided)
)
%mp_abort(iftrue=(%mf_isblank(&name)=1)
  ,mac=&sysmacroname
  ,msg=%str(name value must be provided)
)

options noquotelenmax;

* remove any trailing slash ;
%if "%substr(&path,%length(&path),1)" = "/" %then
  %let path=%substr(&path,1,%length(&path)-1);

/* ensure folder exists */
%&dbg.put &sysmacroname: Path &path being checked / created;
%mv_createfolder(path=&path)

%local base_uri; /* location of rest apis */
%let base_uri=%mf_getplatform(VIYARESTAPI);

/* fetching folder details for provided path */
%local fname1;
%let fname1=%mf_getuniquefileref();
proc http method='GET' out=&fname1 &oauth_bearer
  url="&base_uri/folders/folders/@item?path=&path";
%if &grant_type=authorization_code %then %do;
  headers "Authorization"="Bearer &&&access_token_var";
%end;
run;
%if &mdebug=1 %then %do;
  data _null_;
    infile &fname1;
    input;
    putlog _infile_;
  run;
%end;
%mp_abort(iftrue=(&SYS_PROCHTTP_STATUS_CODE ne 200)
  ,mac=&sysmacroname
  ,msg=%str(&SYS_PROCHTTP_STATUS_CODE &SYS_PROCHTTP_STATUS_PHRASE)
)

/* path exists. Grab follow on link to check members */
%local libref1;
%let libref1=%mf_getuniquelibref();
libname &libref1 JSON fileref=&fname1;

data _null_;
  set &libref1..links;
  if rel='members' then
    call symputx('membercheck',quote("&base_uri"!!trim(href)),'l');
  else if rel='self' then call symputx('parentFolderUri',href,'l');
run;
data _null_;
  set &libref1..root;
  call symputx('folderid',id,'l');
run;
%local fname2;
%let fname2=%mf_getuniquefileref();
proc http method='GET'
    out=&fname2
    &oauth_bearer
    url=%unquote(%superq(membercheck));
    headers
  %if &grant_type=authorization_code %then %do;
            "Authorization"="Bearer &&&access_token_var"
  %end;
            'Accept'='application/vnd.sas.collection+json'
            'Accept-Language'='string';
%if &mdebug=1 %then %do;
  debug level = 3;
%end;
run;
/*data _null_;infile &fname2;input;putlog _infile_;run;*/
%mp_abort(iftrue=(&SYS_PROCHTTP_STATUS_CODE ne 200)
  ,mac=&sysmacroname
  ,msg=%str(&SYS_PROCHTTP_STATUS_CODE &SYS_PROCHTTP_STATUS_PHRASE)
)

%if %upcase(&replace)=YES %then %do;
  %mv_deletejes(path=&path, name=&name)
%end;
%else %do;
  /* check that job does not already exist in that folder */
  %local libref2;
  %let libref2=%mf_getuniquelibref();
  libname &libref2 JSON fileref=&fname2;
  %local exists; %let exists=0;
  data _null_;
    set &libref2..items;
    if contenttype='jobDefinition' and upcase(name)="%upcase(&name)" then
      call symputx('exists',1,'l');
  run;
  %mp_abort(iftrue=(&exists=1)
    ,mac=&sysmacroname
    ,msg=%str(Job &name already exists in &path)
  )
  libname &libref2 clear;
%end;

/* set up the body of the request to create the service */
%local fname3;
%let fname3=%mf_getuniquefileref();
data _null_;
  file &fname3 TERMSTR=' ';
  length string $32767;
  string=cats('{"version": 0,"name":"'
    ,"&name"
    ,'","type":"Compute","parameters":[{"name":"_addjesbeginendmacros"'
    ,',"type":"CHARACTER","defaultValue":"false"}');
  context=quote(cats(symget('contextname')));
  if context ne '""' then do;
    string=cats(string,',{"version": 1,"name": "_contextName","defaultValue":'
      ,context,',"type":"CHARACTER","label":"Context Name","required": false}');
  end;
  string=cats(string,'],"code":"');
  put string;
run;

/**
  * Add webout macro
  * These put statements are auto generated - to change the macro, change the
  * source (mv_webout) and run `build.py`
  */
filename &adapter temp lrecl=3000;
data _null_;
  file &adapter;
  put "/* Created on %sysfunc(datetime(),datetime19.) by &sysuserid */";
/* WEBOUT BEGIN */
  put '%macro mp_jsonout(action,ds,jref=_webout,dslabel=,fmt=Y ';
  put '  ,engine=DATASTEP ';
  put '  ,missing=NULL ';
  put '  ,showmeta=N ';
  put '  ,maxobs=MAX ';
  put ')/*/STORE SOURCE*/; ';
  put '%local tempds colinfo fmtds i numcols numobs stmt_obs lastobs optval ';
  put '  tmpds1 tmpds2 tmpds3 tmpds4; ';
  put '%let numcols=0; ';
  put '%if &maxobs ne MAX %then %let stmt_obs=%str(if _n_>&maxobs then stop;); ';
  put ' ';
  put '%if &action=OPEN %then %do; ';
  put '  options nobomfile; ';
  put '  data _null_;file &jref encoding=''utf-8'' lrecl=200; ';
  put '    put ''{"PROCESSED_DTTM" : "'' "%sysfunc(datetime(),E8601DT26.6)" ''"''; ';
  put '  run; ';
  put '%end; ';
  put '%else %if (&action=ARR or &action=OBJ) %then %do; ';
  put '  /* force variable names to always be uppercase in the JSON */ ';
  put '  options validvarname=upcase; ';
  put '  /* To avoid issues with _webout on EBI - such as encoding diffs and truncation ';
  put '    (https://support.sas.com/kb/49/325.html) we use temporary files */ ';
  put '  filename _sjs1 temp lrecl=200 ; ';
  put '  data _null_; file _sjs1 encoding=''utf-8''; ';
  put '    put ", ""%lowcase(%sysfunc(coalescec(&dslabel,&ds)))"":"; ';
  put '  run; ';
  put '  /* now write to _webout 1 char at a time */ ';
  put '  data _null_; ';
  put '    infile _sjs1 lrecl=1 recfm=n; ';
  put '    file &jref mod lrecl=1 recfm=n; ';
  put '    input sourcechar $char1. @@; ';
  put '    format sourcechar hex2.; ';
  put '    put sourcechar char1. @@; ';
  put '  run; ';
  put '  filename _sjs1 clear; ';
  put ' ';
  put '  /* grab col defs */ ';
  put '  proc contents noprint data=&ds ';
  put '    out=_data_(keep=name type length format formatl formatd varnum label); ';
  put '  run; ';
  put '  %let colinfo=%scan(&syslast,2,.); ';
  put '  proc sort data=&colinfo; ';
  put '    by varnum; ';
  put '  run; ';
  put '  /* move meta to mac vars */ ';
  put '  data &colinfo; ';
  put '    if _n_=1 then call symputx(''numcols'',nobs,''l''); ';
  put '    set &colinfo end=last nobs=nobs; ';
  put '    name=upcase(name); ';
  put '    /* fix formats */ ';
  put '    if type=2 or type=6 then do; ';
  put '      typelong=''char''; ';
  put '      length fmt $49.; ';
  put '      if format='''' then fmt=cats(''$'',length,''.''); ';
  put '      else if formatl=0 then fmt=cats(format,''.''); ';
  put '      else fmt=cats(format,formatl,''.''); ';
  put '    end; ';
  put '    else do; ';
  put '      typelong=''num''; ';
  put '      if format='''' then fmt=''best.''; ';
  put '      else if formatl=0 then fmt=cats(format,''.''); ';
  put '      else if formatd=0 then fmt=cats(format,formatl,''.''); ';
  put '      else fmt=cats(format,formatl,''.'',formatd); ';
  put '    end; ';
  put '    /* 32 char unique name */ ';
  put '    newname=''sasjs''!!substr(cats(put(md5(name),$hex32.)),1,27); ';
  put ' ';
  put '    call symputx(cats(''name'',_n_),name,''l''); ';
  put '    call symputx(cats(''newname'',_n_),newname,''l''); ';
  put '    call symputx(cats(''length'',_n_),length,''l''); ';
  put '    call symputx(cats(''fmt'',_n_),fmt,''l''); ';
  put '    call symputx(cats(''type'',_n_),type,''l''); ';
  put '    call symputx(cats(''typelong'',_n_),typelong,''l''); ';
  put '    call symputx(cats(''label'',_n_),coalescec(label,name),''l''); ';
  put '    /* overwritten when fmt=Y and a custom format exists in catalog */ ';
  put '    if typelong=''num'' then call symputx(cats(''fmtlen'',_n_),200,''l''); ';
  put '    else call symputx(cats(''fmtlen'',_n_),min(32767,ceil((length+10)*1.5)),''l''); ';
  put '  run; ';
  put ' ';
  put '  %let tempds=%substr(_%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32); ';
  put '  proc sql; ';
  put '  select count(*) into: lastobs from &ds; ';
  put '  %if &maxobs ne MAX %then %let lastobs=%sysfunc(min(&lastobs,&maxobs)); ';
  put ' ';
  put '  %if &engine=PROCJSON %then %do; ';
  put '    %if &missing=STRING %then %do; ';
  put '      %put &sysmacroname: Special Missings not supported in proc json.; ';
  put '      %put &sysmacroname: Switching to DATASTEP engine; ';
  put '      %goto datastep; ';
  put '    %end; ';
  put '    data &tempds; ';
  put '      set &ds; ';
  put '      &stmt_obs; ';
  put '    %if &fmt=N %then format _numeric_ best32.;; ';
  put '    /* PRETTY is necessary to avoid line truncation in large files */ ';
  put '    filename _sjs2 temp lrecl=131068 encoding=''utf-8''; ';
  put '    proc json out=_sjs2 pretty ';
  put '        %if &action=ARR %then nokeys ; ';
  put '        ;export &tempds / nosastags fmtnumeric; ';
  put '    run; ';
  put '    /* send back to webout */ ';
  put '    data _null_; ';
  put '      infile _sjs2 lrecl=1 recfm=n; ';
  put '      file &jref mod lrecl=1 recfm=n; ';
  put '      input sourcechar $char1. @@; ';
  put '      format sourcechar hex2.; ';
  put '      put sourcechar char1. @@; ';
  put '    run; ';
  put '    filename _sjs2 clear; ';
  put '  %end; ';
  put '  %else %if &engine=DATASTEP %then %do; ';
  put '    %datastep: ';
  put '    %if %sysfunc(exist(&ds)) ne 1 & %sysfunc(exist(&ds,VIEW)) ne 1 ';
  put '    %then %do; ';
  put '      %put &sysmacroname:  &ds NOT FOUND!!!; ';
  put '      %return; ';
  put '    %end; ';
  put ' ';
  put '    %if &fmt=Y %then %do; ';
  put '      /** ';
  put '        * Extract format definitions ';
  put '        * First, by getting library locations from dictionary.formats ';
  put '        * Then, by exporting the width using proc format ';
  put '        * Cannot use maxw from sashelp.vformat as not always populated ';
  put '        * Cannot use fmtinfo() as not supported in all flavours ';
  put '        */ ';
  put '      %let tmpds1=%substr(fmtsum%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32); ';
  put '      %let tmpds2=%substr(cntl%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32); ';
  put '      %let tmpds3=%substr(cntl%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32); ';
  put '      %let tmpds4=%substr(col%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32); ';
  put '      proc sql noprint; ';
  put '      create table &tmpds1 as ';
  put '          select cats(libname,''.'',memname) as FMTCAT, ';
  put '          FMTNAME ';
  put '        from dictionary.formats ';
  put '        where fmttype=''F'' and libname is not null ';
  put '          and fmtname in (select format from &colinfo where format is not null) ';
  put '        order by 1; ';
  put '      create table &tmpds2( ';
  put '          FMTNAME char(32), ';
  put '          LENGTH num ';
  put '      ); ';
  put '      %local catlist cat fmtlist i; ';
  put '      select distinct fmtcat into: catlist separated by '' '' from &tmpds1; ';
  put '      %do i=1 %to %sysfunc(countw(&catlist,%str( ))); ';
  put '        %let cat=%scan(&catlist,&i,%str( )); ';
  put '        proc sql; ';
  put '        select distinct fmtname into: fmtlist separated by '' '' ';
  put '          from &tmpds1 where fmtcat="&cat"; ';
  put '        proc format lib=&cat cntlout=&tmpds3(keep=fmtname length); ';
  put '          select &fmtlist; ';
  put '        run; ';
  put '        proc sql; ';
  put '        insert into &tmpds2 select distinct fmtname,length from &tmpds3; ';
  put '      %end; ';
  put ' ';
  put '      proc sql; ';
  put '      create table &tmpds4 as ';
  put '        select a.*, b.length as MAXW ';
  put '        from &colinfo a ';
  put '        left join &tmpds2 b ';
  put '        on cats(a.format)=cats(upcase(b.fmtname)) ';
  put '        order by a.varnum; ';
  put '      data _null_; ';
  put '        set &tmpds4; ';
  put '        if not missing(maxw); ';
  put '        call symputx( ';
  put '          cats(''fmtlen'',_n_), ';
  put '          /* vars need extra padding due to JSON escaping of special chars */ ';
  put '          min(32767,ceil((max(length,maxw)+10)*1.5)) ';
  put '          ,''l'' ';
  put '        ); ';
  put '      run; ';
  put ' ';
  put '      /* configure varlenchk - as we are explicitly shortening the variables */ ';
  put '      %let optval=%sysfunc(getoption(varlenchk)); ';
  put '      options varlenchk=NOWARN; ';
  put '      data _data_(compress=char); ';
  put '        /* shorten the new vars */ ';
  put '        length ';
  put '      %do i=1 %to &numcols; ';
  put '          &&name&i $&&fmtlen&i ';
  put '      %end; ';
  put '          ; ';
  put '        /* rename on entry */ ';
  put '        set &ds(rename=( ';
  put '      %do i=1 %to &numcols; ';
  put '          &&name&i=&&newname&i ';
  put '      %end; ';
  put '        )); ';
  put '      &stmt_obs; ';
  put ' ';
  put '      drop ';
  put '      %do i=1 %to &numcols; ';
  put '        &&newname&i ';
  put '      %end; ';
  put '        ; ';
  put '      %do i=1 %to &numcols; ';
  put '        %if &&typelong&i=num %then %do; ';
  put '          &&name&i=cats(put(&&newname&i,&&fmt&i)); ';
  put '        %end; ';
  put '        %else %do; ';
  put '          &&name&i=put(&&newname&i,&&fmt&i); ';
  put '        %end; ';
  put '      %end; ';
  put '        if _error_ then do; ';
  put '          call symputx(''syscc'',1012); ';
  put '          stop; ';
  put '        end; ';
  put '      run; ';
  put '      %let fmtds=&syslast; ';
  put '      options varlenchk=&optval; ';
  put '    %end; ';
  put ' ';
  put '    proc format; /* credit yabwon for special null removal */ ';
  put '    value bart (default=40) ';
  put '    %if &missing=NULL %then %do; ';
  put '      ._ - .z = null ';
  put '    %end; ';
  put '    %else %do; ';
  put '      ._ = [quote()] ';
  put '      . = null ';
  put '      .a - .z = [quote()] ';
  put '    %end; ';
  put '      other = [best.]; ';
  put ' ';
  put '    data &tempds; ';
  put '      attrib _all_ label=''''; ';
  put '      %do i=1 %to &numcols; ';
  put '        %if &&typelong&i=char or &fmt=Y %then %do; ';
  put '          length &&name&i $&&fmtlen&i...; ';
  put '          format &&name&i $&&fmtlen&i...; ';
  put '        %end; ';
  put '      %end; ';
  put '      %if &fmt=Y %then %do; ';
  put '        set &fmtds; ';
  put '      %end; ';
  put '      %else %do; ';
  put '        set &ds; ';
  put '      %end; ';
  put '      &stmt_obs; ';
  put '      format _numeric_ bart.; ';
  put '    %do i=1 %to &numcols; ';
  put '      %if &&typelong&i=char or &fmt=Y %then %do; ';
  put '        if findc(&&name&i,''"\''!!''0A0D09000E0F010210111A''x) then do; ';
  put '          &&name&i=''"''!!trim( ';
  put '            prxchange(''s/"/\\"/'',-1,        /* double quote */ ';
  put '            prxchange(''s/\x0A/\n/'',-1,      /* new line */ ';
  put '            prxchange(''s/\x0D/\r/'',-1,      /* carriage return */ ';
  put '            prxchange(''s/\x09/\\t/'',-1,     /* tab */ ';
  put '            prxchange(''s/\x00/\\u0000/'',-1, /* NUL */ ';
  put '            prxchange(''s/\x0E/\\u000E/'',-1, /* SS  */ ';
  put '            prxchange(''s/\x0F/\\u000F/'',-1, /* SF  */ ';
  put '            prxchange(''s/\x01/\\u0001/'',-1, /* SOH */ ';
  put '            prxchange(''s/\x02/\\u0002/'',-1, /* STX */ ';
  put '            prxchange(''s/\x10/\\u0010/'',-1, /* DLE */ ';
  put '            prxchange(''s/\x11/\\u0011/'',-1, /* DC1 */ ';
  put '            prxchange(''s/\x1A/\\u001A/'',-1, /* SUB */ ';
  put '            prxchange(''s/\\/\\\\/'',-1,&&name&i) ';
  put '          )))))))))))))!!''"''; ';
  put '        end; ';
  put '        else &&name&i=quote(cats(&&name&i)); ';
  put '      %end; ';
  put '    %end; ';
  put '    run; ';
  put ' ';
  put '    filename _sjs3 temp lrecl=131068 ; ';
  put '    data _null_; ';
  put '      file _sjs3 encoding=''utf-8''; ';
  put '      if _n_=1 then put "["; ';
  put '      set &tempds; ';
  put '      if _n_>1 then put "," @; put ';
  put '      %if &action=ARR %then "[" ; %else "{" ; ';
  put '      %do i=1 %to &numcols; ';
  put '        %if &i>1 %then  "," ; ';
  put '        %if &action=OBJ %then """&&name&i"":" ; ';
  put '        "&&name&i"n /* name literal for reserved variable names */ ';
  put '      %end; ';
  put '      %if &action=ARR %then "]" ; %else "}" ; ; ';
  put ' ';
  put '    /* close out the table */ ';
  put '    data _null_; ';
  put '      file _sjs3 mod encoding=''utf-8''; ';
  put '      put '']''; ';
  put '    run; ';
  put '    data _null_; ';
  put '      infile _sjs3 lrecl=1 recfm=n; ';
  put '      file &jref mod lrecl=1 recfm=n; ';
  put '      input sourcechar $char1. @@; ';
  put '      format sourcechar hex2.; ';
  put '      put sourcechar char1. @@; ';
  put '    run; ';
  put '    filename _sjs3 clear; ';
  put '  %end; ';
  put ' ';
  put '  proc sql; ';
  put '  drop table &colinfo, &tempds; ';
  put ' ';
  put '  %if %substr(&showmeta,1,1)=Y %then %do; ';
  put '    filename _sjs4 temp lrecl=131068 encoding=''utf-8''; ';
  put '    data _null_; ';
  put '      file _sjs4; ';
  put '      length label $350; ';
  put '      put ", ""$%lowcase(%sysfunc(coalescec(&dslabel,&ds)))"":{""vars"":{"; ';
  put '      do i=1 to &numcols; ';
  put '        name=quote(trim(symget(cats(''name'',i)))); ';
  put '        format=quote(trim(symget(cats(''fmt'',i)))); ';
  put '        label=quote(prxchange(''s/\\/\\\\/'',-1,trim(symget(cats(''label'',i))))); ';
  put '        length=quote(trim(symget(cats(''length'',i)))); ';
  put '        type=quote(trim(symget(cats(''typelong'',i)))); ';
  put '        if i>1 then put "," @@; ';
  put '        put name '':{"format":'' format '',"label":'' label ';
  put '          '',"length":'' length '',"type":'' type ''}''; ';
  put '      end; ';
  put '      put ''}}''; ';
  put '    run; ';
  put '    /* send back to webout */ ';
  put '    data _null_; ';
  put '      infile _sjs4 lrecl=1 recfm=n; ';
  put '      file &jref mod lrecl=1 recfm=n; ';
  put '      input sourcechar $char1. @@; ';
  put '      format sourcechar hex2.; ';
  put '      put sourcechar char1. @@; ';
  put '    run; ';
  put '    filename _sjs4 clear; ';
  put '  %end; ';
  put '%end; ';
  put ' ';
  put '%else %if &action=CLOSE %then %do; ';
  put '  data _null_; file &jref encoding=''utf-8'' mod ; ';
  put '    put "}"; ';
  put '  run; ';
  put '%end; ';
  put '%mend mp_jsonout; ';
  put ' ';
  put '%macro mf_getuser( ';
  put ')/*/STORE SOURCE*/; ';
  put '  %local user; ';
  put ' ';
  put '  %if %symexist(_sasjs_username) %then %let user=&_sasjs_username; ';
  put '  %else %if %symexist(SYS_COMPUTE_SESSION_OWNER) %then %do; ';
  put '    %let user=&SYS_COMPUTE_SESSION_OWNER; ';
  put '  %end; ';
  put '  %else %if %symexist(_metaperson) %then %do; ';
  put '    %if %length(&_metaperson)=0 %then %let user=&sysuserid; ';
  put '    /* sometimes SAS will add @domain extension - remove for consistency */ ';
  put '    /* but be sure to quote in case of usernames with commas */ ';
  put '    %else %let user=%unquote(%scan(%quote(&_metaperson),1,@)); ';
  put '  %end; ';
  put '  %else %let user=&sysuserid; ';
  put ' ';
  put '  %quote(&user) ';
  put ' ';
  put '%mend mf_getuser; ';
  put '%macro mv_webout(action,ds,fref=_mvwtemp,dslabel=,fmt=N,stream=Y,missing=NULL ';
  put '  ,showmeta=N,maxobs=MAX,workobs=0 ';
  put '); ';
  put '%global _webin_file_count _webin_fileuri _debug _omittextlog _webin_name ';
  put '  sasjs_tables SYS_JES_JOB_URI; ';
  put '%if %index("&_debug",log) %then %let _debug=131; ';
  put ' ';
  put '%local i tempds table; ';
  put '%let action=%upcase(&action); ';
  put ' ';
  put '%if &action=FETCH %then %do; ';
  put '  %if %upcase(&_omittextlog)=FALSE or %str(&_debug) ge 131 %then %do; ';
  put '    options mprint notes mprintnest; ';
  put '  %end; ';
  put ' ';
  put '  %if not %symexist(_webin_fileuri1) %then %do; ';
  put '    %let _webin_file_count=%eval(&_webin_file_count+0); ';
  put '    %let _webin_fileuri1=&_webin_fileuri; ';
  put '    %let _webin_name1=&_webin_name; ';
  put '  %end; ';
  put ' ';
  put '  /* if the sasjs_tables param is passed, we expect param based upload */ ';
  put '  %if %length(&sasjs_tables.X)>1 %then %do; ';
  put ' ';
  put '    /* convert data from macro variables to datasets */ ';
  put '    %do i=1 %to %sysfunc(countw(&sasjs_tables)); ';
  put '      %let table=%scan(&sasjs_tables,&i,%str( )); ';
  put '      %if %symexist(sasjs&i.data0)=0 %then %let sasjs&i.data0=1; ';
  put '      data _null_; ';
  put '        file "%sysfunc(pathname(work))/&table..csv" recfm=n; ';
  put '        retain nrflg 0; ';
  put '        length line $32767; ';
  put '        do i=1 to &&sasjs&i.data0; ';
  put '          if &&sasjs&i.data0=1 then line=symget("sasjs&i.data"); ';
  put '          else line=symget(cats("sasjs&i.data",i)); ';
  put '          if i=1 and substr(line,1,7)=''%nrstr('' then do; ';
  put '            nrflg=1; ';
  put '            line=substr(line,8); ';
  put '          end; ';
  put '          if i=&&sasjs&i.data0 and nrflg=1 then do; ';
  put '            line=substr(line,1,length(line)-1); ';
  put '          end; ';
  put '          put line +(-1) @; ';
  put '        end; ';
  put '      run; ';
  put '      data _null_; ';
  put '        infile "%sysfunc(pathname(work))/&table..csv" termstr=crlf ; ';
  put '        input; ';
  put '        if _n_=1 then call symputx(''input_statement'',_infile_); ';
  put '        list; ';
  put '      data work.&table; ';
  put '        infile "%sysfunc(pathname(work))/&table..csv" firstobs=2 dsd ';
  put '          termstr=crlf; ';
  put '        input &input_statement; ';
  put '      run; ';
  put '    %end; ';
  put '  %end; ';
  put '  %else %do i=1 %to &_webin_file_count; ';
  put '    /* read in any files that are sent */ ';
  put '    /* this part needs refactoring for wide files */ ';
  put '    filename indata filesrvc "&&_webin_fileuri&i" lrecl=999999; ';
  put '    data _null_; ';
  put '      infile indata termstr=crlf lrecl=32767; ';
  put '      input; ';
  put '      if _n_=1 then call symputx(''input_statement'',_infile_); ';
  put '      %if %str(&_debug) ge 131 %then %do; ';
  put '        if _n_<20 then putlog _infile_; ';
  put '        else stop; ';
  put '      %end; ';
  put '      %else %do; ';
  put '        stop; ';
  put '      %end; ';
  put '    run; ';
  put '    data &&_webin_name&i; ';
  put '      infile indata firstobs=2 dsd termstr=crlf ; ';
  put '      input &input_statement; ';
  put '    run; ';
  put '    %let sasjs_tables=&sasjs_tables &&_webin_name&i; ';
  put '  %end; ';
  put '%end; ';
  put '%else %if &action=OPEN %then %do; ';
  put '  /* setup webout */ ';
  put '  OPTIONS NOBOMFILE; ';
  put '  %if "X&SYS_JES_JOB_URI.X"="XX" %then %do; ';
  put '    filename _webout temp lrecl=999999 mod; ';
  put '  %end; ';
  put '  %else %do; ';
  put '    filename _webout filesrvc parenturi="&SYS_JES_JOB_URI" ';
  put '      name="_webout.json" lrecl=999999 mod; ';
  put '  %end; ';
  put ' ';
  put '  /* setup temp ref */ ';
  put '  %if %upcase(&fref) ne _WEBOUT %then %do; ';
  put '    filename &fref temp lrecl=999999 permission=''A::u::rwx,A::g::rw-,A::o::---''; ';
  put '  %end; ';
  put ' ';
  put '  /* setup json */ ';
  put '  data _null_;file &fref; ';
  put '    put ''{"SYSDATE" : "'' "&SYSDATE" ''"''; ';
  put '    put '',"SYSTIME" : "'' "&SYSTIME" ''"''; ';
  put '  run; ';
  put '%end; ';
  put '%else %if &action=ARR or &action=OBJ %then %do; ';
  put '    %mp_jsonout(&action,&ds,dslabel=&dslabel,fmt=&fmt,jref=&fref ';
  put '      ,engine=DATASTEP,missing=&missing,showmeta=&showmeta,maxobs=&maxobs ';
  put '    ) ';
  put '%end; ';
  put '%else %if &action=CLOSE %then %do; ';
  put '  %if %str(&workobs) > 0 %then %do; ';
  put '    /* send back first XX records of each work table for debugging */ ';
  put '    data;run;%let tempds=%scan(&syslast,2,.); ';
  put '    ods output Members=&tempds; ';
  put '    proc datasets library=WORK memtype=data; ';
  put '    %local wtcnt;%let wtcnt=0; ';
  put '    data _null_; ';
  put '      set &tempds; ';
  put '      if not (upcase(name) =:"DATA"); /* ignore temp datasets */ ';
  put '      i+1; ';
  put '      call symputx(cats(''wt'',i),name,''l''); ';
  put '      call symputx(''wtcnt'',i,''l''); ';
  put '    data _null_; file &fref mod; put ",""WORK"":{"; ';
  put '    %do i=1 %to &wtcnt; ';
  put '      %let wt=&&wt&i; ';
  put '      data _null_; file &fref mod; ';
  put '        dsid=open("WORK.&wt",''is''); ';
  put '        nlobs=attrn(dsid,''NLOBS''); ';
  put '        nvars=attrn(dsid,''NVARS''); ';
  put '        rc=close(dsid); ';
  put '        if &i>1 then put '',''@; ';
  put '        put " ""&wt"" : {"; ';
  put '        put ''"nlobs":'' nlobs; ';
  put '        put '',"nvars":'' nvars; ';
  put '      %mp_jsonout(OBJ,&wt,jref=&fref,dslabel=first10rows,showmeta=Y ';
  put '        ,maxobs=&workobs ';
  put '      ) ';
  put '      data _null_; file &fref mod;put "}"; ';
  put '    %end; ';
  put '    data _null_; file &fref mod;put "}";run; ';
  put '  %end; ';
  put ' ';
  put '  /* close off json */ ';
  put '  data _null_;file &fref mod; ';
  put '    length SYSPROCESSNAME syserrortext syswarningtext autoexec $512; ';
  put '    put ",""_DEBUG"" : ""&_debug"" "; ';
  put '    _PROGRAM=quote(trim(resolve(symget(''_PROGRAM'')))); ';
  put '    put '',"_PROGRAM" : '' _PROGRAM ; ';
  put '    autoexec=quote(urlencode(trim(getoption(''autoexec'')))); ';
  put '    put '',"AUTOEXEC" : '' autoexec; ';
  put '    put ",""MF_GETUSER"" : ""%mf_getuser()"" "; ';
  put '    SYS_JES_JOB_URI=quote(trim(resolve(symget(''SYS_JES_JOB_URI'')))); ';
  put '    put '',"SYS_JES_JOB_URI" : '' SYS_JES_JOB_URI ; ';
  put '    put ",""SYSJOBID"" : ""&sysjobid"" "; ';
  put '    put ",""SYSCC"" : ""&syscc"" "; ';
  put '    syserrortext=cats(symget(''syserrortext'')); ';
  put '    if findc(syserrortext,''"\''!!''0A0D09000E0F010210111A''x) then do; ';
  put '      syserrortext=''"''!!trim( ';
  put '        prxchange(''s/"/\\"/'',-1,        /* double quote */ ';
  put '        prxchange(''s/\x0A/\n/'',-1,      /* new line */ ';
  put '        prxchange(''s/\x0D/\r/'',-1,      /* carriage return */ ';
  put '        prxchange(''s/\x09/\\t/'',-1,     /* tab */ ';
  put '        prxchange(''s/\x00/\\u0000/'',-1, /* NUL */ ';
  put '        prxchange(''s/\x0E/\\u000E/'',-1, /* SS  */ ';
  put '        prxchange(''s/\x0F/\\u000F/'',-1, /* SF  */ ';
  put '        prxchange(''s/\x01/\\u0001/'',-1, /* SOH */ ';
  put '        prxchange(''s/\x02/\\u0002/'',-1, /* STX */ ';
  put '        prxchange(''s/\x10/\\u0010/'',-1, /* DLE */ ';
  put '        prxchange(''s/\x11/\\u0011/'',-1, /* DC1 */ ';
  put '        prxchange(''s/\x1A/\\u001A/'',-1, /* SUB */ ';
  put '        prxchange(''s/\\/\\\\/'',-1,syserrortext) ';
  put '      )))))))))))))!!''"''; ';
  put '    end; ';
  put '    else syserrortext=cats(''"'',syserrortext,''"''); ';
  put '    put '',"SYSERRORTEXT" : '' syserrortext; ';
  put '    put ",""SYSHOSTNAME"" : ""&syshostname"" "; ';
  put '    put ",""SYSPROCESSID"" : ""&SYSPROCESSID"" "; ';
  put '    put ",""SYSPROCESSMODE"" : ""&SYSPROCESSMODE"" "; ';
  put '    SYSPROCESSNAME=quote(urlencode(cats(SYSPROCESSNAME))); ';
  put '    put ",""SYSPROCESSNAME"" : " SYSPROCESSNAME; ';
  put '    put ",""SYSJOBID"" : ""&sysjobid"" "; ';
  put '    put ",""SYSSCPL"" : ""&sysscpl"" "; ';
  put '    put ",""SYSSITE"" : ""&syssite"" "; ';
  put '    put ",""SYSUSERID"" : ""&sysuserid"" "; ';
  put '    sysvlong=quote(trim(symget(''sysvlong''))); ';
  put '    put '',"SYSVLONG" : '' sysvlong; ';
  put '    syswarningtext=cats(symget(''syswarningtext'')); ';
  put '    if findc(syswarningtext,''"\''!!''0A0D09000E0F010210111A''x) then do; ';
  put '      syswarningtext=''"''!!trim( ';
  put '        prxchange(''s/"/\\"/'',-1,        /* double quote */ ';
  put '        prxchange(''s/\x0A/\n/'',-1,      /* new line */ ';
  put '        prxchange(''s/\x0D/\r/'',-1,      /* carriage return */ ';
  put '        prxchange(''s/\x09/\\t/'',-1,     /* tab */ ';
  put '        prxchange(''s/\x00/\\u0000/'',-1, /* NUL */ ';
  put '        prxchange(''s/\x0E/\\u000E/'',-1, /* SS  */ ';
  put '        prxchange(''s/\x0F/\\u000F/'',-1, /* SF  */ ';
  put '        prxchange(''s/\x01/\\u0001/'',-1, /* SOH */ ';
  put '        prxchange(''s/\x02/\\u0002/'',-1, /* STX */ ';
  put '        prxchange(''s/\x10/\\u0010/'',-1, /* DLE */ ';
  put '        prxchange(''s/\x11/\\u0011/'',-1, /* DC1 */ ';
  put '        prxchange(''s/\x1A/\\u001A/'',-1, /* SUB */ ';
  put '        prxchange(''s/\\/\\\\/'',-1,syswarningtext) ';
  put '      )))))))))))))!!''"''; ';
  put '    end; ';
  put '    else syswarningtext=cats(''"'',syswarningtext,''"''); ';
  put '    put '',"SYSWARNINGTEXT" : '' syswarningtext; ';
  put '    put '',"END_DTTM" : "'' "%sysfunc(datetime(),E8601DT26.6)" ''" ''; ';
  put '    length memsize $32; ';
  put '    memsize="%sysfunc(INPUTN(%sysfunc(getoption(memsize)), best.),sizekmg.)"; ';
  put '    memsize=quote(cats(memsize)); ';
  put '    put '',"MEMSIZE" : '' memsize; ';
  put '    put "}"; ';
  put ' ';
  put '  %if %upcase(&fref) ne _WEBOUT and &stream=Y %then %do; ';
  put '    data _null_; rc=fcopy("&fref","_webout");run; ';
  put '  %end; ';
  put ' ';
  put '%end; ';
  put ' ';
  put '%mend mv_webout; ';
/* WEBOUT END */
  put '/* if calling viya service with _job param, _program will conflict */';
  put '/* so it is provided by SASjs instead as __program */';
  put '%global __program _program;';
  put '%let _program=%sysfunc(coalescec(&__program,&_program));';
  put ' ';
  put '%macro webout(action,ds,dslabel=,fmt=,missing=NULL,showmeta=NO';
  put '    ,maxobs=MAX';
  put ');';
  put '  %mv_webout(&action,ds=&ds,dslabel=&dslabel,fmt=&fmt,missing=&missing';
  put '    ,showmeta=&showmeta,maxobs=&maxobs';
  put '  )';
  put '%mend;';
run;

/* insert the code, escaping double quotes and carriage returns */
%&dbg.put &sysmacroname: Creating final input file;
%local x fref freflist;
%let freflist= &adapter &precode &code ;
%do x=1 %to %sysfunc(countw(&freflist));
  %let fref=%scan(&freflist,&x);
  %&dbg.put &sysmacroname: adding &fref fileref;
  data _null_;
    length filein 8 fileid 8;
    filein = fopen("&fref","I",1,"B");
    fileid = fopen("&fname3","A",1,"B");
    rec = "20"x;
    do while(fread(filein)=0);
      rc = fget(filein,rec,1);
      if rec='"' then do;  /* DOUBLE QUOTE */
        rc =fput(fileid,'\');rc =fwrite(fileid);
        rc =fput(fileid,'"');rc =fwrite(fileid);
      end;
      else if rec='0A'x then do; /* LF */
        rc =fput(fileid,'\');rc =fwrite(fileid);
        rc =fput(fileid,'n');rc =fwrite(fileid);
      end;
      else if rec='0D'x then do; /* CR */
        rc =fput(fileid,'\');rc =fwrite(fileid);
        rc =fput(fileid,'r');rc =fwrite(fileid);
      end;
      else if rec='09'x then do; /* TAB */
        rc =fput(fileid,'\');rc =fwrite(fileid);
        rc =fput(fileid,'t');rc =fwrite(fileid);
      end;
      else if rec='5C'x then do; /* BACKSLASH */
        rc =fput(fileid,'\');rc =fwrite(fileid);
        rc =fput(fileid,'\');rc =fwrite(fileid);
      end;
      else if rec='01'x then do; /* Unprintable */
        rc =fput(fileid,'\');rc =fwrite(fileid);
        rc =fput(fileid,'u');rc =fwrite(fileid);
        rc =fput(fileid,'0');rc =fwrite(fileid);
        rc =fput(fileid,'0');rc =fwrite(fileid);
        rc =fput(fileid,'0');rc =fwrite(fileid);
        rc =fput(fileid,'1');rc =fwrite(fileid);
      end;
      else if rec='07'x then do; /* Bell Char */
        rc =fput(fileid,'\');rc =fwrite(fileid);
        rc =fput(fileid,'u');rc =fwrite(fileid);
        rc =fput(fileid,'0');rc =fwrite(fileid);
        rc =fput(fileid,'0');rc =fwrite(fileid);
        rc =fput(fileid,'0');rc =fwrite(fileid);
        rc =fput(fileid,'7');rc =fwrite(fileid);
      end;
      else if rec='1B'x then do; /* escape char */
        rc =fput(fileid,'\');rc =fwrite(fileid);
        rc =fput(fileid,'u');rc =fwrite(fileid);
        rc =fput(fileid,'0');rc =fwrite(fileid);
        rc =fput(fileid,'0');rc =fwrite(fileid);
        rc =fput(fileid,'1');rc =fwrite(fileid);
        rc =fput(fileid,'B');rc =fwrite(fileid);
      end;
      else do;
        rc =fput(fileid,rec);
        rc =fwrite(fileid);
      end;
    end;
    rc=fclose(filein);
    rc=fclose(fileid);
  run;
%end;

/* finish off the body of the code file loaded to JES */
data _null_;
  file &fname3 mod TERMSTR=' ';
  put '"}';
run;

%if &mdebug=1 and &SYS_PROCHTTP_STATUS_CODE ne 201 %then %do;
  %put &sysmacroname: input about to be POSTed;
  data _null_;infile &fname3;input;putlog _infile_;run;
%end;

%&dbg.put &sysmacroname: Creating the actual service!;
%local fname4;
%let fname4=%mf_getuniquefileref();
proc http method='POST'
    in=&fname3
    out=&fname4
    &oauth_bearer
    url="&base_uri/jobDefinitions/definitions?parentFolderUri=&parentFolderUri";
    headers 'Content-Type'='application/vnd.sas.job.definition+json'
  %if &grant_type=authorization_code %then %do;
            "Authorization"="Bearer &&&access_token_var"
  %end;
            "Accept"="application/vnd.sas.job.definition+json";
%if &mdebug=1 %then %do;
    debug level = 3;
%end;
run;
%if &mdebug=1 and &SYS_PROCHTTP_STATUS_CODE ne 201 %then %do;
  %put &sysmacroname: output from POSTing job definition;
  data _null_;infile &fname4;input;putlog _infile_;run;
%end;
%mp_abort(iftrue=(&SYS_PROCHTTP_STATUS_CODE ne 201)
  ,mac=&sysmacroname
  ,msg=%str(&SYS_PROCHTTP_STATUS_CODE &SYS_PROCHTTP_STATUS_PHRASE)
)

/* get the url so we can give a helpful log message */
%local url;
data _null_;
  if symexist('_baseurl') then do;
    url=symget('_baseurl');
    if subpad(url,length(url)-9,9)='SASStudio'
      then url=substr(url,1,length(url)-11);
    else url="&systcpiphostname";
  end;
  else url="&systcpiphostname";
  call symputx('url',url);
run;

%if &mdebug=1 %then %do;
  %put &sysmacroname exit vars:;
  %put _local_;
%end;
%else %do;
  /* clear refs */
  filename &fname1 clear;
  filename &fname2 clear;
  filename &fname3 clear;
  filename &fname4 clear;
  filename &adapter clear;
  libname &libref1 clear;
%end;

%put &sysmacroname: Job &name created!  Check it out:;
%put &url/SASJobExecution?_PROGRAM=&path/&name;

%mend mv_createwebservice;


/* system macros for build process end */







%let path=jobs;

%let path=jobs/demo;

%let service=response;
filename sascode temp lrecl=32767;
data _null_;
file sascode;

 put '* Job Variables start;';
 put '* Job Variables end;';
 put '* SAS Macros start;';
 put '* SAS Macros end;';
 put '* SAS Includes start;';
 put '* SAS Includes end;';
 put '* Binary Files start;';
 put '* Binary Files end;';
 put '* Job start;';
 put '/**';
 put '@file';
 put '@brief';
 put '@details example webout response';
 put '**/';
 put 'filename _webout filesrvc parenturi="&SYS_JES_JOB_URI" name="_webout.json";';
 put 'data _null_;';
 put 'file _webout;';
 put 'put ''{"name" : "value")'';';
 put 'run;';
 put '* Job end;';

run;
%mv_createwebservice(path=&appLoc/&path, name=&service, code=sascode,replace=yes)
filename sascode clear;



%let path=tests;

%let path=tests/jobs;

%let path=tests/jobs/demo;

%let service=response.test;
filename sascode temp lrecl=32767;
data _null_;
file sascode;

 put '%macro mf_getuser(';
 put ')/*/STORE SOURCE*/;';
 put '%local user;';
 put '%if %symexist(_sasjs_username) %then %let user=&_sasjs_username;';
 put '%else %if %symexist(SYS_COMPUTE_SESSION_OWNER) %then %do;';
 put '%let user=&SYS_COMPUTE_SESSION_OWNER;';
 put '%end;';
 put '%else %if %symexist(_metaperson) %then %do;';
 put '%if %length(&_metaperson)=0 %then %let user=&sysuserid;';
 put '/* sometimes SAS will add @domain extension - remove for consistency */';
 put '/* but be sure to quote in case of usernames with commas */';
 put '%else %let user=%unquote(%scan(%quote(&_metaperson),1,@));';
 put '%end;';
 put '%else %let user=&sysuserid;';
 put '%quote(&user)';
 put '%mend mf_getuser;';
 put '/**';
 put '@file mp_jsonout.sas';
 put '@brief Writes JSON in SASjs format to a fileref';
 put '@details This macro can be used to OPEN a JSON stream and send one or more';
 put 'tables as arrays of rows, where each row can be an object or a nested array.';
 put 'There are two engines available - DATASTEP or PROCJSON.';
 put 'PROC JSON is fast but will produce errs like the ones below if';
 put 'special chars are encountered.';
 put '> (ERR)OR: Some code points did not transcode.';
 put '> An object or array close is not valid at this point in the JSON text.';
 put '> Date value out of range';
 put 'If this happens, try running with ENGINE=DATASTEP.';
 put 'The DATASTEP engine is used to handle special SAS missing numerics, and';
 put 'can also convert entire datasets to formatted values.  Output JSON is always';
 put 'in UTF-8.';
 put 'Usage:';
 put 'filename tmp temp;';
 put 'data class; set sashelp.class;run;';
 put '%mp_jsonout(OPEN,jref=tmp)';
 put '%mp_jsonout(OBJ,class,jref=tmp)';
 put '%mp_jsonout(OBJ,class,dslabel=class2,jref=tmp,showmeta=Y)';
 put '%mp_jsonout(CLOSE,jref=tmp)';
 put 'data _null_;';
 put 'infile tmp;';
 put 'input;putlog _infile_;';
 put 'run;';
 put 'If you are building web apps with SAS then you are strongly encouraged to use';
 put 'the mX_createwebservice macros in combination with the';
 put '[sasjs adapter](https://github.com/sasjs/adapter).';
 put 'For more information see https://sasjs.io';
 put '@param [in] action Valid values:';
 put '@li OPEN - opens the JSON';
 put '@li OBJ - sends a table with each row as an object';
 put '@li ARR - sends a table with each row in an array';
 put '@li CLOSE - closes the JSON';
 put '@param [in] ds The dataset to send.  Must be a work table.';
 put '@param [out] jref= (_webout) The fileref to which to send the JSON';
 put '@param [out] dslabel= The name to give the table in the exported JSON';
 put '@param [in] fmt= (Y) Whether to keep (Y) or strip (N) formats from the table';
 put '@param [in] engine= (DATASTEP) Which engine to use to send the JSON. Options:';
 put '@li PROCJSON (default)';
 put '@li DATASTEP (more reliable when data has non standard characters)';
 put '@param [in] missing= (NULL) Special numeric missing values can be sent as NULL';
 put '(eg `null`) or as STRING values (eg `".a"` or `".b"`)';
 put '@param [in] showmeta= (N) Set to Y to output metadata alongside each table,';
 put 'such as the column formats and types.  The metadata is contained inside an';
 put 'object with the same name as the table but prefixed with a dollar sign - ie,';
 put '`,"$tablename":{"formats":{"col1":"$CHAR1"},"types":{"COL1":"C"}}`';
 put '@param [in] maxobs= (MAX) Provide an integer to limit the number of input rows';
 put 'that should be converted to JSON';
 put '<h4> Related Files </h4>';
 put '@li mp_ds2fmtds.sas';
 put '@version 9.2';
 put '@author Allan Bowe';
 put '@source https://github.com/sasjs/core';
 put '**/';
 put '%macro mp_jsonout(action,ds,jref=_webout,dslabel=,fmt=Y';
 put ',engine=DATASTEP';
 put ',missing=NULL';
 put ',showmeta=N';
 put ',maxobs=MAX';
 put ')/*/STORE SOURCE*/;';
 put '%local tempds colinfo fmtds i numcols numobs stmt_obs lastobs optval';
 put 'tmpds1 tmpds2 tmpds3 tmpds4;';
 put '%let numcols=0;';
 put '%if &maxobs ne MAX %then %let stmt_obs=%str(if _n_>&maxobs then stop;);';
 put '%if &action=OPEN %then %do;';
 put 'options nobomfile;';
 put 'data _null_;file &jref encoding=''utf-8'' lrecl=200;';
 put 'put ''{"PROCESSED_DTTM" : "'' "%sysfunc(datetime(),E8601DT26.6)" ''"'';';
 put 'run;';
 put '%end;';
 put '%else %if (&action=ARR or &action=OBJ) %then %do;';
 put '/* force variable names to always be uppercase in the JSON */';
 put 'options validvarname=upcase;';
 put '/* To avoid issues with _webout on EBI - such as encoding diffs and truncation';
 put '(https://support.sas.com/kb/49/325.html) we use temporary files */';
 put 'filename _sjs1 temp lrecl=200 ;';
 put 'data _null_; file _sjs1 encoding=''utf-8'';';
 put 'put ", ""%lowcase(%sysfunc(coalescec(&dslabel,&ds)))"":";';
 put 'run;';
 put '/* now write to _webout 1 char at a time */';
 put 'data _null_;';
 put 'infile _sjs1 lrecl=1 recfm=n;';
 put 'file &jref mod lrecl=1 recfm=n;';
 put 'input sourcechar $char1. @@;';
 put 'format sourcechar hex2.;';
 put 'put sourcechar char1. @@;';
 put 'run;';
 put 'filename _sjs1 clear;';
 put '/* grab col defs */';
 put 'proc contents noprint data=&ds';
 put 'out=_data_(keep=name type length format formatl formatd varnum label);';
 put 'run;';
 put '%let colinfo=%scan(&syslast,2,.);';
 put 'proc sort data=&colinfo;';
 put 'by varnum;';
 put 'run;';
 put '/* move meta to mac vars */';
 put 'data &colinfo;';
 put 'if _n_=1 then call symputx(''numcols'',nobs,''l'');';
 put 'set &colinfo end=last nobs=nobs;';
 put 'name=upcase(name);';
 put '/* fix formats */';
 put 'if type=2 or type=6 then do;';
 put 'typelong=''char'';';
 put 'length fmt $49.;';
 put 'if format='''' then fmt=cats(''$'',length,''.'');';
 put 'else if formatl=0 then fmt=cats(format,''.'');';
 put 'else fmt=cats(format,formatl,''.'');';
 put 'end;';
 put 'else do;';
 put 'typelong=''num'';';
 put 'if format='''' then fmt=''best.'';';
 put 'else if formatl=0 then fmt=cats(format,''.'');';
 put 'else if formatd=0 then fmt=cats(format,formatl,''.'');';
 put 'else fmt=cats(format,formatl,''.'',formatd);';
 put 'end;';
 put '/* 32 char unique name */';
 put 'newname=''sasjs''!!substr(cats(put(md5(name),$hex32.)),1,27);';
 put 'call symputx(cats(''name'',_n_),name,''l'');';
 put 'call symputx(cats(''newname'',_n_),newname,''l'');';
 put 'call symputx(cats(''length'',_n_),length,''l'');';
 put 'call symputx(cats(''fmt'',_n_),fmt,''l'');';
 put 'call symputx(cats(''type'',_n_),type,''l'');';
 put 'call symputx(cats(''typelong'',_n_),typelong,''l'');';
 put 'call symputx(cats(''label'',_n_),coalescec(label,name),''l'');';
 put '/* overwritten when fmt=Y and a custom format exists in catalog */';
 put 'if typelong=''num'' then call symputx(cats(''fmtlen'',_n_),200,''l'');';
 put 'else call symputx(cats(''fmtlen'',_n_),min(32767,ceil((length+10)*1.5)),''l'');';
 put 'run;';
 put '%let tempds=%substr(_%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32);';
 put 'proc sql;';
 put 'select count(*) into: lastobs from &ds;';
 put '%if &maxobs ne MAX %then %let lastobs=%sysfunc(min(&lastobs,&maxobs));';
 put '%if &engine=PROCJSON %then %do;';
 put '%if &missing=STRING %then %do;';
 put '%put &sysmacroname: Special Missings not supported in proc json.;';
 put '%put &sysmacroname: Switching to DATASTEP engine;';
 put '%goto datastep;';
 put '%end;';
 put 'data &tempds;';
 put 'set &ds;';
 put '&stmt_obs;';
 put '%if &fmt=N %then format _numeric_ best32.;;';
 put '/* PRETTY is necessary to avoid line truncation in large files */';
 put 'filename _sjs2 temp lrecl=131068 encoding=''utf-8'';';
 put 'proc json out=_sjs2 pretty';
 put '%if &action=ARR %then nokeys ;';
 put ';export &tempds / nosastags fmtnumeric;';
 put 'run;';
 put '/* send back to webout */';
 put 'data _null_;';
 put 'infile _sjs2 lrecl=1 recfm=n;';
 put 'file &jref mod lrecl=1 recfm=n;';
 put 'input sourcechar $char1. @@;';
 put 'format sourcechar hex2.;';
 put 'put sourcechar char1. @@;';
 put 'run;';
 put 'filename _sjs2 clear;';
 put '%end;';
 put '%else %if &engine=DATASTEP %then %do;';
 put '%datastep:';
 put '%if %sysfunc(exist(&ds)) ne 1 & %sysfunc(exist(&ds,VIEW)) ne 1';
 put '%then %do;';
 put '%put &sysmacroname:  &ds NOT FOUND!!!;';
 put '%return;';
 put '%end;';
 put '%if &fmt=Y %then %do;';
 put '/**';
 put '* Extract format definitions';
 put '* First, by getting library locations from dictionary.formats';
 put '* Then, by exporting the width using proc format';
 put '* Cannot use maxw from sashelp.vformat as not always populated';
 put '* Cannot use fmtinfo() as not supported in all flavours';
 put '*/';
 put '%let tmpds1=%substr(fmtsum%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32);';
 put '%let tmpds2=%substr(cntl%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32);';
 put '%let tmpds3=%substr(cntl%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32);';
 put '%let tmpds4=%substr(col%sysfunc(compress(%sysfunc(uuidgen()),-)),1,32);';
 put 'proc sql noprint;';
 put 'create table &tmpds1 as';
 put 'select cats(libname,''.'',memname) as FMTCAT,';
 put 'FMTNAME';
 put 'from dictionary.formats';
 put 'where fmttype=''F'' and libname is not null';
 put 'and fmtname in (select format from &colinfo where format is not null)';
 put 'order by 1;';
 put 'create table &tmpds2(';
 put 'FMTNAME char(32),';
 put 'LENGTH num';
 put ');';
 put '%local catlist cat fmtlist i;';
 put 'select distinct fmtcat into: catlist separated by '' '' from &tmpds1;';
 put '%do i=1 %to %sysfunc(countw(&catlist,%str( )));';
 put '%let cat=%scan(&catlist,&i,%str( ));';
 put 'proc sql;';
 put 'select distinct fmtname into: fmtlist separated by '' ''';
 put 'from &tmpds1 where fmtcat="&cat";';
 put 'proc format lib=&cat cntlout=&tmpds3(keep=fmtname length);';
 put 'select &fmtlist;';
 put 'run;';
 put 'proc sql;';
 put 'insert into &tmpds2 select distinct fmtname,length from &tmpds3;';
 put '%end;';
 put 'proc sql;';
 put 'create table &tmpds4 as';
 put 'select a.*, b.length as MAXW';
 put 'from &colinfo a';
 put 'left join &tmpds2 b';
 put 'on cats(a.format)=cats(upcase(b.fmtname))';
 put 'order by a.varnum;';
 put 'data _null_;';
 put 'set &tmpds4;';
 put 'if not missing(maxw);';
 put 'call symputx(';
 put 'cats(''fmtlen'',_n_),';
 put '/* vars need extra padding due to JSON escaping of special chars */';
 put 'min(32767,ceil((max(length,maxw)+10)*1.5))';
 put ',''l''';
 put ');';
 put 'run;';
 put '/* configure varlenchk - as we are explicitly shortening the variables */';
 put '%let optval=%sysfunc(getoption(varlenchk));';
 put 'options varlenchk=NOWARN;';
 put 'data _data_(compress=char);';
 put '/* shorten the new vars */';
 put 'length';
 put '%do i=1 %to &numcols;';
 put '&&name&i $&&fmtlen&i';
 put '%end;';
 put ';';
 put '/* rename on entry */';
 put 'set &ds(rename=(';
 put '%do i=1 %to &numcols;';
 put '&&name&i=&&newname&i';
 put '%end;';
 put '));';
 put '&stmt_obs;';
 put 'drop';
 put '%do i=1 %to &numcols;';
 put '&&newname&i';
 put '%end;';
 put ';';
 put '%do i=1 %to &numcols;';
 put '%if &&typelong&i=num %then %do;';
 put '&&name&i=cats(put(&&newname&i,&&fmt&i));';
 put '%end;';
 put '%else %do;';
 put '&&name&i=put(&&newname&i,&&fmt&i);';
 put '%end;';
 put '%end;';
 put 'if _error_ then do;';
 put 'call symputx(''syscc'',1012);';
 put 'stop;';
 put 'end;';
 put 'run;';
 put '%let fmtds=&syslast;';
 put 'options varlenchk=&optval;';
 put '%end;';
 put 'proc format; /* credit yabwon for special null removal */';
 put 'value bart (default=40)';
 put '%if &missing=NULL %then %do;';
 put '._ - .z = null';
 put '%end;';
 put '%else %do;';
 put '._ = [quote()]';
 put '. = null';
 put '.a - .z = [quote()]';
 put '%end;';
 put 'other = [best.];';
 put 'data &tempds;';
 put 'attrib _all_ label='''';';
 put '%do i=1 %to &numcols;';
 put '%if &&typelong&i=char or &fmt=Y %then %do;';
 put 'length &&name&i $&&fmtlen&i...;';
 put 'format &&name&i $&&fmtlen&i...;';
 put '%end;';
 put '%end;';
 put '%if &fmt=Y %then %do;';
 put 'set &fmtds;';
 put '%end;';
 put '%else %do;';
 put 'set &ds;';
 put '%end;';
 put '&stmt_obs;';
 put 'format _numeric_ bart.;';
 put '%do i=1 %to &numcols;';
 put '%if &&typelong&i=char or &fmt=Y %then %do;';
 put 'if findc(&&name&i,''"\''!!''0A0D09000E0F010210111A''x) then do;';
 put '&&name&i=''"''!!trim(';
 put 'prxchange(''s/"/\\"/'',-1,        /* double quote */';
 put 'prxchange(''s/\x0A/\n/'',-1,      /* new line */';
 put 'prxchange(''s/\x0D/\r/'',-1,      /* carriage return */';
 put 'prxchange(''s/\x09/\\t/'',-1,     /* tab */';
 put 'prxchange(''s/\x00/\\u0000/'',-1, /* NUL */';
 put 'prxchange(''s/\x0E/\\u000E/'',-1, /* SS  */';
 put 'prxchange(''s/\x0F/\\u000F/'',-1, /* SF  */';
 put 'prxchange(''s/\x01/\\u0001/'',-1, /* SOH */';
 put 'prxchange(''s/\x02/\\u0002/'',-1, /* STX */';
 put 'prxchange(''s/\x10/\\u0010/'',-1, /* DLE */';
 put 'prxchange(''s/\x11/\\u0011/'',-1, /* DC1 */';
 put 'prxchange(''s/\x1A/\\u001A/'',-1, /* SUB */';
 put 'prxchange(''s/\\/\\\\/'',-1,&&name&i)';
 put ')))))))))))))!!''"'';';
 put 'end;';
 put 'else &&name&i=quote(cats(&&name&i));';
 put '%end;';
 put '%end;';
 put 'run;';
 put 'filename _sjs3 temp lrecl=131068 ;';
 put 'data _null_;';
 put 'file _sjs3 encoding=''utf-8'';';
 put 'if _n_=1 then put "[";';
 put 'set &tempds;';
 put 'if _n_>1 then put "," @; put';
 put '%if &action=ARR %then "[" ; %else "{" ;';
 put '%do i=1 %to &numcols;';
 put '%if &i>1 %then  "," ;';
 put '%if &action=OBJ %then """&&name&i"":" ;';
 put '"&&name&i"n /* name literal for reserved variable names */';
 put '%end;';
 put '%if &action=ARR %then "]" ; %else "}" ; ;';
 put '/* close out the table */';
 put 'data _null_;';
 put 'file _sjs3 mod encoding=''utf-8'';';
 put 'put '']'';';
 put 'run;';
 put 'data _null_;';
 put 'infile _sjs3 lrecl=1 recfm=n;';
 put 'file &jref mod lrecl=1 recfm=n;';
 put 'input sourcechar $char1. @@;';
 put 'format sourcechar hex2.;';
 put 'put sourcechar char1. @@;';
 put 'run;';
 put 'filename _sjs3 clear;';
 put '%end;';
 put 'proc sql;';
 put 'drop table &colinfo, &tempds;';
 put '%if %substr(&showmeta,1,1)=Y %then %do;';
 put 'filename _sjs4 temp lrecl=131068 encoding=''utf-8'';';
 put 'data _null_;';
 put 'file _sjs4;';
 put 'length label $350;';
 put 'put ", ""$%lowcase(%sysfunc(coalescec(&dslabel,&ds)))"":{""vars"":{";';
 put 'do i=1 to &numcols;';
 put 'name=quote(trim(symget(cats(''name'',i))));';
 put 'format=quote(trim(symget(cats(''fmt'',i))));';
 put 'label=quote(prxchange(''s/\\/\\\\/'',-1,trim(symget(cats(''label'',i)))));';
 put 'length=quote(trim(symget(cats(''length'',i))));';
 put 'type=quote(trim(symget(cats(''typelong'',i))));';
 put 'if i>1 then put "," @@;';
 put 'put name '':{"format":'' format '',"label":'' label';
 put ''',"length":'' length '',"type":'' type ''}'';';
 put 'end;';
 put 'put ''}}'';';
 put 'run;';
 put '/* send back to webout */';
 put 'data _null_;';
 put 'infile _sjs4 lrecl=1 recfm=n;';
 put 'file &jref mod lrecl=1 recfm=n;';
 put 'input sourcechar $char1. @@;';
 put 'format sourcechar hex2.;';
 put 'put sourcechar char1. @@;';
 put 'run;';
 put 'filename _sjs4 clear;';
 put '%end;';
 put '%end;';
 put '%else %if &action=CLOSE %then %do;';
 put 'data _null_; file &jref encoding=''utf-8'' mod ;';
 put 'put "}";';
 put 'run;';
 put '%end;';
 put '%mend mp_jsonout;';
 put '/**';
 put '@file';
 put '@brief Send data to/from the SAS Viya Job Execution Service';
 put '@details This macro should be added to the start of each Job Execution';
 put 'Service, **immediately** followed by a call to:';
 put '%mv_webout(FETCH)';
 put 'This will read all the input data and create same-named SAS datasets in the';
 put 'WORK library.  You can then insert your code, and send data back using the';
 put 'following syntax:';
 put 'data some datasets; * make some data ;';
 put 'retain some columns;';
 put 'run;';
 put '%mv_webout(OPEN)';
 put '%mv_webout(ARR,some)  * Array format, fast, suitable for large tables ;';
 put '%mv_webout(OBJ,datasets) * Object format, easier to work with ;';
 put '%mv_webout(CLOSE)';
 put '@param [in] action Either OPEN, ARR, OBJ or CLOSE';
 put '@param [in] ds The dataset to send back to the frontend';
 put '@param [in] _webout= fileref for returning the json';
 put '@param [out] fref=(_mvwtemp) Temp fileref to which to write the output';
 put '@param [out] dslabel= value to use instead of table name for sending to JSON';
 put '@param [in] fmt= (N) Setting Y converts all vars to their formatted values';
 put '@param [in] stream=(Y) Change to N if not streaming to _webout';
 put '@param [in] missing= (NULL) Special numeric missing values can be sent as NULL';
 put '(eg `null`) or as STRING values (eg `".a"` or `".b"`)';
 put '@param [in] showmeta= (N) Set to Y to output metadata alongside each table,';
 put 'such as the column formats and types.  The metadata is contained inside an';
 put 'object with the same name as the table but prefixed with a dollar sign - ie,';
 put '`,"$tablename":{"formats":{"col1":"$CHAR1"},"types":{"COL1":"C"}}`';
 put '@param [in] maxobs= (MAX) Provide an integer to limit the number of input rows';
 put 'that should be converted to output JSON';
 put '@param [in] workobs= (0) When set to a positive integer, will create a new';
 put 'output object (WORK) which contains this number of observations from all';
 put 'tables in the WORK library.';
 put '<h4> SAS Macros </h4>';
 put '@li mp_jsonout.sas';
 put '@li mf_getuser.sas';
 put '<h4> Related Macros </h4>';
 put '@li ms_webout.sas';
 put '@li mm_webout.sas';
 put '@version Viya 3.3';
 put '@author Allan Bowe, source: https://github.com/sasjs/core';
 put '**/';
 put '%macro mv_webout(action,ds,fref=_mvwtemp,dslabel=,fmt=N,stream=Y,missing=NULL';
 put ',showmeta=N,maxobs=MAX,workobs=0';
 put ');';
 put '%global _webin_file_count _webin_fileuri _debug _omittextlog _webin_name';
 put 'sasjs_tables SYS_JES_JOB_URI;';
 put '%if %index("&_debug",log) %then %let _debug=131;';
 put '%local i tempds table;';
 put '%let action=%upcase(&action);';
 put '%if &action=FETCH %then %do;';
 put '%if %upcase(&_omittextlog)=FALSE or %str(&_debug) ge 131 %then %do;';
 put 'options mprint notes mprintnest;';
 put '%end;';
 put '%if not %symexist(_webin_fileuri1) %then %do;';
 put '%let _webin_file_count=%eval(&_webin_file_count+0);';
 put '%let _webin_fileuri1=&_webin_fileuri;';
 put '%let _webin_name1=&_webin_name;';
 put '%end;';
 put '/* if the sasjs_tables param is passed, we expect param based upload */';
 put '%if %length(&sasjs_tables.X)>1 %then %do;';
 put '/* convert data from macro variables to datasets */';
 put '%do i=1 %to %sysfunc(countw(&sasjs_tables));';
 put '%let table=%scan(&sasjs_tables,&i,%str( ));';
 put '%if %symexist(sasjs&i.data0)=0 %then %let sasjs&i.data0=1;';
 put 'data _null_;';
 put 'file "%sysfunc(pathname(work))/&table..csv" recfm=n;';
 put 'retain nrflg 0;';
 put 'length line $32767;';
 put 'do i=1 to &&sasjs&i.data0;';
 put 'if &&sasjs&i.data0=1 then line=symget("sasjs&i.data");';
 put 'else line=symget(cats("sasjs&i.data",i));';
 put 'if i=1 and substr(line,1,7)=''%nrstr('' then do;';
 put 'nrflg=1;';
 put 'line=substr(line,8);';
 put 'end;';
 put 'if i=&&sasjs&i.data0 and nrflg=1 then do;';
 put 'line=substr(line,1,length(line)-1);';
 put 'end;';
 put 'put line +(-1) @;';
 put 'end;';
 put 'run;';
 put 'data _null_;';
 put 'infile "%sysfunc(pathname(work))/&table..csv" termstr=crlf ;';
 put 'input;';
 put 'if _n_=1 then call symputx(''input_statement'',_infile_);';
 put 'list;';
 put 'data work.&table;';
 put 'infile "%sysfunc(pathname(work))/&table..csv" firstobs=2 dsd';
 put 'termstr=crlf;';
 put 'input &input_statement;';
 put 'run;';
 put '%end;';
 put '%end;';
 put '%else %do i=1 %to &_webin_file_count;';
 put '/* read in any files that are sent */';
 put '/* this part needs refactoring for wide files */';
 put 'filename indata filesrvc "&&_webin_fileuri&i" lrecl=999999;';
 put 'data _null_;';
 put 'infile indata termstr=crlf lrecl=32767;';
 put 'input;';
 put 'if _n_=1 then call symputx(''input_statement'',_infile_);';
 put '%if %str(&_debug) ge 131 %then %do;';
 put 'if _n_<20 then putlog _infile_;';
 put 'else stop;';
 put '%end;';
 put '%else %do;';
 put 'stop;';
 put '%end;';
 put 'run;';
 put 'data &&_webin_name&i;';
 put 'infile indata firstobs=2 dsd termstr=crlf ;';
 put 'input &input_statement;';
 put 'run;';
 put '%let sasjs_tables=&sasjs_tables &&_webin_name&i;';
 put '%end;';
 put '%end;';
 put '%else %if &action=OPEN %then %do;';
 put '/* setup webout */';
 put 'OPTIONS NOBOMFILE;';
 put '%if "X&SYS_JES_JOB_URI.X"="XX" %then %do;';
 put 'filename _webout temp lrecl=999999 mod;';
 put '%end;';
 put '%else %do;';
 put 'filename _webout filesrvc parenturi="&SYS_JES_JOB_URI"';
 put 'name="_webout.json" lrecl=999999 mod;';
 put '%end;';
 put '/* setup temp ref */';
 put '%if %upcase(&fref) ne _WEBOUT %then %do;';
 put 'filename &fref temp lrecl=999999 permission=''A::u::rwx,A::g::rw-,A::o::---'';';
 put '%end;';
 put '/* setup json */';
 put 'data _null_;file &fref;';
 put 'put ''{"SYSDATE" : "'' "&SYSDATE" ''"'';';
 put 'put '',"SYSTIME" : "'' "&SYSTIME" ''"'';';
 put 'run;';
 put '%end;';
 put '%else %if &action=ARR or &action=OBJ %then %do;';
 put '%mp_jsonout(&action,&ds,dslabel=&dslabel,fmt=&fmt,jref=&fref';
 put ',engine=DATASTEP,missing=&missing,showmeta=&showmeta,maxobs=&maxobs';
 put ')';
 put '%end;';
 put '%else %if &action=CLOSE %then %do;';
 put '%if %str(&workobs) > 0 %then %do;';
 put '/* send back first XX records of each work table for debugging */';
 put 'data;run;%let tempds=%scan(&syslast,2,.);';
 put 'ods output Members=&tempds;';
 put 'proc datasets library=WORK memtype=data;';
 put '%local wtcnt;%let wtcnt=0;';
 put 'data _null_;';
 put 'set &tempds;';
 put 'if not (upcase(name) =:"DATA"); /* ignore temp datasets */';
 put 'i+1;';
 put 'call symputx(cats(''wt'',i),name,''l'');';
 put 'call symputx(''wtcnt'',i,''l'');';
 put 'data _null_; file &fref mod; put ",""WORK"":{";';
 put '%do i=1 %to &wtcnt;';
 put '%let wt=&&wt&i;';
 put 'data _null_; file &fref mod;';
 put 'dsid=open("WORK.&wt",''is'');';
 put 'nlobs=attrn(dsid,''NLOBS'');';
 put 'nvars=attrn(dsid,''NVARS'');';
 put 'rc=close(dsid);';
 put 'if &i>1 then put '',''@;';
 put 'put " ""&wt"" : {";';
 put 'put ''"nlobs":'' nlobs;';
 put 'put '',"nvars":'' nvars;';
 put '%mp_jsonout(OBJ,&wt,jref=&fref,dslabel=first10rows,showmeta=Y';
 put ',maxobs=&workobs';
 put ')';
 put 'data _null_; file &fref mod;put "}";';
 put '%end;';
 put 'data _null_; file &fref mod;put "}";run;';
 put '%end;';
 put '/* close off json */';
 put 'data _null_;file &fref mod;';
 put 'length SYSPROCESSNAME syserrortext syswarningtext autoexec $512;';
 put 'put ",""_DEBUG"" : ""&_debug"" ";';
 put '_PROGRAM=quote(trim(resolve(symget(''_PROGRAM''))));';
 put 'put '',"_PROGRAM" : '' _PROGRAM ;';
 put 'autoexec=quote(urlencode(trim(getoption(''autoexec''))));';
 put 'put '',"AUTOEXEC" : '' autoexec;';
 put 'put ",""MF_GETUSER"" : ""%mf_getuser()"" ";';
 put 'SYS_JES_JOB_URI=quote(trim(resolve(symget(''SYS_JES_JOB_URI''))));';
 put 'put '',"SYS_JES_JOB_URI" : '' SYS_JES_JOB_URI ;';
 put 'put ",""SYSJOBID"" : ""&sysjobid"" ";';
 put 'put ",""SYSCC"" : ""&syscc"" ";';
 put 'syserrortext=cats(symget(''syserrortext''));';
 put 'if findc(syserrortext,''"\''!!''0A0D09000E0F010210111A''x) then do;';
 put 'syserrortext=''"''!!trim(';
 put 'prxchange(''s/"/\\"/'',-1,        /* double quote */';
 put 'prxchange(''s/\x0A/\n/'',-1,      /* new line */';
 put 'prxchange(''s/\x0D/\r/'',-1,      /* carriage return */';
 put 'prxchange(''s/\x09/\\t/'',-1,     /* tab */';
 put 'prxchange(''s/\x00/\\u0000/'',-1, /* NUL */';
 put 'prxchange(''s/\x0E/\\u000E/'',-1, /* SS  */';
 put 'prxchange(''s/\x0F/\\u000F/'',-1, /* SF  */';
 put 'prxchange(''s/\x01/\\u0001/'',-1, /* SOH */';
 put 'prxchange(''s/\x02/\\u0002/'',-1, /* STX */';
 put 'prxchange(''s/\x10/\\u0010/'',-1, /* DLE */';
 put 'prxchange(''s/\x11/\\u0011/'',-1, /* DC1 */';
 put 'prxchange(''s/\x1A/\\u001A/'',-1, /* SUB */';
 put 'prxchange(''s/\\/\\\\/'',-1,syserrortext)';
 put ')))))))))))))!!''"'';';
 put 'end;';
 put 'else syserrortext=cats(''"'',syserrortext,''"'');';
 put 'put '',"SYSERRORTEXT" : '' syserrortext;';
 put 'put ",""SYSHOSTNAME"" : ""&syshostname"" ";';
 put 'put ",""SYSPROCESSID"" : ""&SYSPROCESSID"" ";';
 put 'put ",""SYSPROCESSMODE"" : ""&SYSPROCESSMODE"" ";';
 put 'SYSPROCESSNAME=quote(urlencode(cats(SYSPROCESSNAME)));';
 put 'put ",""SYSPROCESSNAME"" : " SYSPROCESSNAME;';
 put 'put ",""SYSJOBID"" : ""&sysjobid"" ";';
 put 'put ",""SYSSCPL"" : ""&sysscpl"" ";';
 put 'put ",""SYSSITE"" : ""&syssite"" ";';
 put 'put ",""SYSUSERID"" : ""&sysuserid"" ";';
 put 'sysvlong=quote(trim(symget(''sysvlong'')));';
 put 'put '',"SYSVLONG" : '' sysvlong;';
 put 'syswarningtext=cats(symget(''syswarningtext''));';
 put 'if findc(syswarningtext,''"\''!!''0A0D09000E0F010210111A''x) then do;';
 put 'syswarningtext=''"''!!trim(';
 put 'prxchange(''s/"/\\"/'',-1,        /* double quote */';
 put 'prxchange(''s/\x0A/\n/'',-1,      /* new line */';
 put 'prxchange(''s/\x0D/\r/'',-1,      /* carriage return */';
 put 'prxchange(''s/\x09/\\t/'',-1,     /* tab */';
 put 'prxchange(''s/\x00/\\u0000/'',-1, /* NUL */';
 put 'prxchange(''s/\x0E/\\u000E/'',-1, /* SS  */';
 put 'prxchange(''s/\x0F/\\u000F/'',-1, /* SF  */';
 put 'prxchange(''s/\x01/\\u0001/'',-1, /* SOH */';
 put 'prxchange(''s/\x02/\\u0002/'',-1, /* STX */';
 put 'prxchange(''s/\x10/\\u0010/'',-1, /* DLE */';
 put 'prxchange(''s/\x11/\\u0011/'',-1, /* DC1 */';
 put 'prxchange(''s/\x1A/\\u001A/'',-1, /* SUB */';
 put 'prxchange(''s/\\/\\\\/'',-1,syswarningtext)';
 put ')))))))))))))!!''"'';';
 put 'end;';
 put 'else syswarningtext=cats(''"'',syswarningtext,''"'');';
 put 'put '',"SYSWARNINGTEXT" : '' syswarningtext;';
 put 'put '',"END_DTTM" : "'' "%sysfunc(datetime(),E8601DT26.6)" ''" '';';
 put 'length memsize $32;';
 put 'memsize="%sysfunc(INPUTN(%sysfunc(getoption(memsize)), best.),sizekmg.)";';
 put 'memsize=quote(cats(memsize));';
 put 'put '',"MEMSIZE" : '' memsize;';
 put 'put "}";';
 put '%if %upcase(&fref) ne _WEBOUT and &stream=Y %then %do;';
 put 'data _null_; rc=fcopy("&fref","_webout");run;';
 put '%end;';
 put '%end;';
 put '%mend mv_webout;';
 put '/* if calling viya service with _job param, _program will conflict */';
 put '/* so we provide instead as __program */';
 put '%global __program _program;';
 put '%let _program=%sysfunc(coalescec(&__program,&_program));';
 put '%macro webout(action,ds,dslabel=,fmt=,missing=NULL,showmeta=NO,maxobs=MAX);';
 put '%mv_webout(&action,ds=&ds,dslabel=&dslabel,fmt=&fmt';
 put ',missing=&missing';
 put ',showmeta=&showmeta';
 put ',maxobs=&maxobs';
 put ') %mend;';
 put '/* provide additional debug info */';
 put '%global _program;';
 put '%put &=syscc;';
 put '%put user=%mf_getuser();';
 put '%put pgm=&_program;';
 put '%put timestamp=%sysfunc(datetime(),datetime19.);';
 put '* Test Variables start;';
 put '* Test Variables end;';
 put '* SAS Macros start;';
 put '%macro mf_getapploc(pgm);';
 put '%if "&pgm"="" %then %do;';
 put '%if %symexist(_program) %then %let pgm=&_program;';
 put '%else %do;';
 put '%put &sysmacroname: No value provided and no _program variable available;';
 put '%return;';
 put '%end;';
 put '%end;';
 put '%local root;';
 put '/**';
 put '* First check we are not in the tests/macros folder (which has no subfolders)';
 put '* or specifically in the testsetup or testteardown services';
 put '*/';
 put '%if %index(&pgm,/tests/macros/)';
 put 'or %index(&pgm,/tests/testsetup)';
 put 'or %index(&pgm,/tests/testteardown)';
 put '%then %do;';
 put '%let root=%substr(&pgm,1,%index(&pgm,/tests)-1);';
 put '&root';
 put '%return;';
 put '%end;';
 put '/**';
 put '* Next, move up two levels to avoid matches on subfolder or service name';
 put '*/';
 put '%let root=%substr(&pgm,1,%length(&pgm)-%length(%scan(&pgm,-1,/))-1);';
 put '%let root=%substr(&root,1,%length(&root)-%length(%scan(&root,-1,/))-1);';
 put '%if %index(&root,/tests/) %then %do;';
 put '%let root=%substr(&root,1,%index(&root,/tests/)-1);';
 put '%end;';
 put '%else %if %index(&root,/services) %then %do;';
 put '%let root=%substr(&root,1,%index(&root,/services)-1);';
 put '%end;';
 put '%else %if %index(&root,/jobs) %then %do;';
 put '%let root=%substr(&root,1,%index(&root,/jobs)-1);';
 put '%end;';
 put '%else %put &sysmacroname: Could not find an app location from &pgm;';
 put '&root';
 put '%mend mf_getapploc ;';
 put '%macro mf_mval(var);';
 put '%if %symexist(&var) %then %do;';
 put '%superq(&var)';
 put '%end;';
 put '%mend mf_mval;';
 put '%macro mf_trimstr(basestr,trimstr);';
 put '%local baselen trimlen trimval;';
 put '/* return if basestr is shorter than trimstr (or 0) */';
 put '%let baselen=%length(%superq(basestr));';
 put '%let trimlen=%length(%superq(trimstr));';
 put '%if &baselen < &trimlen or &baselen=0 %then %return;';
 put '/* obtain the characters from the end of basestr */';
 put '%let trimval=%qsubstr(%superq(basestr)';
 put ',%length(%superq(basestr))-&trimlen+1';
 put ',&trimlen);';
 put '/* compare and if matching, chop it off! */';
 put '%if %superq(basestr)=%superq(trimstr) %then %do;';
 put '%return;';
 put '%end;';
 put '%else %if %superq(trimval)=%superq(trimstr) %then %do;';
 put '%qsubstr(%superq(basestr),1,%length(%superq(basestr))-&trimlen)';
 put '%end;';
 put '%else %do;';
 put '&basestr';
 put '%end;';
 put '%mend mf_trimstr;';
 put '%macro mf_getplatform(switch';
 put ')/*/STORE SOURCE*/;';
 put '%local a b c;';
 put '%if &switch.NONE=NONE %then %do;';
 put '%if %symexist(sasjsprocessmode) %then %do;';
 put '%if &sasjsprocessmode=Stored Program %then %do;';
 put 'SASJS';
 put '%return;';
 put '%end;';
 put '%end;';
 put '%if %symexist(sysprocessmode) %then %do;';
 put '%if "&sysprocessmode"="SAS Object Server"';
 put 'or "&sysprocessmode"= "SAS Compute Server" %then %do;';
 put 'SASVIYA';
 put '%end;';
 put '%else %if "&sysprocessmode"="SAS Stored Process Server"';
 put 'or "&sysprocessmode"="SAS Workspace Server"';
 put '%then %do;';
 put 'SASMETA';
 put '%return;';
 put '%end;';
 put '%else %do;';
 put 'BASESAS';
 put '%return;';
 put '%end;';
 put '%end;';
 put '%else %if %symexist(_metaport) or %symexist(_metauser) %then %do;';
 put 'SASMETA';
 put '%return;';
 put '%end;';
 put '%else %do;';
 put 'BASESAS';
 put '%return;';
 put '%end;';
 put '%end;';
 put '%else %if &switch=SASSTUDIO %then %do;';
 put '/* return the version of SAS Studio else 0 */';
 put '%if %mf_mval(_CLIENTAPP)=%str(SAS Studio) %then %do;';
 put '%let a=%mf_mval(_CLIENTVERSION);';
 put '%let b=%scan(&a,1,.);';
 put '%if %eval(&b >2) %then %do;';
 put '&b';
 put '%end;';
 put '%else 0;';
 put '%end;';
 put '%else 0;';
 put '%end;';
 put '%else %if &switch=VIYARESTAPI %then %do;';
 put '%mf_trimstr(%sysfunc(getoption(servicesbaseurl)),/)';
 put '%end;';
 put '%mend mf_getplatform;';
 put '%macro mf_abort(mac=mf_abort.sas, msg=, iftrue=%str(1=1)';
 put ')/des=''ungraceful abort'' /*STORE SOURCE*/;';
 put '%if not(%eval(%unquote(&iftrue))) %then %return;';
 put '%put NOTE: ///  mf_abort macro executing //;';
 put '%if %length(&mac)>0 %then %put NOTE- called by &mac;';
 put '%put NOTE - &msg;';
 put '%abort;';
 put '%mend mf_abort;';
 put '/** @endcond */';
 put '%macro mf_getuniquefileref(prefix=_,maxtries=1000,lrecl=32767);';
 put '%local rc fname;';
 put '%if &prefix=0 %then %do;';
 put '%let rc=%sysfunc(filename(fname,,temp,lrecl=&lrecl));';
 put '%if &rc %then %put %sysfunc(sysmsg());';
 put '&fname';
 put '%end;';
 put '%else %do;';
 put '%local x len;';
 put '%let len=%eval(8-%length(&prefix));';
 put '%let x=0;';
 put '%do x=0 %to &maxtries;';
 put '%let fname=&prefix%substr(%sysfunc(ranuni(0)),3,&len);';
 put '%if %sysfunc(fileref(&fname)) > 0 %then %do;';
 put '%let rc=%sysfunc(filename(fname,,temp,lrecl=&lrecl));';
 put '%if &rc %then %put %sysfunc(sysmsg());';
 put '&fname';
 put '%return;';
 put '%end;';
 put '%end;';
 put '%put unable to find available fileref after &maxtries attempts;';
 put '%end;';
 put '%mend mf_getuniquefileref;';
 put '%macro mfv_getpathuri(filepath';
 put ')/*/STORE SOURCE*/;';
 put '%mf_abort(';
 put 'iftrue=(&syscc ne 0),';
 put 'msg=Cannot enter &sysmacroname with syscc=&syscc';
 put ')';
 put '%local fref rc path name var /* var is used to avoid delete timing issue */;';
 put '%let fref=%mf_getuniquefileref();';
 put '%let name=%scan(&filepath,-1,/);';
 put '%let path=%substr(&filepath,1,%length(&filepath)-%length(&name)-1);';
 put '%if %sysfunc(filename(fref,,filesrvc,folderPath="&path" filename="&name"))=0';
 put '%then %do;';
 put '%let var=_FILESRVC_&fref._URI;';
 put '%str(&&&var)';
 put '%let rc=%sysfunc(filename(fref));';
 put '%symdel &var;';
 put '%end;';
 put '%else %do;';
 put '%put &sysmacroname: did not find &filepath;';
 put '%let syscc=0;';
 put '%end;';
 put '%mend mfv_getpathuri;';
 put '* SAS Macros end;';
 put '* SAS Includes start;';
 put '* SAS Includes end;';
 put '* Binary Files start;';
 put '* Binary Files end;';
 put '* Test start;';
 put '/**';
 put '@file';
 put '@brief calling JES API for response';
 put '<h4> SAS Macros </h4>';
 put '@li mf_getapploc.sas';
 put '@li mf_getplatform.sas';
 put '@li mfv_getpathuri.sas';
 put '**/';
 put '/* get app location */';
 put '%let jobloc=%mf_getapploc(&_program)/jobs/demo/response;';
 put '/* get uri of the job */';
 put '%put %mfv_getpathuri(&jobloc);';
 put '/* prepare the job code */';
 put 'filename ft15f001 temp;';
 put 'parmcards4;';
 put '{';
 put '"name": "exec-startupservice",';
 put '"description": "Powered by SASjs",';
 put '"jobDefinition": {';
 put '"creationTimeStamp": "2025-06-05T14:39:15.607Z",';
 put '"modifiedTimeStamp": "2025-06-05T14:39:15.607Z",';
 put '"createdBy": "allan@4gl.io",';
 put '"modifiedBy": "allan@4gl.io",';
 put '"version": 2,';
 put '"id": "57477668-288d-4699-8f51-e85483f12851",';
 put '"name": "startupservice",';
 put '"type": "Compute",';
 put '"parameters": [';
 put '{';
 put '"version": 1,';
 put '"name": "_addjesbeginendmacros",';
 put '"defaultValue": "false",';
 put '"type": "CHARACTER",';
 put '"required": false';
 put '}';
 put '],';
 put '"code": "filename _webout filesrvc parenturi=\"&SYS_JES_JOB_URI\" name=\"_webout.json\";data;file _webout;put ''gm'';run;"';
 put '},';
 put '"arguments": {';
 put '"_contextName": "SAS Job Execution compute context",';
 put '"_program": "$APPLOC",';
 put '"_webin_file_count": 0,';
 put '"_OMITJSONLISTING": false,';
 put '"_OMITJSONLOG": false,';
 put '"_OMITSESSIONRESULTS": false,';
 put '"_OMITTEXTLISTING": false,';
 put '"_OMITTEXTLOG": false';
 put '}';
 put '}';
 put ';;;;';
 put '/* swap out with currently deployed job location */';
 put 'filename body temp;';
 put 'data _null_;';
 put 'infile ft15f001;';
 put 'file body;';
 put 'input;';
 put 'length new $1000;';
 put 'if index(_infile_,''$APPLOC'') then do;';
 put 'new=tranwrd(_infile_,''$APPLOC'',"&jobloc");';
 put 'put new;';
 put 'end;';
 put 'else put _infile_;';
 put 'run;';
 put 'data _null_;';
 put 'infile body;';
 put 'input;';
 put 'putlog _infile_;';
 put 'run;';
 put 'options noquotelenmax;';
 put '%let base_uri=%mf_getplatform(VIYARESTAPI);';
 put 'filename f1 temp;';
 put 'proc http method=''POST'' in=body out=f1 oauth_bearer=sas_services';
 put 'url="&base_uri/jobExecution/jobs"';
 put 'ct="application/json";';
 put 'debug level=1;';
 put 'headers "Accept"="application/json";';
 put 'run;';
 put 'data _null_;';
 put 'infile f1;';
 put 'input; putlog _infile_;';
 put 'run;';
 put 'libname json1 JSON fileref=f1;';
 put 'data _null_;';
 put 'set json1.links;';
 put 'putlog (_all_)(=);';
 put 'if rel=''self'' and method=''GET'' then do;';
 put 'call symputx(''href'',quote(cats("&base_uri",href)),''l'');';
 put 'end;';
 put 'run;';
 put '/* give the job a chance to finish */';
 put '%put going to sleep at: %sysfunc(datetime(),datetime19.);';
 put '%let rc=%sysfunc(sleep(10,1));';
 put '%put waking up at: %sysfunc(datetime(),datetime19.);';
 put '/* GET the results */';
 put 'filename f2 temp;';
 put 'proc http method=''GET'' in=body out=f2 oauth_bearer=sas_services';
 put 'url=&href;';
 put 'debug level=1;';
 put 'headers "Accept"="application/json";';
 put 'run;';
 put 'data _null_;';
 put 'infile f2;';
 put 'input; putlog _infile_;';
 put 'run;';
 put 'libname json2 JSON fileref=f2;';
 put 'proc datasets lib=json2 details;';
 put 'quit;';
 put 'data results;';
 put 'set json2.results;';
 put 'putlog (_all_)(=);';
 put 'run;';
 put '/**';
 put 'if you open the log from the above results, you can extract the';
 put '_webout that WAS executed, and it DOES persist';
 put 'It''s just not returned with the job results!!!';
 put '**/';
 put '* Test end;';

run;
%mv_createwebservice(path=&appLoc/&path, name=&service, code=sascode,replace=yes)
filename sascode clear;



* BuildTerm start;
/**
  @file
  @brief <Your brief here>
  <h4> SAS Macros </h4>
**/
%put;%put;
%put Open the above url in a browser with %str(&)_DEBUG=2477 at the end;
%put;%put;%put;
* BuildTerm end;