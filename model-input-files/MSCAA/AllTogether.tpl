//AllTogether, Attempt 136
     //Input CB as 3darray so varies annually
//Function Details:
//  data_section = data_parameter_sections_combined, 11-26-11
//  parameter_section = data_parameter_sections_combined, 11-23-11
//  procedure to top_of_main sections = MSP Version, 11-23-11
//  calc_initial_states = MSP Version, 11-23-11
//  calc_suitability = MSP Version, 11-23-11
//  calc_predation_mortality = 11-26-11 (CB vary by year)
//  calc_pop_dy = MSP Version, 02-08-10
//  calc_survey_abundance = 10-19-10 Version
//  calc_biomass_penalty = 09-14-09 Version
//  calc_year1_constraint = 06-24-09 Version
//  calc_recruitment_penalty = 12-20-09 Version
//  calc_food_habits = 02-08-10 Version (works with new Rsim input files)
//  objective_function through final section = MSP Version, 11-23-11
//  report_section = MSP Version, 11-23-11

//Debug statements
  //  0's = Main body of program
      //  =  1:  Exits after data section
      //  =  2:  Exits after parameter section
      //  =  3:  Exits at end of procedure section
      //  =  4:  Prints checkpoints after each function in the procedure section, except those within the year loop
      //  =  5:  Prints checkpoints after each function within the year loop
      //  =  6:  Prints parameter estimates after each iteration
      //  =  7:  Prints pop dy variables after each iteration
      //  =  8:  Prints trophic matrices at end of year loop, then exits
      //  =  9:  Prints out predicted indices that go into the objective function after each iteration
  //10's = Initial states function
      //  = 10:  Outputs food-selection parameters at the end of the function and then exits
      //  = 11:  Outputs food selection paramters at the end of each iteration
      //  = 12:  Outputs fishery and survey selectivity matrices at the end of the function and then exits
      //  = 13:  Outputs abundance and biomass arrays and then exits
      //  = 14:  Outputs Yr1, Age1 and iFt matrices to ensure 'means + devt'ns' parameterized correctly and then exits
  //20's = Suitability function
      //  = 20:  Output at end of function
      //  = 21:  Output at end of function, then exits
      //  = 22:  Outputs suitability and scaled suitability for each predator sp and then exits
      //  = 23:  Outputs Eta, Sigma, WtRatio, G for pred, prey combos and then exits
  //30's = Predation mortality function
      //  = 30:  Prints checkpoints throughout the function
      //  = 31:  Prints intermediate arrays (Avail, scAv, Consum) for each predator sp and then exits
      //  = 32:  Prints intermediate arrays for each prey sp (Consum, sumCon) and then exits
      //  = 33:  Prints sumCon and B right before M2 is calculated, and M2 after it is calculated
      //  = 34:  Debug 31 but does not exit
      //  = 35:  Prints nf and Other Food in every year
  //40's = Population dynamics function
      //  = 40:  Outputs N, C_hat and Cprop_hat at end of function
      //  = 41:  Outputs N, C_hat and Cprop_hat at end of function and then exits
      //  = 42:  Outputs mortality components for each species in each year and exits at end of year loop after trophic =1
      //  = 43:  Outputs mortality components at end of function and then exits
  //50's = Survey abundance function
      //  = 50:  Prints intermediate arrays for species where survey data is one contiguous time series and then exits
      //  = 51:  Prints intermediate arrays for species where the survey data is split into multiple segments and then exits
      //  = 52:  Prints predicted q, FICs, FIC_hat and N for each species and then exits
      //  = 53:  Prints estimated q matrix at the end of each iteration
  //60's = Log likelihood function
      //  = 60: Prints checkpoints after each likelihood component
      //  = 61: Prints checkpoints for multinomial components within the year loop
      //  = 62: Prints predicted and log predicted indices for TotC and TotFIC
      //  = 63: Prints predicted and log predicted indices for Cprop
      //  = 64: Prints predicted and log predicted indices for Sprop
      //  = 65: FHprop, when added
      //  = 66: Prints summary of objective function components at end of each iteration
  //70's = Food habits function
      //  = 70:  Prints Avpd (scAv) and FHtmp for each predator, year and then exits
      //  = 71:  Prints Avpd, Avpy for each prey species within Avpd, the colsum of Avpy, and FHtmp; exits after all predator species
      //  = 72:  Prints bin years, FHtmp, FHsum, and average FH, FH_hat, for each FH bin, and exits after all predator species
      //  = 73:  Prints total %W for each pred age, summed across prey sp, for each pred sp and FH bin.  This value should always == 1.
  //80's = Penalty functions
      // = 80: Biomass penalty function: Prints pre- and post- B for every species and year, regardless of whether a penalty is imposed
      // = 81: Biomass penalty function: Prints pre- and post- biomass and assorted arrays when biomass falls below threshold
      // = 82: Yr1 penalty function: Prints avgZ, thYr1, Yr1 and Ypen arrays and then exits at end of function
      // = 83: Recruitment penalty function: Prints Age1 parameter estimates, calculated CV and recruitment penalty


DATA_SECTION

  int debug;                          //Debugger switch
  !!debug = 0;

  int i; int j; int t; int pd; int py; int b; int pdA; int pyA; int a; int seg;
  int nf;                                //Number of function evaluations/iterations
    !!nf = 1;
  init_int sim;                        //Counter for simulation number
  init_int trophic;                   //Switch for trophic calculations

  init_int nsp;                        //Number of species
  int nsp2;                            //4darray index; init_int declarations (i.e. nsp) will not work
    !!nsp2 = nsp;
  init_int nFIC;                      //Number of trawl survey datasets
  int nFIC2;
    !!nFIC2 = nFIC;

  init_number o;                    //Tiny number for calculation of lognormal distributions
  init_number p;                    //Tiny number for calculation of multinomial residuals

  int tphase;                          //first phase where trophic calculations are conducted
  init_int nint;                        //Number of species interactions
  int nrho;                             //Number of rho parameters that need to be estimated
  init_int binsize;                   //Number of years in each food habits (FH) bin; within each bin, the average FH is calculated
  int nbins;                           //Number of FH bins in the FH data
  init_number EcoB;             //Total ecosystem biomass
  init_number Owt;               //Weight for the other food penalty term, Open
  int nOpen;                         //Number of iterations where Open (within calc M2 function) was > 0
  int lOpen;                          //Last iteration where Open was > 0
 
  init_ivector fyr(1,nsp);        //First year in model; species-specific
  int minfyr;                          //Across all species, the earliest fyr
    !!minfyr = min(fyr);
  init_ivector lyr(1,nsp);         //Last year in model; species-specific
  int maxlyr;                         //Across all species, the latest lyr
    !!maxlyr = max(lyr);

  init_int FHfyr;                    //First year of the FH data
  int FHfyr2;                        //4darray index
    !!FHfyr2=FHfyr;
  init_int FHlyr;                    //Last year of the FH data
  int FHlyr2;                        //4darray index
    !!FHlyr2=FHlyr;
  !!nbins=ceil((double(FHlyr)-double(FHfyr)+1.)/double(binsize));

  ivector nyr(1,nsp);              //Total number of years; species-specific
    !!nyr = 1 + lyr - fyr;
  ivector binfyr(1,nbins);        //First year in each FH bin
  ivector binlyr(1,nbins);        //Last year in each FH bin

  int sage;                              //Total number of age classes summed across all species
  init_ivector nage(1,nsp);       //Number of age classes for each species
    !!sage = sum(nage);
  ivector fage(1,nsp);              //Vector of the first age class for each species; used in trophic calcs
    !!for (i=1;i<=nsp;i++) {fage(i)=sum(nage(1,i))-(nage(i)-1);}
  ivector lage(1,nsp);              //Vector of the last age class for each species; used in trophic calcs
    !!for (i=1;i<=nsp;i++) {lage(i)=sum(nage(1,i));}

  init_int nMseg;                    //Number of segments of different natural mortality (M1) rates
  init_ivector nseg(1,nsp);      //Number of segments for the FIC data; for each segment, a separate q is estimated
  vector segtmp(1,nsp);
    !!segtmp = nseg;  for (i=1; i<=nsp; i++) {if (segtmp(i) > 1) {segtmp(i)--;}}
         //#breaks = FIC segments -1 unless there is only one segment, in which case #breaks= 1

  ivector nFt(1,nsp);                       //Number of fishing mortality parameters to be estimated
  ivector Fct(1,nsp);                       //Counter to assign initial F parameters, iFt, to the correct years
  init_ivector agePR(1,nsp);           //First age when partially recruited to the fishery; used to avoid hitting parameter bounds
  init_ivector ageFR(1,nsp);           //Age of full recruitment to the fishery
  init_ivector ficFR(1,nsp);             //Age of full recruitment to the fishery-independent survey, FIC
                                                     //ficFR == 0 for species that never become fully recruited to the survey
  init_vector FICs_lage(1,nsp);      //Survey selectivity coefficient for the last age class; used to anchor the curve
                                                     //FICs_lage == 1 for species that become fully recruited to the survey
  ivector nsel(1,nsp);                     //Number of fishery selectivity parameters estimated per species
  ivector FICnsel(1,nsp);               //Number of survey selectivity parameters estimated per species

  init_ivector aAge1ph(1,nsp);       //aAge1 estimation phase
  init_ivector aFtph(1,nsp);            //aFt estimation phase
  init_ivector dAge1ph(1,nsp);       //dAge1 estimation phase
  init_ivector dFtph(1,nsp);            //dFt estimation phase
  init_ivector ficph(1,nsp);             //Survey selectivity, FICsel, estimation phase
  init_ivector fishph(1,nsp);            //Fishery selectivity, agesel, estimation phase
  init_ivector Yr1ph(1,nsp);           //Yr1 estimation phase

  init_vector pred(1,nint);      //Vector identifying the predator species for each interaction
  init_vector prey(1,nint);      //Vector identifying the prey species for each interaction
  ivector rhosp(1,nsp);          //The number of estimable rhos for each species (rhosp>0 if species consumes other modeled species)
    !!rhosp.initialize();
    !!for (j=1; j<=nint; j++) {rhosp(pred(j))++;}
    !!nrho = sum(rhosp);       //Total number of estimable rho parameters
  init_ivector Rhoph(1,nrho);//Phase in which each rho is estimated
  ivector pdct(1,nsp);           //The number of interactions in which a particular species is a predator

  vector CPideal(1,nsp);                //Best possible value for the multinomial residuals of CAA proportions
  vector FHideal(1,nsp);                //Best possible value for the multinomial residuals of FH proportions

  init_ivector M1yr(1,nMseg);       //The first year in each M1 segment, including the first segment

  //Data weightings
  init_vector TCwt(1,nsp);            //Total annual commercial catch in weight
  init_vector CPwt(1,nsp);            //Commercial catch proportions at age
  init_vector Bwt(1,nsp);              //Weight for Biomass Penalty Term, Bpen
  init_vector Ywt(1,nsp);              //Weight for Yr1 Penalty Term, Ypen)
  init_vector Rwt(1,nsp);              //Weight for Recruitment Penalty Term, Rpen
  init_vector FHwt(1,nsp);           //Food habits proportions by weight

  init_vector Bthres(1,nsp); //Biomass threshold used in the penalty function to avoid B == 0, which would cause M2 calc to crash
  ivector nBpen(1,nsp);      //Number of iterations where Bpen (within the calc_biomass_penalty function) was > 0
  ivector lBpen(1,nsp);       //Last iteration where the Bpen was > 0
  init_vector Rthres(1,nsp);//Threshold for the coefficient of variation of recruitment
  init_matrix TSwt(1,nsp,1,nFIC); //Total annual survey catch in number/tow
  init_matrix SPwt(1,nsp,1,nFIC); //Survey catch proportions-at-age

  init_matrix iM2(1,nsp,1,nage);              //M2 estimates (from previous model run) to use in non-FH years
  init_matrix Eta(1,nsp,1,nsp);                 //Preferred predator/prey weight ratio
  init_matrix Sig1(1,nsp,1,nsp);               //Variance in predator/prey weight ratios < eta
  init_matrix Sig2(1,nsp,1,nsp);               //Variance in predator/prey weight ratios > eta
  
  matrix SPideal(1,nsp,1,nFIC);              //Best possible value for the multiomial residuals of Survey, FIC, proportions  
  init_imatrix FICfage(1,nsp,1,nFIC);      //First age captured for each trawl survey
  init_imatrix FIClage(1,nsp,1,nFIC);      //Last age captured for each trawl survey

  //Time series data
  init_matrix FICmon(1,nsp,1,nFIC);                //Month correpsonding to each trawl survey
  init_matrix FICyr(1,nsp,1,segtmp);                 //Beginning year of FIC segments for species where nseg > 1; if nseg == 1, FICyr = 0
  init_matrix TotC(1,nsp,fyr,lyr);                       //Total commercial catch; metric tons
  matrix sumC(1,nsp,fyr,lyr);                            //Sum of obs CAA; Used to determine if age samples were taken in a particular yr
  init_3darray Wt(1,nsp,fyr,lyr,1,nage);            //Average individual weight-at-age; kg
  init_3darray C(1,nsp,fyr,lyr,1,nage);              //Commercial Catch-at-Age (CAA); 10^6 number
  3darray Cprop(1,nsp,fyr,lyr,1,nage);             //Proportion-at-age of the commercical catch, CAA
  init_3darray TotFIC(1,nsp,1,nFIC,fyr,lyr);    //Total annual survey catch; Necessary for years where age samples were not taken
  3darray sumFIC(1,nsp,1,nFIC,fyr,lyr);         //Sum of observed FIC; Used to determine if age samples were taken in a particular yr  

  init_3darray M1seg(1,nsp,1,nMseg,1,nage);  //Natural mortality rates or each segment, Mseg
  3darray M1(1,nsp,fyr,lyr,1,nage);                 //Natural mortality rate expanded to full 3darray
  init_3darray CB(1,nsp,fyr,lyr,1,nage);           //Age-specific consumption:biomass ratios

  init_4darray FIC(1,nsp2,1,nFIC2,fyr,lyr,1,nage);   //Fishery-Independent trawl survey Catch-at-age (FIC); number/tow
  4darray Sprop(1,nsp2,1,nFIC2,fyr,lyr,1,nage);       //Proportion-at-age of the survey catch, FIC

  init_4darray FH(1,nsp2,1,nbins,1,nage,1,nsp2+1);            //Food habits: Observed predator stomach contents, proportions by wt
                                                                                             //Observed FH, summed over all prey ages, including OFood
  4darray WtRatio(1,nsp,FHfyr2,FHlyr2,1,sage,1,nage);     //Observed predator:prey weight ratio; pred X year X prey X pred age
  4darray G(1,nsp,FHfyr2,FHlyr2,1,sage,1,nage);               //Predator size preference 
  
  init_int eof;

	LOCAL_CALCS
    
    nFt.initialize();  nOpen = 0;  lOpen = 0;
    Cprop.initialize();  Sprop.initialize();  SPideal.initialize();  CPideal.initialize();
    nBpen.initialize();  lBpen.initialize();
    FHideal.initialize();  pdct.initialize();
    G.initialize();

    //The first phase where trophic calculations are conducted
    tphase = posmin(Rhoph);

    //The first and last years of each FH bin:
    binfyr.fill_seqadd(FHfyr,binsize);
    binlyr = binfyr + binsize-1;
    binlyr(nbins) = FHlyr;

    //Number of fishery selectivity parameters for each species:
    nsel =  ageFR-agePR;

    for (i = 1; i<=nsp; i++)
      {
      //Number of survey selectivity parameters for each species
      if (ficFR(i) == 0)  {FICnsel(i) = nage(i) - 1;}
      else {FICnsel(i) = ficFR(i) - 1;}

      sumC(i) = rowsum(C(i));        //Observed total catch in each year
      int M1ct = 1;                         //Count to correctly fill M1 array

      for (t = fyr(i); t<=lyr(i); t++)
        {
        //Fill M1 3darray
		    if (M1ct < nMseg && t == M1yr(M1ct + 1)) {M1ct ++;}
   		  M1(i,t) = M1seg(i,M1ct);

        //Determine number of F parameters to initialize; only initialize if fish were caught 
        if (TotC(i,t) > 0.) {nFt(i)++;}

        //Calculate the CAA proportions if the total catch in that year is not zero
        if (sumC(i,t) != 0) {Cprop(i)(t) = C(i)(t)/sumC(i)(t);}

        //Multinomial residuals for the CAA data for a perfect fit
        dvector CPobs = CPwt(i)*elem_prod( Cprop(i,t)+p , log(Cprop(i,t)+p) );
        CPideal(i) -= sum(CPobs);
        }  //end of year loop

      //Survey datasets
      for (j = 1; j<=nFIC; j++)
        {
        int bage = FICfage(i,j);
        int eage = FIClage(i,j);
        sumFIC(i,j) = colsum( trans(FIC(i,j)).sub(bage,eage) );  //Observed total survey catch in each year
        for (t = fyr(i); t<=lyr(i); t++)
          {
          if (sumFIC(i,j,t) != 0)
            {
            //Calculating the FIC proportions
            Sprop(i)(j)(t).sub(bage,eage) = FIC(i)(j)(t).sub(bage,eage)/sumFIC(i)(j)(t);
            //Multinomial residuals for the CAA data for a perfect fit
            dvector SPobs =  SPwt(i)(j)*elem_prod( Sprop(i,j,t).sub(bage,eage)+p , log(Sprop(i,j,t).sub(bage,eage)+p) );
            SPideal(i)(j) -= sum(SPobs);
            }  //end of sumFIC If Statement
          }  //end of year loop
        }  //end of nFIC loop

      if (rhosp(i) > 0) //  Indicates whether a species is a predator
        {
        for (b=1; b<=nbins; b++)
          {
          for (pdA=1; pdA<=nage(i); pdA++)
            {
            //Multinomial residuals for the FH data for a perfect fit
            if (sum(FH(i,b,pdA))<=100)  //Missing data are marked with 9999, making the rowsum >> 100
              {
              dvector FHobs = FHwt(i)*elem_prod( FH(i,b,pdA)+p , log(FH(i,b,pdA)+p) );
              FHideal(i) -= sum(FHobs);
              }
            }  //end of predator age loop
          }  //end of FH year bin loop
        }  //end of rhosp if statement

      }  //end of species loop

    //Calculate 1) predator:prey weight ratios, WtRatio, and 2) predator size-preference, G, for years with FH data
    for (pd=1; pd<=nsp; pd++)
      {
      for (py=1; py<=nsp; py++)
        {
        for (t=FHfyr; t<=FHlyr; t++)  //Note, this used to be FHfyr2 and FHlyr2 so make sure the change is aok
          {
          //Weight ratios
          dmatrix Wttmp = outer_prod(1/Wt(py,t),Wt(pd,t));
          Wttmp.rowshift(fage(py));  //Shifting the row index to the first age class for each species
          WtRatio(pd)(t).sub(fage(py),lage(py)) = Wttmp;

          //Size-preference
          //G(pd,t,py) = exp(-square( log(WtRatio)-Eta )/(2.*square(Sig)) );          
          if (Sig1(pd,py)!=-99)  // Does not matter which Sig is used
            {
            //Set Sigtmp depending on whether WtRatio is < or > than Eta
            dmatrix Ratiosub = log(WtRatio(pd)(t).sub(fage(py),lage(py)));
            dmatrix Sigtmp(fage(py),lage(py),1,nage(pd));
            Sigtmp.initialize();
            for (pyA=fage(py); pyA<=lage(py); pyA++)  {
              for (pdA=1; pdA<=nage(pd); pdA++)  {
                if (Ratiosub(pyA,pdA) < Eta(pd,py))  {Sigtmp(pyA,pdA) = Sig1(pd,py);}
                else                                                    {Sigtmp(pyA,pdA) = Sig2(pd,py);}
                }  // end of prey age loop
              }  // end of predator age loop
            if(debug == 23 | debug == 24)  
              {cout<<"Eta: "<<Eta(pd,py)<<"\nWtRatio.sub\n"<<Ratiosub<<"\nSigtmp\n"<<Sigtmp<<endl<<endl;}
            //Calculate size preference
            dmatrix Gtmp = square( Ratiosub - Eta(pd,py) );
            G(pd)(t).sub(fage(py),lage(py)) = exp(elem_div(-Gtmp , 2.*square(Sigtmp)));
            if(debug == 23) {cout<<"G(pd)(t).sub(fage(py),lage(py))\n"<<G(pd)(t).sub(fage(py),lage(py))<<endl<<endl;}
            }  //end of sigma If statement
          
          }  //end of FHyr loop
        }  //end of prey species loop
      }  //end of predator species loop

  if (debug == 1)
    {
    cout<<"trophic\n"<<trophic<<endl;
    cout<<"nint\n"<<nint<<endl;
    cout<<"nrho\n"<<nrho<<endl;
    cout<<"o\n"<<o<<endl;
    cout<<"aAge1ph\n"<<aAge1ph<<endl;
    cout<<"dAge1ph\n"<<dAge1ph<<endl;
    cout<<"ficph\n"<<ficph<<endl;
    cout<<"Yr1ph\n"<<Yr1ph<<endl;
    cout<<"Rhoph\n"<<Rhoph<<endl;
    cout<<"tphase\n"<<tphase<<endl;
    cout<<"binsize\n"<<binsize<<endl;
    cout<<"nbins\n"<<nbins<<endl;
    cout<<"EcoB\n"<<EcoB<<endl;
    cout<<"nage\n"<<nage<<endl;
    cout<<"ageFR\n"<<ageFR<<endl;
    cout<<"nsel\n"<<nsel<<endl;
    cout<<"nFIC\n"<<nFIC<<endl;
    cout<<"FICfage\n"<<FICfage<<endl;
    cout<<"FIClage\n"<<FIClage<<endl;
    cout<<"FICmonth\n"<<FICmon<<endl;
    cout<<"ficFR\n"<<ficFR<<endl;
    cout<<"FICs_lage\n"<<FICs_lage<<endl;
    cout<<"FICnsel\n"<<FICnsel<<endl;
    cout<<"M1seg\n"<<M1seg<<endl;
    cout<<"nseg\n"<<nseg<<endl;
    cout<<"segtmp\n"<<segtmp<<endl;
    cout<<"FICyr\n"<<FICyr<<endl;
    cout<<"binfyr\n"<<binfyr<<endl;
    cout<<"binlyr\n"<<binlyr<<endl;
    cout<<"pred\n"<<pred<<endl;
    cout<<"prey\n"<<prey<<endl;
    cout<<"rhosp\n"<<rhosp<<endl;
    cout<<"CB\n"<<CB<<endl;
    cout<<"Eta\n"<<Eta<<endl;
    cout<<"Sig1\n"<<Sig1<<endl;
    cout<<"Sig2\n"<<Sig2<<endl;
    cout<<"C\n"<<C<<endl;
    cout<<"FIC\n"<<FIC<<endl;
    cout<<"TotC\n"<<TotC<<endl;
    cout<<"sumFIC\n"<<sumFIC<<endl;
    cout<<"TotFIC\n"<<TotFIC<<endl;
    cout<<"Cprop\n"<<Cprop<<endl;
    cout<<"Sprop\n"<<Sprop<<endl;
    cout<<"nFt\n"<<nFt<<endl;
    cout<<"TSwt\n"<<TSwt<<endl;
    cout<<"SPwt\n"<<SPwt<<endl;
    cout<<setprecision(10)<<"FH\n"<<FH<<endl;
    cout<<setprecision(10)<<"FHideal\n"<<FHideal<<endl;
    cout<<"SPideal\n"<<SPideal<<endl;
    cout<<"G\n"<<G<<endl;
    cout<<"eof\n"<<eof<<endl;
    }

  if(eof != 54321) {cout<<"Stop, data not inputted correctly"<<endl<<"eof: "<<eof<<endl; exit(1);}

  if (debug == 1) {cout<<"\nManually exiting at end of data section..."<<endl;  exit(-1);}

	END_CALCS


PARAMETER_SECTION
  objective_function_value ofv;
  number ofv_ideal;         //Total objective function value, subtracting out the ideal multinomial values
  number Open;              //Penalty function for Other Food (within M2 fx)
  number tOpen;             //Total Other Food penalty

  vector Bpen(1,nsp);      //Penalty function for biomass and calculation of M2
  vector tBpen(1,nsp);     //Total Biomass penalty for each species
  vector Ypen(1,nsp);      //Penalty function for Year 1 abundances
  vector tYpen(1,nsp);     //Total Yr1 penalty for each species
  vector Rpen(1,nsp);      //Penalty function for the CV of recruitment
  vector tRpen(1,nsp);     //Total Recruitment penalty for each species
  vector Devs(1,nsp);      //Square of summed deviations for each deviation initial parameter
  vector TCres(1,nsp);     //Objective function component: Total commercial catch
  vector CPmulti(1,nsp);  //Objective function component: Catch-at-age proportions
  vector FHmulti(1,nsp);  //Objective function component: Predator diet proportions
  vector ofvsp(1,nsp);          //Total objective function value for each species
  vector ofvsp_ideal(1,nsp); //Total objective function value for each species, subtracting out the ideal multinomial values

  init_bounded_number_vector iRho(1,nrho,-30,100,Rhoph); //General prey species preference coefficient
  init_number_vector aAge1(1,nsp,aAge1ph);                        //Average annual recruits, log space
  init_bounded_number_vector aFt(1,nsp,-20,10,aFtph);       //Average annual fishing mortality rates; averaged over yrs where TotC > 0, log space

  init_bounded_vector_vector idAge1(1,nsp,fyr+1,lyr,-15,5,dAge1ph); //Annual deviations in recruits, log space
  init_bounded_vector_vector idFt(1,nsp,1,nFt,-5,5,dFtph);                  //Annual deviations in species-specific Fs in yrs where TotC > 0, log space
  init_bounded_vector_vector iFICsel(1,nsp,1,FICnsel,0,1,ficph);         //Age-specific survey selectivity parameters
  init_bounded_vector_vector iagesel(1,nsp,1,nsel,0,1,fishph);               //Age-specific fishery selectivity/vulnerability parameters
  init_vector_vector iYr1(1,nsp,1,nage,Yr1ph);                                     //Year 1 age-specific N's, log space

  matrix dAge1(1,nsp,fyr+1,lyr);     //Annual deviations in recruits (necessary because initially declared as vector_vector)
  matrix Age1(1,nsp,fyr+1,lyr);       //Annual recruits, 'means + deviations', log space
  matrix dFt(1,nsp,1,nFt);               //Annual deviations in Fs (necessary because initially declared as vector_vector)
  matrix iFt(1,nsp,1,nFt);                //Fishing mortality rates, 'means + deviations',  in yrs where TotC > 0, log space
  matrix Ft(1,nsp,fyr,lyr);                //Annual species-specific fishing mortality rates

  matrix FICsel(1,nsp,1,FICnsel);   //Age-specific survey selectivity parameters (necessary because initially declared as vector_vector)
  matrix agesel(1,nsp,1,nsel);          //Age-specific fishery selectivity/vulnerability parameters (necessary because initially declared as vector_vector)
  matrix s(1,nsp,1,nage);                //Fishery selectivity/vulnerability at age
    !!s = 1;                                     //Set s = 1 so that oldest ages will be fully recruited when the initial parameters are added to s
  matrix FICs(1,nsp,1,nage);          //Survey selectivity at age
    //Set selectivity coefficients to the inputted values for the final ages, FICs_lage, to anchor the curve
    //For species that become fully recruited to the survey, FICs_lage = 1; for others it represents the selectivity of the last age class 
    !!for (i=1; i<=nsp; i++)  {FICs(i)(FICnsel(i)+1,nage(i)) = FICs_lage(i);}
  matrix Yr1(1,nsp,1,nage);            //Year 1 age-specific N's, log space

  matrix TotC_hat(1,nsp,fyr,lyr);     //Total commerical catch *in weight*; summed over ages
  matrix sumC_hat(1,nsp,fyr,lyr);    //Total commercial catch *in numbers*; summed over ages; for calc of CAA proportions
  matrix TCresid(1,nsp,fyr,lyr);       //Residuals of total commercial catch *in weight*; summed over ages
  matrix TSresA(1,nsp,1,nFIC);      //Objective function component: Total survey catch, method A
  matrix TSresB(1,nsp,1,nFIC);      //Objective function component: Total survey catch, method B
  matrix SPmulti(1,nsp,1,nFIC);      //Objective function component: Survey catch-at-age proportions

  //Food selection parameters; converting initial parameters to [predator species x prey species] matrices
  matrix Rho(1,nsp,1,nsp+1);                       //There are "nsp+1" prey species to account for "other food"

  3darray q(1,nsp,1,nFIC,1,nseg);                 //Survey catchability; age-invariant
  3darray toteps(1,nsp,1,nFIC,fyr,lyr);          //Residuals of total survey catch, TotFIC, Calculated by Method A
  3darray TotFIC_hat(1,nsp,1,nFIC,fyr,lyr); //Total survey (FIC) catch in number/tow; summed over ages
  3darray TSresid(1,nsp,1,nFIC,fyr,lyr);       //Residuals of total survey catch, TotFIC, Calculated by Method B

  3darray N(1,nsp,fyr,lyr,1,nage);               //Abundance-at-age
  3darray B(1,nsp,fyr,lyr,1,nage);               //Biomass-at-age = N * Wt
  3darray F(1,nsp,fyr,lyr,1,nage);               //Predicted realized F-at-age = Ft * s
  3darray Z(1,nsp,fyr,lyr,1,nage);               //Total mortality-at-age
  3darray M2(1,nsp,fyr,lyr,1,nage);            //Predation mortality-at-age
  3darray C_hat(1,nsp,fyr,lyr,1,nage);        //Predicted commercial catch-at-age
  3darray Cprop_hat(1,nsp,fyr,lyr,1,nage);  //Commercial catch proportions-at-age

  3darray sumCon(1,nsp,FHfyr,FHlyr,1,nage); //Total prey consumed by each predator

  4darray FIC_hat(1,nsp2,1,nFIC2,fyr,lyr,1,nage);     //Predicted survey catch, FIC, at age
  4darray Sprop_hat(1,nsp2,1,nFIC2,fyr,lyr,1,nage);  //Survey catch proportions-at-age

  //Trophic 4darrays; all are ragged in either the 3rd or 4th dimension; when the prey dimension = sage+1, it is to account for "other food"
    //Dimensions of following arrays = [pred, year, prey, pred age]
  4darray Nu(1,nsp,FHfyr2,FHlyr2,1,sage+1,1,nage);         //Prey suitability
  4darray Snu(1,nsp,FHfyr2,FHlyr2,1,sage+1,1,nage);        //Scaled prey suitability; scaled across prey speces
    //Dimensions of following arrays = [pred, year, pred age, prey]
  4darray Avail(1,nsp,FHfyr2,FHlyr2,1,nage,1,sage+1);      //Biomass of available prey
  4darray scAv(1,nsp,FHfyr2,FHlyr2,1,nage,1,sage+1);      //Biomass of available prey, scaled across prey species
  4darray Consum(1,nsp,FHfyr2,FHlyr2,1,nage,1,sage+1);  //Biomass of consumed prey
    //Dimensions of following arrays = [pred, FHbin, pred age, prey]
  4darray FH_hat(1,nsp2,1,nbins,1,nage,1,nsp2+1);             //Predicted FH, summed over all prey ages, and including OFood
      //Note: Predicted FH = available prey biomass, summed over year and prey ages, scaled across prey speces

	LOCAL_CALCS
    if (debug == 2)
      {
      cout<<"aAge1\n"<<aAge1<<endl<<"aFt\n"<<aFt<<endl;
      for (i=1; i<=nsp; i++)  {
        cout<<"species: "<<i<<endl;
        cout<<"idAge1\n"<<idAge1(i)<<endl<<"idFt\n"<<idFt(i)<<endl;
        cout<<"iagesel\n"<<iagesel(i)<<endl<<"iFICsel\n"<<iFICsel(i)<<endl;
        cout<<"iYr1\n"<<iYr1(i)<<endl<<endl;
        }
      cout<<"iRho\n"<<iRho<<endl;
      cout<<"\nManually exiting at the end of the parameter section...\n"<<endl;
      exit(-1);
      }
	END_CALCS


PROCEDURE_SECTION

  //Turn on trophic interactions if in the correct phase
  if (current_phase()>=tphase  &&  tphase>0) trophic=1;

  calc_initial_states();  if (debug == 4) {cout<<"completed Initial States"<<endl;}

  for (t = minfyr; t<=maxlyr; t++) 
    {
    if (debug == 5) {cout<<"Year: "<<t<<endl;}
    calc_biomass_penalty();  if (debug == 5) {cout<<"completed Biomass Penalty"<<endl;}
    if (trophic)  //Only calculate trophic parameters if trophic indicator == 1
      {
      if (t>=FHfyr && t<=FHlyr)  //Only loop over years for which data for all species are available
        {
        calc_suitability();  if (debug == 5) {cout<<"completed Suitability"<<endl;}
        calc_predation_mortality();  if (debug == 5) {cout<<"completed Predation Mortality"<<endl;}
        }  //end of FH year loop
      }  //end of trophic if statement
    pop_dynamics();  if (debug == 5) {cout<<"completed Pop Dynamics"<<endl;}
    }  //end of year loop
  if (debug == 4) {cout<<"completed Year loop"<<endl;}

  //Debug suitability (23) and trophic matrices (8)
  if (debug == 23)  {
    int coutyr = FHfyr + 1;  int pdtmp = 1;  int pytmp = 2;
    cout<<"coutyr\n"<<coutyr<<endl<<"pdtmp\n"<<pdtmp<<endl<<"pytmp\n"<<pytmp<<endl
          <<"Eta\n"<<Eta(pdtmp,pytmp)<<endl<<"Sig1\n"<<Sig1(pdtmp,pytmp)<<endl
          <<"Sig2\n"<<Sig2(pdtmp,pytmp)<<endl<<"Rho\n"<<Rho(pdtmp,pytmp)<<endl
          <<"WtRatio\n"<<WtRatio(pdtmp,coutyr).sub(fage(pytmp),lage(pytmp))<<endl
          <<"g.1.coutyr\n"<<G(pdtmp,coutyr).sub(fage(pytmp),lage(pytmp))<<endl
          <<"Nu.1.coutyr\n"<<Nu(pdtmp,coutyr).sub(fage(pytmp),lage(pytmp))<<endl
          <<"Snu.1.coutyr\n"<<Snu(pdtmp,coutyr).sub(fage(pytmp),lage(pytmp))<<endl;
    cout<<"manually exiting...\n"<<endl; exit(-1);  }
  if (debug == 8)
    {cout<<"nsp\n"<<nsp<<endl<<"nage\n"<<nage<<endl<<"FHfyr\n"<<FHfyr<<endl<<"FHlyr\n"<<FHlyr<<endl<<
    "Avail\n"<<Avail<<endl<<"scAv\n"<<scAv<<endl<<"Consum\n"<<Consum<<endl<<"manually exiting...\n"<<endl; exit(-1);}

  calc_survey_abundance();  if (debug == 4) {cout<<"completed Survey Abundance"<<endl;}

  //Calculate FH_hat if trophic indicator = 1
  if (trophic) { calc_food_habits();  if (debug == 4) {cout<<"completed Food Habits"<<endl;} }

  calc_year1_constraint();  if (debug == 4) {cout<<"completed Yr1 Constraint"<<endl;}
  calc_recruitment_penalty();  if (debug == 4) {cout<<"completed Recruitment Penalty"<<endl;}
  calc_negative_loglikelihood();  if (debug == 4) {cout<<"completed Log Likelihood"<<endl;}

  if (debug == 6) {cout<<"Parameter estimates:\n"<<"Yr1\n"<<Yr1<<"\nAge1\n"<<Age1<<"\nagesel\n"<<agesel<<"\nFt\n"<<Ft
                                 <<"\nFICsel\n"<<FICsel<<endl<<endl;
                           if (trophic) {cout<<"\niRho\n"<<iRho<<endl<<endl;}  }
  if (debug == 7) {cout<<"N\n"<<N<<"\nB\n"<<B<<"\nC_hat\n"<<C_hat<<"\nZ\n"<<Z<<"\nM2\n"<<M2<<"\nF\n"<<F<<endl<<endl;}
  if (debug == 9) {cout<<"TotC_hat\n"<<TotC_hat<<"\nTotC\n"<<TotC<<"\nTCresid\n"<<TCresid
                                 <<"\nTotFIC_hat\n"<<TotFIC_hat<<"\nTotFIC\n"<<TotFIC<<"\ntoteps\n"<<toteps
                                 <<"\nCprop_hat\n"<<Cprop_hat<<"\nCprop\n"<<Cprop
                                 <<"\nSprop_hat\n"<<Sprop_hat<<"\nSprop\n"<<Sprop<<endl;
                           if (trophic) {cout<<"FH_hat\n"<<FH_hat<<"\nFH\n"<<FH<<endl;}  }

  nf++;  //Tallys the # of function evaluations
  if (debug == 66 | debug == 4) {cout<<"\nmoving onto next iteration "<<"nf: "<<nf<<endl<<endl;}

  if (debug == 3) 
    {
    cout<<endl<<"manually exiting at end of procedure section...\n"<<endl;
    exit(-1);
    }


GLOBALS_SECTION
  //Including C++ libraries
  #include <stats.cxx>
  #include <klclib.cxx>


RUNTIME_SECTION
  convergence_criteria 1.e-3 ,  1.e-4
  maximum_function_evaluations 80000


TOP_OF_MAIN_SECTION
  arrmblsize = 8000000;  //Increase amount of available dvar memory
  gradient_structure::set_CMPDIF_BUFFER_SIZE(6000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(3000000);


FUNCTION calc_initial_states
  /*
  Function 1) Initializes the differentiable arrays, 2) Calculates fishery recruitment at age (s),
    3)Initializes age-specific abundance and biomass in the first year, and 4) Expands the food-selection 
    initial parameter vectors into matrices [predator x prey species]
  */
  Fct.initialize();                                                     //Counter to determine number of estimable fishing mortality rates
  pdct.initialize();  int rhoct;  rhoct = 0;                   //Counters to determine number of estimable rhos
  N.initialize();  B.initialize();  
  Bpen.initialize();  Ypen.initialize();  Rpen.initialize();  Open.initialize();
  M2.initialize();  Z.initialize();  C_hat.initialize();  FIC_hat.initialize();  TotFIC_hat.initialize();  TotC_hat.initialize();
  Cprop_hat.initialize();  Sprop_hat.initialize();  q.initialize();  toteps.initialize();  TSresid.initialize();
  if (trophic)  {sumCon.initialize();  Nu.initialize();  Snu.initialize();  Avail.initialize();  scAv.initialize();
                     FH_hat.initialize();  Consum.initialize();}
  dAge1.initialize();  Age1.initialize();  dFt.initialize();  iFt.initialize();
  FICsel.initialize();  agesel.initialize();  Yr1.initialize();

  for (j = 1; j<=nint; j++)
    {
    pd = pred(j);
    py = prey(j);
    //If OFood is not in the model
    /*
    if (pdct(pd) == 0)  {Rho(pd,py) = 1;}
    else  {rhoct++;  Rho(pd,py) = mfexp(iRho(rhoct)); }
    */
    //If OFood is in the model:
    /**/
    rhoct++;
    Rho(pd,py) = mfexp(iRho(rhoct));
    /**/
    pdct(pd)++;
    }  //end of species interaction loop

  for (i=1; i<=nsp; i++)
    {
    //Fill matrices with init_vector_vector parameters
    dAge1(i) = idAge1(i);
    dFt(i) =  idFt(i);
    FICsel(i) = iFICsel(i); 
    agesel(i) = iagesel(i);
    Yr1(i) = iYr1(i);

    //If OFood is included in the model, set the rho for Other Food = 1
    Rho(i,nsp+1) = 1;

    //Expand species-specific averages and annual/age-specific deviations into full arrays
    Age1(i) = aAge1(i) + dAge1(i);
    iFt(i) = aFt(i) + dFt(i);

    //Add the fishery selectivity initial parameters to the selectivity matrix; method is a function of agePR
    if (agePR(i) == 1) {s(i)(1,nsel(i)) = agesel(i);}
    else {
      s(i)(1,agePR(i)-1) = 0;
      agesel(i).shift(agePR(i));  //Shift the index to ensure compatible array bounds
      iagesel(i).shift(agePR(i));  //Shift the index to ensure compatible array bounds
      s(i)(agePR(i),agePR(i)+nsel(i)-1) = agesel(i);
           }
    //Add the survey selectivity initial parameters to the survey selectivity matrix
    FICs(i)(1,FICnsel(i)) = FICsel(i);

    //Initializes the populations in the first year
    N(i)(fyr(i)) = mfexp(Yr1(i));                                     //Abundance in first year
    B(i)(fyr(i)) = elem_prod(N(i)(fyr(i)),Wt(i)(fyr(i)));     //Biomass-at-age = N * Wt
    if (debug == 13) {cout<<"sp: "<<i<<endl<<"N(i,fyr)\n"<<N(i)(fyr(i))<<endl<<"Wt(i,fyr)\n"<<Wt(i)(fyr(i))<<endl
                                      <<"B(i,fyr)\n"<<B(i)(fyr(i))<<endl<<endl;}

    }  //end of species loop

  if (debug == 10) {cout<<"Rho\n"<<Rho<<endl<<"pred\n"<<pred<<endl<<"prey\n"<<prey<<endl<<"pdct\n"<<pdct
                                    <<endl<<"rhoct\n"<<rhoct<<endl<<"rhosp\n"<<rhosp<<endl; exit(-1);}
  if (debug == 11) {cout<<"Rho\n"<<Rho<<endl;}
  if (debug == 12) {cout<<"nage\n"<<nage<<endl<<"agePR\n"<<agePR<<endl<<"ageFR\n"<<ageFR<<endl
                                   <<"nsel\n"<<nsel<<endl<<"agesel\n"<<agesel<<endl<<"s\n"<<s<<endl
                                   <<"ficFR\n"<<ficFR<<endl<<"FICs_lage\n"<<FICs_lage<<endl<<"FICnsel\n"<<FICnsel<<endl
                                   <<"FICsel\n"<<FICsel<<endl<<"FICs\n"<<FICs<<endl; exit(-1);}
  if (debug == 13) {cout<<"N\n"<<N<<endl<<"B\n"<<B<<endl;  exit(-1);}
  if (debug == 14) {cout<<"Yr1\n"<<Yr1<<endl<<"aAge1\n"<<aAge1<<endl<<"dAge1\n"<<dAge1<<endl<<"Age1\n"<<Age1<<endl
                                    <<"aFt\n"<<aFt<<endl<<"dFt\n"<<dFt<<endl<<"iFt\n"<<iFt<<endl ;  exit(-1);}


FUNCTION calc_suitability
  /*
  Function calculates 1) prey suitability, Nu, and 2) scaled suitability matrix across prey species, Snu
  */
  for (pd=1; pd<=nsp; pd++)
    {
    for (py=1; py<=nsp; py++)
      {
      //Nu = G * Rho
      Nu(pd)(t).sub(fage(py),lage(py)) = G(pd)(t).sub(fage(py),lage(py))*Rho(pd,py);
      Nu(pd)(t)(sage+1) = Rho(pd,nsp+1);  //Other food component
      }  //end of prey species loop      
      //Sum suitabilities across prey species
      dvar_vector sumnu = colsum(Nu(pd)(t));  //This summation includes OFood;

      //Scale the suitability matrix; Snu = Nu/sumnu
      dvar_matrix Nutmp = trans(Nu(pd)(t));
      dvar_matrix Snutmp = trans((Snu)(pd)(t));
      for (pdA=1; pdA<=nage(pd); pdA++)
        {
        if (sumnu(pdA)!=0) {Snutmp(pdA) = Nutmp(pdA)/sumnu(pdA);}
        else {Snutmp(pdA) = 0;}  //*****Make sure filling empty cells with zero is a valid assumption*****
        }  //end of predator age loop
      Snu(pd)(t) = trans(Snutmp);
      if (debug == 22) {cout<<"Predator: "<<pd<<"  Nu(pd)(t)\n"<<Nu(pd)(t)<<endl
                                       <<"Predator: "<<pd<<"  Snu(pd)(t)\n"<<Snu(pd)(t)<<endl;}

    }  //end of predator species loop

  if (debug == 20) {cout<<"Size pref, g:"<<"  nf: "<<nf<<"  yr: "<<t<<endl<<G<<endl
                                   <<"Nu:"<<"  nf: "<<nf<<"  yr: "<<t<<endl<<Nu<<endl
                                   <<"Snu: "<<"  nf: "<<nf<<"  yr: "<<t<<endl<<Snu<<endl;}
  if (debug == 21) {cout<<"Size pref, g:"<<"  nf: "<<nf<<"  yr: "<<t<<endl<<G<<endl
                                   <<"Nu:"<<"  nf: "<<nf<<"  yr: "<<t<<endl<<Nu<<endl
                                   <<"Snu: "<<"  nf: "<<nf<<"  yr: "<<t<<endl<<Snu<<endl
                                   <<endl<<"manually exiting...\n"<<endl;exit(-1);}
  if (debug == 22) {cout<<"\nmanually exiting..."<<endl;exit(-1);}


FUNCTION calc_predation_mortality
  /*
  Function calculates the 1) biomass of available prey, Avail, 2) scaled available prey biomass scaled across prey species, scAv,
  3) biomass of prey consumed, Consum, 4) total prey consumed across predator species, sumCon, and 5) predation mortality, M2
  */

  //Total annual biomass of modeled fish species
  dvar_matrix AnB(1,nsp,1,nage);
  AnB.initialize();
  for (i=1; i<=nsp; i++)  { AnB(i) = B(i,t); }

  //Other food = Total ecosystem biomass - Total annual modeled biomass
  dvariable OFood = EcoB - sum(AnB);
  if (debug == 35)  {cout<<"nf: "<<nf<<"  t: "<<t<<"    OFood: "<<OFood<<"    OPen: "<<Open<<endl;}
  OFood = posnum(OFood, 0, (EcoB/2), Open);
  if (debug == 35)  {cout<<"nf: "<<nf<<"  t: "<<t<<"  p.OFood: "<<OFood<<"    OPen: "<<Open<<endl;}

  //Biomass of available prey, Avail
  for (pd=1; pd<=nsp; pd++)
    {
    for (py=1; py<=nsp; py++)  
      {
      //Subset the scaled suitabilities for a particular prey species; row = prey age; column = pred age
      dvar_matrix temp1 = Snu(pd)(t).sub(fage(py),lage(py));
      //Subset the age-specific biomass for a particular prey species; vector = prey age
      dvar_vector temp2 = B(py,t);

      for (pdA=1; pdA<=nage(pd); pdA++)
        {
          dvar_vector temp3 = trans(temp1)(pdA);
          temp2.shift(fage(py));
          dvar_vector temp4 = elem_prod(temp3,temp2);
          Avail(pd)(t)(pdA).sub(fage(py),lage(py)) = temp4;
          Avail(pd)(t)(pdA)(sage+1) = Snu(pd)(t)(sage+1)(pdA)*OFood;
              //perfect example of where I would love to be able to loop over all pred ages!!
        }  //end of predator age loop

      }  //end of prey species loop
    }  //end of predator loop
  if (debug == 30) {cout<<"nf: "<<nf<<"  yr: "<<t<<"  calculated Avail"<<endl;}

  //Scaled available prey, scAv, biomass of prey consumed, Consum, and total biomass of each prey sp consumed, sumCon
  for (pd=1; pd<=nsp; pd++)
    {
    if (debug == 31 | debug == 34) {cout<<"pd: "<<pd<<"  yr: "<<t<<"  Avail(pd)(t):\n"<<Avail(pd)(t)<<endl;}
    //Total prey available to a particular pred; summed across prey sp
    dvar_vector sumAv = rowsum(Avail(pd)(t));
    if (debug == 31 | debug == 34) {cout<<"pd: "<<pd<<"  yr: "<<t<<"  sumAv\n"<<sumAv<<endl;}

    for (pdA=1; pdA<=nage(pd); pdA++)
      {
      //Scaled available prey biomass
      if (sumAv(pdA) != 0) { scAv(pd,t,pdA) = Avail(pd,t,pdA) / sumAv(pdA); }
      //Biomass of consumed prey
      Consum(pd,t,pdA) = CB(pd,t,pdA) * B(pd,t,pdA) * scAv(pd,t,pdA); //Simulation eq. used Ing and N; GB equation uses CB and B
      }  //end of predator age loop

      if (debug == 31 | debug == 34) {cout<<"pd: "<<pd<<"  yr: "<<t<<"  scaled Available Prey (pd,t)\n"<<scAv(pd,t)<<endl
                                                            <<"pd: "<<pd<<"  yr: "<<t<<"  transpose of scaled Available Prey (pd,t)\n"<<trans(scAv(pd,t))<<endl
                                                            <<"pd: "<<pd<<"  yr: "<<t<<"  Consum(pd,t)\n"<<Consum(pd,t)<<endl;}
      
      for (py=1; py<=nsp; py++)
        {
        //Total prey consumed, summed across predator species; sumCon 
        dvar_matrix temp5 = trans(Consum(pd,t)).sub(fage(py),lage(py));
        temp5.rowshift(1);
        //Loop to sum across all pred sp and ages using += operator
        sumCon(py,t) += rowsum(temp5);
        if (debug == 32) {
          cout<<"pd: "<<pd<<"  yr: "<<t<<"  py: "<<py<<"  temp5 = Consum for one prey sp\n"<<temp5<<endl
          <<"pd: "<<pd<<"  yr: "<<t<<"  py: "<<py<<"  rowsum (temp5)\n"<<rowsum(temp5)<<endl
          <<"pd: "<<pd<<"  yr: "<<t<<"  py: "<<py<<"  sumCon(py) where sumCon(py,t) += rowsum(temp5) and sumCon is a 3darray (sp,year,age)\n"<<sumCon(py)<<endl<<endl;}
        }  //end of prey loop  

    }  //end of predator species loop

    if (debug == 31 | debug == 32) {cout<<"manually exiting..."<<endl;exit(-1);}
    if (debug == 30) {cout<<"nf: "<<nf<<"  yr: "<<t<<"  calculated scAv, Consum and sumCon"<<endl;}

  //Predation mortality
  for (i = 1; i<=nsp; i++)
    {
    if (debug == 33) {cout<<"sp: "<<i<<"  t: "<<t<<endl<<"sumCon\n"<<sumCon(i,t)<<endl<<"B\n"<<B(i,t)<<endl;}
    M2(i,t) = elem_div(sumCon(i,t),B(i,t));
    if (debug == 33) {cout<<"M2\n"<<M2(i,t)<<endl;}
    }
  if (debug == 30) {cout<<"nf: "<<nf<<"  yr: "<<t<<"  calculated M2"<<endl;}


FUNCTION pop_dynamics
  /*
  Function calculates 1) age-specific fishing mortality, F, 2) total mortality, Z, 3) state dynamics, N & B, and 4) commercial ,
  catch, including A) catch-at-age, C_hat, B) total catch in weight, TotC_hat, C) total catch in number, sumC_hat, and
  D) catch proportions at age, Cprop_hat.
  Function assumes N and C are in millions, TotC is in thousands of metric tons  
  */
  for (i=1; i<=nsp; i++)
    {
    //Only calculate state dynamics if the year is between the first and last yrs for that particular species
    if (t>=fyr(i) && t<=lyr(i))
      {
      //Expand initial F parameters, iFt, into a species x year matrix, Ft
      if (TotC(i,t) != 0) {
        Fct(i)++;
        Ft(i,t) = mfexp(iFt(i,Fct(i))); }
      else {Ft(i,t) = 0.;}

      //Age-specific fishing mortality
      F(i,t) = Ft(i,t)*s(i);

      //Total mortality
        //When either 1) trophic interactions are not turned on, or 2) data for all species are not available for year t, M2 is set
        //at a fixed/inputted species- and age- specific value.  Otherwise, M2 is set to the value calculated within the model
      Z(i)(t)=M1(i)(t)+F(i)(t)+iM2(i);
      if (trophic) {
        if (t>=FHfyr && t<=FHlyr) {Z(i)(t)=M1(i)(t)+F(i)(t)+M2(i)(t);}
        }  //end of Trophic If Statement
      
      if (debug == 42) {cout<<"nf: "<<nf<<" trophic: "<<trophic<<" sp: "<<i<<" yr: "<<t<<endl
                                       <<"M1\n"<<M1(i)(t)<<endl<<"iM2\n"<<iM2(i)<<endl<<"M2\n"<<M2(i)(t)<<endl
                                       <<"F\n"<<F(i)(t)<<endl<<"Z\n"<<Z(i)(t)<<endl<<endl;}
      //State dynamics
      if (t != lyr(i))
        {
        N(i)(t+1,1)=mfexp(Age1(i,t+1));
          //Annual recruits
        N(i)(t+1)(2,nage(i))=++ elem_prod(N(i)(t)(1,nage(i)-1),mfexp(-Z(i)(t)(1,nage(i)-1)));
          //Subsequent age classes
        N(i)(t+1)(nage(i)) += N(i)(t,nage(i))*mfexp(-Z(i)(t,nage(i)));
          //Plus group
        B(i)(t+1) = elem_prod(N(i)(t+1),Wt(i)(t+1)); 
        }  //end of lyr If statement

      //Commercial catch, Baranov catch equation
      dvar_vector Ntmp = elem_prod( N(i)(t),1. - exp(-Z(i)(t)) );
      C_hat(i)(t) = elem_prod( elem_div(F(i)(t),Z(i)(t)),Ntmp );                  //millions of fish
      //TotC_hat(i)(t) = sum(1000*elem_prod( C_hat(i)(t),Wt(i)(t) ));         //metric tons
      //TotC_hat(i)(t) = 1.e-6*sum(1000*elem_prod( C_hat(i)(t),Wt(i)(t) ));  //millions of metric tons
      TotC_hat(i)(t) = 1.e-3*sum(1000*elem_prod( C_hat(i)(t),Wt(i)(t) ));  //thousands of metric tons
      sumC_hat(i)(t) = sum(C_hat(i)(t));
      if (sumC_hat(i)(t) != 0) {Cprop_hat(i)(t) = C_hat(i)(t)/sumC_hat(i)(t);}

      } //end of fyr&&lyr If statement
    }  //end of species loop
  if (debug == 40) {cout<<"Z\n"<<Z<<endl<<"N\n"<<N<<endl<<"C_hat\n"<<C_hat<<endl<<"Cprop_hat\n"<<Cprop_hat<<endl;}
  if (debug == 41) {cout<<"Z\n"<<Z<<"N\n"<<N<<endl<<"C_hat\n"<<C_hat<<endl<<"Cprop_hat\n"<<Cprop_hat<<endl;  exit(-1);}
  if (debug == 43) {cout<<"M1yr\n"<<M1yr<<endl<<"FHfyr\n"<<FHfyr<<endl<<"FHlyr\n"<<FHlyr<<endl
                                   <<"Z\n"<<Z<<endl<<"M1\n"<<M1<<endl<<"iM2\n"<<iM2<<endl<<"M2\n"<<M2<<endl
                                   <<"F\n"<<F<<endl<<"Ft\n"<<Ft<<endl<<"s\n"<<s<<endl;  exit(-1);}


FUNCTION calc_survey_abundance
    /*
    Function calculates the residuals between the fishery-independent survey catch-at-age (FIC) data
    and the vulnerbale proportions-at-age to the survey
    */
    for (i=1; i <= nsp; i++)
      {
    for (j=1; j<=nFIC; j++)
      {
      int bage = FICfage(i,j);
      int eage = FIClage(i,j);
            
      //Species for which FIC data are not split into multiple segments
      if (nseg(i) == 1)
        {
        //The following code assumes one calculated q for every species (age-invariant) but calculates this q using total FIC and total 
          //N instead of age-specific indices.  Assumes time-invariant, but age-specific, survey selectivity (Method 2)

        //Incorporating time-invariant survey selectivity and the timing of the trawl survey;
            //selN represents the portion of N that has survived to the time of the survey and the proportion of that portion selected by the survey
 				    //selN = FICs * N * exp(-Z*FICmon/12):  Incorporates amount of Z the sp has incurred between Jan1 and FICmon
        dvar_matrix selN(fyr(i),lyr(i),bage,eage);
        for (t = fyr(i); t<=lyr(i); t++) {
          selN(t) = elem_prod(  elem_prod(FICs(i),N(i,t)) , mfexp(-Z(i,t)*(FICmon(i,j)/12))  ).sub(bage,eage);  }

       //Calculate q from deviations between FIC and N
       dvar_vector FICtmp = TotFIC(i)(j);
       dvar_vector Ntmp = rowsum(selN);
       if (debug == 50) {cout<<"\nLoop for species without any breaks in the survey data\nSpecies\n"<<i<<endl<<"Survey\n"<<j<<endl
                                        <<"q\n"<<q(i)(j)<<endl<<"FIC\n"<<FIC(i)(j)<<endl<<"FICtmp\n"<<FICtmp<<endl
                                        <<"FICs\n"<<FICs(i)<<endl<<"N\n"<<N(i)<<endl<<"selN\n"<<selN<<endl<<"Ntmp\n"<<Ntmp<<endl;}
                                        
       dvar_vector zt = log(FICtmp+p) - log(Ntmp+p);
       q(i,j,1) = mean(zt);
       if (debug == 50) {cout<<"zt\n"<<zt<<endl<<"q(i,j) = mean(zt)\n"<<q(i,j,1)<<endl;}
       toteps(i,j) = zt - q(i,j,1);
       if (debug == 50) {cout<<"toteps(i,j)\n"<<toteps(i,j)<<endl;}

       //Calculate predicted FIC
 			   //FIC_hat = q * FICs * N * exp(-Z*FICmon/12):  Incorporates amount of Z the sp has incurred between Jan1 and FICmon
       for (t = fyr(i); t<=lyr(i); t++) {FIC_hat(i,j,t) = mfexp(q(i,j,1))*elem_prod(  elem_prod(FICs(i),N(i,t)) , mfexp(-Z(i,t)*(FICmon(i,j)/12))); }

       if (debug == 52) {cout<<"sp: "<<i<<endl<<"survey: "<<j<<endl<<"mfexp(q)\n"<<mfexp(q(i,j,1))<<endl<<"FICs\n"<<FICs(i)<<endl
                                        <<"FIC_hat\n"<<FIC_hat(i)(j)<<endl<<"N\n"<<N(i)<<endl;}
      if (debug == 50) {cout<<"End of nseg=1 If statement\n"<<endl;}

        }  //end of nseg=1 If statement


      if (nseg(i) >1)
        {
        //The following code assumes multiple calculated q's for every species (age-invariant, but splitting the FIC time series) but
          //calculates this q using total FIC and total N instead of age-specific indices (Method 2A)

        //Incorporating time-invariant survey selectivity and the timing of the trawl survey;
            //selN represents the portion of N that has survived to the time of the survey and the proportion of that portion selected by the survey
 				    //selN = FICs * N * exp(-Z*FICmon/12):  Incorporates amount of Z the sp has incurred between Jan1 and FICmon

        dvar_matrix selN(fyr(i),lyr(i),bage,eage);
        for (t = fyr(i); t<=lyr(i); t++) {
          selN(t) = elem_prod(  elem_prod(FICs(i),N(i,t)) , mfexp(-Z(i,t)*(FICmon(i,j)/12))  ).sub(bage,eage);  }    //Incorp. survey timing

        //First and last years of each FIC segment
        if (debug == 51) {cout<<"\nLoop for species with breaks in the survey data\nSpecies'n"<<i<<endl<<"Survey\n"<<j<<endl
                                         <<"q\n"<<q(i)(j)<<endl<<"FIC\n"<<FIC(i)(j)<<endl<<"FICs\n"<<FICs(i)<<endl<<"N\n"<<N(i)<<endl
                                         <<"selN\n"<<selN<<endl<<"FICyr\n"<<FICyr(i)<<endl;}
        dvector FICfyr(1,nseg(i));  FICfyr.initialize();
        dvector FIClyr(1,nseg(i));  FIClyr.initialize();
        FICfyr(1) = fyr(i);
        FIClyr(nseg(i)) = lyr(i);

        for (seg=1; seg<nseg(i); seg++)
          {
          FICfyr(seg+1) = FICyr(i,seg);
          FIClyr(seg) = FICyr(i,seg)-1;
          }
        if (debug == 51) {cout<<"FICfyr\n"<<FICfyr<<endl<<"FIClyr\n"<<FIClyr<<endl;}

       //For each segment, calculate q from deviations between FIC and N
        for (seg=1; seg<=nseg(i); seg++)
          {
          int byr = FICfyr(seg);
          int eyr = FIClyr(seg);
          dvar_vector FICtmp = TotFIC(i)(j).sub(byr,eyr);
          dvar_vector Ntmp = rowsum(selN.sub(byr,eyr));
          if (debug == 51) {cout<<"\nseg: "<<seg<<endl<<"FICfyr: "<<byr<<"  FIClyr: "<<eyr<<endl<<"TotFIC\n"<<TotFIC(i,j)
                                          <<endl<<"FICtmp\n"<<FICtmp<<endl<<"N\n"<<N(i)<<endl<<"Ntmp\n"<<Ntmp<<endl;}
          dvar_vector zt = log(FICtmp+p) - log(Ntmp+p);
          q(i,j,seg) = mean(zt);
          if (debug == 51) {cout<<"zt\n"<<zt<<endl<<"q(i,j,seg) = mean(zt)\n"<<q(i,j,seg)<<endl;}
          toteps(i)(j).sub(byr,eyr) = zt - q(i,j,seg);
          if (debug == 51) {cout<<"toteps\n"<<toteps(i)(j).sub(byr,eyr)<<endl;}

         //Calculate predicted FIC
 				   //FIC_hat = q * FICs * N * exp(-Z*FICmon/12):  Incorporates amount of Z the sp has incurred between Jan1 and FICmon
         for (t = byr; t<=eyr; t++) {FIC_hat(i,j,t) = mfexp(q(i,j,seg))*elem_prod(  elem_prod(FICs(i),N(i,t)) , mfexp(-Z(i,t)*(FICmon(i,j)/12))); }
 
          } //end of seg loop
 
         if (debug == 52) {cout<<"sp: "<<i<<endl<<"survey: "<<j<<endl<<"mfexp(q)\n"<<mfexp(q(i,j,1))<<endl<<"FICs\n"<<FICs(i)<<endl
                                        <<"FIC_hat\n"<<FIC_hat(i)(j)<<endl<<"N\n"<<N(i)<<endl;}

        }  //end of nseg  > 1 If statement
      if (debug == 51) {cout<<"End of nseg>1 If statement\n"<<endl;}

      //Calculating Survey Catch, FIC, proportions-at-ge
      TotFIC_hat(i)(j) = colsum(trans(FIC_hat(i)(j)).sub(bage,eage));
      for (t = fyr(i); t<=lyr(i); t++)
        {
        if (TotFIC_hat(i,j,t) != 0) { Sprop_hat(i)(j)(t).sub(bage,eage) = FIC_hat(i)(j)(t).sub(bage,eage)/TotFIC_hat(i)(j)(t); } 
        }
      if (debug == 52)  {cout<<"TotFIC_hat\n"<<TotFIC_hat(i)(j)<<endl<<"Sprop_hat\n"<<Sprop_hat(i)(j)<<endl<<endl;}

      }  //end of nFIC loop

      if (debug == 50 | debug == 51) {cout<<"Exit at end of nFIC loop\n"<<endl; exit(-1);}

      }  //end of species loop

      if (debug == 52) {cout<<"Exit at end of species loop\n"<<endl;  exit(-1);}
      if (debug == 53) {cout<<"mfexp.q\n"<<mfexp(q)<<endl;}


FUNCTION calc_biomass_penalty
  /*
  Function calculates a penalty, Bpen, that is added to the objective function in the calc_likelihood function if the biomass for
  any species falls below a threshold value, Bthres
  */
  dvariable tmpBpen;
  for (i = 1; i<=nsp; i++)
    {
    if (t >= fyr(i) && t<=lyr(i))
      {
      if (debug == 80) {cout<<"t: "<<t<<"  sp: "<<i<<endl<<"B(i,t)\n"<<B(i,t)<<endl;}
      if (debug == 81 && min(B(i,t)) <= Bthres(i)) {cout<<"t: "<<t<<"  sp: "<<i<<endl<<"B(i,t)\n"<<B(i,t)<<endl;}

      for (a=1; a<=nage(i); a++)
        {
        tmpBpen.initialize();
        if (debug == 81 && B(i,t,a) <= Bthres(i)) {cout<<"a: "<<a<<"  pre Bpen: "<<Bpen<<endl;}  //09-14-09
        B(i,t,a) = posfun(B(i,t,a),Bthres(i),tmpBpen);
        Bpen(i) += tmpBpen;
        if (debug == 81 && B(i,t,a) <= Bthres(i)) {cout<<"a: "<<a<<"      Bpen: "<<Bpen<<"     tmpBpen: "<<tmpBpen<<endl;}
        }  //end of prey age loop

      if (debug == 80) {cout<<"Post B(i,t)\n"<<B(i,t)<<endl<<endl; }
      if (debug == 81 && min(B(i,t)) <= Bthres(i)) {cout<<"Post B(i,t)\n"<<B(i,t)<<endl<<endl; }
      }  //end of year If Statement
    }  //end of species loop


FUNCTION calc_year1_constraint
  /*
  Function calculates a penalty, Ypen, that is added to the objective function in the calc_likelihood function.  This penalty ensures
  that species-specific Yr1 abundances approximate an exponential decline with increasing age.  Specifically, the function
  1) calculates the average species- and age-specific total mortality, avgZ, observed over the time series, 2) uses this avgZ and
  the predicted age-1 abundance in year-1, Yr1(i,1), to create a synthetic, theoretical cohort in year 1, thYr1, and 3) calculates the 
  penalty, Ypen, as the sum of the squared deviations between the theoretical, thYr1, and predicted, Yr1, Year-1 abundances.
  */

  dvar_matrix avgZ(1,nsp,1,nage);   //Average age-specific total mortality; averaged across years
  dvar_matrix thYr1(1,nsp,1,nage);  //Theoretical year-1 abundances
  thYr1.initialize();
  for (i = 1; i<=nsp; i++)
    {
    avgZ(i) = colMeans(Z(i));
    if (debug == 82) {cout<<"sp: "<<i<<endl<<"Z\n"<<Z(i)<<endl<<"avgZ\n"<<avgZ(i)<<endl;}
    //Set Age-1 abundance equal to predicted Yr1 abundance
    thYr1(i,1) = mfexp(Yr1(i,1));
    //Loop over ages to calculate remaining N's instead of using elem_prod because age, in this case, is recursive
    for (a =2; a<=nage(i); a++)
      {thYr1(i,a) = thYr1(i,a-1)*mfexp(-avgZ(i,a-1));}
    if (debug == 82) {cout<<"mfexp(Yr1(i,1)): "<<mfexp(Yr1(i,1))<<endl<<"thYr1\n"<<thYr1(i)<<endl;}

    //Calculate penalty, Ypen, as the sum of squared deviations between theoretical and predicted Year-1 abundance
    Ypen(i) = norm2(mfexp(Yr1(i)) - thYr1(i));
    if (debug == 82) {cout<<"Ypen(i)\n"<<Ypen(i)<<endl<<endl;}
    }  //end of species loop

  if (debug == 82) {cout<<"Exiting at end of Yr1_constraint function..."<<endl;  exit(-1);}


FUNCTION calc_recruitment_penalty
  /*
  Function calculates a penalty, Rpen, that is added to the objective function in the calc_likelihood function if the coefficient of variation for the recruitment of any species becomes greater than a threshold value, Rthres
  */
  for (i = 1; i<=nsp; i++)
    {
    if (debug == 83) {cout<<"sp: "<<i<<endl<<"Age1(i)\n"<<Age1(i)<<endl;
                               cout<<"std_dev: "<<std_dev(Age1(i))<<endl;
                               cout<<"avg: "<< mean(Age1(i))<<endl;  }
    dvariable cvAge1 = std_dev(Age1(i))/mean(Age1(i));
    if (debug == 83) {cout<<"cvAge1:  "<<cvAge1<<endl;}    
    if  (cvAge1>Rthres(i))  
      { Rpen(i) = .01*square(cvAge1-Rthres(i));  }
    if (debug == 83) {cout<<"Rpen(i):  "<<Rpen(i)<<endl<<endl;}
    }  //end of species loop

    if (debug == 83) {cout<<"Rpen\n"<<Rpen<<endl<<endl;}    


FUNCTION calc_food_habits
  /*
  Function calculates predicted food habits, FHhat, as proportion (by weight) at age
  The average FH is calculated within each FH year bin, and the FH bins are comprised of a set number of years (binsize).
  FH_hat is calculated by 1) summing biomass of scaled available prey, scAv, across prey ages to obtain the total annual %W
  of each prey species for each predator, FHtmp, 2) averaging FHtmp over years within the FH bin to obtain the average %W 
  for each FH bin, FH_hat
    Obs FH:  init_4darray FH  (1,nsp2,1,nbins,1,nage,1,nsp2+1) = (Pd.sp, FH.bins, PdA, Py.sp)
    Pred FH: 4darray FH_hat
    4darray scAv(1,nsp,FHfyr2,FHlyr2,1,nage,1,sage+1) = (Pd.sp, Yr, PdA, Py)
  */

  if (debug == 70) {cout<<"scAv\n"<<scAv<<endl<<endl;}
  if (debug == 72) {cout<<"binsize\n"<<binsize<<endl<<"binfyr\n"<<binfyr<<endl<<"binlyr\n"<<binlyr<<endl;}

  //For each predator species, year and prey species, sum scaled available prey, scAv, over prey age to produce FHtmp
  for (pd=1; pd<=nsp; pd++)
    {
    dvar3_array FHtmp(FHfyr,FHlyr,1,nage(pd),1,nsp+1);  FHtmp.initialize();
    for (t = FHfyr; t<=FHlyr; t++)
      {
      dvar_matrix Avpd = scAv(pd,t);  //pdage X prey age&sp
      if (debug == 70 | debug == 71) {cout<<"pd: "<<pd<<"  t: "<<t<<endl<<"Avpd\n"<<Avpd<<endl;}
      for (py=1; py<=nsp; py++)
        {
        dvar_matrix Avpy = trans(Avpd).sub(fage(py),lage(py)); //pyage X pdage
        if (debug == 71) {cout<<"py: "<<py<<endl<<"Avpy\n"<<Avpy<<endl;}
        FHtmp(t).colfill(py,colsum(Avpy));   
          //Filling the py_th column of the matrix, FHtmp(t), with the vector, colsum(Avpy)
        if (debug == 71) {cout<<"colsum(Avpy)\n"<<colsum(Avpy)<<endl<<"FHtmp(t)\n"<<FHtmp(t)<<endl;}
        }  //end of prey species loop

      //cout<<"test\n"<<trans(Avpd)(sage+1)<<endl;
      FHtmp(t).colfill(nsp+1,trans(Avpd)(sage+1));
      if (debug == 70) {cout<<"FHtmp(t)\n"<<FHtmp(t)<<endl;}
      }  //end of FH year loop

  //For each predator sp, FH bin, calculate the average FH over the FH bin, FH_hat
    for (b = 1; b<=nbins; b++)
      {
      dvar_matrix FHsum(1,nage(pd),1,nsp+1);
      FHsum.initialize();
      for (t = binfyr(b); t<=binlyr(b); t++) {FHsum += FHtmp(t); }
      FH_hat(pd,b) = FHsum / binsize;		
      if (debug == 72) {cout<<"binfyr: "<<binfyr(b)<<"  binlyr: "<<binlyr(b)<<endl
                                       <<"FHtmp(bin)\n"<<FHtmp.sub(binfyr(b),binlyr(b))<<endl<<"FHsum\n"<<FHsum<<endl
                                       <<"FH_hat\n"<<FH_hat(pd,b)<<endl;}
      if (debug == 73) {cout<<"pd: "<<pd<<"  b: "<<b<<endl<<"sum FH_hat\n"<<rowsum(FH_hat(pd,b))<<endl;}
      }  //end of FH bin loop

    }  //end of predator species loop
    
  if (debug == 70 | debug == 71 | debug == 72)
    {cout<<"manually exiting at end of calc_food_habits fx..."<<endl;exit(-1);}
  
  
FUNCTION calc_negative_loglikelihood
  /*
  Function calculates the likelihood components for each data source:
    1.  Total commercial catch, in weight; TotC and TotC_hat
    2.  Commercial catch proportions-at-age; Cprop and Cprop_hat
    3.  Total fishery-independent survey catch in number/tow; toteps
    4.  Survey catch proportion-at-age; Sprop and Sprop_hat
    5.  Predator food-habits in proportion, by weight, at-age; FH and FH_hat

  For data sources 1 and 3, a lognormal distribution is assumed.  For data sources 2, 4, and 5, two different types of residuals were
  calculated - A) assuming a multinomial distribution, and B) assuming a multinomial distribution but first subtracting the best possible value of the residuals assuming a perfect fit (so that now the objective function goes to zero)
  In prevoius versions, I also tried an arcsine squareroot transformation, but I was unable to take the arcsine of the value 1
  when the element is part of a differentiable array.  Accordingly, I had to subtract a tiny number (5.e-8) from the observed 
  and predicted FH so that I could still take the arcsine of the element even if the predicted FH value == 1, but I was not
  confortable with this modification.

  The function also calculates the likelihood components for each of the initial parameters that represent deviations from means (dYr1,
  dAge1, and dFt).  The likelihood components for each of these parameters ensure that all species-specific deviations sum to zero.
  */

   //Total commercial catch component
       //init_matrix TotC(1,nsp,fyr,lyr);                //Total commercial catch; metric tons
       //matrix TotC_hat(1,nsp,fyr,lyr);                //Total commerical catch *in weight*; summed over ages
       //dvar_vector TCres(1,nsp);
  TCres.initialize();                                          //Lognormal distribution

   //Catch-at-age proportions component
       //3darray Cprop(1,nsp,fyr,lyr,1,nage);        //Proportion-at-age of the commercical catch, CAA
       //3darray Cprop_hat(1,nsp,fyr,lyr,1,nage);  //Commercial catch proportions-at-age
       //dvar_vector CPmulti(1,nsp);
  CPmulti.initialize();                                        //Multinomial distribution

  //Total survey catch component
      //3darray TotFIC(1,nsp,1,nFIC,fyr,lyr);        //Sum of observed FIC  
      //3darray TotFIC_hat(1,nsp,1,nFIC,fyr,lyr); //Total survey (FIC) catch in number/tow; summed over ages
      //matrix TSresA(1,nsp,1,nFIC);      //Objective function component: Total survey catch, method A
      //matrix TSresB(1,nsp,1,nFIC);      //Objective function component: Total survey catch, method B
  TSresA.initialize();  TSresB.initialize();           //Lognormal distribution

  //FIC, Survey catch-at-age proportions component
      //init_3darray sumFIC(1,nsp,1,nFIC,fyr,lyr); //Sum of observed FIC  
      //3darray Sprop(1,nsp,fyr,lyr,1,nage);           //Proportion-at-age of the survey catch, FIC
      //3darray Sprop_hat(1,nsp,fyr,lyr,1,nage);    //Survey catch proportions-at-age
  SPmulti.initialize();                                         //Multinomial distribution

  //Food habits component
      //init_4darray FH(1,nsp2,1,nbins,1,nage,1,nsp2+1);  //Observed FH, proportions by wt
      //4darray FH_hat(1,nsp2,1,nbins,1,nage,1,nsp2+1);  //Predicted FH, summed over all prey ages, and including OFood
      //dvar_vector FHmulti(1,nsp);
  FHmulti.initialize();                                          //Multinomial distribution

  //Penalties
  tBpen.initialize();                                           //Total Biomass penalty for each species
  tYpen.initialize();                                          //Total Yr1 penalty for each species
  tRpen.initialize();                                           //Total Recruitment penalty for each species
  tOpen = 0;                                                   //Total Other Food penalty

  //Deviations component
      //init_bounded_matrix dYr1(1,nsp,1,nage,-5,5,2);        //Age-specific deviations in Year 1 N's, log space
      //init_bounded_matrix dAge1(1,nsp,fyr+1,lyr,-5,5,2);   //Annual deviations in recruits, log space
      //init_bounded_matrix dFt(1,nsp,1,nFt,-5,5,2);             //Annual deviations in species-specific fishing mortality rates in yrs where TotC > 0
  Devs.initialize();

  //Total objective function value, some species-specific;
  ofvsp.initialize();  ofvsp_ideal.initialize();  ofv_ideal.initialize();

  //Species loop for each likelihood component
  for (i = 1; i<=nsp; i++)
    {

    //Total commercial catch component
    TCresid(i) = log(TotC(i)+o) - log(TotC_hat(i)+o);
    TCres(i) = TCwt(i) *norm2(TCresid(i));
      /* ******In future datasets, double check to make sure TotC and TotC_hat units are the same! --
            Best way to check is to make sure the numbers are of the same order of magnitude!******* */
    if (debug == 62) {cout<<"TotC_hat\n"<<TotC_hat(i)+o<<endl<<"log TotC_hat\n"<<log(TotC_hat(i)+o)<<endl;}
    if (debug == 60) {cout<<"Completed lognormal TotC,  sp="<<i<<endl;}

    //Total survey catch component; two possible ways to calculate TotFIC residuals
    //B
    TSresid(i) = log(TotFIC(i)+o) - log(TotFIC_hat(i)+o);
    for (j=1; j<=nFIC; j++)
      {
      //A
      TSresA(i)(j) = TSwt(i)(j) *norm2(toteps(i)(j));
      //B
      TSresB(i)(j) = TSwt(i)(j) *norm2(TSresid(i)(j));
      }  //end of nFIC loop
    if (debug == 62) {cout<<"TotFIC_hat\n"<<TotFIC_hat(i)+o<<endl<<"log TotFIC_hat\n"<<log(TotFIC_hat(i)+o)<<endl;}
    if (debug == 60) {cout<<"Completed lognormal TotFIC,  sp="<<i<<endl;}
    
    for (t = fyr(i); t<=lyr(i); t++)
      {

      //Catch-at-age proportions component
      if (sumC(i,t) != 0)  //only calculate CPmulti if sum of obs CAA > 0, i.e. if age samples were taken in a particular year
        {
        //Multinomial distribution
        dvar_vector CPvec = CPwt(i) *elem_prod( Cprop(i,t)+p, log(Cprop_hat(i,t)+p) );
          //It does not matter if CPwt is in- or outisde of the summation
        CPmulti(i) -= sum(CPvec);
        if (debug == 63) {cout<<"t: "<<t<<endl
                                         <<"Cprop_hat\n"<<Cprop_hat(i,t)+p<<endl<<"log Cprop_hat\n"<<log(Cprop_hat(i,t)+p)<<endl;}
        }  //end of CAA proportions component
      if (debug == 61) {cout<<"Completed multinomial Cprop,  sp="<<i<<"  yr="<<t<<endl;}

      //Survey catch-at-age proportions component
      for (j=1; j<=nFIC; j++)
        {
        int bage = FICfage(i,j);
        int eage = FIClage(i,j);
        if (sumFIC(i,j,t) != 0)
          {
          //Multinomial distribution
          dvar_vector SPvec = SPwt(i)(j) *elem_prod( Sprop(i,j,t).sub(bage,eage)+p,log(Sprop_hat(i,j,t).sub(bage,eage)+p) );
          SPmulti(i,j) -= sum(SPvec);
          if (debug == 64) {cout<<"t: "<<j<<"  j: "<<t<<endl
                                           <<"Sprop_hat\n"<<Sprop_hat(i,j,t)+p<<endl<<"log Sprop_hat\n"<<log(Sprop_hat(i,j,t)+p)<<endl;}
          }  //end of FIC proportions component
        }  //end of nFIC loop
      if (debug == 61) {cout<<"Completed multinomial Sprop,  sp="<<i<<"  yr="<<t<<endl;}

      }  //end of year loop
    if (debug == 60) {cout<<"Completed multinomial Cprop and Sprop,  sp="<<i<<endl;}

    //Food habits, proportion by weight, component
    if (trophic && rhosp(i) > 0) 
      {
      for (b=1; b<=nbins; b++)
        {
        for (pdA = 1; pdA<=nage(i); pdA++)
          {
          if (sum(FH(i,b,pdA))<=100)    //Rows of missing data are marked with 9999, making the rowsum >> 100
            {
            //Multinomial distribution
            dvar_vector FHvec = FHwt(i) *elem_prod( FH(i,b,pdA)+p , log(FH_hat(i,b,pdA)+p) );
            FHmulti(i) -= sum(FHvec);
            if (debug == 65) {cout<<"b: "<<b<<"  pdA: "<<pdA<<endl
                                           <<"FH_hat\n"<<(FH_hat(i,b,pdA)+p)<<endl<<"log FH_hat\n"<<log(FH_hat(i,b,pdA)+p)<<endl;}
            }  //end of sum(FH) If Statement
          }  //end of predator age loop
        }  //end of FH bin loop
      }  //end of trophic If statement
    if (debug == 60) {cout<<"Completed multinomial FH,  sp="<<i<<endl;}

    //Putting all of the likelihood components together
    //Commercial and survey catch data
    ofvsp_ideal(i) = TCres(i) + (CPmulti(i) - CPideal(i)) + sum(TSresA(i)) + sum( (SPmulti(i) - SPideal(i)) );
    ofvsp(i) = TCres(i) + CPmulti(i) + sum(TSresA(i)) + sum(SPmulti(i));
    //Food habits data only included if 1) trophic interactions are turned on, and 2) the species conumes other species in the model
      //From data section, rhosp(i,nsp) indicates whether a species is ever a predator
    if (trophic) {
      ofvsp_ideal(i) += FHmulti(i) - FHideal(i);
      ofvsp(i) += FHmulti(i);  }  //end of Trophic If statement

    }  //end of species loop

  //Penalty terms
  tBpen = elem_prod(Bwt,Bpen);        //Total biomass penalty for each species
  tYpen = elem_prod(Ywt,Ypen);        //Total Yr1 penalty for each species
  tRpen = elem_prod(Rwt,Rpen);        //Total recruitment penalty for each species
  tOpen = Owt*Open;                        //Total Other Food penalty

  //Diagnostics for biomass and other food penalty terms
  for (i=1; i<=nsp; i++)
    {
    if(Bpen(i)> 0 ) {
    nBpen(i)++;  lBpen(i) = nf;
    cout<<"\nSpecies: "<<i<<"  B penalty = "<<Bpen(i)<<"  tBpen =  "<<tBpen(i)<<"  nBpen = "<<nBpen(i)<<endl;}
    }  //end of species loop
  if(Open > 0)  {
    nOpen++;  lOpen = nf;
    cout<<"OF penalty = "<<Open<<"  tOpen =  "<<tOpen<<"  nOpen = "<<nOpen<<endl;  }

  //Deviation terms
  Devs = 1.e6*( square(rowsum(dAge1)) + square(rowsum(dFt)) );
  if (debug == 66 && sum(Devs) > 0.001) {
    cout<<"Age1 devs: "<<square(rowsum(dAge1))<<endl<<"Ft devs: "<<square(rowsum(dFt))<<endl<<endl; }

  //Total objective function value
  ofv = sum(ofvsp)                   + sum(tBpen) + sum(tYpen) + sum(tRpen) + tOpen + sum(Devs);
  ofv_ideal = sum(ofvsp_ideal) + sum(tBpen) + sum(tYpen) + sum(tRpen) + tOpen + sum(Devs);

  if (debug == 66 | debug == 3) {
    cout<<"ofv: "<<ofv<<"   phase: "<<current_phase()<<"   trophic: "<<trophic<<"   nf: "<<nf<<endl;
    cout<<setprecision(10)<<"ofvsp: "<<ofvsp<<"   ofv_ideal: "<<ofv_ideal<<"   ofvsp_ideal: "<<ofvsp_ideal<<endl
      <<"TotC.Res: "<<TCres<<endl
      <<"TotFIC.ResA\n"<<TSresA<<endl<<"TotFIC.ResB\n"<<TSresB<<endl
      <<"CAA.proportions: "<<CPmulti<<endl<<"FIC.proportions\n"<<SPmulti<<endl
      <<"FH.proportions: "<<FHmulti<<endl
      <<"CAA.proportions.revised: "<<CPmulti - CPideal<<endl<<"FIC.prop.revised\n"<<SPmulti - SPideal<<endl
      <<"FHprop.revised: "<<(FHmulti - FHideal)<<endl;
    cout<<"tYpen: "<<tYpen<<"  tRpen: "<<tRpen<<"   Devs: "<<Devs<<endl;
    cout<<"tOpen: "<<tOpen<<"  Last nf with Open: "<<lOpen<<"  # of nf's with Open: "<<nOpen<<endl;
    cout<<"Last nf with Bpen: "<<lBpen<<"  # of nf's with Bpen: "<<nBpen<<endl<<endl;  }


BETWEEN_PHASES_SECTION
  cout<<"\nbetween_phases_section, moving to phase: "<<current_phase()<<endl<<endl;


FINAL_SECTION
  cout<<"\nofv\n"<<ofv<<endl<<"nf\n"<<nf<<endl;
  cout<<setprecision(10)<<"ofvsp\n"<<ofvsp<<endl<<"ofv_ideal\n"<<ofv_ideal<<endl<<"ofvsp_ideal\n"<<ofvsp_ideal<<endl
    <<"TotC.Res\n"<<TCres<<endl
    <<"TotFIC.ResA\n"<<TSresA<<endl<<"TotFIC.ResB\n"<<TSresB<<endl
    <<"CAA.proportions\n"<<CPmulti<<endl<<"FIC.proportions\n"<<SPmulti<<endl
    <<"FH.proportions\n"<<FHmulti<<endl
    <<"CAA.proportions.revised\n"<<CPmulti - CPideal<<endl<<"FIC.prop.revised\n"<<SPmulti - SPideal<<endl
    <<"FHprop.revised\n"<<(FHmulti - FHideal)<<endl;
  cout<<"tYpen: "<<tYpen<<"  tRpen: "<<tRpen<<"   Devs: "<<Devs<<endl;
  cout<<"tOpen: "<<tOpen<<"  Last nf with Open: "<<lOpen<<"  # of nf's with Open: "<<nOpen<<endl;
  cout<<"Last nf with Bpen\n"<<lBpen<<endl<<"# of nf's with Bpen: "<<nBpen<<endl<<endl;


REPORT_SECTION
  report<<"mgc\n"<<objective_function_value::pobjfun->gmax<<endl;
  report<<"ofv\n"<<ofv<<endl;
  report<<"ofv_ideal\n"<<ofv_ideal<<endl;
  report<<"nsp\n"<<nsp<<endl;
  report<<"nf\n"<<nf<<endl;
  report<<"trophic\n"<<trophic<<endl;
  report<<"nFIC\n"<<nFIC<<endl;

  imatrix yrs(1,nsp,fyr,lyr);
  for (i = 1; i<=nsp; i++)  { yrs(i).fill_seqadd(fyr(i),1); }
  report<<"yrs\n"<<yrs<<endl;
  report<<"nyr\n"<<nyr<<endl;
  report<<"FH.fyr\n"<<FHfyr<<endl;
  report<<"FH.lyr\n"<<FHlyr<<endl;
  report<<"nage\n"<<nage<<endl;
  report<<"FICnseg\n"<<nseg<<endl;
  report<<"M1nseg\n"<<nMseg<<endl;
  report<<"agePR\n"<<agePR<<endl;
  report<<"ageFR\n"<<ageFR<<endl;
  report<<"ficFR\n"<<ficFR<<endl;
  report<<"FICfage\n"<<FICfage<<endl;
  report<<"FIClage\n"<<FIClage<<endl;

  report<<"binsize\n"<<binsize<<endl;
  report<<"nbins\n"<<nbins<<endl;
  report<<"EcoB\n"<<EcoB<<endl;
  report<<"pred\n"<<pred<<endl;
  report<<"prey\n"<<prey<<endl;
  report<<"rhosp\n"<<rhosp<<endl;

  report<<"Ft\n"<<Ft<<endl;
  report<<"nFt\n"<<nFt<<endl;
  report<<"nsel\n"<<nsel<<endl;
  report<<"FICnsel\n"<<FICnsel<<endl;

  report<<"obs.sumC\n"<<sumC<<endl;
  report<<"obs.totc\n"<<TotC<<endl;
  report<<"pred.totc\n"<<TotC_hat<<endl;
  report<<"resid.totc\n"<<TCresid<<endl;

  report<<"Age1\n"<<Age1<<endl;
  report<<"Eta\n"<<Eta<<endl;
  report<<"Sig1\n"<<Sig1<<endl;
  report<<"Sig2\n"<<Sig2<<endl;
  report<<"Rho\n"<<Rho<<endl;

  for (i=1; i<=nsp; i++)
    {
    report<<"FICmonth."<<i<<endl<<FICmon(i)<<endl;
    report<<"sel."<<i<<endl<<s(i)<<endl;
    report<<"FICsel."<<i<<endl<<FICs(i)<<endl;
    report<<"FIC.q."<<i<<endl<<mfexp(q(i))<<endl;
    report<<"FIC.yr."<<i<<endl<<FICyr(i)<<endl;
    report<<"M1."<<i<<endl<<M1seg(i)<<endl;

    report<<"dAge1."<<i<<endl<<dAge1(i)<<endl;
    report<<"dFt."<<i<<endl<<dFt(i)<<endl;

    report<<"Wt."<<i<<endl<<Wt(i)<<endl;

    report<<"obs.cprop."<<i<<endl<<Cprop(i)<<endl;
    report<<"pred.cprop."<<i<<endl<<Cprop_hat(i)<<endl;

    report<<"N."<<i<<endl<<N(i)<<endl;
    report<<"iM2."<<i<<endl<<iM2(i)<<endl;
    report<<"M2."<<i<<endl<<M2(i)<<endl;
    report<<"F."<<i<<endl<<F(i)<<endl;
    report<<"Z."<<i<<endl<<Z(i)<<endl;

    report<<"obs.totFIC."<<i<<endl<<TotFIC(i)<<endl;
    report<<"pred.totFIC."<<i<<endl<<TotFIC_hat(i)<<endl;
    report<<"toteps."<<i<<endl<<toteps(i)<<endl;
    report<<"resid.totFIC."<<i<<endl<<TSresid(i)<<endl;

    report<<"Age1CV."<<i<<endl<<(std_dev(Age1(i))/mean(Age1(i)))<<endl;

    for (j = 1; j<= nFIC; j++)
      {
      report<<"obs.sprop."<<j<<".sp."<<i<<endl<<Sprop(i)(j)<<endl;
      report<<"pred.sprop."<<j<<".sp."<<i<<endl<<Sprop_hat(i)(j)<<endl;
      }  //end of nFIC loop

    report<<"obs.fh."<<i<<endl<<FH(i)<<endl;
    report<<"pred.fh."<<i<<endl<<FH_hat(i)<<endl;
    report<<"consum."<<i<<endl<<Consum(i)<<endl;
    report<<"G."<<i<<endl<<G(i)<<endl;
    report<<"sumCon."<<i<<endl<<sumCon(i)<<endl;

    }  //end of species loop

  report<<"TCwt\n"<<TCwt<<endl;
  report<<"TSwt\n"<<TSwt<<endl;
  report<<"CPwt\n"<<CPwt<<endl;
  report<<"SPwt\n"<<SPwt<<endl;
  report<<"FHwt\n"<<FHwt<<endl;
  report<<"nBpen\n"<<nBpen<<endl;
  report<<"lBpen\n"<<lBpen<<endl;
  report<<"Bthres\n"<<Bthres<<endl;
  report<<"Bwt\n"<<Bwt<<endl;
  report<<"Ywt\n"<<Ywt<<endl;
  report<<"Rwt\n"<<Rwt<<endl;
  report<<"Rthres\n"<<Rthres<<endl;

  report<<"TCres\n"<<TCres<<endl;
  report<<"CPmulti\n"<<CPmulti<<endl;
  report<<"CPideal\n"<<CPideal<<endl;
  report<<"TSresA\n"<<TSresA<<endl;
  report<<"TSresB\n"<<TSresB<<endl;
  report<<"SPmulti\n"<<SPmulti<<endl;
  report<<"SPideal\n"<<SPideal<<endl;
  report<<"FHmulti\n"<<FHmulti<<endl;
  report<<"FHideal\n"<<FHideal<<endl;
  report<<"tYpen\n"<<tYpen<<endl;
  report<<"tRpen\n"<<tRpen<<endl;

  report<<"tOpen\n"<<tOpen<<endl;
  report<<"nOpen\n"<<nOpen<<endl;
  report<<"lOpen\n"<<lOpen<<endl;

  report<<"Devs\n"<<Devs<<endl;
  report<<"ofvsp\n"<<ofvsp<<endl;
  report<<"ofvsp_ideal\n"<<ofvsp_ideal<<endl;
  report<<"sim\n"<<sim<<endl;


