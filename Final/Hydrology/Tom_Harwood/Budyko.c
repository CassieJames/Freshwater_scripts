#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rdefines.h>


//*************************************************************************
// Calculate Net radiation.. does what it says on the box, based on Temp range
// Uses procedures from Allen et al
// Arguments:
//
// Returns: double Net Radiation  Rnl
//*************************************************************************
double CalculateNetRadiation(const double arg_d_lat,
                             const double arg_d_Z,
                             const double arg_d_dr,
                             const double arg_d_declination,
                             const double arg_f_kRs,
                             const double arg_d_Tmax,
                             const double arg_d_Tmin)
{
	double d_omega; // sunset hour angle
	double d_Ra;  // Radiation at top of atmosphere
	double d_Rs;  // incoming shortwave
	double d_Rn;  // Net Radiation
	double d_Rso; // outgoing shortwave
	double d_Rns; // Net shortwave
	double d_Rnl; //Net longwave
	double d_sTminK4, d_sTmaxK4, d_sigma_etc;  //Stephan Boltzmann corrections
	double d_ea,d_ea_etc,d_rso_etc;     // dewpoint stuff
	//Sunseet hour angle
	d_omega=acos( -tan(arg_d_lat)*tan(arg_d_declination)  );    // Allen eq 25

	// Calc rad at top of atmosphere
	d_Ra=37.58603136*arg_d_dr*(	d_omega*sin(arg_d_lat)*sin(arg_d_declination)+cos(arg_d_lat)*cos(arg_d_declination)*sin(d_omega) );     // Allen eq 21
	// Now calculate Shortwave radiation
	d_Rs=arg_f_kRs*sqrt(arg_d_Tmax-arg_d_Tmin)*d_Ra;    // Allen eq 50
	d_Rso=(0.75+0.00002*arg_d_Z)*d_Ra; // Allen eq 37
	d_Rns=0.77*d_Rs;  // for grass ref albedo
	d_sTmaxK4=0.5195*arg_d_Tmax+26.361;         // steph boltz correction
	d_sTminK4=0.5195*arg_d_Tmin+26.361;
	d_sigma_etc= (d_sTmaxK4+d_sTminK4)/2;      // Allen eq 39
	d_ea=0.6108*exp((17.27*arg_d_Tmin)/ (arg_d_Tmin+237.3));  // Allen eq 14
	d_ea_etc=0.34-0.14*sqrt(d_ea);   // Allen eq39
	d_rso_etc=1.35*(d_Rs/d_Rso)-0.35;
	d_Rnl=d_sigma_etc*d_ea_etc*d_rso_etc;
	d_Rn=d_Rns-d_Rnl;

	return (d_Rn);
} // end func CalcNetRadiation

//*************************************************************************
// Calculate Preistley-Taylor evaporation..
// Uses procedures from Allen et al
// Arguments:
//
// Returns: double Potential evaporation calced as Preistly-Taylor
//*************************************************************************
double CalculatePTEvaporation(const double arg_d_T,   // mean temp
                              const double arg_d_Z,      // altitude METRES
                              const double arg_d_Rn)  // net radiation
{
	double d_P; // pressure
	double d_esT;   // sat vap pressure
	double d_lambda; //lat heat vap water
	double d_gamma; // Psychometric constant
	double d_Delta; // slope of sat vp curve
	double d_E_pot; // potential evaporation
	//Pressure
	d_P=101.38*pow(((293-0.0065*arg_d_Z)/293),5.26);
	//Saturation Vapour Pressure
	d_esT=0.6108*exp(17.27*arg_d_T/ (arg_d_T+273.3));
	//Latent heat of vapourisation of water
	d_lambda=2.501-0.002361*arg_d_T;
	//Psychometric constant
	d_gamma=0.0016286*d_P/d_lambda;
	//Slope of saturation vapour pressure curve
	d_Delta=4098*d_esT/pow((arg_d_T+237.3),2);
	//Potential Evapotranspiration store to array
	d_E_pot=1.26*arg_d_Rn/(d_lambda*(1+d_gamma/d_Delta));
	
	if (!R_FINITE(d_E_pot)) Rprintf ("arg_d_T -- %f, arg_d_Z -- %f, arg_d_Rn -- %f, d_P -- %f, d_esT -- %f, d_lambda -- %f, d_gamma -- %f, d_Delta -- %f, d_E_pot -- %f\n", arg_d_T, arg_d_Z, arg_d_Rn, d_P, d_esT, d_lambda, d_gamma, d_Delta,d_E_pot );
		
	
	// all done, return
	return (d_E_pot);
}// end func Calc PT Evaporation

//---------------------------------------------------------------------------
//*****************************************************************************
// Run Budkyo Bucket Model
// Runs a bucket model at 5km for every month, based on precipitation ,
// temperature and ground characteristics
//
// Outputs
// Monthly grids of ACTUAL EVAPORATION (for further downscaling)
// Monthly grids of RUNOFF (for Cassie)
SEXP RunBudykoBucketModel_5km ( SEXP R_dem, // vector of dem
							SEXP R_rain, //matrix of rows being positions and columns being monthly rainfall data
							SEXP R_tmin, //matrix of rows being positions and columns being monthly min temperature data
							SEXP R_tmax, //matrix of rows being positions and columns being monthly max temperature data
							SEXP R_V_max, // vector of Max soil water(total PAWCH)
							SEXP R_kRs, // vector of Hargreaves coast/interior constant
							SEXP R_lats_radians, //vector of latitudes in radians ... order associated with positions in previous vectors and matrices
							SEXP R_rows // number of rows to cycle through
							)
{
	//define the pointers for the data
	PROTECT(R_dem = coerceVector(R_dem, REALSXP)); double *d_Z_elev = REAL(R_dem);
	PROTECT(R_rain = coerceVector(R_rain, REALSXP)); double *d_rain = REAL(R_rain);
	PROTECT(R_tmin = coerceVector(R_tmin, REALSXP)); double *d_tmin = REAL(R_tmin);
	PROTECT(R_tmax = coerceVector(R_tmax, REALSXP)); double *d_tmax = REAL(R_tmax);
	PROTECT(R_V_max = coerceVector(R_V_max, REALSXP)); double *d_V_max = REAL(R_V_max);
	PROTECT(R_kRs = coerceVector(R_kRs, REALSXP)); double *d_kRs = REAL(R_kRs);
	PROTECT(R_lats_radians = coerceVector(R_lats_radians, REALSXP)); double *d_lats_radians = REAL(R_lats_radians);
	PROTECT(R_rows = coerceVector(R_rows, INTSXP)); int n_rows = INTEGER(R_rows)[0];
	
	//define and protect some outputs
	SEXP R_E_act; // output matrix, identical to input rain, that stores actual evaporation
	SEXP R_E_pot; // output matrix, identical to input rain, that stores potential evaporation
	SEXP R_Q_run; // output matrix, identical to input rain, that stores runoff
	SEXP R_Rn; // output matrix, identical to input rain, that stores net radiation
	PROTECT(R_E_act = allocMatrix(REALSXP, n_rows, 12)); double *d_E_act = REAL(R_E_act);
	PROTECT(R_E_pot = allocMatrix(REALSXP, n_rows, 12)); double *d_E_pot = REAL(R_E_pot);
	PROTECT(R_Q_run = allocMatrix(REALSXP, n_rows, 12)); double *d_Q_run = REAL(R_Q_run);
	PROTECT(R_Rn = allocMatrix(REALSXP, n_rows, 12)); double *d_Rn = REAL(R_Rn);

	SEXP R_V_store; // Soil water actually present
	PROTECT(R_V_store = allocVector(REALSXP, n_rows)); double *d_V_store = REAL(R_V_store);

	//Rprintf ("nrows input is %i\n", n_rows);

	//define some variables
	int i_row;  // row col counters
	int i_month; // month of year  0-11
	int i_year;  // year for Budyko loop
	double d_J[12]={15,45,74,105,135,166,196,227,258,288,319,349};   // Julian day for each month
	double d_n_days[12]={31,28.25,31,30,31,30,31,31,30,31,30,31};   // Julian day for each month
	double d_dr;
	double d_declination;
	double d_E_pot_day; // daily potential evap

	//set all outputs to NA & d_V_Store as half possible storage
	for (i_row=0; i_row<n_rows; i_row++) {
		d_V_store[i_row] = d_V_max[i_row] ;// / 2;
		for (i_month=0; i_month<12; i_month++) {
			d_E_act[i_row+i_month*n_rows] = NA_REAL;
			d_E_pot[i_row+i_month*n_rows] = NA_REAL;
			d_Q_run[i_row+i_month*n_rows] = NA_REAL;
			d_Rn[i_row+i_month*n_rows] = NA_REAL;
		}
	}

	// Step 1 Calculate monthly Potential Evaporation using Preistley Taylor
	// need max and min temp, Z
	for (i_month=0; i_month<12; i_month++) {
		d_dr=1 + 0.033*cos(2*3.141592654*d_J[i_month]/365); //Allen eq23
		d_declination=0.409*sin((2*3.141592654*d_J[i_month]/365)-1.39);  // Allen eq 24
		for (i_row=0; i_row<n_rows; i_row++) {
			// Calc rad at top of atmosphere
			d_Rn[i_row+i_month*n_rows] = CalculateNetRadiation(d_lats_radians[i_row],    // latitiude RADIANS
																d_Z_elev[i_row],      // altitude METRES
																d_dr,     //
																d_declination,  //declination
																d_kRs[i_row],
																d_tmax[i_row+i_month*n_rows],
																d_tmin[i_row+i_month*n_rows]);
			// Calc potential evaporation Priestley Taylor
			double d_T = (d_tmax[i_row+i_month*n_rows]+d_tmin[i_row+i_month*n_rows])/2;
			d_E_pot_day=CalculatePTEvaporation( d_T, // mean temp
											d_Z_elev[i_row],// altitude
											d_Rn[i_row+i_month*n_rows]) ; // net rad
			//Sum monthly Evap
			d_E_pot[i_row+i_month*n_rows]=d_n_days[i_month]*d_E_pot_day ;                
			//Rprintf ("nrows input is %i -- %i -- %i -- %f -- %f\n", i_month, i_row, i_row+i_month*n_rows, d_Rn[i_row+i_month*n_rows],d_E_pot[i_row+i_month*n_rows] );
		}
	}

	// do the Budyko work ... go round for 3 years to near equilibrium then export
	for(i_year=0;i_year<5;i_year++) {
			// Loop through all the months
			for(i_month=0;i_month<12;i_month++)     {
					for(i_row=0;i_row<n_rows;i_row++) {
							// Do bucket
							// Add precipitation to soil
							double d_W=d_V_store[i_row] + d_rain[i_row+i_month*n_rows];
							// do Budyko
							d_E_act[i_row+i_month*n_rows]= (d_E_pot[i_row+i_month*n_rows]*d_W)/pow((pow(d_W,1.9)+pow(d_E_pot[i_row+i_month*n_rows],1.9)),(1/1.9));
							// How much water is left?
							d_V_store[i_row]=d_W-d_E_act[i_row+i_month*n_rows];
							// Calc runoff
							if(d_V_store[i_row]>d_V_max[i_row]) {// runoff
									d_Q_run[i_row+i_month*n_rows]=d_V_store[i_row]-d_V_max[i_row];
									d_V_store[i_row]=d_V_max[i_row];  // soil stays full
							} else { d_Q_run[i_row+i_month*n_rows]=0; }
					}// end for i_row
			}// end for i_month
	}  // end for i_year

	//setup all outputs
	SEXP res; PROTECT(res = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(res, 0, R_E_act);
	SET_VECTOR_ELT(res, 1, R_E_pot);
	SET_VECTOR_ELT(res, 2, R_Q_run);
	SET_VECTOR_ELT(res, 3, R_Rn);

	UNPROTECT(14);
	return(res);
}// end func Run Budyko Bucket

//---------------------------------------------------------------------------
//*****************************************************************************
// Run Budkyo Bucket Model given matrices where rows are locations and columns 12 months
// Runs a bucket model at 5km for every month, based on precipitation ,
// temperature and ground characteristics
//
// Outputs
// Monthly grids of ACTUAL EVAPORATION (for further downscaling)
// Monthly grids of RUNOFF 
SEXP BudykoBucketModelStatic ( SEXP R_dem, // vector of dem
						SEXP R_rain, //matrix of rows being positions and columns being monthly rainfall data
						SEXP R_tmin, //matrix of rows being positions and columns being monthly min temperature data
						SEXP R_tmax, //matrix of rows being positions and columns being monthly max temperature data
						SEXP R_V_max, // vector of Max soil water(total PAWCH)
						SEXP R_kRs, // vector of Hargreaves coast/interior constant
						SEXP R_lats_radians, //vector of latitudes in radians ... order associated with positions in previous vectors and matrices
						SEXP R_rows // number of rows to cycle through
						)
{
	//define the pointers for the data
	PROTECT(R_dem = coerceVector(R_dem, REALSXP)); double *d_Z_elev = REAL(R_dem);
	PROTECT(R_rain = coerceVector(R_rain, REALSXP)); double *d_rain = REAL(R_rain);
	PROTECT(R_tmin = coerceVector(R_tmin, REALSXP)); double *d_tmin = REAL(R_tmin);
	PROTECT(R_tmax = coerceVector(R_tmax, REALSXP)); double *d_tmax = REAL(R_tmax);
	PROTECT(R_V_max = coerceVector(R_V_max, REALSXP)); double *d_V_max = REAL(R_V_max);
	PROTECT(R_kRs = coerceVector(R_kRs, REALSXP)); double *d_kRs = REAL(R_kRs);
	PROTECT(R_lats_radians = coerceVector(R_lats_radians, REALSXP)); double *d_lats_radians = REAL(R_lats_radians);
	PROTECT(R_rows = coerceVector(R_rows, INTSXP)); int n_rows = INTEGER(R_rows)[0];
	
	//define and protect some outputs
	SEXP R_E_act; // output matrix, identical to input rain, that stores actual evaporation
	SEXP R_E_pot; // output matrix, identical to input rain, that stores potential evaporation
	SEXP R_Q_run; // output matrix, identical to input rain, that stores runoff
	SEXP R_Rn; // output matrix, identical to input rain, that stores net radiation
	PROTECT(R_E_act = allocMatrix(REALSXP, n_rows, 12)); double *d_E_act = REAL(R_E_act);
	PROTECT(R_E_pot = allocMatrix(REALSXP, n_rows, 12)); double *d_E_pot = REAL(R_E_pot);
	PROTECT(R_Q_run = allocMatrix(REALSXP, n_rows, 12)); double *d_Q_run = REAL(R_Q_run);
	PROTECT(R_Rn = allocMatrix(REALSXP, n_rows, 12)); double *d_Rn = REAL(R_Rn);

	SEXP R_V_store; // Soil water actually present
	PROTECT(R_V_store = allocVector(REALSXP, n_rows)); double *d_V_store = REAL(R_V_store);

	//Rprintf ("nrows input is %i\n", n_rows);

	//define some variables
	int i_row;  // row col counters
	int i_month; // month of year  0-11
	int i_year;  // year for Budyko loop
	double d_J[12]={15,45,74,105,135,166,196,227,258,288,319,349};   // Julian day for each month
	double d_n_days[12]={31,28.25,31,30,31,30,31,31,30,31,30,31};   // Julian day for each month
	double d_dr;
	double d_declination;
	double d_E_pot_day; // daily potential evap

	//set all outputs to NA & d_V_Store as half possible storage
	for (i_row=0; i_row<n_rows; i_row++) {
		d_V_store[i_row] = d_V_max[i_row] ;// / 2;
		for (i_month=0; i_month<12; i_month++) {
			d_E_act[i_row+i_month*n_rows] = NA_REAL;
			d_E_pot[i_row+i_month*n_rows] = NA_REAL;
			d_Q_run[i_row+i_month*n_rows] = NA_REAL;
			d_Rn[i_row+i_month*n_rows] = NA_REAL;
		}
	}

	// Step 1 Calculate monthly Potential Evaporation using Preistley Taylor
	// need max and min temp, Z
	for (i_month=0; i_month<12; i_month++) {
		d_dr=1 + 0.033*cos(2*3.141592654*d_J[i_month]/365); //Allen eq23
		d_declination=0.409*sin((2*3.141592654*d_J[i_month]/365)-1.39);  // Allen eq 24
		for (i_row=0; i_row<n_rows; i_row++) {
			// Calc rad at top of atmosphere
			d_Rn[i_row+i_month*n_rows] = CalculateNetRadiation(d_lats_radians[i_row],    // latitiude RADIANS
																d_Z_elev[i_row],      // altitude METRES
																d_dr,     //
																d_declination,  //declination
																d_kRs[i_row],
																d_tmax[i_row+i_month*n_rows],
																d_tmin[i_row+i_month*n_rows]);
			// Calc potential evaporation Priestley Taylor
			double d_T = (d_tmax[i_row+i_month*n_rows]+d_tmin[i_row+i_month*n_rows])/2;
			d_E_pot_day=CalculatePTEvaporation( d_T, // mean temp
											d_Z_elev[i_row],// altitude
											d_Rn[i_row+i_month*n_rows]) ; // net rad
			//Sum monthly Evap
			d_E_pot[i_row+i_month*n_rows]=d_n_days[i_month]*d_E_pot_day ;                
			//Rprintf ("nrows input is %i -- %i -- %i -- %f -- %f\n", i_month, i_row, i_row+i_month*n_rows, d_Rn[i_row+i_month*n_rows],d_E_pot[i_row+i_month*n_rows] );
		}
	}

	// do the Budyko work ... go round for 3 years to near equilibrium then export
	for(i_year=0;i_year<5;i_year++) {
			// Loop through all the months
			for(i_month=0;i_month<12;i_month++)     {
					for(i_row=0;i_row<n_rows;i_row++) {
							// Do bucket
							// Add precipitation to soil
							double d_W=d_V_store[i_row] + d_rain[i_row+i_month*n_rows];
							// do Budyko
							if (d_W>0) {
								d_E_act[i_row+i_month*n_rows]= (d_E_pot[i_row+i_month*n_rows]*d_W)/pow((pow(d_W,1.9)+pow(d_E_pot[i_row+i_month*n_rows],1.9)),(1/1.9));
							} else {
								d_E_act[i_row+i_month*n_rows] = 0;
							}
							// How much water is left?
							d_V_store[i_row]=d_W-d_E_act[i_row+i_month*n_rows];
							if (d_V_store[i_row]<0) d_V_store[i_row] = d_V_store[i_row];	
							// Calc runoff
							if(d_V_store[i_row]>d_V_max[i_row]) {// runoff
									d_Q_run[i_row+i_month*n_rows]=d_V_store[i_row]-d_V_max[i_row];
									d_V_store[i_row]=d_V_max[i_row];  // soil stays full
							} else { d_Q_run[i_row+i_month*n_rows]=0; }
					}// end for i_row
			}// end for i_month
	}  // end for i_year

	//setup all outputs
	SEXP res; PROTECT(res = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(res, 0, R_E_act);
	SET_VECTOR_ELT(res, 1, R_E_pot);
	SET_VECTOR_ELT(res, 2, R_Q_run);
	SET_VECTOR_ELT(res, 3, R_Rn);

	UNPROTECT(14);
	return(res);
}// end func Run Budyko Bucket

//---------------------------------------------------------------------------
//*****************************************************************************
// Run Budkyo Bucket Model given vectors of data representing a single location but values are a time series where rows are locations and columns 12 months
// Runs a bucket model at 5km for every month, based on precipitation ,
// temperature and ground characteristics
//
// Outputs
// Monthly grids of ACTUAL EVAPORATION (for further downscaling)
// Monthly grids of RUNOFF 
SEXP BudykoBucketModelDynamic ( SEXP R_dem, // vector of dem
						SEXP R_rain, //matrix of rows being positions and columns being monthly rainfall data
						SEXP R_tmin, //matrix of rows being positions and columns being monthly min temperature data
						SEXP R_tmax, //matrix of rows being positions and columns being monthly max temperature data
						SEXP R_V_max, // vector of Max soil water(total PAWCH)
						SEXP R_kRs, // vector of Hargreaves coast/interior constant
						SEXP R_lats_radians, //vector of latitudes in radians ... order associated with positions in previous vectors and matrices
						SEXP R_rows, // number of rows to cycle through
						SEXP R_n_months // number of rows to cycle through
						)
{
	//define the pointers for the data
	PROTECT(R_dem = coerceVector(R_dem, REALSXP)); double *d_Z_elev = REAL(R_dem);
	PROTECT(R_rain = coerceVector(R_rain, REALSXP)); double *d_rain = REAL(R_rain);
	PROTECT(R_tmin = coerceVector(R_tmin, REALSXP)); double *d_tmin = REAL(R_tmin);
	PROTECT(R_tmax = coerceVector(R_tmax, REALSXP)); double *d_tmax = REAL(R_tmax);
	PROTECT(R_V_max = coerceVector(R_V_max, REALSXP)); double *d_V_max = REAL(R_V_max);
	PROTECT(R_kRs = coerceVector(R_kRs, REALSXP)); double *d_kRs = REAL(R_kRs);
	PROTECT(R_lats_radians = coerceVector(R_lats_radians, REALSXP)); double *d_lats_radians = REAL(R_lats_radians);
	PROTECT(R_rows = coerceVector(R_rows, INTSXP)); int n_rows = INTEGER(R_rows)[0];
	PROTECT(R_n_months = coerceVector(R_n_months, INTSXP)); int n_months = INTEGER(R_n_months)[0];
	
	//define and protect some outputs
	SEXP R_E_act; // output matrix, identical to input rain, that stores actual evaporation
	SEXP R_E_pot; // output matrix, identical to input rain, that stores potential evaporation
	SEXP R_Q_run; // output matrix, identical to input rain, that stores runoff
	SEXP R_Rn; // output matrix, identical to input rain, that stores net radiation
	PROTECT(R_E_act = allocMatrix(REALSXP, n_rows, n_months)); double *d_E_act = REAL(R_E_act);
	PROTECT(R_E_pot = allocMatrix(REALSXP, n_rows, n_months)); double *d_E_pot = REAL(R_E_pot);
	PROTECT(R_Q_run = allocMatrix(REALSXP, n_rows, n_months)); double *d_Q_run = REAL(R_Q_run);
	PROTECT(R_Rn = allocMatrix(REALSXP, n_rows, n_months)); double *d_Rn = REAL(R_Rn);

	SEXP R_V_store; // Soil water actually present
	PROTECT(R_V_store = allocVector(REALSXP, n_rows)); double *d_V_store = REAL(R_V_store);

	//Rprintf ("nrows input is %i\n", n_rows);

	//define some variables
	int i_row;  // row col counters
	int i_month; // month of year  0-11
	double d_J[12]={15,45,74,105,135,166,196,227,258,288,319,349};   // Julian day for each month
	double d_n_days[12]={31,28.25,31,30,31,30,31,31,30,31,30,31};   // Julian day for each month
	double d_dr;
	double d_declination;
	double d_E_pot_day; // daily potential evap

	//set all outputs to NA & d_V_Store as half possible storage
	for (i_row=0; i_row<n_rows; i_row++) {
		d_V_store[i_row] = d_V_max[i_row] ;// / 2;
		for (i_month=0; i_month<n_months; i_month++) {
			d_E_act[i_row+i_month*n_rows] = NA_REAL;
			d_E_pot[i_row+i_month*n_rows] = NA_REAL;
			d_Q_run[i_row+i_month*n_rows] = NA_REAL;
			d_Rn[i_row+i_month*n_rows] = NA_REAL;
		}
	}

	// Step 1 Calculate monthly Potential Evaporation using Preistley Taylor
	// need max and min temp, Z
	for (i_month=0; i_month<n_months; i_month++) {
		d_dr=1 + 0.033*cos(2*3.141592654*d_J[i_month%12]/365); //Allen eq23
		d_declination=0.409*sin((2*3.141592654*d_J[i_month%12]/365)-1.39);  // Allen eq 24
		//Rprintf ("month is %f\n",d_J[i_month%12]);
		for (i_row=0; i_row<n_rows; i_row++) {
			// Calc rad at top of atmosphere
			d_Rn[i_row+i_month*n_rows] = CalculateNetRadiation(d_lats_radians[i_row],    // latitiude RADIANS
																d_Z_elev[i_row],      // altitude METRES
																d_dr,     //
																d_declination,  //declination
																d_kRs[i_row],
																d_tmax[i_row+i_month*n_rows],
																d_tmin[i_row+i_month*n_rows]);
			// Calc potential evaporation Priestley Taylor
			double d_T = (d_tmax[i_row+i_month*n_rows]+d_tmin[i_row+i_month*n_rows])/2;
			d_E_pot_day=CalculatePTEvaporation( d_T, // mean temp
											d_Z_elev[i_row],// altitude
											d_Rn[i_row+i_month*n_rows]) ; // net rad
			//Sum monthly Evap
			d_E_pot[i_row+i_month*n_rows]=d_n_days[i_month%12]*d_E_pot_day ;                
			
			//if (!R_FINITE(d_E_pot[i_row+i_month*n_rows])) Rprintf ("month -- %i, row -- %i, position -- %i, d_Rn -- %f, E_pot -- %f\n", i_month, i_row, i_row+i_month*n_rows, d_Rn[i_row+i_month*n_rows],d_E_pot[i_row+i_month*n_rows] );
		}
	}

	// Loop through 36 months to initialize all values
	for(i_month=0;i_month<36;i_month++)     {
			for(i_row=0;i_row<n_rows;i_row++) {
					// Do bucket
					// Add precipitation to soil
					double d_W=d_V_store[i_row] + d_rain[i_row+i_month*n_rows];
					// do Budyko
					if (d_W>0) {
						d_E_act[i_row+i_month*n_rows]= (d_E_pot[i_row+i_month*n_rows]*d_W)/pow((pow(d_W,1.9)+pow(d_E_pot[i_row+i_month*n_rows],1.9)),(1/1.9));
					} else {
						d_E_act[i_row+i_month*n_rows] = 0;
					}
					//if (i_row==66) Rprintf ("eact for month %i is %f , d_V_max[i_row] is %f\n", i_month,d_E_act[i_row+i_month*n_rows],d_V_max[i_row] );
					// How much water is left?
					d_V_store[i_row]=d_W-d_E_act[i_row+i_month*n_rows];
					if (d_V_store[i_row]<0) d_V_store[i_row] = d_V_store[i_row];					
					// Calc runoff
					if(d_V_store[i_row]>d_V_max[i_row]) {// runoff
							d_Q_run[i_row+i_month*n_rows]=d_V_store[i_row]-d_V_max[i_row];
							d_V_store[i_row]=d_V_max[i_row];  // soil stays full
					} else { d_Q_run[i_row+i_month*n_rows]=0; }
			}// end for i_row
	}// end for i_month

	// do the Budyko work
	for(i_month=0;i_month<n_months;i_month++)     {
			for(i_row=0;i_row<n_rows;i_row++) {
					// Do bucket
					// Add precipitation to soil
					double d_W=d_V_store[i_row] + d_rain[i_row+i_month*n_rows];
					// do Budyko
					if (d_W>0) {
						d_E_act[i_row+i_month*n_rows]= (d_E_pot[i_row+i_month*n_rows]*d_W)/pow((pow(d_W,1.9)+pow(d_E_pot[i_row+i_month*n_rows],1.9)),(1/1.9));
					} else {
						d_E_act[i_row+i_month*n_rows] = 0;
					}
					// How much water is left?
					d_V_store[i_row]=d_W-d_E_act[i_row+i_month*n_rows];
					if (d_V_store[i_row]<0) d_V_store[i_row] = d_V_store[i_row];	
					// Calc runoff
					if(d_V_store[i_row]>d_V_max[i_row]) {// runoff
							d_Q_run[i_row+i_month*n_rows]=d_V_store[i_row]-d_V_max[i_row];
							d_V_store[i_row]=d_V_max[i_row];  // soil stays full
					} else { d_Q_run[i_row+i_month*n_rows]=0; }
			}// end for i_row
	}// end for i_month

	//setup all outputs
	SEXP res; PROTECT(res = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(res, 0, R_E_act);
	SET_VECTOR_ELT(res, 1, R_E_pot);
	SET_VECTOR_ELT(res, 2, R_Q_run);
	SET_VECTOR_ELT(res, 3, R_Rn);

	UNPROTECT(15);
	return(res);
}// end func Run Budyko Bucket

