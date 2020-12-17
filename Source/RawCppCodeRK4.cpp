#include <iostream>
#include <fstream>
#include<math.h>
#include <Rcpp.h>
using namespace Rcpp; 

#define dim @AddDim
#define dimIv @AddDimIv
#define dimOut @AddDimOut

//Note: All pieces of code beginning with a @ will be replaced by the required code by R before compiling
//For instance @AddDim will be replaced by the dimension of the model

@AddFunc

// [[Rcpp::export]]
Rcpp::NumericMatrix RK4(int nt, 
				   	     double byT,
						 std::vector<double> Ry0,
						 std::vector<double> Rparms) {
	int it, it1;
	double *y = &Ry0[0];
	double *parms = &Rparms[0];
	double y1[dim], y2[dim], y3[dim], ydot0[dim], ydot1[dim], ydot2[dim], ydot3[dim], ydots[dim], x0[dimIv], x1[dimIv], x2[dimIv], x3[dimIv];
	Rcpp::NumericMatrix out(nt, dimOut);

	for (it=0; it<dim;it++) {//init out vector
		out(0, it)=y[it];
	}
	
	for (it=0; it<(nt-1); it++) {

			@AddEventTime
			@AddEventVar

			Func(it*byT, y, parms, ydot0, x0);

			for (it1=0; it1<dim; it1++)
				y1[it1] = y[it1] + ydot0[it1]*0.5*byT;
			Func((it + 0.5)*byT, y1, parms, ydot1, x1);

			for (it1=0; it1<dim; it1++)
				y2[it1] = y[it1] + ydot1[it1]*0.5*byT;
			Func((it + 0.5)*byT, y2, parms, ydot2, x2);

			for (it1=0; it1<dim; it1++)
				y3[it1] = y[it1] + ydot2[it1]*byT;
			Func((it+1)*byT, y3, parms, ydot3, x3);

			for (it1=0; it1<dim; it1++) {
				ydots[it1] = (ydot0[it1] + 2.0*ydot1[it1] + 2.0*ydot2[it1] + ydot3[it1])/6.0;
				y[it1] = y[it1] + byT*ydots[it1];
				out(it+1, it1) = y[it1];
			}
			
			@AddReportingVars
				
	}
	return out;
}
