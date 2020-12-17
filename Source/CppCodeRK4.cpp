#include <iostream>
#include <fstream>
#include<math.h>
#include <Rcpp.h>
using namespace Rcpp; 

#define dim 39
#define dimIv 96
#define dimOut 174

//Note: All pieces of code beginning with a @ will be replaced by the required code by R before compiling
//For instance @AddDim will be replaced by the dimension of the model

void Func(double t, double* y, double* parms, double* ydot, double* x) {

ydot[19] = (parms[142]) * y[19];
ydot[20] = y[20] * parms[1];
ydot[21] = y[21] * (parms[0]);
ydot[23] = y[23] * parms[107];
ydot[31] = 0.0;
ydot[36] = parms[135] * y[36];
ydot[37] = parms[136] * (parms[137] * y[36]);
x[1] = parms[19] * y[0];
x[6] = y[0]/(y[2] * parms[3]);
x[12] = y[23] * y[24]/y[22];
x[13] = y[18] * y[19] * y[24] * y[23] + y[36];
x[16] = parms[23] - parms[24] * (y[1]/y[0] - parms[19]);
x[35] = y[25] * y[23]/y[22];
x[40] = parms[143] * 2.0;
x[46] = parms[25] * (y[5] + y[29] * y[24]);
x[74] = parms[5] * y[19] * y[23];
x[82] = parms[85] + parms[144] * parms[145];
x[86] = parms[67] * y[19] * y[23];
x[2] = (x[1] - y[1]);
x[8] = std::max(parms[54], 1.0/(1.0 + pow((parms[74]/(1.0 - parms[74]) * x[12] * (1.0 + parms[58])), parms[93])));
x[9] = std::max(parms[51], 1.0/(1.0 + pow((parms[75]/(1.0 - parms[75]) * x[12] * (1.0 + parms[58])), parms[94])));
x[10] = std::max(parms[52], 1.0/(1.0 + pow((parms[73]/(1.0 - parms[73]) * x[12] * (1.0 + parms[56])), parms[91])));
x[11] = std::max(parms[53], 1.0/(1.0 + pow((parms[72]/(1.0 - parms[72]) * x[12] * (1.0 + parms[57])), parms[92])));
x[14] = parms[76] * pow((x[12]), parms[65]);
x[15] = (1.0 + x[16]) * y[11];
x[39] = (1.0 + x[40]) * x[82];
x[47] = parms[30] * (x[46] - y[13]);
x[79] = x[82] + (parms[86] - parms[85]);
ydot[13] = x[47];
ydot[14] = parms[13] * (x[10] - y[14]);
ydot[15] = parms[13] * (x[11] - y[15]);
ydot[16] = parms[13] * (x[8] - y[16]);
ydot[17] = parms[13] * (x[9] - y[17]);
ydot[18] = parms[14] * (x[14] - y[18]);
ydot[22] = parms[9] * (x[15] - y[22]);
x[3] = y[0] + x[2];
x[41] = x[39] + y[26];
x[78] = (1.0 + x[79]);
x[19] = y[22] * y[0] - y[11] * x[3] - y[27] * y[5] - x[41] * y[29] * y[25];
x[34] = (1.0/x[35]) * parms[55] * x[3];
x[65] = parms[46] * (x[3] * y[22]);
x[66] = parms[47] * (x[3] * y[22]) + (1.0 - parms[136]) * (parms[137] * y[36]);
x[72] = parms[40] + ydot[22]/y[22] + parms[41] * (ydot[22]/y[22] - parms[42]);
x[94] = ydot[22]/y[22];
ydot[38] = parms[138] * (x[66] - y[38]);
x[20] = (1.0 - parms[45]) * (x[19]);
x[49] = x[72] - parms[83] * pow(((y[5] + y[6])/y[7]), parms[100]);
x[64] = x[65] + y[38];
x[21] = x[20]/(y[22] * y[2]);
x[23] = parms[20] * x[20];
x[42] = parms[101] + parms[84] * pow(((y[29] * y[25] + y[5])/x[20]), parms[102]);
x[51] = (y[7] * x[72] + x[49] * (y[4] + y[37]))/(y[7] + y[4] + y[37]);
x[58] = 1.0 - parms[79] * pow((x[49] - ydot[22]/y[22]), parms[97]);
x[59] = 1.0 - parms[77] * pow((x[49] - ydot[22]/y[22]), parms[95]);
x[60] = 1.0 - parms[78] * pow((x[49] - ydot[22]/y[22]), parms[96]);
ydot[26] = 2.0 * (x[42] - y[26]);
x[18] = (parms[21] + parms[22] * (x[21] - ydot[22]/y[22])) * y[2];
x[50] = x[51] + y[26];
ydot[27] = parms[62] * (x[50] - y[27]);
x[22] = x[18] * y[22] - x[23];
x[26] = 1.0 + x[50];
x[28] = x[18];
ydot[2] = x[28] - parms[4] * y[2];
x[0] = y[12] + x[64] + x[28] * y[22] + x[13];
x[7] = y[16] * (y[12])/y[22] + y[17] * (x[64])/y[22] + y[15] * (x[28]) + y[14] * x[13]/(y[24] * y[23]);
x[70] = (parms[58] * (y[16] * (y[12])/y[22]) * y[23] * y[24]) + (parms[58] * (y[17] * (x[64])/y[22]) * y[23] * y[24]) + (parms[56] * (y[14] * x[13]/(y[24] * y[23])) * y[23] * y[24]) + (parms[57] * (y[15] * (x[28])) * y[23] * y[24]);
ydot[0] = parms[12] * (x[0]/y[22] - y[0]) + (parms[0] + parms[1]) * y[0];
ydot[1] = x[3] - x[0]/y[22];
x[4] = x[3] - x[7];
x[71] = ydot[22]/y[22] + (parms[50]) * (y[9]/(x[3] * y[22] - x[7] * y[23] * y[25]));
x[81] = -((y[32] * y[24] - y[28] * y[24] - y[35])/(x[3] * y[22] - x[7] * y[23] * y[24]));
x[87] = y[0] * y[22] - x[7] * y[23] * y[25];
x[5] = x[4]/(y[2] * parms[3]);
x[38] = parms[139] + parms[140] * x[81] + parms[141] * ((ydot[0] + x[2])/x[3]);
x[48] = y[27] * y[5] + x[71] * y[6] - x[49] * (y[4] + y[37]) - x[72] * y[7] + x[41] * y[29] * y[24] - x[39] * y[28] * y[24];
x[61] = parms[104] + parms[82] * pow((((1.0 + x[71])/(1.0 + x[49]))), parms[99]);
x[62] = x[4]/y[21];
x[80] = ((0.015/(1.0 + exp(-parms[64] * x[81] + parms[60]))) + parms[89]);
x[85] = x[86] * y[24] - x[71] * y[35] - x[39] * y[28] * y[24];
x[88] = y[32]/y[22]/x[4];
x[89] = (x[13] - x[7] * y[24] * y[23])/x[87];
x[90] = x[0]/y[22]/x[4];
x[91] = (x[13] - x[7] * y[24] * y[23] - x[39] * y[28] * y[24] + x[86] * y[24] - x[71] * y[35])/x[87];
x[92] = y[9]/x[87];
x[93] = x[18]/x[4];
x[95] = y[35]/x[87];
ydot[3] = y[3] * (parms[70] + parms[6] * (x[62]/y[20] - parms[8]) + parms[7] * ydot[22]/y[22]);
ydot[25] = parms[33] * (parms[34] * (pow((((1.0 + x[82])/((1.0 + x[72]) * (1.0 - x[80])))), parms[39])) * y[24] - y[25]);
x[17] = (y[3] * x[62] + y[23] * y[24] * x[7] + x[70])/x[3];
x[29] = x[0] - y[3] * x[62] - y[23] * y[24] * x[7] - y[27] * y[5] - x[41] * y[29] * y[24] - parms[137] * y[36];
x[37] = 1.0 - x[62]/y[20];
x[52] = (1.0 - parms[44]) * (x[48]);
x[67] = parms[48] * y[3] * (parms[49] * (y[20] - x[62]));
ydot[11] = parms[11] * (x[17] - y[11]);
x[27] = (1.0 + x[41]) * (y[25] + ydot[25])/y[24];
x[30] = (1.0 - parms[45]) * (x[29]);
x[53] = x[52] - x[47];
x[55] = ((1.0 - parms[43]) * y[3] * x[62] + x[67]);
x[63] = x[64] + x[67] + y[9] * x[71];
x[69] = parms[43] * y[3] * x[62] + parms[45] * x[29] + parms[44] * x[48] + x[70];
x[77] = ((1.0 + x[71]) * (1.0 - x[80]))/((ydot[25] + y[25])/y[24]);
x[25] = (x[26] - x[27]);
x[31] = x[30]/(y[22] * y[2]);
x[68] = x[69] + parms[137] * y[36] + x[49] * y[37];
x[76] = (x[77] - x[78]);
ydot[9] = x[63] - (x[68] - parms[136] * (parms[137] * y[36])) - x[72] * y[7];
x[24] = parms[59] + parms[63] * (x[25]);
x[75] = parms[66] + parms[63] * (x[76]);
ydot[35] = std::max(x[75] * x[74] * y[24], -y[35]);
x[36] = x[24] * x[22]/y[24];
ydot[29] = x[38] * x[36];
ydot[5] = x[22] - ydot[29] * y[24];
ydot[28] = ydot[29];
ydot[30] = parms[68] * ydot[29];
x[73] = (((ydot[35]/y[24] + ydot[29]) >= 0.001) ?  std::max(parms[69] * x[7] * y[23] - y[33], 0.0) :  parms[69] * x[7] * y[23] - y[33]);
ydot[32] = x[13]/y[24] - x[7] * y[23] - x[39] * y[28] + ydot[28] + x[86] + x[75] * x[74] - x[71] * y[35]/y[24];
x[32] = x[30] - x[23] - ydot[30] * y[24];
x[33] = x[20] - parms[20] * x[20] - ydot[30] * y[25];
x[43] = ydot[30];
x[84] = x[13]/y[24] + ydot[28] + x[86] - x[73] + ydot[35]/y[24];
ydot[33] = ydot[32] - x[43];
ydot[34] = x[43];
x[44] = ydot[32] - x[73];
x[56] = x[49] * y[4] + x[86] * y[24] + x[71] * y[10] + x[53] + x[33];
x[83] = x[7] * y[23] + x[39] * y[28] + x[43] + x[71] * y[35]/y[24];
ydot[24] = y[24] * parms[32] * (x[83] - x[84])/x[84];
x[54] = x[58] * (x[55]) + x[59] * (x[56]) + x[60] * (y[4] + y[10]);
x[57] = (x[55] + x[56]) - (y[12]);
ydot[10] = x[61] * x[57];
ydot[12] = parms[10] * (x[54] - y[12]);
ydot[4] = x[57] - ydot[10];
ydot[6] = ydot[9] - ydot[10] - ydot[35];
ydot[8] = parms[29] * ydot[4];
x[45] = (ydot[5] + ydot[6]) + parms[29] * (y[4] + ydot[4] + y[37] + ydot[37]) - (ydot[4] + ydot[37] + ydot[13]) - y[8];
ydot[7] = x[45];
}

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

			if(it*byT>=5 && it*byT - 5<byT) { 
parms[144] = 1.0; 
} 

			

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
			
			for(it1=0;it1<dim;it1++){
							out(it+1, dim+it1) = ydots[it1];
						}
						for(it1=0;it1<dimIv;it1++){
							out(it+1, 2*dim+it1) = (x0[it1] + 2.0*x1[it1] + 2.0*x2[it1] + x3[it1])/6.0;
						}
				
	}
	return out;
}
