#include <math.h>
#include <stdio.h>

double PI = 3.1415926535;
double haversine(double lat1d, double lon1d, double lat2d, double lon2d)
{
  double k = 180.0/PI;
  double lat1r = lat1d/k;
  double lon1r = lon1d/k;
  double lat2r = lat2d/k;
  double lon2r = lon2d/k;
  // R = 6367000
  double R= 6367000.0;
  // dlon = lon2 - lon1
  double dlonr = lon2r- lon1r;
  // dlat = lat2 - lat1
  double dlatr = lat2r-lat1r;
  // a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  double a = 
    sin(dlatr/2.0)*(sin (dlatr/2.0))+
    cos(lat1r)*cos(lat2r)*sin(dlonr/2.0)*sin(dlonr/2.0);
  // c = 2 * atan2( sqrt(a), sqrt(1-a) )
  double c = 2*atan2(sqrt(a), sqrt(1.0 - a));
  // d = R * c
  double d = R * c;
  return d/1000.0;
}
int main(int argc, char **argv) {
  double dist = haversine(37.4375, -122.125,
			  46.979167, 142.7083);
  printf("%F\n", dist);
  return 0;
}




