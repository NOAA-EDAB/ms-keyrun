#include <admodel.h>


///Calculating the minimum positive number of a data vector
int posmin(const dvector& tmp)
{
  int ct = 0;
  int ct1 = 0;

  //dvector tmp = ph3;
  int nobs = size_count(tmp);

  int tmpout;

    //cout<<"tmp\n"<<tmp<<endl;
    //cout<<"nobs\n"<<nobs<<endl;

  for (int h = 1; h<=nobs; h++)  { if (tmp[h] > 0)  {ct++;} }
  //cout<<"ct\n"<<ct<<endl;
  if (ct == 0)  {tmpout = -99;}
  else {
    dvector newtmp(1,ct);
    for (int h = 1; h<=nobs; h++)
      {
      //cout<<"tmp[h]\n"<<tmp[h]<<endl;
      if (tmp[h] > 0)
        {
        ct1++;
        newtmp[ct1] = tmp[h];
        //cout<<"newtmp\n"<<newtmp<<endl;
        } //end of if statement
      }  //end of for loop
      //cout<<"newtmp\n"<<newtmp<<endl;
    tmpout = min(newtmp);
    }  //end of else statement
  return tmpout;
}



//modifying posfun so that eps can equal zero and renaming it posnum
dvariable  posnum(const  dvariable&x, const  double  eps, const double output, dvariable&  pen)
     //inputs:  x = variable,  eps = threshold.value,  output = return.value,  pen = penalty.variable)
{
if  (x>=eps)  {
return  x;
}  else  {
pen+=.01*square(x-eps);
return  (output);
}
}





