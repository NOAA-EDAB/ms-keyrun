//stats.cxx
/*
	Statistical distributions commonly used in ADMB programs.
	Based on the documenation found in R.
	All distributions are in negative log-space.
	Author: Martell
	Date: 12/18/2007

	NOTE: I ALTERED COLMEANS FUNCTION ON 2/10/09
*/


#include <admodel.h>

const double pi=3.141593;
//plogis
dvariable plogis(const dvariable& x, const double& mu, const dvariable& std)
{
	return 1./(1.+mfexp((mu-x)/std));
}

dvar_vector plogis(const dvector& x, const dvariable& mu, const dvariable& std)
{
	return 1./(1.+mfexp((mu-x)/std));
}

dvector plogis(const dvector& x, const double& mu, const double& std)
{
	return 1./(1.+mfexp((mu-x)/std));
}

dvar_vector plogis(const dvar_vector& x, const dvariable& mu, const dvariable& std)
{
	return 1./(1.+mfexp((mu-x)/std));
}


//uniform distribution
/*
dvariable dunif(const dvariable& x, const double min, const double max)
{
	return log(max-min);
}
*/
//beta distribution
dvariable dbeta(const dvariable& x, const double a, const double b)
{
	return - gammln(a+b)+(gammln(a)+gammln(b))-(a-1.)*log(x)-(b-1.)*log(1.-x);
}

//inverse-gamma
dvariable dinvgamma(const dvariable& x, const double a, const double b)
{
	return -a*log(b)+gammln(a)+(a+1)*log(x)+b/x;
}

//normal distribution
dvariable dnorm(const dvariable& x, const double& mu, const double& std)
{
	double pi=3.141593;
	return 0.5*log(2.*pi)+log(std)+0.5*square(x-mu)/(std*std);
}

dvariable dnorm(const dvar_vector& residual, const double& std)
{
	double pi=3.141593;
	long n=size_count(residual);
	dvariable SS=norm2(residual);
	return(n*(0.5*log(2.*pi)+log(std))+0.5*SS/(std*std));
	//return(n*(0.5*log(2.*pi)-log(std))+0.5*SS*(std*std));
}

dvariable dnorm(const dvar_vector& residual, const dvariable std)
{
	double pi=3.141593;
	long n=size_count(residual);
	dvariable SS=norm2(residual);
	return(n*(0.5*log(2.*pi)+log(std))+0.5*SS/(std*std));
	//return(n*(0.5*log(2.*pi)-log(std))+0.5*SS*(std*std));
}


dvariable dnorm(const dvar_vector& residual, const dvector std)
{
	double pi=3.141593;
	int n=size_count(residual);
	dvector var=elem_prod(std,std);
	dvar_vector SS=elem_prod(residual,residual);
	return(0.5*n*log(2.*pi)+sum(log(std))+sum(elem_div(SS,var)));
}

dvariable dnorm(const dvar_vector& residual, const dvar_vector std)
{
	double pi=3.141593;
	int n=size_count(residual);
	dvar_vector var=elem_prod(std,std);
	dvar_vector SS=elem_prod(residual,residual);
	return(0.5*n*log(2.*pi)+sum(log(std))+sum(elem_div(SS,2.*var)));
}

dvariable dnorm(const dvar_matrix& r, const double sig)
{
		int n;  // number of observations
		n=(r.rowmax()-r.rowmin()+1) * (r.colmax()-r.colmin() + 1);
		return 0.5*n*log(2.*pi)+norm2(r)/(2.*sig*sig);
}


//log normal distribution
dvariable dlnorm(const dvariable& x, const double& mu, const double& std)
{
	double pi=3.141593;
	return 0.5*log(2.*pi)+log(std)+log(x)+square(log(x)-mu)/(2.*std*std);
}

dvariable dlnorm(const dmatrix& obs, const dvar_matrix& pred, const double& std)
{
		//Martell is not happy with this yet, so don't use it!  You've been warned.
		// If this is Frisk, Yer Screwed!
		int n;  // number of observations
		n=(obs.rowmax()-obs.rowmin()+1) * (obs.colmax()-obs.colmin() + 1);
		return n*(0.5*log(2.*pi)+log(std) )+ sum(log(obs)) +norm2(log(obs)-log(pred))/(2.*std*std);
}

// log multinomial distribution
dvariable dmultinom(const dvector& x, const dvar_vector& p)
{
	double n=sum(x);
	return -gammln(n+1.)+sum(gammln(x+1.))-x*log(p/sum(p));
}

double neff(const dvector& obs, const dvar_vector& pred)
{
	dvector resid=value(obs-pred);
	dvector var=value(elem_prod(1.-pred,pred));
	return sum(var)/norm2(resid);
}

//poisson distribution
dvariable dpois(const double& x, const dvariable& lambda)
{
	return -x*log(lambda)+lambda+gammln(x+1);
}


//robust normal approximation to the multinomial distribution
dvariable multifan(const dmatrix oprop,const dvar_matrix& pprop, const int& Nsamp)
{	//Vivian Haist.
    dvariable extra=0.1/14.;
    dvar_matrix resid=elem_div((oprop-pprop),sqrt((elem_prod(pprop,1.-pprop)+extra)/Nsamp));
    return sum(0.5*log(elem_prod(pprop,1. -pprop)+extra) -log(mfexp(-0.5*elem_prod(resid,resid))+0.01));
}

dvariable multifan(const int& n, const dmatrix obs, const dvar_matrix pred,double& nef)
{
	int A=obs.colmax()-obs.colmin()+1;
	//dvar_matrix xi=(elem_prod(1.-pred,pred)+0.1/A)/n; //variance from Fourniers paper.
	dvar_matrix xi=(elem_prod(1.-obs,obs)+0.1/A)/n;	 //variance from the multifanCL manual.
	dvar_matrix resid=obs-pred;
	nef=value(sum(elem_prod(1.-pred,pred))/sum(elem_prod(resid,resid)));
	//cout<<nef<<endl;
	return sum(0.5*log(2.*pi*xi)-log(mfexp(-0.5*elem_div(elem_prod(resid,resid),xi))+0.01));
}

dvariable multifan(const double& s,const dvector obsQ,const dvar_vector& preQ, double& nmle)
{
	//using Fournier's robust likelihood for length frequency data.
	//s is the sample size
	//neff is the sample size limit  This seems to be fucked...
	//RETURN_ARRAYS_INCREMENT();
	double pi=3.141593;
	dvariable like;
	dvariable tau;
	int lb=obsQ.indexmin();
	int nb=obsQ.indexmax();

	dvar_vector epsilon(lb,nb);
	dvar_vector Q=obsQ/sum(obsQ);
	dvar_vector Qhat=preQ/sum(preQ);

	//dvariable nmle;		//effective sample size
	nmle=value(sum(elem_prod(Qhat,1.-Qhat))/norm2(Q-Qhat));
	//cout<<nmle<<endl;
	tau=1./s;
	epsilon=elem_prod(1.-Qhat,Qhat);

	like=0.5*sum(log(2.*pi*(epsilon+0.1/nb)))+nb*log(sqrt(tau));
	like+= -1.*sum(log(mfexp(-1.*elem_div(square(Q-Qhat),2.*tau*(epsilon+0.1/nb)))+0.01));
	//RETURN_ARRAYS_DECREMENT();
	return like;
}

dvar_matrix ALK(dvar_vector mu, dvar_vector sig, dvector x)
{
	//RETURN_ARRAYS_INCREMENT();
	int i, j;
	dvariable z1;
	dvariable z2;
	int si,ni; si=mu.indexmin(); ni=mu.indexmax();
	int sj,nj; sj=x.indexmin(); nj=x.indexmax();
	dvar_matrix pdf(si,ni,sj,nj);
	pdf.initialize();
	double xs=0.5*(x[sj+1]-x[sj]);
	for(i=si;i<=ni;i++) //loop over ages
	{
		 for(j=sj;j<=nj;j++) //loop over length bins
		{
			z1=((x(j)-xs)-mu(i))/sig(i);
			z2=((x(j)+xs)-mu(i))/sig(i);
			pdf(i,j)=cumd_norm(z2)-cumd_norm(z1);
		}//end nbins
		//pdf(i)/=sum(pdf(i));
	}//end nage
	pdf/=sum(pdf);
	//RETURN_ARRAYS_DECREMENT();
	return(pdf);
}

dvector pearson_residuals(long m, dvector obs_p, dvector pred_p)
{
	{
		dvector O=obs_p/sum(obs_p);
		dvector P=pred_p/sum(pred_p);

		//double neff;		//effective sample size
		//neff=norm(elem_prod(pred_p,1.-pred_p))/norm2(obs_p-pred_p);
		dvector var=elem_prod(P,(1.-P))/m;
		max(var)<=0 ? var=1.: var=var;
		dvector r=elem_div(O-P,sqrt(var+0.01/14));
		if(sum(P)==0) r=0;
		return(r);
	}


}


dvector colMeans(const dmatrix& x)
{
	/* Returns the mean of columns
		while excluding 0's from the data
	*/
	int ir,nr,ic,nc,i,j;
	ir=x.rowmin();	nr=x.rowmax();
	ic=x.colmin(); 	nc=x.colmax();
	ivector n(ic,nc); n.initialize();
	for(j=ic; j<=nc; j++)
		for(i=ir; i<=nr; i++)
			n(j)++;
			//x(i,j)>0 ? n(j)++: NULL;

	dvector colmeans = elem_div(colsum(x),n);
	return(colmeans);
}
dvar_vector colMeans(const dvar_matrix& x)
{
	/* Returns the mean of columns
		while excluding 0's from the data
	*/
	int ir,nr,ic,nc,i,j;
	ir=x.rowmin();	nr=x.rowmax();
	ic=x.colmin(); 	nc=x.colmax();
	ivector n(ic,nc); n.initialize();
	for(j=ic; j<=nc; j++)
		for(i=ir; i<=nr; i++)
			 n(j)++;
			//x(i,j)>0 ? n(j)++: NULL;

	dvar_vector colmeans = elem_div(colsum(x),n);
	return(colmeans);
}

dvar_vector rowMeans(const dvar_matrix& x)
{
	return colMeans(trans(x));
}
