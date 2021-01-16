// Stan model to evaluated the cases of Covid-19 - Poisson model
// model: generalized static logistics


data {
  
  //-----------------------------
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new cases
  int<lower=0> pop;
  real<lower=0,upper=1> perPop;
  //-----------------------------
}


parameters {
  real b1_1;
  real<lower=0, upper=pop*perPop*exp(b1_1)> a1;
  real b1_2;
  real<lower=0, upper=pop*perPop*exp(b1_2)> a2;
  real<lower=0> alpha1;
  real<lower=0> alpha2;
  real delta1;
  real delta2;

  real<lower=0> c1;
  real<lower=0> c2;
}

transformed parameters{
  
  real<lower=0> b1 = exp(b1_1);
  real<lower=0> b2 = exp(b1_2);
  vector<lower=0>[n] mu1;
  vector<lower=0>[n] mu2;
  vector<lower=0>[n] mu;
//  real<lower=0> peak1 = -log(b1/(f1+1))/c1;
//  real<lower=0> peak2 = -log(b2/(f2+1))/c2;

  for(t in 1:n){
    mu1[t] = exp(log(a1)+log(c1)-(c1*t)-2*log( b1+exp(-c1*t) ));
    mu2[t] = exp(log(a2)+log(c2)-(c2*t)-2*log( b2+exp(-c2*t) ));
    //mu[t] = f*a*c*exp(-c*t)/ (b+exp(-c*t))^(f+1);
    mu[t] = normal_cdf(alpha1*(t-delta1),0,1) * mu1[t] + normal_cdf(alpha2*(t-delta2),0,1) * mu2[t];
  }

}

model {
  //----------------------------
  // likelihood function
    y ~ poisson(mu); // observed model
  //----------------------
   // prior distributions
   a1 ~ gamma(0.1, 0.1);
   a2 ~ gamma(0.1, 0.1);
   //b ~ gamma(0.1, 0.1);
   c1 ~ gamma(2,9);
   c2 ~ gamma(2,9);
   alpha1 ~ gamma(0.01,0.01);   // shape, scale
   alpha2 ~ gamma(0.01,0.01);   // shape, scale
   delta1 ~ normal(0,100);
   delta2 ~ normal(0,100);
  //f1 ~ normal(0, sqrt(10));  // sqrt(1/0.1)
  b1_1 ~ normal(0, sqrt(20));  // sqrt(1/0.1)
  b1_2 ~ normal(0, sqrt(20));  // sqrt(1/0.1)
}
