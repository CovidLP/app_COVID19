

//
// Stan model to evaluated the cases of Covid-19 - Poisson model
// model: generalized static logistics


data {
  
  //-----------------------------
  // observed data
  int<lower=1> n; // number of observations
  int y[n]; // counts of new cases
  int<lower=1> L;     // number of predictions
  
  //-----------------------------
 
}


parameters {
  
  real<lower=0> a;  
  real<lower=0> b; 
  real<lower=0> c;
  real<lower=0> f;
  //real f1;
  
}

transformed parameters{
  
  //real f;
  real mu[n];
  
  //f=exp(f1);
  
  for(t in 1:n){
    mu[t] = f*a*c*exp(-c*t)/ (b+exp(-c*t))^(f+1);
  }
  
}


model {
  //----------------------------
  // likelihood function
  for(t in 1:n){
    y[t] ~ poisson(mu[t]); // observed model
  }
   //----------------------
   // prior distributions
   a ~ gamma(0.1, 0.1);
   b ~ gamma(0.1, 0.1);
   c ~ beta(2,6);
   f ~ gamma(0.01,0.01);   // shape, scale 
  //f1 ~ normal(0, sqrt(10));  // sqrt(1/0.1)

}

generated quantities{
  real mufut[L]; // media of predictions
  int yfut[L]; // predictions of new cases
  
  for(j in 1:L){
    mufut[j]=f*a*c*exp(-c*(n+j))/ (b+exp(-c*(n+j)))^(f+1);
    yfut[j] = poisson_rng(mufut[j]);
  }
  
}
