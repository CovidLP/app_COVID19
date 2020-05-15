

//
// Stan model to evaluated the cases of Covid-19 - Poisson model
// model: generalized static logistics


data {
  
  //-----------------------------
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new cases
  int<lower=1> L;     // number of predictions
  real pop;
  //-----------------------------
}


parameters { 
  real<lower=0> a;  

  //real<lower=0> b; 
  real<lower=-30> b1;

  real<lower=0, upper=1> c;

  real<lower=0> f;
  //real f1;
  
}

transformed parameters{
  
  //real f;
  real<lower=0> b;
  real<lower=0, upper=pop> mu[n];
  
  //f=exp(f1);
  b = exp(b1);
  
  for(t in 1:n){
    mu[t] = exp(log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) ) );
    //mu[t] = f*a*c*exp(-c*t)/ (b+exp(-c*t))^(f+1);
  }
  
}


model {
  //----------------------------
  // likelihood function
    y ~ poisson(mu); // observed model
  //----------------------
   // prior distributions
   a ~ gamma(0.1, 0.1);
   //b ~ gamma(0.1, 0.1);
   c ~ beta(2,6);
   f ~ gamma(0.01,0.01);   // shape, scale 
  //f1 ~ normal(0, sqrt(10));  // sqrt(1/0.1)
  b1 ~ normal(0, sqrt(20));  // sqrt(1/0.1)
}
