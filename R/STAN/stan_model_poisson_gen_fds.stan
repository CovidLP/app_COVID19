

//
// Stan model to evaluated the cases of Covid-19 - Poisson model
// model: generalized static logistics

functions{
  
  // mean of the number of new cases 
  real mu_new_cases(real time, real a, real b, real c, real d, real f){
    //return((c * f * (a-d) * exp(-c*time)) / (b+exp(-c*time))^(f+1));
    return(log(f)+log(a-d)+log(c)-(c*time)-(f+1)*log( b+exp(-c*time) ) );
  }

}


data {
  
  //-----------------------------
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new cases
  real<lower=0> pop;
  real<lower=0,upper=1> perPop;
  int<lower=1> n_sundays; // number of sundays
  int<lower=1> n_mondays; // number of mondays
  int<lower=1> n_others; // n - n_sundays - n_mondays
  int index_sundays[n_sundays]; // index of the sundays in the vector of observations
  int index_mondays[n_mondays]; // index of the mondays in the vector of observations
  int index_others[n_others]; // index of the other days in the vector of observations
  //-----------------------------
}


parameters { 
  real<lower=0> c;
  real<lower=1> f;
  real<lower=-30> b1;

  //real<lower=0, upper=perPop*pop*(f^b)> a;
  real<lower=0, upper=perPop*pop*exp(f*b1)> a;
  //real<lower=0> beta_sunday1;
  //real<lower=0> beta_monday1;
  real<lower=0> beta_sunday;
  real<lower=0> beta_monday;
 
  
}

transformed parameters{
  real<lower=0> b;
  real<lower=0, upper=pop> mu[n];
  //real<lower=0,upper=1> beta_sunday;
  //real<lower=0,upper=1> beta_monday;

  b = exp(b1);
  for(t in index_others){
    //mu[t] = mu_new_cases(t, a, b, c, 0, f);
    mu[t] = exp(mu_new_cases(t, a, b, c, 0, f));
  }
  for(t in index_sundays){
    mu[t] = exp(mu_new_cases(t, a, b, c, 0, f))*beta_sunday;
    //mu[t] = exp(mu_new_cases(t, a, b, c, 0, f)-beta_sunday1);
  }
  for(t in index_mondays){
    mu[t] = exp(mu_new_cases(t, a, b, c, 0, f))*beta_monday;
    //mu[t] = exp(mu_new_cases(t, a, b, c, 0, f)-beta_monday1);
  }
  //beta_sunday = exp(-beta_sunday1);   
  //beta_monday = exp(-beta_monday1);
}


model {
  //----------------------------
  // likelihood function
    y ~ poisson(mu); // observed model
  //----------------------
   // prior distributions
   a ~ gamma(0.1, 0.1);
   //b ~ gamma(0.1, 0.1);  // sqrt(1/0.1)
   b1 ~ normal(0, sqrt(20));
   c ~ gamma(2,9);
   f ~ gamma(0.01,0.01);   // shape, scale 
   beta_sunday ~ gamma(2, 1);
   beta_monday ~ gamma(2, 1); 
   //beta_sunday1 ~ exponential(2);
   //beta_monday1 ~ exponential(2); 

}
