data {
  int<lower=1> nSubject;
  int<lower=1> maxTrialN;
  int nTrial[nSubject];
  int<lower=1> nSession [nSubject];
  int Infusion_f [nSubject,maxTrialN]; 
  int Infusion [nSubject,maxTrialN]; 
  int Trial_Feedback [nSubject,maxTrialN]; 
  int Contingency_Feedback [nSubject,maxTrialN]; 
  int ExpRat [nSubject,maxTrialN]; 
  //int MoodRat [nSubject,2,maxTrialN]; 
  //int<lower=1,upper=4> Condition [nSubject,2,maxTrialN]; 
  int missing_choice_exp [nSubject,maxTrialN];
  //int missing_choice_mood [nSubject,2,maxTrialN];
  //int num_learning_rate; //add arguments here to determine how many learning rate;
}

parameters { 
   //Learning Rate(s)
  real<lower=0,upper=1> alpha_m;
  real beta_m;
  real kappa_m;

  real<lower=0> alpha_group_s;
  real<lower=0> beta_group_s;
  real<lower=0> kappa_group_s;
 
  
  vector[nSubject] alpha_subject_raw;
  vector[nSubject] beta_subject_raw;
  vector[nSubject] kappa_subject_raw;

  
} 

transformed parameters {
  vector[nSubject] alpha;
  vector[nSubject] beta;
  vector[nSubject] kappa;
  vector[nSubject] alpha_norm;
  vector[nSubject] beta_norm;


  alpha_norm = alpha_m + (alpha_group_s * alpha_subject_raw);
  beta_norm = beta_m + (beta_group_s * beta_subject_raw);
  kappa = kappa_m + (kappa_group_s * kappa_subject_raw);

  //Transform Para
  alpha=inv_logit(alpha_norm);
  beta=exp(beta_norm);
  

}

model {
  int choice;
  real delta[nSubject,maxTrialN];
  real Q[nSubject,maxTrialN+1,4];
  
  alpha_m ~ normal(0,2.5);
  beta_m ~ normal(0,5);
  kappa_m ~ normal(0,5);
  
  alpha_group_s ~ cauchy(0,1);
  beta_group_s ~ cauchy(0,1);
  kappa_group_s ~ cauchy(0,1);


  for (s in 1:nSubject) {
    //prev_choice=0;
      if(nTrial[s]!=0){
        for(ne in 1:4){
          Q[s,1,ne] = 0.25;
        }
        for (t in 1:nTrial[s]){
          if(missing_choice_exp[s,t]==0){
            
            choice = ExpRat[s,t]==1;
            choice ~ bernoulli_logit(  (Q[s,t,Infusion_f[s,t]]+ kappa[s])  * beta[s] );
            //prev_choice = choice? 1:-1 ; //1 if choice NO, -1 if choice YED
            
            //Decay everything;
            for (xe in 1:4) {
              Q[s,t+1,xe] = Q[s,t,xe] * 0.99;
            }
            //Except for the one getting udpated, Update the value
            delta[s,t] = (Trial_Feedback[s,t]) - Q[s,t,Infusion_f[s,t]];
            Q[s,t+1,Infusion_f[s,t]] = Q[s,t,Infusion_f[s,t]] + (alpha[s]*delta[s,t]);
           
            

          } else {
            //decay all options
            for (nj in 1:4) {
                Q[s,t+1,nj] = Q[s,t,nj] * 0.99;
            }
            //prev_choice=0;
          }//End of else
                  
        }//End of Trial Loop
      }//End of no data logical statement
  } //End Subject Loop
  
  
}

generated quantities { 
  //int prev_choice;
  real log_lik[nSubject,maxTrialN];
  // int Invert_Infus;
  // int Invert_ProbReinf;
  // int which_Infus;
  // int which_PorbReinf;
  int choice;
  real delta[nSubject,maxTrialN];
  real Q[nSubject,maxTrialN+1,4];

  
  for (s in 1:nSubject) {
      if(nTrial[s]!=0){
        for(ne in 1:4){
            Q[s,1,ne] = 0.25;
        }
        //prev_choice=0;
        for (t in 1:maxTrialN){
          
          if(missing_choice_exp[s,t]==0){
          
            //choice yes or no
            choice = ExpRat[s,t]==1;
            log_lik[s,t] = bernoulli_logit_lpmf(choice | (Q[s,t,Infusion_f[s,t]]+ kappa[s])  * beta[s]  );
            //prev_choice = choice? 1:-1 ; //1 if choice NO, -1 if choice YED
            
            //Decay everything;
            for (xe in 1:4) {
            Q[s,t+1,xe] = Q[s,t,xe] * 0.99;
            }
            //Except for the one getting udpated, Update the value
            delta[s,t] = (Trial_Feedback[s,t]) - Q[s,t,Infusion_f[s,t]];
            Q[s,t+1,Infusion_f[s,t]] = Q[s,t,Infusion_f[s,t]] + (alpha[s]*delta[s,t]);
           

          } else {
            //decay all options
            if(t<=nTrial[s]){
              for (nj in 1:4) {
                Q[s,t+1,nj] = Q[s,t,nj] * 0.99;
              }
            }else {
              for (nj in 1:4) {
                Q[s,t+1,nj] = 0;
              }
            }
            log_lik[s,t]=0;
           // prev_choice=0;
            delta[s,t]=0;
          }//End of else
                  
        }//End of Trial Loop
      }//End of no data logical statement

  } //End Subject Loop
  
  
  
}

