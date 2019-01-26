data {
  int<lower=1> nSubject;
  int nTrial[nSubject,2];
  int<lower=1> nSession [nSubject];
  int<lower=1> maxTrialN;
  int<lower=0,upper=1> SessionIndex[nSubject,2]; 
  int Infusion [nSubject,2,maxTrialN]; 
  int Trial_Feedback [nSubject,2,maxTrialN]; 
  int Contingency_Feedback [nSubject,2,maxTrialN]; 
  int ExpRat [nSubject,2,maxTrialN]; 
  //int MoodRat [nSubject,2,maxTrialN]; 
  //int<lower=1,upper=4> Condition [nSubject,2,maxTrialN]; 
  int missing_choice_exp [nSubject,2,maxTrialN];
  //int missing_choice_mood [nSubject,2,maxTrialN];
  //int num_learning_rate; //add arguments here to determine how many learning rate;
}

parameters { 
  //Learning Rate(s)
  real<lower=0,upper=1> alpha_m; //this is also the alpha learning rate if it's only one 
  //Inverse Temperature
  real beta_m;
  //Reward Sensitivity
  //real tau_m;
  //Perseverance
  real pers_m;
  //Decay factor
  //real omega_m;
 
  //real alpha_Visit;
  //real beta_Visit;
  //real omega_Visit;
  //real tau_Visit;
  //real pers_Visit;
  
  real<lower=0>alpha_group_s;
  real<lower=0>beta_group_s;
  //real<lower=0> tau_group_s;
  real<lower=0> pers_group_s;
  //real<lower=0> omega_group_s;
  
 
  
  vector[nSubject] alpha_subject_raw;
  vector[nSubject] beta_subject_raw;
  //vector[nSubject] tau_subject_raw;
  //vector[nSubject] omega_subject_raw;
  vector[nSubject] pers_subject_raw;
  //vector[nSubject] per_raw;
  

  
} 

transformed parameters {
  
  vector<lower=0,upper=1>[nSubject] alpha;
  vector<lower=0>[nSubject] beta;
  vector[nSubject] pers;
  
  vector[nSubject] alpha_norm;
  vector[nSubject] beta_norm;
  
  alpha_norm = alpha_m + (alpha_group_s * alpha_subject_raw);
  beta_norm = beta_m + (beta_group_s * beta_subject_raw);
  pers = pers_m + (pers_group_s * pers_subject_raw);

  //Transform Para
  alpha=inv_logit(alpha_norm);
  beta=exp(beta_norm);
  
  
}

model {
  int prev_choice;
  int Invert_Infus;
  int which_Infus;
  int choice;
  real delta[nSubject,2,maxTrialN];
  real Q[nSubject,2,maxTrialN+1];
  
  alpha_m ~ normal(0,2.5);
  beta_m ~ normal(0,5);
  //tau_m ~ normal(0,5);
  pers_m ~ normal(0,5);
  
  alpha_group_s ~ cauchy(0,1);
  beta_group_s ~ cauchy(0,1);
  //tau_group_s ~ cauchy(0,1);
  pers_group_s ~ cauchy(0,1);
  


  for (s in 1:nSubject) {
    prev_choice=0;
    for (se in 1:nSession[s]){
      if(nTrial[s,se]!=0){
            Q[s,se,1] = 0;
        for (t in 1:nTrial[s,se]){
          if(missing_choice_exp[s,se,t]==0){
            
            //which_Infus=Infusion[s,se,t]==1 ? 1:2;
            //Invert_Infus = Infusion[s,se,t]==1 ? 2:1;
            //choice yes or no
            choice=ExpRat[s,se,t]==1;
            
            choice ~ bernoulli_logit( (Q[s,se,t] * beta[s]) + (pers[s]*prev_choice) );
            
            prev_choice = choice? 1:-1 ; //1 if choice NO, -1 if choice YED
            
            //Update the value
            if (Infusion[s,se,t] == 1){
              delta[s,se,t] = (Trial_Feedback[s,se,t]) - Q[s,se,t];
              Q[s,se,t+1] = Q[s,se,t] + (alpha[s]*delta[s,se,t]);
            } else {
              delta[s,se,t] = (Trial_Feedback[s,se,t]) + Q[s,se,t];
              Q[s,se,t+1] = Q[s,se,t] - (alpha[s]*delta[s,se,t]);
            }
            
           
            //Decay a little;
            Q[s,se,t+1] = Q[s,se,t+1] * 0.99;
            

          } else {
            //decay all options
            Q[s,se,t+1] = Q[s,se,t] * 0.99;
            prev_choice=0;
          }//End of else
                  
        }//End of Trial Loop
      }//End of no data logical statement
    }//End Session Loop
  } //End Subject Loop
  
  
}

generated quantities { 
  int prev_choice;
  real log_lik[nSubject,2,maxTrialN];
  int Invert_Infus;
  int Invert_ProbReinf;
  int which_Infus;
  int which_PorbReinf;
  int choice;
  real delta[nSubject,2,maxTrialN];
  real Q[nSubject,2,maxTrialN+1];

  
  for (s in 1:nSubject) {
    for (se in 1:nSession[s]){
      if(nTrial[s,se]!=0){
        Q[s,se,1] = 0;
        prev_choice=0;
        for (t in 1:maxTrialN){
          
          if(missing_choice_exp[s,se,t]==0){
          
            //choice yes or no
            choice = ExpRat[s,se,t]==1;
            log_lik[s,se,t] = bernoulli_logit_lpmf(choice | ( (Q[s,se,t] * beta[s]) + (pers[s]*prev_choice) ) );
            prev_choice = choice? 1:-1 ; //1 if choice NO, -1 if choice YED
            
            //Update the value
            if (Infusion[s,se,t] == 1){
              delta[s,se,t] = (Trial_Feedback[s,se,t]) - Q[s,se,t];
              Q[s,se,t+1] = Q[s,se,t] + (alpha[s]*delta[s,se,t]);
            } else {
              delta[s,se,t] = (Trial_Feedback[s,se,t]) + Q[s,se,t];
              Q[s,se,t+1] = Q[s,se,t] - (alpha[s]*delta[s,se,t]);
            }
            
           
            //Decay a little;
            Q[s,se,t+1] = Q[s,se,t+1] * 0.99;
            
          } else {
            //decay all options
            if(t<=nTrial[s,se]){
            Q[s,se,t+1] = Q[s,se,t] * 0.99;
            }else {
            Q[s,se,t+1] = 0;
            }
            log_lik[s,se,t]=0;
            prev_choice=0;
            delta[s,se,t]=0;
          }//End of else
                  
        }//End of Trial Loop
      }//End of no data logical statement
    }//End Session Loop
  } //End Subject Loop
  
  
  
}

