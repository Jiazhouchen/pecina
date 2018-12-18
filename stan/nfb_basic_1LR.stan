data {
  int<lower=1> nSubject;
  int<lower=1> nTrial[nSubject,2];
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
  real tau_m;
  //Perseverance
  //real per_m;
  //Decay factor
  real omega_m;
 
  real alpha_Visit;
  real beta_Visit;
  real omega_Visit;
  real tau_Visit;
  
  real<lower=0>alpha_group_s;
  real<lower=0>beta_group_s;
  real<lower=0> tau_group_s;
  //real<lower=0> per_s;
  real<lower=0> omega_group_s;
  
  //Raw Parameter Estimates
  vector[nSubject] alpha_subj_s;
  vector[nSubject] beta_subj_s;
  vector[nSubject] tau_subj_s;
  vector[nSubject] omega_subj_s;
  
  vector[nSubject] alpha_subject_raw;
  vector[nSubject] beta_subject_raw;
  vector[nSubject] tau_subject_raw;
  vector[nSubject] omega_subject_raw;
  //vector[nSubject] per_raw;
  
  vector[2] alpha_session_raw[nSubject];
  vector[2] beta_session_raw[nSubject];
  vector[2] tau_session_raw[nSubject];
  vector[2] omega_session_raw[nSubject];

  
} 

transformed parameters {
  vector[2] alpha[nSubject];
  vector[2] beta[nSubject];
  vector[2] tau[nSubject];
  vector[2] omega[nSubject];
  
  vector[2] alpha_norm[nSubject];
  vector[2] beta_norm[nSubject];
  vector[2] tau_norm[nSubject];
  vector[2] omega_norm[nSubject];
  
  vector[nSubject] alpha_subj_m;
  vector[nSubject] beta_subj_m;
  vector[nSubject] tau_subj_m;
  vector[nSubject] omega_subj_m;
  
  //Group level
  alpha_subj_m=alpha_m + (alpha_group_s * alpha_subject_raw);
  beta_subj_m=beta_m + (beta_group_s * beta_subject_raw);
  tau_subj_m=tau_m + (tau_group_s * tau_subject_raw);
  omega_subj_m=omega_m + (omega_group_s * omega_subject_raw);
  //per = per_m + (per_s * per_raw)
  
  //Session level
  for(S in 1: nSubject){
    for (Se in 1:nSession[S]){ 
      alpha_norm[S,Se]=alpha_subj_m[S] + (alpha_subj_s[S] * alpha_session_raw[S,Se]) + (alpha_Visit * SessionIndex[S,Se]);
      beta_norm[S,Se]=beta_subj_m[S] + (beta_subj_s[S] * beta_session_raw[S,Se]) + (beta_Visit * SessionIndex[S,Se]);
      tau_norm[S,Se]=tau_subj_m[S] + (tau_subj_s[S] * tau_session_raw[S,Se]) + (tau_Visit * SessionIndex[S,Se]);
      omega_norm[S,Se]=omega_subj_m[S] + (omega_subj_s[S] * omega_session_raw[S,Se]) + (omega_Visit * SessionIndex[S,Se]);
    }
  }
  
  //Transform Para
  alpha=inv_logit(alpha_norm);
  beta=beta_norm;
  tau=tau_norm;
  omega=omega_norm;

}

model {
  int Invert_Infus;
  int Invert_ProbReinf;
  int which_Infus;
  int which_PorbReinf;
  int choice;
  real delta[nSubject,2,maxTrialN,2,2];
  real Q[nSubject,2,maxTrialN+1,2,2];
  real Exp_Q[nSubject,2,maxTrialN,2,2];
  
  alpha_m ~ normal(0,2.5);
  beta_m ~ normal(0,5);
  tau_m ~ normal(0,5);
  omega_m ~ normal(0,5);
  //pers_m ~ normal(0,5);
  
  alpha_group_s ~ cauchy(0,1);
  beta_group_s ~ cauchy(0,1);
  tau_group_s ~ cauchy(0,1);
  omega_group_s ~ cauchy(0,1);
  //pers_s ~ cauchy(0,1);
  
  alpha_subj_s ~ cauchy(0,1);
  beta_subj_s ~ cauchy(0,1);
  tau_subj_s ~ cauchy(0,1);
  omega_subj_s ~ cauchy(0,1);
  
  alpha_subject_raw ~ normal(0,1);
  beta_subject_raw ~ normal(0,1);
  tau_subject_raw ~ normal(0,1);
  omega_subject_raw ~ normal(0,1);
  //per_raw ~ normal(0,1);

 
  
  for (s in 1:nSubject) {
  //Set initial value
    alpha_session_raw[s] ~ normal(0,1);
    beta_session_raw[s] ~ normal(0,1);
    tau_session_raw[s] ~ normal(0,1);
    omega_session_raw[s] ~ normal(0,1);
    for (se in 1:nSession[s]){
      if(nTrial[s,se]!=0){
        for(ne in 1:2){
          for (nd in 1:2){
            Q[s,se,1,ne,nd] = 0.25;
          }
        }
        for (t in 1:nTrial[s,se]){
        
          if(missing_choice_exp[s,se,t]==0){
           
            which_Infus=Infusion[s,se,t]==1 ? 1:2;
            which_PorbReinf=Contingency_Feedback[s,se,t]==1 ? 1:2;
            Invert_Infus = Infusion[s,se,t]==1 ? 2:1;
            Invert_ProbReinf = Contingency_Feedback[s,se,t]==1 ? 2:1;
            
            //Get the exp value;
            Exp_Q[s,se,t,which_Infus,which_PorbReinf]=Q[s,se,t,which_Infus,which_PorbReinf];
            
            //choice yes or no
            choice=ExpRat[s,se,t]==1;
            choice ~ bernoulli_logit( Exp_Q[s,se,t,which_Infus,which_PorbReinf] * beta[s,se] );
            
            //Update the value
            delta[s,se,t,which_Infus,which_PorbReinf] = (tau[s,se]*Trial_Feedback[s,se,t]) - Q[s,se,t,which_Infus,which_PorbReinf];
            Q[s,se,t+1,which_Infus,which_PorbReinf] = Q[s,se,t,which_Infus,which_PorbReinf] + (alpha[s,se]*delta[s,se,t,which_Infus,which_PorbReinf]);
            //Decay all other;
            Q[s,se,t+1,which_Infus,Invert_ProbReinf] = Q[s,se,t,which_Infus,Invert_ProbReinf] * ((1-alpha[s,se])*(omega[s,se]));
            for (nx in 1:2) {
              Q[s,se,t+1,Invert_Infus,nx] = Q[s,se,t,Invert_Infus,nx] * ((1-alpha[s,se])*(omega[s,se]));
            }

          } else {
            //decay all options
            for (nj in 1:2) {
              for (ny in 1:2){
                Q[s,se,t+1,nj,ny] = Q[s,se,t,nj,ny] * ((1-alpha[s,se])*(omega[s,se]));
              }
            }
            
          }//End of else
                  
        }//End of Trial Loop
      }//End of no data logical statement
    }//End Session Loop
  } //End Subject Loop
  
  
}

generated quantities { 
  real log_lik[nSubject,2,maxTrialN];
  int Invert_Infus;
  int Invert_ProbReinf;
  int which_Infus;
  int which_PorbReinf;
  int choice;
  real delta[nSubject,2,maxTrialN,2,2];
  real Q[nSubject,2,maxTrialN+1,2,2];
  real Exp_Q[nSubject,2,maxTrialN,2,2];
  
  for (s in 1:nSubject) {
    for (se in 1:nSession[s]){
      if(nTrial[s,se]!=0){
        for(ne in 1:2){
          for (nd in 1:2){
            Q[s,se,1,ne,nd] = 0.25;
          }
        }
        for (t in 1:nTrial[s,se]){
        
          if(missing_choice_exp[s,se,t]==0){
           
            which_Infus=Infusion[s,se,t]==1 ? 1:2;
            which_PorbReinf=Contingency_Feedback[s,se,t]==1 ? 1:2;
            Invert_Infus = Infusion[s,se,t]==1 ? 2:1;
            Invert_ProbReinf = Contingency_Feedback[s,se,t]==1 ? 2:1;
            
            //Get the exp value;
            Exp_Q[s,se,t,which_Infus,which_PorbReinf]=Q[s,se,t,which_Infus,which_PorbReinf];
            
            //choice yes or no
            choice=ExpRat[s,se,t]==1;
            log_lik[s,se,t] = bernoulli_logit_lpmf(choice | Exp_Q[s,se,t,which_Infus,which_PorbReinf] * beta[s,se] );
            
            //Update the value
            delta[s,se,t,which_Infus,which_PorbReinf] = (tau[s,se]*Trial_Feedback[s,se,t]) - Q[s,se,t,which_Infus,which_PorbReinf];
            Q[s,se,t+1,which_Infus,which_PorbReinf] = Q[s,se,t,which_Infus,which_PorbReinf] + (alpha[s,se]*delta[s,se,t,which_Infus,which_PorbReinf]);
            //Decay all other;
            Q[s,se,t+1,which_Infus,Invert_ProbReinf] = Q[s,se,t,which_Infus,Invert_ProbReinf] * ((1-alpha[s,se])*(omega[s,se]));
            for (nx in 1:2) {
              Q[s,se,t+1,Invert_Infus,nx] = Q[s,se,t,Invert_Infus,nx] * ((1-alpha[s,se])*(omega[s,se]));
            }

          } else {
            //decay all options
            for (nj in 1:2) {
              for (ny in 1:2){
                Q[s,se,t+1,nj,ny] = Q[s,se,t,nj,ny] * ((1-alpha[s,se])*(omega[s,se]));
              }
            }
            
          }//End of else
                  
        }//End of Trial Loop
      }//End of no data logical statement
    }//End Session Loop
  } //End Subject Loop
  
  
  
}

