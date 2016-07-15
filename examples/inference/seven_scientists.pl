/*
Seven scientists, posterior estimation in Bayesian models.
Suppose seven scientists all go and perform the same experiment, each collecting
a measurement xi for i=1,..,7.
These scientists are varyingly good at their job, and while we can assume each 
scientist would estimate x correctly on average, some of them may have much more
error in their measurements than others.
They come back with the following seven observations:
[-27.020 3.570 8.191 9.898 9.603 9.945 10.056]
To model this situation, we put a prior on the mean and the standard deviation
of the measurements each of the 7 scientists.
For the mean, we use a Gaussian prior with mean 0 and variance 50^2.
For the standard deviation, we use a uniform prior between 0 and 25.
Given the above measurements, what is the posterior distribution of x?
What distribution over noise levels do we infer for each of these scientists' 
estimates?
From
http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=gaussian-posteriors
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

value(I,X) :- 
  std_dev(I,Sigma),
  mean(M),
  measurement(I,M,Sigma,X).
% for scientist I we see X sampled from a Gaussian with mean M and standard deviation
% Sigma

std_dev(_,S): uniform(S,0,25).
% the standard deviation is sampled for all scientists between 0 and 25 
% uniformly

mean(M): gaussian(M,0, 2500).
% the mean is sampled from a Gaussian with mean 0 and variance 50^2

measurement(_,M,Sigma,X): gaussian(X,M,Sigma*Sigma).
% the measurement is sampled from a Gaussian with mean M and variance
% Sigma^2

:- end_lpad.

hist_uncond(Samples,NBins,Chart):-
  mc_sample_arg(value(0,X),Samples,X,L0),
  histogram(L0,NBins,Chart).
% take Samples samples of X for index 0 (X in val(0,X) and draw a
% histogram of the distribution with NBins bins

dens_lw(Samples,NBins,Chart,E):-
  mc_sample_arg(value(0,Y),Samples,Y,L0),
  mc_lw_sample_arg(mean(X),(value(1,-27.020),value(2,3.570),
  value(3,8.191),value(4,9.898),value(5,9.603),value(6,9.945),
  value(7,10.056)),Samples,X,L),
  exp(L,Samples,E),
  densities(L0,L,NBins,Chart).
% take Samples samples of X for index 0 (X in val(0,X)) before and after
% having observed the scientists' measurements and draw curves of the 
% densities using NBins bins

chart_lw_noise(Samples,Chart,E):-
  mc_lw_sample_arg((std_dev(1,Y1),std_dev(2,Y2),std_dev(3,Y3),std_dev(4,Y4),
    std_dev(5,Y5),std_dev(6,Y6),std_dev(7,Y7)),(value(1,-27.020),value(2,3.570),
  value(3,8.191),value(4,9.898),value(5,9.603),value(6,9.945),
  value(7,10.056)),Samples,(Y1,Y2,Y3,Y4,Y5,Y6,Y7),L),
  exp_noise(L,Samples,E),
  E = (E1,E2,E3,E4,E5,E6,E7),
  Chart = c3{data:_{x:x, rows:[x-e,1-E1,2-E2,3-E3,4-E4,5-E5,6-E6,7-E7],
                    type: bar}}.
% take Samples samples of the standard deviation of the measurements of the
% scientists (Y1,...,Y7 in std_dev(1,Y1),...,std_dev(7,Y7))
% given the 7 observations and draw a bar chart of the mean of the samples
% for each scientist


exp_noise(L,S,(E1,E2,E3,E4,E5,E6,E7)):-
  foldl(agg_noise,L,(0,0,0,0,0,0,0),(S1,S2,S3,S4,S5,S6,S7)),
  E1 is S1/S,
  E2 is S2/S,
  E3 is S3/S,
  E4 is S4/S,
  E5 is S5/S,
  E6 is S6/S,
  E7 is S7/S.

agg_noise((V1,V2,V3,V4,V5,V6,V7)-W,(S1,S2,S3,S4,S5,S6,S7),
  (S1+V1*W,S2+V2*W,S3+V3*W,S4+V4*W,S5+V5*W,S6+V6*W,S7+V7*W)).

exp(L,S,E):-
  foldl(agg,L,0,Sum),
  E is Sum/S.

agg(V-W,S,S+V*W).

/** <examples>
?- dens_lw(1000,40,G,E).
% take Samples samples of X for index 0 (X in val(0,X)) before and after
% having observed the scientists' measurements and draw curves of the 
% densities using NBins bins

?- chart_lw_noise(1000,Chart,E).
% take Samples samples of the standard deviation of the measurements of the
% scientists (Y1,...,Y7 in std_dev(1,Y1),...,std_dev(7,Y7))
% given the 7 observations and draw a bar chart of the mean of the samples
% for each scientist

?- hist_uncond(1000,40,Chart).
% take 1000 samples of X for index 0 (X in val(0,X) and draw an
% histogram of the distribution with NBins bins


*/
 
