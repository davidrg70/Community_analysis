function [PC, Acc, dprime, bias_ratio] = dg_dprime_2afc(cond_data)

% EFIT has a 2-alternative forced choice (2AFC) design, not a yes/no detection task.
% In a yes/no task we can directly compute hit rate vs false alarm rate.
% But in a 2AFC (N vs X discrimination), there is no “noise-only” trial type —
% we always have a target (either N or X). So we can’t compute FA in the usual way.
% We have to compute sensitivity from proportion correct (PC) in 2AFC

% Get results of 1 run (N conditions)
% total trials and correct responses
total_trials = length(cond_data.hit);
targetHits = sum(cond_data.hit == 1);
% log-linear correction for PC (proportion correct) after Stanislaw & Todorov 1999, pg. 144!
PC = (targetHits + 0.5) / (total_trials + 1);
Acc = (targetHits / total_trials) * 100;

% compute d' for 2AFC
dprime = sqrt(2) * norminv(PC);
% bias index (tendency to answer N vs X)
nN = sum(cond_data.answer == 2);
nX = sum(cond_data.answer == 3);
bias_ratio = nN / max(1,(nN+nX));

end
