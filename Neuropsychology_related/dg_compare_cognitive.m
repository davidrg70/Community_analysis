% PRs to Z-scores converter (import using .txt files only) Created by David Garnica
% david.garnica@med.uni-goettingen.de
% 2023, Universitätsmedizin Göttingen, Neurology Department

% dg_compare_cognitive

% load z-scores
clear all;
analysis_dir = 'folder';
load(fullfile(analysis_dir, 'Z_scores_patients_controls.mat'));

subtests = {'Data','TIQ','SV','VRV','FS','AGD','VG','WLT','Lesefehler','TA','Lesefehler_TA','Woerter'};

% determine number of patients and controls
for i = 1:length(subjects)
    if contains(subjects(i), 'P.')
        patients_num = i;
    end
end
controls_num = length(subjects) - patients_num;

% separate data into groups
patients = z(1:patients_num,:);
controls = z(patients_num+1:end,:);

% remove NaNs (that is, children that were not tested)
patients(any(isnan(patients), 2), :) = [];
controls(any(isnan(controls), 2), :) = [];

% compare, using Wilcoxon rank sum test (equivalent to the Mann-Whitney U-test)
for i = 1:length(subtests)-1
    [p(i),h(i)] = ranksum(patients(:,i), controls(:,i));
    Mp(i) = median(patients(:,i));
    Mc(i) = median(controls(:,i));
end

% OPTIONAL: apply FDR correction for multiple comparisons
[h, crit_p, ~, adj_p] = fdr_bh(p, 0.05, 'pdep', 'yes');

results = cat(1, adj_p, h, Mc, Mp);
what = ["p(FDR)"; "h"; "Median patients"; "Median controls"];
results = cat(2, what, results);
results = array2table(results, "VariableNames", string(subtests));
