% PRs to Z-scores converter (import using .txt files only) Created by David Garnica
% david.garnica@med.uni-goettingen.de
% 2022, Universitätsmedizin Göttingen, Neurology Department
clear all;

% Import PRs data first
filename = 'filepath/PRs.txt';
delimiter = '\t';
formatSpec = '%f%f%f%f%f%f%f%f%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'EmptyValue', NaN,  'ReturnOnError', false);
fclose(fileID);
PRs = [dataArray{1:end-1}];
clearvars filename delimiter formatSpec fileID dataArray ans;

%% CONVERSION PR to Z-score
z = []; t = [];

pd = makedist('Normal','mu',0,'sigma',1); % info of normal distribution
x = random(pd,10000000,1);                % 10 million random numbers to fit the normal distribution

for i = 1:size(PRs,1)
    for k = 1:size(PRs,2)
        pr = PRs(i,k);
        z_temp = prctile(x, pr);          % takes care of the z-score estimation
        if z_temp <= -3.1                 % checks for outliers - Iverson (2011) estipulates max/min z-score of +/- 3.10 
            z(i,k) = -3.10;
        elseif z_temp >= 3.1
            z(i,k) = 3.10;
        else
            z(i,k) = z_temp;
        end
    end
end

% disp(['The percentage rank: ',num2str(pr), ' equals a z-score :', num2str(z)]);
disp('Done with PR to Z-scores conversion. Approximate reliability of 0.9 and Confidence interval of 95%');
disp('Based on manual conversion available at https://www.psychometrica.de/normwertrechner.html');

%% CONVERSION Z-score to T-score
% Ask if T-scores are necessary, to calculate them
fprintf('Should T-scores be calculated too? \n');
prompt = {'Answer (1 Yes, 2 No)'};
dlgtitle = 'Calculate T-scores?';
dims = [1 55];
answer = inputdlg(prompt, dlgtitle, dims);
[t_required] = deal(answer{:});
t_required = str2num(t_required);

if t_required == 1
    for i = 1:size(z,1)
        for k = 1:size(z,2)
            z_used = z(i,k);
            t(i,k) = (z_used * 10) + 50;    % z-score to t-score conversion formula
        end
    end
    disp('Done with PR to T-scores conversion. Approximate reliability of 0.9 and Confidence interval of 95%');
    disp('Based on manual conversion available at https://www.psychometrica.de/normwertrechner.html');
    
elseif t_required == 2
    disp('T-scores not required. Thank you for using DG´s converter');
else
    error('Wrong answer, please repeat if T-scores are necessary');
end
