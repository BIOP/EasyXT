
% The EasyXT RunBatch function needs a directory and a handle to the
% function that will do the processing and create the results table.
% see the sampleAnalysisFunction in this folder. 

% Start EasyXT
X = EasyXT();

% Ask the user to provide the location of the Imaris files to batch
dir = uigetdir('Location of .ims files to batch');

% if set to true, it will create a single csv file, otherwise once csv file
% per image opened will be created
append_data = true;


% This runs the batch, notice the @, which means we are passing the
% function as an argument, so that the batch processor can run it when it
% needs to.
X.RunBatch(dir, @sampleAnalysisFunction, append_data);