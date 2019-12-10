function t = sampleAnalysisFunction( X )
%SAMPLEANALYSISFUNCTION Takes an EasyXT handle and does all the processing
%   In this example, we add spots and export the mean intensity in the
%   first channel. 
%   Note that your custom analysis method MUST have an EasyXT handle.
%   Your analysis function must return a table element. 
%   https://ch.mathworks.com/help/matlab/ref/table.html


% Detect some spots
spots = X.DetectSpots(1, 'Name', "My Batch Spots", ...
    'Diameter XY', 3, ...
    'Diameter Z', 3, ...
    'Subtract BG', true, ...
    'Spots Filter', '"Quality" above automatic threshold');

% Add them to the scene
X.AddToScene(spots);

% Get statistics on the mean intensity
stats = X.GetSelectedStatistics(spots, 'Intensity Mean', 'Channel', 1);

% Create the results table
columns = {'IDS', 'C1MeanIntensity'};

% This table 
t = table(stats.ids, stats.values, 'VariableNames', columns );

end

