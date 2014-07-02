% A simple script example

XT = EasyXT();
   
% Get the number of spots object in the surpass scene
nSpots = XT.GetNumberOf('Spots');
% Loop to get the name of each spots object
for i=1:nSpots
    spot = XT.GetObject('Type', 'Spots', 'Number', i);
    disp(['Spot Object ' num2str(i) ' has name: ' XT.GetName(spot) ]);
end

% Detecting some spots
channel = 1;
newSpots = XT.DetectSpots(channel, 'Name', 'My Spots', ...
                                  'Diameter XY', 2, ...
                                  'Spots Filter', '"Quality" above 10.0' ...
                         );
% Changing the color
XT.SetColor(newSpots, [200 255 0]);
    
% Adding them to a new folder
folder = XT.CreateGroup('My Group');

% Displaying the folder
XT.AddToScene(folder);

% Displaying the spots
XT.AddToScene(newSpots, 'Parent', folder);

                   