



% 1 - Download&Unzip (or Clone) EasyXt : https://github.com/BIOP/EasyXT
% 2 - Open Imaris 
% 3 - Open the dataset > Bitplane > Imaris ... > images > celldemo.ims
% 4 - Set IMARIS.exe location , below(need to be done once)
% 5 - Specify the path
% 6 - RUN & play easily with Imaris


% % 
% 4- Set IMARIS.exe location
% 
% First time you need to run the line below (remove %)
% XT = EasyXT('setup')

% % 
% 5- Specify the path to the fodler that contains EasyXT 
% 
EasyXTPath = "\\svfas6.epfl.ch\biop\public\0 - BIOP Data\Scripts IMARIS\EasyXT\";
addpath( EasyXTPath );

%  initialize EasyXT
eXT = EasyXT;
dataset = eXT.ImarisApp.GetDataSet;
eXT.SetSize('C', 3)
  
eXT.SetDataType('32-bit');

eXT.ResetScene()  

ves_spots = eXT.DetectSpots(3, 'Name', 'Vesicles', ...
                               'Color', [255,0,255,128], ...
                              'Diameter XY', 0.75 , ...
                              'Subtract BG', true, ...
                              'Spots Filter', '"Quality" above 4.00') ;
eXT.AddToScene(ves_spots)

% 
% Detect Cell                     
cyto_surface = eXT.DetectSurfaces(1,'Name', 'Cell', ...
                                    'Color', [0,255,0,64]);                                              
eXT.AddToScene( cyto_surface )

% Detect Nuclei 
nuc_surface = eXT.DetectSurfaces(2,...
                                 'Name', 'Nucleus', ...
                                 'Color', [0,0,255,128]);                                              
eXT.AddToScene( nuc_surface ) 

cytoPlasm_dMAP_ch = eXT.DistanceTransform(  cyto_surface,...
                                            'Direction','Inside');

% %                    
dMAP_channel = eXT.ImarisApp.GetDataSet.GetDataVolumeAs1DArrayFloats(cytoPlasm_dMAP_ch-1,0);
dMAP_max = max(dMAP_channel)            
name = eXT.GetChannelName( cytoPlasm_dMAP_ch )

op_expression = strcat ( 'ch' ,num2str( cytoPlasm_dMAP_ch ), './', num2str(dMAP_max) )
eXT.ChannelMath( op_expression );

