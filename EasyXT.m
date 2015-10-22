classdef EasyXT < handle
    %%EASYXT Simplified commands to access Imaris Interface
    %   We're wrapping up some common funtions of Imaris so as to simplify
    %   the creation of XTensions. Access to most functions is simplified
    %   using 'PropertyName', 'PropertyValue' pairs.
    %
    %   It is of course a work in progress and will greatly benefit from
    %   any feedback from the community of users.
    %   
    % Olivier Burri & Romain Guiet, EPFL BioImaging & Optics Platform
    % March 2014
    % olivier dot burri at epfl.ch
    % romain dot guiet at epfl.ch 
    % The necessary disclaimer:
    % We abide by the GNU General Public License (GPL) version 3:
    %     This program is free software: you can redistribute it and/or modify
    %     it under the terms of the GNU General Public License as published by
    %     the Free Software Foundation, either version 3 of the License, or
    %     (at your option) any later version.
    % 
    %     This program is distributed in the hope that it will be useful,
    %     but WITHOUT ANY WARRANTY; without even the implied warranty of
    %     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    %     GNU General Public License for more details.co
    % 
    %     You should have received a copy of the GNU General Public License
    %     along with this program.  If not, see <http://www.gnu.org/licenses/>.
    
    properties
        % The class contains the ImarisApplication instance as a property
        % so it can be accessed by all functions.
        ImarisApp;
        
        % Path To Imaris
        imarisPath = 'C:\Program Files\Bitplane\Imaris x64 7.6.5\';
        
        % We can define here the path where ImarisLib.jar is
        imarisLibPath = ['C:\Program Files\Bitplane\Imaris x64 7.6.5\' 'XT\matlab\ImarisLib.jar'];
        
        
    end
    
    
    methods
        function eXT = EasyXT(varargin)
            %% EASYXT builds a new EasyXT object
            % * eXT = EASYXT(imarisApplication) builds a new EasyXT object
            % and attaches it to the currently running Imaris Instance.
            % * eXT = EASYXT('setup') prompts the user to define the foler
            % where the imaris exectuable is. This only needs to be done
            % once.
            %
            % Optional Arguments
            % o imarisApplicationID - in case you'd like to start EasyXT from
            % an XTension
            % o 'setup' - string to announce that you wish to define the
            % path to Imaris.
      
            
            % Supress Java Duplicate Class warnings
            warning('off','MATLAB:Java:DuplicateClass');
            
            lib = getSavedImarisPath();
            if nargin == 1 && (strcmp(varargin{1}, 'setup') || strcmp(lib, ''))
                 [~,PathName] = uigetfile('.exe','Location of Imaris executable');
                 eXT.imarisLibPath = [PathName, 'XT\matlab\ImarisLib.jar'];

                 eXT.imarisPath = PathName;
                 setSavedImarisPath(eXT.imarisPath);
            end
            
            eXT.imarisPath = getSavedImarisPath();
            eXT.imarisLibPath = [eXT.imarisPath, 'XT\matlab\ImarisLib.jar'];

            % Start Imaris Connection
            javaaddpath(eXT.imarisLibPath);
            
            % Create an instance of ImarisLib
            vImarisLib = ImarisLib;
            
           % Re-enable Java Duplicate Class warnings
            warning('on','MATLAB:Java:DuplicateClass');

            % Attach to given ImarisApplication or create new one
            if nargin == 1 && isa(varargin{1}, 'Imaris.IApplicationPrxHelper')
                eXT.ImarisApp = varargin{1};
            elseif nargin == 1 && ischar(varargin{1})
                ID = round(str2double(varargin{1}));
            else
                % Grab the Imaris server
                server = vImarisLib.GetServer();
                try
                    ID = server.GetObjectID(0);
                    if ischar(ID)
                        ID = round(str2double(ID));
                    end
                catch err
                    disp('Seems Imaris is not open: Could not get Imaris Application');
                    disp('Original error below:');
                    rethrow(err);
                end
            
            
                try
                    % Open the Imaris Application
                    eXT.ImarisApp = vImarisLib.GetApplication(ID);
                catch err
                    disp('Could not get Imaris Application with ID');
                    disp('Original error below:');
                    rethrow(err);

                end
            end
        end
        
    end
    
    %% Public methods.
    methods
        
        function ImarisObject = GetObject(eXT, varargin)
            %% GETOBJECT recovers an object from the Surpass scene
            % Imarisobject = GETOBJECT('Name', name, ...
            %                          'Parent', parent, ...
            %                          'Number', number, ...
            %                          'Type', type, ...
            %                          'Cast', isCast
            %                          )
            %
            % Returns the imaris object defined by the Optional Name,
            % Parent, Number, Type and Cast Parameters.
            %
            % The list of options and default values is:
            %   o Name - Name of the object as it appears on the Surpass
            %   Scene, like 'Volume', 'Surface 1', 'Spots 10', etc.
            %       Defaults to [ [] ]
            %
            %   o Parent - The parent object where we should look for the
            %   object. Useful mostly when creating Groups.
            %       Defaults to Surpass Scene
            %
            %   o Number - The nth object in the scene (Starting at 1). Can
            %   be combined with name and type to search for the nth object
            %   withe the same name or the nth surface, for example.
            %       Defaults to [ 1 ]
            %
            %   o Type - Defines the type of object Choices are:
            %     'All', Spots', Surfaces', 'Points' and 'Groups'
            %       Defaults to [ 'All' ]
            %
            %   o Cast - Determines what kind of object you want in return.
            %   Any Surpass component is of class IDataItem. Surfaces for
            %   example are of a subclass of IDataItem, called ISurfaces.
            %   this property, if set to 'true', returns the object of
            %   the subclass if possible. Otherwise, it returns an IDataItem
            %   Object. Not too useful but good to have in case it's
            %   needed.
            %       Defaults to [ true ]
            %
            % Examples:
            %  %Return the active object in the imaris scene
            %  ImarisObj = GetObject(); 
            %  %Returns the first surface object
            %  spotsObj = GetObject('Type', 'Spots');
            %  %Returns fourth object in the scene.
            %  ImarisObj = GetObject('Number', 4);
            %  %Returns the first surface object with name 'My Surfaces'.
            %  surfObj = GetObject('Type', 'Surfaces', 'Name', 'My Surfaces');
            %  %Get the Second Surfaces object inside the folder 'Group 1'
            %  parentFolder = GetObject('Name', 'Group 1');
            %  surfObj = GetObject('Type', 'Surfaces', 'Number', 2, 'Parent', parentFolder);
            % See also CreateGroup
            
            % Define Defaults:
            parent = eXT.ImarisApp.GetSurpassScene;
            name = [];
            number = 1;
            type = 'All';
            cast = true;
            
            for i=1:2:length(varargin)
                
                switch varargin{i}
                    case 'Parent'
                        parent =  varargin{i+1};
                    case 'Name'
                        name = varargin{i+1};
                    case 'Number'
                        number = varargin{i+1};
                    case 'Type'
                        type = varargin{i+1};
                    case 'Cast'
                        cast = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            object = []; %#ok<*NASGU>
            ImarisObject = [];
            if nargin==1
                % Return the active selection
                ImarisObject = GetImarisObject(eXT, eXT.ImarisApp.GetSurpassSelection, cast);
            else
                
                % Search for the object
                specificTypeNum = 0;
                
                
                nChildren = parent.GetNumberOfChildren;
                for i = 1 : nChildren
                    object = parent.GetChild(i-1);
                    objName = char(object.GetName);
                    
                    objType = GetImarisType(eXT, object);
                    
                    if ~isempty(name) % If we're using the name
                        if  strcmp(objName, name) % If the name matches
                            if strcmp(objType,type) || strcmp(type, 'All') % If the type matches
                                specificTypeNum = specificTypeNum+1;
                                if specificTypeNum == number
                                    % GIVE THE OBJECT BACK
                                    ImarisObject = GetImarisObject(eXT, object, cast);
                                    break;
                                end
                            end
                        end
                        
                    else % Use only the number and type
                        if strcmp(objType,type) || strcmp(type, 'All') % If the type matches
                            specificTypeNum = specificTypeNum+1;
                            if specificTypeNum == number
                                % GIVE THE OBJECT BACK
                                ImarisObject = GetImarisObject(eXT, object, cast);
                                break;
                            end
                        end
                    end
                end
            end
        end
        
        function SelectObject(eXT, varargin)
            %% SELECTOBJECT Makes the selected object active on the Surpass Scene
            % SELECTOBJECT('Name', name, ...)
            % All arguments from GetObject can be applied here
            % See also GetObject
            
            obj = GetObject(eXT, varargin{:});
            eXT.ImarisApp.SetSurpassSelection(obj);
            
        end
        
        function number = GetNumberOf(eXT, type, varargin)
            %% GETNUMBEROF recovers the number of objects with a certain type
            % number = GETNUMBEROF(type, 'Name', name, ... )
            % Arguments 
            % o type - Type of Object: Spots, Surfaces, Group, Points,
            % Cells, Filaments, DataSet, Light Source, Camera, Volume,
            % Clipping Plane, Application
            % Optional Key, Value Pairs:
            % o All optional arguments from GetObject can be applied here
            % 
            % This function is nice for iterating among objects of the same
            % kind.
            %
            % Example:
            % nSpots = GetNumberOf('Spots');
            % for i=1:nSpots
            %   aSpot = GetObject('Type', 'Spots', 'Number', i);
            %   % Do something with spot number i here
            % end
            %
            % See also GetObject

            parent = eXT.ImarisApp.GetSurpassScene;
            name = [];
            for i=1:2:length(varargin)
                
                switch varargin{i}
                    case 'Parent'
                        parent =  varargin{i+1};
                    case 'Name'
                        name = varargin{i+1};
                        
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            % Search for the object
            number = 0;
            
            
            nChildren = parent.GetNumberOfChildren;
            for i = 1 : nChildren
                object = parent.GetChild(i-1);
                objName = char(object.GetName);
                
                objType = GetImarisType(eXT, object);
                
                if ~isempty(name) % If we're using the name
                    if  strcmp(objName, name) % If the name matches
                        if strcmp(objType,type) || strcmp(type, 'All') % If the type matches
                            number = number+1;
                        end
                    end
                    
                else % Use only the number and type
                    if strcmp(objType,type) || strcmp(type, 'All') % If the type matches
                        number = number+1;
                    end
                end
            end
            
        end
        
        function name = GetName(eXT, object)
            %% GETNAME returns the name of the object if possible
            % name = GETNAME(object)
            % This is mainly for convenience so we never forget to cast the
            % result of object.GetName to a char()
            %
            % Arguments 
            % o object - The Imaris object whose name you want
            % See also SetName
            
            if any(strcmp(methods(object), 'GetName'))
                name = char(object.GetName);
            else 
                name = [];
                disp('This object has no "GetName" method.')
            end
            
        end
        
        function name = SetName(~, object, name)
            %% SETNAME sets the name of the object if possible
            % name = SETNAME(object, name)
            %
            % Arguments 
            % o object - The Imaris object whose name you want to set
            % o name - The name you want to give to the object
            % See also GetName
            
            if any(strcmp(methods(object), 'SetName'))
                object.SetName(name);
            else 
                disp('This object has no "SetName" method.')
            end
            
        end
        
        function SetColor(~, object, colors)
            %% SETCOLOR sets the color of the object passed as argument
            % SETCOLOR(object, colors) Takes an array that can be of the following forms
            %   - a 1x3 array with each element being an integer between
            %  [0-255].
            %       colors(1) is Red
            %       colors(2) is Green
            %       colors(3) is Blue
            %   - a 1x4 array where colors(1:3) are as above
            %          colors(4) is transparency (values [0-128])
            
            
            if length(colors) < 4           % check if there is only 3 ,
                colors(4) = 0;              % means no transparecncy setted, so color(4) = 0
            end
            
            if length(colors) == 1  && colors < 255        % if only 1 value
                colors(1:3) = colors(1);    % set it for each column
            end
            
            if length(colors) == 2 || length(colors) > 4
                disp('That is not a color, dear... I need a matrix with 1, 3 or 4 elements. Did you forget your medication?');
                colors = [128, 128, 128, 0]; % set it to grey
            end
            
            if length(colors) == 1  && colors > 255
                object.SetColorRGBA(double(colors));
            else
                col = colors(1) + colors(2)*256 + colors(3)*256^2 + colors(4)*256^3;
                % Note that the A value (Transparency, or color(4))can only go to 128, this is a bug in
                % Imaris...
                object.SetColorRGBA(double(col));
            end
            
        end
        
        function color = GetColor(~, object)
            %% GETCOLOR returns the color of the current object as a 4 element vector
            % color = GETCOLOR(object) checks if there is a getcolor method
            % available and retuns a 4-element vector as follows:
            % color(1) : RED
            % color(2) : GREEN
            % color(3) : BLUE
            % color(4) : ALPHA
            
             if any(strcmp(methods(object), 'GetColorRGBA'))
                col = object.GetColorRGBA();
                color = [];
                e = 3;
                for i = fliplr(0:e)
                    color(i+1) = floor(col ./ (256.^i)); %#ok<*AGROW>
                    col = col - color(i+1).*256.^i;
                end
            else 
                disp('This object has no "SetName" method.')
            end
           
            
        end
        
        function spots = CreateSpots(eXT, PosXYZ, spotSizes, varargin)
            %% CREATESPOTS creates and returns a new spot object
            % spots = CREATESPOTS(posXYZ, spotSizes, ...,
            %                     'Name', name, ...
            %                     'Single Timepoint', isSingle, ...
            %                     'Time Indexes', t, ... )
            % Creates a new spots object with the given xyz positions and
            % sizes. 
            % Input Arguments:
            %   o posXYZ - an nx3 array where each row contains the X,
            %   Y and Z position of the spots, in the unit of the imaris
            %   dataset (Usually microns).
            %   o spotSizes - either an nx1 or nx3 array containing the radius
            %   of each spot, making the spot either spherical or defined by
            %   its X Y and Z semi-major axes lenghts.
            % Optional Parameters:
            %   o Name - The name of the spots object, otherwise it uses
            %   Imaris's default naming scheme (Spots 1, Spots 2, ...)
            %   o Single Timepoint - Should the time indexes not be defined
            %   explicitely using the 'Time Indexes' option, are the spots
            %   to be created only for the currently selected timepoint or
            %   for each timepoint in the dataset? Defaults to [ true ]
            %   o Time Indexes - nx1 array with the time index
            %   corresponding to each spot. Must be the same length as
            %   posXYZ.
            %
            % NOTE: The spot is not added to the scene, use ADDTOSCENE for
            % this.
            
            
            t = zeros(size(PosXYZ,1),1);
            isOnlyCurrent = true;
            name = [];
            % in order to complete for time
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Single Timepoint'
                        isOnlyCurrent = varargin{i+1};
                    case 'Name'
                        name = varargin{i+1};
                    case 'Time Indexes'
                        t =  varargin{i+1};
                        isOnlyCurrent = true;
                        
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
             if (all(size(spotSizes) == [1 1]))
                    spotSizes = repmat(spotSizes, size(PosXYZ,1),1);
             end
            
            if ~( size(spotSizes,2) == 3 || size(spotSizes,2) == 1 )&& ( size(spotSizes,1) == size(PosXYZ,1) )
                error('Radii must be either an nx1 array or an nx3 array');
            end
            if size(PosXYZ,2) ~= 3
                error('XYZ must be nx3 in size.');
            end

            % Create the spots
            spots = eXT.ImarisApp.GetFactory().CreateSpots();
            
            if isOnlyCurrent
                spots.Set(PosXYZ,  t, spotSizes);
            else
                nT = eXT.GetSize('T');
                spots.Set(repmat(PosXYZ,nT,1), repmat(t,nT,1), repmat(spotSizes, nT,1));
            end
            
            if ~isempty(name)
                spots.SetName(name);
            end
        end
        
        function CreateNewScene(eXT)
            %% CREATENEWSCENE Creates a new Surpass scene
            % CreateNewScene() is useful for clearning the current scene in
            % the case that we are batch opening images, for examples. 
            
            vSurpassScene = eXT.ImarisApp.GetFactory.CreateDataContainer;
            vSurpassScene.SetName('Scene');
            %% Add a light source
            vLightSource = eXT.ImarisApp.GetFactory.CreateLightSource;
            vLightSource.SetName('Light source');
            %% Add a frame (otherwise no 3D rendering)
            vFrame = eXT.ImarisApp.GetFactory.CreateFrame;
            vFrame.SetName(strcat('Frame'));
            %% Add a Volume (otherwise no 3D rendering)
            vVolume = eXT.ImarisApp.GetFactory.CreateVolume;
            vVolume.SetName(strcat('Volume'));
            
            %% Set up the surpass scene
            eXT.ImarisApp.SetSurpassScene(vSurpassScene);
            AddToScene(eXT, vLightSource);
            AddToScene(eXT, vFrame);
            AddToScene(eXT, vVolume);
            
        end
        
        
        
        function AddToScene(eXT, object, varargin)
            %% ADDTOSCENE adds the object to the Imaris Scene
            % ADDTOSCENE(object, 'Parent', parent) adds the object to the
            % Imaris Scene or to the optional parent object.
            % Optional Parameter/Value pairs
            %   o Parent - The parent object to add this new object to.
            parent = eXT.ImarisApp.GetSurpassScene;
            
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Parent'
                        parent = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            parent.AddChild(object, -1);
            
        end
        
        function RemoveFromScene(eXT, object, varargin)
            %% REMOVEFROMSCENE removes the object from Imaris Scene
            % Optional Parameter/Value pairs
            %   o Parent - The parent object inside which the object lives.
            parent = eXT.ImarisApp.GetSurpassScene;
            
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Parent'
                        parent = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            parent.RemoveChild(object);
        end
        
        function [newChannel, vDataSet] = MakeChannelFromSurfaces(eXT, surface, varargin)
            %% MAKECHANNELFROMSURFACES builds a new channel from a surface mask.
            % [newChannel vDataSet] = MAKECHANNELFROMSURFACES(surface, ...
            %                                      'Mask Channel', maskChannel, ...
            %                                      'IDs', surfIDs, ...
            %                                      'Time Indexes', tInd);
            %                                      'Add Channel', isAdd) ;
            
            % Creates a new channel containing one of either:
            % A mask based on the surface used OR
            % An exsisting channel, masked by the surface object. All
            % values outside the surface object are set to 0. Inside values
            % are set to the value of the original channel.
            % In all cases, this function creates a new channel and returns
            % the index of that channel, as well as the new IDataSet object.
            % Optional parameters are:
            %   o Mask Channel - If set, the number of the channel to mask
            %   with surface.
            %   o Time Indexes - The timepoints to consider. If not set,
            %   all timepoints will be treated. Index starts at 1.
            %   o IDs - IDs of the surfaces to use as masks. You cannot use
            %   this option in conjunction with time indexes, as surface
            %   IDs are unique for each surface, this includes timepoints.
            %   Masking based on track IDs could be implemented at a later
            %   date if needed.
            %   o Add Channel - Defines whether the function adds the
            %   channel to the current or not. You can grab the updated
            %   dataset through the second output argument. [ true ]
            %
            % Output Arguments
            %  newChannel returns the number of the new channel
            %  aDataSet returns the newly created IDataSet object.
            %
            % Example usage
            % >> newChannel = MakeChannelFromSurfaces(surface, 'Mask Channel', 1);
            % Creates a new channel based on channel 1 and masked using
            % surface.
            % >> newChannel = MakeChannelFromSurfaces(surface); Creates a
            % mask of surface and puts it in a new channnel.
            % >> [newChannel aDataSet] = MakeChannelFromSurfaces(surface, 'Add Channel', false);
            % Creates a mask of surface and only returns the dataset without creating it.
            
            time = [];
            surfIDs = 'All';
            maskChannel = 'None';
            isAddChannel = true;
            vDataSet = [];
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'IDs'
                        surfIDs = varargin{i+1};
                    case 'Time Indexes'
                        time = varargin{i+1};
                    case 'Mask Channel'
                        maskChannel =  varargin{i+1};
                    case 'Add Channel'
                        isAddChannel =  varargin{i+1};
                    case 'DataSet'
                        vDataSet = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            % Do all timepoints if nothing is specified
            if isempty(time)
                time = 1:eXT.GetSize('T');
            end
            
            
            if isempty(vDataSet)
                vDataSet = eXT.ImarisApp.GetDataSet.Clone;
            end
            % Get some info from the scene
            [aData, aSize, aMin, aMax, ~] = GetDataSetData(vDataSet);
            
            % Make sure the object is a surface
            if strcmp(eXT.GetImarisType(surface), 'Surfaces')
                
                
                % Get the mask for the surface
                if ~strcmp(surfIDs,'All')
                    for ind=1:size(surfIDs,1)
                        surfaceMask(ind) = surface.GetSingleMask( surfIDs(ind), ...
                            aMin(1), aMin(2),aMin(3), ...
                            aMax(1), aMax(2), aMax(3), ...
                            aSize(1), aSize(2), aSize(3));
                        % Get the time index
                        time(ind) = surface.GetTimeIndex(surfIDs(ind))+1;
                    end
                    
                else
                    % Iterate through all selected timepoints
                    for ind=1:size(time,2)
                        
                        surfaceMask(ind) = surface.GetMask( aMin(1), aMin(2),aMin(3), ...
                            aMax(1), aMax(2), aMax(3), ...
                            aSize(1), aSize(2), aSize(3), ...
                            time(ind)-1);
                        
                        
                    end
                end
                
                % Now make the new channel for each timepoint
                newChannel = eXT.GetSize('C');
                vDataSet.SetSizeC(newChannel+1);
                dataType = eXT.GetDataType();
                
                % And now create the new volumes for each timepoint or for
                % each subsurface
                for ind=1:size(surfaceMask,1)
                    switch dataType
                        case '8-bit'
                            maskVol = surfaceMask(ind).GetDataVolumeBytes(0,0);
                        case '16-bit'
                            maskVol = surfaceMask(ind).GetDataVolumeShorts(0,0);
                        case '32-bit'
                            maskVol = surfaceMask(ind).GetDataVolumeFloats(0,0);
                    end
                    
                    if strcmp(maskChannel, 'None')
                        chanVol = aData + 255;
                    else
                        
                        switch dataType
                            case '8-bit'
                                chanVol = vDataSet.GetDataVolumeBytes(maskChannel-1,time(ind)-1);
                            case '16-bit'
                                chanVol = vDataSet.GetDataVolumeShorts(maskChannel-1,time(ind)-1);
                            case '32-bit'
                                chanVol = vDataSet.GetDataVolumeFloats(maskChannel-1,time(ind)-1);
                        end
                    end
                    % class(chanVol)
                    % class(maskVol)
                    
                    % Write to the dataset.
                    switch dataType
                        case '8-bit'
                            vDataSet.SetDataVolumeBytes(chanVol .* maskVol, newChannel, time(ind)-1);
                        case '16-bit'
                            vDataSet.SetDataVolumeShorts(chanVol .* maskVol, newChannel, time(ind)-1);
                        case '32-bit'
                            vDataSet.SetDataVolumeFloats(chanVol .* maskVol, newChannel, time(ind)-1);
                    end
                end
                
                % Name the channel
                if ~strcmp(maskChannel, 'None')
                    vDataSet.SetChannelName(newChannel, ['Masked Channel: ' char(vDataSet.GetChannelName(maskChannel-1)) ' using "' char(surface.GetName) ' "']);
                    % Give the channel the same color as the Surface object
                    vRGBA = vDataSet.GetChannelColorRGBA(maskChannel-1);
                    vDataSet.SetChannelColorRGBA(newChannel, vRGBA);
                    
                else
                    vDataSet.SetChannelName(newChannel, ['Masked from Surface ' char(surface.GetName)]);
                    % Give the channel the same color as the Masked Channel
                    vRGBA = surface.GetColorRGBA;
                    vDataSet.SetChannelColorRGBA(newChannel, vRGBA);
                    
                end
                
                if isAddChannel
                    disp('Adding');
                    eXT.ImarisApp.SetDataSet(vDataSet);
                end
                
            else
                name = char(surface.GetName());
                disp([name ' is not a surface.']);
            end
            newChannel = newChannel + 1;
        end
        
        function [newChannel, vDataSet] = MakeChannelFromSpots(eXT, mySpots, varargin)
            %% MAKECHANNELFROMSPOTS builds a new channel from a spots mask.
            % [newChannel, vDataSet] = MAKECHANNELFROMSPOTS(spots, ...
            %                                      'IDs', spotIDs, ...
            %                                      'Time Indexes', tInd;
            %                                      'Add Channel', isAdd
            %                                      'Interpolate', isInterp, ...
            %                                      'DataSet', vDataSet) ;
            
            % Creates a new channel containing
            % A mask based on the spots used OR
            %
            % In all cases, this function creates a new channel and returns
            % the index of that channel, as well as the new IDataSet object.
            % Optional parameters are:
            %   o Time Indexes - The timepoints to consider. If not set,
            %   all timepoints will be treated. Index starts at 1.
            %   o IDs - IDs of the surfaces to use as masks. You cannot use
            %   this option in conjunction with time indexes, as surface
            %   IDs are unique for each surface, this includes timepoints.
            %   Masking based on track IDs could be implemented at a later
            %   date if needed.
            %   o Add Channel - Defines whether the function adds the
            %   channel to the current or not. You can grab the updated
            %   dataset through the second output argument. [ true ]
            %   o Interpolate - Whether or not to use interpolation at the
            %   edge of the spots. if True, you will not have a mask
            %   channel, as there will be values between 0 and 1.
            %   o DataSet - Provide the dataset to draw the spots in
            %   explicitly. This is useful if you want to update a dataset
            %   with multiple calls to this function, so that you don't
            %   need to update the Scene every time.
            %
            %  Output Arguments
            %    newChannel returns the number of the new channel
            %    aDataSet returns the newly created IDataSet object.
            %
            % Example usage
            % >> newChannel = MakeChannelFromSurfaces(spots, 'Interpolate', true);
            % Creates a new channel based on spots with interpolation
            
            isInterpolate  = false;
            isAddChannel = true;
            spotIDs = 'All';
            time = [];
            vDataSet = [];
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'IDs'
                        spotIDs = varargin{i+1};
                    case 'Time Indexes'
                        time = varargin{i+1};
                    case 'Add Channel'
                        isAddChannel =  varargin{i+1};
                    case 'Interpolate'
                        isInterp        olate = varargin{i+1};
                    case 'DataSet'
                        vDataSet = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            % Do all timepoints if nothing is specified
            if isempty(time)
                time = 1:eXT.GetSize('T');
            end
            
            % Grab the spots Info
            vSpotsXYZ    = mySpots.GetPositionsXYZ;
            vSpotsRadius = mySpots.GetRadiiXYZ;
            vSpotsTime   = mySpots.GetIndicesT;
            
            
            
            if ~strcmp(spotIDs, 'All')
                % Append to the processing only the selected spot IDs
                vSpotsXYZ = vSpotsXYZ(spotIDs, :);
                vSpotsRadius = vSpotsRadius(spotIDs, :);
                vSpotsTime   = vSpotsTime(spotIDs);
                
            end
            
            if isempty(vDataSet)
                vDataSet = eXT.ImarisApp.GetDataSet.Clone;
            end
            
            [aData, ~, aMin, aMax, vType] = GetDataSetData(vDataSet);
            
            
            % Prepare the new channel.
            newChannel = vDataSet.GetSizeC;
            vDataSet.SetSizeC(newChannel+1);
            
            
            
            % Start Building the spots
            for vTime = time
                aData(:) = 0; % Initialize vData to 0 for each new timepoint
                
                
                % Select spots associated with current time
                vSpotsIndex = find(vSpotsTime == (vTime-1))';
                for vIndex = vSpotsIndex
                    vPos = vSpotsXYZ(vIndex, :); % Get their positions
                    vRadius = vSpotsRadius(vIndex,:); % And their Radii
                    aData = DrawSphere(aData, vPos, vRadius, aMin, aMax, vType, isInterpolate); % Now draw the sphere
                end
                
                % If there were spots drawn at this time, set the dataset
                % of the appropriate type.
                if ~isempty(vSpotsIndex)
                    if strcmp(vDataSet.GetType, 'eTypeUInt8')
                        vDataSet.SetDataVolumeBytes(aData, newChannel, (vTime-1));
                    elseif strcmp(vDataSet.GetType, 'eTypeUInt16')
                        vDataSet.SetDataVolumeShorts(aData, newChannel, (vTime-1));
                    else
                        vDataSet.SetDataVolumeFloats(aData, newChannel, (vTime-1));
                    end
                end
                
            end
            % Give the channel the same color as the spot object
            vRGBA = mySpots.GetColorRGBA;
            vDataSet.SetChannelColorRGBA(newChannel, vRGBA);
            vDataSet.SetChannelName(newChannel, mySpots.GetName);
            % Finally Add everyting to the dataset
            
            if (isAddChannel)
                eXT.ImarisApp.SetDataSet(vDataSet);
            end
            newChannel = newChannel+1;
            
        end
        
        function [newChannel, vDataSet] = ChannelMath(eXT, expression, varargin)
            %% CHANNELMATH Perform channel-based operations
            % [newChannel, vDataSet] = CHANNELMATH(expression) recovers
            % data from imaris channels (Called using ch1, ch2, ...) and
            % evaluates a matlab operation on those channels. All Matlab
            % functions are accessible.
            % expression is a string that contains the Matlab-language
            % calculation you want to perform on the channels.
            % Channel names: ch1, ch2, Use matlab operators, i.e.
            % +, -, .*, ./, .^, sqrt
            % Optional parameters:
            %   o Add Channel - Adds the computed channel to the Imaris
            %   DataSet. Defaults to [ true ]. If false, the result is in
            %   the vDataSet output.
            %   o DataSet - Provide the dataset to draw the spots in
            %   explicitly. This is useful if you want to update a dataset
            %   with multiple calls to this function, so that you don't
            %   need to update the Scene every time.
            % Output parameters
            %   o newChannel - The index of the new channel in the DataSet
            %   o vDataSet - A clone of the original dataset with the new
            %   channel.
            
            vDataSet = [];
            isAddChannel = true;
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Add Channel'
                        isAddChannel =  varargin{i+1};
                    case 'DataSet'
                        vDataSet =  varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            
            if isempty(expression)
                disp('Write an expression, guy')
            end
            
            if isempty(vDataSet)
                vDataSet = eXT.ImarisApp.GetDataSet.Clone;
            end
            
            vLastC = vDataSet.GetSizeC;
            vDataSet.SetSizeC(vLastC + 1);
            vMin = vDataSet.GetChannelRangeMin(0);
            vMax = vDataSet.GetChannelRangeMax(0);
            vDataSet.SetChannelRange(vLastC, vMin, vMax);
            
            %Iterate through all time points
            for vTime = 1:vDataSet.GetSizeT
                for vSlice = 1:vDataSet.GetSizeZ
                    
                    for vChannel = 1:vLastC
                        % works on double to allow division and prevent overflows
                        eval(sprintf(['ch%i = double(vDataSet.GetDataSliceFloats(', ...
                            'vSlice-1, vChannel-1, vTime-1));'], vChannel));
                    end
                    
                    try
                        vData = eval(expression{1});
                        
                    catch er
                        fprintf(['Error while evaluating the expression.\n\n', ...
                            'Possible causes: invalid variable names (ch1, ch2, ...), ', ...
                            'invalid operators (use .* instead of *)...\n\n', er.message])
                        return
                    end
                    
                    try
                        if strcmp(vDataSet.GetType,'eTypeUInt8')
                            vDataSet.SetDataSliceBytes(uint8(vData), vSlice-1, vLastC, vTime-1);
                        elseif strcmp(vDataSet.GetType,'eTypeUInt16')
                            vDataSet.SetDataSliceShorts(uint16(vData), vSlice-1, vLastC, vTime-1);
                        elseif strcmp(vDataSet.GetType,'eTypeFloat')
                            vDataSet.SetDataSliceFloats(single(vData), vSlice-1, vLastC, vTime-1);
                        end
                    catch er
                        fprintf(['The result of the expression is not a valid dataset.\n\n', ...
                            'Possible causes: invalid result size.\n\n', er.message])
                        return
                    end
                    
                end
            end
            
            %NewChannel is the channel number as displayed by Imaris
            %(Starting at 1), and not the channel index from the XTensions
            %(Starting at 0)
            newChannel = vLastC+1;
            
            vDataSet.SetChannelName(vLastC, expression{1});
            if isAddChannel
                eXT.ImarisApp.SetDataSet(vDataSet);
            end
            
            
        end
        
        function surface = DetectSurfaces(eXT, channel, varargin)
            %%DETECTSURFACES Applies the Imaris Surfaces Detection Functions
            % surface = DETECTSURFACES(channel, 'Name', surfaceName, ...
            %                                  'Color', color, ...
            %                                  'Smoothing', smooth, ...
            %                                  'Local Contrast', isLocContrast, ...
            %                                  'Threshold', thr, ...
            %                                  'Filter', filterString, ...
            %                                  'Seed Diameter', seedD, ...
            %                                  'Seed Local Contrast', isSeedLocC, ...
            %                                  'Seed Filter', seedFilterString, ...
            %                                  'DataSet', aDataSet, ...
            % Detects a surface from channel channel with optinal
            % parameters:
            %   o Name - The name of the new surface object. Defaults to
            %   Imaris standard name.
            %   o Color - a single value or, 1x3 or 1x4 arrays with the
            %   color in RGBA format.
            %       colors(1) is Red (or gray if color is 1x1)
            %       colors(2) is Green
            %       colors(3) is Blue
            %       colors(4) is transparency (values [0-128])
            %   Defaults to white [255, 255, 255].
            %   o Smoothing - The smoothing kernel of the gaussian blur, in
            %   microns, usually. Defaults to twice the pixel size in XY.
            %   o Local Contrast - if different than 0, the local contrast
            %   filter size.
            %   o Threshold - The threshold value for determining the
            %   surfaces. If local contrast is 0, it is an absolute
            %   intensity threshold. Otherwise it is the threshold to apply
            %   on the Local Contrast-filtered image. Set to 'Auto' to use 
            %   the Imaris built-in auto threshold method. Defaults to
            %   'Auto';
            %   o Surface Filter - A string containing the filter you want
            %   to apply to your surfaces. Example: '"Quality" above 7.000'
            %   Defaults to '"Number of Voxels" above 10.0'
            %   o Seed Diameter - The diameter of the seed detection for if
            %   you want to split the surfaces
            %   o Seed Local Contrast - If you want to use local contrast
            %   or absolute intensity when looking for seeds
            %   o Seed Filter - Filter string for the seeds. Defaults to
            %   [ '' ]
            %   o DataSet - Provide the dataset to detect the surfaces in
            %   explicitly. This is useful if you want to update a dataset
            %   with multiple calls to this function, so that you don't
            %   need to update the Scene every time.
            %
            
            
            
            
            
            
            %Defaults
            th = 'Auto';
            gb = [];
            filter = '"Number of Voxels" above 10.0';
            name = '';
            isAutoThr = true;
            bgLocContrast = 0; % No automatic background
            color = 255;
            vDataSet = [];
            isSplit = false;
            seedDiam = 1;
            isSeedLocContrast = false;
            seedFilter = '';
            
            % get the value of each argement you have specified, if you
            % have specified one :)
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Smoothing'
                        gb =  varargin{i+1};
                    case 'Local Contrast'
                        bgLocContrast = varargin{i+1};
                    case 'Threshold'
                        th = varargin{i+1};
                        if strcmp(th, 'Auto')
                            isAutoThr = true;
                            th = 0;
                        end
                    case 'Filter'
                        filter =  varargin{i+1};
                    case 'Name'
                        name = varargin{i+1};
                    case 'Color'
                        color = varargin{i+1};
                    case 'DataSet'
                        vDataSet = varargin{i+1};
                    case 'Seed Diameter'
                        seedDiam = varargin{i+1};
                        isSplit = true;
                    case 'Seed Local Contrast'
                        isSeedLocContrast = varargin{i+1};
                        isSplit = true;
                    case 'Seed Filter'
                        seedFilter = varargin{i+1};
                        isSplit = true;
                        
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            if isempty(vDataSet)
                vDataSet = eXT.ImarisApp.GetDataSet;
            end
            
            if isempty(gb)
                [voxelXY, ~] = eXT.getVoxelSize();
                gb = voxelXY*2;
            end
            
            if ~isSplit
                surface = eXT.ImarisApp.GetImageProcessing.DetectSurfaces(vDataSet,[], channel-1,gb,bgLocContrast,isAutoThr,th,filter);
            else
                surface = eXT.ImarisApp.GetImageProcessing.DetectSurfacesRegionGrowing(vDataSet,[], channel-1,gb,bgLocContrast,isAutoThr,th,seedDiam, isSeedLocContrast, seedFilter, filter);
            end
            
            if ~strcmp(name, '') % give name if defined else default by Imaris
                surface.SetName(name);
            end
            
            SetColor(eXT, surface, color);
        end
        
        function channelNumber = CreateSurfaceIDChannel(eXT, surfaceObj)    
            % Create a new channel and for each surface object create a
            dataSet = eXT.ImarisApp.GetDataSet();
            
            dataSet.SetSizeC(dataSet.GetSizeC()+1);
            
            
            nSurf = surfaceObj.GetNumberOfSurfaces();
            
            sX = dataSet.GetSizeX;
            sY = dataSet.GetSizeY;
            sZ = dataSet.GetSizeZ;
            
            sT= dataSet.GetSizeT;
            sC= dataSet.GetSizeC;
            
            % Prepare a 1DByte with the last channel and all timepoints
            for t=0:(sT-1)
                volume(:,t+1) = int8(zeros(sX*sY*sZ,1));
            end
            
            mX = dataSet.GetExtendMinX;
            mY = dataSet.GetExtendMinY;
            mZ = dataSet.GetExtendMinZ;
            
            MX = dataSet.GetExtendMaxX;
            MY = dataSet.GetExtendMaxY;
            MZ = dataSet.GetExtendMaxZ;
            for i=0:(nSurf-1)
                t = surfaceObj.GetTimeIndex(i);
                surfaceMask = surfaceObj.GetSingleMask (i, mX, mY, mZ, MX, MY, MZ, sX, sY, sZ).GetDataVolumeAs1DArrayBytes (0, 0).*(i);
                volume(:,t+1) = volume(:,t+1) + surfaceMask;
                
            end
        
            for t=0:(sT-1)
               dataSet.SetDataVolumeAs1DArrayBytes ( volume(:,t+1), sC-1, t);
            end
        end
        
        function spots = DetectSpots(eXT, channel, varargin)
            %%DETECTSPOTS Applies the Imaris Spots Detection Functions
            % spot = DETECTSPOTS(channel, 'Name', spotName, ...
            %                             'Color', color, ...
            %                             'Diameter XY', dXY, ...
            %                             'Diameter Z', dZ, ...
            %                             'Subtract BG, isSubtractBG, ...
            %                             'Local Contrast', isLocContrast, ...
            %                             'Spots Filter', filterString, ...
            %                             'Seed Diameter', seedD, ...
            %                             'Seed Local Contrast', isSeedLocC, ...
            %                             'Seed Filter', seedFilterString, ...
            %                             'DataSet', aDataSet
            %                   )
            %
            % Detects a spots from channel channel with optional
            % parameters:
            %   o Name - The name of the new spots object. Defaults to
            %   Imaris standard name.
            %
            %   o Color - a single value or, 1x3 or 1x4 arrays with the
            %   color in RGBA format.
            %       colors(1) is Red (or gray if color is 1x1)
            %       colors(2) is Green
            %       colors(3) is Blue
            %       colors(4) is transparency (values [0-128])
            %   Defaults to white [255, 255, 255].
            %
            %   o Diameter XY - the diameter of the spots, usually in
            %   microns, for the spot renderind and detection if using
            %   local contrast. If Diameter Z is not defined, these will be
            %   spherical spots. Defaults to 2x the voxel size in XY
            %
            %   o Diameter Z - the diameter of the spots in Z, if you want
            %   to detect ellipsoids. Defaults to 2x the voxel size in Z.
            %
            %   o Subtract BG - Should the spots be detected using a BG
            %   subtraction or from the raw intensities. Defaults to true
            %
            %   o Region Growing - Logical defining whether you wish to
            %   apply region growing to the spots. Defaults to false.
            %
            %   o Region Growing Local Contrast - Logical defining whether
            %   to use local contrast or absolute intensity for region
            %   growing. Defaults to true.
            %
            %   o Region Threshold - Defines the threshold value to use for
            %   the Region Growing step. Defaults to 'Auto'.
            %
            %   o Diameter From Volume - Logical defining whether the
            %   Region Growing dpot diameters are computed from the volume
            %   of the thresholded regions. Otherwise, the diameter is
            %   calculated from the distance to the region borders.
            %
            %   o Spots Filter - A string containing the filter you want
            %   to apply to your spots. Example: '"Quality" above 7.000'
            %   Defaults to '"Quality" above automatic threshold'
            %
            %   o DataSet - Provide the dataset to detect the surfaces in
            %   explicitly. This is useful if you want to update a dataset
            %   with multiple calls to this function, so that you don't
            %   need to update the Scene every time.            
            
            % Prepare variables
            useEllipse = false; % used internally only
            isRegAutoThr = true; % used internally only
            
            name = [];
            color = [];
            dxy =[];
            dz = [];
            isSubtractBG = true;
            filter = '"Quality" above automatic threshold';
            isRegionGrow = false;
            isRegLocContrast = true;
            regionThr='Auto';
            isDiamFromVol = false;
            vDataSet = [];
            
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Name'
                        name = varargin{i+1};
                    case 'Color'
                        color = varargin{i+1};
                    case 'Diameter XY'
                        dxy = varargin{i+1};
                    case 'Diameter Z'
                        dz = varargin{i+1};
                        useEllipse = true;
                    case 'Subtract BG'
                        isSubtractBG =  varargin{i+1};
                    case 'Spots Filter'
                        filter =  varargin{i+1};
                    case 'Region Growing'
                        isRegionGrow = varargin{i+1};
                    case 'Region Growing Local Contrast'
                        isRegLocContrast = varargin{i+1};
                        isRegionGrow = true;
                    case 'Region Threshold'
                        regionThr= varargin{i+1};
                        isRegionGrow = true;
                    case 'Diameter From Volume'
                        isDiamFromVol = varargin{i+1};
                        isRegionGrow = true;
                    case 'DataSet'
                        vDataSet = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            if strcmp(regionThr, 'Auto')
                isRegAutoThr = true;
                regionThr = -1;
            end
                            
            % Set Defaults
            if isempty(vDataSet)
                vDataSet = eXT.ImarisApp.GetDataSet;
            end
            
            if isempty(dxy)
                [voxelXY, voxelZ] = eXT.getVoxelSize();
                dxy = voxelXY*2;
                if isempty(dz)
                    dz = voxelZ*2;
                end
            end
            
            
            
            % Finally detect the spots...
            if(isRegionGrow)
                if(useEllipse)
                    spots = eXT.ImarisApp.GetImageProcessing().DetectEllipticSpotsRegionGrowing(vDataSet, [], channel-1, [dxy dxy dz], isSubtractBG, filter, isRegLocContrast, isRegAutoThr, regionThr, isDiamFromVol, false);
                else
                    spots = eXT.ImarisApp.GetImageProcessing().DetectSpotsRegionGrowing(vDataSet, [], channel-1, dxy, isSubtractBG, filter, isRegLocContrast, isRegAutoThr, regionThr, isDiamFromVol, false);
                end
                
            else
                if(useEllipse)
                    spots = eXT.ImarisApp.GetImageProcessing().DetectEllipticSpots(vDataSet , [], channel-1, [dxy dxy dz], isSubtractBG, filter);
                else
                    spots = eXT.ImarisApp.GetImageProcessing().DetectSpots2(vDataSet, [], channel-1, dxy, isSubtractBG, filter);
                end
            end
            
            if ~isempty(name)
                spots.SetName(name);
            end
            
            if isempty(color)
                color = double(eXT.ImarisApp.GetDataSet.GetChannelColorRGBA(channel-1));
           end     
                SetColor(eXT,spots, color);
        end
                
        function newPoints = CreateMeasurementPoints(eXT, XYZ, varargin)
            %% CREATEMEASUREMENTPOINTS Creates new Measurement Points with default parameters
            % newPoints = CREATEMEASUREMENTPOINTS(XYZ, 'Name', name, ...
            %                                    'Timepoints', times, ...
            %                                    'Point Names', pointNames)
            % Make a new Points Object and return it. If no optional
            % parameters are set, then it creates the points at all
            % timepoints and gives them names like "A", "B, .... "AA",
            % "AB", ..
            %
            % Parameters: 
            % XYZ: nx3 array with the XYZ coordinates of the measurement
            %      points. 
            %
            % Optional 'Key', value pairs:
            % o 'Name' : String containing the name of the new Measurement
            %            Points.
            %
            % o 'Timepoints' : The timepoints where you want the
            %                  measurement points to be created make sure
            %                  that the rows of `XYZ` match the rows of
            %                  `times`
            %
            % o 'Point Names' : An array with the name of each point.
            
            duplicate = true;
            name = [];
            times = [];
            pointNames = {};
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Name'
                        name =  varargin{i+1};
                    case 'Timepoints'
                        times =  varargin{i+1};
                    case 'Point Names'
                        pointNames =  varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            % Declare new Points
            newPoints = eXT.ImarisApp.GetFactory.CreateMeasurementPoints();
                       
            if size(times,2) ~= size(XYZ,1)
                if isempty(times) 
                    % Get number of timepoints
                    nT = GetSize(eXT, 'T');
                    times = (1:nT) - 1;
                    %Duplicate the points at each timepoint and the times array
                    %as well
                else
                    nT = size(times,2);
                end
                
                 %For each measurement point, duplicate it for all timepoints
                 % We will put the points at the given timepoints
                 
                 XYZ = repmat(XYZ, nT,1); 
                 times = repmat(times, size(XYZ,1),1);
                 times = reshape(times, size(XYZ,1)*nT,1);
                 
                
            end
            
            if size(pointNames,1) ~= size(XYZ,1)
                % Make sure the pointNames make sense.
                if isempty(pointNames)
                    % Create from scratch
                    
                end
                
            end              
            if ~isempty(name)
                newPoints.SetName(name);
            end
            
            % Create the points
            newPoints.Set(XYZ,times, pointNames);
            
        end
    end
    
    %% Colocalization-related Methods
    methods 
        function finalC = Coloc(eXT, channels, thresholds, varargin)
            %% COLOC Implements some basic colocalization formulas
            % Options: Select timepoint, use some channel as mask
            % Do Pearsons test, or CDA for ALL parameters
            % Make coloc channel
            % Show Fluorogram
            % Documentation not finished... sorry
            
            isMakeColocChannel = false;
            times = 1:GetSize(eXT, 'T');
            isFluorogram = false;
            mask = [];
            maskChannel = [];
            maskThreshold = [];
            isZIndependent = false;
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Coloc Channel'
                        isMakeColocChannel =  varargin{i+1};
                    case 'Timepoints'
                        times =  varargin{i+1};
                    case 'Fluorogram'
                        isFluorogram = true;
                    case 'Mask Channel'
                        maskChannel = varargin{i+1};
                        maskThreshold = 1;
                    case 'Mask Threshold'
                        maskThreshold = varargin{i+1};
                    case 'Independent Z'
                        isZIndependent = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            % Get the type of dataset
            dataType = eXT.GetDataType();
            
            finalC = struct([]);
            % Do the coloc for each pair of channels given
            for nI = 1:size(channels,1)
                % Coloc in 3D, for each timepoint
                if(isMakeColocChannel)
                    eXT.SetSize('C', eXT.GetSize('C')+1);
                    newC = eXT.GetSize('C');
                    chNames = eXT.GetChannelNames();
                    eXT.SetChannelName(newC, sprintf('Colocalization %s (%.1f) and %s (%.1f)', chNames{channels(nI,1)}, thresholds(nI,1), chNames{channels(nI,2)}, thresholds(nI,2)));
                end
                
                for nT = times
                    % Get the voxels for the channels and the mask
                    I1 = eXT.GetVoxels(channels(nI,1), 'Time', nT, '1D', true);
                    I2 = eXT.GetVoxels(channels(nI,2), 'Time', nT, '1D', true);
                    if ~isempty(maskChannel)
                        mask = eXT.GetVoxels(maskChannel, 'Time', nT, '1D', true);
                        mask = mask>maskThreshold;
                    end
                    
                    coloc = getColocCoeffs(eXT, I1, I2,  channels(nI,1), channels(nI,2), thresholds(nI,1), thresholds(nI,2), nT, mask, maskChannel, maskThreshold, isZIndependent);
                    tmp = struct2table(coloc);
                    if isempty(finalC)
                        finalC = tmp;
                    else
                        finalC = [finalC; tmp];
                    end
                    
                    
                    if(isMakeColocChannel)
                        
                        % Make Coloc Channel
                        colocArray1D = getColocChannel(I1, I2, thresholds(nI,:));
                        
                        switch dataType
                            case '8-bit'
                                eXT.ImarisApp.GetDataSet.SetDataVolumeAs1DArrayBytes(colocArray1D, newC-1, nT-1);
                            case '16-bit'
                                eXT.ImarisApp.GetDataSet.SetDataVolumeAs1DArrayShorts(colocArray1D, newC-1, nT-1);
                            case '32-bit'
                                eXT.ImarisApp.GetDataSet.SetDataVolumeAs1DArrayFloats(colocArray1D, newC-1, nT-1);
                        end                        
                    end
                    
                    % Make Fluorogram
                    if(isFluorogram)
                        getFluorogram(I1, I2, channels(nI,1), channels(nI,2), thresholds(nI,:), true);
                    end
                    % Run Coloc Test(s)
                end
                
            end
        end
          
    end
    
    %% Helper and Type converting functions
    methods
        
        function V = GetVoxels(eXT, channel, varargin)
            %% GETVOXELS returns an array of voxels of the selected channel
            % GETVOXELS(channel) returns a 1D, 3D or 4D array (if time) of the
            % selected channel.
            % 
            % Optional 'Key', Value pairs:
            % o 'Time', time : Returns only the selected timepoint
            %
            % o '1D', logical: Returns the voxels as a 1D array
            %
            % o 'Slice', slice: Returns only the selected slice
            
            is1D = false;
            slice = [];
            time = eXT.ImarisApp.GetVisibleIndexT+1;
            
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Time'
                        time =  varargin{i+1};
                    case '1D'
                        is1D =  varargin{i+1};
                    case 'Slice'
                        slice =  varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            dataType = eXT.GetDataType();
           
            
            if ~is1D && isempty(slice)
                switch dataType
                    case '8-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataVolumeBytes(channel-1, time-1);
                    case '16-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataVolumeShorts(channel-1, time-1);
                    case '32-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataVolumeFloats(channel-1, time-1);
                    otherwise
                        V = [];   
                end
            elseif ~is1D && ~isempty(slice)
                
                switch dataType
                    case '8-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataSliceBytes(slice-1, channel-1, time-1);
                    case '16-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataSliceShorts(slice-1, channel-1, time-1);
                    case '32-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataSliceFloats(slice-1, channel-1, time-1);
                    otherwise
                        V = [];
                end

            elseif is1D && isempty(slice)

                switch dataType
                    case '8-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataVolumeAs1DArrayBytes(channel-1, time-1);
                    case '16-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataVolumeAs1DArrayShorts(channel-1, time-1);
                    case '32-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataVolumeAs1DArrayFloats(channel-1, time-1);
                    otherwise
                        V = [];
                end
            elseif is1D && ~isempty(slice)
                nx = eXT.GetSize('X');
                ny = eXT.GetSize('Y');
                
                switch dataType
                    case '8-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataSubVolumeAs1DArrayBytes(0,0,slice-1,channel-1, time-1, nx,ny, 1);
                    case '16-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataSubVolumeAs1DArrayShorts(0,0,slice-1,channel-1, time-1, nx,ny, 1);
                    case '32-bit'
                        V = eXT.ImarisApp.GetDataSet.GetDataSubVolumeAs1DArrayFloats(0,0,slice-1,channel-1, time-1, nx,ny, 1);
                    otherwise
                        V = [];
                end
            end
        end
        
        function folderRef = CreateGroup(eXT, name)
            %% Create a group
            folderRef = eXT.ImarisApp.GetFactory.CreateDataContainer;
            % Check name doesn't already exist
            newName = name;
            obj = GetObject(eXT,'Name', name);
            k=1;
            while (~isempty(obj))
                newName = sprintf('%s -%i',name, k);
                obj = GetObject(eXT, 'Name', newName);
                k=k+1;

            end
            folderRef.SetName(newName);
        end
        
        function RunXtension(eXT, name)
            addpath([eXT.imarisPath 'XT\matlab']);
            eval([name '(eXT.ImarisApp);']);
        end
        
        function GaussianSmooth(eXT, channel, sigma, varargin)
            %% GAUSSIANSMOOTH Smoothes the selected channel by a gaussian
            % GAUSSIANSMOOTH(channel, sigma, 'Duplicate', true smooths a
            % channel with a gaussian kernel of sigma. If 'Duplicate'
            % parameter value is set to false, then it will overwrite the
            % existing channel
            
            duplicate = true;
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Duplicate'
                        duplicate =  varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            vData = eXT.ImarisApp.GetDataSet;
            %Duplicate the channel
            channelName = char(vData.GetChannelName(channel-1));
            color= vData.GetChannelColorRGBA(channel-1);
            if (duplicate)
                chString = strcat('ch', num2str(channel));
                exp = {chString};
                c = GetSize(eXT, 'C');
                ChannelMath(eXT, exp);
            end
            % Now smooth it.
            c = GetSize(eXT, 'C')-1;
            
            eXT.ImarisApp.GetImageProcessing.GaussFilterChannel(vData,c,sigma);
            vData.SetChannelName(c, [channelName ' Gaussian Sigma ' num2str(sigma)]);
            vData.SetChannelColorRGBA(c, color);
            
        end
        
        function newChannel = DistanceTransform(eXT, object, varargin)
            %% DISTANCETRANSFORM applies a distance transform based on the current object
            % DISTANCETRANSFORM(object, 'Type', 'Inside', ...
            %                           'Name', 'Name for Distance Channel');
            % Optional 'Key', Value pairs:
            % o 'Type', string : 'Inside' or 'Outside' to define where the
            % Distance transform takes place
            %
            % o 'Name', string: The name of the new channel with the
            % distance transform

            isInside = true;
            type = 'Inside';
            objName = eXT.GetName(object);
            customName = '';
                        
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Type'
                        if strcmp(varargin{i+1}, 'Outside')
                            isInside = false;
                            type = varargin{i+1};
                        end
                        
                    case 'Name'
                        customName = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            objType = eXT.GetImarisType(object);
            
            switch objType
                case 'Spots'
                    [newChannel, dataSet] = eXT.MakeChannelFromSpots(object);
                case 'Surfaces'
                    [newChannel, dataSet] = eXT.MakeChannelFromSurfaces(object);
                otherwise
            end
            
            eXT.ImarisApp.GetImageProcessing.DistanceTransformChannel( ...
            dataSet, newChannel-1, 1, isInside);
        
           if ~strcmp(customName, '')
               name = customName;
           else
               name = sprintf('Distance Transform %s %s',objName , type);
           end
           
           eXT.SetChannelName(newChannel, name);
           
           eXT.ImarisApp.SetDataSet(dataSet);

   
        end
        
        
        function [pathstr,name,ext] = GetCurrentFileName(eXT) 
            %% GetCurrentFileName returns the surrent file name path and extension
            % [pathstr,name,ext] = GetCurrentFileName() has no arguments and returns the same variables
            % as the fileparts function 
            % see also FILEPARTS
            fullPath = char(eXT.ImarisApp.GetCurrentFileName());
            [pathstr,name,ext] = fileparts(fullPath);
        end
        
        function [name, number] = SelectDialog(eXT, theType, varargin)
            %% SelectDialog gives a list of available objects of the given type
            % The user can then select one or several names and numbers to
            % be used with GetObject.
            
            parent = eXT.ImarisApp.GetSurpassScene;
            selMode = 'single';
             for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Parent'
                        parent =  varargin{i+1};
                    case 'Select Mode'
                        selMode = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
             end
            
            nGr = GetNumberOf(eXT,theType);
            groupNames={};
            if nGr > 1
                for i=1:nGr
                    grp = eXT.GetObject('Type', theType, 'Number',i, 'Parent', parent);
                    groupNames{i} = eXT.GetName(grp);
                end
            end

            [sel,~] = listdlg('ListString', groupNames, ...
                               'SelectionMode', selMode, ...
                               'Name', ['Select the ' theType], ...
                               'OKString', 'Select' );
            name = groupNames(sel);
            number = sel;
        end
        
        function obj = GetImarisObject(eXT, object, cast)
            %% GETIMARISOBJECT Casts the object to its more specific type if cast is true
            % This is a convenience function to convert all the different
            % Imaris subclasses of IDataItem
            if ~cast
                obj = object;
            else
                IFactory = eXT.ImarisApp.GetFactory;
                if IFactory.IsDataContainer(object)
                    obj = IFactory.ToDataContainer(object);
                elseif IFactory.IsSpots(object)
                    obj = IFactory.ToSpots(object);
                elseif IFactory.IsSurfaces(object)
                    obj = IFactory.ToSurfaces(object);
                elseif IFactory.IsMeasurementPoints(object)
                    obj = IFactory.ToMeasurementPoints(object);
                elseif IFactory.IsCells(object)
                    obj = IFactory.ToCells(object);
                elseif IFactory.IsFilaments(object)
                    obj = IFactory.ToFilaments(object);
                elseif IFactory.IsDataSet(object)
                    obj = IFactory.ToDataSet(object);
                elseif IFactory.IsFactory(object)
                    obj = IFactory.ToFactory(object);
                elseif IFactory.IsFrame(object)
                    obj = IFactory.ToFrame(object);
                elseif IFactory.IsImageProcessing(object)
                    obj = IFactory.ToImageProcessing(object);
                elseif IFactory.IsLightSource(object)
                    obj = IFactory.ToLightSource(object);
                elseif IFactory.IsSurpassCamera(object)
                    obj = IFactory.ToDSurpassCamera(object);
                elseif IFactory.IsVolume(object)
                    obj = IFactory.ToVolume(object);
                elseif IFactory.IsClippingPlane(object)
                    obj = IFactory.ToClippingPlane(object);
                elseif IFactory.IsApplication(object)
                    obj = IFactory.ToApplication(object);
                else
                    obj = object;
                end
            end
            
        end
        
        function type = GetImarisType(eXT, object)
            %% GETIMARISTYPE Returns a string with the type of Imaris Object
            % This is a convenience function to understand the subclasses
            % of IDataItem
            
            if eXT.ImarisApp.GetFactory.IsDataContainer(object)
                type = 'Group';
            elseif eXT.ImarisApp.GetFactory.IsSpots(object)
                type = 'Spots';
            elseif eXT.ImarisApp.GetFactory.IsSurfaces(object)
                type = 'Surfaces';
            elseif eXT.ImarisApp.GetFactory.IsMeasurementPoints(object)
                type = 'Points';
            elseif eXT.ImarisApp.GetFactory.IsCells(object)
                type = 'Cells';
            elseif eXT.ImarisApp.GetFactory.IsFilaments(object)
                type = 'Filaments';
            elseif eXT.ImarisApp.GetFactory.IsDataSet(object)
                type = 'DataSet';
            elseif eXT.ImarisApp.GetFactory.IsFactory(object)
                type = 'Factory';
            elseif eXT.ImarisApp.GetFactory.IsFrame(object)
                type = 'Frame';
            elseif eXT.ImarisApp.GetFactory.IsImageProcessing(object)
                type = 'Image Processing';
            elseif eXT.ImarisApp.GetFactory.IsLightSource(object)
                type = 'Light Source';
            elseif eXT.ImarisApp.GetFactory.IsSurpassCamera(object)
                type = 'Camera';
            elseif eXT.ImarisApp.GetFactory.IsVolume(object)
                type = 'Volume';
            elseif eXT.ImarisApp.GetFactory.IsClippingPlane(object)
                type = 'Clipping Plane';
            elseif eXT.ImarisApp.GetFactory.IsApplication(object)
                type = 'Application';
            else
                type = 'Unknown';
            end
            
            
            
        end
        
        
        function [xy, z] = GetVoxelSize(eXT, varargin)
            %% GETVOXELSIZE returns the xy and z voxel size of the current dataset
            % [xy, z] = GetVoxelSize('DataSet', aDataSet) returns the voxel size
            % of the dataset provided as an optional argument.
            % Otherwise, it returns the voxel size of the
            % ImarisApplication DataSet.
            
            
            vImarisDataSet = eXT.ImarisApp.GetDataSet;
            
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'DataSet'
                        vImarisDataSet =  varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            vExtendMin = [vImarisDataSet.GetExtendMinX, vImarisDataSet.GetExtendMinY, vImarisDataSet.GetExtendMinZ];
            vExtendMax = [vImarisDataSet.GetExtendMaxX, vImarisDataSet.GetExtendMaxY, vImarisDataSet.GetExtendMaxZ];
            vDataSize = [vImarisDataSet.GetSizeX, vImarisDataSet.GetSizeY, vImarisDataSet.GetSizeZ, vImarisDataSet.GetSizeC, vImarisDataSet.GetSizeT];
            %get current Voxel size
            aVoxelSize = (vExtendMax - vExtendMin) ./ vDataSize(1:3);
            
            % Return voxel size.
            xy = aVoxelSize(1);
            z = aVoxelSize(3);
        end
        
        function [datasize, extentMin, extentMax] = GetExtents(eXT, varargin)
            
            aDataSet = eXT.ImarisApp.GetDataSet;
            
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'DataSet'
                        aDataSet =  varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            extentMin = [aDataSet.GetExtendMinX, aDataSet.GetExtendMinY, aDataSet.GetExtendMinZ];
            extentMax = [aDataSet.GetExtendMaxX, aDataSet.GetExtendMaxY, aDataSet.GetExtendMaxZ];
            datasize  = [aDataSet.GetSizeX, aDataSet.GetSizeY, aDataSet.GetSizeZ];
        end
        
        function size = GetSize(eXT, size)
            vDataSet = eXT.ImarisApp.GetDataSet;
            
            switch size
                case 'X'
                    size = vDataSet.GetSizeX;
                case 'Y'
                    size = vDataSet.GetSizeY;
                case 'Z'
                    size = vDataSet.GetSizeZ;
                case 'C'
                    size = vDataSet.GetSizeC;
                case 'T'
                    size = vDataSet.GetSizeT;
                otherwise
                    disp ('Valid arguments are X, Y, Z, C, T');
            end
        end
        
        function SetSize(eXT, size, value)
            vDataSet = eXT.ImarisApp.GetDataSet;
            
            switch size
                case 'X'
                    vDataSet.SetSizeX(value);
                case 'Y'
                    vDataSet.SetSizeY(value);
                case 'Z'
                    vDataSet.SetSizeZ(value);
                case 'C'
                    vDataSet.SetSizeC(value);
                case 'T'
                    vDataSet.SetSizeT(value);
                otherwise
                    disp ('Valid arguments are X, Y, Z, C, T');
            end
        end
        
        function name = GetChannelName(eXT,channel)
            vDataSet = eXT.ImarisApp.GetDataSet;
            name = char(vDataSet.GetChannelName(channel-1));
        end
        
       function names = GetChannelNames(eXT)
            for i=1:eXT.GetSize('C')
                names{i} = eXT.GetChannelName(i);
            end
        end
        
        function type = GetDataType(eXT)
            switch char(eXT.ImarisApp.GetDataSet.GetType)
                case 'eTypeUInt8'
                    type='8-bit';
                case 'eTypeUInt16'
                    type='16-bit';
                case 'eTypeFloat'
                    type='32-bit';
                otherwise
                    type='unknown';
            end
        end
        
        function SetChannelName(eXT, channel, name)
            if eXT.GetSize('C') >= channel
                eXT.ImarisApp.GetDataSet.SetChannelName(channel-1, name);
            else
                fprintf('Channel %d does not exist. Dataset has %d channels', channel, eXT.GetSize('C'))
            end
        end
        function color = GetChannelColor(eXT, number)
            color = eXT.ImarisApp.GetDataSet.GetChannelColorRGBA(number-1);
            
        end
        function SetDataType(eXT, type)
            switch type
                case '32-bit'
                    aType = Imaris.tType.eTypeFloat;
                case '16-bit'
                    aType = Imaris.tType.eTypeUInt16;
                case '8-bit'
                    aType = Imaris.tType.eTypeUInt8;
                otherwise
                    aType = eXT.ImarisApp.GetDataSet.GetType;
            end
            vDataSet = eXT.ImarisApp.GetDataSet;
            vDataSet.SetType(aType);
            eXT.ImarisApp.SetDataSet(vDataSet);
            % Seems we have to do it in two steps, otherwise it is not
            % recorded...
            % Normally I would have used:
            % eXT.ImarisApp.GetDataSet.SetType(aType);
        end  
    end
    
    %% Colocalization chunk
    
    methods
        function [pearsons, manders, ica, fluorogram] = GetColoc(eXT, ch1, ch2, varargin)
            vDataSet = eXT.ImarisApp.GetDataSet;
            thr1 = 0;
            thr2 = 0;
            nT = eXT.GetSize('T');
            T = 1:nT;
            addChannel = false;
            doHist = true;
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Thr1'
                        thr1 = varargin{i+1};
                    case 'Thr2'
                        thr2 = varargin{i+1};
                    case 'Show Histogram'
                        doHist = varargin{i+1};
                    case 'T'
                        T = varargin{i+1};                    
                    case 'Add Channel'
                        addChannel = varargin{i+1};
                        
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            % Channel Names
            ch1Name = char(vDataSet.GetChannelName(ch1-1));
            ch2Name = char(vDataSet.GetChannelName(ch2-1));
            
            if addChannel
                vDataSet.SetSizeC(eXT.GetSize('C')+1);
               
                colocC = eXT.GetSize('C')-1;
                dType = eXT.GetDataType();
                cName = sprintf('Coloc Volume %s vs. %s, Thr1 %d, Thr2 %d', ch1Name, ch2Name, thr1, thr2);
                vDataSet.SetChannelName (colocC, cName);


            end
            
            for t=T
                % Get the channels, slightly blur them first
                ch1V = vDataSet.GetDataVolumeFloats(ch1-1, t-1);
                ch2V = vDataSet.GetDataVolumeFloats(ch2-1, t-1);
                
                % Blurring
                %ch1V = imfilter(ch1V, fspecial('gaussian', [5,5], 2.0));
                %ch2V = imfilter(ch2V, fspecial('gaussian', [5,5], 2.0));
                
                ch1 = reshape(ch1V,[],1);
                ch2 = reshape(ch2V,[],1);
                % ch1 = vDataSet.GetDataVolumeAs1DArrayFloats (ch1-1, t-1);
                % ch2 = vDataSet.GetDataVolumeAs1DArrayFloats (ch2-1, t-1);
                
                % Stats TODO =============================================
                sum(ch1(ch1>thr1))
                sum(ch2(ch2>thr2))
                %sum(ch1(ch1>thr1) & ch2(ch2>thr2))
                colocI = ch1>=thr1 & ch2>=thr2;
                
                meanCh1 = mean(ch1)
                meanCh1Thr = mean(ch1(colocI))
                
                meanCh2 = mean(ch2)
                meanCh2Thr = mean(ch2(colocI))
                
                r= sum( (ch1-meanCh1) .* (ch2-meanCh2) ) 
                r2 = sqrt(sum( (ch1-meanCh1).^2)) .* sqrt(sum( (ch2-meanCh2).^2 ))
                
                
                
                rf = r/r2
                r = corr(ch1, ch2)
                
   
                r= sum( (ch1(colocI)-meanCh1Thr) .* (ch2(colocI)-meanCh2Thr) ) 
                r2 = sqrt(sum( (ch1(colocI)-meanCh1Thr).^2)) .* sqrt(sum( (ch2(colocI)-meanCh2Thr).^2 ))
                
                rf = r/r2
                
                r1= corr(ch1(colocI), ch2(colocI))
                % Stats TODO =============================================

                                
                % Display a histogram, if they want.
                if doHist
                    maxVal1 = max(ch1);
                    maxVal2 = max(ch2);
                    maxVal = max(maxVal2, maxVal1);
                    
                    % Blurring
                    
                    ch1V = smooth3(ch1V ./ maxVal .* 255, 'gaussian', 5, 1.0);
                    ch2V = smooth3(ch2V ./ maxVal .* 255, 'gaussian', 5, 1.0);

                    ch1 = reshape(ch1V,[],1);
                    ch2 = reshape(ch2V,[],1);
                    
                    
                    % Format input for hist3
                    dat = ([ch1, ch2]);
                    
                    ctrs = {0:256+0.5 , 0:256+0.5};
                    % This seems to cause a problem where 
                    
                    n = hist3(dat, ctrs);
                    n1 = log10(n'+1);
                    n1(size(n,1) + 1, size(n,2) + 1) = 0;
                    xb = linspace(0,maxVal,size(n,1)+1);
                    yb = linspace(0,maxVal,size(n,1)+1);
                    h = pcolor(xb,yb,n1);

                    cm = colormap(jet);
                    cm(1,:) = [0,0,0];
                    colormap(cm);
                    set(h, 'EdgeColor', 'none');
                    [cmin cmax] = caxis;
                    caxis([0,cmax*0.75]);
                    cb = colorbar;
                    % Tick mark positions
                    L = [0 1 10 100 1000 10000 100000, 1000000];
                    l = log10(L+1); 
                    set(cb,'YTick',l, 'YTicklabel',L);
                    cb.Label.String = 'Pixel Counts';
                    title(sprintf('Fluorogram %s vs. %s', ch1Name, ch2Name));
                    xlabel(ch1Name);
                    ylabel(ch2Name);
                    
                    % Show thresholds
                    thr1Rect = thr1 / maxVal *255;
                    thr2Rect = thr2 / maxVal *255;
                    
                    rectangle('Position',[thr1Rect thr2Rect maxVal-thr1Rect maxVal-thr2Rect], 'LineWidth',2, 'EdgeColor','y');

                end
                

                if addChannel
                    % Build the coloc channel
                    colocVol = (ch1+ch2) ./ 2;
                    colocVol(~colocI) = 0;
                    
                    switch dType
                        case '8-bit'
                            vDataSet.SetDataVolumeBytes(uint8(reshape(colocVol,size(ch1V))), colocC, t-1);
                        case '16-bit'
                            vDataSet.SetDataVolumeShorts(uint16(reshape(colocVol,size(ch1V))), colocC, t-1);             
                        case '32-bit'
                            vDataSet.SetDataVolumeFloats(float(reshape(colocVol,size(ch1V))), colocC, t-1);             
                    end
                end
                
                
                
            end
            

        end
        
    
    end
    
    
    %% Statistics Related Methods
    methods
        function AddStatistic(~, object, name, values, varargin) 
            units = '';

            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Units'
                        units = varargin{i+1};
                    case 'Time'
                        time = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            vAllStatistics = object.GetStatistics;
            ids= vAllStatistics.mIds;
            nEl = numel(unique(ids(ids>=0)));
            unique(ids(ids>=0))
            
            newStatFactorNames = {'Category'};
            newStatFactors(1:nEl) = {'Custom'};
            
            
            %Add new statistic
            try
                newStatNames   = cell(nEl, 1);
                newStatValues  = 1:nEl;
                newStatUnits   = cell(nEl, 1);
                newStatIds     = 1:nEl;
                for j = 1:nEl
                    newStatNames{j}      = name;
                    newStatValues(j)     = values(j);
                    newStatUnits{j}      = units;
                    newStatIds(j)        = ids(j);
                end
                object.AddStatistics(newStatNames, newStatValues, ...
                                     newStatUnits, newStatFactors, ...
                                     newStatFactorNames, newStatIds);
            catch er
                error('Error in adding statistic');
            end
            
        end
        
        function [stats] = GetSelectedStatistics(eXT, object, selectedStatistic, varargin) %#ok<INUSL>
            %% GETSELECTEDSTATISTICS returns the selected stats
            % stats = GETSELECTEDSTATISTICS(object, selectedStat, ...
            %                               'Channel', channel, ...
            %                               'Time', time)
            % serves as a filter by keeping only the object ids,
            % values, and factors from the statistic with name
            % selectedStat. SelectedStat is a string that matches the
            % name of a statistic like 'Intensity Center', 'Volume',
            % etc...
            % Optional values are
            %   o Channel - When applicable, only keep the stats from
            %   the selected channel
            %   o Time - When applicable, A particular timepoint
            
            time = [];
            channel = [];
            for i=1:2:length(varargin)
                switch varargin{i}
                    case 'Channel'
                        channel = varargin{i+1};
                    case 'Time'
                        time = varargin{i+1};
                    otherwise
                        error(['Unrecognized Command:' varargin{i}]);
                end
            end
            
            % get all the statistics of the selected Object
            % here you can't use vSpots, you have to use vObject!!!
            allStats = GetAllStatistics(eXT, object);
            stats = allStats;
            
            
            
            expr = 'find(strcmp(allStats.names, selectedStatistic)';

            % Get info on the channel
            if ~isempty(channel) 
                chInd = find(strcmpi('Channel', allStats.factorNames));
                
                % Make channel list
                channelsCell = cellfun(@str2num, allStats.factors(:,chInd),  'UniformOutput', false); %#ok<FNDSB>
                emptyIndexes = cellfun(@isempty,channelsCell); 
                channelsCell(emptyIndexes) = {-1}; % Make empty channels -1, so as to keep them.
                channelList = cell2mat(channelsCell);
                
                expr = [expr ' & (channelList == channel)'];                
            end
            
            if ~isempty(time)
                tInd = find(strcmpi('Time', allStats.factorNames));
                % Make time list
                timesCell = cellfun(@str2num, allStats.factors(:,tInd),  'UniformOutput', false); %#ok<FNDSB>
                emptyIndexes = cellfun(@isempty,timesCell); 
                timesCell(emptyIndexes) = {-1}; % Make empty times -1, so as to keep them.
                timeList = cell2mat(timesCell);
                
                expr = [expr ' & (timeList == time)'];          

            end
                        
            % Return results
            expr = [expr ');'];
            
            
            
            selectedStatIdx = eval(expr);

            stats.names         =   allStats.names(selectedStatIdx);
            stats.ids           =   allStats.ids(selectedStatIdx);
            stats.units         =   allStats.units(selectedStatIdx);
            stats.values        =   allStats.values(selectedStatIdx);
            stats.factors       =   allStats.factors(selectedStatIdx,:);
            stats.factorNames   =   allStats.factorNames;
            stats.indexes       =   selectedStatIdx;
            
            
            
        end
        
        function [allStatistics] = GetAllStatistics (eXT, object)
            %% GETALLSTATISTICS returns a struct with all statistics
            % [allStatistics] = GetAllStatistics (object) returns a
            % stat structure with the following fields
            % .names: The name of the statistic
            vAllStatistics = object.GetStatistics;
            
            
            % from this vAllStatistics you can extract
            % Names
            allStatistics.names       = cell(vAllStatistics.mNames);
            % Units
            allStatistics.units       = cell(vAllStatistics.mUnits);
            % Factors
            allStatistics.factors     = cell(vAllStatistics.mFactors)';
            % Factor Names
            allStatistics.factorNames = cellstr(char(vAllStatistics.mFactorNames));
            
            % Values, do not need cell(...), cause it's number;
            allStatistics.values      = vAllStatistics.mValues;
            
            % IDs, do not need cell(...), cause it's number;
            allStatistics.ids         = vAllStatistics.mIds;
            
            % There were the non-specific stats, now we need to get the
            % more specific ones, and these depend on the object.
            switch GetImarisType(eXT, object)
                case 'Spots'
                    allStatistics.tIndices = object.GetIndicesT;
                    allStatistics.posXYZ = object.GetPositionsXYZ;
                    allStatistics.radii = object.GetRadiiXYZ;
                case 'Surfaces'
                    for i=0:(object.GetNumberOfSurfaces-1)
                        allStatistics.posXYZ(i+1,:)  = object.GetCenterOfMass(i);
                        allStatistics.tIndices(i+1) = object.GetTimeIndex(i);
                    end
                    
                case 'Points'
                    allStatistics.posXYZ = object.GetPositionsXYZ;
                    allStatistics.tIndices = object.GetIndicesT;
                    allStatistics.pNames = cellstr(char(object.GetNames));
            end
            
        end
        
        function booleanIsImage = isImage(~, fileName)
            %% isImage takes the fileName as argument and returns
            %  1 if the file name ends with an authorized file format
            %  from the list {'czi' 'tif' 'ids' 'lsm'}, or 0.
            extFileImage = {'czi' 'tif' 'ics' 'lsm'};
            
            % create the Regex that check fileName
            extExpression = strcat('\w*\.',extFileImage,'$');
            
            % check if the fileName ends with any of the listed files format
            booleanIsImage = any(cell2mat(regexp(fileName,extExpression))) > 0 ;
            if  booleanIsImage
                fprintf('%s is an image file\n',fileName)
            end
        end
        
    end
end

function aData = DrawSphere(aData, aPos, aRad, aMin, aMax, aType, aInterpolate)
    vSize = [size(aData, 1), size(aData, 2), size(aData, 3)];
    aPos = (aPos - aMin) ./ (aMax - aMin) .* vSize + 0.5;
    aRad = aRad ./ (aMax - aMin) .* vSize;
    vPosMin = round(max(aPos - aRad, 1));
    vPosMax = round(min(aPos + aRad, vSize));
    vPosX = vPosMin(1):vPosMax(1);
    vPosY = vPosMin(2):vPosMax(2);
    vPosZ = vPosMin(3):vPosMax(3);
    [vX, vY, vZ] = ndgrid(vPosX, vPosY, vPosZ);
    vDist = ((vX - aPos(1))/aRad(1)).^2 + ((vY - aPos(2))/aRad(2)).^2 + ...
        ((vZ - aPos(3))/aRad(3)).^2;
    vInside = vDist < 1;
    vCube = aData(vPosX, vPosY, vPosZ);
    if aInterpolate
        vCube(vInside) = max(vCube(vInside), FixType(255*(1-vDist(vInside)), aType));
    else
        vCube(vInside) = 255;
    end
    aData(vPosX, vPosY, vPosZ) = vCube;
end

function aData = FixType(aData, aType)
    if strcmp(aType, 'eTypeUInt8')
        aData = uint8(aData);
    elseif strcmp(aType, 'eTypeUInt16')
        aData = uint16(aData);
    else
        aData = single(aData);
    end

end

function [aData, aSize, aMin, aMax, aType] = GetDataSetData(aDataSet)
    aMin = [aDataSet.GetExtendMinX, aDataSet.GetExtendMinY, aDataSet.GetExtendMinZ];
    aMax = [aDataSet.GetExtendMaxX, aDataSet.GetExtendMaxY, aDataSet.GetExtendMaxZ];
    aSize = [aDataSet.GetSizeX, aDataSet.GetSizeY, aDataSet.GetSizeZ];
    if strcmp(aDataSet.GetType, 'eTypeUInt8')
        aData = zeros(aSize, 'int8');
    elseif strcmp(aDataSet.GetType, 'eTypeUInt16')
        aData = zeros(aSize, 'int16');
    else
        aData = zeros(aSize, 'single');
    end
    aType = aDataSet.GetType;
end

function libPath = getSavedImarisPath()
    confFile = fopen('config.txt','r');
    if confFile==-1 
        libPath = '';
    else
        rawPath = fgetl(confFile);
        libPath = rawPath(13:end);
        fclose(confFile);
    end
end

function setSavedImarisPath(imPath)
    confFile = fopen('config.txt','w');
    fprintf (confFile, 'ImarisPath: %s\n', imPath);
    fclose(confFile);
end

%% Coloc Helpers
function C = getColocCoeffs(eXT, I1, I2, ch1, ch2, thr1, thr2, nT, mask, maskCh, maskThr, isZIndependent )
    
    sX = eXT.GetSize('X');
    sY = eXT.GetSize('Y');
    sZ = 1;
    

    if isZIndependent
        sZ = numel(I1) ./ (sX.*sY);
    end
    
    for z = 1:sZ
        zStart = sX .* sY .* (z-1) + 1;
        if sZ == 1
            zEnd = numel(I1);
        else
            zEnd = sX .* sY .* z;
        end
        I1s = double(I1(zStart:zEnd));
        I2s = double(I2(zStart:zEnd));
        
        if ~isempty(mask)
            masks = double(mask(zStart:zEnd));
            
        else
            masks = ones(zEnd - zStart + 1, 1);
        end
        
        idx  = find(I1s>=thr1 & I2s>=thr2 & masks>0);
        idx1 = find(I1s>=thr1 & masks>0);
        idx2 = find(I2s>=thr2 & masks>0);
        
        C.Channel1(z,1) = ch1;
        C.Channel2(z,1) = ch2;
        C.Threshold1(z,1) = thr1;
        C.Threshold2(z,1) = thr2;
        if ~isempty(maskCh)
            C.MaskChannel(z,1) = maskCh;
            C.MaskThreshold(z,1) = maskThr;
        end
        
        if isZIndependent
            C.Z(z,1) = z;
        end
        
        C.T(z,1) = nT;       
        
        C.nVoxels(z,1) = size(I1s,1);
        C.nVoxelsColoc(z,1) = size(idx,1);

        C.percentColoc(z,1) = C.nVoxelsColoc(z,1) ./ size (I1s,1) *100;
        if size(idx,1) > 1
            Rp = corr(I1s(idx), I2s(idx));
            Rs = corr(I1s(idx), I2s(idx),'type', 'Spearman');
        else
            Rp = NaN;
            Rs = NaN;
        end

        C.Pearsons(z,1) = Rp;
        C.Spearman(z,1) = Rs;


        C.M1(z,1) = sum(I1s(I2s>0)) ./ sum(I1s);
        C.M2(z,1) = sum(I2s(I1s>0)) ./ sum(I2s);

        C.M1t(z,1) = sum(I1s(idx2)) ./ sum(I1s);
        C.M2t(z,1) = sum(I2s(idx1)) ./ sum(I2s);

        C.MOC(z,1) = sum( I1s .* I2s ) ./ sqrt(sum(I1s.^2)*sum(I2s.^2));
        C.k1(z,1)  = sum( I1s .* I2s ) ./ sum(I1s.^2);
        C.k2(z,1)  = sum( I1s .* I2s ) ./ sum(I2s.^2);

        % Li ICQ, graphs too?
        EI1 = mean(I1s);
        EI2 = mean(I2s);

        EI1t = mean(I1s(idx));
        EI2t = mean(I2s(idx));

        nPxInt = sum( ( (I1s - EI1).*(I2s - EI2) ) > 0 );
        nPxTot = sum( ( (I1s - EI1).*(I2s - EI2) ) ~= 0 );

        nPxIntt = sum( ( (I1s(idx) - EI1t).*(I2s(idx) - EI2t) ) > 0 );
        nPxTott = sum( ( (I1s(idx) - EI1t).*(I2s(idx) - EI2t) ) ~= 0 );

        C.ICQ(z,1)  = nPxInt ./ nPxTot - 0.5;
        C.ICQt(z,1) = nPxIntt ./ nPxTott - 0.5;

        % Not Based on Intensities
        C.Mo1(z,1) = size (idx,1) ./ size(idx1,1);
        C.Mo2(z,1) = size (idx,1) ./ size(idx2,1);
    end
    
end


function F = getFluorogram(I1,I2, ch1, ch2, thresholds, isShow)
    %Prepare for hist3
    X = [I1 I2];
    M = max(X);
    m = double(max(M));
    if (m <= 255)
        m = 255;
    end
    
    centers{1} = linspace(0,m,256);
    centers{2} = linspace(0,m,256);
    
    N = hist3(X,centers);

    
    
    if isShow
        figure, imagesc(log(N')+1);
        axis image;
        axis xy;
        xlim([0,m]);
        ylim([0,m]);
        xlabel(sprintf('Channel %d',ch1));
        xlabel(sprintf('Channel %d',ch2));
        title(sprintf('Fluorogram Ch %d vs. Ch %d', ch1, ch2));
        
        cmap = colormap(jet);
        cmap(1,:) = [0 0 0];
        colormap(cmap);
        colorbar;
        % Show Thresholds
        hold on;
        t = thresholds./m.*256;

        line([0,255],[t(2),t(2)],'Color','w','LineWidth',2);
        line([t(1),t(1)],[0,255],'Color','w','LineWidth',2);
        set(gca, 'CLim', [0, 0.75*max(max(log(N)))]);
    end
    

end

function dataVolume1D = getColocChannel(I1, I2, thresholds)
    tf = (I1>=(thresholds(1)) & I2>=(thresholds(2)));
    dataVolume1D = sqrt(double(I1).^2 + double(I2).^2);
    if isa(I1, 'uint8')
        dataVolume1D = uint8(dataVolume1D);
    else
        if isa(I1, 'uint16')
            dataVolume1D = uint16(dataVolume1D);

        end
    end
        
    dataVolume1D(~tf) = 0;

    
end

