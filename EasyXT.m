classdef EasyXT
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
    %     GNU General Public License for more details.
    % 
    %     You should have received a copy of the GNU General Public License
    %     along with this program.  If not, see <http://www.gnu.org/licenses/>.
    
    properties
        % The class contains the ImarisApplication instance as a property
        % so it can be accessed by all functions.
        ImarisApp;
        
        % We can define here the path where ImarisLib.jar is
        imarisLibPath = 'C:\Program Files\Bitplane\Imaris x64 7.6.5\XT\matlab\ImarisLib.jar';
        
    end
    
    
    methods
        function eXT = EasyXT(varargin)
            %% EASYXT builds a new EasyXT object
            % eXT = EASYXT(imarisApplication)
            % builds a new EasyXT object and connects to an open Imaris
            % Instance.
            % Optional input argument: imarisApplicationID
            % That way you can start easyXT from an Xtension, like you
            % normally would.
            
            % Supress Java Duplicate Class warnings
            warning('off','MATLAB:Java:DuplicateClass');
            
            lib = getSavedImarisLib();
            if nargin == 1 && (strcmp(varargin{1}, 'setup') || strcmp(lib, ''))
                 [FileName,PathName] = uigetfile('.jar','Location of ImarisLib.jar');
                 eXT.imarisLibPath = [PathName, FileName];
                 setSavedImarisLib(eXT.imarisLibPath);
            end
            % Start Imaris Connection
            javaaddpath(eXT.imarisLibPath);
            
            % Create an instance of ImarisLib
            vImarisLib = ImarisLib;
            
            warning('on','MATLAB:Java:DuplicateClass');

            
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
            end
            
            try
                eXT.ImarisApp = vImarisLib.GetApplication(ID);
            catch err
                disp('Could not get Imaris Application with ID');
                disp('Original error below:');
                rethrow(err);
                
            end
        end
        
    end
    
    %% Public methods.
    methods
        
        function ImarisObject = GetObject(eXT, varargin)
            %% GETOBJECT recovers an object from the Surpass scene
            % Imarisobject = GETOBJECT('Name', name, ...
            %                                  'Parent', parent, ...
            %                                   'Number', number, ...
            %                                   'Type', type, ...
            %                                   'Cast', isCast, ...
            %                                   'Active Selection', isSelection)
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
            %   Object.
            %       Defaults to [ true ]
            %
            % Examples:
            %  >> ImarisObj = GetObject(); % Returns the active object
            %  in the imaris scene
            %  >> ImarisObj = GetObject('Type', 'Surfaces'); % Returns
            %  the first surface object
            %  >> ImarisObj = GetObject('Number', 4); % Returns
            %  fourth object in the scene.
            %  >> ImarisObj = GetObject('Type', 'Surfaces', 'Name', 'My Surfaces');
            %  Returns the first surface object with name 'My Surfaces'.
            %
            % See also CREATEGROUP
            
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
            
            object = [];
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
            % All arguments from GETSELECTEDOBJECT can be applied here
            % See also GETSELECTEDOBJECT
            
            obj = GetObject(eXT, varargin{:});
            eXT.ImarisApp.SetSurpassSelection(obj);
            
        end
        
        function number = GetNumberOf(eXT, type, varargin)
            %% GETNUMBEROF recovers the number of objects with a certain name or type
            % number = GETNUMBEROF('Name', name, ...
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
            if any(strcmp(methods(object), 'GetName'))
                name = char(object.GetName);
            end
            
        end
        
        function name = SetName(eXT, object, name)
            if any(strcmp(methods(object), 'SetName'))
                object.SetName(name);
            end
            
        end
        
        function SetColor(eXT, object, colors)
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
        
        function spots = CreateSpots(eXT, PosXYZ, spotSizes, varargin)
            %% CREATESPOTS creates and returns a new spot object
            % spots = CREATESPOTS(posXYZ, spotSizes, ...,
            %                     'Name', name, ...
            %                     'Single Timepoint', isSingle, ...
            %                     'Time Indexes', t, ...
            % creates a new spots object with the given xyz positions and
            % sizes. posXYZ is an nx3 array where each row contains the X,
            % Y and Z position of the spots, int the unit of the imaris
            % dataset (Usually microns).
            % spotSizes is either an nx1 or nx3 array containing the radius
            % of each spot, making the spot either spherical or defined by
            % its X Y and Z semi-major axes lenghts. Must be the same size
            % as posXYZ.
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
                error('XYZ must be nx3 in size...');
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
            %   o Parent - The parent object to remove this object from.
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
            [aData, aSize, aMin, aMax, aType] = GetDataSetData(vDataSet);
            
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
            vDataSet = []
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
            
            [aData, aSize, aMin, aMax, vType] = GetDataSetData(vDataSet);
            
            
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
            %   on the Local Contrast-filtered image.
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
            th = 0;
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
                        if th ~= 0;
                            isAutoThr = false;
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
                [voxelXY, voxelZ] = eXT.getVoxelSize();
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
        
        function spots = DetectSpots(eXT, channel, varargin)
            %%DETECTSPOTS Applies the Imaris Spots Detection Functions
            % spot = DETECTSPOTS(channel, 'Name', surfaceName, ...
            %                                  'Color', color, ...
            %                                  'Diameter XY', dXY, ...
            %                                  'Diameter Z', dZ, ...
            %                                  'Subtract BG, isSubtractBG, ...
            %                                  'Local Contrast', isLocContrast, ...
            %                                  'Spots Filter', filterString, ...
            %                                  'Seed Diameter', seedD, ...
            %                                  'Seed Local Contrast', isSeedLocC, ...
            %                                  'Seed Filter', seedFilterString, ...
            %                                  'DataSet', aDataSet, ...
            % Detects a spots from channel channel with optinal
            % parameters:
            %   o Name - The name of the new spots object. Defaults to
            %   Imaris standard name.
            %   o Color - a single value or, 1x3 or 1x4 arrays with the
            %   color in RGBA format.
            %       colors(1) is Red (or gray if color is 1x1)
            %       colors(2) is Green
            %       colors(3) is Blue
            %       colors(4) is transparency (values [0-128])
            %   Defaults to white [255, 255, 255].
            %   o Diameter XY - the diameter of the spots, usually in
            %   microns, for the spot renderind and detection if using
            %   local contrast. If Diameter Z is not defined, these will be
            %   spherical spots. Defaults to 2x the voxel size in XY
            %   o Diameter Z - the diameter of the spots in Z, if you want
            %   to detect ellipsoids. Defaults to 2x the voxel size in Z.
            %   o Subtract BG - Should the spots be detected using a BG
            %   subtraction or from the raw intensities. Defaults to true
            %   o Region Growing - Logical defining whether you wish to
            %   apply region growing to the spots. Defaults to false.
            %   o Region Growing Local Contrast - Logical defining whether
            %   to use local contrast or absolute intensity for region
            %   growing. Defaults to true.
            %   o Region Threshold - Defines the threshold value to use for
            %   the Region Growing step. Defaults to 'Auto'.
            %   o Diameter From Volume - Logical defining whether the
            %   Region Growing dpot diameters are computed from the volume
            %   of the thresholded regions. Otherwise, the diameter is
            %   calculated from the distance to the region borders.
            %   o Spots Filter - A string containing the filter you want
            %   to apply to your spots. Example: '"Quality" above 7.000'
            %   Defaults to '"Quality" above automatic threshold'
            %   o DataSet - Provide the dataset to detect the surfaces in
            %   explicitly. This is useful if you want to update a dataset
            %   with multiple calls to this function, so that you don't
            %   need to update the Scene every time.
            %
            
            
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
                    case 'Region Local Contrast'
                        isRegLocContrast = varargin{i+1};
                        isRegionGrow = true;
                    case 'Region Threshold'
                        regionThr= varargin{i+1};
                        if ~strcmp(regionThr, 'Auto')
                            isRegAutoThr = false;
                        else
                            regionThr =0;
                        end
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
                    celldisp({ channel-1, [dxy dxy dz], isSubtractBG, filter, isRegLocContrast, isRegAutoThr, regionThr, isDiamFromVol, false});
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
        
        function folderRef = CreateGroup(eXT, name)
            % Create folder
            folderRef = eXT.ImarisApp.GetFactory.CreateDataContainer;
            folderRef.SetName(name);
        end
        
        function RunXtension(eXT, name)
            addpath('C:\Program Files\Bitplane\Imaris x64 7.6.5\XT\matlab');
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
        
        function [pathstr,name,ext] = GetCurrentFileName(eXT) 
            %% GetCurrentFileName returns the surrent file name path and extension
            % [pathstr,name,ext] = GetCurrentFileName() has no arguments and returns the same variables
            % as the fileparts function 
            % see also FILEPARTS
            fullPath = char(eXT.ImarisApp.GetCurrentFileName());
            [pathstr,name,ext] = fileparts(fullPath);
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
            
            % Sizes should be the same
            size(times,2)
            size(XYZ,1)
            size(pointNames,1)
            
            
            if ~isempty(name)
                newPoints.SetName(name);
            end
            
            % Create the points
            newPoints.Set(XYZ,times, pointNames);
            
        end
    end
    %% Helper and Type converting functions
    methods
        
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
        
        function names = GetChannelNames(eXT)
            vDataSet = eXT.ImarisApp.GetDataSet;
            for i=1:vDataSet.GetSizeC
                names{i} = char(vDataSet.GetChannelName(i-1));
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
    

    
    %% Methods in need of comments
    methods
        
        
        function [stats] = GetSelectedStatistics(eXT, object, selectedStatistic, varargin)
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
                channelsCell = cellfun(@str2num, allStats.factors(:,chInd),  'UniformOutput', false);
                emptyIndexes = cellfun(@isempty,channelsCell); 
                channelsCell(emptyIndexes) = {-1}; % Make empty channels -1, so as to keep them.
                channelList = cell2mat(channelsCell);
                
                expr = [expr ' & (channelList == channel)'];                
            end
            
            if ~isempty(time)
                tInd = find(strcmpi('Time', allStats.factorNames));
                % Make time list
                timesCell = cellfun(@str2num, allStats.factors(:,tInd),  'UniformOutput', false);
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

function [aDataSet, aData, aMin, aMax, aType] = InitNewChannel(aImaris)
    aDataSet = aImaris.GetDataSet.Clone;
    aMin = [aDataSet.GetExtendMinX, aDataSet.GetExtendMinY, aDataSet.GetExtendMinZ];
    aMax = [aDataSet.GetExtendMaxX, aDataSet.GetExtendMaxY, aDataSet.GetExtendMaxZ];
    vSize = [aDataSet.GetSizeX, aDataSet.GetSizeY, aDataSet.GetSizeZ];
    if strcmp(aDataSet.GetType, 'eTypeUInt8')
        aData = zeros(vSize, 'uint8');
    elseif strcmp(aDataSet.GetType, 'eTypeUInt16')
        aData = zeros(vSize, 'uint16');
    else
        aData = zeros(vSize, 'single');
    end
    aDataSet.SetSizeC(aDataSet.GetSizeC + 1);
    aType = aDataSet.GetType;
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

function libPath = getSavedImarisLib()
    confFile = fopen('config.txt','r');
    if confFile==-1 
        libPath = '';
    else
        libPath = fscanf(confFile, 'ImarisLib: %s\n');
        fclose(confFile);
    end
end

function setSavedImarisLib(libPath)
    confFile = fopen('config.txt','w');
    fprintf (confFile, 'ImarisLib: %s\n', libPath);
    fclose(confFile);
end



