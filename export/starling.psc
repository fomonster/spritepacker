program starlingexporter;

var 
  i:integer;
  s:ansistring;

{******************************************************************
    Export Functions. Calls after main befin end.
******************************************************************}
function getFileExt():ansistring;
begin
  result:='.xml';
end;

function getResult():ansistring;
begin
  result:=s;
end;

{******************************************************************
  Enabled functions.

  // Global variables
  Sender.AddRegisteredVariable('imagesCount','integer');
  Sender.AddRegisteredVariable('currentPage','integer');
  Sender.AddRegisteredVariable('outputImageWidth','integer');
  Sender.AddRegisteredVariable('outputImageHeight','integer');
  Sender.AddRegisteredVariable('outputImageFileName','string');
  Sender.AddRegisteredVariable('outputImageFileFormat','string');
  Sender.AddRegisteredVariable('outputDateFileName','string');
  Sender.AddRegisteredVariable('spritesOptimization','integer'); 
  Sender.AddRegisteredVariable('namesWithPath','integer'); 

  // Save data to file
  Sender.AddFunction(@psClear,'procedure Clear;');
  Sender.AddFunction(@psSetPosition,'procedure setPosition(position: integer);');
  Sender.AddFunction(@psWriteByte,'procedure writeByte(const value: Byte);');
  Sender.AddFunction(@psWriteWord,'procedure writeWord(const value: Word);');
  Sender.AddFunction(@psWriteInt,'procedure writeInt(const value: integer);');
  Sender.AddFunction(@psWriteCardinal,'procedure writeCardinal(const value: integer);');
  Sender.AddFunction(@psWriteFloat,'procedure writeFloat(const value: Single);');
  Sender.AddFunction(@psWriteDouble,'procedure writeDouble(const value: double);');
  Sender.AddFunction(@psWriteString,'procedure writeString(const value: ansistring);');

  // Sprite image properties 
  Sender.AddFunction(@psImageType, 'function imageType(const index: integer):integer;');
  Sender.AddFunction(@psImageWidth, 'function imageWidth(const index: integer):integer;');
  Sender.AddFunction(@psImageHeight,'function imageHeight(const index: integer):integer;');
  Sender.AddFunction(@psImageX,'function imageX(const index: integer):integer;');
  Sender.AddFunction(@psImageY,'function imageY(const index: integer):integer;');
  Sender.AddFunction(@psImageTrimMinX,'function imageTrimMinX(const index: integer):integer;');
  Sender.AddFunction(@psImageTrimMinY,'function imageTrimMinY(const index: integer):integer;');
  Sender.AddFunction(@psImageTrimMaxX,'function imageTrimMaxX(const index: integer):integer;');
  Sender.AddFunction(@psImageTrimMaxY,'function imageTrimMaxY(const index: integer):integer;');
  Sender.AddFunction(@psImageTurn,'function imageTurn(const index: integer):boolean;');
  Sender.AddFunction(@psImagePage,'function imagePage(const index: integer):integer;');
  Sender.AddFunction(@psImagePath,'function imagePath(const index: integer):string;');
  Sender.AddFunction(@psImageName,'function imageName(const index: integer):string;');
  Sender.AddFunction(@psImageFullName,'function imageFullName(const index: integer):string;');
	
  // Tool functions
  Sender.AddFunction(@showmessage,'procedure showmessage(const s: string);');
  Sender.AddFunction(@ExtractFilePath,'function ExtractFilePath(const FileName: string): string;');
  Sender.AddFunction(@ExtractFileExt,'function ExtractFileExt(const FileName: string): string;');
  Sender.AddFunction(@ExtractFileNameWithoutExt,'function ExtractFileNameWithoutExt(const FileName: string): string;');
  Sender.AddFunction(@ExtractFileName,'function ExtractFileName(const FileName: string): string;');

  // 

******************************************************************}

  var 
    trimMinX:integer;
    trimMinY:integer;
    trimMaxX:integer;
    trimMaxY:integer;
    trimWidth:integer;
    trimHeight:integer;
    
    name:ansistring;
begin
  s := 'Generating Starling xml';

  Clear;
 
  writeString('<?xml version="1.0" encoding="UTF-8"?>'+#13);
  writeString('<TextureAtlas imagePath="'+ExtractFileName(outputImageFileName)+'.'+outputImageFileFormat+'" width="'+intToStr(outputImageWidth)+'" height="'+inttostr(outputImageHeight)+'">'+#13);

  for i:=0 to imagesCount-1 do begin
    if ( imagePage(i) = currentPage ) and ( imageType(i) = 1 ) then begin

      
      if ( namesWithPath > 0 ) then begin        
        name:=imageFullName(i); 
      end else begin
        name:=imageName(i);
      end;
      if ( namesWithExt > 0 ) then begin        
        {name:=name;}
      end else begin
        name:=ExtractFileNameWithoutExt(name);
      end;
      
      trimMinX := imageTrimMinX(i);		
      trimMinY := imageTrimMinY(i);		
      trimMaxX := imageTrimMaxX(i);		
      trimMaxY := imageTrimMaxY(i);		
      trimWidth := trimMaxX-trimMinX+1;	
      trimHeight := trimMaxY-trimMinY+1;	
	
      if imageTurn(i) then begin
	      if (( trimMinX = 0) and ( trimMinY = 0) and ( trimWidth = imageWidth(i) ) and ( trimHeight = imageHeight(i))) or ( spritesOptimization <= 0 ) then begin
	  	writeString('<SubTexture name="'+name+'" x="'+inttostr(imageX(i))+'"'+ 
				      ' y="'+inttostr(imageY(i))+'"'+
	 		  	      ' width="'+inttostr(imageHeight(i))+'"'+
				      ' height="'+inttostr(imageWidth(i))+'" rotated="true"/>'+#13);
	      end else begin
	        writeString('<SubTexture name="'+name+'" x="'+inttostr(imageX(i))+'"'+
				      ' y="'+inttostr(imageY(i))+'"'+
	 		  	      ' width="'+inttostr(trimHeight)+'"'+
				      ' height="'+inttostr(trimWidth)+'"'+
				      ' frameX="'+inttostr(-trimMinX)+'" frameY="'+inttostr(-trimMinY)+'" frameWidth="'+inttostr(imageWidth(i))+'" frameHeight="'+inttostr(imageHeight(i))+'"  rotated="true"/>'+#13);	
	      end;  
      end else begin
	      if (( trimMinX = 0) and ( trimMinY = 0) and ( trimWidth = imageWidth(i) ) and ( trimHeight = imageHeight(i))) or ( spritesOptimization <= 0 ) then begin
		writeString('<SubTexture name="'+name+'" x="'+inttostr(imageX(i))+'"'+
				      ' y="'+inttostr(imageY(i))+'"'+
	 		  	      ' width="'+inttostr(imageWidth(i))+'"'+
				      ' height="'+inttostr(imageHeight(i))+'"/>'+#13);
	      end else begin
	        writeString('<SubTexture name="'+name+'" x="'+inttostr(imageX(i))+'"'+
				      ' y="'+inttostr(imageY(i))+'"'+
	 		  	      ' width="'+inttostr(trimWidth)+'"'+
				      ' height="'+inttostr(trimHeight)+'"'+
				      ' frameX="'+inttostr(-trimMinX)+'" frameY="'+inttostr(-trimMinY)+'" frameWidth="'+inttostr(imageWidth(i))+'" frameHeight="'+inttostr(imageHeight(i))+'"/>'+#13);	
	      end;
      end;
    end;
  end;

 writeString('</TextureAtlas>');
end.