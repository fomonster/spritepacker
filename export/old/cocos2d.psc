program cocos2dexporter;

var 
  i:integer;
  s:ansistring;

function getFileExt():ansistring;
begin
  result:='.xml';
end;

function getResult():ansistring;
begin
  result:=s;
end;

  var 
    trimMinX:integer;
    trimMinY:integer;
    trimMaxX:integer;
    trimMaxY:integer;
    trimWidth:integer;
    trimHeight:integer;
    name:ansistring;
begin
  s:='<?xml version="1.0" encoding="UTF-8"?>'+#13;
  s:=s+'<TextureAtlas imagePath="'+ExtractFileName(outputImageFileName)+'.'+outputImageFileFormat+'" width="'+intToStr(outputImageWidth)+'" height="'+inttostr(outputImageHeight)+'">'+#13;

  for i:=0 to imagesCount-1 do begin
    if ( imagePage(i) = currentPage ) and ( imageType(i) = 1 ) then begin
      name:=ExtractFileNameWithoutExt(imageName(i));
      trimMinX := imageTrimMinX(i);		
      trimMinY := imageTrimMinY(i);		
      trimMaxX := imageTrimMaxX(i);		
      trimMaxY := imageTrimMaxY(i);		
      trimWidth := trimMaxX-trimMinX+1;	
      trimHeight := trimMaxY-trimMinY+1;	
	
      if (( trimMinX = 0) and ( trimMinY = 0) and ( trimWidth = imageWidth(i) ) and ( trimHeight = imageHeight(i))) or ( spritesOptimization <= 0 ) then begin
	s:=s+'<SubTexture name="'+name+'" x="'+inttostr(imageX(i))+'"'+
			      ' y="'+inttostr(imageY(i))+'"'+
 		  	      ' width="'+inttostr(imageWidth(i))+'"'+
			      ' height="'+inttostr(imageHeight(i))+'"/>'+#13;
      end else begin
        s:=s+'<SubTexture name="'+name+'" x="'+inttostr(imageX(i))+'"'+
			      ' y="'+inttostr(imageY(i))+'"'+
 		  	      ' width="'+inttostr(trimWidth)+'"'+
			      ' height="'+inttostr(trimHeight)+'"'+
			      ' frameX="'+inttostr(-trimMinX)+'" frameY="'+inttostr(-trimMinY)+'" frameWidth="'+inttostr(imageWidth(i))+'" frameHeight="'+inttostr(imageHeight(i))+'"/>'+#13;	
      end;

    end;
  end;

  s:=s+'</TextureAtlas>';
end.