{******************************************************************
    SpritePacker: Pixi js exporter.

******************************************************************}
program starlingexporter;

var 
  i:integer;
  s:ansistring;
  tab:ansistring;

function getFileExt():ansistring;
begin
  result:='.json';
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
    notTrimmed:boolean;
    name:ansistring;
begin
  s := 'Generating Starling json';
  tab := '    ';
  Clear;
 
  writeString('{"frames":  {'+#13);
  writeString(''+#13);

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
      
      writeString('"'+name+'":'+#13);
      writeString('{'+#13);

      if imageTurn(i) then begin
        writeString(tab+'"rotated": true,'+#13);
      end else begin
        writeString(tab+'"rotated": false,'+#13);
      end;

      trimMinX := imageTrimMinX(i);		
      trimMinY := imageTrimMinY(i);		
      trimMaxX := imageTrimMaxX(i);		
      trimMaxY := imageTrimMaxY(i);		
      trimWidth := trimMaxX-trimMinX+1;	
      trimHeight := trimMaxY-trimMinY+1;	
      notTrimmed := (( trimMinX = 0) and ( trimMinY = 0) and ( trimWidth = imageWidth(i) ) and ( trimHeight = imageHeight(i))) or ( spritesOptimization <= 0 );
	
      if notTrimmed then begin
        writeString(tab+'"trimmed": false,'+#13);
      end else begin
        writeString(tab+'"trimmed": true,'+#13);
      end;

      if imageTurn(i) then begin
	      if notTrimmed then begin
               
		writeString(tab+'"frame": {"x":'+inttostr(imageX(i))+',"y":'+inttostr(imageY(i))+',"w":'+inttostr(imageWidth(i))+',"h":'+inttostr(imageHeight(i))+'},'+#13);	
		writeString(tab+'"spriteSourceSize": {"x":'+inttostr(0)+',"y":'+inttostr(0)+',"w":'+inttostr(imageWidth(i))+',"h":'+inttostr(imageHeight(i))+'},'+#13);

		//writeString(tab+'"frame": {"x":'+inttostr(imageX(i))+',"y":'+inttostr(imageY(i))+',"w":'+inttostr(imageHeight(i))+',"h":'+inttostr(imageWidth(i))+'},'+#13);	
		//writeString(tab+'"spriteSourceSize": {"x":'+inttostr(0)+',"y":'+inttostr(0)+',"w":'+inttostr(imageHeight(i))+',"h":'+inttostr(imageWidth(i))+'},'+#13);
	  	
	      end else begin
                 writeString(tab+'"frame": {"x":'+inttostr(imageX(i))+',"y":'+inttostr(imageY(i))+',"w":'+inttostr(trimWidth)+',"h":'+inttostr(trimHeight)+'},'+#13);
		 writeString(tab+'"spriteSourceSize": {"x":'+inttostr(trimMinX)+',"y":'+inttostr(trimMinY)+',"w":'+inttostr(trimWidth)+',"h":'+inttostr(trimHeight)+'},'+#13);
	         
		 //writeString(tab+'"frame": {"x":'+inttostr(imageX(i))+',"y":'+inttostr(imageY(i))+',"w":'+inttostr(trimHeight)+',"h":'+inttostr(trimWidth)+'},'+#13);
		 //writeString(tab+'"spriteSourceSize": {"x":'+inttostr(trimMinX)+',"y":'+inttostr(trimMinY)+',"w":'+inttostr(trimHeight)+',"h":'+inttostr(trimWidth)+'},'+#13);
	      end;  
      end else begin
	      if notTrimmed then begin
                writeString(tab+'"frame": {"x":'+inttostr(imageX(i))+',"y":'+inttostr(imageY(i))+',"w":'+inttostr(imageWidth(i))+',"h":'+inttostr(imageHeight(i))+'},'+#13);	 
		writeString(tab+'"spriteSourceSize": {"x":'+inttostr(0)+',"y":'+inttostr(0)+',"w":'+inttostr(imageWidth(i))+',"h":'+inttostr(imageHeight(i))+'},'+#13);
	      end else begin
                writeString(tab+'"frame": {"x":'+inttostr(imageX(i))+',"y":'+inttostr(imageY(i))+',"w":'+inttostr(trimWidth)+',"h":'+inttostr(trimHeight)+'},'+#13);
                writeString(tab+'"spriteSourceSize": {"x":'+inttostr(trimMinX)+',"y":'+inttostr(trimMinY)+',"w":'+inttostr(trimWidth)+',"h":'+inttostr(trimHeight)+'},'+#13);	     
	      end;
      end;

      writeString(tab+'"sourceSize": {"w":'+intToStr(imageWidth(i))+',"h":'+intToStr(imageHeight(i))+'},'+#13);
      writeString(tab+'"pivot": {"x":0.5,"y":0.5}'+#13);

      if i < imagesCount-1 then begin
        writeString('},'+#13);
      end else begin
        writeString('}'+#13);
      end;

    end;
  end;
  writeString('},'+#13+#10);

  writeString('"meta": {'+#13);

  writeString(tab+'"app": "http://spritepacker.com/",'+#13);
  writeString(tab+'"version": "1.0",'+#13);
  writeString(tab+'"image": "'+ExtractFileName(outputImageFileName)+'.'+outputImageFileFormat+'",'+#13);
  writeString(tab+'"format": "RGBA8888",'+#13);
  writeString(tab+'"size": {"w":'+intToStr(outputImageClampedWidth)+',"h":'+inttostr(outputImageClampedHeight)+'},'+#13);
  writeString(tab+'"scale": "1"'+#13);

  writeString('}'+#13);
  writeString('}'+#13);
end.