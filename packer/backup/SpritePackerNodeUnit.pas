{*******************************************************************************

  Проект: SpritePacker
  Автор: Фомин С.С.


  Назначение модуля:


*******************************************************************************}
unit SpritePackerNodeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SSXMLUnit, ProtectionUnit, Dialogs, IntfGraphics, FPimage, RGBGraphics, Graphics;

const
  SPRITE_PACKER_NODE_TYPE_ROOT = 0;
  SPRITE_PACKER_NODE_TYPE_IMAGE = 1;
  SPRITE_PACKER_NODE_TYPE_FOLDER = 2;

type

  TSpritePackerNode=class
  public
    owner:TClass;

    //
    id:integer;
    parentId:integer;

    //
    typeId:integer;

    //
    filepath:AnsiString; // Только путь к файлу с \ в конце
    relativepath:AnsiString; //
    filename:AnsiString; // Имя файла без \ (У папок пустое)
    path:AnsiString; // Относительный путь

    // Что это блять?
    childsCount:integer;
    childs:array of integer;

    //
    bakId:integer;
    isNeedDelete:Boolean;
    age:Longint;

    //
    IsChanged:Boolean;

    //
    bitmap:TRGB32BitMap;

    //
    turn:boolean;
    x:integer;
    y:integer;
    width:integer;
    height:integer;
    page:integer;

    // Заранее расчитанный trim
    trimMinX:integer;
    trimMaxX:integer;
    trimMinY:integer;
    trimMaxY:integer;

    //
    constructor Create;
    destructor Destroy;override;

    procedure SetFromXML(xml:TXMLNode);
    procedure SetToXML(xml:TXMLNode);

    procedure AddChild(_n: integer);

    procedure updateFile;
    function loadFile:Boolean;

    function fullName:AnsiString;

    procedure logChilds;
  end;

implementation

uses SpritePackerUnit, logformUnit;

constructor TSpritePackerNode.Create;
begin
  id := -1;
  parentId := -1;
  typeId := SPRITE_PACKER_NODE_TYPE_ROOT;
  IsChanged := true;
  owner:=nil;

  filename := '';
  filepath := '';

  isNeedDelete := false;

  bitmap := nil;

  turn:=false;
  x:=0;
  y:=0;
  width:=0;
  height:=0;
  trimMinX:=0;
  trimMaxX:=0;
  trimMinY:=0;
  trimMaxY:=0;

  SetLength(Childs, 0);
  ChildsCount:=0;

  age := 0;
end;

destructor TSpritePackerNode.Destroy;
begin

  if ( bitmap <> nil ) then begin
    bitmap.Free;
    bitmap := nil;
  end;

  owner:=nil;
end;

procedure TSpritePackerNode.AddChild(_n: integer);
begin
  SetLength(Childs, ChildsCount + 1);
  Childs[ChildsCount] := _n;
  Inc(ChildsCount);
end;

procedure TSpritePackerNode.SetFromXML(xml:TXMLNode);
var
   i:Integer;
   node:TSpritePackerNode;
   xml1:TXMLNode;
begin
  if not xml.IsNotEmpty then exit;

  typeId := xml.Attribute_Int('typeid');

  if xml['params'].IsNotEmpty then begin

    filename := xml['params'].Attribute_Str('filename');
    filepath := xml['params'].Attribute_Str('filepath');
    relativepath := xml['params'].Attribute_Str('relativepath');

  end;

  if xml['childs'].IsNotEmpty then begin
    for i:=0 to xml['childs'].SubNodesCount-1 do begin
      xml1:=xml['childs'].SubNodes[i];
      if xml1.Name='node' then begin
        node:=TSpritePacker(owner).Add(self);
        node.SetFromXML(xml1);
      end;
    end;
  end;

end;

procedure TSpritePackerNode.SetToXML(xml:TXMLNode);

var
   i:Integer;
   j:Integer;
   node,node1,node2:TXMLNode;

begin

 //
 node:=xml.SubNodes_Add;
 node.Name:='node';
 node.Attribute_SetInt('typeid',typeId);
 node.Attribute_SetInt('id',Id);
 node.Attribute_SetInt('ownerId',parentId);

 //
 node1:=node.SubNodes_Add;
 node1.name:='params';

 node1.Attribute_SetStr('filename',filename);
 node1.Attribute_SetStr('filepath',filepath);
 node1.Attribute_SetStr('relativepath',relativepath);

 //
 node2:=node.SubNodes_Add;
 node2.name:='childs';
 for i:=0 to ChildsCount-1 do begin
   j := Childs[i];
   TSpritePacker(owner)[j].SetToXML(node2);
 end;

end;


procedure TSpritePackerNode.updateFile;
  var
    newFileAge:Longint;
    fileAttributes:integer;
    Info:TSearchRec;
    allFiles:AnsiString;
    ext:AnsiString;
begin

   fileAttributes := FileGetAttr(filepath + filename);
   newFileAge := FileAge(filepath + filename);

   //
   if ( fileAttributes = -1 ) then begin
     AddLog('Image '+filepath + filename+' not exists! Deleted');
     isNeedDelete:=true;
     exit;
   end;

   if (( fileAttributes and faDirectory ) <> 0 ) then begin
     if ( age <> newFileAge ) then IsChanged := true;

     if ( typeId = SPRITE_PACKER_NODE_TYPE_FOLDER ) then begin

        allFiles := '';
        If FindFirst(filepath+filename+'\*.*',faDirectory, Info) = 0 then begin
          Repeat
            if ( Info.Name <> '.' ) and ( Info.Name <> '..' ) then begin
               allFiles += filepath+filename+'\'+Info.Name + '<->';

               if ( (Info.Attr and faDirectory ) <> 0) then begin

                  TSpritePacker(owner).AddFile(filepath+filename+'\'+Info.Name, self);

               end;

            end;

          Until FindNext(Info)<>0;
        end;
        FindClose(Info);

        If FindFirst(filepath+filename+'\*.*',faAnyFile, Info) = 0 then begin
          Repeat
            if ( Info.Name <> '.' ) and ( Info.Name <> '..' ) then begin
               allFiles += filepath+filename+'\'+Info.Name + '<->';

               //TODO: ( ext = '.pcx' )
               //TODO:( ext = '.tga' )
               //TODO:( ext = '.tif' )
               //TODO:( ext = '.psd' )
               //TODO:( ext = '.webp' )

               ext:=LowerCase(ExtractFileExt(Info.Name));
               if ( ext = '.bmp' ) or ( ext = '.png' ) or ( ext = '.jpg' ) or ( ext = '.gif' ) then begin

                 TSpritePacker(owner).AddFile(filepath+filename+'\'+Info.Name, self);

               end;

            end;

          Until FindNext(Info)<>0;
        end;

        FindClose(Info);

     end;

   end else begin

     if ( FileExists(filepath + filename) ) then begin
        if ( age <> newFileAge ) then begin
          age := newFileAge;

          IsChanged := true;

          if not loadFile then begin
            IsNeedDelete:=true;
            TSpritePacker(owner).IsLoadedOnThisFrame:=true;
            AddLog('Image '+filepath + filename+' can not read! Deleted');
          end;

        end;

     end else begin
       AddLog('Image '+filepath + filename+' not exists! Deleted');
       IsNeedDelete:=true;
     end;

   end;
   age := newFileAge;

end;

function TSpritePackerNode.fullName:AnsiString;
  var
    parent:TSpritePackerNode;
begin
  parent:=TSpritePacker(owner).GetParentOf(self);
  if ( parent = nil ) then begin
    result := filename;
  end else begin
    result := parent.fullName +'/'+ filename;
  end;

end;

procedure TSpritePackerNode.logChilds;
  var
    s:AnsiString;
    i:integer;
begin
  for i:=0 to length(Childs)-1 do begin
    s:=s+','+inttostr(Childs[i]);
  end;
  AddLog('> '+inttostr(length(Childs))+' - '+s);
end;

function TSpritePackerNode.loadFile:Boolean;
  var
    color:TFPColor;
    ix,iy,w:integer;
    alpha, alphaY:integer;
    image:TFPCustomImage;
begin
  image := nil;

  if ( bitmap <> nil ) then begin
    bitmap.Free;
    bitmap := nil;
  end;

  try
    image := TFPMemoryImage.Create(2, 2);
    image.LoadFromFile(filepath + filename);

    trimMinX:=10000000;
    trimMaxX:=-10000000;
    trimMinY:=10000000;
    trimMaxY:=-10000000;

    width:=image.width;
    height:=image.Height;

    bitmap := TRGB32Bitmap.Create(image.Width, image.Height);
    w:=Image.Width;
    for iy:=0 to image.Height-1 do begin
      alphaY:=0;
      for ix:=0 to image.Width-1 do begin
        color := image.Colors[ix, iy];
        alpha := color.alpha shr 8;
        if ( alpha > 0 ) then begin
          if ( ix < trimMinX ) then trimMinX:=ix;
          if ( ix > trimMaxX ) then trimMaxX:=ix;
          alphaY:=alpha;
        end;
        bitmap.Set32Pixel(ix, iy, ((alpha) shl 24 ) or (color.blue shr 8) or (  (color.green shr 8)  shl 8 ) or ( (color.red shr 8 ) shl 16));
//        Pixels[ix + iy * w] := ((alpha) shl 24 ) or (color.blue shr 8) or (  (color.green shr 8)  shl 8 ) or ( (color.red shr 8 ) shl 16);
      end;
      if ( alphaY > 0 ) then begin
        if ( iy < trimMinY ) then trimMinY:=iy;
        if ( iy > trimMaxY ) then trimMaxY:=iy;
      end;
    end;

    // Картинка пустая
    if ( ( trimMinX >= image.Width ) or ( trimMaxX < 0 ) or ( trimMinY >= image.Height ) or ( trimMaxY < 0 ) ) then begin
      trimMinX:=0;
      trimMaxX:=0;
      trimMinY:=0;
      trimMaxY:=0;
    end;

    AddLog('Image '+filename+' '+intToStr(Image.Width)+', '+intToStr(image.Height)+' ('+intToStr(trimMinX)+','+intToStr(trimMaxX)+','+intToStr(trimMinY)+','+intToStr(trimMaxY)+') loaded!');

    if ( image <> nil ) then begin
      image.Free;
      image := nil;
    end;

    TSpritePacker(owner).IsChanged:=true;
    result := true;
  except

  end;
end;


end.

