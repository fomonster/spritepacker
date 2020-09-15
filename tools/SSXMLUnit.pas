{*******************************************************************************

  Author: Fomin Sergey (fomonster@gmail.com)
  Date: 2009

  XML parser and creator module for freepascal.

*******************************************************************************}
unit SSXMLUnit;
{$mode delphi}
interface

uses SysUtils, StrUtils,Classes;

const
  TG_EOS=-1;
  TG_NAME=0;
  TG_NUMBER=1;
  TG_EQUAL=2;
  TG_AnsiString=3;
  TG_COMMENT=4;
  TG_BEGIN=5;
  TG_BEGINEND=6;
  TG_END=7;
  TG_ENDBEGIN=8;
  TG_SYMBOL=9;
  TG_TEXT=10;

type
  PXMLAttribute = ^TXMLAttribute;
  TXMLAttribute = object
    Name:AnsiString;
    UCaseName:AnsiString;
    Value:AnsiString;
    function Gs:AnsiString;
    function Gi:Integer;
    function Gf:Single;
    procedure EqV(a:AnsiString);overload;
    procedure EqV(a:Integer);overload;
    procedure EqV(a:Double);overload;
  end;

  TXMLNode = class;

  PXMLNodeArray = ^TXMLNodeArray;
  TXMLNodeArray = array [0..10000000] of TXMLNode;

  TXMLNode = class
    IsNotEmpty:boolean;
    IsLoaded:boolean;

    Name:AnsiString;
    UCaseName:AnsiString;
    Text:AnsiString;
    NoText:Boolean; // true - Поле выглядит как <name ... /> иначе поле выглядит как <name ...>...</name>

    AttributesCount:Integer;
    Attributes:array of TXMLAttribute;

    SubNodesCount:Integer;
    SubNodesArray:PXMLNodeArray;

    constructor Create;
    destructor Destroy;

    procedure SetName(_name:AnsiString);

    procedure SubNodes_Clear;
    procedure SubNodes_SetLength(NewLength:Integer);
    function SubNodes_Add:TXMLNode;
    function SubNodes_Get(index:Integer):TXMLNode;
    procedure SubNodes_Delete(index:Integer);

    function SubNodes_IsWithAttributeStr(_attrname:AnsiString;_value:AnsiString):Boolean;
    function SubNodes_IsWithAttributeB64Str(_attrname:AnsiString;_value:AnsiString):Boolean;
    function SubNodes_IsWithAttributeInt(_attrname:AnsiString;_value:Integer):Boolean;
    function SubNodes_IsWithAttributeFloat(_attrname:AnsiString;_value:Double):Boolean;

    function SubNodes_GetWithAttributeStr(_attrname:AnsiString;_value:AnsiString):TXMLNode;
    function SubNodes_GetWithAttributeB64Str(_attrname:AnsiString;_value:AnsiString):TXMLNode;
    function SubNodes_GetWithAttributeInt(_attrname:AnsiString;_value:Integer):TXMLNode;
    function SubNodes_GetWithAttributeFloat(_attrname:AnsiString;_value:Double):TXMLNode;
    
    procedure Attributes_Clear;
    procedure Attribute_SetLength(NewLength:Integer);
    function Attribute_Add:PXMLAttribute;overload;
    function Attribute_Add(_name:AnsiString):PXMLAttribute;overload;
    // проверка существования атрибута
    function Attribute_Is(index:AnsiString):boolean;
    // установка значения атрибута, если атрибута нет то создается
    function Attribute_Set(_name:AnsiString):PXMLAttribute;
    procedure Attribute_SetStr(_name,_value:AnsiString);
    procedure Attribute_SetB64Str(_name,_value:AnsiString);
    procedure Attribute_SetInt(_name:AnsiString;_value:Integer);
    procedure Attribute_SetFloat(_name:AnsiString;_value:Double);
    // работа с существующими атрибутами
    function Attribute(index:AnsiString):PXMLAttribute;
    function Attribute_Str(index:AnsiString):AnsiString;
    function Attribute_B64Str(index:AnsiString):AnsiString;
    function Attribute_Int(index:AnsiString):Integer;
    function Attribute_Float(index:AnsiString):Double;

    procedure Assign(xml:TXMLNode);
    procedure AssignFromStr(_xmlstr:AnsiString);

    procedure LoadFromStream(stream:TStream);
    procedure SaveToStream(stream:TStringStream);

    procedure LoadFromXMLFile(filename:AnsiString);
    procedure SaveToXMLFile(filename:AnsiString);
    procedure LoadFromXMLString(s:AnsiString);
    function SaveToXMLString:AnsiString;

    procedure LoadFromTextFile(filename:AnsiString;textfileformat:TXMLNode);
    procedure SaveToTextFile(filename:AnsiString;textfileformat:TXMLNode);

    function Parse(stream:TStream;nodename:AnsiString):boolean;
    procedure Writenode(var fl:TextFile;lev:Integer);

    function Get(index:AnsiString):TXMLNode;
    property item[index : AnsiString]:TXMLNode read Get;default;
    property SubNodes[index : Integer]:TXMLNode read SubNodes_Get;


  end;

  function EncodeBase64(const inStr: AnsiString):AnsiString;
  function DecodeBase64(const CinLine: AnsiString):AnsiString;  

var
  emptynode:TXMLNode = nil;
  emptyattr:TXMLAttribute;

implementation

var
  ltagtype:Integer;

function space(n:Integer):AnsiString;
  var
    i:Integer;
begin
  result:='';
  for i:=1 to n do result:=result+' ';
end;



// Процедура для синтаксического анализа текста файла XML
// Разделяет теги
procedure ReadXMLTag(fs:TStream;var tag:AnsiString;var tagtype:Integer);
  label
    ennddd,llll;
  var
    b:Byte;
    c,c1:Char;

begin
  tag:='';
  tagtype:=TG_EOS; // EOS - END OF STREAM
  if (fs.Position>=fs.Size) then exit;
try
  fs.Read(b,1);
  while (b<=32)and(fs.Position<fs.Size) do fs.Read(b,1);
  c:=char(b);
  while (ltagtype=-3)and(c<>'<')and(fs.Position<fs.Size) do begin
    fs.Read(c,1);
    if (fs.Position>=fs.Size) then begin
      goto ennddd;
      exit;
    end;
  end;
  // Текст между тегами
  if ((ltagtype=TG_END) or (ltagtype=TG_ENDBEGIN)) and (c<>'<') then begin
   if (fs.Position<fs.Size) then begin
      tag:=tag+c;
      fs.Read(c,1);
      while (c<>'<') and (fs.Position<fs.Size) do begin
        tag:=tag+c;
        fs.Read(c,1);
      end;
      fs.Position:=fs.Position-1;
      tagtype:=TG_TEXT; // >...<
      ltagtype:=tagtype;
      goto ennddd;
      exit;
    end;
  end;
  // Слова
  if (c in ['a'..'z','A'..'Z','_','а'..'я','А'..'Я']) then begin
    while ((c in ['a'..'z','A'..'Z','0'..'9','_','-',':','а'..'я','А'..'Я'])) and (fs.Position<fs.Size) do begin
      tag:=tag+c;
      fs.Read(c,1);
    end;
    fs.Position:=fs.Position-1;
    tagtype:=TG_NAME; // KEYWORD OR NAME
    ltagtype:=tagtype;
    goto ennddd;
    exit;
  end;
  // Числа
  if (c in ['0'..'9','-']) then begin
    while ((c in ['0'..'9','.','-'])) and (fs.Position<fs.Size) do begin
      tag:=tag+c;
      fs.Read(c,1);
    end;
    fs.Position:=fs.Position-1;
    tagtype:=TG_NUMBER; // NUMBER
    ltagtype:=tagtype;
      goto ennddd;
    exit;
  end;
  // идентификаторы =
  if (c='=') then begin
    tag:=c;
    tagtype:=TG_EQUAL; // =
    ltagtype:=tagtype;
      goto ennddd;
    exit;
  end;
  // строка текста "..."
  if (c='"') then begin
    if (fs.Position<fs.Size) then begin
      fs.Read(c,1);
      while (fs.Position<fs.Size) do begin
        if c='"' then begin
          if (fs.Position<fs.Size) then begin
            fs.Read(c,1);
            if c<>'"' then begin
              fs.Position:=fs.Position-1;
              break;
            end;
          end else break;
        end;
        tag:=tag+c;
        fs.Read(c,1);
      end;
      tagtype:=TG_AnsiString; // "..."
      ltagtype:=tagtype;
      goto ennddd;
      exit;
    end;
  end;
  // комментарии //...
  if (c='/') and (fs.Position<fs.Size) then begin
    fs.Read(c1,1);
    if c1='/' then begin
      tag:=tag+c1;
      while (c<>char(10)) and (fs.Position<fs.Size) do begin
        tag:=tag+c;
        fs.Read(c,1);
      end;
      tagtype:=TG_COMMENT; // comment
      ltagtype:=tagtype;
      goto ennddd;
      exit;
    end else if c1='>' then begin
      tag:='/>';
      tagtype:=TG_ENDBEGIN; // />
      ltagtype:=tagtype;
      goto ennddd;
      exit;
    end else begin
      fs.Position:=fs.Position-1;
    end;
  end;
  // Начало блока <..
  if (c='<') then begin
    fs.Read(c,1);
llll:
    if (c='?') or (c='!') then begin
      while c<>'<' do begin
        if (fs.Position>=fs.Size-1) then begin
          tag:='';
          tagtype:=TG_EOS;
          goto ennddd;
          exit;
        end;
        fs.Read(c,1);
      end;
      fs.Read(c,1);
    end;
    if (c='?') or (c='!') then goto llll;
    if c='/' then begin
      tag:='</';
      tagtype:=TG_BEGINEND; // </
      ltagtype:=tagtype;
      goto ennddd;
      exit;
    end else begin
      fs.Position:=fs.Position-1;
      tag:='<';
      tagtype:=TG_BEGIN; // BEGIN
      ltagtype:=tagtype;
      goto ennddd;
      exit;
    end;
  end;
  // Конец блока }..
  if (c='>') then begin
    tag:=c;
    tagtype:=TG_END; // END
    ltagtype:=tagtype;
      goto ennddd;
    exit;
  end;
  // Остальное
  tag:=c;
  tagtype:=TG_SYMBOL; // ANY SYMBOL
  ltagtype:=tagtype;
ennddd:
//  WriteLn(fla,IntToStr(tagtype)+' === '+tag);
except
end;
end;

constructor TXMLNode.Create;
begin
  SetName('');
  
  Text:='';
  SubNodesCount:=0;
  SubNodesArray:=nil;

  AttributesCount:=0;
  Attributes:=nil;

  IsNotEmpty:=true;
end;

destructor TXMLNode.Destroy;
begin
  Attributes_Clear;
  SubNodes_Clear;
end;

procedure TXMLNode.SetName(_name:AnsiString);
begin
  Name:=_name;
  UCaseName:=AnsiUpperCase(_name);
end;

procedure TXMLNode.SubNodes_Clear;
  var
    i:Integer;
begin
  for i:=0 to SubNodesCount-1 do
    SubNodesArray^[i].Destroy;
  SubNodesCount:=0;
  Freemem(SubNodesArray);
  SubNodesArray:=nil;
end;

procedure TXMLNode.SubNodes_SetLength(NewLength:Integer);
  var
    i:Integer;
begin
  if NewLength = SubNodesCount then exit;
  if NewLength > 0 then begin
    if NewLength < SubNodesCount then begin
      for i:=NewLength to SubNodesCount-1 do
        SubNodesArray^[i].Destroy;
    end;
    ReAllocMem(SubNodesArray,sizeof(TXMLNode) * NewLength);
    if NewLength > SubNodesCount then begin
      for i:=SubNodesCount to NewLength-1 do
        SubNodesArray^[i]:=TXMLNode.Create;
    end;
    SubNodesCount:=NewLength;
  end else begin
    SubNodes_Clear;
  end;
end;

function TXMLNode.SubNodes_Add:TXMLNode;
begin
  SubNodes_SetLength(SubNodesCount+1);
  result:=SubNodesArray^[SubNodesCount-1];
end;

function TXMLNode.SubNodes_Get(index:Integer):TXMLNode;
begin
  if (index<0) or (index>=SubNodesCount) then begin
    result:=emptynode;
    exit;
  end else result:=SubNodesArray^[index];
end;

procedure TXMLNode.SubNodes_Delete(index:Integer);
  var
    i:Integer;
    xmlnode:TXMLNode;
begin
  if SubNodesCount<=0 then exit;
  if (index<0) or (index>=SubNodesCount) then exit;
  for i:=index to SubNodesCount-2 do begin
    xmlnode:=SubNodesArray^[i];
    SubNodesArray^[i]:=SubNodesArray^[i+1];
    SubNodesArray^[i+1]:=xmlnode;
  end;  
  SubNodes_SetLength(SubNodesCount-1);
end;

function TXMLNode.SubNodes_IsWithAttributeStr(_attrname:AnsiString;_value:AnsiString):Boolean;
  var
    i:Integer;
begin
  result:=false;
  _value:=AnsiUpperCase(_value);
  for i:=0 to SubNodesCount-1 do begin
    if SubNodes[i].Attribute_Is(_attrname) and (AnsiUpperCase(SubNodes[i].Attribute_Str(_attrname)) = _value) then begin
      result:=true;
      exit;
    end;
  end;
end;

function TXMLNode.SubNodes_IsWithAttributeB64Str(_attrname:AnsiString;_value:AnsiString):Boolean;
  var
    i:Integer;
begin
  result:=false;
  _value:=EncodeBase64(_value);
  for i:=0 to SubNodesCount-1 do begin
    if SubNodes[i].Attribute_Is(_attrname) and (SubNodes[i].Attribute_Str(_attrname) = _value) then begin
      result:=true;
      exit;
    end;
  end;
end;

function TXMLNode.SubNodes_IsWithAttributeInt(_attrname:AnsiString;_value:Integer):Boolean;
  var
    i:Integer;
begin
  result:=false;
  for i:=0 to SubNodesCount-1 do begin
    if SubNodes[i].Attribute_Is(_attrname) and (SubNodes[i].Attribute_Int(_attrname) = _value) then begin
      result:=true;
      exit;
    end;
  end;
end;

function TXMLNode.SubNodes_IsWithAttributeFloat(_attrname:AnsiString;_value:Double):Boolean;
  var
    i:Integer;
begin
  result:=false;
  for i:=0 to SubNodesCount-1 do begin
    if SubNodes[i].Attribute_Is(_attrname) and (SubNodes[i].Attribute_Float(_attrname) = _value) then begin
      result:=true;
      exit;
    end;
  end;
end;

function TXMLNode.SubNodes_GetWithAttributeStr(_attrname:AnsiString;_value:AnsiString):TXMLNode;
  var
    i:Integer;
begin
  result:=emptynode;
  _value:=AnsiUpperCase(_value);
  for i:=0 to SubNodesCount-1 do begin
    if SubNodes[i].Attribute_Is(_attrname) and (AnsiUpperCase(SubNodes[i].Attribute_Str(_attrname)) = _value) then begin
      result:=SubNodes[i];
      exit;
    end;
  end;
end;

function TXMLNode.SubNodes_GetWithAttributeB64Str(_attrname:AnsiString;_value:AnsiString):TXMLNode;
  var
    i:Integer;
begin
  result:=emptynode;
  _value:=EncodeBase64(_value);
  for i:=0 to SubNodesCount-1 do begin
    if SubNodes[i].Attribute_Is(_attrname) and (SubNodes[i].Attribute_Str(_attrname) = _value) then begin
      result:=SubNodes[i];
      exit;
    end;
  end;
end;

function TXMLNode.SubNodes_GetWithAttributeInt(_attrname:AnsiString;_value:Integer):TXMLNode;
  var
    i:Integer;
begin
  result:=emptynode;
  for i:=0 to SubNodesCount-1 do begin
    if SubNodes[i].Attribute_Is(_attrname) and (SubNodes[i].Attribute_Int(_attrname) = _value) then begin
      result:=SubNodes[i];
      exit;
    end;
  end;
end;

function TXMLNode.SubNodes_GetWithAttributeFloat(_attrname:AnsiString;_value:Double):TXMLNode;
  var
    i:Integer;
begin
  result:=emptynode;
  for i:=0 to SubNodesCount-1 do begin
    if SubNodes[i].Attribute_Is(_attrname) and (SubNodes[i].Attribute_Float(_attrname) = _value) then begin
      result:=SubNodes[i];
      exit;
    end;
  end;
end;

procedure TXMLNode.Attributes_Clear;
begin
  Attributes:=nil;
  AttributesCount:=0;
end;

procedure TXMLNode.Attribute_SetLength(NewLength:Integer);
begin
  if NewLength>0 then begin
    SetLength(Attributes,NewLength);
    AttributesCount:=NewLength;
  end else Attributes_Clear;
end;

function TXMLNode.Attribute_Add:PXMLAttribute;
begin
  Attribute_SetLength(AttributesCount+1);
  result:=@Attributes[AttributesCount-1];
end;

function TXMLNode.Attribute_Set(_name:AnsiString):PXMLAttribute;
begin
  if Attribute_Is(_name) then begin
    result:=Attribute(_name);
  end else begin
    Attribute_SetLength(AttributesCount+1);
    result:=@Attributes[AttributesCount-1];
    result.Name:=_name;
    result.UCaseName:=AnsiUpperCase(_name);
  end;
end;

procedure TXMLNode.Attribute_SetStr(_name,_value:AnsiString);
begin
  Attribute_Set(_name)^.Value:=_value;
end;

procedure TXMLNode.Attribute_SetB64Str(_name,_value:AnsiString);
begin
  Attribute_Set(_name)^.Value:=EncodeBase64(_value);
end;

procedure TXMLNode.Attribute_SetInt(_name:AnsiString;_value:Integer);
begin
  Attribute_Set(_name)^.Value:=IntToStr(_value);
end;

procedure TXMLNode.Attribute_SetFloat(_name:AnsiString;_value:Double);
begin
  Attribute_Set(_name)^.Value:=FloatToStr(_value);
end;

function TXMLNode.Attribute_Add(_name:AnsiString):PXMLAttribute;
  var
    i:Integer;
    _nam:AnsiString;
begin
  _nam:=AnsiUpperCase(_name);
  for i:=0 to AttributesCount-1 do
    if _nam=Attributes[i].UCaseName then begin
      result:=@Attributes[i];
      exit;
    end;
  Attribute_SetLength(AttributesCount+1);
  Attributes[AttributesCount-1].Name:=_name;
  Attributes[AttributesCount-1].UCaseName:=AnsiUpperCase(_name);
  result:=@Attributes[AttributesCount-1];
end;

function TXMLNode.Parse(stream:TStream;nodename:AnsiString):boolean;
  var
    tag:AnsiString;
    tagid:Integer;
    attr:PXMLAttribute;
    attrname:AnsiString;
    attrvalue:AnsiString;
    node:TXMLNode;
begin
  NoText:=false;
  // Читаем атрибуты
  result:=false;
  ReadXMLTag(stream,tag,tagid);
  while tagid<>TG_EOS do begin
    if tagid=TG_NAME then begin
      attrname:=tag;
      ReadXMLTag(stream,tag,tagid);
      if tagid=TG_EQUAL then begin
        ReadXMLTag(stream,tag,tagid);
        if (tagid=TG_NAME) or (tagid=TG_AnsiString) or (tagid=TG_NUMBER) then begin
          attrvalue:=tag;
          attr:=Attribute_Add(attrname);
          attr^.Value:=attrvalue;
        end else exit;
      end else exit;
    end else if tagid=TG_ENDBEGIN then begin
      NoText:=true;
      result:=true;
      exit;
    end else if tagid=TG_END then break;
    ReadXMLTag(stream,tag,tagid);
  end;

  // Читаем дальше
  ReadXMLTag(stream,tag,tagid);
  while tagid<>TG_EOS do begin
    if tagid=TG_BEGIN then begin
      ReadXMLTag(stream,tag,tagid);
      if tagid=TG_NAME then begin
        node:=SubNodes_Add;
        node.SetName(tag);
        if not node.Parse(stream,LowerCase(Name)) then exit;
      end else exit;
    end else if tagid=TG_TEXT then begin
      Text:=Text+tag;
    end else if tagid=TG_BEGINEND then begin
       ReadXMLTag(stream,tag,tagid);
       ReadXMLTag(stream,tag,tagid);
       result:=true;
       exit;
    end else exit;
    ReadXMLTag(stream,tag,tagid);
  end;

  result:=true;
end;

procedure TXMLNode.LoadFromXMLFile(filename:AnsiString);
  var
    fs:TFileStream;
    ms:TMemoryStream;
begin
  if not FileExists(filename) then exit;
  try
    fs:=TFileStream.Create(filename, fmOpenRead );
    ms:=TMemoryStream.Create; // Почему, то если сразу делать LoadFromFile, то не успевает загрузить в память всё
    ms.LoadFromStream(fs);
    LoadFromStream(ms);
  finally
    fs.Free;
    ms.Free;
  end;
end;

procedure TXMLNode.LoadFromXMLString(s:AnsiString);
  var
    ss:TStringStream;
begin
  ss:=TStringStream.Create(s);
  LoadFromStream(ss);
  ss.Destroy;
end;

procedure TXMLNode.AssignFromStr(_xmlstr:AnsiString);
  var
    xml:TXMLNode;
begin
  xml:=TXMLNode.Create;
  xml.LoadFromXMLString(_xmlstr);
  if xml.SubNodesCount>0
    then Assign(xml.SubNodes[0]);
  xml.Destroy;
end;

procedure TXMLNode.Assign(xml:TXMLNode);
  var
    i:Integer;
    sxml:TXMLNode;
begin
  IsNotEmpty:=xml.IsNotEmpty;
  IsLoaded:=xml.IsLoaded;

  SetName(xml.Name);
  Text:=xml.Text;
  NoText:=xml.NoText;

  Attributes_Clear;
  for i:=0 to xml.AttributesCount-1 do begin
    Attribute_Add(xml.Attributes[i].Name)^.Value:=xml.Attributes[i].Value;
  end;
  SubNodes_Clear;
  for i:=0 to xml.SubNodesCount-1 do begin
    sxml:=SubNodes_Add;
    sxml.Assign(xml.SubNodes[i]);
  end;
end;

procedure TXMLNode.LoadFromStream(stream:TStream);
  var
    tagid,i:Integer;
    tag:AnsiString;
    node:TXMLNode;
begin
  IsLoaded:=false;
  ltagtype:=-3;
  SetName('');
  Text:='';
  ReadXMLTag(stream,tag,tagid);
  while tagid<>TG_EOS do begin
    if tagid=TG_BEGIN then begin
      ReadXMLTag(stream,tag,tagid);
      if tagid=TG_NAME then begin
        node:=SubNodes_Add;
        node.SetName(tag);
        if not node.Parse(stream,LowerCase(node.Name)) then break;
      end else break;
    end else break;
    ReadXMLTag(stream,tag,tagid);
  end;
  IsLoaded:=true;
end;

procedure TXMLNode.SaveToStream(stream:TStringStream);
  var
    i:Integer;
begin
  if self=nil then exit;
  stream.WriteString('<'+Name);
  for i:=0 to AttributesCount-1 do begin
    stream.WriteString(' '+Attributes[i].Name+'="'+Attributes[i].Value+'"');
  end;
  if NoText then begin
    stream.WriteString('/>');
  end else begin
    stream.WriteString('>');
    stream.WriteString(Text);
    for i:=0 to SubNodesCount-1 do begin
      SubNodesArray^[i].SaveToStream(stream);
    end;
    stream.WriteString('</'+Name+'>');
  end;
end;

procedure TXMLNode.Writenode(var fl:TextFile;lev:Integer);
  var
    i:Integer;
begin
  if self=nil then exit;
  write(fl,space(5*lev)+'<'+Name);
  for i:=0 to AttributesCount-1 do begin
    write(fl,' '+Attributes[i].Name+'="'+Attributes[i].Value+'"');
  end;
  if NoText then begin
    writeln(fl,'/>');
  end else begin
    write(fl,'>');
    writeln(fl,Text);
    for i:=0 to SubNodesCount-1 do begin
      SubNodesArray^[i].Writenode(fl,lev+1);
    end;
    writeln(fl,space(5*lev)+'</'+Name+'>');
  end;
end;

procedure TXMLNode.SaveToXMLFile(filename:AnsiString);
  var
    fl:TextFile;
    i:Integer;
begin
  AssignFile(fl,filename);
  {$I-}
  rewrite(fl);
  {$I+}
  if IOResult=0 then begin
    for i:=0 to SubNodesCount-1 do begin
      SubNodesArray^[i].Writenode(fl,0);
    end;
    CloseFile(fl);
  end;
end;

function TXMLNode.SaveToXMLString:AnsiString;
  var
    ss:TStringStream;
    i:Integer;
begin
  ss:=TStringStream.Create('');
  ss.Seek(0,0);
  for i:=0 to SubNodesCount-1 do begin
    SubNodesArray^[i].SaveToStream(ss);
  end;
  ss.Seek(0,0);
  result:=ss.ReadString(ss.Size);
  ss.Destroy;
end;

procedure TXMLNode.LoadFromTextFile(filename:AnsiString;textfileformat:TXMLNode);
  var
    fs:TFileStream;
    ms:TMemoryStream;
    s,d:AnsiString;
    n1,n2,fn:TXMLNode;
    c:char;
    i,l,f,e:Integer;
    codepage:Integer;
begin
  try
    fs:=TFileStream.Create(filename, fmOpenRead);
    ms:=TMemoryStream.Create; // Почему, то если сразу делать LoadFromFile, то не успевает загрузить в память всё
    ms.LoadFromStream(fs);
    ms.Seek(0,0);

    SubNodes_Clear;
    n1:=SubNodes_Add;
    n1.SetName('textfile');
    n1.Attribute_Add('name')^.Value:=textfileformat['textfileformat'].Attribute_Str('name');
    fn:=textfileformat['textfileformat'];
    if fn.Attribute_Str('codepage')='oem' then codepage:=0
    else if fn.Attribute_Str('codepage')='ansi' then codepage:=1
    else codepage:=2;
    while ms.Position<ms.Size do begin
      s:='';
      c:=#0;
      while (ms.Position<ms.Size) and (c<>#13) do begin
        ms.read(c,1);
        if c>=#32 then s:=s+c;
      end;
      if s<>'' then begin
        n2:=n1.SubNodes_Add;
        n2.SetName('record');
        for i:=0 to fn.SubNodesCount-1 do begin
          if fn.SubNodes[i].Name='field' then begin

            l:=fn.SubNodes[i].Attribute_Int('length');
            f:=fn.SubNodes[i].Attribute_Int('startpos');
            e:=fn.SubNodes[i].Attribute_Int('endpos');
            if l<=0 then l:=e-f+1;

            case codepage of
              0:d:=AnsiToUtf8((trim(copy(s,f,l))));
              1:d:=AnsiToUtf8(trim(copy(s,f,l)));
              2:d:=trim(copy(s,f,l));
            end;
//            d:=trim(copy(s,f,l));
            n2.Attribute_Add(fn.SubNodes[i].Attribute_Str('name'))^.Value:=d;
          end;

        end;

      end;
    end;
  finally
    ms.Free;
    fs.Free;
  end;
end;

procedure TXMLNode.SaveToTextFile(filename:AnsiString;textfileformat:TXMLNode);
  var
    fs:TFileStream;
    ms:TMemoryStream;
    xml,fxml:TXMLNode;
    i,j,e,f,l,k,codepage:Integer;
    s,v,fn:AnsiString;
    n:Char;
begin
  try
    fs:=TFileStream.Create(filename, fmOpenWrite or fmCreate);
    ms:=TMemoryStream.Create; // Почему, то если сразу делать LoadFromFile, то не успевает загрузить в память всё

    fxml:=textfileformat['textfileformat'];
    xml:=item['textfile'];
    if xml.IsNotEmpty and fxml.IsNotEmpty then begin

      if textfileformat['textfileformat'].Attribute_Str('codepage')='oem' then codepage:=0
      else if textfileformat['textfileformat'].Attribute_Str('codepage')='ansi' then codepage:=1
      else codepage:=2;

      for i:=0 to xml.SubNodesCount-1 do if xml.SubNodes[i].Name='record' then begin

        s:='';

        for j:=0 to fxml.SubNodesCount-1 do if fxml.SubNodes[j].Name='field' then begin

          fn:=fxml.SubNodes[j].Attribute_Str('name');
          l:=fxml.SubNodes[j].Attribute_Int('length');
          e:=fxml.SubNodes[j].Attribute_Int('startpos');
          f:=fxml.SubNodes[j].Attribute_Int('endpos');
          if l<=0 then l:=f-e+1;

          case codepage of
            0:v:=(Utf8ToAnsi(xml.SubNodes[i].Attribute_Str(fn)));
            1:v:=Utf8ToAnsi(xml.SubNodes[i].Attribute_Str(fn));
            2:v:=xml.SubNodes[i].Attribute_Str(fn);
          end;

          while length(s)<e-1 do s:=s+' ';

          if fxml.SubNodes[j].Attribute_Str('align')='left' then begin
            while length(v)<l do v:=v+' ';
          end else begin
            while length(v)<l do v:=' '+v;
          end;
          s:=s+v;

        end;

        for k:=1 to length(s) do fs.Write(byte(s[k]),1);
        n:=#13;
        fs.Write(byte(n),1);
        n:=#10;
        fs.Write(byte(n),1);

      end;

    end;

//    fs.CopyFrom(ms,ms.Size);

  finally
    ms.Free;
    fs.Free;
  end;
end;

function TXMLNode.Get(index:AnsiString):TXMLNode;
  var
    i:Integer;
begin
  index:=UpperCase(index);
  for i:=0 to SubNodesCount-1 do
    if index=UpperCase(SubNodesArray^[i].Name) then begin
      result:=SubNodesArray^[i];
      exit;
    end;
  result:=emptynode;
end;

function TXMLNode.Attribute(index:AnsiString):PXMLAttribute;
  var
    i:Integer;
begin
  index:=UpperCase(index);
  for i:=0 to AttributesCount-1 do
    if index=UpperCase(Attributes[i].Name) then begin
      result:=@Attributes[i];
      exit;
    end;
  result:=@emptyattr;
end;

function TXMLNode.Attribute_Is(index:AnsiString):boolean;
  var
    i:Integer;
begin
  index:=UpperCase(index);
  for i:=0 to AttributesCount-1 do
    if index=UpperCase(Attributes[i].Name) then begin
      result:=true;
      exit;
    end;
  result:=false;
end;

function TXMLNode.Attribute_Str(index:AnsiString):AnsiString;
  var
    i:Integer;
begin
  index:=UpperCase(index);
  for i:=0 to AttributesCount-1 do
    if index=Attributes[i].UCaseName then begin
      result:=Attributes[i].Value;
      exit;
    end;
  result:='';
end;

function TXMLNode.Attribute_B64Str(index:AnsiString):AnsiString;
  var
    i:Integer;
begin
  index:=UpperCase(index);
  for i:=0 to AttributesCount-1 do
    if index=Attributes[i].UCaseName then begin
      result:=DecodeBase64(Attributes[i].Value);
      exit;
    end;
  result:='';
end;

function TXMLNode.Attribute_Int(index:AnsiString):Integer;
  var
    i:Integer;
begin
  index:=UpperCase(index);
  for i:=0 to AttributesCount-1 do
    if index=Attributes[i].UCaseName then begin
      result:=StrToIntDef(Attributes[i].Value,0);
      exit;
    end;
  result:=0;
end;

function TXMLNode.Attribute_Float(index:AnsiString):Double;
  var
    i:Integer;
begin
  index:=UpperCase(index);
  for i:=0 to AttributesCount-1 do
    if index=Attributes[i].UCaseName then begin
      result:=StrToFloatDef(Attributes[i].Value,0);
      exit;
    end;
  result:=0;
end;

function TXMLAttribute.Gs:AnsiString;
begin
  result:=Value;
end;

function TXMLAttribute.Gi:Integer;
begin
  result:=StrToIntDef(Value,0);
end;

function TXMLAttribute.Gf:Single;
  var
    s:AnsiString;
begin
  DecimalSeparator:='.';
  result:=StrToFloatDef(Value,0);
end;

procedure TXMLAttribute.EqV(a:AnsiString);
begin
  Value:=a;
end;

procedure TXMLAttribute.EqV(a:Integer);
begin
  Value:=IntToStr(a);
end;

procedure TXMLAttribute.EqV(a:Double);
begin
  DecimalSeparator:='.';
  Value:=FloatToStr(a);
end;

{******************************************************************************}
// Base64 encoding
function EncodeBase64(const inStr: AnsiString):AnsiString;

  function Encode_Byte(b: Byte): char;
    const
      Base64Code: string[64] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  begin
    Result := Base64Code[(b and $3F)+1];
  end;

var
  i: Integer;
begin
  {$Q-}
  {$R-}
  i := 1;
  Result:= '';
  while i <= Length(InStr) do
  begin
    Result := Result + Encode_Byte(Byte(inStr[i]) shr 2);
    Result := Result + Encode_Byte((Byte(inStr[i]) shl 4) or (Byte(inStr[i+1]) shr 4));
    if i+1 <= Length(inStr) then Result := Result + Encode_Byte((Byte(inStr[i+1]) shl 2) or (Byte(inStr[i+2]) shr 6))
    else Result := Result + '=';
    if i+2 <= Length(inStr) then Result := Result + Encode_Byte(Byte(inStr[i+2]))
    else Result := Result + '=';
    Inc(i, 3);
  end;
  {$Q+}
  {$R+}
end;
{******************************************************************************}
// Base64 decoding
function DecodeBase64(const CinLine: AnsiString):AnsiString;
const
  RESULT_ERROR = -2;
var
  inLineIndex: Integer;
  c: Char;
  x: SmallInt;
  c4: Word;
  StoredC4: array[0..3] of SmallInt;
  InLineLength: Integer;
begin
  {$Q-}
  {$R-}
  Result := '';
  inLineIndex := 1;
  c4 := 0;
  InLineLength := Length(CinLine);

  while inLineIndex <=InLineLength do
  begin
    while (inLineIndex <= InLineLength) and (c4 < 4) do
    begin
      c := CinLine[inLineIndex];
      case c of
        '+'     : x := 62;
        '/'     : x := 63;
        '0'..'9': x := Ord(c) - (Ord('0')-52);
        '='     : x := -1;
        'A'..'Z': x := Ord(c) - Ord('A');
        'a'..'z': x := Ord(c) - (Ord('a')-26);
      else
        x := RESULT_ERROR;
      end;
      if x <> RESULT_ERROR then
      begin
        StoredC4[c4] := x;
        Inc(c4);
      end;
      Inc(inLineIndex);
    end;

    if c4 = 4 then
    begin
      c4 := 0;
      Result := Result + Char((StoredC4[0] shl 2) or (StoredC4[1] shr 4));
      if StoredC4[2] = -1 then Exit;
      Result := Result + Char((StoredC4[1] shl 4) or (StoredC4[2] shr 2));
      if StoredC4[3] = -1 then Exit;
      Result := Result + Char((StoredC4[2] shl 6) or (StoredC4[3]));
    end;
  end;
  {$Q+}
  {$R+}
end;
{******************************************************************************}
initialization
  emptynode:=TXMLNode.Create;
  emptynode.IsNotEmpty:=false;
finalization
  emptynode.Destroy;

end.
