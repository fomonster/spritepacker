{*******************************************************************************

  Проект: SpaceSim
  Автор: Фомин С.С.
  Дата: 2009 год

  Назначение модуля:

   XML - парсер. Читает все виды XML файлов.

*******************************************************************************}
unit SSXMLUnit;

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

    procedure SubNodes_Clear;
    procedure SubNodes_SetLength(NewLength:Integer);
    function SubNodes_Add:TXMLNode;
    function SubNodes_Get(index:Integer):TXMLNode;

    procedure Attributes_Clear;
    procedure Attribute_SetLength(NewLength:Integer);
    function Attribute_Add:PXMLAttribute;overload;
    function Attribute_Add(_name:AnsiString):PXMLAttribute;overload;
    function Attribute(index:AnsiString):PXMLAttribute;
    function Attribute_Is(index:AnsiString):boolean;
    function Attribute_Str(index:AnsiString):AnsiString;
    function Attribute_Int(index:AnsiString):Integer;
    function Attribute_Float(index:AnsiString):Double;

    procedure LoadFromStream(stream:TStream);
    procedure SaveToStream(stream:TStringStream);

    procedure LoadFromXMLFile(filename:AnsiString);
    procedure SaveToXMLFile(filename:AnsiString);
    procedure LoadFromXMLString(s:AnsiString);
    function SaveToXMLString:AnsiString;

    function Parse(stream:TStream;nodename:AnsiString):boolean;
    procedure Writenode(var fl:TextFile;lev:Integer);

    function Get(index:AnsiString):TXMLNode;
    property item[index : AnsiString]:TXMLNode read Get;default;
    property SubNodes[index : Integer]:TXMLNode read SubNodes_Get;

  end;

implementation

var
  ltagtype:Integer;
  emptynode:TXMLNode = nil;
  emptyattr:TXMLAttribute;

//  fla:TextFile;

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
  b:=fs.ReadByte;
  while (b<=32)and(fs.Position<fs.Size) do b:=fs.ReadByte;
  c:=char(b);
  while (ltagtype=-3)and(c<>'<')and(fs.Position<fs.Size) do begin
    c:=char(fs.ReadByte);
    if (fs.Position>=fs.Size) then begin
      goto ennddd;
      exit;
    end;
  end;
  // Текст между тегами
  if ((ltagtype=TG_END) or (ltagtype=TG_ENDBEGIN)) and (c<>'<') then begin
   if (fs.Position<fs.Size) then begin
      tag:=tag+c;
      c:=char(fs.ReadByte);
      while (c<>'<') and (fs.Position<fs.Size) do begin
        tag:=tag+c;
        c:=char(fs.ReadByte);
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
      c:=char(fs.ReadByte);
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
      c:=char(fs.ReadByte);
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
      c:=char(fs.ReadByte);
      while (fs.Position<fs.Size) do begin
        if c='"' then begin
          if (fs.Position<fs.Size) then begin
            c:=char(fs.ReadByte);
            if c<>'"' then begin
              fs.Position:=fs.Position-1;
              break;
            end;
          end else break;
        end;
        tag:=tag+c;
        c:=char(fs.ReadByte);
      end;
      tagtype:=TG_AnsiString; // "..."
      ltagtype:=tagtype;
      goto ennddd;
      exit;
    end;
  end;
  // комментарии //...
  if (c='/') and (fs.Position<fs.Size) then begin
    c1:=char(fs.ReadByte);
    if c1='/' then begin
      tag:=tag+c1;
      while (c<>char(10)) and (fs.Position<fs.Size) do begin
        tag:=tag+c;
        c:=char(fs.ReadByte);
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
    c:=char(fs.ReadByte);
llll:
    if (c='?') or (c='!') then begin
      while c<>'<' do begin
        if (fs.Position>=fs.Size-1) then begin
          tag:='';
          tagtype:=TG_EOS;
          goto ennddd;
          exit;
        end;
        c:=char(fs.ReadByte);
      end;
      c:=char(fs.ReadByte);
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
  Name:='';
  UCaseName:='';
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

procedure TXMLNode.Attributes_Clear;
begin
  Attributes:=nil;
end;

procedure TXMLNode.Attribute_SetLength(NewLength:Integer);
begin
  if NewLength>0 then begin
    SetLength(Attributes,NewLength);
    AttributesCount:=NewLength;
  end else Attributes_Clear;
end;

function TXMLNode.Attribute_Add:PXMLAttribute;overload;
begin
  Attribute_SetLength(AttributesCount+1);
  result:=@Attributes[AttributesCount-1];
end;

function TXMLNode.Attribute_Add(_name:AnsiString):PXMLAttribute;overload;
  var
    i:Integer;
    _nam:AnsiString;
begin
  _nam:=UpperCase(_name);
  for i:=0 to AttributesCount-1 do
    if _nam=Attributes[i].UCaseName then begin
      result:=@Attributes[i];
      exit;
    end;
  Attribute_SetLength(AttributesCount+1);
  Attributes[AttributesCount-1].Name:=_name;
  Attributes[AttributesCount-1].UCaseName:=UpperCase(_name);
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
//          attr^.Name:=attrname;
//          attr^.UCaseName:=UpperCase(attrname);
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
        node.Name:=tag;
        node.UCaseName:=UpperCase(tag);
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

procedure TXMLNode.LoadFromStream(stream:TStream);
  var

    tagid,i:Integer;
    tag:AnsiString;
    node:TXMLNode;
begin
//  AssignFile(fla,'m:\bak.txt');
//  rewrite(fla);

  IsLoaded:=false;
  ltagtype:=-3;
  Name:='';
  Text:='';
  ReadXMLTag(stream,tag,tagid);
  while tagid<>TG_EOS do begin
    if tagid=TG_BEGIN then begin
      ReadXMLTag(stream,tag,tagid);
      if tagid=TG_NAME then begin
        node:=SubNodes_Add;
        node.Name:=tag;
        if not node.Parse(stream,LowerCase(node.Name)) then break;
      end else break;
    end else break;
    ReadXMLTag(stream,tag,tagid);
  end;
  IsLoaded:=true;

//s  CloseFile(fla);
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


initialization
  emptynode:=TXMLNode.Create;
  emptynode.IsNotEmpty:=false;
finalization
  emptynode.Destroy;

end.
