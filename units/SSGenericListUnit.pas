{*******************************************************************************

  Проект: SpaceSim
  Автор: Фомин С.С.
  Дата: 2009 год

  Назначение модуля:


*******************************************************************************}
unit SSGenericListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

TVectorItem = class
private
public
  id:Integer;
  //name:String;
  IsSelected:boolean;

  constructor Create;virtual;
  destructor Destroy;virtual;
end;

TVectorItemArray=array [0..500000000] of TVectorItem;
PVectorItemArray=^TVectorItemArray;

generic TGenericVector<_ListItemClass_> = class

private
  //procedure _quicksort(l,r:Integer);
  _length:Integer;

public

  GenericListArray:PVectorItemArray;


  constructor Create;
  destructor Destroy;

  procedure Clear;
  procedure SetLength(NewLength:Integer);
  function length:integer;

  function Get(index:Integer):_ListItemClass_;
  procedure Put(index:Integer;const value:_ListItemClass_);

  function Add:_ListItemClass_;overload;
  procedure Add(item:_ListItemClass_);overload;

  function Insert(index:Integer):_ListItemClass_;overload;
  function Insert(index:Integer;item:_ListItemClass_):_ListItemClass_;overload;
  function Push:_ListItemClass_;overload;
  procedure Push(item:_ListItemClass_);overload;
  function Pop:_ListItemClass_;
  procedure Delete(index:Integer);
  procedure Splice(index:Integer;count:integer);


  //
  procedure DeleteSelected;

//  procedure ShellSort;
  //procedure Sort;

  //function GetItemId(name:String):Integer;
  //function GetItem(name:String):_ListItemClass_;

  property item[index : Integer]:_ListItemClass_ read Get;default;

end;

implementation

constructor TVectorItem.Create;
begin
  {name:='';}
  IsSelected:=false;
  id:=0;
end;

destructor TVectorItem.Destroy;
begin

end;

constructor TGenericVector.Create;
begin
  GenericListArray:=nil;
  _length:=0;
end;

destructor TGenericVector.Destroy;
begin
  Clear;
end;

procedure TGenericVector.Clear;
  var
    i:Integer;
begin
  for i:=0 to Length-1 do
    if ( GenericListArray^[i] <> nil ) then
      _ListItemClass_(GenericListArray^[i]).Destroy;
  _length:=0;
  Freemem(GenericListArray);
  GenericListArray:=nil;
end;

procedure TGenericVector.SetLength(NewLength:Integer);
  var
    i:Integer;
begin
  if NewLength = _length then exit;
  if NewLength > 0 then begin
    if NewLength < _length then begin
      for i:=NewLength to _length-1 do
        if ( GenericListArray^[i] <> nil ) then
          _ListItemClass_(GenericListArray^[i]).Destroy;
    end;
    ReAllocMem(GenericListArray,sizeof(_ListItemClass_) * NewLength);
    if NewLength > _length then begin
      for i:=_length to NewLength-1 do begin
        GenericListArray^[i]:=_ListItemClass_.Create;
        GenericListArray^[i].id:=i;
      end;
    end;
    _length:=NewLength;
  end else begin
    Clear;
  end;
end;

function TGenericVector.length:integer;
begin
  result:=_length;
end;

function TGenericVector.Get(index:Integer):_ListItemClass_;
begin
  if (GenericListArray<>nil) and (index>=0) and (index<_length)
    then result:=_ListItemClass_(GenericListArray^[index])
    else result:=nil;
end;

procedure TGenericVector.Put(index:Integer;const value:_ListItemClass_);
begin
  if (index>=0) and (index < _length)
    then GenericListArray^[index]:=value;
end;

function TGenericVector.Add:_ListItemClass_;
begin
  SetLength(length+1);
  Result:=_ListItemClass_(GenericListArray^[_length-1]);
end;

procedure TGenericVector.Add(item:_ListItemClass_);
begin
  _length:=_length+1;
  ReAllocMem(GenericListArray,sizeof(_ListItemClass_) * _length);
  GenericListArray^[_length-1]:=item;
  item.id:=_length-1;
end;

function TGenericVector.Insert(index:Integer):_ListItemClass_;
begin
  if (index>=0) and (index<=_length) then begin
    ReAllocMem(GenericListArray,sizeof(_ListItemClass_) * (_length+1));
    if index < _length then begin
      Move(GenericListArray^[index],GenericListArray^[index+1],(_length-index)*sizeof(_ListItemClass_));
    end;
    GenericListArray^[index]:=_ListItemClass_.Create;
    inc(_length);
    result:=_ListItemClass_(GenericListArray^[index]);
  end else result:=nil;
end;

function TGenericVector.Insert(index:Integer;item:_ListItemClass_):_ListItemClass_;
begin
  if (index>=0) and (index<=_length) then begin
    ReAllocMem(GenericListArray,sizeof(_ListItemClass_) * (_length+1));
    if index < _length then begin
      Move(GenericListArray^[index],GenericListArray^[index+1],(_length-index)*sizeof(_ListItemClass_));
    end;
    GenericListArray^[index]:=item;
    inc(_length);
    result:=item;
  end else result:=nil;
end;

function TGenericVector.Push:_ListItemClass_;
begin
  SetLength(_length+1);
  Result:=_ListItemClass_(GenericListArray^[_length-1]);
end;

procedure TGenericVector.Push(item:_ListItemClass_);
begin
  _length:=_length+1;
  ReAllocMem(GenericListArray,sizeof(_ListItemClass_) * _length);
  GenericListArray^[_length-1]:=item;
  item.id:=_length-1;
end;

function TGenericVector.Pop:_ListItemClass_;
begin
  if ( length > 0 ) then begin
    result:=_ListItemClass_(GenericListArray^[_length-1]);
    _length:=_length-1;
    ReAllocMem(GenericListArray,sizeof(_ListItemClass_) * _length);
  end else result:=nil;
end;

procedure TGenericVector.Delete(index:Integer);
begin
  if (index>=0) and (index<_length) then begin
    if ( GenericListArray^[index] <> nil ) then
      _ListItemClass_(GenericListArray^[index]).Destroy;
    if index < _length-1 then begin
      Move(GenericListArray^[index+1],GenericListArray^[index],(_length-index-1)*sizeof(_ListItemClass_));
    end;
    _length:=_length-1;
    ReAllocMem(GenericListArray,sizeof(_ListItemClass_) * _length);
  end;
end;

procedure TGenericVector.Splice(index:Integer;count:integer);
  var
    i:Integer;
begin
  if (index>=0) and (index<length) and ( count > 0 ) then begin
    if ( count > _length - index ) then count:=_length-index;
    for i:=index to index+count-1 do
      if GenericListArray^[index] <> nil then
        _ListItemClass_(GenericListArray^[i]).Destroy;
    if index < _length-count then begin
      Move(GenericListArray^[index+count],GenericListArray^[index],(_length-index-count)*sizeof(_ListItemClass_));
    end;
    _length:=_length-count;
    ReAllocMem(GenericListArray,sizeof(_ListItemClass_) * _length);
  end;
end;

// Процедурка для удаления множества элементов помеченных на удаление
procedure TGenericVector.DeleteSelected;
  var
    i,j,k:Integer;
begin
  j:=-1;
  k:=0;
  for i:=0 to _length-1 do begin
    if ( GenericListArray^[i] <> nil ) and ( GenericListArray^[i].IsSelected ) then begin
      _ListItemClass_(GenericListArray^[i]).Destroy;
      inc(k);
      if j=-1 then j:=i;
    end else begin
      if j>=0 then begin
        GenericListArray^[j]:=GenericListArray^[i];
        j:=j+1;
      end;
    end;
  end;
  _length:=_length-k;
  if k>0 then ReAllocMem(GenericListArray,sizeof(_ListItemClass_)*_length);
end;

// Сортировка Шелла - по именам моделей
{procedure TSSGenericList.ShellSort;
  var
    d,i,j:Integer;
    k:boolean;
    t:_ListItemClass_;
begin
  d:=length div 2;
  while d>0 do begin
    k:=true;
    while k do begin
      k:=false;
      for i:=0 to length-d-1 do begin
        j:=i+d;
        if GenericListArray^[i].name>GenericListArray^[j].name then begin
          t:=_ListItemClass_(GenericListArray^[i]);
          GenericListArray^[i]:=GenericListArray^[j];
          GenericListArray^[j]:=t;
          k:=true;
        end;
      end;
    end;
    d:=d div 2;
  end;
end;
}
{
procedure TGenericVector._quicksort(l,r:Integer);
   var
     i,j: integer;
     x:AnsiString;
     t:_ListItemClass_;
begin
  i := l;
  j := r;
  x :=  GenericListArray^[ (l + r) div 2 ].name;
  repeat
    while GenericListArray^[i].name < x do inc(i);
    while x < GenericListArray^[j].name do dec(j);
    if i<=j then begin
      t:=_ListItemClass_(GenericListArray^[i]);
      GenericListArray^[i] := GenericListArray^[j];
      GenericListArray^[j] := t;
      inc(i);
      dec(j);
    end;
  until i>j;
  if l < j then _quicksort(l,j);
  if i < r then _quicksort(i,r);
end;

procedure TGenericVector.Sort;
begin
  if length=0 then exit;
  _quicksort(0,length-1);
end; }

// Поиск методом деления попалам
{function TGenericVector.GetItemId(name:String):Integer;
  var
    a,b,c:Integer;
begin
  result:=-1;
  name:=LowerCase(name);
  a:=1;
  b:=length;
  if length<=0 then exit;
  c:=(a+b) div 2;
  while b-a>1 do begin
    c:=(a+b) div 2;
    if GenericListArray^[c-1].name>name then b:=c
    else a:=c;
  end;
  if GenericListArray^[a-1].name=name then result:=a-1
  else if GenericListArray^[b-1].name=name then result:=b-1;
  if result<0 then begin
    result:=0;
  end;
end;}

{function TGenericVector.GetItem(name:String):_ListItemClass_;
  var
    a,b,c:Integer;
begin
  result:=nil;
  name:=LowerCase(name);
  a:=1;
  b:=length;
  c:=(a+b) div 2;
  while b-a>1 do begin
    c:=(a+b) div 2;
    if GenericListArray^[c-1].name>name then b:=c
    else a:=c;
  end;
  if GenericListArray^[a-1].name=name then result:=_ListItemClass_(GenericListArray^[a-1])
  else if GenericListArray^[b-1].name=name then result:=_ListItemClass_(GenericListArray^[b-1]);
end;}


end.

