{
 /***************************************************************************
                                  RGBUtils.pas


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author:  Tom Gregorovic (_tom_@centrum.cz)

  Abstract:
}
unit RGBUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type
  TIntArray = array of Integer;
  
  function DivideTrunc(Src, Dest: Integer): TIntArray;
  
  procedure MinMax(var A, B: Integer); inline;

  procedure SortRect(var X1, Y1, X2, Y2: Integer);
  procedure SortRect(var R: TRect);
  
  procedure ClipDimension(ClipMin, ClipMax: Integer;
    var DstPos, SrcPos, SrcSize: Integer);

  operator =(A, B: TPoint): Boolean;

implementation

function DivideTrunc(Src, Dest: Integer): TIntArray;
var
  I: Integer;
  E, C, M, D, DM: Integer;
begin
  if Dest > Src then
  begin
    C := Src;
    SetLength(Result, Src);
    D := Dest div Src;
    M := Dest mod Src;
  end
  else
  begin
    C := Dest;
    SetLength(Result, Dest);
    D := Src div Dest;
    M := Src mod Dest;
  end;

  if M <= C shr 1 then
  begin
    DM := Succ(M);
    E := DM;

    for I := 0 to High(Result) do
    begin
      if (M > 0) and (E >= C) then
      begin
        Result[I] := Succ(D);
        Dec(M);
        Inc(E, DM - C);
      end
      else
      begin
        Result[I] := D;
        Inc(E, DM);
      end;
    end;
  end
  else
  begin
    M := C - M;
    DM := Succ(M);
    E := DM;

    for I := 0 to High(Result) do
    begin
      if (M > 0) and (E >= C) then
      begin
        Result[I] := D;
        Dec(M);
        Inc(E, DM - C);
      end
      else
      begin
        Result[I] := Succ(D);
        Inc(E, DM);
      end;
    end;
  end;
end;

operator =(A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

procedure MinMax(var A, B: Integer); inline;
var
  T: Integer;
begin
  if A > B then
  begin
    T := A;
    A := B;
    B := T;
  end;
end;

procedure SortRect(var X1, Y1, X2, Y2: Integer);
begin
  MinMax(X1, X2);
  MinMax(Y1, Y2);
end;

procedure SortRect(var R: TRect);
begin
  MinMax(R.Left, R.Right);
  MinMax(R.Top, R.Bottom);
end;

procedure ClipDimension(ClipMin, ClipMax: Integer;
    var DstPos, SrcPos, SrcSize: Integer);
var
  C: Integer;
begin
  if ClipMin > DstPos then
  begin
    C := ClipMin - DstPos;
    Inc(SrcPos, C);
    Dec(SrcSize, C);
    DstPos := ClipMin;
  end;
  
  if ClipMax < DstPos + SrcSize then SrcSize := ClipMax - DstPos;
end;
  

end.

