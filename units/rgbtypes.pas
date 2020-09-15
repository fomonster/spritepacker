{
 /***************************************************************************
                                  RGBTypes.pas


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
    TRGB32Pixel - TRGB32Bitmap picture element, contains red, green and blue
      component and is platform dependent!
    TRGB32BitmapCore - core of TRGB32Bitmap.
}
unit RGBTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPImage, IntfGraphics;
  
type
  PRGB32Pixel = ^TRGB32Pixel;
  TRGB32Pixel = DWord;

  PRGB32PixelArray = ^TRGB32PixelArray;
  TRGB32PixelArray = packed array [0..0] of TRGB32Pixel;
  

  TPixelDifference = Word;
const
  MAXDIFFERENCE = 255 + 255 + 255;
type

  { TRGB32BitmapCore }

  TRGB32BitmapCore = class(TPersistent)
  private
    FPixels: PRGB32Pixel;
    FWidth: Integer;
    FHeight: Integer;
    FRowPixelStride: Integer;
  public
    constructor Create(AWidth, AHeight: Integer); virtual;
    constructor CreateAsCopy(ABitmap: TRGB32BitmapCore); virtual;
    constructor CreateFromData(AData: Pointer; AWidth, AHeight: Integer); virtual;
    constructor CreateFromLazIntfImage(AImage: TLazIntfImage); virtual;
    destructor Destroy; override;
    
    procedure Assign(Source: TPersistent); override;
    procedure ReplaceWith(ABitmap: TRGB32BitmapCore);
  public
    function GetPixelPtrUnsafeInline(X, Y: Integer): PRGB32Pixel; inline;
    function GetPixelPtrUnsafe(X, Y: Integer): PRGB32Pixel;
    function GetPixelPtr(X, Y: Integer): PRGB32Pixel;
    function GetPixelPtrInline(X, Y: Integer): PRGB32Pixel; inline;
    function GetPixelUnsafe(X, Y: Integer): TRGB32Pixel;

    procedure SetPixelUnsafeInline(X, Y: Integer; Value: TRGB32Pixel); inline;
    procedure SetPixelUnsafe(X, Y: Integer; Value: TRGB32Pixel);
    procedure SetPixelInline(X, Y: Integer; Value: TRGB32Pixel); inline;
    procedure SetPixel(X, Y: Integer; Value: TRGB32Pixel);
  public
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Pixels: PRGB32Pixel read FPixels;
    property RowPixelStride: Integer read FRowPixelStride;
  end;
  
  function RGB32PixelToColor(P: TRGB32Pixel): TColor;
  function ColorToRGB32Pixel(C: TColor): TRGB32Pixel;

  function GetRedInline(P: TRGB32Pixel): Byte; inline;
  function GetGreenInline(P: TRGB32Pixel): Byte; inline;
  function GetBlueInline(P: TRGB32Pixel): Byte; inline;
  function RGBToRGB32PixelInline(R, G, B: Byte): TRGB32Pixel; inline;

  function RGB32PixelToColorInline(P: TRGB32Pixel): TColor; inline;
  function ColorToRGB32PixelInline(C: TColor): TRGB32Pixel; inline;

  function RGB32PixelDifferenceInline(A, B: TRGB32Pixel): TPixelDifference; inline;

implementation


function GetRedInline(P: TRGB32Pixel): Byte; inline;
begin
  {$IFDEF Win32}
  Result := (P and $FF0000) shr 16;
  {$ELSE}
  Result := P and $FF;
  {$ENDIF}
end;

function GetGreenInline(P: TRGB32Pixel): Byte; inline;
begin
  {$IFDEF Win32}
  Result := (P and $FF00) shr 8;
  {$ELSE}
  Result := (P and $FF00) shr 8;
  {$ENDIF}
end;

function GetBlueInline(P: TRGB32Pixel): Byte; inline;
begin
  {$IFDEF Win32}
  Result := P and $FF;
  {$ELSE}
  Result := (P and $FF0000) shr 16;
  {$ENDIF}
end;

function RGBToRGB32PixelInline(R, G, B: Byte): TRGB32Pixel; inline;
begin
  {$IFDEF Win32}
  Result := B or (G shl 8) or (R shl 16);
  {$ELSE}
  Result := R or (G shl 8) or (B shl 16);
  {$ENDIF}
end;

// TODO: check on big-endian arch.
function RGB32PixelToColorInline(P: TRGB32Pixel): TColor; inline;
begin
  {$IFDEF Win32}
  Result := ((P and $FF0000) shr 16) or (P and $FF00) or ((P and $FF) shl 16);
  {$ELSE}
  Result := P and $FFFFFF;
  {$ENDIF}
end;

function ColorToRGB32PixelInline(C: TColor): TRGB32Pixel; inline;
begin
  {$IFDEF Win32}
  Result := ((C and $FF0000) shr 16) or (C and $FF00) or ((C and $FF) shl 16);
  {$ELSE}
  Result := C and $FFFFFF;
  {$ENDIF}
end;

function FPColorToRGB32PixelInline(F: TFPColor): TRGB32Pixel; inline;
begin
  {$IFDEF Win32}
  Result := ((F.Blue shr 8) and $FF) or (F.Green and $FF00) or ((F.Red shl 8) and $FF0000);
  {$ELSE}
  Result := ((F.Red shr 8) and $FF) or (F.Green and $FF00) or ((F.Blue shl 8) and $FF0000);
  {$ENDIF}
end;

function RGB32PixelDifferenceInline(A, B: TRGB32Pixel): TPixelDifference; inline;
begin
  Result := Abs(((A and $FF0000) shr 16) - ((B and $FF0000) shr 16))
    + Abs(((A and $FF00) shr 8) - ((B and $FF00) shr 8))
    + Abs((A and $FF) - (B and $FF));
end;

function RGB32PixelToColor(P: TRGB32Pixel): TColor;
begin
  Result := RGB32PixelToColorInline(P);
end;

function ColorToRGB32Pixel(C: TColor): TRGB32Pixel;
begin
  Result := ColorToRGB32PixelInline(C);
end;

{ TRGB32BitmapCore }

constructor TRGB32BitmapCore.Create(AWidth, AHeight: Integer);
begin
  inherited Create;
  
  FWidth := AWidth;
  FHeight := AHeight;
  // TODO: check on 64-bit arch.
  FRowPixelStride := AWidth;

  GetMem(FPixels, FHeight * FRowPixelStride * SizeOf(TRGB32Pixel));
end;

constructor TRGB32BitmapCore.CreateAsCopy(ABitmap: TRGB32BitmapCore);
begin
  inherited Create;
  
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  FRowPixelStride := ABitmap.RowPixelStride;

  GetMem(FPixels, FHeight * FRowPixelStride * SizeOf(TRGB32Pixel));
  Move(ABitmap.Pixels^, FPixels^, FHeight * FRowPixelStride * SizeOf(TRGB32Pixel));
end;

constructor TRGB32BitmapCore.CreateFromData(AData: Pointer; AWidth,
  AHeight: Integer);
begin
  inherited Create;
  
  FWidth := AWidth;
  FHeight := AHeight;
  // TODO: check on 64-bit arch.
  FRowPixelStride := AWidth;

  FPixels := AData;
end;

constructor TRGB32BitmapCore.CreateFromLazIntfImage(AImage: TLazIntfImage);
var
  I, J: Integer;
  P, PLine: PRGB32Pixel;
begin
  Create(AImage.Width, AImage.Height);

  PLine := Pixels;
  for J := 0 to Pred(Height) do
  begin
    P := PLine;
    for I := 0 to Pred(Width) do
    begin
      P^ := FPColorToRGB32PixelInline(AImage.Colors[I, J]);
      Inc(P);
    end;
    Inc(PLine, RowPixelStride);
  end;
end;

destructor TRGB32BitmapCore.Destroy;
begin
  FreeMem(FPixels);
  inherited;
end;

procedure TRGB32BitmapCore.Assign(Source: TPersistent);
begin
  if Source = nil then Exit;
  if Source = Self then Exit;
  if Source is TRGB32BitmapCore then
  begin
    FreeMem(FPixels);

    FWidth := (Source as TRGB32BitmapCore).Width;
    FHeight := (Source as TRGB32BitmapCore).Height;
    FRowPixelStride := (Source as TRGB32BitmapCore).RowPixelStride;

    GetMem(FPixels, FHeight * FRowPixelStride * SizeOf(TRGB32Pixel));
    Move((Source as TRGB32BitmapCore).Pixels^, FPixels^,
      FHeight * FRowPixelStride * SizeOf(TRGB32Pixel));
  end
  else
    inherited Assign(Source);
end;

procedure TRGB32BitmapCore.ReplaceWith(ABitmap: TRGB32BitmapCore);
begin
  if ABitmap = nil then Exit;
  FreeMem(FPixels);

  FPixels := ABitmap.Pixels;
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  FRowPixelStride := ABitmap.RowPixelStride;
end;

function TRGB32BitmapCore.GetPixelPtrUnsafeInline(X, Y: Integer): PRGB32Pixel;
begin
  Result := FPixels;
  Inc(Result, Y * FRowPixelStride + X);
end;

function TRGB32BitmapCore.GetPixelPtrUnsafe(X, Y: Integer): PRGB32Pixel;
begin
  Result := GetPixelPtrUnsafeInline(X, Y);
end;

function TRGB32BitmapCore.GetPixelPtrInline(X, Y: Integer): PRGB32Pixel;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
    Result := GetPixelPtrUnsafeInline(X, Y)
  else
    Result := nil;
end;

function TRGB32BitmapCore.GetPixelPtr(X, Y: Integer): PRGB32Pixel;
begin
  Result := GetPixelPtrInline(X, Y);
end;

function TRGB32BitmapCore.GetPixelUnsafe(X, Y: Integer): TRGB32Pixel;
begin
  Result := GetPixelPtrUnsafeInline(X, Y)^;
end;

procedure TRGB32BitmapCore.SetPixelUnsafeInline(X, Y: Integer; Value: TRGB32Pixel);
begin
  GetPixelPtrUnsafeInline(X, Y)^ := Value;
end;

procedure TRGB32BitmapCore.SetPixelUnsafe(X, Y: Integer; Value: TRGB32Pixel);
begin
  SetPixelUnsafeInline(X, Y, Value);
end;

procedure TRGB32BitmapCore.SetPixelInline(X, Y: Integer; Value: TRGB32Pixel);
var
  P: PRGB32Pixel;
begin
  P := GetPixelPtrInline(X, Y);
  if P <> nil then P^ := Value;
end;

procedure TRGB32BitmapCore.SetPixel(X, Y: Integer; Value: TRGB32Pixel);
begin
  SetPixelInline(X, Y, Value);
end;

end.

