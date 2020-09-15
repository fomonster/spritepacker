{
Based on the Public Domain MaxRectanglesBinPack.cpp source by Jukka JylГ¤nki
https://github.com/juj/RectangleBinPack/

Based on C# port by Sven Magnus
http://unifycommunity.com/wiki/index.php?title=MaxRectanglesBinPack


Ported to ActionScript3 by DUZENGQIANG
http://www.duzengqiang.com/blog/post/971.html
This version is also public domain - do whatever you want with it.
}
unit MaxRectsBinPackUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SSGenericListUnit;

const
  METHOD_BESTSHORTSIDEFIT:integer = 0; ///< -BSSF: Positions the Rectangle against the short side of a free Rectangle into which it fits the best.
  METHOD_BESTLONGSIDEFIT:integer = 1; ///< -BLSF: Positions the Rectangle against the long side of a free Rectangle into which it fits the best.
  METHOD_BESTAREAFIT:integer = 2; ///< -BAF: Positions the Rectangle into the smallest free Rectangle into which it fits.
  METHOD_BOTTOMLEFTRULE:integer = 3; ///< -BL: Does the Tetris placement.
  METHOD_CONTACTPOINTRULE:integer = 4; ///< -CP: Choosest the placement where the Rectangle touches other Rectangles as much as possible.



type

  TRectangle = class(TVectorItem)
  public
     data:pointer;
     x:integer;
     y:integer;
     width:integer;
     height:integer;
     rotated:boolean;
     constructor Create;overload;
     constructor Create(_width:integer; _height:integer);overload;
     function Clone:TRectangle;
  end;

  TRectangleList = specialize TGenericVector<TRectangle>;

  TMaxRectsBinPack = class
  private
    function contactPointScoreNode(x:integer; y:integer; width:integer; height:integer):integer;
  public
    binWidth:integer;
    binHeight:integer;
    allowRotations:boolean;

    usedRectangles:TRectangleList;
    freeRectangles:TRectangleList;

    score1:integer;
    score2:integer;


    constructor Init(width:integer; height:integer; rotations:boolean);
    destructor Destroy;

    function Count(n:integer):integer;
    function Insert(width:integer; height:integer;method:integer):TRectangle;
    procedure placeRectangle(node:TRectangle);
    function findPositionForNewNodeBestShortSideFit(width:integer; height:integer):TRectangle;
    function findPositionForNewNodeBottomLeft(width:integer; height:integer; bestX:integer; bestY:integer):TRectangle;
    function findPositionForNewNodeContactPoint(width:integer; height:integer; bestContactScore:integer):TRectangle;
    function findPositionForNewNodeBestLongSideFit(width:integer; height:integer; bestShortSideFit:integer; bestLongSideFit:integer):TRectangle;
    function findPositionForNewNodeBestAreaFit(width:integer; height:integer; bestAreaFit:integer; bestShortSideFit:integer):TRectangle;

    function splitFreeNode(freeNode:TRectangle; usedNode:TRectangle):Boolean;
    procedure pruneFreeList;

  end;

implementation

constructor TRectangle.Create;
begin
  inherited Create;
  x := 0;
  y := 0;
  width := 0;
  height := 0;
  rotated := false;
end;

constructor TRectangle.Create(_width:integer; _height:integer);
begin
  x := 0;
  y := 0;
  width := _width;
  height := _height;
  rotated := false;
end;

function TRectangle.Clone:TRectangle;
begin
  result:=TRectangle.Create;
  result.x := x;
  result.y := y;
  result.width := width;
  result.height := height;
end;


constructor TMaxRectsBinPack.Init(width:integer; height:integer; rotations:boolean);
begin
  score1 := 0;
  score2 := 0;

  if ( width < 256 ) then width := 256;
  if ( width > 16384 ) then width := 16384;
  if ( height < 256 ) then height := 256;
  if ( height > 16384 ) then height := 16384;

  binWidth := width;
  binHeight := height;
  allowRotations := rotations;

  usedRectangles := TRectangleList.Create;
  usedRectangles.Clear;

  freeRectangles := TRectangleList.Create;
  freeRectangles.Clear;

  freeRectangles.Push(TRectangle.Create(width,height));
end;

destructor TMaxRectsBinPack.Destroy;
begin
  usedRectangles.Destroy;
  usedRectangles := nil;

  freeRectangles.Destroy;
  freeRectangles := nil;
end;

function TMaxRectsBinPack.Count(n:integer):integer;
begin
  if ( n >= 2 ) then result:=Count(n div 2)
  else result:=n;
end;

function TMaxRectsBinPack.Insert(width:integer; height:integer;method:integer):TRectangle;
  var
    newNode:TRectangle;
begin
  newNode := TRectangle.Create(0,0);
  score1 := 0;
  score2 := 0;

  //
  if ( method = METHOD_BESTSHORTSIDEFIT ) then begin
    newNode := findPositionForNewNodeBestShortSideFit(width, height);
  end else if ( method = METHOD_BESTLONGSIDEFIT ) then begin
    newNode := findPositionForNewNodeBestLongSideFit(width, height, score2, score1);
  end else if ( method = METHOD_BESTAREAFIT ) then begin
    newNode := findPositionForNewNodeBestAreaFit(width, height, score1, score2);
  end else if ( method = METHOD_BOTTOMLEFTRULE ) then begin
    newNode := findPositionForNewNodeBottomLeft(width, height, score1, score2);
  end else begin
    newNode := findPositionForNewNodeContactPoint(width, height, score1);
  end;

  //
  if ( newNode.height = 0) then begin
    result:=newNode;
    exit;
  end;

  placeRectangle(newNode);
  result:=newNode;
end;

procedure TMaxRectsBinPack.placeRectangle(node:TRectangle);
  var
    i:integer;
    numRectanglesToProcess:integer;
begin
  numRectanglesToProcess := freeRectangles.Length;
  i:=0;
  while ( i < numRectanglesToProcess ) do begin
    if splitFreeNode(freeRectangles[i], node) then begin

      freeRectangles.splice(i,1);

      i:=i-1;
      numRectanglesToProcess := numRectanglesToProcess-1;
    end;
    i:=i+1;
  end;

  pruneFreeList();

  usedRectangles.Push(node);
end;

procedure TMaxRectsBinPack.pruneFreeList;

  function isContainedIn(a:TRectangle; b:TRectangle):Boolean;
  begin
    result := (a.x >= b.x) and (a.y >= b.y) and (a.x+a.width <= b.x + b.width) and (a.y + a.height <= b.y + b.height);
  end;

  var
    i:integer;
    j:integer;
begin
  i:=0;
  while i < freeRectangles.length-1 do begin
    j:=i+1;
    while j < freeRectangles.length do begin

      if ( isContainedIn(freeRectangles[i], freeRectangles[j]) ) then begin
        freeRectangles.Splice(i, 1);
        break;
      end;

      if ( isContainedIn(freeRectangles[j], freeRectangles[i]) ) then begin
        freeRectangles.Splice(j, 1);
      end;

      j:=j+1;
    end;

    i:=i+1;
  end;
end;

function imin(a:integer;b:integer):integer;
begin
  if ( a > b ) then result:=b
  else result:=a;
end;

function imax(a:integer;b:integer):integer;
begin
  if ( a > b ) then result:=a
  else result:=b;
end;

function iabs(a:integer):integer;
begin
  if ( a >= 0 ) then result:=a
  else result:=-a;
end;

function TMaxRectsBinPack.findPositionForNewNodeBestShortSideFit(width:integer; height:integer):TRectangle;
  var
    i:integer;
    bestNode:TRectangle;
    rect:TRectangle;
    leftoverHoriz:integer;
    leftoverVert:integer;
    shortSideFit:integer;
    longSideFit:integer;
    flippedLeftoverHoriz:integer;
    flippedLeftoverVert:integer;
    flippedShortSideFit:integer;
    flippedLongSideFit:integer;

    bestShortSideFit:integer;
    bestLongSideFit:integer;

begin
  bestNode := TRectangle.Create(0,0);

  bestShortSideFit := 2000000000;
  bestLongSideFit := score2;


  for i := 0 to freeRectangles.length-1 do begin
    rect := freeRectangles[i];
    // Try to place the Rectangle in upright (non-flipped) orientation.
    if ((rect.width >= width) and (rect.height >= height)) then begin
      leftoverHoriz := iabs(rect.width - width);
      leftoverVert := iabs(rect.height - height);
      shortSideFit := imin(leftoverHoriz, leftoverVert);
      longSideFit := imax(leftoverHoriz, leftoverVert);

      if ((shortSideFit < bestShortSideFit ) or ((shortSideFit = bestShortSideFit ) and ( longSideFit < bestLongSideFit))) then begin
	bestNode.x := rect.x;
	bestNode.y := rect.y;
	bestNode.width := width;
	bestNode.height := height;
        bestNode.rotated := false;
	bestShortSideFit := shortSideFit;
	bestLongSideFit := longSideFit;
      end;
    end;
    //
    if allowRotations and ( rect.width >= height ) and ( rect.height >= width ) then begin

      flippedLeftoverHoriz := iabs(rect.width - height);
      flippedLeftoverVert := iabs(rect.height - width);
      flippedShortSideFit := imin(flippedLeftoverHoriz, flippedLeftoverVert);
      flippedLongSideFit := imax(flippedLeftoverHoriz, flippedLeftoverVert);

      if (flippedShortSideFit < bestShortSideFit) or ((flippedShortSideFit = bestShortSideFit) and (flippedLongSideFit < bestLongSideFit)) then begin
	bestNode.x := rect.x;
	bestNode.y := rect.y;
	bestNode.width := height;
	bestNode.height := width;
        bestNode.rotated := true;
	bestShortSideFit := flippedShortSideFit;
	bestLongSideFit := flippedLongSideFit;
      end;
    end;

  end;
  result := bestNode;
end;

function TMaxRectsBinPack.findPositionForNewNodeBottomLeft(width:integer; height:integer; bestX:integer; bestY:integer):TRectangle;
  var
    i:integer;
    bestNode:TRectangle;
    rect:TRectangle;
    topSideY:integer;
begin
  bestNode := TRectangle.Create(0,0);
  bestY := 2000000000;
  for i := 0 to freeRectangles.length-1 do begin
    rect := freeRectangles[i];
    // Try to place the Rectangle in upright (non-flipped) orientation.
    if (rect.width >= width) and (rect.height >= height) then begin
      topSideY := rect.y + height;
      if (topSideY < bestY) or ((topSideY = bestY) and (rect.x < bestX)) then begin
        bestNode.x := rect.x;
        bestNode.y := rect.y;
        bestNode.width := width;
        bestNode.height := height;
        bestNode.rotated := false;
        bestY := topSideY;
        bestX := rect.x;
      end;
    end;
    if (allowRotations and ( rect.width >= height ) and ( rect.height >= width)) then begin
      topSideY := rect.y + width;
      if (topSideY < bestY) or ((topSideY = bestY ) and (rect.x < bestX)) then begin
        bestNode.x := rect.x;
        bestNode.y := rect.y;
        bestNode.width := height;
        bestNode.height := width;
        bestNode.rotated := true;
        bestY := topSideY;
        bestX := rect.x;
      end;
    end;
  end;
  result := bestNode;
end;

function TMaxRectsBinPack.findPositionForNewNodeContactPoint(width:integer; height:integer; bestContactScore:integer):TRectangle;
  var
    i:integer;
    bestNode:TRectangle;
    rect:TRectangle;
    score:integer;
begin
  bestNode := TRectangle.Create(0,0);
  bestContactScore := -1;
  for i := 0 to freeRectangles.length-1 do begin
    rect := freeRectangles[i];
    // Try to place the Rectangle in upright (non-flipped) orientation.
    if (rect.width >= width) and (rect.height >= height) then begin
      score := contactPointScoreNode(rect.x, rect.y, width, height);
      if (score > bestContactScore) then begin
          bestNode.x := rect.x;
          bestNode.y := rect.y;
          bestNode.width := width;
          bestNode.height := height;
          bestNode.rotated := false;
          bestContactScore := score;
      end;
    end;
    if allowRotations and ( rect.width >= height ) and ( rect.height >= width ) then begin
        score := contactPointScoreNode(rect.x, rect.y, height, width);
        if (score > bestContactScore) then begin
            bestNode.x := rect.x;
            bestNode.y := rect.y;
            bestNode.width := height;
            bestNode.height := width;
            bestNode.rotated := true;
            bestContactScore := score;
        end;
    end;
  end;
  result := bestNode;
end;

function TMaxRectsBinPack.findPositionForNewNodeBestLongSideFit(width:integer; height:integer; bestShortSideFit:integer; bestLongSideFit:integer):TRectangle;
var
  i:integer;
  bestNode:TRectangle;
  rect:TRectangle;
  leftoverHoriz:integer;
  leftoverVert:integer;
  shortSideFit:integer;
  longSideFit:integer;
begin
  bestNode := TRectangle.Create(0,0);

  bestLongSideFit := 2000000000;

  for i := 0 to freeRectangles.length-1 do begin
    rect := freeRectangles[i];
    // Try to place the Rectangle in upright (non-flipped) orientation.
    if (rect.width >= width) and (rect.height >= height) then begin
        leftoverHoriz := iabs(rect.width - width);
        leftoverVert := iabs(rect.height - height);
        shortSideFit := imin(leftoverHoriz, leftoverVert);
        longSideFit := imax(leftoverHoriz, leftoverVert);

        if (longSideFit < bestLongSideFit) or ((longSideFit = bestLongSideFit) and (shortSideFit < bestShortSideFit)) then begin
            bestNode.x := rect.x;
            bestNode.y := rect.y;
            bestNode.width := width;
            bestNode.height := height;
            bestNode.rotated := false;
            bestShortSideFit := shortSideFit;
            bestLongSideFit := longSideFit;
        end;

    end;

    if (allowRotations) and (rect.width >= height) and ( rect.height >= width) then begin
        leftoverHoriz := iabs(rect.width - height);
        leftoverVert := iabs(rect.height - width);
        shortSideFit := imin(leftoverHoriz, leftoverVert);
        longSideFit := imax(leftoverHoriz, leftoverVert);

        if (longSideFit < bestLongSideFit) or ((longSideFit = bestLongSideFit) and (shortSideFit < bestShortSideFit)) then begin
            bestNode.x := rect.x;
            bestNode.y := rect.y;
            bestNode.width := height;
            bestNode.height := width;
            bestNode.rotated := true;
            bestShortSideFit := shortSideFit;
            bestLongSideFit := longSideFit;
        end;

    end;
  end;
  result := bestNode;
end;

function TMaxRectsBinPack.findPositionForNewNodeBestAreaFit(width:integer; height:integer; bestAreaFit:integer; bestShortSideFit:integer):TRectangle;
  var
    i:integer;
    bestNode:TRectangle;
    rect:TRectangle;
    leftoverHoriz:integer;
    leftoverVert:integer;
    shortSideFit:integer;
    areaFit:integer;
begin
  bestNode := TRectangle.Create(0,0);

  bestAreaFit := 2000000000;

  for i := 0 to freeRectangles.length-1 do begin
    rect := freeRectangles[i];
    areaFit := rect.width * rect.height - width * height;

    // Try to place the Rectangle in upright (non-flipped) orientation.
    if (rect.width >= width) and (rect.height >= height) then begin
        leftoverHoriz := iabs(rect.width - width);
        leftoverVert := iabs(rect.height - height);
        shortSideFit := imin(leftoverHoriz, leftoverVert);

        if (areaFit < bestAreaFit) or ((areaFit = bestAreaFit ) and (shortSideFit < bestShortSideFit)) then begin
            bestNode.x := rect.x;
            bestNode.y := rect.y;
            bestNode.width := width;
            bestNode.height := height;
            bestNode.rotated := false;
            bestShortSideFit := shortSideFit;
            bestAreaFit := areaFit;
        end;
    end;

    if allowRotations and ( rect.width >= height ) and ( rect.height >= width) then begin
        leftoverHoriz := iabs(rect.width - height);
        leftoverVert := iabs(rect.height - width);
        shortSideFit := imin(leftoverHoriz, leftoverVert);

        if (areaFit < bestAreaFit) or ((areaFit = bestAreaFit) and ( shortSideFit < bestShortSideFit)) then begin
            bestNode.x := rect.x;
            bestNode.y := rect.y;
            bestNode.width := height;
            bestNode.height := width;
            bestNode.rotated := true;
            bestShortSideFit := shortSideFit;
            bestAreaFit := areaFit;
        end;
    end;
  end;

  result := bestNode;
end;

function TMaxRectsBinPack.contactPointScoreNode(x:integer; y:integer; width:integer; height:integer):integer;

  /// Returns 0 if the two intervals i1 and i2 are disjoint, or the length of their overlap otherwise.
  function commonIntervalLength(i1start:integer;i1end:integer; i2start:integer; i2end:integer):integer;
  begin
    if (i1end < i2start ) or ( i2end < i1start) then begin
      result := 0;
      exit;
    end;
    result := imin(i1end, i2end) - imax(i1start, i2start);
  end;

  var
    i:integer;
    score:integer;
    rect:TRectangle;
begin
  score := 0;

  if (x = 0) or ( x + width = binWidth) then score := score + height;
  if (y = 0) or ( y + height = binHeight) then score := score + width;
  for i := 0 to usedRectangles.length-1 do begin
    rect := usedRectangles[i];
    if (rect.x = x + width) or (rect.x + rect.width = x) then
      score := score + commonIntervalLength(rect.y, rect.y + rect.height, y, y + height);
    if (rect.y = y + height) or (rect.y + rect.height = y) then
      score := score + commonIntervalLength(rect.x, rect.x + rect.width, x, x + width);
  end;
  result := score;
end;

function TMaxRectsBinPack.splitFreeNode(freeNode:TRectangle; usedNode:TRectangle):Boolean;
  var
    newNode:TRectangle;
begin
  // Test with SAT if the Rectangles even intersect.
  if ((usedNode.x >= (freeNode.x + freeNode.width)) or ( (usedNode.x + usedNode.width) <= freeNode.x ) or
     ( usedNode.y >= (freeNode.y + freeNode.height)) or ( (usedNode.y + usedNode.height) <= freeNode.y )) then begin
    result:=false;
    exit;
  end;

  if ((usedNode.x < (freeNode.x + freeNode.width)) and ( (usedNode.x + usedNode.width) > freeNode.x)) then begin

    // New node at the top side of the used node.
    if ((usedNode.y > freeNode.y ) and ( usedNode.y < (freeNode.y + freeNode.height))) then begin
      newNode := freeNode.Clone;
      newNode.height := usedNode.y - newNode.y;
      freeRectangles.Push(newNode);
    end;

    // New node at the bottom side of the used node.
    if (usedNode.y + usedNode.height < freeNode.y + freeNode.height) then begin
      newNode := freeNode.Clone;
      newNode.y := usedNode.y + usedNode.height;
      newNode.height := freeNode.y + freeNode.height - (usedNode.y + usedNode.height);
      freeRectangles.Push(newNode);
    end;

  end;

  if (usedNode.y < (freeNode.y + freeNode.height)) and ((usedNode.y + usedNode.height) > freeNode.y) then begin

    // New node at the left side of the used node.
    if (usedNode.x > freeNode.x) and (usedNode.x < freeNode.x + freeNode.width) then begin
      newNode := freeNode.Clone;
      newNode.width := usedNode.x - newNode.x;
      freeRectangles.Push(newNode);
    end;

    // New node at the right side of the used node.
    if ( (usedNode.x + usedNode.width) < (freeNode.x + freeNode.width)) then begin
      newNode := freeNode.Clone;
      newNode.x := usedNode.x + usedNode.width;
      newNode.width := freeNode.x + freeNode.width - (usedNode.x + usedNode.width);
      freeRectangles.Push(newNode);
    end;

  end;

 result:=true;
end;

end.

