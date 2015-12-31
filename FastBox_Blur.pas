unit FastBox_Blur;

interface

uses
  Windows, SysUtils, Classes, Graphics;

type

  TFColor = packed record
    b, g, r: Byte end;
    PFColor = ^TFColor;
    TLine = packed array [0 .. 0] of TFColor;
    PLine = ^TLine;
    TPLines = packed array [0 .. 0] of PLine;
    PPLines = ^TPLines;

    procedure FastBoxBlur(Bitmap: TBitmap; pRadius: double = 3;
      pCountOfBoxes: integer = 3);

    // written by e.freimann - depended on Fast Image convolution - http://elynxsdk.free.fr/ext-docs/Blur/Fast_box_blur.pdf
    // Very fast blurring filter, can blur specified region in bitmap.
implementation

uses
  math;

type

  TarrayofInteger = array of integer;

  TRGBTriple = packed record
    b: Byte;
    g: Byte;
    r: Byte;
  end;

  TRGBD = packed record
    DB: double;
    DG: double;
    DR: double;
  end;

  PRow = ^TRow;
  TRow = array [0 .. 1000000] of TRGBTriple;

  PPRows = ^TPRows;
  TPRows = array [0 .. 1000000] of PRow;

function FastboxesForGauss(pRadius: double; pCount: integer): TarrayofInteger;
// standard deviation, number of boxes
var
  lWeiqhtIdeal, lMIdeal: double;
  i, lLower, lUpper, lM: integer;
begin
  lWeiqhtIdeal := sqrt((12 * pRadius * pRadius / pCount) + 1);
  // Ideal averaging filter width
  lLower := math.floor(lWeiqhtIdeal);
  if (lLower mod 2 = 0) then
    lLower := lLower - 1;
  lUpper := lLower + 2;
  lMIdeal := (12 * pRadius * pRadius - pCount * lLower * lLower - 4 * pCount *
    lLower - 3 * pCount) / (-4 * lLower - 4);
  lM := round(lMIdeal);
  setLength(Result, pCount);
  for i := 0 to pCount - 1 do
    if i < lM then
      Result[i] := lLower
    else
      Result[i] := lUpper;
end;

procedure FastBoxBlur(Bitmap: TBitmap; pRadius: double = 3;
  pCountOfBoxes: integer = 3);
var

  J, Width, Height: integer;
  BMP: TBitmap;
  SrcRows, DstRows: PPRows;
  lMatrix: TarrayofInteger;

  function RGBMultiple(pRGBDouble: TRGBD; pWeight: double): TRGBD;
  begin
    with Result do
    begin
      DR := pRGBDouble.DR * pWeight;
      DG := pRGBDouble.DG * pWeight;
      DB := pRGBDouble.DB * pWeight;
    end;
  end;

  function RGBAdd(pRGBDouble: TRGBD; pPixel: TRGBTriple;
    pCoef: integer = 1): TRGBD;
  begin
    with Result do
    begin
      DR := pRGBDouble.DR + pCoef * pPixel.r;
      DG := pRGBDouble.DG + pCoef * pPixel.g;
      DB := pRGBDouble.DB + pCoef * pPixel.b;
    end;
  end;

  function RGBSet(pPixel: TRGBTriple): TRGBD;
  begin
    with Result do
    begin
      DR := pPixel.r;
      DG := pPixel.g;
      DB := pPixel.b;
    end;
  end;

  function TrimReal(Lower, Upper: integer; X: double): integer;
  begin
    if (X < Upper) and (X >= Lower) then
      Result := Trunc(X)
    else if X >= Upper then
      Result := Upper
    else
      Result := Lower;
  end;

  function RoundVal(pRGBDouble: TRGBD): TRGBTriple;
  begin
    with Result do
    begin
      r := TrimReal(0, 255, pRGBDouble.DR);
      g := TrimReal(0, 255, pRGBDouble.DG);
      b := TrimReal(0, 255, pRGBDouble.DB);
    end;
  end;

  procedure FastBoxBlurInternalH(pSrc, pDest: PPRows; pwidth, pheight: integer;
    pR: integer);
  var
    lCoef: double;
    i, J: integer;
    lRGBDouble: TRGBD;
    lDestIdx, lLastIdx, lRIdx: integer;
  begin
    lCoef := 1 / (pR + pR + 1);
    for i := 0 to pheight - 1 do
    begin
      lDestIdx := 0;
      lLastIdx := 0;
      lRIdx := pR;
      lRGBDouble := RGBMultiple(RGBSet(pSrc[i][0]), pR + 1);
      for J := 0 to pR - 1 do
        lRGBDouble := RGBAdd(lRGBDouble, pSrc[i][J]);
      for J := 0 to pR do
      begin
        lRGBDouble := RGBAdd(RGBAdd(lRGBDouble, pSrc[i][lRIdx]),
          pSrc[i][0], -1);
        inc(lRIdx);
        pDest[i][lDestIdx] := RoundVal(RGBMultiple(lRGBDouble, lCoef));
        inc(lDestIdx);
      end;
      for J := pR + 1 to pwidth - pR - 1 do
      begin
        lRGBDouble := RGBAdd(RGBAdd(lRGBDouble, pSrc[i][lRIdx]),
          pSrc[i][lLastIdx], -1);
        inc(lRIdx);
        inc(lLastIdx);
        pDest[i][lDestIdx] := RoundVal(RGBMultiple(lRGBDouble, lCoef));
        inc(lDestIdx);
      end;
      for J := pwidth - pR to pwidth - 1 do
      begin
        lRGBDouble := RGBAdd(RGBAdd(lRGBDouble, pSrc[i][pwidth - 1]),
          pSrc[i][lLastIdx], -1);
        inc(lLastIdx);
        pDest[i][lDestIdx] := RoundVal(RGBMultiple(lRGBDouble, lCoef));
        inc(lDestIdx);
      end;
    end;
  end;

  procedure FastBoxBlurInternalV(pSrc, pDest: PPRows; pwidth, pheight: integer;
    pR: integer);
  var
    lCoef: double;
    i, J: integer;
    lRGBDouble: TRGBD;
    lDestIdx, lLastIdx, lRIdx: integer;
  begin
    lCoef := 1 / (pR + pR + 1);
    for i := 0 to pwidth - 1 do
    begin
      lDestIdx := 0;
      lLastIdx := 0;
      lRIdx := pR;
      lRGBDouble := RGBMultiple(RGBSet(pSrc[0][i]), pR + 1);
      for J := 0 to pR - 1 do
        lRGBDouble := RGBAdd(lRGBDouble, pSrc[J][i]);
      for J := 0 to pR do
      begin
        lRGBDouble := RGBAdd(RGBAdd(lRGBDouble, pSrc[lRIdx][i]),
          pSrc[0][i], -1);
        pDest[lDestIdx][i] := RoundVal(RGBMultiple(lRGBDouble, lCoef));
        inc(lRIdx);
        inc(lDestIdx);
      end;
      for J := pR + 1 to pheight - pR - 1 do
      begin
        lRGBDouble := RGBAdd(RGBAdd(lRGBDouble, pSrc[lRIdx][i]),
          pSrc[lLastIdx][i], -1);
        inc(lLastIdx);
        pDest[lDestIdx][i] := RoundVal(RGBMultiple(lRGBDouble, lCoef));
        inc(lDestIdx);
        inc(lRIdx);
      end;
      for J := pheight - pR to pheight - 1 do
      begin
        lRGBDouble := RGBAdd(RGBAdd(lRGBDouble, pSrc[pheight - 1][i]),
          pSrc[lLastIdx][i], -1);
        inc(lLastIdx);

        pDest[lDestIdx][i] := RoundVal(RGBMultiple(lRGBDouble, lCoef));
        inc(lDestIdx);
      end;
    end;
  end;

  procedure FastBoxBlurInternal(pSrc, pDest: PPRows; pwidth, pheight: integer;
    pR: integer);
  var
    i, J: integer;
  begin
    for i := 0 to Bitmap.Height - 1 do
    begin
      for J := 0 to Bitmap.Width - 1 do
        pDest[i][J] := pSrc[i][J];
    end;
    FastBoxBlurInternalH(pDest, pSrc, pwidth, pheight, pR);
    FastBoxBlurInternalV(pSrc, pDest, pwidth, pheight, pR);
  end;

begin

  // Check and prepare Bitmap for processing
  if Bitmap = nil then
    Exit;
  if Bitmap.Empty or (Bitmap.Width < 1) or (Bitmap.Height < 1) then
    Exit;
  Bitmap.PixelFormat := pf24bit; // We working only with true RGB bitmaps

  // Check and prepare region for processing
  Width := Bitmap.Width;
  Height := Bitmap.Height;

  // Prepare temporary copy of Bitmap
  BMP := TBitmap.Create;
  BMP.Assign(Bitmap);

  lMatrix := FastboxesForGauss(pRadius, pCountOfBoxes);

  // Prepare tables of pointers to bitmap's data
  GetMem(SrcRows, BMP.Height * SizeOf(PRow));
  GetMem(DstRows, Bitmap.Height * SizeOf(PRow));

  for J := 0 to Bitmap.Height - 1 do
  begin
    SrcRows[J] := BMP.Scanline[J];
    DstRows[J] := Bitmap.Scanline[J];
  end;

  for J := 0 to pCountOfBoxes - 1 do
  begin
    if J mod 2 = 0 then
      FastBoxBlurInternal(SrcRows, DstRows, Width, Height,
        (lMatrix[J] - 1) div 2)
    else
      FastBoxBlurInternal(DstRows, SrcRows, Width, Height,
        (lMatrix[J] - 1) div 2)
  end;

  FreeMem(SrcRows, BMP.Height * SizeOf(PRow));
  FreeMem(DstRows, Bitmap.Height * SizeOf(PRow));
  BMP.Free;
  finalize(lMatrix);
end;

end.
