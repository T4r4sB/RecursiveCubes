unit ConvexTree;

interface

uses
  Geometry, Points;

type
  PHighNode = ^HighNode;

  HighNode = record
    pmin, pmax: Point;
    subtracting: boolean;

    case leaf: boolean of
      FALSE: (
        walls: integer;
        subWalls: integer;
      );
      TRUE: (   
        center: Point;
        childs: array [0..7] of integer
      );
  end;

implementation

end.
