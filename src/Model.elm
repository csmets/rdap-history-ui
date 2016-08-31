module Model exposing (Period, Version, History)

type alias Period =
    { whence : String
    , until  : String }

type alias Version =
    { applicability : Period
    , rpsl          : List String }

type alias History =
    { handle    : String
    , ipVersion : String
    , versions  : List Version }
