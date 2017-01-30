-- Compute relative spans between two Dates
module Relativity exposing (relativeSpan)

import Date  exposing (..)
import Time  exposing (..)
import Maybe exposing (..)
import List  exposing (..)

when : Bool -> a -> Maybe a
when c e = if c then Just e else Nothing

seconds : Float
seconds = 1000

minutes : Float
minutes = seconds * 60

hours : Float
hours = minutes * 60

days : Float
days = hours * 24

months : Float
months = days * 30

years : Float
years = days * 366

plural : Float -> String -> String
plural n s = let v = round n in toString v ++ s ++ if v == 1 then "" else "s"

ranges : List (Time -> Maybe String)
ranges =
    [ \t -> when (t < minutes) (plural (t / seconds) " second")
    , \t -> when (t < hours)   (plural (t / minutes) " minute")
    , \t -> when (t < days)    (plural (t / hours)   " hour")
    , \t -> when (t < months)  (plural (t / days)    " day")
    , \t -> when (t < years)   (plural (t / months)  " month")
    , \t -> Just               (plural (t / years)   " year")
    ]

relativeSpan : Date -> Date -> String
relativeSpan a b =
    let ax  = toTime a
        bx  = toTime b
        d   = abs (ax - bx)
        suf = if ax > bx then " ago" else " from now"
        def = "a long time"
        z   = filterMap (\f -> f d) ranges
     in "about " ++ withDefault def (head z) ++ suf
