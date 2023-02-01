module EpochTests

open FsCheck.NUnit
open Epoch.Days

[<Property>]
let ``Leap year is every 4 year`` (y:int) =
    isLeap y = isLeap (y-4) || isLeap (y+4)


