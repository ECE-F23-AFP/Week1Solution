namespace Epoch

open Days

module Date =
    let yearOf days =
        let Y400 = 400*365+97
        let Y100 = 100*365+24
        let Y4   = 4*365+1
        let Y3   = 365*4
        let Y1   = 365
        let rec yearOfRec days =
            match days with
            | d when d > Y400 -> 400 + yearOfRec (d-Y400)
            | d when d > Y100 -> 100 + yearOfRec (d-Y100)
            | d when d > Y4   -> 4 + yearOfRec (d-Y4)
            | d when d = Y3   -> 3 + yearOfRec (d-Y3)
            | d when d > Y1   -> 1 + yearOfRec (d-Y1)
            | _               -> 0
        1970 + (yearOfRec days)
    
    let monthOf days = 
        let y = yearOf days
        let z = days - epochDays (1, 1, y)
        let c = match (isLeap y, z) with
                | (true, z)  when z<60  -> 0
                | (true, _)             -> 1
                | (false, z) when z<59  -> 0
                | (false, _)            -> 2

        (12*(z+c)+373)/367
        
    let dateOf days =
        let y = yearOf days
        let m = monthOf days
        let d = days - epochDays (1, m, y)
        (d, m, y)