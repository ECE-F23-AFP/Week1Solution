namespace Epoch

module Days =
    let epoch = 1970
    
    let isLeap = function
        | year when (year % 400) = 0 -> true
        | year when (year % 100 <> 0 && year % 4 = 0) -> true
        | _ -> false
    
    let daysToEndYear year =
        let leapYears = List.filter isLeap [epoch..year] 
        (year-epoch+1)*365 + (List.length leapYears)
    
    let daysToEndMonth (month, year) =
        let correction = match month with
                         | 1 -> 0
                         | _ when isLeap year -> 1
                         | _ -> 2
        (367*month+5)/12 - correction + daysToEndYear (year-1)
        
    let epochDays (day, month, year) =
        day + daysToEndMonth (month-1, year) 