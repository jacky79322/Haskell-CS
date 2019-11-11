doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2 --else statements are required with if statements
                        -- optional: doubleSmallNumber' x = (if x > 100 then x else x*2) + 1  
