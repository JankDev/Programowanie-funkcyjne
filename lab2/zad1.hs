odwroc [] = []
odwroc l = (last l) : odwroc (init l)
