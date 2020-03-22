czyPalindrom [] = True
czyPalindrom [n] = True
czyPalindrom l = if head l /= last l then False else czyPalindrom (init (tail l))
