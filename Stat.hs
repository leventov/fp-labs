module Stat () where

import M0 (median)


percentile percent xs = (sort xs) !! (length xs * (percent / 100))

q1 = percentile 25
q3 = percentile 75

iqr xs = q3 xs - q1 xs

lowOutlinerBound xs =
	q1' - 1.5 * (q3' - q1') 
	where q1' = q1 xs
		  q3' = q3 xs


highOutlinerBound xs =
	q3' + 1.5 * (q3' - q1') 
	where q1' = q1 xs
		  q3' = q3 xs

minimumIgnoreOutliners xs = 