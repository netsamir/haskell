-- Test lists
l0 = [(2, 2, 3), (0, 4, 2), (4, 6, 2), (4.7, 3, 0.5)]
l1 = [(0, 0, 2.1)]
l2 = [(2, 4, 2), (3, 9, 3)]
l3 = [(3, 3, 3), (2, 2, 1), (3, 5, 1.5)]

-- Distance
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)

-- Surface
surface :: Double -> Double
surface r = pi * r * r

-- √(A/π) = r
radius :: Double -> Double
radius a = sqrt(a/pi)

-- Intersection Area
-- https://stackoverflow.com/questions/4247889/area-of-intersection-between-two-circles
intersection :: Double -> Double -> Double -> Double
intersection r1 r2 d
    | r2 < r1 = intersection' r2 r1 d
    | otherwise = intersection' r1 r2 d

intersection' :: Double -> Double -> Double -> Double
intersection' r1 r2 d = p1 + p2 - p3
    where p1 = r1*r1 * acos((d*d + r1*r1 - r2*r2)/(2*d*r1))
          p2 = r2*r2 * acos((d*d + r2*r2 - r1*r1)/(2*d*r2))
          p3 = 0.5 * sqrt((-d+r1+r2) * (d+r1-r2) * (d-r1+r2) * (d+r1+r2))

-- Percentage of Intersection Area with regards to the Surface
percentage :: Double -> Double -> (Double, Double) -> (Double, Double) -> Double
percentage r1 r2 (x1, y1) (x2, y2) = intersection r1 r2 d / surface r1
    where d = (dist (x1, y1) (x2, y2))

-- The intersection area of the two black holes is greater than or
-- equal to 55% (>= 55%) of one of the two black holes area.
merge_cond1 :: Double -> Double -> Double -> Bool
merge_cond1 i s1 s2 = i >= s1 * 0.55 || i >= s2 * 0.55

-- The area of one of the two black holes is over 20% (>= 20%) more than
-- the area of the other.
merge_cond2 :: Double -> Double -> Bool
merge_cond2 s1 s2 = s1 * 1.2 >= s2 || s2 * 1.2 >= s1

merge_cond :: (Double, Double, Double) -> (Double, Double, Double) -> Bool
merge_cond (x1, y1, r1) (x2, y2, r2)
    | cond1 && cond2 = True
    | otherwise = False
    where cond1 = merge_cond1 i s1 s2
          cond2 = merge_cond2 s1 s2
          i = intersection r1 r2 (dist (x1, y1) (x2, y2))
          s1 = surface r1
          s2 = surface r2

merge :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
merge (x1, y1, r1) (x2, y2, r2) = (x1, y1, radius (s1 + s2))
    where s1 = surface r1
          s2 = surface r2
