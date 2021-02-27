import Codec.Picture


imgConv :: Image PixelRGB8 -> Image Pixel8
imgConv img = pixelMap rgbtoGrey

rgbtoGrey :: PixelRGB8 -> Pixel8
rgbtoGrey (PixelRGB8 r g b) = (r + g + b) `div` 3


get image filter xi yi xf yf
  | xi + (xf - ((length filter)  `div` 2))  < 0 = 0
  | yi + (yf - ((length (filter !! 0))  `div` 2)) < 0 = 0
  | xi + (xf - ((length filter)  `div` 2)) > ((length image) - 1) = 0
  | yi + (yf - ((length (filter !! 0))  `div` 2)) > ((length (image !! 0)) - 1) = 0
  | otherwise = (image !! (xi + (xf - ((length filter)  `div` 2))) !! (yi + (yf - ((length (filter !! 0))  `div` 2)))) * (filter !! xf !! yf)

imaget = [[1.0,1,2],[3,4,5],[6,7,8]]
filtert = [[0,1.0,0],[1,0,1],[0,1,0]]

iter1D i image filter xi yi xf
    | i < (length (filter !! 0)) =  (get image filter xi yi xf i) + (iter1D (i+1) image filter xi yi xf)
    | otherwise = 0

iter2D i image filter xi yi
    | i < (length filter) = (iter1D 0 image filter xi yi i) + (iter2D (i+1) image filter xi yi)
    | otherwise = 0

convolveX::Num a => [[a]] -> [[a]] -> Int -> [a]
convolveX image filter xi = foldl (\res y -> if y < (length (image !! 0)) then res ++ [iter2D 0 image filter xi y] else res) [] [0,1..(length (image !! 0))]

convolveXY::Num a => [[a]] -> [[a]] -> [[a]]
convolveXY image filter = foldl (\res x -> if x < (length (image)) then res ++ [convolveX image filter x] else res) [] [0,1..(length (image))]