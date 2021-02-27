
import Codec.Picture ()         
import Codec.Picture.Types ()
import Graphics.Image.Processing ()
import qualified Graphics.Image as I
import qualified Graphics.Image.Interface as Inte
import Graphics.Image.ColorSpace ( Y, Pixel )
import Data.Word (Word8)
 


-- We want to be able to read the image that the user inputs. Utilizing the hip library, we can process and read images.
-- Before we can apply the filter to the image, we need to also break down the image into a 2D Array, 
-- with the number of lists and number of elements within each list determined by the dimensions (in pixels) of the image. 
-- Using the readImageY function from Graphics.Image, we use VS, a storable vector representation of the image to 
-- obtain the dimensions of the image. readImageY also reads the image as grayscale, and stores the pixels with 
-- Double precision. toLists will then take the colorspace and the precision of every pixel and store it in a nested list. 
readImg :: FilePath -> IO [[Graphics.Image.ColorSpace.Pixel Y Double]]
readImg img = do
    procImg <- I.readImageY I.VS img 
    let newImg = I.toLists procImg
    return newImg



get :: Num p => [[p]] -> [[p]] -> Int -> Int -> Int -> Int -> p
get image filter xi yi xf yf
  | xi + (xf - ((length filter)  `div` 2))  < 0 = 0
  | yi + (yf - ((length (filter !! 0))  `div` 2)) < 0 = 0
  | xi + (xf - ((length filter)  `div` 2)) > ((length image) - 1) = 0
  | yi + (yf - ((length (filter !! 0))  `div` 2)) > ((length (image !! 0)) - 1) = 0
  | otherwise = (image !! (xi + (xf - ((length filter)  `div` 2))) !! (yi + (yf - ((length (filter !! 0))  `div` 2)))) * (filter !! xf !! yf)

imaget :: [[Double]]
imaget = [[1.0,1,2],[3,4,5],[6,7,8]]
filtert :: [[Double]]
filtert = [[0,1.0,0],[1,0,1],[0,1,0]]

iter1D :: Num p => Int -> [[p]] -> [[p]] -> Int -> Int -> Int -> p
iter1D i image filter xi yi xf
    | i < (length (filter !! 0)) =  (get image filter xi yi xf i) + (iter1D (i+1) image filter xi yi xf)
    | otherwise = 0

iter2D :: Num p => Int -> [[p]] -> [[p]] -> Int -> Int -> p
iter2D i image filter xi yi
    | i < (length filter) = (iter1D 0 image filter xi yi i) + (iter2D (i+1) image filter xi yi)
    | otherwise = 0

convolveX::Num a => [[a]] -> [[a]] -> Int -> [a]
convolveX image filter xi = foldl (\res y -> if y < (length (image !! 0)) then res ++ [iter2D 0 image filter xi y] else res) [] [0,1..(length (image !! 0))]

convolveXY::Num a => [[a]] -> [[a]] -> [[a]]
convolveXY image filter = foldl (\res x -> if x < (length (image)) then res ++ [convolveX image filter x] else res) [] [0,1..(length (image))]



-- As we have invoked the do keyword in the readImg function, readImage will return an IO type. convolveXY and convolveX do not take in 
-- IO, so we need to work within the IO monad, and use the <- keyword to unwrap the value of readImg.
main :: IO ()
main = do
    putStrLn "Please specify the file path to the image you would like to transform."
    str <- getLine 
    res <- readImg str
    print (convolveXY res [[0.33,0.33,0.33],[0.33,0.33,0.33],[0.33,0.33,0.33]])