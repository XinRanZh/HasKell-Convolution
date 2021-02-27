-- The essence of Gaussian image blur, or image blur, is to convert a pixel of a point into a "weighted average value" 
-- that approximates it and its surrounding pixels in a certain way. If we understand an image as a matrix,
-- we can express this algorithm of converting a pixel into a weighted average of a pixel and its surrounding pixels as another matrix. 
-- In other words, it can be understood as a matrix of weights.

-- We now assume two matrices. A is a larger matrix that stores all the data in the picture. 
-- We can understand that each value in matrix A is the greyscale of the picture at the corresponding point, 
-- and B is the Gaussian filter we use, which is the weight of a point and its surrounding points in the process of generating new values

-- For this, we introduce the concept of convolution
-- To understand this algorithm, you must first understand the basic principle of convolution. Convolution is an operation that calculates two matrices.

-- Convolution

-- The convolution is, up-side-down the B matrix and place it in a certain position of the A matrix, multiply the values ​​of the corresponding matrix positions of A and B one by one, and fill in the result obtained The new matrix corresponds to the middle position of the B matrix, and the B matrix is ​​translated to cover each position of the A matrix. In this process, new values ​​are continuously calculated and the new values ​​are filled into the corresponding place in the result matrix. This process is called convolution

-- In order to easily calculate the B matrix we use, that is, the Gaussian image filter, it is an asymmetric matrix, so we can use correlation (which can be understood as the non-inverted B matrix version of the convolution) to replace the convolution operation

-- Padding

-- The next question is, when you move the B matrix in the A matrix, when the B matrix moves to the corner of A,
-- how to deal with the problem that the B matrix crosses the corner of A?

-- For the sake of simplicity, in this project, it is assumed that as long as the center of matrix B is still in matrix A, 
-- the result of this time will be calculated, so that when calculating, all parts beyond the edge are recorded as 0


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