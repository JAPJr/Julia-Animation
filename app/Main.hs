module Main where

import Data.Complex
import System.Random
import System.IO
import qualified Data.Array.IO as A
import Control.Monad
import System.Process (callCommand)
import System.Console.Haskeline (outputStr, getInputLine, runInputT, defaultSettings)


juliaMapLimit = 2.0 
xLimit = 2.0
yLimit = 2.0
nValuesPerLine = 1001
deltaX = 2.0 * xLimit / fromIntegral (nValuesPerLine - 1) 
deltaY = 2.0 * yLimit / fromIntegral (nValuesPerLine - 1) 

xValues = [ negate xLimit + fromIntegral i * deltaX | i <- [0 .. nValuesPerLine - 1] ]
yValues = [ negate yLimit + fromIntegral i * deltaY | i <- [0 .. nValuesPerLine - 1] ]

data PathType = Linear | Arc | Radial deriving (Read, Show)
 
script c calcName = "set terminal pngcairo  font 'Times, 16'\n\ 
                 \set output '" ++ calcName ++ ".png'\n\
                 \set xrange [-2:2]\n\
                 \set yrange [-1.5:1.5]\n\
                 \set title 'c = " ++ show (realPart c) ++ " + i " ++ show (imagPart c) ++ "'\n\
                 \plot '" ++ calcName ++ ".txt' using 1:2 title '" ++ calcName ++ "' with dots lc rgb 'dark-blue'"

main = do
   baseName <- getString "Enter base file name for animation:  "
   cs <- getCs
   calcJuliaSets baseName cs
   callCommand "convert -delay 10 -loop 1 $(ls -1 *.png | sort -V) animated.gif"
   putStrLn "The the the that's all folks!"


calcJuliaSets :: String -> [(Int, Complex Double)] -> IO ()
calcJuliaSets _ [] = return ()
calcJuliaSets baseName ((n,c) : cs') = do
   let nameOfCalc = baseName ++ "_" ++ show n
   hData <- openFile (nameOfCalc ++ ".txt")  WriteMode
   hPutStrLn hData $ zToGnuplotOut' $ calcJulia c 50
   hPutStrLn hData ("# c = " ++ show (realPart c) ++ "  " ++ show (imagPart c))
   hPutStrLn hData "#\n\n           X                       Y\n"
   hClose hData
   callCommand ("echo " ++ "\"" ++ script c nameOfCalc ++ "\"" ++ " | gnuplot")
   putStrLn ("Julia set for c = " ++ show c ++ " has been calculated and stored.")
   calcJuliaSets baseName cs'


calcJulia :: Complex Double -> Int -> [Complex Double]
calcJulia c n = foldr (\y juliaList -> juliaList ++ getPointsAlongX y ) [] yValues
   where getPointsAlongX yForLine = foldr (addPointIfJulia yForLine) [] xValues
         addPointIfJulia yValue xValue juliaListForLine = if isJulia c n (xValue :+ yValue) 
                                                             then (xValue :+ yValue) : juliaListForLine
                                                             else juliaListForLine  

isJulia :: Complex Double -> Int -> Complex Double -> Bool
isJulia c maxIterations z = isJulia' 0 z
   where isJulia' iterations zMapped
           |magnitude zMapped  > juliaMapLimit = False
           |iterations > maxIterations         = True
           |otherwise                          = isJulia' (iterations + 1) (zMapped**2 + c)


zToGnuplotOut' :: [Complex Double] -> String
zToGnuplotOut' zList = foldr (\(x :+ y) gnuOut -> (show x ++ "   " ++ show y ++ "\n") ++ gnuOut)  "x          y\n" zList
 

getString :: String -> IO String
getString prompt = runInputT defaultSettings querry
  where querry = do
          maybeText <- getInputLine prompt
          case maybeText of
            Nothing -> return ""
            Just input -> return input



      
getCsLinear  c1 c2 n = map (\idx -> (idx, c1 + fromIntegral idx * deltaC)) ns
   where ns = [0 .. n-1]
         deltaC = (c2 - c1)/fromIntegral (n - 1)

getCsArc r theta1 theta2 n = map ( \idx -> ( idx, c idx) ) ns
   where ns = [0 .. n-1]
         deltaTheta = (theta2 - theta1)/fromIntegral (n-1)
         c i = mkPolar r (theta1 + fromIntegral i * deltaTheta)
        
getCsRadial r1 r2 theta n = map ( \idx -> (idx, c idx) ) is
   where is = [0 .. n-1]
         deltaR = (r2 - r1) / fromIntegral (n-1)
         c i = mkPolar (r1 + fromIntegral i * deltaR) theta
          

getCs :: IO [(Int, Complex Double)]
getCs = do
   nFrames <- (read <$> getString "Enter the number of frames in the animation:  ") :: IO Int
   path <- (read <$> getString "Enter 'Linear', 'Arc', or 'Radial' for path type:  ") :: IO PathType
   case path of
      Linear -> do
                   c1 <- (read <$> getString "Enter c1:  ") :: IO (Complex Double)
                   c2 <- (read <$> getString "Enter c2:  ") :: IO (Complex Double)
                   return $ getCsLinear c1 c2 nFrames
      Arc    -> do 
                   r <- (read <$> getString "Enter r:  ") :: IO Double
                   theta1D <- (read <$> getString "Enter theta1 in degrees:  ") :: IO Double
                   theta2D <- (read <$> getString "Enter theta2 in degrees:  ") :: IO Double
                   return $ getCsArc r (theta1D * pi/180) (theta2D * pi/180) nFrames 
      Radial -> do 
                   r1 <- (read <$> getString "Enter r1:  ") :: IO Double
                   r2 <- (read <$> getString "Enter r2:  ") :: IO Double
                   thetaD <- (read <$> getString "Enter theta in degrees:  ") :: IO Double
                   return $ getCsRadial r1 r2 (thetaD * pi/180) nFrames
   
         

 


mapN :: Complex Double -> Int -> Complex Double -> Complex Double
mapN c n z = foldr (\ _ mappedZ -> mappedZ**2 + c) z [1 .. n]


-- *************************** Not currently used *******************************

getOutFile :: IO (Handle, String)
getOutFile = do
   fName <- getString "Enter file name for output:  " :: IO String
   h <- openFile fName WriteMode
   return (h, fName)



showNDec :: Double -> Int -> String
showNDec r n = if abs r < 1.0 then sign ++ "0." ++ padding ++ theDigits
               else sign ++ intDigits ++ "." ++ decDigits
   where sign = if r < 0 then "-" else ""
         theDigits =  show (round $ (10 ^ n) * abs r :: Int)
         nDigits = length theDigits
         intDigits  = take (nDigits - n) theDigits
         decDigits = drop (nDigits - n) theDigits
         padding = replicate (n - nDigits) '0'

