-- Zapoctak na Haskell
import System.Environment
import qualified Data.ByteString.Lazy as B
import qualified Data.Complex as C
import Data.Binary.Get
import Data.Word
import qualified Numeric as Numeric
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Function as DataFunction

nthPrimitiveSquareRoot :: Int -> C.Complex Float
nthPrimitiveSquareRoot n = C.conjugate $ C.mkPolar 1 (2*pi/(fromIntegral n))

-- returns filters odd-indexed elements
takeOdd :: [C.Complex a] -> [C.Complex a]
takeOdd lst = map snd $ filter (\(x,_) -> x `mod` 2 == 1) $ zip [0..] lst
-- returns filters even-indexed elements
takeEven :: [C.Complex a] -> [C.Complex a]
takeEven lst = map snd $ filter (\(x,_) -> x `mod` 2 == 0) $ zip [0..] lst

--FFT see: http://mj.ucw.cz/vyuka/ads/44-fft.pdf
fftHelper1 :: Int -> Int -> C.Complex Float -> [C.Complex Float] -> [C.Complex Float] -> [C.Complex Float]
fftHelper1 _ _ _ [] [] = []
fftHelper1 i n omega (e:evenLst) (o:oddLst) | i == n    = []
                                            | otherwise = (e + ((omega^i) * o)):(fftHelper1 (i+1) n omega evenLst oddLst)
--FFT see: http://mj.ucw.cz/vyuka/ads/44-fft.pdf
fftHelper2 :: Int -> Int -> C.Complex Float -> [C.Complex Float] -> [C.Complex Float] -> [C.Complex Float]
fftHelper2 _ _ _ [] [] = []
fftHelper2 i n omega (e:evenLst) (o:oddLst) | i == n    = []
                                            | otherwise = (e - ((omega^i) * o)):(fftHelper2 (i+1) n omega evenLst oddLst)

--FFT see: http://mj.ucw.cz/vyuka/ads/44-fft.pdf
--fft :: (Integral a) => a -> C.Complex Float -> [C.Complex Float] -> [C.Complex Float]
fft :: Int -> C.Complex Float -> [C.Complex Float] -> [C.Complex Float]
fft n omega lst = if n == 1 then (take 1 lst) else 
                    let evenLst = (fft (n `div` 2) (omega^2) (takeEven lst))
                        oddLst  = (fft (n `div` 2) (omega^2) (takeOdd lst))
                    in ((fftHelper1 0 (n `div` 2) omega evenLst oddLst) ++ (fftHelper2 0 (n `div` 2) omega evenLst oddLst))


           
--fftForward :: (Integral a, RealFloat a) => a -> [C.Complex Float] -> [C.Complex Float]
fftForward :: Int -> [C.Complex Float] -> [C.Complex Float]
fftForward n lst = fft n (nthPrimitiveSquareRoot n) lst

-- divides complex number by components
div' :: C.Complex Float -> Int -> C.Complex Float
div' c r = ((C.realPart c) / (fromIntegral r)) C.:+ ((C.imagPart c) / (fromIntegral r))

--fftBackward :: (Integral a, RealFloat a) => a -> [C.Complex a] -> [C.Complex a]
fftBackward :: Int -> [C.Complex Float] -> [C.Complex Float]
fftBackward n lst = map (`div'` n) $ fft n ((C.conjugate (nthPrimitiveSquareRoot n))) lst


main = do 
    (fileName:_) <- getArgs
    wav <- parseFile fileName
    return ()  
    
--loads the WaveFile from a file given a path
parseFile filePath = do
    fileContents <- B.readFile filePath
    let result = runGet getWaveFile fileContents
    putStrLn (filePath ++ " loaded!")
    return (result)

data WaveFile = WaveFile {
    chunkID ::      Int,
    fileSize ::     Int,
    riffType ::     Int, 
    fmtId    ::     Int, 
    fmtSize ::      Int, 
    fmtCode ::      Int, 
    channels ::     Int, 
    sampleRate ::   Int, 
    fmtAvgBPS ::    Int, 
    fmtBlockAlign ::Int, 
    bitDepth ::     Int,
    dataID ::       Int,
    dataSize ::     Int,
    fileData ::     [Float]
} -- deriving (Show)

instance Show WaveFile where 
    show wav = "FileSize: " ++ (show $ fileSize wav) ++ "\nDataSize: " ++ (show $ dataSize wav) ++ "\nSampleRate: " ++ (show $ sampleRate wav) ++ "\nBitDepth: " ++ (show $ bitDepth wav) ++ "\nChannels: " ++ (show $ channels wav) ++ "\n"

-- parses the header of wav file and reads its contents
getWaveFile :: Get WaveFile
getWaveFile = do
    w_chunkID <- getWord32le
    let chunkID = (fromIntegral w_chunkID)
    w_fileSize <- getWord32le
    let fileSize = fromIntegral w_fileSize
    w_riffType <- getWord32le
    let riffType = fromIntegral w_riffType
    w_fmtID <- getWord32le
    let fmtID = fromIntegral w_fmtID
    w_fmtSize <- getWord32le
    let fmtSize = fromIntegral w_fmtSize
    w_fmtCode <- getWord16le
    let fmtCode = fromIntegral w_fmtCode
    w_channels <- getWord16le
    let channels = fromIntegral w_channels
    w_sampleRate <- getWord32le
    let sampleRate = fromIntegral w_sampleRate
    w_fmtAvgBPS <- getWord32le
    let fmtAvgBPS = fromIntegral w_fmtAvgBPS
    w_fmtBlockAlign <- getWord16le
    let fmtBlockAlign = fromIntegral w_fmtBlockAlign
    w_bitDepth <- getWord16le
    let bitDepth = fromIntegral w_bitDepth
    w_dataID <- getWord32le
    let dataID = fromIntegral w_dataID
    w_dataSize <- getWord32le
    let dataSize = fromIntegral w_dataSize
    fileData <- getWaveContents 0 dataSize
    return $! WaveFile chunkID fileSize riffType fmtID fmtSize fmtCode channels sampleRate fmtAvgBPS fmtBlockAlign bitDepth dataID dataSize fileData


--getWaveContents :: (Integral a) => a -> a-> Get [Float]
getWaveContents i size = do
    if i == size 
       then return []
       else do
           fl <- getFloat
           rest <- getWaveContents (i+2) size
           return (fl:rest)

-- turns UInt16 to Int16
wordToInt :: Word16 -> Int
wordToInt word | word < notWord = (fromIntegral word :: Int)
               | otherwise      = (-1) * (fromIntegral notWord :: Int)
               where notWord = (-word)

-- read 16bit Float from the stream
getFloat :: Get Float
getFloat = do 
    word <- getWord16le
    let signedWord = wordToInt word
    return ((fromIntegral signedWord :: Float)/(2^15))

-- Duration of the file
wavDuration :: WaveFile -> Float
wavDuration wav = ((fromIntegral $ dataSize wav) * (fromIntegral $ channels wav) * 8) / ((fromIntegral $ sampleRate wav) * (fromIntegral $ bitDepth wav) )

-- turns the list into a list of small intervals
intervalLength = 15

intervals_internal [] = []
intervals_internal lst@(head:tail) = (take (intervalLength) lst) : (intervals_internal tail)

intervals lst = filter (\l -> (length l) == (intervalLength)) (intervals_internal lst)

-- calculates the average of the absolute values in the list
my_average :: [Float] -> Float
my_average lst = (sum $ map (abs) lst) / (fromIntegral $ length lst)

-- get the interval of values from to in msec
getInterval :: Float -> Float -> WaveFile -> [Float]
getInterval from to wav = let rate = ((fromIntegral $ sampleRate wav) :: Float)
                              fromIndex = floor $ (rate * from)
                              toIndex = floor $ (rate * to)
                          in take (toIndex - fromIndex) (drop fromIndex $ fileData wav)

getBeeps _ [] _ = []
getBeeps i (interval:tailOfIntervals) globalAverage = (i, if globalAverage < intervalAverage then True else False, head interval):(getBeeps (i+1) tailOfIntervals globalAverage)                                                       
                                                   where intervalAverage = my_average interval

indexToTime index wav = (fromIntegral index) / (fromIntegral $ sampleRate wav)

filterBeepValues [] = []
filterBeepValues ((index, isQuiet, value):tail)     |   (not isQuiet)     = []
                                                    |   otherwise         = (index, value):(filterBeepValues tail)
                                                   
getBeepIntervals wav = let wavData = fileData wav
                           globalAverage = my_average wavData
                           listOfIntervals = intervals wavData
                           beeps = getBeeps 0 listOfIntervals globalAverage
                           listOfBeeps = List.groupBy (\(_, isQuiet1, _) (_, isQuiet2, _) -> isQuiet1 == isQuiet2) beeps
                       in filter (\lst -> length lst > 9) $ filter (not . null) $ map (filterBeepValues) listOfBeeps
                           -- firstLast = map (\lst -> (head lst, last lst)) listOfBeeps
                       -- in  filter (\t -> (snd $ fst t)) firstLast

{-
DTMF keypad frequencies (with sound clips) 	
            5/1209 Hz 	6/1336 Hz 	7/1477 Hz 	8/1633 Hz
1/697 Hz  	 1 / 5   	 2 / 6    	 3 / 7   	 A / 8
2/770 Hz 	 4 / 10  	 5 / 12   	 6 / 14  	 B / 16
3/852 Hz 	 7 / 15 	 8 / 18   	 9 / 21 	 C / 24 
4/941 Hz 	 * / 20 	 0 / 24 	 # / 28 	 D / 32
-}                

freqLimit = 15 

dtmfFrequencies = [697,770,852,941,1209,1336,1477,1633]

dtmfFrequencyRanges_internal :: [Int] -> [Set.Set Int]
dtmfFrequencyRanges_internal [] = []
dtmfFrequencyRanges_internal (freq:rest) = (Set.fromList [(freq - freqLimit)..(freq + freqLimit)]):(dtmfFrequencyRanges_internal rest)

dtmfFrequencyRanges :: [Set.Set Int]
dtmfFrequencyRanges = dtmfFrequencyRanges_internal dtmfFrequencies

dtmfTonesDict = Map.fromList $ zip ([x*y | x<- [1..4], y<-[5..8]]) ['1','2','3','A','4','5','6','B','7','8','9','C','*','0','#','D']

isDTMFFreq :: Int -> Int
isDTMFFreq freq = snd $ foldl f (1,0) dtmfFrequencyRanges
                where f acc@(index, result) set = if result /= 0 then acc 
                                                  else 
                                                    if freq `Set.member` set then (index+1, index) 
                                                    else (index+1, 0)
                                                    
debugStep = do
          wav <-parseFile "nahravka.wav"
          let beep = (getBeepIntervals wav) !! 28
          return (analyzeInterval beep 8000)
                                                    
debugF = do
        wav <- parseFile "nahravka.wav"
        let beeps = (getBeepIntervals wav)
        return(analyzeIntervals beeps (sampleRate wav))
                                                    
analyzeFFTOutput :: Int -> Int -> [C.Complex Float] -> Int -> Float -> [(Int, Float)] 
analyzeFFTOutput i n (x:xs) smplRate avrgValue   | i == n          = []
                                                 | i > (n `div` 2) = []
                                                 | otherwise       = let freq = (fromIntegral i) / ((fromIntegral n) / (fromIntegral smplRate))  -- calculate the frequency
                                                                         dfmtFreq = isDTMFFreq $ floor freq
                                                                         currentVal = (C.realPart $ abs x)
                                                                     in if dfmtFreq == 0 then restOfRecurrsion
                                                                        else if currentVal > (10 * avrgValue) then (dfmtFreq, currentVal):(restOfRecurrsion)
                                                                             else restOfRecurrsion
                                                                     where restOfRecurrsion = analyzeFFTOutput (i+1) n xs smplRate avrgValue
maybeCharToChar :: Maybe Char -> Char
maybeCharToChar Nothing = '?'
maybeCharToChar (Just c) = c
                                                                     
analyzeInterval :: [(Int, Float)] -> Int -> Char
analyzeInterval interval smplRate = let len = length interval
                                        floorPow2 = if len <= 1 then 0 else 2 ^ (floor $ logBase 2 (fromIntegral len)) -- calculate the n for FFT
                                        --startIndex = fst $ head interval
                                        --endIndex = fst $ last interval
                                        cmplxValues = map ((C.:+0).snd) interval -- make the values complex and trim the index
                                        fftImage = fftForward floorPow2 cmplxValues
                                        avrgValue = (sum $ map (C.realPart . abs) fftImage) / (fromIntegral floorPow2)
                                        frequencies = reverse $ List.sortBy (compare `DataFunction.on` snd) $ analyzeFFTOutput 0 floorPow2 fftImage smplRate avrgValue
                                        tone = if (length frequencies) < 2 then 0 else (fst $ head frequencies) * (fst $ head (tail frequencies))
                                    in  maybeCharToChar $ Map.lookup tone dtmfTonesDict

analyzeIntervals :: [[(Int, Float)]] -> Int -> String
analyzeIntervals [] _ = []
analyzeIntervals (interval:rest) smplRate = (analyzeInterval interval smplRate):(analyzeIntervals rest smplRate)