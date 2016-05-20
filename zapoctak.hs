-- Zapoctak na Haskell
import System.Environment
import qualified Data.ByteString.Lazy as B
import qualified Data.Complex as C
import Data.Binary.Get
import Data.Word
import qualified Numeric as Numeric


nthPrimitiveSquareRoot :: (RealFloat n) => n -> C.Complex n
nthPrimitiveSquareRoot n = C.conjugate $ C.mkPolar 1 (2*pi/n)

takeOdd :: [C.Complex a] -> [C.Complex a]
takeOdd lst = map snd $ filter (\(x,_) -> x `mod` 2 == 1) $ zip [0..] lst
takeEven :: [C.Complex a] -> [C.Complex a]
takeEven lst = map snd $ filter (\(x,_) -> x `mod` 2 == 0) $ zip [0..] lst

--fftHelper1 :: (Num a, Integral b) => b -> b -> C.Complex a -> [C.Complex a] -> [C.Complex a] -> [C.Complex a]
fftHelper1 _ _ _ [] [] = []
fftHelper1 i n omega (e:evenLst) (o:oddLst) = (e + ((omega^i) * o)):(fftHelper1 (i+1) n omega evenLst oddLst)

--fftHelper2 :: Int -> Int -> C.Complex a -> [C.Complex a] -> [C.Complex a] -> [C.Complex a]
fftHelper2 _ _ _ [] [] = []
fftHelper2 i n omega (e:evenLst) (o:oddLst) = (e - ((omega^i) * o)):(fftHelper2 (i+1) n omega evenLst oddLst)

fft n omega lst = if n == 1 then (take 1 lst) else 
                    let evenLst = (fft (n / 2) (omega^2) (takeEven lst))
                        oddLst  = (fft (n / 2) (omega^2) (takeOdd lst))
                    in ((fftHelper1 0 (n / 2) omega evenLst oddLst) ++ (fftHelper2 0 (n/2) omega evenLst oddLst))

fftForward :: (RealFloat a) => a -> [C.Complex a] -> [C.Complex a]
fftForward n lst = fft n (nthPrimitiveSquareRoot n) lst

div' :: (RealFloat a) => C.Complex a -> a -> C.Complex a
div' c r = ((C.realPart c) / r) C.:+ ((C.imagPart c) / r)

fftBackward :: (RealFloat a) => a -> [C.Complex a] -> [C.Complex a]
fftBackward n lst = map (`div'` n) $ fft n ((C.conjugate (nthPrimitiveSquareRoot n))) lst

main = do 
    (fileName:_) <- getArgs
    wav <- parseFile fileName
    return ()  
    
parseFile fileName = do
    fileContents <- B.readFile fileName
    let result = runGet getWaveFile fileContents
    putStrLn (fileName ++ " loaded!")
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

wordToInt :: Word16 -> Int
wordToInt word | word < notWord = (fromIntegral word :: Int)
               | otherwise      = (-1) * (fromIntegral notWord :: Int)
               where notWord = (-word)

getFloat :: Get Float
getFloat = do 
    word <- getWord16le
    let signedWord = wordToInt word
    return ((fromIntegral signedWord :: Float)/(2^15))

wavDuration :: WaveFile -> Float
wavDuration wav = ((fromIntegral $ dataSize wav) * (fromIntegral $ channels wav) * 8) / ((fromIntegral $ sampleRate wav) * (fromIntegral $ bitDepth wav) )

intervalLength = 10

intervals_internal [] = []
intervals_internal lst@(head:tail) = (take (intervalLength) lst) : (intervals_internal tail)

intervals lst = filter (\l -> (length l) == (intervalLength)) (intervals_internal lst)

my_average :: [Float] -> Float
my_average lst = (sum $ map (abs) lst) / (fromIntegral $ length lst)

-- get the interval of values from to in msec
getInterval :: Float -> Float -> WaveFile -> [Float]
getInterval from to wav = let rate = ((fromIntegral $ sampleRate wav) :: Float)
                              fromIndex = floor $ (rate * from)
                              toIndex = floor $ (rate * to)
                          in take (toIndex - fromIndex) (drop fromIndex $ fileData wav)