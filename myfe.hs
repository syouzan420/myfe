--{--# LANGUAGE BangPatterns #--}
import System.IO(IOMode(..), openFile, hClose, hGetContents, hSetEncoding, utf8, hPutStr,
                 hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Directory(doesFileExist)
import Data.Time.LocalTime(getZonedTime,ZonedTime(zonedTimeToLocalTime),LocalTime(localDay))
import Data.Char(isDigit)

type Contents = String
type Orders = String
data Dms = Dm String Int [Dms] | Rp | Er | Qi deriving (Eq,Show)

tgPass :: FilePath
tgPass = "myfe.txt"

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

weeklist :: [String]
weeklist = ["su","m","tu","w","th","f","sa"]

demands :: Dms
demands = Dm "" 0 [Dm "a" 1 [Dm "w" 2 [Dm "$" 3 dwork1],
                             Dm "m" 2 [Dm "$" 0 []],
                             Dm "t" 2 [Dm "$" 0 []]],
                   Dm "d" 0 []]

dwork1 :: [Dms]
dwork1 = [Dm "y" 4 [Dm "RY" 8 dwork2],
          Dm "m" 5 [Dm "RM" 8 dwork2],
          Dm "w" 6 [Dm "RW" 8 dwork2],
          Dm "d" 7 [Dm "RD" 8 dwork2]]

dwork2 :: [Dms]
dwork2 = [Dm "h" 9 [Dm "I" 10 [Dm "I" 11 dwork3]], Dm "d" 9 [Dm "I" 10 [Dm "I" 11 dwork3]]]

dwork3 :: [Dms]
dwork3 = [Dm "RH" 13 [Dm "YY" 0 [], Dm "YN" 14 [Dm "D" 0 []]]]

messages :: [String]
messages = ["enter the operation -- a: add, d: delete",
            "w: work, m: money, t: todo",
            "enter name",
            "y: yearly, m: monthly, w: weekly, d: exact day",
            "give a month and a day --ie. 120, 1023, 0920 ...",
            "give a day --ie. 20, 11, 31 ...",
            "su: Sunday, m: Monday, tu: Tuesday, w: Wednesday, th: Thursday, f: Friday, sa: Saturaday",
            "give an exact day -- ie. 20220420, 20231024 ...",
            "h: hourly wage, d: dayly wage",
            "enter the wage --ie. 1000, 1200 ...",
            "enter travelling expenses per day --ie. 880, 1200 ...",
            "enter start time --ie. 915, 1330, 1500 ...",
            "enter finish time --ie. 1300, 1600, 2330 ...",
            "from today? (Y/n) (Y is default and Enter means default)",
            "enter the day which the work starts --ie 20220927 20231221"
           ]

errors :: [String]
errors = ["wrong command. enter 'a' or 'd'",
          "you should choose work or todo",
          "enter something",
          "wrong command. enter 'y' or 'm' or 'w' or 'd'",
          "",
          "",
          "wrong code -- enter the short letters for week",
          "",
          "h or d",
          "enter numbers",
          "enter numbers",
          "enter hour and minute",
          "enter hour and minute",
          "Yes or No",
          "day format YYYYMMDD"
         ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  e <- doesFileExist tgPass
  c <- if e then fileOut
       else do
         fileIn ""
         return ""
  comLoop 1 0 "" demands 

comLoop :: Int -> Int -> Orders -> Dms -> IO ()
comLoop r co o dm@(Dm s i d) = do
  putStrLn (messages!!(i+co))
  putStr "> "
  g <- getLine
  let (no, nd) = checkInput g o d
      ((Dm ns _ _):_) = if (d==[]) then [Dm "-" 0 []] else d
      (hs:ts) = ns
      r' = if(ts=="H") then if (co==1) then r-1 else r else r
  (no', nd') <- if (hs=='R') then repeatInput r' no nd ts else return (no, nd) 
  case nd' of
    Er -> do
      putStrLn (errors!!i)
      comLoop r co o dm 
    Qi -> return ()
    Rp -> do
      putStrLn no'
      let co' = if (ts=="H") then if (co==0) then 1 else 0 else co
      comLoop r' co' no' dm
    _  -> do
      putStrLn no'
      let nr = if (s=="w") then length (tail$last$sepChar ';' no) else r
      putStrLn (show nr)
      comLoop nr 0 no' nd'

repeatInput :: Int -> Orders -> Dms -> String -> IO (Orders, Dms) 
repeatInput r o d s = do
  y <- if (s=="H") then return "" else do
    putStr "Another Data? (Y/n) :"
    getLine
  if (y=="n" || y=="N" || y=="no") then return (o++";",d) else do
    let lo = last$sepChar ';' o
        len = length lo
        ip = case s of "M" -> len < 29; "W" -> len < 8; _ -> True
        ir = r > 0
    if (ip && ir) then return (o, Rp) 
                  else do
            if (s=="H") then return (o, d) else do
              putStrLn "Can't add data!"
              return (o++";", d)
          



checkInput :: String -> Orders -> [Dms] -> (Orders, Dms) 
checkInput g o d =
  let cms = map (\(Dm m _ _) -> m) d
      hc = head$cms
      iq = g==":q" || g=="exit"
      iany = hc=="$"
      inum = hc=="I"
      irp = head hc == 'R'
      len = length hc 
      (cm,els) = if (length g>=len) then (take 2 g,drop 2 g) else ("","")
      is = elem cm cms
      id = if is then getIndex cm cms else (-1)
      nd = if iq then Qi else
           if irp then (if (isDay (tail hc) g) then head d else Er) else
           if inum then (if (isNum g) then head d else Er) else
           if iany then head d else
           if is then d!!id else Er
      no = case hc of
             "RH" -> o++g++";"
             ('R':xs) -> o++(addDay (last$sepChar ';' o) g xs)
             "$" -> o++g++";"
             "I" -> o++g++";"
             _   -> o++cm 
   in (no, nd)

addDay :: Orders -> String -> String -> String
addDay lo g t =
  case t of
    "W" -> let r = show$getIndex g weeklist 
               ism = elem (head r) lo
            in if ism then "" else r
    _   -> undefined

isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = (isDigit x) && (isNum xs)


isDay :: String -> String -> Bool
isDay t s =
  let s' = if (head s=='0') then tail s else s 
      len = length s'
   in case t of
        "Y" -> let (mo,dy) = if (len==3) then ([head s'],tail s') else (take 2 s',drop 2 s')
                   (moi,dyi) = (read mo::Int, read dy::Int)
                in moi>0 && moi<13 && dyi>0 && dyi<1+(daylist!!(moi-1))
        "M" -> let dyi = read s'::Int
                in dyi>0 && dyi<32
        "W" -> elem s' weeklist 
        "D" -> let (mo,dy) = if (len==7) then ([s'!!4],drop 5 s') 
                                         else (take 2 (drop 4 s'),drop 6 s')
                   (moi,dyi) = (read mo::Int, read dy::Int)
                in moi>0 && moi<13 && dyi>0 && dyi<1+(daylist!!(moi-1))
        _   -> True


fileOut :: IO Contents
fileOut = do
  h <- openFile tgPass ReadMode
  hSetEncoding h utf8
  con <- hGetContents h 
  mapM_ putStrLn (lines con)
  hClose h
  return con

fileIn :: Contents -> IO ()
fileIn str = do
  h <- openFile tgPass WriteMode
  hSetEncoding h utf8
  hPutStr h str 
  hClose h

today :: IO String 
today = do
  (a:b:c:d:_:e:f:_:g) <- show <$> localDay <$> zonedTimeToLocalTime <$> getZonedTime
  return (a:b:c:d:e:f:g)

uru :: Int -> Bool
uru y = let r1 = mod y 4 == 0
            r2 = mod y 100 == 0
            r3 = mod y 400 == 0
         in r3 || (r1 && not r2)

getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0
getIndex t (x:xs) = if(t==x) then 0 else 1+(getIndex t xs)


sepChar :: Char -> String -> [String]
sepChar ch [x]    = if (x==ch) then [[]] else [[x]]
sepChar ch (x:xs) = if (x==ch) then [[]]++(hd:tl)
                               else [x:hd]++tl
                          where (hd:tl) = sepChar ch xs

dataChange :: Contents -> Orders -> Contents 
dataChange c (a:as) =
  let cs = lines c
      nms = map (head . (sepChar ';')) cs
      nm = head$sepChar ';' as
      is = elem nm nms
      id = if is then getIndex nm nms else (-1)
   in case a of
        'a' -> if is then c else unlines $
                case (head as) of
                  'w' -> cs++[as]
                  't' -> cs++[as++";"++(last$sepChar ';' as)]
        'd' -> if is then unlines $ take id cs ++ drop (id+1) cs else c 

-----------------------
