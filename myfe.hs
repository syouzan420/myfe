import System.IO(IOMode(..), openFile, hClose, hGetContents, hSetEncoding, utf8, hPutStr,
                 hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Directory(doesFileExist)
import Data.Time.LocalTime(getZonedTime,ZonedTime(zonedTimeToLocalTime),LocalTime(localDay))
import Data.Char(isDigit)
import Data.Data(toConstr,Data)

type Contents = String
type Orders = String
data Dms = Dm String Int [Dms] | Rp | Er | Qi deriving (Eq,Show)
data Td = N Int | L Char | LN Char Int | S String | Ot deriving (Data,Eq,Show)

tgPass :: FilePath
tgPass = "myfe.txt"

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

weeklist :: [String]
weeklist = ["su","m","tu","w","th","f","sa"]

weekTList :: [String]
weekTList = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

demands :: Dms
demands = Dm "" 0 [Dm "a" 1 [Dm "w" 2 [Dm "$" 3 awork1],
                             Dm "m" 2 [Dm "$" 15 amoney1],
                             Dm "t" 2 [Dm "$" 23 atodo1]],
                   Dm "d" 1 [Dm "w" 2 lany, Dm "m" 2 lany, Dm "t" 2 lany],
                   Dm "s" 1 [Dm "w" 2 [Dm "$" 18 swork1],
                             Dm "m" 2 [],
                             Dm "t" 2 []]]

awork1 :: [Dms]
awork1 = [Dm "y" 4 [Dm "RY" 8 awork2],
          Dm "m" 5 [Dm "RM" 8 awork2],
          Dm "w" 6 [Dm "RW" 8 awork2],
          Dm "d" 7 [Dm "RD" 8 awork2]]

awork2 :: [Dms]
awork2 = [Dm "h" 9 [Dm "I" 10 [Dm "I" 11 awork3]], Dm "d" 9 [Dm "I" 10 [Dm "I" 11 awork3]]]

awork3 :: [Dms]
awork3 = [Dm "RH" 13 [Dm "DT" 0 [], Dm "N" 14 [Dm "JD" 0 []]]]

amoney1 :: [Dms]
amoney1 = [Dm "a" 16 [Dm "I" 17 lany], Dm "s" 16 [Dm "I" 17 lany]]

atodo1 :: [Dms]
atodo1 = [Dm "b" 24 atodo2, Dm "w" 24 atodo2, Dm "p" 24 atodo2,
          Dm "r" 24 atodo2, Dm "o" 24 atodo2]

atodo2 :: [Dms]
atodo2 = [Dm "$" 25 [Dm "JD" 26 [Dm "P" 0 []]]]

sworkf1 :: Int -> [Dms] -> [Dms]
sworkf1 m dms = [Dm "t" 5 [Dm "JM" m dms], Dm "p" 21 [Dm "I" 5 [Dm "JM" m dms]],
               Dm "n" 22 [Dm "I" 5 [Dm "JM" m dms]], Dm "e" 7 [Dm "JD" m dms]]

swork1 :: [Dms]
swork1 = [Dm "Saltw" 19 (sworkf1 20 swork2)]

swork2 :: [Dms]
swork2 = sworkf1 0 [] 

lany :: [Dms]
lany = [Dm "$" 0 []]

messages :: [String]
messages = ["0;enter the operation -- a: add, d: delete, s: show",
            "0;w: work, m: money, t: todo",
            "1;enter name",
            "0;y: yearly, m: monthly, w: weekly, d: exact day",
            "2;give a month and a day --ie. 120, 1023, 0920 ...",
            "3;give a day --ie. 20, 11, 31 ...",
            "4;su: Sunday, m: Monday, tu: Tuesday, w: Wednesday, th: Thursday, f: Friday, sa: Saturaday",
            "5;give an exact day -- ie. 20220420, 20231024 ...",
            "0;h: hourly wage, d: dayly wage",
            "6;enter the wage --ie. 1000, 1200 ...",
            "6;enter travelling expenses per day --ie. 880, 1200 ...",
            "7;enter start time --ie. 915, 1330, 1500 ...",
            "7;enter finish time --ie. 1300, 1600, 2330 ...",
            "8;from today? (Y/n) (Y is default and Enter means default)",
            "5;enter the day which the work starts --ie 20220927 20231221",
            "0;a: add, s: spent",
            "6;enter the amount",
            "1;description",
            "0;choose to show -- a:all, l:work list, t:sum of travels, w:sum of wages",
            "0;From -- t:this month, p:previous month, n:next month, e:exact month-day",
            "0;To -- t:this month, p:previous month, n:next month, e:exact month-day",
            "6;enter the number of months before: ie. 0(this month), 1(previous), 2...",
            "6;enter the number of months after: ie. 0(this month), 1(next), 2...",
            "0;b: book, w: work, p: print, r: report, o: other",
            "1;enter the description",
            "5;enter the deadline",
            "10;enter the todo format -- ie. 3-10,12-14,16,a,b,A,D,..."
           ]

errors :: [String]
errors = ["wrong command ", "enter something", "enter month and day",
          "enter day", "wrong week day", "day format YYYYMMDD",
          "enter numbers", "enter hour and minute", "Yes or No",
          "work length is negative", "not the todo format"
         ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  e <- doesFileExist tgPass
  c <- if e then fileOut
       else fileIn "" >> return ""
  sLoop c

sLoop :: Contents -> IO ()
sLoop c = do
  o <- comLoop "" "" 0 "" demands
  putStrLn o
  let (f:ord) = o
      cs = lines c
  nc <- case f of
    f' | f'=='a' || f'=='s' -> do
      b <- confirm "add"
      let ord' = if(f'=='s') then 's':ord else ord
          s = idSame ord' cs
      cs' <- if (b && s>(-1)) then do
               putStrLn "There is a data of the same name. "
               putStrLn (show (cs!!s))
               r <- confirm "replace"
               if r then return$replOrd s ord' cs else return cs
                    else return cs
      if b then do
              let nc = unlines (cs' ++ (if (s>(-1)) then [] else [ord']))
              fileIn nc
              putStrLn "wrote to myfe.txt. success!"
              return nc
           else putStrLn "add data -- canceled." >> return c
    'd' -> do
      let s = idSame ord cs
      cs' <- if (s>(-1)) then do
              putStrLn "The target data is"
              putStrLn (show (cs!!s))
              b <- confirm "delete"
              if b then return$delOrd s cs else return cs
                   else putStrLn "There is no target data-- delete canceled." >> return cs
      if(cs==cs') then return c
                  else do
                    let nc = unlines cs'
                    fileIn nc
                    putStrLn "delete a data from myfe.txt. success!" >> return nc
    _ -> return c
  if (f=='q') then return () else putStr ("\n"++nc++"\n") >> sLoop nc

replOrd :: Int -> Orders -> [Orders] -> [Orders]
replOrd id ord cs = take id cs ++ [ord] ++ drop (id+1) cs

delOrd :: Int -> [Orders] -> [Orders]
delOrd id cs = take id cs ++ drop (id+1) cs

idSame :: Orders -> [Orders] -> Int 
idSame ord cs = 
  let oname = head$sepChar ';' ord
      csname = map (head . (sepChar ';')) cs
   in if (elem oname csname) then getIndex oname csname else (-1)

confirm :: String -> IO Bool
confirm s = do
  putStrLn ("Are you sure to "++s++"? (Y/n)")
  y <- getLine
  if (y=="n" || y=="N" || y=="no") then return False else return True

noticeWeek :: Char -> IO ()
noticeWeek w = do
  let wn = weekTList !! (read [w]::Int)
  putStrLn ("For "++wn++":")

comLoop :: String -> String -> Int -> Orders -> Dms -> IO Orders 
comLoop _ _ _ o (Dm _ _ []) = return o
comLoop ex r co o dm@(Dm s i d) = do
  let ((Dm ns _ _):_) = if (d==[]) then [Dm "-" 0 []] else d
      (hs:ts) = ns
      r' = if(ts=="H") then if (co==1) then tail r else r else r
  if (ts=="H") then noticeWeek (head r) else return ()
  let (ecs:mes) = sepChar ';' (messages!!(i+co))
  g <- if (ex=="") then do
    mapM_ putStrLn mes 
    putStr "> "
    getLine
                   else return ex
  let (no, nd, nex) = checkInput g o d
  (no', nd') <- if (hs=='R') then repeatInput r' no nd ts else return (no, nd) 
  let iwr = ts=="H" && co==1 && o/="" && howLong (last$sepChar ';' o) g < 0
      ec = if iwr then 9 else read ecs 
      nd'' = if iwr then Er else nd'
  case nd'' of
    Er -> do
      putStrLn ("ERROR!!: "++(errors!!ec))
      putStrLn "--Press Enter To Continue--"
      getLine
      comLoop nex r co o dm 
    Qi -> return "q" 
    Rp -> do
      let co' = if (ts=="H" && (not iwr)) then if (co==0) then 1 else 0 else co
      comLoop nex r' co' no' dm
    _  -> do
      let nr = if (s=="w") then tail$last$sepChar ';' no else r
      no'' <- if(ns=="DT" && g=="") then today >>= (\td -> return (no'++td++";"))
                                    else return no'
      comLoop nex nr 0 no'' nd'

repeatInput :: String -> Orders -> Dms -> String -> IO (Orders, Dms) 
repeatInput r o d s = do
  if (d==Er || d==Qi) then return (o,d) else do
    y <- if (s=="H") then return "" else do
      putStr "Another Data? (Y/n) :"
      getLine
    if (y=="n" || y=="N" || y=="no") then return (o++";",d) else do
      let lo = last$sepChar ';' o
          len = length lo
          ip = case s of "M" -> len < 29; "W" -> len < 8; _ -> True
          ir = length r > 0
      if (ip && ir) then return (o, Rp) 
                    else do
              if (s=="H") then return (o, d) else do
                putStrLn "Can't add data!"
                return (o++";", d)
          

checkInput :: String -> Orders -> [Dms] -> (Orders, Dms, String) 
checkInput g o dm@(d:ds) =
  let cms = map (\(Dm m _ _) -> m) dm
      hc@(h:t) = head$cms
      len = length hc 
      iq = g==":q" || g=="exit"
      ich = not$elem h "$DIJPRS"
      (cm,ex) = if ich then (if (length g>=len) then (take len g,drop len g) else ("",""))
                       else if (g=="") then ("","") else
                          let hgs = head$sepChar ';' g; ln = length hgs in (hgs,drop (ln+1) g)
      nd = if iq then Qi else
           if ich then (if (elem cm cms) then dm!!(getIndex cm cms) else Er) else
           case h of
             'D' -> if (g=="n" || g=="N" || g=="No") then head$ds else d
             'S' -> if (isChar cm t) then d else Er
             'P' -> if (isTodo cm) then d else Er
             'I' -> if (isNum cm) then d else Er
             '$' -> d
             h' | h'=='R' || h'=='J' -> if (isDay t cm) then d else Er
             _   -> Er
      no = case hc of
             hc' | hc'=="$" || hc'=="I" || hc'=="P" -> o++cm++";"
             ('R':xs) -> o++(addDay (last$sepChar ';' o) cm xs)
             ('J':_)  -> o++cm++";"
             ('S':_)  -> o++cm++";"
             _   -> o++cm
      ex' = if (nd==Er || nd==Qi) then "" else ex
   in (no, nd, ex')

addDay :: Orders -> String -> String -> String
addDay lo g t =
  case t of
    "W" -> let r = show$getIndex g weeklist 
               ism = elem (head r) lo
            in if ism then "" else r
    _   -> g++";"

isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = (isDigit x) && (isNum xs)

isTodo :: String -> Bool
isTodo s = 
  let tdl = sepChar ',' s
   in and$map isto tdl

isto :: String -> Bool
isto s =
  if(elem '-' s) then 
      let (a:b:c) = sepChar '-' s
       in if(c==[]) then (toConstr$cvTd a)==(toConstr$cvTd b) else False
                 else if (cvTd s==Ot) then False else True

cvTd :: String -> Td 
cvTd [] = Ot
cvTd s@(h:t)
  | (isDigit h) && (isNum t)     = N (read s)
  | (not$isDigit h) && (isNum t) = LN h (read t)
  | t==[] && (not$isDigit h)     = L h
  | isStr s                      = S s
  | otherwise                    = Ot 

isChar :: String -> String -> Bool
isChar [] _ = True 
isChar (x:xs) str = (elem x str) && (isChar xs str)

isStr :: String -> Bool
isStr [] = True
isStr (x:xs) = (not$isDigit x) && (isStr xs)


isDay :: String -> String -> Bool
isDay t s =
  let s' = if (head s=='0') then tail s else s 
      len = length s'
   in case t of
        "Y" -> let (mo,dy) = if (len==3) then ([head s'],tail s') else (take 2 s',drop 2 s')
                   (moi,dyi) = (read mo::Int, read dy::Int)
                in len>2 && len<5 && moi>0 && moi<13 && dyi>0 && dyi<1+(daylist!!(moi-1))
        "M" -> let dyi = read s'::Int
                in dyi>0 && dyi<32
        "W" -> elem s' weeklist 
        "D" -> let (mo,dy) = if (len==7) then ([s'!!4],drop 5 s') 
                                         else (take 2 (drop 4 s'),drop 6 s')
                   (moi,dyi) = (read mo::Int, read dy::Int)
                in len>6 && len <9 && moi>0 && moi<13 && dyi>0 && dyi<1+(daylist!!(moi-1))
        "H" -> let (h,m) = if (len==3) then ([head s'],tail s') else (take 2 s',drop 2 s')
                   (hi,mi) = (read h::Int, read m::Int)
                in len>2 && len<5 && hi>=0 && hi<24 && mi>=0 && mi<60


fileOut :: IO Contents
fileOut = do
  h <- openFile tgPass ReadMode
  hSetEncoding h utf8
  con <- hGetContents h 
  putStr (con++"\n")
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
sepChar _ [] = []
sepChar ch [x]    = if (x==ch) then [[]] else [[x]]
sepChar ch (x:xs) = if (x==ch) then [[]]++(hd:tl)
                               else [x:hd]++tl
                          where (hd:tl) = sepChar ch xs

toHour :: String -> (Int, Int)
toHour s =
  let len = length s
      (ho,mi) = if (len==3) then ([head s],tail s) else (take 2 s,drop 2 s)
   in (read ho, read mi)

howLong :: String -> String -> Int
howLong s f =
  let (sho,smi) = toHour s
      (fho,fmi) = toHour f
      sami = sho * 60 + smi
      fami = fho * 60 + fmi
   in fami - sami

---------------------
