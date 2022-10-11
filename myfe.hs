import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Useful(getIndex,sepChar,joinChar,sorting,isNum,isChar,chooseData,replCon,delCon)
import Mydate(today,howLong,hmDays,isDay,addDay)
import Mydata(Orders,Contents,Dms(..),demands,messages,errors,weekTList,nameMoney)
import Myfile(fileIn,fileOut,isFile)
import Todo(isTodo,conTodo,showTodo)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  e <- isFile 
  c <- if e then fileOut
       else fileIn "" >> return ""
  sLoop c

adjustOrd :: Orders -> (Char,Orders)
adjustOrd [] = (' ',"")
adjustOrd (a:[]) = (a,"")
adjustOrd (a:b:ord) =
  case a of
    'a' -> if(b=='t') then
              (a,(b:ord)++(joinChar ',' (map show (conTodo$last$sepChar ';' ord)))++";")
                      else (a,b:ord)
    's' -> ('a',a:b:ord)
    _   -> (a,b:ord)

changeData :: Orders -> Orders -> Orders
changeData pd (t:ord) =
  case t of
    't' -> let pdl = sepChar ';' pd
               odl = sepChar ';' ord
               ptd = sepChar ',' (last pdl)
               otd = map show (conTodo$tail$last odl)
               fl = head$last odl
               nptd = if(fl=='c') then joinChar ',' (dataSub ptd otd)
                                  else joinChar ',' (dataAdd ptd otd)
            in joinChar ';' ((init pdl)++[nptd])
    _   -> pd

dataSub :: Eq a => [a] -> [a] -> [a]
dataSub org [] = org 
dataSub org (t:ts) =
  let ie = elem t org
      id = if ie then getIndex t org else (-1)
   in if (id>(-1)) then dataSub (delCon id org) ts
                   else dataSub org ts

dataAdd :: Eq a => [a] -> [a] -> [a]
dataAdd org [] = org 
dataAdd org (t:ts) =
  let ie = elem t org
   in if ie then dataAdd org ts
            else dataAdd (org++[t]) ts

sLoop :: Contents -> IO ()
sLoop c = do
  tdy <- today
  mapM_ putStrLn (showData c)
  mapM_ putStrLn (showTodo c tdy)
  o <- comLoop "" "" 0 "" demands
  putStrLn o
  let cs = lines c
      (f,ord) = adjustOrd o
      im = head ord=='m'
      it = head ord=='t'
      dpth = if it then 2 else 1
      ids = idSame dpth ord cs
  nc <- case f of
    'a' -> do
      b <- confirm "add"
      cs' <- if (b && ids>(-1) && not im) then do
               putStrLn "There is a data of the same name. "
               putStrLn (show (cs!!ids))
               r <- confirm "replace"
               if r then return$replCon ids ord cs else return cs
                    else return cs
      if b then do
              let nc = unlines (cs' ++ (if (ids>(-1) && not im) then [] else [ord]))
              fileIn nc
              putStrLn "wrote to myfe.txt. success!"
              return nc
           else putStrLn "add data -- canceled." >> return c
    'c' -> do
      cs' <- if (ids>(-1)) then do
              let pd = cs!!ids
              putStrLn "The data is"
              putStrLn (show pd)
              let nord = changeData pd ord 
              return$replCon ids nord cs
                           else do
              putStrLn "There is no changeable data" >> return cs
      b <- confirm "change"
      if b then do
              let nc = unlines cs'
              fileIn nc
              putStrLn "wrote to myfe.txt. success!"
              return nc
           else putStrLn "change data -- canceled." >> return c
    'd' -> do
      cs' <- if (ids>(-1)) then do
              putStrLn "The target data is"
              putStrLn (show (cs!!ids))
              b <- confirm "delete"
              if b then return$delCon ids cs else return cs
                   else putStrLn "There is no target data-- delete canceled." >> return cs
      if(cs==cs') then return c
                  else do
                    let nc = unlines cs'
                    fileIn nc
                    putStrLn "delete a data from myfe.txt. success!" >> return nc
    _ -> return c
  if (f=='q') then return () else putStr ("\n"++nc++"\n") >> sLoop nc

idSame :: Int -> Orders -> [Orders] -> Int 
idSame dpt ord cs = 
  let oname = concat$take dpt (sepChar ';' ord)
      csname = map (concat . (take dpt) . (sepChar ';')) cs
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

showData :: Contents -> [String]
showData [] = []
showData c =
  let cs = lines c
      sds = chooseData "s" cs
   in showFunc cs sds

showFunc :: [String] -> [String] -> [String]
showFunc _ [] = [[]]
showFunc cs ((t:ts):xs) = 
  case t of
    'm' -> let ords = sepChar ';' ts
               nm = head ords
               dls = chooseData ("m"++nm++";") cs
               dls' = map (head . (sepChar ';')) dls
               days = map (last . (sepChar ';')) dls
               sdl = sorting$zip days dls'
               rsl = sumUp sdl "" 0
               ic = elem 'c' (ords!!1)
               lrs = length rsl
            in if (ic && lrs>1) then (("Money: "++(nameMoney nm)):(showMC rsl ("",0)))
                                      ++(showFunc cs xs) 
                                else showFunc cs xs 
    _   -> showFunc cs xs 
               
showMC :: [(String,Int)] -> (String,Int) -> [String]
showMC [] _ = []
showMC ((day,amo):xs) ("",0) = showMC xs (day,amo)
showMC ((day,amo):xs) (pday,pamo) =
  let dl = hmDays day pday
      sa = amo-pamo
      ip = sa>=0
      rate = floor$(fromIntegral sa)/(fromIntegral dl)
   in [pday++" to "++day++": change: "++(show sa)
           ++" ("++(if ip then "" else "-")++(show rate)++"/day)"]
      ++ showMC xs (day,amo)

sumUp :: [(String,String)] -> String -> Int -> [(String,Int)]
sumUp [] pday pam = [(pday,pam)]
sumUp ((day,(fl:amo)):xs) pday pam =
  if (day==pday) then 
      let nam = case fl of 'a' -> pam+(read amo); 'r' -> read amo
       in sumUp xs day nam
                 else if (pday=="") then sumUp xs day (read amo)
                                    else (pday,pam):(sumUp xs day (read amo))
      
