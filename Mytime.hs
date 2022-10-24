module Mytime (today,howLong,hmDays,isDay,addDay) where

import Data.Time.LocalTime(getZonedTime,ZonedTime(zonedTimeToLocalTime),LocalTime(localDay))
import Useful(getIndex)

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

weeklist :: [String]
weeklist = ["su","m","tu","w","th","f","sa"]

today :: IO String 
today = do
  (a:b:c:d:_:e:f:_:g) <- show <$> localDay <$> zonedTimeToLocalTime <$> getZonedTime
  return (a:b:c:d:e:f:g)

howLong :: String -> String -> Int
howLong s f =
  let (sho,smi) = toHour s
      (fho,fmi) = toHour f
      sami = sho * 60 + smi
      fami = fho * 60 + fmi
   in fami - sami

hmDays :: String -> String -> Int
hmDays fday sday =
  let (fy,fm,fd) = sepday fday
      (sy,sm,sd) = sepday sday
      fal = if (fm>1) then (sum$take (fm-1) daylist)+fd else fd
      sal = if (sm>1) then (sum$take (sm-1) daylist)+sd else sd
      fal' = if (uru fy && fm>2) then fal+1 else fal
      sal' = if (uru sy && fm>2) then sal+1 else sal
   in (dfYdays fy sy) + (sal'-fal')

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

addDay :: String -> String -> String -> String
addDay lo g t =
  case t of
    "W" -> let r = show$getIndex g weeklist 
               ism = elem (head r) lo
            in if ism then "" else r
    _   -> g++";"

uru :: Int -> Bool
uru y = let r1 = mod y 4 == 0
            r2 = mod y 100 == 0
            r3 = mod y 400 == 0
         in r3 || (r1 && not r2)

toHour :: String -> (Int, Int)
toHour s =
  let len = length s
      (ho,mi) = if (len==3) then ([head s],tail s) else (take 2 s,drop 2 s)
   in (read ho, read mi)

sepday :: String -> (Int,Int,Int)
sepday (a:b:c:d:e:f:g) = (read (a:b:c:d:[]), read (e:f:[]), read g)

dfYdays :: Int -> Int -> Int
dfYdays fy sy =
  if (fy==sy) then 0
              else (if (uru fy) then 366 else 365) + (dfYdays (fy+1) sy)

