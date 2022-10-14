module Mydata where

type Contents = String
type Orders = String
data Dms = Dm String Int [Dms] | Rp | Er | Qi deriving (Eq,Show)
data Td = N Int | L Char | LN Char Int | S String | Ot deriving (Eq)

instance Show Td where
  show (N i) = show i
  show (L c) = [c]
  show (LN c i) = [c]++(show i)
  show (S s) = s
  show Ot = "Ot"

tgPass :: FilePath
tgPass = "myfe.txt"

weekTList :: [String]
weekTList = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

demands :: Dms
demands = Dm "" 0 [Dm "a" 1 [Dm "w" 2 [Dm "$" 3 awork1],
                             Dm "m" 2 [Dm "$" 15 amoney1],
                             Dm "t" 2 [Dm "$" 23 atodo1]],
                   Dm "c" 1 [Dm "w" 0 [],
                             Dm "m" 0 [],
                             Dm "t" 2 [Dm "$" 23 ctodo1]],
--                   Dm "d" 1 [Dm "w" 2 lany, Dm "m" 2 lany,
--                             Dm "t" 2 [Dm "$" 23 dtodo1]],
                   Dm "d" 30 ([Dm "s" 1 dall]++dall),
                   Dm "s" 1 [Dm "w" 2 [Dm "$" 18 swork1],
                             Dm "m" 2 [Dm "$" 27 smoney1],
                             Dm "t" 0 []]]

dall :: [Dms]
dall = [Dm "w" 2 lany, Dm "m" 2 lany, Dm "t" 2 [Dm "$" 23 dtodo1]]

awork1 :: [Dms]
awork1 = [Dm "y" 4 [Dm "RY" 8 awork2],
          Dm "m" 5 [Dm "RM" 8 awork2],
          Dm "w" 6 [Dm "RW" 8 awork2],
          Dm "d" 7 [Dm "RD" 8 awork2]]

awork2 :: [Dms]
awork2 = [Dm "h" 9 [Dm "I" 10 [Dm "I" 11 awork3]], Dm "d" 9 [Dm "I" 10 [Dm "I" 11 awork3]]]

awork3 :: [Dms]
awork3 = [Dm "RH" 13 ltdy]

amoney1 :: [Dms]
amoney1 = [Dm "a" 16 [Dm "I" 17 [Dm "$" 13 ltdy]],
           Dm "r" 16 [Dm "I" 17 [Dm "$" 13 ltdy]],
           Dm "s" 16 [Dm "I" 17 [Dm "$" 13 ltdy]]]

todof1 :: Int -> [Dms] -> [Dms]
todof1 m dms = [Dm "b" m dms, Dm "w" m dms, Dm "p" m dms, Dm "r" m dms, Dm "o" m dms]

atodo1 :: [Dms]
atodo1 = todof1 24 atodo2

atodo2 :: [Dms]
atodo2 = [Dm "$" 25 [Dm "JD" 26 [Dm "P" 0 []]]]

ctodo1 :: [Dms]
ctodo1 = todof1 24 ctodo2

ctodo2 :: [Dms]
ctodo2 = [Dm "$" 29 [Dm "c" 26 [Dm "P" 0 []], Dm "n" 26 [Dm "P" 0 []]]]

dtodo1 :: [Dms]
dtodo1 = todof1 24 lany

sworkf1 :: Int -> [Dms] -> [Dms]
sworkf1 m dms = [Dm "t" 5 [Dm "JM" m dms], Dm "p" 21 [Dm "I" 5 [Dm "JM" m dms]],
               Dm "n" 22 [Dm "I" 5 [Dm "JM" m dms]], Dm "e" 7 [Dm "JD" m dms]]

swork1 :: [Dms]
swork1 = [Dm "Saltw" 19 (sworkf1 20 swork2)]

swork2 :: [Dms]
swork2 = sworkf1 0 [] 

smoney1 :: [Dms]
smoney1 = [Dm "Saclr" 0 []]

lany :: [Dms]
lany = [Dm "$" 0 []]

ltdy :: [Dms]
ltdy = [Dm "DT" 0 [], Dm "N" 14 [Dm "JD" 0 []]]

messages :: [String]
messages = ["0;enter the operation -- a: add, c: change, d: delete, s: show",          --0
            "0;w: work, m: money, t: todo",
            "1;enter name",
            "0;y: yearly, m: monthly, w: weekly, d: exact day",
            "2;give a month and a day --ie. 120, 1023, 0920 ...",
            "3;give a day --ie. 20, 11, 31 ...",                                       --5
            "4;su: Sunday, m: Monday, tu: Tuesday, w: Wednesday, th: Thursday, f: Friday, sa: Saturaday",
            "5;give an exact day -- ie. 20220420, 20231024 ...",
            "0;h: hourly wage, d: dayly wage",
            "6;enter the wage --ie. 1000, 1200 ...",
            "6;enter travelling expenses per day --ie. 880, 1200 ...",                 --10
            "7;enter start time --ie. 915, 1330, 1500 ...",
            "7;enter finish time --ie. 1300, 1600, 2330 ...",
            "8;from today? (Y/n) (Y is default and Enter means default)",
            "5;enter the day which the work starts --ie 20220927 20231221",
            "0;a: add, r: remain, s: spent",                                           --15
            "6;enter the amount",
            "1;description",
            "0;choose to show -- a:all, l:work list, t:sum of travels, w:sum of wages",
            "0;From -- t:this month, p:previous month, n:next month, e:exact month-day",
            "0;To -- t:this month, p:previous month, n:next month, e:exact month-day", --20
            "6;enter the number of months before: ie. 0(this month), 1(previous), 2...",
            "6;enter the number of months after: ie. 0(this month), 1(next), 2...",
            "0;b: book, w: work, p: print, r: report, o: other",
            "1;enter the description",
            "5;enter the deadline",                                                    --25
            "10;enter the todo format -- ie. 3-10,12-14,16,a,b,A,D,...",
            "0;choose to show -- a:all, c:change, l:list",
            "6;how many data to show?",
            "1;c: complete and delete, n: new task add",
            "0;s: show, w: work, m: money, t: todo"                                    --30
           ]

errors :: [String]
errors = ["wrong command ", "enter something", "enter month and day",
          "enter day", "wrong week day", "day format YYYYMMDD",
          "enter numbers", "enter hour and minute", "Yes or No",
          "work length is negative", "not the todo format"
         ]

nameTodo :: String -> String
nameTodo s =
  case s of "ma" -> "Math"; "en" -> "English"; "ni" -> "Kokugo"; "ph" -> "Physics";
            "bi" -> "Biology"; "ch" -> "Chemistry"; "hi" -> "History"; "ge" -> "Geology";
            "te" -> "Technique"; "li" -> "Life"; "pe" -> "Physical Excersize";
            "mu" -> "Music"; "ar" -> "Art"; "so" -> "Society"; "ec" -> "Economy"; _ -> s

nameMoney :: String -> String
nameMoney s =
  case s of "wa" -> "Wallet"; "co" -> "Cozeni"; "sa" -> "Saving"; _ -> s

todoType :: Char -> String
todoType c = 
  case c of 'b' -> "Book"; 'w' -> "Work"; 'p' -> "Print"; 'r' -> "Report"; 'o' -> "Other"

