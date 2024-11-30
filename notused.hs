{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import Data.Char
import Data.Maybe
import Data.Tuple
import Data.List
import System.IO
import Debug.Trace
import GHC.IO.Encoding
import System.Process
-- import Data.ByteString (fromFilePath)
-- import Data.ByteString.Char8 as C8 (hGetContents, pack, unpack)

type OBJECTIVE = String --용언
type OBJECT = String --체언




data FUNCTOR = FM [CAT] VAR-- 수식어([명사]) 
                | NM [CAT] VAR -- [수식어] (수식어)
                | F [CAT] VAR-- [명사] -> 명사
                | N [CAT] VAR -- [수식어] -> 수식어
        deriving (Eq, Show)

data M = M VAR VAR
       deriving (Eq, Show)

type SENTENCE = [CAT]



type CATS = (CAT, CAT)
data CAT = Object OBJECT
            | Objective OBJECTIVE
            | Functor FUNCTOR
            | M' M
            | Cats (CAT, CAT)
            | NULL
       deriving (Eq, Show)


type VAR = String
type Env = [(VAR, CAT)]

printOBJ :: [CAT] -> IO ()
printOBJ cat =  mapM_ (putStr . cattoStr) cat >> putStrLn ""

-- printENV :: Env -> IO ()
-- printENV env = putStrLn $ envtoStr env

extEnv :: Env -> Env -> Env
extEnv e0 e1
   = e0 ++ e1

appEnv :: VAR -> Env -> CAT
appEnv v [] = error $ "all your fault: vocab" ++ v ++ " not found"
appEnv v (e:es) =
    let (var, value) = e in
        if var == v then value else appEnv v es

findOBJ :: VAR -> Env -> CAT
findOBJ var env
    = fromMaybe (error $ var   ++ ": vocab not found") $ var `lookup` env

findVar :: CAT -> Env -> VAR
findVar cat env
    = fromMaybe (error $ {-cattoStr cat ++-} " :not found") $ cat `lookup` map swap env

def :: VAR -> CAT -> Env -> Env
def x c = extEnv [(x,c)]


morpS :: CAT -> SENTENCE -> Env -> SENTENCE
morpS f s e =
    case f of
        Functor f ->
    -- let Just (init, last) = ((\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse x)) in
            map (flip (morpF f) e) s
        M' m ->
            morpM m s e
        Objective o -> 
        _ -> error $ "because of you i lost hope "++ show f ++ " is nothing"
        

morpFold :: [CAT]-> SENTENCE -> Env -> SENTENCE
morpFold fs s e = foldl (\ s f -> morpS f s e) s fs

morpMap :: [CAT]-> SENTENCE -> Env -> SENTENCE
morpMap fs s e = concatMap ((\ s f -> morpS f s e) s) fs

morpF :: FUNCTOR -> CAT -> Env -> CAT
morpF f x e =
        case x of
            NULL -> NULL
            Cats (c1, c2) ->
                Cats (morpF f c1 e, morpF f c2 e)
            _ ->
                case f of
                F c v ->
                    let v' = appEnv v e in
                        case x of
                            Object cat -> if v' `elem` c then Cats (NULL, v') else Cats  (NULL, x) -- error $ "notBelongObj: error caused by you " ++ cat ++ " is not " ++ concatMap (++", ") o1
                            Objective cat -> Cats (NULL, x)-- error $ "you caused serious grammer issue " ++ cat ++ " is not noun"
                            Functor cat -> morpF cat v' e
                N c v->
                    let v' = appEnv v e in
                        case x of
                            Objective cat ->
                                if v' `elem` c then
                                    Cats (v', x)
                                    else Cats (NULL, x) --error $ "notBelongFn: error caused by you" ++ cat ++ " doesn`t exist"
                            Object cat ->
                                    if v' `elem` c then
                                    Cats (v', x)
                                    else Cats (NULL, x)  --error $  "notBelongFn: error caused by you" ++ cat ++ " doesn`t exist"
                            Functor cat -> morpF cat v' e
                FM c v ->
                    let v' = appEnv v e in
                        case x of
                            Object cat -> if v' `elem` c then Cats (v', x) else Cats (NULL, x) -- error $ "notBelongObj: error caused by you " ++ cat ++ " is not " ++ concatMap (++", ") o1
                            Objective cat -> Cats (NULL, x)-- error $ "you caused serious grammer issue " ++ cat ++ " is not noun"
                            Functor cat -> morpF cat v' e
                NM c v->
                    let v' = appEnv v e in
                        case x of
                            Objective cat ->
                                if v' `elem` c then
                                    Cats (v', x)
                                    else Cats (NULL, x) --error $ "notBelongFn: error caused by you" ++ cat ++ " doesn`t exist"
                            Object cat -> Cats (NULL, x)  --error $  "notBelongFn: error caused by you" ++ cat ++ " doesn`t exist"
                            Functor cat -> morpF cat v' e--             case f of

morpM :: M -> [CAT] -> Env -> [CAT]
morpM  (M m v) x e =
    let mv = (`appEnv` e) m in
        case mv of
            Functor f -> map (flip (morpF f) e) x
            M' m -> morpM m x e

morp :: [VAR] -> [VAR] -> Env -> SENTENCE
morp fs xs e =
    let f = map (`appEnv` e) fs in
    let x = map (`appEnv` e) xs in
        morpMap f x e

morp' :: VAR -> [VAR] -> Env -> SENTENCE
morp' f x' e =
    let f' = (`appEnv` e) f in
        let x = map (`appEnv` e) x'  in
            case f' of
                Functor f ->
                  morpS f' x e
                Object o ->
                    error$  "your fault" ++ o ++ " is noun"
                Objective o ->
                    Cats (NULL, f') : x
                M' m->
                    morpM m x e
                Cats (c1, c2) ->
                    error "efewf"
                NULL -> error "Null"


-- dic :: IO ()
-- dic = do
--     system "chcp 65001"
--     setLocaleEncoding utf8
--     handle <- openFile "dic.txt" ReadWriteMode
--     -- encoding <- hGetEncoding handle
--     -- hSetEncoding handle$ fromMaybe (error "?") encoding
--     -- hSetEncoding stdin $ fromMaybe (error "?") encoding
--     x <- getLine
--     cat <- getLine
--     let cat' = catParser cat

--     dic <- hGetContents handle
--     -- print =<< hGetEncoding handle
--     --putStrLn dic
--     let dict = envParser  dic
--     let str = envtoStr $ def x cat' dict
--     putStrLn dic
--     hClose handle

--     writeFile "dic.txt" cat



-- envtoStr :: Env -> String
-- envtoStr (e:es) =
--     let (v, c) = e in
--         "(" ++ v ++ "^" ++ cattoStr c ++ "$" ++ ")" ++ envtoStr es
-- envtoStr e = []

-- envParser :: String -> Env
-- envParser s = []

-- catParser :: String -> CAT
-- catParser = Object


cattoStr :: CAT ->  String
cattoStr c = case c of
    Object o -> o
    Objective o -> "*" ++ o
    Functor f ->
        case f of
            FM os v -> "@" ++ v ++ "+" ++ concatMap cattoStr os  ++ "@"
            NM os c -> "#" ++c ++"+" ++  concatMap cattoStr  os ++ "#"
            F os c -> "$" ++ c++"->" ++  concatMap cattoStr os ++ "$"
            N fs c ->
                    "%" ++ c ++ "," ++ concatMap cattoStr fs ++ "%"
    M' (M v1 v2) -> "?" ++ v1 ++ "," ++ v2 ++ "?"
    Cats (c1, c2) -> "(" ++ cattoStr c1 ++ "/" ++ cattoStr c2 ++ ")"
    NULL -> "_"
-- functoStr :: [FUNCTOR] -> String
-- functoStr (f:fs) = case f of
--             FM os v -> "@" ++ (take $ length os*2 - 1) (concatMap (++ ",") os)  ++ "@"
--             NM os c -> "#" ++ (take $ length os*3 - 1) (concatMap ((++ ",") . ("*" ++))  os) ++ "#"
--             F os c -> "$" ++ (take $ length os*2 - 1) (concatMap (++ ",") os) ++ "$"
--             N (f':fs') c ->
--                     "%" ++ cattoStr (Functor f')++ functoStr fs' ++ "%"










            --tokenizer--
data TokenType = LEFTPARAN
                | RIGHTPARAN
                | LEFTBRACE
                | RIGHTBRACE
                | COLON
                | PLUS
                | MINUS
                | STAR
                | SLASH
                | SEMICOLON
                | SHARP
                | WAVEDASH
                | DASH
                | GREATERTHEN
                | NUM
                | UPPERCASE
                | LOWERCASE
                | EQUAL
                | AT
            deriving (Show)

type Token = ([Char], TokenType)

tokenizer :: String -> [Token]
tokenizer (s:ss)  =
    case s of
        '(' -> ([s], LEFTPARAN) : tokenizer ss
        ')' -> ([s], RIGHTPARAN) : tokenizer ss
        '{' -> ([s], LEFTBRACE) : tokenizer ss
        '}' -> ([s], RIGHTBRACE) : tokenizer ss
        ':' -> ([s], COLON) : tokenizer ss
        ';' -> ([s], SEMICOLON) : tokenizer ss
        '+' -> ([s], PLUS) : tokenizer ss
        '-' -> ([s], MINUS) : tokenizer ss
        '*' -> ([s], STAR) : tokenizer ss
        '/' -> ([s], SLASH) : tokenizer ss
        '#' -> ([s], SHARP) : tokenizer ss
        '~' -> ([s], WAVEDASH) : tokenizer ss
        '>' -> ([s], GREATERTHEN) : tokenizer ss
        '=' -> ([s], EQUAL) : tokenizer ss
        '@' -> ([s], AT) : tokenizer ss
        ' ' -> tokenizer ss
        _ -> if isDigit s
                then (takeWhile isDigit (s:ss), NUM)
                    : tokenizer (dropWhile isDigit ss)
                else if isUpper s
                    then (takeWhile isAlphaNum (s:ss), UPPERCASE)
                        : tokenizer (dropWhile isAlphaNum ss)
                    else if isLower s
                        then (takeWhile ((||) <$> isDigit <*> isLower) (s:ss), LOWERCASE)
                            : tokenizer (dropWhile ((||) <$> isDigit <*> isLower) ss)
                        else error "UserFault"
tokenizer [] = []