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


data FUNCTOR = FM [OBJECT] VAR -- 수식어([명사]) 
            | NM [OBJECTIVE] VAR -- [수식어] (수식어)
            | AM [VAR] VAR -- [여기서 정의한 이름] (여기서 정의한 이름)
            | F [OBJECT] VAR -- [명사] -> 명사
            | N [FUNCTOR] VAR -- [수식어] -> 수식어
            | A VAR VAR -- [여기서 정의한 이름] -> 여기서 정의한 이름


       deriving (Eq, Show)

data CAT = Object OBJECT
            | Objective OBJECTIVE
            | Functor FUNCTOR
       deriving (Eq, Show)


type VAR = String
type Env = [(VAR, CAT)]

printOBJ :: CAT -> IO ()
printOBJ cat = putStrLn $  cattoStr cat


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
    = fromMaybe (error $ cattoStr cat ++ " :not found") $ cat `lookup` map swap env

def :: VAR -> CAT -> Env -> Env
def x c = extEnv [(x,c)]



morpF :: FUNCTOR -> CAT -> Env -> CAT
morpF f x e =
            case f of
                    FM o1 o2 ->
                        let Objective o = appEnv o2 e in
                        case x of
                            Object cat -> if cat `elem` o1 then Object (o ++ "/" ++ cat) else error $ "notBelongObj: error caused by you " ++ cat ++ " is not " ++ concatMap (++", ") o1
                            Objective cat -> error $ "you caused serious grammer issue " ++ cat ++ " is not noun"
                            Functor cat -> morpF cat (Object o) e
                    NM o1 o2 ->
                        let Objective o = appEnv o2 e in
                            case x of
                                Objective cat ->
                                    if cat `elem` o1 then
                                        Objective $ o ++ cat
                                        else error $ "notBelongFn: error caused by you" ++ o2 ++ " doesn`t exist"
                                Object cat ->
                                        if cat `elem` o1 then
                                        Object $ o ++ cat
                                        else error $ "notBelongFn: error caused by you" ++ o2 ++ " doesn`t exist"
                    AM xs v -> let vs = map (`appEnv` e) xs in
                            let v2 = appEnv v e in
                            case (head vs, v2) of
                            (Object object, Object obj)->
                                morpF (F (map (\(Object x) -> x) vs) v) x e
                            (Object object, Objective obj)->
                                morpF (F (map (\(Object x) -> x) vs) v) x e
                            (Objective objective,  Objective obj)->
                                morpF (NM (map (\(Objective x) -> x) vs) v) x e
                            (Functor functor, _)->
                                error $ "you made this program doomed " ++ concatMap cattoStr vs ++ " is functor"
                            _ -> error $ "your life wrong because " ++ concatMap cattoStr vs ++ " <- " ++ cattoStr v2 ++ " not exist"
                    F o1 o2 ->
                        let Object o = appEnv o2 e in
                        case x of
                            Object object ->
                                if object `elem` o1 then Object (object ++ "/" ++ o) else error $ "notBelongMorp: error caused by you " ++ o ++ " is Dead"
                            Objective objective ->
                                if objective `elem` o1 then Object (objective ++ " " ++ o) else error $ "notBelongMorp: error caused by you " ++ o ++ " is Dead"
                            Functor functor -> error $ "you wrote wrong" ++ cattoStr x ++ "is functor"
                    N o1 o2 ->
                        case appEnv o2 e of
                            Functor fun ->
                                let Functor functor = x in
                                        if functor `elem` o1 then morpF fun x e else error $ "notBelongMorp: error caused by you " ++ cattoStr x ++ " is Dead"
                            Objective obj ->
                                let Object object = x in
                                    Objective (obj  ++ object)

                    A x1 x2 -> let v = appEnv x1 e in
                            let v2 = appEnv x2 e in
                            case (v, v2) of
                            (Object object, Object obj)->
                                v2
                            (Functor functor, Objective obj)->
                                v2
                            (_, Functor functor)->
                                v2
                            _ -> error $ "i wrote this error message for you its so gross " ++  cattoStr v  ++ " <- " ++ cattoStr v2 ++ " can`t exist like your happiness"

                    _ -> error "UNREACHABLE CODE REACHED!!"



morp :: VAR -> VAR -> Env -> CAT
morp c x' e =
    case appEnv c e of
        Object o -> let x = appEnv x' e in
            case x of
                Objective cat -> Object (cat ++ " " ++ o)
                Object cat -> Object (o ++ cat)
                Functor cat -> morp o c $ extEnv [(o, x), (c, Functor cat)] e
        Objective o -> let x = appEnv x' e in
            case x of
                Objective cat -> Objective (cat ++ " " ++ o)
                Object cat -> Objective (o ++ " " ++ cat)
                Functor cat -> morp o c $ extEnv [(o, x), (c, Functor cat)] e
        Functor f' -> let x = appEnv x' e in
            morpF f' x e



dic :: IO ()
dic = do
    system "chcp 65001"
    setLocaleEncoding utf8
    handle <- openFile "dic.txt" ReadWriteMode
    -- encoding <- hGetEncoding handle
    -- hSetEncoding handle$ fromMaybe (error "?") encoding
    -- hSetEncoding stdin $ fromMaybe (error "?") encoding
    x <- getLine
    cat <- getLine
    let cat' = catParser cat

    dic <- hGetContents handle
    -- print =<< hGetEncoding handle
    --putStrLn dic
    let dict = envParser  dic
    let str = dictoStr $ def x cat' dict
    putStrLn dic
    hClose handle

    writeFile "dic.txt" cat



dictoStr :: Env -> String

dictoStr (e:es) =
    let (v, c) = e in
        v ++ "^" ++ cattoStr c ++ "$" ++ dictoStr es
dictoStr e = []

envParser :: String -> Env
envParser s = []

catParser :: String -> CAT
catParser = Object


cattoStr :: CAT -> String
cattoStr c = case c of
    Object o -> o
    Objective o -> "*" ++ o
    Functor f ->
        case f of
            FM os v -> "@" ++ v ++ "+" ++ concatMap (++ ",") os  ++ "@"
            NM os c -> "#" ++c ++"+" ++  concatMap ((++ ",") . ("*" ++))  os ++ "#"
            F os c -> "$" ++ c++"->" ++  concatMap (++ ",") os ++ "$"
            N (f':fs) c ->
                    "%" ++ cattoStr (Functor f') ++ "," ++ functoStr fs ++ "%"


functoStr :: [FUNCTOR] -> String
functoStr (f:fs) = case f of
            FM os v -> "@" ++ (take $ length os*2 - 1) (concatMap (++ ",") os)  ++ "@"
            NM os c -> "#" ++ (take $ length os*3 - 1) (concatMap ((++ ",") . ("*" ++))  os) ++ "#"
            F os c -> "$" ++ (take $ length os*2 - 1) (concatMap (++ ",") os) ++ "$"
            N (f':fs') c ->
                    "%" ++ cattoStr (Functor f')++ functoStr fs' ++ "%"










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