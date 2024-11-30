data OBJECT = String
        deriving (Eq, Show)

data SENTENCE a = MODIFIER a
                | MODIFICAND (SENTENCE a, SENTENCE a)

data FUNCTOR = FM [CAT] VAR-- 수식어([명사]) 
                | NM [CAT] VAR -- [수식어] (수식어)
                | F [CAT] VAR-- [명사] -> 명사
                | N [CAT] VAR -- [수식어] -> 수식어
        deriving (Eq, Show)

data CAT = SENTENCE CAT
        | OBJECT
        | NULL
        deriving (Eq, Show)

type VAR = String
type Env = [(VAR, CAT)]

