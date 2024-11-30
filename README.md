# Cation
이 프로젝트의 목적은 `object`와 `objective` 서로와 또는 자기 자신과 가능한  
자연어에는 없는 새로운 `FUNCTOR`를 잔뜩 만들어 내는 것입니다.  물론 그러려면  `Env` 가 저장되어야겠지만 아직 그런 기능은 없습니다. 추가예정

   
원래는 자연어 처리를 하려고 했습니다. 근데 너무 끔찍해서 그냥 인공어를 만들고 싶어졌는데, 다른 게 된 것 같습니다.  
아직 파서가 없습니다. 그러니까 아래처럼 입력하시면 됩니다.  
만드는중..

*더 나은 수식 구조를 위해 완전히 처음부터 수정중..*


```
data OBJECT = String
        deriving (Eq, Show)

data SENTENCE a = MODIFIER a
                | MODIFICAND (SENTENCE a, SENTENCE a)

data FUNCTOR = FM [CAT] VAR-- 수식어([명사]) 
                | NM [CAT] VAR -- [수식어] (수식어)
                | F [CAT] VAR-- [명사] -> 명사
                | N [CAT] VAR -- [수식어] -> 수식어


data CAT = SENTENCE CAT
        | OBJECT
        | NULL

```


# 규칙

단어에는 간단한 타입이 있습니다.
```
나중에 씀
```

todo: 파서, Env 사전 파일

