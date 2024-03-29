{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Parser where

import Course.Core
import Course.Person
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional
import Data.Char

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

type Input = Chars -- no order of execution
-- do whatever ?

-- down to 
data ParseResult a =
    UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | UnexpectedString Chars
  | Result Input a   -- input no error
  deriving Eq

instance Show a => Show (ParseResult a) where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    stringconcat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    stringconcat ["Unexpected character: ", show [c]]
  show (UnexpectedString s) =
    stringconcat ["Unexpected string: ", show s]
  show (Result i a) =
    stringconcat ["Result >", hlist i, "< ", show a]


    -- parseresult fmaps.. a to b. parseresult a to parseresult b.
instance Functor ParseResult where
  _ <$> UnexpectedEof =
    UnexpectedEof
  _ <$> ExpectedEof i =
    ExpectedEof i
  _ <$> UnexpectedChar c =
    UnexpectedChar c
  _ <$> UnexpectedString s =
    UnexpectedString s
  f <$> Result i a =
    Result i (f a)




-- Convenience functions
-- Function to determine is a parse result is an error.
isErrorResult ::
  ParseResult a
  -> Bool
isErrorResult (Result _ _) =
  False
isErrorResult UnexpectedEof =
  True
isErrorResult (ExpectedEof _) =
  True
isErrorResult (UnexpectedChar _) =
  True
isErrorResult (UnexpectedString _) =
  True

-- | Runs the given function on a successful parse result. Otherwise return the same failing parse result.
onResult ::
  ParseResult a
  -> (Input -> a -> ParseResult b)
  -> ParseResult b
onResult UnexpectedEof _ =
  UnexpectedEof
onResult (ExpectedEof i) _ =
  ExpectedEof i
onResult (UnexpectedChar c) _ =
  UnexpectedChar c
onResult (UnexpectedString s)  _ =
  UnexpectedString s
onResult (Result i a) k =
  k i a

  -- -- #IMPORTANT KEY LINE of code here
  -- - -- - - - - - - 
-- - -- - - - - - - P is the constructor. one P to get parser
-- data type          - P constructor Parser ( takes one argument. type)
-- might make that function arg as lambdainput \input

data Parser a = P (Input -> ParseResult a)
-- - -- - - - - - - 
-- - -- - - - - - - 
-- - -- - - - - - - 


parse ::
  Parser a
  -> Input
  -> ParseResult a
parse (P p) =
  p

-- | Produces a parser that always fails with @UnexpectedChar@ using the given character.
unexpectedCharParser ::
  Char
  -> Parser a
unexpectedCharParser c =
  P (\_ -> UnexpectedChar c)

--- | Return a parser that always returns the given parse result.
---
--- >>> isErrorResult (parse (constantParser UnexpectedEof) "abc")
--- True
constantParser ::
  ParseResult a
  -> Parser a
constantParser =
  P . const

-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
character ::
  Parser Char
character = P (\input -> 
  case input of 
    Nil -> UnexpectedEof
    ch:.rest -> Result rest ch)

-- 
-- Result takes in this order: the rest of the input (rest) and then the thing producing c ch char.
-- P (\s -> case s of Nil -> UnexpectedEof
--   (c:.r) -> Result r c)

-- error "todo: Course.Parser#character" 
-- I need to make a parser so what
-- do i call?  P
-- if empty then EOF error. check type of input. either Nil or cons
-- if nil - unexpectedEOF. if cons then it is char with rest of the input.
-- expect a parseresult of characters.
-- :info ParseResult
-- 5 possible way to make a ParseResult char. 4 fails 1 succeeding constructor.



-- | Parsers can map.
-- Write a Functor instance for a @Parser@.
--
-- >>> parse (toUpper <$> character) "amz"
-- Result >mz< 'A'
instance Functor Parser where
  (<$>) ::
    (a -> b)
    -> Parser a
    -> Parser b
  (<$>) = 
    -- \f -> _ 
    -- \f -> \p -> _  -- :r i need a parser b. use capital P
    -- \f -> \p -> P _
    -- \f -> \p -> P (\input -> _)
    
-- ParseResult is itself a Functor. call a Functor to do it for me. spacesuitburrito
    --  error "todo: Course.Parser (<$>)#instance Parser"
-- take this input and give it to that \p parser i will have a parseresult a
-- how do i take parseresult a and turn it into parseresult b
-- use a function
-- i have a function called \f that will do that
    -- \f -> \(P p) -> P (\input -> p) -- pattern match directly here.
    -- this is now of the type input parseresult a?

    -- but this resrve syntax one constructor footnote not quite true. 
    -- not q constructor. p q . can't. 

    -- \f -> \(P p) -> P (\input -> p input)
    -- \f -> \(P p) -> P (\input -> _ (p input)) -- found hole ghc says need a functino of ParseResult a -> ParseResult b
    
    -- if don't know. reasoning partial answer. suspect using f
    -- use typeof. "Help me, ghc!"

    \f -> \(P p) -> P (\input -> (<$>) f (p input))  -- help me ghc i suspect use f
    -- \f -> \(P p) -> P (\input -> (<$>) f . p ))  -- equivalent syntax 
    -- \f -> \(P p) -> P (\input -> ((<$>) f <$> p ))  -- equivalent
    -- \f -> \(P p) -> P (\input -> ((f <$>) <$> p ))  -- equivalent but dont do these
      -- equivalent rule 
      -- . is equivalent to \x -> f(g x)
      -- f.g
      -- . is a fmap
      -- 
      -- f <$> g
      -- underlying fmap
      -- 

-- | Return a parser that always succeeds with the given value and consumes no input.
--
{-FMAP HAS THIS TYPE a->b -> f a -> f b (parseresult thing)
-- fmap has this type

23 of 26] Compiling Course.Parser    ( src/Course/Parser.hs, interpreted )

src/Course/Parser.hs:193:34: error:
    • Found hole: _ :: (a -> b) -> ParseResult a -> ParseResult b
      Where: ‘a’, ‘b’ are rigid type variables bound by
               the type signature for:
                 (<$>) :: forall a b. (a -> b) -> Parser a -> Parser b
               at src/Course/Parser.hs:(166,5)-(168,15)
    • In the expression: _
      In the expression: _ f (p input)
      In the first argument of ‘P’, namely ‘(\ input -> _ f (p input))’
    • Relevant bindings include
        input :: Input (bound at src/Course/Parser.hs:193:25)
        p :: Input -> ParseResult a (bound at src/Course/Parser.hs:193:15)
        f :: a -> b (bound at src/Course/Parser.hs:193:6)
        (<$>) :: (a -> b) -> Parser a -> Parser b
          (bound at src/Course/Parser.hs:169:3)
      Valid hole fits include
        (<$>) :: forall (f :: * -> *) a b.
                 Functor f =>
                 (a -> b) -> f a -> f b
          with (<$>) @ParseResult @a @b
          (imported from ‘Course.Functor’ at src/Course/Parser.hs:11:1-21
           (and originally defined at src/Course/Functor.hs:(27,3)-(30,10)))
    |
193 |     \f -> \(P p) -> P (\input -> _ f (p input))  -- help me ghc i suspect use f

-}


-- >>> parse (valueParser 3) "abc"
-- Result >abc< 3
valueParser ::
  a
  -> Parser a
valueParser = 
  -- \a -> P -- pure for parser. neutral parser that does nothing -- go lambda a and how to call parser? P
  -- valueParser
  -- pure -- ? 
  -- \a -> P f -- asdijaoid
  -- P (`Result` f)
    -- \a -> _
    -- \a -> P -- need a parseresult a0 :i ParseResult look at contructor
    -- \a -> P (\input -> _) -- takes two things alright two type holes
    -- \a -> P (\input -> Result _ _)
    -- \a -> P (\input -> Result input _)
    
    -- correct answer worked out through here.
    -- \a -> P (\input -> Result input a) -- now need an a here. right here earlier
    -- useful sometimes need a parser that does nothing.

    -- use pointfree.io -- to cheat. - gg
    P . flip Result
    
 
    -- Why would I need a parser that does nothing.
    -- I trust you?
    -- chain of parsers
    -- without consuming input when already have?
    -- use ValueParser to copy put it into the changer
    -- patchcable to have the value without consuming extra input that would be parsing next.
    -- getting input to convert back into the chain of parsers.
    -- also see why the parser is useful when we write the monad

    

    -- e.g. day month pairs 
    -- 3 july
    -- certain range
  -- parser that does nothing but very useful
  -- it's already implemented down here somewhere pure = that.
  
   -- type of bind for us for parsers?
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    -- (>>=) character
    -- character :: Parser Char       -- a just turned into Char

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    -- (>>=) character _   -- thing that has to go here. \c return Parser
    -- (>>=) character (\c1 -> )    -- gg
    -- (>>=) character (\c1 -> character >>= \c2 -> _)    -- gg
    -- (>>=) character (\c1 -> character >>= \c2 -> (c1, c2))    -- gg
-- turn a pair of characters into a parser for a pair of chars
-- how about a parser that does nothing
-- no arrow in the type signature. just a parser.


    -- (>>=) character (\c1 -> character >>= \c2 -> valueParser (c1, c2)) 
    -- twoChars :: Parser (Char, Char)
    -- twoChars (>>=) character (\c1 -> character >>= \c2 -> valueParser (c1, c2)) 
    -- works but we need to implement bind


    -- character :: Parser Char       -- a just turned into Char





  -- error "todo: Course.Parser#valueParser"
-- haskell evaluation model trap repeated expression parser

-- | Return a parser that tries the first parser for a successful value.
--
--   * If the first parser succeeds then use this parser.
--
--   * If the first parser fails, try the second parser.
--
-- >>> parse (character ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (constantParser UnexpectedEof ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (constantParser UnexpectedEof ||| valueParser 'v') "abc"
-- Result >abc< 'v'
(|||) ::
  Parser a
  -> Parser a
  -> Parser a
-- (|||) =
  -- P _
  -- P _
  -- P _
(|||) (P p) (P q) = 
  -- P (\input -> p input) -- this will compile but not correct
  -- error "todo: Course.Parser#(|||)"
  -- -->> :t P
  -- P :: (Input -> ParseResult a) -> Parser a
  -- >>
-- -- choice parser


  -- did that succeed
  -- P (\input -> case p input of 
  --                 Result j a -> Result j a  -- did succeed
  --                 _ -> q input) -- else try on second parser.

  -- -- DONESKIS
  -- P (\input -> case p input of
  --     r@(Result _ _) -> r   -- if it did hit the result constructor.
  --     _ -> q input  -- if didn't hit Result constructor.
  -- )

-- see what isErrorResult
-- bool is if then else with args other way???
  -- P (\input -> bool _ _ (isErrorResult (p input))) -- ? not working. call p input again RIP. workaround
  -- P (\input -> bool (p input) (q input) (isErrorResult (p input))) -- did that hit error constructor otherwise put into other parser q input) oops it will reevaluate run quadratic
  -- P (\input -> bool (p input) (q input) (isErrorResult (p input))) -- workaround let assign expression
  P (\input -> 
    let r = p input
    in bool r (q input) (isErrorResult r) -- workaround let assign expression
  ) -- let r instead of re-evaluating the expression.
-- -- threading r down the call tree. maybe can use lift
-- -- harder each time ? 
-- -- space cost. haskell won't optimise this and shouldn't optimise.
--   P (\input -> 
--     let r = p input
--     in bool r (const (q input) r) (isErrorResult r) -- workaround let assign expression
--   ) -- const takes an a of b and returns an a

--   P (\input -> 
--     let r = p input
--     in lift3 bool (id) (const . q input) (isErrorResult) r 
--   )

-- point free
-- P (ap (flip (lift3 bool id . (const .) . q) isErrorResult) p)

infixl 3 |||

-- P (\input -> 
--   let r = p input
--   in bool (id r) (q input) (isErrorResult r) -- workaround let assign expression
-- )
-- threading r down the call tree
-- dependency injectino. depending on r e.g. id r
-- actually lift3. applicative.


-- | Parsers can bind.
-- Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- >>> parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "abc"
-- Result >bc< 'v'
--
-- >>> parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "a"
-- Result >< 'v'
--
-- >>> parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "xabc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "")
-- True
--
-- >>> isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "x")
-- True
instance Monad Parser where
  (=<<) ::
    (a -> Parser b)
    -> Parser a
    -> Parser b
  (=<<) =
    -- \f -> _
    -- \f -> \(P p) -> _ -- pattern match work at once -- Parser b. how to make a Parser P :P 
    -- \f -> \(P p) -> P _ -- could also use f -- Follow the Types! to the answer
   -- a function of the type Input - > ParseResult b
    -- \f -> \(P p) -> P (\input -> _) -- now need to get a ParseResult b
    -- f is of type
    -- f :: (a -> Parser b)
    -- p :: Input -> ParseResult a
    -- input :: Input
    -- ?goalpost :: ParseResult b
    -- \f -> \(P p) -> P (\input -> ) 
    -- \f -> \(P p) -> P (\input -> p input)  - not there yet
    -- \f -> \(P p) -> P (\input -> p input) -- five constructors. match every single one of them
    -- \f -> \(P p) -> P (\input -> case p input of 
    --                                 UnexpectedEof -> UnexpectedEof
    -- -- )
    -- \f -> \(P p) -> P (\input -> case p input of 
    --                           UnexpectedEof -> UnexpectedEof
    --                           UnexpectedChar c -> UnexpectedChar c
    --                           UnexpectedString s -> UnexpectedString s
    --                           -- i expected the end of file but has input. if it has
    --                             -- any of the fail. reconstruct
    --                           ExpectedEof i -> ExpectedEof i
    --                           -- last case - succeeded
    --                           -- have not used f
    --                           -- only use f on the result case.
    --                           -- that is what bind is going to do. if succeeded.
    --                             -- when pass a to f i have a Parser b 
    --                             -- Parser b how to get ParseResult b.
    --                             -- if get the function out and give it some input
    --                             -- will then have a ParseResult b
    --                             -- handy function at top called parse to get the function out
    --                           Result input2 a -> Result(parse (f a) input2)
    -- )
-- 
    \f -> \(P p) -> P (\input -> case p input of 
                        UnexpectedEof -> UnexpectedEof
                        UnexpectedChar c -> UnexpectedChar c
                        UnexpectedString s -> UnexpectedString s
                        ExpectedEof j -> ExpectedEof j
                        Result input2 a -> parse (f a) input2
    )
      --twicely composition greater = greater fish one. call on parsers.
    -- error "todo: Course.Parser (=<<)#instance Parser"


-- new exercise
character2 :: Parser (Char, Char)
-- character :: Parser Char
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b  -- flipped bind
-- valueParser :: a -> Parser a
-- character2 = ?
-- character2 = character >>= _ -- • Found hole: _ :: Char -> Parser (Char, Char)

-- Char -> Parser charpair -> parser PairOfChar
-- Char -> Parser (Char, Char)

-- character2 = character >>= \c1 -> -- need to make a Parser pair of char here
-- character2 = character >>= \c1 -> 
--               character >>= _
-- character2 = character >>= \c1 -> 
--               character >>= \c2 ->
--                 (c1, c2) -- made a pair of char!. but this won't type check
--                 -- not a Parser (c1, c2)
-- character2 = character >>= \c1 -> 
--               character >>= \c2 ->
--                 _ (c1, c2)

character2 =  character >>= \c1 -> 
              character >>= \c2 ->
              valueParser (c1, c2) -- can replace with pure

-- lol pointfree character2 = (character >>=) . (valueParser .) . (,) =<< character
-- done! Type checks.
-- type in ghci Prelude console. parse character2 "abc"
-- need something to take (Char, Char) -> Parser (Char, Char) 
-- patterns!


-- | Write an Applicative functor instance for a @Parser@.
-- /Tip:/ Use @(=<<)@.
instance Applicative Parser where
  pure ::
    a
    -> Parser a
  pure =
    valueParser -- pure = valueParser!
  (<*>) ::
    Parser (a -> b)
    -> Parser a
    -> Parser b
  (<*>) =
    \f a ->
      f >>= \ff ->
        a >>= \aa ->
          pure (ff aa)
    -- lol data entry.
    -- function onResult on type to do the bind pattern checking.
    -- bind. frmo this moment on I am using this one
    -- This is the relationship between applicative and Monad
    -- can do the same thing in Functor btw
    -- ex Do Functor then Applicative Monad data entry
    -- same is true. can write this for Functor. macro write generically different ways
    -- Language extension called Template Haskell
    -- program against Haskell itself to have Haskell lang as a DataType itself
    -- error "todo: Course.Parser (<*>)#instance Parser"

-- | Return a parser that produces a character but fails if
--
--   * The input is empty.
--
--   * The character does not satisfy the given predicate.
--
-- /Tip:/ The @(=<<)@, @unexpectedCharParser@ and @character@ functions will be helpful here.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy ::
  (Char -> Bool)
  -> Parser Char
satisfy pr =
  -- character >>= \c -> _
  -- character >>= \c -> valueParser c
  -- character >>= \c -> if pr c then _ else _ - succeed with c predicate otherwise i want to fail
  -- character >>= \c -> if pr c then _ else _
  -- lol always succeeds. valueParser or pure gg
  -- character >>= \c -> if pr c then pure c else _
  -- character >>= \c -> if pr c then pure c else _
  -- character >>= 
  --   \c -> if pr c then pure c else unexpectedCharParser c

  -- character >>= 
  --   \c -> 
  --     -- if pr c then pure c else unexpectedCharParser c
  --     _ bool unexpectedCharParser pure pr c
  
  -- character >>= \c -> 
  --     _ bool unexpectedCharParser pure pr c
  
  -- character >>= \c -> 
      -- bool (unexpectedCharParser c) (pure c) (pr c)
  
      --fn spaceship muscle memory
      -- \f \a bind a arrow pure 
      -- push code it's on another branch now

  character >>= 
    lift3 bool unexpectedCharParser pure pr
  -- check branch https://github.com/data61/fp-course.git 20190710 answers
      -- lift3 type (a - b - c - d) lift3 passes c down three times.

    -- only works if
  -- try
  -- parse (satisfy (\c -> c == 'x')) "abc"
  -- parse (satisfy) (\c -> c == 'x')) "xabc"
  -- like bool because functinon

  -- executive uqcs ? 
  -- preference if then else.
  -- case of the bool of true false
  -- bool function

  -- bind. lamblambdaequals sign >>=
  -- satisfy.
  -- parse one character
  -- and make sure it satisfies that predicate.
  -- error "todo: Course.Parser#satisfy"

-- | Return a parser that produces the given character but fails if
--
--   * The input is empty.
--
--   * The produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
is ::
  Char -> Parser Char
is =
  -- \c -> satisfy (\d -> c == d)
  -- \c -> satisfy (\d -> c == d)
  satisfy . (==) -- by rules of functino composition and inner reductions
  --
  -- error "todo: Course.Parser#is"


  -- gg functional parsing better than for loop regex parser blow up change requirement

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * The input is empty.
--
--   * The produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isDigit@ functions.
digit ::
  Parser Char
digit =
  satisfy isDigit
  -- error "todo: Course.Parser#digit"
-- use satisfy and isDigit to write this parser
--
-- | Return a parser that produces a space character but fails if
--
--   * The input is empty.
--
--   * The produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isSpace@ functions.
space ::
  Parser Char
space =
  satisfy isSpace

-- not functional composition. it just takes in the char->bool straight in
-- >> :t satisfy
-- satisfy :: (Char -> Bool) -> Parser Char
-- >> :t isSpace
-- isSpace :: Char -> Bool
  -- cool
  -- satisfy takes in a Char -> Bool
  -- lol Char -> Bools
  -- cons false
-- | Return a parser that continues producing a list of values from the given parser.
--
-- /Tip:/ Use @list1@, @pure@ and @(|||)@.
--
-- >>> parse (list character) ""
-- Result >< ""
--
-- >>> parse (list digit) "123abc"
-- Result >abc< "123"
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
--
-- >>> parse (list character) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character *> valueParser 'v')) ""
-- Result >< ""
list ::
  Parser a
  -> Parser (List a)
list p =
  list1 p ||| valueParser Nil
  -- error "todo: Course.Parser#list"

-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
--
-- /Tip:/ Use @(=<<)@, @list@ and @pure@.
--
-- >>> parse (list1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list1 (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (list1 (character *> valueParser 'v')) "")
-- True
list1 ::
  Parser a
  -> Parser (List a)
-- list1 p =
--   p >>= \x ->
--     list p

list1 p =
  p >>= (\x ->
    list p >>= (\y ->
      pure (x:.y)))
  -- call "And then" 
  -- bind! - some languages call it AND THEN?
  -- error "todo: Course.Parser#list1"
  -- mutually recursive!! crazy only works in lazy lang haskell. py blow up
  -- ghci prelude parse (list character) "abc"
  -- output Result >< "abc"

-- | Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--
--   * The input is empty.
--
--   * The first produced character is not a space.
--
-- /Tip:/ Use the @list1@ and @space@ functions.
spaces1 ::
  Parser Chars
spaces1 =
  error "todo: Course.Parser#spaces1"

-- | Return a parser that produces a lower-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isLower@ functions.
lower ::
  Parser Char
lower =
  error "todo: Course.Parser#lower"

-- | Return a parser that produces an upper-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isUpper@ functions.
upper ::
  Parser Char
upper =
  error "todo: Course.Parser#upper"

-- | Return a parser that produces an alpha character but fails if
--
--   * The input is empty.
--
--   * The produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isAlpha@ functions.
alpha ::
  Parser Char
alpha =
  error "todo: Course.Parser#alpha"

-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
--
-- /Tip:/ Use @(=<<)@ and @pure@.
-- /Tip:/ Optionally use @List#foldRight@. If not, an explicit recursive call.
--
-- >>> parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
-- True
sequenceParser ::
  List (Parser a)
  -> Parser (List a)
sequenceParser =
  error "todo: Course.Parser#sequenceParser"

-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @List.replicate@.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany ::
  Int
  -> Parser a
  -> Parser (List a)
thisMany =
  error "todo: Course.Parser#thisMany"

-- | This one is done for you.
--
-- /Age: positive integer/
--
-- >>> parse ageParser "120"
-- Result >< 120
--
-- >>> isErrorResult (parse ageParser "abc")
-- True
--
-- >>> isErrorResult (parse ageParser "-120")
-- True
ageParser ::
  Parser Int
ageParser =
  (\k -> case read k of Empty  -> constantParser (UnexpectedString k)
                        Full h -> pure h) =<< (list1 digit)

-- | Write a parser for Person.firstName.
-- /First Name: non-empty string that starts with a capital letter and is followed by zero or more lower-case letters/
--
-- /Tip:/ Use @(=<<)@, @pure@, @upper@, @list@ and @lower@.
--
-- >>> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- >>> isErrorResult (parse firstNameParser "abc")
-- True
firstNameParser ::
  Parser Chars
firstNameParser =
  error "todo: Course.Parser#firstNameParser"

-- | Write a parser for Person.surname.
--
-- /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
--
-- /Tip:/ Use @(=<<)@, @pure@, @upper@, @thisMany@, @lower@ and @list@.
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> parse surnameParser "Abcdefghijklmnopqrstuvwxyz"
-- Result >< "Abcdefghijklmnopqrstuvwxyz"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True
surnameParser ::
  Parser Chars
surnameParser =
  error "todo: Course.Parser#surnameParser"

-- | Write a parser for Person.smoker.
--
-- /Smoker: character that must be @'y'@ or @'n'@/
--
-- /Tip:/ Use @is@ and @(|||)@./
--
-- >>> parse smokerParser "yabc"
-- Result >abc< True
--
-- >>> parse smokerParser "nabc"
-- Result >abc< False
--
-- >>> isErrorResult (parse smokerParser "abc")
-- True
smokerParser ::
  Parser Bool
smokerParser =
  error "todo: Course.Parser#smokerParser"

-- | Write part of a parser for Person#phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
--
-- /Phone: string of digits, dots or hyphens .../
--
-- /Tip:/ Use @list@, @digit@, @(|||)@ and @is@.
--
-- >>> parse phoneBodyParser "123-456"
-- Result >< "123-456"
--
-- >>> parse phoneBodyParser "123-4a56"
-- Result >a56< "123-4"
--
-- >>> parse phoneBodyParser "a123-456"
-- Result >a123-456< ""
phoneBodyParser ::
  Parser Chars
phoneBodyParser =
  error "todo: Course.Parser#phoneBodyParser"

-- | Write a parser for Person.phone.
--
-- /Phone: ... but must start with a digit and end with a hash (#)./
--
-- /Tip:/ Use @(=<<)@, @pure@, @digit@, @phoneBodyParser@ and @is@.
--
-- >>> parse phoneParser "123-456#"
-- Result >< "123-456"
--
-- >>> parse phoneParser "123-456#abc"
-- Result >abc< "123-456"
--
-- >>> isErrorResult (parse phoneParser "123-456")
-- True
--
-- >>> isErrorResult (parse phoneParser "a123-456")
-- True
phoneParser ::
  Parser Chars
phoneParser =
  error "todo: Course.Parser#phoneParser"

-- | Write a parser for Person.
--
-- /Tip:/ Use @(>>=)@,
--            @pure@,
--            @(*>)@,
--            @spaces1@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @smokerParser@,
--            @phoneParser@.
--
-- /Tip:/ Follow-on exercise: Use *(<*>)* instead of @(>>=)@.
--
-- /Tip:/ Follow-on exercise: Use *(<*>~)* instead of @(<*>)@ and @(*>)@.
--
-- >>> isErrorResult (parse personParser "")
-- True
--
-- >>> isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
-- True
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789#"
-- Result >< Person 123 "Fred" "Clarkson" True "123-456.789"

--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789# rest"
-- Result > rest< Person 123 "Fred" "Clarkson" True "123-456.789"

--
-- >>> parse personParser "123  Fred   Clarkson    y     123-456.789#"
-- Result >< Person 123 "Fred" "Clarkson" True "123-456.789"
personParser ::
  Parser Person
personParser =
  error "todo: Course.Parser#personParser"

-- Make sure all the tests pass!

----

-- Did you repeat yourself in `personParser` ? This might help:

(>>=~) ::
  Parser a
  -> (a -> Parser b)
  -> Parser b
(>>=~) p f =
  (p <* spaces1) >>= f

infixl 1 >>=~

-- or maybe this

(<*>~) ::
  Parser (a -> b)
  -> Parser a
  -> Parser b
(<*>~) f a =
  f <*> spaces1 *> a

infixl 4 <*>~
