-- balance paranthesis -> E.g. "()(()())" whereas "()(()" is not
	-- () is in BP
	-- if s and t are in BP, so is the concatenation st of s and t
	-- if s is in BP then so is (s)
-- A = set of strings representing simple arithmetic expressions
	-- x, y and z are in A
	-- if e is in A then so is (e)
	-- if e and e' are in A, then so are e1+e2 and e1*e2

-- The templates for BP:
--   - ()
--   - {}{}
--   - ({})
--
-- For A:
--   - three templates: x y z
--   - ({})
--   - {}+{} {}*{}
  
-- Yes, it says above to stick to the Prelude, but this one function
-- (intercalate) is a favorite of the instructor.
import Data.List (intercalate)

data Slot = Slot
          deriving (Show, Eq)

data Template =
  Template [Either String Slot]
  deriving Eq

-- explicitly define show for templates so that they print like in the
-- discussion above
instance Show Template where
  show (Template xs) =   concat $ map f xs
    where f (Left x) = x
          f (Right x) = "{}"

-- An inductive definition is just a list of templates.
data Def =
  Def [Template]
  deriving Eq

-- A definition is displayed as a sequence of rules separated by "//".
instance Show Def where
  show (Def rs) = "Templates: " ++ (intercalate " // " $ map show rs)

-- QUESTION 1.
-- The number of slots in a template.
-- Example: templateArity ({})foo{}{} = 3
templateArity :: Template -> Int
templateArity (Template xs) = foldr (\x acc -> if x == (Right Slot) then acc+1 else acc) 0 xs

--
-- Balenced paren example
--

-- templates from the discussion above coded in Haskell
bpBase = Template [Left "()"]
bpStep1 = Template [Left "(", Right Slot, Left ")"]
bpStep2 = Template [Right Slot, Right Slot]
bp = Def [bpBase, bpStep1, bpStep2]

-- show bp =
-- "Templates: () // ({}) // {}{}"

--
-- Arithmetic expressions example
--

arithX = Template [Left "x"]
arithY = Template [Left "y"]
arithZ = Template [Left "z"]
arithPlus = Template [Right Slot, Left "+", Right Slot]
arithTimes = Template [Right Slot, Left "*", Right Slot]
arithParens = Template [Left "(", Right Slot, Left ")"]
arith = Def [arithX, arithY, arithZ, arithPlus, arithTimes, arithParens]

-- show arith =
-- "Templates: x // y // z // {}+{} // {}*{} // ({})"



subst :: [String] -> [Either String Slot] -> [Either String Slot]
subst [] xs = xs
subst (str : strs) (Right _ : xs) = Left str : subst strs xs
subst (str : strs) (x : xs) = x : subst (str : strs) xs

-- Question 2
-- Replace the slots with the given strings.
-- Example: templateInstance {}foo{}{} ["1", "2", "3"] = 1foo23
-- Note: you can use the show function to turn objects into strings.
templateInstance :: Template -> [String] -> String
templateInstance (Template xs) strs = if (templateArity $ Template xs) > 0 then show (Template (subst strs xs)) else show (Template xs)

-- A derivation is a tree of templates.
-- A derivation (Derivation t ds) is *valid* if the derivations in ds
-- are valid and if the arity of t is the same as the length of ds.
-- The *result* of a derivation (Derivation t ds) is the instance of
-- the template t obtained by replacing the slots in t with the
-- results of the derivations in ds.
data Derivation = Derivation Template [Derivation]
                deriving Show

-- Question 3
-- Analogous to the foldr function defined in class for binary trees.
-- Example: derivationFoldR (\t vs -> maximum $ (templateArity t : vs))
-- is a function that computes the maximum template arity in a
-- derivation.
derivationFoldR :: (Template -> [a] -> a) -> Derivation -> a
derivationFoldR f (Derivation r []) = f r []
derivationFoldR f (Derivation r l) = f r [derivationFoldR f $ head l]

-- Question 4
-- derivationResult d returns Nothing if d is not valid; otherwise it
-- returns Just s where s is the *result* of d (see the comment before
-- the definition of Derivation
rebuild (Derivation t []) = show t
rebuild (Derivation t ms) = if (templateArity t)==(length ms) then templateInstance t (map rebuild ms) else "invalid"  

isvalid (Derivation t []) = "valid"
isvalid (Derivation t xs) = if (templateArity t)==(length xs) then (if "invalid" `elem` (map rebuild xs) then "invalid" else "valid") else "invalid" 

derivationResult :: Derivation -> Maybe String
derivationResult (Derivation d ds) = if (isvalid (Derivation d ds)=="valid") then Just (rebuild (Derivation d ds)) else Nothing
