{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import qualified Data.Aeson as A
import Data.Char
import Data.List
import qualified Data.HashTable.ST.Basic as H
import qualified Data.HashTable.Class as H'
import qualified Data.Text as T

newtype Terminal = Terminal T.Text deriving (Show, Eq, Ord)

mkTerminal :: T.Text -> Terminal
mkTerminal s
  | T.null s = error "Empty string"
  | isUpper (T.head s) = error $ (T.unpack s) ++ "starts with lowercase, hence it's a nonterminal"
  | otherwise = Terminal s

newtype NonTerminal = NonTerminal T.Text deriving (Show, Eq, Ord)

mkNonTerminal :: T.Text -> NonTerminal
mkNonTerminal s
  | T.null s = error "Empty string"
  | isLower (T.head s) = error $ (T.unpack s) ++ "starts with lowercase, hence it's a terminal"
  | otherwise = NonTerminal s

data Symbol = SymNT NonTerminal | SymT Terminal deriving (Show, Eq)

mkSymbol :: T.Text -> Symbol
mkSymbol s
  | T.null s = error "Empty string"
  | isLower (T.head s) = SymT $ Terminal s
  | isUpper (T.head s) = SymNT $ NonTerminal s

data Rule = Rule {ruleNT :: NonTerminal, ruleSyms :: [Symbol]}
  deriving (Show, Eq)

type ChomskyRule = Rule -- Rules that only have two symbols after the arrow

newtype CFG r = CFG { rules :: [r] } deriving (Show, Eq)

parseGrammar :: T.Text -> CFG Rule
parseGrammar s = CFG $ map toRule ls
  where ls = map T.words $ T.lines s
        toRule [] = error "Empty line"
        toRule (first : rest) = Rule (mkNonTerminal first) (map mkSymbol rest)

sentenceTerminals :: T.Text -> [Terminal]
sentenceTerminals = map mkTerminal . T.words

-- | Getting a grammar that plays nicely with CKY
chomsky :: CFG Rule -> CFG ChomskyRule
chomsky (CFG rules) = CFG $ units $ units $ concatMap break rules
  where
    break :: Rule -> [Rule]
    break r@(Rule nt@(NonTerminal s) (x:xs)) | length xs > 1 =
      let nt' = NonTerminal (T.append s (T.pack "'")) in
      Rule nt [x, SymNT nt'] : break (Rule nt' xs)
    break r@(Rule _ _) = [r]

    units :: [Rule] -> [Rule]
    units (r@(Rule nt [SymNT nt']) : rules) =
      (map (\(Rule _ xs) -> Rule nt xs) $ filter ((== nt') . ruleNT) rules) ++ units rules
    units (r : rules) = r : units rules
    units [] = []

unitProductions :: CFG ChomskyRule -> Terminal -> [ChomskyRule]
unitProductions gr t = filter match (rules gr)
  where match (Rule nt [SymT t']) = t == t'
        match _ = False

data Tree a = Leaf a | Node a [Tree a] deriving (Show, Eq)
type Parse = [(Tree Symbol, ChomskyRule)]
type Coord = (Int, Int)
type SymbolTable s = H.HashTable s Coord Parse

-- | All the diagonal coordinates starting from (n,0) to upper right
diagonal :: Int -> [Coord]
diagonal n = reverse [(i, n - i) | i <- [0..n]]

-- | All the pairs of coordinates that cover the whole
addUp :: Int -> Coord -> [(Coord, Coord)]
addUp n (i, j) = [((y, j), (i,x)) | x <- [j+1..n-1], y <- [i+1..n-1], x + y == n]

type Partial = [Tree Symbol]
type Full = [Tree Symbol]

-- | Returns a pair of full parses and all partial parses sorted by length
cky :: CFG ChomskyRule -> [Terminal] -> Either Partial Full
cky gr sentence = runST $ do
    h <- initial
    through h
    full <- nub
          . map fst
          . filter (isStart . snd)
          . maybe [] id <$> H.lookup h (0,0)

    case full of
      [] -> Left <$> partial h (0,0)
      _ -> return $ Right full
  where
    len = length sentence
    isStart (Rule nt _) = nt == ruleNT (head (rules gr))

    initial :: ST s (SymbolTable s)
    initial = do
      h <- H.new
      forM_ (zip sentence (diagonal (len - 1))) $ \(terminal, (i,j)) -> do
        H.insert h (i, j) $
          map (\r@(Rule nt _) -> (Node (SymNT nt) [Leaf (SymT terminal)], r))
              (unitProductions gr terminal)
      return h

    through :: SymbolTable s -> ST s ()
    through h = do
      forM_ (reverse [0..len - 2]) $ \n ->
        forM_ (diagonal n) $ \(i,j) -> do
          forM_ (addUp len (i,j)) $ \(p1,p2) -> do
            cell1 <- H.lookup h p1
            cell2 <- H.lookup h p2
            case (cell1, cell2) of
              (Just cell1', Just cell2') -> do
                let pos = [ [(tree1, SymNT nt1), (tree2, SymNT nt2)]
                          | (tree1, Rule nt1 _) <- cell1'
                          , (tree2, Rule nt2 _) <- cell2']
                let apps = concatMap (\r@(Rule nt xs) ->
                               let ruleMatches = filter (\ys -> xs == map snd ys) pos
                               in map (\[(t1, sym1),(t2,sym2)] ->
                                        (Node (SymNT nt) [t1, t2], r)) ruleMatches)
                                    (rules gr)
                H.mutate h (i,j) (\m -> (maybe (Just apps) (Just . (apps ++)) m, ()))
              _ -> return ()

    partial :: SymbolTable s -> Coord -> ST s Partial
    partial h c@(i,j) = do
      let add = (+ i) *** (+ j)
      let pairs = map add ((if c == (0,0) then tail else id) $ concat [diagonal n | n <- [0 .. (len - (i+j) - 1)]])
      res <- foldM (\acc p ->
                                  case acc of { Nothing -> do x <- H.lookup h p
                                                              case x of
                                                                Just [] -> return Nothing
                                                                _ -> return ((p,) <$> x)
                                              ; m -> return m})
                                 Nothing pairs
      let Just (p, top:_) = res
      let matches = map (add *** add) (addUp (uncurry (+) p - 1) (0,0))
      case matches of
        [] -> return [fst top]
        _ -> do
          let match = foldl' (\acc (p1, p2) ->
                                     case acc of
                                      { Nothing -> if p1 == (i,j) then Just p2
                                                   else if p2 == (i,j) then Just p1
                                                   else Nothing
                                      ; m -> m})
                                  Nothing matches
          case match of
            Nothing -> return [fst top]
            Just match -> do
              rest <- partial h match
              let (i',j') = match
              return (fst top : rest)

-- | Printing trees as S-expressions
prettyTree :: Tree Symbol -> T.Text
prettyTree t = case t of
    Leaf sym -> prettySym sym
    Node sym xs -> 
      T.concat [ T.pack "("
               , prettySym sym
               , T.pack " "
               , T.concat (intersperse (T.pack " ") (map prettyTree xs))
               , T.pack ")"
               ]
  where
    prettyNT (NonTerminal s) = s
    prettyT (Terminal s) = s
    prettySym (SymNT nt) = prettyNT nt
    prettySym (SymT t) = prettyT t

instance A.ToJSON (Tree Symbol) where
  toJSON t = A.String $ prettyTree t
