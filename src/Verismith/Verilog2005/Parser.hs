-- Module      : Verismith.Verilog2005.Parser
-- Description : Partial Verilog 2005 parser to reconstruct the AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE OverloadedLists #-}

module Verismith.Verilog2005.Parser
  ( parseVerilog2005,
  )
where

import Control.Applicative (liftA2, (<**>))
import Control.Lens hiding ((<|))
import Data.Functor.Compose
import Control.Monad (join)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as LB
import Data.Data (Data, constrIndex, toConstr)
import Data.Bifunctor
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.IntMap.Strict as IntMap
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as V
import Text.Parsec hiding (satisfy, uncons)
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Printf (printf)
import Verismith.Utils
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.Lexer
import Verismith.Verilog2005.PrettyPrinter
import Verismith.Verilog2005.Token
import Verismith.Verilog2005.Utils

-- | The parser monad with LocalCompDir (local values of compiler directives) as local state
-- | and a writer monad for the list of warnings as the base monad
type Parser = ParsecT [PosToken] LocalCompDir (Writer [String])

-- | A production rule associated to a token without data
type Produce a = (Token, a)

type LProduce a = [Produce a]

-- | A branching in the parser based on a token without data
type Branch a = Produce (Parser a)

type LBranch a = [Branch a]

-- | Same as above but parametrised by attributes
type AProduce a = Produce (Attributes -> a)

type LAProduce a = [AProduce a]

type ABranch a = AProduce (Parser a)

type LABranch a = [ABranch a]

type APBranch a = Produce (Attributes -> SourcePos -> Parser a)

type LAPBranch a = [APBranch a]

-- | An error that is not merged with other errors and expected tokens
hardfail :: String -> Parser a
hardfail m =
  mkPT $ pure . Consumed . pure . Error . newErrorMessage (Message m) . statePos

-- | Warning formatting
warn :: SourcePos -> String -> Parser ()
warn pos s = lift $ tell [printf "Line %d, column %d: %s" (sourceLine pos) (sourceColumn pos) s]

-- | Gets a number from a Token, erases the data associated with it
getConsIndex :: Data a => a -> Int
getConsIndex = constrIndex . toConstr

-- | Efficient token branching utility
mkActionMap :: LProduce a -> IntMap.IntMap a
mkActionMap =
  IntMap.fromListWithKey (\k _ _ -> error $ "Conflict on " ++ show k)
    . map (first getConsIndex)

-- | Updates the position information
nextPos :: SourcePos -> PosToken -> [PosToken] -> SourcePos
nextPos pos _ ptl = case ptl of
  PosToken (Position l c (PSDefine _) :| _) _ : _ ->
    setSourceColumn (setSourceLine pos $ fromEnum l) $ fromEnum c
  PosToken (Position l c (PSFile f) :| _) _ : _ -> newPos f (fromEnum l) (fromEnum c)
  PosToken (Position l c (PSLine f _) :| _) _ : _ -> newPos f (fromEnum l) (fromEnum c)
  [] -> pos

-- | Pass an argument before joining
joinWith :: Monad m => m (a -> m b) -> a -> m b
joinWith mf x = mf >>= \f -> f x

-- | Parse exactly one token and produce a value
producePrim :: (Token -> Maybe a) -> Parser a
producePrim f = tokenPrim show nextPos (f . _ptToken)

-- | Parse exactly one token and branches
branchPrim :: (Token -> Maybe (Parser a)) -> Parser a
branchPrim = join . producePrim

-- | Parse these annoying compiler directives that can appear anywhere
anywherecompdir :: Parser ()
anywherecompdir = skipMany $
  branchPrim $ \t -> case t of
    CDCelldefine -> Just $ modifyState $ lcdCell .~ True
    CDEndcelldefine -> Just $ modifyState $ lcdCell .~ False
    _ -> Nothing

-- | Basic one token parsing able to produce a value using a function, uninformative error on failure
fproduce :: (Token -> Maybe a) -> Parser a
fproduce f = producePrim f <* anywherecompdir

-- | Branches on a Token without data, informative error on failure
lproduce :: LProduce a -> Parser a
lproduce l =
  fproduce (\t -> IntMap.lookup (getConsIndex t) $ mkActionMap l)
    `labels` map (show . fst) l

-- | Maps a function on the data given by branching on a Token without data
maplproduce :: (a -> b) -> LProduce a -> LProduce b
maplproduce = map . second

-- | Same as above but for branching with attributes
maplaproduce :: (a -> b) -> LAProduce a -> LAProduce b
maplaproduce f = map $ second (f .)

-- | Try to consume the provided token, informative error on failure
consume :: Token -> Parser ()
consume et = fproduce (\at -> if at == et then Just () else Nothing) <?> show et

-- | Try to consume the provided token and returns true on success (cannot fail)
optConsume :: Token -> Parser Bool
optConsume et = option False $ fproduce $ \at -> if at == et then Just True else Nothing

-- | Branch on the next token using a function
fbranch :: (Token -> Maybe (Parser a)) -> Parser a
fbranch = join . fproduce

-- | Branch on the next token but remembers position of the branching
fpbranch :: (SourcePos -> Token -> Maybe (Parser a)) -> Parser a
fpbranch f = join $ (getPosition >>= producePrim . f) <* anywherecompdir

-- | Parse attributes then branches on the next token using a LABranch
lbranch :: LBranch a -> Parser a
lbranch = join . lproduce

-- | Parse attributes then branches on the next token using a LABranch
labranch :: LABranch a -> Parser a
labranch l = join $ attributes <**> lproduce l

-- | Maps a function on the data given by a branching with attributes
maplbranch :: (a -> b) -> LBranch a -> LBranch b
maplbranch = maplproduce . fmap

-- | Maps a function on the data given by a single branch with attributes
mapabranch :: (a -> b) -> ABranch a -> ABranch b
mapabranch f = second $ \p a -> f <$> p a

-- | Maps a function on the data given by a branching with attributes
maplabranch :: (a -> b) -> LABranch a -> LABranch b
maplabranch = map . mapabranch

-- | Specialised repeating combinator
monoAccum :: Monoid a => Parser a -> Parser a
monoAccum p = optionMaybe p >>= maybe (pure mempty) (\x -> (x <>) <$> monoAccum p)

-- | What many1 should have been
manyNE :: Parser a -> Parser (NonEmpty a)
manyNE = fmap NE.fromList . many1

-- | Consume a closing delimiter
closeConsume :: SourcePos -> Token -> Token -> Parser ()
closeConsume p o c =
  fproduce (\t -> if t == c then Just () else Nothing)
    <?> printf
      "closing %s to match opening %s at %d:%d"
      (show c)
      (show o)
      (sourceLine p)
      (sourceColumn p)

-- | Enclose a parser with tokens and keep track of the opening token position
enclosed :: Token -> Token -> Parser a -> Parser a
enclosed l r x = do
  p <- getPosition
  consume l
  res <- x
  closeConsume p l r
  return res

-- | Enclosed between parentheses/brackets/braces
parens :: Parser a -> Parser a
parens = enclosed SymParenL SymParenR

brackets :: Parser a -> Parser a
brackets = enclosed SymBrackL SymBrackR

braces :: Parser a -> Parser a
braces = enclosed SymBraceL SymBraceR

-- | Comma separated list
csl :: Parser a -> Parser [a]
csl p = sepBy p $ consume SymComma

-- | Comma separated list with at least 1 element
csl1 :: Parser a -> Parser (NonEmpty a)
csl1 p = NE.fromList <$> sepBy1 p (consume SymComma)

-- | Warning on 0 element to follow SystemVerilog2017 syntax when 1 is required in Verilog2005
wempty :: String -> Parser [a] -> Parser [a]
wempty s p = do
  pos <- getPosition
  l <- p
  if null l then warn pos $ printf "Zero %s is a SystemVerilog feature" s else return ()
  return l

-- | Comma separated list potentially ended by a comma to be lenient
xcsl :: String -> Parser a -> Parser [a]
xcsl s p = (optionMaybe p >>=) $ maybe (pure []) $ \h -> do
  pos <- getPosition
  b <- optConsume SymComma
  if b
    then do
      t <- xcsl s p
      if null t
        then warn pos (printf "Extraneous comma at the end of %s is not correct Verilog" s)
        else pure ()
      return $ h : t
    else return [h]

-- | Comma separated list potentially ended by a comma to be lenient
xcsl1 :: String -> Parser a -> Parser (NonEmpty a)
xcsl1 s p = do
  x <- p
  pos <- getPosition
  b <- optConsume SymComma
  if b
    then ((x <|) <$> xcsl1 s p)
        <|> warn pos (printf "Extraneous comma at the end of %s is not correct Verilog" s)
          *> return [x]
    else return [x]

-- | Just read the definition
wxcsl :: String -> Parser a -> Parser [a]
wxcsl m = wempty m . xcsl m

-- | Safe parsing comma separated list with at least 1 elements
scsl1 :: Bool -> Parser a -> (a -> Parser b) -> Parser (NonEmpty b)
scsl1 safety d p =
  liftA2 (:|) (d >>= p) $ many $ ((if safety then try else id) $ consume SymComma *> d) >>= p

-- | Safe parsing of several elements of type B then of type C
-- | when B and C start with a common part of type A
smanythen :: Parser a -> (a -> Parser b) -> (a -> Parser c) -> Parser ([b], [c])
smanythen pa pb pc =
  (optionMaybe (pa >>= \a -> Left <$> pb a <|> Right <$> pc a) >>=) $
    maybe (pure ([], [])) $
      either
        (\hb -> first (hb :) <$> smanythen pa pb pc)
        (\hc -> (,) [] . (hc :) <$> many (pa >>= pc))

-- | Parenthesised comma separated list
pcsl :: Parser a -> Parser [a]
pcsl = parens . csl

pcsl1 :: Parser a -> Parser (NonEmpty a)
pcsl1 = parens . csl1

bcsl1 :: Parser a -> Parser (NonEmpty a)
bcsl1 = braces . csl1

parseBS :: Parser B.ByteString
parseBS =
  fproduce (\t -> case t of IdSimple s -> Just s; IdEscaped s -> Just s; _ -> Nothing)
    <?> "identifier"

-- | Extracts an identifier
ident :: Parser Identifier
ident = Identifier <$> parseBS

lenientIdent :: Parser Identifier
lenientIdent = do
  pos <- getPosition
  fbranch
    ( \t -> case t of
        IdSimple s -> Just $ return $ Identifier s
        IdEscaped s -> Just $ return $ Identifier s
        IdSystem s -> Just $ do
          warn
            pos
            "Dollar prefixed identifier outside system function or task is not correct Verilog"
          return $ Identifier s
        _ -> Nothing
    )
    <?> "identifier"

-- | Library prefixed cell for config blocks
dot1Ident :: Parser Dot1Ident
dot1Ident = do
  f <- parseBS
  s <- optionMaybe $ consume SymDot *> ident
  return $ case s of Nothing -> Dot1Ident Nothing $ Identifier f; Just s -> Dot1Ident (Just f) s

attribute :: Parser Attribute
attribute = do
  attr <- parseBS
  value <- optionMaybe $ consume SymEq
    *> genExpr (pure . Identifier) (optionMaybe constRangeExpr) (pure ()) Just constifyMaybeRange
  return $ Attribute attr value

-- TODO: this is likely incorrectly used but I leave this bug on purpose atm
attributeOne :: Parser [Attribute]
attributeOne = enclosed SymParenAster SymAsterParen $ NE.toList <$> csl1 attribute

-- | Flattened list of attributes
attributes :: Parser Attributes
attributes = many attributeOne

-- | Number after base
number :: Base -> Parser Number
number b = case b of
  BBin -> NBinary <$> fproduce (\t -> case t of LitBinary b -> Just $ NE.fromList b; _ -> Nothing)
  BOct -> NOctal <$> fproduce (\t -> case t of LitOctal o -> Just $ NE.fromList o; _ -> Nothing)
  BDec -> fproduce $ \t -> case t of
    LitXZ b -> Just $ NXZ b
    LitDecimal i -> Just $ NDecimal i
    _ -> Nothing
  BHex -> NHex <$> fproduce (\t -> case t of LitHex h -> Just $ NE.fromList h; _ -> Nothing)

type PGenExpr g i r a =
  (B.ByteString -> Parser i) ->
  Parser r ->
  Parser a ->
  (i -> Maybe Identifier) ->
  (Maybe DimRange -> Maybe r) ->
  Parser (g i r a)

-- | Parametric primary expression
genPrim :: PGenExpr GenPrim i r a
genPrim pi pr pa ci cr = fpbranch $ \p t -> case t of
  -- try parse braceL and let that decide the path, otherwise it is wrong for constExpr
  SymBraceL -> Just $ do
    e <- genExpr pi dimRange pa ci Just
    p2 <- getPosition
    b <- optConsume SymBraceL
    ee <- if b
      then case trConstifyGenExpr ci constifyMaybeRange e of
        Nothing -> hardfail "Replication takes a constant expression as multiplicity"
        Just e -> PrimMultConcat e <$> csl1 parseExpr <* closeConsume p2 SymBraceL SymBraceR
      else case trConstifyGenExpr Just cr e of
        Nothing -> hardfail "Invalid kind of expression"
        Just e -> PrimConcat . (e :|) <$> option [] (consume SymComma *> csl parseExpr)
    closeConsume p SymBraceL SymBraceR
    return ee
  SymParenL -> Just $ PrimMinTypMax <$> mtm parseExpr <* closeConsume p SymParenL SymParenR
  LitDecimal i -> Just $
    option (PrimNumber Nothing True $ NDecimal i) $
      fbranch $ \t -> case t of
        NumberBase s b -> Just $ PrimNumber (Just i) s <$> number b
        _ -> Nothing
  LitReal s -> Just $ return $ PrimReal s
  NumberBase s b -> Just $ PrimNumber Nothing s <$> number b
  LitString s -> Just $ return $ PrimString s
  IdSystem s ->
    Just $ PrimSysFun s <$> option [] (parens $ wempty "system function argument" $ csl parseExpr)
  IdSimple s -> Just $ idp s
  IdEscaped s -> Just $ idp s
  _ -> Nothing
  where
    parseExpr = genExpr pi pr pa ci cr
    fp s = PrimFun s <$> pa <*> parens (wempty "function argument" $ csl parseExpr)
    idp s = pi s >>= \ss -> fp ss <|> PrimIdent ss <$> pr

-- | Unary operator can only be applied on primary expressions
genBase :: PGenExpr GenExpr i r a
genBase pi pr pa ci cr = do
  op <- optionMaybe $
    mkpair
      ( fproduce $ \t -> case t of
          UnTilde -> Just UnNot
          UnBang -> Just UnLNot
          UnTildeAmp -> Just UnNand
          UnTildeBar -> Just UnNor
          AmBar -> Just UnOr
          AmHat -> Just UnXor
          AmAmp -> Just UnAnd
          AmTildeHat -> Just UnXNor
          SymPlus -> Just UnPlus
          SymDash -> Just UnMinus
          _ -> Nothing
      )
      pa
  p <- genPrim pi pr pa ci cr
  return $ maybe (ExprPrim p) (flip (uncurry ExprUnOp) p) op

-- | Facility for expression parsing
genExprBuildParser :: PGenExpr GenExpr i r a
genExprBuildParser pi pr pa ci cr =
  buildExpressionParser
    [ infixop $ \t -> case t of BinAsterAster -> Just BinPower; _ -> Nothing,
      infixop $ \t -> case t of
        SymAster -> Just BinTimes
        BinSlash -> Just BinDiv
        BinPercent -> Just BinMod
        _ -> Nothing,
      infixop $ \t -> case t of SymPlus -> Just BinPlus; SymDash -> Just BinMinus; _ -> Nothing,
      infixop $ \t -> case t of
        BinLtLt -> Just BinLSL
        BinGtGt -> Just BinLSR
        BinLtLtLt -> Just BinASL
        BinGtGtGt -> Just BinASR
        _ -> Nothing,
      infixop $ \t -> case t of
        BinLt -> Just BinLT
        SymLtEq -> Just BinLEq
        BinGt -> Just BinGT
        BinGtEq -> Just BinGEq
        _ -> Nothing,
      infixop $ \t -> case t of
        BinEqEq -> Just BinEq
        BinBangEq -> Just BinNEq
        BinEqEqEq -> Just BinCEq
        BinBangEqEq -> Just BinCNEq
        _ -> Nothing,
      infixop $ \t -> case t of AmAmp -> Just BinAnd; _ -> Nothing,
      infixop $ \t -> case t of AmHat -> Just BinXor; AmTildeHat -> Just BinXNor; _ -> Nothing,
      infixop $ \t -> case t of AmBar -> Just BinOr; _ -> Nothing,
      infixop $ \t -> case t of BinAmpAmp -> Just BinLAnd; _ -> Nothing,
      infixop $ \t -> case t of BinBarBar -> Just BinLOr; _ -> Nothing
    ]
    (genBase pi pr pa ci cr)
  where
    infixop fp = [Infix ((\op a l -> ExprBinOp l op a) <$> fproduce fp <*> pa) AssocLeft]

-- | Parametric expression
genExpr :: PGenExpr GenExpr i r a
genExpr pi pr pa ci cr = do
  e <- genExprBuildParser pi pr pa ci cr
  b <- optConsume SymQuestion
  if b
    then ExprCond e <$> pa <*> genExpr pi pr pa ci cr <* consume SymColon <*> genExpr pi pr pa ci cr
    else return e

expr :: Parser Expr
expr = Expr <$> genExpr (trHierIdent True) dimRange attributes constifyIdent Just

constExpr :: Parser CExpr
constExpr =
  CExpr
    <$> genExpr
      (pure . Identifier)
      (optionMaybe constRangeExpr)
      attributes
      Just
      constifyMaybeRange

-- | Minimum, Typical, Maximum on a base type recognised by the argument parser
mtm :: Parser a -> Parser (GenMinTypMax a)
mtm p = do
  x <- p
  b <- optConsume SymColon
  if b
    then MTMFull x <$> p <* consume SymColon <*> p
    else return $ MTMSingle x

-- | Ranges
range2 :: Parser Range2
range2 = brackets $ Range2 <$> constExpr <* consume SymColon <*> constExpr

genRangeExpr :: Parser e -> (e -> Maybe CExpr) -> Parser (GenRangeExpr e)
genRangeExpr pe constf =
  brackets $ do
    b <- pe
    f <- optionMaybe $ try $ fproduce $ \t -> case t of
      SymColon -> case constf b of
        Nothing -> Nothing
        Just m -> Just $ GREPair . Range2 m
      SymPlusColon -> Just $ GREBaseOff b False
      SymDashColon -> Just $ GREBaseOff b True
      _ -> Nothing
    maybe (pure $ GRESingle b) (flip fmap constExpr) f

constRangeExpr :: Parser (CRangeExpr)
constRangeExpr = genRangeExpr constExpr Just

-- | Specify terminal
specTerm :: Parser SpecTerm
specTerm = SpecTerm <$> ident <*> optionMaybe constRangeExpr

-- | Reference and constant minimum typical maximum
cmtmRef :: Parser (Identified (Maybe CMinTypMax))
cmtmRef = Identified <$> ident <*> optionMaybe (brackets $ mtm constExpr)

-- | Sized reference
instName :: Parser InstanceName
instName = InstanceName <$> ident <*> optionMaybe range2

-- | Signedness and range, both optional
signRange :: Parser SignRange
signRange = SignRange <$> optConsume KWSigned <*> optionMaybe range2

-- | Index for each dimension then bit range
genDimRange :: Parser e -> (e -> Maybe CExpr) -> Parser (Maybe (GenDimRange e))
genDimRange pe constf = do
  l <- many $ genRangeExpr pe constf
  case l of
    [] -> return Nothing
    h : t ->
      maybe
        (hardfail "Only the last bracketed expression is allowed to be a range")
        (pure . Just . uncurry (flip GenDimRange))
        $ foldrMapM1
          (\x -> Just (x, []))
          (\x (y, t) -> (,) y . (: t) <$> case x of GRESingle e -> Just e; _ -> Nothing)
          (h :| t)

dimRange :: Parser (Maybe DimRange)
dimRange = genDimRange expr constifyExpr

constDimRange :: Parser (Maybe CDimRange)
constDimRange = genDimRange constExpr Just

-- | Hierarchical identifier
trHierIdent :: Bool -> B.ByteString -> Parser HierIdent
trHierIdent safety s = do
  l <- many $
    mkpair
      ((if safety then try else id) $ optionMaybe (brackets constExpr) <* consume SymDot)
      ident
  return $
    hiPath %~ reverse $ 
      foldl'
        (\(HierIdent p i) (index, ss) -> HierIdent ((i, index) : p) ss)
        (HierIdent [] $ Identifier s)
        l

hierIdent :: Bool -> Parser HierIdent
hierIdent safety = parseBS >>= trHierIdent safety

-- | Lvalues
lval :: Parser (Maybe dr) -> Parser (LValue dr)
lval p = LVConcat <$> bcsl1 (lval p) <|> liftA2 LVSingle (hierIdent True) p

netLV :: Parser NetLValue
netLV = lval constDimRange

varLV :: Parser VarLValue
varLV = lval dimRange

-- | Assignments
varAssign :: Parser VarAssign
varAssign = Assign <$> varLV <* consume SymEq <*> expr

-- | Common abtract types for variables, parameters, functions and tasks
abstractType :: Parser AbsType
abstractType = fproduce $ \t -> case t of
  KWInteger -> Just ATInteger
  KWReal -> Just ATReal
  KWRealtime -> Just ATRealtime
  KWTime -> Just ATTime
  _ -> Nothing

-- | Common types for variables, parameters, functions and tasks
comType :: Parser t -> Parser (ComType t)
comType p = CTAbstract <$> abstractType <|> CTConcrete <$> p <*> signRange

-- | Net types
netType :: LProduce NetType
netType =
  [ (KWSupply0, NTSupply0),
    (KWSupply1, NTSupply1),
    (KWTri, NTTri),
    (KWTriand, NTTriAnd),
    (KWTrior, NTTriOr),
    (KWTri0, NTTri0),
    (KWTri1, NTTri1),
    (KWUwire, NTUwire),
    (KWWire, NTWire),
    (KWWand, NTWAnd),
    (KWWor, NTWOr)
  ]

-- | Parses local and nonlocal parameters declarations
trParamDecl :: Bool -> Parser (NonEmpty (Identifier, Parameter))
trParamDecl safety = do
  t <- comType $ pure ()
  scsl1 safety ident $ \s -> consume SymEq >> (,) s . Parameter t <$> mtm constExpr

paramDecl :: Bool -> Parser (NonEmpty (Identifier, Parameter))
paramDecl b = trParamDecl b <* consume SymSemi

-- | Function and task input arguments
funArgDecl :: Bool -> LABranch (NonEmpty (AttrIded (TFBlockDecl ())))
funArgDecl safety =
  [ ( KWInput,
      \a -> do
        kind <- comType $ optConsume KWReg
        scsl1 safety ident (\s -> return $ AttrIded a s $ TFBDPort () kind)
    )
  ]

taskArgDecl :: Bool -> LABranch (NonEmpty (AttrIded (TFBlockDecl Dir)))
taskArgDecl safety =
  [ (KWInput, pp DirIn),
    (KWInout, pp DirInOut),
    (KWOutput, pp DirOut)
  ]
  where
    pp dir a = do
      kind <- comType $ optConsume KWReg
      scsl1 safety ident (\s -> return $ AttrIded a s $ TFBDPort dir kind)

-- | Delay1/2/3
delayCom :: Parser NumIdent
delayCom = fproduce $ \t -> case t of
  LitDecimal i -> Just $ NINumber i
  LitReal s -> Just $ NIReal s
  IdSimple s -> Just $ NIIdent $ Identifier s
  IdEscaped s -> Just $ NIIdent $ Identifier s
  _ -> Nothing

delay1 :: Parser Delay1
delay1 = D1Base <$> delayCom <|> D11 <$> parens (mtm expr)

delay2 :: Parser Delay2
delay2 = do
  consume SymPound
  D2Base <$> delayCom
    <|> parens (mtm expr >>= \a -> option (D21 a) $ consume SymComma >> D22 a <$> mtm expr)

delay3 :: Parser Delay3
delay3 = do
  consume SymPound
  D3Base <$> delayCom <|> do
    l <- pcsl1 (mtm expr)
    case l of
      [a] -> return $ D31 a
      [a, b] -> return $ D32 a b
      [a, b, c] -> return $ D33 a b c
      _ -> hardfail "A delay cannot have more than 3 elemets"

-- | Drive strength
strength :: Parser (Either Bool (Strength, Bool))
strength = fproduce $ \t -> case t of
  KWSupply0 -> Just $ Right (StrSupply, False)
  KWSupply1 -> Just $ Right (StrSupply, True)
  KWStrong0 -> Just $ Right (StrStrong, False)
  KWStrong1 -> Just $ Right (StrStrong, True)
  KWPull0 -> Just $ Right (StrPull, False)
  KWPull1 -> Just $ Right (StrPull, True)
  KWWeak0 -> Just $ Right (StrWeak, False)
  KWWeak1 -> Just $ Right (StrWeak, True)
  KWHighz0 -> Just $ Left False
  KWHighz1 -> Just $ Left True
  _ -> Nothing

comDriveStrength :: Parser DriveStrength
comDriveStrength = do
  s1 <- strength
  consume SymComma
  s2 <- strength
  case (s1, s2) of
    (Left _, Left _) -> hardfail "Only one of the strength can be high impedence"
    (Left bl, Right (s, br)) ->
      if bl == br
        then hardfail "Both strength must refer to a different value"
        else return DSHighZ {_dsHZ = bl, _dsStr = s}
    (Right (s, br), Left bl) ->
      if bl == br
        then hardfail "Both strength must refer to a different value"
        else return DSHighZ {_dsHZ = bl, _dsStr = s}
    (Right (s1, b1), Right (s2, b2)) -> case (b1, b2) of
      (False, True) -> return DSNormal {_ds0 = s1, _ds1 = s2}
      (True, False) -> return DSNormal {_ds0 = s2, _ds1 = s1}
      _ -> hardfail "Both strength must refer to a different value"

driveStrength :: Parser DriveStrength
driveStrength = option dsDefault $ try $ parens comDriveStrength

-- | Best effort PATHPULSE parser
pathpulse :: Parser SpecParamDecl
pathpulse = do
  imid <- fproduce $ \t -> case t of TknPP s -> Just s; _ -> Nothing
  miid <- if B.null imid then optionMaybe parseBS else return $ Just imid
  c0 <- case miid of
    Nothing -> return $ Left Nothing
    Just iid -> do
      irng <- pMRE
      let ist = SpecTerm (Identifier iid) irng
      option (Right (iid, irng)) $ Left . Just . (,) ist <$> fbranch
        ( \t -> case t of
            SymDollar -> Just $ specTerm
            IdSystem s -> Just $ SpecTerm (Identifier s) <$> pMRE
            _ -> Nothing
        )
  c1 <- case c0 of
    Left x -> return $ Left x
    Right (iid, irng) -> case reverse $ B.split (c2w '$') iid of
      [_] -> failure
      h : t | B.null h && irng == Nothing ->
        Left . Just . (,) (SpecTerm (Identifier $ restore_id t) Nothing) <$> specTerm
          <|> pure (Right (irng, h : t))
      l -> return $ Right (irng, l)
  iost <- case c1 of
    Left x -> return x
    Right (irng, l) -> let (dollar_suffix, rest) = span B.null l in case rest of
      not_dollar : x@(_ : _) ->
        return $
          Just
            ( SpecTerm (Identifier $ restore_id $ dollar_suffix <> [not_dollar]) Nothing,
              SpecTerm (Identifier $ restore_id x) irng
            )
      _ -> failure
  consume SymEq
  parens $ do
    rej <- mtm constExpr
    err <- option rej $ consume SymComma *> mtm constExpr
    return $ SPDPathPulse iost rej err
  where
    pMRE = optionMaybe constRangeExpr
    restore_id = B.intercalate "$" . reverse
    failure = fail "Pathpulse expects two dollar separated specify terminals"

-- | Specify parameter declaration
specParam :: Parser (Maybe Range2, NonEmpty SpecParamDecl)
specParam = do
  mkpair (optionMaybe range2) $
    csl1 $ SPDAssign <$> ident <* consume SymEq <*> mtm constExpr <|> pathpulse

-- | Event control
eventControl :: Parser EventControl
eventControl = fpbranch $ \p t -> case t of
  SymAster -> Just $ return ECDeps
  SymParenAster -> Just $ closeConsume p SymParenAster SymParenR *> pure ECDeps -- yeah, f*** that
  IdSimple s -> Just $ ECIdent <$> trHierIdent False s
  IdEscaped s -> Just $ ECIdent <$> trHierIdent False s
  SymParenL ->
    Just $ consume SymAsterParen *> pure ECDeps -- yeah, f*** that
      <|> (consume SymAster *> pure ECDeps <|> ECExpr . NE.fromList <$> eventexpr)
        <* closeConsume p SymParenL SymParenR
  _ -> Nothing
  where
    eventexpr =
      sepBy1
        ( do
            p <- option EPAny $
              fproduce $ \t -> case t of
                KWPosedge -> Just EPPos
                KWNegedge -> Just EPNeg
                _ -> Nothing
            EventPrim p <$> expr
        )
        (fproduce $ \t -> case t of SymComma -> Just (); KWOr -> Just (); _ -> Nothing)

-- | Statement blocks: begin/end and fork/join
stmtBlock :: Bool -> SourcePos -> Parser Statement
stmtBlock kind pos = do
  ms <- optionMaybe $ consume SymColon *> ident
  (decl, body) <- smanythen
    attributes
    (fmap NE.toList . joinWith (lproduce stdBlockDecl))
    (\a -> Attributed a <$> statement)
  let d = concat decl
  h <- case (d, ms) of
    (_ : _, Nothing) -> do
      warn pos "Declaration in an unnamed statement block is a SystemVerilog feature"
      return $
        Just (Identifier $ B.pack $ map c2w $ printf "__block_at_line_%d__" (sourceLine pos), d)
    _ -> return $ flip (,) d <$> ms
  closeConsume pos (if kind then KWFork else KWBegin) (if kind then KWJoin else KWEnd)
  return $ SBlock h kind body

-- | Statement case: case, casex and casez
caseX :: ZOX -> Parser Statement
caseX zox = do
  cond <- parens expr
  l0 <- pci
  (d, l1) <- (if null l0 then id else option (Attributed [] Nothing, [])) $
    consume KWDefault *> optional (consume SymColon) *> mkpair optStmt pci
  return $ SCase zox cond (l0 <> l1) d
  where
    pci = many $ CaseItem <$> csl1 expr <* consume SymColon <*> optStmt

blockass :: VarLValue -> Parser Statement
blockass lv = do
  bl <- fproduce $ \t -> case t of SymEq -> Just True; SymLtEq -> Just False; _ -> Nothing
  delev <- optionMaybe $
    fbranch $ \t -> case t of
      SymPound -> Just $ DECDelay <$> delay1
      SymAt -> Just $ DECEvent <$> eventControl
      KWRepeat -> Just $ DECRepeat <$> parens expr <* consume SymAt <*> eventControl
      _ -> Nothing
  e <- expr
  consume SymSemi
  return $ SBlockAssign bl (Assign lv e) delev

-- | Statement
statement :: Parser Statement
statement = fpbranch $ \p t -> case t of
  SymPound -> Just $ SProcTimingControl . Left <$> delay1 <*> optStmt
  SymAt -> Just $ SProcTimingControl . Right <$> eventControl <*> optStmt
  SymDashGt -> Just $ SEventTrigger <$> hierIdent True <*> many (brackets expr) <* consume SymSemi
  KWFork -> Just $ stmtBlock True p
  KWBegin -> Just $ stmtBlock False p
  KWCase -> Just $ caseX ZOXO <* closeConsume p KWCase KWEndcase
  KWCasez -> Just $ caseX ZOXZ <* closeConsume p KWCasez KWEndcase
  KWCasex -> Just $ caseX ZOXX <* closeConsume p KWCasex KWEndcase
  KWDisable -> Just $ SDisable <$> hierIdent False <* consume SymSemi
  KWWait -> Just $ SWait <$> parens expr <*> optStmt
  KWIf ->
    Just $
      SIf <$> parens expr <*> optStmt <*> option (Attributed [] Nothing) (consume KWElse *> optStmt)
  KWAssign -> Just $ SProcContAssign . PCAAssign <$> varAssign <* consume SymSemi
  KWDeassign -> Just $ SProcContAssign . PCADeassign <$> varLV <* consume SymSemi
  KWForce -> Just $ do
    va <- varAssign
    consume SymSemi
    return $
      SProcContAssign $
        PCAForce $
          maybe (Left va) (\nl -> Right $ Assign nl $ _aValue va) $ constifyLV $ _aLValue va
  KWRelease -> Just $ do
    vl <- varLV
    consume SymSemi
    return $ SProcContAssign $ PCARelease $ maybe (Left vl) Right $ constifyLV vl
  KWForever -> Just $ stmtLoop LSForever
  KWRepeat -> Just $ LSRepeat <$> parens expr >>= stmtLoop
  KWWhile -> Just $ LSWhile <$> parens expr >>= stmtLoop
  KWFor ->
    Just $
      parens (LSFor <$> varAssign <* consume SymSemi <*> expr <* consume SymSemi <*> varAssign)
        >>= stmtLoop
  IdSystem s ->
    Just $
      SSysTaskEnable s <$> option [] (NE.toList <$> pcsl1 (optionMaybe expr)) <* consume SymSemi
  IdSimple s -> Just $ blockassortask s
  IdEscaped s -> Just $ blockassortask s
  SymBraceL -> Just $ (LVConcat <$> csl1 varLV <* closeConsume p SymBraceL SymBraceR) >>= blockass
  _ -> Nothing
  where
    stmtLoop ls = SLoop ls <$> attrStmt
    blockassortask s = do
      hi <- trHierIdent True s
      (dimRange >>= blockass . LVSingle hi)
        <|> do
          args <- option [] $ parens $ wempty "task argument" $ csl expr
          consume SymSemi
          return $ STaskEnable hi args

attrStmt :: Parser AttrStmt
attrStmt = Attributed <$> attributes <*> statement

trOptStmt :: Attributes -> Parser MybStmt
trOptStmt a = fmap (Attributed a) $ Just <$> statement <|> consume SymSemi *> return Nothing

optStmt :: Parser MybStmt
optStmt = attributes >>= trOptStmt

-- | Block declarations except parameters, including local parameters
blockDecl :: Parser t -> LBranch (BlockDecl (Compose NonEmpty Identified) t)
blockDecl p =
  [ (KWReg, BDReg <$> signRange <*> ppl p),
    (KWInteger, BDInt <$> ppl p),
    (KWReal, BDReal <$> ppl p),
    (KWTime, BDTime <$> ppl p),
    (KWRealtime, BDRealTime <$> ppl p),
    (KWEvent, BDEvent <$> ppl (many range2)),
    (KWLocalparam, BDLocalParam <$> comType (pure ()) <*> ppl (consume SymEq *> mtm constExpr))
  ]
  where
    ppl ps = Compose <$> csl1 (Identified <$> ident <*> ps) <* consume SymSemi

-- | Standard block declarations
stdBlockDecl :: LABranch (NonEmpty (AttrIded StdBlockDecl))
stdBlockDecl =
  (KWParameter, \a -> (fmap $ \(s, p) -> AttrIded a s $ SBDParameter p) <$> paramDecl False) :
    maplproduce
      (\p a -> fmap (\(Identified i x) -> AttrIded a i $ SBDBlockDecl x) . toStdBlockDecl <$> p)
      (blockDecl $ many range2)

type GIF a = Maybe InstanceName -> NetLValue -> NonEmpty Expr -> Maybe a

-- | Gate instantiation utility functions
gateInst :: GIF a -> Parser (NonEmpty a)
gateInst f = do
  l <- csl1 $ do
    n <- optionMaybe instName
    mgi <- parens $
      f n <$> netLV <* consume SymComma <*> csl1 expr
    case mgi of Just gi -> return gi; Nothing -> hardfail "Unexpected arguments"
  consume SymSemi
  return l

-- | Strength for pullup/pulldown
pullStrength :: Bool -> Parser DriveStrength
pullStrength ud = do
  e1 <- strength
  e2 <- optionMaybe $ consume SymComma *> strength
  case (e1, e2) of
    (Left b, Nothing) | ud == b -> return DSHighZ {_dsHZ = b, _dsStr = StrStrong}
    (Left bl, Just (Right (s, br))) | bl /= br -> return DSHighZ {_dsHZ = bl, _dsStr = s}
    (Right (s, br), Just (Left bl)) | bl /= br -> return DSHighZ {_dsHZ = bl, _dsStr = s}
    (Right (s, b), Nothing) | ud == b ->
      return $
        if b then DSNormal {_ds0 = StrStrong, _ds1 = s} else DSNormal {_ds1 = StrStrong, _ds0 = s}
    (Right (s1, False), Just (Right (s2, True))) -> return DSNormal {_ds0 = s1, _ds1 = s2}
    (Right (s1, True), Just (Right (s2, False))) -> return DSNormal {_ds0 = s2, _ds1 = s1}
    _ -> hardfail "Unexpected arguments"

-- | Task and function common parts
taskFun ::
  Bool ->
  (Bool -> LABranch (NonEmpty (AttrIded (TFBlockDecl a)))) ->
  Parser ([AttrIded (TFBlockDecl a)], [MybStmt])
taskFun zeroarg arglb = do
  l <- optionMaybe $
    parens $ ww "function ports" $ concat <$> csl (NE.toList <$> labranch (arglb True))
  consume SymSemi
  let dp = case l of
        Nothing -> maplaproduce (<* consume SymSemi) (arglb False) ++ sbd
        Just _ -> sbd
  (d, b) <- smanythen attributes (fmap NE.toList . joinWith (lproduce dp)) trOptStmt
  ww "function declarations" $ pure d
  case b of
    [x] -> pure ()
    _ -> do
      pos <- getPosition
      warn pos $
        printf "No or multiple statements in a %s is a SystemVerilog feature" $
          if zeroarg then "function" else "task" :: String
  return (concat d, b)
  where
    sbd = maplabranch (fmap $ fmap TFBDStd) stdBlockDecl
    ww s = if zeroarg then id else wempty s

-- | Trireg and net common properties
netProp :: Parser NetProp
netProp = do
  vs <- optionMaybe $
    fproduce $ \t -> case t of
      KWVectored -> Just True
      KWScalared -> Just False
      _ -> Nothing
  sn <- optConsume KWSigned
  vec <- if isJust vs then Just . (,) vs <$> range2 else optionMaybe $ (,) vs <$> range2
  d3 <- optionMaybe delay3
  return $ NetProp sn vec d3

-- | Uniform list of either initialisation or dimension
ediList :: Parser (Either (NonEmpty NetInit) (NonEmpty NetDecl))
ediList = do
  hdid <- ident
  hddi <- (consume SymEq >> Left <$> expr) <|> Right <$> many range2
  b <- optConsume SymComma
  if b
    then case hddi of
      Left hdi -> Left . (NetInit hdid hdi :|) <$> csl (NetInit <$> ident <* consume SymEq <*> expr)
      Right hdd -> Right . (NetDecl hdid hdd :|) <$> csl (NetDecl <$> ident <*> many range2)
    else return $ bimap ((:|[]) . NetInit hdid) ((:|[]) . NetDecl hdid) hddi

-- | Net declaration
netDecl :: NetType -> Parser ModGenSingleItem
netDecl nt = do
  ods <- optionMaybe $ parens comDriveStrength
  np <- netProp
  x <- case ods of
    Just ds -> MGINetInit nt ds np <$> csl1 (NetInit <$> ident <* consume SymEq <*> expr)
    Nothing -> either (MGINetInit nt dsDefault np) (MGINetDecl nt np) <$> ediList
  consume SymSemi
  return x

-- | Trireg declaration
triregDecl :: Parser ModGenSingleItem
triregDecl = do
  ods_cs <- optionMaybe $
    parens $
      Right <$> fproduce
        ( \t -> case t of
          KWSmall -> Just CSSmall
          KWMedium -> Just CSMedium
          KWLarge -> Just CSLarge
          _ -> Nothing
        )
        <|> Left <$> comDriveStrength
  np <- netProp
  x <- case ods_cs of
    Just (Left ds) -> MGITriD ds np <$> csl1 (NetInit <$> ident <* consume SymEq <*> expr)
    Just (Right cs) -> MGITriC cs np <$> csl1 (NetDecl <$> ident <*> many range2)
    Nothing -> either (MGITriD dsDefault np) (MGITriC CSMedium np) <$> ediList
  consume SymSemi
  return x

-- | Module or Generate region statement
comModGenItem :: LProduce (SourcePos -> Parser ModGenSingleItem)
comModGenItem =
  ( maplproduce (const . fmap MGIBlockDecl) $
      blockDecl $ (consume SymEq >> Right <$> constExpr) <|> Left <$> many range2
  )
  ++ maplproduce (const . netDecl) netType
  ++ [ (KWCmos, gateCmos False),
       (KWRcmos, gateCmos True),
       (KWBufif0, gateEnable False False),
       (KWBufif1, gateEnable False True),
       (KWNotif0, gateEnable True False),
       (KWNotif1, gateEnable True True),
       (KWNmos, gateMos False True),
       (KWPmos, gateMos False False),
       (KWRnmos, gateMos True True),
       (KWRpmos, gateMos True False),
       (KWAnd, gateNinp NITAnd False),
       (KWNand, gateNinp NITAnd True),
       (KWOr, gateNinp NITOr False),
       (KWNor, gateNinp NITOr True),
       (KWXor, gateNinp NITXor False),
       (KWXnor, gateNinp NITXor True),
       (KWBuf, gateNout False),
       (KWNot, gateNout True),
       (KWTranif0, gatePassen False False),
       (KWTranif1, gatePassen False True),
       (KWRtranif0, gatePassen True False),
       (KWRtranif1, gatePassen True True),
       (KWTran, gatePass False),
       (KWRtran, gatePass True),
       (KWPulldown, gatePull False),
       (KWPullup, gatePull True),
       (KWInitial, const $ MGIInitial <$> attrStmt),
       (KWAlways, const $ MGIAlways <$> attrStmt),
       (KWTrireg, const triregDecl),
       (KWGenvar, const $ MGIGenVar <$> csl1 ident <* consume SymSemi),
       ( KWIf,
         const $
           fmap MGICondItem $
             MGCIIf <$> parens constExpr
               <*> genCondBlock
               <*> option GCBEmpty (consume KWElse *> genCondBlock)
       ),
       ( KWBegin,
         \pos -> do
           warn pos "Generate blocks outside of for/if/case is a Verilog 2001 feature disallowed by later standards"
           s <- option "" $ consume SymColon *> ident
           gr <- many $ parseItem id []
           closeConsume pos KWBegin KWEnd
           return $
             MGICondItem $
               MGCIIf (CExpr $ genexprnumber 1) (GCBBlock $ Identified s $ concat gr) GCBEmpty
       ),
       ( KWAssign,
         const $
           MGIContAss <$> driveStrength
             <*> optionMaybe delay3
             <*> csl1 (Assign <$> netLV <* consume SymEq <*> expr)
             <* consume SymSemi
       ),
       ( KWDefparam,
         const $
           MGIDefParam <$> csl1 (ParamOver <$> hierIdent False <* consume SymEq <*> mtm constExpr)
             <* consume SymSemi
       ),
       ( KWCase,
         \pos -> do
           cond <- parens constExpr
           (d, b) <- do
             cb <- many cbranch
             (if null cb then id else option (GCBEmpty, cb)) $ do
               consume KWDefault
               option () $ consume SymColon
               mkpair genCondBlock $ (cb <>) <$> many cbranch
           closeConsume pos KWCase KWEndcase
           return $ MGICondItem $ MGCICase cond b d
       ),
       ( KWFor,
         const $
           parens
             ( MGILoopGen <$> ident
                 <* consume SymEq
                 <*> constExpr
                 <* consume SymSemi
                 <*> constExpr
                 <* consume SymSemi
                 <*> ident
                 <* consume SymEq
                 <*> constExpr
             )
             <*> (genBlock <|> Identified "" <$> genSingle)
       ),
       ( KWTask,
         \pos -> do
           auto <- optConsume KWAutomatic
           name <- ident
           (decl, body) <- taskFun True taskArgDecl
           closeConsume pos KWTask KWEndtask
           return $
             MGITask auto name decl $
               case body of
                 [x] -> x
                 _ -> Attributed [] $ Just $ SBlock Nothing False $ map fromMybStmt body
       ),
       ( KWFunction,
         \pos -> do
           auto <- optConsume KWAutomatic
           t <- optionMaybe (comType $ pure ())
           name <- ident
           (decl, body) <- taskFun False funArgDecl
           b <- case traverse (traverse fromStatement . fromMybStmt) body of
             Nothing ->
              hardfail
                "Events, delays, continuous assignments and tasks are forbidden in function statements"
             Just [Attributed [] x] -> return x
             Just l -> return $ FSBlock Nothing False l
           closeConsume pos KWFunction KWEndfunction
           return $ MGIFunc auto t name decl b
       )
     ]
  where
    cbranch = GenCaseItem <$> csl1 constExpr <* consume SymColon <*> genCondBlock
    gateCmos r _ =
      MGICMos r <$> optionMaybe delay3
        <*> gateInst
          (\mn lv args -> case args of [i, n, p] -> Just $ GICMos mn lv i n p; _ -> Nothing)
    gateEnable r b _ =
      MGIEnable r b <$> driveStrength
        <*> optionMaybe delay3
        <*> gateInst
          (\n lv args -> case args of [inp, en] -> Just $ GIEnable n lv inp en; _ -> Nothing)
    gateMos r np _ =
      MGIMos r np <$> optionMaybe delay3
        <*> gateInst
          (\n lv args -> case args of [inp, en] -> Just $ GIMos n lv inp en; _ -> Nothing)
    gateNinp t n _ =
      MGINIn t n <$> driveStrength
        <*> optionMaybe delay2
        <*> gateInst (\n o i -> Just $ GINIn n o i)
    gateNout r _ =
      MGINOut r <$> driveStrength
        <*> optionMaybe delay2
        <*> gateInst
          ( \n lv args ->
              fmap (\(e, t) -> GINOut n (lv :| t) e) $
                foldrMapM1 (\x -> Just (x, [])) (\x (y, t) -> (,) y . (: t) <$> expr2netlv x) args
          )
    gatePassen r b _ =
      MGIPassEn r b <$> optionMaybe delay2
        <*> gateInst
          ( \n lv args -> case args of
              [x, y] -> (flip (GIPassEn n lv) y) <$> expr2netlv x
              _ -> Nothing
          )
    gatePass r _ =
      fmap (MGIPass r) $
        gateInst $ \n lv args -> case args of [x] -> GIPass n lv <$> expr2netlv x; _ -> Nothing
    gatePull ud _ =
      MGIPull ud <$> option dsDefault (try $ parens $ pullStrength ud)
        <*> csl1 (GIPull <$> optionMaybe instName <*> parens netLV)
        <* consume SymSemi

data MPUD
  = MPUDUDPDelay (Maybe Delay2)
  | MPUDModParam ParamAssign
  | MPUDUknNone
  | MPUDUknSingle Expr
  | MPUDUknDouble Expr Expr

ismod :: MPUD -> Bool
ismod x = case x of MPUDModParam _ -> True; _ -> False

isukn :: MPUD -> Bool
isukn x = case x of MPUDModParam _ -> False; MPUDUDPDelay _ -> False; _ -> True

-- | Module parameters or udp delay
modparamudpdelay :: Parser MPUD
modparamudpdelay =
  MPUDUDPDelay . Just . D2Base <$> delayCom
    <|> parens (MPUDModParam . ParamNamed . NE.toList <$> namedparam <|> mpud)
  where
    namedparam =
      xcsl1 "parameter instantiation" $
        consume SymDot >> Identified <$> ident <*> parens (optionMaybe $ mtm expr)
    mpud = do
      l <- wxcsl "parameter instantiation or delay specification" $ mtm expr
      case mapM (\p -> case p of MTMSingle e -> Just e; _ -> Nothing) l of
        Nothing -> case l of
          [x] -> return $ MPUDUDPDelay $ Just $ D21 x
          [x, y] -> return $ MPUDUDPDelay $ Just $ D22 x y
          _ -> hardfail "Delay expression cannot have more than 2 elements"
        Just [x] -> return $ MPUDUknSingle x
        Just [x, y] -> return $ MPUDUknDouble x y
        Just l -> return $ MPUDModParam $ ParamPositional l

data MUUPayload
  = MUUPMod ModInst
  | MUUPUDP UDPInst
  | MUUPUkn UknInst

-- | The actual instance of a module or user defined primive
modudpinstance :: MPUD -> Parser MUUPayload
modudpinstance what = do
  mn <- optionMaybe instName
  args <- parens $ do
    a <- attributes
    do {
      x <- namePort a;
      PortNamed . (x :) <$> commathen (xcsl "port connections" $ attributes >>= namePort)
    }
      <|> (ordPort a >>= \x -> PortPositional . (x :) <$> commathen (csl $ attributes >>= ordPort))
  let argl = case args of
        PortPositional l@(_ : _ : _) ->
          traverse (\x -> case x of Attributed [] y -> y; _ -> Nothing) l
        _ -> Nothing
  case (mn, argl) of
    (Just n, Just (l : h : t)) | isukn what -> case expr2netlv l of
      Just lv -> return $ MUUPUkn $ UknInst n lv $ h :| t
      Nothing -> return $ MUUPMod $ ModInst n args
    (_, Just (l : h : t)) | not (ismod what) -> case expr2netlv l of
      Just lv -> return $ MUUPUDP $ UDPInst mn lv $ h :| t
      Nothing -> failure
    (Just n, Nothing) | isukn what -> return $ MUUPMod $ ModInst n args
    (Just n, _) | ismod what -> return $ MUUPMod $ ModInst n args
    _ -> failure
  where
    failure = hardfail "Got mixed elements of module and udp instatiation"
    commathen p = option [] $ consume SymComma *> p
    -- Port instantiation relying on order
    ordPort a = Attributed a <$> optionMaybe expr
    -- Port instantiation relying on name
    namePort a = consume SymDot >> AttrIded a <$> ident <*> parens (optionMaybe expr)

-- | Module or udp instantiation, they are tricky to differentiate (if not impossible sometimes)
modudpInst :: Parser ModGenSingleItem
modudpInst = do
  kind <- lenientIdent
  ds <- optionMaybe $ try $ parens comDriveStrength
  del_par <- option MPUDUknNone $
    if isJust ds then MPUDUDPDelay . Just <$> delay2 else consume SymPound *> modparamudpdelay
  insts <- csl1 $ modudpinstance del_par
  del_par <- if isukn del_par
    then maybe failure pure $ foldrM reduce_unknown del_par insts
    else pure del_par
  consume SymSemi
  return $ case del_par of
    MPUDUDPDelay d2 -> MGIUDPInst kind (maybe dsDefault id ds) d2 $ mkudp <$> insts
    MPUDModParam pa -> MGIModInst kind pa $ mkmod <$> insts
    MPUDUknNone -> MGIUnknownInst kind Nothing $ mkukn <$> insts
    MPUDUknSingle x -> MGIUnknownInst kind (Just $ Left x) $ mkukn <$> insts
    MPUDUknDouble x y -> MGIUnknownInst kind (Just $ Right (x, y)) $ mkukn <$> insts
  where
    failure = hardfail "Got mixed elements of module and udp instatiation"
    unreachable = error "Internal error, unreachable case reached"
    reduce_unknown i a = case i of
      MUUPUkn _ -> Just a
      MUUPMod _ -> case a of
        MPUDUDPDelay _ -> Nothing
        MPUDModParam _ -> Just a
        MPUDUknNone -> Just $ MPUDModParam $ ParamPositional []
        MPUDUknSingle x -> Just $ MPUDModParam $ ParamPositional [x]
        MPUDUknDouble x y -> Just $ MPUDModParam $ ParamPositional [x, y]
      MUUPUDP _ -> case a of
        MPUDModParam _ -> Nothing
        MPUDUDPDelay _ -> Just a
        MPUDUknNone -> Just $ MPUDUDPDelay $ Nothing
        MPUDUknSingle x -> Just $ MPUDUDPDelay $ Just $ D21 $ MTMSingle x
        MPUDUknDouble x y -> Just $ MPUDUDPDelay $ Just $ D22 (MTMSingle x) (MTMSingle y)
    mkmod i = case i of
      MUUPMod i -> i
      MUUPUDP _ -> unreachable
      MUUPUkn (UknInst n lv args) ->
        ModInst n $ PortPositional $ map (Attributed [] . Just) $ netlv2expr lv : NE.toList args
    mkudp i = case i of
      MUUPMod _ -> unreachable
      MUUPUDP i -> i
      MUUPUkn (UknInst n lv args) -> UDPInst (Just n) lv args
    mkukn i = case i of
      MUUPMod _ -> unreachable
      MUUPUDP _ -> unreachable
      MUUPUkn i -> i

-- | Parse a module or generate region item along other given possibilities
-- | and converts it to the right type using the provided conversion function
parseItem :: (Attributed ModGenBlockedItem -> a) -> LAPBranch (NonEmpty a) -> Parser [a]
parseItem f lb = do
  a <- attributes
  pos <- getPosition
  fmap NE.toList $
    do {
      p <- lproduce $
        lb ++ maplproduce
          (\p a pos -> fmap (f . Attributed a) . toMGBlockedItem <$> p pos)
          comModGenItem;
      p a pos
    }
      <|> (\p a -> fmap (f . Attributed a) . toMGBlockedItem <$> p) modudpInst a

-- | Generate block
genBlock :: Parser GenerateBlock
genBlock = do
  pos <- getPosition
  consume KWBegin
  i <- option "" $ consume SymColon *> ident
  b <- many $ parseItem id []
  closeConsume pos KWBegin KWEnd
  return $ Identified i $ concat b

genSingle ::  Parser [Attributed ModGenBlockedItem]
genSingle = do
  a <- attributes
  pos <- getPosition
  b <- joinWith (lproduce comModGenItem) pos <|> modudpInst
  return $ Attributed a <$> toList (toMGBlockedItem b)

genCondBlock :: Parser GenerateCondBlock
genCondBlock = consume SymSemi *> return GCBEmpty <|> GCBBlock <$> genBlock <|> do
  gb <- genSingle
  return $ case gb of
    [Attributed a (MGICondItem ci)] -> GCBConditional $ Attributed a ci
    _ -> GCBBlock $ Identified "" gb

type PortInterface = Identified [Identified (Maybe CRangeExpr)]

-- | Simple port declaration (Input, Output, InOut)
portsimple ::
  Maybe NetType ->
  Bool ->
  Dir ->
  Attributes ->
  Parser (NonEmpty (NonEmpty ModuleItem, PortInterface))
portsimple dnt fullspec d a = do
  nt <- optionMaybe $ lproduce netType
  sr <- signRange
  sl <- scsl1 fullspec ident pure
  let pd i = MIPort $ AttrIded a i (d, sr)
      nd t i =
        Attributed [] $
          MGINetDecl
            t
            (NetProp (_srSign sr) ((,) Nothing <$> _srRange sr) Nothing)
            (Identity $ NetDecl i [])
      pi i = Identified i [Identified i Nothing]
  case nt of
    Just nt -> return $ (\i -> ([pd i, MIMGI $ nd nt i], pi i)) <$> sl
    Nothing | fullspec -> case dnt of
      Just nt -> return $ (\i -> ([pd i, MIMGI $ nd nt i], pi i)) <$> sl
      Nothing ->
        hardfail "Ports declared in a module header must be typed when default_nettype is none"
    Nothing -> return $ (\i -> ([pd i], pi i)) <$> sl

-- | Declaration of a port with a variable type
portvariable ::
  Bool ->
  Attributes ->
  ( Compose Identity Identified (Either [Range2] CExpr) ->
    BlockDecl (Compose Identity Identified) (Either [Range2] CExpr)
  ) ->
  Parser (NonEmpty (NonEmpty ModuleItem, PortInterface))
portvariable fullspec a f =
  scsl1 fullspec ident $
    \s -> do
      e <- optionMaybe $ consume SymEq *> constExpr
      return
        ( [ MIPort $ AttrIded a s (DirOut, SignRange False Nothing),
            MIMGI $
              Attributed a $
                MGIBlockDecl $ f $ Compose $ Identity $ Identified s $ maybe (Left []) Right e
          ],
          Identified s [Identified s Nothing]
        )

-- | Port declaration
portDecl :: Maybe NetType -> Bool -> LABranch (NonEmpty ModuleItem, NonEmpty PortInterface)
portDecl dnt fullspec =
  [ (KWInput, ps DirIn),
    (KWInout, ps DirInOut),
    ( KWOutput,
      \a ->
        fbranch
          ( \t -> case t of
              KWReg -> Just $ signRange >>= pv a . BDReg
              KWInteger -> Just $ pv a BDInt
              KWTime -> Just $ pv a BDTime
              _ -> Nothing
          )
          <|> ps DirOut a
    )
  ]
  where
    mk p = first join . NE.unzip <$> p <* if fullspec then pure () else consume SymSemi
    ps d a = mk $ portsimple dnt fullspec d a
    pv a f = mk $ portvariable fullspec a f

-- | Port expression
portExpr :: Parser [Identified (Maybe CRangeExpr)]
portExpr = option [] $ (: []) <$> pp <|> NE.toList <$> bcsl1 pp
  where
    pp = Identified <$> ident <*> optionMaybe constRangeExpr

-- | Path declaration
trPathDecl :: SourcePos -> ModulePathCondition -> Parser (NonEmpty SpecifyBlockedItem)
trPathDecl pos cond = do
  edge <- optionMaybe $
    fproduce $ \t -> case t of
      KWPosedge -> Just True
      KWNegedge -> Just False
      _ -> Nothing
  inp <- csl1 specTerm
  po <- optionMaybe $
    fproduce $ \t -> case t of
      SymPlus -> Just True
      SymDash -> Just False
      _ -> Nothing
  pf <- fproduce $ \t -> case t of
    SymAsterGt -> Just False
    SymEqGt -> Just True
    _ -> Nothing
  (out, pol, eds) <-
    parens
      ( do
          outp <- csl1 specTerm
          po <- fbranch $ \t -> case t of
            SymPlus -> Just $ consume SymColon *> return (Just True)
            SymDash -> Just $ consume SymColon *> return (Just False)
            SymColon -> Just $ return Nothing
            SymPlusColon -> Just $ return $ Just True
            SymDashColon -> Just $ return $ Just False
            _ -> Nothing
          e <- expr
          return (outp, po, Just (e, edge))
      )
      <|> (\outp -> (outp, po, Nothing)) <$> csl1 specTerm
  co <- if pf
    then case (inp, out) of
      (i :| [], o :| []) -> return $ SPParallel i o
      _ -> hardfail "Parallel delay only accept single nets as source and destination"
    else return (SPFull inp out)
  case () of
    () | isJust po && isJust eds -> hardfail "Edge sensitive path with misplaced polarity operator"
    () | isJust edge && eds == Nothing ->
      hardfail "Mixed edge and non edge sensitive delay path elements"
    () -> return ()
  closeConsume pos SymParenL SymParenR
  consume SymEq
  vals <- try (csl1 $ mtm constExpr) <|> pcsl1 (mtm constExpr) -- cancer optional parentheses
  (:|[]) . SIPathDeclaration cond co pol eds <$> case vals of
    [e] -> return $ PDV1 e
    [e1, e2] -> return $ PDV2 e1 e2
    [e1, e2, e3] -> return $ PDV3 e1 e2 e3
    [e1, e2, e3, e4, e5, e6] -> return $ PDV6 e1 e2 e3 e4 e5 e6
    [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12] ->
      return $ PDV12 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12
    _ -> hardfail "Wrong number of argument"

pathDecl :: ModulePathCondition -> Parser (NonEmpty SpecifyBlockedItem)
pathDecl mpc = getPosition >>= \p -> consume SymParenL *> trPathDecl p mpc

-- | Timing check event
edgeDesc :: Parser EdgeDesc
edgeDesc = fbranch $ \t -> case t of
  KWPosedge -> Just $ return $ V.fromList [True, True, False, False, False, True]
  KWNegedge -> Just $ return $ V.fromList [False, False, True, True, True, False]
  KWEdge ->
    Just $
      (V.replicate 6 False V.//) . NE.toList
        <$> brackets
          ( csl1 $
              fproduce $ \t -> case t of
                EdgeEdge BXZ0 BXZ1 -> Just (0, True)
                EdgeEdge BXZ0 BXZX -> Just (1, True)
                EdgeEdge BXZ0 BXZZ -> Just (1, True)
                EdgeEdge BXZ1 BXZ0 -> Just (2, True)
                EdgeEdge BXZ1 BXZX -> Just (3, True)
                EdgeEdge BXZ1 BXZZ -> Just (3, True)
                EdgeEdge BXZX BXZ0 -> Just (4, True)
                EdgeEdge BXZX BXZ1 -> Just (5, True)
                EdgeEdge BXZZ BXZ0 -> Just (4, True)
                EdgeEdge BXZZ BXZ1 -> Just (5, True)
                _ -> Nothing
          )
  _ -> Nothing

-- | Timing check condition after &&&
-- | This thing is cancer because of ambiguous parens, tilde, and comparisons
-- | Especially parens and comparison because it requires early exit in expression
-- | And that is PANTS: Pure Absurdity Nobody Truly Supports
-- | So I decided to NOT parse it correctly.
-- | If you think you have a simple parser, your parser idea is likely wrong
-- | If you think it's complicated parser, your parser idea is likely wrong
-- | If you think it's absurd and you have a parser the size of this file,
-- | you're right but your parser idea is still likely wrong
timingCheckCond :: Parser (Bool, Expr)
timingCheckCond = do
  consume SymAmpAmpAmp
  try ((consume UnTilde >> (,) True <$> expr) <|> (,) False <$> expr)
    <|> (consume UnTilde >> (,) True <$> expr)

timingCheckEvent :: Parser TimingCheckEvent
timingCheckEvent =
  TimingCheckEvent <$> optionMaybe edgeDesc <*> specTerm <*> optionMaybe timingCheckCond

controlledTimingCheckEvent :: Parser ControlledTimingCheckEvent
controlledTimingCheckEvent =
  ControlledTimingCheckEvent <$> edgeDesc <*> specTerm <*> optionMaybe timingCheckCond

-- | Standard system timing check arguments
optoptChain :: b -> Parser a -> Parser b -> Parser (Maybe a, b)
optoptChain dn px pn =
  optConsume SymComma >>= \b -> if b then mkpair (optionMaybe px) pn else return (Nothing, dn)

comStcArgs :: Parser (TimingCheckEvent, TimingCheckEvent, Expr)
comStcArgs =
  (,,) <$> timingCheckEvent <* consume SymComma <*> timingCheckEvent <* consume SymComma <*> expr

stdStcArgs :: Parser STCArgs
stdStcArgs = do
  (r, d, e) <- comStcArgs
  STCArgs d r e <$> option Nothing (consume SymComma *> optionMaybe ident)

addStcArgs :: Parser (STCArgs, STCAddArgs)
addStcArgs = do
  (r, d, ll) <- comStcArgs
  consume SymComma
  lr <- expr
  let def1 = (Nothing, Nothing)
      def2 = (Nothing, def1)
      def3 = (Nothing, def2)
  (n, (sc, (cc, (dr, dd)))) <-
    optoptChain def3 ident $
      optoptChain def2 (mtm expr) $
        optoptChain def1 (mtm expr) $
          optoptChain Nothing cmtmRef $ option Nothing $ consume SymComma *> optionMaybe cmtmRef
  return (STCArgs d r ll n, STCAddArgs lr sc cc dr dd)

skewStcArgs :: Parser (Maybe Identifier, Maybe CExpr, Maybe CExpr)
skewStcArgs = do
  (n, (eb, ra)) <-
    optoptChain (Nothing, Nothing) ident $
      optoptChain Nothing constExpr $ option Nothing $ consume SymComma *> optionMaybe constExpr
  return (n, eb, ra)

-- | System timing check functions
stcfMap :: HashMap.HashMap B.ByteString (Parser SpecifyBlockedItem)
stcfMap =
  HashMap.fromList
    [ ("setup", SISetup . (\(STCArgs d r e n) -> STCArgs r d e n) <$> stdStcArgs),
      ("hold", SIHold <$> stdStcArgs),
      ("setuphold", uncurry SISetupHold <$> addStcArgs),
      ("recovery", SIRecovery <$> stdStcArgs),
      ("removal", SIRemoval <$> stdStcArgs),
      ("recrem", uncurry SIRecrem <$> addStcArgs),
      ("skew", SISkew <$> stdStcArgs),
      ( "timeskew",
        do
          (r, d, e) <- comStcArgs
          (n, eb, ra) <- skewStcArgs
          return $ SITimeSkew (STCArgs d r e n) eb ra
      ),
      ( "fullskew",
        do
          (r, d, e) <- comStcArgs
          consume SymComma
          tcl <- expr
          (n, eb, ra) <- skewStcArgs
          return $ SIFullSkew (STCArgs d r e n) tcl eb ra
      ),
      ( "period",
        SIPeriod <$> controlledTimingCheckEvent
          <* consume SymComma
          <*> expr
          <*> option Nothing (consume SymComma *> optionMaybe ident)
      ),
      ( "width",
        do
          e <- controlledTimingCheckEvent
          consume SymComma
          tcl <- expr
          (t, n) <- option (Nothing, Nothing) $ do
            consume SymComma
            mkpair (Just <$> constExpr) $ option Nothing $ do
              consume SymComma
              pos <- getPosition
              mid <- optionMaybe ident
              if mid == Nothing
                then warn pos "Optional $width notifier is a SystemVerilog feature"
                else pure ()
              return mid
          return $ SIWidth e tcl t n
      ),
      ( "nochange",
        SINoChange <$> timingCheckEvent
          <* consume SymComma
          <*> timingCheckEvent
          <* consume SymComma
          <*> mtm expr
          <* consume SymComma
          <*> mtm expr
          <*> option Nothing (consume SymComma *> optionMaybe ident)
      )
    ]

-- | Specify block item
specifyItem :: Parser (NonEmpty SpecifyBlockedItem)
specifyItem = fpbranch $ \p t -> case t of
  KWSpecparam -> Just $ (\(rng, l) -> SISpecParam rng . Identity <$> l) <$> specParam
  KWPulsestyleonevent -> Just $ psi SIPulsestyleOnevent
  KWPulsestyleondetect -> Just $ psi SIPulsestyleOndetect
  KWShowcancelled -> Just $ psi SIShowcancelled
  KWNoshowcancelled -> Just $ psi SINoshowcancelled
  KWIf -> Just $ do
    c <- parens $
      genExpr (pure . Identifier) (pure ()) attributes Just $ maybe (Just ()) $ const Nothing
    pathDecl $ MPCCond c
  KWIfnone -> Just $ pathDecl MPCNone
  SymParenL -> Just $ trPathDecl p MPCAlways
  IdSystem s -> fmap (:| []) . parens <$> HashMap.lookup s stcfMap
  _ -> Nothing
  where
    psi f = csl1 (f . Identity <$> specTerm)

-- | Non port declaration module item
npmodItem :: LAPBranch (NonEmpty ModuleItem)
npmodItem =
  (KWParameter, \a _ -> fmap (\(i, x) -> MIParameter $ AttrIded a i x) <$> paramDecl False) :
  [ (KWSpecparam, \a _ -> (\(rng, l) -> MISpecParam a rng <$> l) <$> specParam <* consume SymSemi),
    ( KWGenerate,
      \a pos -> if null a
        then (:|[]) . MIGenReg . concat <$> many (parseItem id [])
          <* closeConsume pos KWGenerate KWEndgenerate
        else hardfail "Generate region doesn't accept attributes"
    ),
    ( KWSpecify,
      \a pos -> if null a
        then (:|[]) . MISpecBlock . concat <$> many (NE.toList <$> specifyItem <* consume SymSemi)
          <* closeConsume pos KWSpecify KWEndspecify
        else hardfail "Specify region doesn't accept attributes"
    )
  ]

-- | Module
parseModule :: Bool -> LocalCompDir -> Attributes -> Parser Verilog2005
parseModule b (LocalCompDir ts cl pull dnt) a = do
  s <- lenientIdent
  params <- option [] $ do
    consume SymPound
    parens (wxcsl "parameter declaration" $ consume KWParameter *> trParamDecl True)
  -- if there is nothing the standard says the module cannot contain port declaration
  -- if there is empty parenthesis `()` the standard says it can or cannot, so it can
  (pd, pi) <- option (Just [], []) $ parens $ fullPort <|> (,) Nothing <$> csl partialPort
  consume SymSemi
  mi <- case pd of
    Nothing ->
      many $ parseItem MIMGI $ maplaproduce (const . fmap fst) (portDecl dnt False) ++ npmodItem
    Just pd -> (pd :) <$> many (parseItem MIMGI npmodItem)
  return mempty {_vModule = [ModuleBlock a b s pi (concat mi) ts cl pull dnt]}
  where
    -- Fully specified port declaration list
    fullPort =
      bimap (Just . NE.toList . join) (NE.toList . join) . NE.unzip
        <$> xcsl1 "port declaration" (labranch $ portDecl dnt True)
    -- Partially specified port declaration list
    partialPort =
      (consume SymDot >> Identified <$> ident <*> parens portExpr)
        <|> (\l -> case l of [Identified s _] -> Identified s l; _ -> Identified "" l) <$> portExpr

-- | Primitive output port
udpOutput :: Parser (PrimPort, Identifier)
udpOutput = do
  reg <- optConsume KWReg
  s <- ident
  pp <- if reg
    then PPOutReg <$> optionMaybe (consume SymEq *> constExpr)
    else return PPOutput
  return (pp, s)

-- | Udp port declaration
udpHead :: Parser (Identifier, NonEmpty Identifier, Maybe (NonEmpty (AttrIded PrimPort)))
udpHead =
  (,,) <$> ident <* consume SymComma <*> csl1 ident <*> pure Nothing <|> do
    attr <- attributes
    consume KWOutput
    (od, o) <- udpOutput
    consume SymComma
    ins <- csl1 $ do
      attr <- attributes
      consume KWInput
      l <- scsl1 True ident $ return . flip (AttrIded attr) PPInput
      return l
    let inl = join ins
    return (o, _aiIdent <$> inl, Just $ AttrIded attr o od <| inl)

-- | Parse a udp port declaration list
udpPort :: Parser (NonEmpty (AttrIded PrimPort))
udpPort = do
  a <- attributes
  l <- fbranch $ \t -> case t of
    KWReg -> Just $ (:|[]) . (,) PPReg <$> ident
    KWOutput -> Just $ (:|[]) <$> udpOutput
    KWInput -> Just $ csl1 $ (,) PPInput <$> ident
    _ -> Nothing
  consume SymSemi
  return $ uncurry (flip $ AttrIded a) <$> l

-- | Sequential primitive output initial value
initVal :: Parser ZOX
initVal = fbranch $ \t -> case t of
  LitDecimal 0 -> Just $ return ZOXZ
  LitDecimal 1 -> Just $
    option ZOXO $ do
      consume $ NumberBase False BBin
      fproduce $ \t -> case t of
        LitBinary [BXZ0] -> Just ZOXZ
        LitBinary [BXZ1] -> Just ZOXO
        LitBinary [BXZX] -> Just ZOXX
        _ -> Nothing
  _ -> Nothing

-- | Signal level
level :: Parser SigLevel
level = fproduce $ \t -> case t of
  TableOut ZOXZ -> Just L0
  TableOut ZOXO -> Just L1
  TableOut ZOXX -> Just LX
  TableIn False -> Just LQ
  TableIn True -> Just LB
  _ -> Nothing

-- | Sequential primitive nest state
nextState :: Parser (Maybe ZOX)
nextState =
  fproduce (\t -> case t of TableOut zox -> Just $ Just zox; SymDash -> Just Nothing; _ -> Nothing)
    <* consume SymSemi

-- | Sequential primitive input row
seqRow :: Parser SeqIn
seqRow = do
  comb <- many level
  res <- (if null comb then id else option $ SIComb $ NE.fromList comb) $ do
    e <- fpbranch $ \sp t -> case t of
      TableEdge AFRNPA -> Just $ return $ EdgeDesc LQ LQ
      TableEdge AFRNPF -> Just $ return $ EdgeDesc L1 L0
      TableEdge AFRNPR -> Just $ return $ EdgeDesc L0 L1
      TableEdge AFRNPN -> Just $ return (EdgePos_neg False)
      TableEdge AFRNPP -> Just $ return (EdgePos_neg True)
      SymParenL -> Just $ EdgeDesc <$> level <*> level <* closeConsume sp SymParenL SymParenR
      _ -> Nothing
    SISeq comb e <$> many level
  consume SymColon
  return res

-- | Parses a primitive block
udp :: Attributes -> Parser Verilog2005
udp attr = do
  udpid <- ident
  (out, ins, mpd) <- parens udpHead
  consume SymSemi
  pd <- maybe (join <$> manyNE udpPort) pure mpd
  init <- optionMaybe $ do
    consume KWInitial
    s <- ident
    if s /= out
      then hardfail "Initial value can only be assigned to output port"
      else consume SymEq *> initVal <* consume SymSemi
  consume KWTable
  -- parse a seqRow to know the table kind, if it's not an allowed kind we can error later
  hdi <- seqRow
  sl <- level
  hdd <- fbranch $ \t -> case t of
    SymSemi -> Just $ return Nothing
    SymColon -> Just $ Just <$> nextState
    _ -> Nothing
  let hdu = case sl of L0 -> Just ZOXZ; L1 -> Just ZOXO; LX -> Just ZOXX; _ -> Nothing
  -- use all the information to decide if the primitive is sequential or combinational
  body <- case (hdd, hdi, hdu) of
    (Nothing, SIComb comb, Just zox) | init == Nothing ->
      CombTable . (CombRow comb zox :|) <$> many combRow
    (Just ns, _, _) ->
      SeqTable init . (SeqRow hdi sl ns :|)
        <$> many (SeqRow <$> seqRow <*> level <* consume SymColon <*> nextState)
    _ -> hardfail "Got mixed information between sequential and combinatorial UDP"
  consume KWEndtable
  return mempty {_vPrimitive = [PrimitiveBlock attr udpid out ins pd body]}
  where
    -- Combinational primitive input row
    combRow = do
      l <- manyNE level
      consume SymColon
      o <- fproduce $ \t -> case t of TableOut zox -> Just zox; _ -> Nothing
      consume SymSemi
      return $ CombRow l o

-- | Parses an element of config blocks body
configItem :: Parser ConfigItem
configItem = do
  ci <- fbranch $ \t -> case t of
    KWCell -> Just $ CICell <$> dot1Ident
    KWInstance -> Just $ CIInst . NE.fromList <$> sepBy1 ident (consume SymDot)
    _ -> Nothing
  llu <- fbranch $ \t -> case t of
    KWLiblist -> Just $ LLULiblist <$> many parseBS
    KWUse ->
      Just $
        LLUUse <$> dot1Ident
          <*> option False (consume SymColon *> consume KWConfig *> return True)
    _ -> Nothing
  consume SymSemi
  return $ ConfigItem ci llu

-- | Parses a config block
config :: Parser Verilog2005
config = do
  s <- ident
  consume SymSemi
  consume KWDesign
  design <- many dot1Ident
  consume SymSemi
  b <- many configItem
  (b', d) <- option ([], []) $ do
    consume KWDefault
    consume KWLiblist
    d <- many parseBS
    consume SymSemi
    body <- many configItem
    return (body, d)
  return mempty {_vConfig = [ConfigBlock s design (b <> b') d]}

-- | Parses compiler directives
compDir :: Parser ()
compDir = fbranch $ \t -> case t of
  CDUnconnecteddrive ->
    Just $
      fbranch $
        \t -> case t of
          KWPull0 -> Just $ modifyState $ lcdPull .~ Just False
          KWPull1 -> Just $ modifyState $ lcdPull .~ Just True
          _ -> Nothing
  CDNounconnecteddrive -> Just $ modifyState $ lcdPull .~ Nothing
  CDDefaultnettype -> Just $ do
    nt <-
      Just <$> fproduce (\t -> IntMap.lookup (getConsIndex t) $ mkActionMap netType)
        <|> fproduce (\t -> if t == IdSimple "none" then Just Nothing else Nothing)
    modifyState $ lcdDefNetType .~ nt
  CDResetall -> Just $ putState lcdDefault
  CDTimescale -> Just $ do
    uoff <- fproduce $ \t -> case t of CDTSInt i -> Just i; _ -> Nothing
    ubase <- fproduce $ \t -> case t of CDTSUnit i -> Just i; _ -> Nothing
    poff <- fproduce $ \t -> case t of CDTSInt i -> Just i; _ -> Nothing
    pbase <- fproduce $ \t -> case t of CDTSUnit i -> Just i; _ -> Nothing
    modifyState $ lcdTimescale .~ Just (ubase + uoff, pbase + poff)
  CDBeginKeywords -> Just $ pure () -- handled at lexing, this makes sure it is used outside modules
  CDEndKeywords -> Just $ pure () -- same as above
  _ -> Nothing

-- | Parses a top-level declaration: config, module or primitive block
topDecl :: Parser Verilog2005
topDecl =
  skipMany1 compDir *> return mempty <|> do
    -- I'm not sure whether these compiler directives are allowed here
    a <- many (attributeOne <* skipMany compDir)
    st <- getState
    fpbranch $ \p t -> case t of
      KWPrimitive -> Just $ udp a <* closeConsume p KWPrimitive KWEndprimitive
      KWModule -> Just $ parseModule False st a <* closeConsume p KWModule KWEndmodule
      KWMacromodule -> Just $ parseModule True st a <* closeConsume p KWMacromodule KWEndmodule
      KWConfig | null a -> Just $ config <* closeConsume p KWConfig KWEndconfig
      _ -> Nothing

-- | Parses a verilog file by accumulating top-level declarations
verilog2005Parser :: Parser Verilog2005
verilog2005Parser = anywherecompdir *> monoAccum topDecl <* eof

-- | Parse a file containing Verilog 2005 code, also return non Verilog2005 conformity warnings
-- The lists in the Verilog2005 structure are in reverse order from source
parseVerilog2005 :: FilePath -> IO (Verilog2005, [String])
parseVerilog2005 file = do
  stres <- scanTokens file
  case stres of
    Left s -> error s
    Right strm ->
      let (x, w) = runWriter $ runParserT verilog2005Parser lcdDefault file strm
       in case x of
          Left s -> error $ show s
          Right ast -> return (ast, w)
