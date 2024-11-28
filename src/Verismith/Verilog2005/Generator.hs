-- Module      : Verismith.Verilog2005.Generator
-- Description : AST random generator
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : stable
-- Portability : POSIX
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Verismith.Verilog2005.Generator
  ( runGarbageGeneration,
    GarbageOpts,
    GenM,
    defGarbageOpts,
  )
where

import Control.Applicative (liftA2, liftA3)
import Data.Functor.Compose
import Data.Bifunctor (second)
import Control.Lens hiding ((<.>))
import Control.Monad (join, replicateM)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NE
import Data.Tuple
import qualified Data.Vector.Unboxed as VU
import Numeric.Natural
import System.Random.MWC.Probability
import Verismith.Config
import Verismith.Utils (mkpair)
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.Lexer
import Verismith.Verilog2005.Randomness

infixl 4 <.>

-- | Compose through several monad
(<.>) :: (Monad m, Applicative m) => m (a -> m b) -> m a -> m b
(<.>) mf mx = join $ mf <*> mx

-- | Attenuate the weight of a categorical probability if told so
-- | This avoids infinite size AST
attenuateCat :: (NonEmpty (Bool, a)) -> Double -> CategoricalProbability -> CategoricalProbability
attenuateCat l d p = case p of
  CPDiscrete wl -> CPDiscrete $ NE.zipWith (\a w -> w * if fst a then d else 1) l wl
  CPBiasedUniform wl wb ->
    let im = IntMap.fromListWith (+) $ map swap wl
     in CPDiscrete $
          NE.map (\(k, a) -> IntMap.findWithDefault wb k im * if fst a then d else 1) $
            NE.zip [0..] l

-- | Attenuate the weight of a numerical probability
-- | by aprroximately multiplying by a factor raised to the value of the outcome
-- | making higer numbers exponentially less likely than smaller ones
-- | This avoids infinite size AST
attenuateNum :: Double -> NumberProbability -> NumberProbability
attenuateNum d p =
  if d == 1
    then p
    else case p of
      NPUniform l h ->
        NPDiscrete $
          if d == 0 then [(1, l)] else NE.fromList (zipWith mkdistrfor [1 ..] [l .. h])
      NPBinomial off t p ->
        if d == 0
          then NPDiscrete [(1, off)]
          else NPBinomial off t $ p * d
      NPNegativeBinomial off pf f ->
        if d == 0
          then NPDiscrete [(1, off)]
          else NPNegativeBinomial off (1 - (1 - pf) * d) f
      NPPoisson off p ->
        if d == 0
          then NPDiscrete [(1, off)]
          else NPPoisson off $ p * d
      NPDiscrete l -> NPDiscrete $ if d == 0 then [NE.head l] else NE.map (uncurry mkdistrfor) l
      NPLinearComb l -> NPLinearComb $ NE.map (second $ attenuateNum d) l
  where
    mkdistrfor bw n = (bw * d ** fromIntegral n, n)

type GenM' = GenM GarbageOpts

-- | Apply an attenuation multiplier to avoid infinitely deep recursion
applyAttenuation :: Int -> GarbageAttenuationOpts -> GarbageAttenuationOpts
applyAttenuation n x = x & gaoCurrent *~ _gaoDecrease x ** fromIntegral n

tameExprRecursion :: Int -> GenM' a -> GenM' a
tameExprRecursion n = local (_1 . goExpr . geoAttenuation %~ applyAttenuation n)

repeatExprRecursive :: (GarbageOpts -> NumberProbability) -> GenM' a -> GenM' [a]
repeatExprRecursive p m = do
  n <- sampleAttenuatedNum (_geoAttenuation . _goExpr) p
  tameExprRecursion n $ replicateM n m

tameStmtRecursion :: Int -> GenM' a -> GenM' a
tameStmtRecursion n = local (_1 . goStatement . gstoAttenuation %~ applyAttenuation n)

repeatStmtRecursive :: (GarbageOpts -> NumberProbability) -> GenM' a -> GenM' [a]
repeatStmtRecursive p m = do
  n <- sampleAttenuatedNum (_gstoAttenuation . _goStatement) p
  tameStmtRecursion n $ replicateM n m

tameModGenRecursion :: Int -> GenM' a -> GenM' a
tameModGenRecursion n = local (_1 . goGenerate . ggoAttenuation %~ applyAttenuation n)

repeatModGenRecursive :: (GarbageOpts -> NumberProbability) -> GenM' a -> GenM' [a]
repeatModGenRecursive p m = do
  n <- sampleAttenuatedNum (_ggoAttenuation . _goGenerate) p
  tameModGenRecursion n $ replicateM n m

-- | Branching with attenuation
sampleAttenuatedBranch ::
  (GarbageOpts -> GarbageAttenuationOpts)
  -> (GarbageOpts -> CategoricalProbability)
  -> (NonEmpty (Bool, GenM' a))
  -> GenM' a
sampleAttenuatedBranch f p l = do
  gen <- asks snd
  d <- asks $ p . fst
  a <- asks $ _gaoCurrent . f . fst
  join $ sampleIn (toList $ NE.map snd l) gen (attenuateCat l a d)

-- | Number with attenuation
sampleAttenuatedNum ::
  (GarbageOpts -> GarbageAttenuationOpts) -> (GarbageOpts -> NumberProbability) -> GenM' Int
sampleAttenuatedNum f p = do
  gen <- asks snd
  d <- asks $ p . fst
  a <- asks $ _gaoCurrent . f . fst
  sampleNumberProbability gen $ attenuateNum a d

-- | Letters available for simple identifiers
idSimpleLetter :: B.ByteString -- 0-9$ are forbidden as first letters
idSimpleLetter = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789$"

digitCharacter :: B.ByteString
digitCharacter = "0123456789"

-- Start of actual generation

garbageSimpleBS :: GenM' B.ByteString
garbageSimpleBS =
  avoidKW <$> sampleFromString (i _gioSimpleLetter) (B.take 53 idSimpleLetter)
    <.> sampleString (i _gioSimpleLetters) (i _gioSimpleLetter) idSimpleLetter
  where
    i x = x . _goIdentifier
    avoidKW fl t =
      let s = B.cons fl t
       in if isKW s
            then do
              x <- sampleFromString (i _gioSimpleLetter) idSimpleLetter
              avoidKW fl $ B.cons x t
            else return s

garbageEscapedBS :: GenM' B.ByteString
garbageEscapedBS =
  B.pack <$> sampleN (i _gioEscapedLetters) (toEnum <$> sampleSegment (i _gioEscapedLetter) 33 126)
  where i x = x . _goIdentifier

garbageBS :: GenM' B.ByteString
garbageBS = choice (_gioEscaped_Simple . _goIdentifier) garbageEscapedBS garbageSimpleBS

garbageIdent :: GenM' Identifier
garbageIdent = Identifier <$> garbageBS

garbageIdentified :: GenM' x -> GenM' (Identified x)
garbageIdentified = liftA2 Identified garbageIdent

garbageSysIdent :: GenM' B.ByteString
garbageSysIdent =
  B.cons <$> sampleFromString (i _gioSystemFirstLetter) idSimpleLetter
    <*> sampleString (i _gioSystemLetters) (i _gioSimpleLetter) idSimpleLetter
  where i x = x . _goIdentifier

garbageHierIdent :: GenM' HierIdent
garbageHierIdent = do
  hip <- repeatExprRecursive _goPathDepth $
    mkpair garbageIdent $ sampleMaybe (_geoDimRange . _goExpr) garbageCExpr
  HierIdent hip <$> garbageIdent

garbageInteger :: GenM' Natural
garbageInteger =
  parseDecimal <$> sampleString (e _geoDecimalSymbols) (e _geoDecimalSymbol) digitCharacter
  where e x = x . _goExpr

garbageReal :: GenM' B.ByteString
garbageReal =
  choice
    (e _geoFixed_Floating)
    ( do
        p <- number
        f <- number
        return $ p <> "." <> f
    )
    ( do
        p <- number
        f <- sampleString (e _geoDecimalSymbols) (e _geoDecimalSymbol) digitCharacter
        s <- sampleFrom (e _geoExponentSign) ["", "+", "-"]
        e <- number
        return $ p <> (if B.null f then "" else B.cons (c2w '.') f) <> "e" <> s <> e
    )
  where
    e x = x . _goExpr
    number = sampleNEString (e _geoDecimalSymbols) (e _geoDecimalSymbol) digitCharacter

garbageNumIdent :: GenM' NumIdent
garbageNumIdent =
  sampleBranch
    _goIntRealIdent
    [ NINumber <$> garbageInteger,
      NIReal <$> garbageReal,
      NIIdent <$> garbageIdent
    ]

garbagePrim :: GenM' i -> Bool -> GenM' r -> GenM' a -> GenM' (GenPrim i r a)
garbagePrim ident attrng grng gattr =
  sampleAttenuatedBranch
    (e _geoAttenuation)
    (e _geoPrimary)
    [ mknum $ NXZ <$> sampleBernoulli (e _geoX_Z),
      mknum $ NBinary <$> sampleNE (e _geoBinarySymbols) (sampleEnum $ e _geoBinarySymbol),
      mknum $ NOctal <$> sampleNE (e _geoOctalSymbols) (sampleEnum $ e _geoOctalSymbol),
      mknum $ NDecimal <$> garbageInteger,
      mknum $ NHex <$> sampleNE (e _geoHexadecimalSymbols) (sampleEnum $ e _geoHexadecimalSymbol),
      (False, PrimReal <$> garbageReal),
      ( False,
        PrimString . makeString . map w2c
          <$> sampleN (e _geoStringCharacters) (sampleEnum $ e _geoStringCharacter)
      ),
      (attrng, PrimIdent <$> ident <*> grng),
      ( True,
        do
          n <- succ <$> sNum (e _geoConcatenations)
          PrimConcat . NE.fromList <$> tameExprRecursion n (replicateM n gexpr)
      ),
      ( True,
        do
          n <- succ <$> sNum (e _geoConcatenations)
          tameExprRecursion (n + 1) $
            PrimMultConcat <$> garbageGenExpr
                garbageIdent
                True
                (sampleMaybe (_geoDimRange . _goExpr) garbageCRangeExpr)
                gattr
              <*> fmap NE.fromList (replicateM n gexpr)
      ),
      ( True,
        do
          n <- succ <$> sNum (_ggoTaskFunPorts . _goGenerate)
          tameExprRecursion n $ PrimFun <$> ident <*> gattr <*> replicateM n gexpr
      ),
      (True, PrimSysFun <$> garbageSysIdent <*> repeatExprRecursive (e _geoSysFunArgs) gexpr),
      (True, PrimMinTypMax <$> garbageGenMinTypMax gexpr)
    ]
  where
    e x = x . _goExpr
    sNum = sampleAttenuatedNum (e _geoAttenuation)
    mknum x =
      ( False,
        do
          sz <- sampleSegment (e _geoLiteralWidth) 0 65535
          sn <- sampleBernoulli (e _geoLiteralSigned)
          PrimNumber (if sz == 0 then Nothing else Just (toEnum sz)) sn <$> x
      )
    gexpr = garbageGenExpr ident attrng grng gattr

garbageGenExpr :: GenM' i -> Bool -> GenM' r -> GenM' a -> GenM' (GenExpr i r a)
garbageGenExpr ident attrng grng gattr =
  sampleAttenuatedBranch
    (e _geoAttenuation)
    (e _geoItem)
    [ (False, ExprPrim <$> garbagePrim ident attrng grng gattr),
      ( False,
        ExprUnOp <$> sampleEnum (e _geoUnary) <*> gattr <*> garbagePrim ident attrng grng gattr
      ),
      ( True,
        tameExprRecursion 2 $ ExprBinOp <$> gexpr <*> sampleEnum (e _geoBinary) <*> gattr <*> gexpr
      ),
      (True, tameExprRecursion 3 $ ExprCond <$> gexpr <*> gattr <*> gexpr <*> gexpr)
    ]
  where
    e x = x . _goExpr
    gexpr = garbageGenExpr ident attrng grng gattr

garbageGenMinTypMax :: GenM' e -> GenM' (GenMinTypMax e)
garbageGenMinTypMax gexpr =
  choice
    (_geoMinTypMax . _goExpr)
    (tameExprRecursion 3 $ MTMFull <$> gexpr <*> gexpr <*> gexpr)
    (tameExprRecursion 1 $ MTMSingle <$> gexpr)

garbageRange2 :: GenM' Range2
garbageRange2 = tameExprRecursion 2 $ Range2 <$> garbageCExpr <*> garbageCExpr

garbageDims :: GenM' [Range2]
garbageDims = repeatExprRecursive (_gtoDimensions . _goType) garbageRange2

garbageGenRangeExpr :: GenM' e -> GenM' (GenRangeExpr e)
garbageGenRangeExpr ge =
  sampleBranch
    (e _geoRange)
    [ GRESingle <$> ge,
      GREPair <$> garbageRange2,
      tameExprRecursion 2 $
        GREBaseOff <$> ge <*> sampleBernoulli (e _geoRangeOffsetPos_Neg) <*> garbageCExpr
    ]
  where e x = x . _goExpr

garbageGenDimRange :: GenM' e -> GenM' (GenDimRange e)
garbageGenDimRange ge = do
  n <- sampleAttenuatedNum (_geoAttenuation . _goExpr) (_gtoDimensions . _goType)
  tameExprRecursion (n + 1) $ GenDimRange <$> replicateM n ge <*> garbageGenRangeExpr ge

garbageExpr :: GenM' Expr
garbageExpr =
  Expr <$> garbageGenExpr
    garbageHierIdent
    True
    (sampleMaybe (_geoDimRange . _goExpr) garbageDimRange)
    garbageAttributes

garbageCExpr :: GenM' CExpr
garbageCExpr =
  CExpr <$> garbageGenExpr
    garbageIdent
    True
    (sampleMaybe (_geoDimRange . _goExpr) garbageCRangeExpr)
    garbageAttributes

garbageRangeExpr :: GenM' RangeExpr
garbageRangeExpr = garbageGenRangeExpr garbageExpr

garbageCRangeExpr :: GenM' CRangeExpr
garbageCRangeExpr = garbageGenRangeExpr garbageCExpr

garbageDimRange :: GenM' DimRange
garbageDimRange = garbageGenDimRange garbageExpr

garbageCDimRange :: GenM' CDimRange
garbageCDimRange = garbageGenDimRange garbageCExpr

garbageMinTypMax :: GenM' MinTypMax
garbageMinTypMax = garbageGenMinTypMax garbageExpr

garbageCMinTypMax :: GenM' CMinTypMax
garbageCMinTypMax = garbageGenMinTypMax garbageCExpr

garbageBareCMTM :: GenM' CMinTypMax
garbageBareCMTM =
  choice
    (_goBareMinTypMax)
    (MTMFull <$> garbageCExpr <*> garbageCExpr <*> garbageCExpr)
    (MTMSingle <$> garbageCExpr)

garbageAttributes :: GenM' Attributes
garbageAttributes =
  repeatExprRecursive _goAttributes $
    repeatExprRecursive _goAttributes $
      Attribute <$> garbageBS <*> sampleMaybe _goAttributeOptionalValue gattr
  where
    gattr =
      garbageGenExpr
        garbageIdent
        True
        (sampleMaybe (_geoDimRange . _goExpr) garbageCRangeExpr)
        (pure ())

garbageAttributed :: GenM' x -> GenM' (Attributed x)
garbageAttributed = liftA2 Attributed garbageAttributes

garbageAttrIded :: GenM' x -> GenM' (AttrIded x)
garbageAttrIded = liftA3 AttrIded garbageAttributes garbageIdent

garbageDelay1 :: GenM' Delay1
garbageDelay1 =
  sampleBranch
    _goDelay
    [ D1Base <$> garbageNumIdent,
      D11 <$> garbageMinTypMax
    ]

garbageDelay2 :: GenM' Delay2
garbageDelay2 =
  sampleBranch
    _goDelay
    [ D2Base <$> garbageNumIdent,
      D21 <$> garbageMinTypMax,
      D22 <$> garbageMinTypMax <*> garbageMinTypMax
    ]

garbageDelay3 :: GenM' Delay3
garbageDelay3 =
  sampleBranch
    _goDelay
    [ D3Base <$> garbageNumIdent,
      D31 <$> garbageMinTypMax,
      D32 <$> garbageMinTypMax <*> garbageMinTypMax,
      D33 <$> garbageMinTypMax <*> garbageMinTypMax <*> garbageMinTypMax
    ]

garbageLValue :: GenM' dr -> GenM' (LValue dr)
garbageLValue gdr = do
  l <- repeatExprRecursive _goLValues $ garbageLValue gdr
  case l of
    [] -> LVSingle <$> garbageHierIdent <*> sampleMaybe _goOptionalLValue gdr
    h : t -> return $ LVConcat $ h :| t

garbageNetLV :: GenM' NetLValue
garbageNetLV = garbageLValue garbageCDimRange

garbageVarLV :: GenM' VarLValue
garbageVarLV = garbageLValue garbageDimRange

garbageVarAssign :: GenM' VarAssign
garbageVarAssign = Assign <$> garbageVarLV <*> garbageExpr

garbageNetAssign :: GenM' NetAssign
garbageNetAssign = Assign <$> garbageNetLV <*> garbageExpr

garbageEvCtl :: GenM' EventControl
garbageEvCtl =
  sampleBranch
    (s _gstoEvent)
    [ pure ECDeps,
      ECIdent <$> garbageHierIdent,
      ECExpr <$> sampleNE
        (s _gstoEvents)
        (EventPrim <$> sampleEnum (s _gstoEventPrefix) <*> garbageExpr)
    ]
  where s x = x . _goStatement

garbageDelEvCtl :: GenM' DelayEventControl
garbageDelEvCtl =
  sampleBranch
    (_gstoDelayEventRepeat . _goStatement)
    [ DECDelay <$> garbageDelay1,
      DECEvent <$> garbageEvCtl,
      DECRepeat <$> garbageExpr <*> garbageEvCtl
    ]

garbageLoopStatement :: GenM' LoopStatement
garbageLoopStatement =
  sampleBranch
    (_gstoLoop . _goStatement)
    [ pure LSForever,
      LSRepeat <$> garbageExpr,
      LSWhile <$> garbageExpr,
      LSFor <$> garbageVarAssign <*> garbageExpr <*> garbageVarAssign
    ]

garbageStmtBlockHeader :: GenM' (Maybe (Identifier, [AttrIded StdBlockDecl]))
garbageStmtBlockHeader =
  sampleMaybe (s _gstoBlockHeader) $
    mkpair garbageIdent $
      sampleN (s _gstoBlockDecls) $
        garbageAttrIded $ sampleBranch (s _gstoBlockDecl) stdBlockDeclList
  where
    s x = x . _goStatement

garbageFunctionStatement :: GenM' FunctionStatement
garbageFunctionStatement =
  sampleAttenuatedBranch
    (s _gstoAttenuation)
    (s _gstoItem)
    [ (False, FSBlockAssign <$> garbageVarAssign),
      ( True,
        do
          x <- sampleEnum $ s _gstoCase
          e <- garbageExpr 
          pn <- sampleAttenuatedNum (s _gstoAttenuation) (s _gstoCaseBranches)
          d <- tameStmtRecursion pn gmybfstmt
          let n = if d == Attributed [] Nothing then pn + 1 else pn
          c <-
            tameStmtRecursion n $
              replicateM n $
                FCaseItem <$> sampleNE (s _gstoCaseBranchPatterns) garbageExpr <*> gmybfstmt
          return $ FSCase x e c d
      ),
      (True, tameStmtRecursion 2 $ FSIf <$> garbageExpr <*> gmybfstmt <*> gmybfstmt),
      (False, FSDisable <$> garbageHierIdent),
      (True, FSLoop <$> garbageLoopStatement <*> tameStmtRecursion 1 gattrfstmt),
      ( True,
        FSBlock <$> garbageStmtBlockHeader
          <*> sampleBernoulli (s _gstoBlockPar_Seq)
          <*> repeatStmtRecursive (s _gstoItems) gattrfstmt
      )
    ]
  where
    s x = x . _goStatement
    gmybfstmt = garbageAttributed $ sampleMaybe (s _gstoOptional) garbageFunctionStatement
    gattrfstmt = garbageAttributed garbageFunctionStatement

garbageStatement :: GenM' Statement
garbageStatement =
  sampleAttenuatedBranch
    (s _gstoAttenuation)
    (s _gstoItem)
    [ ( False,
        SBlockAssign <$> sampleBernoulli (s _gstoAssignmentBlocking)
          <*> garbageVarAssign
          <*> sampleMaybe (s _gstoOptionalDelEvCtl) garbageDelEvCtl
      ),
      ( True,
        do
          x <- sampleEnum $ s _gstoCase
          e <- garbageExpr 
          pn <- sampleAttenuatedNum (s _gstoAttenuation) (s _gstoCaseBranches)
          d <- tameStmtRecursion pn garbageMybStmt
          let n = if d == Attributed [] Nothing then pn + 1 else pn
          c <-
            tameStmtRecursion n $
              replicateM n $
                CaseItem <$> sampleNE (s _gstoCaseBranchPatterns) garbageExpr <*> garbageMybStmt
          return $ SCase x e c d
      ),
      (True, tameStmtRecursion 2 $ SIf <$> garbageExpr <*> garbageMybStmt <*> garbageMybStmt),
      (False, SDisable <$> garbageHierIdent),
      (True, SLoop <$> garbageLoopStatement <*> tameStmtRecursion 1 garbageAttrStmt),
      ( True,
        SBlock <$> garbageStmtBlockHeader
          <*> sampleBernoulli (s _gstoBlockPar_Seq)
          <*> repeatStmtRecursive (s _gstoItems) garbageAttrStmt
      ),
      ( False,
        SEventTrigger <$> garbageHierIdent <*> sampleN (_gtoDimensions . _goType) garbageExpr
      ),
      ( False,
        SProcContAssign <$> sampleBranch
            (s _gstoProcContAssign)
            [ PCAAssign <$> garbageVarAssign,
              PCADeassign <$> garbageVarLV,
              PCAForce <$> sampleEither (s _gstoPCAVar_Net) garbageVarAssign garbageNetAssign,
              PCARelease <$> sampleEither (s _gstoPCAVar_Net) garbageVarLV garbageNetLV
            ]
      ),
      ( True,
        SProcTimingControl <$> sampleBranch
            (s _gstoDelayEventRepeat)
            [Left <$> garbageDelay1, Right <$> garbageEvCtl]
          <*> tameStmtRecursion 1 garbageMybStmt
      ),
      ( False,
        SSysTaskEnable <$> garbageSysIdent
          <*> sampleN (s _gstoSysTaskPorts) (sampleMaybe (s _gstoSysTaskOptionalPort) garbageExpr)
      ),
      ( False,
        STaskEnable <$> garbageHierIdent <*> sampleN (_ggoTaskFunPorts . _goGenerate) garbageExpr
      ),
      (True, SWait <$> garbageExpr <*> tameStmtRecursion 1 garbageMybStmt)
    ]
  where s x = x . _goStatement

garbageMybStmt :: GenM' MybStmt
garbageMybStmt = garbageAttributed $ sampleMaybe (_gstoOptional . _goStatement) garbageStatement

garbageAttrStmt :: GenM' AttrStmt
garbageAttrStmt = garbageAttributed garbageStatement

garbageSR :: GenM' SignRange
garbageSR =
  SignRange <$> sampleBernoulli (t _gtoConcreteSignedness)
    <*> sampleMaybe (t _gtoConcreteBitRange) garbageRange2
  where t x = x . _goType

garbageComType :: GenM' x -> GenM' (ComType x)
garbageComType m =
  choice
    (t _gtoAbstract_Concrete)
    (CTAbstract <$> sampleEnum (t _gtoAbstract))
    (CTConcrete <$> m <*> garbageSR)
  where t x = x . _goType

garbageParameter :: GenM' Parameter
garbageParameter = Parameter <$> (garbageComType $ pure ()) <*> garbageBareCMTM

blockDeclList :: (forall x. GenM' x -> GenM' (f x)) -> GenM' t -> [GenM' (BlockDecl f t)]
blockDeclList f m =
  [ BDReg <$> garbageSR <*> f m,
    BDInt <$> f m,
    BDReal <$> f m,
    BDTime <$> f m,
    BDRealTime <$> f m,
    BDEvent <$> f garbageDims,
    BDLocalParam <$> (garbageComType $ pure ()) <*> f garbageBareCMTM
  ]

stdBlockDeclList :: [GenM' StdBlockDecl]
stdBlockDeclList =
  map (fmap SBDBlockDecl) (blockDeclList (fmap Identity) garbageDims)
  ++ [SBDParameter <$> garbageParameter]

garbageDriveStrength :: GenM' DriveStrength
garbageDriveStrength = do
  x <- strall
  y <- strall
  case (x, y) of
    (Just a, Just b) -> return $ DSNormal a b
    (Nothing, Just b) -> return $ DSHighZ False b
    (Just a, Nothing) -> return $ DSHighZ True a
    _ -> garbageDriveStrength
  where strall = sampleMaybeEnum _goDriveStrength

garbageTFBlockDecl :: GenM' x -> GenM' (TFBlockDecl x)
garbageTFBlockDecl m =
  sampleBranch (g _ggoTaskFunDecl) $ map (fmap TFBDStd) stdBlockDeclList ++
    [TFBDPort <$> m <*> garbageComType (sampleBernoulli $ g _ggoTaskFunRegister)]
  where g x = x . _goGenerate

garbageInstanceName :: GenM' InstanceName
garbageInstanceName =
  InstanceName <$> garbageIdent <*> sampleMaybe (_ggoInstOptionalRange . _goGenerate) garbageRange2

garbageGateInst :: (forall x. GenM' x -> GenM' (f x)) -> GenM' (ModGenItem f)
garbageGateInst f =
  sampleBranch
    (g _ggoGateInst)
    [ mkf (MGICMos False <$> optd3) $
        GICMos <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr <*> garbageExpr,
      mkf (MGICMos True <$> optd3) $
        GICMos <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr <*> garbageExpr,
      mkf (MGIEnable False False <$> garbageDriveStrength <*> optd3) $
        GIEnable <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
      mkf (MGIEnable False True <$> garbageDriveStrength <*> optd3) $
        GIEnable <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
      mkf (MGIEnable True False <$> garbageDriveStrength <*> optd3) $
        GIEnable <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
      mkf (MGIEnable True True <$> garbageDriveStrength <*> optd3) $
        GIEnable <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
      mkf (MGIMos False False <$> optd3) $
        GIMos <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
      mkf (MGIMos False True <$> optd3) $
        GIMos <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
      mkf (MGIMos True False <$> optd3) $
        GIMos <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
      mkf (MGIMos True True <$> optd3) $
        GIMos <$> optname <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
      mkf
        (flip MGINIn False <$> sampleEnum (g _ggoGateNInputType) <*> garbageDriveStrength <*> optd2)
        (GINIn <$> optname <*> garbageNetLV <*> sampleNE (g _ggoGateInputs) garbageExpr),
      mkf
        (flip MGINIn True <$> sampleEnum (g _ggoGateNInputType) <*> garbageDriveStrength <*> optd2)
        (GINIn <$> optname <*> garbageNetLV <*> sampleNE (g _ggoGateInputs) garbageExpr),
      mkf (MGINOut False <$> garbageDriveStrength <*> optd2) $
        GINOut <$> optname <*> sampleNE (g _ggoGateOutputs) garbageNetLV <*> garbageExpr,
      mkf (MGINOut True <$> garbageDriveStrength <*> optd2) $
        GINOut <$> optname <*> sampleNE (g _ggoGateOutputs) garbageNetLV <*> garbageExpr,
      mkf (MGIPassEn False False <$> optd2) $
        GIPassEn <$> optname <*> garbageNetLV <*> garbageNetLV <*> garbageExpr,
      mkf (MGIPassEn False True <$> optd2) $
        GIPassEn <$> optname <*> garbageNetLV <*> garbageNetLV <*> garbageExpr,
      mkf (MGIPassEn True False <$> optd2) $
        GIPassEn <$> optname <*> garbageNetLV <*> garbageNetLV <*> garbageExpr,
      mkf (MGIPassEn True True <$> optd2) $
        GIPassEn <$> optname <*> garbageNetLV <*> garbageNetLV <*> garbageExpr,
      mkf (pure $ MGIPass False) $ GIPass <$> optname <*> garbageNetLV <*> garbageNetLV,
      mkf (pure $ MGIPass True) $ GIPass <$> optname <*> garbageNetLV <*> garbageNetLV,
      mkf (MGIPull False <$> garbageDriveStrength) $ GIPull <$> optname <*> garbageNetLV,
      mkf (MGIPull True <$> garbageDriveStrength) $ GIPull <$> optname <*> garbageNetLV
    ]
  where
    g x = x . _goGenerate
    mkf c m = c <*> f m
    optname = sampleMaybe (g _ggoGateOptIdent) garbageInstanceName
    optd3 = sampleMaybe (g _ggoInstOptionalDelay) garbageDelay3
    optd2 = sampleMaybe (g _ggoInstOptionalDelay) garbageDelay2

garbageGenIf :: GenM' ModGenCondItem
garbageGenIf =
  tameModGenRecursion 2 $ MGCIIf <$> garbageCExpr <*> garbageGenCondBlock <*> garbageGenCondBlock

garbageGenCase :: GenM' ModGenCondItem
garbageGenCase = do
  e <- garbageCExpr
  pn <- sampleAttenuatedNum (g _ggoAttenuation) (g _ggoCaseBranches)
  d <- tameModGenRecursion pn garbageGenCondBlock
  let n = if d == GCBEmpty then pn + 1 else pn
  c <-
    tameModGenRecursion n $
      replicateM n $
        GenCaseItem <$> sampleNE (g _ggoCaseBranchPatterns) garbageCExpr <*> garbageGenCondBlock
  return $ MGCICase e c d
  where g x = x . _goGenerate

-- do not generate unknown instantiations, there is no need to
garbageModGenItem :: (forall x. GenM' x -> GenM' (f x)) -> GenM' (ModGenItem f)
garbageModGenItem f =
  sampleAttenuatedBranch
    (g _ggoAttenuation)
    (g _ggoItem)
    [ ( False,
        MGINetInit <$> sampleEnum (g _ggoNetType) <*> garbageDriveStrength <*> gnetprop <*> f gnetinit
      ),
      (False, MGINetDecl <$> sampleEnum (g _ggoNetType) <*> gnetprop <*> f gnetdecl),
      (False, MGITriD <$> garbageDriveStrength <*> gnetprop <*> f gnetinit),
      (False, MGITriC <$> sampleEnum (g _ggoChargeStrength) <*> gnetprop <*> f gnetdecl),
      ( False,
        MGIBlockDecl <$> sampleBranch (g _ggoDeclItem)
          (blockDeclList
            (fmap Compose . f . garbageIdentified)
            (sampleEither (g _ggoDeclDim_Init) garbageDims garbageCExpr))
      ),
      (False, MGIGenVar <$> f garbageIdent),
      ( False,
        MGITask <$> sampleBernoulli (g _ggoTaskFunAutomatic)
          <*> garbageIdent
          <*> sampleN
            (g _ggoTaskFunPorts)
            (garbageAttrIded $ garbageTFBlockDecl $ sampleEnum $ g _ggoTaskPortDirection)
          <*> garbageMybStmt
      ),
      ( False,
        MGIFunc <$> sampleBernoulli (g _ggoTaskFunAutomatic)
          <*> sampleMaybe (g _ggoFunRetType) (garbageComType $ pure ())
          <*> garbageIdent
          <*> (toList
            <$> sampleNE (g _ggoTaskFunPorts) (garbageAttrIded $ garbageTFBlockDecl $ pure ()))
          <*> garbageFunctionStatement
      ),
-- TODO MAYBE: make a BareCMinTypMax and use it here
      (False, MGIDefParam <$> f (ParamOver <$> garbageHierIdent <*> garbageCMinTypMax)),
      (False, MGIContAss <$> garbageDriveStrength <*> optd3 <*> f garbageNetAssign),
      (False, garbageGateInst f),
      ( False,
        MGIUDPInst <$> garbageIdent
          <*> garbageDriveStrength
          <*> optd2
          <*> f
            ( UDPInst
              <$> sampleMaybe (g _ggoPrimitiveOptIdent) garbageInstanceName
              <*> garbageNetLV
              <*> sampleNE (_gpoPorts . _goPrimitive) garbageExpr
            )
      ),
      ( False,
        MGIModInst <$> garbageIdent
          <*> choice
            (m _gmoNamed_Positional)
            ( ParamNamed <$> sampleN
                  (m _gmoParameters)
-- TODO MAYBE: make a BareCMinTypMax and use it here
                  (garbageIdentified $ sampleMaybe (m _gmoOptionalParameter) garbageMinTypMax)
            )
            (ParamPositional <$> sampleN (m _gmoParameters) garbageExpr)
          <*> f (ModInst <$> garbageInstanceName
            <*> choice
              (m _gmoNamed_Positional)
              (PortNamed <$> sampleN (m _gmoPorts) (garbageAttrIded optexpr))
              (PortPositional <$> sampleN (m _gmoPorts) (garbageAttributed optexpr)))
      ),
      (False, MGIInitial <$> garbageAttrStmt),
      (False, MGIAlways <$> garbageAttrStmt),
      ( True,
        MGILoopGen <$> garbageIdent
          <*> garbageCExpr
          <*> garbageCExpr
          <*> garbageIdent
          <*> garbageCExpr
          <*> garbageGenerateBlock
      ),
      (True, MGICondItem <$> garbageGenIf),
      (True, MGICondItem <$> garbageGenCase)
    ]
  where
    g x = x . _goGenerate
    m x = x . _goModule
    optd3 = sampleMaybe (g _ggoInstOptionalDelay) garbageDelay3
    optd2 = sampleMaybe (g _ggoInstOptionalDelay) garbageDelay2
    optexpr = sampleMaybe (m _gmoOptionalPort) garbageExpr
    optblock = sampleMaybe (g _ggoOptionalBlock) garbageGenerateBlock
    gnetprop = NetProp <$> sampleBernoulli (_gtoConcreteSignedness . _goType)
      <*> sampleMaybe (g _ggoNetRange)
        (mkpair (sampleMaybeEnum $ g _ggoNetVectoring) garbageRange2)
      <*> optd3
    gnetdecl = NetDecl <$> garbageIdent <*> garbageDims
    gnetinit = NetInit <$> garbageIdent <*> garbageExpr

garbageModGenBlockedItem :: GenM' (Attributed ModGenBlockedItem)
garbageModGenBlockedItem = garbageAttributed $ garbageModGenItem $ fmap Identity

garbageGenerateBlock :: GenM' GenerateBlock
garbageGenerateBlock =
  garbageIdentified $ repeatModGenRecursive (_ggoItems . _goGenerate) $ garbageModGenBlockedItem

garbageGenCondBlock :: GenM' GenerateCondBlock
garbageGenCondBlock =
  sampleAttenuatedBranch
    (g _ggoAttenuation)
    (g _ggoCondBlock)
    [ (False, pure GCBEmpty),
      (True, GCBBlock <$> garbageGenerateBlock),
      (True, GCBConditional <$> garbageAttributed garbageGenIf),
      (True, GCBConditional <$> garbageAttributed garbageGenCase)
    ]
  where g x = x . _goGenerate

garbageSpecTerm :: GenM' SpecTerm
garbageSpecTerm =
  SpecTerm <$> garbageIdent <*> sampleMaybe (_gsyoTermRange . _goSpecify) garbageCRangeExpr

garbagePPIdentifier :: GenM' Identifier
garbagePPIdentifier =
  Identifier <$> choice (_gsyoPathPulseEscaped_Simple . _goSpecify) garbageEscapedBS garbageSimpleBS

garbagePPTerm :: GenM' SpecTerm
garbagePPTerm =
  SpecTerm <$> garbagePPIdentifier
    <*> sampleMaybe (_gsyoPathPulseRange . _goSpecify) garbageCRangeExpr

garbageSPRange :: GenM' (Maybe Range2)
garbageSPRange = sampleMaybe (_gsyoParamRange . _goSpecify) garbageRange2

garbageSpecParamAssign :: GenM' SpecParamDecl
garbageSpecParamAssign = SPDAssign <$> garbageIdent <*> garbageCMinTypMax

garbageNoPathPulse :: GenM' SpecParamDecl
garbageNoPathPulse = SPDPathPulse Nothing <$> garbageCMinTypMax <*> garbageCMinTypMax

garbagePathPulse :: GenM' SpecParamDecl
garbagePathPulse =
  SPDPathPulse . Just <$> mkpair garbagePPTerm garbagePPTerm
    <*> garbageCMinTypMax
    <*> garbageCMinTypMax

garbageSpecifyItem :: GenM' SpecifyBlockedItem
garbageSpecifyItem =
  sampleBranch
    (s _gsyoItem)
    [ SISpecParam <$> garbageSPRange <*> fmap Identity garbageSpecParamAssign,
      SISpecParam <$> garbageSPRange <*> fmap Identity garbageNoPathPulse,
      SISpecParam <$> garbageSPRange <*> fmap Identity garbagePathPulse,
      SIPulsestyleOnevent <$> gst,
      SIPulsestyleOndetect <$> gst,
      SIShowcancelled <$> gst,
      SINoshowcancelled <$> gst,
      do
        cond <- sampleBranch
          (p _gspoCondition)
          [ pure MPCNone,
            pure MPCAlways,
            MPCCond <$> garbageGenExpr garbageIdent False (pure ()) garbageAttributes
          ]
        conn <- choice
          (p _gspoFull_Parallel)
          ( SPFull <$> sampleNE (p _gspoFullSources) garbageSpecTerm
              <*> sampleNE (p _gspoFullDestinations) garbageSpecTerm
          )
          (SPParallel <$> garbageSpecTerm <*> garbageSpecTerm)
        pol <- sampleMaybeEnum $ p _gspoPolarity
        eds <- sampleMaybe (p _gspoEdgeSensitive) $
          mkpair garbageExpr $ sampleMaybeEnum $ p _gspoEdgeSensitivity
        pdv <- sampleBranch
          (p _gspoDelayKind)
          [ PDV1 <$> garbageCMinTypMax,
            PDV2 <$> garbageCMinTypMax <*> garbageCMinTypMax,
            PDV3 <$> garbageCMinTypMax <*> garbageCMinTypMax <*> garbageCMinTypMax,
            PDV6 <$> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax,
            PDV12 <$> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
              <*> garbageCMinTypMax
          ]
        return $ SIPathDeclaration cond conn pol eds pdv,
      SISetup <$> gstca,
      SIHold <$> gstca,
      SISetupHold <$> gstca <*> gstcaa,
      SIRecovery <$> gstca,
      SIRemoval <$> gstca,
      SIRecrem <$> gstca <*> gstcaa,
      SISkew <$> gstca,
      SITimeSkew <$> gstca <*> gmce <*> gmce,
      SIFullSkew <$> gstca <*> garbageExpr <*> gmce <*> gmce,
      SIPeriod <$> gctce <*> garbageExpr <*> sampleMaybe (t _gstcoOptionalArg) garbageIdent,
      do
        (me, i) <- choice (t _gstcoOptionalArg) (pure (Nothing, Nothing)) $
          mkpair (Just <$> garbageCExpr) $ sampleMaybe (t _gstcoOptionalArg) garbageIdent
        cre <- gctce
        tcl <- garbageExpr
        return $ SIWidth cre tcl me i,
      SINoChange <$> gtce
        <*> gtce
        <*> garbageMinTypMax
        <*> garbageMinTypMax
        <*> sampleMaybe (t _gstcoOptionalArg) garbageIdent
    ]
  where
    s x = x . _goSpecify
    p x = s $ x . _gsyoPath
    t x = s $ x . _gsyoTimingCheck
    gst = Identity <$> garbageSpecTerm
    gmce = sampleMaybe (t _gstcoOptionalArg) garbageCExpr
    gtcc = mkpair (sampleBernoulli $ t _gstcoCondNeg_Pos) garbageExpr
    ged = do
      v <- VU.replicateM 6 (sampleBernoulli $ t _gstcoEventEdge)
      return (if VU.or v then v else VU.replicate 6 True)
    gtce =
      TimingCheckEvent <$> sampleMaybe (t _gstcoEvent) ged
        <*> garbageSpecTerm
        <*> sampleMaybe (t _gstcoCondition) gtcc
    gctce =
      ControlledTimingCheckEvent <$> ged
        <*> garbageSpecTerm
        <*> sampleMaybe (t _gstcoCondition) gtcc
    gstca =
      STCArgs <$> gtce <*> gtce <*> garbageExpr <*> sampleMaybe (t _gstcoOptionalArg) garbageIdent
    gstcaa = STCAddArgs <$> garbageExpr <*> gmmtm <*> gmmtm <*> gde <*> gde
    gmmtm = sampleMaybe (t _gstcoOptionalArg) garbageMinTypMax
    gde = sampleMaybe (t _gstcoOptionalArg) $
      garbageIdentified $ sampleMaybe (t _gstcoDelayedMinTypMax) garbageCMinTypMax

garbageModuleBlock :: Bool -> GenM' ModuleBlock
garbageModuleBlock ts = do
  nah <- asks $ m _gmoNonAsciiHeader . fst
  header <- sampleN (m _gmoPorts) $
    if nah
    then 
      garbageIdentified $
        sampleN (m _gmoPortLValues) $
          garbageIdentified $ sampleMaybe (m _gmoPortRange) garbageCRangeExpr
    else (\i -> Identified i [Identified i Nothing]) <$> garbageIdent
  ModuleBlock <$> garbageAttributes
    <*> sampleBernoulli (m _gmoMacro)
    <*> garbageIdent
    <*> pure header
    <*> sampleN
      (m _gmoItems)
      ( sampleBranch
          (m _gmoItem)
          [ MIMGI <$> garbageModGenBlockedItem,
            MIPort <$> garbageAttrIded (mkpair (sampleEnum $ m _gmoPortDir) garbageSR),
            MIParameter <$> garbageAttrIded garbageParameter,
            MIGenReg <$> sampleN (_ggoItems . _goGenerate) garbageModGenBlockedItem,
            MISpecBlock <$> sampleN (_gsyoItems . _goSpecify) garbageSpecifyItem,
            MISpecParam <$> garbageAttributes <*> garbageSPRange <*> garbageSpecParamAssign,
            MISpecParam <$> garbageAttributes <*> garbageSPRange <*> garbageNoPathPulse,
            MISpecParam <$> garbageAttributes <*> garbageSPRange <*> garbagePathPulse
          ]
      )
    <*> (if ts then Just <$> mkpair gts gts else pure Nothing)
    <*> sampleBernoulli (m _gmoCell)
    <*> sampleMaybeEnum (m _gmoUnconnectedDrive)
    <*> sampleMaybeEnum (m _gmoDefaultNetType)
  where
    m x = x . _goModule
    gts = sampleSegment (m _gmoTimeMagnitude) (-15) 2

garbagePrimitiveBlock :: GenM' PrimitiveBlock
garbagePrimitiveBlock =
  PrimitiveBlock <$> garbageAttributes
    <*> garbageIdent
    <*> garbageIdent
    <*> sampleNE (p _gpoPorts) garbageIdent
    <*> sampleNE (p _gpoPorts) (garbageAttrIded $
      sampleBranch (p _gpoPortType)
        [ pure PPInput,
          pure PPOutput,
          pure PPReg,
          PPOutReg <$> sampleMaybe (p _gpoRegInit) garbageCExpr -- no sem
        ])
    <*> choice
      (p _gpoSeq_Comb)
      ( SeqTable
          <$> sampleMaybeEnum (p _gpoCombInit)
          <*> sampleNE (p _gpoTableRows) gseqrow
      )
      (CombTable <$> sampleNE (p _gpoTableRows) (CombRow <$> gnein <*> goutlv))
  where
    p x = x . _goPrimitive
    ginlv = sampleEnum $ p _gpoInLevel
    goutlv = sampleEnum $ p _gpoOutLevel
    gnein = sampleNE (p _gpoPorts) ginlv
    glin = sampleN (p _gpoPorts) ginlv
    gedgeseq =
      SISeq <$> glin
        <*> sampleBranch
          (p _gpoEdgeSimplePosNeg)
          [ EdgeDesc <$> ginlv <*> ginlv,
            pure $ EdgePos_neg True,
            pure $ EdgePos_neg False
          ]
        <*> glin
    gseqrow =
      SeqRow <$> choice (p _gpoEdgeSensitive) (SIComb <$> gnein) gedgeseq
        <*> ginlv
        <*> sampleMaybe (p _gpoOutputNoChange) goutlv

garbageVerilog2005 :: GenM' Verilog2005
garbageVerilog2005 =
  Verilog2005
    <$> (sampleBernoulli (m _gmoTimeScale) >>= sampleN (m _gmoBlocks) . garbageModuleBlock)
    <*> sampleN (_gpoBlocks . _goPrimitive) garbagePrimitiveBlock
    <*> sampleN
      (c _gcoBlocks)
      ( ConfigBlock <$> garbageIdent
          <*> sampleN (c _gcoDesigns) gdot1
          <*> sampleN
            (c _gcoItems)
            ( ConfigItem
                <$> choice
                  (c _gcoCell_Inst)
                  (CICell <$> gdot1)
                  (CIInst <$> sampleNE _goPathDepth garbageIdent)
                <*> choice
                  (c _gcoLiblist_Use)
                  (LLULiblist <$> glibs)
                  (LLUUse <$> gdot1 <*> sampleBernoulli (c _gcoConfig))
            )
          <*> glibs
      )
  where
    m x = x . _goModule
    c x = x . _goConfig
    glibs = sampleN (c _gcoLibraries) garbageBS
    gdot1 = Dot1Ident <$> sampleMaybe (c _gcoLibraryScope) garbageBS <*> garbageIdent

runGarbageGeneration :: Config -> IO Verilog2005
runGarbageGeneration c = do
  let conf = _configGarbageGenerator c
  gen <- maybe createSystemRandom initialize $ _goSeed conf
  runReaderT garbageVerilog2005 (conf, gen)
