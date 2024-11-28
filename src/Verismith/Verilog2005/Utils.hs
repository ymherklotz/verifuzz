-- Module      : Verismith.Verilog2005.Utils
-- Description : AST utilitary functions.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

{-# LANGUAGE OverloadedLists #-}

module Verismith.Verilog2005.Utils
  ( makeIdent,
    regroup,
    addAttributed,
    genexprnumber,
    constifyIdent,
    constifyMaybeRange,
    trConstifyGenExpr,
    constifyExpr,
    constifyLV,
    expr2netlv,
    netlv2expr,
    toStatement,
    fromStatement,
    fromMybStmt,
    toMGIBlockDecl,
    fromMGIBlockDecl1,
    fromMGIBlockDecl_add,
    toStdBlockDecl,
    toSpecBlockedItem,
    fromSpecBlockedItem,
    toMGBlockedItem,
    fromMGBlockedItem1,
    fromMGBlockedItem_add,
    fromMGBlockedItem,
    resolveInsts
  )
where

import Control.Lens ((%~))
import Data.Data.Lens (biplate)
import Numeric.Natural
import Text.Printf (printf)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Function (on, (&))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w, packChars)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty (..), (<|), toList)
import qualified Data.List.NonEmpty as NE
import Verismith.Verilog2005.Lexer (VerilogVersion (..), isIdentSimple)
import Verismith.Verilog2005.AST
import Verismith.Utils (nonEmpty, foldrMap1)

-- AST utils

makeIdent :: BS.ByteString -> Identifier
makeIdent =
  Identifier . BS.concatMap
    (\w -> if 33 <= w && w <= 126 then BS.pack [w] else packChars $ printf "\\%02x" w)

-- | Groups `x`s into `y`s by converting a single `x` and merging previous `x`s to the result
regroup :: (x -> y) -> (x -> y -> Maybe y) -> NonEmpty x -> NonEmpty y
regroup mk add = foldrMap1 ((:|[]) . mk) (\e (h :| t) -> maybe (mk e :| h : t) (:| t) $ add e h)

-- | Merges `Attributed x`s if we can merge `x`s
addAttributed :: (x -> y -> Maybe y) -> Attributed x -> Attributed y -> Maybe (Attributed y)
addAttributed f (Attributed na x) (Attributed a y) =
  if a /= na then Nothing else Attributed a <$> f x y

-- | Makes a Verilog2005 expression out of a number
genexprnumber :: Natural -> GenExpr i r a
genexprnumber = ExprPrim . PrimNumber Nothing False . NDecimal

-- | converts HierIdent into Identifier
constifyIdent :: HierIdent -> Maybe Identifier
constifyIdent (HierIdent p i) = case p of [] -> Just i; _ -> Nothing

-- | the other way
unconstIdent :: Identifier -> HierIdent
unconstIdent = HierIdent []

-- | converts Prim into GenPrim i r
constifyGenPrim ::
  (si -> Maybe di) ->
  (Maybe DimRange -> Maybe r) ->
  GenPrim si (Maybe DimRange) a ->
  Maybe (GenPrim di r a)
constifyGenPrim fi fr x = case x of
  PrimNumber s b n -> Just $ PrimNumber s b n
  PrimReal s -> Just $ PrimReal s
  PrimIdent s rng -> PrimIdent <$> fi s <*> fr rng
  PrimConcat e -> PrimConcat <$> mapM ce e
  PrimMultConcat m e -> PrimMultConcat m <$> mapM ce e
  PrimFun s a e -> PrimFun <$> fi s <*> pure a <*> mapM ce e
  PrimSysFun s e -> PrimSysFun s <$> mapM ce e
  PrimMinTypMax (MTMSingle e) -> PrimMinTypMax . MTMSingle <$> ce e
  PrimMinTypMax (MTMFull l t h) -> PrimMinTypMax <$> (MTMFull <$> ce l <*> ce t <*> ce h)
  PrimString s -> Just $ PrimString s
  where
    ce = trConstifyGenExpr fi fr

-- | the other way
unconstPrim :: GenPrim Identifier (Maybe CRangeExpr) a -> GenPrim HierIdent (Maybe DimRange) a
unconstPrim x = case x of
  PrimNumber s b n -> PrimNumber s b n
  PrimReal s -> PrimReal s
  PrimIdent s rng -> PrimIdent (unconstIdent s) (GenDimRange [] . unconstRange <$> rng)
  PrimConcat e -> PrimConcat (NE.map trUnconstExpr e)
  PrimMultConcat m e -> PrimMultConcat m (NE.map trUnconstExpr e)
  PrimFun s a e -> PrimFun (unconstIdent s) a (map trUnconstExpr e)
  PrimSysFun s e -> PrimSysFun s (map trUnconstExpr e)
  PrimMinTypMax (MTMSingle e) -> PrimMinTypMax (MTMSingle (trUnconstExpr e))
  PrimMinTypMax (MTMFull l t h) ->
    PrimMinTypMax $ MTMFull (trUnconstExpr l) (trUnconstExpr t) (trUnconstExpr h)
  PrimString s -> PrimString s

-- | converts `GenExpr si (MaybeDimRange) a` into `GenExpr i r a`
trConstifyGenExpr ::
  (si -> Maybe di) ->
  (Maybe DimRange -> Maybe r) ->
  GenExpr si (Maybe DimRange) a ->
  Maybe (GenExpr di r a)
trConstifyGenExpr fi fr x = case x of
  ExprPrim p -> ExprPrim <$> constifyGenPrim fi fr p
  ExprUnOp op a p -> ExprUnOp op a <$> constifyGenPrim fi fr p
  ExprBinOp lhs op a rhs -> ExprBinOp <$> ce lhs <*> pure op <*> pure a <*> ce rhs
  ExprCond c a t f -> ExprCond <$> ce c <*> pure a <*> ce t <*> ce f
  where
    ce = trConstifyGenExpr fi fr

-- | converts Expr's `DimRange` into CExpr's `CRangeExpr`
constifyMaybeRange :: Maybe DimRange -> Maybe (Maybe CRangeExpr)
constifyMaybeRange =
  maybe (Just Nothing) $ \(GenDimRange l r) -> if null l then Just <$> constifyRange r else Nothing

-- | converts Expr's `GenExpr` into CExpr `GenExpr`
trConstifyExpr ::
  GenExpr HierIdent (Maybe DimRange) a -> Maybe (GenExpr Identifier (Maybe CRangeExpr) a)
trConstifyExpr = trConstifyGenExpr constifyIdent constifyMaybeRange

-- | the other way
trUnconstExpr :: GenExpr Identifier (Maybe CRangeExpr) a -> GenExpr HierIdent (Maybe DimRange) a
trUnconstExpr x = case x of
  ExprPrim p -> ExprPrim (unconstPrim p)
  ExprUnOp op a p -> ExprUnOp op a (unconstPrim p)
  ExprBinOp lhs op a rhs -> ExprBinOp (trUnconstExpr lhs) op a (trUnconstExpr rhs)
  ExprCond c a t f -> ExprCond (trUnconstExpr c) a (trUnconstExpr t) (trUnconstExpr f)

-- | converts Expr to CExpr
constifyExpr :: Expr -> Maybe CExpr
constifyExpr (Expr e) = CExpr <$> trConstifyExpr e

-- | the other way
unconstExpr :: CExpr -> Expr
unconstExpr (CExpr e) = Expr (trUnconstExpr e)

-- | converts RangeExpr into CRangeExpr
constifyRange :: RangeExpr -> Maybe CRangeExpr
constifyRange x = case x of
  GRESingle e -> GRESingle <$> constifyExpr e
  GREPair r2 -> Just $ GREPair r2
  GREBaseOff b mp o -> (\cb -> GREBaseOff cb mp o) <$> constifyExpr b

-- | the other way
unconstRange :: CRangeExpr -> RangeExpr
unconstRange x = case x of
  GRESingle e -> GRESingle (unconstExpr e)
  GREPair r2 -> GREPair r2
  GREBaseOff b mp o -> GREBaseOff (unconstExpr b) mp o

-- | converts DimRange into CDimRange
constifyDR :: GenDimRange Expr -> Maybe (GenDimRange CExpr)
constifyDR (GenDimRange dim rng) = GenDimRange <$> mapM constifyExpr dim <*> constifyRange rng

-- | the other way
unconstDR :: GenDimRange CExpr -> GenDimRange Expr
unconstDR (GenDimRange dim rng) = GenDimRange (map unconstExpr dim) (unconstRange rng)

-- | converts variable lvalue into net lvalue
constifyLV :: VarLValue -> Maybe NetLValue
constifyLV v = case v of
  LVSingle hi mdr -> LVSingle hi <$> maybe (Just Nothing) (fmap Just . constifyDR) mdr
  LVConcat l -> LVConcat <$> mapM constifyLV l

-- | the other way
unconstLV :: NetLValue -> VarLValue
unconstLV n = case n of
  LVSingle hi dr -> LVSingle hi (unconstDR <$> dr)
  LVConcat l -> LVConcat (NE.map unconstLV l)

-- | converts expression into net lvalue
expr2netlv :: Expr -> Maybe NetLValue
expr2netlv (Expr x) = aux x
  where
    aux x = case x of
      ExprPrim (PrimConcat c) -> LVConcat <$> mapM aux c
      ExprPrim (PrimIdent s sub) -> LVSingle s <$> maybe (Just Nothing) (Just . constifyDR) sub
      _ -> Nothing

-- | the other way
netlv2expr :: NetLValue -> Expr
netlv2expr = Expr . aux
  where
    aux x = case x of
      LVConcat e -> ExprPrim $ PrimConcat $ NE.map aux e
      LVSingle s dr -> ExprPrim $ PrimIdent s $ unconstDR <$> dr

-- | Converts Function statements to statements
toStatement :: FunctionStatement -> Statement
toStatement x = case x of
  FSBlockAssign va -> SBlockAssign True va Nothing
  FSCase zox e l d -> SCase zox e (map (\(FCaseItem p v) -> CaseItem p $ mybf v) l) (mybf d)
  FSIf e t f -> SIf e (mybf t) (mybf f)
  FSDisable hi -> SDisable hi
  FSLoop ls b -> SLoop ls $ toStatement <$> b
  FSBlock h ps b -> SBlock h ps $ fmap toStatement <$> b
  where mybf = fmap $ fmap toStatement

-- | the other way
fromStatement :: Statement -> Maybe FunctionStatement
fromStatement x = case x of
  SBlockAssign True va Nothing -> Just $ FSBlockAssign va
  SCase zox e l d -> FSCase zox e <$> traverse (\(CaseItem p v) -> FCaseItem p <$> mybf v) l <*> mybf d
  SIf e t f -> FSIf e <$> mybf t <*> mybf f
  SDisable hi -> Just $ FSDisable hi
  SLoop ls b -> FSLoop ls <$> traverse fromStatement b
  SBlock h ps b -> FSBlock h ps <$> traverse (traverse fromStatement) b
  _ -> Nothing
  where mybf s = traverse (traverse fromStatement) s

-- | Converts MybStmt to AttrStmt
fromMybStmt :: MybStmt -> AttrStmt
fromMybStmt x = case x of
  Attributed _ Nothing -> Attributed [] $ SIf (Expr $ genexprnumber 1) x $ Attributed [] Nothing
  Attributed a (Just s) -> Attributed a s

type BD f t = BlockDecl (Compose f Identified) t

-- | Converts ModGenSingleItem's `BlockDecl` into ModGenBlockedItem's `BlockDecl`
toMGIBlockDecl :: BD NonEmpty t -> NonEmpty (BD Identity t)
toMGIBlockDecl x = case x of
  BDReg sr d -> conv (BDReg sr) d
  BDInt d -> conv BDInt d
  BDReal d -> conv BDReal d
  BDTime d -> conv BDTime d
  BDRealTime d -> conv BDRealTime d
  BDEvent d -> conv BDEvent d
  BDLocalParam t d -> conv (BDLocalParam t) d
  where
    conv f = fmap (f . Compose . Identity) . getCompose

-- | Converts one ModGenBlockedItem's `BlockDecl` into ModGenSingleItem's `BlockDecl`
fromMGIBlockDecl1 :: BD Identity t -> BD NonEmpty t
fromMGIBlockDecl1 x = case x of
  BDReg sr d -> conv (BDReg sr) d
  BDInt d -> conv BDInt d
  BDReal d -> conv BDReal d
  BDTime d -> conv BDTime d
  BDRealTime d -> conv BDRealTime d
  BDEvent d -> conv BDEvent d
  BDLocalParam t d -> conv (BDLocalParam t) d
  where
    conv f = f . Compose . (:|[]) . runIdentity . getCompose

-- | Merges one ModGenBlockedItem's `BlockDecl` with one ModGenSingleItem's `BlockDecl`
fromMGIBlockDecl_add :: BD Identity t -> BD NonEmpty t -> Maybe (BD NonEmpty t)
fromMGIBlockDecl_add x y = case (x, y) of
  (BDReg nsr d, BDReg sr l) | nsr == sr -> add (BDReg sr) d l
  (BDInt d, BDInt l) -> add BDInt d l
  (BDReal d, BDReal l) -> add BDReal d l
  (BDTime d, BDTime l) -> add BDTime d l
  (BDRealTime d, BDRealTime l) -> add BDRealTime d l
  (BDEvent d, BDEvent l) -> add BDEvent d l
  (BDLocalParam nt d, BDLocalParam t l) | nt == t -> add (BDLocalParam t) d l
  _ -> Nothing
  where
    add f x l = Just $ f $ Compose $ runIdentity (getCompose x) <| getCompose l

-- | Converts ModGenSingleItem like `BlockDecl` into StdBlockDecl `BlockDecl`
toStdBlockDecl :: BD NonEmpty t -> NonEmpty (Identified (BlockDecl Identity t))
toStdBlockDecl x = case x of
  BDReg sr d -> conv (BDReg sr) d
  BDInt d -> conv BDInt d
  BDReal d -> conv BDReal d
  BDTime d -> conv BDTime d
  BDRealTime d -> conv BDRealTime d
  BDEvent d -> conv BDEvent d
  BDLocalParam t d -> conv (BDLocalParam t) d
  where
    conv f = fmap (fmap $ f . Identity) . getCompose

-- | Converts `SpecifySingleItem` into `SpecifyBlockedItem`s
toSpecBlockedItem :: SpecifySingleItem -> NonEmpty SpecifyBlockedItem
toSpecBlockedItem x = case x of
  SISpecParam rng d -> conv (SISpecParam rng) d
  SIPulsestyleOnevent st -> conv SIPulsestyleOnevent st
  SIPulsestyleOndetect st -> conv SIPulsestyleOndetect st
  SIShowcancelled st -> conv SIShowcancelled st
  SINoshowcancelled st -> conv SINoshowcancelled st
  SIPathDeclaration mpc con pol eds v -> [SIPathDeclaration mpc con pol eds v]
  SISetup a -> [SISetup a]
  SIHold a -> [SIHold a]
  SISetupHold a aa -> [SISetupHold a aa]
  SIRecovery a -> [SIRecovery a]
  SIRemoval a -> [SIRemoval a]
  SIRecrem a aa -> [SIRecrem a aa]
  SISkew a -> [SISkew a]
  SITimeSkew a ev rem -> [SITimeSkew a ev rem]
  SIFullSkew a tcl ev rem -> [SIFullSkew a tcl ev rem]
  SIPeriod ref tcl s -> [SIPeriod ref tcl s]
  SIWidth ref tcl t s -> [SIWidth ref tcl t s]
  SINoChange ref dat st en s -> [SINoChange ref dat st en s]
  where
    conv f = fmap (f . Identity)

fromSpecBlockedItem1 :: SpecifyBlockedItem -> SpecifySingleItem
fromSpecBlockedItem1 x = case x of
  SISpecParam rng d -> conv (SISpecParam rng) d
  SIPulsestyleOnevent st -> conv SIPulsestyleOnevent st
  SIPulsestyleOndetect st -> conv SIPulsestyleOndetect st
  SIShowcancelled st -> conv SIShowcancelled st
  SINoshowcancelled st -> conv SINoshowcancelled st
  SIPathDeclaration mpc con pol eds v -> SIPathDeclaration mpc con pol eds v
  SISetup a -> SISetup a
  SIHold a -> SIHold a
  SISetupHold a aa -> SISetupHold a aa
  SIRecovery a -> SIRecovery a
  SIRemoval a -> SIRemoval a
  SIRecrem a aa -> SIRecrem a aa
  SISkew a -> SISkew a
  SITimeSkew a ev rem -> SITimeSkew a ev rem
  SIFullSkew a tcl ev rem -> SIFullSkew a tcl ev rem
  SIPeriod ref tcl s -> SIPeriod ref tcl s
  SIWidth ref tcl t s -> SIWidth ref tcl t s
  SINoChange ref dat st en s -> SINoChange ref dat st en s
  where
    conv f = f . (:|[]) . runIdentity

fromSpecBlockedItem_add :: SpecifyBlockedItem -> SpecifySingleItem -> Maybe SpecifySingleItem
fromSpecBlockedItem_add x y = case (x, y) of
  (SISpecParam nrng d, SISpecParam rng l) | nrng == rng-> add (SISpecParam rng) d l
  (SIPulsestyleOnevent st, SIPulsestyleOnevent l) -> add SIPulsestyleOnevent st l
  (SIPulsestyleOndetect st, SIPulsestyleOndetect l) -> add SIPulsestyleOndetect st l
  (SIShowcancelled st, SIShowcancelled l) -> add SIShowcancelled st l
  (SINoshowcancelled st, SINoshowcancelled l) -> add SINoshowcancelled st l
  _ -> Nothing
  where
    add f x y = Just $ f $ runIdentity x <| y

-- | Converts `SpecifyBlockedItem`s into `SpecifySingleItem`s
fromSpecBlockedItem :: [SpecifyBlockedItem] -> [SpecifySingleItem]
fromSpecBlockedItem = nonEmpty [] $ toList . regroup fromSpecBlockedItem1 fromSpecBlockedItem_add

-- | Converts `ModGenSingleItem` into `ModGenBlockedItem`s
toMGBlockedItem :: ModGenSingleItem -> NonEmpty ModGenBlockedItem
toMGBlockedItem x = case x of
  MGINetInit nt ds np ni -> conv (MGINetInit nt ds np) ni
  MGINetDecl nt np nd -> conv (MGINetDecl nt np) nd
  MGITriD ds np ni -> conv (MGITriD ds np) ni
  MGITriC cs np nd -> conv (MGITriC cs np) nd
  MGIBlockDecl d -> fmap MGIBlockDecl $ toMGIBlockDecl d
  MGIGenVar i -> conv MGIGenVar i
  MGITask b i d s -> [MGITask b i d s]
  MGIFunc b t i d s -> [MGIFunc b t i d s]
  MGIDefParam po -> conv MGIDefParam po
  MGIContAss ds d3 na -> conv (MGIContAss ds d3) na
  MGICMos r d3 l -> conv (MGICMos r d3) l
  MGIEnable r b ds d3 l -> conv (MGIEnable r b ds d3) l
  MGIMos r np d3 l -> conv (MGIMos r np d3) l
  MGINIn nt n ds d2 l -> conv (MGINIn nt n ds d2) l
  MGINOut r ds d2 l -> conv (MGINOut r ds d2) l
  MGIPassEn r b d2 l -> conv (MGIPassEn r b d2) l
  MGIPass r l -> conv (MGIPass r) l
  MGIPull b ds l -> conv (MGIPull b ds) l
  MGIUDPInst udp ds d2 i -> conv (MGIUDPInst udp ds d2) i
  MGIModInst mod pa i -> conv (MGIModInst mod pa) i
  MGIUnknownInst t p i -> conv (MGIUnknownInst t p) i
  MGIInitial s -> [MGIInitial s]
  MGIAlways s -> [MGIAlways s]
  MGILoopGen ii iv c ui uv b -> [MGILoopGen ii iv c ui uv b]
  MGICondItem ci -> [MGICondItem ci]
  where
    conv f = fmap (f . Identity)

fromMGBlockedItem1 :: ModGenBlockedItem -> ModGenSingleItem
fromMGBlockedItem1 x = case x of
  MGINetInit nt ds np ni -> conv (MGINetInit nt ds np) ni
  MGINetDecl nt np nd -> conv (MGINetDecl nt np) nd
  MGITriD ds np ni -> conv (MGITriD ds np) ni
  MGITriC cs np nd -> conv (MGITriC cs np) nd
  MGIBlockDecl d -> MGIBlockDecl $ fromMGIBlockDecl1 d
  MGIGenVar i -> conv MGIGenVar i
  MGITask b i d s -> MGITask b i d s
  MGIFunc b t i d s -> MGIFunc b t i d s
  MGIDefParam po -> conv MGIDefParam po
  MGIContAss ds d3 na -> conv (MGIContAss ds d3) na
  MGICMos r d3 i -> conv (MGICMos r d3) i
  MGIEnable r b ds d3 i -> conv (MGIEnable r b ds d3) i
  MGIMos r np d3 i -> conv (MGIMos r np d3) i
  MGINIn nt n ds d2 i -> conv (MGINIn nt n ds d2) i
  MGINOut r ds d2 i -> conv (MGINOut r ds d2) i
  MGIPassEn r b d2 i -> conv (MGIPassEn r b d2) i
  MGIPass r i -> conv (MGIPass r) i
  MGIPull b ds i -> conv (MGIPull b ds) i
  MGIUDPInst udp ds d2 i -> conv (MGIUDPInst udp ds d2) i
  MGIModInst mod pa i -> conv (MGIModInst mod pa) i
  MGIUnknownInst t p i -> conv (MGIUnknownInst t p) i
  MGIInitial s -> MGIInitial s
  MGIAlways s -> MGIAlways s
  MGILoopGen ii iv c ui uv b -> MGILoopGen ii iv c ui uv b
  MGICondItem ci -> MGICondItem ci
  where
    conv f = f . (:|[]) . runIdentity

fromMGBlockedItem_add :: ModGenBlockedItem -> ModGenSingleItem -> Maybe ModGenSingleItem
fromMGBlockedItem_add x y = case (x, y) of
  (MGINetInit nnt nds nnp ni, MGINetInit nt ds np l) | nnt == nt && nds == ds && nnp == np ->
    add (MGINetInit nt ds np) ni l
  (MGINetDecl nnt nnp nd, MGINetDecl nt np l) | nnt == nt && nnp == np ->
    add (MGINetDecl nt np) nd l
  (MGITriD nds nnp ni, MGITriD ds np l) | nds == ds && nnp == np -> add (MGITriD ds np) ni l
  (MGITriC ncs nnp nd, MGITriC cs np l) | ncs == cs && nnp == np -> add (MGITriC cs np) nd l
  (MGIBlockDecl d, MGIBlockDecl l) -> MGIBlockDecl <$> fromMGIBlockDecl_add d l
  (MGIGenVar i, MGIGenVar l) -> add MGIGenVar i l
  (MGIDefParam po, MGIDefParam l) -> add MGIDefParam po l
  (MGIContAss nds nd3 na, MGIContAss ds d3 l) | nds == ds && nd3 == d3 ->
    add (MGIContAss ds d3) na l
  (MGICMos nr nd3 i, MGICMos r d3 l) | nr == r && nd3 == d3 -> add (MGICMos r d3) i l
  (MGIEnable nr nb nds nd3 i, MGIEnable r b ds d3 l)
   | nr == r && nb == b && nds == ds && nd3 == d3 -> add (MGIEnable r b ds d3) i l
  (MGIMos nr nnp nd3 i, MGIMos r np d3 l) | nr == r && nnp == np && nd3 == d3 ->
    add (MGIMos r np d3) i l
  (MGINIn nnt nn nds nd2 i, MGINIn nt n ds d2 l)
    | nnt == nt && nn == n && nds == ds && nd2 == d2 -> add (MGINIn nt n ds d2) i l
  (MGINOut nr nds nd2 i, MGINOut r ds d2 l) | nr == r && nds == ds && nd2 == d2 ->
    add (MGINOut r ds d2) i l
  (MGIPassEn nr nb nd2 i, MGIPassEn r b d2 l) | nr == r && nb == b && nd2 == d2 ->
    add (MGIPassEn r b d2) i l
  (MGIPass nr i, MGIPass r l) | nr == r -> add (MGIPass r) i l
  (MGIPull nb nds i, MGIPull b ds l) | nb == b && nds == ds -> add (MGIPull b ds) i l
  (MGIUDPInst nudp nds nd2 i, MGIUDPInst udp ds d2 l)
    | nudp == udp && nds == ds && nd2 == d2 -> add (MGIUDPInst udp ds d2) i l
  (MGIModInst nmod npa i, MGIModInst mod pa l) | nmod == mod && npa == pa ->
    add (MGIModInst mod pa) i l
  (MGIUnknownInst nt np i, MGIUnknownInst t p l) | nt == t && np == p ->
    add (MGIUnknownInst t p) i l
  _ -> Nothing
  where
    add f x y = Just $ f $ runIdentity x <| y

-- | Converts `ModGenBlockedItem`s into `ModGenSingleItem`s
fromMGBlockedItem :: [Attributed ModGenBlockedItem] -> [Attributed ModGenSingleItem]
fromMGBlockedItem =
  nonEmpty [] $ toList . regroup (fmap fromMGBlockedItem1) (addAttributed fromMGBlockedItem_add)

-- | Resolves Module and Primitive instantiation if possible
-- | Also checks there are no duplicate toplevel elements
resolveInsts :: Verilog2005 -> Either String Verilog2005
resolveInsts v = do
  nm <-
    foldr
      ( \m h ->
          h >>=
            let Identifier k = _mbIdent m
             in HashMap.alterF (maybe (Right $ Just True) $ const $ Left $ duperr k) k
      )
      (Right HashMap.empty)
      (_vModule v)
  nm <-
    foldr
      (\p h ->
          h >>=
            let Identifier k = _pbIdent p
             in HashMap.alterF (maybe (Right $ Just False) $ const $ Left $ duperr k) k
      )
      (Right nm)
      (_vPrimitive v)
  return $ v & biplate %~ \mgi -> case mgi of
    MGIUnknownInst k@(Identifier i) param (Identity (UknInst n lv args)) ->
      case HashMap.lookup i nm of
        Nothing -> mgi
        Just False ->
          MGIUDPInst k dsDefault (either (D21 . MTMSingle) (uncurry $ on D22 MTMSingle) <$> param) $
            Identity $ UDPInst (Just n) lv args
        Just True ->
          MGIModInst
            k
            ( ParamPositional $
                case param of Nothing -> []; Just (Right (e0, e1)) -> [e0, e1]; Just (Left e) -> [e]
            )
            ( Identity $
                ModInst n $
                  PortPositional $ map (Attributed [] . Just) $ netlv2expr lv : NE.toList args
            )
    _ -> mgi
  where duperr = printf "module or primitive %s defined more than once" . show
