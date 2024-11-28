-- Module      : Verismith.Verilog2005.Mutation
-- Description : AST mutation.
-- Copyright   : (c) 2024 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE OverloadedLists #-}

module Verismith.Verilog2005.Mutation
  ( 
  )
where

import Data.Typeable
import Data.Maybe
import Data.List.NonEmpty (NonEmpty)
import Data.Bifunctor (second)
import Data.Bitraversable (bitraverse)
import GHC.IsList
import Control.Monad
import Control.Monad.Reader
import Verismith.Utils (mkpair)
import Verismith.Verilog2005.Randomness
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.Utils (fromStatement, toStatement)

data MutationOpts = MutationOpts
  { _moIdentity :: !Double
  }

data MutationStore = MutationStore
  { _msMTM :: !(forall t. MutationVector (GenMinTypMax t)),
    _msPrim :: !(forall i r a. MutationVector (GenPrim i r a)),
    _msExpr :: !(forall i r a. MutationVector (GenExpr i r a)),
    _msAttr :: !(MutationVector Attributes),
    _msRangeExpr :: !(forall t. MutationVector (GenRangeExpr t)),
    _msLValue :: !(forall t. MutationVector (LValue t)),
    _msAssign :: !(forall t. MutationVector (Assign t)),
    _msEventPrim :: !(MutationVector EventPrim),
    _msDelay1 :: !(MutationVector Delay1),
    _msDelay2 :: !(MutationVector Delay2),
    _msDelay3 :: !(MutationVector Delay3),
    _msLoopStmt :: !(MutationVector LoopStatement),
    _msCasePat :: !(forall t. MutationVector (NonEmpty t)),
    _msStmt :: !(MutationVector Statement),
    _msGenCond :: !(MutationVector ModGenCondItem),
    _msGenBlk :: !(MutationVector GenerateBlock),
    _msModGenItem :: !(MutationVector ModGenBlockedItem),
    _msModItem :: !(MutationVector ModuleItem),
    _msPDV :: !(MutationVector PathDelayValue),
    _msSpecItem :: !(MutationVector SpecifyBlockedItem),
    _msModule :: !(MutationVector ModuleBlock),
    _msPrimTable :: !(MutationVector PrimTable),
    _msPrimitive :: !(MutationVector PrimitiveBlock),
    _msV2005 :: !(MutationVector Verilog2005),
    _msList :: !(forall t. IsList t => MutationVector t)
  }

type Mutator = GenM MutationStore
type PureMutation t = t -> Maybe t
type PrimMutation t = t -> Maybe (Mutator t)
type MutationVector t = [(Double, PrimMutation t)]
type Mutation t = t -> Mutator t

-- Mutation list:
-- MTM-> if single make 3, if 3 and same merge
-- Identifier: Need to build the name hierarchy, only doable at module/Primitive/Generate/Config/function/whatever level
-- Prim-> conversion of constant to a specific representation, concat one, multconcat from concat
-- Function changes at global level
-- Expression splitting at MGI level
-- Concat reordering then fixing the references at global level
-- Expr-> ternary branch swapping, "-"-"+ -", "*1"-"", "/1"-"", "+0"-"", constant folding,
--   "*x/x"-"", plus assoc, plus commut, times commut, times assoc, "~ +1"-"-", "- -1"-"~", "un+"-"",
--   "!"-"&~", "!"-"~|", "|"-"!=0", "|~"-"~&", "~ &"-"~&", "~ |"-"~|", "! |"-"~|", "! &"-"~&",
--   "~ ^"-"~^", "! ^"-"~^", "~ ~"-""
-- Some may be incorrect because of signedness
-- type aware rewrites? neq is |xor, "- -"-"", all rewrites in my ESA
-- GenRangeExpr-> make single, make pair, make baseoff+ and baseoff
-- Delay-> 1/B to make2, 1/B to make3, B/2/3 to make1, 1 to Base
-- SignRange: Change range and range references, change signedness and correct at callsite
-- Assignment: At Global Level, rename; if never referenced elswhere, Merge Expr; At local level, Split Expr
-- Parameter: At global level, rename, shuffle, offset the value at assign and deoffset at use
-- ParamOver (DefParam): At MGI level, change the value inside the module instantiation and put the real value in a defparam, resolve the defparam
-- (Param/Port)Assign: At global level or with enough info, change a named for a positional and the other way around
-- EventPrim-> change edge and expression accordingly
-- EventControl: at statement level, Deps <-> Expr
-- LoopStatement-> repeat-for, for-while, forever-for
-- Statement: Check ESA, if/case-loop
-- BlockDecl: At Block level, rename
-- ModGenCondItem-> If-Case comversion, If transforms, Case-transforms, check ESA
-- ModGenItem-> See ESA, gate conversion, always-initial forever, cond-loop
-- PathDelayValue-> refer to conversion table

-- buildStore :: MutationOpts -> MutationStore
-- TODO

mutate :: MutationVector t -> Mutation t
mutate v x = join $ sampleWeighted $ mapMaybe (traverse ($ x)) v

mutateWith :: (MutationStore -> MutationVector t) -> Mutation t
mutateWith p x = asks (p . fst) >>= flip mutate x

mutateList :: IsList t => Mutation t
mutateList l = fromList <$> mutateWith _msList (toList l)

mutateGMTM :: Mutation t -> Mutation (GenMinTypMax t)
mutateGMTM f = mutateWith _msMTM >=> \x -> case x of
  MTMSingle e -> MTMSingle <$> f e
  MTMFull em et eM -> MTMFull <$> f em <*> f et <*> f eM

mutateCMTM :: Mutation CMinTypMax
mutateCMTM = mutateGMTM mutateCExpr

mutateMTM :: Mutation MinTypMax
mutateMTM = mutateGMTM mutateExpr

mutatePrim :: Mutation i -> Mutation r -> Mutation a -> Mutation (GenPrim i r a)
mutatePrim fi fr fa = mutateWith _msPrim >=> \x -> case x of
  PrimIdent i r -> PrimIdent <$> fi i <*> fr r
  PrimConcat c -> PrimConcat <$> mapM mExpr c
  PrimMultConcat m c ->
    PrimMultConcat <$> mutateGExpr pure (traverse mutateCRE) fa m <*> mapM mExpr c
  PrimFun i a args -> PrimFun <$> fi i <*> fa a <*> mapM mExpr args
  PrimSysFun i args -> PrimSysFun i <$> mapM mExpr args
  PrimMinTypMax m -> PrimMinTypMax <$> mutateGMTM mExpr m
  _ -> pure x
  where mExpr = mutateGExpr fi fr fa

mutateHI :: Mutation HierIdent
mutateHI (HierIdent p i) = flip HierIdent i <$> mapM (traverse $ traverse mutateCExpr) p

mutateGDR :: Mutation e -> Mutation (GenDimRange e)
mutateGDR f (GenDimRange d r) = GenDimRange <$> mapM f d <*> mutateGRE f r

mutateDR :: Mutation DimRange
mutateDR = mutateGDR mutateExpr

mutateCDR :: Mutation CDimRange
mutateCDR = mutateGDR mutateCExpr

mutateGExpr :: Mutation i -> Mutation r -> Mutation a -> Mutation (GenExpr i r a)
mutateGExpr fi fr fa = mutateWith _msExpr >=> \x -> case x of
  ExprPrim p -> ExprPrim <$> mPrim p
  ExprUnOp op a p -> ExprUnOp op <$> fa a <*> mPrim p
  ExprBinOp l op a r -> flip ExprBinOp op <$> mExpr l <*> fa a <*> mExpr r
  ExprCond c a t f -> ExprCond <$> mExpr c <*> fa a <*> mExpr t <*> mExpr f
  where
    mExpr = mutateGExpr fi fr fa
    mPrim = mutatePrim fi fr fa

mutateCExpr :: Mutation CExpr
mutateCExpr (CExpr e) = CExpr <$> mutateGExpr pure (traverse mutateCRE) mutateAttr e

mutateExpr :: Mutation Expr
mutateExpr (Expr e) = Expr <$> mutateGExpr mutateHI (traverse mutateDR) mutateAttr e

mutateAttr :: Mutation Attributes
mutateAttr = mutateWith _msAttr >=> mutateList >=> mapM (mapM mAttr)
  where
    mAttr (Attribute i v) = Attribute i <$> traverse (mutateGExpr pure (traverse mutateCRE) pure) v

mutateAttributed :: Mutation t -> Mutation (Attributed t)
mutateAttributed f (Attributed a x) = Attributed <$> mutateAttr a <*> f x

mutateAttrIded :: Mutation t -> Mutation (AttrIded t)
mutateAttrIded f (AttrIded a i x) = flip AttrIded i <$> mutateAttr a <*> f x

mutateR2 :: Mutation Range2
mutateR2 (Range2 m l) = Range2 <$> mutateCExpr m <*> mutateCExpr l

mutateGRE :: Mutation e -> Mutation (GenRangeExpr e)
mutateGRE f = mutateWith _msRangeExpr >=> \x -> case x of
  GRESingle e -> GRESingle <$> f e
  GREPair r2 -> GREPair <$> mutateR2 r2
  GREBaseOff b mp o -> flip GREBaseOff mp <$> f b <*> mutateCExpr o

mutateRE :: Mutation RangeExpr
mutateRE = mutateGRE mutateExpr

mutateCRE :: Mutation CRangeExpr
mutateCRE = mutateGRE mutateCExpr

mutateD3 :: Mutation Delay3
mutateD3 = mutateWith _msDelay3 >=> \x -> case x of
  D31 m -> D31 <$> mutateMTM m
  D32 r f -> D32 <$> mutateMTM r <*> mutateMTM f
  D33 r f h -> D33 <$> mutateMTM r <*> mutateMTM f <*> mutateMTM h
  _ -> pure x

mutateD2 :: Mutation Delay2
mutateD2 = mutateWith _msDelay2 >=> \x -> case x of
  D21 m -> D21 <$> mutateMTM m
  D22 r f -> D22 <$> mutateMTM r <*> mutateMTM f
  _ -> pure x

mutateD1 :: Mutation Delay1
mutateD1 = mutateWith _msDelay1 >=> \x -> case x of
  D11 m -> D11 <$> mutateMTM m
  _ -> pure x

mutateSR :: Mutation SignRange
mutateSR (SignRange sn r) = SignRange sn <$> traverse mutateR2 r

mutateST :: Mutation SpecTerm
mutateST (SpecTerm i r) = SpecTerm i <$> traverse mutateCRE r

mutateCT :: Mutation (ComType t)
mutateCT x = case x of
  CTConcrete e sr -> CTConcrete e <$> mutateSR sr
  _ -> pure x

mutateLV :: Mutation dr -> Mutation (LValue dr)
mutateLV f = mutateWith _msLValue >=> \x -> case x of
  LVSingle hi dr -> LVSingle <$> mutateHI hi <*> traverse f dr
  LVConcat l -> LVConcat <$> mapM (mutateLV f) l

mutateNLV :: Mutation NetLValue
mutateNLV = mutateLV mutateCDR

mutateVLV :: Mutation VarLValue
mutateVLV = mutateLV mutateDR

mutateAss :: Mutation dr -> Mutation (Assign dr)
mutateAss f = mutateWith _msAssign >=> \(Assign lv e) -> Assign <$> mutateLV f lv <*> mutateExpr e

mutateNAss :: Mutation NetAssign
mutateNAss = mutateAss mutateCDR

mutateVAss :: Mutation VarAssign
mutateVAss = mutateAss mutateDR

mutateParam :: Mutation Parameter
mutateParam (Parameter t v) = Parameter <$> mutateCT t <*> mutateCMTM v

mutatePO :: Mutation ParamOver
mutatePO (ParamOver hi v) = ParamOver <$> mutateHI hi <*> mutateCMTM v

mutateParamAss :: Mutation ParamAssign
mutateParamAss x = case x of
  ParamPositional l -> ParamPositional <$> mapM mutateExpr l
  ParamNamed l -> ParamNamed <$> mapM (traverse $ traverse mutateMTM) l

mutatePortAss :: Mutation PortAssign
mutatePortAss x = case x of
  PortNamed l -> PortNamed <$> mapM (mutateAttrIded $ traverse mutateExpr) l
  PortPositional l -> PortPositional <$> mapM (mutateAttributed $ traverse mutateExpr) l

mutateEP :: Mutation EventPrim
mutateEP = mutateWith _msEventPrim >=> \(EventPrim p e) -> EventPrim p <$> mutateExpr e

mutateEC :: Mutation EventControl
mutateEC x = case x of
  ECIdent hi -> ECIdent <$> mutateHI hi
  ECExpr l -> fmap ECExpr $ mapM mutateEP l >>= mutateList
  _ -> pure x

mutateDEC :: Mutation DelayEventControl
mutateDEC x = case x of
  DECDelay d -> DECDelay <$> mutateD1 d
  DECEvent ec -> DECEvent <$> mutateEC ec
  DECRepeat e ec -> DECRepeat <$> mutateExpr e <*> mutateEC ec

mutatePCA :: Mutation ProcContAssign
mutatePCA x = case x of
  PCAAssign va -> PCAAssign <$> mutateVAss va
  PCADeassign vlv -> PCADeassign <$> mutateVLV vlv
  PCAForce vana -> PCAForce <$> bitraverse mutateVAss mutateNAss vana
  PCARelease vlvnlv -> PCARelease <$> bitraverse mutateVLV mutateNLV vlvnlv

mutateLS :: Mutation LoopStatement
mutateLS = mutateWith _msLoopStmt >=> \x -> case x of
  LSRepeat e -> LSRepeat <$> mutateExpr e
  LSWhile e -> LSWhile <$> mutateExpr e
  LSFor vi c vu -> LSFor <$> mutateVAss vi <*> mutateExpr c <*> mutateVAss vu
  _ -> pure x

mutateFStmt :: Mutation FunctionStatement
mutateFStmt x = do
  y <- maybe x id . fromStatement <$> mutateWith _msStmt (toStatement x)
  case y of
    FSBlockAssign va -> FSBlockAssign <$> mutateVAss va
    FSCase zox e b d ->
      FSCase zox <$> mutateExpr e
        <*> ( mapM
                ( \(FCaseItem pat v) ->
                    FCaseItem <$> (mapM mutateExpr pat >>= mutateList) <*> mutateMFStmt v
                )
                b
                >>= mutateList
            )
        <*> mutateMFStmt d
    FSIf c t f -> FSIf <$> mutateExpr c <*> mutateMFStmt t <*> mutateMFStmt f
    FSDisable hi -> FSDisable <$> mutateHI hi
    FSLoop ls b -> FSLoop <$> mutateLS ls <*> mutateAFStmt b
    FSBlock h ps b ->
      flip FSBlock ps <$> traverse (traverse $ mapM (mutateAttrIded mutateSBD) >=> mutateList) h
        <*> mapM mutateAFStmt b
  where
    mutateAFStmt = mutateAttributed mutateFStmt
    mutateMFStmt = mutateAttributed $ traverse mutateFStmt

mutateStmt :: Mutation Statement
mutateStmt = mutateWith _msStmt >=> \x -> case x of
  SBlockAssign b ass dec -> SBlockAssign b <$> mutateVAss ass <*> traverse mutateDEC dec
  SCase zox e b d ->
    SCase zox <$> mutateExpr e
      <*> ( mapM
              ( \(CaseItem pat v) ->
                  CaseItem <$> (mapM mutateExpr pat >>= mutateList) <*> mutateMStmt v
              )
              b
              >>= mutateList
          )
      <*> mutateMStmt d
  SIf c t f -> SIf <$> mutateExpr c <*> mutateMStmt t <*> mutateMStmt f
  SDisable hi -> SDisable <$> mutateHI hi
  SEventTrigger hi e -> SEventTrigger <$> mutateHI hi <*> mapM mutateExpr e
  SLoop ls b -> SLoop <$> mutateLS ls <*> mutateAStmt b
  SProcContAssign pca -> SProcContAssign <$> mutatePCA pca
  SProcTimingControl tec s ->
    SProcTimingControl <$> bitraverse mutateD1 mutateEC tec <*> mutateMStmt s
  SBlock h ps b ->
    flip SBlock ps <$> traverse (traverse $ mapM (mutateAttrIded mutateSBD) >=> mutateList) h
      <*> mapM mutateAStmt b
  SSysTaskEnable i args -> SSysTaskEnable i <$> mapM (traverse mutateExpr) args
  STaskEnable hi args -> STaskEnable <$> mutateHI hi <*> mapM mutateExpr args
  SWait e s -> SWait <$> mutateExpr e <*> mutateMStmt s

mutateAStmt :: Mutation AttrStmt
mutateAStmt = mutateAttributed mutateStmt

mutateMStmt :: Mutation MybStmt
mutateMStmt = mutateAttributed $ traverse mutateStmt

mutateNP :: Mutation NetProp
mutateNP (NetProp sn v d) = NetProp sn <$> traverse (traverse mutateR2) v <*> traverse mutateD3 d

mutateND :: Mutation NetDecl
mutateND (NetDecl i r2) = NetDecl i <$> mapM mutateR2 r2

mutateNI :: Mutation NetInit
mutateNI (NetInit i e) = NetInit i <$> mutateExpr e

mutateBD :: (forall x. Mutation x -> Mutation (f x)) -> Mutation t -> Mutation (BlockDecl f t)
mutateBD ff ft x = case x of
  BDReg sr d -> BDReg <$> mutateSR sr <*> ff ft d
  BDInt d -> BDInt <$> ff ft d
  BDReal d -> BDReal <$> ff ft d
  BDTime d -> BDTime <$> ff ft d
  BDRealTime d -> BDRealTime <$> ff ft d
  BDEvent d -> BDEvent <$> ff (mapM mutateR2) d
  BDLocalParam ct v -> BDLocalParam <$> mutateCT ct <*> ff mutateCMTM v

mutateSBD :: Mutation StdBlockDecl
mutateSBD x = case x of
  SBDBlockDecl bd -> SBDBlockDecl <$> mutateBD traverse (mapM mutateR2) bd
  SBDParameter p -> SBDParameter <$> mutateParam p

mutateTFBD :: Mutation (TFBlockDecl t)
mutateTFBD x = case x of
  TFBDStd sbd -> TFBDStd <$> mutateSBD sbd
  TFBDPort d t -> TFBDPort d <$> mutateCT t

mutateGCI :: Mutation GenCaseItem
mutateGCI (GenCaseItem pat v) =
  GenCaseItem <$> (mapM mutateCExpr pat >>= mutateList) <*> mutateGCB v

mutateMGCI :: Mutation ModGenCondItem
mutateMGCI = mutateWith _msGenCond >=> \x -> case x of
  MGCIIf c t f -> MGCIIf <$> mutateCExpr c <*> mutateGCB t <*> mutateGCB f
  MGCICase e b d -> MGCICase <$> mutateCExpr e <*> (mapM mutateGCI b >>= mutateList) <*> mutateGCB d

mutateGCB :: Mutation GenerateCondBlock
mutateGCB x = case x of
  GCBBlock b -> GCBBlock <$> mutateGB b
  GCBConditional c -> GCBConditional <$> mutateAttributed mutateMGCI c
  _ -> pure x

mutateInstanceName :: Mutation InstanceName
mutateInstanceName (InstanceName i r2) = InstanceName i <$> traverse mutateR2 r2

mutateGICMos :: Mutation GICMos
mutateGICMos (GICMos i lv inp nc pc) =
  GICMos <$> traverse mutateInstanceName i
    <*> mutateNLV lv
    <*> mutateExpr inp
    <*> mutateExpr nc
    <*> mutateExpr pc

mutateGIEnable :: Mutation GIEnable
mutateGIEnable (GIEnable i lv inp en) =
  GIEnable <$> traverse mutateInstanceName i <*> mutateNLV lv <*> mutateExpr inp <*> mutateExpr en

mutateGIMos :: Mutation GIMos
mutateGIMos (GIMos i lv inp en) =
  GIMos <$> traverse mutateInstanceName i <*> mutateNLV lv <*> mutateExpr inp <*> mutateExpr en

mutateGINIn :: Mutation GINIn
mutateGINIn (GINIn i lv inp) =
  GINIn <$> traverse mutateInstanceName i <*> mutateNLV lv <*> (mapM mutateExpr inp >>= mutateList)

mutateGINOut :: Mutation GINOut
mutateGINOut (GINOut i lv inp) =
  GINOut <$> traverse mutateInstanceName i <*> (mapM mutateNLV lv >>= mutateList) <*> mutateExpr inp

mutateGIPassEn :: Mutation GIPassEn
mutateGIPassEn (GIPassEn i lhs rhs en) =
  GIPassEn <$> traverse mutateInstanceName i <*> mutateNLV lhs <*> mutateNLV rhs <*> mutateExpr en

mutateGIPass :: Mutation GIPass
mutateGIPass (GIPass i lhs rhs) =
  GIPass <$> traverse mutateInstanceName i <*> mutateNLV lhs <*> mutateNLV rhs

mutateGIPull :: Mutation GIPull
mutateGIPull (GIPull i lv) = GIPull <$> traverse mutateInstanceName i <*> mutateNLV lv

mutateUDPInst :: Mutation UDPInst
mutateUDPInst (UDPInst i lv args) =
  UDPInst <$> traverse mutateInstanceName i <*> mutateNLV lv <*> mapM mutateExpr args

mutateModInst :: Mutation ModInst
mutateModInst (ModInst i ports) = ModInst <$> mutateInstanceName i <*> mutatePortAss ports

mutateUknInst :: Mutation UknInst
mutateUknInst (UknInst i a0 args) =
  UknInst <$> mutateInstanceName i <*> mutateNLV a0 <*> mapM mutateExpr args

mutateMGI :: Mutation ModGenBlockedItem
mutateMGI = mutateWith _msModGenItem >=> \x -> case x of
  MGINetInit nt ds np ni -> MGINetInit nt ds <$> mutateNP np <*> traverse mutateNI ni
  MGINetDecl nt np nd -> MGINetDecl nt <$> mutateNP np <*> traverse mutateND nd
  MGITriD ds np ni -> MGITriD ds <$> mutateNP np <*> traverse mutateNI ni
  MGITriC cs np nd -> MGITriC cs <$> mutateNP np <*> traverse mutateND nd
  MGIBlockDecl bd ->
    MGIBlockDecl <$> mutateBD traverse (bitraverse (mapM mutateR2) mutateCExpr) bd
  MGITask a i d b ->
    MGITask a i <$> (mapM (mutateAttrIded mutateTFBD) d >>= mutateList) <*> mutateMStmt b
  MGIFunc a t i d b ->
    flip (MGIFunc a) i <$> traverse mutateCT t
      <*> (mapM (mutateAttrIded mutateTFBD) d >>= mutateList)
      <*> mutateFStmt b
  MGIDefParam po -> MGIDefParam <$> traverse mutatePO po
  MGIContAss ds d3 na -> MGIContAss ds <$> traverse mutateD3 d3 <*> traverse mutateNAss na
  MGICMos r d3 i -> MGICMos r <$> traverse mutateD3 d3 <*> traverse mutateGICMos i
  MGIEnable r b ds d3 i -> MGIEnable r b ds <$> traverse mutateD3 d3 <*> traverse mutateGIEnable i
  MGIMos r np d3 i -> MGIMos r np <$> traverse mutateD3 d3 <*> traverse mutateGIMos i
  MGINIn nin n ds d2 i -> MGINIn nin n ds <$> traverse mutateD2 d2 <*> traverse mutateGINIn i
  MGINOut r ds d2 i -> MGINOut r ds <$> traverse mutateD2 d2 <*> traverse mutateGINOut i
  MGIPassEn r b d2 i -> MGIPassEn r b <$> traverse mutateD2 d2 <*> traverse mutateGIPassEn i
  MGIPass r i -> MGIPass r <$> traverse mutateGIPass i
  MGIPull ud ds i -> MGIPull ud ds <$> traverse mutateGIPull i
  MGIUDPInst kind ds d2 i ->
    MGIUDPInst kind ds <$> traverse mutateD2 d2 <*> traverse mutateUDPInst i
  MGIModInst kind params i ->
    MGIModInst kind <$> mutateParamAss params <*> traverse mutateModInst i
  MGIUnknownInst kind params i ->
    MGIUnknownInst kind
      <$> traverse (bitraverse mutateExpr $ bitraverse mutateExpr mutateExpr) params
      <*> traverse mutateUknInst i
  MGIInitial s -> MGIInitial <$> mutateAStmt s
  MGIAlways s -> MGIAlways <$> mutateAStmt s
  MGILoopGen ii iv c ui uv b ->
    MGILoopGen ii <$> mutateCExpr iv
      <*> mutateCExpr c
      <*> pure ui
      <*> mutateCExpr uv
      <*> mutateGB b
  MGICondItem ci -> MGICondItem <$> mutateMGCI ci
  _ -> pure x

mutateTCE :: Mutation TimingCheckEvent
mutateTCE (TimingCheckEvent ec st tcc) =
  TimingCheckEvent ec <$> mutateST st <*> traverse (traverse mutateExpr) tcc

mutateCTCE :: Mutation ControlledTimingCheckEvent
mutateCTCE (ControlledTimingCheckEvent ec st tcc) =
  ControlledTimingCheckEvent ec <$> mutateST st <*> traverse (traverse mutateExpr) tcc

mutateSTCA :: Mutation STCArgs
mutateSTCA (STCArgs de re tcl n) =
  STCArgs <$> mutateTCE de <*> mutateTCE re <*> mutateExpr tcl <*> pure n

mutateSTCAA :: Mutation STCAddArgs
mutateSTCAA (STCAddArgs tcl sc ctc dr dd) =
  STCAddArgs <$> mutateExpr tcl
    <*> traverse mutateMTM sc
    <*> traverse mutateMTM ctc
    <*> traverse (traverse $ traverse mutateCMTM) dr
    <*> traverse (traverse $ traverse mutateCMTM) dd

mutateMPC :: Mutation ModulePathCondition
mutateMPC x = case x of
  MPCCond e -> MPCCond <$> mutateGExpr pure pure mutateAttr e
  _ -> pure x

mutateSP :: Mutation SpecPath
mutateSP x = case x of
  SPParallel inp outp -> SPParallel <$> mutateST inp <*> mutateST outp
  SPFull inp outp ->
    SPFull <$> (mapM mutateST inp >>= mutateList) <*> (mapM mutateST outp >>= mutateList)

mutatePDV :: Mutation PathDelayValue
mutatePDV = mutateWith _msPDV >=> \x -> case x of
  PDV1 x -> PDV1 <$> mutateCMTM x
  PDV2 r f -> PDV2 <$> mutateCMTM r <*> mutateCMTM f
  PDV3 r f z -> PDV3 <$> mutateCMTM r <*> mutateCMTM f <*> mutateCMTM z
  PDV6 t01 t10 t0z tz1 t1z tz0 ->
    PDV6 <$> mutateCMTM t01
      <*> mutateCMTM t10
      <*> mutateCMTM t0z
      <*> mutateCMTM tz1
      <*> mutateCMTM t1z
      <*> mutateCMTM tz0
  PDV12 t01 t10 t0z tz1 t1z tz0 t0x tx1 t1x tx0 txz tzx ->
    PDV12 <$> mutateCMTM t01
      <*> mutateCMTM t10
      <*> mutateCMTM t0z
      <*> mutateCMTM tz1
      <*> mutateCMTM t1z
      <*> mutateCMTM tz0
      <*> mutateCMTM t0x
      <*> mutateCMTM tx1
      <*> mutateCMTM t1x
      <*> mutateCMTM tx0
      <*> mutateCMTM txz
      <*> mutateCMTM tzx

mutateSI :: Mutation SpecifyBlockedItem
mutateSI = mutateWith _msSpecItem >=> \x -> case x of
  SISpecParam r2 spd -> SISpecParam <$> traverse mutateR2 r2 <*> traverse mutateSPD spd
  SIPulsestyleOnevent st -> SIPulsestyleOnevent <$> traverse mutateST st
  SIPulsestyleOndetect st -> SIPulsestyleOndetect <$> traverse mutateST st
  SIShowcancelled st -> SIShowcancelled <$> traverse mutateST st
  SINoshowcancelled st -> SINoshowcancelled <$> traverse mutateST st
  SIPathDeclaration mpc con pol eds v ->
    SIPathDeclaration <$> mutateMPC mpc
      <*> mutateSP con
      <*> pure pol
      <*> traverse (bitraverse mutateExpr pure) eds
      <*> mutatePDV v
  SISetup a -> SISetup <$> mutateSTCA a
  SIHold a -> SIHold <$> mutateSTCA a
  SISetupHold a aa -> SISetupHold <$> mutateSTCA a <*> mutateSTCAA aa
  SIRecovery a -> SIRecovery <$> mutateSTCA a
  SIRemoval a -> SIRemoval <$> mutateSTCA a
  SIRecrem a aa -> SIRecrem <$> mutateSTCA a <*> mutateSTCAA aa
  SISkew a -> SISkew <$> mutateSTCA a
  SITimeSkew a eb ra ->
    SITimeSkew <$> mutateSTCA a <*> traverse mutateCExpr eb <*> traverse mutateCExpr ra
  SIFullSkew a tcl eb ra ->
    SIFullSkew <$> mutateSTCA a
      <*> mutateExpr tcl
      <*> traverse mutateCExpr eb
      <*> traverse mutateCExpr ra
  SIPeriod ctce tcl n -> SIPeriod <$> mutateCTCE ctce <*> mutateExpr tcl <*> pure n
  SIWidth ctce tcl t n ->
    SIWidth <$> mutateCTCE ctce <*> mutateExpr tcl <*> traverse mutateCExpr t <*> pure n
  SINoChange re de se ee n ->
    SINoChange <$> mutateTCE re <*> mutateTCE de <*> mutateMTM se <*> mutateMTM ee <*> pure n

mutateSPD :: Mutation SpecParamDecl
mutateSPD x = case x of
  SPDAssign i v -> SPDAssign i <$> mutateCMTM v
  SPDPathPulse io rej err ->
    SPDPathPulse <$> traverse (bitraverse mutateST mutateST) io
      <*> mutateCMTM rej
      <*> mutateCMTM err

mutateMI :: Mutation ModuleItem
mutateMI x = do
  y <- mutateWith _msModItem x
  case y of
    MIMGI mgi -> MIMGI <$> mutateAttributed mutateMGI mgi
    MIPort p -> MIPort <$> mutateAttrIded (traverse mutateSR) p
    MIParameter p -> MIParameter <$> mutateAttrIded mutateParam p
    MIGenReg l -> MIGenReg <$> mapM (mutateAttributed mutateMGI) l
    MISpecParam a r2 spd ->
      MISpecParam <$> mutateAttr a <*> traverse mutateR2 r2 <*> mutateSPD spd
    MISpecBlock l -> fmap MISpecBlock $ mapM mutateSI l >>= mutateList

mutateGB :: Mutation GenerateBlock
mutateGB = mutateWith _msGenBlk >=> traverse (mapM $ mutateAttributed mutateMGI)

mutateMB :: Mutation ModuleBlock
mutateMB = mutateWith _msModule >=> \(ModuleBlock a b i pi mi ts c p dnt) ->
  (\a pi mi -> ModuleBlock a b i pi mi ts c p dnt) <$> mutateAttr a
    <*> mapM (traverse $ mapM $ traverse $ traverse mutateCRE) pi
    <*> mapM mutateMI mi

mutatePT :: Mutation PrimTable
mutatePT = mutateWith _msPrimTable >=> \x -> case x of
  CombTable l -> CombTable <$> mutateList l
  SeqTable i l -> SeqTable i <$> mutateList l

mutatePP :: Mutation PrimPort
mutatePP x = case x of
  PPOutReg e -> PPOutReg <$> traverse mutateCExpr e
  _ -> pure x

mutatePB :: Mutation PrimitiveBlock
mutatePB = mutateWith _msPrimitive >=> \(PrimitiveBlock a i outp inp pd b) ->
  (\a b c -> PrimitiveBlock a i outp inp b c) <$> mutateAttr a
    <*> mapM (mutateAttrIded mutatePP) pd
    <*> mutatePT b

mutateCB :: Mutation ConfigBlock
mutateCB (ConfigBlock i de b dft) = fmap (flip (ConfigBlock i de) dft) $ mapM pure b >>= mutateList

mutateV2005 :: Mutation Verilog2005
mutateV2005 = mutateWith _msV2005 >=> \(Verilog2005 m p c) ->
  Verilog2005 <$> mapM mutateMB m
    <*> (mapM mutatePB p >>= mutateList)
    <*> (mapM mutateCB c >>= mutateList)
