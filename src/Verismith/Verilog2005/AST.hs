-- Module      : Verismith.Verilog2005.AST
-- Description : Verilog 2005 AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, StandaloneDeriving, QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Verismith.Verilog2005.AST
  ( GenMinTypMax (..),
    CMinTypMax,
    MinTypMax,
    Identifier (..),
    Identified (..),
    UnaryOperator (..),
    BinaryOperator (..),
    Number (..),
    GenPrim (..),
    HierIdent (..),
    GenDimRange (..),
    DimRange,
    CDimRange,
    GenExpr (..),
    CExpr (..),
    Expr (..),
    Attribute (..),
    Attributes,
    Attributed (..),
    AttrIded (..),
    Range2 (..),
    GenRangeExpr (..),
    RangeExpr,
    CRangeExpr,
    NumIdent (..),
    Delay3 (..),
    Delay2 (..),
    Delay1 (..),
    SignRange (..),
    SpecTerm (..),
    EventPrefix (..),
    Dir (..),
    AbsType (..),
    ComType (..),
    NetType (..),
    Strength (..),
    DriveStrength (..),
    dsDefault,
    ChargeStrength (..),
    LValue (..),
    NetLValue,
    VarLValue,
    Assign (..),
    NetAssign,
    VarAssign,
    Parameter (..),
    ParamOver (..),
    ParamAssign (..),
    PortAssign (..),
    EventPrim (..),
    EventControl (..),
    DelayEventControl (..),
    ProcContAssign (..),
    LoopStatement (..),
    FCaseItem (..),
    CaseItem (..),
    FunctionStatement (..),
    AttrFStmt,
    MybFStmt,
    Statement (..),
    AttrStmt,
    MybStmt,
    NInputType (..),
    EdgeDesc (..),
    InstanceName (..),
    GICMos (..),
    GIEnable (..),
    GIMos (..),
    GINIn (..),
    GINOut (..),
    GIPassEn (..),
    GIPass (..),
    GIPull (..),
    TimingCheckEvent (..),
    ControlledTimingCheckEvent (..),
    STCArgs (..),
    STCAddArgs (..),
    ModulePathCondition (..),
    SpecPath (..),
    PathDelayValue (..),
    SpecifyItem (..),
    SpecifySingleItem,
    SpecifyBlockedItem,
    SpecParamDecl (..),
    NetProp (..),
    NetDecl (..),
    NetInit (..),
    BlockDecl (..),
    StdBlockDecl (..),
    TFBlockDecl (..),
    GenCaseItem (..),
    UDPInst (..),
    ModInst (..),
    UknInst (..),
    ModGenCondItem (..),
    GenerateCondBlock (..),
    ModGenItem (..),
    ModGenBlockedItem,
    ModGenSingleItem,
    ModuleItem (..),
    GenerateBlock,
    ModuleBlock (..),
    SigLevel (..),
    ZOX (..),
    CombRow (..),
    Edge (..),
    SeqIn (..),
    SeqRow (..),
    PrimTable (..),
    PrimPort (..),
    PrimitiveBlock (..),
    Dot1Ident (..),
    Cell_inst (..),
    LLU (..),
    ConfigItem (..),
    ConfigBlock (..),
    Verilog2005 (..),
    SystemFunction (..),
    Logic (..),
    sfMap,
    BXZ (..),
    OXZ (..),
    HXZ (..),
    hiPath,
    mbIdent,
    pbIdent,
    _CIInst,
    ciCell_inst,
    cbIdent,
    cbBody,
  )
where

import Control.Lens
import Data.Functor.Compose
import Data.Functor.Classes
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w, packChars)
import Data.Data
import Data.Data.Lens
import Data.String (IsString (..))
import Text.Show (showListWith)
import Text.Printf (printf)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector.Unboxed as V
import GHC.Generics (Generic)
import Numeric.Natural
import Verismith.Verilog2005.Token (BXZ (..), HXZ (..), OXZ (..), ZOX (..))

-- | Minimum, Typical, Maximum
data GenMinTypMax et
  = MTMSingle !et
  | MTMFull
      { _mtmMin :: !et,
        _mtmTyp :: !et,
        _mtmMax :: !et
      }
  deriving (Show, Eq, Data, Generic)

type CMinTypMax = GenMinTypMax CExpr

type MinTypMax = GenMinTypMax Expr

-- | Identifier, do not use for other things (like a string literal), used for biplate
newtype Identifier = Identifier ByteString
  deriving (Show, Eq, Data, Generic)

instance IsString Identifier where
  fromString =
    Identifier . packChars . concatMap
      (\c -> if ' ' < c && c <= '~' then [c] else printf "\\%02x" c)

-- | Quickly add an identifier to all members of a sum type, other uses are discouraged
data Identified t = Identified {_identIdent :: !Identifier, _identData :: !t}
  deriving (Show, Eq, Data, Generic)

instance Functor Identified where
  fmap f (Identified i x) = Identified i $ f x

instance Foldable Identified where
  foldr f acc (Identified i x) = f x acc

instance Traversable Identified where
  traverse f (Identified i x) = Identified i <$> f x
  sequenceA (Identified i x) = Identified i <$> x

showHelper :: (Int -> a -> ShowS) -> Identified a -> ShowS
showHelper fp (Identified i x) = showString "Identified " . shows i . showChar ' ' . fp 0 x

instance Show1 Identified where
  liftShowsPrec fp _ p = showHelper fp
  liftShowList fp _ = showListWith $ showHelper fp
instance Eq1 Identified where
  liftEq f (Identified ia a) (Identified ib b) = ia == ib && f a b

-- | Unary operators
data UnaryOperator
  = UnPlus
  | UnMinus
  | UnLNot
  | UnNot
  | UnAnd
  | UnNand
  | UnOr
  | UnNor
  | UnXor
  | UnXNor
  deriving (Eq, Data, Generic, Enum, Bounded)

instance Show UnaryOperator where
  show x = case x of
    UnPlus -> "+"
    UnMinus -> "-"
    UnLNot -> "!"
    UnNot -> "~"
    UnAnd -> "&"
    UnNand -> "~&"
    UnOr -> "|"
    UnNor -> "~|"
    UnXor -> "^"
    UnXNor -> "~^"

-- | Binary operators
data BinaryOperator
  = BinPlus
  | BinMinus
  | BinTimes
  | BinDiv
  | BinMod
  | BinEq
  | BinNEq
  | BinCEq
  | BinCNEq
  | BinLAnd
  | BinLOr
  | BinLT
  | BinLEq
  | BinGT
  | BinGEq
  | BinAnd
  | BinOr
  | BinXor
  | BinXNor
  | BinPower
  | BinLSL
  | BinLSR
  | BinASL
  | BinASR
  deriving (Eq, Data, Generic, Enum, Bounded)

instance Show BinaryOperator where
  show x = case x of
    BinPlus -> "+"
    BinMinus -> "-"
    BinTimes -> "*"
    BinDiv -> "/"
    BinMod -> "%"
    BinEq -> "=="
    BinNEq -> "!="
    BinCEq -> "==="
    BinCNEq -> "!=="
    BinLAnd -> "&&"
    BinLOr -> "||"
    BinLT -> "<"
    BinLEq -> "<="
    BinGT -> ">"
    BinGEq -> ">="
    BinAnd -> "&"
    BinOr -> "|"
    BinXor -> "^"
    BinXNor -> "~^"
    BinPower -> "**"
    BinLSL -> "<<"
    BinLSR -> ">>"
    BinASL -> "<<<"
    BinASR -> ">>>"

data Number
  = NBinary !(NonEmpty BXZ)
  | NOctal !(NonEmpty OXZ)
  | NDecimal !Natural
  | NHex !(NonEmpty HXZ)
  | NXZ !Bool
  deriving (Show, Eq, Data, Generic)

-- | Parametric primary expression
data GenPrim i r a
  = PrimNumber
      { _pnSize :: !(Maybe Natural),
        _pnSigned :: !Bool,
        _pnValue :: !Number
      }
  | PrimReal !ByteString
  | PrimIdent
      { _piIdent :: !i,
        _piSub :: !r
      }
  | PrimConcat !(NonEmpty (GenExpr i r a))
  | PrimMultConcat
      { _pmcMul :: !(GenExpr Identifier (Maybe CRangeExpr) a),
        _pmcExpr :: !(NonEmpty (GenExpr i r a))
      }
  | PrimFun
      { _pfIdent :: !i,
        _pfAttr :: !a,
        _pfArg :: ![GenExpr i r a]
      }
  | PrimSysFun
      { _psfIdent :: !ByteString,
        _psfArg :: ![GenExpr i r a]
      }
  | PrimMinTypMax !(GenMinTypMax (GenExpr i r a))
  | PrimString !ByteString
  deriving (Show, Eq, Data, Generic)

-- | Hierarchical identifier
data HierIdent = HierIdent {_hiPath :: ![(Identifier, Maybe CExpr)], _hiIdent :: !Identifier}
  deriving (Show, Eq, Data, Generic)

-- | Indexing for dimension and range
data GenDimRange e = GenDimRange {_gdrDim :: ![e], _gdrRange :: !(GenRangeExpr e)}
  deriving (Show, Eq, Data, Generic)

type DimRange = GenDimRange Expr

type CDimRange = GenDimRange CExpr

-- | Parametric expression
data GenExpr i r a
  = ExprPrim !(GenPrim i r a)
  | ExprUnOp
      { _euOp :: !UnaryOperator,
        _euAttr :: !a,
        _euPrim :: !(GenPrim i r a)
      }
  | ExprBinOp
      { _ebLhs :: !(GenExpr i r a),
        _ebOp :: !BinaryOperator,
        _ebAttr :: !a,
        _ebRhs :: !(GenExpr i r a)
      }
  | ExprCond
      { _ecCond :: !(GenExpr i r a),
        _ecAttr :: !a,
        _ecTrue :: !(GenExpr i r a),
        _ecFalse :: !(GenExpr i r a)
      }
  deriving (Show, Eq, Data, Generic)

instance (Data i, Data r, Data a) => Plated (GenExpr i r a) where
  plate = uniplate

newtype CExpr = CExpr (GenExpr Identifier (Maybe CRangeExpr) Attributes)
  deriving (Show, Eq, Data, Generic)

newtype Expr = Expr (GenExpr HierIdent (Maybe DimRange) Attributes)
  deriving (Show, Eq, Data, Generic)

-- | Attributes which can be set to various nodes in the AST.
data Attribute = Attribute
  { _attrIdent :: !ByteString,
    _attrValue :: !(Maybe (GenExpr Identifier (Maybe CRangeExpr) ()))
  }
  deriving (Show, Eq, Data, Generic)

type Attributes = [[Attribute]]

data Attributed t = Attributed {_attrAttr :: !Attributes, _attrData :: !t}
  deriving (Show, Eq, Data, Generic)

instance Functor Attributed where
  fmap f (Attributed a x) = Attributed a $ f x

instance Applicative Attributed where
  pure = Attributed []
  (<*>) (Attributed a1 f) (Attributed a2 x) = Attributed (a1 <> a2) $ f x

instance Foldable Attributed where
  foldMap f (Attributed _ x) = f x

instance Traversable Attributed where
  sequenceA (Attributed a x) = fmap (Attributed a) x

data AttrIded t = AttrIded {_aiAttr :: !Attributes, _aiIdent :: !Identifier, _aiData :: !t}
  deriving (Show, Eq, Data, Generic)

instance Functor AttrIded where
  fmap f (AttrIded a s x) = AttrIded a s $ f x

-- | Range2
data Range2 = Range2 {_r2MSB :: !CExpr, _r2LSB :: !CExpr}
  deriving (Show, Eq, Data, Generic)

-- | Range expressions
data GenRangeExpr e
  = GRESingle !e
  | GREPair !Range2
  | GREBaseOff
      { _greBase :: !e,
        _greMin_plus :: !Bool,
        _greOffset :: !CExpr
      }
  deriving (Show, Eq, Data, Generic)

type RangeExpr = GenRangeExpr Expr

type CRangeExpr = GenRangeExpr CExpr

-- TODO? this can definitely be omitted and expressed as a MTM
-- | Number or Identifier
data NumIdent
  = NIIdent !Identifier
  | NIReal !ByteString
  | NINumber !Natural
  deriving (Show, Eq, Data, Generic)

-- | Delay3
data Delay3
  = D3Base !NumIdent
  | D31 !MinTypMax
  | D32 { _d32Rise :: !MinTypMax, _d32Fall :: !MinTypMax }
  | D33 { _d33Rise :: !MinTypMax, _d33Fall :: !MinTypMax, _d33HighZ :: !MinTypMax }
  deriving (Show, Eq, Data, Generic)

-- | Delay2
data Delay2
  = D2Base !NumIdent
  | D21 !MinTypMax
  | D22 { _d22Rise :: !MinTypMax, _d22Fall :: !MinTypMax }
  deriving (Show, Eq, Data, Generic)

-- | Delay1
data Delay1
  = D1Base !NumIdent
  | D11 !MinTypMax
  deriving (Show, Eq, Data, Generic)

-- | Signedness and range are often together
data SignRange = SignRange {_srSign :: !Bool, _srRange :: !(Maybe Range2)}
  deriving (Show, Eq, Data, Generic)

-- | Specify terminal
data SpecTerm = SpecTerm {_stIdent :: !Identifier, _stRange :: !(Maybe CRangeExpr)}
  deriving (Show, Eq, Data, Generic)

-- | Event expression prefix
data EventPrefix = EPAny | EPPos | EPNeg
  deriving (Show, Eq, Bounded, Enum, Data, Generic)

-- | Port datatransfer directions
data Dir = DirIn | DirOut | DirInOut
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show Dir where
  show x = case x of DirIn -> "input"; DirOut -> "output"; DirInOut -> "inout"

-- | Abstract types for variables, parameters, functions and tasks
data AbsType = ATInteger | ATReal | ATRealtime | ATTime
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show AbsType where
  show x = case x of
    ATInteger -> "integer"
    ATReal -> "real"
    ATRealtime -> "realtime"
    ATTime -> "time"

-- | Function, parameter and task type
data ComType t
  = CTAbstract !AbsType
  | CTConcrete
    { _ctcExtra :: !t,
      _ctcSignRange :: !SignRange
    }
  deriving (Show, Eq, Data, Generic)

-- | Net type
data NetType
  = NTSupply1
  | NTSupply0
  | NTTri
  | NTTriAnd
  | NTTriOr
  | NTTri1
  | NTTri0
  | NTUwire
  | NTWire
  | NTWAnd
  | NTWOr
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show NetType where
  show x = case x of
    NTSupply1 -> "supply1"
    NTSupply0 -> "supply0"
    NTTri -> "tri"
    NTTriAnd -> "triand"
    NTTriOr -> "trior"
    NTTri1 -> "tri1"
    NTTri0 -> "tri0"
    NTUwire -> "uwire"
    NTWire -> "wire"
    NTWAnd -> "wand"
    NTWOr -> "wor"

-- | Net drive strengths
data Strength = StrSupply | StrStrong {-default-} | StrPull | StrWeak
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show Strength where
  show x = case x of
    StrSupply -> "supply"
    StrStrong -> "strong"
    StrPull -> "pull"
    StrWeak -> "weak"

data DriveStrength
  = DSNormal
      { _ds0 :: !Strength,
        _ds1 :: !Strength
      }
  | DSHighZ
      { _dsHZ :: !Bool,
        _dsStr :: !Strength
      }
  deriving (Show, Eq, Data, Generic)

dsDefault = DSNormal {_ds0 = StrStrong, _ds1 = StrStrong}

-- | Capacitor charge
data ChargeStrength = CSSmall | CSMedium {-default-} | CSLarge
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show ChargeStrength where
  show x = case x of CSSmall -> "(small)"; CSMedium -> "(medium)"; CSLarge -> "(large)"

-- | Left side of assignments
data LValue dr
  = LVSingle
      { _lvIdent :: !HierIdent,
        _lvDimRange :: !(Maybe dr)
      }
  | LVConcat !(NonEmpty (LValue dr))
  deriving (Show, Eq, Data, Generic)

type NetLValue = LValue CDimRange

type VarLValue = LValue DimRange

-- | Assignment
data Assign dr = Assign {_aLValue :: !(LValue dr), _aValue :: !Expr}
  deriving (Show, Eq, Data, Generic)

type NetAssign = Assign CDimRange
type VarAssign = Assign DimRange

-- | Parameter
data Parameter = Parameter {_paramType :: !(ComType ()), _paramValue :: !CMinTypMax}
  deriving (Show, Eq, Data, Generic)

-- | DefParam assignment
data ParamOver = ParamOver {_poIdent :: !HierIdent, _poValue :: !CMinTypMax}
  deriving (Show, Eq, Data, Generic)

-- | Parameter assignment list
data ParamAssign
  = ParamPositional ![Expr]
  | ParamNamed ![Identified (Maybe MinTypMax)]
  deriving (Show, Eq, Data, Generic)

-- | Port assignment list
data PortAssign
  = PortNamed ![AttrIded (Maybe Expr)]
  | PortPositional ![Attributed (Maybe Expr)]
  deriving (Show, Eq, Data, Generic)

-- | Event primitive
data EventPrim = EventPrim {_epOp :: !EventPrefix, _epExpr :: !Expr}
  deriving (Show, Eq, Data, Generic)

-- | Event control
data EventControl
  = ECIdent !HierIdent
  | ECExpr !(NonEmpty EventPrim)
  | ECDeps
  deriving (Show, Eq, Data, Generic)

-- | Delay or Event control
data DelayEventControl
  = DECDelay !Delay1
  | DECEvent !EventControl
  | DECRepeat
      { _decrExpr :: !Expr,
        _decrEvent :: !EventControl
      }
  deriving (Show, Eq, Data, Generic)

-- | Procedural continuous assignment
data ProcContAssign
  = PCAAssign !VarAssign
  | PCADeassign !VarLValue
  | PCAForce !(Either VarAssign NetAssign)
  | PCARelease !(Either VarLValue NetLValue)
  deriving (Show, Eq, Data, Generic)

-- | Loop statement
data LoopStatement
  = LSForever
  | LSRepeat !Expr
  | LSWhile !Expr
  | LSFor
      { _lsfInit :: !VarAssign,
        _lsfCond :: !Expr,
        _lsfUpd :: !VarAssign
      }
  deriving (Show, Eq, Data, Generic)

-- | Case item
data FCaseItem = FCaseItem {_fciPat :: !(NonEmpty Expr), _fciVal :: !MybFStmt}
  deriving (Show, Eq, Data, Generic)
data CaseItem = CaseItem {_ciPat :: !(NonEmpty Expr), _ciVal :: !MybStmt}
  deriving (Show, Eq, Data, Generic)

-- | Function statement, more limited than general statement because they are purely combinational
data FunctionStatement
  = FSBlockAssign !VarAssign
  | FSCase
      { _fscType :: !ZOX,
        _fscExpr :: !Expr,
        _fscBody :: ![FCaseItem],
        _fscDef :: !MybFStmt
      }
  | FSIf
      { _fsiExpr :: !Expr,
        _fsiTrue :: !MybFStmt,
        _fsiFalse :: !MybFStmt
      }
  | FSDisable !HierIdent
  | FSLoop
      { _fslHead :: !LoopStatement,
        _fslBody :: !AttrFStmt
      }
  | FSBlock
      { _fsbHeader :: !(Maybe (Identifier, [AttrIded StdBlockDecl])),
        _fsbPar_seq :: !Bool,
        _fsbBody :: ![AttrFStmt]
      }
  deriving (Show, Eq, Data, Generic)

instance Plated FunctionStatement where
  plate = uniplate

type AttrFStmt = Attributed FunctionStatement

type MybFStmt = Attributed (Maybe FunctionStatement)

-- | Statement
data Statement
  = SBlockAssign
      { _sbaBlock :: !Bool,
        _sbaAssign :: !VarAssign,
        _sbaDelev :: !(Maybe DelayEventControl)
      }
  | SCase
      { _scType :: !ZOX,
        _scExpr :: !Expr,
        _scBody :: ![CaseItem],
        _scDef :: !MybStmt
      }
  | SIf
      { _siExpr :: !Expr,
        _siTrue :: !MybStmt,
        _siFalse :: !MybStmt
      }
  | SDisable !HierIdent
  | SEventTrigger
      { _setIdent :: !HierIdent,
        _setIndex :: ![Expr]
      }
  | SLoop
      { _slHead :: !LoopStatement,
        _slBody :: !AttrStmt
      }
  | SProcContAssign !ProcContAssign
  | SProcTimingControl
      { _sptcControl :: !(Either Delay1 EventControl),
        _sptcStmt :: !MybStmt
      }
  | SBlock
      { _sbHeader :: !(Maybe (Identifier, [AttrIded StdBlockDecl])),
        _sbPar_seq :: !Bool,
        _sbBody :: ![AttrStmt]
      }
  | SSysTaskEnable
      { _ssteIdent :: !ByteString,
        _ssteArgs :: ![Maybe Expr]
      }
  | STaskEnable
      { _steIdent :: !HierIdent,
        _steArgs :: ![Expr]
      }
  | SWait
      { _swExpr :: !Expr,
        _swStmt :: !MybStmt
      }
  deriving (Show, Eq, Data, Generic)

instance Plated Statement where
  plate = uniplate

type AttrStmt = Attributed Statement

type MybStmt = Attributed (Maybe Statement)

-- | N-input logic gate types
data NInputType = NITAnd | NITOr | NITXor
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show NInputType where
  show x = case x of NITAnd -> "and"; NITOr -> "or"; NITXor -> "xor"

-- | Instance name
data InstanceName = InstanceName { _INIdent :: !Identifier, _INRange :: !(Maybe Range2) }
  deriving (Show, Eq, Data, Generic)

-- | Gate instances
data GICMos = GICMos
  { _gicmName :: !(Maybe InstanceName),
    _gicmOutput :: !NetLValue,
    _gicmInput :: !Expr,
    _gicmNControl :: !Expr,
    _gicmPControl :: !Expr
  }
  deriving (Show, Eq, Data, Generic)

data GIEnable = GIEnable
  { _gieName :: !(Maybe InstanceName),
    _gieOutput :: !NetLValue,
    _gieInput :: !Expr,
    _gieEnable :: !Expr
  }
  deriving (Show, Eq, Data, Generic)

data GIMos = GIMos
  { _gimName :: !(Maybe InstanceName),
    _gimOutput :: !NetLValue,
    _gimInput :: !Expr,
    _gimEnable :: !Expr
  }
  deriving (Show, Eq, Data, Generic)

data GINIn = GINIn
  { _giniName :: !(Maybe InstanceName),
    _giniOutput :: !NetLValue,
    _giniInput :: !(NonEmpty Expr)
  }
  deriving (Show, Eq, Data, Generic)

data GINOut = GINOut
  { _ginoName :: !(Maybe InstanceName),
    _ginoOutput :: !(NonEmpty NetLValue),
    _ginoInput :: !Expr
  }
  deriving (Show, Eq, Data, Generic)

data GIPassEn = GIPassEn
  { _gipeName :: !(Maybe InstanceName),
    _gipeLhs :: !NetLValue,
    _gipeRhs :: !NetLValue,
    _gipeEnable :: !Expr
  }
  deriving (Show, Eq, Data, Generic)

data GIPass = GIPass
  { _gipsName :: !(Maybe InstanceName),
    _gipsLhs :: !NetLValue,
    _gipsRhs :: !NetLValue
  }
  deriving (Show, Eq, Data, Generic)

data GIPull = GIPull
  { _giplName :: !(Maybe InstanceName),
    _giplOutput :: !NetLValue
  }
  deriving (Show, Eq, Data, Generic)

-- | Edge descriptor, a 6 Bool array (01, 0x, 10, 1x, x0, x1)
type EdgeDesc = V.Vector Bool

-- | Timing check (controlled) event
data TimingCheckEvent = TimingCheckEvent
  { _tceEvCtl :: !(Maybe EdgeDesc),
    _tceSpecTerm :: !SpecTerm,
    _tceTimChkCond :: !(Maybe (Bool, Expr))
  }
  deriving (Show, Eq, Data, Generic)

data ControlledTimingCheckEvent = ControlledTimingCheckEvent
  { _ctceEvCtl :: !EdgeDesc,
    _ctceSpecTerm :: !SpecTerm,
    _ctceTimChkCond :: !(Maybe (Bool, Expr))
  }
  deriving (Show, Eq, Data, Generic)

-- | System timing check common arguments
data STCArgs = STCArgs
  { _stcaDataEvent :: !TimingCheckEvent,
    _stcaRefEvent :: !TimingCheckEvent,
    _stcaTimChkLim :: !Expr,
    _stcaNotifier :: !(Maybe Identifier)
  }
  deriving (Show, Eq, Data, Generic)

-- | Setuphold and Recrem additionnal arguments
data STCAddArgs = STCAddArgs
  { _stcaaTimChkLim :: !Expr,
    _stcaaStampCond :: !(Maybe MinTypMax),
    _stcaaChkTimCond :: !(Maybe MinTypMax),
    _stcaaDelayedRef :: !(Maybe (Identified (Maybe CMinTypMax))),
    _stcaaDelayedData :: !(Maybe (Identified (Maybe CMinTypMax)))
  }
  deriving (Show, Eq, Data, Generic)

-- | Module path condition
data ModulePathCondition
  = MPCCond !(GenExpr Identifier () Attributes)
  | MPCNone
  | MPCAlways
  deriving (Show, Eq, Data, Generic)

-- | Specify path declaration
data SpecPath
  = SPParallel
      { _sppInput :: !SpecTerm,
        _sppOutput :: !SpecTerm
      }
  | SPFull
      { _spfInput :: !(NonEmpty SpecTerm),
        _spfOutput :: !(NonEmpty SpecTerm)
      }
  deriving (Show, Eq, Data, Generic)

-- | Specify Item path delcaration delay value list
data PathDelayValue
  = PDV1 !CMinTypMax
  | PDV2 !CMinTypMax !CMinTypMax
  | PDV3 !CMinTypMax !CMinTypMax !CMinTypMax
  | PDV6 !CMinTypMax !CMinTypMax !CMinTypMax !CMinTypMax !CMinTypMax !CMinTypMax
  | PDV12
    !CMinTypMax !CMinTypMax !CMinTypMax !CMinTypMax !CMinTypMax !CMinTypMax
    !CMinTypMax !CMinTypMax !CMinTypMax !CMinTypMax !CMinTypMax !CMinTypMax
  deriving (Show, Eq, Data, Generic)

-- | Specify block item
-- | f is either Identity or NonEmpty
-- | it is used to abstract between several specify items in a block and a single comma separated one
data SpecifyItem f
  = SISpecParam
    { _sipcRange :: !(Maybe Range2),
      _sipcDecl :: !(f SpecParamDecl)
    }
  | SIPulsestyleOnevent !(f SpecTerm)
  | SIPulsestyleOndetect !(f SpecTerm)
  | SIShowcancelled !(f SpecTerm)
  | SINoshowcancelled !(f SpecTerm)
  | SIPathDeclaration
      { _sipdCond :: !ModulePathCondition,
        _sipdConn :: !SpecPath,
        _sipdPolarity :: !(Maybe Bool),
        _sipdEDS :: !(Maybe (Expr, Maybe Bool)),
        _sipdValue :: !PathDelayValue
      }
  | SISetup !STCArgs
  | SIHold !STCArgs
  | SISetupHold
      { _sishArgs :: !STCArgs,
        _sishAddArgs :: !STCAddArgs
      }
  | SIRecovery !STCArgs
  | SIRemoval !STCArgs
  | SIRecrem
      { _sirArgs :: !STCArgs,
        _sirAddArgs :: !STCAddArgs
      }
  | SISkew !STCArgs
  | SITimeSkew
      { _sitsArgs :: !STCArgs,
        _sitsEvBased :: !(Maybe CExpr),
        _sitsRemActive :: !(Maybe CExpr)
      }
  | SIFullSkew
      { _sifsArgs :: !STCArgs,
        _sifsTimChkLim :: !Expr,
        _sifsEvBased :: !(Maybe CExpr),
        _sifsRemActive :: !(Maybe CExpr)
      }
  | SIPeriod
      { _sipCRefEvent :: !ControlledTimingCheckEvent,
        _sipTimCtlLim :: !Expr,
        _sipNotif :: !(Maybe Identifier)
      }
  | SIWidth
      { _siwRefEvent :: !ControlledTimingCheckEvent,
        _siwTimCtlLim :: !Expr,
        _siwThresh :: !(Maybe CExpr),
        _siwNotif :: !(Maybe Identifier)
      }
  | SINoChange
      { _sincRefEvent :: !TimingCheckEvent,
        _sincDataEvent :: !TimingCheckEvent,
        _sincStartEdgeOff :: !MinTypMax,
        _sincEndEdgeOff :: !MinTypMax,
        _sincNotif :: !(Maybe Identifier)
      }

deriving instance (Show1 f, forall a. Show a => Show (f a)) => Show (SpecifyItem f)
deriving instance (Eq1 f, forall a. Eq a => Eq (f a)) => Eq (SpecifyItem f)
deriving instance (Typeable f, forall a. Data a => Data (f a)) => Data (SpecifyItem f)
deriving instance (forall a. Generic a => Generic (f a)) => Generic (SpecifyItem f)

type SpecifySingleItem = SpecifyItem NonEmpty
type SpecifyBlockedItem = SpecifyItem Identity

-- | Specparam declaration
data SpecParamDecl
  = SPDAssign
    { _spdaIdent :: !Identifier,
      _spdaValue :: !CMinTypMax
    }
  | SPDPathPulse -- Not completely accurate input/output as it is ambiguous
      { _spdpInOut :: !(Maybe (SpecTerm, SpecTerm)),
        _spdpReject :: !CMinTypMax,
        _spdpError :: !CMinTypMax
      }
  deriving (Show, Eq, Data, Generic)

-- | Net common properties
data NetProp = NetProp
  { _npSigned :: !Bool,
    _npVector :: !(Maybe (Maybe Bool, Range2)),
    _npDelay :: !(Maybe Delay3)
  }
  deriving (Show, Eq, Data, Generic)

-- | Net declaration
data NetDecl = NetDecl {_ndIdent :: !Identifier, _ndDim :: ![Range2]}
  deriving (Show, Eq, Data, Generic)

-- | Net initialisation
data NetInit = NetInit {_niIdent :: !Identifier, _niValue :: !Expr}
  deriving (Show, Eq, Data, Generic)

-- | Block declaration
-- | t is used to abstract between block_decl and modgen_decl
-- | f is used to abstract between the separated and grouped modgen_item
data BlockDecl f t
  = BDReg
    { _bdrgSR :: !SignRange,
      _bdrgData :: !(f t)
    }
  | BDInt !(f t)
  | BDReal !(f t)
  | BDTime !(f t)
  | BDRealTime !(f t)
  | BDEvent !(f [Range2])
  | BDLocalParam
    { _bdlpType :: !(ComType ()),
      _bdlpValue :: !(f CMinTypMax)
    }

deriving instance (Show t, forall a. Show a => Show (f a)) => Show (BlockDecl f t)
deriving instance (Eq t, forall a. Eq a => Eq (f a)) => Eq (BlockDecl f t)
deriving instance (Typeable t, Data t, Typeable f, forall a. Data a => Data (f a)) => Data (BlockDecl f t)
deriving instance (Generic t, forall a. Generic a => Generic (f a)) => Generic (BlockDecl f t)

-- | Block item declaration (for statement blocks [begin/fork], tasks, and functions)
data StdBlockDecl
  = SBDBlockDecl !(BlockDecl Identity [Range2])
  | SBDParameter !Parameter
  deriving (Show, Eq, Data, Generic)

-- | Task and Function block declaration
data TFBlockDecl t
  = TFBDStd !StdBlockDecl
  | TFBDPort
    { _tfbdpDir :: !t,
      _tfbdpType :: !(ComType Bool)
    }
  deriving (Show, Eq, Data, Generic)

-- | Case generate branch
data GenCaseItem = GenCaseItem {_gciPat :: !(NonEmpty CExpr), _gciVal :: !GenerateCondBlock}
  deriving (Show, Eq, Data, Generic)

-- | UDP named instantiation
data UDPInst = UDPInst
  { _udpiName :: !(Maybe InstanceName),
    _udpiLValue :: !NetLValue,
    _udpiArgs :: !(NonEmpty Expr)
  }
  deriving (Show, Eq, Data, Generic)

-- | Module named instantiation
data ModInst = ModInst { _miName :: !InstanceName, _miPort :: !PortAssign }
  deriving (Show, Eq, Data, Generic)

-- | Unknown named instantiation
data UknInst = UknInst
  { _uiName :: !InstanceName,
    _uiArg0 :: !NetLValue,
    _uiArgs :: !(NonEmpty Expr)
  }
  deriving (Show, Eq, Data, Generic)

-- | Module or Generate conditional item because scoping rules are special
data ModGenCondItem
  = MGCIIf
      { _mgiiExpr :: !CExpr,
        _mgiiTrue :: !GenerateCondBlock,
        _mgiiFalse :: !GenerateCondBlock
      }
  | MGCICase
      { _mgicExpr :: !CExpr,
        _mgicBranch :: ![GenCaseItem],
        _mgicDefault :: !GenerateCondBlock
      }
  deriving (Show, Eq, Data, Generic)

-- | Generate Block or Conditional Item or nothing because scoping rules are special
data GenerateCondBlock
  = GCBEmpty
  | GCBBlock !GenerateBlock
  | GCBConditional !(Attributed ModGenCondItem)
  deriving (Show, Eq, Data, Generic)

-- | Module or Generate item
-- | f is either Identity or NonEmpty
-- | it is used to abstract between several modgen items in a block and a single comma separated one
data ModGenItem f
  = MGINetInit
      { _mginiType :: !NetType,
        _mginiDrive :: !DriveStrength,
        _mginiProp :: !NetProp,
        _mginiInit :: !(f NetInit)
      }
  | MGINetDecl
      { _mgindType :: !NetType,
        _mgindProp :: !NetProp,
        _mgindDecl :: !(f NetDecl)
      }
  | MGITriD
      { _mgitdDrive :: !DriveStrength,
        _mgitdProp :: !NetProp,
        _mgitdInit :: !(f NetInit)
      }
  | MGITriC
      { _mgitcCharge :: !ChargeStrength,
        _mgitcProp :: !NetProp,
        _mgitcDecl :: !(f NetDecl)
      }
  | MGIBlockDecl !(BlockDecl (Compose f Identified) (Either [Range2] CExpr))
  | MGIGenVar !(f Identifier)
  | MGITask
      { _mgitAuto :: !Bool,
        _mgitIdent :: !Identifier,
        _mgitDecl :: ![AttrIded (TFBlockDecl Dir)],
        _mgitBody :: !MybStmt
      }
  | MGIFunc
      { _mgifAuto :: !Bool,
        _mgifType :: !(Maybe (ComType ())),
        _mgifIdent :: !Identifier,
        _mgifDecl :: ![AttrIded (TFBlockDecl ())],
        _mgifBody :: !FunctionStatement
      }
  | MGIDefParam !(f ParamOver)
  | MGIContAss
      { _mgicaStrength :: !DriveStrength,
        _mgicaDelay :: !(Maybe Delay3),
        _mgicaAssign :: !(f NetAssign)
      }
  | MGICMos
      { _mgicmR :: !Bool,
        _mgicmDelay :: !(Maybe Delay3),
        _mgicmInst :: !(f GICMos)
      }
  | MGIEnable
      { _mgieR :: !Bool,
        _mgie1_0 :: !Bool,
        _mgieStrength :: !DriveStrength,
        _mgieDelay :: !(Maybe Delay3),
        _mgieInst :: !(f GIEnable)
      }
  | MGIMos
      { _mgimR :: !Bool,
        _mgimN_P :: !Bool,
        _mgimDelay :: !(Maybe Delay3),
        _mgimInst :: !(f GIMos)
      }
  | MGINIn
      { _mgininType :: !NInputType,
        _mgininN :: !Bool,
        _mgininStrength :: !DriveStrength,
        _mgininDelay :: !(Maybe Delay2),
        _mgininInst :: !(f GINIn)
      }
  | MGINOut
      { _mginoR :: !Bool,
        _mginoStrength :: !DriveStrength,
        _mginoDelay :: !(Maybe Delay2),
        _mginoInst :: !(f GINOut)
      }
  | MGIPassEn
      { _mgipeR :: !Bool,
        _mgipe1_0 :: !Bool,
        _mgipeDelay :: !(Maybe Delay2),
        _mgipeInst :: !(f GIPassEn)
      }
  | MGIPass
      { _mgipsR :: !Bool,
        _mgipsInst :: !(f GIPass)
      }
  | MGIPull
      { _mgiplUp_down :: !Bool,
        _mgiplStrength :: !DriveStrength,
        _mgiplInst :: !(f GIPull)
      }
  | MGIUDPInst
      { _mgiudpiUDP :: !Identifier,
        _mgiudpiStrength :: !DriveStrength,
        _mgiudpiDelay :: !(Maybe Delay2),
        _mgiudpiInst :: !(f UDPInst)
      }
  | MGIModInst
      { _mgimiMod :: !Identifier,
        _mgimiParams :: !ParamAssign,
        _mgimiInst :: !(f ModInst)
      }
  | MGIUnknownInst -- Sometimes identifying what is instantiated is impossible
      { _mgiuiType :: !Identifier,
        _mgiuiParam :: !(Maybe (Either Expr (Expr, Expr))),
        _mgiuiInst :: !(f UknInst)
      }
  | MGIInitial !AttrStmt
  | MGIAlways !AttrStmt
  | MGILoopGen
      { _mgilgInitIdent :: !Identifier,
        _mgilgInitValue :: !CExpr,
        _mgilgCond :: !CExpr,
        _mgilgUpdIdent :: !Identifier,
        _mgilgUpdValue :: !CExpr,
        _mgilgBody :: !GenerateBlock
      }
  | MGICondItem !ModGenCondItem

deriving instance (Show1 f, forall a. Show a => Show (f a)) => Show (ModGenItem f)
deriving instance (Eq1 f, forall a. Eq a => Eq (f a)) => Eq (ModGenItem f)
deriving instance (Typeable f, forall a. Data a => Data (f a)) => Data (ModGenItem f)
deriving instance (forall a. Generic a => Generic (f a)) => Generic (ModGenItem f)

type ModGenBlockedItem = ModGenItem Identity
type ModGenSingleItem = ModGenItem NonEmpty

instance Plated ModGenBlockedItem where
  plate = uniplate

-- | Module item: body of module
-- | Caution: if MIPort sign is False then it can be overriden by a MGINetDecl/Init
data ModuleItem
  = MIMGI !(Attributed ModGenBlockedItem)
  | MIPort !(AttrIded (Dir, SignRange))
  | MIParameter !(AttrIded Parameter)
  | MIGenReg ![Attributed ModGenBlockedItem]
  | MISpecParam
    { _mispAttribute :: !Attributes,
      _mispRange :: !(Maybe Range2),
      _mispDecl :: !SpecParamDecl
    }
  | MISpecBlock ![SpecifyBlockedItem]
  deriving (Show, Eq, Data, Generic)

type GenerateBlock = Identified [Attributed ModGenBlockedItem]

-- | Module block
data ModuleBlock = ModuleBlock
  { _mbAttr :: !Attributes,
    _mbMacro :: !Bool,
    _mbIdent :: !Identifier,
    _mbPortInter :: ![Identified [Identified (Maybe CRangeExpr)]],
    _mbBody :: ![ModuleItem],
    _mbTimescale :: !(Maybe (Int, Int)),
    _mbCell :: !Bool,
    _mbPull :: !(Maybe Bool),
    _mbDefNetType :: !(Maybe NetType)
  }
  deriving (Show, Eq, Data, Generic)

-- | Signal level
data SigLevel = L0 | L1 | LX | LQ | LB
  deriving (Eq, Bounded, Enum, Data, Generic)

instance Show SigLevel where
  show x = case x of L0 -> "0"; L1 -> "1"; LX -> "x"; LQ -> "?"; LB -> "b"

-- | Combinatorial table row
data CombRow = CombRow {_crInput :: !(NonEmpty SigLevel), _crOutput :: !ZOX}
  deriving (Show, Eq, Data, Generic)

-- | Edge specifier
data Edge
  = EdgePos_neg !Bool
  | EdgeDesc
      { _edFrom :: !SigLevel,
        _edTo :: !SigLevel
      }
  deriving (Eq, Data, Generic)

instance Show Edge where
  show x = case x of
    EdgePos_neg b -> if b then "p" else "n"
    EdgeDesc LQ LQ -> "*"
    EdgeDesc f t -> '(' : show f ++ show t ++ ")"

-- | Seqential table inputs: a list of input levels with at most 1 edge specifier
data SeqIn
  = SIComb !(NonEmpty SigLevel)
  | SISeq ![SigLevel] !Edge ![SigLevel]
  deriving (Eq, Data, Generic)

instance Show SeqIn where
  show x = case x of
    SIComb l -> concatMap show l
    SISeq l0 e l1 -> concatMap show l0 ++ show e ++ concatMap show l1

-- | Sequential table row
data SeqRow = SeqRow
  { _srowInput :: !SeqIn,
    _srowState :: !SigLevel,
    _srowNext :: !(Maybe ZOX)
  }
  deriving (Show, Eq, Data, Generic)

-- | Primitive transition table
data PrimTable
  = CombTable !(NonEmpty CombRow)
  | SeqTable
      { _stInit :: !(Maybe ZOX),
        _stRow :: !(NonEmpty SeqRow)
      }
  deriving (Show, Eq, Data, Generic)

-- | Primitive port type
data PrimPort
  = PPInput
  | PPOutput
  | PPReg
  | PPOutReg !(Maybe CExpr) -- no sem
  deriving (Show, Eq, Data, Generic)

-- | Primitive block
data PrimitiveBlock = PrimitiveBlock
  { _pbAttr :: !Attributes,
    _pbIdent :: !Identifier,
    _pbOutput :: !Identifier,
    _pbInput :: !(NonEmpty Identifier),
    _pbPortDecl :: !(NonEmpty (AttrIded PrimPort)),
    _pbBody :: !PrimTable
  }
  deriving (Show, Eq, Data, Generic)

-- | Library prefixed cell
data Dot1Ident = Dot1Ident {_d1iLib :: !(Maybe ByteString), _d1iCell :: !Identifier}
  deriving (Show, Eq, Data, Generic)

-- | Cell or instance
data Cell_inst
  = CICell !Dot1Ident
  | CIInst !(NonEmpty Identifier)
  deriving (Show, Eq, Data, Generic)

-- | Liblist or Use
data LLU
  = LLULiblist ![ByteString]
  | LLUUse
      { _lluUIdent :: !Dot1Ident,
        _lluUConfig :: !Bool
      }
  deriving (Show, Eq, Data, Generic)

-- | Items in a config block
data ConfigItem = ConfigItem
  { _ciCell_inst :: !Cell_inst,
    _ciLLU :: !LLU
  }
  deriving (Show, Eq, Data, Generic)

-- | Config Block: Identifier, Design lines, Configuration items
data ConfigBlock = ConfigBlock
  { _cbIdent :: !Identifier,
    _cbDesign :: ![Dot1Ident],
    _cbBody :: ![ConfigItem],
    _cbDef :: ![ByteString]
  }
  deriving (Show, Eq, Data, Generic)

-- | Internal representation of Verilog2005 AST
data Verilog2005 = Verilog2005
  { _vModule :: ![ModuleBlock],
    _vPrimitive :: ![PrimitiveBlock],
    _vConfig :: ![ConfigBlock]
  }
  deriving (Show, Eq, Data, Generic)

instance Semigroup Verilog2005 where
  (<>) v2a v2b =
    v2a
      { _vModule = _vModule v2a <> _vModule v2b,
        _vPrimitive = _vPrimitive v2a <> _vPrimitive v2b,
        _vConfig = _vConfig v2a <> _vConfig v2b
      }

instance Monoid Verilog2005 where
  mempty = Verilog2005 [] [] []

$(makeLenses ''HierIdent)
$(makeLenses ''ModuleBlock)
$(makeLenses ''PrimitiveBlock)
$(makePrisms ''Cell_inst)
$(makeLenses ''ConfigItem)
$(makeLenses ''ConfigBlock)

data Logic = LAnd | LOr | LNand | LNor
  deriving (Eq, Data)

instance Show Logic where
  show x = case x of LAnd -> "and"; LOr -> "or"; LNand -> "nand"; LNor -> "nor"

data SystemFunction
  = SFDisplay
  | SFDisplayb
  | SFDisplayh
  | SFDisplayo
  | SFStrobe
  | SFStrobeb
  | SFStrobeh
  | SFStrobeo
  | SFWrite
  | SFWriteb
  | SFWriteh
  | SFWriteo
  | SFMonitor
  | SFMonitorb
  | SFMonitorh
  | SFMonitoro
  | SFMonitoroff
  | SFMonitoron
  | SFFclose
  | SFFdisplay
  | SFFdisplayb
  | SFFdisplayh
  | SFFdisplayo
  | SFFstrobe
  | SFFstrobeb
  | SFFstrobeh
  | SFFstrobeo
  | SFSwrite
  | SFSwriteb
  | SFSwriteh
  | SFSwriteo
  | SFFscanf
  | SFFread
  | SFFseek
  | SFFflush
  | SFFeof
  | SFSdfannotate
  | SFFopen
  | SFFwrite
  | SFFwriteb
  | SFFwriteh
  | SFFwriteo
  | SFFmonitor
  | SFFmonitorb
  | SFFmonitorh
  | SFFmonitoro
  | SFSformat
  | SFFgetc
  | SFUngetc
  | SFFgets
  | SFSscanf
  | SFRewind
  | SFFtell
  | SFFerror
  | SFReadmemb
  | SFReadmemh
  | SFPrinttimescale
  | SFTimeformat
  | SFFinish
  | SFStop
  | SFQinitialize
  | SFQremove
  | SFQexam
  | SFQadd
  | SFQfull
  | SFRealtime
  | SFTime
  | SFStime
  | SFBitstoreal
  | SFItor
  | SFSigned
  | SFRealtobits
  | SFRtoi
  | SFUnsigned
  | SFRandom
  | SFDisterlang
  | SFDistnormal
  | SFDistt
  | SFDistchisquare
  | SFDistexponential
  | SFDistpoisson
  | SFDistuniform
  | SFClog2
  | SFLn
  | SFLog10
  | SFExp
  | SFSqrt
  | SFPow
  | SFFloor
  | SFCeil
  | SFSin
  | SFCos
  | SFTan
  | SFAsin
  | SFAcos
  | SFAtan
  | SFAtan2
  | SFHypot
  | SFSinh
  | SFCosh
  | SFTanh
  | SFAsinh
  | SFAcosh
  | SFAtanh
  | SFTestplusargs
  | SFValueplusargs
  | SFPla
      { _sfpSync :: !Bool,
        _sfpLogic :: !Logic,
        _sfpPla_arr :: !Bool
      }
  | SFSVPast
  | SFSVStable
  | SFSVRose
  | SFSVFell
  deriving (Eq, Data)

instance Show SystemFunction where
  show x = case x of
    SFDisplay -> "display"
    SFDisplayb -> "displayb"
    SFDisplayh -> "displayh"
    SFDisplayo -> "displayo"
    SFStrobe -> "strobe"
    SFStrobeb -> "strobeb"
    SFStrobeh -> "strobeh"
    SFStrobeo -> "strobeo"
    SFWrite -> "write"
    SFWriteb -> "writeb"
    SFWriteh -> "writeh"
    SFWriteo -> "writeo"
    SFMonitor -> "monitor"
    SFMonitorb -> "monitorb"
    SFMonitorh -> "monitorh"
    SFMonitoro -> "monitoro"
    SFMonitoroff -> "monitoroff"
    SFMonitoron -> "monitoron"
    SFFclose -> "fclose"
    SFFdisplay -> "fdisplay"
    SFFdisplayb -> "fdisplayb"
    SFFdisplayh -> "fdisplayh"
    SFFdisplayo -> "fdisplayo"
    SFFstrobe -> "fstrobe"
    SFFstrobeb -> "fstrobeb"
    SFFstrobeh -> "fstrobeh"
    SFFstrobeo -> "fstrobeo"
    SFSwrite -> "swrite"
    SFSwriteb -> "swriteb"
    SFSwriteh -> "swriteh"
    SFSwriteo -> "swriteo"
    SFFscanf -> "fscanf"
    SFFread -> "fread"
    SFFseek -> "fseek"
    SFFflush -> "fflush"
    SFFeof -> "feof"
    SFSdfannotate -> "sdf_annotate"
    SFFopen -> "fopen"
    SFFwrite -> "fwrite"
    SFFwriteb -> "fwriteb"
    SFFwriteh -> "fwriteh"
    SFFwriteo -> "fwriteo"
    SFFmonitor -> "fmonitor"
    SFFmonitorb -> "fmonitorb"
    SFFmonitorh -> "fmonitorh"
    SFFmonitoro -> "fmonitoro"
    SFSformat -> "sformat"
    SFFgetc -> "fgetc"
    SFUngetc -> "ungetc"
    SFFgets -> "gets"
    SFSscanf -> "sscanf"
    SFRewind -> "rewind"
    SFFtell -> "ftell"
    SFFerror -> "ferror"
    SFReadmemb -> "readmemb"
    SFReadmemh -> "readmemh"
    SFPrinttimescale -> "printtimescale"
    SFTimeformat -> "timeformat"
    SFFinish -> "finish"
    SFStop -> "stop"
    SFQinitialize -> "q_initialize"
    SFQremove -> "q_remove"
    SFQexam -> "q_exam"
    SFQadd -> "q_add"
    SFQfull -> "q_full"
    SFRealtime -> "realtime"
    SFTime -> "time"
    SFStime -> "stime"
    SFBitstoreal -> "bitstoreal"
    SFItor -> "itor"
    SFSigned -> "signed"
    SFRealtobits -> "realtobits"
    SFRtoi -> "rtoi"
    SFUnsigned -> "unsigned"
    SFRandom -> "random"
    SFDisterlang -> "dist_erlang"
    SFDistnormal -> "dist_normal"
    SFDistt -> "dist_t"
    SFDistchisquare -> "dist_chi_square"
    SFDistexponential -> "dist_exponential"
    SFDistpoisson -> "dist_poisson"
    SFDistuniform -> "dist_uniform"
    SFClog2 -> "clog2"
    SFLn -> "ln"
    SFLog10 -> "log10"
    SFExp -> "exp"
    SFSqrt -> "sqrt"
    SFPow -> "pow"
    SFFloor -> "floor"
    SFCeil -> "ceil"
    SFSin -> "sin"
    SFCos -> "cos"
    SFTan -> "tan"
    SFAsin -> "asin"
    SFAcos -> "acos"
    SFAtan -> "atan"
    SFAtan2 -> "atan2"
    SFHypot -> "hypot"
    SFSinh -> "sinh"
    SFCosh -> "cosh"
    SFTanh -> "tanh"
    SFAsinh -> "asinh"
    SFAcosh -> "acosh"
    SFAtanh -> "atanh"
    SFTestplusargs -> "test$plusargs"
    SFValueplusargs -> "value$plusargs"
    SFPla {_sfpSync = True, _sfpLogic = LAnd, _sfpPla_arr = False} -> "sync$and$array"
    SFPla {_sfpSync = True, _sfpLogic = LAnd, _sfpPla_arr = True} -> "sync$and$plane"
    SFPla {_sfpSync = True, _sfpLogic = LOr, _sfpPla_arr = False} -> "sync$or$array"
    SFPla {_sfpSync = True, _sfpLogic = LOr, _sfpPla_arr = True} -> "sync$or$plane"
    SFPla {_sfpSync = True, _sfpLogic = LNand, _sfpPla_arr = False} -> "sync$nand$array"
    SFPla {_sfpSync = True, _sfpLogic = LNand, _sfpPla_arr = True} -> "sync$nand$plane"
    SFPla {_sfpSync = True, _sfpLogic = LNor, _sfpPla_arr = False} -> "sync$nor$array"
    SFPla {_sfpSync = True, _sfpLogic = LNor, _sfpPla_arr = True} -> "sync$nor$plane"
    SFPla {_sfpSync = False, _sfpLogic = LAnd, _sfpPla_arr = False} -> "async$and$array"
    SFPla {_sfpSync = False, _sfpLogic = LAnd, _sfpPla_arr = True} -> "async$and$plane"
    SFPla {_sfpSync = False, _sfpLogic = LOr, _sfpPla_arr = False} -> "async$or$array"
    SFPla {_sfpSync = False, _sfpLogic = LOr, _sfpPla_arr = True} -> "async$or$plane"
    SFPla {_sfpSync = False, _sfpLogic = LNand, _sfpPla_arr = False} -> "async$nand$array"
    SFPla {_sfpSync = False, _sfpLogic = LNand, _sfpPla_arr = True} -> "async$nand$plane"
    SFPla {_sfpSync = False, _sfpLogic = LNor, _sfpPla_arr = False} -> "async$nor$array"
    SFPla {_sfpSync = False, _sfpLogic = LNor, _sfpPla_arr = True} -> "async$nor$plane"
    SFSVPast -> "past"
    SFSVStable -> "stable"
    SFSVRose -> "rose"
    SFSVFell -> "fell"

sfMap :: HashMap.HashMap ByteString SystemFunction
sfMap =
  HashMap.fromList
    [ ("display", SFDisplay),
      ("displayb", SFDisplayb),
      ("displayh", SFDisplayh),
      ("displayo", SFDisplayo),
      ("strobe", SFStrobe),
      ("strobeb", SFStrobeb),
      ("strobeh", SFStrobeh),
      ("strobeo", SFStrobeo),
      ("write", SFWrite),
      ("writeb", SFWriteb),
      ("writeh", SFWriteh),
      ("writeo", SFWriteo),
      ("monitor", SFMonitor),
      ("monitorb", SFMonitorb),
      ("monitorh", SFMonitorh),
      ("monitoro", SFMonitoro),
      ("monitoroff", SFMonitoroff),
      ("monitoron", SFMonitoron),
      ("fclose", SFFclose),
      ("fdisplay", SFFdisplay),
      ("fdisplayb", SFFdisplayb),
      ("fdisplayh", SFFdisplayh),
      ("fdisplayo", SFFdisplayo),
      ("fstrobe", SFFstrobe),
      ("fstrobeb", SFFstrobeb),
      ("fstrobeh", SFFstrobeh),
      ("fstrobeo", SFFstrobeo),
      ("swrite", SFSwrite),
      ("swriteb", SFSwriteb),
      ("swriteh", SFSwriteh),
      ("swriteo", SFSwriteo),
      ("fscanf", SFFscanf),
      ("fread", SFFread),
      ("fseek", SFFseek),
      ("fflush", SFFflush),
      ("feof", SFFeof),
      ("sdf_annotate", SFSdfannotate),
      ("fopen", SFFopen),
      ("fwrite", SFFwrite),
      ("fwriteb", SFFwriteb),
      ("fwriteh", SFFwriteh),
      ("fwriteo", SFFwriteo),
      ("fmonitor", SFFmonitor),
      ("fmonitorb", SFFmonitorb),
      ("fmonitorh", SFFmonitorh),
      ("fmonitoro", SFFmonitoro),
      ("sformat", SFSformat),
      ("fgetc", SFFgetc),
      ("ungetc", SFUngetc),
      ("gets", SFFgets),
      ("sscanf", SFSscanf),
      ("rewind", SFRewind),
      ("ftell", SFFtell),
      ("ferror", SFFerror),
      ("readmemb", SFReadmemb),
      ("readmemh", SFReadmemh),
      ("printtimescale", SFPrinttimescale),
      ("timeformat", SFTimeformat),
      ("finish", SFFinish),
      ("stop", SFStop),
      ("q_initialize", SFQinitialize),
      ("q_remove", SFQremove),
      ("q_exam", SFQexam),
      ("q_add", SFQadd),
      ("q_full", SFQfull),
      ("realtime", SFRealtime),
      ("time", SFTime),
      ("stime", SFStime),
      ("bitstoreal", SFBitstoreal),
      ("itor", SFItor),
      ("signed", SFSigned),
      ("realtobits", SFRealtobits),
      ("rtoi", SFRtoi),
      ("unsigned", SFUnsigned),
      ("random", SFRandom),
      ("dist_erlang", SFDisterlang),
      ("dist_normal", SFDistnormal),
      ("dist_t", SFDistt),
      ("dist_chi_square", SFDistchisquare),
      ("dist_exponential", SFDistexponential),
      ("dist_poisson", SFDistpoisson),
      ("dist_uniform", SFDistuniform),
      ("clog2", SFClog2),
      ("ln", SFLn),
      ("log10", SFLog10),
      ("exp", SFExp),
      ("sqrt", SFSqrt),
      ("pow", SFPow),
      ("floor", SFFloor),
      ("ceil", SFCeil),
      ("sin", SFSin),
      ("cos", SFCos),
      ("tan", SFTan),
      ("asin", SFAsin),
      ("acos", SFAcos),
      ("atan", SFAtan),
      ("atan2", SFAtan2),
      ("hypot", SFHypot),
      ("sinh", SFSinh),
      ("cosh", SFCosh),
      ("tanh", SFTanh),
      ("asinh", SFAsinh),
      ("acosh", SFAcosh),
      ("atanh", SFAtanh),
      ("test$plusargs", SFTestplusargs),
      ("value$plusargs", SFValueplusargs),
      ("sync$and$array", SFPla {_sfpSync = True, _sfpLogic = LAnd, _sfpPla_arr = False}),
      ("sync$and$plane", SFPla {_sfpSync = True, _sfpLogic = LAnd, _sfpPla_arr = True}),
      ("sync$or$array", SFPla {_sfpSync = True, _sfpLogic = LOr, _sfpPla_arr = False}),
      ("sync$or$plane", SFPla {_sfpSync = True, _sfpLogic = LOr, _sfpPla_arr = True}),
      ("sync$nand$array", SFPla {_sfpSync = True, _sfpLogic = LNand, _sfpPla_arr = False}),
      ("sync$nand$plane", SFPla {_sfpSync = True, _sfpLogic = LNand, _sfpPla_arr = True}),
      ("sync$nor$array", SFPla {_sfpSync = True, _sfpLogic = LNor, _sfpPla_arr = False}),
      ("sync$nor$plane", SFPla {_sfpSync = True, _sfpLogic = LNor, _sfpPla_arr = True}),
      ("async$and$array", SFPla {_sfpSync = False, _sfpLogic = LAnd, _sfpPla_arr = False}),
      ("async$and$plane", SFPla {_sfpSync = False, _sfpLogic = LAnd, _sfpPla_arr = True}),
      ("async$or$array", SFPla {_sfpSync = False, _sfpLogic = LOr, _sfpPla_arr = False}),
      ("async$or$plane", SFPla {_sfpSync = False, _sfpLogic = LOr, _sfpPla_arr = True}),
      ("async$nand$array", SFPla {_sfpSync = False, _sfpLogic = LNand, _sfpPla_arr = False}),
      ("async$nand$plane", SFPla {_sfpSync = False, _sfpLogic = LNand, _sfpPla_arr = True}),
      ("async$nor$array", SFPla {_sfpSync = False, _sfpLogic = LNor, _sfpPla_arr = False}),
      ("async$nor$plane", SFPla {_sfpSync = False, _sfpLogic = LNor, _sfpPla_arr = True}),
      ("past", SFSVPast),
      ("stable", SFSVStable),
      ("rose", SFSVRose),
      ("fell", SFSVFell)
    ]
