-- Module      : Verismith.Config
-- Description : TOML Configuration file format and parser.
-- Copyright   : (c) 2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Verismith.Config
  ( -- * TOML Configuration
    -- $conf
    Config (..),
    defaultConfig,

    -- ** Probabilities
    Probability (..),

    -- *** Expression
    ProbExpr (..),

    -- *** Module Item
    ProbModItem (..),

    -- *** Statement
    ProbStatement (..),

    -- *** Module
    ProbMod (..),

    -- ** EMI Configuration
    ConfEMI (..),

    -- ** ConfProperty
    ConfProperty (..),

    -- ** ConfGarbage
    NumberProbability (..),
    CategoricalProbability (..),
    uniformCP,
    GarbageOpts (..),
    GarbageConfigOpts (..),
    GarbagePrimitiveOpts (..),
    GarbageModuleOpts (..),
    GarbageSpecifyOpts (..),
    GarbageSpecifyPathOpts (..),
    GarbageSpecifyTimingCheckOpts (..),
    GarbageGenerateOpts (..),
    GarbageTypeOpts (..),
    GarbageAttenuationOpts (..),
    GarbageStatementOpts (..),
    GarbageExprOpts (..),
    GarbageIdentifierOpts (..),
    defGarbageOpts,

    -- ** Simulator Description
    SimDescription (..),

    -- ** Synthesiser Description
    SynthDescription (..),

    -- * Useful Lenses
    fromXST,
    fromYosys,
    fromVivado,
    fromQuartus,
    fromQuartusLight,
    configEMI,
    configProbability,
    configGarbageGenerator,
    configProperty,
    configSimulators,
    configSynthesisers,
    confEMIGenerateProb,
    confEMINoGenerateProb,
    probModItem,
    probMod,
    probModDropOutput,
    probModKeepOutput,
    probStmnt,
    probExpr,
    probExprNum,
    probExprId,
    probExprRangeSelect,
    probExprUnOp,
    probExprBinOp,
    probExprCond,
    probExprConcat,
    probExprStr,
    probExprSigned,
    probExprUnsigned,
    probModItemAssign,
    probModItemSeqAlways,
    probModItemCombAlways,
    probModItemInst,
    probStmntBlock,
    probStmntNonBlock,
    probStmntCond,
    probStmntFor,
    propSampleSize,
    propSampleMethod,
    propSize,
    propSeed,
    propStmntDepth,
    propModDepth,
    propMaxModules,
    propCombine,
    propDeterminism,
    propNonDeterminism,
    propDefaultYosys,
    goSeed,
    gaoCurrent,
    goGenerate,
    goStatement,
    goExpr,
    ggoAttenuation,
    geoAttenuation,
    gstoAttenuation,
    parseConfigFile,
    parseConfig,
    parseConfigFileRelaxed,
    parseConfigRelaxed,
    encodeConfig,
    encodeConfigFile,
    versionInfo,
  )
where

import Control.Applicative (Alternative, liftA2, liftA3, (<|>))
import Control.Lens hiding ((.=))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as VU
import Data.Version (showVersion)
import Data.Word (Word32)
import Development.GitRev
import Hedgehog.Internal.Seed (Seed)
import Paths_verismith (version)
import Shelly (toTextIgnore)
import Toml (TomlCodec, (.=))
import qualified Toml
import Verismith.Tool.Quartus
import Verismith.Tool.QuartusLight
import Verismith.Tool.Vivado
import Verismith.Tool.XST
import Verismith.Tool.Yosys
import Verismith.Utils (uncurry3)

-- $conf
--
-- Verismith supports a TOML configuration file that can be passed using the @-c@
-- flag or using the 'parseConfig' and 'encodeConfig' functions. The
-- configuration can then be manipulated using the lenses that are also provided
-- in this module.
--
-- The configuration file can be used to tweak the random Verilog generation by
-- passing different probabilities to each of the syntax nodes in the AST. It
-- can also be used to specify which simulators to fuzz with which options. A
-- seed for the run can also be set, to replay a previous run using the same
-- exact generation. A default value is associated with each key in the
-- configuration file, which means that only the options that need overriding
-- can be added to the configuration. The defaults can be observed in
-- 'defaultConfig' or when running @verismith config@.
--
-- == Configuration Sections
--
-- There are four main configuration sections in the TOML file:
--
--   [@probability@] The @probability@ section defines the probabilities at
--   every node in the AST.
--
--   [@property@] Controls different properties of the generation, such as
--   adding a seed or the depth of the statements.
--
--   [@simulator@] This is an array of tables containing descriptions of the
--   different simulators that should be used. It currently only supports
--   <http://iverilog.icarus.com/ Icarus Verilog>.
--
--   [@synthesiser@] This is also an array of tables containing descriptions of
--   the different synthesisers that should be used. The synthesisers that are
--   currently supported are:
--
--     - <https://www.intel.com/content/www/us/en/programmable/downloads/download-center.html Quartus>
--     - <https://www.xilinx.com/products/design-tools/ise-design-suite.html ISE Design Suite>
--     - <https://www.xilinx.com/products/design-tools/ise-design-suite.html Vivado Design Suite>
--     - <http://www.clifford.at/yosys/ Yosys Open SYnthesis Suite>

-- | Probability of different expressions nodes.
data ProbExpr = ProbExpr
  { -- | @expr.number@: probability of generation a number like
    -- @4'ha@. This should never be set to 0, as it is used
    -- as a fallback in case there are no viable
    -- identifiers, such as none being in scope.
    _probExprNum :: {-# UNPACK #-} !Int,
    -- | @expr.variable@: probability of generating an identifier that is in
    -- scope and of the right type.
    _probExprId :: {-# UNPACK #-} !Int,
    -- | @expr.rangeselect@: probability of generating a range
    -- selection from a port (@reg1[2:0]@).
    _probExprRangeSelect :: {-# UNPACK #-} !Int,
    -- | @expr.unary@: probability of generating a unary operator.
    _probExprUnOp :: {-# UNPACK #-} !Int,
    -- | @expr.binary@: probability of generation a binary operator.
    _probExprBinOp :: {-# UNPACK #-} !Int,
    -- | @expr.ternary@: probability of generating a conditional ternary
    -- operator.
    _probExprCond :: {-# UNPACK #-} !Int,
    -- | @expr.concatenation@: probability of generating a concatenation.
    _probExprConcat :: {-# UNPACK #-} !Int,
    -- | @expr.string@: probability of generating a string. This is not
    -- fully supported therefore currently cannot be set.
    _probExprStr :: {-# UNPACK #-} !Int,
    -- | @expr.signed@: probability of generating a signed function
    -- @$signed(...)@.
    _probExprSigned :: {-# UNPACK #-} !Int,
    -- | @expr.unsigned@: probability of generating an unsigned function
    -- @$unsigned(...)@.
    _probExprUnsigned :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Probability of generating different nodes inside a module declaration.
data ProbModItem = ProbModItem
  { -- | @moditem.assign@: probability of generating an @assign@.
    _probModItemAssign :: {-# UNPACK #-} !Int,
    -- | @moditem.sequential@: probability of generating a sequential @always@ block.
    _probModItemSeqAlways :: {-# UNPACK #-} !Int,
    -- | @moditem.combinational@: probability of generating an combinational @always@
    -- block. This is currently not implemented.
    _probModItemCombAlways :: {-# UNPACK #-} !Int,
    -- | @moditem.instantiation@: probability of generating a module
    -- instantiation.
    _probModItemInst :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Probability of generating different statements.
data ProbStatement = ProbStatement
  { -- | @statement.blocking@: probability of generating blocking assignments.
    _probStmntBlock :: {-# UNPACK #-} !Int,
    -- | @statement.nonblocking@: probability of generating nonblocking assignments.
    _probStmntNonBlock :: {-# UNPACK #-} !Int,
    -- | @statement.conditional@: probability of generating conditional
    -- statements (@if@ statements).
    _probStmntCond :: {-# UNPACK #-} !Int,
    -- | @statement.forloop@: probability of generating for loops.
    _probStmntFor :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Probability of generating various properties of a module.
data ProbMod = ProbMod
  { -- | "@module.drop_output@: frequency of a wire or register being dropped from the output."
    _probModDropOutput :: {-# UNPACK #-} !Int,
    -- | "@module.keep_output@: frequency of a wire or register being kept in the output."
    _probModKeepOutput :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | @[probability]@: combined probabilities.
data Probability = Probability
  { -- | Probabilities for module items.
    _probModItem :: {-# UNPACK #-} !ProbModItem,
    -- | Probabilities for statements.
    _probStmnt :: {-# UNPACK #-} !ProbStatement,
    -- | Probaiblities for expressions.
    _probExpr :: {-# UNPACK #-} !ProbExpr,
    _probMod :: {-# UNPACK #-} !ProbMod
  }
  deriving (Eq, Show)

data ConfEMI = ConfEMI
  { -- | Probability of generating a new EMI statement with a new EMI entry.
    _confEMIGenerateProb :: {-# UNPACK #-} !Int,
    _confEMINoGenerateProb :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | @[property]@: properties for the generated Verilog file.
data ConfProperty = ConfProperty
  { -- | @size@: the size of the generated Verilog.
    _propSize :: {-# UNPACK #-} !Int,
    -- | @seed@: a possible seed that could be used to
    -- generate the same Verilog.
    _propSeed :: !(Maybe Seed),
    -- | @statement.depth@: the maximum statement depth that should be
    -- reached.
    _propStmntDepth :: {-# UNPACK #-} !Int,
    -- | @module.depth@: the maximium module depth that should be
    -- reached.
    _propModDepth :: {-# UNPACK #-} !Int,
    -- | @module.max@: the maximum number of modules that are
    -- allowed to be created at each level.
    _propMaxModules :: {-# UNPACK #-} !Int,
    -- | @sample.method@: the sampling method that should be used to
    -- generate specific distributions of random
    -- programs.
    _propSampleMethod :: !Text,
    -- | @sample.size@: the number of samples to take for the
    -- sampling method.
    _propSampleSize :: {-# UNPACK #-} !Int,
    -- | @output.combine@: if the output should be combined into one
    -- bit or not.
    _propCombine :: !Bool,
    -- | @nondeterminism@: the frequency at which nondeterminism
    -- should be generated (currently a work in progress).
    _propNonDeterminism :: {-# UNPACK #-} !Int,
    -- | @determinism@: the frequency at which determinism should
    -- be generated (currently modules are always deterministic).
    _propDeterminism :: {-# UNPACK #-} !Int,
    -- | @default.yosys@: Default location for Yosys, which will be used for
    -- equivalence checking.
    _propDefaultYosys :: !(Maybe Text)
  }
  deriving (Eq, Show)

data NumberProbability
  = NPUniform
      { _NPULow :: !Int,
        _NPUHigh :: !Int
      }
  | NPBinomial
      { _NPBOffset :: !Int,
        _NPBTrials :: !Int,
        _NPBSuccess :: !Double
      }
  | NPNegativeBinomial -- Geometric is negative binomial with 1 failure
      { _NPNBOffset :: !Int,
        _NPNBFailRate :: !Double,
        _NPNBFailure :: !Int
      }
  | NPPoisson
      { _NPPOffset :: !Int,
        _NPPParam :: !Double
      }
  | NPDiscrete !(NonEmpty (Double, Int)) -- Weight, outcome index
  | NPLinearComb !(NonEmpty (Double, NumberProbability))
  deriving (Eq, Show)

data CategoricalProbability
  = CPDiscrete !(NonEmpty Double)
  | CPBiasedUniform
      { _CPBUBiases :: ![(Double, Int)],
        _CPBUUniformWeight :: Double
      }
  deriving (Eq, Show)

uniformCP :: CategoricalProbability
uniformCP = CPBiasedUniform [] 1

data GarbageOpts = GarbageOpts
  { _goSeed :: !(Maybe (VU.Vector Word32)),
    _goConfig :: !GarbageConfigOpts,
    _goPrimitive :: !GarbagePrimitiveOpts,
    _goModule :: !GarbageModuleOpts,
    _goSpecify :: !GarbageSpecifyOpts,
    _goGenerate :: !GarbageGenerateOpts,
    _goType :: !GarbageTypeOpts,
    _goStatement :: !GarbageStatementOpts,
    _goExpr :: !GarbageExprOpts,
    _goIdentifier :: !GarbageIdentifierOpts,
    _goDriveStrength :: !CategoricalProbability,
    _goLValues :: !NumberProbability,
    _goOptionalLValue :: !Double,
    _goAttributes :: !NumberProbability,
    _goAttributeOptionalValue :: !Double,
    _goDelay :: !CategoricalProbability,
    _goIntRealIdent :: !CategoricalProbability,
    _goPathDepth :: !NumberProbability,
    _goBareMinTypMax :: !Double
  }
  deriving (Eq, Show)

data GarbageConfigOpts = GarbageConfigOpts
  { _gcoBlocks :: !NumberProbability,
    _gcoDesigns :: !NumberProbability,
    _gcoItems :: !NumberProbability,
    _gcoLibraries :: !NumberProbability,
    _gcoCell_Inst :: !Double,
    _gcoLiblist_Use :: !Double,
    _gcoConfig :: !Double,
    _gcoLibraryScope :: !Double
  }
  deriving (Eq, Show)

data GarbagePrimitiveOpts = GarbagePrimitiveOpts
  { _gpoBlocks :: !NumberProbability,
    _gpoPorts :: !NumberProbability,
    _gpoPortType :: !CategoricalProbability,
    _gpoSeq_Comb :: !Double,
    _gpoRegInit :: !Double,
    _gpoCombInit :: !CategoricalProbability,
    _gpoTableRows :: !NumberProbability,
    _gpoInLevel :: !CategoricalProbability,
    _gpoOutLevel :: !CategoricalProbability,
    _gpoEdgeSensitive :: !Double,
    _gpoEdgeSimplePosNeg :: !CategoricalProbability,
    _gpoOutputNoChange :: !Double
  }
  deriving (Eq, Show)

data GarbageModuleOpts = GarbageModuleOpts
  { _gmoBlocks :: !NumberProbability,
    _gmoNamed_Positional :: !Double,
    _gmoParameters :: !NumberProbability,
    _gmoOptionalParameter :: !Double,
    _gmoPorts :: !NumberProbability,
    _gmoPortLValues :: !NumberProbability,
    _gmoPortRange :: !Double,
    _gmoPortDir :: !CategoricalProbability,
    _gmoOptionalPort :: !Double,
    _gmoItems :: !NumberProbability,
    _gmoItem :: !CategoricalProbability,
    _gmoMacro :: !Double,
    _gmoTimeScale :: !Double,
    _gmoTimeMagnitude :: !CategoricalProbability,
    _gmoCell :: !Double,
    _gmoUnconnectedDrive :: !CategoricalProbability,
    _gmoDefaultNetType :: !CategoricalProbability,
    _gmoNonAsciiHeader :: !Bool
  }
  deriving (Eq, Show)

data GarbageSpecifyOpts = GarbageSpecifyOpts
  { _gsyoPath :: !GarbageSpecifyPathOpts,
    _gsyoTimingCheck :: !GarbageSpecifyTimingCheckOpts,
    _gsyoItems :: !NumberProbability,
    _gsyoItem :: !CategoricalProbability,
    _gsyoTermRange :: !Double,
    _gsyoParamRange :: !Double,
    _gsyoPathPulseEscaped_Simple :: !Double,
    _gsyoPathPulseRange :: !Double
  }
  deriving (Eq, Show)

data GarbageSpecifyPathOpts = GarbageSpecifyPathOpts
  { _gspoCondition :: !CategoricalProbability,
    _gspoFull_Parallel :: !Double,
    _gspoEdgeSensitive :: !Double,
    _gspoFullSources :: !NumberProbability,
    _gspoFullDestinations :: !NumberProbability,
    _gspoPolarity :: !CategoricalProbability,
    _gspoEdgeSensitivity :: !CategoricalProbability,
    _gspoDelayKind :: !CategoricalProbability
  }
  deriving (Eq, Show)

data GarbageSpecifyTimingCheckOpts = GarbageSpecifyTimingCheckOpts
  { _gstcoOptionalArg :: !Double,
    _gstcoEvent :: !Double,
    _gstcoEventEdge :: !Double,
    _gstcoCondition :: !Double,
    _gstcoCondNeg_Pos :: !Double,
    _gstcoDelayedMinTypMax :: !Double
  }
  deriving (Eq, Show)

data GarbageGenerateOpts = GarbageGenerateOpts
  { _ggoAttenuation :: !GarbageAttenuationOpts,
    _ggoItems :: !NumberProbability,
    _ggoItem :: !CategoricalProbability,
    _ggoOptionalBlock :: !Double,
    _ggoInstOptionalDelay :: !Double,
    _ggoInstOptionalRange :: !Double,
    _ggoPrimitiveOptIdent :: !Double,
    _ggoCondBlock :: !CategoricalProbability,
    _ggoNetType :: !CategoricalProbability,
    _ggoNetRange :: !Double,
    _ggoNetVectoring :: !CategoricalProbability,
    _ggoDeclItem :: !CategoricalProbability,
    _ggoDeclDim_Init :: !Double,
    _ggoChargeStrength :: !CategoricalProbability,
    _ggoTaskFunAutomatic :: !Double,
    _ggoTaskFunDecl :: !CategoricalProbability,
    _ggoTaskFunRegister :: !Double,
    _ggoTaskFunPorts :: !NumberProbability,
    _ggoTaskFunPortType :: !CategoricalProbability,
    _ggoTaskPortDirection :: !CategoricalProbability,
    _ggoFunRetType :: !Double,
    _ggoGateInst :: !CategoricalProbability,
    _ggoGateOptIdent :: !Double,
    _ggoGateNInputType :: !CategoricalProbability,
    _ggoGateInputs :: !NumberProbability,
    _ggoGateOutputs :: !NumberProbability,
    _ggoCaseBranches :: !NumberProbability,
    _ggoCaseBranchPatterns :: !NumberProbability
  }
  deriving (Eq, Show)

data GarbageTypeOpts = GarbageTypeOpts
  { _gtoAbstract_Concrete :: !Double,
    _gtoAbstract :: !CategoricalProbability,
    _gtoConcreteSignedness :: !Double,
    _gtoConcreteBitRange :: !Double,
    _gtoDimensions :: !NumberProbability
  }
  deriving (Eq, Show)

data GarbageAttenuationOpts = GarbageAttenuationOpts
  { _gaoCurrent :: !Double,
    _gaoDecrease :: !Double
  }
  deriving (Eq, Show)

data GarbageStatementOpts = GarbageStatementOpts
  { _gstoAttenuation :: !GarbageAttenuationOpts,
    _gstoOptional :: !Double,
    _gstoItem :: !CategoricalProbability,
    _gstoItems :: !NumberProbability,
    _gstoOptionalDelEvCtl :: !Double,
    _gstoAssignmentBlocking :: !Double,
    _gstoCase :: !CategoricalProbability,
    _gstoCaseBranches :: !NumberProbability,
    _gstoCaseBranchPatterns :: !NumberProbability,
    _gstoLoop :: !CategoricalProbability,
    _gstoBlockPar_Seq :: !Double,
    _gstoBlockHeader :: !Double,
    _gstoBlockDecls :: !NumberProbability,
    _gstoBlockDecl :: !CategoricalProbability,
    _gstoProcContAssign :: !CategoricalProbability,
    _gstoPCAVar_Net :: !Double,
    _gstoDelayEventRepeat :: !CategoricalProbability,
    _gstoEvent :: !CategoricalProbability,
    _gstoEvents :: !NumberProbability,
    _gstoEventPrefix :: !CategoricalProbability,
    _gstoSysTaskPorts :: !NumberProbability,
    _gstoSysTaskOptionalPort :: !Double
  }
  deriving (Eq, Show)

data GarbageExprOpts = GarbageExprOpts
  { _geoAttenuation :: !GarbageAttenuationOpts,
    _geoItem :: !CategoricalProbability,
    _geoPrimary :: !CategoricalProbability,
    _geoUnary :: !CategoricalProbability,
    _geoBinary :: !CategoricalProbability,
    _geoMinTypMax :: !Double,
    _geoDimRange :: !Double,
    _geoRange :: !CategoricalProbability,
    _geoRangeOffsetPos_Neg :: !Double,
    _geoConcatenations :: !NumberProbability,
    _geoSysFunArgs :: !NumberProbability,
    _geoLiteralWidth :: !CategoricalProbability,
    _geoLiteralSigned :: !Double,
    _geoStringCharacters :: !NumberProbability,
    _geoStringCharacter :: !CategoricalProbability,
    _geoFixed_Floating :: !Double,
    _geoExponentSign :: !CategoricalProbability,
    _geoX_Z :: !Double,
    _geoBinarySymbols :: !NumberProbability,
    _geoBinarySymbol :: !CategoricalProbability,
    _geoOctalSymbols :: !NumberProbability,
    _geoOctalSymbol :: !CategoricalProbability,
    _geoDecimalSymbols :: !NumberProbability,
    _geoDecimalSymbol :: !CategoricalProbability,
    _geoHexadecimalSymbols :: !NumberProbability,
    _geoHexadecimalSymbol :: !CategoricalProbability
  }
  deriving (Eq, Show)

data GarbageIdentifierOpts = GarbageIdentifierOpts
  { _gioEscaped_Simple :: !Double,
    _gioSimpleLetters :: !NumberProbability,
    _gioSimpleLetter :: !CategoricalProbability,
    _gioEscapedLetters :: !NumberProbability,
    _gioEscapedLetter :: !CategoricalProbability,
    _gioSystemLetters :: !NumberProbability,
    _gioSystemFirstLetter :: !CategoricalProbability
  }
  deriving (Eq, Show)

defAttenuationOpts :: GarbageAttenuationOpts
defAttenuationOpts = GarbageAttenuationOpts 1.0 0.7

defGarbageOpts :: GarbageOpts
defGarbageOpts =
  GarbageOpts
    { _goSeed = Nothing,
      _goConfig = GarbageConfigOpts
        { _gcoBlocks = NPPoisson 0 1,
          _gcoDesigns = NPPoisson 0 1,
          _gcoItems = NPPoisson 0 1,
          _gcoLibraries = NPPoisson 0 1,
          _gcoCell_Inst = 0.5,
          _gcoLiblist_Use = 0.5,
          _gcoConfig = 0.5,
          _gcoLibraryScope = 0.5
        },
      _goPrimitive = GarbagePrimitiveOpts
        { _gpoBlocks = NPPoisson 0 2,
          _gpoPorts = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _gpoPortType = uniformCP,
          _gpoSeq_Comb = 0.5,
          _gpoRegInit = 0.5,
          _gpoCombInit = uniformCP,
          _gpoTableRows = NPPoisson 0 4,
          _gpoInLevel = uniformCP,
          _gpoOutLevel = uniformCP,
          _gpoEdgeSensitive = 0.5,
          _gpoEdgeSimplePosNeg = uniformCP,
          _gpoOutputNoChange = 0.5
        },
      _goModule = GarbageModuleOpts
        { _gmoBlocks = NPPoisson 1 2,
          _gmoNamed_Positional = 0.5,
          _gmoParameters = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _gmoOptionalParameter = 0.5,
          _gmoPorts = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _gmoPortLValues = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _gmoPortRange = 0.5,
          _gmoPortDir = uniformCP,
          _gmoOptionalPort = 0.5,
          _gmoItems = NPPoisson 0 3,
          _gmoItem = CPDiscrete [6, 2, 2, 3, 2, 1, 1, 1],
          _gmoMacro = 0.5,
          _gmoTimeScale = 0.5,
          _gmoTimeMagnitude = uniformCP,
          _gmoCell = 0.5,
          _gmoUnconnectedDrive = uniformCP,
          _gmoDefaultNetType = uniformCP,
          _gmoNonAsciiHeader = True
        },
      _goSpecify = GarbageSpecifyOpts
        { _gsyoPath = GarbageSpecifyPathOpts
            { _gspoCondition = uniformCP,
              _gspoFull_Parallel = 0.5,
              _gspoEdgeSensitive = 0.5,
              _gspoFullSources = NPPoisson 0 1,
              _gspoFullDestinations = NPPoisson 0 1,
              _gspoPolarity = uniformCP,
              _gspoEdgeSensitivity = uniformCP,
              _gspoDelayKind = uniformCP
            },
          _gsyoTimingCheck = GarbageSpecifyTimingCheckOpts
            { _gstcoOptionalArg = 0.5,
              _gstcoEvent = 0.5,
              _gstcoEventEdge = 0.25,
              _gstcoCondition = 0.5,
              _gstcoCondNeg_Pos = 0.5,
              _gstcoDelayedMinTypMax = 0.5
            },
          _gsyoItems = NPPoisson 0 1,
          _gsyoItem = uniformCP,
          _gsyoTermRange = 0.5,
          _gsyoParamRange = 0.5,
          _gsyoPathPulseEscaped_Simple = 0.5,
          _gsyoPathPulseRange = 0.5
        },
      _goGenerate = GarbageGenerateOpts
        { _ggoAttenuation = defAttenuationOpts,
          _ggoItems = NPPoisson 0 3,
          _ggoItem = uniformCP,
          _ggoOptionalBlock = 0.5,
          _ggoInstOptionalDelay = 0.5,
          _ggoInstOptionalRange = 0.5,
          _ggoPrimitiveOptIdent = 0.5,
          _ggoCondBlock = uniformCP,
          _ggoNetType = uniformCP,
          _ggoNetRange = 0.5,
          _ggoNetVectoring = uniformCP,
          _ggoDeclItem = uniformCP,
          _ggoDeclDim_Init = 0.5,
          _ggoChargeStrength = uniformCP,
          _ggoTaskFunAutomatic = 0.5,
          _ggoTaskFunDecl = uniformCP,
          _ggoTaskFunRegister = 0.5,
          _ggoTaskFunPorts = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _ggoTaskFunPortType = uniformCP,
          _ggoTaskPortDirection = uniformCP,
          _ggoFunRetType = 0.5,
          _ggoGateInst = uniformCP,
          _ggoGateOptIdent = 0.5,
          _ggoGateNInputType = uniformCP,
          _ggoGateInputs = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _ggoGateOutputs = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _ggoCaseBranches = NPNegativeBinomial 0 0.75 2,
          _ggoCaseBranchPatterns = NPNegativeBinomial 0 (2.0 / 5.0) 1
        },
      _goType = GarbageTypeOpts
        { _gtoAbstract_Concrete = 0.5,
          _gtoAbstract = uniformCP,
          _gtoConcreteSignedness = 0.5,
          _gtoConcreteBitRange = 0.5,
          _gtoDimensions = NPNegativeBinomial 0 0.5 1
        },
      _goStatement = GarbageStatementOpts
        { _gstoAttenuation = defAttenuationOpts,
          _gstoOptional = 0.5,
          _gstoItems = NPPoisson 0 3,
          _gstoItem = uniformCP,
          _gstoOptionalDelEvCtl = 0.5,
          _gstoAssignmentBlocking = 0.5,
          _gstoCase = uniformCP,
          _gstoCaseBranches = NPNegativeBinomial 0 0.75 2,
          _gstoCaseBranchPatterns = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _gstoLoop = uniformCP,
          _gstoBlockPar_Seq = 0.5,
          _gstoBlockHeader = 0.5,
          _gstoBlockDecls = NPPoisson 0 1,
          _gstoBlockDecl = uniformCP,
          _gstoProcContAssign = uniformCP,
          _gstoPCAVar_Net = 0.5,
          _gstoDelayEventRepeat = uniformCP,
          _gstoEvent = uniformCP,
          _gstoEvents = NPNegativeBinomial 0 0.5 1,
          _gstoEventPrefix = uniformCP,
          _gstoSysTaskPorts = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _gstoSysTaskOptionalPort = 0.5
        },
      _goExpr = GarbageExprOpts
        { _geoAttenuation = defAttenuationOpts,
          _geoItem = CPDiscrete [2, 2, 2, 1],
          _geoPrimary = CPDiscrete [2, 4, 4, 4, 4, 4, 2, 4, 1, 1, 1, 1, 1],
          _geoUnary = uniformCP,
          _geoBinary = uniformCP,
          _geoMinTypMax = 0.5,
          _geoDimRange = 0.5,
          _geoRange = uniformCP,
          _geoRangeOffsetPos_Neg = 0.5,
          _geoConcatenations = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _geoSysFunArgs = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          _geoLiteralWidth =
            CPBiasedUniform
              [(1024, 1), (512, 8), (256, 16), (128, 32), (64, 64), (32, 128), (16, 256), (8, 512)]
              1,
          _geoLiteralSigned = 0.5,
          _geoStringCharacters = NPNegativeBinomial 0 0.5 3,
          _geoStringCharacter = uniformCP,
          _geoFixed_Floating = 0.5,
          _geoExponentSign = uniformCP,
          _geoX_Z = 0.5,
          _geoBinarySymbols = NPNegativeBinomial 0 0.5 3,
          _geoBinarySymbol = uniformCP,
          _geoOctalSymbols = NPNegativeBinomial 0 0.5 3,
          _geoOctalSymbol = uniformCP,
          _geoDecimalSymbols = NPNegativeBinomial 0 0.5 3,
          _geoDecimalSymbol = uniformCP,
          _geoHexadecimalSymbols = NPNegativeBinomial 0 0.5 3,
          _geoHexadecimalSymbol = uniformCP
        },
      _goIdentifier = GarbageIdentifierOpts
        { _gioEscaped_Simple = 0.5,
          _gioSimpleLetters = NPNegativeBinomial 0 0.5 3,
          _gioSimpleLetter = uniformCP,
          _gioEscapedLetters = NPNegativeBinomial 0 0.5 3,
          _gioEscapedLetter = uniformCP,
          _gioSystemLetters = NPNegativeBinomial 0 0.5 3,
          _gioSystemFirstLetter = uniformCP
        },
      _goDriveStrength = uniformCP,
      _goLValues = NPNegativeBinomial 0 0.5 1,
      _goOptionalLValue = 0.5,
      _goAttributes = NPLinearComb [(2, NPDiscrete [(1, 0)]), (1, NPNegativeBinomial 0 0.75 1)],
      _goAttributeOptionalValue = 0.5,
      _goDelay = CPDiscrete [1, 1, 2, 4],
      _goIntRealIdent = uniformCP,
      _goPathDepth = NPNegativeBinomial 0 0.75 1,
      _goBareMinTypMax = 0.5
    }

data Info = Info
  { -- | @commit@: the hash of the commit that was compiled.
    _infoCommit :: !Text,
    -- | @version@: the version of Verismith that was compiled.
    _infoVersion :: !Text
  }
  deriving (Eq, Show)

-- | Description of the simulator
data SimDescription = SimDescription {simName :: {-# UNPACK #-} !Text}
  deriving (Eq, Show)

-- | @[[synthesiser]]@: description of the synthesis tool. There can be multiple of these sections in a config
-- file.
data SynthDescription = SynthDescription
  { -- | @name@: type of the synthesis tool. Can either be @yosys@, @quartus@,
    -- @quartuslight@, @vivado@, @xst@.
    synthName :: {-# UNPACK #-} !Text,
    -- | @bin@: location of the synthesis tool binary.
    synthBin :: Maybe Text,
    -- | @description@: description that should be used for the synthesis tool.
    synthDesc :: Maybe Text,
    -- | @output@: name of the output Verilog file.
    synthOut :: Maybe Text
  }
  deriving (Eq, Show)

data Config = Config
  { _configEMI :: {-# UNPACK #-} !ConfEMI,
    _configInfo :: {-# UNPACK #-} !Info,
    _configProbability :: {-# UNPACK #-} !Probability,
    _configProperty :: {-# UNPACK #-} !ConfProperty,
    _configGarbageGenerator :: {-# UNPACK #-} !GarbageOpts,
    _configSimulators :: [SimDescription],
    _configSynthesisers :: [SynthDescription]
  }
  deriving (Eq, Show)

$(makeLenses ''ProbExpr)

$(makeLenses ''ProbModItem)

$(makeLenses ''ProbStatement)

$(makeLenses ''ProbMod)

$(makeLenses ''Probability)

$(makeLenses ''ConfEMI)

$(makeLenses ''ConfProperty)

$(makeLenses ''GarbageOpts)
$(makeLenses ''GarbageConfigOpts)
$(makeLenses ''GarbagePrimitiveOpts)
$(makeLenses ''GarbageModuleOpts)
$(makeLenses ''GarbageSpecifyOpts)
$(makeLenses ''GarbageSpecifyPathOpts)
$(makeLenses ''GarbageSpecifyTimingCheckOpts)
$(makeLenses ''GarbageGenerateOpts)
$(makeLenses ''GarbageTypeOpts)
$(makeLenses ''GarbageAttenuationOpts)
$(makeLenses ''GarbageStatementOpts)
$(makeLenses ''GarbageExprOpts)
$(makeLenses ''GarbageIdentifierOpts)

$(makeLenses ''Info)

$(makeLenses ''Config)

$(makePrisms ''CategoricalProbability)

$(makePrisms ''NumberProbability)

defaultValue :: a -> TomlCodec a -> TomlCodec a
defaultValue x = Toml.dimap Just (fromMaybe x) . Toml.dioptional

fromXST :: XST -> SynthDescription
fromXST (XST a b c) =
  SynthDescription "xst" (toTextIgnore <$> a) (Just b) (Just $ toTextIgnore c)

fromYosys :: Yosys -> SynthDescription
fromYosys (Yosys a b c) =
  SynthDescription
    "yosys"
    (toTextIgnore <$> a)
    (Just b)
    (Just $ toTextIgnore c)

fromVivado :: Vivado -> SynthDescription
fromVivado (Vivado a b c) =
  SynthDescription
    "vivado"
    (toTextIgnore <$> a)
    (Just b)
    (Just $ toTextIgnore c)

fromQuartus :: Quartus -> SynthDescription
fromQuartus (Quartus a b c) =
  SynthDescription
    "quartus"
    (toTextIgnore <$> a)
    (Just b)
    (Just $ toTextIgnore c)

fromQuartusLight :: QuartusLight -> SynthDescription
fromQuartusLight (QuartusLight a b c) =
  SynthDescription
    "quartuslight"
    (toTextIgnore <$> a)
    (Just b)
    (Just $ toTextIgnore c)

defaultConfig :: Config
defaultConfig =
  Config
    (ConfEMI 2 8)
    (Info (pack $(gitHash)) (pack $ showVersion version))
    (Probability defModItem defStmnt defExpr defMod)
    (ConfProperty 20 Nothing 3 2 5 "random" 10 False 0 1 Nothing)
    defGarbageOpts
    []
    [fromYosys defaultYosys, fromVivado defaultVivado]
  where
    defMod =
      ProbMod
        0 -- Drop Output
        1 -- Keep Output
    defModItem =
      ProbModItem
        5 -- Assign
        1 -- Sequential Always
        1 -- Combinational Always
        1 -- Instantiation
    defStmnt =
      ProbStatement
        0 -- Blocking assignment
        3 -- Non-blocking assignment
        1 -- Conditional
        0 -- For loop
    defExpr =
      ProbExpr
        1 -- Number
        5 -- Identifier
        5 -- Range selection
        5 -- Unary operator
        5 -- Binary operator
        5 -- Ternary conditional
        3 -- Concatenation
        0 -- String
        5 -- Signed function
        5 -- Unsigned funtion

twoKey :: Toml.Piece -> Toml.Piece -> Toml.Key
twoKey a b = Toml.Key (a :| [b])

int :: Toml.Piece -> Toml.Piece -> TomlCodec Int
int a = Toml.int . twoKey a

catProbCodec :: TomlCodec CategoricalProbability
catProbCodec =
  Toml.dimatch (firstOf _CPDiscrete) CPDiscrete (Toml.arrayNonEmptyOf Toml._Double "Discrete")
    <|> Toml.table
      ( liftA2
          CPBiasedUniform
          ( defaultValue [] (Toml.list (Toml.pair (Toml.double "weight") (Toml.int "value")) "biases")
              .= _CPBUBiases
          )
          (Toml.double "weight" .= _CPBUUniformWeight)
      )
      "BiasedUniform"

numProbCodec :: TomlCodec NumberProbability
numProbCodec =
  Toml.dimatch
    (firstOf _NPUniform)
    (uncurry NPUniform)
    (Toml.table (Toml.pair (defaultValue 0 $ Toml.int "low") (Toml.int "high")) "Uniform")
    <|> Toml.dimatch
      (firstOf _NPBinomial)
      (uncurry3 NPBinomial)
      ( Toml.table
          ( Toml.triple
              (defaultValue 0 (Toml.int "offset"))
              (Toml.int "trials")
              (Toml.double "succesRate")
          )
          "Binomial"
      )
    <|> Toml.dimatch
      (firstOf _NPNegativeBinomial)
      (uncurry3 NPNegativeBinomial)
      ( Toml.table
          ( Toml.triple
              (defaultValue 0 (Toml.int "offset"))
              (Toml.double "failureRate")
              (Toml.int "numberOfFailures")
          )
          "NegativeBinomial"
      )
    <|> Toml.dimatch
      (firstOf _NPNegativeBinomial)
      (uncurry3 NPNegativeBinomial)
      ( Toml.table
          ( Toml.triple
              (defaultValue 0 (Toml.int "offset"))
              (Toml.double "failureRate")
              (pure 1)
          )
          "Geometric"
      )
    <|> Toml.dimatch
      (firstOf _NPPoisson)
      (uncurry NPPoisson)
      (Toml.table (Toml.pair (defaultValue 0 (Toml.int "offset")) (Toml.double "lambda")) "Poisson")
    <|> Toml.dimatch
      (firstOf _NPDiscrete)
      NPDiscrete
      (Toml.nonEmpty (Toml.pair (Toml.double "weight") (Toml.int "value")) "Discrete")
    <|> Toml.dimatch
      (firstOf _NPLinearComb)
      NPLinearComb
      (Toml.nonEmpty (Toml.pair (Toml.double "weight") numProbCodec) "LinearCombination")

exprCodec :: TomlCodec ProbExpr
exprCodec =
  ProbExpr
    <$> defaultValue (defProb probExprNum) (intE "number")
    .= _probExprNum
    <*> defaultValue (defProb probExprId) (intE "variable")
    .= _probExprId
    <*> defaultValue (defProb probExprRangeSelect) (intE "rangeselect")
    .= _probExprRangeSelect
    <*> defaultValue (defProb probExprUnOp) (intE "unary")
    .= _probExprUnOp
    <*> defaultValue (defProb probExprBinOp) (intE "binary")
    .= _probExprBinOp
    <*> defaultValue (defProb probExprCond) (intE "ternary")
    .= _probExprCond
    <*> defaultValue (defProb probExprConcat) (intE "concatenation")
    .= _probExprConcat
    <*> defaultValue (defProb probExprStr) (intE "string")
    .= _probExprStr
    <*> defaultValue (defProb probExprSigned) (intE "signed")
    .= _probExprSigned
    <*> defaultValue (defProb probExprUnsigned) (intE "unsigned")
    .= _probExprUnsigned
  where
    defProb i = defaultConfig ^. configProbability . probExpr . i
    intE = int "expr"

stmntCodec :: TomlCodec ProbStatement
stmntCodec =
  ProbStatement
    <$> defaultValue (defProb probStmntBlock) (intS "blocking")
    .= _probStmntBlock
    <*> defaultValue (defProb probStmntNonBlock) (intS "nonblocking")
    .= _probStmntNonBlock
    <*> defaultValue (defProb probStmntCond) (intS "conditional")
    .= _probStmntCond
    <*> defaultValue (defProb probStmntFor) (intS "forloop")
    .= _probStmntFor
  where
    defProb i = defaultConfig ^. configProbability . probStmnt . i
    intS = int "statement"

modItemCodec :: TomlCodec ProbModItem
modItemCodec =
  ProbModItem
    <$> defaultValue (defProb probModItemAssign) (intM "assign")
    .= _probModItemAssign
    <*> defaultValue (defProb probModItemSeqAlways) (intM "sequential")
    .= _probModItemSeqAlways
    <*> defaultValue (defProb probModItemCombAlways) (intM "combinational")
    .= _probModItemCombAlways
    <*> defaultValue (defProb probModItemInst) (intM "instantiation")
    .= _probModItemInst
  where
    defProb i = defaultConfig ^. configProbability . probModItem . i
    intM = int "moditem"

modCodec :: TomlCodec ProbMod
modCodec =
  ProbMod
    <$> defaultValue (defProb probModDropOutput) (intM "drop_output")
    .= _probModDropOutput
    <*> defaultValue (defProb probModKeepOutput) (intM "keep_output")
    .= _probModKeepOutput
  where
    defProb i = defaultConfig ^. configProbability . probMod . i
    intM = int "module"

probCodec :: TomlCodec Probability
probCodec =
  Probability
    <$> defaultValue (defProb probModItem) modItemCodec
    .= _probModItem
    <*> defaultValue (defProb probStmnt) stmntCodec
    .= _probStmnt
    <*> defaultValue (defProb probExpr) exprCodec
    .= _probExpr
    <*> defaultValue (defProb probMod) modCodec
    .= _probMod
  where
    defProb i = defaultConfig ^. configProbability . i

propCodec :: TomlCodec ConfProperty
propCodec =
  ConfProperty
    <$> defaultValue (defProp propSize) (Toml.int "size")
    .= _propSize
    <*> Toml.dioptional (Toml.read "seed")
    .= _propSeed
    <*> defaultValue (defProp propStmntDepth) (int "statement" "depth")
    .= _propStmntDepth
    <*> defaultValue (defProp propModDepth) (int "module" "depth")
    .= _propModDepth
    <*> defaultValue (defProp propMaxModules) (int "module" "max")
    .= _propMaxModules
    <*> defaultValue
      (defProp propSampleMethod)
      (Toml.text (twoKey "sample" "method"))
    .= _propSampleMethod
    <*> defaultValue (defProp propSampleSize) (int "sample" "size")
    .= _propSampleSize
    <*> defaultValue
      (defProp propCombine)
      (Toml.bool (twoKey "output" "combine"))
    .= _propCombine
    <*> defaultValue (defProp propNonDeterminism) (Toml.int "nondeterminism")
    .= _propNonDeterminism
    <*> defaultValue (defProp propDeterminism) (Toml.int "determinism")
    .= _propDeterminism
    <*> Toml.dioptional (Toml.text (twoKey "default" "yosys"))
    .= _propDefaultYosys
  where
    defProp i = defaultConfig ^. configProperty . i

garbageAttenuationCodec :: Toml.Key -> TomlCodec GarbageAttenuationOpts
garbageAttenuationCodec =
  Toml.dimap _gaoDecrease (GarbageAttenuationOpts 1.0)
  . defaultValue (_gaoDecrease defAttenuationOpts)
  . Toml.double

garbageConfigCodec :: TomlCodec GarbageConfigOpts
garbageConfigCodec =
  GarbageConfigOpts
    <$> tfield _gcoBlocks "blocks" numProbCodec
    <*> tfield _gcoDesigns "designs" numProbCodec
    <*> tfield _gcoItems "items" numProbCodec
    <*> tfield _gcoLibraries "items" numProbCodec
    <*> dfield _gcoCell_Inst "cell_or_inst"
    <*> dfield _gcoLiblist_Use "liblist_or_use"
    <*> dfield _gcoConfig "config"
    <*> dfield _gcoLibraryScope "libraryScope"
  where
    tfield p n c =
      defaultValue (p $ _goConfig $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n =
      defaultValue (p $ _goConfig $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

garbagePrimitiveCodec :: TomlCodec GarbagePrimitiveOpts
garbagePrimitiveCodec =
  GarbagePrimitiveOpts
    <$> tfield _gpoBlocks "blocks" numProbCodec
    <*> tfield _gpoPorts "ports" numProbCodec
    <*> tfield _gpoPortType "portType" catProbCodec
    <*> dfield _gpoSeq_Comb "seq_or_comb"
    <*> dfield _gpoRegInit "regInitNoSem"
    <*> tfield _gpoCombInit "combInit" catProbCodec
    <*> tfield _gpoTableRows "tableRows" numProbCodec
    <*> tfield _gpoInLevel "inLevel" catProbCodec
    <*> tfield _gpoOutLevel "outLevel" catProbCodec
    <*> dfield _gpoEdgeSensitive "edgeSensitive"
    <*> tfield _gpoEdgeSimplePosNeg "edgeSimplePosNeg" catProbCodec
    <*> dfield _gpoOutputNoChange "outputNoChange"
  where
    tfield p n c =
      defaultValue (p $ _goPrimitive $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n =
      defaultValue (p $ _goPrimitive $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

garbageModuleCodec :: TomlCodec GarbageModuleOpts
garbageModuleCodec =
  GarbageModuleOpts
    <$> tfield _gmoBlocks "blocks" numProbCodec
    <*> dfield _gmoNamed_Positional "instance_named_or_positional"
    <*> tfield _gmoParameters "instance_parameters" numProbCodec
    <*> dfield _gmoOptionalParameter "instance_optparam"
    <*> tfield _gmoPorts "port_count" numProbCodec
    <*> tfield _gmoPortLValues "port_lvalues" numProbCodec
    <*> dfield _gmoPortRange "port_range"
    <*> tfield _gmoPortDir "port_dir" catProbCodec
    <*> dfield _gmoOptionalPort "port_optional"
    <*> tfield _gmoItems "items" numProbCodec
    <*> tfield _gmoItem "item" catProbCodec
    <*> dfield _gmoMacro "macromodule"
    <*> dfield _gmoTimeScale "timescale_optional"
    <*> tfield _gmoTimeMagnitude "timescale_magnitude" catProbCodec
    <*> dfield _gmoCell "cell"
    <*> tfield _gmoUnconnectedDrive "unconnectedDrive" catProbCodec
    <*> tfield _gmoDefaultNetType "defaultNetType" catProbCodec
    <*> bfield _gmoNonAsciiHeader "nonAsciiHeader"
  where
    tfield p n c =
      defaultValue (p $ _goModule $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n =
      defaultValue (p $ _goModule $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p
    bfield p n =
      defaultValue (p $ _goModule $ _configGarbageGenerator defaultConfig) (Toml.bool n) .= p

garbageSpecifyPathCodec :: TomlCodec GarbageSpecifyPathOpts
garbageSpecifyPathCodec =
  GarbageSpecifyPathOpts
    <$> tfield _gspoCondition "condition" catProbCodec
    <*> dfield _gspoFull_Parallel "full_or_parallel"
    <*> dfield _gspoEdgeSensitive "edgeSensitive"
    <*> tfield _gspoFullSources "full_sources" numProbCodec
    <*> tfield _gspoFullDestinations "full_destinations" numProbCodec
    <*> tfield _gspoPolarity "polarity" catProbCodec
    <*> tfield _gspoEdgeSensitivity "edgeSensitivity" catProbCodec
    <*> tfield _gspoDelayKind "delayKind" catProbCodec
  where
    tfield p n c =
      defaultValue
        (p $ _gsyoPath $ _goSpecify $ _configGarbageGenerator defaultConfig)
        (Toml.table c n)
      .= p
    dfield p n =
      defaultValue
        (p $ _gsyoPath $ _goSpecify $ _configGarbageGenerator defaultConfig)
        (Toml.double n)
      .= p

garbageSpecifyTimingCheckCodec :: TomlCodec GarbageSpecifyTimingCheckOpts
garbageSpecifyTimingCheckCodec =
  GarbageSpecifyTimingCheckOpts
    <$> dfield _gstcoOptionalArg "optarg"
    <*> dfield _gstcoEvent "event_optional"
    <*> dfield _gstcoEventEdge "event_edge"
    <*> dfield _gstcoCondition "condition_optional"
    <*> dfield _gstcoCondNeg_Pos "condition_neg_or_pos"
    <*> dfield _gstcoDelayedMinTypMax "delayedMinTypMax"
  where
    dfield p n =
      defaultValue
        (p $ _gsyoTimingCheck $ _goSpecify $ _configGarbageGenerator defaultConfig)
        (Toml.double n)
      .= p

garbageSpecifyCodec :: TomlCodec GarbageSpecifyOpts
garbageSpecifyCodec =
  GarbageSpecifyOpts
    <$> tfield _gsyoPath "path" garbageSpecifyPathCodec
    <*> tfield _gsyoTimingCheck "timingCheck" garbageSpecifyTimingCheckCodec
    <*> tfield _gsyoItems "items" numProbCodec
    <*> tfield _gsyoItem "item" catProbCodec
    <*> dfield _gsyoTermRange "termRange"
    <*> dfield _gsyoParamRange "parameter_range"
    <*> dfield _gsyoPathPulseEscaped_Simple "pathpulse_escaped_or_simple"
    <*> dfield _gsyoPathPulseRange "pathpulse_term_range"
  where
    tfield p n c =
      defaultValue (p $ _goSpecify $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n =
      defaultValue (p $ _goSpecify $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

garbageGenerateCodec :: TomlCodec GarbageGenerateOpts
garbageGenerateCodec =
  GarbageGenerateOpts
    <$> garbageAttenuationCodec "attenuation" .= _ggoAttenuation
    <*> tfield _ggoItems "items" numProbCodec
    <*> tfield _ggoItem "item" catProbCodec
    <*> dfield _ggoOptionalBlock "optionalBlock"
    <*> dfield _ggoInstOptionalDelay "instance_optional_delay"
    <*> dfield _ggoInstOptionalRange "instance_optional_range"
    <*> dfield _ggoPrimitiveOptIdent "primitive_optional_name"
    <*> tfield _ggoCondBlock "nested_condition" catProbCodec
    <*> tfield _ggoNetType "net_type" catProbCodec
    <*> dfield _ggoNetRange "net_range"
    <*> tfield _ggoNetVectoring "net_vectoring" catProbCodec
    <*> tfield _ggoDeclItem "declaration" catProbCodec
    <*> dfield _ggoDeclDim_Init "declaration_dim_or_init"
    <*> tfield _ggoChargeStrength "chargeStrength" catProbCodec
    <*> dfield _ggoTaskFunAutomatic "taskFun_automatic"
    <*> tfield _ggoTaskFunDecl "taskFun_declaration" catProbCodec
    <*> dfield _ggoTaskFunRegister "taskFun_register"
    <*> tfield _ggoTaskFunPorts "taskFun_ports" numProbCodec
    <*> tfield _ggoTaskFunPortType "taskFun_portType" catProbCodec
    <*> tfield _ggoTaskPortDirection "taskPortDir" catProbCodec
    <*> dfield _ggoFunRetType "funReturnType"
    <*> tfield _ggoGateInst "gate" catProbCodec
    <*> dfield _ggoGateOptIdent "gate_optional_name"
    <*> tfield _ggoGateNInputType "gate_ninput" catProbCodec
    <*> tfield _ggoGateInputs "gate_ninputs" numProbCodec
    <*> tfield _ggoGateOutputs "gate_noutputs" numProbCodec
    <*> tfield _ggoCaseBranches "case_branches" numProbCodec
    <*> tfield _ggoCaseBranchPatterns "case_patterns" numProbCodec
  where
    tfield p n c =
      defaultValue (p $ _goGenerate $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n =
      defaultValue (p $ _goGenerate $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

garbageTypeCodec :: TomlCodec GarbageTypeOpts
garbageTypeCodec =
  GarbageTypeOpts
    <$> dfield _gtoAbstract_Concrete "abstract_or_concrete"
    <*> tfield _gtoAbstract "abstract" catProbCodec
    <*> dfield _gtoConcreteSignedness "concrete_signedness"
    <*> dfield _gtoConcreteBitRange "concrete_bitRange"
    <*> tfield _gtoDimensions "dimensions" numProbCodec
  where
    tfield p n c =
      defaultValue (p $ _goType $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n =
      defaultValue (p $ _goType $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

garbageStatementCodec :: TomlCodec GarbageStatementOpts
garbageStatementCodec =
  GarbageStatementOpts
    <$> garbageAttenuationCodec "attenuation" .= _gstoAttenuation
    <*> dfield _gstoOptional "optional"
    <*> tfield _gstoItem "item" catProbCodec
    <*> tfield _gstoItems "items" numProbCodec
    <*> dfield _gstoOptionalDelEvCtl "optionalDelayEventControl"
    <*> dfield _gstoAssignmentBlocking "assignmentBlocking"
    <*> tfield _gstoCase "case_kind" catProbCodec
    <*> tfield _gstoCaseBranches "case_branches" numProbCodec
    <*> tfield _gstoCaseBranchPatterns "case_patterns" numProbCodec
    <*> tfield _gstoLoop "loop" catProbCodec
    <*> dfield _gstoBlockPar_Seq "block_par_or_seq"
    <*> dfield _gstoBlockHeader "block_header"
    <*> tfield _gstoBlockDecls "block_declarations" numProbCodec
    <*> tfield _gstoBlockDecl "block_declaration" catProbCodec
    <*> tfield _gstoProcContAssign "procContAss_assDeassForceRel" catProbCodec
    <*> dfield _gstoPCAVar_Net "procContAss_var_or_net"
    <*> tfield _gstoDelayEventRepeat "delayEventRepeat" catProbCodec
    <*> tfield _gstoEvent "event" catProbCodec
    <*> tfield _gstoEvents "event_exprs" numProbCodec
    <*> tfield _gstoEventPrefix "event_prefix" catProbCodec
    <*> tfield _gstoSysTaskPorts "sysTask_ports" numProbCodec
    <*> dfield _gstoSysTaskOptionalPort "sysTask_optionalPort"
  where
    tfield p n c =
      defaultValue (p $ _goStatement $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n =
      defaultValue (p $ _goStatement $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

garbageExprCodec :: TomlCodec GarbageExprOpts
garbageExprCodec =
  GarbageExprOpts
    <$> garbageAttenuationCodec "attenuation" .= _geoAttenuation
    <*> tfield _geoItem "item" catProbCodec
    <*> tfield _geoPrimary "primary" catProbCodec
    <*> tfield _geoUnary "op_unary" catProbCodec
    <*> tfield _geoBinary "op_binary" catProbCodec
    <*> dfield _geoMinTypMax "minTypMax"
    <*> dfield _geoDimRange "dimRange"
    <*> tfield _geoRange "range_kind" catProbCodec
    <*> dfield _geoRangeOffsetPos_Neg "range_offset_pos_or_neg"
    <*> tfield _geoConcatenations "concatenations" numProbCodec
    <*> tfield _geoSysFunArgs "sysFunArgs" numProbCodec
    <*> tfield _geoLiteralWidth "literal_width" catProbCodec
    <*> dfield _geoLiteralSigned "literal_signed"
    <*> tfield _geoStringCharacters "string_characters" numProbCodec
    <*> tfield _geoStringCharacter "string_character" catProbCodec
    <*> dfield _geoFixed_Floating "fixed_or_float"
    <*> tfield _geoExponentSign "exponentSign" catProbCodec
    <*> dfield _geoX_Z "X_or_Z"
    <*> tfield _geoBinarySymbols "binary_digits" numProbCodec
    <*> tfield _geoBinarySymbol "binary_digit" catProbCodec
    <*> tfield _geoOctalSymbols "octal_digits" numProbCodec
    <*> tfield _geoOctalSymbol "octal_digit" catProbCodec
    <*> tfield _geoDecimalSymbols "decimal_digits" numProbCodec
    <*> tfield _geoDecimalSymbol "decimal_digit" catProbCodec
    <*> tfield _geoHexadecimalSymbols "hex_digits" numProbCodec
    <*> tfield _geoHexadecimalSymbol "hex_digit" catProbCodec
  where
    tfield p n c =
      defaultValue (p $ _goExpr $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n =
      defaultValue (p $ _goExpr $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

garbageIdentifierCodec :: TomlCodec GarbageIdentifierOpts
garbageIdentifierCodec =
  GarbageIdentifierOpts
    <$> dfield _gioEscaped_Simple "escaped_or_simple"
    <*> tfield _gioSimpleLetters "simple_length" numProbCodec
    <*> tfield _gioSimpleLetter "simple_letter" catProbCodec
    <*> tfield _gioEscapedLetters "escaped_length" numProbCodec
    <*> tfield _gioEscapedLetter "escaped_letter" catProbCodec
    <*> tfield _gioSystemLetters "system_length" numProbCodec
    <*> tfield _gioSystemFirstLetter "system_first_letter" catProbCodec
  where
    tfield p n c =
      defaultValue (p $ _goIdentifier $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n =
      defaultValue (p $ _goIdentifier $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

garbageCodec :: TomlCodec GarbageOpts
garbageCodec =
  GarbageOpts
    <$> Toml.dioptional (Toml.read "seed") .= _goSeed
    <*> tfield _goConfig "config" garbageConfigCodec
    <*> tfield _goPrimitive "primitive" garbagePrimitiveCodec
    <*> tfield _goModule "module" garbageModuleCodec
    <*> tfield _goSpecify "specify" garbageSpecifyCodec
    <*> tfield _goGenerate "generate" garbageGenerateCodec
    <*> tfield _goType "type" garbageTypeCodec
    <*> tfield _goStatement "statement" garbageStatementCodec
    <*> tfield _goExpr "expr" garbageExprCodec
    <*> tfield _goIdentifier "identifier" garbageIdentifierCodec
    <*> tfield _goDriveStrength "driveStrength" catProbCodec
    <*> tfield _goLValues "lvalue_items" numProbCodec
    <*> dfield _goOptionalLValue "lvalue_optional"
    <*> tfield _goAttributes "attribute_items" numProbCodec
    <*> dfield _goAttributeOptionalValue "attribute_optional_value"
    <*> tfield _goDelay "delay_items" catProbCodec
    <*> tfield _goIntRealIdent "delay_base" catProbCodec
    <*> tfield _goPathDepth "path_depth" numProbCodec
    <*> dfield _goBareMinTypMax "parameter_mintypmax_or_single"
  where
    tfield p n c = defaultValue (p $ _configGarbageGenerator defaultConfig) (Toml.table c n) .= p
    dfield p n = defaultValue (p $ _configGarbageGenerator defaultConfig) (Toml.double n) .= p

simulator :: TomlCodec SimDescription
simulator = Toml.textBy pprint parseIcarus "name"
  where
    parseIcarus i@"icarus" = Right $ SimDescription i
    parseIcarus s = Left $ "Could not match '" <> s <> "' with a simulator."
    pprint (SimDescription a) = a

synthesiser :: TomlCodec SynthDescription
synthesiser =
  SynthDescription
    <$> Toml.text "name"
    .= synthName
    <*> Toml.dioptional (Toml.text "bin")
    .= synthBin
    <*> Toml.dioptional (Toml.text "description")
    .= synthDesc
    <*> Toml.dioptional (Toml.text "output")
    .= synthOut

emiCodec :: TomlCodec ConfEMI
emiCodec =
  ConfEMI
    <$> defaultValue
      (defaultConfig ^. configEMI . confEMIGenerateProb)
      (Toml.int "generate_prob")
    .= _confEMIGenerateProb
    <*> defaultValue
      (defaultConfig ^. configEMI . confEMINoGenerateProb)
      (Toml.int "nogenerate_prob")
    .= _confEMINoGenerateProb

infoCodec :: TomlCodec Info
infoCodec =
  Info
    <$> defaultValue
      (defaultConfig ^. configInfo . infoCommit)
      (Toml.text "commit")
    .= _infoCommit
    <*> defaultValue
      (defaultConfig ^. configInfo . infoVersion)
      (Toml.text "version")
    .= _infoVersion

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> defaultValue
      (defaultConfig ^. configEMI)
      (Toml.table emiCodec "emi")
    .= _configEMI
    <*> defaultValue
      (defaultConfig ^. configInfo)
      (Toml.table infoCodec "info")
    .= _configInfo
    <*> defaultValue
      (defaultConfig ^. configProbability)
      (Toml.table probCodec "probability")
    .= _configProbability
    <*> defaultValue
      (defaultConfig ^. configProperty)
      (Toml.table propCodec "property")
    .= _configProperty
    <*> defaultValue
      (defaultConfig ^. configGarbageGenerator)
      (Toml.table garbageCodec "invalid_generator")
    .= _configGarbageGenerator
    <*> defaultValue
      (defaultConfig ^. configSimulators)
      (Toml.list simulator "simulator")
    .= _configSimulators
    <*> defaultValue
      (defaultConfig ^. configSynthesisers)
      (Toml.list synthesiser "synthesiser")
    .= _configSynthesisers

parseConfigFile :: FilePath -> IO (Either Text Config)
parseConfigFile fp = do
  decoded <- Toml.decodeFileExact configCodec fp
  case decoded of
    Right c -> return $ Right c
    Left e -> return . Left $ Toml.prettyTomlDecodeErrors e

parseConfig :: Text -> Either Text Config
parseConfig t = case Toml.decodeExact configCodec t of
  Right c -> Right c
  Left e -> Left $ Toml.prettyTomlDecodeErrors e

parseConfigFileRelaxed :: FilePath -> IO (Either Text Config)
parseConfigFileRelaxed fp = do
  decoded <- Toml.decodeFileEither configCodec fp
  case decoded of
    Right c -> return $ Right c
    Left e -> return . Left $ Toml.prettyTomlDecodeErrors e

parseConfigRelaxed :: Text -> Either Text Config
parseConfigRelaxed t = case Toml.decode configCodec t of
  Right c -> Right c
  Left e -> Left $ Toml.prettyTomlDecodeErrors e

encodeConfig :: Config -> Text
encodeConfig = Toml.encode configCodec

encodeConfigFile :: FilePath -> Config -> IO ()
encodeConfigFile f = T.writeFile f . encodeConfig

versionInfo :: String
versionInfo =
  "Verismith "
    <> showVersion version
    <> " ("
    <> $(gitCommitDate)
    <> " "
    <> $(gitHash)
    <> ")"
