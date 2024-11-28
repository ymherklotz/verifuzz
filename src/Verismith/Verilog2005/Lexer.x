-- Module      : Verismith.Verilog2005.Lexer
-- Description : Partial Verilog 2005 lexer to reconstruct the AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

{
{-# OPTIONS_GHC -w #-}

module Verismith.Verilog2005.Lexer
  ( scanTokens
  , parseDecimal
  , isKW
  , isIdentSimple
  , VerilogVersion (..)
  , makeString
  )
where

import Data.Bits
import Data.Word (Word8)
import Numeric.Natural
import GHC.Natural
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.Bifunctor (second)
import Text.Printf (printf)
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad
import Control.Exception
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Internal (c2w, w2c, unpackChars, packChars)
import qualified Data.HashMap.Strict as HashMap
import Verismith.Verilog2005.Token
}

%encoding "latin1"

$white = [\ \t\n\f]
$all = [\0-\255]

-- | Comments and white space
@oneLineComment = "//" .*
@blockComment   = "/*" [$all # \*]* (\*+ [$all # [\*\/]] [$all # \*]*)* \*+ \/

-- | Operators
@unaryOperator     = [~!] | "~&" | "~|"
@binaryOperator    = [\/\<\>\%] | "==" | "!=" | "===" | "!==" | "&&" | "||" | "**" | ">=" | "<<" | ">>" | "<<<" | ">>>"

-- | Identifiers
$identifierFirstChar = [a-z A-Z _]
$identifierChar      = [a-z A-Z 0-9 _ \$]
@escapedIdentifier   = \\ [\!-\~]*
@simpleIdentifier    = $identifierFirstChar $identifierChar*
@systemIdentifier    = \$ $identifierChar+
@compilerDirective   = ` ( @simpleIdentifier | @escapedIdentifier )

-- | Table symbols, and edges
$tableout  = [01 xX]
$tablein   = [bB \?]
$tableedge = [fF pP rR nN \*]
@edgedesc  = [01] [01 xX zZ] | [xX zZ] [01]

-- | Numeric building blocks
$decimalDigit = 0-9
$specialDigit = [xX zZ \?]
$binaryDigit  = [$specialDigit 0-1]
$octalDigit   = [$specialDigit 0-7]
$hexDigit     = [$specialDigit 0-9 a-f A-F]

@unsigned = $decimalDigit (_ | $decimalDigit)*
@xzValue  = $specialDigit _*
@binValue = $binaryDigit (_ | $binaryDigit)*
@octValue = $octalDigit (_ | $octalDigit)*
@hexValue = $hexDigit (_ | $hexDigit)*

@fixed = @unsigned \. @unsigned
@real  = @fixed | @unsigned (\. @unsigned)? [eE] [\+\-]? @unsigned

-- | Strings

$stringChar = [$all # [\\"]]
@string   = \" $stringChar* (\\ . $stringChar*)* \"

-- | Timescale tokens
@tsvalue = 1(00?)?
@tsunit  = [munpf]?s

tokens :-
  <0,table,edge,pre2,pre8,pre10,pre16,ts0,ts1,ts2,ts3,ts4> {
    $white+ ;
    @blockComment ;
    @oneLineComment ;
    @compilerDirective { cdcident }
  }

  <0> {
    @real   { to LitReal   }
    @string { to (LitString . SBS.tail . SBS.init) }
    @simpleIdentifier  { toa kwident }
    @escapedIdentifier { to (escSimpleIdent . SBS.tail) }
    @systemIdentifier  { to (IdSystem . SBS.tail) }
    \'[sS]?[bB] { toa (numberBase BBin) }
    \'[sS]?[oO] { toa (numberBase BOct) }
    \'[sS]?[dD] { toa (numberBase BDec) }
    \'[sS]?[hH] { toa (numberBase BHex) }
    & { tok AmAmp  }
    ! { tok UnBang }
    = { tok SymEq  }
    @ { tok SymAt  }
    \| { tok AmBar       }
    \^ { tok AmHat       }
    \~ { tok UnTilde     }
    \/ { tok BinSlash    }
    \% { tok BinPercent  }
    \< { tok BinLt       }
    \> { tok BinGt       }
    \{ { tok SymBraceL   }
    \} { tok SymBraceR   }
    \+ { tok SymPlus     }
    \? { tok SymQuestion }
    \# { tok SymPound    }
    \* { tok SymAster    }
    \. { tok SymDot      }
    \$ { tok SymDollar   }
    "^~" { tok AmTildeHat    } -- Sorry
    "~^" { tok AmTildeHat    }
    "~&" { tok UnTildeAmp    }
    "~|" { tok UnTildeBar    }
    "==" { tok BinEqEq       }
    "!=" { tok BinBangEq     }
    "&&" { tok BinAmpAmp     }
    "||" { tok BinBarBar     }
    "**" { tok BinAsterAster }
    ">=" { tok BinGtEq       }
    "<<" { tok BinLtLt       }
    ">>" { tok BinGtGt       }
    "(*" { tok SymParenAster }
    "*)" { tok SymAsterParen }
    "<=" { tok SymLtEq       }
    "+:" { tok SymPlusColon  }
    "-:" { tok SymDashColon  }
    "->" { tok SymDashGt     }
    "=>" { tok SymEqGt       }
    "*>" { tok SymAsterGt    }
    "===" { tok BinEqEqEq    }
    "!==" { tok BinBangEqEq  }
    "<<<" { tok BinLtLtLt    }
    ">>>" { tok BinGtGtGt    }
    "&&&" { tok SymAmpAmpAmp }
  }

  <ts0> @tsvalue { startcode ts1 (to tsValue) }
  <ts1> @tsunit  { startcode ts2 (to tsUnit)  }
  <ts2> \/       { \_ _ -> sc ts3 >> scan     }
  <ts3> @tsvalue { startcode ts4 (to tsValue) }
  <ts4> @tsunit  { startcode 0   (to tsUnit)  }

  <pre2>    @binValue { startcode 0 (to binary)  }
  <pre8>    @octValue { startcode 0 (to octal)   }
  <pre16>   @hexValue { startcode 0 (to hex)     }
  <pre10>   @xzValue  { startcode 0 (to xz)      }
  <0,pre10> @unsigned { startcode 0 (to decimal) }


  <0,table> {
    \( { tok SymParenL }
    \) { tok SymParenR }
    \- { tok SymDash   }
    :  { tok SymColon  }
    \; { tok SymSemi   }
  }
  <0,edge> {
    \, { tok SymComma  }
    \[ { tok SymBrackL }
    \] { startcode 0 (tok SymBrackR) }
  }

  -- Reusing token constructors
  <edge>  @edgedesc  { to edgeDesc }
  <table> {
    $tableout  { to tableOut }
    $tablein   { to tableIn  }
    $tableedge { to tableEdge }
    "endtable" { startcode 0 (tok KWEndtable) }
  }

{
-- not using any wrapper so I have to define things myself

data AlexInput = AlexInput
  { _aiPosition  :: !Position,
    _aiPrevChar  :: !Char,
    _aiInput     :: !LBS.ByteString
  }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput (Position ln cl s) _ inp) = case LBS.uncons inp of
  Nothing -> Nothing
  Just (b, inp) -> Just (b, let c = w2c b in AlexInput
      ( case c of
          '\t' -> Position ln ((cl .|. 3) + 1) s
          '\n' -> Position (ln + 1) 1 s
          _ -> Position ln (cl + 1) s
      )
      c
      inp
    )

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = _aiPrevChar

-- above was mandatory declarations for alex, below is the interesting stuff

data VerilogVersion = SV2023 | SV2017 | SV2012 | SV2009 | SV2005 | V2005 | V2001 | V2001_nc | V1995

instance Show VerilogVersion where
  show x = case x of
    SV2023 -> "1800-2023"
    SV2017 -> "1800-2017"
    SV2012 -> "1800-2012"
    SV2009 -> "1800-2009"
    SV2005 -> "1800-2005"
    V2005 -> "1364-2005"
    V2001 -> "1364-2001"
    V2001_nc -> "1364-2001-noconfig"
    V1995 -> "1364-1995"

data AlexState = AlexState
  { _asStartCode  :: !Int,
    _asInput      :: !AlexInput,
    _asKeywords   :: ![(NonEmpty Position, VerilogVersion)],
    -- LATER: User defined compiler directives support, the value (as in key/value) type is wrong
    _asDefines    :: !(HashMap.HashMap SBS.ByteString LBS.ByteString),
    _asSavedInput :: ![(Position, LBS.ByteString)]
  }

type Alex = StateT AlexState (ExceptT String IO)

scan :: Alex (Maybe PosToken)
scan = get >>= \(AlexState sc inp kw d si) -> case alexScan inp sc of
  AlexEOF -> case si of
    (p, i) : t -> put (AlexState sc (AlexInput p (_aiPrevChar inp) i) kw d t) >> scan
    [] -> case kw of
      [] -> return Nothing
      (p, vv) : _ ->
        throwError $
          printf "Missing `end_keywords to match `begin_keywords %s at %s" (show vv) $
            helperShowPositions p
  AlexError (AlexInput p pc i) ->
    throwError $
      printf
        "lexical error between %s and %s"
        (show $ _aiPosition inp)
        (helperShowPositions $ p :| map fst si)
  AlexSkip inp' _ -> modify' (\s -> s { _asInput = inp' }) >> scan
  AlexToken inp' n action -> do
    modify' $ \s -> s { _asInput = inp' }
    action (_aiPosition inp) $ LBS.toStrict $ LBS.take (toEnum n) $ _aiInput inp

scanTokens :: String -> IO (Either String [PosToken])
scanTokens f = do
  inp <- LBS.readFile f
  runExceptT $
    evalStateT loop $
      AlexState 0 (AlexInput (Position 1 1 $ PSFile f) '\n' inp) [] HashMap.empty []
  where
    loop = scan >>= maybe (pure []) (\x -> (x :) <$> loop)

type AlexAction = Position -> SBS.ByteString -> Alex (Maybe PosToken)

mkPos :: Position -> Token -> Alex (Maybe PosToken)
mkPos p t = (\l -> Just $ PosToken (p :| map fst l) t) <$> gets _asSavedInput

tok :: Token -> AlexAction
tok t p _ = mkPos p t

to :: (SBS.ByteString -> Token) -> AlexAction
to f p = mkPos p . f

toa :: (SBS.ByteString -> Alex Token) -> AlexAction
toa f p = f >=> mkPos p

sc :: Int -> Alex ()
sc n = modify' $ \s -> s { _asStartCode = n }

startcode :: Int -> AlexAction -> AlexAction
startcode n f p s = sc n >> f p s

alexPosError :: (NonEmpty Position) -> String -> Alex a
alexPosError p s =
  throwError $ s ++ " at " ++ helperShowPositions p

alexError :: Position -> String -> Alex a
alexError p s = gets _asSavedInput >>= flip alexPosError s . (p :|) . map fst

-- Lexer actions

tsValue :: SBS.ByteString -> Token
tsValue s = CDTSInt (SBS.length s - 1)

tsUnit :: SBS.ByteString -> Token
tsUnit s =
  CDTSUnit $
    case s of "s" -> 0 ; "ms" -> -3 ; "us" -> -6 ; "ns" -> -9 ; "ps" -> -12 ; "fs" -> -15

numberBase :: Base -> SBS.ByteString -> Alex Token
numberBase ba bs = do
  sc (case ba of BBin -> pre2 ; BOct -> pre8 ; BDec -> pre10 ; BHex -> pre16)
  return $ NumberBase (SBS.length bs == 3) ba

binary :: SBS.ByteString -> Token
binary s = LitBinary $ mapMaybe (\c -> case c of
  '0' -> Just BXZ0
  '1' -> Just BXZ1
  'x' -> Just BXZX
  'X' -> Just BXZX
  'z' -> Just BXZZ
  'Z' -> Just BXZZ
  '?' -> Just BXZZ
  _ -> Nothing) $ unpackChars s

octal :: SBS.ByteString -> Token
octal s = LitOctal $ mapMaybe (\c -> case c of
  'x' -> Just OXZX
  'X' -> Just OXZX
  'z' -> Just OXZZ
  'Z' -> Just OXZZ
  '?' -> Just OXZZ
  _ | '0' <= c && c <= '7' -> Just $ toEnum $ fromEnum c - fromEnum '0'
  _ -> Nothing) $ unpackChars s

hex :: SBS.ByteString -> Token
hex s = LitHex $ mapMaybe (\c -> case c of
  'x' -> Just HXZX
  'X' -> Just HXZX
  'z' -> Just HXZZ
  'Z' -> Just HXZZ
  '?' -> Just HXZZ
  _ | '0' <= c && c <= '9' -> Just $ toEnum $ fromEnum c - fromEnum '0'
  _ | 'a' <= c && c <= 'f' -> Just $ toEnum $ fromEnum c + 10 - fromEnum 'a'
  _ | 'A' <= c && c <= 'F' -> Just $ toEnum $ fromEnum c + 10 - fromEnum 'A'
  _ -> Nothing) $ unpackChars s

xz :: SBS.ByteString -> Token
xz s = LitXZ $ let c = SBS.head s in c == c2w 'x' || c == c2w 'X'

parseDecimal :: SBS.ByteString -> Natural
parseDecimal =
  fromInteger . SBS.foldl
    (\acc d -> if c2w '0' <= d && d <= c2w '9' then 10*acc + toInteger (d - c2w '0') else acc)
    0

decimal :: SBS.ByteString -> Token
decimal = LitDecimal . parseDecimal

unbxz :: SBS.ByteString -> BXZ
unbxz s = case s of "0" -> BXZ0; "1" -> BXZ1; "x" -> BXZX; "X" -> BXZX; "z" -> BXZZ; "Z" -> BXZZ

edgeDesc :: SBS.ByteString -> Token
edgeDesc s = EdgeEdge (unbxz $ SBS.init s) (unbxz $ SBS.tail s)

tableOut :: SBS.ByteString -> Token
tableOut s = TableOut $ case s of "0" -> ZOXZ; "1" -> ZOXO; "x" -> ZOXX; "X" -> ZOXX

tableIn :: SBS.ByteString -> Token
tableIn s = TableIn $ case s of "b" -> True; "B" -> True; "?" -> False

tableEdge :: SBS.ByteString -> Token
tableEdge s =
  TableEdge $ case s of
    "*" -> AFRNPA
    "f" -> AFRNPF
    "F" -> AFRNPF
    "r" -> AFRNPR
    "R" -> AFRNPR
    "n" -> AFRNPN
    "N" -> AFRNPN
    "p" -> AFRNPP
    "P" -> AFRNPP

cdcident :: AlexAction
cdcident p s = case HashMap.lookup (SBS.tail s) cdMap of
  Just a -> a p
  Nothing -> gets _asDefines >>= \defs -> case HashMap.lookup s defs of
    Nothing ->
      alexError p $
        printf "Compiler directive %s not declared nor supported, preprocess input file" $
          show s
    Just i ->
      alexError p $
        printf
          "User defined compiler directive replacement in not implemented, %s was encountered"
          (show s)

kwident :: SBS.ByteString -> Alex Token
kwident s = case SBS.stripPrefix "PATHPULSE$" s of
  Just ss -> return $ TknPP ss
  Nothing -> do
    vv <- gets _asKeywords
    let m = HashMap.lookup s $ case vv of
          (_, V1995) : _ -> kwV1995Map
          (_, V2001) : _ -> kwV2001Map
          (_, V2001_nc) : _ -> kwV2001NCMap
          (_, SV2005) : _ -> kwSV2005Map
          (_, SV2009) : _ -> kwSV2009Map
          (_, SV2012) : _ -> kwSV2012Map
          (_, SV2017) : _ -> kwSV2012Map
          (_, SV2023) : _ -> kwSV2012Map
          _ -> kwV2005Map
    case m of
      Nothing -> return $ if isKW s then IdEscaped $ SBS.cons (c2w '\\') s else IdSimple s
      Just act -> act

isIdentSimple :: SBS.ByteString -> Bool
isIdentSimple s = case SBS.uncons s of
  Just (c, t) ->
    testfirst c
      && not (isKW s)
      && SBS.all (\c -> testfirst c || (c2w '0' <= c && c <= c2w '9') || c == c2w '$') t
  _ -> False
  where
    testfirst c = (c2w 'A' <= c && c <= c2w 'Z') || (c2w 'a' <= c && c <= c2w 'z') || c == c2w '_'

escSimpleIdent :: SBS.ByteString -> Token
escSimpleIdent s = if isIdentSimple s then IdSimple s else IdEscaped s

makeString :: String -> SBS.ByteString
makeString s = packChars $ concatMap esc s
  where esc c = case w2c $ c2w c of '"' -> "\\\""; '\\' -> "\\\\"; '\n' -> "\\n"; x -> [x]

cdMap :: HashMap.HashMap SBS.ByteString (Position -> Alex (Maybe PosToken))
cdMap = HashMap.fromList $
  ("include", includecompdir)
  -- LATER: `define would go here and change state to store input as is in _asDefines
  : ("line", linecompdir)
  : ("begin_keywords", beginkwcompdir)
  : ("end_keywords", endkwcompdir)
  : ("undefineall", \_ -> modify' (\s -> s { _asDefines = HashMap.empty }) >> scan)
  : ("__LINE__", \p -> mkPos p $ LitString $ packChars $ show $ _posLine p)
  : ("__FILE__", \p -> mkPos p $ LitString $ makeString $ show $ _posSource p)
  : map (second (\y p -> y >>= mkPos p))
    ( ("timescale", sc ts0 *> pure CDTimescale)
      : ("resetall", modify' (\s -> s { _asDefines = HashMap.empty }) *> pure CDResetall)
      -- , ("pragma", *> pure CDPragma) -- TODO: Another layer of hell
      : map (second pure)
        [ ("celldefine", CDCelldefine)
        , ("default_nettype", CDDefaultnettype)
        -- , ("default_decay_time", CDUnknown)
        -- , ("default_trireg_strength", CDUnknown)
        -- , ("delay_mode_distributed", CDUnknown)
        -- , ("delay_mode_path", CDUnknown)
        -- , ("delay_mode_unit", CDUnknown)
        -- , ("delay_mode_zero", CDUnknown)
        , ("endcelldefine", CDEndcelldefine)
        , ("nounconnected_drive", CDNounconnecteddrive)
        , ("unconnected_drive", CDUnconnecteddrive)
        ]
    )

includecompdir :: Position -> Alex (Maybe PosToken)
includecompdir _ = do
  oldsc <- gets _asStartCode
  sc 0
  t <- scan
  case t of
    Nothing -> return Nothing
    Just (PosToken _ (LitString s)) -> do
      let f = unpackChars s
      -- LATER: search for file
      i <- liftIO $ LBS.readFile f
      modify' $ \(AlexState _ (AlexInput p pc bs) kw d si) ->
        AlexState oldsc (AlexInput (Position 1 1 $ PSFile f) pc i) kw d $ (p, bs) : si
      scan
    Just (PosToken p _) -> alexPosError p "Expected a filename to include"

linecompdir :: Position -> Alex (Maybe PosToken)
linecompdir _ = do
  oldsc <- gets _asStartCode
  sc 0
  l <- scan
  f <- scan
  c <- scan
  case (,,) <$> l <*> f <*> c of
    Nothing -> return Nothing
    Just (PosToken _ (LitDecimal l), PosToken _ (LitString s), PosToken _ (LitDecimal n))
      | n < 3 -> do
        modify' $ \(AlexState _ (AlexInput p pc bs) kw d si) ->
          AlexState
            oldsc
            (AlexInput (Position (naturalToWord l - 1) 1 $ PSLine (unpackChars s) (n == 1)) pc bs)
            kw
            d
            ((p, "") : if n /= 2 then si else smashline si)
        scan
    Just (PosToken _ (LitDecimal _), PosToken _ (LitString _), PosToken p _) ->
      alexPosError p "Expected a number between 0 and 2 to override position"
    Just (PosToken _ (LitDecimal _), PosToken p _, _) ->
      alexPosError p "Expected a filename and a number between 0 and 2 to override position"
    Just (PosToken p _, _, _) ->
      alexPosError
        p
        "Expected a number, a filename and a number between 0 and 2 to override position"
  where
    smashline l =
      case dropWhile (\(p, _) -> case _posSource p of PSLine _ b -> not b; _ -> False) l of
        (Position _ _ (PSLine _ True), _) : t -> t
        ll -> ll

beginkwcompdir :: Position -> Alex (Maybe PosToken)
beginkwcompdir _ = do
  oldsc <- gets _asStartCode
  sc 0
  t <- scan
  case t of
    Nothing -> return Nothing
    Just (PosToken p (LitString s)) -> case versionFromString s of
      Nothing -> alexPosError p "Expected a standard number string (ex. 1364-2005)"
      Just vv -> do
        modify' $ \(AlexState _ i kw d si) -> AlexState oldsc i ((p, vv) : kw) d si
        return $ Just $ PosToken p CDBeginKeywords
    Just (PosToken p _) -> alexPosError p "Expected a standard number string (ex. 1364-2005)"
  where
    versionFromString s = case splitAt 5 $ unpackChars s of
      ("1800-", '2' : '0' : year) -> case year of
        "05" -> Just SV2005
        "09" -> Just SV2009
        "12" -> Just SV2012
        "17" -> Just SV2017
        "23" -> Just SV2023
        _ -> Nothing
      ("1364-", year) -> case year of
        "1995" -> Just V1995
        "2001" -> Just V2001
        "2001-noconfig" -> Just V2001_nc
        "2005" -> Just V2005
        _ -> Nothing
      _ -> Nothing

endkwcompdir :: Position -> Alex (Maybe PosToken)
endkwcompdir p = do
  mkw <- gets _asKeywords
  case mkw of
    [] -> alexError p "`end_kewords encountered without a previous matching `begin_keywords"
    _ : kw -> modify' (\s -> s { _asKeywords = kw }) >> mkPos p CDEndKeywords

isKW :: SBS.ByteString -> Bool
isKW s = HashMap.member s kwSV2012Map

kwV1995 :: [(SBS.ByteString, Alex Token)]
kwV1995 =
  [ ("always", pure KWAlways),
    ("and", pure KWAnd),
    ("assign", pure KWAssign),
    ("begin", pure KWBegin),
    ("buf", pure KWBuf),
    ("bufif0", pure KWBufif0),
    ("bufif1", pure KWBufif1),
    ("case", pure KWCase),
    ("casex", pure KWCasex),
    ("casez", pure KWCasez),
    ("cmos", pure KWCmos),
    ("deassign", pure KWDeassign),
    ("default", pure KWDefault),
    ("defparam", pure KWDefparam),
    ("disable", pure KWDisable),
    ("edge", sc edge *> pure KWEdge),
    ("else", pure KWElse),
    ("end", pure KWEnd),
    ("endcase", pure KWEndcase),
    ("endfunction", pure KWEndfunction),
    ("endmodule", pure KWEndmodule),
    ("endprimitive", pure KWEndprimitive),
    ("endspecify", pure KWEndspecify),
    ("endtable", sc 0 *> pure KWEndtable),
    ("endtask", pure KWEndtask),
    ("event", pure KWEvent),
    ("for", pure KWFor),
    ("force", pure KWForce),
    ("forever", pure KWForever),
    ("fork", pure KWFork),
    ("function", pure KWFunction),
    ("highz0", pure KWHighz0),
    ("highz1", pure KWHighz1),
    ("if", pure KWIf),
    ("ifnone", pure KWIfnone),
    ("initial", pure KWInitial),
    ("inout", pure KWInout),
    ("input", pure KWInput),
    ("integer", pure KWInteger),
    ("join", pure KWJoin),
    ("large", pure KWLarge),
    ("macromodule", pure KWMacromodule),
    ("medium", pure KWMedium),
    ("module", pure KWModule),
    ("nand", pure KWNand),
    ("negedge", pure KWNegedge),
    ("nmos", pure KWNmos),
    ("nor", pure KWNor),
    ("not", pure KWNot),
    ("notif0", pure KWNotif0),
    ("notif1", pure KWNotif1),
    ("or", pure KWOr),
    ("output", pure KWOutput),
    ("parameter", pure KWParameter),
    ("pmos", pure KWPmos),
    ("posedge", pure KWPosedge),
    ("primitive", pure KWPrimitive),
    ("pull0", pure KWPull0),
    ("pull1", pure KWPull1),
    ("pulldown", pure KWPulldown),
    ("pullup", pure KWPullup),
    ("rcmos", pure KWRcmos),
    ("real", pure KWReal),
    ("realtime", pure KWRealtime),
    ("reg", pure KWReg),
    ("release", pure KWRelease),
    ("repeat", pure KWRepeat),
    ("rnmos", pure KWRnmos),
    ("rpmos", pure KWRpmos),
    ("rtran", pure KWRtran),
    ("rtranif0", pure KWRtranif0),
    ("rtranif1", pure KWRtranif1),
    ("scalared", pure KWScalared),
    ("small", pure KWSmall),
    ("specify", pure KWSpecify),
    ("specparam", pure KWSpecparam),
    ("strong0", pure KWStrong0),
    ("strong1", pure KWStrong1),
    ("supply0", pure KWSupply0),
    ("supply1", pure KWSupply1),
    ("table", sc table *> pure KWTable),
    ("task", pure KWTask),
    ("time", pure KWTime),
    ("tran", pure KWTran),
    ("tranif0", pure KWTranif0),
    ("tranif1", pure KWTranif1),
    ("tri", pure KWTri),
    ("tri0", pure KWTri0),
    ("tri1", pure KWTri1),
    ("triand", pure KWTriand),
    ("trior", pure KWTrior),
    ("trireg", pure KWTrireg),
    ("vectored", pure KWVectored),
    ("wait", pure KWWait),
    ("wand", pure KWWand),
    ("weak0", pure KWWeak0),
    ("weak1", pure KWWeak1),
    ("while", pure KWWhile),
    ("wire", pure KWWire),
    ("wor", pure KWWor),
    ("xnor", pure KWXnor),
    ("xor", pure KWXor)
  ]

kwV2001_nc :: [(SBS.ByteString, Alex Token)]
kwV2001_nc =
  ("automatic", pure KWAutomatic)
  : ("endgenerate", pure KWEndgenerate)
  : ("generate", pure KWGenerate)
  : ("genvar", pure KWGenvar)
  : ("localparam", pure KWLocalparam)
  : ("noshowcancelled", pure KWNoshowcancelled)
  : ("pulsestyle_ondetect", pure KWPulsestyleondetect)
  : ("pulsestyle_onevent", pure KWPulsestyleonevent)
  : ("showcancelled", pure KWShowcancelled)
  : ("signed", pure KWSigned)
  : ("unsigned", pure KWUnsigned)
  : kwV1995

kwV2001 :: [(SBS.ByteString, Alex Token)]
kwV2001 =
  ("cell", pure KWCell)
  : ("config", pure KWConfig)
  : ("design", pure KWDesign)
  : ("endconfig", pure KWEndconfig)
  : ("incdir", pure KWIncdir)
  : ("include", pure KWInclude)
  : ("instance", pure KWInstance)
  : ("liblist", pure KWLiblist)
  : ("library", pure KWLibrary)
  : ("use", pure KWUse)
  : kwV2001_nc

kwV2005 :: [(SBS.ByteString, Alex Token)]
kwV2005 =
  ("uwire", pure KWUwire)
  : kwV2001

kwSV2005 :: [(SBS.ByteString, Alex Token)]
kwSV2005 =
  ("alias", pure $ TokSVKeyword "alias")
  : ("always_comb", pure $ TokSVKeyword "always_comb")
  : ("always_ff", pure $ TokSVKeyword "always_ff")
  : ("always_latch", pure $ TokSVKeyword "always_latch")
  : ("assert", pure $ TokSVKeyword "assert")
  : ("assume", pure $ TokSVKeyword "assume")
  : ("before", pure $ TokSVKeyword "before")
  : ("bind", pure $ TokSVKeyword "bind")
  : ("bins", pure $ TokSVKeyword "bins")
  : ("binsof", pure $ TokSVKeyword "binsof")
  : ("bit", pure $ TokSVKeyword "bit")
  : ("break", pure $ TokSVKeyword "break")
  : ("byte", pure $ TokSVKeyword "byte")
  : ("chandle", pure $ TokSVKeyword "chandle")
  : ("class", pure $ TokSVKeyword "class")
  : ("clocking", pure $ TokSVKeyword "clocking")
  : ("const", pure $ TokSVKeyword "const")
  : ("constraint", pure $ TokSVKeyword "constraint")
  : ("context", pure $ TokSVKeyword "context")
  : ("continue", pure $ TokSVKeyword "continue")
  : ("cover", pure $ TokSVKeyword "cover")
  : ("covergroup", pure $ TokSVKeyword "covergroup")
  : ("coverpoint", pure $ TokSVKeyword "coverpoint")
  : ("cross", pure $ TokSVKeyword "cross")
  : ("dist", pure $ TokSVKeyword "dist")
  : ("do", pure $ TokSVKeyword "do")
  : ("endclass", pure $ TokSVKeyword "endclass")
  : ("endclocking", pure $ TokSVKeyword "endclocking")
  : ("endgroup", pure $ TokSVKeyword "endgroup")
  : ("endinterface", pure $ TokSVKeyword "endinterface")
  : ("endpackage", pure $ TokSVKeyword "endpackage")
  : ("endprogram", pure $ TokSVKeyword "endprogram")
  : ("endproperty", pure $ TokSVKeyword "endproperty")
  : ("endsequence", pure $ TokSVKeyword "endsequence")
  : ("enum", pure $ TokSVKeyword "enum")
  : ("expect", pure $ TokSVKeyword "expect")
  : ("export", pure $ TokSVKeyword "export")
  : ("extends", pure $ TokSVKeyword "extends")
  : ("extern", pure $ TokSVKeyword "extern")
  : ("final", pure $ TokSVKeyword "final")
  : ("first_match", pure $ TokSVKeyword "first_match")
  : ("foreach", pure $ TokSVKeyword "foreach")
  : ("forkjoin", pure $ TokSVKeyword "forkjoin")
  : ("iff", pure $ TokSVKeyword "iff")
  : ("ignore_bins", pure $ TokSVKeyword "ignore_bins")
  : ("illegal_bins", pure $ TokSVKeyword "illegal_bins")
  : ("import", pure $ TokSVKeyword "import")
  : ("inside", pure $ TokSVKeyword "inside")
  : ("int", pure $ TokSVKeyword "int")
  : ("interface", pure $ TokSVKeyword "interface")
  : ("intersect", pure $ TokSVKeyword "intersect")
  : ("join_any", pure $ TokSVKeyword "join_any")
  : ("join_none", pure $ TokSVKeyword "join_none")
  : ("local", pure $ TokSVKeyword "local")
  : ("logic", pure $ TokSVKeyword "logic")
  : ("longint", pure $ TokSVKeyword "longint")
  : ("matches", pure $ TokSVKeyword "matches")
  : ("modport", pure $ TokSVKeyword "modport")
  : ("new", pure $ TokSVKeyword "new")
  : ("null", pure $ TokSVKeyword "null")
  : ("package", pure $ TokSVKeyword "package")
  : ("packed", pure $ TokSVKeyword "packed")
  : ("priority", pure $ TokSVKeyword "priority")
  : ("program", pure $ TokSVKeyword "program")
  : ("property", pure $ TokSVKeyword "property")
  : ("protected", pure $ TokSVKeyword "protected")
  : ("pure", pure $ TokSVKeyword "pure")
  : ("rand", pure $ TokSVKeyword "rand")
  : ("randc", pure $ TokSVKeyword "randc")
  : ("randcase", pure $ TokSVKeyword "randcase")
  : ("randsequence", pure $ TokSVKeyword "randsequence")
  : ("ref", pure $ TokSVKeyword "ref")
  : ("return", pure $ TokSVKeyword "return")
  : ("sequence", pure $ TokSVKeyword "sequence")
  : ("shortint", pure $ TokSVKeyword "shortint")
  : ("shortreal", pure $ TokSVKeyword "shortreal")
  : ("solve", pure $ TokSVKeyword "solve")
  : ("static", pure $ TokSVKeyword "static")
  : ("string", pure $ TokSVKeyword "string")
  : ("struct", pure $ TokSVKeyword "struct")
  : ("super", pure $ TokSVKeyword "super")
  : ("tagged", pure $ TokSVKeyword "tagged")
  : ("this", pure $ TokSVKeyword "this")
  : ("throughout", pure $ TokSVKeyword "throughout")
  : ("timeprecision", pure $ TokSVKeyword "timeprecision")
  : ("timeunit", pure $ TokSVKeyword "timeunit")
  : ("type", pure $ TokSVKeyword "type")
  : ("typedef", pure $ TokSVKeyword "typedef")
  : ("union", pure $ TokSVKeyword "union")
  : ("unique", pure $ TokSVKeyword "unique")
  : ("var", pure $ TokSVKeyword "var")
  : ("virtual", pure $ TokSVKeyword "virtual")
  : ("void", pure $ TokSVKeyword "void")
  : ("wait_order", pure $ TokSVKeyword "wait_order")
  : ("wildcard", pure $ TokSVKeyword "wildcard")
  : ("with", pure $ TokSVKeyword "with")
  : ("within", pure $ TokSVKeyword "within")
  : kwV2005

kwSV2009 :: [(SBS.ByteString, Alex Token)]
kwSV2009 =
  ("accept_on", pure $ TokSVKeyword "accept_on")
  : ("checker", pure $ TokSVKeyword "checker")
  : ("endchecker", pure $ TokSVKeyword "endchecker")
  : ("eventually", pure $ TokSVKeyword "eventually")
  : ("global", pure $ TokSVKeyword "global")
  : ("implies", pure $ TokSVKeyword "implies")
  : ("let", pure $ TokSVKeyword "let")
  : ("nexttime", pure $ TokSVKeyword "nexttime")
  : ("reject_on", pure $ TokSVKeyword "reject_on")
  : ("restrict", pure $ TokSVKeyword "restrict")
  : ("s_always", pure $ TokSVKeyword "s_always")
  : ("s_eventually", pure $ TokSVKeyword "s_eventually")
  : ("s_nexttime", pure $ TokSVKeyword "s_nexttime")
  : ("s_until", pure $ TokSVKeyword "s_until")
  : ("s_until_with", pure $ TokSVKeyword "s_until_with")
  : ("strong", pure $ TokSVKeyword "strong")
  : ("sync_accept_on", pure $ TokSVKeyword "sync_accept_on")
  : ("sync_reject_on", pure $ TokSVKeyword "sync_reject_on")
  : ("unique0", pure $ TokSVKeyword "unique0")
  : ("until", pure $ TokSVKeyword "until")
  : ("until_with", pure $ TokSVKeyword "until_with")
  : ("untyped", pure $ TokSVKeyword "untyped")
  : ("weak", pure $ TokSVKeyword "weak")
  : kwSV2005

kwSV2012 :: [(SBS.ByteString, Alex Token)]
kwSV2012 =
  ("implements", pure $ TokSVKeyword "implements")
  : ("interconnect", pure $ TokSVKeyword "interconnect")
  : ("nettype", pure $ TokSVKeyword "nettype")
  : ("soft", pure $ TokSVKeyword "soft")
  : kwSV2009

kwV1995Map :: HashMap.HashMap SBS.ByteString (Alex Token)
kwV1995Map = HashMap.fromList kwV1995

kwV2001Map :: HashMap.HashMap SBS.ByteString (Alex Token)
kwV2001Map = HashMap.fromList kwV2001

kwV2001NCMap :: HashMap.HashMap SBS.ByteString (Alex Token)
kwV2001NCMap = HashMap.fromList kwV2001_nc

kwV2005Map :: HashMap.HashMap SBS.ByteString (Alex Token)
kwV2005Map = HashMap.fromList kwV2005

kwSV2005Map :: HashMap.HashMap SBS.ByteString (Alex Token)
kwSV2005Map = HashMap.fromList kwSV2005

kwSV2009Map :: HashMap.HashMap SBS.ByteString (Alex Token)
kwSV2009Map = HashMap.fromList kwSV2009

kwSV2012Map :: HashMap.HashMap SBS.ByteString (Alex Token)
kwSV2012Map = HashMap.fromList kwSV2012

}
