{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import System.Environment
import System.Random

import qualified Data.Time.Format as Time
import qualified Data.Time.LocalTime as Time

import qualified Data.Map.Strict as Map

import Data.Foldable (traverse_, foldl')
import Data.Either (fromRight)
import Data.Maybe (catMaybes, maybeToList, fromMaybe)

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)

import qualified Data.ByteString.Short as SBS
import Data.ByteString.Short (ShortByteString, toShort, fromShort)

import Text.Ascii (ascii)
import LLVM.AST hiding (function)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Instruction as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import LLVM.Module
import LLVM.Context

import Codec.Picture (Image, PixelRGBA8( .. ), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Graphics.Text.TrueType (Font, loadFontFile)


simpleModule :: LLVM.AST.Module
simpleModule = buildModule "exampleModule" $ mdo
  function "Factorial" [(AST.i32, "a"), (AST.i32, "b")] AST.i32 $ \[a, b] -> mdo
    entry <- block `named` "entry"; do
      c <- add a b
      ret c

simple :: LLVM.AST.Module -> IO ByteString
simple m = withContext $ \context -> 
    withModuleFromAST context m moduleLLVMAssembly

as_main :: IO ()
as_main = BS.putStrLn "Hello, Haskell!" >> (simple simpleModule >>= BS.putStrLn)

type E_Terminator = (Name, Maybe Operand, [Name], [ByteString])

type E_Instruction = (Name, [Operand], [Operand])

type E_Block = (Name, [(Name, E_Instruction)], (Name, E_Terminator))
type E_Def = (Name, [Name], [E_Block])

data E_Ty   = E_Fun | E_Label | E_Instruction | E_Terminator | E_Param | E_Operand | E_Store | E_Dest
type E_Cell = (E_Ty, Maybe ByteString, ByteString)

ref :: Operand -> Maybe ByteString
ref op = case op of
  LocalReference ty name -> Just (rast name)
  _ -> Nothing

rast :: Name -> ByteString
rast n = case n of
  Name sbs -> fromShort sbs
  UnName w -> "%" <> BS.pack (show w)

scatter :: [E_Def] -> [E_Cell]
scatter defs = concatMap scat_def defs
  where
    ref_to_cell bs = Just (E_Operand, Nothing, bs)

    scat_ops = catMaybes . map ( (>>= ref_to_cell) . ref)

    scat_stores = map (\dest -> (E_Store, ref dest, fromMaybe (error "no name for store") (ref dest)))
    scat_inst (n, (n_i, ops, sts)) = scat_ops ops ++ scat_stores sts ++ [(E_Instruction, Just (rast n), rast n_i)]

    scat_dests = map (\(dest, t) -> (E_Dest, Just (rast dest), t))
    scat_term (n, maybe_op, ds, ts) = (scat_dests (zip ds ts))
                                      ++ maybeToList (maybe_op >>= ref >>= ref_to_cell)
                                      ++ [(E_Terminator, Nothing, rast n)]

    scat_block (n, is, (term_n, t)) =  scat_term t ++ (concatMap scat_inst (reverse is)) ++ [(E_Label, Nothing, rast n)]
    scat_param n = (E_Param, Nothing, rast n)
    scat_def (n, ps, blocks) = (E_Fun, Nothing, rast n) : (concatMap scat_block (reverse blocks)) ++ (map scat_param (reverse ps))

gather :: LLVM.AST.Module -> [E_Def]
gather m = catMaybes $ map map_def (moduleDefinitions m)
  where
    map_term named_t = case named_t of
      (n := t) -> (n, map_terminator t)
      Do t     -> (mkName "u", map_terminator t)
    map_inst named_i = case named_i of
      (n := i) -> (n, map_instruction i)
      Do i     -> (mkName "u", map_instruction i)
    map_basicBlock (BasicBlock name instructions terminator) 
        = (name, map map_inst instructions, map_term terminator)
    map_parameter (Parameter ty name attrs) = name
    map_func name parameters basicBlocks = (name, map map_parameter parameters, map map_basicBlock basicBlocks)
    map_global g = case g of
      Function 
          linkage
          visibility
          dllStorageClass
          callingConvention
          returnAttributes
          returnType
          name
          parameters
          functionAttributes
          section
          comdat
          alignment
          garbageCollectorName
          prefix
          basicBlocks
          personalityFunction
          metadata
        -> Just (map_func name (fst parameters) basicBlocks)
      _ -> Nothing
    map_def d = case d of
      GlobalDefinition g -> map_global g
      _ -> Nothing
    map_terminator :: AST.Terminator -> (E_Terminator)
    map_terminator block_terminator = case block_terminator of
      Ret returnOperand m -> ("ret", returnOperand, [], [])
      CondBr condition trueDest falseDest m -> ("cond", Just condition, [trueDest, falseDest], ["true", "false"])
      Br dest m -> ("br", Nothing, [dest], [""])
      _ -> ("u", Nothing, [], [])
    map_instruction :: AST.Instruction -> (E_Instruction)
    map_instruction inst = case inst of
      Phi ty incomingValues m -> ("phi", map fst incomingValues, [])
      Add nsw nuw operand0 operand1 m -> ("add", [operand0, operand1], [])
      Sub nsw nuw operand0 operand1 m -> ("sub", [operand0, operand1], [])
      Mul nsw nuw operand0 operand1 m -> ("mul", [operand0, operand1], [])
      And operand0 operand1 m -> ("and", [operand0, operand1], [])
      Or  operand0 operand1 m -> ("or",  [operand0, operand1], [])
      Xor operand0 operand1 m -> ("xor", [operand0, operand1], [])
      ICmp ipred operand0 operand1 m -> ("icmp", [operand0, operand1], [])
      ZExt operand ty m -> ("zext", [operand], [])
      Call tc cc ra f args fa m -> ("call", [fromRight (error "inline assembly encountered") f], [])
      GetElementPtr inbounds address indices md -> ("gep", [address], [])
      Store volatile address value maybeAtomicity alignment metadata -> ("store", [value], [address])
      Load volatile address maybeAtomicity alignment metadata -> ("load", [address], [])
      Alloca ty num alignment md -> ("alloca", [], [])
      _     -> ("null", [], [])

bg = PixelRGBA8 0 0 0 0
drawColor = PixelRGBA8 0 0x86 0xc1 255
recColor = PixelRGBA8 0xFF 0x53 0x73 255
pal_white = PixelRGBA8 255 255 255 255
pal_black = PixelRGBA8 0 0 0 255
pal_yellow = PixelRGBA8 0xFF 0xF5 0x77 255
pal_green  = PixelRGBA8 0x7d 0xce 0x82 255
pal_red    = PixelRGBA8 0xff 0x70 0x85 255
pal_bl1    = PixelRGBA8 0x3c 0xdb 0xd3 255
pal_bl2    = PixelRGBA8 0x00 0xff 0xf5 255

ty_color ty = case ty of
  E_Fun -> pal_white
  E_Label -> pal_yellow
  E_Instruction -> pal_red
  E_Terminator -> pal_bl2
  E_Operand -> pal_green
  E_Store -> pal_green
  E_Param   -> pal_green
  E_Dest -> pal_bl2

ty_width ty = case ty of
  E_Fun -> 6
  E_Label -> 4
  E_Instruction -> 4
  E_Terminator -> 4
  E_Operand -> 4
  E_Store -> 4
  E_Param -> 4
  E_Dest -> 4


t :: Int -> Font -> [(E_Ty, Int, Int, ByteString)] -> Image PixelRGBA8
t spine_length font spine = renderDrawing size size pal_black $ do
          (flip traverse_) spine $ \(ty, row_i, col_i, t) -> do
            case compare col_i row_i of
              EQ -> pure ()
              LT -> 
                withTexture (uniformTexture (ty_color ty)) $ do
                    stroke 1 JoinRound (CapRound, CapRound) $
                      line (vec ((row_i + 1) * 48 - 2) (col_i * 48 - 2)) (vec ((row_i + 1) * 48 - 2) ((row_i + 1) * 48 - 2))
                    stroke 1 JoinRound (CapRound, CapRound) $
                      line (vec ((row_i + 1) * 48 - 2) (col_i * 48 - 2)) (vec (col_i * 48 - 2) (col_i * 48 - 2))
              GT -> 
                withTexture (uniformTexture (ty_color ty)) $ do
                    stroke 1 JoinRound (CapRound, CapRound) $
                      line (vec (row_i * 48 - 2) ((col_i + 1) * 48 - 2)) (vec (row_i * 48 - 2) (row_i * 48 - 2))
                    stroke 1 JoinRound (CapRound, CapRound) $
                      line (vec (row_i * 48 - 2) ((col_i + 1) * 48 - 2)) (vec ((col_i + 1) * 48 - 2) ((col_i + 1) * 48 - 2))
          (flip traverse_) spine $ \(ty, row_i, col_i, t) -> do
            withTexture (uniformTexture (ty_color ty)) $ do
              stroke (ty_width ty) JoinRound (CapRound, CapRound) $ do
                rectangle (vec (row_i * 48 - 2) (col_i * 48 - 2)) 46 46
            withTexture (uniformTexture pal_white) $ do
              stroke 2 JoinRound (CapRound, CapRound) $ do
                rectangle (vec (row_i * 48 - 2) (col_i * 48 - 2)) 4 4
              printTextAt font (PointSize 12) (vec (row_i * 48 + 4) (col_i * 48 + 16)) (BS.unpack t)
  where
    size = spine_length * 48
    vec x y = V2 (fromIntegral x) (fromIntegral y)

main :: IO ()
main = do
  args <- getArgs
  let llfile = head (args ++ ["fib.ll"])

  m <- withContext $ \context -> do
      m <- withModuleFromLLVMAssembly context (File llfile) moduleAST
      bs <- withModuleFromAST context m moduleLLVMAssembly
      BS.putStrLn bs
      return m

  let nerves = (scatter . gather) m
      ins m i k = Map.insert k i m
      first_pass (label_map, operand_map, i, prev) (ty, n, t) = case ty of
        E_Label       -> (Map.insert t i label_map, operand_map, i + 1, (ty, n, i, t) : prev)
        E_Instruction -> (label_map, ins operand_map i (fromMaybe (error "no name for instruction") n) ,    i + 1, (ty, n, i, t) : prev)
        E_Param       -> (label_map, Map.insert t i operand_map, i + 1, (ty, n, i, t) : prev)
        E_Terminator  -> (label_map, operand_map,                i + 1, (ty, n, i, t) : prev)
        E_Fun         -> (label_map, operand_map,                i + 1, (ty, n, i, t) : prev)
        E_Operand     -> (label_map, operand_map,                i    , (ty, n, i, t) : prev)
        E_Store       -> (label_map, operand_map,                i    , (ty, n, i, t) : prev)
        E_Dest        -> (label_map, operand_map,                i    , (ty, n, i, t) : prev)

      (label_map, operand_map, spine_length, nerves1) = foldl' first_pass (Map.empty, Map.empty, 0, []) nerves

      second_pass prev (ty, n, i, t) = case ty of
        E_Label       -> (ty, i, i, t) : prev
        E_Instruction -> (ty, i, i, t) : prev
        E_Param       -> (ty, i, i, t) : prev
        E_Terminator  -> (ty, i, i, t) : prev
        E_Fun         -> (ty, i, i, t) : prev
        E_Operand     -> (ty, Map.findWithDefault 0 t operand_map, i, t) : prev
        E_Store       -> (ty, i, Map.findWithDefault 0 t operand_map, t) : prev
        E_Dest        -> (ty, i, Map.findWithDefault i (fromMaybe (error "unknown dest label") n) label_map, t) : prev

      nerves2 = foldl' second_pass [] nerves1

  efont <- loadFontFile "../font.ttf"
  let font = fromRight (error "could not load font") efont
  let ex = renderDrawing 400 200 pal_white $
         withTexture (uniformTexture drawColor) $ do
            fill $ circle (V2 0 0) 30
            stroke 4 JoinRound (CapRound, CapRound) $
                   circle (V2 400 200) 40
            withTexture (uniformTexture recColor) .
                   fill $ rectangle (V2 100 100) 200 100
  let pi4 = (rotate (pi/4))

  zonedTime <- Time.getZonedTime
  let file_name = Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat (Just " %H-%M-%S")) zonedTime
      fn = "i/" ++ file_name ++ ".png"

  writePng fn (t spine_length font nerves2)
