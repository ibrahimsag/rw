{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

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

import Control.Monad.Trans
import Control.Monad.State.Strict

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


type E_Terminator = (Name, Maybe Operand, [Name], [ByteString])

data O_Type = O_To | O_From
type E_Instruction = (Name, [(O_Type, Operand)])

type E_Block = (Int, Name, [(Int, Name, E_Instruction)], (Int, Name, E_Terminator))

type E_Def = (Int, Name, [(Int, Name)], [E_Block])

data GatherState = GatherState {
    name_map :: Map.Map Name Int
  , next_index :: Int
  }

initialGather :: GatherState
initialGather = GatherState {
    name_map = Map.empty
  , next_index = 0
  }

type Gather = State GatherState

get_and_increment_index :: Gather Int
get_and_increment_index = do
  s <- get
  let ind = next_index s
  put (s {next_index = ind + 1})
  pure ind

insert_name :: Name -> Int -> Gather ()
insert_name n i = do
  s <- get
  let m = name_map s
  put (s {name_map = Map.insert n i m})
  pure ()

gather :: LLVM.AST.Module -> [Gather E_Def]
gather m = catMaybes $ map map_def (moduleDefinitions m)
  where
    map_def :: Definition -> Maybe (Gather E_Def)
    map_def d = case d of
      GlobalDefinition g -> map_global g
      _ -> Nothing
    map_global :: Global -> Maybe (Gather E_Def)
    map_global g = case g of
      Function linkage visibility dllStorageClass callingConvention returnAttributes returnType name parameters functionAttributes section comdat alignment garbageCollectorName prefix basicBlocks personalityFunction metadata
        -> Just (map_func name (fst parameters) basicBlocks)
      _ -> Nothing
    map_func :: Name -> [Parameter] -> [BasicBlock] -> Gather E_Def
    map_func name parameters basicBlocks = do
      ind <- get_and_increment_index
      blocks <- traverse map_basicBlock (reverse basicBlocks)
      params <- traverse map_parameter (reverse parameters)
      pure (ind, name, params, blocks)
    map_parameter (Parameter ty name attrs) = do
      ind <- get_and_increment_index
      insert_name name ind
      pure (ind, name)
    map_basicBlock :: BasicBlock -> Gather (E_Block)
    map_basicBlock (BasicBlock name instructions terminator) = do
      t <- map_term terminator
      inst <- traverse map_inst (reverse instructions)
      ind <- get_and_increment_index
      insert_name name ind
      pure (ind, name, inst, t)
    map_term named_t = do
      ind <- get_and_increment_index
      case named_t of
        (n := t) -> pure (ind, n, map_terminator t)
        Do t     -> pure (ind, mkName "u",  map_terminator t)
    map_inst named_i = do
      ind <- get_and_increment_index
      case named_i of
        (n := instr) -> do
          insert_name n ind
          pure (ind, n, map_instruction instr)
        Do i     -> pure (ind, mkName "u", map_instruction i)
    map_terminator :: AST.Terminator -> (E_Terminator)
    map_terminator block_terminator = case block_terminator of
      Ret returnOperand m -> ("ret", returnOperand, [], [])
      CondBr condition trueDest falseDest m -> ("cond", Just condition, [trueDest, falseDest], ["true", "false"])
      Br dest m -> ("br", Nothing, [dest], [""])
      _ -> ("u", Nothing, [], [])
    map_instruction :: AST.Instruction -> (E_Instruction)
    map_instruction inst = case inst of
      Phi ty incomingValues m -> ("phi", map ((O_From,) . fst) incomingValues)
      Add nsw nuw operand0 operand1 m -> ("add", map (O_From,) [operand0, operand1])
      Sub nsw nuw operand0 operand1 m -> ("sub", map (O_From,) [operand0, operand1])
      Mul nsw nuw operand0 operand1 m -> ("mul", map (O_From,) [operand0, operand1])
      And operand0 operand1 m -> ("and", map (O_From,) [operand0, operand1])
      Or  operand0 operand1 m -> ("or",  map (O_From,) [operand0, operand1])
      Xor operand0 operand1 m -> ("xor", map (O_From,) [operand0, operand1])
      ICmp ipred operand0 operand1 m -> ("icmp", map (O_From,) [operand0, operand1])
      ZExt operand ty m -> ("zext", map (O_From,) [operand])
      Call tc cc ra f args fa m -> let fo = (fromRight (error "inline assembly encountered") f)
                                   in ("call", (O_From, fo) : map ((O_From,). fst) args)
      GetElementPtr inbounds address indices md -> ("gep", [(O_From, address)])
      Store volatile address value maybeAtomicity alignment metadata -> ("store", [(O_From, value), (O_To, address)])
      Load volatile address maybeAtomicity alignment metadata -> ("load", [(O_From, address)])
      Alloca ty num alignment md -> ("alloca", [])
      _     -> ("null", [])

data E_Ty   = E_Fun | E_Label | E_Instruction | E_Terminator | E_Param | E_Operand | E_Store | E_Dest
type E_Cell = (E_Ty, Int, Int, ByteString)

ref :: Operand -> Maybe Name
ref op = case op of
  LocalReference ty name -> Just name
  _ -> Nothing

rast :: Name -> ByteString
rast n = case n of
  Name sbs -> fromShort sbs
  UnName w -> "%" <> BS.pack (show w)

scatter :: GatherState ->  E_Def -> [E_Cell]
scatter s def = scat_def def
  where
    nm = name_map s

    from_cell :: Int -> Name -> E_Cell
    from_cell i n = (E_Operand, Map.findWithDefault (i+1) n nm, i, rast n)

    to_cell :: Int -> Name -> E_Cell
    to_cell i n = (E_Store, i, Map.findWithDefault (i+1) n nm, rast n)

    op_cell :: Int -> (O_Type, Operand) -> Maybe E_Cell
    op_cell i (ot, op) = case ot of
      O_To -> ref op >>= (Just . to_cell i)
      O_From -> ref op >>= (Just . from_cell i)

    scat_inst (i, n, (n_i, ops))
      = (catMaybes . map (op_cell i)) ops
        ++ [(E_Instruction, i, i, rast n_i)]

    scat_term i (n, maybe_op, ds, ts)
      = (map (\(dest, t) -> (E_Dest, i, Map.findWithDefault (i + 1) dest nm, t))
            (zip ds ts))
        ++ maybeToList (maybe_op >>= ref >>= (Just . from_cell i))
        ++ [(E_Terminator, i, i, rast n)]

    scat_block (i, n, is, (term_i, term_n, t))
       = scat_term term_i t
         ++ (concatMap scat_inst is)
         ++ [(E_Label, i, i, rast n)]

    scat_param (i, n) = (E_Param, i, i, rast n)

    scat_def :: E_Def -> [E_Cell]
    scat_def (i, n, ps, blocks) = (E_Fun, i, i, rast n) : (concatMap scat_block blocks) ++ (map scat_param ps)

bg         = PixelRGBA8 0 0 0 0
drawColor  = PixelRGBA8 0 0x86 0xc1 255
recColor   = PixelRGBA8 0xFF 0x53 0x73 255
pal_white  = PixelRGBA8 255 255 255 255
pal_black  = PixelRGBA8 0 0 0 255
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

t :: Font -> Int -> [E_Cell] -> Image PixelRGBA8
t font spine_length spine = renderDrawing size size pal_black $ do
          (flip traverse_) spine $ \(ty, row_i, col_i, t) -> do
            case compare col_i row_i of
              EQ -> pure ()
              LT ->
                withTexture (uniformTexture (ty_color ty)) $ do
                    stroke 1 JoinRound (CapRound, CapRound) $
                      line (vec ((row_i + 1) * 48) (col_i * 48)) (vec ((row_i + 1) * 48) ((row_i + 1) * 48))
                    stroke 1 JoinRound (CapRound, CapRound) $
                      line (vec ((row_i + 1) * 48) (col_i * 48)) (vec (col_i * 48) (col_i * 48))
              GT ->
                withTexture (uniformTexture (ty_color ty)) $ do
                    stroke 1 JoinRound (CapRound, CapRound) $
                      line (vec (row_i * 48) ((col_i + 1) * 48)) (vec (row_i * 48) (row_i * 48))
                    stroke 1 JoinRound (CapRound, CapRound) $
                      line (vec (row_i * 48) ((col_i + 1) * 48)) (vec ((col_i + 1) * 48) ((col_i + 1) * 48))
          (flip traverse_) spine $ \(ty, row_i, col_i, t) -> do
            withTexture (uniformTexture (ty_color ty)) $ do
              stroke (ty_width ty) JoinRound (CapRound, CapRound) $ do
                rectangle (vec (row_i * 48) (col_i * 48)) 46 46
            withTexture (uniformTexture pal_white) $ do
              stroke 2 JoinRound (CapRound, CapRound) $ do
                rectangle (vec (row_i * 48) (col_i * 48)) 4 4
              printTextAt font (PointSize 12) (vec (row_i * 48 + 6) (col_i * 48 + 18)) (BS.unpack t)
  where
    size = spine_length * 48
    vec x y = V2 (fromIntegral x) (fromIntegral y)

main :: IO ()
main = do
  args <- getArgs
  let llfile = head (args ++ ["fib.ll"])

  efont <- loadFontFile "../font.ttf"
  let font = fromRight (error "could not load font") efont

  zonedTime <- Time.getZonedTime
  let file_name = Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat (Just " %H-%M-%S")) zonedTime
      fn i = "i/" ++ file_name ++ "-" ++ show i ++ ".png"

  m <- withContext $ \context -> do
      m <- withModuleFromLLVMAssembly context (File llfile) moduleAST
      bs <- withModuleFromAST context m moduleLLVMAssembly
      BS.putStrLn bs
      return m

  let defs = (map (\(d, s) -> (next_index s, scatter s d)) . map (`runState` initialGather) . gather ) m

  traverse (\(i, (length, cells)) -> writePng (fn i) (t font length cells)) (zip [1..] defs)
  pure ()
