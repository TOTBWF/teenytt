{-# LANGUAGE RankNTypes #-}
-- |
module TeenyTT.Frontend.Parser.Monad
  (
  -- * Parser Monad
    Parser
  , runParser
  -- * Parse Errors
  , ParseError(..)
  , parseError
  -- * Start Codes
  , startCode
  , pushStartCode
  , popStartCode
  -- * Layout
  , openBlock
  , closeBlock
  , currentBlock
  -- * Locations
  , location
  , located
  , getColumn
  -- * Alex Interface
  , AlexInput(..)
  , alexGetByte
  , advance
  , getInput
  , slice
  -- ** Tokens
  , emitToken
  , emitSymbol
  , emitKeyword
  , emitLayout
  , emitNumLiteral
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad.State.Strict

import Codec.Binary.UTF8.String as UTF8
import Data.ByteString.Char8 qualified as ASCII_BS
import Data.ByteString.UTF8 qualified as UTF8_BS

import Data.Maybe (listToMaybe)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

import TeenyTT.Base.ByteString (ByteString)
import TeenyTT.Base.Location (Pos(..), Span(..), Loc(..))
import TeenyTT.Base.Location qualified as Loc

import TeenyTT.Frontend.Parser.Token

--------------------------------------------------------------------------------
-- The Parser Monad

newtype Parser a = Parser { unParser :: State ParserState a }
    deriving newtype (Functor, Applicative, Monad, MonadState ParserState)

data ParserState =
    ParserState { input      :: {-# UNPACK #-} AlexInput
                , startCodes :: {-# UNPACK #-} (NonEmpty Int)
                , layout     :: [Int]
                , span       :: Span
                }

initState :: FilePath -> [Int] -> ByteString -> ParserState
initState path codes buffer =
    ParserState { input = Input (Loc.posStart path) '\n' buffer []
                , startCodes = NE.fromList (codes ++ [0])
                , layout = []
                , span = Loc.spanStart path
                }

runParser :: (NFData a) => FilePath -> [Int] -> ByteString -> Parser a -> IO (Either ParseError a)
runParser path codes buffer (Parser m) =
    try $ evaluate $ force $ evalState m (initState path codes buffer)

--------------------------------------------------------------------------------
-- Errors

data ParseError
    = EmptyTokenStream
    | UnexpectedToken Token
    | InvalidLexeme Pos
    deriving stock Show
    deriving anyclass Exception

parseError :: ParseError -> Parser a
parseError err = throw err
    
--------------------------------------------------------------------------------
-- [NOTE: Start Codes]
--
-- Start Codes are a means of adding state to your lexer.
-- As an example, say we wanted to lex string templates.
-- A stateless lexer would have some serious difficulty here,
-- as we wouldn't be able to limit the lexing rules that apply inside
-- of the literal. However, by using Start Codes, we can have some
-- rules only apply in some states.
--
-- For our requirements, we actually need a /stack/ of start codes.
--
-- See Section 3.2.2.2 of the Alex Manual for more info.

-- | Get the current start code.
startCode :: Parser Int
startCode =
    gets \s -> NE.head s.startCodes

-- | Push a new start code to the stack.
pushStartCode :: Int -> Parser ()
pushStartCode code =
    modify' \st -> st { startCodes = code <| st.startCodes }

-- | Pop a start code off the stack.
popStartCode :: Parser ()
popStartCode =
    modify' \st ->
      st { startCodes =
           case st.startCodes of
             _ :| []     -> 0 :| []
             _ :| (x:xs) -> x :| xs
          }

--------------------------------------------------------------------------------
-- Layout

{-# INLINE openBlock #-}
openBlock :: Int -> Parser ()
openBlock cols = modify' \s -> s { layout = cols:s.layout }

{-# INLINE closeBlock #-}
closeBlock :: Parser ()
closeBlock = modify' \s -> s { layout = drop 1 $ s.layout }

{-# INLINE currentBlock #-}
currentBlock :: Parser (Maybe Int)
currentBlock = gets \s -> listToMaybe s.layout
-- (listToMaybe . parseLayout)

--------------------------------------------------------------------------------
-- Locations

{-# INLINE getColumn #-}
getColumn :: Parser Int
getColumn = gets \s -> s.input.pos.col

{-# INLINE location #-}
location :: Parser Span
location = gets \s -> s.span

{-# INLINE located #-}
located :: a -> Parser (Loc a)
located a = do
    sp <- location
    pure $ (Loc sp a)

--------------------------------------------------------------------------------
-- Alex Primitives
-- See Section 5.2 of the Alex User Manual for some explanation of these.
--
-- [NOTE: Unicode Characters + Source Positions]
-- Alex requires us to implement 'alexGetByte' when we are working with our
-- own custom lexer monad. However, this is a bit problematic when handling
-- unicode characters. 
--
-- In particular, the naive solution to handling source
-- positions doesn't work, as UTF8 characters may occupy
-- multiple bytes. To solve this, we keep track of a little
-- list of bytes for each character we lex in 'lexPendingBytes'.
--
-- Whenever we encounter a multi-byte character, we emit
-- the first byte, and store the remaining bytes inside of that
-- buffer. When Alex calls 'alexGetByte' again, instead of advancing
-- our position, we pop a byte off of that buffer instead.
-- Finally, once that buffer is exhausted, we grab another character off
-- 'lexBytes', and the process repeats.

data AlexInput =
    Input { pos             :: Pos
          , prevChar        :: Char
          , buffer          :: ByteString
          , pendingBytes    :: [Word8]
          -- ^ See [NOTE: Unicode Characters + Source Positions]
          }

{-# INLINE nextLine #-}
nextLine :: ByteString -> AlexInput -> AlexInput
nextLine rest input =
    Input { pos = Loc.nextLine input.pos
          , prevChar = '\n'
          , buffer = rest
          , pendingBytes = []
          }

{-# INLINE nextCol #-}
nextCol :: Char -> ByteString -> AlexInput -> AlexInput
nextCol c rest input =
    input { pos = Loc.nextCol input.pos 
          , prevChar = c
          , buffer = rest
          }

{-# INLINE popBufferedBytes #-}
popBufferedBytes :: AlexInput -> Maybe (Word8, AlexInput)
popBufferedBytes input =
    case input.pendingBytes of
      [] -> Nothing
      (b : bs) -> Just (b, input { pendingBytes = bs })

{-# INLINE bufferBytes #-}
bufferBytes :: Char -> [Word8] -> ByteString -> AlexInput -> AlexInput
bufferBytes c bytes rest input =
    input { prevChar = c
          , buffer = rest
          , pendingBytes = bytes
          }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input =
    case popBufferedBytes input of
      Nothing -> nextChar <$> UTF8_BS.uncons input.buffer
      ok -> ok
    where
      nextChar :: (Char, ByteString) -> (Word8, AlexInput)
      nextChar ('\n', rest) = (0x0a, nextLine rest input)
      nextChar (c, rest) = 
          case UTF8.encodeChar c of
            [b]      -> (b, nextCol c rest input)
            (b : bs) -> (b, bufferBytes c bs rest input)
            []       -> error "The impossible happened! A char decoded to 0 bytes."

{-# INLINE getInput #-}
getInput :: Parser AlexInput
getInput = gets \s -> s.input

{-# INLINE advance #-}
advance :: AlexInput -> Parser ()
advance input =
    modify' \s -> s { input = input
                    , span = Loc.spanning (Loc.stopPos s.span) input.pos
                    }

{-# INLINE slice #-}
slice :: Int -> AlexInput -> ByteString
slice n input = UTF8_BS.take n input.buffer

--------------------------------------------------------------------------------
-- Tokens

{-# INLINE emitToken #-}
emitToken :: (Loc Text -> Token) -> ByteString -> Parser Token
emitToken k bs = k <$> located (TE.decodeUtf8 bs)

{-# INLINE emitNumLiteral #-}
emitNumLiteral :: ByteString -> Parser Token
emitNumLiteral bs = do
    sp <- location
    case ASCII_BS.readInteger bs of
      Just (n, _) -> pure $ TokLiteral (NumLit (Loc sp n))
      Nothing -> parseError $ InvalidLexeme (Loc.startPos sp)

{-# INLINE emitSymbol #-}
emitSymbol :: Symbol -> ByteString -> Parser Token
emitSymbol sym _ = TokSymbol <$> located sym

{-# INLINE emitKeyword #-}
emitKeyword :: Keyword -> ByteString -> Parser Token
emitKeyword key _ = TokKeyword <$> located key

{-# INLINE emitLayout #-}
emitLayout :: Symbol -> Parser Token
emitLayout sym = TokSymbol <$> located sym
