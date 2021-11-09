module TeenyTT.Frontend.Parser.Monad
  ( Parser
  , runParser
  -- * Start Codes
  , startCode
  , pushStartCode
  , popStartCode
  -- * Errors
  , ParseError(..)
  , parseError
  -- * Tokens
  , token
  , symbol
  , keyword
  , literal
  -- * Layout
  , openBlock
  , closeBlock
  , currentBlock
  -- * State Management
  , advance
  , getInput
  , getParseColumn
  , location
  , located
  -- * Alex Primitives
  , AlexInput
  , alexGetByte
  , alexPrevInputChar
  , slice
  ) where

import Control.DeepSeq

import Data.Maybe (listToMaybe)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NE

import GHC.Generics

import Control.Monad.State.Strict
import Control.Monad.Except

import Codec.Binary.UTF8.String as UTF8
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSChar
import Data.ByteString.Internal qualified as BS
import Data.ByteString.UTF8 qualified as UTFBS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

import TeenyTT.Core.Position
import TeenyTT.Frontend.Parser.Token

newtype Parser a = Parser { unParser :: StateT ParserState (Except ParseError) a }
    deriving newtype (Functor, Applicative, Monad, MonadState ParserState, MonadError ParseError)

data ParserState =
    ParserState { parseInput      :: {-# UNPACK #-} AlexInput
                , parseStartCodes :: {-# UNPACK #-} (NonEmpty Int)
                , parseLayout     :: [Int]
                , parseFile       :: FilePath
                , parseSpan       :: Span
                }

initState :: FilePath -> [Int] -> ByteString -> ParserState
initState path codes bs =
    ParserState { parseInput      = Input (Pos 0 1) '\n' bs []
                , parseStartCodes = NE.fromList (codes ++ [0])
                , parseLayout     = []
                , parseFile       = path
                , parseSpan       = Span (Pos 0 1) (Pos 0 1)
                }

runParser :: FilePath -> [Int] -> ByteString -> Parser a -> Either ParseError a
runParser path codes bs lex = runExcept $ evalStateT (unParser lex) (initState path codes bs)

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
startCode = gets (NE.head . parseStartCodes)

-- | Push a new start code to the stack.
pushStartCode :: Int -> Parser ()
pushStartCode code = modify' $ \st ->
  st { parseStartCodes = code <| (parseStartCodes st) }

-- | Pop a start code off the stack.
popStartCode :: Parser ()
popStartCode = modify' $ \st ->
  st { parseStartCodes =
       case parseStartCodes st of
         _ :| []     -> 0 :| []
         _ :| (x:xs) -> x :| xs
       }

--------------------------------------------------------------------------------
-- Errors

data ParseError = ParseError
    { errMsg      :: Text
    } deriving (Show, Generic)

instance NFData ParseError

parseError :: Text -> Parser a
parseError msg = do
    throwError $ ParseError { errMsg = msg }

--------------------------------------------------------------------------------
-- Tokens

{-# INLINE token #-}
token :: (Loc Text -> Token) -> ByteString -> Parser Token
token k bs = k <$> located (TE.decodeUtf8 bs)

{-# INLINE symbol #-}
symbol :: Symbol -> ByteString -> Parser Token
symbol sym _ = TokSymbol sym <$> location

keyword :: Keyword -> ByteString -> Parser Token
keyword key _ = TokKeyword key <$> location

literal :: (Int -> Literal) -> ByteString -> Parser Token
literal k bs =
    case BSChar.readInt bs of
      Just (n, _) -> TokLiteral <$> located (k n)
      Nothing     -> parseError "Invariant Violated: Could not read literal."

--------------------------------------------------------------------------------
-- Layout

{-# INLINE openBlock #-}
openBlock :: Int -> Parser ()
openBlock cols = modify' $ \s -> s { parseLayout = cols:(parseLayout s) }

{-# INLINE closeBlock #-}
closeBlock :: Parser ()
closeBlock = modify' $ \s -> s { parseLayout = drop 1 $ parseLayout s }

{-# INLINE currentBlock #-}
currentBlock :: Parser (Maybe Int)
currentBlock = gets (listToMaybe . parseLayout)


--------------------------------------------------------------------------------
-- State Management

{-# INLINE advance #-}
advance :: AlexInput -> Parser ()
advance input@Input{ lexPos } = do
    modify' $ \s -> s { parseInput = input
                      , parseSpan = Span (endPos $ parseSpan s) lexPos
                      }

{-# INLINE getInput #-}
getInput :: Parser AlexInput
getInput = gets parseInput

{-# INLINE getParseColumn #-}
getParseColumn :: Parser Int
getParseColumn = gets (posCol . lexPos . parseInput)

{-# INLINE location #-}
location :: Parser Span
location = gets parseSpan

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
    Input { lexPos          :: Position
          , lexPrevChar     :: Char
          , lexBytes        :: ByteString
          , lexPendingBytes :: [Word8]
          -- ^ See [NOTE: Unicode Characters + Source Positions]
          }

{-# INLINE nextLine #-}
nextLine :: ByteString -> AlexInput -> AlexInput
nextLine rest Input{..} =
    Input { lexPos = lexPos { posLine = posLine lexPos + 1, posCol = 1 }
          , lexPrevChar = '\n'
          , lexBytes = rest
          , lexPendingBytes = []
          }

{-# INLINE nextCol #-}
nextCol :: Char -> ByteString -> AlexInput -> AlexInput
nextCol c rest Input{..} =
    Input { lexPos = lexPos { posCol = posCol lexPos + 1 }
          , lexPrevChar = c
          , lexBytes = rest
          , ..
          }

{-# INLINE popBufferedBytes #-}
popBufferedBytes :: AlexInput -> Maybe (Word8, AlexInput)
popBufferedBytes Input{..} = 
    case lexPendingBytes of
      [] -> Nothing
      (b : bs) -> Just (b, Input { lexPendingBytes = bs, .. })

{-# INLINE bufferBytes #-}
bufferBytes :: Char -> [Word8] -> ByteString -> AlexInput -> AlexInput
bufferBytes c bytes rest Input{..} =
    Input { lexPrevChar = c
          , lexBytes = rest
          , lexPendingBytes = bytes
          , ..
          }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@Input{..} =
    case popBufferedBytes input of
        Nothing -> advanceChar <$> UTFBS.uncons lexBytes
        ok      -> ok
    where
      advanceChar :: (Char, ByteString) -> (Word8, AlexInput)
      advanceChar ('\n', rest) = (BS.c2w '\n', nextLine rest input)
      advanceChar (c, rest)   =
          case UTF8.encodeChar c of
            [b]    -> (b, nextCol c rest input)
            (b:bs) -> (b, bufferBytes c bs rest input)
            []     -> error "The impossible happened! A Char decoded to 0 bytes."

{-# INLINE alexPrevInputChar #-}
alexPrevInputChar :: AlexInput -> Char
alexPrevInputChar = lexPrevChar

{-# INLINE slice #-}
slice :: Int -> AlexInput -> ByteString
slice n Input{..} = BS.take n lexBytes
