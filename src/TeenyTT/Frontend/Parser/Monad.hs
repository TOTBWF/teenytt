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
  , token_
  , symbol
  , keyword
  , literal
  -- * Layout
  , openBlock
  , closeBlock
  , currentBlock
  -- * State Management
  , setInput
  , getInput
  , getParseColumn
  , getSpan
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
                }

initState :: FilePath -> [Int] -> ByteString -> ParserState
initState path codes bs =
    ParserState { parseInput      = Input (Span 0 1 0 1) '\n' bs []
                , parseStartCodes = NE.fromList (codes ++ [0])
                , parseLayout     = []
                , parseFile       = path
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
token k bs = do
    sp <- getSpan
    pure $ k $ Loc sp (TE.decodeUtf8 bs)

{-# INLINE token_ #-}
token_ :: Token -> ByteString -> Parser Token
token_ tok _ = pure tok

{-# INLINE symbol #-}
symbol :: Symbol -> ByteString -> Parser Token
symbol sym _ = TokSymbol sym <$> getSpan

keyword :: Keyword -> ByteString -> Parser Token
keyword key _ =
    TokKeyword key <$> getSpan

literal :: (Int -> Literal) -> ByteString -> Parser Token
literal k bs = do
    sp <- getSpan
    case BSChar.readInt bs of
      Just (n, _) -> pure $ TokLiteral $ Loc sp (k n)
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

{-# INLINE setInput #-}
setInput :: AlexInput -> Parser ()
setInput input = modify $ \s -> s { parseInput = input }

{-# INLINE getInput #-}
getInput :: Parser AlexInput
getInput = gets parseInput

{-# INLINE getParseColumn #-}
getParseColumn :: Parser Int
getParseColumn = gets (endCol . lexSpan . parseInput)

{-# INLINE getSpan #-}
getSpan :: Parser Span
getSpan = gets (lexSpan . parseInput)


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
-- list of bytes for each character we lex in 'lexCharBytes'.
--
-- Whenever we encounter a multi-byte character, we emit
-- the first byte, and store the remaining bytes inside of that
-- buffer. When Alex calls 'alexGetByte' again, instead of advancing
-- our position, we pop a byte off of that buffer instead.
-- Finally, once that buffer is exhausted, we grab another character off
-- 'lexBytes', and the process repeats.

data AlexInput =
    Input { lexSpan      :: Span
          , lexPrevChar  :: Char
          , lexBytes     :: ByteString
          , lexCharBytes :: [Word8]
          -- ^ See [NOTE: Unicode Characters + Source Positions]
          }

{-# INLINE newline #-}
newline :: ByteString -> AlexInput -> AlexInput
newline rest Input{..} =
    Input { lexSpan = lexSpan { startLine = startLine lexSpan + 1, startCol = endCol lexSpan, endLine = endLine lexSpan + 1, endCol = 1 }
    -- lexLine = lexLine + 1
          , lexPrevChar = '\n'
          , lexBytes = rest
          , lexCharBytes = []
          }

{-# INLINE nextCol #-}
nextCol :: Char -> ByteString -> AlexInput -> AlexInput
nextCol c rest Input{..} =
    Input { lexSpan = lexSpan { startCol = endCol lexSpan, endCol = endCol lexSpan + 1 }
          , lexPrevChar = c
          , lexBytes = rest
          , ..
          }

{-# INLINE popBufferedBytes #-}
popBufferedBytes :: AlexInput -> Maybe (Word8, AlexInput)
popBufferedBytes Input{..} = 
    case lexCharBytes of
      [] -> Nothing
      (b : bs) -> Just (b, Input { lexCharBytes = bs, .. })

{-# INLINE bufferBytes #-}
bufferBytes :: Char -> [Word8] -> ByteString -> AlexInput -> AlexInput
bufferBytes c bytes rest Input{..} =
    Input { lexPrevChar = c
          , lexBytes = rest
          , lexCharBytes = bytes
          , ..
          }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@Input{..} =
    case popBufferedBytes input of
        Nothing -> advance <$> UTFBS.uncons lexBytes
        ok      -> ok
    where
      advance :: (Char, ByteString) -> (Word8, AlexInput)
      advance ('\n', rest) = (BS.c2w '\n', newline rest input)
      advance (c, rest)   =
          case UTF8.encodeChar c of
            [b]    -> (b, nextCol c rest input)
            (b:bs) -> (b, bufferBytes c bs rest input)
            []     -> error "The impossible happened! A Char decoded to 0 bytes."

alexPrevInputChar :: AlexInput -> Char
alexPrevInputChar = lexPrevChar

{-# INLINE slice #-}
slice :: Int -> AlexInput -> ByteString
slice n Input{..} = BS.take n lexBytes
