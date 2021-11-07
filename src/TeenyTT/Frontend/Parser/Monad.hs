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
  , literal
  -- * Layout
  , openBlock
  , closeBlock
  , currentBlock
  -- * State Management
  , setInput
  , getInput
  , getColumn
  -- * Alex Primitives
  , AlexInput
  , alexGetByte
  , alexPrevInputChar
  , slice
  ) where

import Data.Maybe (listToMaybe)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NE

import Control.Monad.State.Strict
import Control.Monad.Except

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSChar
import Data.ByteString.Internal qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

import TeenyTT.Frontend.Position
import TeenyTT.Frontend.Parser.Token

newtype Parser a = Parser { unParser :: StateT ParserState (Except ParseError) a }
    deriving (Functor, Applicative, Monad, MonadState ParserState, MonadError ParseError)

data ParserState =
    ParserState { parseInput      :: AlexInput
                , parseStartCodes :: (NonEmpty Int)
                , parseLayout     :: [Int]
                , parseFile       :: FilePath
                }

initState :: FilePath -> [Int] -> ByteString -> ParserState
initState path codes bs =
    ParserState { parseInput      = Input 0 1 '\n' bs
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
    { errPos      :: Position
    , errPrevByte :: ByteString
    , errMsg      :: Text
    } deriving Show

parseError :: Text -> Parser a
parseError msg = do
    pos <- getPosition
    prevByte <- gets (BS.take 1 . lexBytes . parseInput)
    throwError $ ParseError
        { errPos = pos
        , errPrevByte = prevByte
        , errMsg = msg
        }

--------------------------------------------------------------------------------
-- Tokens

{-# INLINE token #-}
token :: (Text -> Token) -> ByteString -> Parser Token
token k bs = pure (k $ TE.decodeUtf8 bs)

{-# INLINE token_ #-}
token_ :: Token -> ByteString -> Parser Token
token_ tok _ = pure tok

literal :: (Int -> Token) -> ByteString -> Parser Token
literal k bs =
    case BSChar.readInt bs of
      Just (n, _) -> pure $ k n
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

{-# INLINE getColumn #-}
getColumn :: Parser Int
getColumn = gets (lexCol . parseInput)

{-# INLINE getLine #-}
getLine :: Parser Int
getLine = gets (lexLine . parseInput)

getPosition :: Parser Position
getPosition = do
    state <- get
    let input = parseInput state
    pure $ Position
        { posLine = lexLine input
        , posCol  = lexCol input
        , posFile = parseFile state
        }


--------------------------------------------------------------------------------
-- Alex Primitives
-- See Section 5.2 of the Alex User Manual for some explanation of these.

data AlexInput =
    Input { lexLine  :: Int
          , lexCol   :: Int
          , lexPrev  :: Char
          , lexBytes :: ByteString
          }

{-# INLINE newline #-}
newline :: ByteString -> AlexInput -> AlexInput
newline rest Input{..} =
    Input { lexLine = lexLine + 1
          , lexCol = 1
          , lexPrev = '\n'
          , lexBytes = rest
          }

{-# INLINE nextCol #-}
nextCol :: Char -> ByteString -> AlexInput -> AlexInput
nextCol c rest Input{..} =
    Input { lexCol = lexCol + 1
          , lexPrev = c
          , lexBytes = rest
          , ..
          }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@Input{..} = advance <$> BS.uncons lexBytes
    where
      advance :: (Word8, ByteString) -> (Word8, AlexInput)
      advance (byte, rest) =
          case BS.w2c byte of
            '\n' -> (byte, newline rest input)
            c    -> (byte, nextCol c rest input)

alexPrevInputChar :: AlexInput -> Char
alexPrevInputChar = lexPrev

{-# INLINE slice #-}
slice :: Int -> AlexInput -> ByteString
slice n Input{..} = BS.take n lexBytes
