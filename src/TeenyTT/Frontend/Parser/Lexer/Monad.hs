-- [FIXME: Reed M, 06/11/2021] Rename this to TeenyTT.Frontend.Parser.Monad
module TeenyTT.Frontend.Parser.Lexer.Monad
  ( Lexer
  , runLexer
  -- * Start Codes
  , startCode
  , pushStartCode
  , popStartCode
  -- * Errors
  , lexError
  -- * Tokens
  , token
  , token_
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
import Data.ByteString.Internal qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

import TeenyTT.Frontend.Parser.Token

newtype Lexer a = Lexer { unLexer :: StateT LexerState (Except ByteString) a }
    deriving (Functor, Applicative, Monad, MonadState LexerState, MonadError ByteString)

data LexerState =
    LexerState { lexInput      :: {-# UNPACK #-} AlexInput
               , lexStartCodes :: {-# UNPACK #-} (NonEmpty Int)
               , lexLayout     :: [Int]
               }

initState :: [Int] -> ByteString -> LexerState
initState codes bs =
    LexerState { lexInput      = Input 0 1 '\n' bs
               , lexStartCodes = NE.fromList (codes ++ [0])
               , lexLayout     = []
               }

runLexer :: [Int] -> ByteString -> Lexer a -> Either ByteString a
runLexer codes bs lex = runExcept $ evalStateT (unLexer lex) (initState codes bs)

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
startCode :: Lexer Int
startCode = gets (NE.head . lexStartCodes)

-- | Push a new start code to the stack.
pushStartCode :: Int -> Lexer ()
pushStartCode code = modify' $ \st ->
  st { lexStartCodes = code <| (lexStartCodes st) }

-- | Pop a start code off the stack.
popStartCode :: Lexer ()
popStartCode = modify' $ \st ->
  st { lexStartCodes =
       case lexStartCodes st of
         _ :| []     -> 0 :| []
         _ :| (x:xs) -> x :| xs
       }

--------------------------------------------------------------------------------
-- Errors

lexError :: AlexInput -> Lexer a
lexError Input{..} =
    throwError (BS.take 1 $ lexBytes)

--------------------------------------------------------------------------------
-- Tokens

{-# INLINE token #-}
token :: (Text -> Token) -> ByteString -> Lexer Token
token k bs = pure (k $ TE.decodeUtf8 bs)

{-# INLINE token_ #-}
token_ :: Token -> ByteString -> Lexer Token
token_ tok _ = pure tok

--------------------------------------------------------------------------------
-- Layout

{-# INLINE openBlock #-}
openBlock :: Int -> Lexer ()
openBlock cols = modify' $ \s -> s { lexLayout = cols:(lexLayout s) }

{-# INLINE closeBlock #-}
closeBlock :: Lexer ()
closeBlock = modify' $ \s -> s { lexLayout = drop 1 $ lexLayout s }

{-# INLINE currentBlock #-}
currentBlock :: Lexer (Maybe Int)
currentBlock = gets (listToMaybe . lexLayout)


--------------------------------------------------------------------------------
-- State Management

{-# INLINE setInput #-}
setInput :: AlexInput -> Lexer ()
setInput input = modify $ \s -> s { lexInput = input }

{-# INLINE getInput #-}
getInput :: Lexer AlexInput
getInput = gets lexInput

{-# INLINE getColumn #-}
getColumn :: Lexer Int
getColumn = gets (lexCol . lexInput)

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
